/* Virtual DOM API */

use diff::{diff, Action::*};
use std::collections::HashMap;
use std::hash::Hash;
use std::ptr::null;

mod diff;

#[derive(Copy, Clone)]
pub struct Vtype<C: Context> {
    pub mount: fn(
        ctx: &mut C,
        attributes: std::collections::hash_map::Iter<C::Attribute, C::AttributeValue>,
        cd: &[Movement],
        index: usize,
    ),
    /// Remove the element at the specifed position.
    ///
    /// Necessarily per-type since all implementations might not store destructors.
    pub unmount: fn(ctx: &mut C, cd: &[Movement], index: usize),
    pub update_attributes:
        fn(
            ctx: &mut C,
            cd: &[Movement],
            index: usize,
            set: &mut dyn Iterator<Item = (&C::Attribute, &C::AttributeValue)>,
            remove: &mut dyn Iterator<Item = (&C::Attribute, &C::AttributeValue)>,
        ),
}

impl<C: Context> std::fmt::Debug for Vtype<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:p}", self as *const _)
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum Key {
    Integer(i32),
    // TODO Uncomment. Problem is that the diffing algorithm has to copy the keys.
    // String(Cow<'static, str>),
    /// Equivalent to `Key::Integer(child_index)`.
    None,
}

#[derive(Debug)]
struct Vnode<C: Context + 'static> {
    /// The type of the node.
    t: &'static Vtype<C>,
    /// Key for efficient diffing. Unique among siblings.
    /// If one sibling has an explicit key then all siblings must too.
    key: Key,
    /// The number of children plus the sum of their descendants.
    descendants: usize,
    attributes: HashMap<C::Attribute, C::AttributeValue>,
}

/**
 * TODO change movement to only encode delta between calls to context.
 * have a NextSibling(index)
 * hava a Parent(/*maybe*/ index)
 * Question: how does removal work.
 * Evaluate whether we need to pass slice to context.
 *
 * With this change we would ideally only pass cd and not index to context.
 */

/** Describes for DOM updating code where to find DOM node that need updating from the previous DOM
 * node that was updated. */
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Movement {
    Up,
    NthChild(usize),
}

pub trait Context {
    type Attribute: Eq + Hash;
    type AttributeValue: PartialEq;

    fn move_node(&mut self, cd: &[Movement], from: usize, index: usize);

    /// Callback for when a render is started.
    fn on_start(&mut self);
}

#[derive(Default)]
pub struct Vtree<C: Context + 'static> {
    stack: Vec<Vnode<C>>,
    parent_stack: Vec<usize>,
}

impl<C: Context> Vtree<C> {
    pub fn new() -> Vtree<C> {
        Vtree {
            stack: Vec::new(),
            parent_stack: Vec::new(),
        }
    }

    pub fn create_element(&mut self, t: &'static Vtype<C>) {
        self.parent_stack.push(self.stack.len());
        self.stack.push(Vnode {
            t,
            key: Key::None,
            attributes: HashMap::new(),
            descendants: 0,
        });
    }

    pub fn add_attribute<V: Into<C::AttributeValue>>(&mut self, key: C::Attribute, value: V) {
        self.stack
            .last_mut()
            .expect("Have no element to set attribute on")
            .attributes
            .insert(key, value.into());
    }

    pub fn pop_parent(&mut self) {
        let child_descendants =
            self.stack[self
                           .parent_stack
                           .pop()
                           .expect("Popped parent without pushing")].descendants;
        if let Some(&parent_idx) = self.parent_stack.last() {
            self.stack[parent_idx].descendants += 1 + child_descendants;
        }
    }

    pub fn set_key(&mut self, key: i32) {
        self.stack
            .last_mut()
            .expect("Have no element to set key on")
            .key = Key::Integer(key);
    }

    /// Returns whether the type of the active parent matches the specified type.
    #[doc(hidden)]
    pub fn parent_type_eq(&self, t: &'static Vtype<C>) -> bool {
        self.stack[*self.parent_stack.last().unwrap()].t as *const _ == t as *const _
    }
}

/// Returns an iterator of the children of a virtual node
fn get_children<C: Context>(node: *const Vnode<C>) -> impl Iterator<Item = *const Vnode<C>> {
    struct ChildIterator<C: Context + 'static> {
        node: *const Vnode<C>,
        i: usize,
    }

    impl<C: Context> Iterator for ChildIterator<C> {
        type Item = *const Vnode<C>;

        fn next(&mut self) -> Option<Self::Item> {
            // Let there be nasal demons
            unsafe {
                if (*self.node).descendants < self.i {
                    None
                } else {
                    let child = self.node.offset(self.i as isize);
                    self.i += 1 + (*child).descendants;
                    Some(child)
                }
            }
        }
    }

    ChildIterator { node, i: 1 }
}

#[macro_export]
macro_rules! html {
    ($ctx:ident, < $tag:ident $($tt:tt)*) => {
        $ctx.create_element(&$tag);
        html!{@tag $ctx, $($tt)*}
    };
    ($ctx:ident, </ $tag:ident > $($tt:tt)*) => {
        debug_assert!($ctx.parent_type_eq(&$tag), "Mismatched closing tag.");
        $ctx.pop_parent();
        html!{$ctx, $($tt)*}
    };
    /* Self-closing tag */
    (@tag $ctx:ident, /> $($tt:tt)*) => {
        $ctx.pop_parent();
        html!{$ctx, $($tt)*}
    };
    // Key setter
    (@tag $ctx:ident, key = $val:expr, $($tt:tt)*) => {
        $ctx.set_key($val);
        html!{@tag $ctx, $($tt)*}
    };
    // Setter for attribute
    (@tag $ctx:ident, $attr:path = $val:expr, $($tt:tt)*) => {
        $ctx.add_attribute($attr, $val);
        html!{@tag $ctx, $($tt)*}
    };
    (@tag $ctx:ident, > $($tt:tt)*) => {
        html!{$ctx, $($tt)*}
    };
    // Inline block
    ($ctx:ident, $block:block $($tt:tt)*) => {
        $block
        html!{$ctx, $($tt)*}
    };
    // End of macro
    ($ctx:ident,) => {};
}

pub fn patch<C: Context>(ctx: &mut C, prev: Vtree<C>, next: &Vtree<C>) {
    // Stack of where to go to find next DOM node that needs updating
    let mut cd: Vec<Movement> = Vec::new();
    ctx.on_start();

    fn insert<C: Context>(
        ctx: &mut C,
        cd: &mut Vec<Movement>,
        node: *const Vnode<C>,
        index: usize,
    ) {
        (unsafe { &*node }.t.mount)(ctx, unsafe { &*node }.attributes.iter(), cd, index);
        cd.push(Movement::NthChild(index));
        for child in get_children(node).enumerate() {
            insert(ctx, cd, child.1, child.0);
        }
        cd.pop();
    }

    fn remove<C: Context>(
        ctx: &mut C,
        cd: &mut Vec<Movement>,
        node: *const Vnode<C>,
        index: usize,
    ) {
        cd.push(Movement::NthChild(index));
        for child in get_children(node) {
            remove(ctx, cd, child, 0);
        }
        cd.pop();
        (unsafe { &*node }.t.unmount)(ctx, cd, index);
    }

    fn diff_node<C: Context>(
        ctx: &mut C,
        cd: &mut Vec<Movement>,
        p: *const Vnode<C>,
        n: *const Vnode<C>,
        index: usize,
    ) {
        match unsafe { (p.as_ref(), n.as_ref()) } {
            (None, None) => {}
            (Some(node), None) => {
                // Detach old node since new one is null
                remove(ctx, cd, node, index);
            }
            (None, Some(node)) => {
                // Attach new node since old one was null
                insert(ctx, cd, node, index);
            }
            (Some(old), Some(new)) => {
                if old.t as *const _ != new.t as *const _ {
                    // Differing types; tear down and rebuild
                    remove(ctx, cd, old, index);
                    insert(ctx, cd, new, index);
                } else {
                    // Diff attributes
                    (new.t.update_attributes)(
                        ctx,
                        cd,
                        index,
                        &mut new.attributes.iter().filter(|(key, val)| {
                            old.attributes
                                .get(key)
                                .map_or(true, |ref other| val != other)
                        }),
                        &mut old
                            .attributes
                            .iter()
                            .filter(|(key, _)| !new.attributes.contains_key(key)),
                    );

                    // Diff children
                    cd.push(Movement::NthChild(index));
                    let (pchildren, nchildren) = (
                        get_children(p).collect::<Vec<_>>(),
                        get_children(n).collect::<Vec<_>>(),
                    );
                    let get_keys = |children: &Vec<*const Vnode<C>>| {
                        children
                            .iter()
                            .map(|&x| unsafe { &*x }.key)
                            .enumerate()
                            .map(|(i, x)| {
                                if let Key::None = x {
                                    Key::Integer(i as i32)
                                } else {
                                    x
                                }
                            }).collect::<Vec<_>>()
                    };

                    for action in diff(&get_keys(&pchildren), &get_keys(&nchildren)) {
                        match action {
                            Insert(i) => {
                                diff_node(ctx, cd, null(), nchildren[i], i);
                            }
                            Remove(i) => {
                                diff_node(ctx, cd, pchildren[i], null(), i);
                            }
                            Move(i, j, old_index) => {
                                ctx.move_node(cd, i, j);
                                diff_node(ctx, cd, pchildren[old_index], nchildren[j], i);
                            }
                            Update(i, j) => {
                                diff_node(ctx, cd, pchildren[j], nchildren[i], i);
                            }
                        }
                    }
                    cd.pop();
                }
            }
        }
    }

    // Diff root nodes
    diff_node(
        ctx,
        &mut cd,
        if prev.stack.is_empty() {
            null()
        } else {
            prev.stack.as_ptr()
        },
        if next.stack.is_empty() {
            null()
        } else {
            next.stack.as_ptr()
        },
        0,
    );
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
