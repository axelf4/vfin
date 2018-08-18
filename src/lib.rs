/* Virtual DOM API */

#![feature(raw)]
use diff::{diff, Action::*};
use std::collections::HashMap;
use std::hash::Hash;
use std::ptr::{null, null_mut};
use std::{mem, raw};

mod diff;

/// A virtual DOM type corresponding to a real DOM element.
pub trait Vtype<C: Context, S = Self> {
    /// Inserts the element at the specified position.
    fn mount(
        &mut self,
        ctx: &mut C,
        cd: &[Movement],
        attributes: std::collections::hash_map::Iter<C::Attribute, C::AttributeValue>,
    );
    /// Remove the element at the specifed position.
    ///
    /// Necessarily per-type since all implementations might not store destructors.
    fn unmount(&mut self, ctx: &mut C, cd: &[Movement]);
    /// Updates by removing and setting - replacing if necessary - the specified attibutes.
    fn update_attributes(
        &mut self,
        ctx: &mut C,
        cd: &[Movement],
        set: &mut dyn Iterator<Item = (&C::Attribute, &C::AttributeValue)>,
        remove: &mut dyn Iterator<Item = (&C::Attribute, &C::AttributeValue)>,
        last_props: Option<&mut S>,
    );
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
    t: *mut Vtype<C, ()>,
    /// A key, unique among siblings, for efficient diffing.
    /// If one sibling has an explicit key then all siblings must follow.
    key: Key,
    /// The number of children plus the sum of their descendants.
    descendants: usize,
    attributes: HashMap<C::Attribute, C::AttributeValue>,
}

impl<C: Context> Drop for Vnode<C> {
    fn drop(&mut self) {
        // Deallocate node properties if needed
        if !unsafe { mem::transmute::<_, raw::TraitObject>(self.t) }
            .data
            .is_null()
        {
            unsafe { Box::from_raw(self.t) };
        }
    }
}

/// Deltas for finding a DOM node starting from the last affected.
///
/// Removals should leave the cursor at the parent.
/// After insertions, moves and updates the cursor should point to the affected node.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Movement {
    /// Move to the parent of the current node.
    Parent,
    /// Move to the n-th child of the current node.
    NthChild(usize),
    /// Equivalent to NthChild(0).
    FirstChild,
    /// Move to the next sibling. Equivalent to Parent, followed by NthChild(usize).
    NextSibling(usize),
}

/// User-defined context for carrying out the patching. Passed to all callback.
pub trait Context {
    /// Type used as key for attributes.
    type Attribute: Eq + Hash;
    /// The type of the attribute values.
    type AttributeValue: PartialEq;

    /// Moves the node at child index `from` to the location of `cd` at the same level.
    fn move_node(&mut self, cd: &[Movement], from: usize);

    /// Callback for when a render is started.
    fn on_start(&mut self);

    /// Constructs a new context for the root node specified by `cd`.
    /// Used by components for rendering their own virtual tree.
    fn derive_from(&mut self, cd: &[Movement]) -> Self;
}

/// A virtual DOM tree.
#[derive(Default)]
pub struct Vtree<C: Context + 'static> {
    stack: Vec<Vnode<C>>,
    parent_stack: Vec<usize>,
}

impl<C: Context> Vtree<C> {
    /// Constructs a new, empty, `Vtree`.
    pub fn new() -> Vtree<C> {
        Vtree {
            stack: Vec::new(),
            parent_stack: Vec::new(),
        }
    }

    /// Adds a node to the tree. Followed by a mandatory complementary call to `pop_parent`.
    ///
    /// Following calls will add children to the added node.
    pub fn create_element<S: Vtype<C>>(&mut self, mut s: S) {
        self.parent_stack.push(self.stack.len());
        self.stack.push(Vnode {
            t: if std::mem::size_of::<S>() == 0 {
                let t: &mut Vtype<C, S> = &mut s;
                let mut raw_object: raw::TraitObject = unsafe { mem::transmute(t) };
                raw_object.data = null_mut();
                unsafe { mem::transmute(raw_object) }
            } else {
                let boxed: Box<Vtype<C, S>> = Box::new(s);
                Box::into_raw(boxed) as *mut Vtype<C, ()>
            },
            key: Key::None,
            attributes: HashMap::new(),
            descendants: 0,
        });
    }

    /// Adds the specified attribute to the current node.
    pub fn add_attribute<V: Into<C::AttributeValue>>(&mut self, key: C::Attribute, value: V) {
        self.stack
            .last_mut()
            .expect("Have no element to set attribute on")
            .attributes
            .insert(key, value.into());
    }

    /// Stops adding children to the current node.
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

    /// Sets the key of the current node.
    pub fn set_key(&mut self, key: i32) {
        self.stack
            .last_mut()
            .expect("Have no element to set key on")
            .key = Key::Integer(key);
    }

    /*/// Returns whether the type of the active parent matches the specified type.
    #[doc(hidden)]
    pub fn parent_type_eq(&self, t: &'static Vtype<C>) -> bool {
        self.stack[*self.parent_stack.last().unwrap()].t as *const _ == t as *const _
    }*/
}

/// A convenience macro for building trees with a HTML-esque language.
#[macro_export]
macro_rules! html {
    // Start of component tag
    ($ctx:ident, <C $cmp:expr, $($tt:tt)*) => {
        $ctx.create_element($cmp);
        html!{@tag $ctx, $($tt)*}
    };
    // Start of opening tag
    ($ctx:ident, < $tag:ident $($tt:tt)*) => {
        $ctx.create_element($tag);
        html!{@tag $ctx, $($tt)*}
    };
    // End tag
    ($ctx:ident, </ $tag:ident > $($tt:tt)*) => {
        // debug_assert!($ctx.parent_type_eq(&$tag), "Mismatched closing tag.");
        $ctx.pop_parent();
        html!{$ctx, $($tt)*}
    };
    // Self-closing tag
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

/// Returns an iterator of the children of a virtual node.
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

/// Patches the `previous` tree to the one in `next`, with the specified context `ctx`.
pub fn patch<C: Context>(ctx: &mut C, prev: Vtree<C>, next: &Vtree<C>) {
    use Movement::{FirstChild, NextSibling};
    // Stack of where to go to find next DOM node that needs updating
    let mut cd: Vec<Movement> = vec![FirstChild];
    ctx.on_start();

    /// Recursively mount the specified node.
    fn insert<C: Context>(ctx: &mut C, cd: &mut Vec<Movement>, node: *const Vnode<C>) {
        unsafe { &mut *(*node).t }.mount(ctx, cd, unsafe { &*node }.attributes.iter());
        cd.clear();
        let mut has_children = false;
        for (i, child) in get_children(node).enumerate() {
            cd.push(if i == 0 { FirstChild } else { NextSibling(i) });
            insert(ctx, cd, child);
            has_children = true;
        }
        if has_children {
            cd.push(Movement::Parent);
        }
    }

    /// Recursively unmount the specified node.
    fn remove<C: Context>(ctx: &mut C, cd: &mut Vec<Movement>, node: *const Vnode<C>) {
        for child in get_children(node) {
            cd.push(FirstChild);
            remove(ctx, cd, child);
            cd.clear();
        }
        unsafe { &mut *(*node).t }.unmount(ctx, cd);
    }

    fn diff_node<C: Context>(
        ctx: &mut C,
        cd: &mut Vec<Movement>,
        p: *const Vnode<C>,
        n: *const Vnode<C>,
    ) {
        match unsafe { (p.as_ref(), n.as_ref()) } {
            (None, None) => {}
            (Some(node), None) => {
                remove(ctx, cd, node);
            }
            (None, Some(node)) => {
                insert(ctx, cd, node);
            }
            (Some(old), Some(new)) => {
                if unsafe { mem::transmute::<_, raw::TraitObject>(old.t) }.vtable
                    != unsafe { mem::transmute::<_, raw::TraitObject>(new.t) }.vtable
                {
                    // Differing types; tear down and rebuild
                    let next = match *cd.last().unwrap() {
                        Movement::NextSibling(i) => Movement::NthChild(i),
                        movement => movement,
                    };
                    remove(ctx, cd, old);
                    cd.clear();
                    cd.push(next);
                    insert(ctx, cd, new);
                } else {
                    // Diff attributes
                    unsafe { &mut *new.t }.update_attributes(
                        ctx,
                        cd,
                        &mut new.attributes.iter().filter(|(key, val)| {
                            old.attributes
                                .get(key)
                                .map_or(true, |ref other| val != other)
                        }),
                        &mut old
                            .attributes
                            .iter()
                            .filter(|(key, _)| !new.attributes.contains_key(key)),
                        unsafe { mem::transmute::<_, raw::TraitObject>(old.t).data.as_mut() },
                    );
                    cd.clear();

                    // Diff children
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

                    let mut first = true;
                    for action in diff(&get_keys(&pchildren), &get_keys(&nchildren)) {
                        match action {
                            // Removals are always done first
                            Remove(i) => {
                                cd.push(Movement::NthChild(i));
                                remove(ctx, cd, pchildren[i]);
                                cd.clear();
                            }
                            Insert(i) => {
                                cd.push(if first { FirstChild } else { NextSibling(i) });
                                first = false;
                                insert(ctx, cd, nchildren[i]);
                                cd.clear();
                            }
                            Move(i, j, old_index) => {
                                cd.push(if first { FirstChild } else { NextSibling(j) });
                                first = false;
                                ctx.move_node(cd, i);
                                cd.clear();
                                diff_node(ctx, cd, pchildren[old_index], nchildren[j]);
                            }
                            Update(i, j) => {
                                cd.push(if first { FirstChild } else { NextSibling(i) });
                                first = false;
                                diff_node(ctx, cd, pchildren[j], nchildren[i]);
                            }
                        }
                    }
                    // If first is false we are in child land
                    if !first {
                        cd.push(Movement::Parent);
                    }
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
    );
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
