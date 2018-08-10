#![feature(use_extern_macros)]

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;
extern crate vfin;
use vfin::{html, patch, Context, Movement, Vtree, Vtype};

#[wasm_bindgen]
extern "C" {
    type HTMLDocument;
    static document: HTMLDocument;
    #[wasm_bindgen(method, js_name = createElement)]
    fn create_element(this: &HTMLDocument, tagName: &str) -> Element;
    #[wasm_bindgen(method, getter)]
    fn body(this: &HTMLDocument) -> Element;
    // #[wasm_bindgen(method, js_name = getElementById)]
    // fn get_element_by_id(this: &HTMLDocument, elementId: &str) -> Element;

    type Element;
    #[wasm_bindgen(method, setter = innerHTML)]
    fn set_inner_html(this: &Element, html: &str);
    #[wasm_bindgen(method, js_name = appendChild)]
    fn append_child(this: &Element, other: Element);
    #[wasm_bindgen(method, js_name = insertBefore)]
    fn insert_before(this: &Element, newNode: Element, referenceNode: Element);
    #[wasm_bindgen(method, getter)]
    fn children(this: &Element) -> HTMLCollection;
    type Node;
    #[wasm_bindgen(method, js_name = removeChild)]
    fn remove_child(this: &Element, child: Element) -> Node;
    #[wasm_bindgen(method, js_name = setAttribute)]
    fn set_attribute(this: &Element, name: &str, value: &str);
    #[wasm_bindgen(method, js_name = getAttribute)]
    fn get_attribute(this: &Element, name: &str) -> String;
    #[wasm_bindgen(method, js_name = hasAttribute)]
    fn has_attribute(this: &Element, name: &str) -> bool;
    #[wasm_bindgen(method, js_name = addEventListener)]
    fn add_event_listener(this: &Element, typeName: &str, listener: &Closure<Fn()>);
    #[wasm_bindgen(method, js_name = removeEventListener)]
    fn remove_event_listener(this: &Element, typeName: &str, listener: &Closure<Fn()>);

    type HTMLCollection;
    #[wasm_bindgen(method)]
    fn item(this: &HTMLCollection, index: usize) -> Element;
    #[wasm_bindgen(method, getter)]
    fn length(this: &HTMLCollection) -> usize;

    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
    // #[wasm_bindgen(js_name = requestAnimationFrame)]
    // fn request_animation_frame(listener: &Closure<Fn()>);

    #[wasm_bindgen(module = "./index")]
    fn exit_with_live_runtime();
}

#[derive(Default)]
struct ElementStorage {
    listeners: BTreeMap<Attribute, Closure<Fn()>>,
}

struct DomContext {
    storage: Vec<ElementStorage>,
    free_ids: Vec<usize>,
}

impl DomContext {
    fn new() -> Self {
        DomContext {
            storage: Vec::new(),
            free_ids: Vec::new(),
        }
    }

    fn get_element(&self, cd: &[Movement]) -> Element {
        cd.iter().fold(document.body(), |acc, x| match x {
            &Movement::NthChild(i) => acc.children().item(i),
            _ => panic!(),
        })
    }

    fn insert(&self, parent: Element, index: usize, element: Element) {
        let children = parent.children();
        let len = children.length();
        if len == 0 {
            log(&format!("appending child"));
            parent.append_child(element);
        } else {
            log(&format!("inserting before"));
            let sibling = children.item(index);
            parent.insert_before(element, sibling);
        }
    }

    fn insert_cd(&self, cd: &[Movement], index: usize, element: Element) {
        let parent = self.get_element(cd);
        self.insert(parent, index, element);
    }
}

impl Context for DomContext {
    type Attribute = Attribute;
    type AttributeValue = AttributeValue;

    fn move_node(&mut self, cd: &[Movement], from: usize, index: usize) {
        let parent = self.get_element(cd);
        let child = parent.children().item(from);
        log("Moving node.");
        self.insert(parent, index, child);
    }

    fn on_start(&mut self) {
        log("Starting render!-------");
    }
}

struct Callback(RefCell<Option<Closure<Fn()>>>);

impl PartialEq for Callback {
    fn eq(&self, _other: &Callback) -> bool {
        false
    }
}
impl Eq for Callback {}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
enum Attribute {
    Text,
    OnClick,
}

use Attribute::*;

#[derive(PartialEq, Eq)]
enum AttributeValue {
    Text(Cow<'static, str>),
    Callback(Callback),
}

impl From<String> for AttributeValue {
    fn from(s: String) -> Self {
        AttributeValue::Text(s.into())
    }
}

impl From<&'static str> for AttributeValue {
    fn from(s: &'static str) -> Self {
        AttributeValue::Text(s.into())
    }
}

impl From<i32> for AttributeValue {
    fn from(i: i32) -> Self {
        AttributeValue::Text(i.to_string().into())
    }
}

impl<F: Fn() + 'static> From<F> for AttributeValue {
    fn from(listener: F) -> Self {
        AttributeValue::Callback(Callback(RefCell::new(Some(Closure::new(listener)))))
    }
}

fn unmount(ctx: &mut DomContext, cd: &[Movement], index: usize) {
    log(&format!("unmounting {}", index));
    let parent = ctx.get_element(cd);
    let child = parent.children().item(index);
    if child.has_attribute("data-id") {
        // Add element storage id to free list
        let id = child.get_attribute("data-id").parse().unwrap();
        ctx.free_ids.push(id);
    }
    parent.remove_child(child);
}

fn update_attributes(
    ctx: &mut DomContext,
    cd: &[Movement],
    index: usize,
    set: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
    remove: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
) {
    log(&format!(
        "update attributes: cd: {:?}, index: {}",
        cd, index
    ));
    let element = ctx.get_element(cd).children().item(index);

    // Facilitate lazy acquisition of element storage
    let mut cached_id = None;
    fn get_storage<'a>(
        ctx: &'a mut DomContext,
        element: &Element,
        cached_id: &mut Option<usize>,
    ) -> &'a mut ElementStorage {
        let id = *cached_id.get_or_insert_with(|| {
            if element.has_attribute("data-id") {
                // Already has storage
                element.get_attribute("data-id").parse().unwrap()
            } else {
                let id = ctx.free_ids.pop().unwrap_or_else(|| {
                    let id = ctx.storage.len();
                    ctx.storage.push(Default::default());
                    id
                });
                element.set_attribute("data-id", &id.to_string());
                id
            }
        });
        &mut ctx.storage[id]
    }

    for (key, _) in remove {
        log(&format!("removing key {:?}", key));
        match key {
            Attribute::Text => {}
            Attribute::OnClick => {
                element.remove_event_listener(
                    "click",
                    &get_storage(ctx, &element, &mut cached_id)
                        .listeners
                        .remove(&Attribute::OnClick)
                        .unwrap(),
                );
            }
        }
    }

    for (key, val) in set {
        match key {
            Attribute::Text => {
                if let AttributeValue::Text(s) = val {
                    element.set_inner_html(s);
                } else {
                    panic!();
                }
            }
            Attribute::OnClick => {
                if let AttributeValue::Callback(test) = val {
                    let closure = test.0.borrow_mut().take().unwrap();
                    element.add_event_listener("click", &closure);
                    let storage = get_storage(ctx, &element, &mut cached_id);
                    if let Some(old) = storage.listeners.insert(Attribute::OnClick, closure) {
                        element.remove_event_listener("click", &old);
                    }
                } else {
                    panic!();
                }
            }
        }
    }
}

const DIV: Vtype<DomContext> = Vtype {
    mount: |ctx, mut attributes, cd, index| {
        let e = document.create_element("div");
        ctx.insert_cd(cd, index, e);
        update_attributes(ctx, cd, index, &mut attributes, &mut std::iter::empty());
    },
    unmount,
    update_attributes,
};

const BUTTON: Vtype<DomContext> = Vtype {
    mount: |ctx, mut attributes, cd, index| {
        let e = document.create_element("button");
        ctx.insert_cd(cd, index, e);
        update_attributes(ctx, cd, index, &mut attributes, &mut std::iter::empty());
    },
    unmount,
    update_attributes,
};

struct State {
    count: i32,
}

impl State {
    fn new() -> Self {
        State { count: 0 }
    }
}

#[derive(Copy, Clone)]
enum Action {
    DecrementCount,
    IncrementCount,
}

fn reduce(state: &mut State, action: Action) {
    match action {
        Action::DecrementCount => {
            state.count -= 1;
        }
        Action::IncrementCount => {
            state.count += 1;
        }
    }
}

fn render_part(app: Rc<RefCell<App>>, vtree: &mut Vtree<DomContext>) {
    let state = &app.borrow().state;
    html!{vtree,
        <DIV key=42, Attribute::Text="Fin katt", />
        {
            if state.count > 2 {
                html!{vtree, <DIV key=69, Attribute::Text="Count is larger than 2", />}
            }
        }
    }
}

fn render(app: Rc<RefCell<App>>) -> Vtree<DomContext> {
    let state = &app.borrow().state;
    let mut vtree = Vtree::new();
    html!{vtree,
        <DIV>
            <DIV>
                <DIV key=1, />
                <DIV key=0, />
            </DIV>
            { render_part(app.clone(), &mut vtree); }
            <DIV Attribute::Text=state.count.to_string(), />
            <BUTTON Attribute::Text="Decrement", Attribute::OnClick={
                let app = Rc::clone(&app);
                move || {
                    dispatch(&app, Action::DecrementCount);
                }
            }, />
            <BUTTON Attribute::Text="Fin knapp", Attribute::OnClick={
                let app = app.clone();
                move || {
                    dispatch(&app, Action::IncrementCount);
                }
            }, />
            <DIV>
            {
                for i in 0..4 {
                    let i = if state.count < -1 { 5 - i } else { i };
                    html!{vtree, <DIV key=i, Text=i, />}
                }
            }
            </DIV>
        </DIV>
    }
    vtree
}

struct App {
    ctx: DomContext,
    state: State,
    vdom: Vtree<DomContext>,
}

// Causes long stack traces - every closure is indirectly created recursively
fn dispatch(app: &Rc<RefCell<App>>, action: Action) {
    reduce(&mut app.borrow_mut().state, action);
    update(Rc::clone(app));
}

/*fn dispatch2(app: &Rc<RefCell<App>>, action: Action) {
    let app = app.clone();
    let closure = Closure::new(move || {
        dispatch2(&app, action);
    });
    request_animation_frame(&closure);
    // TODO
    closure.forget();
}*/

fn update(app: Rc<RefCell<App>>) {
    let next = render(app.clone());
    let app = &mut *app.borrow_mut();
    let old = std::mem::replace(&mut app.vdom, next);
    patch(&mut app.ctx, old, &app.vdom);
}

#[wasm_bindgen]
pub fn run() {
    let app = Rc::new(RefCell::new(App {
        ctx: DomContext::new(),
        state: State::new(),
        vdom: Vtree::new(),
    }));
    update(app);

    exit_with_live_runtime();
    unreachable!();
}
