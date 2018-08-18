#![feature(use_extern_macros)]

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::mem;
use std::rc::Rc;
extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;
extern crate vfin;
use vfin::{html, patch, Context, Movement, Vtree, Vtype};

#[wasm_bindgen]
extern "C" {
    type Node;
    #[wasm_bindgen(module = "./index")]
    fn testing_js(test: Option<Node>) -> Option<Node>;
}

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
    #[wasm_bindgen(method, js_name = insertBefore)]
    fn insert_before(this: &Element, newNode: &Element, referenceNode: Option<&Element>)
        -> Element;
    #[wasm_bindgen(method, getter, js_name = parentElement)]
    fn parent_element(this: &Element) -> Option<Element>;
    #[wasm_bindgen(method, getter)]
    fn children(this: &Element) -> HTMLCollection;
    #[wasm_bindgen(method, js_name = removeChild)]
    fn remove_child(this: &Element, child: &Element) -> Element;
    #[wasm_bindgen(method, js_name = setAttribute)]
    fn set_attribute(this: &Element, name: &str, value: &str);
    #[wasm_bindgen(method, js_name = getAttribute)]
    fn get_attribute(this: &Element, name: &str) -> Option<String>;
    #[wasm_bindgen(method, js_name = addEventListener)]
    fn add_event_listener(this: &Element, typeName: &str, listener: &Closure<Fn()>);
    #[wasm_bindgen(method, js_name = removeEventListener)]
    fn remove_event_listener(this: &Element, typeName: &str, listener: &Closure<Fn()>);
    #[wasm_bindgen(method, setter = textContent)]
    fn set_text_content(this: &Element, s: &str);
    // #[wasm_bindgen(method, js_name = firstElementChild)]
    // fn first_element_child(this: &Element) -> Option<Element>;
    // #[wasm_bindgen(method, js_name = nextElementSibling)]
    // fn next_element_sibling(this: &Element) -> Option<Element>;

    type HTMLCollection;
    #[wasm_bindgen(method)]
    fn item(this: &HTMLCollection, index: usize) -> Option<Element>;
    #[wasm_bindgen(method, getter, js_name = length)]
    fn len(this: &HTMLCollection) -> usize;

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

struct DomStorage {
    buf: Vec<ElementStorage>,
    free_ids: Vec<usize>,
}

impl DomStorage {
    fn new() -> Self {
        DomStorage {
            buf: Vec::new(),
            free_ids: Vec::new(),
        }
    }
}

pub struct DomContext {
    storage: DomStorage,
    cursor: Option<Element>,
}

impl DomContext {
    fn new() -> Self {
        DomContext {
            storage: DomStorage::new(),
            cursor: Some(document.body()),
        }
    }

    /// Returns the parent of the cursor.
    fn set_cursor(&mut self, cd: &[Movement]) -> Element {
        if let Some((last, cd)) = cd.split_last() {
            for movement in cd {
                self.cursor = {
                    let cursor = self.cursor.as_ref().unwrap();
                    match *movement {
                        Movement::Parent => cursor.parent_element(),
                        Movement::NthChild(i) => cursor.children().item(i),
                        // Movement::FirstChild => cursor.first_element_child(),
                        // Movement::NextSibling(_) => cursor.next_element_sibling(),
                        Movement::FirstChild => cursor.children().item(0),
                        Movement::NextSibling(i) => {
                            cursor.parent_element().unwrap().children().item(i)
                        }
                    }
                };
            }
            match *last {
                Movement::Parent => {
                    self.cursor = self.cursor.as_ref().unwrap().parent_element();
                    self.cursor.as_ref().unwrap().parent_element()
                }
                Movement::NthChild(i) => {
                    let next = self.cursor.as_ref().unwrap().children().item(i);
                    mem::replace(&mut self.cursor, next)
                }
                Movement::FirstChild => {
                    // let next = self.cursor.as_ref().unwrap().first_element_child();
                    let next = self.cursor.as_ref().unwrap().children().item(0);
                    mem::replace(&mut self.cursor, next)
                }
                Movement::NextSibling(i) => {
                    // let next = self.cursor.as_ref().unwrap().next_element_sibling();
                    let next = self
                        .cursor
                        .as_ref()
                        .unwrap()
                        .parent_element()
                        .unwrap()
                        .children()
                        .item(i);
                    mem::replace(&mut self.cursor, next)
                        .unwrap()
                        .parent_element()
                }
            }.unwrap()
        } else {
            self.cursor.as_ref().unwrap().parent_element().unwrap()
        }
    }

    fn insert(&mut self, cd: &[Movement], element: &Element) {
        let parent = self.set_cursor(cd);
        self.cursor = Some(parent.insert_before(element, self.cursor.as_ref()));
    }

    fn remove(&mut self, cd: &[Movement]) {
        let parent = self.set_cursor(cd);
        {
            let cursor = self.cursor.as_ref().unwrap();
            if let Some(id) = cursor.get_attribute("data-id") {
                // Add element storage id to free list
                self.storage.free_ids.push(id.parse().unwrap());
            }
            parent.remove_child(cursor);
        }
        self.cursor = Some(parent);
    }

    fn update_attributes(
        &mut self,
        cd: &[Movement],
        set: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
        remove: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
    ) {
        self.set_cursor(cd);
        let cursor = self.cursor.as_ref().unwrap();

        // Facilitate lazy acquisition of element storage
        let mut cached_id = None;
        fn get_storage<'a>(
            storage: &'a mut DomStorage,
            element: &Element,
            cached_id: &mut Option<usize>,
        ) -> &'a mut ElementStorage {
            let id = *cached_id.get_or_insert_with(|| {
                if let Some(id) = element.get_attribute("data-id") {
                    // Already has storage
                    id.parse().unwrap()
                } else {
                    let id = storage.free_ids.pop().unwrap_or_else(|| {
                        let id = storage.buf.len();
                        storage.buf.push(Default::default());
                        id
                    });
                    element.set_attribute("data-id", &id.to_string());
                    id
                }
            });
            &mut storage.buf[id]
        }

        for (key, _) in remove {
            match key {
                Attribute::Text => {}
                Attribute::OnClick => {
                    cursor.remove_event_listener(
                        "click",
                        &get_storage(&mut self.storage, cursor, &mut cached_id)
                            .listeners
                            .remove(&Attribute::OnClick)
                            .unwrap(),
                    );
                }
            }
        }

        for (key, val) in set {
            match key {
                Attribute::Text => cursor.set_text_content(val.into()),
                Attribute::OnClick => {
                    if let AttributeValue::Callback(test) = val {
                        let closure = test.0.borrow_mut().take().unwrap();
                        cursor.add_event_listener("click", &closure);
                        let storage = get_storage(&mut self.storage, cursor, &mut cached_id);
                        if let Some(old) = storage.listeners.insert(Attribute::OnClick, closure) {
                            cursor.remove_event_listener("click", &old);
                        }
                    } else {
                        panic!();
                    }
                }
            }
        }
    }
}

impl Context for DomContext {
    type Attribute = Attribute;
    type AttributeValue = AttributeValue;

    fn move_node(&mut self, cd: &[Movement], from: usize) {
        let parent = self.set_cursor(cd);
        let element = parent.children().item(from).unwrap();
        self.cursor = Some(parent.insert_before(&element, self.cursor.as_ref()));
    }

    fn on_start(&mut self) {
        self.cursor = Some(document.body());
    }

    fn derive_from(&mut self, _cd: &[Movement]) -> Self {
        unimplemented!();
    }
}

pub struct Callback(RefCell<Option<Closure<Fn()>>>);

impl PartialEq for Callback {
    fn eq(&self, _other: &Callback) -> bool {
        false
    }
}
impl Eq for Callback {}
impl std::fmt::Debug for Callback {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Callback")
    }
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Attribute {
    Text,
    OnClick,
}

use Attribute::*;

#[derive(PartialEq, Eq, Debug)]
pub enum AttributeValue {
    Text(Cow<'static, str>),
    Callback(Callback),
}

impl<'a> From<&'a AttributeValue> for &'a str {
    fn from(v: &'a AttributeValue) -> Self {
        match *v {
            AttributeValue::Text(ref s) => s,
            _ => unreachable!(),
        }
    }
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

macro_rules! impl_tags {
    ($($name:ident),*) => {
        $(
            pub struct $name;
            impl Vtype<DomContext> for $name {
                fn mount(
                    &mut self,
                    ctx: &mut DomContext,
                    cd: &[Movement],
                    mut attributes: std::collections::hash_map::Iter<Attribute, AttributeValue>,
                    ) {
                    ctx.insert(cd, &document.create_element(stringify!($name)));
                    ctx.update_attributes(&[], &mut attributes, &mut std::iter::empty());
                }
                fn unmount(&mut self, ctx: &mut DomContext, cd: &[Movement]) {
                    ctx.remove(cd);
                }
                fn update_attributes(
                    &mut self,
                    ctx: &mut DomContext,
                    cd: &[Movement],
                    set: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
                    remove: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
                    _last_props: Option<&mut Self>,
                    ) {
                    ctx.update_attributes(cd, set, remove);
                }
            }
            )*
    };
}

impl_tags!{
    A, Abbr, Acronym, Address, Applet, Area, Article, Aside, Audio, B, Base, Basefont, Bdi,
    Bdo, Big, Blockquote, Body, Br, Button, Canvas, Caption, Center, Cite, Code, Col, Colgroup,
    Datalist, Dd, Del, Details, Dfn, Dialog, Dir, Div, Dl, Dt, Em, Embed, Fieldset,
    Figcaption, Figure, Font, Footer, Form, Frame, Framset, H1, H2, H3, H4, H5, H6, Head,
    Header, Hr, I, Iframe, Img, Input, Ins, Kbd, Keygen, Label, Legend, Li, Link, Main, Map,
    Mark, Menu, Menuitem, Meta, Meter, Nav, Noframes, Noscript, Object, Ol, Optgroup, /* Option, */
    Output, P, Param, Pre, Progress, Q, Rp, Rt, Ruby, S, Samp, Script, Section, Select, Small,
    Source, Span, Strike, Strong, Style, Sub, Summary, Sup, Table, Tbody, Td, Textarea, Tfoot,
    Th, Thead, Time, Title, Tr, Track, Tt, U, Ul, Var, Video, WBR
}

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
        <Div Attribute::Text="Fin katt", />
        {
            if state.count > 0 {
                html!{vtree, <Div Attribute::Text="Count is larger than 2", />}
            }
        }
    }
}

fn render(app: Rc<RefCell<App>>) -> Vtree<DomContext> {
    let state = &app.borrow().state;
    let mut vtree = Vtree::new();
    html!{vtree,
        <Div>
            <Div Text=state.count.to_string(), />
            { render_part(app.clone(), &mut vtree); }
            <Button Attribute::Text="Decrement", Attribute::OnClick={
                let app = Rc::clone(&app);
                move || {
                    dispatch(&app, Action::DecrementCount);
                }
            }, />
            <Button Attribute::Text="Fin knapp", Attribute::OnClick={
                let app = app.clone();
                move || {
                    dispatch(&app, Action::IncrementCount);
                }
            }, />
            <Div>
                <Div Text="Hello there little kid", />
                <Div Text="Instead of component", />
                <C Component{num: if state.count < 5 {54} else {13},}, />
            </Div>
            <Div>
            {
                for i in 0..4 {
                    let i = if state.count < -1 { 5 - i } else { i };
                    html!{vtree, <Div key=i, Text=i, />}
                }
            }
            </Div>
        </Div>
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
  closure.forget();
}*/

fn update(app: Rc<RefCell<App>>) {
    let next = render(app.clone());
    let app = &mut *app.borrow_mut();
    let old = std::mem::replace(&mut app.vdom, next);
    patch(&mut app.ctx, old, &app.vdom);
}

struct Component {
    num: i32,
}

impl Vtype<DomContext> for Component {
    fn mount(
        &mut self,
        ctx: &mut DomContext,
        cd: &[Movement],
        mut _attributes: std::collections::hash_map::Iter<Attribute, AttributeValue>,
    ) {
        let e = document.create_element("span");
        e.set_text_content(&self.num.to_string());
        ctx.insert(cd, &e);
    }
    fn unmount(&mut self, ctx: &mut DomContext, cd: &[Movement]) {
        ctx.remove(cd);
    }
    fn update_attributes(
        &mut self,
        ctx: &mut DomContext,
        cd: &[Movement],
        set: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
        remove: &mut dyn Iterator<Item = (&Attribute, &AttributeValue)>,
        last_props: Option<&mut Self>,
    ) {
        ctx.update_attributes(cd, set, remove);

        if let Some(last_props) = last_props {
            self.num = last_props.num + 1;
            ctx.cursor
                .as_ref()
                .unwrap()
                .set_text_content(&self.num.to_string());
        }
    }
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
