# vfin

vfin is a GUI framework agnostic virtual DOM library.
Due to the small number of assumptions made about the underlying framework
the design is different from other VDOM libraries.
While fully functional, the interface is not yet stable
nor are all performance improvements implemented.

See the [web DOM example](vfin-dom/src/lib.rs) for usage.

## The `html!` macro

`Vtree` defines simple stack operations, `create_element` and `pop_parent`,
unlike in the JS-world where one would typically nest function calls instead.

```rust
fn render(app: Rc<RefCell<App>>) -> Vtree<DomContext> {
    use Attribute::*;
    let mut vtree = Vtree::new();
    html!{vtree,
        <DIV>
            <DIV Text=state.count, />
            // Inline code-block
            { render_part(&mut vtree); }
            <BUTTON Text="Increment", OnClick={
                let app = Rc::clone(&app);
                move || {
                    dispatch(&app, Action::IncrementCount);
                }
            }, />
            <DIV>
            {
                for i in 0..4 {
                    let i = if state.count < -1 { 5 - i } else { i };
                    // Resume pushing html
                    html!{vtree, <DIV key=i, Text=i, />}
                }
            }
            </DIV>
        </DIV>
    }
    vtree
}
```

Here `DIV` and `BUTTON` are references to structs that contain procedures for
un-/mounting and updating attributes for the respective types.

`DomContext` is a DOM backend specific type that implements the `Context` trait
and is responsible for actually carrying out the patching.

The function `patch` takes a `Context` and the previous `Vtree` and a reference
to the new `Vtree` and performs the diffing.

## vfin-dom

vfin-dom is a vfin backend for the web.

### Disadvantages compared to Yew or similiar

- No lazy diffing. The whole tree is descended every render. You could say this
  it's optimized for the worst case. :wink:
- Potentially larger generated binaries. Each tag needs an associated function
  and structure.
- No batteries included. This is only a virtual DOM framework and not a general
  application framework. (Though I would consider it positive. You are free to
  use whatever state store you wish)

## Before 1.0

- [x] Working example.
- [ ] New more performant diffing algorithm, using LIS or similar.
- [ ] String keys. Currently only integer keys are supported.
- [ ] Only send delta movements to context.
- [ ] Recursive unmounting. Otherwise you'd get memory leaks in non-garbage
  collected backends that don't store destructor addresses.

