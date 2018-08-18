# vfin

vfin is a GUI framework agnostic virtual DOM library.
Due to the small number of assumptions made about the underlying framework
the design is different from other VDOM libraries.
While fully functional, the interface is not yet stable.

See the [web DOM example](vfin-dom/src/lib.rs) for usage.

## The `html!` macro

`Vtree` defines simple stack operations, `create_element` and `pop_parent`,
unlike in the JS-world where one would typically nest function calls instead.
The `html!` macro is sugar for creating trees.

```rust
fn render(app: Rc<RefCell<App>>) -> Vtree<DomContext> {
    use Attribute::*;
    let mut vtree = Vtree::new();
    html!{vtree,
        <Div>
            <Div Text=state.count, />
            // Inline code-block
            { render_part(&mut vtree); }
            <Button Text="Increment", OnClick={
                let app = Rc::clone(&app);
                move || {
                    dispatch(&app, Action::IncrementCount);
                }
            }, />
            <Div>
            {
                for i in 0..4 {
                    let i = if state.count < -1 { 5 - i } else { i };
                    // Resume adding nodes
                    html!{vtree, <Div key=i, Text=i, />}
                }
            }
            </Div>
        </Div>
    }
    vtree
}
```

Here `Div` and `Button` are trait objects that contain procedures for
un-/mounting and updating attributes for the respective types.

`DomContext` is a DOM backend specific type that implements the `Context` trait
and is responsible for actually carrying out the patching.

The function `patch` takes a `Context` and the previous `Vtree` and a reference
to the new `Vtree` and performs the diffing.

## vfin-dom

vfin-dom is a vfin backend for the web.

### Disadvantages compared to Yew or similiar

- Potentially larger generated binaries. Each tag needs its own trait
  implementation.
- No batteries included. This is only a virtual DOM framework and not a general
  application framework. (Though I would consider it an advantage. You are free
to use whatever state store you wish)

## Roadmap

- [x] Working example.
- [x] Only send delta movements to context.
- [x] Recursive unmounting. Otherwise you'd get memory leaks in non-garbage
  collected backends that don't store destructor addresses.
- [x] Stateful virtual nodes.
- [ ] New more performant diffing algorithm, using LIS or similar. Currently
  Heckel diff is used, which is still `O(n)`.
- [ ] Sub-tree rendering. Possible without API breaking changes.
- [ ] String keys. Currently only integer keys are supported.

