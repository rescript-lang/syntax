**Abbreviation**

The placement of `@react.component` is an abbreviation as described below.

**Normal Case**

```rescript
@react.component
let make = (~x, ~y, ~z) => body

// is an abbreviation for

let make = @react.component (~x, ~y, ~z) => body
```

**Forward Ref**

```rescript
@react.component
let make = React.forwardRef((~x, ~y, ref) => body)

// is an abbreviation for

let make = React.forwardRef({
  let fn =
    @react.component (~x, ~y, ~ref=?) => {
      let ref = ref->Js.Nullable.fromOption
      body
    }
  (props, ref) => fn({...props, ref: {ref->Js.Nullable.toOption}})
})
```

**Conversion**

Conversion applies to an arrow function definition where all the arguments are labelled.
It produces a type definition and a new function.

**Definition**

```rescript
@react.component (~x, ~y=3+x, ?z) => body

// is converted to

type props<'x, 'y, 'z> = {x: 'x, @optional y: 'y, @optional z: 'z, @optional key: string}

({x, y, z}: props<_>) => {
  let y = switch props.y {
  | None => 3 + x
  | Some(y) => y
  }
  body
}
```

**Application**

```rescript
<Comp x>
// is converted to
React.createElement(Comp.make, {x})

<Comp x y=7 ?z>
// is converted to
React.createElement(Comp.make, {x, y:7, @optional z})

<Comp x key="7">
// is converted to
React.createElement(Comp.make, {x, key: "7"})
```

**The new JSX transform**

The JSX PPX V4 supports [the new JSX transform](https://reactjs.org/blog/2020/09/22/introducing-the-new-jsx-transform.html) of React.js.

It affects only the application.

```rescript
<Comp x>
// is converted to
Js.React.jsx(Comp.make, {x})
```

```rescript
<div name="div" />
// is converted to
Js.React.jsxDom("div", { name: "div" })
```

The props type of dom elements, e.g. `div`, is inferred to `Js.React.domProps`.

```rescript
type domProps = {
  key?: string,
  id?: string,
  ...
}
```

**Interface And External**

```rescript
@react.component (~x: int, ~y: int=?, ~z: int=?) => React.element

// is converted to

type props<'x, 'y, 'z> = {x: 'x, @optional y: 'y, @optional z: 'z}

props<int, int, int> => React.element
```

Since an external is a function declaration, it follows the same rule.

**Component Name**

Use the V3 convention for names, and make sure the generated
function has the name of the enclosing module/file.

**Fragment**

```rescript
<> comp1 comp2 comp3 </>

// is converted to

ReactDOMRe.createElement(ReasonReact.fragment, [comp1, comp2, comp3])
```
