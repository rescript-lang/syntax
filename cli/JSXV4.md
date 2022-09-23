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
    @react.component (~x, ~y) => ref => body
  (props, ref) => fn(props, ref)
})
```

**Conversion**

Conversion applies to an arrow function definition where all the arguments are labelled.
It produces a type definition and a new function.

**Definition**

```rescript
@react.component (~x, ~y=3+x, ?z) => body

// is converted to

type props<'x, 'y, 'z> = {x: 'x, y?: 'y, z?: 'z}

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
React.createElement(Comp.make, {x, y:7, ?z})

<Comp x key="7">
// is converted to
React.createElement(Comp.make, Jsx.addKeyProp({x}, "7"))
```

**New "jsx" transform**

The V4 ppx supports [the new "jsx" transform](https://reactjs.org/blog/2020/09/22/introducing-the-new-jsx-transform.html) of React.js.

The "jsx" transform affects component application but not the definition.

```rescript
<Comp x>
// is converted to
React.jsx(Comp.make, {x})
```

```rescript
<div name="div" />
// is converted to
ReactDOM.jsx("div", { name: "div" })
```

The props type of dom elements, e.g. `div`, is inferred to `ReactDOM.domProps`.

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

type props<'x, 'y, 'z> = {x: 'x, y?: 'y, z?: 'z}

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

// v4
ReactDOMRe.createElement(ReasonReact.fragment, [comp1, comp2, comp3])

// v4 @ new jsx transform
React.jsxs(React.jsxFragment, {children: [comp1, comp2, comp3]})
```

**File-level config**

The top-level attribute `@@jsxConfig` is used to update the jsx config for the rest of the file (or until the next config update). Only the values mentioned are updated, the others are left unchanged.

```rescript
@@jsxConfig({ version: 4, mode: "automatic" })

module Wrapper = {
  module R1 = {
    @react.component  // V4 & new jsx transform
    let make = () => body
  }

  @@jsxConfig({ version: 4, mode: "classic" })

  module R2 = {
    @react.component  // V4 with `React.createElement`
    let make = () => body
  }
}

@@jsxConfig({ version: 3 })

@react.component  // V3
let make = () => body
```

**Spread props**

V4 ppx supports the spread props `{...p}`.

```rescript
module A = {
  @react.component
  let make = (~x, ~y) => body
}

let p: A.props<_> = {x: "x", y: "y"}

<A {...p}>
<A {...p} x="X">

// not allowed
<A x="X" {...p}>
<A {...p} {...p1}>
```
