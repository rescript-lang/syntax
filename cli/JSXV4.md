## Introduction

JSX V4, supported in the compiler version introduces a new idiomatic record-based representation of components which is incompatible with V3. Because of this, either the entire project or dependencies need to be compiled in V4 mode, or some compatibility features need to be used to mix V3 and V4 in the same project.
The V4 representation is part of the spec, so `@react.component` is effectively just an abbreviation for code that can be writtend by hand.

## Turn On V4

To build an entire project in V4 mode, including all its dependencies, use the new `"jsx"` configuration in `bsconfig.json` instead of the old `"reason"`:

```json
"jsx": { "version": 4 }
```

> Note that JSX V4 requires the rescript compiler 10.1 or higher, and `rescript-react` version `0.11` or higher. In addition, `react` version `18.2` is required.

## Configuration And Upgrade

### Dependency-level config

Dependencies inherit the `jsx` configuration of the root project. So if the root project uses V4 then the dependencies are built using V4, and the same for V3.
To build certain dependencies in V3 compatibility mode, whatever the version used in the root project, use `"v3-dependencies"` as in the example:

```json
"jsx": {
 "version": 4,
 "v3-dependencies": ["rescript-react-native", "rescript-react-navigation"]
}
```

In V3 compatibility mode, the listed dependencies are built in V3 mode, and in addition `-open ReatcV3` is added to the compiler options, so that the `ReactV3` compatibility module in rescript-react is used.

### Classic and Automatic Mode

Classic mode is the default and generates calls to `React.createElement` just as with V3.

```json
"jsx": {
  "version": 4,
  "mode": "classic"
}
```

Automatic mode is an experimental mode that generate calls to `_jsx` functions (similar to TypeScript's `react-jsx` mode)

```json
"jsx": {
  "version": 4,
  "mode": "automatic"
}
```

### File-level config

The top-level attribute `@@jsxConfig` is used to update the `jsx` config for the rest of the file (or until the next config update). Only the values mentioned are updated, the others are left unchanged.

```rescript
@@jsxConfig({ version: 4, mode: "automatic" })

module Wrapper = {
  module R1 = {
    @react.component  // V4 and new _jsx transform
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

### Migration of V3 components that depend on the internal representation

Some components in existing projects are written in a way that is dependent on the V3 internal representation.
Here are a few examples of how to convert them to V4.

#### `makeProps` does not exist in V4

Rewrite this:

```rescript
// V3
module M = {
  @obj external makeProps: (~msg: 'msg, ~key: string=?, unit) => {"msg": 'msg} = "" // No more makeProps

  let make = (~msg) => {
    <div> {React.string(msg)} </div>
  }
}
```

To this:

```rescript
// V4
module M = {
  type props<'msg> = {msg: 'msg}
  let make = props => <div> {React.string(props.msg)} </div>
}
```

#### `React.Context`

```rescript
module Context = {
  let context = React.createContext(() => ())

  module Provider = {
    let provider = React.Context.provider(context)

    @react.component
    let make = (~value, ~children) => {
      React.createElement(provider, {"value": value, "children": children}) // Error
      // Fix : use the record
      React.createElement(provider, {value, children}) // OK
    }
  }
}
```

#### `React.forwardRef`(Discouraged)

```rescript
module FancyInput = {
  @react.component
  let make = React.forwardRef((
    ~className=?,
    ~children,
    ref, // only `ref` is allowed
  ) =>
    <div>
      <input
        type_="text"
        ?className
        ref=?{ref->Js.Nullable.toOption->Belt.Option.map(ReactDOM.Ref.domRef)}
      />
      children
    </div>
  )
}

@react.component
let make = () => {
  let input = React.useRef(Js.Nullable.null)

  <div>
    <FancyInput ref=input> // only `ref` is allowed
      <button onClick> {React.string("Click to focus")} </button>
    </FancyInput>
  </div>
}
```

## V4 Spec

This is the specification that decribes how the JSX V4 transformation works.

### Abbreviation

The placement of `@react.component` is an abbreviation as described below.

### Normal Case

```rescript
@react.component
let make = (~x, ~y, ~z) => body

// is an abbreviation for

let make = @react.component (~x, ~y, ~z) => body
```

### Forward Ref

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

### Component Definition

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

### Component Application

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

### New experimental automatic mode

The V4 ppx supports [the new jsx transform](https://reactjs.org/blog/2020/09/22/introducing-the-new-jsx-transform.html) of React.js.

The jsx transform only affects component application, but not the definition.

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

### Interface And External

```rescript
@react.component (~x: int, ~y: int=?, ~z: int=?) => React.element

// is converted to

type props<'x, 'y, 'z> = {x: 'x, y?: 'y, z?: 'z}

props<int, int, int> => React.element
```

Since an external is a function declaration, it follows the same rule.

### Component Name

Use the V3 convention for names, and make sure the generated
function has the name of the enclosing module/file.

### Fragments

```rescript
<> comp1 comp2 comp3 </>

// is converted to

// v4
ReactDOMRe.createElement(ReasonReact.fragment, [comp1, comp2, comp3])

// v4 @ new jsx transform
React.jsxs(React.jsxFragment, {children: [comp1, comp2, comp3]})
```

### Spread props

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
