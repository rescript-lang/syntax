**JSX V4 Upgrade**

The record-based representation of components introduced by JSX V4 would brings incompatibility with the components which were written basend on V3. Here are the expected cases and shows you how to fix it.

**`makeProps`**

```rescript
// V3
module M = {
  @obj external makeProps: (~msg: 'msg, ~key: string=?, unit) => {"msg": 'msg} = "" // No more makeProps

  let make = (~msg) => {
    <div> {React.string(msg)} </div>
  }
}

// V4
module M = {
  type props<'msg> = {msg: 'msg}
  let make = props => <div> {React.string(props.msg)} </div>
}
```

**`React.Context`**

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

**`React.forwardRef`(Discouraged)**

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
  <div>
    <FancyInput ref=input> // only `ref` is allowed
      <button onClick> {React.string("Click to focus")} </button>
    </FancyInput>
  </div>
}
```
