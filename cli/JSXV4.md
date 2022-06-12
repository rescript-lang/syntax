**Abbreviation**
Tha placement of `@react.component` is an abbreviation as described below.

***Normal Case***

```rescript
@react.component
let make = (~x, ~y, ~z) => body

// is an abbreviation for

let make = @react.component (~x, ~y, ~z) => body
```

***Forward Ref***

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

***Definition***

```rescript
@react.component (~x, ~y=3+x, ?z) => body

// is converted to

type props<'x, 'y, 'z> = {x: 'x, @optional y: 'y, @optional z: 'z, @optional key: string}

(props: props<_>) => {
  let x = props.x
  let y = switch props.y { | None => 3+x | Some (y) => y }
  let z = props.z
  React.createElement(body: React.element, props)
}
```

***Application***

```rescript
<Comp x>
// is converted to
Comp.make({x})

<Comp x y=7 ?z>
// is converted to
Comp.make({x, y:7, ~optional z})

<Comp x key="7">
// is converted to
Comp.make({x, key: "7"})
```

***Interface***

```rescript
@react.component
let make: (~x: int, ~y: int=?, ~z: int=?) => React.element

// is converted to

type props<'x, 'y, 'z> = {x: 'x, @optional y: 'y, @optional z: 'z}

let make: (props<int, int, int>) => React.element
```
