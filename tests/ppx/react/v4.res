// Component with type constraint
@react.component
let make = (~x: string, ~y: string) => React.string(x ++ y)

module AnotherName = {
  // Component with another name than "make"
  @react.component
  let anotherName = (~x) => React.string(x)
}

module Rec = {
  @react.component
  let rec make = () => {
    make({}:props)
  }
}

module Rec1 = {
  @react.component
  let rec make = () => {
    React.null
  }
}

module Rec2 = {
  @react.component
  let rec make = () => {
    mm(({}: props))
  }
  and mm = (x) => make(x)
}
