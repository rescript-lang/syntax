module Parens = {
  @react.component
  let make = (~id=?) => {
    <div
      id=?{
        let _ = ()
        id->Option.map(x => x)
      }
    />
  }
}

module WithoutParens = {
  @react.component
  let make = (~id=?) => {
    <div id=?{id->Option.map(x => x)} />
  }
}
