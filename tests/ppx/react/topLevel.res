@@jsxConfig({version: 3})

@react.component
let make = (~a, ~b, _) => {
  Js.log("This function should be named 'TopLevel.react'")
  <div />
}

@@jsxConfig({version: 4, mode: "classic"})

@react.component
let make = (~a, ~b, _) => {
  Js.log("This function should be named 'TopLevel.react'")
  <div />
}

@@jsxConfig({version: 4, mode: "automatic"})

@react.component
let make = (~a, ~b, _) => {
  Js.log("This function should be named 'TopLevel.react'")
  <div />
}
