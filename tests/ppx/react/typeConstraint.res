@@jsxConfig({version: 3})

@react.component
let make:
  type a. (~a: a, ~b: a, a) => React.element =
  (~a, ~b, _) => <div />

@@jsxConfig({version: 4, mode: "classic"})

@react.component
let make:
  type a. (~a: a, ~b: a, a) => React.element =
  (~a, ~b, _) => <div />

@@jsxConfig({version: 4, mode: "automatic"})

@react.component
let make:
  type a. (~a: a, ~b: a, a) => React.element =
  (~a, ~b, _) => <div />
