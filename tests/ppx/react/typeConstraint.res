@@jsxConfig({version: 3})

module V3 = {
  @react.component
  let make:
    type a. (~a: a, ~b: a, a) => React.element =
    (~a, ~b, _) => <div />
}

@@jsxConfig({version: 4, mode: "classic"})

module V4C = {
  @react.component
  let make:
    type a. (~a: a, ~b: a, a) => React.element =
    (~a, ~b, _) => <div />
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4A = {
  @react.component
  let make:
    type a. (~a: a, ~b: a, a) => React.element =
    (~a, ~b, _) => <div />
}
