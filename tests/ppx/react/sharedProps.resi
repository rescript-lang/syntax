@@jsxConfig({version:4, mode: "classic"})

module V4C1: {
  @react.component(:sharedProps<string>)
  let make: (~x: string) => React.element
}

module V4C2: {
  @react.component(:sharedProps<'a>)
  let make: (~x: string) => React.element
}

@@jsxConfig({version:4, mode: "automatic"})

module V4A1: {
  @react.component(:sharedProps<string>)
  let make: (~x: string) => React.element
}

module V4A2: {
  @react.component(:sharedProps<'a>)
  let make: (~x: string) => React.element
}