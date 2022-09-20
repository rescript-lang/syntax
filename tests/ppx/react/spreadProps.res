@@jsxConfig({version:4, mode: "classic"})
let c0 = <A x="x" {...p} />

// ignore second one
let c0 = <A x="x" {...p0} {...p1} />

// only spread props
let c1 = <A {...p} />

// reversed order
let c2 = <A {...p} x="x" />

@@jsxConfig({version:4, mode: "automatic"})
let c0 = <A x="x" {...p} />

// ignore second one
let c0 = <A x="x" {...p0} {...p1} />

// only spread props
let c1 = <A {...p} />

// reversed order
let c2 = <A {...p} x="x" />
