@@jsxConfig({version:4, mode: "classic"})
// Error: spreadProps should be first in order than other props
// let c0 = <A x="x" {...p} />

// Error: multiple spreadProps not allowed
// let c0 = <A x="x" {...p0} {...p1} />

// only spread props
let c1 = <A {...p} />

// reversed order
let c2 = <A {...p} x="x" />

@@jsxConfig({version:4, mode: "automatic"})
// Error: spreadProps should be first in order than other props
// let c0 = <A x="x" {...p} />

// Error: multiple spreadProps not allowed
// let c0 = <A x="x" {...p0} {...p1} />

// only spread props
let c1 = <A {...p} />

// reversed order
let c2 = <A {...p} x="x" />
