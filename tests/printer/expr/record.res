let coord = {x: 3.13, y: 3.14}
let record = {firstField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, secondField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, thirdField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer}

let forceBreak = {
  x: Omega.x,
  y: Theta.y
}

let withSpread = {...initialState, time: nextTime,}
let withSpreadAndForceBreak = {
  ...initialState,
  time: nextTime,
}

let withSpreadAndNaturalBreak = {...fields, firstField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, secondField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, thirdField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer}


let x = @attr {x: 1, y: 2}
let x = @attr {...initialState, superLongName: 1, superLongName: 2, superLongName: 5}
let x = @attr {...initialState, superLongName: 1, superLongName: 2, superLongName: 5, superLongName: 20}

// print parens around constrained expr in rhs
let user = {name: (ceo.name: string)}
// braces should be preserved on rhs
let user = {name: {ceo.name}}
let user = {name: {
  ceo.name
}}
// braced + constrained expr
let user = {name: {(ceo.name: string)}}


// Punning
let r = {a} // actually not a record, just an expression in braces
let r = {a: a} // single-element record, not punned
let r = {A.a: a} // single-element record, not punned
let r = {a, b}
let r = {a, b, c: 42}
let r = {A.a, b}
let r = {A.a: a, b}
let r = {a: a, b}
let r = {a, b: b}

// Punning + comments
let r = {
  // a
  a,
  // b
  b,
}
let r = {
  a, // a
  b, // b
}
let r = {
  /* a */
  a,
  /* b */
  b,
}
let r = {
  a /* a */,
  b /* b */,
}
let r = {a /* a */, b /* b */}

let r = {x: @optional None, y: @optional None, z: @optional None}

let z = name => { name : @optional name, x: 3}

let _ = switch z {
  | {x: @optional None, y: @optional None, z: @optional None} => 11
  | {name:  @optional name, x: 3} => 42
}