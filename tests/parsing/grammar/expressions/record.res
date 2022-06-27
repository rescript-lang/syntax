let r = {a: expr}
let r = {a: expr,} // trailing comma

let r = {Parsetree.pexp_attributes: [], Parsetree.loc: loc}

// punning
let r = {a, b, c} 
let r = {A.a, b}
let r = {A.a, b, C.c}

let r = {Parsetree.pexp_attributes, Parsetree.loc}
// trailing comma
let r = {Parsetree.pexp_attributes, Parsetree.loc,}

// with constraint
let r = {a: (expr : int), b: (x : string)}

// spread
let r = {...expr, pexp_attributes: []}
let r = {...expr, pexp_attributes: [], pexp_loc: loc}
let r = {...expr, pexp_attributes: [],} // trailing comma

// with type constraint on spread
let r = {...make() : myRecord, foo: bar}
let r = {...(make() : myRecord), foo: bar} // parens optional

let r = {x: @optional None, y: @optional None, z: @optional (None:tt)}

let z = name => { name : @optional name, x: 3}

let z = name => { @optional name, x: 3}

let z = name => { name, @optional x }

let _ = switch z {
  | {x: @optional None, y: @optional None, z: @optional (None:tt)} => 11
  | {name:  @optional name, x: 3} => 42
  | {@optional name, x: 3} => 4242
}