let #shape = x
let #Shape = x
let #\"type" = x
let #\"test 🏚" = x
let #\"Shape✅" = x
let #"1" = x
let #"123" = x
let #"10space" = x

let #space10 = x

let #Shape(\"module", \"ExoticIdent") = x

let #\"type"(\"module", \"ExoticIdent") = x
let #\"Shape🎡"(\"module", \"ExoticIdent") = x

let cmp = (selectedChoice, value) =>
  switch (selectedChoice, value) {
  | (#...a, #...a) => true
  | [#...b, #...b] => true
  | list{#...b, #...b} => true
  | {x: #...c, y: #...c} => true
  | Constructor(#...a, #...a) => true
  | #Constuctor(#...a, #...a) => true
  | #...a as x => true
  | #...a | #...b => true
  | (#...a : typ) => true
  | lazy #...a => true
  | exception #...a => true
  | #"1" => true
  | #"123" => true
  | #"10space" => x
  | #space10 => x
  | _ => false
  }

switch x {
| #a(()) => ()
| #a() => ()
}

switch numericPolyVar {
| #42 => ()
| #3(x, y, z) => Js.log3(x, y, z)
}

let e = #""