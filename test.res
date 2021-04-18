type u = unit
let a = (():u)
let b = (():unit)
// type d<'a,'b> = int
// // let c = ():d<'a,'b> => 8
// let a = (():d<int,int>)
// let c = ():[#I] => 9
type d<'a,'b>
let c = ():d<array<int>,'b> => 9
let fn = f => f;
type f = int => unit;
let a = fn(_ => (): f);