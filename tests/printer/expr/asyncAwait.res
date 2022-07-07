let sequentialAwait = async () => {
  let result1 = await paused("first")
  nodeJsAssert.equal(result1, "first")
  
  let result2 = await paused("second")
  nodeJsAssert.equal(result2, "second")
}

let f = async () => ()
let f = async (.) => ()
let f = async f => f()
let f = async (a, b) => a + b
let f = async (. a, b) => a + b


let maybeSomeValue = switch await fetchData(url) {    
| data => Some(data)
| exception JsError(_) => None
}

(await f)(a, b) 
-(await f)
await 1 + await 2

lazy (await f())
assert (await f())

(await f).json()

user.data = await fetch()

<Navbar promise={await gc()}>{await weirdReactSuspenseApi}</Navbar>

let inBinaryExpression = await x->Js.Promise.resolve + 1
let inBinaryExpression = await x->Js.Promise.resolve + await y->Js.Promise.resolve

let () = await (await fetch(url))->(await resolve)
