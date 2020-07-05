let safeDivideZero = (num, denom) => {
  guard denom !== 0 else {
    0
  }
  num / denom
}
let safeDivideNone = (num, denom) => {
  guard denom !== 0 else {
    None
  }
  Some(num / denom)
}

let safeDivideExn = (num, denom) => {
  guard denom !== 0 else {
    raise(DivisionByZero)
  }
  num / denom
}

let nested = (x,y,z) => {
  guard x else {
    raise(BadX)
  }
  guard y else {
    raise(BadY)
  }
  guard z else {
    raise(BadZ)
  }

  Js.log("Hello")
}

let returningUnit = shouldLog => {
  guard shouldLog
  Js.log("Hello")
}

// NOT advised for use, since this syntax allows you to be flat
// This is really running preconditions before throwing, and handling errors
// on the error cases
// Just here to show if doesn't fail
let nested = (x,y,z) => {
  guard x else {
    guard y else {
      guard z else {
        raise(BadZ)
      }
      raise(BadY)
    }
    raise(BadX)
  }

  Js.log("Hello")
}

// Not recommended, but blocks are still valid
let isValid = (width, height, color, status) => {
  guard {
    let size = width * height;
    size < 100
  } else {
    false
  }

  guard color !== Red else {
    false
  }

  true
}

let guardlet = () => {
  guard let Some(x) = foo() else {
    "return"
  }
  doSomethingWithX(x)

  guard let Some(x) = foo() else {
    raise(FailedToUnwrap)
  }

  guard let Some(x) = foo() else {
    raise(FailedToUnwrap)
  }
  doSomethingWithX(x)
}

// Here down is module-level application
// You'll see that these continually nest - so each guard statement puts the remaining body in the
// then branch
guard {
  let one = x > 10 && n > 2
  let two = "test"
  one + 2 + two
} else {
  "fail"
}
"hello"

guard x > 10 else {
  "fail"
}
"hello"

let foo = x => {
  guard x > 10 else {
    ""
  }
  Js.log(x)
}

guard x > 10
Js.log(x)

// nested
let bar = (a,b,c) => {
  guard a > 10 else {
    "test"
    guard b > 20 else {
      c
    }
    "ok"
  }
  guard c > 2

  "hello"
}

// Maintains attributes
let bar = (@b a,b,c) => {
  @ac
  guard @d a > 10 else {
    @e "test"
    guard b > 20 else {
      c
    }
    "ok"
  }
  @f
  guard c > 2

  @g("hello")
  "ok"
}

// top-level
guard test else {
  ()
}

"hello"
