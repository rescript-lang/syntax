let x = list{}
let x = list{1}
let x = list{1, 2}
let x = list{1, 2, 3}
let x = list{1, 2, ...x, 3, ...x}
let x = Belt.List.concatMany([list{1, 2, ...x}, [list{3, ...x}]])


let x = list{
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
}


let x = list{1, ...otherList}

let x = list{
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  ...superLoooooooooooooooooooooooooooooongListHere,
}

let x = Belt.List.concatMany([
  list{
    superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
    superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
    ...superLoooooooooooooooooooooooooooooongListHere,
  },
  list{
    superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
    ...superLoooooooooooooooooooooooooooooongListHere,
  },
])


let x = list{
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer, ...superLoooooooooooooooooooooooooooooongListHere,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  ...superLoooooooooooooooooooooooooooooongListHere,
}
