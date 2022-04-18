// #301 Improve printing of callbacks in first arg position with comments
11->foo(x => {
  let y = 100

  // Do the addition.
  x + y
}, _)

// #463 Cases where comments are lost converting ReasonML to ReScript
f(_
  // This comment vanishes
  => 1);
g(a
  // This comment is moved down
  => 1);
h((~a)
  // This comment works fine
  => 1);
