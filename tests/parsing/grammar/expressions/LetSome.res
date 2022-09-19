let foo = (x, y) => {
  let Some(vx) = x
  let x1 = vx + 1
  let Some({vy, vz: 3}) = y
  let y1 = vy + 1
  Some(x1 + y1)
}
