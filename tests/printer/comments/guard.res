let safeDivideZero = (num, denom) => {
  guard /* i1 */ denom /* i2 */ !== /* i3 */ 0 /* i4 */ else {
    0 // t5
  }
  guard let /* i6 */ Some(/* i7 */ x/* i8 */ ) /* i9 */ = /* i10 */ foo(/* i11 */ ) /* i12 */ else {
    "return" //t13
  }
  num /* i14 */ / /* i15 */ denom // t16
}
