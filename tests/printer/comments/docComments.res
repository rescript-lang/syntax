/*** This is a module comment */

/*** This is another module comment */

/** This is a doc ✅ comment */
let z = 34

@@ns.doc("And this is a ns.doc module annotation")

@ns.doc("And this is a ns.doc ✅ annotation")
let q = 11

/** This
  * is a multi-line
  multiline doc comment
  */
type h = int

/* comment and attribute */
@foo let x = 10

/** doc comment and attribute */
@foo let x = 10

/** doc comment and 3 attributes */
@foo @bar @baz let x = 10

/** doc comment and 0 attributes */
let x = 10