(* Interface to print source code from different languages to res.
 * Takes a filename called "input" and returns the corresponding formatted res syntax *)
val print: [`jsx | `ml | `res | `refmt of string (* path to refmt *)] -> input: string -> string
