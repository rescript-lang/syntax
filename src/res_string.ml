let hexTable =
  [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; |]
  [@ocamlformat "disable"]

let convertOctalToHex ~strOctal =
  try
    let intNum = int_of_string strOctal in
    let c1 = Array.get hexTable (intNum lsr 4) in
    let c2 = Array.get hexTable (intNum land 15) in
    "\\x" ^ String.concat "" [String.make 1 c1; String.make 1 c2]
  with Invalid_argument _ | Failure _ -> strOctal

let convertDecEscapes txt =
  let regex = Str.regexp "\\\\[0-9][0-9][0-9]" in
  let convertEscape s =
    let strOctal = Str.string_after (Str.matched_string s) 1 in
    try convertOctalToHex ~strOctal
    with _ -> s (* in theory this should never happen *)
  in
  Str.global_substitute regex convertEscape txt
