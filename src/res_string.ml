let hexTable = [|'0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'|]

let convertDecEscapes txt =
  let regex = Str.regexp "\\\\[0-9][0-9][0-9]" in
  let convertEscape s =
    let strNum = Str.string_after (Str.matched_string s) 1 in
    try
      let intNum = int_of_string strNum in
      let c1 = Array.get hexTable (intNum lsr 4) in
      let c2 = Array.get hexTable (intNum land 15) in
      "\\x" ^ String.concat "" [ String.make 1 c1; String.make 1 c2 ]
    with _ -> s (* in theory this should never happen *)
  in
  Str.global_substitute regex convertEscape txt
