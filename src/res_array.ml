(* Hopefully these gradually disappear one day when we switch more and more stuff to array *)
(* These are still needed for a while since lots of AST payloads are lists *)

let mapListi f l =
  match l with
  | [] -> [||]
  | head::rest as l ->
    let result = (Array.make [@doesNotRaise]) (List.length l) (f 0 head) in
    let rec loop lst i =
      match lst with
      | [] -> result
      | head::rest ->
        Array.unsafe_set result i (f i head);
        loop rest (i + 1)
    in
    loop rest 1

let mapList f l =
  match l with
  | [] -> [||]
  | head::rest as l ->
    let result = (Array.make [@doesNotRaise]) (List.length l) (f head) in
    let rec loop lst i =
      match lst with
      | [] -> result
      | head::rest ->
        Array.unsafe_set result i (f head);
        loop rest (i + 1)
    in
    loop rest 1

let appendToList arr lst =
  let rec loop i acc =
    if i < 0 then acc
    else loop (i - 1) ((Array.unsafe_get arr i)::acc)
  in
  loop (Array.length arr - 1) lst
