module MiniBuffer = Res_minibuffer

type mode = Break | Flat

type lineStyle =
  | Classic (* fits? -> replace with space *)
  | Soft (* fits? -> replaced with nothing *)
  | Hard (* always included, forces breaks in parents *)
   (* always included, forces breaks in parents, but doesn't increase indentation
    use case: template literals, multiline string content *)
  | Literal

type t =
  | Nil
  | Text of string
  | Concat of t list
  | Indent of t
  | IfBreaks of {yes: t; no: t; mutable broken: bool} (* when broken is true, treat as the yes branch *)
  | LineSuffix of t
  | LineBreak of lineStyle
  | Group of {mutable shouldBreak: bool; doc: t}
  | CustomLayout of t list
  | BreakParent

let nil = Nil
let line = LineBreak Classic
let hardLine = LineBreak Hard
let softLine = LineBreak Soft
let literalLine = LineBreak Literal
let text s = Text s

(* Optimization. We eagerly collapse and reduce whatever allocation we can *)
let rec _concat acc l =
  match l with
  | Text s1 :: Text s2 :: rest -> Text (s1 ^ s2) :: _concat acc rest
  | Nil :: rest -> _concat acc rest
  | Concat l2 :: rest -> _concat (_concat acc rest) l2 (* notice the order here *)
  | x :: rest ->
    let rest1 = _concat acc rest in
    if rest1 == rest then l else x :: rest1
  | [] -> acc

let concat l = Concat(_concat [] l)

let indent d = Indent d
let ifBreaks t f = IfBreaks {yes = t; no = f; broken = false}
let lineSuffix d = LineSuffix d
let group d = Group {shouldBreak = false; doc = d}
let breakableGroup ~forceBreak d = Group {shouldBreak = forceBreak; doc = d}
let customLayout gs = CustomLayout gs
let breakParent = BreakParent

let space = Text " "
let comma = Text ","
let dot = Text "."
let dotdot = Text ".."
let dotdotdot = Text "..."
let lessThan = Text "<"
let greaterThan = Text ">"
let lbrace = Text "{"
let rbrace = Text "}"
let lparen = Text "("
let rparen = Text ")"
let lbracket = Text "["
let rbracket = Text "]"
let question = Text "?"
let tilde = Text "~"
let equal = Text "="
let trailingComma = ifBreaks comma nil
let doubleQuote = Text "\""

let propagateForcedBreaks doc =
  let rec walk doc = match doc with
  | Text _ | Nil | LineSuffix _ ->
    false
  | BreakParent ->
    true
  | LineBreak (Hard | Literal) ->
    true
  | LineBreak (Classic | Soft) ->
    false
  | Indent children ->
    let childForcesBreak = walk children in
    childForcesBreak
  | IfBreaks ({yes = trueDoc; no = falseDoc} as ib) ->
    let falseForceBreak = walk falseDoc in
    if falseForceBreak then
      let _ = walk trueDoc in
      ib.broken <- true;
      true
    else
      let forceBreak = walk trueDoc in
      forceBreak
  | Group ({shouldBreak = forceBreak; doc = children} as gr) ->
    let childForcesBreak = walk children in
    let shouldBreak = forceBreak || childForcesBreak in
    gr.shouldBreak <- shouldBreak;
    shouldBreak
  | Concat children ->
    List.fold_left (fun forceBreak child ->
      let childForcesBreak = walk child in
      forceBreak || childForcesBreak
    ) false children
  | CustomLayout children ->
    (* When using CustomLayout, we don't want to propagate forced breaks
     * from the children up. By definition it picks the first layout that fits
     * otherwise it takes the last of the list.
     * However we do want to propagate forced breaks in the sublayouts. They
     * might need to be broken. We just don't propagate them any higher here *)
    let _ = walk (Concat children) in
    false
  in
  let _ = walk doc in
  ()

(* See documentation in interface file *)
let rec willBreak doc = match doc with
  | LineBreak (Hard | Literal) | BreakParent | Group {shouldBreak = true} -> true
  | Group {doc} | Indent doc | CustomLayout (doc::_) -> willBreak doc
  | Concat docs -> List.exists willBreak docs
  | IfBreaks {yes; no} -> willBreak yes || willBreak no
  | _ -> false

let join ~sep docs =
  let rec loop acc sep docs =
    match docs with
    | [] -> List.rev acc
    | [x] -> List.rev (x::acc)
    | x::xs -> loop (sep::x::acc) sep xs
  in
  concat(loop [] sep docs)

type slot = {mutable ind:int; mutable mode:mode; mutable doc:t}

module St = struct

  type t = {mutable pool: slot array; mutable maxSize:int; mutable first:int}
  let maxSize = 500
  let dummySlot = {ind=1; mode=Break; doc=Nil}

  let createWithSize size = {maxSize=size; pool = Array.make size dummySlot; first=size}

  let extend st =
    let oldSize = st.maxSize in
    let newSize = oldSize * 2 in
    let oldPool = st.pool in
    let newPool = Array.make newSize dummySlot in
    for i = 0 to oldSize-1 do
      newPool.(oldSize+i) <- oldPool.(i)
    done;
    st.first <- st.first + oldSize;
    st.maxSize <- newSize;
    st.pool <- newPool

  let _ = extend
  let create () = createWithSize maxSize

  let isEmpty s = s.first = s.maxSize

  let getHeadUnsafe s =
    assert (s.first < s.maxSize);
    let slot = s.pool.(s.first) in
    s.first <- s.first + 1;
    slot

  let add ind mode doc s =
    if (s.first <= 0) then extend s;
    s.first <- s.first -1;
    let slot = s.pool.(s.first) in
    if slot == dummySlot
    then (
      let newSlot = {ind; mode; doc} in
      s.pool.(s.first) <- newSlot
    )
    else (
    slot.ind <- ind;
    slot.mode <- mode;
    slot.doc <- doc
    )
  let getLength s = s.maxSize - s.first

  let getNthUnsafe n s = s.pool.(n+s.first)
end

let fits w ind mode doc stack =
  let width = ref w in
  let result = ref None in

  let rec calculate indent mode doc =
    match mode, doc with
    | _ when result.contents != None -> ()
    | _ when width.contents < 0 -> result := Some false
    | _, Nil
    | _, LineSuffix _
    | _, BreakParent -> ()
    | _, Text txt -> width := width.contents - (String.length txt)
    | _, Indent doc -> calculate (indent + 2) mode doc
    | Flat, LineBreak Hard
    | Flat, LineBreak Literal -> result := Some true
    | Flat, LineBreak Classic -> width := width.contents - 1
    | Flat, LineBreak Soft -> ()
    | Break, LineBreak _ -> result := Some true
    | _, Group {shouldBreak = true; doc} -> calculate indent Break doc
    | _, Group {doc} -> calculate indent mode doc
    | _, IfBreaks {yes = breakDoc; broken = true} -> calculate indent mode breakDoc
    | Break, IfBreaks {yes = breakDoc} -> calculate indent mode breakDoc
    | Flat, IfBreaks {no = flatDoc} -> calculate indent mode flatDoc
    | _, Concat docs -> calculateConcat indent mode docs
    | _, CustomLayout (hd::_) ->
      (* TODO: if we have nested custom layouts, what we should do here? *)
        calculate indent mode hd
    | _, CustomLayout [] -> ()
  and calculateConcat indent mode docs =
    if result.contents == None then (
      match docs with
      | [] -> ()
      | doc::rest ->
        calculate indent mode doc;
        calculateConcat indent mode rest
    )
  in
  let len = St.getLength(stack) in
  let rec calculateAll n =
    match result.contents with
    | Some r -> r
    | None when n = len -> !width >= 0
    | None ->
      let {ind; mode; doc} = St.getNthUnsafe n stack in
      calculate ind mode doc;
      calculateAll (n+1)
  in
  calculate ind mode doc;
  calculateAll 0

let toString ~width doc =
  propagateForcedBreaks doc;
  let buffer = MiniBuffer.create 1000 in

  let rec addSuffices lineSuffices stack = match lineSuffices with
    | [] -> ()
    | (ind, mode, doc)::rest ->
      St.add ind mode doc stack;
      addSuffices rest stack
  in

  let rec addDocs ind mode docs stack = match docs with
    | [] -> ()
    | doc::rest ->
      addDocs ind mode rest stack;
      St.add ind mode doc stack
  in

  let rec process ~pos lineSuffices stack =
    match St.isEmpty stack with
    | false ->
      let {ind; mode; doc} = St.getHeadUnsafe stack in
      begin match doc with
      | Nil | BreakParent ->
        process ~pos lineSuffices stack
      | Text txt ->
        MiniBuffer.add_string buffer txt;
        process ~pos:(String.length txt + pos) lineSuffices stack
      | LineSuffix doc ->
        process ~pos ((ind, mode, doc)::lineSuffices) stack
      | Concat docs ->
        addDocs ind mode docs stack;
        process ~pos lineSuffices stack
      | Indent doc ->
        St.add (ind + 2) mode doc stack;
        process ~pos lineSuffices stack
      | IfBreaks {yes = breakDoc; broken = true} ->
        St.add ind mode breakDoc stack;
        process ~pos lineSuffices stack
      | IfBreaks {yes = breakDoc; no = flatDoc} ->
        if mode = Break then
          let () = St.add ind mode breakDoc stack in
          process ~pos lineSuffices stack
        else
          let () = St.add ind mode flatDoc stack in
          process ~pos lineSuffices stack
      | LineBreak lineStyle  ->
        if mode = Break then (
          begin match lineSuffices with
          | [] ->
            if lineStyle = Literal then (
              MiniBuffer.add_char buffer '\n';
              process ~pos:0 [] stack
            ) else (
              MiniBuffer.flush_newline buffer;
              MiniBuffer.add_string buffer (String.make ind ' ' [@doesNotRaise]);
              process ~pos:ind [] stack
            )
          | _docs ->
            St.add ind mode doc stack;
            addSuffices lineSuffices stack;
            process ~pos:ind [] stack
          end
        ) else (* mode = Flat *) (
          let pos = match lineStyle with
          | Classic -> MiniBuffer.add_string buffer " "; pos + 1
          | Hard -> MiniBuffer.flush_newline buffer; 0
          | Literal -> MiniBuffer.add_char buffer '\n'; 0
          | Soft -> pos
          in
          process ~pos lineSuffices stack
        )
      | Group {shouldBreak; doc} ->
        if shouldBreak || not (fits (width - pos) ind Flat doc stack) then
          let () = St.add ind Break doc stack in
          process ~pos lineSuffices stack
        else
          let () = St.add ind Flat doc stack in
          process ~pos lineSuffices stack
      | CustomLayout docs ->
        let rec findGroupThatFits groups = match groups with
        | [] -> Nil
        | [lastGroup] -> lastGroup
        | doc::docs ->
          if (fits (width - pos) ind Flat doc stack) then
            doc
          else
            findGroupThatFits docs
        in
        let doc = findGroupThatFits docs in
        St.add ind Flat doc stack;
        process ~pos lineSuffices stack
      end
    | true ->
      begin match lineSuffices with
      | [] -> ()
      | suffices ->
        addSuffices suffices stack;
        process ~pos:0 [] stack;
      end
  in
  let stack = St.create() in
  St.add 0 Flat doc stack;
  process ~pos:0 [] stack;


  MiniBuffer.contents buffer


let debug t =
  let rec toDoc = function
    | Nil -> text "nil"
    | BreakParent -> text "breakparent"
    | Text txt -> text ("text(\"" ^ txt ^ "\")")
    | LineSuffix doc -> group(
        concat [
          text "linesuffix(";
          indent (
            concat [line; toDoc doc]
          );
          line;
          text ")"
        ]
      )
    | Concat [] -> text "concat()"
    | Concat docs -> group(
        concat [
          text "concat(";
          indent (
            concat [
              line;
              join ~sep:(concat [text ","; line])
                (List.map toDoc docs) ;
            ]
          );
          line;
          text ")"
        ]
      )
    | CustomLayout docs -> group(
        concat [
          text "customLayout(";
          indent (
            concat [
              line;
              join ~sep:(concat [text ","; line])
                (List.map toDoc docs) ;
            ]
          );
          line;
          text ")"
        ]
      )
    | Indent doc ->
        concat [
          text "indent(";
          softLine;
          toDoc doc;
          softLine;
          text ")";
        ]
    | IfBreaks {yes = trueDoc; broken = true} -> toDoc trueDoc
    | IfBreaks {yes = trueDoc; no = falseDoc} ->
      group(
        concat [
          text "ifBreaks(";
          indent (
            concat [
              line;
              toDoc trueDoc;
              concat [text ",";  line];
              toDoc falseDoc;
            ]
          );
          line;
          text ")"
        ]
      )
    | LineBreak break ->
      let breakTxt = match break with
        | Classic -> "Classic"
        | Soft -> "Soft"
        | Hard -> "Hard"
        | Literal -> "Liteal"
      in
      text ("LineBreak(" ^ breakTxt ^ ")")
    | Group {shouldBreak; doc} ->
      group(
        concat [
          text "Group(";
          indent (
            concat [
              line;
              text ("{shouldBreak: " ^ (string_of_bool shouldBreak) ^ "}");
              concat [text ",";  line];
              toDoc doc;
            ]
          );
          line;
          text ")"
        ]
      )
  in
  let doc = toDoc t in
  toString ~width:10 doc |> print_endline
  [@@live]
