open Parsetree

let rewrite_implementation ~jsxVersion ~jsxModule ~jsxMode
    (code : Parsetree.structure) : Parsetree.structure =
  match (jsxVersion, jsxModule, jsxMode) with
  | 3, _, _ -> Reactjs_jsx_ppx_v3.rewrite_implementation code
  | 4, _, "classic" ->
    Reactjs_jsx_ppx_v4.rewrite_implementation ~jsxMode code
  | 4, _, "automatic" ->
    Reactjs_jsx_ppx_v4.rewrite_implementation ~jsxMode code
  | _ -> code
  [@@raises Invalid_argument, Failure]

let rewrite_signature ~jsxVersion ~jsxModule ~jsxMode
    (code : Parsetree.signature) : Parsetree.signature =
  match (jsxVersion, jsxModule, jsxMode) with
  | 3, _, _ -> Reactjs_jsx_ppx_v3.rewrite_signature code
  | 4, _, "classic" ->
    Reactjs_jsx_ppx_v4.rewrite_signature ~jsxMode code
  | 4, _, "automatic" ->
    Reactjs_jsx_ppx_v4.rewrite_signature ~jsxMode code
  | _ -> code
  [@@raises Invalid_argument, Failure]
