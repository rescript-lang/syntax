open Asttypes
open Parsetree

type jsxConfig = {
  mutable version: int;
  mutable module_: string;
  mutable mode: string;
  mutable nestedModules: string list;
  mutable hasReactComponent: bool;
}

(* Helper method to look up the [@react.component] attribute *)
let hasAttr (loc, _) = loc.txt = "react.component"

(* Iterate over the attributes and try to find the [@react.component] attribute *)
let hasAttrOnBinding {pvb_attributes} =
  List.find_opt hasAttr pvb_attributes <> None

let raiseError ~loc msg = Location.raise_errorf ~loc msg

let raiseErrorMultipleReactComponent ~loc =
  raiseError ~loc
    "Only one component definition is allowed for each module. Move to a \
     submodule or other file if necessary."
