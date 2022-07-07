open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

type jsxConfig = {
  mutable version: int;
  mutable module_: string;
  mutable mode: string;
  mutable nestedModules: string list;
}

module V3 = struct
  let rec find_opt p = function
    | [] -> None
    | x :: l -> if p x then Some x else find_opt p l

  let nolabel = Nolabel

  let labelled str = Labelled str

  let optional str = Optional str

  let isOptional str =
    match str with
    | Optional _ -> true
    | _ -> false

  let isLabelled str =
    match str with
    | Labelled _ -> true
    | _ -> false

  let getLabel str =
    match str with
    | Optional str | Labelled str -> str
    | Nolabel -> ""

  let optionIdent = Lident "option"

  let constantString ~loc str =
    Ast_helper.Exp.constant ~loc (Pconst_string (str, None))

  let safeTypeFromValue valueStr =
    let valueStr = getLabel valueStr in
    match String.sub valueStr 0 1 with
    | "_" -> "T" ^ valueStr
    | _ -> valueStr
    [@@raises Invalid_argument]

  let keyType loc =
    Typ.constr ~loc {loc; txt = optionIdent}
      [Typ.constr ~loc {loc; txt = Lident "string"} []]

  type 'a children = ListLiteral of 'a | Exact of 'a

  type componentConfig = {propsName: string}

  (* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
  let transformChildrenIfListUpper ~loc ~mapper theList =
    let rec transformChildren_ theList accum =
      (* not in the sense of converting a list to an array; convert the AST
         reprensentation of a list to the AST reprensentation of an array *)
      match theList with
      | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> (
        match accum with
        | [singleElement] -> Exact singleElement
        | accum -> ListLiteral (Exp.array ~loc (List.rev accum)))
      | {
       pexp_desc =
         Pexp_construct
           ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
      } ->
        transformChildren_ acc (mapper.expr mapper v :: accum)
      | notAList -> Exact (mapper.expr mapper notAList)
    in
    transformChildren_ theList []

  let transformChildrenIfList ~loc ~mapper theList =
    let rec transformChildren_ theList accum =
      (* not in the sense of converting a list to an array; convert the AST
         reprensentation of a list to the AST reprensentation of an array *)
      match theList with
      | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} ->
        Exp.array ~loc (List.rev accum)
      | {
       pexp_desc =
         Pexp_construct
           ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
      } ->
        transformChildren_ acc (mapper.expr mapper v :: accum)
      | notAList -> mapper.expr mapper notAList
    in
    transformChildren_ theList []

  let extractChildren ?(removeLastPositionUnit = false) ~loc propsAndChildren =
    let rec allButLast_ lst acc =
      match lst with
      | [] -> []
      | [(Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})] ->
        acc
      | (Nolabel, _) :: _rest ->
        raise
          (Invalid_argument
             "JSX: found non-labelled argument before the last position")
      | arg :: rest -> allButLast_ rest (arg :: acc)
      [@@raises Invalid_argument]
    in
    let allButLast lst =
      allButLast_ lst [] |> List.rev
      [@@raises Invalid_argument]
    in
    match
      List.partition
        (fun (label, _) -> label = labelled "children")
        propsAndChildren
    with
    | [], props ->
      (* no children provided? Place a placeholder list *)
      ( Exp.construct ~loc {loc; txt = Lident "[]"} None,
        if removeLastPositionUnit then allButLast props else props )
    | [(_, childrenExpr)], props ->
      (childrenExpr, if removeLastPositionUnit then allButLast props else props)
    | _ ->
      raise
        (Invalid_argument "JSX: somehow there's more than one `children` label")
    [@@raises Invalid_argument]

  let unerasableIgnore loc =
    ( {loc; txt = "warning"},
      PStr [Str.eval (Exp.constant (Pconst_string ("-16", None)))] )

  let merlinFocus = ({loc = Location.none; txt = "merlin.focus"}, PStr [])

  (* Helper method to look up the [@react.component] attribute *)
  let hasAttr (loc, _) = loc.txt = "react.component"

  (* Helper method to filter out any attribute that isn't [@react.component] *)
  let otherAttrsPure (loc, _) = loc.txt <> "react.component"

  (* Iterate over the attributes and try to find the [@react.component] attribute *)
  let hasAttrOnBinding {pvb_attributes} =
    find_opt hasAttr pvb_attributes <> None

  (* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
  let rec getFnName binding =
    match binding with
    | {ppat_desc = Ppat_var {txt}} -> txt
    | {ppat_desc = Ppat_constraint (pat, _)} -> getFnName pat
    | _ ->
      raise (Invalid_argument "react.component calls cannot be destructured.")
    [@@raises Invalid_argument]

  let makeNewBinding binding expression newName =
    match binding with
    | {pvb_pat = {ppat_desc = Ppat_var ppat_var} as pvb_pat} ->
      {
        binding with
        pvb_pat =
          {pvb_pat with ppat_desc = Ppat_var {ppat_var with txt = newName}};
        pvb_expr = expression;
        pvb_attributes = [merlinFocus];
      }
    | _ ->
      raise (Invalid_argument "react.component calls cannot be destructured.")
    [@@raises Invalid_argument]

  (* Lookup the value of `props` otherwise raise Invalid_argument error *)
  let getPropsNameValue _acc (loc, exp) =
    match (loc, exp) with
    | {txt = Lident "props"}, {pexp_desc = Pexp_ident {txt = Lident str}} ->
      {propsName = str}
    | {txt}, _ ->
      raise
        (Invalid_argument
           ("react.component only accepts props as an option, given: "
          ^ Longident.last txt))
    [@@raises Invalid_argument]

  (* Lookup the `props` record or string as part of [@react.component] and store the name for use when rewriting *)
  let getPropsAttr payload =
    let defaultProps = {propsName = "Props"} in
    match payload with
    | Some
        (PStr
          ({
             pstr_desc =
               Pstr_eval ({pexp_desc = Pexp_record (recordFields, None)}, _);
           }
          :: _rest)) ->
      List.fold_left getPropsNameValue defaultProps recordFields
    | Some
        (PStr
          ({
             pstr_desc =
               Pstr_eval ({pexp_desc = Pexp_ident {txt = Lident "props"}}, _);
           }
          :: _rest)) ->
      {propsName = "props"}
    | Some (PStr ({pstr_desc = Pstr_eval (_, _)} :: _rest)) ->
      raise
        (Invalid_argument
           "react.component accepts a record config with props as an options.")
    | _ -> defaultProps
    [@@raises Invalid_argument]

  (* Plucks the label, loc, and type_ from an AST node *)
  let pluckLabelDefaultLocType (label, default, _, _, loc, type_) =
    (label, default, loc, type_)

  (* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
  let filenameFromLoc (pstr_loc : Location.t) =
    let fileName =
      match pstr_loc.loc_start.pos_fname with
      | "" -> !Location.input_name
      | fileName -> fileName
    in
    let fileName =
      try Filename.chop_extension (Filename.basename fileName)
      with Invalid_argument _ -> fileName
    in
    let fileName = String.capitalize_ascii fileName in
    fileName

  (* Build a string representation of a module name with segments separated by $ *)
  let makeModuleName fileName nestedModules fnName =
    let fullModuleName =
      match (fileName, nestedModules, fnName) with
      (* TODO: is this even reachable? It seems like the fileName always exists *)
      | "", nestedModules, "make" -> nestedModules
      | "", nestedModules, fnName -> List.rev (fnName :: nestedModules)
      | fileName, nestedModules, "make" -> fileName :: List.rev nestedModules
      | fileName, nestedModules, fnName ->
        fileName :: List.rev (fnName :: nestedModules)
    in
    let fullModuleName = String.concat "$" fullModuleName in
    fullModuleName

  (*
  AST node builders
  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
*)

  (* Build an AST node representing all named args for the `external` definition for a component's props *)
  let rec recursivelyMakeNamedArgsForExternal list args =
    match list with
    | (label, default, loc, interiorType) :: tl ->
      recursivelyMakeNamedArgsForExternal tl
        (Typ.arrow ~loc label
           (match (label, interiorType, default) with
           (* ~foo=1 *)
           | label, None, Some _ ->
             {
               ptyp_desc = Ptyp_var (safeTypeFromValue label);
               ptyp_loc = loc;
               ptyp_attributes = [];
             }
           (* ~foo: int=1 *)
           | _label, Some type_, Some _ -> type_
           (* ~foo: option(int)=? *)
           | ( label,
               Some {ptyp_desc = Ptyp_constr ({txt = Lident "option"}, [type_])},
               _ )
           | ( label,
               Some
                 {
                   ptyp_desc =
                     Ptyp_constr
                       ({txt = Ldot (Lident "*predef*", "option")}, [type_]);
                 },
               _ )
           (* ~foo: int=? - note this isnt valid. but we want to get a type error *)
           | label, Some type_, _
             when isOptional label ->
             type_
           (* ~foo=? *)
           | label, None, _ when isOptional label ->
             {
               ptyp_desc = Ptyp_var (safeTypeFromValue label);
               ptyp_loc = loc;
               ptyp_attributes = [];
             }
           (* ~foo *)
           | label, None, _ ->
             {
               ptyp_desc = Ptyp_var (safeTypeFromValue label);
               ptyp_loc = loc;
               ptyp_attributes = [];
             }
           | _label, Some type_, _ -> type_)
           args)
    | [] -> args
    [@@raises Invalid_argument]

  (* Build an AST node for the [@bs.obj] representing props for a component *)
  let makePropsValue fnName loc namedArgListWithKeyAndRef propsType =
    let propsName = fnName ^ "Props" in
    {
      pval_name = {txt = propsName; loc};
      pval_type =
        recursivelyMakeNamedArgsForExternal namedArgListWithKeyAndRef
          (Typ.arrow nolabel
             {
               ptyp_desc = Ptyp_constr ({txt = Lident "unit"; loc}, []);
               ptyp_loc = loc;
               ptyp_attributes = [];
             }
             propsType);
      pval_prim = [""];
      pval_attributes = [({txt = "bs.obj"; loc}, PStr [])];
      pval_loc = loc;
    }
    [@@raises Invalid_argument]

  (* Build an AST node representing an `external` with the definition of the [@bs.obj] *)
  let makePropsExternal fnName loc namedArgListWithKeyAndRef propsType =
    {
      pstr_loc = loc;
      pstr_desc =
        Pstr_primitive
          (makePropsValue fnName loc namedArgListWithKeyAndRef propsType);
    }
    [@@raises Invalid_argument]

  (* Build an AST node for the signature of the `external` definition *)
  let makePropsExternalSig fnName loc namedArgListWithKeyAndRef propsType =
    {
      psig_loc = loc;
      psig_desc =
        Psig_value
          (makePropsValue fnName loc namedArgListWithKeyAndRef propsType);
    }
    [@@raises Invalid_argument]

  (* Build an AST node for the props name when converted to an object inside the function signature  *)
  let makePropsName ~loc name =
    {
      ppat_desc = Ppat_var {txt = name; loc};
      ppat_loc = loc;
      ppat_attributes = [];
    }

  let makeObjectField loc (str, attrs, type_) =
    Otag ({loc; txt = str}, attrs, type_)

  (* Build an AST node representing a "closed" object representing a component's props *)
  let makePropsType ~loc namedTypeList =
    Typ.mk ~loc
      (Ptyp_object (List.map (makeObjectField loc) namedTypeList, Closed))

  (* Builds an AST node for the entire `external` definition of props *)
  let makeExternalDecl fnName loc namedArgListWithKeyAndRef namedTypeList =
    makePropsExternal fnName loc
      (List.map pluckLabelDefaultLocType namedArgListWithKeyAndRef)
      (makePropsType ~loc namedTypeList)
    [@@raises Invalid_argument]

  let newtypeToVar newtype type_ =
    let var_desc = Ptyp_var ("type-" ^ newtype) in
    let typ (mapper : Ast_mapper.mapper) typ =
      match typ.ptyp_desc with
      | Ptyp_constr ({txt = Lident name}, _) when name = newtype ->
        {typ with ptyp_desc = var_desc}
      | _ -> Ast_mapper.default_mapper.typ mapper typ
    in
    let mapper = {Ast_mapper.default_mapper with typ} in
    mapper.typ mapper type_

  (* TODO: some line number might still be wrong *)
  let jsxMapper ~config =
    let transformUppercaseCall3 modulePath mapper loc attrs _ callArguments =
      let children, argsWithLabels =
        extractChildren ~loc ~removeLastPositionUnit:true callArguments
      in
      let argsForMake = argsWithLabels in
      let childrenExpr = transformChildrenIfListUpper ~loc ~mapper children in
      let recursivelyTransformedArgsForMake =
        argsForMake
        |> List.map (fun (label, expression) ->
               (label, mapper.expr mapper expression))
      in
      let childrenArg = ref None in
      let args =
        recursivelyTransformedArgsForMake
        @ (match childrenExpr with
          | Exact children -> [(labelled "children", children)]
          | ListLiteral {pexp_desc = Pexp_array list} when list = [] -> []
          | ListLiteral expression ->
            (* this is a hack to support react components that introspect into their children *)
            childrenArg := Some expression;
            [
              ( labelled "children",
                Exp.ident ~loc {loc; txt = Ldot (Lident "React", "null")} );
            ])
        @ [(nolabel, Exp.construct ~loc {loc; txt = Lident "()"} None)]
      in
      let isCap str =
        let first = String.sub str 0 1 [@@raises Invalid_argument] in
        let capped = String.uppercase_ascii first in
        first = capped
        [@@raises Invalid_argument]
      in
      let ident =
        match modulePath with
        | Lident _ -> Ldot (modulePath, "make")
        | Ldot (_modulePath, value) as fullPath when isCap value ->
          Ldot (fullPath, "make")
        | modulePath -> modulePath
      in
      let propsIdent =
        match ident with
        | Lident path -> Lident (path ^ "Props")
        | Ldot (ident, path) -> Ldot (ident, path ^ "Props")
        | _ ->
          raise
            (Invalid_argument
               "JSX name can't be the result of function applications")
      in
      let props =
        Exp.apply ~attrs ~loc (Exp.ident ~loc {loc; txt = propsIdent}) args
      in
      (* handle key, ref, children *)
      (* React.createElement(Component.make, props, ...children) *)
      match !childrenArg with
      | None ->
        Exp.apply ~loc ~attrs
          (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "createElement")})
          [(nolabel, Exp.ident ~loc {txt = ident; loc}); (nolabel, props)]
      | Some children ->
        Exp.apply ~loc ~attrs
          (Exp.ident ~loc
             {loc; txt = Ldot (Lident "React", "createElementVariadic")})
          [
            (nolabel, Exp.ident ~loc {txt = ident; loc});
            (nolabel, props);
            (nolabel, children);
          ]
      [@@raises Invalid_argument]
    in

    let transformLowercaseCall3 mapper loc attrs callArguments id =
      let children, nonChildrenProps = extractChildren ~loc callArguments in
      let componentNameExpr = constantString ~loc id in
      let childrenExpr = transformChildrenIfList ~loc ~mapper children in
      let createElementCall =
        match children with
        (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
        | {
         pexp_desc =
           ( Pexp_construct
               ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple _})
           | Pexp_construct ({txt = Lident "[]"}, None) );
        } ->
          "createDOMElementVariadic"
        (* [@JSX] div(~children= value), coming from <div> ...(value) </div> *)
        | _ ->
          raise
            (Invalid_argument
               "A spread as a DOM element's children don't make sense written \
                together. You can simply remove the spread.")
      in
      let args =
        match nonChildrenProps with
        | [_justTheUnitArgumentAtEnd] ->
          [
            (* "div" *)
            (nolabel, componentNameExpr);
            (* [|moreCreateElementCallsHere|] *)
            (nolabel, childrenExpr);
          ]
        | nonEmptyProps ->
          let propsCall =
            Exp.apply ~loc
              (Exp.ident ~loc
                 {loc; txt = Ldot (Lident "ReactDOMRe", "domProps")})
              (nonEmptyProps
              |> List.map (fun (label, expression) ->
                     (label, mapper.expr mapper expression)))
          in
          [
            (* "div" *)
            (nolabel, componentNameExpr);
            (* ReactDOMRe.props(~className=blabla, ~foo=bar, ()) *)
            (labelled "props", propsCall);
            (* [|moreCreateElementCallsHere|] *)
            (nolabel, childrenExpr);
          ]
      in
      Exp.apply
        ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
        ~attrs
        (* ReactDOMRe.createElement *)
        (Exp.ident ~loc
           {loc; txt = Ldot (Lident "ReactDOMRe", createElementCall)})
        args
      [@@raises Invalid_argument]
    in

    let rec recursivelyTransformNamedArgsForMake mapper expr args newtypes =
      let expr = mapper.expr mapper expr in
      match expr.pexp_desc with
      (* TODO: make this show up with a loc. *)
      | Pexp_fun (Labelled "key", _, _, _) | Pexp_fun (Optional "key", _, _, _)
        ->
        raise
          (Invalid_argument
             "Key cannot be accessed inside of a component. Don't worry - you \
              can always key a component from its parent!")
      | Pexp_fun (Labelled "ref", _, _, _) | Pexp_fun (Optional "ref", _, _, _)
        ->
        raise
          (Invalid_argument
             "Ref cannot be passed as a normal prop. Please use `forwardRef` \
              API instead.")
      | Pexp_fun (arg, default, pattern, expression)
        when isOptional arg || isLabelled arg ->
        let () =
          match (isOptional arg, pattern, default) with
          | true, {ppat_desc = Ppat_constraint (_, {ptyp_desc})}, None -> (
            match ptyp_desc with
            | Ptyp_constr ({txt = Lident "option"}, [_]) -> ()
            | _ ->
              let currentType =
                match ptyp_desc with
                | Ptyp_constr ({txt}, []) ->
                  String.concat "." (Longident.flatten txt)
                | Ptyp_constr ({txt}, _innerTypeArgs) ->
                  String.concat "." (Longident.flatten txt) ^ "(...)"
                | _ -> "..."
              in
              Location.prerr_warning pattern.ppat_loc
                (Preprocessor
                   (Printf.sprintf
                      "React: optional argument annotations must have explicit \
                       `option`. Did you mean `option(%s)=?`?"
                      currentType)))
          | _ -> ()
        in
        let alias =
          match pattern with
          | {ppat_desc = Ppat_alias (_, {txt}) | Ppat_var {txt}} -> txt
          | {ppat_desc = Ppat_any} -> "_"
          | _ -> getLabel arg
        in
        let type_ =
          match pattern with
          | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
          | _ -> None
        in

        recursivelyTransformNamedArgsForMake mapper expression
          ((arg, default, pattern, alias, pattern.ppat_loc, type_) :: args)
          newtypes
      | Pexp_fun
          ( Nolabel,
            _,
            {ppat_desc = Ppat_construct ({txt = Lident "()"}, _) | Ppat_any},
            _expression ) ->
        (args, newtypes, None)
      | Pexp_fun
          ( Nolabel,
            _,
            {
              ppat_desc =
                ( Ppat_var {txt}
                | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) );
            },
            _expression ) ->
        (args, newtypes, Some txt)
      | Pexp_fun (Nolabel, _, pattern, _expression) ->
        Location.raise_errorf ~loc:pattern.ppat_loc
          "React: react.component refs only support plain arguments and type \
           annotations."
      | Pexp_newtype (label, expression) ->
        recursivelyTransformNamedArgsForMake mapper expression args
          (label :: newtypes)
      | Pexp_constraint (expression, _typ) ->
        recursivelyTransformNamedArgsForMake mapper expression args newtypes
      | _ -> (args, newtypes, None)
      [@@raises Invalid_argument]
    in

    let argToType types (name, default, _noLabelName, _alias, loc, type_) =
      match (type_, name, default) with
      | ( Some {ptyp_desc = Ptyp_constr ({txt = Lident "option"}, [type_])},
          name,
          _ )
        when isOptional name ->
        ( getLabel name,
          [],
          {
            type_ with
            ptyp_desc =
              Ptyp_constr ({loc = type_.ptyp_loc; txt = optionIdent}, [type_]);
          } )
        :: types
      | Some type_, name, Some _default ->
        ( getLabel name,
          [],
          {
            ptyp_desc = Ptyp_constr ({loc; txt = optionIdent}, [type_]);
            ptyp_loc = loc;
            ptyp_attributes = [];
          } )
        :: types
      | Some type_, name, _ -> (getLabel name, [], type_) :: types
      | None, name, _ when isOptional name ->
        ( getLabel name,
          [],
          {
            ptyp_desc =
              Ptyp_constr
                ( {loc; txt = optionIdent},
                  [
                    {
                      ptyp_desc = Ptyp_var (safeTypeFromValue name);
                      ptyp_loc = loc;
                      ptyp_attributes = [];
                    };
                  ] );
            ptyp_loc = loc;
            ptyp_attributes = [];
          } )
        :: types
      | None, name, _ when isLabelled name ->
        ( getLabel name,
          [],
          {
            ptyp_desc = Ptyp_var (safeTypeFromValue name);
            ptyp_loc = loc;
            ptyp_attributes = [];
          } )
        :: types
      | _ -> types
      [@@raises Invalid_argument]
    in

    let argToConcreteType types (name, loc, type_) =
      match name with
      | name when isLabelled name -> (getLabel name, [], type_) :: types
      | name when isOptional name ->
        (getLabel name, [], Typ.constr ~loc {loc; txt = optionIdent} [type_])
        :: types
      | _ -> types
    in

    let nestedModules = ref [] in
    let transformStructureItem mapper item =
      match item with
      (* external *)
      | {
          pstr_loc;
          pstr_desc =
            Pstr_primitive
              ({pval_name = {txt = fnName}; pval_attributes; pval_type} as
              value_description);
        } as pstr -> (
        match List.filter hasAttr pval_attributes with
        | [] -> [item]
        | [_] ->
          let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
            match ptyp_desc with
            | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
              when isLabelled name || isOptional name ->
              getPropTypes ((name, ptyp_loc, type_) :: types) rest
            | Ptyp_arrow (Nolabel, _type, rest) -> getPropTypes types rest
            | Ptyp_arrow (name, type_, returnValue)
              when isLabelled name || isOptional name ->
              (returnValue, (name, returnValue.ptyp_loc, type_) :: types)
            | _ -> (fullType, types)
          in
          let innerType, propTypes = getPropTypes [] pval_type in
          let namedTypeList = List.fold_left argToConcreteType [] propTypes in
          let pluckLabelAndLoc (label, loc, type_) =
            (label, None (* default *), loc, Some type_)
          in
          let retPropsType = makePropsType ~loc:pstr_loc namedTypeList in
          let externalPropsDecl =
            makePropsExternal fnName pstr_loc
              ((optional "key", None, pstr_loc, Some (keyType pstr_loc))
              :: List.map pluckLabelAndLoc propTypes)
              retPropsType
          in
          (* can't be an arrow because it will defensively uncurry *)
          let newExternalType =
            Ptyp_constr
              ( {loc = pstr_loc; txt = Ldot (Lident "React", "componentLike")},
                [retPropsType; innerType] )
          in
          let newStructure =
            {
              pstr with
              pstr_desc =
                Pstr_primitive
                  {
                    value_description with
                    pval_type = {pval_type with ptyp_desc = newExternalType};
                    pval_attributes = List.filter otherAttrsPure pval_attributes;
                  };
            }
          in
          [externalPropsDecl; newStructure]
        | _ ->
          raise
            (Invalid_argument
               "Only one react.component call can exist on a component at one \
                time"))
      (* let component = ... *)
      | {pstr_loc; pstr_desc = Pstr_value (recFlag, valueBindings)} -> (
        let fileName = filenameFromLoc pstr_loc in
        let emptyLoc = Location.in_file fileName in
        let mapBinding binding =
          if hasAttrOnBinding binding then
            let bindingLoc = binding.pvb_loc in
            let bindingPatLoc = binding.pvb_pat.ppat_loc in
            let binding =
              {
                binding with
                pvb_pat = {binding.pvb_pat with ppat_loc = emptyLoc};
                pvb_loc = emptyLoc;
              }
            in
            let fnName = getFnName binding.pvb_pat in
            let internalFnName = fnName ^ "$Internal" in
            let fullModuleName =
              makeModuleName fileName !nestedModules fnName
            in
            let modifiedBindingOld binding =
              let expression = binding.pvb_expr in
              (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
              let rec spelunkForFunExpression expression =
                match expression with
                (* let make = (~prop) => ... *)
                | {pexp_desc = Pexp_fun _} | {pexp_desc = Pexp_newtype _} ->
                  expression
                (* let make = {let foo = bar in (~prop) => ...} *)
                | {pexp_desc = Pexp_let (_recursive, _vbs, returnExpression)} ->
                  (* here's where we spelunk! *)
                  spelunkForFunExpression returnExpression
                (* let make = React.forwardRef((~prop) => ...) *)
                | {
                 pexp_desc =
                   Pexp_apply
                     (_wrapperExpression, [(Nolabel, innerFunctionExpression)]);
                } ->
                  spelunkForFunExpression innerFunctionExpression
                | {
                 pexp_desc =
                   Pexp_sequence (_wrapperExpression, innerFunctionExpression);
                } ->
                  spelunkForFunExpression innerFunctionExpression
                | {pexp_desc = Pexp_constraint (innerFunctionExpression, _typ)}
                  ->
                  spelunkForFunExpression innerFunctionExpression
                | _ ->
                  raise
                    (Invalid_argument
                       "react.component calls can only be on function \
                        definitions or component wrappers (forwardRef, memo).")
                [@@raises Invalid_argument]
              in
              spelunkForFunExpression expression
            in
            let modifiedBinding binding =
              let hasApplication = ref false in
              let wrapExpressionWithBinding expressionFn expression =
                Vb.mk ~loc:bindingLoc
                  ~attrs:(List.filter otherAttrsPure binding.pvb_attributes)
                  (Pat.var ~loc:bindingPatLoc
                     {loc = bindingPatLoc; txt = fnName})
                  (expressionFn expression)
              in
              let expression = binding.pvb_expr in
              let unerasableIgnoreExp exp =
                {
                  exp with
                  pexp_attributes =
                    unerasableIgnore emptyLoc :: exp.pexp_attributes;
                }
              in
              (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
              let rec spelunkForFunExpression expression =
                match expression with
                (* let make = (~prop) => ... with no final unit *)
                | {
                 pexp_desc =
                   Pexp_fun
                     ( ((Labelled _ | Optional _) as label),
                       default,
                       pattern,
                       ({pexp_desc = Pexp_fun _} as internalExpression) );
                } ->
                  let wrap, hasUnit, exp =
                    spelunkForFunExpression internalExpression
                  in
                  ( wrap,
                    hasUnit,
                    unerasableIgnoreExp
                      {
                        expression with
                        pexp_desc = Pexp_fun (label, default, pattern, exp);
                      } )
                (* let make = (()) => ... *)
                (* let make = (_) => ... *)
                | {
                 pexp_desc =
                   Pexp_fun
                     ( Nolabel,
                       _default,
                       {
                         ppat_desc =
                           Ppat_construct ({txt = Lident "()"}, _) | Ppat_any;
                       },
                       _internalExpression );
                } ->
                  ((fun a -> a), true, expression)
                (* let make = (~prop) => ... *)
                | {
                 pexp_desc =
                   Pexp_fun
                     ( (Labelled _ | Optional _),
                       _default,
                       _pattern,
                       _internalExpression );
                } ->
                  ((fun a -> a), false, unerasableIgnoreExp expression)
                (* let make = (prop) => ... *)
                | {
                 pexp_desc =
                   Pexp_fun (_nolabel, _default, pattern, _internalExpression);
                } ->
                  if hasApplication.contents then
                    ((fun a -> a), false, unerasableIgnoreExp expression)
                  else
                    Location.raise_errorf ~loc:pattern.ppat_loc
                      "React: props need to be labelled arguments.\n\
                      \  If you are working with refs be sure to wrap with \
                       React.forwardRef.\n\
                      \  If your component doesn't have any props use () or _ \
                       instead of a name."
                (* let make = {let foo = bar in (~prop) => ...} *)
                | {pexp_desc = Pexp_let (recursive, vbs, internalExpression)} ->
                  (* here's where we spelunk! *)
                  let wrap, hasUnit, exp =
                    spelunkForFunExpression internalExpression
                  in
                  ( wrap,
                    hasUnit,
                    {expression with pexp_desc = Pexp_let (recursive, vbs, exp)}
                  )
                (* let make = React.forwardRef((~prop) => ...) *)
                | {
                 pexp_desc =
                   Pexp_apply
                     (wrapperExpression, [(Nolabel, internalExpression)]);
                } ->
                  let () = hasApplication := true in
                  let _, hasUnit, exp =
                    spelunkForFunExpression internalExpression
                  in
                  ( (fun exp -> Exp.apply wrapperExpression [(nolabel, exp)]),
                    hasUnit,
                    exp )
                | {
                 pexp_desc =
                   Pexp_sequence (wrapperExpression, internalExpression);
                } ->
                  let wrap, hasUnit, exp =
                    spelunkForFunExpression internalExpression
                  in
                  ( wrap,
                    hasUnit,
                    {
                      expression with
                      pexp_desc = Pexp_sequence (wrapperExpression, exp);
                    } )
                | e -> ((fun a -> a), false, e)
              in
              let wrapExpression, hasUnit, expression =
                spelunkForFunExpression expression
              in
              (wrapExpressionWithBinding wrapExpression, hasUnit, expression)
            in
            let bindingWrapper, hasUnit, expression = modifiedBinding binding in
            let reactComponentAttribute =
              try Some (List.find hasAttr binding.pvb_attributes)
              with Not_found -> None
            in
            let _attr_loc, payload =
              match reactComponentAttribute with
              | Some (loc, payload) -> (loc.loc, Some payload)
              | None -> (emptyLoc, None)
            in
            let props = getPropsAttr payload in
            (* do stuff here! *)
            let namedArgList, newtypes, forwardRef =
              recursivelyTransformNamedArgsForMake mapper
                (modifiedBindingOld binding)
                [] []
            in
            let namedArgListWithKeyAndRef =
              ( optional "key",
                None,
                Pat.var {txt = "key"; loc = emptyLoc},
                "key",
                emptyLoc,
                Some (keyType emptyLoc) )
              :: namedArgList
            in
            let namedArgListWithKeyAndRef =
              match forwardRef with
              | Some _ ->
                ( optional "ref",
                  None,
                  Pat.var {txt = "key"; loc = emptyLoc},
                  "ref",
                  emptyLoc,
                  None )
                :: namedArgListWithKeyAndRef
              | None -> namedArgListWithKeyAndRef
            in
            let namedArgListWithKeyAndRefForNew =
              match forwardRef with
              | Some txt ->
                namedArgList
                @ [
                    ( nolabel,
                      None,
                      Pat.var {txt; loc = emptyLoc},
                      txt,
                      emptyLoc,
                      None );
                  ]
              | None -> namedArgList
            in
            let pluckArg (label, _, _, alias, loc, _) =
              let labelString =
                match label with
                | label when isOptional label || isLabelled label ->
                  getLabel label
                | _ -> ""
              in
              ( label,
                match labelString with
                | "" -> Exp.ident ~loc {txt = Lident alias; loc}
                | labelString ->
                  Exp.apply ~loc
                    (Exp.ident ~loc {txt = Lident "##"; loc})
                    [
                      ( nolabel,
                        Exp.ident ~loc {txt = Lident props.propsName; loc} );
                      (nolabel, Exp.ident ~loc {txt = Lident labelString; loc});
                    ] )
            in
            let namedTypeList = List.fold_left argToType [] namedArgList in
            let loc = emptyLoc in
            let externalArgs =
              (* translate newtypes to type variables *)
              List.fold_left
                (fun args newtype ->
                  List.map
                    (fun (a, b, c, d, e, maybeTyp) ->
                      match maybeTyp with
                      | Some typ ->
                        (a, b, c, d, e, Some (newtypeToVar newtype.txt typ))
                      | None -> (a, b, c, d, e, None))
                    args)
                namedArgListWithKeyAndRef newtypes
            in
            let externalTypes =
              (* translate newtypes to type variables *)
              List.fold_left
                (fun args newtype ->
                  List.map
                    (fun (a, b, typ) -> (a, b, newtypeToVar newtype.txt typ))
                    args)
                namedTypeList newtypes
            in
            let externalDecl =
              makeExternalDecl fnName loc externalArgs externalTypes
            in
            let innerExpressionArgs =
              List.map pluckArg namedArgListWithKeyAndRefForNew
              @
              if hasUnit then
                [(Nolabel, Exp.construct {loc; txt = Lident "()"} None)]
              else []
            in
            let innerExpression =
              Exp.apply
                (Exp.ident
                   {
                     loc;
                     txt =
                       Lident
                         (match recFlag with
                         | Recursive -> internalFnName
                         | Nonrecursive -> fnName);
                   })
                innerExpressionArgs
            in
            let innerExpressionWithRef =
              match forwardRef with
              | Some txt ->
                {
                  innerExpression with
                  pexp_desc =
                    Pexp_fun
                      ( nolabel,
                        None,
                        {
                          ppat_desc = Ppat_var {txt; loc = emptyLoc};
                          ppat_loc = emptyLoc;
                          ppat_attributes = [];
                        },
                        innerExpression );
                }
              | None -> innerExpression
            in
            let fullExpression =
              Exp.fun_ nolabel None
                {
                  ppat_desc =
                    Ppat_constraint
                      ( makePropsName ~loc:emptyLoc props.propsName,
                        makePropsType ~loc:emptyLoc externalTypes );
                  ppat_loc = emptyLoc;
                  ppat_attributes = [];
                }
                innerExpressionWithRef
            in
            let fullExpression =
              match fullModuleName with
              | "" -> fullExpression
              | txt ->
                Exp.let_ Nonrecursive
                  [
                    Vb.mk ~loc:emptyLoc
                      (Pat.var ~loc:emptyLoc {loc = emptyLoc; txt})
                      fullExpression;
                  ]
                  (Exp.ident ~loc:emptyLoc {loc = emptyLoc; txt = Lident txt})
            in
            let bindings, newBinding =
              match recFlag with
              | Recursive ->
                ( [
                    bindingWrapper
                      (Exp.let_ ~loc:emptyLoc Recursive
                         [
                           makeNewBinding binding expression internalFnName;
                           Vb.mk
                             (Pat.var {loc = emptyLoc; txt = fnName})
                             fullExpression;
                         ]
                         (Exp.ident {loc = emptyLoc; txt = Lident fnName}));
                  ],
                  None )
              | Nonrecursive ->
                ( [{binding with pvb_expr = expression; pvb_attributes = []}],
                  Some (bindingWrapper fullExpression) )
            in
            (Some externalDecl, bindings, newBinding)
          else (None, [binding], None)
          [@@raises Invalid_argument]
        in
        let structuresAndBinding = List.map mapBinding valueBindings in
        let otherStructures (extern, binding, newBinding)
            (externs, bindings, newBindings) =
          let externs =
            match extern with
            | Some extern -> extern :: externs
            | None -> externs
          in
          let newBindings =
            match newBinding with
            | Some newBinding -> newBinding :: newBindings
            | None -> newBindings
          in
          (externs, binding @ bindings, newBindings)
        in
        let externs, bindings, newBindings =
          List.fold_right otherStructures structuresAndBinding ([], [], [])
        in
        externs
        @ [{pstr_loc; pstr_desc = Pstr_value (recFlag, bindings)}]
        @
        match newBindings with
        | [] -> []
        | newBindings ->
          [{pstr_loc = emptyLoc; pstr_desc = Pstr_value (recFlag, newBindings)}]
        )
      | _ -> [item]
      [@@raises Invalid_argument]
    in

    let transformSignatureItem _mapper item =
      match item with
      | {
          psig_loc;
          psig_desc =
            Psig_value
              ({pval_name = {txt = fnName}; pval_attributes; pval_type} as
              psig_desc);
        } as psig -> (
        match List.filter hasAttr pval_attributes with
        | [] -> [item]
        | [_] ->
          let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
            match ptyp_desc with
            | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
              when isOptional name || isLabelled name ->
              getPropTypes ((name, ptyp_loc, type_) :: types) rest
            | Ptyp_arrow (Nolabel, _type, rest) -> getPropTypes types rest
            | Ptyp_arrow (name, type_, returnValue)
              when isOptional name || isLabelled name ->
              (returnValue, (name, returnValue.ptyp_loc, type_) :: types)
            | _ -> (fullType, types)
          in
          let innerType, propTypes = getPropTypes [] pval_type in
          let namedTypeList = List.fold_left argToConcreteType [] propTypes in
          let pluckLabelAndLoc (label, loc, type_) =
            (label, None, loc, Some type_)
          in
          let retPropsType = makePropsType ~loc:psig_loc namedTypeList in
          let externalPropsDecl =
            makePropsExternalSig fnName psig_loc
              ((optional "key", None, psig_loc, Some (keyType psig_loc))
              :: List.map pluckLabelAndLoc propTypes)
              retPropsType
          in
          (* can't be an arrow because it will defensively uncurry *)
          let newExternalType =
            Ptyp_constr
              ( {loc = psig_loc; txt = Ldot (Lident "React", "componentLike")},
                [retPropsType; innerType] )
          in
          let newStructure =
            {
              psig with
              psig_desc =
                Psig_value
                  {
                    psig_desc with
                    pval_type = {pval_type with ptyp_desc = newExternalType};
                    pval_attributes = List.filter otherAttrsPure pval_attributes;
                  };
            }
          in
          [externalPropsDecl; newStructure]
        | _ ->
          raise
            (Invalid_argument
               "Only one react.component call can exist on a component at one \
                time"))
      | _ -> [item]
      [@@raises Invalid_argument]
    in

    let transformJsxCall mapper callExpression callArguments attrs =
      match callExpression.pexp_desc with
      | Pexp_ident caller -> (
        match caller with
        | {txt = Lident "createElement"} ->
          raise
            (Invalid_argument
               "JSX: `createElement` should be preceeded by a module name.")
        (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
        | {loc; txt = Ldot (modulePath, ("createElement" | "make"))} -> (
          match config.version with
          | 3 ->
            transformUppercaseCall3 modulePath mapper loc attrs callExpression
              callArguments
          | _ -> raise (Invalid_argument "JSX: the JSX version must be  3"))
        (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
        (* turn that into
           ReactDOMRe.createElement(~props=ReactDOMRe.props(~props1=foo, ~props2=bar, ()), [|bla|]) *)
        | {loc; txt = Lident id} -> (
          match config.version with
          | 3 -> transformLowercaseCall3 mapper loc attrs callArguments id
          | _ -> raise (Invalid_argument "JSX: the JSX version must be 3"))
        | {txt = Ldot (_, anythingNotCreateElementOrMake)} ->
          raise
            (Invalid_argument
               ("JSX: the JSX attribute should be attached to a \
                 `YourModuleName.createElement` or `YourModuleName.make` call. \
                 We saw `" ^ anythingNotCreateElementOrMake ^ "` instead"))
        | {txt = Lapply _} ->
          (* don't think there's ever a case where this is reached *)
          raise
            (Invalid_argument
               "JSX: encountered a weird case while processing the code. \
                Please report this!"))
      | _ ->
        raise
          (Invalid_argument
             "JSX: `createElement` should be preceeded by a simple, direct \
              module name.")
      [@@raises Invalid_argument]
    in

    let expr mapper expression =
      match expression with
      (* Does the function application have the @JSX attribute? *)
      | {
       pexp_desc = Pexp_apply (callExpression, callArguments);
       pexp_attributes;
      } -> (
        let jsxAttribute, nonJSXAttributes =
          List.partition
            (fun (attribute, _) -> attribute.txt = "JSX")
            pexp_attributes
        in
        match (jsxAttribute, nonJSXAttributes) with
        (* no JSX attribute *)
        | [], _ -> default_mapper.expr mapper expression
        | _, nonJSXAttributes ->
          transformJsxCall mapper callExpression callArguments nonJSXAttributes)
      (* is it a list with jsx attribute? Reason <>foo</> desugars to [@JSX][foo]*)
      | {
          pexp_desc =
            ( Pexp_construct
                ({txt = Lident "::"; loc}, Some {pexp_desc = Pexp_tuple _})
            | Pexp_construct ({txt = Lident "[]"; loc}, None) );
          pexp_attributes;
        } as listItems -> (
        let jsxAttribute, nonJSXAttributes =
          List.partition
            (fun (attribute, _) -> attribute.txt = "JSX")
            pexp_attributes
        in
        match (jsxAttribute, nonJSXAttributes) with
        (* no JSX attribute *)
        | [], _ -> default_mapper.expr mapper expression
        | _, nonJSXAttributes ->
          let loc = {loc with loc_ghost = true} in
          let fragment =
            Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "fragment")}
          in
          let childrenExpr = transformChildrenIfList ~loc ~mapper listItems in
          let args =
            [
              (* "div" *)
              (nolabel, fragment);
              (* [|moreCreateElementCallsHere|] *)
              (nolabel, childrenExpr);
            ]
          in
          Exp.apply
            ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
            ~attrs:nonJSXAttributes
            (* ReactDOMRe.createElement *)
            (Exp.ident ~loc
               {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
            args)
      (* Delegate to the default mapper, a deep identity traversal *)
      | e -> default_mapper.expr mapper e
      [@@raises Invalid_argument]
    in

    let module_binding mapper module_binding =
      let _ = nestedModules := module_binding.pmb_name.txt :: !nestedModules in
      let mapped = default_mapper.module_binding mapper module_binding in
      let _ = nestedModules := List.tl !nestedModules in
      mapped
      [@@raises Failure]
    in
    (expr, module_binding, transformSignatureItem, transformStructureItem)
    [@@raises Invalid_argument, Failure]
end

module V4 = struct
  let getJsxConfig payload =
    match payload with
    | PStr
        ({
           pstr_desc =
             Pstr_eval ({pexp_desc = Pexp_record (recordFields, None)}, _);
         }
        :: _rest) ->
      recordFields
    | _ -> []

  type configKey = Int | String

  let getJsxConfigByKey ~key ~type_ recordFields =
    let values =
      List.filter_map
        (fun ((lid, expr) : Longident.t Location.loc * expression) ->
          match (type_, lid, expr) with
          | ( Int,
              {txt = Lident k},
              {pexp_desc = Pexp_constant (Pconst_integer (value, None))} )
            when k = key ->
            Some value
          | ( String,
              {txt = Lident k},
              {pexp_desc = Pexp_constant (Pconst_string (value, None))} )
            when k = key ->
            Some value
          | _ -> None)
        recordFields
    in
    match values with
    | [] -> None
    | [v] | v :: _ -> Some v

  let getInt ~key fields =
    match fields |> getJsxConfigByKey ~key ~type_:Int with
    | None -> None
    | Some s -> int_of_string_opt s

  let getString ~key fields = fields |> getJsxConfigByKey ~key ~type_:String

  let updateConfig config payload =
    let fields = getJsxConfig payload in
    (match getInt ~key:"version" fields with
    | None -> ()
    | Some i -> config.version <- i);
    (match getString ~key:"module" fields with
    | None -> ()
    | Some s -> config.module_ <- s);
    match getString ~key:"mode" fields with
    | None -> ()
    | Some s -> config.mode <- s

  let isJsxConfigAttr ((loc, _) : attribute) = loc.txt = "jsxConfig"

  let processConfigAttribute attribute config =
    if isJsxConfigAttr attribute then updateConfig config (snd attribute)

  let rec find_opt p = function
    | [] -> None
    | x :: l -> if p x then Some x else find_opt p l

  let nolabel = Nolabel

  let labelled str = Labelled str

  let isOptional str =
    match str with
    | Optional _ -> true
    | _ -> false

  let isLabelled str =
    match str with
    | Labelled _ -> true
    | _ -> false

  let isForwardRef = function
    | {pexp_desc = Pexp_ident {txt = Ldot (Lident "React", "forwardRef")}} ->
      true
    | _ -> false

  let getLabel str =
    match str with
    | Optional str | Labelled str -> str
    | Nolabel -> ""

  let optionIdent = Lident "option"

  let optionalAttr = [({txt = "ns.optional"; loc = Location.none}, PStr [])]

  let constantString ~loc str =
    Ast_helper.Exp.constant ~loc (Pconst_string (str, None))

  (* {} empty object in Js *)
  let recordWithOnlyKey ~loc =
    Exp.record ~loc
      (* {key: @optional None} *)
      [
        ( {loc; txt = Lident "key"},
          Exp.construct ~attrs:optionalAttr {loc; txt = Lident "None"} None );
      ]
      None

  let safeTypeFromValue valueStr =
    let valueStr = getLabel valueStr in
    match String.sub valueStr 0 1 with
    | "_" -> "T" ^ valueStr
    | _ -> valueStr
    [@@raises Invalid_argument]

  let keyType loc = Typ.constr ~loc {loc; txt = Lident "string"} []

  let refType loc =
    Typ.constr ~loc
      {loc; txt = Ldot (Ldot (Lident "ReactDOM", "Ref"), "currentDomRef")}
      []

  type 'a children = ListLiteral of 'a | Exact of 'a

  (* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
  let transformChildrenIfListUpper ~loc ~mapper theList =
    let rec transformChildren_ theList accum =
      (* not in the sense of converting a list to an array; convert the AST
         reprensentation of a list to the AST reprensentation of an array *)
      match theList with
      | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> (
        match accum with
        | [singleElement] -> Exact singleElement
        | accum -> ListLiteral (Exp.array ~loc (List.rev accum)))
      | {
       pexp_desc =
         Pexp_construct
           ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
      } ->
        transformChildren_ acc (mapper.expr mapper v :: accum)
      | notAList -> Exact (mapper.expr mapper notAList)
    in
    transformChildren_ theList []

  let transformChildrenIfList ~loc ~mapper theList =
    let rec transformChildren_ theList accum =
      (* not in the sense of converting a list to an array; convert the AST
         reprensentation of a list to the AST reprensentation of an array *)
      match theList with
      | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} ->
        Exp.array ~loc (List.rev accum)
      | {
       pexp_desc =
         Pexp_construct
           ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
      } ->
        transformChildren_ acc (mapper.expr mapper v :: accum)
      | notAList -> mapper.expr mapper notAList
    in
    transformChildren_ theList []

  let extractChildren ?(removeLastPositionUnit = false) ~loc propsAndChildren =
    let rec allButLast_ lst acc =
      match lst with
      | [] -> []
      | [(Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})] ->
        acc
      | (Nolabel, _) :: _rest ->
        raise
          (Invalid_argument
             "JSX: found non-labelled argument before the last position")
      | arg :: rest -> allButLast_ rest (arg :: acc)
      [@@raises Invalid_argument]
    in
    let allButLast lst =
      allButLast_ lst [] |> List.rev
      [@@raises Invalid_argument]
    in
    match
      List.partition
        (fun (label, _) -> label = labelled "children")
        propsAndChildren
    with
    | [], props ->
      (* no children provided? Place a placeholder list *)
      ( Exp.construct ~loc {loc; txt = Lident "[]"} None,
        if removeLastPositionUnit then allButLast props else props )
    | [(_, childrenExpr)], props ->
      (childrenExpr, if removeLastPositionUnit then allButLast props else props)
    | _ ->
      raise
        (Invalid_argument "JSX: somehow there's more than one `children` label")
    [@@raises Invalid_argument]

  let unerasableIgnore loc =
    ( {loc; txt = "warning"},
      PStr [Str.eval (Exp.constant (Pconst_string ("-16", None)))] )

  let merlinFocus = ({loc = Location.none; txt = "merlin.focus"}, PStr [])

  (* Helper method to look up the [@react.component] attribute *)
  let hasAttr (loc, _) = loc.txt = "react.component"

  (* Helper method to filter out any attribute that isn't [@react.component] *)
  let otherAttrsPure (loc, _) = loc.txt <> "react.component"

  (* Iterate over the attributes and try to find the [@react.component] attribute *)
  let hasAttrOnBinding {pvb_attributes} =
    find_opt hasAttr pvb_attributes <> None

  (* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
  let rec getFnName binding =
    match binding with
    | {ppat_desc = Ppat_var {txt}} -> txt
    | {ppat_desc = Ppat_constraint (pat, _)} -> getFnName pat
    | _ ->
      raise (Invalid_argument "react.component calls cannot be destructured.")
    [@@raises Invalid_argument]

  let makeNewBinding binding expression newName =
    match binding with
    | {pvb_pat = {ppat_desc = Ppat_var ppat_var} as pvb_pat} ->
      {
        binding with
        pvb_pat =
          {pvb_pat with ppat_desc = Ppat_var {ppat_var with txt = newName}};
        pvb_expr = expression;
        pvb_attributes = [merlinFocus];
      }
    | _ ->
      raise (Invalid_argument "react.component calls cannot be destructured.")
    [@@raises Invalid_argument]

  (* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
  let filenameFromLoc (pstr_loc : Location.t) =
    let fileName =
      match pstr_loc.loc_start.pos_fname with
      | "" -> !Location.input_name
      | fileName -> fileName
    in
    let fileName =
      try Filename.chop_extension (Filename.basename fileName)
      with Invalid_argument _ -> fileName
    in
    let fileName = String.capitalize_ascii fileName in
    fileName

  (* Build a string representation of a module name with segments separated by $ *)
  let makeModuleName fileName nestedModules fnName =
    let fullModuleName =
      match (fileName, nestedModules, fnName) with
      (* TODO: is this even reachable? It seems like the fileName always exists *)
      | "", nestedModules, "make" -> nestedModules
      | "", nestedModules, fnName -> List.rev (fnName :: nestedModules)
      | fileName, nestedModules, "make" -> fileName :: List.rev nestedModules
      | fileName, nestedModules, fnName ->
        fileName :: List.rev (fnName :: nestedModules)
    in
    let fullModuleName = String.concat "$" fullModuleName in
    fullModuleName

  (*
  AST node builders
  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
*)

  (* make record from props and spread props if exists *)
  let recordFromProps ?(removeKey = false) {pexp_loc} callArguments =
    let rec removeLastPositionUnitAux props acc =
      match props with
      | [] -> acc
      | [(Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})] ->
        acc
      | (Nolabel, _) :: _rest ->
        raise
          (Invalid_argument
             "JSX: found non-labelled argument before the last position")
      | prop :: rest -> removeLastPositionUnitAux rest (prop :: acc)
    in
    let props, propsToSpread =
      removeLastPositionUnitAux callArguments []
      |> List.rev
      |> List.partition (fun (label, _) -> label <> labelled "spreadProps")
    in
    let props =
      if removeKey then
        props |> List.filter (fun (arg_label, _) -> "key" <> getLabel arg_label)
      else props
    in
    let fields =
      props
      |> List.map (fun (arg_label, ({pexp_loc} as expr)) ->
             (* In case filed label is "key" only then change expression to option *)
             if isOptional arg_label then
               ( {txt = Lident (getLabel arg_label); loc = pexp_loc},
                 {expr with pexp_attributes = optionalAttr} )
             else ({txt = Lident (getLabel arg_label); loc = pexp_loc}, expr))
    in
    let spreadFields =
      propsToSpread |> List.map (fun (_, expression) -> expression)
    in
    match spreadFields with
    | [] ->
      {pexp_desc = Pexp_record (fields, None); pexp_loc; pexp_attributes = []}
    | [spreadProps] ->
      {
        pexp_desc = Pexp_record (fields, Some spreadProps);
        pexp_loc;
        pexp_attributes = [];
      }
    | spreadProps :: _ ->
      {
        pexp_desc = Pexp_record (fields, Some spreadProps);
        pexp_loc;
        pexp_attributes = [];
      }

  (* make type params for type props<'id, 'name, ...> *)
  let makePropsTypeParamsTvar namedTypeList =
    namedTypeList
    |> List.filter_map (fun (_, label, _, _) ->
           if label = "key" || label = "ref" then None
           else Some (Typ.var label, Invariant))

  (* make type params for make fn arguments *)
  (* let make = ({id, name, children}: props<'id, 'name, 'children>) *)
  let makePropsTypeParams namedTypeList =
    namedTypeList
    |> List.filter_map (fun (_isOptional, label, _, _interiorType) ->
           if label = "key" || label = "ref" then None else Some (Typ.var label))

  (* make type params for make sig arguments *)
  (* let make: React.componentLike<props<string, option<string>>, React.element> *)
  let makePropsTypeParamsSig namedTypeList =
    namedTypeList
    |> List.filter_map (fun (_isOptional, label, _, interiorType) ->
           if label = "key" || label = "ref" then None else Some interiorType)

  (* type props<'id, 'name, ...> = { @optional key: string, @optional id: 'id, ... } *)
  let makePropsRecordType propsName loc namedTypeList =
    let labelDeclList =
      namedTypeList
      |> List.map (fun (isOptional, label, _, _interiorType) ->
             if label = "key" then
               Type.field ~loc ~attrs:optionalAttr {txt = label; loc}
                 (keyType Location.none)
             else if label = "ref" then
               Type.field ~loc
                 ~attrs:(if isOptional then optionalAttr else [])
                 {txt = label; loc} (refType Location.none)
             else if isOptional then
               Type.field ~loc ~attrs:optionalAttr {txt = label; loc}
                 (Typ.var label)
             else Type.field ~loc {txt = label; loc} (Typ.var label))
    in
    (* 'id, 'className, ... *)
    let params = makePropsTypeParamsTvar namedTypeList in
    Str.type_ Nonrecursive
      [
        Type.mk ~loc ~params {txt = propsName; loc}
          ~kind:(Ptype_record labelDeclList);
      ]

  (* type props<'id, 'name, ...> = { @optional key: string, @optional id: 'id, ... } *)
  let makePropsRecordTypeSig propsName loc namedTypeList =
    let labelDeclList =
      namedTypeList
      |> List.map (fun (isOptional, label, _, _interiorType) ->
             if label = "key" then
               Type.field ~loc ~attrs:optionalAttr {txt = label; loc}
                 (keyType Location.none)
             else if isOptional then
               Type.field ~loc ~attrs:optionalAttr {txt = label; loc}
                 (Typ.var label)
             else Type.field ~loc {txt = label; loc} (Typ.var label))
    in
    let params = makePropsTypeParamsTvar namedTypeList in
    Sig.type_ Nonrecursive
      [
        Type.mk ~loc ~params {txt = propsName; loc}
          ~kind:(Ptype_record labelDeclList);
      ]

  let transformUppercaseCall3 ~config modulePath mapper loc attrs callExpression
      callArguments =
    let children, argsWithLabels =
      extractChildren ~loc ~removeLastPositionUnit:true callArguments
    in
    let argsForMake = argsWithLabels in
    let childrenExpr = transformChildrenIfListUpper ~loc ~mapper children in
    let recursivelyTransformedArgsForMake =
      argsForMake
      |> List.map (fun (label, expression) ->
             (label, mapper.expr mapper expression))
    in
    let childrenArg = ref None in
    let args =
      recursivelyTransformedArgsForMake
      @
      match childrenExpr with
      | Exact children -> [(labelled "children", children)]
      | ListLiteral {pexp_desc = Pexp_array list} when list = [] -> []
      | ListLiteral expression -> (
        (* this is a hack to support react components that introspect into their children *)
        childrenArg := Some expression;
        match config.mode with
        | "automatic" ->
          [
            ( labelled "children",
              Exp.apply
                (Exp.ident
                   {txt = Ldot (Lident "React", "array"); loc = Location.none})
                [(Nolabel, expression)] );
          ]
        | _ ->
          [
            ( labelled "children",
              Exp.ident ~loc {loc; txt = Ldot (Lident "React", "null")} );
          ])
    in

    let isCap str =
      let first = String.sub str 0 1 [@@raises Invalid_argument] in
      let capped = String.uppercase_ascii first in
      first = capped
      [@@raises Invalid_argument]
    in
    let ident =
      match modulePath with
      | Lident _ -> Ldot (modulePath, "make")
      | Ldot (_modulePath, value) as fullPath when isCap value ->
        Ldot (fullPath, "make")
      | modulePath -> modulePath
    in
    let isEmptyRecord {pexp_desc} =
      match pexp_desc with
      | Pexp_record (labelDecls, _) when List.length labelDecls = 0 -> true
      | _ -> false
    in

    (* handle key, ref, children *)
    (* React.createElement(Component.make, props, ...children) *)
    match config.mode with
    (* The new jsx transform *)
    | "automatic" ->
      let record = recordFromProps ~removeKey:true callExpression args in
      let props =
        if isEmptyRecord record then recordWithOnlyKey ~loc else record
      in
      let keyProp =
        args |> List.filter (fun (arg_label, _) -> "key" = getLabel arg_label)
      in
      let jsxExpr, key =
        match (!childrenArg, keyProp) with
        | None, (_, keyExpr) :: _ ->
          ( Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsxKeyed")},
            [(nolabel, keyExpr)] )
        | None, [] ->
          (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsx")}, [])
        | Some _, (_, keyExpr) :: _ ->
          ( Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsxsKeyed")},
            [(nolabel, keyExpr)] )
        | Some _, [] ->
          (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsxs")}, [])
      in
      Exp.apply ~loc ~attrs jsxExpr
        ([(nolabel, Exp.ident ~loc {txt = ident; loc}); (nolabel, props)] @ key)
    | _ -> (
      let record = recordFromProps callExpression args in
      (* check if record which goes to Foo.make({ ... } as record) empty or not
         if empty then change it to {key: @optional None} only for upper case jsx
           This would be redundant regarding PR progress https://github.com/rescript-lang/syntax/pull/299
      *)
      let props =
        if isEmptyRecord record then recordWithOnlyKey ~loc else record
      in
      match !childrenArg with
      | None ->
        Exp.apply ~loc ~attrs
          (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "createElement")})
          [(nolabel, Exp.ident ~loc {txt = ident; loc}); (nolabel, props)]
      | Some children ->
        Exp.apply ~loc ~attrs
          (Exp.ident ~loc
             {loc; txt = Ldot (Lident "React", "createElementVariadic")})
          [
            (nolabel, Exp.ident ~loc {txt = ident; loc});
            (nolabel, props);
            (nolabel, children);
          ])
    [@@raises Invalid_argument]

  let transformLowercaseCall3 ~config mapper loc attrs callExpression
      callArguments id =
    let componentNameExpr = constantString ~loc id in
    match config.mode with
    (* the new jsx transform *)
    | "automatic" ->
      let children, nonChildrenProps =
        extractChildren ~removeLastPositionUnit:true ~loc callArguments
      in
      let argsForMake = nonChildrenProps in
      let childrenExpr = transformChildrenIfListUpper ~loc ~mapper children in
      let recursivelyTransformedArgsForMake =
        argsForMake
        |> List.map (fun (label, expression) ->
               (label, mapper.expr mapper expression))
      in
      let childrenArg = ref None in
      let args =
        recursivelyTransformedArgsForMake
        @
        match childrenExpr with
        | Exact children -> [(labelled "children", children)]
        | ListLiteral {pexp_desc = Pexp_array list} when list = [] -> []
        | ListLiteral expression ->
          (* this is a hack to support react components that introspect into their children *)
          childrenArg := Some expression;
          [
            ( labelled "children",
              Exp.apply
                (Exp.ident
                   {txt = Ldot (Lident "React", "array"); loc = Location.none})
                [(Nolabel, expression)] );
          ]
      in
      let isEmptyRecord {pexp_desc} =
        match pexp_desc with
        | Pexp_record (labelDecls, _) when List.length labelDecls = 0 -> true
        | _ -> false
      in
      let record = recordFromProps ~removeKey:true callExpression args in
      let props =
        if isEmptyRecord record then recordWithOnlyKey ~loc else record
      in
      let keyProp =
        args |> List.filter (fun (arg_label, _) -> "key" = getLabel arg_label)
      in
      let jsxExpr, key =
        match (!childrenArg, keyProp) with
        | None, (_, keyExpr) :: _ ->
          ( Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOM", "jsxKeyed")},
            [(nolabel, keyExpr)] )
        | None, [] ->
          (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOM", "jsx")}, [])
        | Some _, (_, keyExpr) :: _ ->
          ( Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOM", "jsxsKeyed")},
            [(nolabel, keyExpr)] )
        | Some _, [] ->
          (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOM", "jsxs")}, [])
      in
      Exp.apply ~loc ~attrs jsxExpr
        ([(nolabel, componentNameExpr); (nolabel, props)] @ key)
    | _ ->
      let children, nonChildrenProps = extractChildren ~loc callArguments in
      let childrenExpr = transformChildrenIfList ~loc ~mapper children in
      let createElementCall =
        match children with
        (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
        | {
         pexp_desc =
           ( Pexp_construct
               ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple _})
           | Pexp_construct ({txt = Lident "[]"}, None) );
        } ->
          "createDOMElementVariadic"
        (* [@JSX] div(~children= value), coming from <div> ...(value) </div> *)
        | _ ->
          raise
            (Invalid_argument
               "A spread as a DOM element's children don't make sense written \
                together. You can simply remove the spread.")
      in
      let args =
        match nonChildrenProps with
        | [_justTheUnitArgumentAtEnd] ->
          [
            (* "div" *)
            (nolabel, componentNameExpr);
            (* [|moreCreateElementCallsHere|] *)
            (nolabel, childrenExpr);
          ]
        | nonEmptyProps ->
          let propsCall =
            Exp.apply ~loc
              (Exp.ident ~loc
                 {loc; txt = Ldot (Lident "ReactDOMRe", "domProps")})
              (nonEmptyProps
              |> List.map (fun (label, expression) ->
                     (label, mapper.expr mapper expression)))
          in
          [
            (* "div" *)
            (nolabel, componentNameExpr);
            (* ReactDOMRe.domProps(~className=blabla, ~foo=bar, ()) *)
            (labelled "props", propsCall);
            (* [|moreCreateElementCallsHere|] *)
            (nolabel, childrenExpr);
          ]
      in
      Exp.apply
        ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
        ~attrs
        (* ReactDOMRe.createElement *)
        (Exp.ident ~loc
           {loc; txt = Ldot (Lident "ReactDOMRe", createElementCall)})
        args
    [@@raises Invalid_argument]

  let rec recursivelyTransformNamedArgsForMake mapper expr args newtypes =
    let expr = mapper.expr mapper expr in
    match expr.pexp_desc with
    (* TODO: make this show up with a loc. *)
    | Pexp_fun (Labelled "key", _, _, _) | Pexp_fun (Optional "key", _, _, _) ->
      raise
        (Invalid_argument
           "Key cannot be accessed inside of a component. Don't worry - you \
            can always key a component from its parent!")
    | Pexp_fun (Labelled "ref", _, _, _) | Pexp_fun (Optional "ref", _, _, _) ->
      raise
        (Invalid_argument
           "Ref cannot be passed as a normal prop. Please use `forwardRef` API \
            instead.")
    | Pexp_fun (arg, default, pattern, expression)
      when isOptional arg || isLabelled arg ->
      let () =
        match (isOptional arg, pattern, default) with
        | true, {ppat_desc = Ppat_constraint (_, {ptyp_desc})}, None -> (
          match ptyp_desc with
          | Ptyp_constr ({txt = Lident "option"}, [_]) -> ()
          | _ ->
            let currentType =
              match ptyp_desc with
              | Ptyp_constr ({txt}, []) ->
                String.concat "." (Longident.flatten txt)
              | Ptyp_constr ({txt}, _innerTypeArgs) ->
                String.concat "." (Longident.flatten txt) ^ "(...)"
              | _ -> "..."
            in
            Location.prerr_warning pattern.ppat_loc
              (Preprocessor
                 (Printf.sprintf
                    "React: optional argument annotations must have explicit \
                     `option`. Did you mean `option(%s)=?`?"
                    currentType)))
        | _ -> ()
      in
      let alias =
        match pattern with
        | {ppat_desc = Ppat_alias (_, {txt}) | Ppat_var {txt}} -> txt
        | {ppat_desc = Ppat_any} -> "_"
        | _ -> getLabel arg
      in
      let type_ =
        match pattern with
        | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
        | _ -> None
      in

      recursivelyTransformNamedArgsForMake mapper expression
        ((arg, default, pattern, alias, pattern.ppat_loc, type_) :: args)
        newtypes
    | Pexp_fun
        ( Nolabel,
          _,
          {ppat_desc = Ppat_construct ({txt = Lident "()"}, _) | Ppat_any},
          _expression ) ->
      (args, newtypes, None)
    | Pexp_fun
        ( Nolabel,
          _,
          {
            ppat_desc =
              Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _);
          },
          _expression ) ->
      (args, newtypes, Some txt)
    | Pexp_fun (Nolabel, _, pattern, _expression) ->
      Location.raise_errorf ~loc:pattern.ppat_loc
        "React: react.component refs only support plain arguments and type \
         annotations."
    | Pexp_newtype (label, expression) ->
      recursivelyTransformNamedArgsForMake mapper expression args
        (label :: newtypes)
    | Pexp_constraint (expression, _typ) ->
      recursivelyTransformNamedArgsForMake mapper expression args newtypes
    | _ -> (args, newtypes, None)
    [@@raises Invalid_argument]

  let argToType types (name, default, _noLabelName, _alias, loc, type_) =
    match (type_, name, default) with
    | Some {ptyp_desc = Ptyp_constr ({txt = Lident "option"}, [type_])}, name, _
      when isOptional name ->
      ( true,
        getLabel name,
        [],
        {
          type_ with
          ptyp_desc =
            Ptyp_constr ({loc = type_.ptyp_loc; txt = optionIdent}, [type_]);
        } )
      :: types
    | Some type_, name, Some _default ->
      ( false,
        getLabel name,
        [],
        {
          ptyp_desc = Ptyp_constr ({loc; txt = optionIdent}, [type_]);
          ptyp_loc = loc;
          ptyp_attributes = [];
        } )
      :: types
    | Some type_, name, _ -> (false, getLabel name, [], type_) :: types
    | None, name, _ when isOptional name ->
      ( true,
        getLabel name,
        [],
        {
          ptyp_desc =
            Ptyp_constr
              ( {loc; txt = optionIdent},
                [
                  {
                    ptyp_desc = Ptyp_var (safeTypeFromValue name);
                    ptyp_loc = loc;
                    ptyp_attributes = [];
                  };
                ] );
          ptyp_loc = loc;
          ptyp_attributes = [];
        } )
      :: types
    | None, name, _ when isLabelled name ->
      ( false,
        getLabel name,
        [],
        {
          ptyp_desc = Ptyp_var (safeTypeFromValue name);
          ptyp_loc = loc;
          ptyp_attributes = [];
        } )
      :: types
    | _ -> types
    [@@raises Invalid_argument]

  let argWithDefaultValue (name, default, _, _, _, _) =
    match default with
    | Some default when isOptional name -> Some (getLabel name, default)
    | _ -> None
    [@@raises Invalid_argument]

  let argToConcreteType types (name, _loc, type_) =
    match name with
    | name when isLabelled name -> (false, getLabel name, [], type_) :: types
    | name when isOptional name -> (true, getLabel name, [], type_) :: types
    | _ -> types

  let transformStructureItem ~config mapper item =
    match item with
    (* external *)
    | {
        pstr_loc;
        pstr_desc =
          Pstr_primitive ({pval_attributes; pval_type} as value_description);
      } as pstr -> (
      match List.filter hasAttr pval_attributes with
      | [] -> [item]
      | [_] ->
        let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
          match ptyp_desc with
          | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
            when isLabelled name || isOptional name ->
            getPropTypes ((name, ptyp_loc, type_) :: types) rest
          | Ptyp_arrow (Nolabel, _type, rest) -> getPropTypes types rest
          | Ptyp_arrow (name, type_, returnValue)
            when isLabelled name || isOptional name ->
            (returnValue, (name, returnValue.ptyp_loc, type_) :: types)
          | _ -> (fullType, types)
        in
        let innerType, propTypes = getPropTypes [] pval_type in
        let namedTypeList = List.fold_left argToConcreteType [] propTypes in
        let retPropsType =
          Typ.constr ~loc:pstr_loc
            (Location.mkloc (Lident "props") pstr_loc)
            (makePropsTypeParams namedTypeList)
        in
        (* type props<'id, 'name> = { @optional key: string, @optional id: 'id, ... } *)
        let propsRecordType =
          makePropsRecordType "props" Location.none
            ((true, "key", [], keyType pstr_loc) :: namedTypeList)
        in
        (* can't be an arrow because it will defensively uncurry *)
        let newExternalType =
          Ptyp_constr
            ( {loc = pstr_loc; txt = Ldot (Lident "React", "componentLike")},
              [retPropsType; innerType] )
        in
        let newStructure =
          {
            pstr with
            pstr_desc =
              Pstr_primitive
                {
                  value_description with
                  pval_type = {pval_type with ptyp_desc = newExternalType};
                  pval_attributes = List.filter otherAttrsPure pval_attributes;
                };
          }
        in
        [propsRecordType; newStructure]
      | _ ->
        raise
          (Invalid_argument
             "Only one react.component call can exist on a component at one \
              time"))
    (* let component = ... *)
    | {pstr_loc; pstr_desc = Pstr_value (recFlag, valueBindings)} -> (
      let fileName = filenameFromLoc pstr_loc in
      let emptyLoc = Location.in_file fileName in
      let mapBinding binding =
        if hasAttrOnBinding binding then
          let bindingLoc = binding.pvb_loc in
          let bindingPatLoc = binding.pvb_pat.ppat_loc in
          let binding =
            {
              binding with
              pvb_pat = {binding.pvb_pat with ppat_loc = emptyLoc};
              pvb_loc = emptyLoc;
            }
          in
          let fnName = getFnName binding.pvb_pat in
          let internalFnName = fnName ^ "$Internal" in
          let fullModuleName =
            makeModuleName fileName config.nestedModules fnName
          in
          let modifiedBindingOld binding =
            let expression = binding.pvb_expr in
            (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
            let rec spelunkForFunExpression expression =
              match expression with
              (* let make = (~prop) => ... *)
              | {pexp_desc = Pexp_fun _} | {pexp_desc = Pexp_newtype _} ->
                expression
              (* let make = {let foo = bar in (~prop) => ...} *)
              | {pexp_desc = Pexp_let (_recursive, _vbs, returnExpression)} ->
                (* here's where we spelunk! *)
                spelunkForFunExpression returnExpression
              (* let make = React.forwardRef((~prop) => ...) *)
              | {
               pexp_desc =
                 Pexp_apply
                   (_wrapperExpression, [(Nolabel, innerFunctionExpression)]);
              } ->
                spelunkForFunExpression innerFunctionExpression
              | {
               pexp_desc =
                 Pexp_sequence (_wrapperExpression, innerFunctionExpression);
              } ->
                spelunkForFunExpression innerFunctionExpression
              | {pexp_desc = Pexp_constraint (innerFunctionExpression, _typ)} ->
                spelunkForFunExpression innerFunctionExpression
              | _ ->
                raise
                  (Invalid_argument
                     "react.component calls can only be on function \
                      definitions or component wrappers (forwardRef, memo).")
              [@@raises Invalid_argument]
            in
            spelunkForFunExpression expression
          in
          let modifiedBinding binding =
            let hasApplication = ref false in
            let wrapExpressionWithBinding expressionFn expression =
              Vb.mk ~loc:bindingLoc
                ~attrs:(List.filter otherAttrsPure binding.pvb_attributes)
                (Pat.var ~loc:bindingPatLoc {loc = bindingPatLoc; txt = fnName})
                (expressionFn expression)
            in
            let expression = binding.pvb_expr in
            let unerasableIgnoreExp exp =
              {
                exp with
                pexp_attributes =
                  unerasableIgnore emptyLoc :: exp.pexp_attributes;
              }
            in
            (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
            let rec spelunkForFunExpression expression =
              match expression with
              (* let make = (~prop) => ... with no final unit *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( ((Labelled _ | Optional _) as label),
                     default,
                     pattern,
                     ({pexp_desc = Pexp_fun _} as internalExpression) );
              } ->
                let wrap, hasUnit, hasForwardRef, exp =
                  spelunkForFunExpression internalExpression
                in
                ( wrap,
                  hasUnit,
                  hasForwardRef,
                  unerasableIgnoreExp
                    {
                      expression with
                      pexp_desc = Pexp_fun (label, default, pattern, exp);
                    } )
              (* let make = (()) => ... *)
              (* let make = (_) => ... *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( Nolabel,
                     _default,
                     {
                       ppat_desc =
                         Ppat_construct ({txt = Lident "()"}, _) | Ppat_any;
                     },
                     _internalExpression );
              } ->
                ((fun a -> a), true, false, expression)
              (* let make = (~prop) => ... *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( (Labelled _ | Optional _),
                     _default,
                     _pattern,
                     _internalExpression );
              } ->
                ((fun a -> a), false, false, unerasableIgnoreExp expression)
              (* let make = (prop) => ... *)
              | {
               pexp_desc =
                 Pexp_fun (_nolabel, _default, pattern, _internalExpression);
              } ->
                if !hasApplication then
                  ((fun a -> a), false, false, unerasableIgnoreExp expression)
                else
                  Location.raise_errorf ~loc:pattern.ppat_loc
                    "React: props need to be labelled arguments.\n\
                    \  If you are working with refs be sure to wrap with \
                     React.forwardRef.\n\
                    \  If your component doesn't have any props use () or _ \
                     instead of a name."
              (* let make = {let foo = bar in (~prop) => ...} *)
              | {pexp_desc = Pexp_let (recursive, vbs, internalExpression)} ->
                (* here's where we spelunk! *)
                let wrap, hasUnit, hasForwardRef, exp =
                  spelunkForFunExpression internalExpression
                in
                ( wrap,
                  hasUnit,
                  hasForwardRef,
                  {expression with pexp_desc = Pexp_let (recursive, vbs, exp)}
                )
              (* let make = React.forwardRef((~prop) => ...) *)
              | {
               pexp_desc =
                 Pexp_apply (wrapperExpression, [(Nolabel, internalExpression)]);
              } ->
                let () = hasApplication := true in
                let _, hasUnit, _, exp =
                  spelunkForFunExpression internalExpression
                in
                let hasForwardRef = isForwardRef wrapperExpression in
                ( (fun exp -> Exp.apply wrapperExpression [(nolabel, exp)]),
                  hasUnit,
                  hasForwardRef,
                  exp )
              | {
               pexp_desc = Pexp_sequence (wrapperExpression, internalExpression);
              } ->
                let wrap, hasUnit, hasForwardRef, exp =
                  spelunkForFunExpression internalExpression
                in
                ( wrap,
                  hasUnit,
                  hasForwardRef,
                  {
                    expression with
                    pexp_desc = Pexp_sequence (wrapperExpression, exp);
                  } )
              | e -> ((fun a -> a), false, false, e)
            in
            let wrapExpression, hasUnit, hasForwardRef, expression =
              spelunkForFunExpression expression
            in
            ( wrapExpressionWithBinding wrapExpression,
              hasUnit,
              hasForwardRef,
              expression )
          in
          let bindingWrapper, _hasUnit, hasForwardRef, expression =
            modifiedBinding binding
          in
          (* do stuff here! *)
          let namedArgList, _newtypes, _forwardRef =
            recursivelyTransformNamedArgsForMake mapper
              (modifiedBindingOld binding)
              [] []
          in
          let namedTypeList = List.fold_left argToType [] namedArgList in
          (* let _ = ref *)
          let vbIgnoreUnusedRef =
            Vb.mk (Pat.any ()) (Exp.ident (Location.mknoloc (Lident "ref")))
          in
          (* let ref = ref->Js.Nullable.fromOption *)
          let vbRefFromOption =
            Vb.mk
              (Pat.var @@ Location.mknoloc "ref")
              (Exp.apply
                 (Exp.ident
                    (Location.mknoloc
                       (Ldot (Ldot (Lident "Js", "Nullable"), "fromOption"))))
                 [(Nolabel, Exp.ident (Location.mknoloc @@ Lident "ref"))])
          in
          let namedArgWithDefaultValueList =
            List.filter_map argWithDefaultValue namedArgList
          in
          let vbMatch (label, default) =
            Vb.mk
              (Pat.var (Location.mknoloc label))
              (Exp.match_
                 (Exp.ident {txt = Lident label; loc = Location.none})
                 [
                   Exp.case
                     (Pat.construct
                        (Location.mknoloc @@ Lident "Some")
                        (Some (Pat.var (Location.mknoloc label))))
                     (Exp.ident (Location.mknoloc @@ Lident label));
                   Exp.case
                     (Pat.construct (Location.mknoloc @@ Lident "None") None)
                     default;
                 ])
          in
          let vbMatchList = List.map vbMatch namedArgWithDefaultValueList in
          (* type props = { ... } *)
          let propsRecordType =
            makePropsRecordType "props" emptyLoc
              (((true, "key", [], keyType emptyLoc) :: namedTypeList)
              @
              if hasForwardRef then [(true, "ref", [], refType Location.none)]
              else [])
          in
          let innerExpression =
            if hasForwardRef then
              Exp.apply
                (Exp.ident @@ Location.mknoloc @@ Lident "make")
                [
                  ( Nolabel,
                    Exp.record
                      [
                        ( Location.mknoloc @@ Lident "ref",
                          Exp.apply ~attrs:optionalAttr
                            (Exp.ident
                               (Location.mknoloc
                                  (Ldot
                                     (Ldot (Lident "Js", "Nullable"), "toOption"))))
                            [
                              ( Nolabel,
                                Exp.ident (Location.mknoloc @@ Lident "ref") );
                            ] );
                      ]
                      (Some (Exp.ident (Location.mknoloc @@ Lident "props"))) );
                ]
            else
              Exp.apply
                (Exp.ident (Location.mknoloc @@ Lident "make"))
                [(Nolabel, Exp.ident (Location.mknoloc @@ Lident "props"))]
          in
          let fullExpression =
            (* React component name should start with uppercase letter *)
            (* let make = { let \"App" = props => make(props); \"App" } *)
            (* let make = React.forwardRef({
                 let \"App" = (props, ref) => make({...props, ref: @optional (Js.Nullabel.toOption(ref))})
               })*)
            Exp.fun_ nolabel None
              (match namedTypeList with
              | [] -> Pat.var @@ Location.mknoloc "props"
              | _ ->
                Pat.constraint_
                  (Pat.var @@ Location.mknoloc "props")
                  (Typ.constr (Location.mknoloc @@ Lident "props") [Typ.any ()]))
              (if hasForwardRef then
               Exp.fun_ nolabel None
                 (Pat.var @@ Location.mknoloc "ref")
                 innerExpression
              else innerExpression)
          in
          let fullExpression =
            match fullModuleName with
            | "" -> fullExpression
            | txt ->
              Exp.let_ Nonrecursive
                [
                  Vb.mk ~loc:emptyLoc
                    (Pat.var ~loc:emptyLoc {loc = emptyLoc; txt})
                    fullExpression;
                ]
                (Exp.ident ~loc:emptyLoc {loc = emptyLoc; txt = Lident txt})
          in
          let rec returnedExpression patterns ({pexp_desc} as expr) =
            match pexp_desc with
            | Pexp_fun
                ( _arg_label,
                  _default,
                  {
                    ppat_desc =
                      Ppat_construct ({txt = Lident "()"}, _) | Ppat_any;
                  },
                  expr ) ->
              (patterns, expr)
            | Pexp_fun (arg_label, _default, {ppat_loc; ppat_desc}, expr) -> (
              if isLabelled arg_label || isOptional arg_label then
                returnedExpression
                  (( {loc = ppat_loc; txt = Lident (getLabel arg_label)},
                     Pat.var
                       ~attrs:
                         (if isOptional arg_label then optionalAttr else [])
                       {txt = getLabel arg_label; loc = ppat_loc} )
                  :: patterns)
                  expr
              else
                (* Special case of nolabel arg "ref" in forwardRef fn *)
                (* let make = React.forwardRef(ref => body) *)
                match ppat_desc with
                | Ppat_var {txt}
                | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _)
                  when txt = "ref" ->
                  returnedExpression
                    (( {loc = ppat_loc; txt = Lident txt},
                       Pat.var ~attrs:optionalAttr {txt; loc = ppat_loc} )
                    :: patterns)
                    expr
                | _ -> returnedExpression patterns expr)
            | _ -> (patterns, expr)
          in
          let patternsWithLid, expression = returnedExpression [] expression in
          let pattern =
            match patternsWithLid with
            | [] -> Pat.any ()
            | _ -> Pat.record (List.rev patternsWithLid) Open
          in
          (* add patttern matching for optional prop value *)
          let expression =
            if List.length vbMatchList = 0 then expression
            else Exp.let_ Nonrecursive vbMatchList expression
          in
          (* add let _ = ref to ignore unused warning *)
          let expression =
            match hasForwardRef with
            | true ->
              let expression =
                Exp.let_ Nonrecursive [vbIgnoreUnusedRef] expression
              in
              Exp.let_ Nonrecursive [vbRefFromOption] expression
            | false -> expression
          in
          let expression =
            Exp.fun_ Nolabel None
              (Pat.constraint_ pattern
                 (Typ.constr ~loc:emptyLoc
                    {txt = Lident "props"; loc = emptyLoc}
                    (makePropsTypeParams namedTypeList)))
              expression
          in
          (* let make = ({id, name, ...}: props<'id, 'name, ...>) => { ... } *)
          let bindings, newBinding =
            match recFlag with
            | Recursive ->
              ( [
                  bindingWrapper
                    (Exp.let_ ~loc:emptyLoc Recursive
                       [
                         makeNewBinding binding expression internalFnName;
                         Vb.mk
                           (Pat.var {loc = emptyLoc; txt = fnName})
                           fullExpression;
                       ]
                       (Exp.ident {loc = emptyLoc; txt = Lident fnName}));
                ],
                None )
            | Nonrecursive ->
              ( [{binding with pvb_expr = expression; pvb_attributes = []}],
                Some (bindingWrapper fullExpression) )
          in
          (Some propsRecordType, bindings, newBinding)
        else (None, [binding], None)
        [@@raises Invalid_argument]
      in
      (* END of mapBinding fn *)
      let structuresAndBinding = List.map mapBinding valueBindings in
      let otherStructures (type_, binding, newBinding)
          (types, bindings, newBindings) =
        let types =
          match type_ with
          | Some type_ -> type_ :: types
          | None -> types
        in
        let newBindings =
          match newBinding with
          | Some newBinding -> newBinding :: newBindings
          | None -> newBindings
        in
        (types, binding @ bindings, newBindings)
      in
      let types, bindings, newBindings =
        List.fold_right otherStructures structuresAndBinding ([], [], [])
      in
      types
      @ [{pstr_loc; pstr_desc = Pstr_value (recFlag, bindings)}]
      @
      match newBindings with
      | [] -> []
      | newBindings ->
        [{pstr_loc = emptyLoc; pstr_desc = Pstr_value (recFlag, newBindings)}])
    | _ -> [item]
    [@@raises Invalid_argument]

  let transformSignatureItem _mapper item =
    match item with
    | {
        psig_loc;
        psig_desc = Psig_value ({pval_attributes; pval_type} as psig_desc);
      } as psig -> (
      match List.filter hasAttr pval_attributes with
      | [] -> [item]
      | [_] ->
        let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
          match ptyp_desc with
          | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
            when isOptional name || isLabelled name ->
            getPropTypes ((name, ptyp_loc, type_) :: types) rest
          | Ptyp_arrow (Nolabel, _type, rest) -> getPropTypes types rest
          | Ptyp_arrow (name, type_, returnValue)
            when isOptional name || isLabelled name ->
            (returnValue, (name, returnValue.ptyp_loc, type_) :: types)
          | _ -> (fullType, types)
        in
        let innerType, propTypes = getPropTypes [] pval_type in
        let namedTypeList = List.fold_left argToConcreteType [] propTypes in
        let retPropsType =
          Typ.constr
            (Location.mkloc (Lident "props") psig_loc)
            (makePropsTypeParamsSig namedTypeList)
        in
        let propsRecordType =
          makePropsRecordTypeSig "props" Location.none
            ((true, "key", [], keyType Location.none) :: namedTypeList)
        in
        (* can't be an arrow because it will defensively uncurry *)
        let newExternalType =
          Ptyp_constr
            ( {loc = psig_loc; txt = Ldot (Lident "React", "componentLike")},
              [retPropsType; innerType] )
        in
        let newStructure =
          {
            psig with
            psig_desc =
              Psig_value
                {
                  psig_desc with
                  pval_type = {pval_type with ptyp_desc = newExternalType};
                  pval_attributes = List.filter otherAttrsPure pval_attributes;
                };
          }
        in
        [propsRecordType; newStructure]
      | _ ->
        raise
          (Invalid_argument
             "Only one react.component call can exist on a component at one \
              time"))
    | _ -> [item]
    [@@raises Invalid_argument]

  let transformJsxCall ~config mapper callExpression callArguments attrs =
    match callExpression.pexp_desc with
    | Pexp_ident caller -> (
      match caller with
      | {txt = Lident "createElement"} ->
        raise
          (Invalid_argument
             "JSX: `createElement` should be preceeded by a module name.")
      (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
      | {loc; txt = Ldot (modulePath, ("createElement" | "make"))} ->
        transformUppercaseCall3 ~config modulePath mapper loc attrs
          callExpression callArguments
      (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
      (* turn that into
         ReactDOMRe.createElement(~props=ReactDOMRe.props(~props1=foo, ~props2=bar, ()), [|bla|]) *)
      | {loc; txt = Lident id} ->
        transformLowercaseCall3 ~config mapper loc attrs callExpression
          callArguments id
      | {txt = Ldot (_, anythingNotCreateElementOrMake)} ->
        raise
          (Invalid_argument
             ("JSX: the JSX attribute should be attached to a \
               `YourModuleName.createElement` or `YourModuleName.make` call. \
               We saw `" ^ anythingNotCreateElementOrMake ^ "` instead"))
      | {txt = Lapply _} ->
        (* don't think there's ever a case where this is reached *)
        raise
          (Invalid_argument
             "JSX: encountered a weird case while processing the code. Please \
              report this!"))
    | _ ->
      raise
        (Invalid_argument
           "JSX: `createElement` should be preceeded by a simple, direct \
            module name.")
    [@@raises Invalid_argument]

  let signature ~config mapper items =
    let items = default_mapper.signature mapper items in
    List.map
      (fun item ->
        if config.version = 4 then transformSignatureItem mapper item
        else [item])
      items
    |> List.flatten
    [@@raises Invalid_argument]

  let structure ~config mapper items =
    let items = default_mapper.structure mapper items in
    List.map
      (fun item ->
        if config.version = 4 then transformStructureItem ~config mapper item
        else [item])
      items
    |> List.flatten
    [@@raises Invalid_argument]

  let expr ~config mapper expression =
    match expression with
    (* Does the function application have the @JSX attribute? *)
    | {pexp_desc = Pexp_apply (callExpression, callArguments); pexp_attributes}
      -> (
      let jsxAttribute, nonJSXAttributes =
        List.partition
          (fun (attribute, _) -> attribute.txt = "JSX")
          pexp_attributes
      in
      match (jsxAttribute, nonJSXAttributes) with
      (* no JSX attribute *)
      | [], _ -> default_mapper.expr mapper expression
      | _, nonJSXAttributes ->
        transformJsxCall ~config mapper callExpression callArguments
          nonJSXAttributes)
    (* is it a list with jsx attribute? Reason <>foo</> desugars to [@JSX][foo]*)
    | {
        pexp_desc =
          ( Pexp_construct
              ({txt = Lident "::"; loc}, Some {pexp_desc = Pexp_tuple _})
          | Pexp_construct ({txt = Lident "[]"; loc}, None) );
        pexp_attributes;
      } as listItems -> (
      let jsxAttribute, nonJSXAttributes =
        List.partition
          (fun (attribute, _) -> attribute.txt = "JSX")
          pexp_attributes
      in
      match (jsxAttribute, nonJSXAttributes) with
      (* no JSX attribute *)
      | [], _ -> default_mapper.expr mapper expression
      | _, nonJSXAttributes ->
        let loc = {loc with loc_ghost = true} in
        let fragment =
          match config.mode with
          | "automatic" ->
            Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsxFragment")}
          | "classic" | _ ->
            Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "fragment")}
        in
        let childrenExpr = transformChildrenIfList ~loc ~mapper listItems in
        let args =
          [
            (nolabel, fragment);
            (match config.mode with
            | "automatic" ->
              ( nolabel,
                Exp.record
                  [
                    ( Location.mknoloc @@ Lident "children",
                      match childrenExpr with
                      | {pexp_desc = Pexp_array children} -> (
                        match children with
                        | [] -> recordWithOnlyKey ~loc:Location.none
                        | [child] -> child
                        | _ -> childrenExpr)
                      | _ -> childrenExpr );
                  ]
                  None )
            | "classic" | _ -> (nolabel, childrenExpr));
          ]
        in
        let countOfChildren = function
          | {pexp_desc = Pexp_array children} -> List.length children
          | _ -> 0
        in
        Exp.apply
          ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
          ~attrs:nonJSXAttributes
          (* ReactDOMRe.createElement *)
          (match config.mode with
          | "automatic" ->
            if countOfChildren childrenExpr > 1 then
              Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsxs")}
            else Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsx")}
          | "classic" | _ ->
            Exp.ident ~loc
              {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
          args)
    (* Delegate to the default mapper, a deep identity traversal *)
    | e -> default_mapper.expr mapper e
    [@@raises Invalid_argument]

  let module_binding ~config mapper module_binding =
    config.nestedModules <- module_binding.pmb_name.txt :: config.nestedModules;
    let mapped = default_mapper.module_binding mapper module_binding in
    config.nestedModules <- List.tl config.nestedModules;
    mapped
    [@@raises Failure]

  (* TODO: some line number might still be wrong *)
  let jsxMapper ~config =
    let module_binding = module_binding ~config in
    let expr = expr ~config in
    (expr, module_binding, transformSignatureItem, transformStructureItem)
    [@@raises Invalid_argument, Failure]
end

let getMapper ~config =
  let expr3, module_binding3, transformSignatureItem3, transformStructureItem3 =
    V3.jsxMapper ~config
  in
  let expr4, module_binding4, transformSignatureItem4, transformStructureItem4 =
    V4.jsxMapper ~config
  in

  let expr mapper e =
    match config.version with
    | 3 -> expr3 mapper e
    | 4 -> expr4 mapper e
    | _ -> default_mapper.expr mapper e
  in
  let module_binding mapper mb =
    match config.version with
    | 3 -> module_binding3 mapper mb
    | 4 -> module_binding4 mapper mb
    | _ -> default_mapper.module_binding mapper mb
  in
  let signature mapper items =
    List.map
      (fun item ->
        (match item.psig_desc with
        | Psig_attribute attr -> V4.processConfigAttribute attr config
        | _ -> ());
        let item = default_mapper.signature_item mapper item in
        if config.version = 3 then transformSignatureItem3 mapper item
        else if config.version = 4 then transformSignatureItem4 mapper item
        else [item])
      items
    |> List.flatten
    [@@raises Invalid_argument]
  in
  let structure mapper items =
    List.map
      (fun item ->
        (match item.pstr_desc with
        | Pstr_attribute attr -> V4.processConfigAttribute attr config
        | _ -> ());
        let item = default_mapper.structure_item mapper item in
        if config.version = 3 then transformStructureItem3 mapper item
        else if config.version = 4 then
          transformStructureItem4 ~config mapper item
        else [item])
      items
    |> List.flatten
    [@@raises Invalid_argument]
  in

  {default_mapper with expr; module_binding; signature; structure}

let rewrite_implementation ~jsxVersion ~jsxModule ~jsxMode
    (code : Parsetree.structure) : Parsetree.structure =
  let config =
    {
      version = jsxVersion;
      module_ = jsxModule;
      mode = jsxMode;
      nestedModules = [];
    }
  in
  let mapper = getMapper ~config in
  mapper.structure mapper code
  [@@raises Invalid_argument, Failure]

let rewrite_signature ~jsxVersion ~jsxModule ~jsxMode
    (code : Parsetree.signature) : Parsetree.signature =
  let config =
    {
      version = jsxVersion;
      module_ = jsxModule;
      mode = jsxMode;
      nestedModules = [];
    }
  in
  let mapper = getMapper ~config in
  mapper.signature mapper code
  [@@raises Invalid_argument, Failure]
