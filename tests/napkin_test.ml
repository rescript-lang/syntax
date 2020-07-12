module IO = Napkin_io

module Snapshot = struct
  let take ~filename ~contents =
    let snapFilename = filename ^ ".snapshot" in
    if Sys.file_exists snapFilename then
      ()
    else
      IO.writeFile ~filename:snapFilename ~contents
    ;
    print_endline (
      "✅ " ^ filename
    )
end

(* test printing of .res file*)
let () =
  let filename = "./tests/api/resSyntax.res" in
  let prettySource = Napkin_multi_printer.print `res ~input:filename in
  assert (
    prettySource = {|// test file

if true {
  Js.log("true")
} else {
  Js.log("false")
}
|}
  )

(* test printing of .resi file*)
let () =
  let filename = "./tests/api/resiSyntax.resi" in
  let prettySource = Napkin_multi_printer.print `res ~input:filename in
  assert (
    prettySource = {|// test interface file

let x: int
|}
  )

let refmtBinaryPath = "./lib/refmt.exe"

(* test printing of reason .re file *)
let () =
  let filename = "./tests/api/reasonSyntax.re" in
  let prettySource = Napkin_multi_printer.print (`refmt refmtBinaryPath) ~input:filename in
  assert (
    prettySource = {|// test .re file
let \"+++" = (a, b) => a + b

let unicode = "🙈 😅 🙌"

let d = `Sehr Schön` /* test */

let () = print_endline("foo")
|}
  )

(* test printing of reason .rei file *)
let () =
  let filename = "./tests/api/reiSyntax.rei" in
  let prettySource = Napkin_multi_printer.print (`refmt refmtBinaryPath) ~input:filename in
  assert (
    prettySource = {|// test .rei file
let x: int
|}
  )

(* test printing of ocaml .ml file *)
let () =
  let filename = "./tests/api/mlSyntax.ml" in
  let prettySource = Napkin_multi_printer.print `ml ~input:filename in
  assert (
    prettySource = {|/* test ml file */

let () = print_endline("hello world")

let unicode = "🙈 😅 🙌"

let d = `Sehr Schön`
|}
  )

(* test printing of ocaml .mli file *)
let () =
  let filename = "./tests/api/mliSyntax.mli" in
  let prettySource = Napkin_multi_printer.print `ml ~input:filename in
  assert (
    prettySource = {|/* test mli file */

let x: int

/* comment */
let y: float
|}
  )

let () = print_endline "✅ multi printer api tests"

module OutcomePrinterTests = struct
  let parseFile filename =
    let result = Napkin_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename in

    if result.Napkin_driver.invalid then (
      prerr_string (
        Napkin_driver.parsingEngine.stringOfDiagnostics
          ~source:(result.source) ~filename:result.filename result.diagnostics
      );
      exit 1
    ) else
     result.Napkin_driver.parsetree


  let outcomeOfStructure structure =
    Lazy.force Napkin_outcome_printer.setup;

    Compmisc.init_path false;
    let env = Compmisc.initial_env () in
    try (
      let (_typedStructure, signature, _newenv) =
        Typemod.type_toplevel_phrase env structure in
      signature
      |> Printtyp.tree_of_signature
      |> (!Oprint.out_signature) Format.str_formatter;
      Format.flush_str_formatter()
    ) with
    | Typetexp.Error (_, _, err) ->
      Typetexp.report_error env Format.str_formatter err;
      prerr_string (Format.flush_str_formatter ());
      exit 1;
    | _ ->
      prerr_string "Unknown error while trying to print outcome tree";
      exit 1

  let run () =
    let testFileName = "tests/oprint/oprint.res" in
    let printedOutcomeTree =
      parseFile testFileName |> outcomeOfStructure
    in
    Snapshot.take ~filename:testFileName ~contents:printedOutcomeTree
end

let () = OutcomePrinterTests.run()
