#!/bin/zsh

# pack and parse the whole codebase using the compiler itself. Kind of a test
ocaml unix.cma ./scripts/bspack.ml -bs-main Res_cli -I cli -I src -o ./lib/rescript.ml
rescript ./lib/rescript.ml > ./lib/rescript.res
ocamlopt.opt -w a -pp "rescript -print binary" -O2 -o rescript -I +compiler-libs ocamlcommon.cmxa -I lib -impl ./lib/rescript.res
