SHELL = /bin/bash

build:
	dune build

bootstrap: build
	dune exec -- bash ./scripts/bootstrap.sh

bench: build
	dune exec -- bench

test: reanalyze
	dune exec -- testrunner
	dune exec -- bash ./scripts/test.sh

roundtrip-test: reanalyze
	dune exec -- testrunner
	ROUNDTRIP_TEST=1 dune exec -- bash ./scripts/test.sh

reanalyze: build
	reanalyze.exe -set-exit-code -all-cmt _build/default -suppress testrunner,compiler-libs-406 -exclude-paths compiler-libs-406 

format:
	dune build @fmt --auto-promote

clean:
	dune clean

.PHONY: bench clean test roundtrip-test reanalyze bootstrap build-native
