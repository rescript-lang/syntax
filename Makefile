SHELL = /bin/bash

build:
	dune build

bootstrap: build
	dune exec -- bash ./scripts/bootstrap.sh

bench: build
	dune exec -- bench

test: reanalyze
	dune exec -- tests
	dune exec -- bash ./scripts/test.sh

roundtrip-test: reanalyze
	dune exec -- tests
	ROUNDTRIP_TEST=1 dune exec -- bash ./scripts/test.sh

reanalyze: build
	reanalyze.exe -all-cmt _build/default -suppress tests,compiler-libs-406 -exclude-paths compiler-libs-406 

clean:
	dune clean

.PHONY: bench clean test roundtrip-test reanalyze bootstrap build-native
