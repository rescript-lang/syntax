SHELL = /bin/bash

build:
	dune build

bootstrap: build
	dune exec -- ./scripts/bootstrap.sh

bench: build
	dune exec -- bench

test:
	dune exec -- tests
	dune exec -- ./test.sh

roundtrip-test:
	dune exec -- tests
	ROUNDTRIP_TEST=1 dune exec -- ./test.sh

reanalyze: build
	reanalyze.exe -all-cmt _build/default -suppress tests

clean:
	dune clean

.PHONY: bench clean test roundtrip-test reanalyze bootstrap build-native
