.PHONY: default build install coq gen run html clean

default: build

build:
	cabal build

install:
	cabal install --overwrite-policy=always

test: test/Tests.agda

test/Tests.agda: test/Main.hs test/agda2lambox-tests.agda-lib test/untyped/*.agda test/typed/*.agda test/agda2rust/*.agda
	cabal test

coq:
	coq_makefile -f _CoqProject -o CoqMakefile
	make -f CoqMakefile

gen: test
	make -C test gen

run: test
	make -C test run

html: test
	make -C test html

clean:
	rm -rf test/Tests.agda test/dist/
	make -C test clean

%.ast:
	agda2lambox -o build test/$*.agda

%.wasm: %.ast
	peregrine wasm -o demo/$@ build/$*.ast

%.elm: %.typed
	peregrine elm -o demo/$@ build/$*.ast

%.rs: %.typed
	peregrine rust -o demo/$@ build/$*.ast

%.v:
	agda2lambox -o build --rocq test/$*.agda

%.typed:
	agda2lambox -o build --typed --no-blocks test/$*.agda
