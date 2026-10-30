.PHONY: default build install coq gen run html clean
.DELETE_ON_ERROR:

SOURCES := agda2lambox.cabal $(wildcard cabal.project cabal.project.local) $(shell find src -name '*.hs')
TEST_DIRS := test/untyped test/typed test/agda2rust

default: build

build:
	cabal build

install:
	cabal install --overwrite-policy=always

test: test/Tests.agda

test/Tests.agda: test/Main.hs test/agda2lambox-tests.agda-lib $(TEST_DIRS) $(wildcard $(TEST_DIRS:=/*.agda)) $(SOURCES)
	rm -rf test/dist
	cabal test

coq:
	coq_makefile -f _CoqProject -o CoqMakefile
	make -f CoqMakefile

gen: test
	make -C test gen

run: coq gen
	make -C test run

html: coq test
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
	agda2lambox -o build --typed test/$*.agda
