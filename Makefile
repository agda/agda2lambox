.PHONY: default build install html clean

default: build

build:
	cabal build

install:
	cabal install --overwrite-policy=always

test: test/Tests.agda

test/Tests.agda: test/Main.hs test/typed/*.agda test/untyped/*.agda
	cabal test

html: test
	make -C test html

clean:
	rm -rf test/Tests.agda test/dist/

%.ast:
	agda2lambox -o build test/$*.agda

%.wasm: %.ast
	lbox wasm -o demo/$@ build/$*.ast

%.elm: %.typed
	lbox elm -o demo/$@ build/$*.ast

%.rs: %.typed
	lbox rust -o demo/$@ build/$*.ast

%.v:
	agda2lambox -o build --rocq test/$*.agda

%.typed:
	agda2lambox -o build --typed --no-blocks test/$*.agda
