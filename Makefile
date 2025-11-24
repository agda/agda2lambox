.PHONY: default

default:
	cabal install --overwrite-policy=always

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
