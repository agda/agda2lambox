AGDA2LAMBOX_BIN ?= agda2lambox

.PHONY: default

default:
	cabal install --overwrite-policy=always

%.ast:
	$(AGDA2LAMBOX_BIN) -o build test/$*.agda

%.wasm: %.ast
	lbox wasm -o demo/$@ build/$*.ast

%.elm: %.typed
	lbox elm -o demo/$@ build/$*.ast

%.rs: %.typed
	lbox rust -o demo/$@ build/$*.ast

%.v:
	$(AGDA2LAMBOX_BIN) -o build --rocq test/$*.agda

%.typed:
	$(AGDA2LAMBOX_BIN) -o build --typed --no-blocks test/$*.agda
