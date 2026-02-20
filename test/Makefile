# Index file `test/Tests.agda` is generated with `cabal test`
MAIN=Tests
.PHONY : gen html clean

default: html

gen:
	@echo == Generating WASM/C/OCaml/Cakeml/Rust/Elm ==
	mkdir -p gen/
	./compile.sh

html: gen
	@echo == Generating HTML ==
	mkdir -p html/ && cp Agda.css html/
	-cabal run -- agda2lambox --html --css=Agda.css $(MAIN).agda
	./renderTranslations.sh
	cp html/$(MAIN).html html/index.html

clean:
	rm -rf gen/ html/ dist/*.html dist/*.md gen/*/*.html gen/*/*.md
