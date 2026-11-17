# Index file `test/Tests.agda` is generated with `cabal test`
MAIN=Tests
.PHONY : gen html clean

default: html

gen:
	@echo == Compiling to targets ==
	mkdir -p gen/
	./compile.sh

run: # gen
	@echo == Evaluating λ-box output ==
	./runRocq.sh
	@echo == Running targets ==
	./runRust.sh
	./runNative.sh
	@echo == Statistics ==
	./stats.sh

html: # gen run
	@echo == Generating HTML ==
	mkdir -p html/ && cp Agda.css html/ && cp favicon.ico html/
	-cabal run -- agda2lambox --html --css=Agda.css $(MAIN).agda
	./renderTranslations.sh
	./renderStats.sh $(MAIN)

clean:
	rm -rf gen/ html/ dist/*.html dist/*.md gen/*/*.html gen/*/*.md
