# Index file `test/Tests.agda` is generated with `cabal test`
MAIN=Tests
.PHONY : html clean

html:
	@echo == Generating HTML ==
	mkdir -p html/ && cp Agda.css html/
	-cabal run -- agda2lambox \
		--html \
		--html-highlight=code \
		$(MAIN).agda
	./renderTranslations.sh
	cp html/$(MAIN).html html/index.html

clean:
	rm -rf html/ dist/*.html dist/*.md
