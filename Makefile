.PHONY: default build install test coq gen run html clean
.NOTPARALLEL:
.DELETE_ON_ERROR:

SOURCES := agda2lambox.cabal $(wildcard cabal.project cabal.project.local) $(shell find src -name '*.hs')
TEST_DIRS := test/agda2lambox test/agda2rust test/agda2hs

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
	test/targets.sh gen

run: coq gen
	test/targets.sh run

html: test
	test/targets.sh html

clean:
	rm -rf test/Tests.agda test/dist test/gen test/html
