#=========================================================================
# Makefile for CheckCamelMake

.SILENT:

.PHONY: bisect open_bisect clean build run test

.DEFAULT_GOAL := run

clean:
	dune clean

build: clean
	dune build

run: build
	dune exec bin/main.exe

test: build
	dune test --force

bisect:
	find . -name '*.coverage' | xargs rm -f
	dune test --instrument-with bisect_ppx --force
	bisect-ppx-report html

open-bisect: bisect
	open _coverage/index.html

format: clean
	dune build @fmt
	dune promote

line-check: clean
	cloc --by-file --include-lang=OCaml .