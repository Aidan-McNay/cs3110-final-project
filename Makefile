#=========================================================================
# Makefile for CheckCamelMake

.SILENT:

.PHONY: bisect open_bisect clean build run test

.DEFAULT_GOAL := run

clean:
	dune clean

build:
	dune build

run: build
	dune exec bin/main.exe

test:
	dune test --force

bisect:
	find . -name '*.coverage' | xargs rm -f
	dune test --instrument-with bisect_ppx --force
	bisect-ppx-report html

open_bisect: bisect
	open _coverage/index.html