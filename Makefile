.PHONY: bisect open_bisect

bisect:
	find . -name '*.coverage' | xargs rm -f
	dune test --instrument-with bisect_ppx --force
	bisect-ppx-report html

open_bisect: bisect
	open _coverage/index.html