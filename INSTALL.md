# Installation

Users should first follow the instructions from [OCaml Programming](https://cs3110.github.io/textbook/chapters/preface/install.html) (specifically the preface) on installing OCaml. This can be verified by entering into a `utop` session with the following command:

```bash
utop
```

The session can then be exited by typing `#quit;;` and pressing "Enter"

## Packages

In addition to the base OCaml installation, our game requires 4 packages, 3 of which can be installed with the following commands:

```bash
opam update
opam upgrade
opam install ounit2 bisect_ppx ppx_inline_test
```

For our final package, Bogue, we found that the distributed version caused a segmentation fault on some MacOS machines due to a call to the underlying `Tsdl` library containing a mis-wrapped SDL binding in C. To fix this, we created our own fork of Bogue, which can be installed from our secondary repository:

```bash
git clone git@github.com:Aidan-McNay/bogue.git
cd bogue
opam pin add .
```

If you find that Bogue works for you as normal (no segmentation faults), or that [the particular issue](https://discuss.ocaml.org/t/ann-bogue-the-ocaml-gui/9099/60) has been resolved, Bogue could instead be installed as normal using `opam install bogue`

_(The first two, `ounit2` and `bisect_ppx`, are installed with the textbook's installation, but are replicated here for completeness.)_

# Building

Our game can be built by navigating to the top-level directory and running

```bash
dune build
```

Once the game is built, it can be run with the following command

```bash
dune exec bin/main.exe
```

From here, users can follow the in-game prompts, supplying moves (in the given format) as desired to move chess pieces around the board.
