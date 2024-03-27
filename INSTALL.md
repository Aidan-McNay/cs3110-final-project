# Installation

Users should first follow the instructions from [OCaml Programming](https://cs3110.github.io/textbook/chapters/preface/install.html) (specifically the preface) on installing OCaml. This can be verified by entering into a `utop` session with the following command:

```bash
utop
```

The session can then be exited by typing `#quit;;` and pressing "Enter"

## Packages

In addition to the base OCaml installation, our game requires 3 packages, which can be done with the following commands:

```bash
opam update
opam upgrade
opam install ounit2 bisect_ppx ppx_inline_test
```

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
