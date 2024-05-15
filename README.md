# CS 3110 Final Project - CheckCamelMate

<p align="center">
  <img src="assets/camel-chess-image.jpg" width=50% style="border-radius: 5%">
</p>

CheckCamelMate is an Ocaml project that allows two users to play chess against one another, according to the rules of chess, using a graphical interface.

## Members

- Aidan McNay ([acm289@cornell.edu](mailto:acm289@cornell.edu))
- Andro Janashia ([aj454@cornell.edu](mailto:aj454@cornell.edu))
- Henry Toll ([hht26@cornell.edu](mailto:hht26@cornell.edu))

## Installation

To install, users should follow the [installation instructions](./INSTALL.md)

## Building

Users can build the project from the top-level directory using Dune with

```bash
dune build
```

Alternatively, we provide a Makefile to run many common commands; users can also run

```bash
make build
```

## Running

Users can begin a game using Dune with

```bash
dune exec bin/main.exe
```

This can also be done with our Makefile with

```bash
make run
```

## Testing

To run our test suite with Dune, users can run

```bash
dune test
```

Alternatively, using the Makefile, users could run

```bash
make test
```

These will run the entire test suite. If users want to see statistics on coverage, they can do so using Bisect; assuming it has been installed according to the installation guide, a coverage report can be generated using

```bash
make bisect
```

This will generate an HTML report at `_coverage/index.html`. On Mac systems, this can be generated and opened simultaneously with the command

```bash
make open-bisect
```

## Lines of Code

One of the main metrics that our project is evaluated on is physical lines of code. A useful report for this can be generated using [cloc](https://github.com/AlDanial/cloc); assuming it has already been installed (likely using [Homebrew](https://formulae.brew.sh/formula/cloc)), one can print a report to the terminal using the command

```bash
make line-check
```
