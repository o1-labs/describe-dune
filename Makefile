all: describe-dune describe-dune.1

PACKAGES = cmdliner parsexp yojson stdio base

COMP ?= ocamlopt

PREFIX ?= /usr/local

describe-dune: src/describe_dune.ml
	ocamlfind $(COMP) $(patsubst %,-package %,$(PACKAGES)) -linkpkg $^ -o $@

describe-dune.1: describe-dune
	./$< --help=groff >$@

install: describe-dune describe-dune.1 describe-dune.install
	opam-installer --prefix=$(PREFIX) describe-dune.install

clean:
	rm -f src/*.cm* src/*.o

distclean: clean
	rm -f describe-dune describe-dune.1 describe-dune.install
