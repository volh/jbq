DUNE = opam exec -- dune
OCAML_COMPILER = ocaml-base-compiler.5.4.1

.PHONY: build
build:
	$(DUNE) build @all

.PHONY: test
test:
	$(DUNE) build @runtest

.PHONY: run
run:
	$(DUNE) exec bin/main.exe -- $(ARGS)

.PHONY: dev
dev:
	$(DUNE) build -w @all

.PHONY: clean
clean:
	$(DUNE) clean

.PHONY: create-switch
create-switch:
	opam switch create . $(OCAML_COMPILER) --deps-only --with-test -y

.PHONY: install-deps
install-deps:
	opam install . --deps-only --with-test -y

.PHONY: init
init: create-switch build

.PHONY: fmt
fmt:
	$(DUNE) build @fmt --auto-promote
