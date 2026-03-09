DUNE = opam exec -- dune

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

.PHONY: install-deps
install-deps:
	opam install . --deps-only --with-test -y

.PHONY: fmt
fmt:
	$(DUNE) build @fmt --auto-promote
