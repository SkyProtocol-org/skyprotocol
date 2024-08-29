
.PHONY: help
help:
	@echo "use: make <command>"
	@echo
	@echo "Where <command>:"
	@echo "  help       -- show this help"
	@echo "  shell      -- opens a development shell for the smart contract project"
	@echo "  code       -- start a vscode instance in the nix development shell"
	@echo "  clean      -- cabal clean"
	@echo "  build      -- build the smart contracts"
	@echo "  test       -- run the smart contract tests"
	@echo "  ci         -- run ci job"
	@echo "  export     -- exports the contracts to the exported directory"

.PHONY: shell
shell:
	nix develop --extra-experimental-features ca-derivations

.PHONY: code
code:
	nix-shell --command "code ."

.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build

.PHONY: test
test:
	cabal test --test-show-details=direct

.PHONY: ci
ci:
	nix flake check --accept-flake-config --extra-experimental-features ca-derivations

.PHONY: export
export:
	cabal run skyprotocol-cardano -f optimized
