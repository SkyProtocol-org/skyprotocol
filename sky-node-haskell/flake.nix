{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    merkle-trie = {
      url = "github:Skyprotocol-org/skyprotocol/issue-22/haskell-implementation-of-sky-node?dir=merkle-patricia-trie";
    };
  };
  outputs = { self, nixpkgs, flake-utils, merkle-trie, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      overlay = final: prev: {
        merkle-patricia-trie = prev.callCabal2nix "merkle-patricia-trie" "${merkle-trie}" { };
        sky-node = prev.callCabal2nix "sky-node" ./. { };
      };
      haskPkgs = pkgs.haskellPackages.extend overlay;
    in
    {
      devShells.default = haskPkgs.shellFor {
        packages = p: [
          p.sky-node
        ];
        nativeBuildInputs = with haskPkgs; [
          cabal-install
          haskell-language-server
          hlint
        ];
        shellHook = ''
          echo "Welcome"
        '';
      };
    }
  );
}
