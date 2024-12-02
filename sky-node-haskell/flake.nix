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
        sky-node = final.callCabal2nix "sky-node" ./. { };
        merkle-trie = final.callCabal2nix "merkle-patricia-trie" merkle-trie.src { };
      };
      haskPkgs = pkgs.haskellPackages.extend overlay;
    in
    {
      devShells.default = haskPkgs.shellFor {
        packages = p: [
          p.sky-node
          p.merkle-patricia-trie
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
