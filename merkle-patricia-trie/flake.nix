{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      overlay = final: prev: {
        merkle-patricia-trie = final.callCabal2nix "merkle-patricia-trie" ./. { };
      };
      haskPkgs = pkgs.haskellPackages.extend overlay;
    in
    {
      packages.default = haskPkgs.merkle-patricia-trie;
      overlays.default = overlay;
      devShells.default = haskPkgs.shellFor {
        packages = p: [
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
