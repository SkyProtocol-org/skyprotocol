{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      overlay = final: prev: {
        sky-node = final.callCabal2nix "sky-node" ./. { };
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
