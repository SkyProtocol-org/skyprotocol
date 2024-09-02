{
  inputs = {
    nixpkgs.url = "github:MuKnIO/nixpkgs/devel";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # gerbilTools = pkgs.gerbil-support;
        gPkgs = pkgs.gerbilPackages-unstable;
        gerbilInputs = with gPkgs; [
          gerbil-utils
          gerbil-poo
        ];
        devTools = with pkgs; [
          gerbil-unstable
          python3
        ] ++ gerbilInputs;
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = devTools;
          LD_LIBRARY_PATH = lib.makeLibraryPath devTools;
          shellHook = ''
            echo "Welcome to simple TCP pub/sub server/client"
          '';
        };
      }
    );
}
