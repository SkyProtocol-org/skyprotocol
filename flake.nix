{
  description = "A flake for Sky Protocol";

  inputs = {
    nixpkgs.url = "github:MuKnIO/nixpkgs/devel";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # useful tools to build gerbil packages
        gerbilTools = pkgs.gerbil-support;
        # gerbil packages
        gPkgs = pkgs.gerbilPackages-unstable;

        gerbilInputs = with gPkgs; [
          gerbil-utils
          gerbil-crypto
          gerbil-poo
          gerbil-persist
          gerbil-leveldb
          # gerbil-ethereum
          # gerbil-libp2p
        ];
        devTools = with pkgs; [
          gerbil-unstable
        ] ++ gerbilInputs
          # ++ self.packages.${system}.skyprotocol.buildInputs
        ;
      in
      with pkgs;
      {
        overlays.default = (final: prev: {
          skyprotocol = self.packages.${final.system}.skyprotocol;
          legacyPackages.${final.system}.gerbilPackages-unstable = prev.legacyPackages.${final.system}.gerbilPackages-unstable // {
            skyprotocol = self.packages.${final.system}.skyprotocol;
          };
        });
        packages = {
          skyprotocol = gerbilTools.gerbilPackage {
            pname = "skyprotocol";
            version = "0.1";
            softwareName = "SkyProtocol";
            gerbil-package = "skyprotocol";
            version-path = "version";

            pre-src = gerbilTools.path-src (gerbilTools.gerbilFilterSource ./.);

            gerbilInputs = gerbilInputs;

            postInstall = ''
              mkdir -p $out/bin $out/gerbil/lib/skyprotocol
              cp main.ss $out/gerbil/lib/skyprotocol/
              cat > $out/bin/sky <<EOF
              #!/bin/sh
              ORIG_GERBIL_LOADPATH="\$GERBIL_LOADPATH"
              ORIG_GERBIL_PATH="\$GERBIL_PATH"
              ORIG_GERBIL_HOME="\$GERBIL_HOME"
              unset GERBIL_HOME
              GERBIL_LOADPATH="${gerbilTools.gerbilLoadPath (["$out"] ++ gerbilInputs)}"
              SKY_SOURCE="\''${SKY_SOURCE:-$out/share/sky}"
              GERBIL_PATH="\$HOME/.cache/sky/gerbil"
              export GERBIL_PATH GERBIL_LOADPATH SKY_SOURCE ORIG_GERBIL_PATH ORIG_GERBIL_LOADPATH ORIG_GERBIL_HOME
              exec ${pkgs.gerbil-unstable}/bin/gxi $out/gerbil/lib/skyprotocol/main.ss "\$@"
              EOF
              chmod a+x $out/bin/sky
            '';

            meta = with lib; {
              description = "Sky Protocol: Data Availability for safe modular Decentralized Applications (DApps)";
              homepage = "https://skyprotocol.org";
              license = licenses.asl20;
              platforms = platforms.unix;
              maintainers = with maintainers; [ fare ];
            };

          };

          default = self.packages.${system}.skyprotocol;
        };

        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.skyprotocol}/bin/sky";
          };
        };

        # For use by `nix develop`
        devShells.default = mkShell {
          buildInputs = devTools;
          LD_LIBRARY_PATH = lib.makeLibraryPath devTools;
          shellHook = ''
            echo "Welcome to Sky Protocol"
            ${self.packages.${system}.skyprotocol.postConfigure}
            PATH="${self.packages.${system}.skyprotocol.out}/bin:$PATH"
            GERBIL_APPLICATION_HOME="$PWD"
            GERBIL_APPLICATION_SOURCE="$PWD"
          '';
        };

      }
    );
}
