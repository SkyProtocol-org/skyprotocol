let
  pkgs = import <nixpkgs> { inherit config; }; # selfPkgs
  inherit (pkgs) lib;

  config = {
    packageOverrides = superPkgs:
      let
        superGS = superPkgs.gerbil-support;
        superPPU = superGS.prePackages-unstable;
        inherit (superGS) path-src overrideSrcIfShaDiff gerbilFilterSource gerbilLoadPath
          gerbilVersionFromGit;
        source = gerbilFilterSource ./.;
      in
      rec {
        gerbil-support = superGS // {
          inherit pkgs;
          # Skip extra files so we can play with CI configuration yet have the CI reuse cached builds.
          gerbilSkippableFiles = superGS.gerbilSkippableFiles ++
            [
              ".gitlab.yml"
              ".github"
              "pkgs.nix"
              "shell.nix"
              "default.nix"
              "docs"
              "future"
              "dep"
              "Dockerfile"
              "Dockerfile.nixos"
              "sky-install"
              "ci.ss"
              ".build"
            ];

          prePackages-unstable =
            let
              skyprotocol = rec {
                pname = "skyprotocol";
                softwareName = "SkyProtocol";
                gerbil-package = "skyprotocol";
                version-path = "version";
                gerbilInputs = with pkgs.gerbil-support.gerbilPackages-unstable;
                  [
                    gerbil-utils
                    gerbil-crypto
                    gerbil-poo
                    gerbil-persist
                    gerbil-ethereum
                    gerbil-leveldb # gerbil-libp2p
                  ];
                pre-src = path-src source;
                postInstall = ''
                  mkdir -p $out/bin $out/gerbil/lib/skyprotocol
                  cp main.ss $out/gerbil/lib/skyprotocol/
                  cat > $out/bin/sky <<EOF
                  #!/bin/sh
                  ORIG_GERBIL_LOADPATH="\$GERBIL_LOADPATH"
                  ORIG_GERBIL_PATH="\$GERBIL_PATH"
                  ORIG_GERBIL_HOME="\$GERBIL_HOME"
                  unset GERBIL_HOME
                  GERBIL_LOADPATH="${gerbil-support.gerbilLoadPath (["$out"] ++ gerbilInputs)}"
                  SKY_SOURCE="\''${SKY_SOURCE:-$out/share/glow}"
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
              } // gerbilVersionFromGit source "version";
            in
            superPPU // { inherit skyprotocol; };
        };

        skyprotocol = gerbil-support.gerbilPackages-unstable.skyprotocol;

        testGerbilLoadPath =
          "${gerbilLoadPath ([skyprotocol] ++ skyprotocol.passthru.pre-pkg.gerbilInputs)}:${source}";

      };
  };
in
pkgs
