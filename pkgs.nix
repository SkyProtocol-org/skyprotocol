let
  pkgs = import <nixpkgs> { inherit config; }; # selfPkgs
  inherit (pkgs) lib;

  config = {
    packageOverrides = superPkgs:
      let superGS = superPkgs.gerbil-support;
          superPPU = superGS.prePackages-unstable;
          inherit (superGS) path-src overrideSrcIfShaDiff gerbilFilterSource gerbilLoadPath
                  gerbilVersionFromGit;
          source = gerbilFilterSource ./.; in
      rec {
        gerbil-support = superGS // {
          inherit pkgs;
          # Skip extra files so we can play with CI configuration yet have the CI reuse cached builds.
          gerbilSkippableFiles = superGS.gerbilSkippableFiles ++
            [".gitlab.yml" ".github" "pkgs.nix" "shell.nix" "default.nix" "docs" "future" "dep"
             "Dockerfile" "Dockerfile.nixos" "sky-install" "ci.ss" ".build"];

          prePackages-unstable =
            let skyprotocol = superPPU.skyprotocol //
                    { pre-src = path-src source; inherit pkgs source;} //
                    gerbilVersionFromGit source "version"; in
            superPPU // { inherit skyprotocol;};};

        glow-lang = gerbil-support.gerbilPackages-unstable.skyprotocol;

        testGerbilLoadPath =
          "${gerbilLoadPath ([glow-lang] ++ skyprotocol.passthru.pre-pkg.gerbilInputs)}:${source}";

      };}; in
  pkgs
