{
  description = "A SkyProtocol Plutarch project";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://cache.zw3rk.com" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/v2.9.2";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    liqwid-libs.url =
      "github:Liqwid-Labs/liqwid-libs";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pkgs = import inputs.nixpkgs-latest { inherit system; };
        in
        {
          pre-commit = {
            settings = {
              src = ./.;
              hooks = {
                commitizen.enable = true;
              };
            };
          };
          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            fourmolu.package = pkgs.haskell.packages.ghc943.fourmolu;
            hlint = { };
            cabalFmt = { };
            hasktags = { };
            applyRefact = { };
            shell = {
              extraCommandLineTools =
                [ pkgs.commitizen ];
            };
            hoogleImage.enable = false;
            enableBuildChecks = true;
            enableHaskellFormatCheck = true;
            # extraHackageDeps = [
            #   "${inputs.liqwid-libs}/plutarch-quickcheck"
            #   "${inputs.liqwid-libs}/plutarch-context-builder"
            #   "${inputs.liqwid-libs}/liqwid-plutarch-extra"
            #   "${inputs.liqwid-libs}/liqwid-script-export"
            #   "${inputs.liqwid-libs}/plutarch-unit"
            #   "${inputs.liqwid-libs.inputs.ply}/ply-core"
            #   "${inputs.liqwid-libs.inputs.ply}/ply-plutarch"
            # ];
          };
          ci.required = [ "all_onchain" ];
        };

      flake.hydraJobs.x86_64-linux = (
        self.checks.x86_64-linux
        // self.packages.x86_64-linux
      );
    };
}
