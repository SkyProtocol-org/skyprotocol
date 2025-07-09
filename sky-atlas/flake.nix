{

  description = "sky-atlas";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        # TODO: get overlays in their own file and leave only the skyAtlas config here
        overlay = final: prev: {
          cardano-cli = with final; stdenv.mkDerivation rec {
            pname = "cardano-cli";
            version = "10.11.0.0";

            src = fetchurl {
              url = "https://github.com/IntersectMBO/cardano-cli/releases/download/cardano-cli-${version}/cardano-cli-${version}-${system}.tar.gz";
              sha256 = "sha256-bPc3a5NWX+GzVgSlKk2XoE7OhMZZ78wq5olSZr2J6OI=";
            };

            nativeBuildInputs = [ autoPatchelfHook ];
            buildInputs = [ zlib ];

            sourceRoot = ".";

            installPhase = ''
              mkdir -p $out/bin
              cp "cardano-cli-${system}" $out/bin/cardano-cli
              chmod +x $out/bin/cardano-cli
            '';
          };
          haskell-nix = prev.haskell-nix // {
            extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
              # String pkgconfig-depends names are mapped to lists of Nixpkgs
              # package names
              "libblst" = [ "blst" ];
            };
          };
        };

        overlays = [
          haskellNix.overlay
          (final: prev: {
            webkitgtk = final.webkitgtk_4_0;
            libsodium =
              with final; stdenv.mkDerivation rec {
                pname = "libsodium";

                src = fetchGit {
                  url = "https://github.com/IntersectMBO/libsodium";
                  rev = version;
                };
                version = "dbb48cce5429cb6585c9034f002568964f1ce567";

                nativeBuildInputs = [ autoreconfHook ];

                configureFlags = [ "--enable-static" ]
                  # Fixes a compilation failure: "undefined reference to `__memcpy_chk'". Note
                  # that the more natural approach of adding "stackprotector" to
                  # `hardeningDisable` does not resolve the issue.
                  ++ lib.optional stdenv.hostPlatform.isMinGW "CFLAGS=-fno-stack-protector";

                outputs = [ "out" "dev" ];
                separateDebugInfo = stdenv.isLinux && stdenv.hostPlatform.libc != "musl";

                enableParallelBuilding = true;

                doCheck = true;

                meta = with lib; {
                  description = "A modern and easy-to-use crypto library - VRF fork";
                  homepage = "http://doc.libsodium.org/";
                  license = licenses.isc;
                  maintainers = [ "tdammers" "nclarke" ];
                  platforms = platforms.all;
                };
              };
          })
          (final: prev: {
            skyAtlasProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc966";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                  fourmolu = { };
                  cabal-fmt = { };
                };

                # Non-Haskell shell tools go here
                shell.buildInputs = with final; [
                  nixpkgs-fmt
                  cardano-cli
                ];

                # ???: Fix for `nix flake show --allow-import-from-derivation`
                evalSystem = "x86_64-linux";
                inputMap = { "https://chap.intersectmbo.org/" = CHaP; };

                # modules = with final; [{
                #   packages.postgresql-libpq-configure.components.library.libs = lib.mkForce [ [ postgresql ] ];
                # }];

              };
          })
          overlay
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.skyAtlasProject.flake { };
      in
      flake // {
        legacyPackages = pkgs;
        # Built by `nix build .`
        packages.default = flake.packages."sky-atlas:exe:sky-atlas";
      });

  nixConfig = {
    allow-import-from-derivation = "true";
    extra-substituters =
      [
        "https://sky-protocol.cachix.org"
      ];
    extra-trusted-public-keys =
      [
        "sky-protocol.cachix.org-1:dI30Dt7GXBzRrn7dhO+VpEAvFBq7XmioeNEhFXv6AR4="
      ];
  };
}
