{ repoRoot, inputs, pkgs, lib, system }:

cabalProject: {
  name = "sky-atlas";

  packages = [
  ];

  # scripts = {
  #   foo = {
  #      description = "";
  #      group = "general";
  #      enabled = true;
  #      exec = ''
  #        echo "Hello, World!"
  #      '';
  #    };
  # };

  # env = {
  #   KEY = "VALUE";
  # };

  shellHook = ''
    # Custom shellHook
  '';

  preCommit = {
    cabal-fmt.enable = true;
    # stylish-haskell.enable = true;
    fourmolu.enable = true;
    hlint.enable = true;
    # editorconfig-checker.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
 
