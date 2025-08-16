# Sky Protocol

Sky Protocol is infrastructure for modular blockchains, starting with
a Data Availability Network.

## Haskell Sky Node

### Installation

You should have Nix package manager (1) on your system or NixOS (2) as your OS for this to work:

1. https://github.com/DeterminateSystems/nix-installer
2. https://nixos.org/

If you're using NixOS, make sure to [enable nix flakes](https://nixos.wiki/wiki/Flakes)
if not yet enabled by default by its version of nix.
Determinate Nix has flakes enabled by default.

### Building
(Optional, since `nix run` later in the guide will build everything as needed.)

```bash
  nix build
```

### Automated Tests

To run the automated tests:

```bash
  nix develop
  cabal test
```

## Manual Tests

To run manual tests directly against the Cardano preview network,
see [doc/Preview.md](doc/Preview.md).

## Developing

If you want to participate in active development,
see [doc/HACKING.md](doc/HACKING.md)

## Closing Matters

### Copyright and License

Copyright 2024 Olapa Networks, Inc. All rights reserved.
Sky Protocol is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

### More information

For more information on Sky Protocol, see our [website](https://skyprotocol.org).
