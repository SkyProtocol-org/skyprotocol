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

### Manual Tests

The instructions below are to run manual tests against the Cardano preview network.

#### Generating keys for manual tests

_If_ you are going to run _manual_ tests against the cardano preview network,
generate the verification and signing key as well as payment address.
(Automated tests above will generate their own keys against a private node.)

Generating keys:
```bash
  (cd ./config ;
  mkdir -p admin offerer claimant ;
  for i in admin offerer claimant ; do
    nix run .#cardano-cli -- address key-gen \
      --verification-key-file $i/payment.vkey \
      --signing-key-file $i/payment.skey ;
    nix run .#cardano-cli -- address build \
      --payment-verification-key-file $i/payment.vkey \
      --testnet-magic 2 \
      --out-file $i/payment.addr ;
  done
  )
```

Note: we're targeting cardano preview network for now, so we use the testnet magic number, `2`.

#### Get ADA tokens on the preview network

Get each of your admin addresses:
```bash
  for i in config/*/payment.addr ; do
    cat $i ; echo ;
  done
```

Use each of these addresses to get funds at the Cardano preview network faucet:
https://faucet.preview.world.dev.cardano.org/basic-faucet

You may have to wait a few minutes between attempts, and/or
you can send funds from one address to the others (TODO: explain how).

You can view that the addresses were funded by using the explorer at:
https://preview.cexplorer.io/

#### Configuration

For node to work you should have a Maestro account and config the node accordingly:

1) Get the Maestro API token (refer to [Setting up the provider](doc/Providers.md))
2) Copy the `config/example-config.yaml` to the `config/local-test.yaml` and configure appropriately
   (the config fields have docs in comments, see immediately above the two `EDIT THIS` comments)

#### Running Manual Tests

This should compile everything and run the node _locally_:

```bash
  nix run -- ./config/admin/ ./config/offerer/ ./config/claimant/
```

NOTES:
* path to the directory with the keys should include trailing slash
* the order matters

To check if the node is running do the curl request to the health endpoint
```bash
  curl localhost:8080/health
```
You should see `"OK"` as a response and
```
  GET /health
    Accept: */*
    Status: 200 OK 0.00004094s
```
in the node logs (Time can vary).

If everything is okay, go to the [running](/doc/Running.md) for further instructions.

## Closing Matters

### Copyright and License

Copyright 2024 Olapa Networks, Inc. All rights reserved.
Sky Protocol is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

### More information

For more information on Sky Protocol, see our [website](https://skyprotocol.org).
