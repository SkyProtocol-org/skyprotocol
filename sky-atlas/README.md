### Haskell Sky Node

### Installation

You should have Nix package manager on your system or NixOS as your OS for this to work:

#### Setting up the binary cache

The binary cache is [sky-protocol](https://sky-protocol.cachix.org). To use it, you should get the `cachix` cli tool and set the authtoken with `cachix authtoken [your auth token]`.
If you intend to push to the cache, you should make sure, that you have the "write" token, which you can get from Fare and that the cachix daemon is running.

#### Building
```
  nix build
```

#### Configuration
For node to work you should have a Maestro account and config the node accordingly:

1) Get the Maestro API token(refer to [Setting up the provider](docs/Providers.md))
2) Copy the `config/example-config.yaml` to the `config/local-test.yaml` and configure appropriately (the config fields have docs in comments)

#### Generating keys
Generate the verification and signing key as well as payment address

Generating keys:
```
  cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey
```

Generating address:
```
  cardano-cli address build \
    --payment-verification-key-file payment.vkey \
    --testnet-magic 2 \
    --out-file payment.addr
```
Note: we're targeting cardano preview network for now, so testnet magic number is 2

You should do this 2 steps 3 times for admin, offerer and claimant. Each set should be in it's own folder with the names specified in the command.

#### Running
```
  nix run -- [path-to-admin-keys] [path-to-offerer-keys] [path-to-claimant-keys]
```

This should compile everything and run the node locally.
