### Haskell Sky Node

### Installation

You should have Nix package manager on your system or NixOS as your OS for this to work:

#### Setting up the binary cache

The binary cache is [sky-protocol](https://sky-protocol.cachix.org).
To use it, you should get the `cachix` cli tool and set the authtoken with
`cachix authtoken $YOUR_AUTH_TOKEN`.

If you are a team member and want to push to the cache, you should make sure
that you have a `write` token, which you can get from Fare,
and that the cachix daemon is running (somehow Yaroslav had to `cachix daemon run sky-protocol`).

#### Building
```
nix build
```

#### Generating keys for the tests
Generate the verification and signing key as well as payment address

Generating keys:
```
(cd tests/OffChain/ ;
mkdir -p admin offerer claimant ;
for i in admin offerer claimant ; do
  cardano-cli address key-gen \
    --verification-key-file $i/payment.vkey \
    --signing-key-file $i/payment.skey ;
  cardano-cli address build \
    --payment-verification-key-file $i/payment.vkey \
    --testnet-magic 2 \
    --out-file $i/payment.addr ;
done
)
```

Note: we're targeting cardano preview network for now, so we use the testnet magic number, `2`.

#### Get ADA tokens on the preview network

Get each of your admin addresses:
```
for i in tests/OffChain/*/payment.addr ; do
  cat $i ; echo ;
done
```

Use each of these addresses to get funds at the faucet:
https://faucet.preview.world.dev.cardano.org/basic-faucet

You may have to wait a few minutes between attempts, and/or
you can send funds from one address to the others (TODO: explain how).

You can view that the addresses were funded by using the explorer at:
https://preview.cexplorer.io/

#### Configuration

For node to work you should have a Maestro account and config the node accordingly:

1) Get the Maestro API token (refer to [Setting up the provider](docs/Providers.md))
2) Generate keys for the tests as below
3) Copy the `config/example-config.yaml` to the `config/local-test.yaml` and configure appropriately
   (the config fields have docs in comments, see immediately above the two `EDIT THIS` comments)

#### Running
```
nix run -- tests/OffChain/admin/ tests/OffChain/offerer/ tests/OffChain/claimant/
```

This should compile everything and run the node locally.
