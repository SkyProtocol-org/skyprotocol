# Using the Preview Network

The instructions below are to run manual tests against the Cardano preview network.

## Setting up SkyDA for the Preview Network

### Get an account on a blockchain access provider

Right now Sky Protocol only supports Maestro as a provider,
with more planned in the future (local privnet, other providers).

To set up the Maestro you need:

1. Go to their [website](https://www.gomaestro.org/) and
   create an account (say you are alone prelaunch for 33333 credits)

2. Create a project with the Cardano "preview" network.

3. Copy `config/example-config.yaml` to `config/local-test.yaml`,
   look for the `EDIT THIS` string, and
   set `atlas.coreProvider.maestroToken` to your Maestro token (between double-quotes),
   and set `atlas.networkId` to `"preview"`.

4. Enjoy access to the network.

### Configure your shell

You can configure your shell with the following commands,
that you can put in your `~/.bashrc` or `~/.zshrc` or repeat in each shell:
```
MAESTRO_TOKEN=O4uu0bsPpWVRqC94HxurfQE0gLqKD7bw # replace with your actual maestro token
MAESTRO_URL="https://preview.gomaestro-api.org/v1"

maestro_get () { curl -X GET -H "api-key: $MAESTRO_TOKEN" "$MAESTRO_URL/$@"; }
maestro_post () { curl -X POST -H "api-key: $MAESTRO_TOKEN" "$MAESTRO_URL/$@"; }

cardano-cli () { nix run .#cardano-cli -- "$@" ; }
```

The latter `cardano-cli` command will only run properly
within the `skyprotocol` directory and its subdirectories, but
is only needed if you don't otherwise have a recent enough `cardano-cli` installed.

Then you can test your access with commands such as:
```bash
maestro_get chain-tip
```

### Generating keys

You can generate the verification key, signing key and payment address
for each participant role in your tests using the following command:

```bash
generate_role_key () {
  role="$1"
  mkdir -p "config/$role"
  cardano-cli address key-gen \
    --verification-key-file "config/$role/payment.vkey" \
    --signing-key-file "config/$role/payment.skey" ;
  cardano-cli address build \
    --payment-verification-key-file "config/$role/payment.vkey" \
    --testnet-magic 2 \
    --out-file "config/$role/payment.addr" ;
}

for i in admin offerer claimant ; do
  generate_role_key $i ;
done
```

NB: we use the testnet magic number `2` for the Cardano preview network.
If you want to adapt to a different network, be sure to use the correct number.

### Get ADA tokens on the Preview Network

You must get ADA tokens on the Preview Network for your participant addresses.

First, get a list of those addresses:
```bash
for i in config/*/payment.addr ; do
  cat $i ; echo ;
done
```

Then use the Cardano preview network faucet to get funds for each of these addresses:
https://faucet.preview.world.dev.cardano.org/basic-faucet

You may have to wait a few minutes between attempts, and/or
you can send funds from one address to the others.

TODO: explain how to send funds from one address to the other.
<!--

with commands such as:
```bash
get_addr () {
  role="$1"
  cat < config/$role/payment.addr
  # TODO: how do we use maestro with such an address??? sed -e 's/^addr_test/addr_vkh/' ???
}

send_preview_ada () {
  from="$1" to="$2" amount="$3"
  cardano-cli -- transaction build \
    --chang-era --testnet-magic 2 \
    --change-address $(get_addr $from) \
    --tx-in $(get_addr $from) \
    --tx-out $(get_addr $to)+${amount} \
    --out-file tx.signed
}

send_preview_ada admin claimant 40000
maestro_post < tx.signed
```
-->

You can view that the addresses were funded by using the explorer at:
https://preview.cexplorer.io/

### Running a local SkyDA test node

This should compile everything and run the SkyDA node _locally_:

```bash
nix run -- ./config/admin/ ./config/offerer/ ./config/claimant/
```

NOTES:
* path to the directory with the keys should include trailing slash
* the order matters

### Checking that your local SkyDA test node is running

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

At this point you should have a running sky node in the terminal.
Start new terminal and run the next commands there.

### Create the Bridge

NOTE: make sure you funded the admin address,
refer to [Getting ADA tokens on the Preview network](#get-ada-tokens-on-the-preview-network)

Start by creating the bridge using admin address:
```bash
bash scripts/create-bridge.sh "admin_addr"
```

The script output would look something like this:
```
  Payload to submit:
  {
    "changeAddr": "[your address here]",
    "usedAddrs": [
      "[your address here]"
    ]
  }
  "85ad195c468431f58148497374061eadb619df0b0220164bbaf3491d2490c1fa"%
```
The last line is the transaction id which you can copy (without quotes) and
insert in the search bar in the [explorer](https://preview.cexplorer.io/) to see the results.

### Populate the DA

The next step will be to populate the DA with data:

For this to happen, you should initialize 2 variables
in your bash session, since the API that we will be
interacting with is protected with basic auth (for now).

```bash
  export API_USER="skyAdmin" API_PASS="1234"
```

Now you can interact with the DA API.

Start with creating a new topic:
```bash
  bash scripts/create-topic.sh
```

The response will include the created Topic ID.

Then populate that topic with some data:
```bash
  # the first argument is the topic id that you get from previous step.
  # we also include some random data in the scripts folder for you to use.
  bash scripts/publish-message.sh 0 scripts/message_to_publish
```

The next step will be to update the bridge:
```bash
  bash scripts/update-bridge.sh "admin-addr"
```

The response will again include the transaction id,
which you can copy-paste into the [explorer](https://preview.cexplorer.io/).
