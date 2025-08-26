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
   Maestro will generate a token for you, a 32-character string such as
   `O4uu0bsPpWVRqC94HxurfQE0gLqKD7bw` — copy it.

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

get_addr () { cat config/$1/payment.addr; }

cardano-cli () { nix run .#cardano-cli -- "$@" ; }
```

The `maestro_get` and `maestro_post` commands send data to Maestro.

The `generate_role_key` command will generate a test key for a given named role (see below),
while the `get_addr` command will extract the address for that role.

The `cardano-cli` is a version of cardano-cli that runs well with Sky Protocol,
but will only run properly within the `skyprotocol` directory and its subdirectories.
You don't need it if you already have a recent enough `cardano-cli` installed.

Then you can test your access to Maestro with:
```bash
maestro_get chain-tip
```

### Generating keys

You can generate the verification key, signing key and payment address
for each participant role in your tests using the following function
that you may invoke for each role:

```bash
for role in admin offerer claimant ; do
  generate_role_key $role ;
done
```

NB: we use the testnet magic number `2` for the Cardano preview network.
If you want to adapt to a different network, be sure to use the correct magic number.

The keys for a role are stored in files in a role-named subdirectory of the `config/` directory,
with `payment.skey` for the (secret) signing key, `payment.vkey` for the (public) verification key,
and `payment.addr` for the address (hash of the verification key);
e.g. `config/admin/payment.addr` for the `admin` address.
Be careful that the `generate_role_key` command will overwrite any existing files,
making the (test) tokens inaccessible if you don't have a backup.
If you want to generate a new admin account but key access to the previous tokens,
rename the old `admin` directory away to a new name (e.g. `admin.0` and such).

After generating a key, you can refer to the corresponding address with commands like:
```bash
get_addr admin
```

### Get ADA tokens on the Preview Network

You must get ADA tokens on the Preview Network for your participant addresses.

First, get a list of those addresses:
```bash
for role in admin offerer claimant ; do
  echo "$role: $(get_addr $role)"
done
```

For instance, in one test run, the following addresses were generated for me:
```
admin: addr_test1vzjfhrxw8cuh0y0u0mp3tgv6qkgrv0wzwkyvwqcl0dlmn8que9j3x
offerer: addr_test1vq7n4lzvn6dcq4v9g2trt8gavywkg2m49wgw00n4jdlpewctkep2n
claimant: addr_test1vrzpm7yp4sjk3cp5r5ufvh54exlqm9n2n76thwup5vfh64cgtkrkk
```

Then use the Cardano preview network faucet to get funds for each of these addresses:
https://faucet.preview.world.dev.cardano.org/basic-faucet

You may have to wait a few minutes between attempts, and/or
you can send funds from one address to the others.

You can view that the addresses were funded by using the explorer at:
https://preview.cexplorer.io/

For instance, funding the above admin address, I got a JSON response:
```
{"amount":{"lovelace":10000200000},
 "txid":"1f40c3e4df93e579338c28894a33b6056093379d574b3a456c46c0633304fd23",
 "txin":"d47861845f454490f842f7957960bac07e648164d3724c66c2edbdd544811d30#12"}
```

And indeed looking at this transaction in the explorer, we can see the account funded:
https://preview.cexplorer.io/tx/1f40c3e4df93e579338c28894a33b6056093379d574b3a456c46c0633304fd23
https://preview.cexplorer.io/address/addr_test1vzjfhrxw8cuh0y0u0mp3tgv6qkgrv0wzwkyvwqcl0dlmn8que9j3x

<!--
TODO: explain how to send funds from one address to the other
so you don't need to use the faucet more than once and wait for it.

We just added a "transfer" API for that.
-->

### Running a local SkyDA test node

This should compile everything and run the SkyDA node _locally_:
```bash
nix run . -- ./config/admin/ ./config/offerer/ ./config/claimant/
```

Or if you're developing, you may prefer to run it from within a `nix develop` shell with:
```bash
cabal run sky-atlas -- ./config/admin/ ./config/offerer/ ./config/claimant/
```

NOTES:
* path to the directory with the keys should include trailing slash
* the order matters

The server will keep running in its terminal to display its execution logs,
so you will want to start another terminal (or tab)
for the client, and use `nix develop` for that client, too, if you are developing.
Then you will want to configure your shell client with the same definitions as above,
if you didn't put them directly in your shell configuration file.

### Checking that your local SkyDA test node is running

To check if the node is running do the curl request to the health endpoint,
issue the command in the client terminal:
```bash
curl localhost:8080/health ; echo
```

You should see `"OK"` as a response on the client terminal,
and in the server terminal, you will should the following server logs (query duration can vary):
```
GET /health
  Accept: */*
  Status: 200 OK 0.00004094s
```

At this point you should have a running Sky node.

### Create the Bridge

NOTE: make sure you funded the admin address,
refer to [Getting ADA tokens on the Preview network](#get-ada-tokens-on-the-preview-network)

Start by creating the bridge with the admin address (using the functions defined earlier,
that must be defined in your client):
```bash
bash scripts/create-bridge.sh "$(get_addr admin)"
```

The script will issue a transaction on the Preview network, which may take up to 13-25 seconds,
and the output would look something like this (from the same test execution):
```
Payload to submit:
{
  "changeAddr": "addr_test1vzjfhrxw8cuh0y0u0mp3tgv6qkgrv0wzwkyvwqcl0dlmn8que9j3x",
  "usedAddrs": [
    "addr_test1vzjfhrxw8cuh0y0u0mp3tgv6qkgrv0wzwkyvwqcl0dlmn8que9j3x"
  ]
}
"97559b99eca31ecb934315331be8ea41b1fbe88a750e3c52f132b72937dd47b7"
```
The last line, printed when the transaction is complete, is the transaction id,
which you can copy (without quotes) and
insert in the search bar in the [explorer](https://preview.cexplorer.io/) to see the results.
(e.g. https://preview.cexplorer.io/tx/97559b99eca31ecb934315331be8ea41b1fbe88a750e3c52f132b72937dd47b7
for the execution above).

_PS_: The bridge is identified by a NFT that is determined by the admin address.
You cannot re-create the NFT, and thus cannot create a new bridge with the same admin address.
In theory, given committee signatures, you could recycle the bridge and the NFT,
but we haven't written code for that yet.
In production, the bridge will only be created once.
[Don't mind the small amount of ADA left on the bridge; see issue #66 for that.
https://github.com/SkyProtocol-org/skyprotocol/issues/66]

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
A new Topic ID is created each time, simply incrementing from 0.

You may then populate your topics with message data, with commands as follow,
wherein the first argument is the Topic ID as above, and the second argument is a file name:
```bash
bash scripts/publish-message.sh 0 scripts/message_to_publish
```

You will get an answer like:
```
Published message_id: 0
Published message hash: 58b5cf2999c3a08d032c4d57368a5b659aae66200cef25f0054b472cc67aa5e9
```

You can repeat multiple times, adding new messages to the DA each time.
It doesn't have to be the same message; it can be anything.

The next step will be to update the bridge with the new state of the DA:
```bash
bash scripts/update-bridge.sh $(get_addr admin)
```

The response will again include the transaction id,
which you can copy-paste into the [explorer](https://preview.cexplorer.io/).

For instance, after publishing a bunch of messages on bunch of topics,
I had the following transaction
(as usual, the txid was printed when confirmed, after a few seconds):
```
Payload to submit:
{
  "changeAddr": "addr_test1vzjfhrxw8cuh0y0u0mp3tgv6qkgrv0wzwkyvwqcl0dlmn8que9j3x",
  "usedAddrs": [
    "addr_test1vzjfhrxw8cuh0y0u0mp3tgv6qkgrv0wzwkyvwqcl0dlmn8que9j3x"
  ]
}
"135d23dd85d1d2d9aec702fb9d63e6931525ec203e6a3cbedfb92982c62927c5"
```
And you can see the transaction at:
https://preview.cexplorer.io/tx/135d23dd85d1d2d9aec702fb9d63e6931525ec203e6a3cbedfb92982c62927c5
See how one UTXO carries the state of the bridge,
with the input UTXO previously holding the bridge state losing its SkyBridge NFT (-1)
while the output UTXO now holding the bridge state gains this SkyBridge NFT (1).
The data attached is the new root hash of the DA from which all can be verified.

<!-- TODO Complete this section:

Now you can offer a bounty:
```bash
topicId=0 deadline=1000000 amount=42666007
bash scripts/offer-bounty.sh $topicId $(cat scripts/message_hash) $deadline $amount $(get_addr offerer)
```

The `topicId` is the one you got from creating topic response.
`messageHash` can be found in the `scripts/message_hash`
(or computing the Blake2b_256 digest of the message for which you offer the bounty).
`deadline` is a number of slots (each about 13s) that the offer is active for
(You can safely input some big number here if you're testing the positive case,
a small number if you're testing the negative case).
`amount` is an amount that you want to offer in Lovelace (1e-6 ADA).
[Nit: some data about the offerer and claimant is currently being taken
from the server command line invocation instead of arguments plus a wallet;
that's OK, that's an easy issue we'll fix shortly;
what matters is that the contract invocation works great, which is what we're demoing.]

In my example run, I get:
```
Payload to submit:
{
  "topicId": {
    "topic_id": 0
  },
  "messageHash": {
    "hash": "58b5cf2999c3a08d032c4d57368a5b659aae66200cef25f0054b472cc67aa5e9"
  },
  "deadline": 10000,
  "changeAddr": "addr_test1vq7n4lzvn6dcq4v9g2trt8gavywkg2m49wgw00n4jdlpewctkep2n",
  "usedAddrs": [
    "addr_test1vq7n4lzvn6dcq4v9g2trt8gavywkg2m49wgw00n4jdlpewctkep2n"
  ],
  "amount": 42
}
["164122e5b1f3ddc16ae888793d93f42c3aee368ab11d51c046a15b7f99b21285",89584206]
```
As you see this transaction creates a contract with the reward amount in the contract:
https://preview.cexplorer.io/tx/3ea29aaa145ee45ece13c52a705f58f52734a7de0271ffcd3c4cc5259547be39

To claim a bounty:
```bash
bash scripts/claim-bounty.sh $topicId $messageId $(cat scripts/message_hash) $deadlineStart $deadline $(get_addr claimant)
```
The `topicId` is the one you got from creating topic response.
`messageId` is the one you got from publishing a message.
`messageHash` can be found in the `scripts/message_hash`.
`deadlineStart` is a slot where the offering of bounty happends. You should get this as the second output from `scripts/offer-bounty.sh` (in the above case, 89563139).
`deadline` is a number of slots that the offer is active for.
NOTE: `topicId`, `messageHash` and `deadline` should be the same, as inthe call to offer a bounty!.
-->
