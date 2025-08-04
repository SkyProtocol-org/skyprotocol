# General Flow for the Demo Script

## Terms
- admin: creates minting policy and signs it, creates bridge
- offerer: creates bounty contract
- claimant: publishes data and claims bounty from contract

## Step-by-Step Flow

### Mint NFT and create Bridge Contract

Sky foundation node connects to Cardano node,
Mint a regular NFT with supply of 1,
to mark the state of the bridge contract.
Generate some initial state for the SkyDA and attach it to the UTXO with the NFT.

- Currency Symbol: hash of the sky policy minting
- user's change address: addresses from which to spend your money
- payment signing key: look at the MeshJS wallet on how to procure one

### Offerer creates Bounty contract

Offerer connects to Cardano node, creates Bounty validating contract on Cardano,
putting 10 ADA in it, that will go to claimant's address
if hash preimage data appears in the DA at given topic before deadline,
or else will revert to offerer after timeout.

Offerer creates a second contract for good measure, with different hash,
so as to let the second one timeout.

### Claimant publishes data on DA

Claimant connects to Sky node, publishes hash preimage data as message
on the DA at the given topic, gets a message ID.

Claimant connects to Cardano node, watches the chain and waits for the DA bridge to be updated.

### DA updates Bridge

Sky node with admin keys connects to Cardano node,
pushes update of DA state onto the bridge contract.

### Claimant sees update on Cardano, asks DA for proof

Claimant see on Cardano node connection that DA bridge state was updated,
gets new state, waits for full confirmation.

Claimant connects to Sky Node, asks for proof of inclusion of his message at given message ID,
gets a proof in return that they can check against the Bridge state.

### Claimant claims Bounty

Claimant connects to Cardano node, publishes the proof onto the Bounty contract.
Contract automatically releases funds to Claimant's address.

Alternatively, second contract times out, and Offerer connect to Cardano node,
exercises timeout onto the Bounty contract, get funds released back to them.

### Offerer gets data from DA

Offerer connects to Cardano node, sees that the bounty was claimed,
extracts the proof from the blockchain.

Offerer connects to Sky node for the topic, queries for the data at given messageID.
