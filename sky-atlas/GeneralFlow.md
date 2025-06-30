### Terms
admin - creates minting policy and signs it, bridge
offerer -
claimant - used for bounty blueprint

### First step: Mint & create bridge using the Admin's keys

currency symbol - hash of the sky policy minting

user's change address - addresses from which to spend your money

payment signing key - look at the MeshJS wallet on how to procure one

### Second step: Offer bounty

sends 10 ada to bounty validator
sign with offerrer pkh

### Third step: Offer bounty check

get the address of the validator
get the utxos
check for the amount and unit

### Fourth step: Update bridge

admin updates the bridge
get the address of validator
get the hash of minting policy
call the backend endpoint to update the SkyDa
call the backend endpoint to update the bridge:
  create the BridgeRedeemer
  get the validator utxos
  get the nft utxo
  create the updated datum
  

### Fifth step: Claiming bounty

get the bridge outputs
get the nft
get the bounty outputs & select the first one
create client redeemer with SkyDataProof
create Transaction with value from bounty, the script is the bounty validator and the redeemer that we created earlier

don't consume the nft by making it a reference input

sign it with the recipient wallet

### Sixth step: Verifying bounty claiming

check the length of utxos of bounty validator


