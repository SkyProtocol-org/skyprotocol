#!/usr/bin/env bash
set -eux

# Distribute some Ada to offerer and claimant
node send-lovelace.mjs var/admin var/offerer 2500
sleep 5
node send-lovelace.mjs var/admin var/claimant 2500
sleep 5
# Create minting policy
cabal run gen-minting-policy-blueprint -- "$(cat var/admin.pkh)" var/sky-minting-policy.json
cat var/sky-minting-policy.json | jq -r '.validators[0].hash' > var/sky-minting-policy.hash
# Generate Bridge Validator
cabal run gen-validator-blueprint -- "$(cat var/sky-minting-policy.hash)" var/sky-bridge-validator.json
cat var/sky-bridge-validator.json | jq -r '.validators[0].hash' > var/sky-bridge-validator.hash
# Mint the bridge NFT
node mint-nft.mjs var/admin
# Generate Bounty validator with our desired target hash 1111
cabal run gen-bounty-blueprint -- "$(cat var/sky-minting-policy.hash)" 1111 var/sky-bounty-validator.json

echo OK
