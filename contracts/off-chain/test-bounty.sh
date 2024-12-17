#!/usr/bin/env bash
set -eux

# Create a bounty offer that will be paid out if claimant can prove
# that the hash CAFE is stored in topic 00 the DA layer.  This
# locks some Ada at the contract.

node offer-bounty.mjs var/offerer

# Verify that Ada has been locked at bounty contract

while true; do
  node verify-bounty-offered.mjs && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done

# Claim the offer by supplying a simplified merkle proof

node claim-bounty.mjs var/claimant

# Verify that Ada has been claimed

while true; do
  node verify-bounty-claimed.mjs && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done
