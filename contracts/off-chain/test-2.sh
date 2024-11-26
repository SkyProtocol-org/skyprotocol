#!/usr/bin/env bash
set -eux

# Create a bounty offer that will be paid out if claimant can prove
# that the hash 1111 is stored in the DA layer.  This locks some Ada
# at the contract.

node offer-bounty.mjs var/offerer

# Verify that Ada has been locked at bounty contract

while true; do
  node verify-bounty-offered.mjs && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done

# Claim the offer by supplying a simplified merkle proof with two
# arms, 0000 and 1111.  1111 is the hash for which the bounty will be
# payed out.

# Fingerprint of committee public key
export FINGERPRINT=4b4f90f0670c7d8d26949bfc1b90de7e12a572094ec1cdf23fec3e1f9a4bcf71

export LEFT_HASH=0000  # left arm of merkle proof
export RIGHT_HASH=1111 # right arm of merkle proof

node claim-bounty.mjs var/claimant $LEFT_HASH $RIGHT_HASH $FINGERPRINT

# Verify that Ada has been claimed

while true; do
  node verify-bounty-claimed.mjs && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done
