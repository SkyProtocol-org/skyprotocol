#!/usr/bin/env bash
set -eux

# This test updates the bridge NFT verifies that it has been updated afterwards

# Store top hash 2 in bridge

curl -X POST localhost:3030/update-bridge

# Verify that top hash 2 is stored in bridge

while true; do
  node verify-bridge.mjs && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done

echo OK
