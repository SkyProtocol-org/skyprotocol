#!/usr/bin/env bash
set -eux

# Install deps
sudo apt-get install jq
npm install
# Create directory for application data
mkdir var
# Generate keys for admin as well as bounty offerer and claimant
node generate-keys.mjs var/admin
node generate-keys.mjs var/offerer
node generate-keys.mjs var/claimant

echo OK
