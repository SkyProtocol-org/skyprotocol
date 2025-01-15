#!/usr/bin/env bash
set -eux

# Install deps
sudo apt-get install jq
npm install
# Create directory for application data
rm -rf var
mkdir var
# Generate keys for admin as well as bounty offerer and claimant
node generate-keys.mjs var/admin
node generate-keys.mjs var/offerer
node generate-keys.mjs var/claimant

export ADMIN_ADDR=$(cat var/admin.addr)
export OFFERER_ADDR=$(cat var/offerer.addr)
export CLAIMANT_ADDR=$(cat var/claimant.addr)

# XXX
export DEVKIT=/home/sky/yaci/yaci-devkit-0.10.0-preview2/bin/devkit.sh

# Stop devkit if it's running
sudo $DEVKIT stop

# Launch devkit in background.
# Script makes the devkit think it's running inside a tty, otherwise it won't work
script -q -c "sudo $DEVKIT start" /dev/null <<EOF &
create-node --overwrite --start
topup $ADMIN_ADDR 10000
topup $OFFERER_ADDR 10000
topup $CLAIMANT_ADDR 10000
EOF

# The following loop waits until the last address has been topped up.
URL="http://localhost:8080/api/v1/addresses/$CLAIMANT_ADDR/balance"
while true; do
    echo "Fetching the balance..."
    # Because we run the whole script under -e which makes it exit on error,
    # we need to handle network errors from curl specially.  Return a fake 500 error.
    STATUS=$(curl -w "%{http_code}" -o /dev/null --max-time 60 "$URL" || echo "500")
    echo "Fetched the balance..."
    if [ "$STATUS" -eq 200 ]; then
        break
    else
        echo "Failed to fetch the balance, retrying..."
    fi
    sleep 5
done

# Create minting policy
cabal run gen-minting-policy-blueprint -- "$(cat var/admin.pkh)" var/sky-minting-policy.json
cat var/sky-minting-policy.json | jq -r '.validators[0].hash' > var/sky-minting-policy.hash
# Generate Bridge Validator
cabal run gen-validator-blueprint -- "$(cat var/sky-minting-policy.hash)" var/sky-bridge-validator.json
cat var/sky-bridge-validator.json | jq -r '.validators[0].hash' > var/sky-bridge-validator.hash
# Mint the bridge NFT
node mint-nft.mjs var/admin
# Generate Bounty validator with our desired topic ID 00 and target hash CAFE
cabal run gen-bounty-blueprint -- "$(cat var/sky-minting-policy.hash)" 00 CAFE "$(cat var/claimant.pkh)" var/sky-bounty-validator.json
