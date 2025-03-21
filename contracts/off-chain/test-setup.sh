#!/usr/bin/env bash
set -eux

# This script:
# - Installs deps
# - Generates wallets used for tests
# - Launches the Yaci devkit and tops up the wallets
# - Generates blueprints for the NFT, and bridge and bounty contracts
# - Mints the NFT
# - Starts the HTTP API

# Utility that waits until an URL becomes 200
wait_for_url() {
    local URL="$1"
    while true; do
        echo "Waiting for $URL to become available"
        # Because we run the whole script under -e which makes it exit on error,
        # we need to handle network errors from curl specially. Return a fake 500 error.
        STATUS=$(curl -w "%{http_code}" -o /dev/null --max-time 60 "$URL" || echo "500")
        if [ "$STATUS" -eq 200 ]; then
            break
        else
            echo "$URL not yet available, retrying..."
        fi
        sleep 5
    done
}

# Install deps used for test
sudo apt-get install jq
npm install

# No idea why this is needed -- would be better to get those values
# from cabal.project
cabal v2-update 'cardano-haskell-packages,2024-11-27T20:49:28Z'
cabal v2-update 'hackage.haskell.org,2024-12-02T09:19:04Z'

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

# Create minting policy
cabal run gen-minting-policy-blueprint -- "$(cat var/admin.pkh)" var/sky-minting-policy.json
cat var/sky-minting-policy.json | jq -r '.validators[0].hash' > var/sky-minting-policy.hash
# Generate Bridge Validator
cabal run gen-validator-blueprint -- "$(cat var/sky-minting-policy.hash)" var/sky-bridge-validator.json
cat var/sky-bridge-validator.json | jq -r '.validators[0].hash' > var/sky-bridge-validator.hash
# Generate Bounty validator with our desired topic ID 00 and target hash CAFE
cabal run gen-bounty-blueprint -- "$(cat var/sky-minting-policy.hash)" 00 CAFE "$(cat var/claimant.pkh)" var/sky-bounty-validator.json

# Install devkit if it isn't installed
if [ ! -d "$HOME/.yaci-devkit" ]; then
    curl --proto '=https' --tlsv1.2 -LsSf https://devkit.yaci.xyz/install.sh | bash
fi
DEVKIT="$HOME/.yaci-devkit/bin/devkit"

# Stop devkit if it's running
sudo $DEVKIT stop

# Launch devkit in background.  `script` makes the devkit think it's
# running inside a tty, otherwise it won't work :-(
script -q -c "sudo $DEVKIT start" /dev/null <<EOF &
create-node --overwrite --start
topup $ADMIN_ADDR 10000
topup $OFFERER_ADDR 10000
topup $CLAIMANT_ADDR 10000
EOF

# Wait until claimant address is topped up (it's the last of the addresses we top up)
wait_for_url "http://localhost:8080/api/v1/addresses/$CLAIMANT_ADDR/balance"

# Finally, after all addresses have been topped up, we can mint the bridge NFT.
node mint-nft.mjs var/admin

# Start HTTP API
pushd http-api
sudo docker compose kill
sudo docker compose up --build -d
# Wait for HTTP API to become available
wait_for_url "http://localhost:3030/"
popd
