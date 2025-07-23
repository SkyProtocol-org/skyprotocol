#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 4 ]]; then
  echo "Usage: $0 <address> <signingKey> <firstAmt> <sndAmt>"
  exit 1
fi

BASE_URL="http://localhost:8080"
addr="$1"
signingKey="$2"
firstAmt="$3"
secondAmt="$4"

# Construct JSON body
payload=$(jq -n \
  --arg addr "$addr" \
  --arg signingKey "$signingKey" \
  --argjson fstAmt "$firstAmt" \
  --argjson sndAmt "$secondAmt" '
  {
    addr: $addr,
    signingKey: $signingKey,
    funds: $fstAmt,
    collateral: $sndAmt,
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
curl -X POST "$BASE_URL/util/create_collateral" \
  -H "Content-Type: application/json" \
  -d "$payload"

