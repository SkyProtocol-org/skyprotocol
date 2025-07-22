#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <address>"
  exit 1
fi

BASE_URL="http://localhost:8080"
changeAddress="$1"
usedAddrs="[\"$1\"]"

echo $changeAddress
echo $usedAddrs

# Construct JSON body
payload=$(jq -n \
  --arg change "$changeAddress" \
  --argjson used "$usedAddrs"  '
  {
    changeAddr: $change,
    usedAddrs: $used,
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
curl -X POST "$BASE_URL/bridge/create" \
  -H "Content-Type: application/json" \
  -d "$payload"

