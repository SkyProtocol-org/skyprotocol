#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 4 ]]; then
  echo "Usage: $0 <topicId> <messageHash> <deadline> <address>"
  exit 1
fi

topicId="$1"
messageHash="$2"
deadline="$3"
changeAddress="$4"
usedAddrs="[\"$4\"]"
BASE_URL="localhost:8080"

# Construct JSON body
payload=$(jq -n \
  --arg change "$changeAddress" \
  --argjson tId "$topicId" \
  --arg mH "$messageHash" \
  --argjson dl "$deadline" \
  --argjson used "$usedAddrs"  '
  {
    topicId: {"topic_id": $tId},
    messageHash: {"hash": $mH},
    deadline: $dl,
    changeAddr: $change,
    usedAddrs: $used,
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
response=$(curl -s -X POST "$BASE_URL/bounty/offer" \
  -H "Content-Type: application/json" \
  -d "$payload")

echo "Response: $response"

tx_id=$(echo "$response" | jq -r '.tx_id')

echo "Transaction id: $tx_id"
