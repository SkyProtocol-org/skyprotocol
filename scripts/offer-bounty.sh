#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 5 ]]; then
  echo "Usage: $0 <topicId> <messageHash> <deadline> <amount> <address>"
  exit 1
fi

topicId="$1"
messageHash="$2"
deadline="$3"
amount=$4
changeAddress="$5"
usedAddrs="[\"$5\"]"
BASE_URL="localhost:8080"

# Construct JSON body
payload=$(jq -n \
  --arg change "$changeAddress" \
  --argjson tId "$topicId" \
  --arg mH "$messageHash" \
  --argjson dl "$deadline" \
  --argjson amt "$amount" \
  --argjson used "$usedAddrs"  '
  {
    topicId: {"topic_id": $tId},
    messageHash: {"hash": $mH},
    deadline: $dl,
    changeAddr: $change,
    usedAddrs: $used,
    amount: $amt,
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
curl -s -X POST "$BASE_URL/bounty/offer" \
  -H "Content-Type: application/json" \
  -d "$payload"

echo
