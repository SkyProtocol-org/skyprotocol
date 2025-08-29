#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 4 ]]; then
  echo "Usage: $0 <topicId> <messageHash> <deadline> <amount>"
  exit 1
fi

topicId="$1"
messageHash="$2"
deadline="$3"
amount=$4
BASE_URL="localhost:8080"

# Construct JSON body
payload=$(jq -n \
  --argjson tId "$topicId" \
  --arg mH "$messageHash" \
  --argjson dl "$deadline" \
  --argjson amt "$amount" \ '
  {
    topicId: {"topic_id": $tId},
    messageHash: {"hash": $mH},
    deadline: $dl,
    amount: $amt,
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
curl -s -X POST "$BASE_URL/bounty/offer" \
  -H "Content-Type: application/json" \
  -d "$payload"

echo
