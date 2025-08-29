#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "Usage: $0 <topicId> <messageId> <messageHash>"
  exit 1
fi

topicId="$1"
messageId="$2"
messageHash="$3"
BASE_URL="localhost:8080"

# Construct JSON body
payload=$(jq -n \
  --argjson tId "$topicId" \
  --argjson mId "$messageId" \
  --arg mH "$messageHash" \ '
  {
    topicId: {"topic_id": $tId},
    messageId: {"message_id": $mId},
    messageHash: {"hash": $mH},
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
curl -s -X POST "$BASE_URL/bounty/claim" \
  -H "Content-Type: application/json" \
  -d "$payload"
