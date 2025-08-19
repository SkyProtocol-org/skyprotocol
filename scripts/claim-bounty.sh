#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 6 ]]; then
  echo "Usage: $0 <topicId> <messageId> <messageHash> <deadlineStart> <deadline> <address>"
  exit 1
fi

topicId="$1"
messageId="$2"
messageHash="$3"
deadlineStart="$4"
deadline="$5"
changeAddress="$6"
usedAddrs="[\"$6\"]"
BASE_URL="localhost:8080"

# Construct JSON body
payload=$(jq -n \
  --arg change "$changeAddress" \
  --argjson tId "$topicId" \
  --argjson mId "$messageId" \
  --arg mH "$messageHash" \
  --argjson dls "$deadlineStart" \
  --argjson dl "$deadline" \
  --argjson used "$usedAddrs"  '
  {
    topicId: {"topic_id": $tId},
    messageId: {"message_id": $mId},
    messageHash: {"hash": $mH},
    deadlineStart: $dls,
    deadline: $dl,
    changeAddr: $change,
    usedAddrs: $used,
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
curl -s -X POST "$BASE_URL/bounty/claim" \
  -H "Content-Type: application/json" \
  -d "$payload"
