#!/usr/bin/env bash
set -euo pipefail

if [ $# -ne 2 ]; then
  echo "Usage: $0 <topic_id> <file>"
  exit 1
fi

# ---- Config ----
TOPIC_ID="$1"
FILE="$2"
BASE_URL="http://localhost:8080"
USERNAME="$API_USER"
PASSWORD="$API_PASS"

# ---- Request ----
response=$(curl -s -u "$USERNAME:$PASSWORD" -X POST "$BASE_URL/da/publish_message/$TOPIC_ID" \
  -H "Content-Type: application/octet-stream" \
  -H "Accept: application/json" \
  --data-binary @"$FILE")

# echo "Response: $response"

message_id=$(echo "$response" | jq -r '.[0].message_id')
message_hash=$(echo "$response" | jq -r '.[1].hash')

echo "Published message_id: $message_id"
echo "Published message hash: $message_hash"
