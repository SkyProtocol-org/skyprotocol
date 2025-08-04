#!/usr/bin/env bash

set -euo pipefail

BASE_URL="http://localhost:8080"
USERNAME="$API_USER"
PASSWORD="$API_PASS"

response=$(curl -s -u "$USERNAME:$PASSWORD" -X POST "$BASE_URL/da/create_topic" \
  -H "Accept: application/json")

echo "Response: $response"

topic_id=$(echo "$response" | jq -r '.topic_id')

echo "Created topic_id: $topic_id"
