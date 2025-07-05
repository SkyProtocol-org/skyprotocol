#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "Usage: $0 <changeAddress> <usedAddrs> <apiUrl> [--amount <int>]"
  exit 1
fi

changeAddress="$1"
usedAddrs="$2"
apiUrl="$3"
shift 3

amount=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    --amount)
      amount="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

# Construct JSON body
payload=$(jq -n \
  --arg change "$changeAddress" \
  --argjson amt "$amount" \
  --argjson used "$usedAddrs"  '
  {
    changeAddr: $change,
    usedAddrs: $used,
    amount: $amt
  } ')

echo "Payload to submit:"
echo "$payload"

# Submit to API
curl -X POST "$apiUrl" \
  -H "Content-Type: application/json" \
  -d "$payload"

