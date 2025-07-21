#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "Usage: $0 <address> <apiUrl>"
  exit 1
fi

changeAddress="$1"
usedAddrs="[$1]"
apiUrl="$2"
shift 3

while [[ $# -gt 0 ]]; do
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

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
curl -X POST "$apiUrl" \
  -H "Content-Type: application/json" \
  -d "$payload"

