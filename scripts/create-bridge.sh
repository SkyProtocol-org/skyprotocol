#!/usr/bin/env bash

set -euo pipefail

BASE_URL="http://localhost:8080"

# Submit to API
curl -X POST "$BASE_URL/bridge/create"

echo
