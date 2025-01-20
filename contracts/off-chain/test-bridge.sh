#!/usr/bin/env bash
set -eux

# This test updates the bridge NFT verifies that it has been updated afterwards

# Store top hash 2 in bridge

# -d @- means read data from stdin
curl -X POST localhost:3030/update-bridge -H "Content-Type: application/json" -d @- <<EOF
{
  "committee": {
    "publicKeys": [
      "3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF65",
      "42FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE",
      "22B9524D37A16C945DEEC3455D92A1EBC5AC857174F5A0A8B376517A205DCA73"
    ],
    "n": 2
  },
  "oldRootHash": "9f06268167a61b7f54210ebcd0a92d9000211a41401f7827b5bf905b8fd3e263",
  "newTopHash": "3c7dfafe47aac5454629d9280529b90b82d07ba80b89757d652bff047f0534a1",
  "newTopHashSignature": [
    { "publicKey": "3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF65",
      "signature": "87E894C503E40A8CB98DEB8618DC068323092871C717D4781D56FCBBE10FCD6B1965ADE766FFDFAF8F7B2964F3ED8A6066703DD9AA68F583055ED53FBA27A90E" },
    { "publicKey": "42FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE",
      "signature": "99E3BBBCA63ECDA27ADC6ED426A695E32AA5D7185CFC16F550834919C96F7FA17E19992E6FB2D302BE8FF71CF71907F654F25727425C0F30989B4AAC7767B003" }
  ]
}
EOF


# Verify that top hash 2 is stored in bridge

while true; do
  node verify-bridge.mjs && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done

echo OK
