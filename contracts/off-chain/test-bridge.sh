#!/usr/bin/env bash
set -eux

# This test updates the bridge NFT two times and verifies that it has
# been updated afterwards each time.

# The single public key used to sign the update. Will be a multisig pubkey in later versions.

export PUBKEY=F710662FC12F8852CBA4BAD811E770005A6E5311DA328D12FA5A46AEC707C405

# Two merkle root top hashes, and their signatures.

export TOPHASH1=AAA596B8A223956226D3607A37819AB7A6D588D394BB0F244C690E25A898F042
export SIG1=886F601FCCCBD9E4699CA364D652FE9E32088B0B183C0430EFEAF89D81AD5792039F7ADF46964903A8DC2702B9E9CE6F9A318C5C76E66B2F85300DFE3BDF8C00

export TOPHASH2=1D3FF551B82C53714E4CE0F9CEC89AB4EF8AA92C6DA742462F295DF9D765814E
export SIG2=D4F4F7B89F392CB2B0B5A0EF8C59DB515551F0FF1EFF7F05A309BF55CA2A0DE86E4A4CC51371A70EF8CE4208F0793600FD81B88AC50EDD381892BCCE0AC39D07

# Store top hash 1 in bridge

node update-bridge.mjs var/admin $PUBKEY $TOPHASH1 $SIG1

# Verify that top hash 1 is stored in bridge

while true; do
  node verify-bridge.mjs $TOPHASH1 && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done

# Store top hash 2 in bridge

node update-bridge.mjs var/admin $PUBKEY $TOPHASH2 $SIG2

# Verify that top hash 2 is stored in bridge

while true; do
  node verify-bridge.mjs $TOPHASH2 && break || { echo "Attempt failed, retrying in 5 seconds..."; sleep 5; }
done

echo OK
