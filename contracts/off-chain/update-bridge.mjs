/*
  node update-bridge.mjs <wallet> <pubkey> <top-hash-hex> <sig>

  Updates the top hash stored in the bridge NFT.
  
  Wallet will be used to pay fees.
  Pubkey is the committee public key.
  Top-hash-hex is hash of merkle root and committee fingerprint.
  Sig is signature of tophash.
*/

import cbor from 'cbor'
import {
    BlockfrostProvider,
    MeshWallet,
    MeshTxBuilder,
    Transaction,
    serializePlutusScript,
    conStr,
    byteString,
    scriptAddress,
    serializeAddressObj,
    resolveScriptHash,
    stringToHex
} from '@meshsdk/core'

import fs from 'node:fs'

import { findUTXOWithSpecificUnit, waitUntilTxReady } from "./util.mjs"

//////////////////////////////////////////////////////////////////////////////
// Setup
//////////////////////////////////////////////////////////////////////////////

const blockfrostKey = fs.readFileSync(`var/blockfrost.api-key`).toString().trim()
const blockchainProvider = new BlockfrostProvider(blockfrostKey)

const wallet = new MeshWallet({
  networkId: 0,
  fetcher: blockchainProvider,
  submitter: blockchainProvider,
  key: {
    type: 'root',
      bech32: fs.readFileSync(`${process.argv[2]}.skey`).toString().trim()
  }
})

const publicKeyHex = process.argv[3]
const newTopHashHex = process.argv[4]
const sigHex = process.argv[5]

console.log(`Updating bridge with new top hash ${newTopHashHex}\nPublic key: ${publicKeyHex}\nSignature: ${sigHex}`);

const validatorBlueprint = JSON.parse(
  fs.readFileSync('./var/sky-bridge-validator.json')
)

const validator = {
  code: cbor
    .encode(
      Buffer.from(validatorBlueprint.validators[0].compiledCode, 'hex')
    )
    .toString('hex'),
  version: 'V2'
}

const validatorAddress = serializePlutusScript(validator).address

const mintingPolicyBlueprint = JSON.parse(
  fs.readFileSync('./var/sky-minting-policy.json')
)

const mintingPolicy = {
  code: cbor
    .encode(
      Buffer.from(mintingPolicyBlueprint.validators[0].compiledCode, 'hex')
    )
    .toString('hex'),
  version: 'V2'
}

const mintingPolicyHash = resolveScriptHash(
  mintingPolicy.code,
  mintingPolicy.version
)

//////////////////////////////////////////////////////////////////////////////
// Create Data to Send to Bridge Contract
//////////////////////////////////////////////////////////////////////////////

function mkDataHash(hex) { return { alternative: 0, fields: [hex] } }
function mkPubKey(hex) { return { alternative: 0, fields: [hex] } }

// Create MultiSigPubKey
const msPublicKey = {
    alternative: 0,
    fields: [
	[ mkPubKey(publicKeyHex) ], // List of public keys in signatures
	1 // Number of public keys that must sign
    ]
}

// MultiSig
const ms = {
    alternative: 0,
    fields: [
	// [SingleSig]
	[ { alternative: 0,
	    fields: [ sigHex ] } ]
    ]
}

// Create UpdateBridge redeemer
const redeemer = {
    alternative: 0,
    fields: [
	msPublicKey,
	mkDataHash(oldDataHashHex),
	mkDataHash(newTopHashHex),
	ms
    ]
}

//////////////////////////////////////////////////////////////////////////////
// Find Bridge NFT and Create Datum Updated With New Top Hash
//////////////////////////////////////////////////////////////////////////////

const utxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress);
const utxo = findUTXOWithSpecificUnit(utxos, mintingPolicyHash + stringToHex('SkyBridge'))

const updatedDatum = {
    alternative: 0,
    fields: [
	mkDataHash(newTopHashHex)
    ]
};

//////////////////////////////////////////////////////////////////////////////
// Unlock NFT UTXO and send it back to bridge contract with updated datum
//////////////////////////////////////////////////////////////////////////////

const recipient = {
    address: validatorAddress,
    datum: { value: updatedDatum, inline: true }
};

const tx = new Transaction({ initiator: wallet, verbose: true })
      .redeemValue({
	  value: utxo,
	  script: validator,
	  redeemer: { data: redeemer }
      })
      .sendAssets(recipient, [
	  {
              unit: mintingPolicyHash + stringToHex('SkyBridge'),
              quantity: '1'
	  }
      ]);

const unsignedTx = await tx.build();
const signedTx = await wallet.signTx(unsignedTx);
const txHash = await wallet.submitTx(signedTx);

await waitUntilTxReady(blockchainProvider, txHash);
