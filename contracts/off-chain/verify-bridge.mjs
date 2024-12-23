/*
 * node verify-bridge.mjs <top-hash-hex>
 *
 * Verifies that the top hash is stored in the NFT, fails otherwise.
 */

import cbor from 'cbor'
import {
    MeshWallet,
    MeshTxBuilder,
    Transaction,
    serializePlutusScript,
    conStr,
    byteString,
    scriptAddress,
    serializeAddressObj,
    resolveScriptHash,
    stringToHex,
    deserializeDatum
} from '@meshsdk/core'

import fs from 'node:fs'

import { newProvider, findUTXOWithSpecificUnit, waitUntilTxReady } from "./util.mjs"

const blockchainProvider = newProvider();

// top hash 2
const topHashHex = "3c7dfafe47aac5454629d9280529b90b82d07ba80b89757d652bff047f0534a1";

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

const validatorAddress = serializePlutusScript(validator).address

const utxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress);

const utxo = findUTXOWithSpecificUnit(utxos, mintingPolicyHash + stringToHex('SkyBridge'))

const data = deserializeDatum(utxo.output.plutusData)

const nftTopHashHex = data.fields[0].fields[0].bytes;

if (nftTopHashHex.toLowerCase() === topHashHex.toLowerCase()) {
    console.log("Top Hash valid");
} else {
    throw `Top hash in NFT ${nftTopHashHex} doesn't match argument hash ${topHashHex}`;
g}
