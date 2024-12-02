/*
 * node verify-bridge.mjs <top-hash-hex>
 *
 * Verifies that the top hash is stored in the NFT, fails otherwise.
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
    stringToHex,
    deserializeDatum
} from '@meshsdk/core'

import fs from 'node:fs'

import { findUTXOWithSpecificUnit, waitUntilTxReady } from "./util.mjs"

const blockfrostKey = fs.readFileSync(`var/blockfrost.api-key`).toString().trim()
const blockchainProvider = new BlockfrostProvider(blockfrostKey)

const topHashHex = process.argv[2]

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
}
