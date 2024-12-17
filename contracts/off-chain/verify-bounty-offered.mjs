/*
  Verifies that some funds are locked at the bounty contract address.
*/

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
    stringToHex
} from '@meshsdk/core'
import cbor from 'cbor'

import fs from 'node:fs'

import { newProvider, findUTXOWithSpecificUnit, waitUntilTxReady } from "./util.mjs"

const blockchainProvider = newProvider();

const bountyBlueprint = JSON.parse(
  fs.readFileSync('./var/sky-bounty-validator.json')
)

const bountyValidator = {
  code: cbor
    .encode(
      Buffer.from(bountyBlueprint.validators[0].compiledCode, 'hex')
    )
    .toString('hex'),
  version: 'V2'
}

const bountyAddress = serializePlutusScript(bountyValidator).address
const bountyUtxos = await blockchainProvider.fetchAddressUTxOs(bountyAddress);
const bountyUtxo = bountyUtxos[0]
const amount = bountyUtxo.output.amount[0];
console.log(JSON.stringify(bountyUtxo))

if ((amount.unit === "lovelace") && (amount.quantity === "10000000")) {
    console.log("10 Ada have been locked at bounty contract");
} else {
    throw "No Ada locked at bounty contract";
}
