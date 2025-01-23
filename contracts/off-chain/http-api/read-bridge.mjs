/*
 * Returns the top hash stored in the NFT.
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

export async function readBridge()
{
    const blockchainProvider = newProvider();

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

    const utxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress)
    const utxo = findUTXOWithSpecificUnit(utxos, mintingPolicyHash + stringToHex('SkyBridge'))
    const data = deserializeDatum(utxo.output.plutusData)

    return data.fields[0].fields[0].bytes
}
