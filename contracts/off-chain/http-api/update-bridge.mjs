/*
  HTTP endpoint that updates the top hash stored in the bridge NFT.

  The var/admin Wallet will be used to pay fees.
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
    stringToHex
} from '@meshsdk/core'

import fs from 'node:fs'

import { newProvider, findUTXOWithSpecificUnit, waitUntilTxReady } from "./util.mjs"

// See test-bridge.sh for example of input JSON format
export async function updateBridge(inputJson)
{

    //////////////////////////////////////////////////////////////////////////////
    // Setup
    //////////////////////////////////////////////////////////////////////////////

    const blockchainProvider = newProvider();

    const wallet = new MeshWallet({
        networkId: 0,
        fetcher: blockchainProvider,
        submitter: blockchainProvider,
        key: {
            type: 'root',
            bech32: fs.readFileSync(`./var/admin.skey`).toString().trim()
        }
    })

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

    // Transform the endpoint's JSON input into the data structure understood by the contract
    function transformData(data) {
        return {
            alternative: 0,
            fields: [
                {
                    alternative: 0,
                    fields: [
                        data.committee.publicKeys.map(publicKey => ({
                            alternative: 0,
                            fields: [publicKey]
                        })),
                        data.committee.n
                    ]
                },
                {
                    alternative: 0,
                    fields: [data.oldRootHash]
                },
                {
                    alternative: 0,
                    fields: [data.newTopHash]
                },
                {
                    alternative: 0,
                    fields: [
                        data.newTopHashSignature.map(signatureObj => ({
                            alternative: 0,
                            fields: [
                                {
                                    alternative: 0,
                                    fields: [signatureObj.publicKey]
                                },
                                signatureObj.signature
                            ]
                        }))
                    ]
                }
            ]
        };
    }

    const redeemer = transformData(inputJson);

    //////////////////////////////////////////////////////////////////////////////
    // Find Bridge NFT and Create Datum Updated With New Top Hash
    //////////////////////////////////////////////////////////////////////////////

    const utxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress);
    const utxo = findUTXOWithSpecificUnit(utxos, mintingPolicyHash + stringToHex('SkyBridge'))

    const updatedDatum = {
        alternative: 0,
        fields: [
	    { alternative: 0, fields: [inputJson.newTopHash] }
        ]
    };

    //////////////////////////////////////////////////////////////////////////////
    // Unlock NFT UTXO and send it back to bridge contract with updated datum
    //////////////////////////////////////////////////////////////////////////////

    const recipient = {
        address: validatorAddress,
        datum: { value: updatedDatum, inline: true }
    };

    const tx = new Transaction({ initiator: wallet, fetcher: blockchainProvider, verbose: true })
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

}
