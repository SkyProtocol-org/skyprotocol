/*
  node claim-bounty.mjs

  Claims the bounty funds by supplying a Merkle proof
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

//////////////////////////////////////////////////////////////////////////////
// Setup
//////////////////////////////////////////////////////////////////////////////

const blockchainProvider = newProvider();

const principal = process.argv[2]

const wallet = new MeshWallet({
  networkId: 0,
  fetcher: blockchainProvider,
  submitter: blockchainProvider,
  key: {
    type: 'root',
      bech32: fs.readFileSync(`${principal}.skey`).toString().trim()
  }
})

const recipient = {
    address: fs.readFileSync(`${principal}.addr`).toString().trim()
};

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

//////////////////////////////////////////////////////////////////////////////
// Find NFT for Top Hash, Find Bounty Reward, and Unlock it Via Merkle Proof
//////////////////////////////////////////////////////////////////////////////

const bridgeUtxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress);
const nft = findUTXOWithSpecificUnit(bridgeUtxos, mintingPolicyHash + stringToHex('SkyBridge'))

const bountyUtxos = await blockchainProvider.fetchAddressUTxOs(bountyAddress);
const bountyUtxo = bountyUtxos[0] // TBD for now claim only one of the UTXOs at bounty

// ClaimBounty
const redeemer = {
    alternative: 0,
    fields: [
	// messageInTopicProof :: SimplifiedMerkleProof
	{ alternative: 0,
	  fields: [
	      // DataHash
	      { alternative: 0, fields: [ "CAFE" ] },
	      // DataHash
	      { alternative: 0, fields: [ "BABE" ] }
	  ]
	},
	// topicInDAProof :: SimplifiedMerkleProof
	{ alternative: 0,
	  fields: [
	      // DataHash
	      { alternative: 0, fields: [ "5c82f057ac60bbc4c347d15418960d453468ffa2b6f8b2e0041d0cad3453f67f" ] }, // topic 1 top hash
	      // DataHash
	      { alternative: 0, fields: [ "0000" ] } // topic 2 top hash
	  ]
	},
	// topicCommitteeFingerprint :: DataHash
	{ alternative: 0, fields: [ "b25f003443ff6eb36a6baafaf5bc5d5e78c1dbd4533e3c49be498f23a9ac5767" ] }, // topic 1 committee fp
	// mainCommitteeFingerprint :: DataHash
	{ alternative: 0, fields: [ "5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f" ] } // main committee fingerprint
    ]
}

// Unlock the reward UTXO locked at bounty and send it to claimant
const tx = new Transaction({ initiator: wallet, verbose: true })
      .redeemValue({
	  value: bountyUtxo,
	  script: bountyValidator,
	  redeemer: { data: redeemer }
      })
      .sendValue(recipient, bountyUtxo)
      .setTxRefInputs([ nft ]);

const unsignedTx = await tx.build();
const signedTx = await wallet.signTx(unsignedTx);
const txHash = await wallet.submitTx(signedTx);

console.log("OK: tx: " + txHash)

await waitUntilTxReady(blockchainProvider, txHash);
