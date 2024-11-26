/*
  node claim-bounty.mjs <left> <right> <fingerprint>

  Claims the bounty funds by supplying a Merkle proof with left and
  right arms, and the fingerprint of the committee multisig.
*/

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
import cbor from 'cbor'

import fs from 'node:fs'

import { findUTXOWithSpecificUnit, waitUntilTxReady } from "./util.mjs"

const blockfrostKey = fs.readFileSync(`var/blockfrost.api-key`).toString().trim()
const blockchainProvider = new BlockfrostProvider(blockfrostKey)

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

const leftHex = process.argv[3]
const rightHex = process.argv[4]
const multiSigPubKeyHashHex = process.argv[5]

console.log(`Left ${leftHex}`);
console.log(`Right ${rightHex}`);
console.log(`Committee hash ${multiSigPubKeyHashHex}`);

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

const bridgeUtxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress);
const nft = findUTXOWithSpecificUnit(bridgeUtxos, mintingPolicyHash + stringToHex('SkyBridge'))

console.log(JSON.stringify(nft));

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
const bountyUtxo = bountyUtxos[0] // TBD for now claim only one of the UTXOs at bounty
console.log(JSON.stringify(bountyUtxo))

// ClientRedeemer
const redeemer = {
    alternative: 0,
    fields: [
	// SimplifiedMerkleProof
	{ alternative: 0,
	  fields: [
	      // DataHash
	      { alternative: 0, fields: [ leftHex ] },
	      // DataHash
	      { alternative: 0, fields: [ rightHex ] }
	  ]
	},
	// DataHash
	{ alternative: 0, fields: [ multiSigPubKeyHashHex ] }
    ]
}

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
