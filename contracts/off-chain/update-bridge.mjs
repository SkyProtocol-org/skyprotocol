/*
  node update-bridge.mjs <wallet>

  Updates the top hash stored in the bridge NFT.

  Wallet will be used to pay fees.
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
      bech32: fs.readFileSync(`${process.argv[2]}.skey`).toString().trim()
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

function mkDataHash(hex) { return { alternative: 0, fields: [hex] } }
function mkPubKey(hex) { return { alternative: 0, fields: [hex] } }

const pk1 = mkPubKey("3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF65");
const pk2 = mkPubKey("42FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE");
const pk3 = mkPubKey("22B9524D37A16C945DEEC3455D92A1EBC5AC857174F5A0A8B376517A205DCA73");

// Create main committee MultiSigPubKey
const msPublicKey = {
    alternative: 0,
    fields: [
	[ pk1, pk2, pk3 ],
	2 // Number of public keys that must sign
    ]
}

// MultiSig
const ms = {
    alternative: 0,
    fields: [
	// [SingleSig]
	[ { alternative: 0, // top hash 2 sig 1
	    fields: [ pk1, "87E894C503E40A8CB98DEB8618DC068323092871C717D4781D56FCBBE10FCD6B1965ADE766FFDFAF8F7B2964F3ED8A6066703DD9AA68F583055ED53FBA27A90E" ] },
	  { alternative: 0, // top hash 2 sig 2
	    fields: [ pk2, "99E3BBBCA63ECDA27ADC6ED426A695E32AA5D7185CFC16F550834919C96F7FA17E19992E6FB2D302BE8FF71CF71907F654F25727425C0F30989B4AAC7767B003" ] }
	]
    ]
}

const TOPHASH2 = mkDataHash("3c7dfafe47aac5454629d9280529b90b82d07ba80b89757d652bff047f0534a1"); // top hash 2

// Create UpdateBridge redeemer
const redeemer = {
    alternative: 0,
    fields: [
	msPublicKey,
	mkDataHash("9f06268167a61b7f54210ebcd0a92d9000211a41401f7827b5bf905b8fd3e263"), // main root hash 1
	TOPHASH2,
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
	TOPHASH2
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
