/*
  node offer-bounty.mjs <wallet>

  Offer a bounty.

  Sends some Ada from offerer wallet to bounty contract address.
*/

import cbor from 'cbor'
import {
  MeshWallet,
  Transaction,
  serializePlutusScript,
  conStr,
  byteString,
  mTuple
} from '@meshsdk/core'

import fs from 'node:fs'

import { newProvider, waitUntilTxReady } from "./util.mjs";

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
  fs.readFileSync('./var/sky-bounty-validator.json')
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

//////////////////////////////////////////////////////////////////////////////
// Send 10 Ada to Bounty Contract
//////////////////////////////////////////////////////////////////////////////

const recipient = {
    address: validatorAddress,
    datum: { value: [], inline: true }
}

const unsignedTx = await new Transaction({ initiator: wallet, verbose: true })
  .sendLovelace(recipient, '10000000')
  .build()
const signedTx = await wallet.signTx(unsignedTx)
const txHash = await wallet.submitTx(signedTx)

console.log(`Ada sent. Tx hash: ${txHash}`)

await waitUntilTxReady(blockchainProvider, txHash);
