import { BlockfrostProvider, MeshWallet, Transaction } from '@meshsdk/core'

import fs from 'node:fs'

import { waitUntilTxReady } from "./util.mjs";

const blockfrostKey = fs.readFileSync(`var/blockfrost.api-key`).toString().trim()
const blockchainProvider = new BlockfrostProvider(blockfrostKey)

const senderSkey = fs.readFileSync(`${process.argv[2]}.skey`).toString().trim()
const recipient = fs.readFileSync(`${process.argv[3]}.addr`).toString()

const wallet = new MeshWallet({
  networkId: 0,
  fetcher: blockchainProvider,
  submitter: blockchainProvider,
  key: {
    type: 'root',
    bech32: senderSkey
  }
})

const unsignedTx = await new Transaction({ initiator: wallet })
  .sendLovelace(recipient, process.argv[4] + '000000')
  .build()

const signedTx = await wallet.signTx(unsignedTx)

const txHash = await wallet.submitTx(signedTx)

console.log(`Ada sent. Recipient: ${recipient}, Tx hash: ${txHash}`)

await waitUntilTxReady(blockchainProvider, txHash);
