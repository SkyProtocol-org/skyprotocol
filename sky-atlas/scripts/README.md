# Scripts for Sky Protocol

This directory contains scripts to help run, use, and test Sky Protocol

_TODO_: split in three directories `admin`, `user` and `test`, one for each?

## Overview

* Ancillary scripts:
  - `create-collateral.sh`: create a collateral UTXO for the sake of calling contracts.

* Admin scripts:
  - `create-bridge.sh`: create the bridge between Sky and Cardano
  - `update-bridge.sh`: trigger an update of the bridge between Sky and Cardano

* User scripts:
  - `publish-message.sh`: publish a message on the Sky DA
  - `create-topic.sh`: create a topic on the Sky DA
  - `offer-bounty.sh`: offer a bounty for publishing a message on the Sky DA

* Test Data:
  - `message_to_publish`: a message
  - `message_hash`: its hash

## create-collateral.sh

This scripts creates a collateral UTXO, as needed to call contracts on Cardano in general,
and Sky Protocol contracts in particular.
Your wallet probably does that for you usually, but if using Sky from a program,
you will need something like it.

The script takes four parameters:
  1. The address that will receive the amount to for the UTXO to be used as collateral,
     e.g. `addr_test1vq7n4lzvn6dcq4v9g2trt8gavywkg2m49wgw00n4jdlpewctkep2n`
  2. The signing key for the UTXOs from which the funds will be taken.
  3. The amount sent to a first UTXO (_TODO_: at what address? The one in the first argument)
  3. The amount sent to a second UTXO (_TODO_: at the address in first argument?) to be used as collateral

NB: Assumes the Sky node is running on `localhost:8080`

_TODO_: what happens when the sum of the two amounts isn't exactly what's in the input UTXO?
Where does the change go? To what address? Shouldn't you specify only one amount,
and the rest should be determined from the inputs and put in a change UTXO?

## create-bridge.sh

This script creates the bridge between Sky and Cardano.

The script takes four parameters:
  1. The address of the admin who will create the bridge
     e.g. `addr_test1vq7n4lzvn6dcq4v9g2trt8gavywkg2m49wgw00n4jdlpewctkep2n`

NB: Assumes the Sky node is running on `localhost:8080`.
Also has a builtin notion of the initial state of the system and the bridge...

_TODO_: make it more configurable what the initial state is?

## update-bridge.sh

This script triggers an update to the Cardano side of the bridge between Sky and Cardano.

The script takes four parameters:
  1. The address of the admin who will update the bridge
     e.g. `addr_test1vq7n4lzvn6dcq4v9g2trt8gavywkg2m49wgw00n4jdlpewctkep2n`

NB: Assumes the Sky node is running on `localhost:8080`.
Also has a builtin notion of the initial state of the system and the bridge...

_TODO_: Just take the address from the running node's config?

## publish-message.sh

This script publishes a message on the Sky DA. This is what users use.

The script takes two parameters:
  1. The topic Id (a 64-bit number)
  2. The filename for a file containing the message to publish.

Implicit parameters passed as environment variables:
  - `${API_USER}`: the username to pass to the server to login.
  - `${API_PASS}`: the password to pass to the server to login.

NB: Currently assumes the Sky node is running on `localhost:8080`.

_TODO_: Get username and password and URL from some config file?

## create-topic.sh

This script creates a topic on the Sky DA.

The script does not take any parameter.

Implicit parameters passed as environment variables:
  - `${API_USER}`: the username to pass to the server to login.
  - `${API_PASS}`: the password to pass to the server to login.

NB: Currently assumes the Sky node is running on `localhost:8080`.

_TODO_: Get username and password and URL from some config file?

## offer-bounty.sh

This script offers a bounty for publishing a message on the Sky DA.

The script takes four parameters:
  1. The topicId.
  2. The messageHash.
  3. The deadline.
  4. The address to use change from the contract creation.

_TODO_:
  - What format for the deadline?
  - Why not let the change be just as implicit as the source?
  - The address (parameter #4) is conditional claimant address, isn't it?
    It shouldn't be the same as the change address.

NB: Currently assumes the Sky node is running on `localhost:8080`.
