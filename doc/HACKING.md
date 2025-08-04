# Hacking Sky Protocol

## Understanding our code structure

* Data structures and common code is in common/
* Onchain contracts are in onchain/
* Offchain services are in offchain/
* Tests are in tests/
* Main loop for executables are in app/
* Documentation is in doc/
* You may have to configure things in config/ according to explanation in README.md to hit the Cardano preview network.
* Some scripts to interact with the network are in scripts/

## Setting up the binary cache

The binary cache is [sky-protocol](https://sky-protocol.cachix.org).
To be able to push package to it as a developer, you should get the `cachix` cli tool
and set the authtoken with
`cachix authtoken $YOUR_AUTH_TOKEN`.

If you are a team member and want to push to the cache, you should make sure
that you have a `write` token, which you can get from Fare,
and that the cachix daemon is running (somehow Yaroslav had to `cachix daemon run sky-protocol`).
