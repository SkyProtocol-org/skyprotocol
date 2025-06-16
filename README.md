# Sky Protocol

Sky Protocol is infrastructure for modular blockchains, starting with
a Data Availability Network.

### Copyright and License

Copyright 2024 Olapa Networks, Inc. All rights reserved.
Sky Protocol is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

### More information

For more information on Sky Protocol, see our [website](https://skyprotocol.org).


### Setting up the provider

Right now Sky Protocol only supports Maestro as a provider, with more planned in the future(Local privnet, other providers).
To set up the Maestro you need:
1) Go to their [website](https://www.gomaestro.org/) and create an account.
2) Create a project with "preview" network.
3) Copy API key into the config.yaml, in the `atlas.coreProvider.maestroToken` and set `atlas.networkId` to "preview".
4) Enjoy
