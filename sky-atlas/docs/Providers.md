### Setting up the provider

Right now Sky Protocol only supports Maestro as a provider, with more planned in the future(Local privnet, other providers).
To set up the Maestro you need:
1) Go to their [website](https://www.gomaestro.org/) and create an account (say you are alone prelaunch for 33333 credits)
2) Create a project with the Cardano "preview" network.
3) Copy config/example-config.yaml to config/config-test.yaml and insert the API key into the it (see EDIT THIS), in the `atlas.coreProvider.maestroToken` and set `atlas.networkId` to "preview".
4) Enjoy
