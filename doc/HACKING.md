#### Setting up the binary cache

The binary cache is [sky-protocol](https://sky-protocol.cachix.org).
To use it, you should get the `cachix` cli tool and set the authtoken with
`cachix authtoken $YOUR_AUTH_TOKEN`.

If you are a team member and want to push to the cache, you should make sure
that you have a `write` token, which you can get from Fare,
and that the cachix daemon is running (somehow Yaroslav had to `cachix daemon run sky-protocol`).
