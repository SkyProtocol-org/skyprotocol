### Running

At this point you should have a running sky node in the terminal.
Start new terminal and run the next commands there.


NOTE: make sure you funded the admin address,
refer to [Getting ADA tokens on the preview network](/README.md#get-ada-tokens-on-the-preview-network)


Start by creating the bridge using admin address:
```bash
  bash scripts/create-bridge.sh "admin_addr"
```

The script output would look something like this:
```
  Payload to submit:
  {
    "changeAddr": "[your address here]",
    "usedAddrs": [
      "[your address here]"
    ]
  }
  "85ad195c468431f58148497374061eadb619df0b0220164bbaf3491d2490c1fa"%
```
The last line is the transaction id which you can copy(without ") and insert in the
search bar in the [explorer](https://preview.cexplorer.io/) to see the results.

The next step will be to populate the DA with data:

For this to happen, you should initialize 2 variables
in your bash session, since the API that we will be
interacting with is protected with basic auth(for now).

```bash
  export API_USER="skyAdmin" API_PASS="1234"
```

Now you can interact with the DA API.


Start with creating a new topic:
```bash
  bash scripts/create-topic.sh  
```

The response will include the created Topic ID.

Then populate that topic with some data:
```bash
  # the first argument is the topic id that you get from previous step.
  # we also include some random data in the scripts folder for you to use.
  bash scripts/publish-message.sh 0 scripts/message_to_publish
```

The next step will be to update the bridge:
```bash
  bash scripts/update-bridge.sh "admin-addr"
```

The response will again include the transaction id,
which you can copy-paste into the [explorer](https://preview.cexplorer.io/).
