export function findUTXOWithSpecificUnit(utxos, unitToFind) {
    for (const utxo of utxos) {
        for (const amount of utxo.output.amount) {
            if (amount.unit === unitToFind) {
                return utxo;
            }
        }
    }
    return null;
}

export async function waitUntilTxReady(blockchainProvider, txHash) {
    while (true) {
	try {
	    console.log("Waiting for tx confirmation: " + txHash);
	    let result = await blockchainProvider.fetchTxInfo(txHash);
	    // If we don't get an error, we are done, return
	    console.log("Tx confirmed: " + txHash);
	    return;
	} catch (errString) {
	    const err = JSON.parse(errString);
	    if (err.status === 404) {
		await new Promise(resolve => setTimeout(resolve, 5000));
		continue;
	    } else {
		throw errString;
	    }
	}
    }
}
