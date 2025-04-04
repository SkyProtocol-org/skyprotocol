import express from 'express';
import { readBridge } from './read-bridge.mjs';
import { updateBridge } from './update-bridge.mjs';

const app = express();
const hostname = '0.0.0.0'; // Listen on all network interfaces
const port = 3030;

// Middleware to parse JSON
app.use(express.json());

// Simple status endpoint
app.get('/', (req, res) => {
    res.status(200).send("OK");
});

// Read bridge endpoint
app.get('/read-bridge', async (req, res) => {
    try {
        res.status(200).end(await readBridge());
    } catch (err) {
        res.status(500).send(err.toString());
    }
});

// Update bridge endpoint
app.post('/update-bridge', async (req, res) => {
    try {
        await updateBridge(req.body);
        res.status(200).end();
    } catch (err) {
        res.status(500).send(err.toString());
    }
});

// Catch-all route for unknown endpoints
app.use((req, res) => {
    res.status(404).send('Not Found');
});

// Start the server
app.listen(port, hostname, () => {
    console.log(`Server running at http://${hostname}:${port}/`);
});
