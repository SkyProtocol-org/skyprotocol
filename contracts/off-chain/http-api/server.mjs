import http from 'node:http';
import fs from 'node:fs';
import { updateBridge } from './update-bridge.mjs';

const hostname = '0.0.0.0'; // Listen on all network interfaces
const port = 3030;

const server = http.createServer(async (req, res) => {
    if ((req.method === 'GET') && (req.url === '/')) {
        // Simple status endpoint
        res.statusCode = 200;
        res.end("OK");
    } else if (req.method === 'POST') {
        // Update bridge endpoint
        if (req.url === '/update-bridge') {
            res.statusCode = 200;
            await updateBridge();
            res.end();
        } else {
            res.statusCode = 404;
            res.end('Not Found');
        }
    } else {
        res.statusCode = 400;
        res.end('Bad request');
    }
});

server.listen(port, hostname, () => {
    console.log(`Server running at http://${hostname}:${port}/`);
});
