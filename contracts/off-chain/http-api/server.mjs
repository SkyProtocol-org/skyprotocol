import http from 'node:http';
import fs from 'node:fs';
import { updateBridge } from './update-bridge.mjs';

const hostname = '0.0.0.0'; // Listen on all network interfaces
const port = 3030;

const server = http.createServer(async (req, res) => {
    if (req.method === 'POST') {
        if (req.url === '/update-bridge') {
            res.statusCode = 200;
            await updateBridge();
            res.end();
        } else if (req.url === '/b') {
            res.statusCode = 200;
            res.setHeader('Content-Type', 'text/plain');
            res.end('You posted to /b');
        } else if (req.url === '/c') {
            res.statusCode = 200;
            res.setHeader('Content-Type', 'text/plain');
            res.end('You posted to /c');
        } else {
            res.statusCode = 404;
            res.end('Not Found');
        }
    } else {
        res.statusCode = 405;
        res.end('Method Not Allowed');
    }
});

server.listen(port, hostname, () => {
    console.log(`Server running at http://${hostname}:${port}/`);
});
