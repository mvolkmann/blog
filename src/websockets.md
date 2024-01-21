---
eleventyNavigation:
  key: WebSockets
layout: topic-layout.njk
---

## Overview

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_client_applications",
"WebSockets" %} are a standardized protocol for
two-way communication between clients and servers using TCP.
They are widely supported by web browsers.

Many WebSocket libraries for server-side
programming languages/environments exist.
The example below demonstrates using the highly popular Node.js library
{% aTargetBlank "https://github.com/websockets/ws", "ws" %}.

All the example code below can be found in the GitHub repository
{% aTargetBlank "https://github.com/mvolkmann/websocket-examples",
"websocket-examples" %}.

## Use Cases

TODO: Add this section.

## Issues

TODO: Add this section.

## Demo Client

The following code is an example client HTML file
that connects to the WebSocket server and receives messages.
Note the use of the class `WebSocket`.

```js
<!DOCTYPE html>
<html>
  <head>
    <title>WebSocket Demo</title>
    <script>
      const ws = new WebSocket('ws://localhost:3001');
      ws.onopen = () => {
        console.log('ws open');
        ws.send('Hello from client!');
      };
      ws.onmessage = event => {
        console.log(`received "${event.data}"`);

        // Either the client or the server can close the connection.
        // ws.send('stop'); // ask server to close the WebSocket
        ws.close(); // close WebSocket from client
      };
      ws.onerror = error => {
        console.log('ws error:', error);
      };
      ws.onclose = () => {
        console.log('ws closed');
      };
    </script>
  </head>
  <body>
    <h1>WebSockets Demo</h1>
    <p>See output in DevTools Console.</p>
  </body>
</html>
```

## Demo Server

The following server code uses Node.js,
the {% aTargetBlank "https://expressjs.com", "Express" %} framework, and the
{% aTargetBlank "https://www.npmjs.com/package/ws", "ws" %} WebSocket library.
It also uses {% aTargetBlank "https://nodemon.io", "nodemon" %}
to automatically restart the server when its code is modified,
which is useful during iterative development and debugging.

```js
import express from 'express';
import WebSocket from 'ws';

const app = express();
app.use(express.static('public'));

app.get('/greet', (req, res) => {
  res.send('Hello World!');
});

// Create a WebSocket server.
const wsServer = new WebSocket.Server({port: 3001});

// When a client connects ...
wsServer.on('connection', ws => {
  ws.onopen = () => {
    console.log('WebSocket is open.');
  };

  // Listen for messages from the client.
  // ws.on('message', message => {
  ws.onmessage = event => {
    const message = event.data;
    // console.log('server.js onmessage: event =', event);
    console.log(`received "${message}"`);
    if (message === 'stop') {
      ws.close();
    } else {
      ws.send('Hello from server!');
    }

    /*
    // Broadcast the message to all the clients.
    // wsServer.clients is not an Array, so you cannot use a for-of loop.
    wsServer.clients.forEach(client => {
      const isOpen = client.readyState === WebSocket.OPEN;

      // To send to all open clients,
      // including the one that sent the message ...
      if (isOpen) client.send(message);

      // To send to all open clients
      // except the one that sent the message ...
      //const isSelf = client === ws;
      //if (isOpen && !isSelf) client.send(message);
    });
    */
  };

  ws.onerror = error => {
    console.error('WebSocket error:', error);
  };

  ws.onclose = () => {
    console.log('WebSocket is closed.');
  };
});

app.listen(3000, function () {
  console.log('listening on port', this.address().port);
});
```

The following `package.json` file can be used to start the server.

```json
{
  "name": "node-websockets",
  "type": "module",
  "scripts": {
    "dev": "nodemon src/server.js",
    "format": "prettier --write '**/*.{css,html,js,ts,tsx}'"
  },
  "dependencies": {
    "express": "^4.18.2",
    "ws": "^7.4.5"
  },
  "devDependencies": {
    "nodemon": "^2.0.7"
  }
}
```

## Running Demo

To run this demo, enter `npm install`, enter `npm run dev`,
browse localhost:3000, open the browser DevTools, and view the Console output.

The following screenshot shows the request and response HTTP headers
for the WebSocket connection.

<img alt="WebSockets DevTools Network tab Headers" style="width: 100%"
  src="/blog/assets/websockets-node-devtools-network-headers.png?v={{pkg.version}}">

The following screenshot shows the messages for the WebSocket connection.

<img alt="WebSockets DevTools Network tab Messages" style="width: 70%"
  src="/blog/assets/websockets-node-devtools-network-messages.png?v={{pkg.version}}">

## Bun and Hono

The following server code uses Bun and
the {% aTargetBlank "https://hono.dev", "Hono" %} framework.
The client code remains the same.

```ts
import {Hono} from 'hono';
import type {Context} from 'hono';
import {serveStatic} from 'hono/bun';

const app = new Hono();
app.use('/*', serveStatic({root: './public'}));

app.get('/greet', (c: Context) => {
  return c.text('Hello Bun!');
});

const wsServer = Bun.serve({
  // The WebSocket port defaults to 3000 which conflicts with the HTTP server.
  port: 3001,
  fetch(req, server) {
    // Upgrade the request to support WebSockets.
    if (server.upgrade(req)) return; // no Response
    return new Response('WebSockets upgrade failed', {status: 500});
  },
  websocket: {
    open(ws) {
      console.log('WebSocket is open.');
    },
    // TODO: Wby is this never called?
    drain(ws) {
      console.log('WebSocket is ready to receive more data.');
    },
    message(ws, message) {
      console.log(`received "${message}"`);
      if (message === 'stop') {
        ws.close();
      } else {
        ws.send('Hello from server!');
      }
    },
    error(ws, error) {
      console.error('WebSocket error:', error);
    },
    // See WebSocket protocol status codes at
    // https://datatracker.ietf.org/doc/html/rfc6455#section-7.4
    // 1000 is normal closure.
    // 1005 is used when the client closes the WebSocket.
    close(ws, code, message) {
      console.log('WebSocket closed with code', code);
      if (message) console.log(`WebSocket closed with message "${message}"`);
    }
  }
});

console.log('WebSocket server is listening on port', wsServer.port);

export default app;
```

The following `package.json` file can be used to start the Bun server.

```json
{
  "name": "bun-websockets",
  "type": "module",
  "scripts": {
    "dev": "bun run --watch src/server.ts",
    "format": "prettier --write '**/*.{css,html,js,ts,tsx}'"
  },
  "dependencies": {
    "hono": "^3.12.6"
  },
  "devDependencies": {
    "@types/bun": "latest"
  },
  "peerDependencies": {
    "typescript": "^5.0.0"
  }
}
```
