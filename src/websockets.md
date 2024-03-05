---
eleventyNavigation:
  key: WebSockets
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="WebSocket logo" style="border: none; width: 30%"
  src="/blog/assets/websocket-logo.svg?v={{pkg.version}}">

## Overview

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_client_applications",
"WebSockets" %} are a standardized protocol for
two-way communication between clients and servers using TCP.
They are widely supported by web browsers.

For one-way communication from servers to clients, consider using
<a href="/blog/topics/#/blog/server-sent-events/"
target="_blank">Server-Sent Events</a> (SSE).
WebSockets support text and binary data, whereas SSE only supports UTF-8 text.
When only text is needed, SSE is a good option because
the required code is a bit easier to write than the code for WebSockets.

WebSockets require upgrading an existing HTTP connection.
For details, see the MDN page {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/HTTP/Protocol_upgrade_mechanism",
"Protocal upgrade mechanism" %}.

Unlike Server-Sent Events (SSE), WebSockets do not provide
automatic reconnection after the connection is closed.
However, this can be implemented as show in the "Bun and Hono" section below.

Many WebSocket libraries for server-side
programming languages/environments exist.
The example below demonstrates using the highly popular Node.js library
{% aTargetBlank "https://github.com/websockets/ws", "ws" %}.

All the example code below can be found in the GitHub repository
{% aTargetBlank "https://github.com/mvolkmann/websocket-examples",
"websocket-examples" %}.

## Use Cases

Common uses of WebSockets include:

- live data feeds such as weather and sports updates
- gathering and displaying information about server-side progress
- client-side logging of server-side activity

## Issues

WebSockets do not provide automatic reconnection.
If a WebSocket connection is closed, perhaps due to a network issue,
clients will not automatically attempt to reconnect to the server.
Additional code must be written to poll the server and reconnect.

Some WebSocket libraries such as {% aTargetBlank "https://socket.io",
"Socket.IO" %} provide reconnection support.
This is also a built-in feature of Server-Sent Events (SSE).

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

The following code is an example client HTML file
that connects to the WebSocket server and receives messages.
It differs from the previous example in that it automatically attempts
to reconnect to the WebSocket server if the connection is closed.

<img alt="WebSocket reconnecting" style="width: 50%"
  src="/blog/assets/websocket-reconnecting.png?v={{pkg.version}}">

```js
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>WebSocket Demo</title>
    <link rel="stylesheet" href="styles.css" />
    <script
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
      integrity="sha384-U7nmwYozlJwxnpCeC+X+JOS3WWNqSfJKSgSs/VAaek8V0QOo5txs7K1MYuKpjzkI"
      crossorigin="anonymous"
    ></script>
    <script>
      let ws;

      function connect() {
        console.log('attempting WebSocket connection');
        ws = new WebSocket('ws://localhost:3001');

        ws.onopen = () => {
          console.log('WebSocket connection was opened');
          ws.send('Hello from client!');
        };

        ws.onmessage = event => {
          const received = document.getElementById('received');
          received.textContent = event.data;
        };

        ws.onerror = error => {
          console.log('ws error:', error);
        };

        ws.onclose = () => {
          console.log('WebSocket connection was closed');
          // Attempt to reconnect after two seconds.
          setTimeout(connect, 2000);
        };
      }

      connect();

      function close() {
        // Either the client or the server can close the connection.
        // ws.send('stop'); // ask server to close the WebSocket
        ws?.close(); // close WebSocket from client
      }

      function send(event, message) {
        const form = event.target;
        ws?.send(message);
        form.reset();
      }
    </script>
  </head>
  <body x-data="{message: ''}">
    <h1>WebSockets Demo</h1>
    <form @submit.prevent="send(event, message)">
      <label>
        Message
        <input type="text" x-model="message" />
      </label>
      <button>Send</button>
    </form>
    <div>
      <button @click="close()">Close WebSocket Connection</button>
    </div>
    <fieldset>
      <legend>Last Message Received</legend>
      <div id="received"></div>
    </fieldset>
  </body>
</html>
```

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
    // TODO: Why is this never called?
    drain(ws) {
      console.log('WebSocket is ready to receive more data.');
    },
    message(ws, message) {
      console.log(`received "${message}"`);
      if (message === 'stop') {
        ws.close();
      } else {
        ws.send(`Thank you for sending "${message}".`);
      }
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

## Alternatives

In scenarios where data only needs to be sent from servers to clients,
consider using
<a href="/blog/topics/#/blog/long-polling/" target="_blank">Long Polling</a>.
or
<a href="/blog/topics/#/blog/server-sent-events"
target="_blank">Server-Sent Events</a>.
