---
eleventyNavigation:
  key: WebSockets
layout: topic-layout.njk
---

## Overview

WebSockets are a standardized protocol for two-way communication
between clients and servers using TCP.
They are widely supported by web browsers.

Many WebSocket libraries for server-side
programming languages/environments exist.
The example below demonstrates using the highly popular Node.js library
{% aTargetBlank "https://github.com/websockets/ws", "ws" %}.
This of course requires installing
{% aTargetBlank "https://nodejs.org/", "Node.js" %}.

## Server

To run the server:

- Enter `npm install`
- Enter `npm run start`

This uses {% aTargetBlank "https://nodemon.io", "nodemon" %}
to automatically restart the server when its code is modified,
which is useful during iterative development and debugging.

### Server package.json

```json
{
  "name": "server",
  "version": "1.0.0",
  "description": "WebSockets demo server",
  "author": "R. Mark Volkmann",
  "license": "MIT",
  "type": "module",
  "scripts": {
    "start": "nodemon server.js"
  },
  "dependencies": {
    "ws": "^7.4.5"
  },
  "devDependencies": {
    "nodemon": "^2.0.7"
  }
}
```

### server.js

```js
import WebSocket from 'ws';

// Create a WebSocket server.
const wss = new WebSocket.Server({port: 1919});

// When a client connects ...
wss.on('connection', ws => {
  // Listen for messages from the client.
  ws.on('message', message => {
    // Broadcast the message to all the clients.
    // wss.clients is not an Array, so you cannot use a for-of loop.
    wss.clients.forEach(client => {
      const isOpen = client.readyState === WebSocket.OPEN;

      // To send to all open clients,
      // including the one that sent the message ...
      if (isOpen) client.send(message);

      // To send to all open clients
      // except the one that sent the message ...
      //const isSelf = client === ws;
      //if (isOpen && !isSelf) client.send(message);
    });
  });

  // Send an initial message to the newly connected client.
  ws.send('connected to WebSocket server');
});
```

## Client

To run the client:

- Enter `npm install`
- Enter `npm run start`
- Browse `localhost:1920` in multiple browser windows.
- Enter a message in the "Send" input and
  press the Enter key or click the "Send" button to send it.
- The message will appear in all connected browser windows.
- Try sending multiple messages from each connected browser window.

### Client package.json

```json
{
  "name": "client",
  "version": "1.0.0",
  "description": "WebSockets demo client",
  "author": "R. Mark Volkmann",
  "license": "MIT",
  "scripts": {
    "start": "http-server --port 1920"
  },
  "devDependencies": {
    "http-server": "^0.12.3"
  }
}
```

### index.html

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />

    <script>
      window.onload = () => {
        // Find various DOM elements.
        const errorMessage = document.getElementById('error-message');
        const form = document.getElementById('form');
        const receivedArea = document.getElementById('received-area');
        const sendInput = document.getElementById('send-input');
        const status = document.getElementById('status');

        // Open a connection to the WebSocket server.
        let wsOpen = false;
        const WS_URL = 'ws://localhost:1919';
        const ws = new WebSocket(WS_URL);

        // When the WebSocket connection is opened ...
        ws.addEventListener('open', event => {
          wsOpen = true;
          status.textContent = 'The WebSocket is open.';
        });

        // When the WebSocket connection is closed ...
        ws.addEventListener('close', event => {
          wsOpen = false;
          status.textContent =
            'The WebSocket is closed. ' + 'Refresh when the server is ready.';
        });

        // When a WebSocket message is received ...
        ws.addEventListener('message', event => {
          // Display the message in the received area of the UI.
          const div = document.createElement('div');
          div.textContent = event.data;
          receivedArea.append(div);
        });

        // When a WebSocket error occurs ...
        ws.addEventListener('error', event => {
          // For security reasons, there is no
          // useful information in this event object.
          errorMessage.textContent = `Failed to connect to ${WS_URL}.`;
        });

        // When the form containing a message to send is submitted ...
        form.addEventListener('submit', event => {
          event.preventDefault();
          if (wsOpen) {
            // Send the message to the WebSocket server.
            ws.send(sendInput.value);
            sendInput.value = '';
          } else {
            errorMessage.textContent =
              'Cannot send because the WebSocket is closed.';
          }
        });
      };
    </script>

    <style>
      #error-message {
        color: red;
      }

      #form {
        display: flex;
      }

      input,
      label {
        margin-right: 0.5rem;
      }

      label {
        font-weight: bold;
      }
    </style>
  </head>
  <body>
    <div id="status"></div>
    <div id="error-message"></div>
    <form id="form">
      <label>Send</label>
      <input id="send-input" autofocus />
      <button>Send</button>
    </form>
    <div>
      <label>Received</label>
      <div id="received-area"></div>
    </div>
  </body>
</html>
```

## Bun and Hono

The server code is a bit simpler when using Bun and Hono instead of Node.
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
    if (server.upgrade(req)) {
      return; // do not return a Response
    }
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
