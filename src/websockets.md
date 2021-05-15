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

### Server `package.json`

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

### `server.js

```js
import WebSocket from 'ws';

const wss = new WebSocket.Server({port: 1919});

wss.on('connection', ws => {
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

  ws.send('connected to WebSocket server');
});
```

## Client

To run the client:

- Enter `npm install`
- Enter `npm run start`
- Browse `localhost:8080` in multiple browser windows.
- Enter a message in the "Send" input and
  press the Enter key or click the "Send" button to send it.
- The message will appear in all connected browser windows.
- Try sending multiple messages from each connected browser window.

### Client `package.json`

```json
{
  "name": "client",
  "version": "1.0.0",
  "description": "WebSockets demo client",
  "author": "R. Mark Volkmann",
  "license": "MIT",
  "scripts": {
    "start": "http-server"
  },
  "devDependencies": {
    "http-server": "^0.12.3"
  }
}
```

### `index.html`

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />

    <script>
      window.onload = () => {
        const errorMessage = document.getElementById('error-message');
        const form = document.getElementById('form');
        const receivedArea = document.getElementById('received-area');
        const sendInput = document.getElementById('send-input');
        const status = document.getElementById('status');

        let wsOpen = false;

        form.addEventListener('submit', event => {
          event.preventDefault();
          if (wsOpen) {
            ws.send(sendInput.value);
            sendInput.value = '';
          } else {
            errorMessage.textContent =
              'Cannot send because WebSocket is closed.';
          }
        });

        const WS_URL = 'ws://localhost:1919';
        const ws = new WebSocket(WS_URL);

        ws.addEventListener('open', event => {
          wsOpen = true;
          status.textContent = 'The WebSocket is open.';
        });

        ws.addEventListener('close', event => {
          wsOpen = false;
          status.textContent =
            'The WebSocket is closed. ' + 'Refresh when the server is ready.';
        });

        ws.addEventListener('message', event => {
          const div = document.createElement('div');
          div.textContent = event.data;
          receivedArea.append(div);
        });

        ws.addEventListener('error', event => {
          // By design, there is no useful information in this event object.
          errorMessage.textContent = `Failed to connect to ${WS_URL}.`;
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
