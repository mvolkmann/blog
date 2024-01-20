---
eleventyNavigation:
  key: Server-Sent Events (SSE)
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events",
"Server-Sent Events" %} (SSE) are used to send data from a server to a client,
but not in the other direction.

SSE is built on HTTP.

Messages are sent over TCP, not UDP, so there is
some overhead for error checking and message coordination.

For a great video on SSE, see {% aTargetBlank
"https://www.youtube.com/watch?v=4HlNv1qpZFY",
"Server-Sent Events Crash Course" %}.

## Demo Client

The following code is an example client HTML file that
connects to the server and receives server-sent events.
Note the use of the class `EventSource`.

```js
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>SSE Demo</title>
    <script>
      const eventSource = new EventSource('http://localhost:3000/sse');
      eventSource.onmessage = event => {
        console.log(event.data);
      };
    </script>
  </head>
  <body>
    <h1>Server-Sent Events Demo</h1>
    <p>See output in DevTools Console.</p>
  </body>
</html>
```

## Demo Server

Responses sent from an SSE endpoint must set
the "Content-Type" header to "text/event–stream" and
the "Transfer-Encoding" header to "chunked".

When using the Node.js Express library,
the "Transfer-Encoding" response header is set to "chunked"
automatically when the request header "Accept" is set to "text/event-stream".

The following code is an example Node.js server that uses the Express library.

```js
const express = require('express');
const app = express();
app.use(express.static('public'));

let count = 0;
app.get('/sse', (req, res) => {
  res.setHeader('Content-Type', 'text/event-stream');
  while (count < 10) {
    count++;
    res.write(`data:demo${count}\n\n`);
  }
});

app.listen(3000, function () {
  console.log('listening on port', this.address().port);
});
```

The following `package.json` file can be used to start the server.

```json
{
  "name": "node-sse",
  "version": "1.0.0",
  "scripts": {
    "dev": "nodemon src/server.js"
  },
  "dependencies": {
    "express": "^4.18.2"
  },
  "devDependencies": {
    "nodemon": "^3.0.3"
  }
}
```

To run this demo, enter `npm run dev`, browse localhost:3000,
open the browser DevTools, and view the Console output.

The server is responsible for deciding whether to keep the connection alive.

## Use Cases

Common uses of SSE include:

- live data feeds such as weather and sports updates
- gathering and displaying information about server-side progress
- client-side logging of server-side activity

## Alternatives

If data must be sent in both directions,
consider using long polling or WebSockets.

With long polling, the server can delay sending a response.

### Long Polling

### WebSockets

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API",
"WebSockets" %} can send data in both directions.
This is a major reason to choose WebSockets over SSE.

For a great video on WebSockets, see {% aTargetBlank
"https://www.youtube.com/watch?v=2Nt-ZrNP22A", "WebSockets Crash Course" %}.

TODO: Show how to use Web sockets with HTMX.

when sending data from the server, write to the stream, but do not close it until the server is finished. Sending data on that connection.
must prefix data with “data:” and end with two newlines.
The data will be in the data property of the MessageEvent Object that is created in Node.js.
you can restart the server without restarting the client and the client will continue getting messages.
look at all the properties in EventMessage objects besides the data property.
individual messages do not include HTTP headers, so they are very small.
