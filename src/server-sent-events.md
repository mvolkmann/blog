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

All the example code below can be found in the GitHub repository
{% aTargetBlank "https://github.com/mvolkmann/server-sent-events-examples",
"server-sent-events-examples" %}.

## Use Cases

Common uses of SSE include:

- live data feeds such as weather and sports updates
- gathering and displaying information about server-side progress
- client-side logging of server-side activity

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
        const {data, origin, timestamp} = event;
        console.log(data);
        if (data.endsWith('5')) eventSource.close();
      };
      eventSource.onerror = error => {
        console.error("error =", error);
      };
    </script>
  </head>
  <body>
    <h1>Server-Sent Events Demo</h1>
    <p>See output in DevTools Console.</p>
  </body>
</html>
```

The individual messages sent from the server
do not include HTTP headers, so they are very small.

If the client does not close the connection,
the server can be restarted and the client will continue receiving events.

## Demo Server

Responses sent from an SSE endpoint must set
the "Content-Type" header to "text/event–stream" and
the "Transfer-Encoding" header to "chunked".

When using the Node.js Express library,
the "Transfer-Encoding" response header is set to "chunked" automatically
when the request header "Accept" is set to "text/event-stream".

The following code is an example Node.js server that uses the Express library.

```js
import express from 'express';
import {v4 as uuidv4} from 'uuid';

const app = express();
app.use(express.static('public'));

app.get('/sse', (req, res) => {
  res.setHeader('Content-Type', 'text/event-stream');
  let count = 0;
  while (count < 10) {
    count++;
    // Using res.write instead of res.send avoids closing the connection.
    // The text sent must begin with "data:" and end with two newlines.
    // Any spaces after "data:" are automatically removed.
    res.write(`event: count\n`); // optional
    res.write(`id: ${uuidv4()}\n`); // optional
    res.write(`data: ${count}\n\n`);
  }

  // This is invoked when the client calls close on the EventSource.
  res.socket.on('close', () => {
    console.log('server.js: got close event');
    res.end();
  });
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
  "type": "module",
  "scripts": {
    "dev": "nodemon src/server.js",
    "format": "prettier --write '**/*.{css,html,js,ts,tsx}'"
  },
  "dependencies": {
    "express": "^4.18.2",
    "uuid": "^9.0.1"
  },
  "devDependencies": {
    "nodemon": "^3.0.3"
  }
}
```

## Running Demo

To run this demo, enter `npm run dev`, browse localhost:3000,
open the browser DevTools, and view the Console output.

The following screenshot shows the request and response HTTP headers
for the SSE connection.

<img alt="SSE DevTools Network tab Headers" style="width: 100%"
  src="/blog/assets/sse-devtools-network-headers.png?v={{pkg.version}}">

The following screenshot shows the event stream for the SSE connection.

<img alt="SSE DevTools Network tab EventStream" style="width: 70%"
  src="/blog/assets/sse-devtools-network-eventstream.png?v={{pkg.version}}">

## Bun and Hono

The server code is a bit simpler when using Bun and Hono
instead of Node and Express.
The client code can remain the same.

```ts
import {Context, Hono} from 'hono';
import {serveStatic} from 'hono/bun';
import {streamSSE} from 'hono/streaming';

const app = new Hono();
app.use('/*', serveStatic({root: './public'}));

app.get('/sse', (c: Context) => {
  return streamSSE(c, async stream => {
    let count = 0;
    while (count < 10) {
      count++;

      await stream.writeSSE({
        id: String(crypto.randomUUID()), // optional
        event: 'count', // optional
        data: String(count) // TODO: Is this required to be a string?
      });
    }
  });

  /*
  // This is invoked when the client calls close on the EventSource.
  // TODO: FIX THIS!  Do you need to capture the streamSSE return value?
  const { res } = c;
  res.socket.on("close", () => {
    console.log("server.js: got close event");
    res.end();
  });
  */
});

export default app;
```

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

TODO: Implement this same demo using vanilla Bun and Bun with Hono.
