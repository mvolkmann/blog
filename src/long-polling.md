---
eleventyNavigation:
  key: Long Polling
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

Polling is a technique that web clients can use
to get changing data from a server.
The easiest way to implementing polling is to
sends requests to a server at fixed time intervals,
such as once every 30 seconds.

Polling is useful in many scenarios such as getting updates to
sports scores, stock prices, and weather forecasts.

Fixed interval polling has drawbacks.
The data presented can be out-of-date until the next request/response completes.
Also, there is more network traffic and server load than necessary
because many consecutive requests can result in the same response.

Long polling addresses the issues with fixed interval polling.
The client sends an initial request for data.
The server does not respond until there is a change in the data to be reported.
When the client receives the data, it immediately requests another update.

All the example code below can be found in the GitHub repository
{% aTargetBlank "https://github.com/mvolkmann/long-polling-examples",
"long-polling-examples" %}.

## Demo Client

The following code is an example client HTML file that
connects to the server and receives long polling responses.
This uses a bit of <a href="/blog/topics/#/blog/alpine/">Alpine</a>
to simplify the DOM manipulation.

```js
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Long Polling Demo</title>
    <script defer src="//unpkg.com/alpinejs"></script>
    <script defer>
      async function longPoll() {
        const res = await fetch('/score');
        const score = await res.text();
        const scoreP = document.getElementById('score');
        scoreP.innerText = score;

        // When a score is received, immediately ask for the next update.
        longPoll();
      }

      longPoll();
    </script>
  </head>
  <body x-data="{name: '', score: ''}">
    <h1>Long Polling Demo</h1>

    <p id="score"></p>

    <!-- This demonstrates that user interaction is not blocked
         while waiting for the next score update. -->
    <label>
      Name:
      <input type="text" x-model="name" />
    </label>
    <p x-show="name">Hello, <span x-text="name"></span>!</p>
  </body>
</html>
```

If the server is restarted, the client will not automatically reconnect.

## Demo Server

Responses sent from a long polling endpoint must
set the "Transfer-Encoding" header to "chunked".

The following server code uses Node.js and the
{% aTargetBlank "https://expressjs.com", "Express" %} library.

```js
import express from 'express';

const app = express();
app.use(express.static('public'));

let chiefsHaveBall = true;
let bills = 0;
let chiefs = 0;
let lastScore;

// Randomly get points for a touchdown, field goal, or nothing.
function getPoints() {
  const number = Math.floor(Math.random() * 10);
  const touchdown = 7;
  const fieldGoal = 3;
  return number >= 8 ? touchdown : number >= 6 ? fieldGoal : 0;
}

function getScore() {
  if (chiefsHaveBall) {
    chiefs += getPoints();
  } else {
    bills += getPoints();
  }
  chiefsHaveBall = !chiefsHaveBall;
  return `Chiefs: ${chiefs}, Bills: ${bills}`;
}

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));

app.get('/score', async (req, res) => {
  let score;
  while (true) {
    score = getScore();
    if (score !== lastScore) break;
    await sleep(2000);
  }

  lastScore = score;
  res.setHeader('Transfer-Encoding', 'chunked');
  res.write(score);
  res.end();
});

app.listen(3000, function () {
  console.log('listening on port', this.address().port);
});
```

The following `package.json` file can be used to start the Node server.

```json
{
  "name": "long-polling",
  "type": "module",
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

## Running Demo

To run this demo, enter `npm install`, enter `npm run dev`,
and browse localhost:3000.
The scores will continually update, but not at a fixed rate.
Requests for score updates do not block user interactions.
A name can be entered in the text input
and a greeting will appear below it.

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
The client code remains the same.

```ts
import {Hono} from 'hono';
import type {Context} from 'hono';
import {serveStatic} from 'hono/bun';

const app = new Hono();
app.use('/*', serveStatic({root: './public'}));

let chiefsHaveBall = true;
let bills = 0;
let chiefs = 0;
let lastScore;

// Randomly get points for a touchdown, field goal, or nothing.
function getPoints() {
  const number = Math.floor(Math.random() * 10);
  const touchdown = 7;
  const fieldGoal = 3;
  return number >= 8 ? touchdown : number >= 6 ? fieldGoal : 0;
}

function getScore() {
  if (chiefsHaveBall) {
    chiefs += getPoints();
  } else {
    bills += getPoints();
  }
  chiefsHaveBall = !chiefsHaveBall;
  return `Chiefs: ${chiefs}, Bills: ${bills}`;
}

app.get('/score', async (c: Context) => {
  let score;
  while (true) {
    score = getScore();
    if (score !== lastScore) break;
    await Bun.sleep(2000);
  }

  lastScore = score;
  c.header('Transfer-Encoding', 'chunked');
  return c.text(score);
});

export default app;
```

The following `package.json` file can be used to start the Bun server.

```json
{
  "name": "bun-long-polling",
  "type": "module",
  "scripts": {
    "dev": "bun run --watch src/server.ts"
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

To run this demo, enter `bun install`, enter `bun dev`,
and browse localhost:3000.

## Alternatives

Another alternative is
<a href="/blog/topics/#/blog/server-sent-events/" target="_blank">Server-Sent Events</a>.

If data must be sent in both directions, consider using
<a href="/blog/topics/#/blog/websockets" target="_blank">WebSockets</a>.
