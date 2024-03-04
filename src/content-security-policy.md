---
eleventyNavigation:
  key: Content Security Policy (CSP)
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

<a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP"
target="_blank">Content Security Policy</a> provides
the ability to detect and prevent some types of attacks,
including Cross-Site Scripting (XSS) and data injection.
It can also report attack attempts.

There are two ways to enable CSP.

1. "Content-Security-Policy" HTTP response header
1. HTML `meta` tag

It both cases, the policy is specified by a list of
directives separated by semicolons.
Each directive is specified with a name and one or more values,
all separated by a space.
The values are allowed URL patterns or the keyword "self".

The following `meta` tag is an example.

```html
<meta
  http-equiv="Content-Security-Policy"
  content="default-src 'self'; img-src https://*; script-src 'none';"
/>
```

Some features can only be enabled in an HTTP header. Which?

can only execute scripts downloaded from approved domains using approved protocols (ex. HTTPS)

## Directives

Commonly used directives include:

- `default-src`: restricts access to all kinds of resources
- `connect-src`: restricts use of <a>, fetch, XMLHttpRequest, WebSocket, and more
- `font-src`: restricts @font-face CSS at-rule
- `form-action`: restricts URLs used in `form` element `action` attributes
- `img-src`: restricts `<img>` elements
- `media-src`: restricts `<audio>` and `<video>` elements
- `object-src`: restricts `<object>` and `<embed>` elements
- `report-to`: specifies the URL where violation reports are sent
- `script-src-attr`: restricts sources for JavaScript inline event handlers like `onclick`
- `script-src-elem`: restricts `<script>` elements
- `script-src`: combines the previous two in one directive
- `worker-src`: restricts Worker, SharedWorker, and ServiceWorker scripts

It is recommended to make `default-src` very restrictive (maybe just "self")
and used more specific directives to open access for specific kinds of resources.

See <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP"
target="_blank">Content Security Policy</a>
for a table of CSP directives that are supported by each browser.

## Example Header Values

- `Content-Security-Policy: default-src 'self' demo.com *.demo.com`

  This specifies that by default all resources must come from
  the same domain ss this request OR
  from `demo.com` or any domain that ends in `demo.com`.

- `Content-Security-Policy: default-src 'self'; img-src *; media-src my-media.org; script-src https://coder.io`

  This specifies that images can come from anywhere,
  audio and video can come from `my-media.org`,
  scripts can come from `coder.io` only using HTTPS,
  and all other resources must come from the same domain as this request.
  `default-src` specifies the policy for all resource types
  unless policies for specific resource types are also provided.

## Reporting

To report attempts to violate the CSP but not prevent them,
use the `Content-Security-Policy-Report-Only` header.
It seems odd to want only report these attempts.

Specify the `Content-Security-Policy` AND `Content-Security-Policy-Report`
headers to both prevent violations and report attempted violations.
The policies specified in each can be the same or they can differ.

Include the `report-to: {url}` directive in the
`Content-Security-Policy-Report` header to indicate
where POST requests including violation details should be sent.
This must be specified in an HTTP response header, not in a `meta` tag.

The report is a JSON object with many properties including:

- `blocked-uri` gives the URI that violated a policy.
- `disposition` will be "enforce" or "report".
- `document-uri` gives the URI of the document that requested the resource.
- `effective-directive` gives the directive that was violated.
- `script-sample` gives the first 40 characters of violating script or CSS.

The following is an example report that describes
an issue with getting an image from Unsplash.
Note the properties "effective-directive" and "blocked-uri".

```json
{
  "csp-report": {
    "document-uri": "http://localhost:3000/",
    "referrer": "http://localhost:3000/",
    "violated-directive": "img-src",
    "effective-directive": "img-src",
    "original-policy": "default-src 'self'; connect-src 'self' https://jsonplaceholder.typicode.com ws:; font-src 'self' https://fonts.googleapis.com https://fonts.gstatic.com; media-src 'self' http://commondatastorage.googleapis.com; script-src-elem 'self' https://unpkg.com; style-src 'self' 'unsafe-inline' https://fonts.googleapis.com; report-uri /csp-report",
    "disposition": "report",
    "blocked-uri": "https://images.unsplash.com/photo-1629985692757-48648f4f1fc1",
    "line-number": 55,
    "source-file": "http://localhost:3000/",
    "status-code": 200,
    "script-sample": ""
  }
}
```

## Example

The following code implements an HTTP server using the JavaScript-based
<a href="https://hono.dev" target="_blank">Hono</a> library.

```typescript
import {type Context, Hono, type Next} from 'hono';
import {serveStatic} from 'hono/bun';
import './reload-server.js';

const policies = [
  // Only resources from the current domain are allowed
  // unless overridden by a more specific directive.
  "default-src 'self'",

  // This allows sending HTTP requests to the JSONPlaceholder API.
  // It also allows reload-client.js to create a WebSocket.
  "connect-src 'self' https://jsonplaceholder.typicode.com ws:",

  // This allows getting Google fonts.
  // "link" tags for Google fonts have an href
  // that begins with https://fonts.googleapis.com.
  // The linked font file contains @font-face CSS rules
  // with a src URL beginning with https://fonts.gstatic.com.
  "font-src 'self' https://fonts.googleapis.com https://fonts.gstatic.com",

  // This allows getting images from Unsplash.
  "img-src 'self' https://images.unsplash.com",

  // This allows getting videos from googleapis.
  "media-src 'self' http://commondatastorage.googleapis.com",

  // This allows downloading the htmx library from a CDN.
  "script-src-elem 'self' https://unpkg.com",

  // This allows htmx.min.js to insert style elements.
  "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com"
];
const csp = policies.join('; ');
console.log('server.tsx: csp =', csp);

const app = new Hono();

// Serve static files from the public directory.
// app.use('/*', serveStatic({root: './public'}));
app.use('/*', (c: Context, next: Next) => {
  c.header('Content-Security-Policy', csp);
  c.header(
    'Content-Security-Policy-Report-Only',
    csp + '; report-uri /csp-report'
  );
  const fn = serveStatic({root: './public'});
  return fn(c, next);
});

app.get('/version', (c: Context) => {
  // Return a Response whose body contains
  // the version of Bun running on the server.
  return c.text('v' + Bun.version);
});

// This receives reports of CSP violations in a JSON object.
app.post('/csp-report', async (c: Context) => {
  const report = await c.req.json();
  console.log(report);
  c.status(403);
  return c.text('CSP violation');
});

export default app;
```

The following HTML file in `public/index.html` relies on
the CSP defined in the server to access several resources.

```html
<html>
  <head>
    <title>CSP Demo</title>

    <!-- This loads a Google font. -->
    <link
      rel="stylesheet"
      href="https://fonts.googleapis.com/css?family=Kode+Mono"
    />

    <link rel="stylesheet" href="styles.css" />

    <!-- This loads the htmx library from a CDN. -->
    <script
      src="https://unpkg.com/htmx.org@1.9.10"
      integrity="sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
      crossorigin="anonymous"
    ></script>
    <!-- We could get the htmx library from
         a local copy instead of from a CDN. -->
    <!-- <script src="htmx.min.js"></script> -->

    <script src="reload-client.js" type="module"></script>
  </head>
  <body>
    <h2>This demonstrates the Google font "Kode Mono".</h2>

    <img
      alt="Grand Prismatic Spring"
      src="https://images.unsplash.com/photo-1629985692757-48648f4f1fc1"
      width="300"
    />

    <video
      src="http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4"
      controls
      width="300"
    ></video>

    <div>
      <!-- When this button is clicked,
           an HTTP GET request is sent to /version.
           The text it returns replaces the innerHTML
           of the element with id "version". -->
      <button hx-get="/version" hx-target="#version">Get Bun Version</button>
      <span id="version"></span>
    </div>

    <!-- This should require the form-action CSP directive, but it doesn't. -->
    <!-- <form method="post" action="https://jsonplaceholder.typicode.com/todos"> -->

    <form
      hx-post="https://jsonplaceholder.typicode.com/todos"
      hx-target="#todo"
    >
      <label>Title:<input type="text" name="title" value="" /></label>
      <label>Body:<input type="text" name="body" value="" /></label>
      <button>Submit</button>
    </form>
    <div id="todo"></div>
  </body>
</html>
```

## Questions

Does using a CSP remove the need to sanitize HTML?

In "stored cross-site scripting", script tags entered in text input
get stored in databases and later displayed on pages.
They are not executed if they are used as `textContent`,
but are if they are used as `innerHTML`.

There are other types of cross-site scripting attacks. What are they?
