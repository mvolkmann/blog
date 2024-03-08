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

A <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP"
target="_blank">Content Security Policy</a> provides
the ability to detect and prevent some types of attacks,
including Cross-Site Scripting (XSS).
It can also report attempted attacks.

A CSP can be enabled with an HTTP response header or with an HTML `meta` tag.
It both cases, the policy is specified by a list of
directives separated by semicolons.

Each directive is specified with a name and one or more values,
all separated by a space.
The values are CSP-specific keywords (such as `self`)
and/or allowed URL patterns.

The following `meta` tag provides an example.
It specifies that by default all resource types
can only be downloaded from the current origin.
An exception is made for images which can
come from any origin as long as HTTPS is used.
No scripts are allowed to be loaded, even those from the current origin.

```html
<meta
  http-equiv="Content-Security-Policy"
  content="default-src 'self'; img-src https://*; script-src 'none';"
/>
```

Using a CSP reduces, but does not eliminate
the need to sanitize and/or escape user-supplied content
that is inserted into HTML.

## Directives

The following table describes commonly used CSP directives.

| Directive         | Description                                                              |
| ----------------- | ------------------------------------------------------------------------ |
| `default-src`     | restricts access to all kinds of resources                               |
| `connect-src`     | restricts use of `<a>`, `fetch`, `XMLHttpRequest`, `WebSocket`, and more |
| `font-src`        | restricts use of the `@font-face` CSS at-rule                            |
| `form-action`     | restricts `<form>` element `action` attributes                           |
| `img-src`         | restricts `<img>` elements                                               |
| `media-src`       | restricts `<audio>` and `<video>` elements                               |
| `object-src`      | restricts `<object>` and `<embed>` elements                              |
| `report-uri`      | specifies the URL where violation reports are sent                       |
| `script-src-attr` | restricts sources for JavaScript inline event handlers like `onclick`    |
| `script-src-elem` | restricts `<script>` elements                                            |
| `script-src`      | combines the previous two directives into one                            |
| `worker-src`      | restricts `Worker`, `SharedWorker`, and `ServiceWorker` scripts          |

The `default-src` directive specifies the policy for all resource types
unless policies for specific resource types are also provided.

It is recommended to make `default-src` very restrictive
(typically just `'self'`) and supply more targeted directives to
open access for specific kinds of resources.

The `report-uri` directive will be replaced by `report-to` in the future.

A small set of directives that are not commonly used
can only be specified in HTTP headers and not in `meta` tags.
The only commonly used directive that must be
specified in an HTTP header is the `report-uri` directive.

See <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP#browser_compatibility"
target="_blank">Content Security Policy</a>
for a table of CSP directives that are supported by each browser.

## Keywords

CSP keywords are surrounded by single quotes
to distinguish them from URL patterns.

The following table describes the keywords that can be used in directive values.

| Keyword                    | Description                                                                                                                                                             |
| -------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `inline-speculation-rules` | This allows inclusion of "speculation rules" which are experimental.                                                                                                    |
| `nonce-*`                  | This is a whitelist of inline scripts, indentified by a cryptographic nonce value, that are allowed.                                                                    |
| `none`                     | This prevents loading any resources of a given type.                                                                                                                    |
| `report-sample`            | This causes a sample of the violating code to be included in violation reports. It is used in `script-src` and `script-src-elem` directives.                            |
| `self`                     | This only allows loading resources from the current origin. It is the most commonly used keyword.                                                                       |
| `sha{algorithm}-{value}`   | This is used in `script-src` and `styles-src` directives to allow resources with a matching hash value.                                                                 |
| `strict-dynamic`           | This allows dynamically generated JavaScript code to be executed only if it is generated by a script that is whitelisted using the `nonce-*` keyword.                   |
| `unsafe-eval`              | This enables use of the JavaScript `eval` function, the `Function` constructor, and passing strings of JavaScript code to the `setTimeout` and `setInterval` functions. |
| `unsafe-inline`            | This enables evaluating inline `script` elements, `javascript:` URLs, inline event handlers, and inline `style` elements.                                               |
| `unsafe-hashes`            | This enables evaluating inline event handling functions, which is a subset of what `unsafe-inline` enables.                                                             |
| `wasm-unsafe-eval`         | This enables loading and executing WebAssembly modules.                                                                                                                 |

## Example CSP Headers

- `Content-Security-Policy: default-src 'self' demo.com *.demo.com`

  This header specifies that by default all resources must come from
  the same domain as this request, `demo.com`,
  or any domain that ends in `demo.com`.

- `Content-Security-Policy: default-src 'self'; img-src *; media-src my-media.org; script-src https://coder.io`

  This header specifies that images can come from anywhere,
  audio and video can come from `my-media.org`,
  scripts can come from `coder.io` only if HTTPS is used, and
  all other resources must come from the same domain as this request.

## Reporting

To report attempts to violate the CSP but not prevent them,
use the `Content-Security-Policy-Report-Only` header.
This may be useful during development to determine the
CSP directives that are desired before going to production.

Violation attempts are reported by
sending a JSON object in an HTTP POST request.
To specify where reports will be sent,
add the `report-uri` directive with a value that
is the URL where POST requests will be sent.
This can be added in the `Content-Security-Policy` or
`Content-Security-Policy-Report` header.
There is no need to supply both headers.

The `report-uri` directive must be specified in an HTTP response header,
not in a `meta` tag.

A report JSON object contains many properties including the following.

| Property              | Description                                                                                                                        |
| --------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `blocked-uri`         | URI that violated a policy                                                                                                         |
| `disposition`         | "enforce" if triggered by a `Content-Security-Policy` header or "report" if triggered by a `Content-Security-Report-Policy` header |
| `document-uri`        | URI of the document that requested the resource                                                                                    |
| `effective-directive` | directive that was violated                                                                                                        |
| `script-sample`       | first 40 characters of the violating script or CSS                                                                                 |

The following is an example report that describes
an issue with getting an image from Unsplash.
Note the properties `effective-directive` and `blocked-uri`.

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

## Building a CSP

A great way to arrive at the desired CSP to start with only the following:

```ts
const policies = ['report-uri /csp-report', "default-src 'self'"];
const csp = policies.join('; ');
```

In the server code that configures serving static files
from a directory like "public", add the
"Content-Security-Policy" header with the value in the `csp` variable.

With the Hono TypeScript library this can be done as follows:

```typescript
app.use('/*', (c: Context, next: Next) => {
  c.header('Content-Security-Policy', csp);

  // Tell the browser that the site can only be accessed using HTTPS,
  // and that future attempts to access it using HTTP
  // should be automatically converted to HTTPS.
  const yearSeconds = 31536000;
  c.header(
    'Strict-Transport-Security',
    `max-age=${yearSeconds}; includeSubDomains`
  );

  const fn = serveStatic({root: './public'});
  return fn(c, next);
});
```

Now define an endpoint to receive violation reports.
With Hono this can be done as follows:

```typescript
app.post('/csp-report', async (c: Context) => {
  const json = await c.req.json();
  const report = json['csp-report'];
  let file = report['document-uri'];
  const origin = c.req.raw.headers.get('origin');
  if (file === origin + '/') file = 'index.html';
  console.error(
    `${file} attempted to access ${report['blocked-uri']} which ` +
      `violates the ${report['effective-directive']} CSP directive.`
  );
  c.status(403);
  return c.text('CSP violation');
});
```

Start the server, browse the app, and exercise all of its functionality.
Output in the terminal where the server is running will describe
all the CSP violations.
One-by-one add CSP directives in the `policies` array
until all the desired policies are in place.

Once the app is in production, logging attempted CSP violations will
keep you informed about whether and how the site is being attacked.

## Example Web App

The following code implements an HTTP server using
<a href="https://hono.dev" target="_blank">Hono</a>.
It also uses <a href="/blog/topics/#/blog/htmx/" target="_blank">htmx</a>.
Comments in the code explain everything related to
the CSP that it constructs and uses.

```typescript
import {type Context, Hono, type Next} from 'hono';
import {serveStatic} from 'hono/bun';

const policies = [
  // This specifies where POST requests for violation reports will be sent.
  'report-uri /csp-report',

  // Only resources from the current domain are allowed
  // unless overridden by a more specific directive.
  "default-src 'self'",

  // This allows sending HTTP requests to the JSONPlaceholder API.
  // It also allows client-side JavaScript code to create a WebSocket.
  "connect-src 'self' https://jsonplaceholder.typicode.com ws:",

  // This allows getting Google fonts.
  // "link" tags for Google fonts have an href
  // that begins with https://fonts.googleapis.com.
  // The linked font file contains @font-face CSS rules
  // with a src URL beginning with https://fonts.gstatic.com.
  'font-src https://fonts.googleapis.com https://fonts.gstatic.com',

  // This allows getting images from Unsplash.
  'img-src https://images.unsplash.com',

  // This allows getting videos from googleapis.
  'media-src http://commondatastorage.googleapis.com',

  // This allows downloading the htmx library from a CDN.
  "script-src-elem 'self' https://unpkg.com",

  // This allows the htmx library to insert style elements.
  "style-src-elem 'self' 'unsafe-inline' https://fonts.googleapis.com"
];

const csp = policies.join('; ');

const app = new Hono();

// Serve static files from the public directory.
app.use('/*', (c: Context, next: Next) => {
  // Add a header to enforce the CSP.
  c.header('Content-Security-Policy', csp);

  const yearSeconds = 31536000;
  c.header(
    'Strict-Transport-Security',
    `max-age=${yearSeconds}; includeSubDomains`
  );

  const fn = serveStatic({root: './public'});
  return fn(c, next);
});

// This can be used to test blocking a DOM XSS attack.
app.get('/dom-xss', (c: Context) => {
  return c.text("alert('A DOM XSS occurred!')");
});

// This can be used to test blocking a reflective XSS attack.
app.get('/reflective-xss', (c: Context) => {
  return c.html("<script>alert('A reflective XSS occurred!');</script>");
});

// This can be used to test blocking a stored XSS attack.
app.get('/version', (c: Context) => {
  // The html tagged template literal escapes
  // HTML elements in strings, but not in JSX!
  const storedContent = '<script>alert("XSS!");</script>';
  const escaped = html`v${Bun.version} ${storedContent}`;
  return c.html(escaped);
});

// This receives reports of CSP violations in a JSON object.
app.post('/csp-report', async (c: Context) => {
  const json = await c.req.json();
  const report = json['csp-report'];
  let file = report['document-uri'];
  const origin = c.req.raw.headers.get('origin');
  if (file === origin + '/') file = 'index.html';
  console.error(
    `${file} attempted to access ${report['blocked-uri']} which ` +
      `violates the ${report['effective-directive']} CSP directive.`
  );
  c.status(403);
  return c.text('CSP violation');
});

export default app;
```

The following HTML in `public/index.html` relies on the
CSP defined in the server above to access several resources.

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

    <!-- This is used to verify that DOM XSS attacks are blocked. -->
    <script>
      async function domXSS() {
        const res = await fetch('/dom-xss');
        const text = await res.text();
        eval(text);
      }
      window.onload = domXSS;
    </script>
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

    <!-- When this form is submitted,
         an HTTP POST request is sent to the specified URL
         and the HTML it returns becomes the innerHTML of
         the div element below with the id "todo". -->
    <form
      hx-post="https://jsonplaceholder.typicode.com/todos"
      hx-target="#todo"
    >
      <label>Title:<input type="text" name="title" value="" /></label>
      <label>Body:<input type="text" name="body" value="" /></label>
      <button>Submit</button>
    </form>
    <div id="todo"></div>

    <button hx-get="/reflective-xss" hx-target="#reflective-xss">
      Reflective XSS
    </button>
    <div id="reflective-xss"></div>
  </body>
</html>
```

## SubResource Integrity (SRI)

To prevent executing a script whose contents have been altered,
perhaps maliciously, include an `integrity` attribute in the `script` tag.
The value is a hash that is computed based on the contents of the script.
This is referred to as "SubResource Integrity".

Using SRI is especially important when scripts are obtained from a CDN.
SRI is not typically enforced for scripts loaded from the same origin.

The `integrity` value must begin with a string that identifies
the hash algorithm, followed by a dash and the hash.
For an example, see the `script` tag for htmx in the HTML shown
in the previous section.

One way to generate a hash for a trusted, online resource is to use the site
<a href="https://www.srihash.org" target="_blank">SRI Hash Generator</a>.

One way to generate a hash for a trusted, local file
is the use the `openssl` command. For example:

```bash
cat public/my-script.js | openssl dgst -sha384 -binary | openssl base64 -A
```

## Cross-Site Scripting Attacks (XSS)

A XSS attack can occur when JavaScript running in a browser obtains text
that may contain JavaScript code and uses it in one of the following ways
which result in executing the JavaScript.

- set the `innerHTML` property of a DOM element to text
  which includes `script` tags that contain JavaScript code
- pass text containing JavaScript code to the `eval` function
- pass text containing JavaScript code as the first argument
  to the `setTimeout` or `setInterval` function

This is particularly concerning when
the text includes calls to the `fetch` function.
With a strict default CSP in place,
calling the `fetch` function from an inline script is only allowed
if the `script-src` or `script-src-elem` directive
includes the `unsafe-inline` keyword.

Similarly, calling the `eval` function is only allowed if the
`script-src` or `script-src-elem` directive includes `'unsafe-eval'`.

A CSP can prevent scripts found in text from being executed.
The easiest way is to include the directive `default-src 'self'`.
To intentionally allow executing such scripts,
include the `'unsafe-inline'` keyword in the value of the
`script-src` or `script-src-elem` directive.

There are three types of XSS attacks.

### Reflected XSS

In this form of XSS, an HTTP endpoint returns
text containing one or more `script` tags.
Client-side JavaScript then uses it in one of the ways described above.

For an example of an endpoint that returns a `script` tag,
see the GET endpoint for `/reflective-xss` above.

### Stored XSS

In this form of XSS, user-supplied content is stored, perhaps in a database.
The content is later used in generated HTML
in one of the ways described above.

### DOM XSS

In this form of XSS, client-side JavaScript gets text
from a source such as the page URL or a fetch request,
and uses it in one of the ways described above.

For an example of an endpoint that returns a string of JavaScript code,
see the GET endpoint for `/dom-xss` above.
