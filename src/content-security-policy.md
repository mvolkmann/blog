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
- `report-to`: specifies the URL where violation reports are sent (using a POST request?)
- `script-src-attr`: restricts sources for JavaScript inline event handlers like `onclick`
- `script-src-elem`: restricts `<script>` elements
- `script-src`: compiles the previous two in one directive
- `worker-src`: restricts Worker, SharedWorker, and ServiceWorker scripts

It is recommended to make `default-src` very restrictive (maybe just "self")
and used more specific directives to open access for specific kinds of resources.

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

Include the `report-to: {url}` directive to specify
where violation reports should be sent.
This must be specified in an HTTP response header, not in a `meta` tag.

The report is a JSON object with many properties including:

- `blocked-uri` gives the URI that violated a policy.
- `disposition` will be "enforce" or "report".
- `document-uri` gives the URI of the document that requested the resource.
- `effective-directive` gives the directive that was violated.
- `script-sample` gives the first 40 characters of violating script or CSS.

Here is an example report:

```json
{
  "csp-report": {
    "blocked-uri": "http://example.com/css/style.css",
    "disposition": "report",
    "document-uri": "http://example.com/signup.html",
    "effective-directive": "style-src-elem",
    "original-policy": "default-src 'none'; style-src cdn.example.com; report-to /_/csp-reports",
    "referrer": "",
    "status-code": 200,
    "violated-directive": "style-src-elem"
  }
}
```

Try creating an endpoint that receives violation reports!

Specify both the `Content-Security-Policy` and `Content-Security-Policy-Report`
headers to both prevent some violations and report some attempted violations.
The policies specified in each can be the same or they can differ.

See <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP"
target="_blank">Content Security Policy</a>
for a table of CSP directives that are supported by each browser.

Does using a CSP remove the need to sanitize HTML?

In "stored cross-site scripting", script tags entered in text input
get stored in databases and later displayed on pages.
They are not executed if they are used as `textContent`,
but are if they are used as `innerHTML`.
There are other types of cross-site scripting attacks. What are they?
