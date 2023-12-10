---
eleventyNavigation:
  key: HTMX
layout: topic-layout.njk
---

<figure style="width: 60%">
  <img alt="HTMX logo" style="border: 0"
    src="/blog/assets/htmx-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://htmx.org", "HTMX" %} is a client-side JavaScript library
that adds support for new HTML attributes.
These attributes enable many things including:

- Specific interactions (ex. click) with any HTML element can trigger
  an HTTP request using any verb (GET, POST, PUT, PATCH, or DELETE).
- The response from an HTTP request should contain HTML.
- The returned HTML can replace an existing DOM element
  or be inserted relative to an existing DOM element.

The HTMX approach is based on "Hypermedia As The Engine Of Application State"
({% aTargetBlank "https://htmx.org/essays/hateoas/", "HATEOAS" %})
which is a specific use of the REST architecture.
REST is described in chapter 5 of the famous PhD dissertation by Roy Fielding
"{% aTargetBlank "https://ics.uci.edu/~fielding/pubs/dissertation/fielding_dissertation.pdf",
"Architectural Styles and the Design of Network-based Software Architectures" %}".

HTMX is not appropriate for all features of web apps.
Using HTMX to update the UI on every mouse move or drag
would be too slow since each movement would trigger a new HTTP request.
Examples of apps that require this kind of functionality include
Google Maps and many games.
However, HTMX can be used in conjunction with other approaches,
so it can be used for the parts of apps that do not require constant reaction to mouse movements.

A good option to pair with HTMX is
{% aTargetBlank "https://alpinejs.dev", "Alpine.js" %}.
Like HTMLx, Alpine is a client-side JavaScript library
that adds support for new HTML attributes.
It differs from HTMX in that it focuses on client-side interactions.

## Installing

## Requests

Interacting with any HTML element can trigger an HTTP request.

hx-get
hx-post
hx-put
hx-patch
hx-delete

hx-sync
hx-params
hx-confirm

## Triggers

hx-trigger

## Targets

Content returned from HTTP endpoints can be placed at
a specific target location in the DOM.
The new content can replace existing content
or be inserted relative to existing content.
This is specified with the `hx-swap` attribute.
Supported values include:

- `outerHTML`: replace target element
- `innerHTML`: replace content of target element
- `beforebegin`: insert before target element
- `afterbegin`: insert before first child of target element
- `beforeend`: insert after last child of target element
- `afterend`: insert after target element
- `delete`: delete target element; new content is not used
- `none`: do not change the DOM; new content is not used

hx-swap
hx-select
hx-preserve

## Indicators

hx-indicator
hx-disabled-elt

## Out-of-band Updates

hx-swap-oob
hx-select-oob

## Events

hx-on

## Validation

hx-validate

## Other

hx-boost
hx-disinherit
hx-encoding
hx-vals
hx-vars

## Debugging

## Animation

## Boosting

## WebSockets

hx-ws

## Server Sent Events

hx-sse

## History

hx-history-elt
hx-push-url

## Resources

- {% aTargetBlank "https://htmx.org", "HTMX Home Page" %}
- {% aTargetBlank "https://htmx.org/docs/", "HTMX Documentation" %}
- {% aTargetBlank "https://htmx.org/reference/", "HTMX Reference" %}
- {% aTargetBlank "https://htmx.org/examples/", "HTMX Examples" %}
- {% aTargetBlank "https://htmx.org/essays/", "HTMX-related Essays" %}

- {% aTargetBlank "https://htmx.org/discord", "HTMX Discord Channel" %}
- {% aTargetBlank "https://hypermedia.systems", "Hypermedia Systems" %} book
- {% aTargetBlank "https://bigsky.software", "Big Sky Software" %}
- {% aTargetBlank "https://twitter.com/htmx_org", "Carson Gross on Twitter" %}
