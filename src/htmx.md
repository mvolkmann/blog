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
- The server that responds to the HTTP requests can be implemented using
  any programming language and any server framework.

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

## URLs

HTMX endpoints return HTML, not JSON.
Their URLs are based on user interface functionality, not resource paths.
For example, `/todos/:id/toggle` is a reasonable URL path
for an endpoint that toggles the completed status of todo
and returns HTML for the updated todo item.

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

## Hyperscript

{% aTargetBlank "https://hyperscript.org", "Hyperscript" %} is
"an easy & approachable language for modern web front-ends".
It can be used in conjunction with HTMX.

To enable use of hyperscript, add a script tag like the following:

```html
<script src="https://unpkg.com/hyperscript.org@0.9.12"></script>
```

One use of hyperscript is to clear form inputs after a form is submitted.
Specify the code to run as the value of the underscore attribute
on the `form` element.
For example:

```html
_="on submit target.reset()"
```

## Server-Side Options

The server that responds to the HTTP requests triggered by
interactions with HTML elements can be implemented using
any programming language and any server framework.

Desirable characteristics include:

- good performance
- ability to construct HTML responses using some kind of templating
  rather than string concatenation
- editor tooling that can validate proper HTML elements, attributes, and nesting

One option is to use
{% aTargetBlank "https://bun.sh", "Ban" %} (Node.js alternative) and
{% aTargetBlank "https://elysiajs.com", "ElysiaJS" %} (Express alternative).
ElysiaJS supports constructing HTML using JSX syntax.
JSX is only used to create strings of HTML.
A virtual DOM is not used.

Another popular option is to use Python with either
{% aTargetBlank "https://www.djangoproject.com", "Django" %} or
{% aTargetBlank "https://flask.palletsprojects.com/", "Flask" %}.

## Mobile Apps

The hypermedia approach used by HTMX to build web applications
can also be used to build mobile apps.
See {% aTargetBlank "https://hyperview.org", "Hyperview" %}
which builds on React Native.

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
