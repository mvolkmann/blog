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

{% aTargetBlank "https://htmx.org", "HTMX" %} is
a client-side JavaScript library that adds support for
new HTML attributes that make HTML more expressive.

The new HTML attributes enable responding to
specific interactions (ex. click) with any HTML element
by sending HTTP request using any verb (GET, POST, PUT, PATCH, or DELETE).
The response from these HTTP must contain HTML.
Rather than performing a complete page refresh,
the returned HTML replaces an existing DOM element or
is inserted relative to an existing DOM element.

The fact that all HTML rendered by HTMX applications
is either static or server rendered makes it great for SEO.

The server can be implemented using
any programming language and server framework.
The server typically plays two roles.
First, it serves static files such as HTML, CSS, JavaScript, and images.
Second, it responds to HTTP requests by returning dynamically generated HTML.

HTMX was created by {% aTargetBlank "https://bigsky.software/cv/", "Carson Gross" %}.
He is a principal software engineer at
{% aTargetBlank "https://bigsky.software/", "Big Sky Software" %}
and a part-time CS instructor at Montana State University.

The HTMX approach is based on "Hypermedia As The Engine Of Application State"
({% aTargetBlank "https://htmx.org/essays/hateoas/", "HATEOAS" %})
which is a specific use of the REST architecture.
REST is described in chapter 5 of the famous PhD dissertation by Roy Fielding
"{% aTargetBlank "https://ics.uci.edu/~fielding/pubs/dissertation/fielding_dissertation.pdf",
"Architectural Styles and the Design of Network-based Software Architectures" %}".

HTMX simplifies state management because all the state is in one place,
on the server.

HTMX is not appropriate for all features of web apps.
Using HTMX to update the UI on every mouse move or drag
would be too slow since each movement would trigger a new HTTP request.
Examples of apps that require this kind of functionality include
Google Maps and many games.
However, HTMX can be used in conjunction with other approaches,
so it can be used for the parts of apps that
do not require constant reaction to mouse movements.

A good option to pair with HTMX is
{% aTargetBlank "https://alpinejs.dev", "Alpine.js" %}.
Like HTMLx, Alpine is a client-side JavaScript library
that adds support for new HTML attributes.
It differs from HTMX in that it focuses on client-side interactions.

## History and Future

Carson Gross been working on the approach of simplifying web development
by adding attributes to HTML for a long time.

The predecessor of HTMX, also created by Carson Gross,
is {% aTargetBlank "https://intercoolerjs.org", "intercooler.js" %}.
The first version of intercooler.js was released in April, 2014.
Intercooler had a dependency on jQuery, but HTMX does not.

The first version of HTMX was released in May, 2020.
The 1.0 version was released in November, 2020.
The latest version as of December 2023 is 1.9.9.
It is 15K gzipped and 47K unzipped.

HTMX 2.0 is expected in late 2023 or early 2024.
It will remove legacy support for things like IE,
old style websockets, and old style server sent events.
There will also be some changes to default behavior
such as not enabling CORS by default.
Other changes are not expected to be dramatic.

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

The `hx-target` attribute specifies a target DOM element.
Supported values include:

- `this` targets current element (default)
- `{css-selector}` targets first element that matches
- `closest {css-selector}` targets closest ancestor element or itself that matches
- `find {css-selector}` targets first descendant element that matches
- `next` targets next sibling element
- `next {css-selector}` targets next matching element
- `previous` targets previous sibling element
- `previous {css-selector}` targets previous matching sibling

The new content can replace existing content
or be inserted relative to existing content.
This is specified with the `hx-swap` attribute.
Supported values include:

- `outerHTML`: replace target element
- `innerHTML`: replace content of target element (default)
- `beforebegin`: insert before target element
- `afterbegin`: insert before first child of target element
- `beforeend`: insert after last child of target element
- `afterend`: insert after target element
- `delete`: delete target element; new content is not used
- `none`: do not change the DOM; new content is not used

The `hx-swap` attribute also supports the following space-separated modifiers:

- `focus-scroll`: enables scrolling to a focused input
- `ignoreTitle`: disables default behavior of updating the page title when the content contains a `title` element
- `scroll`: smoothly scrolls the page to the `top` or `bottom` of the target element or a specified element
- `settle`: specifies the time between the swap and settle (?) logic
- `show`: abruptly scrolls the page to the `top` or `bottom` of the target element or a specified element
- `swap`: specifies time to wait after receiving new content before swapping/inserting it
- `transition`: uses the View Transitions API

hx-select
hx-preserve

## Indicators

TODO: Try this.
hx-indicator
hx-disabled-elt

## Out-of-band Updates

TODO: Try this.
hx-swap-oob
hx-select-oob

## Events

TODO: Try this.
hx-on

## Validation

TODO: Try this.
hx-validate

## Other

hx-boost
hx-disinherit
hx-encoding
hx-vals
hx-vars

## Debugging

TODO: Try this.

## Animation

TODO: Try this.

## Boosting

TODO: Try this.

## WebSockets

TODO: Try this.
hx-ws

## Server Sent Events

TODO: Try this.
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
This is referred to as HowEver you Would Like (HOWL).
A downside is that there are no canonical examples of
implementing the backend services for an HTMX application.

Desirable characteristics include:

- good performance
- ability to construct HTML responses using some kind of templating
  rather than string concatenation
- editor tooling that can validate proper HTML elements, attributes, and nesting

One option is to use
{% aTargetBlank "https://bun.sh", "Bun" %} (Node.js alternative) and
{% aTargetBlank "https://elysiajs.com", "ElysiaJS" %} (Express alternative).
ElysiaJS supports constructing HTML using JSX syntax.
JSX is only used to create strings of HTML.
A virtual DOM is not used.

When combined with {% aTargetBlank "https://turso.tech", "Turso" %}
(SQLite deployed at the edge), this is referred to as the BETH stack
(Bun, Elysia, Turso, and HTMX).

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

## Unorganized Content

- Learn how to automatically refresh browser on code save.
- Should you use auto focus for the new to do input?
- Can you use JavaScript code in place of hyper script code?
- Study how HTMX supports dialogue boxes.
- Study Alpine JS and consider using it with HTMX

try the hx-confirm attribute on delete buttons.
try the hx-indicator attribute to show a loading spinner. it changes the opacity from zero to one and then back to zero.

Big emphasis of HTMX is to enable sending HTTPrequest triggered by an interaction on any HTML element, return HTML from HTTP requests, and to be able to update only parts of a page.

HTMX is implemented in JavaScript, not TypeScript, in a single source file. he plans to add JSDoc to describe TypeScript types in the future.
HATEOS was described in Roy Fielding’s PhD dissertation.
read essays by Carson Gross on the HTMX website. there is one essay where he discusses how HTMX is not a solution for every web app. He also said HTMX is not a replacement for every use of React.
options for Client side scripting include vanilla, JS, Alpine, JS, and Hyperscript, which Carson worked on.
HTMX 2.0 will drop support for IE.
HTMX has an extensive set of integration tests implemented in mocha.
interest in HTMX exploded in 2023 due to videos by the Primeagen and Fireship.
you can still use REST APIs that return JSON. You just need to add another layer of HTTP services that call those and translate JSON into HTML. This could be useful for also implementing mobile apps that use the JSON-based API’s.
hx-push-url=“true” gives history support, so the back button can go to a previous state of the UI. Try this! it does this by snapshoting the entire DOM and saving it in localstorage. this uses the standard history API. writing to localstorage can be disabled if desired.
use the hx-request HTTP header to determine how to render a page when the browser is refreshed. this enables support for deep links. this is useful when HTMX has replaced portions of a page.

see the htmxLogAll method to enable debug logging. Is this on the server side?
One way of debugging is to use an unminified version of the HMTX library, and set breakpoints in its functions.
HTMX can work with JavaScript disabled … progressive enhancement and hx-boost=“true”. How is that possible?

Carson Gross wishes that the functionality of HTMX would just be folded into HTML.
swapping a chunk of HTML with a new chunk is called transclusion.

see unpoly.com which is a competitor to HTMX. supposedly it is better at progressive enhancement.
Carson Gross picked Alpine JS to call out in the podcast. It works well in combination with HTMX. it is much lighter weight than react and is great for implementing client side interactivity.
