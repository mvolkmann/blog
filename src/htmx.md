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
(Replacing an HTML element with a new element is called transclusion.)

Interest in HTMX exploded in 2023 after YouTube videos
from ThePrimeagen and Fireship were released.
See {% aTargetBlank "https://www.youtube.com/watch?v=zjHHIqI9lUY", "HTMX" %} and
{% aTargetBlank "https://www.youtube.com/watch?v=r-GSGH2RxJs", "HTMX in 100 seconds" %}.

The fact that all HTML rendered by HTMX applications
is either static or server rendered makes it great for SEO.

The HTMX library is implemented in JavaScript,
not TypeScript, in a single source file.
There are plans to add JSDoc TypeScript type definitions
for better code editor support.

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
({% aTargetBlank "https://htmx.org/essays/hateoas/", "HATEOAS" %}) which is
a specific use of the REST architecture where services return hypermedia.
REST is described in chapter 5 of the famous PhD dissertation by Roy Fielding
"{% aTargetBlank "https://ics.uci.edu/~fielding/pubs/dissertation/fielding_dissertation.pdf",
"Architectural Styles and the Design of Network-based Software Architectures" %}".

HTMX simplifies state management because all the state is in one place,
on the server.

## Client-side Processing

HTMX is not appropriate for all features of web apps.
Using HTMX to update the UI on every mouse move or drag
would be too slow since each movement would trigger a new HTTP request.
Examples of apps that require this kind of functionality include
Google Maps and many games.

HTMX can be used in conjunction with other approaches,
so it can be used for the parts of apps that
do not require constant reaction to mouse movements.

Options for implementing client-side processing include vanilla JavaScript,
{% aTargetBlank "https://alpinejs.dev", "Alpine" %}, and
{% aTargetBlank "https://hyperscript.org", "_hyperscript" %}.

Like HTMLx, Alpine and \_hyperscript are client-side JavaScript libraries.
These are much lighter that libraries and frameworks like React.

Alpine adds support for many new HTML attributes.
\_hyperscript adds support for one new HTML attribute
whose name is a single underscore (`_`) and whose value is \_hyperscript code.
Both differ from HTMX in that they focus on client-side interactions
rather that processing HTTP requests.

## JSON

While HTMX applications do not require implementing and using
REST APIs that returns JSON data, it may still be desirable to implement them.
For example, Android and iOS mobile apps may wish to use such APIs.
To use these in HTMX apps, call them from HTTP endpoints
that use the data to generate HTML responses.

Alternatively, write functions that return the desired data
and call them from both the endpoints that return JSON
and the endpoints that return HTML.

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
old style WebSockets, and old style server sent events.
There will also be some changes to default behavior
such as not enabling CORS by default.
Other changes are not expected to be dramatic.

HTMX has an extensive set of integration tests implemented in Mocha.

Carson Gross wishes that the functionality of HTMX would be folded into HTML,
making HTMX unnecessary.

## Installing

There is no need to install HTMX using a package manager like npm or bun.
It can be used from a CDN by including the following `script` tag:

```html
<script src="https://unpkg.com/htmx.org@1.9.9"></script>
```

Alternatively, it can be downloaded as a single, minified JavaScript file
by clicking the "~14k min.gz’d" link near the top of the
{% aTargetBlank "https://htmx.org", "HTMX home page" %}.
Place the downloaded file in a directory whose files are served by
your application server and include a `script` tag like the following:

```html
<script src="htmx.min.js"></script>
```

## Requests

Interacting with any HTML element can trigger an HTTP request.

hx-get
hx-post
hx-put
hx-patch
hx-delete

hx-sync
hx-params

The `hx-confirm` attribute specifies a message
to display in a confirmation dialog.
An HTTP request is only sent and processed if the user clicks the OK button.

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

The `hx-indicator` attribute specifies a CSS selector
that matches one or more elements.
Those elements will have their opacity changed from 0 to 1
while an associated HTTP request is being processed.

The `hx-disabled-elt` attribute specifies a CSS selector
that matches one or more elements.
Those elements will have the `:disabled` CSS pseudo-class applied
while an associated HTTP request is being processed.

The following HTML demonstrates
showing an indicator and disabling the "Add" button
while waiting for a new todo to be added.
Note how the spinner image has the CSS class `htmx-indicator`.

```html
<form
  hx-post="/todos"
  hx-swap="afterend"
  hx-disabled-elt="#add-btn"
  hx-indicator="#spinner"
  _="on submit target.reset()" // resets form using _hyperscript
>
  <input name="description" />
  <button id="add-btn" type="submit">Add</button>
  <img
    alt="loading..."
    class="htmx-indicator h-6 w-6"
    id="spinner"
    src="/public/spinner.gif"
  />
</form>
```

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

## \_hyperscript

{% aTargetBlank "https://hyperscript.org", "_hyperscript" %} is
"an easy & approachable language for modern web front-ends".
It can be used in conjunction with HTMX.

To enable use of \_hyperscript, add a script tag like the following:

```html
<script src="https://unpkg.com/hyperscript.org@0.9.12"></script>
```

One use of \_hyperscript is to clear form inputs after a form is submitted.
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
- Read essays by Carson Gross on the HTMX website.

- `hx-push-url="true"` provides history support so the back button
  can go to a previous state of the UI. Try this!
  It does this by creating a snapshot of the entire DOM
  and saving it in `localStorage`.
  This uses the standard Web {% aTargetBlank
  "https://developer.mozilla.org/en-US/docs/Web/API/History_API", "History API" %}.
  Writing to `localstorage` can be disabled if desired.

- The `hx-request` HTTP header can be used to determine
  how to render a page when the browser is refreshed.
  This enables support for deep links.
  This is useful when HTMX has replaced portions of a page.

- The `htmxLogAll` method enables debug logging. Is this on the server side?

- One debugging approach is to use an unminified version of the HTMX library
  and set breakpoints in its functions.

- HTMX can work with JavaScript disabled
  by setting the `hx-boost="true"` attribute.
  Does this need to be on the `body` element? How does this work?

- See unpoly.com which is a competitor to HTMX.
  Supposedly it is better at progressive enhancement.
