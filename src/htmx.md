---
eleventyNavigation:
  key: htmx
layout: topic-layout.njk
---

<figure style="width: 60%">
  <img alt="htmx logo" style="border: 0"
    src="/blog/assets/htmx-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://htmx.org", "Htmx" %}
is a hypermedia-oriented, client-side JavaScript library.
Hypermedia is any data format that can describe
branching from one "media" (ex. a document) to another.
A hypermedia control is an element that describes a server interaction
such as anchor (`a`) and `form` elements in HTML.

Htmx adds support for new HTML attributes that make HTML more expressive.
These attributes enable implementing "Hypermedia-Driven Applications" (HDAs).

The new HTML attributes enable responding to
any kind of interaction (ex. click) with any HTML element
by sending an HTTP request using any method (GET, POST, PUT, PATCH, or DELETE).
The response must contain HTML.
Rather than performing a complete page refresh, the returned HTML
replaces an existing DOM element (transclusion) or is inserted relative to one.

Htmx removes the need to serialize data to JSON on the server,
parse the JSON on the client, and build an HTML representation from the data.

The server can be implemented using
any programming language and server framework.
The server typically plays two roles.
First, it serves static files such as HTML, CSS, JavaScript, and images.
Second, it responds to HTTP requests by returning HTML.

Htmx simplifies state management because
all the state is in one place, on the server.

Many web app features typically thought to require custom JavaScript code
can instead be implemented with only htmx.
Examples include lazy loading of data, infinite scrolling,
and searching while the user types in an `input`.

The fact that all HTML rendered by htmx applications
is either static or server-rendered makes it great for SEO.

Users perceive apps built with htmx to be fast because
the initial page load only requires
the htmx library (< 17K) and the initial HTML to render.
Subsequent interactions only require fetching snippets of HTML.
No client-side hydration of JavaScript code is needed.
Browsers are very efficient at updating the DOM from strings of HTML.

The htmx library is implemented in JavaScript,
not TypeScript, in a single source file.
There are plans to add JSDoc TypeScript type definitions
for better code editor support.

## Simplified Client-side

Using endpoints that return HTML instead of JSON
removes the need for many things including:

- versioning

  This is unnecessary because the browser does not need to
  interpret the meaning of the HTML being returned.
  It just needs to render it.

- client-side routing

  This is unnecessary because page updates and transitions
  are handled by HTTP requests triggered on elements
  described in the HTML returned by the server.

- managing client-side data models

  This is unnecessary because all the data remains only on the server.

- nearly all client-side logic

  This is mostly unnecessary because the logic is embedded in
  the HTML elements returned by the server.

## History and Future

Htmx was created by {% aTargetBlank "https://bigsky.software/cv/",
"Carson Gross" %}. He is a principal software engineer at
{% aTargetBlank "https://bigsky.software/", "Big Sky Software" %}
and a part-time Computer Science instructor at Montana State University.

Carson been working on the approach of simplifying web development
by adding attributes to HTML for a long time.

The predecessor of htmx, also created by Carson Gross,
is {% aTargetBlank "https://intercoolerjs.org", "intercooler.js" %}.
Work on intercooler.js began in 2013
and the first version was released in April 2014.
Intercooler had a dependency on jQuery, but htmx does not.

The first version of htmx was released in May 2020.
The 1.0 version was released in November, 2020.
The latest version as of December 2023 is 1.9.10 which is less than 17K.

Htmx has an extensive set of integration tests implemented in Mocha.

Interest in htmx exploded in 2023 after YouTube videos
from ThePrimeagen and Fireship were released.
See {% aTargetBlank "https://www.youtube.com/watch?v=zjHHIqI9lUY", "htmx" %} and
{% aTargetBlank "https://www.youtube.com/watch?v=r-GSGH2RxJs", "htmx in 100 seconds" %}.

Htmx had a strong showing in the 2023 JavaScript Rising Stars results.
See {% aTargetBlank "https://risingstars.js.org/2023/en#section-framework",
"Front-end Frameworks" %}.

Htmx 2.0 is expected in early 2024.
It will remove legacy support for things like IE,
old style WebSockets, and old style server sent events.
There will also be some changes to default behavior
such as not enabling CORS by default.
Other changes are not expected to be dramatic.

Carson Gross wishes that the functionality of htmx would be folded into HTML,
making htmx unnecessary.

Companies that sponsor the development of htmx are listed on the
{% aTargetBlank "https://htmx.org", "htmx home page" %}.
They include GitHub and JetBrains.

## REST

Web app frameworks such as React, Svelte, Angular, and Vue
have popularized the creation of single-page applications (SPAs).
SPA web apps typically use client-side JavaScript code
to send HTTP requests to server-side endpoints that
query/update databases, serialize the data into JSON, and return it.
The browser parses the JSON into a JavaScript object
and client code updates the page with HTML that is generated from that object.
Client-side code is responsible for understanding the meaning of JSON data
and breaks if the endpoints change to return different data.
Many developers refer to this architecture as "REST".

This is not what Roy Fielding had in mind when he wrote his famous dissertation
"{% aTargetBlank
"https://ics.uci.edu/~fielding/pubs/dissertation/fielding_dissertation.pdf",
"Architectural Styles and the Design of Network-based Software Architectures" %}"
that gave birth to REST.
Roy has been {% aTargetBlank
"https://roy.gbiv.com/untangled/2008/rest-apis-must-be-hypertext-driven",
"quoted" %} saying "I am getting frustrated by
the number of people calling any HTTP-based interface a REST API.
... That is RPC."

This does not mean that architectures commonly referred to as REST are bad.
They just do not fit the Fielding definition of REST.

A software architecture is RESTful if it:

- uses a client/server model
- is stateless
- caches responses
- supports a uniform interface

A uniform interface is one where:

- requests identify a resource
- resources are manipulated through representations
- messages are self-descriptive
- HATEOAS is used (described next)

## HATEOAS

"Hypermedia As The Engine Of Application State"
({% aTargetBlank "https://htmx.org/essays/hateoas/", "HATEOAS" %}) is
a specific use of the REST architecture where services return hypermedia.
The acronym HATEOAS does not appear in the dissertation,
but its concepts are discussed.

Carson Gross describes HATEOAS systems as follows:
"Given an entry point into the system, the rest of the system
can be accessed simply by inspecting the hypermedia."

HTML is a kind of hypermedia and web browsers are hypermedia clients.
Responses to requests can contain HTML.
Browsers render the HTML which enables users to view and operate on resources.

Such responses are similar to object-oriented programming
where objects contain data and methods for operating on their data.
Browsers simply render the HTML returned.
They do not require any resource-specific knowledge
about how to render or interact with resources.

JSON is not hypermedia.
JSON responses do not support HATEOAS because they typically
only contain raw data with no information on how to interact with it.
In addition, browsers do not know how to render JSON data.
Client code is required to understand the meaning of the data
in order to render it in a human-readable way.

## HTML Issues

Htmx addresses four important shortcomings of HTML.

1. Which elements can trigger sending an HTTP request?

   The only HTML elements that send an HTTP request are `a` (anchor) and `form`
   When an anchor is clicked, it sends a GET requests to a specified URL.
   When a `form` is submitted, it sends a GET or POST request to a specified URL.
   For GET requests, form data is attached to the URL in query parameters.
   For POST requests, form data is included in the request body.

   Htmx enables any element to send an HTTP request.

1. What events trigger sending an HTTP request?

   In HTML, clicking an anchor or submitting a `form`
   are the only events that trigger sending an HTTP request.

   Htmx enables any DOM event, and custom events,
   to trigger sending an HTTP request.

1. What kinds of HTTP requests can be sent?

   HTML only sends GET and POST requests.

   Htmx enables sending any kind of HTTP request
   which includes PUT, PATCH, and DELETE.

1. How is the response rendered?

   When HTML sends an HTTP request, the entire page is replaced by the response.

   Htmx enables replacing specific elements with the response (transclusion)
   or inserting the response relative to an existing element,
   while leaving the rest of the page intact.

## Questions to Consider

What is the benefit of serializing data to JSON on the server,
returning JSON to the browser, parsing JSON in the browser,
and using client-side code to convert the data into HTML?
With htmx the steps of serializing data to JSON on the server
and parsing JSON in the browser are elimated.

What are the advantages of generating HTML in the browser
instead of doing the same work on the server?

Why should we restrict ourselves to only using JavaScript
to implement web applications?
With htmx the server code can be implemented in any programming language
that supports implementing HTTP servers.

## Use Cases

Htmx is great for CRUD-based applications and dashboards.
But htmx is not appropriate for all web app features.

Using htmx to update the UI on every mouse move or drag
would be too slow since each movement would trigger a new HTTP request.
Examples of apps that require this kind of functionality include
Google Maps and many games.

Htmx is not appropriate for spreadsheet-like UIs
where a change in one part of the UI triggers
changes in many other parts that need to be reflected quickly.

Htmx cannot be used in apps that require offline support
because they rely on sending HTTP requests to
get new HTML that updates the UI.

Htmx can be used in conjunction with other approaches.
Consider using it for all the parts of apps
that do not require high frequency updates.

## Client-side Processing

Htmx applications to not require sending an HTTP request for every user interaction.
They can use HTML elements such as `details` and `dialog`
and client-side scripting.
Scripting options include vanilla JavaScript,
{% aTargetBlank "https://alpinejs.dev", "Alpine" %}, and
{% aTargetBlank "https://hyperscript.org", "_hyperscript" %}.

Alpine adds support for many new HTML attributes.
\_hyperscript adds support for one new HTML attribute
whose name is a single underscore (`_`) and whose value is \_hyperscript code.
Both differ from htmx in that they focus on client-side interactions
rather that processing HTTP requests.

Like HTMLx, Alpine and \_hyperscript are client-side JavaScript libraries.
These are much lighter than libraries and frameworks like React.

## JSON

While htmx applications do not require implementing and using
REST APIs that returns JSON data, it may still be desirable to implement them.
For example, Android and iOS mobile apps may wish to use such APIs.
To use these in htmx apps, call them from HTTP endpoints
that use the data to generate HTML responses.

Alternatively, write functions that return the desired data
and call them from both the endpoints that return JSON
and the endpoints that return HTML.

## Installing

There is no need to install htmx using a package manager like npm or bun.
It can be used from a CDN by including the following `script` tag:

```html
<script src="https://unpkg.com/htmx.org@1.9.9"></script>
```

Alternatively, it can be downloaded as a single, minified JavaScript file
by clicking the "~14k min.gz’d" link near the top of the
{% aTargetBlank "https://htmx.org", "htmx home page" %}.
Place the downloaded file in a directory whose files are served by
your application server and include a `script` tag like the following:

```html
<script src="htmx.min.js"></script>
```

## Using TypeScript

The steps below provide one way to use TypeScript
for developing the front end of an htmx project.
They create a new project that uses htmx, TypeScript, and Vite.
Vite provides a local HTTP server with hot reload.

A back end can be created using any programming language and framework
that support implementing an HTTP server.

- Enter `npm init vite@latest`

  - Enter a project name.
  - For the framework, select "Vanilla".
  - For the variant, select "TypeScript".

- cd to the newly created project directory

- Enter `npm install`

- Enter `npm install alpinejs`

- Enter `npm install -D @types/alpinejs`

- Create the file `src/global.d.ts` containing the following:

  ```ts
  import {Alpine as AlpineType} from 'alpinejs';

  declare global {
    var Alpine: AlpineType;
  }
  ```

- Replace the contents of `src/main.ts` with the following:

  ```ts
  import Alpine from 'alpinejs';
  globalThis.Alpine = Alpine;
  Alpine.start();
  ```

- Edit `index.html` which already contains a `script` tag for `/src/main.ts`.
  Add HTML that uses Alpine directives here.
  For example, change the `body` content to the following:

  ```html
  <div id="app" x-data>
    <button @click="alert('got click')">Press Me</button>
  </div>
  <script type="module" src="/src/main.ts"></script>
  ```

- Enter `npm run dev`

- Browse localhost:5173.

## Using Tailwind

There are two approaches for using Tailwind for CSS styling in an htmx app.

The easiest approach is to include it from a CDN with this `link` tag:

```html
<script src="https://cdn.tailwindcss.com"></script>
```

This has the downside that it includes every Tailwind CSS class,
not just the ones actually used in the app.

A more involved approach is to generate a CSS file that
only contains the Tailwind CSS classes that are actually used.
The steps to do this are as follows:

1. Install Tailwind by entering `bun install -d tailwindcss`
1. Enter `bunx tailwindcss init` to create the file `tailwind.config.js`.
1. Edit the value of `content` in `tailwind.config.js`
   to be `content: ['**/*.tsx'],`
1. Create the file `global.css` containing the following:

   ```css
   @tailwind base;
   @tailwind components;
   @tailwind utilities;
   ```

   This file can also define custom CSS classes.

1. Generate a CSS file containing only the Tailwind classes used in your app.

   Enter `bunx tailwindcss -i ./global.css -o public/tailwind.css --watch`
   to generate `public/tailwind.css` now and
   again every time any of thew "content" files are modified.

   Consider adding a `package.json` script for this such as:

   ```json
   "tw": "bunx tailwindcss -i ./global.css -o public/tailwind.css --watch"
   ```

   To run this, enter `bun run tw`.

1. Include the following `link` element in the base HTML of the app.

   ```html
   <link href="/public/tailwind.css" rel="stylesheet" />
   ```

   The documentation for the `@elysiajs/static` plugin says
   it defaults to looking in the `public` directory,
   but it actual defaults to looking in the root directory
   and I haven't found a way to change that.
   See this {% aTargetBlank "https://github.com/elysiajs/elysia/issues/352",
   "issue" %}.

This also requires enabling serving static files with the following steps:

1. Install a plugin by entering `bun add @elysiajs/static`
1. In the server code, add a call to `app.use(staticPlugin());`

## Triggers

With htmx, any element can trigger an HTTP request.
The first thing to consider is what will trigger a request to be sent.

HTML elements such as `input`, `textarea`, and `select`
automatically trigger when they emit a "change" event.
HTML `form` elements automatically trigger when they emit a `submit` event.
The default trigger for all other elements is "click".

To trigger on different events, add the `hx-trigger` attribute.
The value of this attribute can be
a single event, a comma-separated list of events,
or `every {timing}` to trigger repeatedly at a given time interval.

DOM event names include `blur`, `change`, `click`, `contextmenu`,
`dblclick`, `focus`, `hashchange`, `input`, `keydown`, `keypress`,
`keyup`, `load`, `mousedown`, `mouseenter`, `mouseleave`, `mousemove`,
`mouseout`, `mouseover`, `mouseup`, `resize`, `scroll`, `submit`,
`touchcancel`, `touchend`, `touchmove`, `touchstart`, `unload`, and more.
Custom event names can also be used.

A space-separated list of event modifiers can follow an event name.
Supported event modifiers include:

- `changed` - only send request if the element value has changed
- `delay:{time}` - wait at least this long before each request is sent
- `from:{css-selector}` - listen for the event on a different element
- `throttle:{time}` - only send the last event received in the specified time
  and then wait again

For example, the following trigger is useful for implementing active search:

```html
hx-trigger="keyup changed delay:500ms"
```

Events can be filtered so they only trigger in specific circumstances.
This is specified in square brackets after an event name.
For example, `click[shiftKey]` only triggers when
an element is clicked while the shift key is held down.
Multiple filters can be specify by and'ing them.
For example, `click[shiftKey&&ctrlKey]`.
The square brackets can also contain
a call to a JavaScript function that returns a Boolean value
where triggering doesn't occur if the function returns `false`.

## Requests

Interacting with any HTML element can trigger an HTTP request.

The following attributes all specify the URL
to which a specific kind of HTTP request should be sent
when the user triggers the element:
{% aTargetBlank "https://htmx.org/attributes/hx-get/", "hx-get" %},
{% aTargetBlank "https://htmx.org/attributes/hx-post/", "hx-post" %},
{% aTargetBlank "https://htmx.org/attributes/hx-put/", "hx-put" %},
{% aTargetBlank "https://htmx.org/attributes/hx-patch/", "hx-patch" %}, and
{% aTargetBlank "https://htmx.org/attributes/hx-delete/", "hx-delete" %}.

The {% aTargetBlank "https://htmx.org/attributes/hx-sync/", "hx-sync" %}
attribute coordinates concurrent requests.
For example, entering data in an `input` that is inside a `form`
could trigger two requests, one to validate the `input` value
and one to submit the `form`.
By default, these requests will run in parallel.
If `hx-swap="closest form:abort` is applied to the `input`,
the validation request will be sent first.
If the validation succeeds then the submit request will be sent.
Otherwise it will be aborted.

When a `form` is submitted,
the request includes the values of all its `form` elements by default.

To include the values of additional form elements
that are outside the form, add the {% aTargetBlank
"https://htmx.org/attributes/hx-include/", "hx-include" %}.

To omit the values of some of the form elements
that are inside the form, add the `hx-params` attribute.
For more detail, see {% aTargetBlank
"https://htmx.org/attributes/hx-params/", "hx-params" %}.

The `hx-confirm` attribute specifies a message
to display in a confirmation dialog using the `Window` method `confirm`.
The dialog is very plain and cannot be styled.
It may be preferable to use a dialog that can be styled.
An HTTP request is only sent and processed if the user clicks the OK button.

## URLs

Htmx endpoints return HTML, not JSON.
Their URLs are based on user interface functionality, not resource paths.
For example, `/todos/:id/toggle` is a reasonable URL path
for an endpoint that toggles the completed status of todo
and returns HTML for the updated todo item.

## Cross-Site Scripting Attacks

Cross-Site Scripting (XSS) attacks can occur if
user-entered content is included in HTML responses.
To prevent this, sanitize the HTML before returning it.
A good library for doing this is {% aTargetBlank
"https://github.com/apostrophecms/sanitize-html", "sanitize-html" %}.

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

The `hx-select` attribute specifies
a CSS selector or a comma-separated list of them
that indicates which elements from the returned HTML to include.
When this is not specified, all the returned HTML is used.
When a response contains a full HTML document,
Htmx automatically only includes the content of the `body` element.

The `hx-preserve` attribute can be applied to
elements that should be preserved through a swap.
For example, if there is an `audio` or `video` element that is playing
and a request for new content returns the same element,
it can continue playing without restarting at the beginning.

## Active Search

Htmx can be used to implement an active search where a list of matching data
is retrieved as the user enters search text.
The following code demonstrates this.
The full project can be found in {% aTargetBlank
"https://github.com/mvolkmann/bun-examples/tree/main/active-search", "GitHub" %}.

In particular, see the `hx-` attributes on the `input` element below.

```js
import {Elysia} from 'elysia';
import {html} from '@elysiajs/html'; // enables use of JSX
import {staticPlugin} from '@elysiajs/static'; // enables static file serving
import {Html} from '@kitajs/html';

const app = new Elysia();
app.use(html());
app.use(staticPlugin());

const names: string[] = [
  'Amanda',
  'Gerri',
  'Jeremy',
  'Mark',
  'Meghan',
  'Pat',
  'RC',
  'Richard',
  'Tami'
];

const BaseHtml = ({children}: {children: Html.Children}) => (
  <html>
    <head>
      <title>htmx Active Search</title>
      <link href="/public/tailwind.css" rel="stylesheet" />
      <script src="https://unpkg.com/htmx.org@1.9.9"></script>
    </head>
    <body class="p-8">{children}</body>
  </html>
);

app.get('/', () => {
  return (
    <BaseHtml>
      <main>
        <label class="font-bold mr-4" for="name">
          Name
        </label>
        <input
          autofocus="true"
          class="border border-gray-500 p-1 rounded-lg"
          hx-post="/search"
          hx-trigger="keyup changed delay:200ms"
          hx-target="#matches"
          name="name"
          size="10"
        />
        <ul id="matches" />
      </main>
    </BaseHtml>
  );
});

type Body = {name: string};
app.post('/search', ({body}) => {
  const lowerName = (body as Body).name.toLowerCase();
  if (lowerName == '') return '';
  const matches = names.filter(n => n.toLowerCase().includes(lowerName));
  return (
    <ul>
      {matches.map(name => (
        <li>{name}</li>
      ))}
    </ul>
  );
});

app.listen(1919);
console.log('listening on port', app.server?.port);
```

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

## Infinite Scroll

See the example at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/infinite-scroll",
"infinite-scroll" %}.

## Out-of-band Updates

The {% aTargetBlank "https://htmx.org/attributes/hx-swap-oob/",
"hx-swap-oob" %} attribute specifies that an element
should be placed relative to an existing element with the same `id` value.
When the value of the attribute is `true`,
this element replaces the one with the same `id` value.

For example, in a todo app, adding a new todo needs to
insert a new element in the list of todos AND
update the count of todos that may appear somewhere above the list.

The {% aTargetBlank "https://htmx.org/attributes/hx-select-oob/",
"hx-select-oob" %} attribute provides a list of CSS selectors
that specify elements in the response to be included out-of-band.
It provides a way to only use a subset of the elements in the response
that have the `hx-swap-oob` attributes.
The `hx-select-oob` attribute is typically used together with
the `hx-select` attribute which also provides a list of CSS selectors,
but those specify a subset the elements to be included at the target location.

## Components

One issue with htmx is that it doesn't support defining components
in the sense that SPA frameworks like React do.
For example, we can't define a "ProgressBar" component and then
render it with HTML like `<ProgressBar value={value} max={100} />`.

One solution is to use <a href="/blog/bun" target="_blank">Components</a>
which has builtin support for JSX.
This enables writing JavaScript functions that act like React components
(minus support for hooks and DOM diffing), taking props and returning JSX.
Bun can be combined with a server framework like
{% aTargetBlank "https://elysiajs.com", "Elysia" %}.

For a potential solution, see
<a href="/blog/alpine#components" target="_blank">Components</a>.

## Events

The {% aTargetBlank "https://htmx.org/docs/#hx-on", "hx-on" %} attribute
supports handling of more types of events
than the HTML `on*` attributes such as `onclick`.
It provides a small subset of the capabilities in
{% aTargetBlank "https://alpinejs.dev", "Alpine" %} and
{% aTargetBlank "https://hyperscript.org", "_hyperscript" %}
which can be used in place of this attribute.

## Validation

TODO: Try this.
The {% aTargetBlank "https://htmx.org/attributes/hx-validate/",
"hx-validate" %} attribute cause an element to validate itself
using the HTML5 Validation API before a request is send.
This occurs by default for form elements, but not for other kinds of elements.
TODO: What other kinds of elements have a value to be validated?

## Other

- {% aTargetBlank "https://htmx.org/attributes/hx-boost/", "hx-boost" %}

  This attribute changes anchor (`<a>`) and `form` elements to use AJAX
  which allows HTTP requests to be sent when JavaScript is disabled.
  It applies to all anchor and `form` elements on which is applied
  and also those that are descendants of any element on which it is applied.
  Apply this attribute to the `body` element
  to "boost" all anchor and `form` elements.

  TODO: Doesn't AJAX itself require JavaScript?
  TODO: Htmx is a JavaScript library, so how can it
  TODO: process `hx-boost` attributes if JavaScript is disabled?

  By default, anchor elements will
  send a GET request to the specified URL when clicked.
  and `form` elements will send a GET or POST request
  (depending on the value of the `method` attribute) when submitted.
  Both will replace the `innerHTML` of the `body` element with the response.
  These behaviors can be customized with additional htmx attributes.

- {% aTargetBlank "https://htmx.org/attributes/hx-disinherit/", "hx-disinherit" %}

- {% aTargetBlank "https://htmx.org/attributes/hx-encoding/", "hx-encoding" %}

- {% aTargetBlank "https://htmx.org/attributes/hx-vals/", "hx-vals" %}

This specifies additional data to be passed in HTTP requests.

- {% aTargetBlank "https://htmx.org/attributes/hx-vars/", "hx-vars" %}

## Debugging

TODO: Try this.

To cause htmx to log all the actions it performs
to the browser DevTools console, create a JavaScript file
with a name like `setup.js` containing the following:

```js
document.body.addEventListener('htmx:load', () => {
  htmx.logAll();
  console.log('setup.js: logging enabled');
});
```

Then add the following `script` tag to the base HTML used by all the pages.

```html
<script defer src="setup.js"></script>
```

A single interaction with an element that triggers an HTTP reaquest
will cause a lot of debugging output, perhaps too much to be useful.
For example:

```text
htmx.min.js:1 htmx:trigger ...
htmx.min.js:1 htmx:confirm ...
htmx.min.js:1 htmx:validation:validate ...
htmx.min.js:1 htmx:configRequest ...
htmx.min.js:1 htmx:validateUrl ...
htmx.min.js:1 htmx:beforeRequest ...
htmx.min.js:1 htmx:beforeSend ...
htmx.min.js:1 htmx:xhr:loadstart ...
htmx.min.js:1 htmx:xhr:loadstart ...
htmx.min.js:1 htmx:xhr:progress ...
htmx.min.js:1 htmx:xhr:loadend ...
htmx.min.js:1 htmx:xhr:progress ...
htmx.min.js:1 htmx:beforeOnLoad ...
htmx.min.js:1 htmx:beforeSwap ...
htmx.min.js:1 htmx:afterSwap ...
htmx.min.js:1 htmx:afterRequest ...
htmx.min.js:1 htmx:afterOnLoad ...
htmx.min.js:1 htmx:xhr:loadend ...
htmx.min.js:1 htmx:beforeProcessNode ...
htmx.min.js:1 htmx:beforeProcessNode ...
htmx.min.js:1 htmx:beforeProcessNode ...
htmx.min.js:1 htmx:load ...
htmx.min.js:1 htmx:afterSettle ...
```

Another debugging approach is to use an unminified version of the htmx library
and set breakpoints in its functions.

## Animation

TODO: Try this.

## Boosting

TODO: Try this.

## WebSockets

TODO: Try this.
hx-ws

## Server Sent Events

TODO: Try this.
{% aTargetBlank "https://htmx.org/extensions/server-sent-events/", "hx-sse" %}

## History

hx-history-elt
hx-push-url

## \_hyperscript

{% aTargetBlank "https://hyperscript.org", "_hyperscript" %} is
"an easy & approachable language for modern web front-ends".
It can be used in conjunction with htmx.

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
implementing the backend services for an htmx application.

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
(Bun, Elysia, Turso, and htmx).

Another popular option is to use Python with either
{% aTargetBlank "https://www.djangoproject.com", "Django" %} or
{% aTargetBlank "https://flask.palletsprojects.com/", "Flask" %}.

## Optimistic Updates

If an endpoint might be slow to return a response,
using `hx-indicator` to display a spinner is a good idea.
Alternatively, the front end can assume success and update the UI optimistically.
For example, clicking a "like" button can immediately change its color
to a muted version of the color that will be used when the response is received.
If the response indicates success, the color can be changed to the full color.
If the response indicates failure, the color can be reset.

## Mobile Apps

The hypermedia approach used by htmx to build web applications
can also be used to build mobile apps.
See {% aTargetBlank "https://hyperview.org", "Hyperview" %}
which builds on React Native.

## Resources

- {% aTargetBlank "https://htmx.org", "htmx Home Page" %}
- {% aTargetBlank "https://htmx.org/docs/", "htmx Documentation" %}
- {% aTargetBlank "https://htmx.org/reference/", "htmx Reference" %}
- {% aTargetBlank "https://htmx.org/examples/", "htmx Examples" %}
- {% aTargetBlank "https://htmx.org/essays/", "htmx-related Essays" %}
- {% aTargetBlank "https://htmx.org/essays/how-did-rest-come-to-mean-the-opposite-of-rest/",
  "How Did REST Come To Mean The Opposite of REST?" %}

- {% aTargetBlank "https://htmx.org/discord", "htmx Discord Channel" %}
- {% aTargetBlank "https://hypermedia.systems", "Hypermedia Systems" %} book
- {% aTargetBlank "https://bigsky.software", "Big Sky Software" %}
- {% aTargetBlank "https://twitter.com/htmx_org", "Carson Gross on Twitter" %}
- {% aTargetBlank "https://thevalleyofcode.com/htmx", "The Valley of CODE - htmx" %}
- {% aTargetBlank "https://ahastack.dev", "The AHA Stack" %}

TODO: Read "The Valley of CODE" page above!

## Unorganized Content

- Learn how to automatically refresh browser on code save.
- Should you use auto focus for the new to do input?
- Can you use JavaScript code in place of hyper script code?
- Study how htmx supports dialogue boxes.
- Study Alpine JS and consider using it with htmx.
- Read essays by Carson Gross on the htmx website.

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
  This is useful when htmx has replaced portions of a page.

- Htmx can work with JavaScript disabled
  by setting the `hx-boost="true"` attribute.
  Does this need to be on the `body` element? How does this work?

- See unpoly.com which is a competitor to htmx.
  Supposedly it is better at progressive enhancement.

- Learn about hx-boost.

- See https://www.npmjs.com/package/@jop-software/hx-chart.

- See HoTMiXer for starting new htmx projects at
  https://www.npmjs.com/package/hotmixer
