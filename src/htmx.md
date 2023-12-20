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
by sending an HTTP request using any verb (GET, POST, PUT, PATCH, or DELETE).
The response must contain HTML.
Rather than performing a complete page refresh, the returned HTML
replaces an existing DOM element (transclusion) or is inserted relative to one.

The HTMX approach removes the need to serialize data to JSON on the server,
parse the JSON on the client, and convert it to HTML.

The server can be implemented using
any programming language and server framework.
The server typically plays two roles.
First, it serves static files such as HTML, CSS, JavaScript, and images.
Second, it responds to HTTP requests by returning HTML.

HTMX simplifies state management because all the state is in one place,
on the server.

Many web app features typically thought to require custom JavaScript code
can instead be implemented with only HTMX.
Examples include lazy loading of data, infinite scrolling,
and searching while the user types in an `input`.

The fact that all HTML rendered by HTMX applications
is either static or server rendered makes it great for SEO.

The HTMX library is implemented in JavaScript,
not TypeScript, in a single source file.
There are plans to add JSDoc TypeScript type definitions
for better code editor support.

## REST?

Web app frameworks such as React, Svelte, Angular, and Vue
have popularized the creation of single-page applications (SPAs).
Currently, SPA web apps typically use client-side JavaScript code
to send HTTP requests to server-side endpoints that
query/update databases and return JSON data.
The client code then transforms JSON into an HTML presentation.
Many developers refer to this architecture as "REST".

This is not what Roy Fielding had in mind when he wrote his famous dissertation
"{% aTargetBlank
"https://ics.uci.edu/~fielding/pubs/dissertation/fielding_dissertation.pdf",
"Architectural Styles and the Design of Network-based Software Architectures" %}"
that gave birth to REST.
Roy has been quoted saying "I am getting frustrated by
the number of people calling any HTTP-based interface a REST API.
Today's example is the SocialSite REST API.
That is RPC. It screams RPC."

But who decides what is REST and what isn’t?
Is it Roy Fielding or popular opinion?

The HTMX approach is based on "Hypermedia As The Engine Of Application State"
({% aTargetBlank "https://htmx.org/essays/hateoas/", "HATEOAS" %}) which is
a specific use of the REST architecture where services return hypermedia.
The acronym HATEOAS does not appear in the dissertation,
but its concepts are discussed.

Carson Gross describes HATEOAS systems as follows:
"Given an entry point into the system, the rest of the system
can be accessed simply by inspecting the hypermedia."

A software architecture is RESTful if it uses a client/server model,
is stateless, caches responses, and supports a uniform interface.
A uniform interface is one where requests identify a resource,
resources are manipulated through representations,
messages are self-descriptive, and use HATEOAS.

HTML is a kind of hypermedia and web browsers are hypermedia clients.
Responses to requests can contain HTML that browsers render
to enable users to view and operate on resources.
This makes the responses similar to object-oriented programming
where objects contain data and methods for operating on their data.
Clients simply render the HTML returned.
They do not require any resource-specific knowledge
about how to render or modify resources.

JSON responses that do not describe valid operations
on the resource they describe and do not support HATEOAS.
While it is possible to describe valid operations in JSON,
doing so places the burden of interpreting and implementing
the operations on client-side code rather than the browser.
JSON is not hypermedia.

## Questions to Consider

Why should we take data from a database and
convert it to JSON just to send raw data to the client?

Why should we decide how to render data on the client instead of on the server?

Why should we restrict ourselves to only using JavaScript
to implement web applications?

## History and Future

HTMX was created by {% aTargetBlank "https://bigsky.software/cv/",
"Carson Gross" %}. He is a principal software engineer at
{% aTargetBlank "https://bigsky.software/", "Big Sky Software" %}
and a part-time Computer Science instructor at Montana State University.

Carson been working on the approach of simplifying web development
by adding attributes to HTML for a long time.

The predecessor of HTMX, also created by Carson Gross,
is {% aTargetBlank "https://intercoolerjs.org", "intercooler.js" %}.
Work on intercooler.js began in 2013
and the first version was released in April 2014.
Intercooler had a dependency on jQuery, but HTMX does not.

The first version of HTMX was released in May 2020.
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

Interest in HTMX exploded in 2023 after YouTube videos
from ThePrimeagen and Fireship were released.
See {% aTargetBlank "https://www.youtube.com/watch?v=zjHHIqI9lUY", "HTMX" %} and
{% aTargetBlank "https://www.youtube.com/watch?v=r-GSGH2RxJs", "HTMX in 100 seconds" %}.

Companies that sponsor the development of HTMX are listed on the
{% aTargetBlank "https://htmx.org", "HTMX home page" %}.
They include GitHub and JetBrains.

## Client-side Processing

HTMX is not appropriate for all features of web apps.
Using HTMX to update the UI on every mouse move or drag
would be too slow since each movement would trigger a new HTTP request.
Examples of apps that require this kind of functionality include
Google Maps and many games.

HTMX can be used in conjunction with other approaches,
so it can be used for the parts of apps that
do not require high frequency updates.

Options for implementing client-side processing include vanilla JavaScript,
{% aTargetBlank "https://alpinejs.dev", "Alpine" %}, and
{% aTargetBlank "https://hyperscript.org", "_hyperscript" %}.

Alpine adds support for many new HTML attributes.
\_hyperscript adds support for one new HTML attribute
whose name is a single underscore (`_`) and whose value is \_hyperscript code.
Both differ from HTMX in that they focus on client-side interactions
rather that processing HTTP requests.

Like HTMLx, Alpine and \_hyperscript are client-side JavaScript libraries.
These are much lighter than libraries and frameworks like React.

## JSON

While HTMX applications do not require implementing and using
REST APIs that returns JSON data, it may still be desirable to implement them.
For example, Android and iOS mobile apps may wish to use such APIs.
To use these in HTMX apps, call them from HTTP endpoints
that use the data to generate HTML responses.

Alternatively, write functions that return the desired data
and call them from both the endpoints that return JSON
and the endpoints that return HTML.

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

## Using TypeScript

The following steps provide one way to use TypeScript with an HTMX project.
They create a new project that uses HTMX, TypeScript, and Vite.
Vite provides a local HTTP server with hot reload.

TODO: Try and correct these steps!

- Enter `npm init vite@latest`

  - Enter a project name.
  - For the framework, select "Vanilla".
  - For the variant, select "TypeScript".

- cd to the newly created project directory
- Enter `npm install alpinejs`
- Enter `npm install -D @types/alpinejs`
- Replace the contents of `src/main.ts` with the following:

  ```ts
  import Alpine from 'alpinejs';
  window.Alpine = Alpine;
  Alpine.start();
  ```

- Create the file `src/global.d.ts` containing the following:

  ```ts
  import {Alpine as AlpineType} from 'alpinejs';

  declare global {
    var Alpine: AlpineType;
  }
  ```

- Edit `index.html` which already contains a `script` tag for `/src/main.ts`.
  Add HTML that uses Alpine directives here.

- Enter `npm run dev`

- Browse localhost:5173.

## Using Tailwind

There are two approaches for using Tailwind for CSS styling in an HTMX app.

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

With HTMX, any element can trigger an HTTP request.
The first thing to consider is what will trigger a request to be sent.

HTML elements such as `input`, `textarea`, and `select`
automatically trigger when the emit a "change" event.
HTML `form` elements automatically trigger when they emit a `submit` event.

To trigger on a different event, add the `hx-trigger` attribute.
The value of this attribute can be
a single event name, a comma-separated list of event names,
or `every {timing}` to trigger repeatedly at a given time interval.

Event names include `click`, `mouseenter`, and custom event names.

Events can be filtered so they only trigger in specific circumstances.
This is specified in square brackets after an event name.
For example, `click[shiftKey]` only triggers when
an element is clicked while the shift key is held down.
Multiple filters can be specify by and'ing them.
For example, `click[shiftKey&&ctrlKey]`.
The square brackets can also contain
a call to a JavaScript function that returns a Boolean value
where triggering doesn't occur if the function returns `false`.

A space-separated list of event modifiers can follow an event name.
For example, `click ...`
TODO: FINISH THIS!

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
to display in a confirmation dialog using the `Window` method `confirm`.
The dialog is very plain and cannot be styled.
It may be preferable to use a dialog that can be styled.
An HTTP request is only sent and processed if the user clicks the OK button.

## URLs

HTMX endpoints return HTML, not JSON.
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

hx-select
hx-preserve

## Active Search

HTMX can be used to implement an active search where a list of matching data
is retrieved as the user enters search text.
The following code demonstrates this.
The full project can be found in {% aTargetBlank
"https://github.com/mvolkmann/bun-examples/tree/main/active-search", "GitHub" %}.

In particular, see the `hx-` attributes on the `input` element below.

```js
import {Elysia} from 'elysia';
import {html} from '@elysiajs/html'; // enables use of JSX
import {staticPlugin} from '@elysiajs/static'; // enables static file serving

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

// TODO: What type should be used for children?
const BaseHtml = ({children}: {children: any}) => (
  <html>
    <head>
      <title>HTMX Active Search</title>
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

TODO: Try this.
hx-swap-oob
hx-select-oob

## Components

One issue with HTMX is that it doesn't support defining components
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

To cause HTMX to log all the actions it performs
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

Another debugging approach is to use an unminified version of the HTMX library
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
- {% aTargetBlank "https://htmx.org/essays/how-did-rest-come-to-mean-the-opposite-of-rest/",
  "How Did REST Come To Mean The Opposite of REST?" %}

- {% aTargetBlank "https://htmx.org/discord", "HTMX Discord Channel" %}
- {% aTargetBlank "https://hypermedia.systems", "Hypermedia Systems" %} book
- {% aTargetBlank "https://bigsky.software", "Big Sky Software" %}
- {% aTargetBlank "https://twitter.com/htmx_org", "Carson Gross on Twitter" %}
- {% aTargetBlank "https://thevalleyofcode.com/htmx", "The Valley of CODE - htmx" %}

TODO: Read "The Valley of CODE" page above!

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

- HTMX can work with JavaScript disabled
  by setting the `hx-boost="true"` attribute.
  Does this need to be on the `body` element? How does this work?

- See unpoly.com which is a competitor to HTMX.
  Supposedly it is better at progressive enhancement.

- Does hx-select from HTMX allow extracting specific content
  from an HTML response? For example, this could be used to
  get the body from a response that is a full HTML document.

- Learn about hx-boost.
