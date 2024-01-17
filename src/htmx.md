---
eleventyNavigation:
  key: htmx
layout: topic-layout.njk
---

<style>
  figcaption {
    margin-top: 0.5rem;
    text-align: center;
  }
  img {
    border: 1px solid gray;
  }
</style>

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

<img alt="hypermedia driven applications" style="border: none; width: 70%"
  src="/blog/assets/hypermedia-driven-applications.png?v={{pkg.version}}">

The server can be implemented using
any programming language and server framework.
This is referred to as "Hypermedia On Whatever you'd Like" (HOWL).
The server typically plays two roles.
First, it serves static files such as HTML, CSS, JavaScript, and images.
Second, it responds to HTTP requests by returning HTML.

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

## Why Use htmx?

The following is a brief summary of the main benefits
of using htmx compared to SPA frameworks.
Many of these are discussed in more detail in subsequent sections.

- Fixes HTML shortcomings

  Htmx enables many kinds of user interactions with any kind of HTML element
  to trigger any kind of HTTP request and insert HTML in the response
  into a specific location in the DOM without a full page refresh.
  This dramatically reduces the need for custom client-side JavaScript code.
  It also preserves scroll state.

- Improves startup time

  With htmx browsers download considerably less JavaScript code which improves
  metrics such as "First Contentful Paint" and "Time to Interactive".
  This results in a better user experience.

- Favors LoB over SoC

  The Locality of Behavior (LoB) pattern places related code together
  which makes the code easier to understand and modify
  than following a Separation of Concerns (SoC) pattern.
  From Richard Gabriel, LoB "enables a programmer to
  understand source by looking at only a small portion of it."

- Eliminates JSON as an intermediate format

  Typically SPA application endpoints fetch data,
  serialize it to JSON, and return the JSON.
  Then code running in the browser parses the JSON
  and generates HTML from it.

  Htmx endpoints fetch data, generate HTML from it, and return the HTML.
  The browser only has to render the returned HTML.
  Removing JSON as an intermediate format provides efficiency gains.

- Enables Hypermedia ON Whatever you'd Like (HOWL)

  Htmx enables implementing web applications using any programming language
  that can implement an HTTP server whose endpoints return HTML responses.
  This includes most programming languages.

- Encourages full-stack development

  Often in SPA development one team of developers implements
  endpoints that return JSON data and another team of developers
  implements web user interfaces that use those endpoints.

  With htmx each developer develops complete features by
  implementing endpoints that return HTML that includes htmx attributes.
  This requires developers to know the selected programming language,
  HTML, and CSS, but not necessarily JavaScript.

- Reduces learning curve

  Learning how to use htmx attributes in HTML
  is significantly easier than learning a SPA framework.

- Simplifies state management

  Typical SPA applications manage state on both the server and client.
  Keeping the state in sync in two places introduces challenges.

  With htmx all the state is only on the server,
  so no state synchronization is needed
  and browser memory usage is reduced.

- Simplifies client-side code

  See the next section for details.

## Simplified Client-side

Using endpoints that return HTML instead of JSON
removes the need for many things including:

- nearly all client-side logic

  This is mostly unnecessary because the logic is embedded in
  the HTML elements returned by the server.
  This implements Hypermedia As The Engine Of Application State (HATEOAS).

  Fewer client-side dependencies are needed
  and client-side build time is reduced.

- managing client-side data models

  This is unnecessary because all the data remains only on the server.

- client-side routing

  This is unnecessary because page updates and transitions
  are handled by HTTP requests triggered on elements
  described in the HTML returned by the server.

- versioning

  This is unnecessary because the browser does not need to
  interpret the meaning of the HTML being returned.
  All that is needed is a URL for rendering a starting page for an application.
  All other interactions are derived through URLs in that page
  and pages reached from it.
  Endpoints are free to modify the HTML they return.
  This is the crux of HATEOAS (described later)

The HTML returned by endpoints is typically
larger than a corresponding JSON response would be.
This is a non-issue because htmx trades "representational efficiency"
for achieving other goals such as the ones described above.

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
and parsing JSON in the browser are eliminated.

What are the advantages of generating HTML in the browser
instead of doing the same work on the server?

Why should we restrict ourselves to only using JavaScript
to implement web applications?
With htmx the server code can be implemented in any programming language
that supports implementing HTTP servers.
The most suitable languages are ones that
provide good support for HTML templating.
It is also desirable to have editor tooling that
provides syntax highlighting of HTML and
checking for proper HTML including attribute usage and element nesting.

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

Htmx cannot currently be used in apps that require offline support
because they rely on sending HTTP requests to
get new HTML that updates the UI.
However, there is a effort to address this.
See {% aTargetBlank "https://github.com/spirodonfl/htmx-offline-mode",
"htmx-offline-mode" %}.

Htmx can be used in conjunction with other approaches.
Consider using it for all the parts of apps
that do not require high frequency updates.

## Client-side Processing

Htmx applications to not require sending an HTTP request for every user interaction.
They can use HTML elements such as `details` and `dialog`
and client-side scripting.
Scripting options include vanilla JavaScript,
{% aTargetBlank "https://mvolkmann.github.io/blog/topics/#/blog/alpine/", "Alpine" %}, and
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

There are three ways to use htmx.

1. Used htmx from a CDN by including the following `script` tag:

   ```html
   <script src="https://unpkg.com/htmx.org@1.9.10"></script>
   ```

1. Install htmx using a package manager like npm or bun.

   Enter `npm install htmx.org` or `bun add htmx.org`.
   Then bundle the file `node_modules/htmx.org/dist/htmx.min.js`
   with other JavaScript files being used by the application
   OR copy that file to a directory whose files are served by
   your application server and include a `script` tag to include it.

1. Download htmx as a single, minified JavaScript file
   by clicking the "~14k min.gz’d" link near the top of the
   {% aTargetBlank "https://htmx.org", "htmx home page" %}.
   Place the downloaded file in a directory whose files are served by
   your application server and include a `script` tag like the following:

   ```html
   <script src="htmx.min.js"></script>
   ```

## Configuring

Htmx supports a large number of configuration options. For details, see
<a href="https://htmx.org/docs/#config" target="_blank">Configuring htmx</a>.

The options can be configured in a `meta` tag
that is a child of the `head` tag of each page.

For example, the configuration option `htmx.config.allowScriptTags`
is `true` by default. The following `meta` tag changes this.

```html
<meta name="htmx-config" content='{"allowScriptTags": false}' />
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
or `every {timing}` to trigger repeatedly at a given time interval
(for polling).

DOM event names include `blur`, `change`, `click`, `contextmenu`,
`dblclick`, `focus`, `hashchange`, `input`, `keydown`, `keypress`,
`keyup`, `load`, `mousedown`, `mouseenter`, `mouseleave`, `mousemove`,
`mouseout`, `mouseover`, `mouseup`, `resize`, `scroll`, `submit`,
`touchcancel`, `touchend`, `touchmove`, `touchstart`, `unload`, and more.
Custom event names can also be used.

A space-separated list of event modifiers can follow an event name.
Supported event modifiers include:

- `changed` - only send request if the element value has changed
- `consume` - stops the event from bubbling up to ancestor elements
- `delay:{time}` - wait at least this long before each request is sent (debouncing)
- `from:{css-selector}` - listen for the event on a different element
- `once` - only trigger the request one time
- `queue` - affects how concurrent events are processed
- `target` - only trigger if event occurred on an element matching a CSS selector
- `throttle:{time}` - only send the last event received in the specified time
  and then wait again

For example, the following trigger is useful for implementing active search.

```html
hx-trigger="keyup changed delay:500ms"
```

The "changed" modifier causes the HTTP request to
only be sent if the value of the input has changed.
For example, using the arrow keys to move the cursor does not change the value.

The "delay" modifier adds debouncing so an HTTP request is
only sent when the user briefly stops typing, not for every change.

Events can be filtered so they only trigger in specific circumstances.
This is specified in square brackets after an event name.
For example, `click[shiftKey]` only triggers when
an element is clicked while the shift key is held down.
Multiple filters can be specify by and'ing them.
For example, `click[shiftKey && ctrlKey]`.
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

When an HTTP request is triggered on a `form` element,
the values of all inputs inside the form will be included in the request.
When a non-GET HTTP request is triggered on a non-form element,
htmx will search upward in the DOM hierarchy for a `form` element.
If one is found, htmx will include the values
of all inputs inside the form in the request.
If a form is not found, only the value of
the element that triggered the request is included.

To include the values of additional form elements
that are outside the `form` (or when no `form` is used),
add the {% aTargetBlank "https://htmx.org/attributes/hx-include/",
"hx-include" %} attribute.
Its value is a comma-separated list of CSS selectors,
include the relative values described in the "Targets" section.

See the working example project that uses `hx-include` at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/non-form-submit",
"non-form-submit" %}.

To omit the values of some of the form elements
that are inside the form, add the `hx-params` attribute.
For more detail, see {% aTargetBlank
"https://htmx.org/attributes/hx-params/", "hx-params" %}.

The `hx-confirm` attribute specifies a question to display in a
browser-supplied confirmation dialog (using the `Window` method `confirm`)
before an HTTP request is sent.
The dialog contain have "OK" and "Cancel" buttons.
The request will only be sent if the user clicks the "OK" button.

The `hx-prompt` attribute specifies a prompt to display in a
browser-supplied prompt dialog (using the `Window` method `prompt`)
before an HTTP request is sent.
The dialog contain a text input and "OK" and "Cancel" buttons.
The request will only be sent if the user clicks the "OK" button.
The request header `hx-prompt` will hold
the value the user entered in the text input.

The confirmation dialog is very plain and cannot be styled.
It may be preferable to use a dialog that can be styled,
perhaps using the HTML `dialog` element.

The {% aTargetBlank "https://htmx.org/attributes/hx-sync/", "hx-sync" %}
attribute coordinates concurrent requests.
For example, entering data in an `input` that is inside a `form`
could trigger two requests, one to validate the `input` value
and one to submit the `form`.
By default, these requests will run in parallel.
If `hx-sync="closest form:abort` is applied to the `input`,
the validation request will be sent first.
If the validation succeeds then the submit request will be sent.
Otherwise it will be aborted.

## URLs

Htmx endpoints return HTML, not JSON.
Their URLs are based on user interface functionality, not resource paths.
For example, `/todos/:id/toggle` is a reasonable URL path
for an endpoint that toggles the completed status of todo
and returns HTML for the updated todo item.

## Security

Three ways to increase the security of an htmx application are:

- Use a Content Security Policy (CSP) which limits the sources of assets.
- Configure htmx to ignore script tags in HTML returned by endpoints.
  This is described in the [Configuring](#configuring) section.
- Prevent Cross-Site Scripting (XSS) attacks by sanitizing the HTML
  returned from endpoints if it includes user-entered content.

All three of these options can prevent executing `script` tags
found in user-entered text.

### Content Security Policy (CSP)

A CSP is specified by adding a `meta` tag as a child of the `head` tag
in each HTML page. For example:

```html
<meta
  http-equiv="Content-Security-Policy"
  content="default-src 'self'; img-src https://*"
/>
```

This CSP says:

- By default all content must come from the domain of this web app.
- An exception is made for images which can come from any `https` URL.

Will this CSP in place, all JavaScript libraries including htmx
will need to be downloaded and served from the web app domain.

For more detail on what can be specified in a CSP, see the MDN {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy",
"Content-Security-Policy" %} page.

### Sanitizing HTML

A good library for sanitizing HTML is {% aTargetBlank
"https://github.com/apostrophecms/sanitize-html", "sanitize-html" %}.
This provides the function `sanitizeHtml` which
strips out all elements that are not in an approved list.
The `script` element is not in this list
because they may do malicious things.

See the working example project at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/sanitizing-html",
"sanitizing-html" %}.

The following are the relevant lines of code from that project.

Requests sent to the `/render` endpoint contain the form data property `markup`.
Users can enter any HTML, including `script` tags.

```ts
import type {Context} from 'hono';
import sanitizeHtml from 'sanitize-html';

app.post('/render', async (c: Context) => {
  const data = await c.req.formData();
  const markup = data.get('markup');
  console.log('index.tsx /render: markup =', markup);
  return c.html(sanitizeHtml(markup));
});
```

If the user enters text like
`<p>Hello</p><script>alert('pwned')</script><p>Goodbye</p>`,
all that will be rendered are the paragraphs containing "Hello" and "Goodbye".

## Targets

Content returned from HTTP endpoints can be placed at
a specific target location in the DOM.

The `hx-target` attribute specifies a target DOM element.
Supported values include:

- `{css-selector}` targets first element that matches
- `this` targets current element (default)
- `closest {css-selector}` targets closest ancestor element or itself that matches
- `find {css-selector}` targets first descendant element that matches
- `next` targets next sibling element
- `next {css-selector}` targets next matching element
- `previous` targets previous sibling element
- `previous {css-selector}` targets previous matching sibling

These enable identifying elements that do not have `id` attributes.

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

An endpoint can prevent a swap from occurring
by returning a status code of 204 (No Content).

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
Nothing about preserved elements is modified.

## Busy Indicators

The `hx-indicator` attribute specifies a CSS selector
that matches one or more elements.
Those elements will have their opacity changed from 0 to 1
while an associated HTTP request is being processed.

In addition, when an HTTP request is triggered,
the CSS class `htmx-request` is added to
the element(s) identified by the `hx-indicator` attribute OR
the element that triggered the request
if the `hx-indicator` attribute is not present.
In the latter case, a CSS rule matching `htmx-request`
can be used to style the triggering element.

The `hx-disabled-elt` attribute specifies a CSS selector
that matches one or more elements.
Those elements will have the `:disabled` CSS pseudo-class applied
while an associated HTTP request is being processed.

The following HTML demonstrates
showing an indicator and disabling the "Add" button
while waiting for a new todo to be added.

The spinner image must be given the CSS class `htmx-indicator`,
which hides the spinner by setting `opacity` to `0`.
When the CSS class `htmx-request` is also applied,
the spinner is shown by setting `opacity` to `1`.

```html
<form
  hx-post="/todos"
  hx-swap="afterend"
  hx-disabled-elt="#add-btn"
  hx-indicator=".htmx-indicator"
  _="on submit target.reset()" // resets form using _hyperscript
>
  <input name="description" />
  <button id="add-btn" type="submit">Add</button>
  <img
    alt="loading..."
    class="htmx-indicator"
    src="/public/spinner.gif"
  />
</form>
```

## Multiple Updates

There are three ways to update multiple parts of the current page.

1. Expanding the target
1. Out-of-band swaps
1. Triggering events

### Expanding the target

Rather than updating multiple elements,
this approach uses a single target that encompasses
all the elements that require updating.

This can be non-optimal because it requires the endpoint
to build and return more HTML that is technically necessary.
Much of it will likely be identical to what is already rendered.

### Out-of-band Updates

The {% aTargetBlank "https://htmx.org/attributes/hx-swap-oob/",
"hx-swap-oob" %} attribute with a value of `"true"` specifies that
an element with the same id should be replaced by this element.
The element being replaced does not need to have the `hx-swap-oob` attribute,
just a matching `id` value.

For example, a todo app can display
the number of uncompleted todos and the total number of todos.
Adding a new todo, deleting a todo, and toggling the completed state of a todo
all change what is displayed.
One way to accomplish this is for all those endpoints to return
the following in addition to any other HTML they need to return.

```ts
<p id="todo-status" hx-swap-oob="true">
  {uncompletedCount} of {totalCount} remaining
</p>
```

The {% aTargetBlank "https://htmx.org/attributes/hx-select-oob/",
"hx-select-oob" %} attribute provides a list of CSS selectors
that specify elements in the response to be included out-of-band.
It provides a way to only use a subset of the elements in the response
that have the `hx-swap-oob` attributes.
The `hx-select-oob` attribute is typically used together with
the `hx-select` attribute which also provides a list of CSS selectors,
but those specify a subset the elements to be included at the target location.

### Custom Events

Any endpoint can trigger a custom event
by setting the HTTP response header `hx-trigger` to the event name.
When an HTML element receives the event,
it can trigger another HTTP request to swap in new content.

For example, a todo app can display
the number of uncompleted todos and the total number of todos.
Adding a new todo, deleting a todo, and toggling the completed state of a todo
all change what is displayed.
One way to accomplish this is for all those endpoints
to trigger the same custom event.

HTML elements that trigger sending HTTP requests to those endpoints
will receive the event.
By default, the event will bubble up the DOM hierarchy.

Any element can listen for the event on the `body` element.
For example, the following element listens for
the custom event "status-change" on the `body` element.
That triggers the element to send a GET request to `/todos/status`
which returns the text to include in the paragraph tag.
This also occurs when the element is initially loaded
because `load` is also one of the triggers.

```js
<p hx-get="/todos/status" hx-trigger="load, status-change from:body" />
```

See the working example project at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/todo-list",
"todo-list" %}.

## Common Patterns

### Lazy Loading

When displaying data that is expensive to acquire,
it is useful to only request it when the part of the page
that will display it scrolls into view.
That can be doe using `hx-trigger="revealed"`.

For example:

```js
<table hx-get="/weather/forecast" hx-trigger="revealed"></table>
```

See the working example project at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/lazy-load",
"lazy-load" %}.

<img alt="htmx Lazy Loading" style="width: 70%"
  src="/blog/assets/htmx-lazy-loading.png?v={{pkg.version}}">

The following HTML appears near the bottom of the page
so it is out of view when the page is first loaded.

```js
<h2>Users</h2>
<div
  hx-get="/users"
  hx-indicator=".htmx-indicator"
  hx-trigger="revealed"
/>
<img alt="loading..." class="htmx-indicator" src="/spinner.gif" />
```

The following code defines the `/users` endpoint
which returns an HTML `table` containing user data.

```js
const URL = 'https://jsonplaceholder.typicode.com/users';

app.get('/users', async () => {
  Bun.sleepSync(1000); // simulates long-running query
  const res = await fetch(URL);
  const users = await res.json();
  return (
    <table>
      <thead>
        <tr>
          <th>ID</th>
          <th>Name</th>
          <th>Email</th>
          <th>Company</th>
        </tr>
      </thead>
      <tbody>
        {users.map(user => (
          <tr>
            <td>{user.id}</td>
            <td>{user.name}</td>
            <td>{user.email}</td>
            <td>{user.company.name}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
});
```

### Input Validation with API calls

Some input validation must be performed on the server.
For example, when validating setup of a new user
that is identified by their email address,
it is common to verify that the provided email address
is not already in use by an existing user.
This can be done as the user types
instead of waiting for the `form` to be submitted.

The following HTML validates an email address as it is typed.
The "/email-validate" endpoint returns
an empty string when the email address is not in use
or the message "email in use".
The returned string is used at the content of the `span` element
that follows the `input` element.

```html
<label for="email">Email</label>
<input
  id="email"
  hx-get="/email-validate"
  hx-target="#email-error"
  hx-trigger="keyup changed delay:200ms"
  name="email"
  type="email"
/>
<span class="error" id="email-error" />
```

See the working example project at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/email-validation",
"email-validation" %}.

### Deleting an Element

Sometimes the result of sending a request to an endpoint
should be deleting the element that triggered the request.
For example, in a todo app clicking a button to delete a todo
needs to send an HTTP request so the todo can be deleted on the server.
Then the row describing the todo should be removed from the DOM.

The following HTML can be used to describe each todo.
Note the use of `hx-swap="outerHTML"` and `hx-target="closest div"`
on the `button` element.
With those in place, the HTML returned by the `/todo/${id}` endpoint
will replace the `div` that contains the `button`.
That endpoint deletes the todo from the collection of todos on the server
and returns nothing, effectively deleting that `div`.

```js
<div class="todo-item">
  <input
    type="checkbox"
    checked={isCompleted}
    hx-patch={`/todos/${id}/toggle`}
    hx-target="closest div"
    hx-swap="outerHTML"
  />
  <div class={isCompleted ? 'completed' : ''}>{description}</div>
  <button
    class="plain"
    hx-confirm="Are you sure?"
    hx-delete={`/todos/${id}`}
    hx-swap="outerHTML"
    hx-target="closest div"
  >
    🗑
  </button>
</div>
```

See the working example project at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/todo-list",
"todo-list" %}.

<img alt="htmx Todo List" style="width: 50%"
  src="/blog/assets/htmx-todo-list.png?v={{pkg.version}}">

### CSS Transitions

When htmx swaps HTML into the DOM it does the following:

- Add the `htmx-swapping` CSS class to the target element.
- Delay for a short time (`htmx.config.defaultSwapDelay` defaults to 0).
- Remove the `htmx-swapping` CSS class to the target element.
- Add the `htmx-settling` CSS class to the target element.
- Create a DOM element representing the new HTML and
  add the CSS class `htmx-added` to it.
- Swap the new DOM element into the DOM,
  either replacing the target or placing it relative to the target,
- Delay for a short time (`htmx.config.defaultSettleDelay` defaults to 20ms).
- Remove the `htmx-added` CSS class from the new DOM element.
- Remove the `htmx-settling` CSS class from the target element.

The delays described above enable adding CSS transitions.

Two things are required to implement a CSS transition.
First, the existing element and the new element
must have the same `id` attribute value.
This is referred to as having a "stable id".
Second, the `swap` delay should be set to
the same duration as the CSS transition.
For example, `hx-swap="outerHTML swap:1s"`.

The todo list app described in the previous section
fades out the row of a todo before deleting it.

The following CSS is added to describe the desired transition.

```css
.todo-item.htmx-swapping {
  opacity: 0;
  transition: opacity 1s ease-out;
}
```

The `hx-swap` attribute on the delete button
is modified to increase the delay between
adding the `htmx-swapping` CSS class to the target and removing it.
The target in this case is the element that
represents the todo row and contains the delete button.
This delays actually removing the target
until the CSS transition has time to complete.

```js
<button
  class="plain"
  hx-confirm="Are you sure?"
  hx-delete={`/todos/${id}`}
  hx-swap="outerHTML swap:1s"
  hx-target="closest div"
>
  🗑
</button>
```

In a similar way, the delay between
adding the `htmx-settling` CSS class to the target and removing it
can be modified by adding the modifier `settle:{time}`.

To fade content into view, no `settle` delay is need.
Suppose the new content has the CSS class `new-content`.
The following CSS are rules are all that is required to fade it into view.

```css
.new-content.htmx-added {
  opacity: 0;
}

.new-content {
  opacity: 1;
  transition: opacity 1s ease-in;
}
```

For another example of using a CSS transition, see the working project
{% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/progress-bar",
"progress-bar" %}.
This repeatedly renders an element with a fixed `id` value
and achieves smooth animation using a CSS transition.

### Resetting a Form

Often it is desirable to reset a `form` after a successful submit.
This can be done by calling `this.reset()` where `this` refers to the `form`.

Checking the request path is needed if any of the inputs
can also send requests, perhaps for validation.

To specify code to run after a request has been sent and
a response has been received, use the `hx-on:htmx:after-request` attribute.
When using JSX, we can avoid cluttering the `form` element
by using attribute spreading.
Another reason to use this is because VS Code does not recognize
`hx-on:htmx:after-request` as a valid attribute name.

```js
const reset = {
  'hx-on:htmx:after-request': `
    if (event.detail.pathInfo.requestPath === '/account' &&
      event.detail.successful) this.reset()`
};

return (
  <>
    ...
    <form hx-post="/account" hx-target="#result" {...reset}>
      {/* Add form inputs here.
          HTML form validation will not work
          if the hx-post attribute is moved
          from the form to this button. */}
      <button>Submit</button>
    </form>
    <div id="result" />
    ...
  </>
);
```

### Active Search

Htmx can be used to implement an active search where a list of matching data
is retrieved as the user enters search text.
The following code demonstrates this.
It uses Tailwind CSS classes.
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
      <script src="https://unpkg.com/htmx.org@1.9.10"></script>
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

### Optimistic Updates

If an endpoint might be slow to return a response,
using `hx-indicator` to display a spinner is a good idea.
Alternatively, the front end can assume success and update the UI optimistically.
For example, clicking a "like" button can immediately change its color
to a muted version of the color that will be used when the response is received.
If the response indicates success, the color can be changed to the full color.
If the response indicates failure, the color can be reset.

TODO: Add an example of this.

### Pagination

See the working example project at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/pagination",
"pagination" %}.

This example initially renders the following HTML which includes
the initial `table`, loaded with the first "page" of rows.
It also includes a row containing the pagination buttons and
a loading spinner that is only displayed when waiting on an HTTP response.

```html
<table
  hx-indicator=".htmx-indicator"
  hx-trigger="load"
  hx-get="/pokemon-rows?page=1"
></table>
<div id="pagination-row">
  <span id="pagination-buttons"></span>
  <img alt="loading..." class="htmx-indicator" src="/spinner.gif" />
</div>
```

The "pokemon-rows" endpoint returns the following JSX
in order to replace the `table` with one containing a new set of rows.
It also returns new pagination buttons that replace the current ones
using the `hx-swap-oob` attribute.

```js
return (
  <>
    <table id="pokemon-table">
      <tr>
        <td>ID</td>
        <td>Name</td>
        <td>Description</td>
      </tr>
      {pokemonList.map((pokemon, index) => {
        const isLast = index === ROWS_PER_PAGE - 1;
        return TableRow(pageNumber, pokemon, isLast);
      })}
    </table>

    {/* The hx-indicator and hx-target attributes are
        inherited by the buttons inside this span. */}
    <span
      id="pagination-buttons"
      hx-swap-oob="true"
      hx-indicator=".htmx-indicator"
      hx-target="#pokemon-table"
    >
      <button
        disabled={pageNumber === 1}
        hx-get={`/pokemon-rows?page=${pageNumber - 1}`}
      >
        Previous
      </button>
      <button hx-get={`/pokemon-rows?page=${pageNumber + 1}`}>Next</button>
    </span>
  </>
);
```

<img alt="htmx Pagination" style="width: 30%"
  src="/blog/assets/htmx-pagination.png?v={{pkg.version}}">

### Infinite Scroll

See the working example project at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/infinite-scroll",
"infinite-scroll" %}.

This example renders the following HTML which includes a `table`
and a loading spinner that is only displayed when waiting on an HTTP response.
It includes `hx-trigger="load"` in order to immediately
send a request to get the first "page" of rows.

```html
<table
  hx-trigger="load"
  hx-get="/pokemon-rows?page=1"
  hx-indicator=".htmx-indicator"
  hx-swap="beforeend"
>
  <tr>
    <td>ID</td>
    <td>Name</td>
    <td>Description</td>
  </tr>
</table>
<img alt="loading..." class="htmx-indicator" src="/spinner.gif" />
```

The "pokemon-rows" endpoint returns the following JSX
in order to append the next set of rows to the `table`.

```js
return (
  <>
    {pokemonList.map((pokemon, index) => {
      const isLast = index === ROWS_PER_PAGE - 1;
      return TableRow(pageNumber, pokemon, isLast);
    })}
  </>
);
```

The following code is definition of the `TableRow` function
which returns a single table row.
This follows the convention where functions that return JSX
should be treated as components and have names begin uppercase.
The `isLast` parameter is used to determine whether `hx-*` attributes
will be included in the `tr` element.
The `hx-trigger` attribute causes a request for the next "page" of rows
to be sent whenever the `tr` becomes visible (`revealed`).

```js
function TableRow(page: number, pokemon: Pokemon, isLast: boolean) {
  const attributes = isLast
    ? {
        'hx-trigger': 'revealed',
        'hx-get': '/pokemon-rows?page=' + (page + 1),
        'hx-indicator': '.htmx-indicator',
        'hx-swap': 'afterend'
      }
    : {};
  const {name, url} = pokemon;
  const id = url.split('/')[6]; // 7th part of the URL
  const imageUrl = `https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/${id}.png`;

  return (
    <tr {...attributes}>
      <td>{id}</td>
      <td>{name}</td>
      <td>
        <img alt={name} src={imageUrl} />
      </td>
    </tr>
  );
}
```

<img alt="htmx Infinite Scroll" style="width: 30%"
  src="/blog/assets/htmx-infinite-scroll.png?v={{pkg.version}}">

### Polling

Polling is a way to repeatedly update the UI using server data.
Two polling approaches supported by htmx are
"fixed rate polling" and "load polling".

#### Fixed Rate Polling

Fixed rate polling sends requests at regular intervals.

See the working example of fixed rate polling at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/fixed-rate-polling",
"fixed-rate-polling" %}.

This example renders the following HTML
which reports the current score of an NFL game.
Score updates are randomly generated every five seconds.

An endpoint can terminate fixed rate polling by
returning a status code of 286 which is not a standard code.
This is done when the score of either team exceeds 30.

<img alt="htmx Fixed Rate Polling" style="width: 30%"
  src="/blog/assets/htmx-fixed-rate-polling.png?v={{pkg.version}}">

```js
app.get('/', () => {
  return (
    <BaseHtml>
      <h1>Polling</h1>
      {/* Send a request to the /score endpoint
          immediately and every five seconds thereafter. */}
      <h2 hx-get="/score" hx-trigger="load, every 5s" />
    </BaseHtml>
  );
});

let chiefsHaveBall = true;
let bills = 0;
let chiefs = 0;

// Randomly get points for a touchdown, field goal, or nothing.
function getPoints() {
  const number = Math.floor(Math.random() * 10);
  const touchdown = 7;
  const fieldGoal = 3;
  return number >= 8 ? touchdown : number >= 5 ? fieldGoal : 0;
}

app.get('/score', async () => {
  if (chiefsHaveBall) {
    chiefs += getPoints();
  } else {
    bills += getPoints();
  }
  chiefsHaveBall = !chiefsHaveBall;
  // Returning a status of 286 terminates fixed rate polling.
  c.status(chiefs > 30 || bills > 30 ? 286 : 200);
  return `Chiefs: ${chiefs}, Bills: ${bills}`;
});
```

#### Load Polling

Load polling sends a single request and then sends another
after the HTML response from the previous request is loaded.

See the working example of load polling at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/progress-bar",
"progress-bar" %}.
This also demonstrates repeatedly rendering an element with a fixed `id` value
and achieving smooth animation using a CSS transition.

<img alt="htmx Load Polling" style="width: 100%"
  src="/blog/assets/htmx-load-polling.png?v={{pkg.version}}">

The following HTML renders a progress bar.

```js
function ProgressBar() {
  // The HTML progress element cannot be animated.
  return (
    <div
      id="progress-container"
      hx-get="/progress"
      hx-swap="outerHTML"
      hx-trigger={percentComplete < 100 ? 'load delay:1s' : ''}
      role="progressbar"
      aria-valuenow={percentComplete}
    >
      <div id="progress-text">{percentComplete.toFixed(1)}%</div>
      {/* This div MUST have an id in order for the transition to work! */}
      <div id="progress-bar" style={`width: ${percentComplete}%`} />
    </div>
  );
}
```

The following endpoint renders the initial progress bar
and button to reset it.

The `/progress` endpoint can be triggered in two ways,
from the `div` with `id="progress-container"` above
or from the `button` with `id="reset-btn"` below.
The `div` above only triggers the endpoint
if `percentComplete` has not yet reached 100.
If that is the case, it waits one second before triggering it.

```js
app.get('/', () => {
  return (
    <BaseHtml>
      <h1>Progress Bar</h1>
      <ProgressBar />
      <button
        id="reset-btn"
        hx-get="/progress"
        hx-swap="outerHTML"
        hx-target="#progress-container"
      >
        Reset
      </button>
    </BaseHtml>
  );
});
```

The value of the `hx-trigger` HTTP request header is the value of
the `id` attribute of the element that triggered the request.
The following endpoint updates the progress bar.
It checks the `hx-trigger` request header to
determine if the endpoint was triggered by the Reset button.

```js
app.get('/progress', ({headers}) => {
  if (headers['hx-trigger'] === 'reset-btn') {
    percentComplete = 0;
  } else {
    const delta = Math.random() * 30;
    percentComplete = Math.min(100, percentComplete + delta);
  }
  return <ProgressBar />;
});
```

The following CSS styles the parts of the progress bar and
defines a `linear` `transition` for the
`width` of the filled portion of the progress bar.

```css
#progress-container {
  background-color: lightgray;
  border: 2px solid black;
  height: 2rem;
  width: 50rem;
  position: relative;
}

/* This centers the value text on the progress bar. */
#progress-text {
  color: black;
  position: absolute;
  left: 50%;
  top: 50%;
  transform: translate(-50%, -50%);
}

#progress-bar {
  background-color: green;
  height: 100%;
  width: 0%;
  transition: width 1s linear;
}

#reset-btn {
  margin-top: 1rem;
}
```

### Adding Header to All Requests

In some cases it is desirable to add a specific HTTP header
to all requests send by htmx.
For example, an authentication token can be passed in a header named "x-token".
This can be done by adding an event listener
for the `htmx:configRequest` event as follows.

```js
document.body.addEventListener('htmx:configRequest', event => {
  event.detail.headers['x-token'] = 'some-token';
});
```

In addition, request parameters can be added
by adding properties to the `event.detail.parameters` object.
Parameters can also be removed or modified, but that seems less useful.
The values of `event.detail.target` and `event.detail.verb`
can also be modified, but both seem like odd things to do.

See the working example at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/token-header",
"token-header" %}.

## HTTP Request Headers

HTTP header names are case-insensitive.

Htmx automatically adds following request headers
to all HTTP requests it sends if they are applicable.
Code that handles requests can use the values of these headers
to determine the appropriate response.

| Header                       | Description                                                                     |
| ---------------------------- | ------------------------------------------------------------------------------- |
| `hx-boosted`                 | "true" if the element making request has `hx-boost` (see [Boosting](#boosting)) |
| `hx-current-url`             | current URL in the browser location bar                                         |
| `hx-history-restore-request` | "true" if the request is for history restoration after a cache miss             |
| `hx-prompt`                  | value user entered in prompt dialog rendered by the `hx-prompt` attribute       |
| `hx-request`                 | always "true"; identifies requests that come from htmx                          |
| `hx-target`                  | value of the "id" attribute of the target element, if an id selector was used   |
| `hx-trigger`                 | value of the "id" attribute of the triggered element, if specified              |
| `hx-trigger-name`            | value of the "name" attribute of the triggered event, if specified              |

The `hx-current-url`, `hx-target`, `hx-trigger`, and `hx-trigger-name` headers
can all be used in an endpoint to determine
the appropriate HTML to send in the response.

From the htmx documentation on
<a href="https://htmx.org/docs/#caching" target="_blank">caching</a>,
"If your server can render different content for the same URL depending on
some other headers, you need to use the `Vary` response HTTP header.
For example, if your server renders the full HTML
when the `hx-request` header is missing or `false`,
and it renders a fragment of that HTML when `hx-request: true`,
you need to add `Vary: HX-Request`.
That causes the cache to be keyed based on a composite of
the response URL and the `hx-request` request header —
rather than being based just on the response URL."

## HTTP Response Headers

HTTP header names are case-insensitive.

An endpoint can cause a client-side redirection
by including the `hx-location` response header.

See the working example at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/redirection",
"redirection" %}.

The following endpoint receives a number in the `value` body property.
It doubles the value and returns it.
But if the result is more than 10, it includes the `hx-location` header
set to `/page2` which causes the browser to redirect to that page.

```js
app.post('/process', ({body, set}) => {
  const {value} = body;
  const doubled = value * 2;
  if (doubled > 10) set.headers['hx-location'] = '/page2';
  return doubled;
});
```

An endpoint can cause the URL in the browser location bar to change
by including the HTTP response header `hx-push-url`.

An endpoint can cause the browser to refresh the current page
by including the HTTP response header `hx-refresh`.

An endpoint can change the target element of its response
by including the HTTP response header `hx-retarget`.

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
"hx-validate" %} attribute causes an element to validate itself
using the HTML5 Validation API before a request is sent.
This occurs by default for `form` elements,
but not for `input`, `textarea`, and `select`elements.
When those elements have `hx-\*` attributes that send an HTTP request,
adding `hx-validate="true"` causes them to be validated
before the request is sent.

## Boosting

Anchor (`<a>`) and `form` elements that
do not have attributes like `hx-get` or `hx-post`
send HTTP requests to a given URL even when JavaScript is disabled.
When an anchor is clicked, a GET request is sent.
When a `form` is submitted, a GET or POST request
(depending on the `method` attribute value) is sent.

The {% aTargetBlank "https://htmx.org/attributes/hx-boost/", "hx-boost" %}
attribute can be added to any element.
This changes descendant anchor and `form` elements
that do not have attributes like `hx-get` or `hx-post`
to use AJAX for sending the HTTP request, but only if JavaScript is enabled.
For example, `hx-boost="true"` can be added to the `body` element
to "boost" all anchor and `form` elements.

The advantage boosting is that less processing will be done.
Rather than a full page refresh, the body content in the response
will replace the body content of the current page
and the `title` in the `head` tag will replace that in the current `head`.
This avoids processing `link` and `script` tags in the `head` tag
of the response which provides a significant performance advantage.
Of course this assumes that the current page has already
loaded all the CSS and JavaScript that is needed by the new content.
The result is faster navigation and a better user experience.

To disable boosting of specific anchors and forms
that are descendants of an element with `hx-boost="true"`,
add `hx-boost="false"` to them.

Note that many features of htmx will not work if JavaScript is disable.

## Other

- {% aTargetBlank "https://htmx.org/attributes/hx-disinherit/", "hx-disinherit" %}

- {% aTargetBlank "https://htmx.org/attributes/hx-encoding/", "hx-encoding" %}

- {% aTargetBlank "https://htmx.org/attributes/hx-vals/", "hx-vals" %}

  This specifies additional, static data to be passed in HTTP requests.
  The value is string representing a JSON object.
  For example: `hx-vals='{"planet": "Earth", "year": 2024}'`

  To evaluate JavaScript expressions in the JSON values,
  add the "js:" prefix to the JSON string.
  For example: `hx-vals='js:{"planet": "Earth", "year": new Date().getFullYear()}'`

  The values will be passed a query parameters for GET requests
  and in the request body for all other kinds of requests.

  See the working example project that uses `hx-vals` at {% aTargetBlank
  "https://github.com/mvolkmann/htmx-examples/tree/main/non-form-submit",
  "non-form-submit" %}.

- {% aTargetBlank "https://htmx.org/attributes/hx-vars/", "hx-vars" %}

## Debugging

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

A single interaction with an element that triggers an HTTP request
will cause a lot of debugging output, perhaps too much to be useful.
For example:

```text
htmx:xhr:progress ...
htmx:beforeOnLoad ...
htmx:beforeSwap ...
htmx:afterSwap ...
htmx:afterRequest ...
htmx:afterOnLoad ...
htmx:xhr:loadend ...
htmx:afterSettle ...
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

TODO: Describe hx-history-elt.

The `hx-push-url` attribute pushes a browser history entry for a page update.
The value of this attribute can be:

- `"true"` to create an entry without changing
  what appears in the browser navigation bar
- `"{some-relative-url}"` to create an entry AND
  change what appears in the browser navigation bar
  (ex. `hx-push-url="/todo/detail"`)
- `"false"` to avoid creating an entry when
  the value `"true"` would be inherited from an ancestor element

See the working example project that uses `hx-push-url` at {% aTargetBlank
"https://github.com/mvolkmann/htmx-examples/tree/main/non-form-submit",
"non-form-submit" %}.

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

- {% aTargetBlank "https://hotwired.dev", "Hotwire" %} competitor to htmx
- {% aTargetBlank "https://unpoly.com", "Unpoly" %} competitor to htmx

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

- patterns to document:
  - redirecting to another endpoint
  - input validation using an HTTP request (see email-validation app)
  - infinite scroll
  - type-ahead
