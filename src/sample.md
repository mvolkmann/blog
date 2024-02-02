---
eleventyNavigation:
  key: ZZ
layout: topic-layout.njk
---

Modern web development has become overly complicated and
often performs more work than necessary to achieve a desired result.
For example, modern web frameworks frequently encourage
the following set of steps:

- The browser downloads somewhat large amounts of JavaScript code.
- User interaction triggers sending an HTTP request to a server endpoint.
- The endpoint queries a database.
- Data from the database is converted to JSON.
- The endpoint returns a JSON response.
- JavaScript running in the browser parses the JSON into a JavaScript object.
- HTML is generates from the JavaScript object and inserted into the DOM.

Htmx is a client-side JavaScript library that simplifies this process.
The steps above can be reduced to the following:

- The browser downloads a very small amount of JavaScript code.
- User interaction triggers sending an HTTP request to a server endpoint.
- The endpoint queries a database.
- Data from the database is converted to HTML.
- The endpoint returns a HTML response.
- JavaScript running in the browser inserts the HTML into the DOM
  without performing a full page refresh.

This results in faster app startup due to downloading less.
It also results in faster interactions because time spent
generating and parsing JSON is eliminated.

The fact that htmx endpoints generate HTML,
means that htmx moves a large portion of web development
from the client to the server.
The server endpoints can be implemented in any programming language
that supports HTML templating and has an HTTP library.
Popular choices include JavaScript, Python, and Go.

If you are not already a full-stack developer,
using htmx will provide motivation to move in that direction.
Front-end web developers need to get comfortable
with implementing server endpoints.
Back-end developers need to get comfortable with HTML and CSS.

Htmx provides a new set of HTML attributes that enable doing all of this
without writing any custom client-side JavaScript code.

Currently htmx defines 36 attributes,
but a small subset of them are commonly used.
By the end of this book we will have explored each of the new attributes,
but let's discuss the primary ones now.

- hx-trigger
- hx-get
- hx-post
- hx-put
- hx-patch
- hx-delete
- hx-target
- hx-swap
