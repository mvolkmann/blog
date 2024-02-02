---
eleventyNavigation:
  key: ZZ
layout: topic-layout.njk
---

Reviewer Notes:

- My plan for the first chapter is quickly motivate the need for htmx
  and show a couple of examples.
  The first chapter will purposely avoid
  getting bogged down in terminology and theory.
- My plan for the second chapter is to cover terminology
  and the history of htmx in more detail.
  This will include describing the meaning of hypermedia, REST, and HATEOAS.

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
The name is short for "HyperText Markup Extensions".

In the htmx approach, endpoints convert data to HTML
rather than JSON, and that is returned.
JavaScript in the browser no longer needs to
parse JSON and generate HTML from it.
It merely needs to insert the HTML into the DOM.
A full page refresh is not necessary.

The htmx library is quite small ... less than 17K.
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

These attributes answer the following questions:

- What events trigger a request? A mouse click, a form submission, other events?

  The `hx-trigger` attribute answers this question.

- What kind of request should be sent: GET, POST, PUT, PATCH or DELETE?
  And where should the request be sent?

  The `hx-get`, `hx-post`, `hx-put`, `hx-patch`, and `hx-delete` attributes
  answer both of these questions.

- When the endpoint returns HTML, what element should receive it?

  The `hx-target` attribute answers this question.

- How should the new HTML be placed relative to the target element?

  The `hx-swap` attribute answers this question.

Htmx keeps most of the application state on the server.
State that is only of concern to the user interface,
such as hiding and showing content, can remain on the client.
But client-only state is typically a small portion of the overall state.

User interactions that do not require server interactions
can be handled in the client using plain JavaScript
or libraries like Alpine and \_hyperscript.
Those libraries have something in common with htmx ...
they are implemented as new HTML attributes.
