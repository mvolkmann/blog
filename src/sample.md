---
eleventyNavigation:
  key: ZZ
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

## Reviewer Notes

- My plan for the first chapter is quickly motivate the need for htmx
  and show an example.
  The first chapter will purposely avoid
  getting bogged down in terminology and theory.
- My plan for the second chapter is to cover terminology and the history of htmx.
  This will include describing the meaning of hypermedia, REST, and HATEOAS.

## Introduction

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
- HTML is generated from the JavaScript object and inserted into the DOM.

Htmx is a client-side JavaScript library that simplifies this process.
The name is short for "HyperText Markup Extensions".

In the htmx approach, endpoints convert data to HTML (or plain text)
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

## htmx Attributes

Htmx provides a new set of HTML attributes that make HTML more expressive.
These attributes enable sending HTTP requests to endpoints
and inserting the resulting HTML into the DOM.
Any event on any HTML element can trigger any kind of HTTP request
(GET, POST, PUT, PATCH, or DELETE) and
the response does not result in a full page refresh.
All this is done without writing any custom client-side JavaScript code.

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

  <img alt="hx-swap" style="width: 80%"
    src="/blog/assets/htmx-hx-swap.png?v={{pkg.version}}">

## Application State

Htmx keeps most of the application state on the server.
State that is only of concern to the user interface,
such as hiding and showing content, can remain on the client.
But client-only state is typically a small portion of the overall state.

## Client-side Interactivity

User interactions that do not require server interactions
can be handled in the client using plain JavaScript
or libraries like Alpine and \_hyperscript.
Those libraries have something in common with htmx ...
they are implemented as new HTML attributes.

## Tech Stack Options

The server side of htmx web apps can be implemented
with any programming language and HTTP server library.
This is referred to as "Hypermedia On Whatever you'd Like" (HOWL).

Some options are better than others.
Good choices make it easy to do the following.

- Create new endpoints for any HTTP verb.

  One of the primary activities when using htmx is defining endpoints.
  It's best when an endpoint can be described in a single source file,
  rather than requiring editing multiple files.
  It's also convenient when multiple, related endpoints
  can be defined in the same source file.

- Specify type checking and validation of request data
  and receive helpful error messages in responses.

  Request data includes request headers, path parameters, query parameters,
  and request bodies that can contain text, form data, JSON.
  An example is the TypeScript library
  {% aTargetBlank "https://zod.dev", "Zod" %}.

- Get request data in the form of request headers, path parameters,
  query parameters, and request bodies that can contain text, form data, JSON.

- Send HTTP responses that can include headers
  and bodies that contain text or HTML.

Good choices have tooling that supports the following:

- Fast server startup with no build process (or a simple one)
  to support iterative development.
- Automatic server restarts after source code changes are detected.
- Good HTML templating support such as JSX,
  rather than relying on string concatenation.
  JSX is an XML-based syntax popularized by the React framework
  for embedding HTML-like syntax directly in JavaScript code.
- Syntax highlighting of HTML in code editors.

One tech stack that meets all these criteria includes:

- <a href="/blog/topics/#/blog/typescript/" target="_blank">TypeScript</a>
  as the programming language.
- <a href="/blog/topics/#/blog/bun/" target="_blank">Bun</a>
  as a JavaScript runtime and package manager.
- <a href="/blog/topics/#/blog/hono/" target="_blank">Hono</a>
  as the HTTP library.

A notable feature of this tech stack is that no build step is required.

Other popular tech stacks for htmx include:

- {% aTargetBlank "https://ahastack.dev", "AHA stack" %}
  which uses Astro, htmx, and {% aTargetBlank
  "/blog/topics/#/blog/alpine/", "AlpineJS" %}
- {% aTargetBlank "https://github.com/ethanniser/the-beth-stack",
  "BETH stack" %} which uses Bun, Elysia, Turso, and htmx.
- Go with {% aTargetBlank "https://templ.guide", "templ" %}
- Python with Flask (see the `render_template` function) or
  Django (see the `loader.get_template` and `template.render` methods)
- {% aTargetBlank "https://github.com/stolinski/hype", "Hype" %}
  which uses TypeScript, Bun, Elysia, SQLite, and Drizzle.

## First Project

Let's create a basic htmx project get a feel for it.

First let's create a default project that uses Bun and Hono.

1. Open a terminal window.
1. Install Bun by entering the following command.

   ```bash
   curl -fsSL https://bun.sh/install | bash
   ```

1. cd to the directory where the project will be created.
1. Enter `bunx create-hono` to create the project.
1. After the "Target directory" prompt, enter a project name like "htmx-demo".
1. After the "Which template do you want to use?" prompt, select "bun".
1. cd to the new project directory.
1. Enter `bun install`.
1. Enter `bun run dev` to start a local server.
1. In a web browser, browse localhost:3000.
1. Verify that it renders "Hello Hono!".

Now that we have a default project, let's modify it to use htmx.

1. Rename the file `src/index.ts` to `src/server.tsx`.

   The `.tsx` file extension allows using JSX to generate HTML.

1. Modify the "dev" script in `package.json` to match the following:

   ```json
   "dev": "bun run --watch src/server.tsx"
   ```

   The `--watch` flag causes the Bun server to be restarted
   if any of the source files it is uses are modified.
   This does not include client-side files in the `public` directory.

1. Create the `public` directory.
1. Create the file `index.html` in the `public` directory
   with the following content.

   ```html
   <html>
     <head>
       <title>htmx Demo</title>
       <link rel="stylesheet" href="styles.css" />
       <script src="https://unpkg.com/htmx.org@1.9.10"></script>
     </head>
     <body>
       <!-- When this button is clicked,
            an HTTP GET request is sent to /version.
            The text it returns replaces the innerHTML
            of the element with id "version". -->
       <button hx-get="/version" hx-target="#version">Get Bun Version</button>
       <div id="version"></div>
     </body>
   </html>
   ```

1. Create the file `styles.css` in the `public` directory
   with the following content.

   ```css
   body {
     font-family: sans-serif;
   }

   button {
     border-radius: 0.5rem;
     margin-bottom: 1rem;
     padding: 0.5rem;
   }
   ```

1. Replace the contents of `src/server.tsx` with the following:

   ```ts
   import {type Context, Hono} from 'hono';
   import {serveStatic} from 'hono/bun';

   const app = new Hono();

   // Serve static files from the public directory.
   app.use('/*', serveStatic({root: './public'}));

   app.get('/version', async (c: Context) => {
     // Return a Response whose body contains
     // the version of Bun running on the server.
     return c.text(Bun.version);
   });

   export default app;
   ```

1. Browse localhost:3000 again.
1. Click the "Get Bun Version" button.
1. Verify that a version number is displayed below the button.

There you have it ... first project done!
Take a moment to consider how the same project could be implemented
in other web frameworks you have used recently.
What code would be required to send an HTTP request when a button is clicked
and insert the response into the current page?
What code would be required to implement the endpoint?

In the future when you want to create a new project
that uses Bun, Hono, and htmx, you can create a copy of this project
rather than repeating all the steps above.

Note that servers for htmx applications plays two roles.
First, they serves static files such as HTML, CSS, JavaScript, and images.
Second, they respond to certain HTTP requests by returning HTML or text.

## Simple CRUD App

Let's step it up a bit and create a project that performs the
basic CRUD operations Create, Retrieve (or Read), Update, and Delete.
Actually, we will hold off on the Update part for now
and address that later because there are a couple of ways
that functionality can be implemented.

This app can maintain a collection of any sort of data.
Let's maintain a list of dogs.
For each dog we will store their name and breed.
To keep things simple, the data will just be held in memory on the server.
Later we will see that Bun makes it very easy to interact with SQLite databases.
This can be used to persist the data so it is not lost when the server restarts.

Begin by copying the previous project.

## List of Lists Project

Throughout the book we will implement an app called "List of Lists" (LOL).
The app allows users to create any number of lists
and add any number of items to each list.
Example lists include todos, desired vacation destinations,
favorite books, and shopping lists.
The items in each list can be
updated, reordered, marked as selected, and deleted.

## Your Turn

Before moving on, try the following things to make sure you
understand how to implement and use HTTP endpoints with htmx.

1. Change the `/version` endpoint to return HTML instead of text.
   For example, `return c.html(<img alt="some description" src="some-image-url" />);`
1. TODO: Add another exercise.
1. TODO: Add another exercise.

## Wrapping Up

TODO: Add this section.
