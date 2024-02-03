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
  and show a couple of examples.
  The first chapter will purposely avoid
  getting bogged down in terminology and theory.
- My plan for the second chapter is to cover terminology and the history of htmx.
  This will include describing the meaning of hypermedia, REST, and HATEOAS.

## Introducing htmx

Modern web development has become overly complicated.
Popular frameworks have somewhat steep learning curves and
they often perform more work than necessary to achieve a desired result.

I have first hand experience with many web development approaches including
vanilla JavaScript, jQuery, AngularJS, Angular, React, Vue, and Svelte.
For me, each of these provided improvements over what came before.
But these were incremental improvements.
I find htmx to be very different from these frameworks.
It is a breath of fresh air that I'm excited to share with you!

The goal of this chapter is to introduce you
to how htmx simplifies web development.
You will learn how htmx enables implementing web applications
that are easier to understand and require less code.
Development web applications using htmx
will increase your enjoyment and productivity.

Modern web frameworks for implementing single-page applications (SPAs)
frequently encourage the following steps:

- The browser downloads somewhat large amounts of JavaScript code.
- User interaction triggers sending an HTTP request to a server endpoint.
- The endpoint queries a database.
- Data from the database is converted to JSON.
- The endpoint returns a JSON response.
- JavaScript running in the browser parses the JSON into a JavaScript object.
- The framework generates HTML from the JavaScript object
  and inserts it into the DOM.

Htmx is a client-side JavaScript library that simplifies this process.
The name is short for "HyperText Markup Extensions".

With htmx, endpoints convert data to HTML (or plain text)
rather than JSON, and that is returned.
JavaScript in the browser no longer needs to
parse JSON and generate HTML from it.
It merely needs to insert the HTML into the DOM.
A full page refresh is not necessary.

The htmx library is quite small ... less than 17K.
App metrics such as "First Contentful Paint" and "Time to Interactive"
see improvements due to downloading less.
Htmx applications also provide faster interactions because
time spent generating and parsing JSON is eliminated.

The fact that htmx endpoints generate HTML,
means that htmx moves a large portion of web development
from the client to the server.
The server endpoints can be implemented in any programming language
that supports HTML templating and has an HTTP library.
Popular choices include JavaScript, Python, and Go.

Htmx keeps most of the application state on the server.
State that is only of concern to the user interface,
such as hiding and showing content, can remain on the client.
But client-only state is typically a small portion of the overall state.

## Required Knowledge

Now that you understand some of the benefits of using htmx,
lets's discuss what you need to know to use it.

It is useful to have some knowledge of the following topics:

- a code editor such as VS Code or Vim
- HTML for specifying what will be rendered in the browser
- CSS for styling what is rendered
- some programming language for implementing HTTP endpoints
- HTTP basics such as verbs, requests, and responses
- command-line basics such as changing the working directory
  and starting a local server

If you are not already a full-stack developer,
using htmx will provide motivation to move in that direction.
Front-end web developers will become comfortable
with implementing server endpoints.
Back-end developers will become comfortable with HTML and CSS.

## Choosing a Tech Stack

Before you can implement a web app using htmx,
you need to choose a tech stack.

The server side of htmx web applications can be implemented
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

This is the stack we will use for the examples in this book.
It has the following appealing characteristics.

- Most readers will be familiar with JavaScript syntax, and
  possibly also with TypeScript which adds types to JavaScript.
- Bun is an alternative to Node.js which provides better performance
  and adds features such as SQLite integration.
- Hono is a simple HTTP library with great performance
  that can be used with many JavaScript run-times including
  AWS Lambda, Bun, Cloudflare Workers, Deno, Netlify, Node.js, and Vercel.
  Elysia is another option to consider, but it only runs on Bun.
- No build step is required.

Other popular tech stacks for htmx include:

- {% aTargetBlank "https://ahastack.dev", "AHA stack" %}
  which uses Astro, htmx, and Alpine
- {% aTargetBlank "https://github.com/ethanniser/the-beth-stack",
  "BETH stack" %} which uses Bun, Elysia, Turso, and htmx.
- Go with {% aTargetBlank "https://templ.guide", "templ" %}
- Python with Flask (see the `render_template` function) or
  Django (see the `loader.get_template` and `template.render` methods)
- {% aTargetBlank "https://github.com/stolinski/hype", "Hype" %}
  which uses TypeScript, Bun, Elysia, SQLite, and Drizzle.

User interactions that do not require sending a request to a server
can be handled in the browser using plain JavaScript
or libraries like Alpine and \_hyperscript.
Those libraries have something in common with htmx ...
they are implemented as new HTML attributes.

## Using htmx Attributes

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

## Creating Your First Project

Now that we have selected a tech stack (TypeScript, Bun, and Hono)
you are ready to create your first project.
We will start simple to get a feel for using htmx.

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
1. cd to the new project directory which will contain the following:

   - `README.md` - contains instructions on running the project
   - `package.json` - describes project dependencies and
     defines a script for running the project
   - `tsconfig.json` - configures the use of TypeScript
   - `.gitignore` - prevents the node_modules directory from being committed
   - `src/index.ts` - implements a Hono HTTP server and
     defines the "GET /" endpoint

1. Enter `bun install`.

   This creates teh `node_modules` directory and
   installs all the required dependencies there.

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
       <button hx-get="/version" hx-target="#version">Get Bun Version</button>
       <div id="version"></div>
     </body>
   </html>
   ```

   The `hx-get` attribute specifies that
   when the `button` element is triggered (clicked)
   an HTTP GET request should be sent to the endpoint at `/version`.

   The `hx-target` attribute specifies that the text the endpoint returns
   should replace the `innerHTML` of the element with id "version".

   We are getting the htmx library from a CDN,
   but it can also be downloaded and accessed as part of the app.
   If the file `htmx.min.js` is placed in the `public` directory
   then the `script` tag above can be replaced by the following.

   ```html
   <script src="htmx.min.js"></script>
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

<img alt="htmx Demo Project" style="width: 30%"
  src="/blog/assets/htmx-demo-project.png?v={{pkg.version}}">

There you have it ... first project done!

Take a moment to consider how the same application
could be implemented in other web frameworks you have used.
What code would be required to send an HTTP request when a button is clicked
and insert the response into the current page?
What code would be required to implement the endpoint?

In the future when you want to create a new project
that uses Bun, Hono, and htmx, rather than repeating all the steps above
you can create a copy of this project and modify the code.

Note that servers for htmx applications play two roles.
First, they serves static files such as HTML, CSS, JavaScript, and images.
Second, they respond to certain HTTP requests by returning HTML or text.

## Creating a CRUD Application

Now that you understand the basics of creating an htmx-based web application,
you are ready to step it up a bit.

Let's create a project that performs the basic CRUD operations
Create, Retrieve (or Read), Update, and Delete.
Actually, we will hold off on the Update part for now
and address that later because there are a couple of ways
that functionality can be implemented.

This app can maintain a collection of any sort of data.
Let's maintain a list of dogs.
For each dog we will store their name and breed.

To keep things simple, the data will just be held in memory on the server.
Later we will see how Bun makes it very easy to interact with SQLite databases.
This can be used to persist the data so it is not lost when the server restarts.

The following screenshot shows what we want to build.

<img alt="htmx CRUD" style="width: 50%"
  src="/blog/assets/htmx-crud.png?v={{pkg.version}}">

To add a dog, enter their name and breed, then click the "Add" button.
To delete a dog, hover over its table row,
then click the white "X" that appears after the row.

Begin by copying the previous project.

Replace `public/index.html` with the following.

```js
<html>
  <head>
    <title>htmx Demo</title>
    <link rel="stylesheet" href="styles.css" />
    <script src="https://unpkg.com/htmx.org@1.9.10"></script>
  </head>
  <body>
    <h1>Dogs</h1>

    <form
      hx-disabled-elt="#add-btn"
      hx-post="/dog"
      hx-target="table tbody"
      hx-swap="afterbegin"
      hx-on:htmx:after-request="this.reset()"
    >
      <div>
        <label for="name">Name</label>
        <input name="name" required size="30" type="text" />
      </div>
      <div>
        <label for="breed">Breed</label>
        <input name="breed" required size="30" type="text" />
      </div>
      <button id="add-btn">Add</button>
    </form>

    <table hx-get="/dog" hx-target="tbody" hx-trigger="revealed">
      <thead>
        <tr>
          <th>Name</th>
          <th>Breed</th>
        </tr>
      </thead>
      <tbody></tbody>
    </table>
  </body>
</html>
```

What is the purpose of all those `hx-` attributes on the `form` element?

The `hx-disabled-elt` attribute disables the "Add button
while any request associated with the `form` is being processed.
In this case it applies to POST requests
that are sent when the `form` is submitted.
This prevents duplicate form submissions.

The `hx-post` attribute specifies that a POST request
should be sent to `/dog` when the form is submitted.
The request body will contain form data for the name and breed.
As we will see soon, the response will contain a new table row.

The `hx-target` attribute specifies that the returned HTML
should be placed relative to the `tbody` element inside the `table` element.

The `hx-swap` attribute specifies that the returned table row
should be inserted after the beginning of the target.
Since the target is the `tbody` element,
the new table row will be inserted before all the existing rows.

The `hx-on` attribute specifies that after the POST request is processed,
the `form` should be reset.
This clears the values of the name and breed inputs.

What is the purpose of all those `hx-` attributes on the `table` element?

The `hx-trigger` attribute specifies the event that triggers an HTTP request.
In this case it is triggered when the table comes into view.
For this app that happens immediately
since there isn't much content above the `table`.
But if there was more content above the table and
the user needed to scroll down to see it,
htmx would wait until the table is "revealed" to send the request.

The `hx-get` attribute specifies that a GET request should be sent to `/dog`.
As we will see soon, the response will contain
one table row for each dog that was previously added.

The `hx-target` attribute specifies that the returned table rows
should replace the contents of the `tbody` element.

Wow, we have defined a lot of client-side functionality
without writing ANY custom JavaScript code!

Now let's look at the server-side code that supports the HTTP requests.

Replace `src/server.tsx` with the following.

```ts
import {type Context, Hono} from 'hono';
import {serveStatic} from 'hono/bun';

type Dog = {id: string; name: string; breed: string};
const dogs = new Map<string, Dog>();

function addDog(name: string, breed: string): Dog {
  const id = crypto.randomUUID();
  const dog = {id, name, breed};
  dogs.set(id, dog);
  return dog;
}

// Some sample data so we don't start out empty.
addDog('Comet', 'Whippet');
addDog('Oscar', 'German Shorthaired Pointer');

const app = new Hono();
app.use('/*', serveStatic({root: './public'}));

function DogRow(dog: Dog) {
  return (
    <tr class="on-hover">
      <td>{dog.name}</td>
      <td>{dog.breed}</td>
      <td class="plain">
        <button
          class="show-on-hover"
          hx-delete={`/dog/${dog.id}`}
          hx-confirm="Are you sure?"
          hx-target="closest tr"
          hx-swap="outerHTML"
        >
          ✕
        </button>
      </td>
    </tr>
  );
}

app.get('/dog', async (c: Context) => {
  const sortedDogs = Array.from(dogs.values()).sort((a, b) =>
    a.name.localeCompare(b.name)
  );
  return c.html(<>{sortedDogs.map(DogRow)}</>);
});

app.post('/dog', async (c: Context) => {
  Bun.sleepSync(1000);
  const formData = await c.req.formData();
  const name = (formData.get('name') as string) || '';
  const breed = (formData.get('breed') as string) || '';
  const dog = addDog(name, breed);
  return c.html(DogRow(dog), 201);
});

app.delete('/dog/:id', async (c: Context) => {
  const id = c.req.param('id');
  dogs.delete(id);
  return c.html('');
});

export default app;
```

The function `DogRow` returns a table row for a given dog using JSX.

What is the purpose of all those `hx-` attributes on the `button` element
inside each table row?

The `hx-delete` attribute specifies that a DELETE request should be
sent to `/dog/{some-dog-id}` when the button is clicked.

The `hx-confirm` attribute specifies a prompt that will appear
in a confirmation dialog that user will see before the request is sent.
The dialog will contain "Cancel" and "OK" buttons.
The request will only be sent if the user clicks the OK button.
Later we will see how to replace this dialog with one that can be styled.

The `hx-target` attribute specifies that we want to target
the table row that contains this button with response (`closest tr').

The `hx-swap` attribute specifies that
we want to replace the target (`outerHTML`) with the response.
The response will have an empty body, so the table row will be removed.

The endpoint for "GET /dog" returns a bunch of table rows,
one for each dog, sorted on their names.

The endpoint for "POST /dog" adds a new dog and
a table row for the new dog.

The endpoint for "DELETE /dog" deletes the dog with a given id
and returns nothing.
This will result in the table row for the dog being deleted.

We won't examine the CSS for this project, but it can be found
<a href="https://github.com/mvolkmann/htmx-examples/blob/main/htmx-crud/public/styles.css" target="_blank">here</a>.

This application provides a surprising amount of functionality
given the small amount of code that was written to implement it.

Like before, take a moment to consider how the same application
could be implemented in other web frameworks you have used.
How much more verbose would the code be
for both the client-side and server-side?

You have now seen some of the most commonly used htmx attributes in action.
But there are many more that will be introduced in later chapters.

## Your Turn

Before moving on, try the following things to make sure you
understand how to implement and use HTTP endpoints with htmx.

1. In the first project, change the `/version` endpoint
   to return HTML instead of text.
   For example, `return c.html(<img alt="some description" src="some-image-url" />);`
1. Modify the CRUD application to manage a different kind of data.
   Perhaps instead of dogs it can maintain a list of favorite books.
1. Modify the CRUD application to persist the data to a file.
   On server startup, read the file into a string and
   use the `JSON.parse` function to
   convert the string to a collection of data.
   Every time the collection of data is modified,
   use the `JSON.stringify` function to turn the collection of data
   into a string and write that to the file.

## Wrapping Up

You have now built two web applications that use htmx.
The first was very basic, just to get your feet wet.
The second was more involved, supporting basic CRUD functionality
through the use of many htmx attributes.

Throughout the book we will implement an app called "List of Lists" (LOL).
The app allows users to create any number of lists
and add any number of items to each list.
Example lists include todos, desired vacation destinations,
favorite books, and shopping lists.
The items in each list can be
updated, reordered, marked as selected, and deleted.

In the next chapter we will uncover more about the motivations behind htmx.
We will discuss the meaning of "hypermedia" and "HATEOAS".
Then we will explore why web application architectures
commonly referred to as "REST" and not really
what Roy Fielding meant when he coined the term.
