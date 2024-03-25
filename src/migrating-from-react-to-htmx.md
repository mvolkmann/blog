---
eleventyNavigation:
  key: Migrating From React to htmx
layout: topic-layout.njk
---

## Overview

React is currently the most popular framework for building web applications.
Alternatives like Svelte, Vue, Angular differ from React in some ways,
but the fundamental approach used is similar.
They generally send HTTP requests to a server
that returns data in the form of JSON.
Once the JSON is returned to the browser,
it is parsed into a JavaScript object.
Then the web framework uses that data to update the DOM,
resulting in page updates without a full page refresh.

htmx is a relatively new JavaScript library
that takes a very different approach.
It still sends HTTP requests to a server,
but the server returns snippets of HTML instead of JSON.

htmx adds support for new HTML attributes
that make it more expressive and powerful.

- `hx-trigger` specifies what triggers a request, ex. click, submit, hover, ...
- `hx-get`, `hx-post`, `hx-put`, `hx-patch`, and `hx-delete` specify
  the HTTP verb to use and the URL path where the request will be sent.
- `hx-target` specifies where the resulting HTML should go.
- `hx-swap` specifies exactly how the result HTML should be inserted.

There are many frameworks that leverage React.
Next.js is one of the most popular.
Let's implement a web app using Next.js and then
implement the same app using htmx to see how they differ.

All the code can be found in the GitHub repository at
<a href="https://github.com/mvolkmann/nextjs-dogs-crud"
target="_blank">nextjs-dogs-crud</a>.

TODO: Add content from the "Example App" section of your Next.js blog page here.

## Next.js

There are many things to note about our Next.js app.

- State is being maintained on both the server and client,
  specifically the `dogMap`.
- A lot of custom client-side JavaScript code was written.
  This is all in the file `src/app/page.tsx`.

## htmx

Unlike Next.js, the htmx library does not
provide a framework for implementing API endpoints.
This is both good and bad.
The good side is that we can choose andy
programming language and server framework.
For example, we could use Go and the "Go Fiber" library.
Or we could use Python and the Fast API, Flask, or Django libraries.

Most web developers are already familiar with JavaScript,
so we will use that for our example app.
Specifically, we will use the Bun JavaScript engine
and the Hono server library.

"{% aTargetBlank "https://bun.sh", "Bun" %} is a fast JavaScript all-in-one toolkit."
It includes a JavaScript runtime, package manager, bundler, and test runner.
All of this is free and open source under the MIT license.

Bun can be used as a drop-in replacement for npm and Node.js.
The Bun runtime supports nearly all Node.js built-in modules (around 40 of them).
It also supports JSX which we will utilize
in our API endpoints to generate HTML.

{% aTargetBlank "https://hono.dev/", "Hono" %}
is a JavaScript HTTP server library that runs in any JavaScript runtime.

All the code for this app can be found in the GitHub repository at
<a href="https://github.com/mvolkmann/htmx-examples/tree/main/htmx-dogs-crud"
target="_blank">htmx-dogs-crud</a>.

The following steps create this app from scratch.

1. Install Bun.

   Bun does not currently support Windows outside of WSL,
   but will release a new version that supports windows very soon.

   Enter `curl -fsSL https://bun.sh/install | bash`

1. Create a new Bun project.

   - Create a directory to hold the new project.
   - `cd` to the new directory.
   - Enter `bun init` accept all the defaults for the prompts.

1. `npm install hono`

1. Add the following line in `tsconfig.json` inside the "compilerOptions":

   ```json
   "jsxImportSource": "hono/jsx",
   ```

1. Add the following in `package.json`:

   ```json
   "scripts": {
     "dev": "bun run --watch server.tsx"
   },
   ```

1. Delete the `index.ts` file.

1. Create the file `server.tsx` with the following contents:

   ```ts
   import {type Context, Hono} from 'hono';
   import {serveStatic} from 'hono/bun';

   type Dog = {id: string; name: string; breed: string};

   let selectedId = '';

   const dogMap = new Map<string, Dog>();

   addDog('Comet', 'Whippet');
   addDog('Oscar', 'German Shorthaired Pointer');

   function addDog(name: string, breed: string): Dog {
     const id = crypto.randomUUID(); // standard web API
     const dog = {id, name, breed};
     dogMap.set(id, dog);
     return dog;
   }

   function dogRow(dog: Dog, updating = false) {
     const attrs: {[key: string]: string} = {};
     if (updating) attrs['hx-swap-oob'] = 'true';
     return (
       <tr class="on-hover" id={`row-${dog.id}`} {...attrs}>
         <td>{dog.name}</td>
         <td>{dog.breed}</td>
         <td class="buttons">
           <button
             class="show-on-hover"
             hx-confirm="Are you sure?"
             hx-delete={`/dog/${dog.id}`}
             hx-target="closest tr"
             hx-swap="delete"
             type="button"
           >
             ✕
           </button>
           {/* This selects the dog which triggers a selection-change event,
                    which causes the form to update. */}
           <button
             class="show-on-hover"
             hx-get={'/select/' + dog.id}
             hx-swap="none"
             type="button"
           >
             ✎
           </button>
         </td>
       </tr>
     );
   }

   const app = new Hono();

   // Serve static files from the public directory.
   app.use('/*', serveStatic({root: './public'}));

   // Deletes the dog with a given id.
   app.delete('/dog/:id', (c: Context) => {
     const id = c.req.param('id');
     dogMap.delete(id);
     return c.body(null);
   });

   // Deselects the currently selected dog.
   app.get('/deselect', (c: Context) => {
     selectedId = '';
     c.header('HX-Trigger', 'selection-change');
     return c.body(null);
   });

   // Gets the proper form for either adding or updating a dog.
   app.get('/form', (c: Context) => {
     const selectedDog = dogMap.get(selectedId);

     const attrs: {[key: string]: string} = {
       'hx-on:htmx:after-request': 'this.reset()'
     };
     if (selectedId) {
       // Update an existing row.
       attrs['hx-put'] = '/dog/' + selectedId;
     } else {
       // Add a new row.
       attrs['hx-post'] = '/dog';
       attrs['hx-target'] = 'tbody';
       attrs['hx-swap'] = 'afterbegin';
     }

     return c.html(
       <form hx-disabled-elt="#submit-btn" {...attrs}>
         <div>
           <label for="name">Name</label>
           <input
             id="name"
             name="name"
             required
             size={30}
             type="text"
             value={selectedDog?.name ?? ''}
           />
         </div>
         <div>
           <label for="breed">Breed</label>
           <input
             id="breed"
             name="breed"
             required
             size={30}
             type="text"
             value={selectedDog?.breed ?? ''}
           />
         </div>
         <div class="buttons">
           <button id="submit-btn">{selectedId ? 'Update' : 'Add'}</button>
           {selectedId && (
             <button hx-get="/deselect" hx-swap="none" type="button">
               Cancel
             </button>
           )}
         </div>
       </form>
     );
   });

   // Selects a dog.
   app.get('/select/:id', (c: Context) => {
     selectedId = c.req.param('id');
     c.header('HX-Trigger', 'selection-change');
     return c.body(null);
   });

   // Gets table rows for all the dogs.
   app.get('/table-rows', (c: Context) => {
     const dogs = Array.from(dogMap.values());
     dogs.sort((a: Dog, b: Dog) => a.name.localeCompare(b.name));
     return c.html(<>{dogs.map(dog => dogRow(dog))}</>);
   });

   // Creates a dog.
   app.post('/dog', async (c: Context) => {
     const formData = await c.req.formData();
     const name = (formData.get('name') as string) || '';
     const breed = (formData.get('breed') as string) || '';
     const dog = addDog(name, breed);
     return c.html(dogRow(dog), 201);
   });

   // Updates a dog
   app.put('/dog/:id', async (c: Context) => {
     const id = c.req.param('id');
     const formData = await c.req.formData();
     const name = (formData.get('name') as string) || '';
     const breed = (formData.get('breed') as string) || '';
     const updatedDog = {id, name, breed};
     dogMap.set(id, updatedDog);

     selectedId = '';
     c.header('HX-Trigger', 'selection-change');
     return c.html(dogRow(updatedDog, true));
   });

   export default app;
   ```

1. Create the file `public/styles.css` containing the following
   which is identical to the styles for the Next.js version of the app:

   ```css
   body {
     background-color: cornflowerblue;
     font-family: sans-serif;
   }

   button {
     background-color: lightgreen;
     border: none;
     border-radius: 0.5rem;
     margin-bottom: 1rem;
     padding: 0.5rem;

     &:disabled {
       background-color: gray;
     }
   }

   .buttons {
     display: flex;
     gap: 1rem;

     background-color: transparent;
   }

   h1 {
     color: orange;
   }

   input {
     background-color: white;
     border: none;
     border-radius: 0.5rem;
     margin-bottom: 1rem;
     padding: 0.5rem;
   }

   label {
     display: inline-block;
     font-weight: bold;
     margin-right: 0.5rem;
     text-align: right;
     width: 3rem;
   }

   .show-on-hover {
     transform: scale(2.5) translate(0.2rem, 0.2rem);
     visibility: hidden;
   }

   .on-hover:hover .show-on-hover {
     visibility: visible;
   }

   table {
     border-collapse: collapse;
     margin-bottom: 0.5rem;
   }

   td,
   th {
     border: 1px solid cornflowerblue;
     padding: 0.5rem;
   }

   td {
     background-color: white;

     & button {
       background-color: transparent;
       color: white;
     }
   }

   th {
     background-color: orange;
   }
   ```

1. Create the file `public/index.html` containing the following:

   ```html
   <html>
     <head>
       <title>htmx CRUD</title>
       <link rel="stylesheet" href="styles.css" />
       <script src="https://unpkg.com/htmx.org@1.9.10"></script>
     </head>
     <body>
       <h1>Dogs</h1>
       <div hx-trigger="load, selection-change from:body" hx-get="/form"></div>
       <table hx-get="/table-rows" hx-target="tbody" hx-trigger="load">
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

1. Start the server by entering `bun dev`.

1. Browse localhost:3000.

1. Add a dog.

   Enter a name and breed in the form at the top. Click the "Add" button to add a new dog.

1. Edit a dog.

   Hover over one of the dog rows and click the pencil icon that appears.
   Modify the name and/or breed in the form at the top.
   Click the "Update" button to submit the changes.

1. Delete a dog.

   Hover over one of the dog rows and click the "X" icon that appears.
   Click the "OK" button in the confirmation dialog.

What conclusions can be drawn from comparing this version of the app
to the one implemented with Next.js?

- There is no custom client-side JavaScript code in the htmx version.
- The htmx library is far smaller than the client-side parts of Next.js.
- All the state is on the server.

It's difficult to compare the required learning curves
if you are already familiar with React.
But try to imagine what the difference would be
if you did not already have that background.
