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

htmx adds support for many new HTML attributes
that make it more expressive and powerful.
The most used of these new attributes include the following:

- `hx-trigger` specifies the events that triggers a request
  such as `click`, `submit`, and `hover`.
- `hx-get`, `hx-post`, `hx-put`, `hx-patch`, and `hx-delete` specify
  the HTTP verb to use for the request and the URL path where it will be sent.
- `hx-target` specifies where the resulting HTML should go.
- `hx-swap` specifies exactly how the resulting HTML should be inserted.

The following image describes commonly used values for the `hx-swap` attribute.

<img alt="hx-swap" style="width: 70%"
  src="/blog/assets/htmx-hx-swap.png?v={{pkg.version}}">

For more detail on htmx, see my
<a href="https://mvolkmann.github.io/blog/topics/#/blog/htmx/"
target="_blank">htmx blog page</a>.

There are many frameworks that leverage React.
Next.js is one of the most popular.
For detail on Next.js, see my
<a href="https://mvolkmann.github.io/blog/topics/#/blog/next-js/"
target="_blank">Next.js blog page</a>.

Let's implement a web app using Next.js and then
implement the same app using htmx to see how they differ.

The app manages a collection of dogs.

<img alt="Dog CRUD app" style="width: 50%"
  src="/blog/assets/htmx-dog-crud.png?v={{pkg.version}}">

Users can add new dogs.
They can also hover over of table row for an existing dog
to reveal buttons for deleting or editing it.

## Next.js

All the code for the Next.js version can be found in the GitHub repository at
<a href="https://github.com/mvolkmann/nextjs-dogs-crud"
target="_blank">nextjs-dogs-crud</a>.

Follow these steps to create the app from scratch.

1. `npx create-next-app@latest`

   I chose the name "dogs-crud" and
   accepted all the defaults except using Tailwind.

1. `cd dogs-crud`
1. `npm run dev`
1. `npm install uuid` and `npm i --save-dev @types/uuid`

   This package will be used to generate unique ids for dogs.

1. Delete the following files that will not be used:

   - `public/next.svg`
   - `public/vercel.svg`
   - `src/app/page.module.css`

1. Replace the contents of `src/app/global.css` with the following:

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

1. Edit `src/app/layout.tsx`.

   - Delete the two lines that refer to the "Inter" font.
   - Change the title from "Create Next App" to "Dogs CRUD".
   - Remove the `className` attribute from the `body` element.

1. Create the file `src/app/api/dogs/dogs.ts` containing the following code
   that manages a collection of dogs in memory:

   ```ts
   import {v4 as uuidv4} from 'uuid';

   export type Dog = {
     id: string;
     name: string;
     breed: string;
   };

   const dogMap = new Map<string, Dog>();

   addDog('Comet', 'Whippet');
   addDog('Oscar', 'German Shorthaired Pointer');

   export function addDog(name: string, breed: string): Dog {
     const id = uuidv4();
     const dog = {id, name, breed};
     dogMap.set(id, dog);
     return dog;
   }

   export function deleteDog(id: string): boolean {
     return dogMap.delete(id);
   }

   export function getDogs(): Dog[] {
     const dogs = Array.from(dogMap.values());
     return dogs.sort((a: Dog, b: Dog) => a.name.localeCompare(b.name));
   }

   export function updateDog(
     id: string,
     name: string,
     breed: string
   ): Dog | undefined {
     const dog = dogMap.get(id);
     if (dog) {
       dog.name = name;
       dog.breed = breed;
     }
     return dog;
   }
   ```

1. Create the file `src/app/api/dogs/dogs.ts` containing the following code
   that manages a collection of dogs in memory:

1. Create the file `src/app/api/dogs/route.ts` containing the following code
   that handles GET and POST requests:

   ```ts
   import {NextResponse} from 'next/server';
   import {addDog, getDogs} from './dogs';

   export function GET(_: Request) {
     return NextResponse.json(getDogs());
   }

   export async function POST(req: Request) {
     try {
       const formData = await req.formData();
       const name = (formData.get('name') as string) || '';
       const breed = (formData.get('breed') as string) || '';
       const newDog = addDog(name, breed);
       return NextResponse.json(newDog);
     } catch (error) {
       return NextResponse.json({error}, {status: 500});
     }
   }
   ```

1. Create the file `src/app/api/dogs/[id]/route.ts` containing the following code
   that handles PUT and DELETE requests:

   ```ts
   import {NextResponse} from 'next/server';
   import {deleteDog, updateDog} from '../dogs';

   type Props = {
     params: {id: string};
   };

   export async function DELETE(req: Request, {params: {id}}: Props) {
     const existed = deleteDog(id);
     return NextResponse.json(
       {error: 'dog not found'},
       {status: existed ? 200 : 404}
     );
   }

   export async function PUT(req: Request, {params: {id}}: Props) {
     const formData = await req.formData();
     const name = formData.get('name') as string;
     const breed = formData.get('breed') as string;
     const dog = updateDog(id, name, breed);
     return dog
       ? NextResponse.json(dog)
       : NextResponse.json({error: 'dog not found'}, {status: 404});
   }
   ```

1. Replace the contents of `src/app/page.tsx` with the following:

   ```tsx
   'use client';
   import {useEffect, useState} from 'react';

   type Dog = {id: string; name: string; breed: string};

   const Home = () => {
     const [dogMap, setDogMap] = useState<Map<string, Dog>>(new Map());
     const [selectedDog, setSelectedDog] = useState<Dog | undefined>();

     useEffect(() => {
       loadDogs();
     }, []);

     function addDog(dog: Dog) {
       const newDogMap = new Map(dogMap);
       newDogMap.set(dog.id, dog);
       setDogMap(newDogMap);
     }

     function deleteDog(id: string) {
       const newDogMap = new Map(dogMap);
       newDogMap.delete(id);
       setDogMap(newDogMap);
     }

     async function handleDelete(event: React.MouseEvent<HTMLButtonElement>) {
       if (!confirm('Are you sure?')) return;

       const tr = event.currentTarget.closest('tr');
       if (!tr) throw new Error('tr not found');

       try {
         const res = await fetch(`/api/dogs/${tr.id}`, {
           method: 'DELETE'
         });
         if (!res.ok) throw new Error('DELETE failed');
         deleteDog(tr.id);
       } catch (error) {
         console.error('Error deleting dog:', error);
       }
     }

     function handleEdit(event: React.MouseEvent<HTMLButtonElement>) {
       const tr = event.currentTarget.closest('tr');
       if (!tr) throw new Error('tr not found');
       setSelectedDog(dogMap.get(tr.id));
     }

     async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
       event.preventDefault();
       const form = event.currentTarget;

       const url = selectedDog ? `/api/dogs/${selectedDog.id}` : '/api/dogs';
       try {
         const res = await fetch(url, {
           method: selectedDog ? 'PUT' : 'POST',
           body: new FormData(form)
         });
         if (!res.ok) throw new Error('POST failed');
         form.reset();
         const newDog = await res.json();
         addDog(newDog);
         setSelectedDog(undefined);
       } catch (error) {
         console.error('Error submitting dog:', error);
       }
     }

     async function loadDogs() {
       const res = await fetch('/api/dogs');
       const dogArray = await res.json();
       const dogMap = new Map<string, Dog>();
       for (const dog of dogArray) {
         dogMap.set(dog.id, dog);
       }
       setDogMap(dogMap);
     }

     return (
       <main>
         <h1>Dogs</h1>
         <form onSubmit={handleSubmit}>
           <div>
             <label htmlFor="name">Name</label>
             <input
               id="name"
               name="name"
               required
               size={30}
               type="text"
               defaultValue={selectedDog ? selectedDog.name : ''}
             />
           </div>
           <div>
             <label htmlFor="breed">Breed</label>
             <input
               id="breed"
               name="breed"
               required
               size={30}
               type="text"
               defaultValue={selectedDog ? selectedDog.breed : ''}
             />
           </div>

           <div className="buttons">
             <button id="submit-btn">{selectedDog ? 'Update' : 'Add'}</button>
             {selectedDog && (
               <button type="button" onClick={() => setSelectedDog(undefined)}>
                 Cancel
               </button>
             )}
           </div>
         </form>
         <table>
           <thead>
             <tr>
               <th>Name</th>
               <th>Breed</th>
             </tr>
           </thead>
           <tbody>
             {Array.from(dogMap.values()).map(dog => (
               <tr className="on-hover" id={dog.id} key={dog.id}>
                 <td>{dog.name}</td>
                 <td>{dog.breed}</td>
                 <td className="buttons">
                   <button
                     className="show-on-hover"
                     onClick={handleDelete}
                     type="button"
                   >
                     ✕
                   </button>
                   {/* This selects the dog which triggers a selection-change event
                which causes the form to update. */}
                   <button
                     className="show-on-hover"
                     onClick={handleEdit}
                     type="button"
                   >
                     ✎
                   </button>
                 </td>
               </tr>
             ))}
           </tbody>
         </table>
       </main>
     );
   };

   export default Home;
   ```

1. Browse localhost:3000.

1. Add a dog.

   Enter a name and breed in the form at the top.
   Click the "Add" button to add a new dog.

1. Edit a dog.

   Hover over one of the dog rows and click the pencil icon that appears.
   Modify the name and/or breed in the form at the top.
   Click the "Update" button to submit the changes.

1. Delete a dog.

   Hover over one of the dog rows and click the "X" icon that appears.
   Click the "OK" button in the confirmation dialog.

## htmx

Unlike Next.js, the htmx library does not
provide a framework for implementing API endpoints.
This is both good and bad.
The good side is that we can choose any
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

For more detail on Bun, see my
<a href="https://mvolkmann.github.io/blog/topics/#/blog/bun/"
target="_blank">bun blog page</a>.

{% aTargetBlank "https://hono.dev/", "Hono" %}
is a JavaScript HTTP server library that runs in any JavaScript runtime.
For more detail on Hono, see my
<a href="https://mvolkmann.github.io/blog/topics/#/blog/hono/"
target="_blank">Hono blog page</a>.

All the code for this app can be found in the GitHub repository at
<a href="https://github.com/mvolkmann/htmx-examples/tree/main/htmx-dogs-crud"
target="_blank">htmx-dogs-crud</a>.

Follow these steps to create the app from scratch.

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

1. Create the file `public/styles.css` containing the same CSS rules
   as found in the Next.js `src/app/global.css` file.

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

## Comparing

What conclusions can be drawn from comparing this version of the app
to the one implemented with Next.js?

- There is no custom client-side JavaScript code in the htmx version,
  but there is a lot of it in the Next.js version.
  Compare the Next.js file `src/app/page.tsx`
  to the `htmx` file `public/index.html`.

- The htmx library is far smaller than the client-side parts of Next.js.

  For the Next.js app, the Chrome DevTools Network tab shows:

  - `webpack.js` at 10.9 kB
  - `main-app.js` at 1.6 MB
  - `app-pages-internals.js` at 41.7 kB
  - `page.js` at 46.2 kB

  For the htmx app, the Chrome DevTools Network tab
  shows `htmx.min.js` at 22.3 kB.
  That's a mere .3% of the total JavaScript size for the Next.js version!

- All the state is only maintained on the server in the htmx version.
  In the Next.js version, `dogMap` is maintained on the server and the client.

- The learning curve for htmx is smaller than for Next.js and React.

  It's difficult to compare the required learning curves
  if you are already familiar with React.
  But try to imagine what the difference would be
  if you did not already have that background.

- The startup time for the Next.js server (almost 2 seconds) is
  noticeably slower than that of the htmx server which is instantaneous.
