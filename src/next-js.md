---
eleventyNavigation:
  key: Next.js
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://nextjs.org", "Next.js" %}
is a full-stack web framework built on React.

Next.js uses {% aTargetBlank "https://turbo.build/pack", "Turbopack" %}
rather than Vite as its module bundler.
Turbopack bills itself as the "Rust-powered successor to Webpack".
It claims to be 10 times faster than Vite and 700 times faster than Webpack.

Next.js supports the following features:

- server-side rendering (SSR)

  This improves first render efficiency
  and search engine optimization (SEO).
  JavaScript is hydrated in the client.

- static site generation (SSG) at build time and on request
- code splitting
- prefetching

  In dev mode this occurs when hovering over a link.
  In prod mode this occurs for all links on a page when the page is loaded.
  To run in prod mode, enter `npm run build` and `npm run start`.
  The output of `npm run build` will indicate
  which pages are static, SSR, or SSG.

- serverless functions

Next.js requires Node.js 16.8 or later.

## Getting Started

To create a new Next.js project, enter `npx create-next-app@latest`.
This will ask a series of questions and
produce output similar to what is shown below.

```text
npx create-next-app@latest
Need to install the following packages:
  create-next-app@13.4.13
Ok to proceed? (y)
✔ What is your project named? … todo
✔ Would you like to use TypeScript? … No / Yes
✔ Would you like to use ESLint? … No / Yes
✔ Would you like to use Tailwind CSS? … No / Yes
✔ Would you like to use `src/` directory? … No / Yes
✔ Would you like to use App Router? (recommended) … No / Yes
✔ Would you like to customize the default import alias? … No / Yes
Creating a new Next.js app in /Users/volkmannm/todo.

Using npm.

Initializing project with template: app-tw

Installing dependencies:
- react
- react-dom
- next
- typescript
- @types/react
- @types/node
- @types/react-dom
- tailwindcss
- postcss
- autoprefixer
- eslint
- eslint-config-next
```

Cd to the new project directory.
It will contain a `package.json` file that defines the following scripts:

```json
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "next lint"
```

To start a local web server for running the app, enter `npm run dev`.
Then browse `localhost:3000`.

See the provided `README.md` file in each project for more detail.

## VS Code Tips

The extension "ES7+ React/Redux/React-Native snippets" is quite useful.
For example, the snippet "tsrfc" inserts the boilerplate code for a
TypeScript React Functional Component which is the following
when the filename is `Foo.tsx`:

```js
import React from 'react';

type Props = {};

export default function Foo({}: Props) {
  return <div>Foo</div>;
}
```

When using Tailwind, consider installing the extensions
"Tailwind CSS IntelliSense" and "Tailwind Fold".

## Tailwind

To configure the use of Tailwind in a Next.js project, see {% aTargetBlank
"https://tailwindcss.com/docs/guides/nextjs",
"Install Tailwind CSS with Next.js" %}.
These steps should not be necessary if you opted into use of Tailwind
when the project was created.

## Modifying App

Begin by modifying `src/app/page.tsx`.

## Page Components

The "app" directory holds source files that define page components.
The directory structure maps directly to the URLs used to access the pages.

To add a new page, create a directory in `src/app`
whose name is the route name and
create the file `page.tsx` (or `page.js`) inside that directory.

While the file must have the name "page",
the name of the function that defines the page component
should reflect the route name.
For example, "TodoPage".
Adding the name suffix "Page" is often useful when TypeScript is being used
because there will likely be a custom type named "Page.

In React, unlike in Svelte, the JSX returned by a component can be split
into related pieces by writing functions that return snippets of JSX.
Often this makes the code easier to read and understand.

## Non-page Components

Components used by page components can be defined in two places.
Page-specific components can be defined in
the same directory as the page that uses them.
Components that can potentially be reused across multiple pages
should be placed in a top-level directory named "components".

## Static Resources

Static resources like images are typically placed in
a top-level "public" directory.
This directory can have subdirectories such as "images".

## Client vs. Server Components

Components that perform data fetching through HTTP requests
are considered to be "server" components.
Functions that define server components are typically `async`.
By default, components are server components.

Components that contain client-side logic like DOM event handling
are considered to be "client" components.
Functions that define client components cannot be `async`.
Their source files must begin with `'use client';`.

Client components cannot send HTTP requested and
server components cannot perform DOM event handling.
Sometimes both are needed.
A solution is to use client components inside server components.

## CSS

One way to use vanilla CSS is to
create a `.css` file for each component source file.
For example, `Foo.tsx` can have a corresponding `Foo.css` file.

To attempt to scope CSS rules to the component:

1. Add a `className` to the component root element that
   matches its name, but in lowercase. For example, `foo`.
1. Begin the selector of all rules in the `.css` file with that class name.
   For example:

   ```css
   .foo button {
     border-color: red;
     border-width: 1px;
   }
   .foo span {
     font-weight: bold;
   }
   ```

Newer browser versions can use {% aTargetBlank
"https://caniuse.com/css-nesting", "CSS Nesting" %} to
avoid having to repeat the root element CSS class name.
For example:

```css
.dog-component {
  > button {
    border-color: red;
    border-width: 1px;
  }
  > span {
    font-weight: bold;
  }
}
```

To enable using CSS Nesting in conjunction with Tailwind:

1. Enter `npm install -D postcss-nesting`
1. Edit `postcss.config.js`
1. Add the following line in the `plugins` object:

```js
'tailwindcss/nesting': 'postcss-nesting',
```

## Event Handling

To determine the TypeScript type of an event
that is passed to an event handling function in VSCode,
temporarily change the value of the event handling prop
to be an anonymous function whose parameter is `e`
and hover over it.
The type will be displayed in a popup and can be copied.

For example, temporarily change `<button onClick={handleClick}`
to `<button onClick={e => handleClick(e)}`.

## Google Fonts

To use a Google Font, import it as follows:

```js
import {Borel} from 'next/font/google';
const font = Borel({
  // This allows a fallback font to be used until the
  // requested font has been loaded and can be swapped in.
  display: 'swap',
  subsets: ['latin'],
  // "Variable fonts" do not need to specify weights.
  weight: '400' // can be an array of weight strings
});
```

Specify the font on the element that should use it
with `className={font.className}`.

## Fontawesome Icons

To use Fontawesome Icons:

1. Install with `npm install react-icons`
1. Import specific icons. For example, `import { FaYouTube, FaLaptop } from 'react-icons/fa';`
1. Use as React components. For example, `<FaLaptop />`

## Layouts

The file `app/layout.tsx` defines the layout for all pages in the app.
Additional `layout.tsx` files in page route directories define
the layout for all pages at specific URL routes.
These are nested inside the main layout.

## Data Fetching

Components can use the fetch API to send API requests.
These can fetch data that is then presented in a page.

The following code is an example of a function that fetches data:

```js
async function getTodos(): Promise<Todo[]> {
  const url = 'https://jsonplaceholder.typicode.com/todosx';
  const res = await fetch(url);

  // Throwing an error triggers the error boundary
  // defined in error.ts.
  if (!res.ok) throw new Error('Failed to fetch todos.');
  return res.json();
}
```

Functions like the one above can be defined
in the source files of components that use them.
They can also be defined in separate source files,
often in the top-level "lib" directory, and imported where needed.

A component can call a data fetching function as follows:

```js
const todos: Todo[] = await getTodos();
```

To fetch data from multiple sources in parallel,
call each of the data fetching functions without the `await` keyword
so `Promise` objects are obtained.
Then call `await Promise.all(promises)`
where `promises` is an array of the promises.

To allow data from the first fetch call to render
before data from the next fetch call is available, use React Suspense.
For example:

```js
const promise2 = getData2(...);
const data1 = await getData1(...);
return (
  <>
    <div>{data1.someProperty}</div>
    <Suspense fallback={<p>loading data2</p>}>
      {/* The Data2 component will render in place of the fallback content
          when promise2 resolves. */}
      <Data2 promise={promise2} />
    </Suspense>
  </>
);
```

## Data Caching

Sometimes it is convenient to call a data fetching function in multiple places.
This will not result in duplicate fetches.
Next.js will automatically cache the result of the first call
and use it for the result of subsequent calls.

To cause cached results to expire after a given number of seconds,
add an option to the `fetch` call. For example:

```js
const res = await fetch(url, {next: {revalidate: 60}});
```

To cause all `fetch` calls made from a given page to
expire after a given number of seconds, add the following:

```js
export const revalidate = 60;
```

To manually control caching, use React's `cache` function.
See {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/data-fetching/fetching-caching-and-revalidating#fetching-data-on-the-server-with-third-party-libraries",
"Fetching data on the Server with third-party libraries" %}.
This is especially useful when data is fetched
in a way that does not use the `fetch` function.

## Loading Pages

Page route directories that contain a `loading.tsx` file
will display that while data is being loaded.
These pages automatically use React Suspense under the hood
to allow other content to render while waiting for a Promise to resolve.

For example:

```js
export default function TodoLoading() {
  return (
    <section>
      <h2>The Todos page is loading.</h2>
    </section>
  );
}
```

When testing loading functionality,
it is useful to simulate long-running requests.
The following function simplifies this.
Just call `await sleep(1000);` before sending a fetch request.

```js
const sleep = async (ms: number) =>
  new Promise(resolve => setTimeout(resolve, ms));
```

## Error Pages

Error pages are rendered when an error is thrown from a page component.
A common example is errors that occur when fetching data.

Error pages wrap the page in a "React Error Boundary" and
must be written in a specific way that differs from normal components.
See {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/routing/error-handling",
"Error Handling" %}.

Source files for error pages must begin with `'use client';`.

## Page Metadata

To add static metadata such as title and description to a page:

```js
import type {Metadata} from 'next';

export const metadata: Metadata = {
  title: 'some title', // results in <title> in <head>
  description: 'some description' // results in <meta> in <head>
};
```

Older versions of Next.js used `head.tsx` files to specify these.
These define a component that renders a fragment containing
elements to add as children of the `<head>` element.

## Hyperlinks

To add hyperlinks to a page:

```js
import Link from 'next/link';

<Link href="some-url">link text</Link>;
```

To add query parameters to a URL, consider using {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams",
"URLSearchParams" %}.

To prevent prefetching of the `Link` target, add `prefetch={false}`.

## Images

The npm package {% aTargetBlank "https://www.npmjs.com/package/sharp",
"sharp" %} is used to "convert large images in common formats to smaller,
web-friendly JPEG, PNG, WebP, GIF and AVIF images of varying dimensions."

To use this, install it with `npm install sharp`
and import it with `import sharp;`.

## Router

The `useRouter` hook creates a router object.
Call the `push` method on the router, passing it a URL,
for for programmatic navigation.
This can only be used in client components.

For example:

```js
'use client';
import {useRouter} from 'next/navigation';

export default function BlogButton() {
  const router = useRouter();

  function goToBlog() {
    router.push('https://mvolkmann.github.io/blog/');
  }

  return (
    <button className="button" onClick={goToBlog}>
      My Blog
    </button>
  );
}
```

The router object also has the methods `back`, `forward`, `prefetch`,
`refresh`, and `replace`.

## Import Aliases

Import statements can use paths that begin with the character `@`
to specify a path that begins at the root of the project
rather than being relative to the current directory. See {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/configuring/absolute-imports-and-module-aliases#module-aliases",
"Module Aliases" %}.

## Dynamic Routes

Dynamic routes are pages whose URLs contain path parameters
that are used to render the page.
For example, the path parameter can be the ID of an object
whose data should be rendered.

Path parameters are represented in the directory structure
below the `app` directory with directories
whose names begin with `[` and end with `]`.
The name between the square brackets is the parameter name.

For example, suppose we want to render the todo with a given id
using the URL `/todo/{id}`.
We would create the directory `app/todo/[id]`
and create the file `page.tsx` inside it.
The follow code could be in this file:

```js
import Link from 'next/link';
import {getTodos} from '@/lib/apis';

type Props = {
  params: {
    id: number
  }
};

export default async function TodoPage({params: {id}}: Props) {
  const todos: Todo[] = await getTodos();

  // The param properties are always strings.
  const number = Number(id);
  const todo = todos.find(t => t.id === number);

  return (
    <section>
      <h1>Todo</h1>
      <p>Id: {todo.id}</p>
      <p>Title: {todo?.title ?? 'missing title'}</p>
      <p>Completed: {String(todo.completed)}</p>
      <Link href="/todos">Back</Link>
    </section>
  );
}
```

## Not Found Pages

The return value of the `notFound` function can be
returned from a component when data it needs is not found.
This provides a default 404 page that just says
"404 | This page could not be found."
For example, suppose we can't find a todo item with a specific id.
We could handle that as follows:

```js
import { notFound } from 'next/navigation';
import { getTodos } from '@/lib/apis';

type Props = {
  params: {
    id: number
  }
}

export default async function TodoPage({ params: { id } }: Props) {
  // params properties are always strings.
  const number = Number(id);

  const todos: Todo[] = await getTodos();
  const todo = todos.find(t => t.id === number);
  if (!todo) return notFound();

  return (
    ... content when found ...
  )
}
```

To provide a custom 404 page, create the file `non-found.tsx`.
This supports having a different 404 page for each route.
For example:

```js
import Link from 'next/link';

export default function NotFound() {
  return (
    <div>
      <h2>Out of luck today!</h2>
      <Link href="/todos">
        <span className="button">Back to List</span>
      </Link>
    </div>
  );
}
```

## Custom Types

Custom TypeScript types can be defined in many places.
One option is to create the top-level file `types.d.ts`
and define all custom types used by a project there.

The file `tsconfig.json` specifies files that should be automatically imported
in the `"include"` option.
By default this includes `**/*.ts`, but this can be modified for each project.
With the default setting, `types.d.ts` is automatically imported.
Any source file in the project can use the types defined there
without importing them.
However, code editors may not recognized the types as being defined
unless they are imported.

## Generating Static Pages

To generate static pages (SSG) that use path parameters at build time,
define and export the {% aTargetBlank
"https://nextjs.org/docs/app/api-reference/functions/generate-static-params",
"generateStaticParams" %} function.
This only works if the `dynamicParams` configuration option is set to `true`,
which it is by default.

The `generateStaticParams` function should return an array of objects
that are the props required by the page component.
A static page will be generated corresponding to each of the objects.

The generated pages can still be generated again at runtime
using the `revalidate` option described in the "Data Fetching" section.

## Context API in Next.js

The React Context API is a great option for
sharing data across multiple React components.
While it isn't particularly easy to configure, it requires far less code
than using other React state management approaches such as Redux.

The first step in using the Context API in a Next.js application
is to create a source file that creates and exports two things.

The first export is a component that takes a `children` prop
and renders it inside a custom context provider.

The second export is a function that calls `useContext`,
passing it a custom context. This second export is not strictly necessary,
but it simplifies the code in components that need access to the context data.

React hooks can only be used in client components.
That includes the `useContext` hook.

The following is an example of a file described above, named `dog-context.tsx`.
It is recommended to create a `context` directory inside the `app` directory
and store this file there.

{% raw %}

```js
'use client';

import {createContext, ReactNode, useContext, useState} from 'react';

interface DogData {
  breed: string;
  name: string;
  setBreed: (breed: string) => void;
  setName: (name: string) => void;
}

// My editor performed bad code formatting here.
const DogContext =
  createContext <
  DogData >
  {
    breed: '',
    name: '',
    setBreed: string => '',
    setName: string => ''
  };

interface Props {
  children: ReactNode;
}

export const DogContextProvider = ({children}: Props) => {
  const [breed, setBreed] = useState('Whippet');
  const [name, setName] = useState('Comet');

  return (
    <DogContext.Provider value={{breed, name, setBreed, setName}}>
      {children}
    </DogContext.Provider>
  );
};

export const useDogContext = () => useContext(DogContext);
```

{% endraw %}

The second step is to use the custom provider in some client component
at a location in the component hierarchy such that
no components above it need the context data and
all components below it have the option to use the context data.
A good choice is to select one of the `layout.tsx` files.
Choosing the topmost `layout.tsx` makes
the context data available throughout the app.

The following code from the top-level `layout.tsx`
demonstrates configuring a custom provider:

```js
import './globals.css';
import {DogContextProvider} from '@/app/context/dog-context';

type Props = {children: React.ReactNode};

export default function RootLayout({children}: Props) {
  return (
    <html lang="en">
      <body>
        <DogContextProvider>
          <h1>My App Title</h1>
          {children}
        </DogContextProvider>
      </body>
    </html>
  );
}
```

The third step is to use the context function
in every component that needs to access the data.

Here is an example of a component the only reads the data:
It could of course render additional JSX.

```js
import {useDogContext} from '@/app/context/dog-context';

export default function Dog() {
  const {breed, name} = useDogContext();
  return (
    <section>
      <p>
        Dog {name} is a {breed}.
      </p>
    </section>
  );
}
```

Here is an example of a component the modifies the data.
It could of course render additional JSX.

```js
'use client';

import {useDogContext} from '@/app/context/dog-context';

export default async function DogManager() {
  const {setBreed, setName} = useDogContext();

  function handleClick() {
    setBreed('Beagle');
    setName('Snoopy');
  }

  return (
    <section>
      <button onClick={handleClick}>Change Dog</button>
    </section>
  );
}
```

## Route Handlers

REST APIs can be implemented by defining {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/routing/route-handlers",
"route handlers" %}.

Like page routes, the directory structure defines the URL paths
that will be used to send HTTP requests.

For each server route, create a directory under `app/api`
and create a source file named `route.ts` in the directory.

Directory names beginning with `[` and ending with `]`
can be used to capture path parameters.

For example, the code below is in the directory `app/api/dogs`
which has the subdirectory `[name]`.
The file `dog.ts` maintains the collection of dogs
and exports functions to perform all the CRUD operations.
The file `route.ts` defines handlers for getting all the dogs,
adding a new dog, and updating an existing dog.
The file `[name]/route.ts` defines handlers for getting a specific dog
and deleting a specific dog.

To get all the dogs, send a GET request to `http://localhost:3000/api/dogs`.

To get the dog with a given name, send a GET request to
`http://localhost:3000/api/dogs/Comet` where Comet is a dog name.

To add a dog, send a POST request to `http://localhost:3000/api/dogs`
with a JSON body like `{ "name": "Snoopy", "breed": "Beagle" }`.

To update an existing dog,
send a PUT request to `http://localhost:3000/api/dogs`
with a JSON body like `{ "name": "Snoopy", "breed": "Great Dane" }`.

To delete an existing dog,
send a DELETE request to `http://localhost:3000/api/dogs/Snoopy`
where "Snoopy" is the name of the dog to delete.

```js
// app/api/dogs/dogs.ts
export type Dog = {
  name: string,
  breed: string
};

export let dogs: Dog[] = [
  {name: 'Comet', breed: 'whippet'},
  {name: 'Maisey', breed: 'Treeing Walker Coonhound'},
  {name: 'Oscar', breed: 'German Shorthaired Pointer'},
  {name: 'Ramsay', breed: 'Native American Indian Dog'}
];

export function addDog(dog: Dog) {
  dogs.push(dog);
}

export function deleteDog(name: string): Dog | undefined {
  const dog = dogs.find(d => d.name === name);
  if (dog) dogs = dogs.filter(d => d.name !== name);
  return dog;
}

export const getDogs = () => dogs;

export function getDog(name: string): Dog | undefined {
  return dogs.find(d => d.name === name);
}

export function updateDog(dog: Dog): Dog | undefined {
  const {breed, name} = dog;
  const existingDog = dogs.find(d => d.name === name);
  if (existingDog) existingDog.breed = breed;
  return existingDog;
}
```

```js
// app/api/dogs/route.ts
import {NextResponse} from 'next/server';
import {addDog, getDogs, updateDog, type Dog} from './dogs';

export function GET(request: Request) {
  const dogs = getDogs();
  return NextResponse.json(dogs);
}

export async function POST(request: Request) {
  const dog: Dog = await request.json();
  addDog(dog);
  return NextResponse.json(dog);
}

export async function PUT(request: Request) {
  const dog: Dog = await request.json();
  const result = updateDog(dog);
  if (result) {
    return NextResponse.json(dog);
  } else {
    return NextResponse.json(null, {status: 404});
  }
}
```

```js
// app/api/dogs/[name]/route.ts
import {NextResponse} from 'next/server';
import {deleteDog, getDog, type Dog} from '../dogs';

type Props = {
  params: {name: string}
};

export async function DELETE(request: Request, {params: {name}}: Props) {
  const result = deleteDog(name);
  const status = result ? 200 : 404;
  return NextResponse.json(null, {status});
}

export function GET(request: Request, {params: {name}}: Props) {
  const dog = getDog(name);
  if (dog) {
    return NextResponse.json(dog);
  } else {
    return NextResponse.json(null, {status: 404});
  }
}
```

For API routes that use query parameters,
they can be obtained as follows:

```js
const {searchParams} = new URL(request.url);
const name = searchParams.get('name');
```

## Environment Variables

To provide environment variables to a Next.js app,
define them in the file `.env.local` in the root project directory.
See {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/configuring/environment-variables",
"Environment Variables" %}.

The default `.gitignore` file in Next.js apps includes this file,
so it will not be committed.

Environment variables whose names that begin with "NEXT_PUBLIC\_"
are available in both client and server code.
Other variables are only available in server code.

For example, suppose the following is in `.env.local`:

```text
DB_HOST=somehost
DB_USER=someuser
DB_PASS=somepswd
NEXT_PUBLIC_DOG=Comet
```

Suppose the following code is in a `page.tsx` file:

```js
console.log('DB_PASS =', process.env.DB_PASS);
console.log('DOG =', process.env.NEXT_PUBLIC_DOG);
```

When this code runs on the server, both values will print.
When this code runs on the client, `DB_PASS` will be undefined,
but `NEXT_PUBLIC_DOG` will print.

It is safe to use this for API keys and other secrets
as long as names do not begin with `NEXT_PUBLIC_`.

## Middleware

Next.js supports defining {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/routing/middleware",
"middleware" %} that runs before API calls are processed.

Each Next.js app can include one `middleware.ts` file in the
project root directory which is typically the `src` directory.

The middleware can do many things including:

- access and set cookies
- add request headers
- return a response and bypass the intended URL
- redirect to a different URL

The middleware runs on every request including
requests for static assets (like images),
requests for client-side files, and API requests.
To configure the middleware to only run on certain request paths,
export a `config` object.
The example below configures it to only run for API requests.

By default, all API routes only accept requests from
the same origin as the Next.js app due to CORS restrictions.
The example below configures CORS so domains other than
the one where the app is deployed can send API requests.
The allowed origins can differ based on whether
we are running in production or development mode.

```js
// src/middleware.ts
import {type NextRequest, NextResponse} from 'next/server';

const allowedOrigins =
  process.env.NODE_ENV === 'production'
    ? ['http://www.mysite.com', 'https://mysite.com']
    : ['http://localhost:3000', 'https://www.google.com']; // for testing CORS

// This function can optionally be async.
export function middleware(request: NextRequest) {
  const {headers, method, nextUrl, url} = request;

  const origin = headers.get('origin');

  // method is GET, POST, PUT, or DELETE.
  // url is a string like "http://localhost:3000/api/dogs".
  // origin can be null or a string like "http://localhost:3000".
  // nextURL is a URL object w/ many properties.
  console.log('middleware:', method, url, 'from', origin);

  // This enforces CORS restrictions.
  // In production mode you may want to add "|| !origin"
  // to block access from tools like Postman or Thunder Client.
  if (origin && !allowedOrigins.includes(origin)) {
    return new NextResponse(null, {
      status: 400,
      statusText: 'Bad Request'
    });
  }

  // Proceed to intended URL.
  return NextResponse.next();
}

// Only run the middleware for API requests.
export const config = {
  // Can specify an array of paths.
  // Each path can be a regular expression.
  matcher: '/api/:path*'
};
```

Create the following file which exports a function that simplifies
creating responses that include the headers required to support CORS:

```js
// src/api/cors.ts
import {NextResponse} from 'next/server';

export function jsonCorsResponse(
  request: Request,
  object: Object | null,
  status: number = 200
): NextResponse {
  const origin = request.headers.get('origin');
  const body = object ? JSON.stringify(object) : '';
  return new NextResponse(body, {
    headers: {
      // '*' allows tools like Postman and Thunder Client to send requests.
      'Access-Control-Allow-Origin': origin || '*',
      'Content-Type': 'application/json'
    },
    status
  });
}
```

Change all router handler functions that return a response
to use the function above. For example,
in the handler that retrieves an array of all dogs, change

```js
return NextResponse.json(dogs);
```

to

```js
return jsonCorsResponse(request, dogs);
```

For testing purposes, we can send requests from the google.com domain
when running in development mode.
To do this, browse google.com, open the DevTools console,
and enter the following:

```js
const res = await fetch('http://localhost:3000/api/dogs');
console.log(await res.json());
```

## Rate Limiting

We can configure routes to perform rate limiting
which limits the number of requests that can be sent to
a given API endpoint or set of them over a given period of time.

First, install the NPM package `limiter" by entering `npm install limiter`.

Second, create the following file which defines the limits to enforce and
exports a function that simplifies checking for requests that exceed the limit.
Routes can call this function and
test whether `null` or a `NextResponse` object is returned.
If `null` is returned, the limit has not been exceeded.
If a `NextResponse` object is returned, the limit has been exceeded
and the route should return that response.

```js
// src/app/api/limiter.ts
import {RateLimiter} from 'limiter';
import {NextResponse} from 'next/server';

const limiter = new RateLimiter({
  fireImmediately: true,
  interval: 'min',
  tokensPerInterval: 8
});

export async function getLimitedResponse(
  request: Request
): Promise<NextResponse | null> {
  const remaining = await limiter.removeTokens(1);
  console.log('limiter.ts: remaining =', remaining);
  if (remaining >= 0) return null; // allow request

  // Reject request.
  const origin = request.headers.get('origin');
  return new NextResponse(null, {
    status: 429,
    statusText: 'Too Many Requests',
    headers: {
      // '*' allows tools like Postman and Thunder Client to send requests.
      'Access-Control-Allow-Origin': origin || '*',
      'Content-Type': 'text/plain'
    }
  });
}
```

Each route that wishes to enforce the limit should include the following code:

```js
import {getLimitedResponse} from '@/app/api/limiter';

const response = await getLimitedResponse(request);
if (response) return response;
```

This cannot be done in `middleware.ts` and I don't know why.
When I tried that I got
"error Cannot read properties of undefined (reading 'now')"
when `limiter.ts` tries to create a `RateLimiter` object.

## Sitemaps

From {% aTargetBlank
"https://developers.google.com/search/docs/crawling-indexing/sitemaps/overview",
"Learn about sitemaps" %}:

> A sitemap is a file where you provide information about the pages, videos,
> and other files on your site, and the relationships between them.
> Search engines like Google read this file to crawl your site more efficiently.
> A sitemap tells Google which pages and files you think are important
> in your site, and also provides valuable information about these files.
> For example, when the page was last updated and any alternate language versions of the page.

To generate a sitemap for a Next.js application:

1. Enter `npm install next-sitemap`

1. Create the file `next-sitemap.config.js` in the root project directory.

1. Add the following in that file:

   ```js
   /** @type { import('next-sitemap').IConfig } */
   module.exports = {
     // Set this environment variable in ".env.local".
     siteUrl: process.env.SITE_URL || 'http://localhost:3000',
     generateRobotsTxt: true,
     generateIndexSitemap: false
   };
   ```

1. Add the following script in `package.json`:

   ```json
   "postbuild": "next-sitemap"
   ```

1. Enter `npm run build`.
   This generates the files `public/robots.txt` and `public/sitemap.xml`.
