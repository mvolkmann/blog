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

## Static Resources

Static resources like images are typically placed in
a top-level "public" directory.
This directory can have subdirectories such as "images".

## Client vs. Server Components

Components that perform data fetching through HTTP requests
are considered to be "server" components.
By default, components are server components.

Components that contain client-side logic like DOM event handling
are considered to be "client" components.
Their source files must begin with `'use client';`.

Client components cannot send HTTP requested and
server components cannot perform DOM event handling.
Sometimes both are needed.
A solution is to use client components inside server components.

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

Sometimes it is convenient to call such a function in multiple places.
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

## Error Pages

Error pages are rendered when an error is thrown from a page component.
A common example is errors that occur when fetching data.

Error pages wrap the page in a "React Error Boundary" and
must be written in a specific way that differs from normal components.
See {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/routing/error-handling",
"Error Handling" %}.

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
import Link from 'next/Link';

<Link href="some-url">link text</Link>;
```

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

type Params = {
  params: {
    id: number
  }
};

export default async function TodoPage({params: {id}}: Params) {
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

type Params = {
  params: {
    id: number
  }
}

export default async function TodoPage({ params: { id } }: Params) {
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
This file is automatically imported,
so any source file in the project can use the types without importing them.

## Generating Static Pages

To generate static pages (SSG) that use path parameters at build time,
define and export the `generateStaticParams` function.
This only works if the `dynamicParams` configuration option is set to `true`,
which it is by default.
This function should return an array of objects.
TODO: How does it know which pages to generate?
The generated pages can still be generated again at runtime
using the `revalidate` option described in the "Data Fetching" section.

## Server Routes

REST APIs can be implemented by defining server routes
in files without the "pages/api" directory. CORRECT?
