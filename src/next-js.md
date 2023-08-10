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

Import statements can use paths that begin with a character like `@`
to specify a path that begins at the root of the project
rather than being relative to the current directory. See {% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/configuring/absolute-imports-and-module-aliases#module-aliases",
"Module Aliases" %}.

## Custom Types

Custom TypeScript types can be defined in many places.
One option is to create the top-level file `types.d.ts`
and define all custom types used by a project there.
This file is automatically imported,
so any source file in the project can use the types without importing them.

## Server Routes

REST APIs can be implemented by defining server routes
in files without the "pages/api" directory. CORRECT?
