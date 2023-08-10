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

- server-side rendering (SSR) for first render efficiency
  and better search engine optimization (SEO);
  hydrates JavaScript in client
- static site generation (SSG) at build time and on request
- code splitting
- prefetching
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
Creating a new Next.js app in /Users/volkmannm/Documents/projects/YugaLabs/todo.

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

## Modifying App

Begin by modifying `src/app/page.tsx`.

## Page Routes

The "app" directory holds source files that define page routes.
The directory structure maps directly to the URLs used to access the pages.

To add a new page, create a directory in `src/app`
whose name is the route name and
create the file `page.tsx` (or `page.js`) inside that directory.

Source files for page-specific components can be added in the same directory.

## Static Resources

Static resources like images are typically placed in
a top-level "public" directory.
This directory can have subdirectories such as "images".

## Layouts

The file `app/layout.tsx` defines the layout for all pages in the app.
Additional `layout.tsx` files in page route directories define
the layout for all pages at specific URL routes.
These are nested inside the main layout.

## Error Pages

Error pages are rendered when an error occurs
while loading a corresponding route.

## Page Metadata

To add static metadata such as title and description to a page:

```js
import type {Metadata} from 'next';

export const metadata: Metadata = {
  title: 'some title', // results in <title> in <head>
  description: 'some description' // results in <meta> in <head>
};
```

Older versions of Next.js used a `head.tsx` file to specify these.

## Hyperlinks

To add hyperlinks to a page:

```js
import Link from 'next/Link';

<Link href="some-url">link text</Link>;
```

## Server Routes

REST APIs can be implemented by defining server routes
in files without the "pages/api" directory. CORRECT?
