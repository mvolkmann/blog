---
eleventyNavigation:
  key: Remix
layout: topic-layout.njk
---

## Overview

## Creating a new project

1. Install Node.js.
1. `cd` to the directory where the project will be created.
1. Enter `npx create-remix@latest`
1. Enter "y" to proceed.
1. Enter a project name.
1. Press return for "Just the basics".
1. Press return for "Remix App Server".
1. Press return for "TypeScript".
1. Enter "Y" to run `npm install`.
1. `cd` to the new project directory.
1. See the `README.md` file for instructions.
1. Enter `npm run dev`
1. Browse localhost:3000.

## Project Structure

The `public` directory contains static assets like images.

The `app` directory defines the routes and components of the app.

The file `root.jsx` defines the root component.
This renders an `Output` component
which is responsible for rendering each page.

The file `entry.server.tsx` defines the server code that runs on every request.
Often no changes are needed in this file.

The file `entry.client.tsx` defines the code that runs in the browser.
Often no changes are needed in this file.

The `app/routes` directory defines all the routes of the app
that are mapped to URL paths.
This begins with only the file `_index.tsx`.

To create a new page in the app,
create a new file in the `app/routes` directory.
To add a link to this page in `_index.tsx`, add `<a href="/demo">Demo</a>`.
But this downloads the page from the server.
To perform client-side routing, import the `Link` component with:

```js
import {Link} from '@remix-run/react';
```

and use the following instead of an anchor tag:

```html
<Link to="/demo">Demo Link</Link>
```

Routes can be in deeper subdirectories to require a deeper URL path.

## Styling

Global CSS can be defined in a file inside the `app` directory.
This can be included in `app/root.jsx` to make it available to all pages.
