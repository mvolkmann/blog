---
eleventyNavigation:
  key: Remix
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://remix.run", "Remix" %} is
"a full stack web framework that lets you
focus on the user interface and work back through web standards
to deliver a fast, slick, and resilient user experience."

Remix is heavily based on React Router.

The hooks `useState` and `useEffect` can be used in Remix apps,
but they tend to be used much less than in standard React apps.

Remix apps use a database for application state instead of a library like Redux

## Resources

- {% aTargetBlank "https://www.epicweb.dev/", "EpicWeb.dev" %} -
  hosts articles and tutorials on Remix from Kent C. Dodds

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
To create additional routes, add more `.tsx` files
and subdirectories in this directory.
Subdirectories define URL paths and support nested routes.

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

## Outlet component

The `Outlet` component rendered in `root.tsx`
renders the content of the current page.

## Styling

Global CSS can be defined in a file inside the `app` directory.
This can be included in `app/root.jsx` to make it available to all pages.

## Links

To create a link to another page, use the `Link` component.
This must be imported from "@remix-run/react".

The `Link` `to` prop supports absolute and relative paths.
Absolute paths begin with a slash.
Relative paths do not and are appended to the current URL.


## Prefetching

The `Link` and `NavLink` components can be configured
to prefetch all data they need when the user hovers over them.
This makes rendering the associated routes faster.
To this, add the prop `prefetch="intent"`.

## Dynamic Routes

Dynamic routes have access to data from the URL used to render them.

Dynamic routes are defined by creating a source file in the `routes` directory
whose name contains `$`.
For example, `notes.$id.tsx`.
The `.` after `notes` is translated to a `/`, so the path for visiting this page
will be `notes/{some-id}`.

For example:

```ts
import {useLoaderData} from '@remix-run/react';

export default function MyDynamicPage() {
  const data = useLoaderData();
  return (
    <main>
      <p>{data.someProperty}</p>
    </main>
  )
}

export async function loader({params}) {
  const id = params.id;
  const data = await getMyData(id);
  if (!data) {
    throw json(
      {message: 'Could not find data.'},
      {status: 404}
    );
  }
  return data;
}
```

## Spinners

See {% aTargetBlank "https://github.com/smeijer/spin-delay", "spin-delay" %}.
