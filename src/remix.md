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

Styling that is specific to a page or component can be defined in `.css` files
that are imported into the source file for the page or component.

CSS files for components can be placed in the same directory
as the corresponding `.tsx` file
and imported using the path `./{some-name}.css`.

CSS files for routes should be placed in the `app/styles` directory
and imported using the path `~/styles/{some-name}.css`.
The tilde (`~`) at the beginning of the path maps to the `app` directory.

For example:

```ts
import type { LinksFunction } from "@remix-run/node";
import styles from "./Demo.css";

// Objects in the returned array can also contain a "media" property
// to specify a media query that must be satisfied to use the styles.
// For example, media: "screen and (min-width: 768px)"
export const links: LinksFunction = () => [
  { rel: "stylesheet", href: styles }
];
```

Remix only looks for "links" and "meta" functions in route components.
So users of components (not pages) need to import and call those functions
to get arrays and spread them into their own links array.
This pattern is called "surfacing links" in the Remix docs.

For example, the file `app/routes/some-route.tsx` could contain the following:

```ts
import Demo, { links as demoLinks } from "~/components/Heading";

import styles from "~/styles/some-route.css";

export const links = () => [
  { rel: "stylesheet", href: styles },
  ...demoLinks(),
];
```

## Links

To create a link to another page, use the `Link` component.
This must be imported from "@remix-run/react".

The `Link` `to` prop supports absolute and relative paths.
Absolute paths begin with a slash.
Relative paths do not and are appended to the current URL.

To include a link that refreshes the current page,
use `<Link to=".">Refresh</Link>`.

## Loaders

Any route can export a `loader` function that is optionally async.
This is invoked before the route is rendered.
It processes GET requests to the route
and is used to fetch data needed by the page.

For example:

```ts

type LoaderData = Todo[];

export function loader({ params, request }) {
  // We could authenticate with something like
  // await requireUserId(request);
  // which could throw if the user is not authenticated.

  // We could support a query parameter like this:
  // const query = new URL(request.url).searchParams.get('query') ?? '';
  // const todos = await searchTodos(query);
  // return json(todos);
  // We can also return other content types including plain text.

  return getTodos();
  // TODO: Why doesn't this also work?
  // return json(getTodos())
}
```

To access the data in the page component, use the `useLoaderData` hook.
For example:

```ts
const todos = useLoaderData<Todo[]>();
```

## Actions

Any route can export an `action` function that is optionally async.
These are invoked when a `form` on the page is submitted.
They only exist and run on the server, never in the browser.
These enable implementing both client and server functionality
for a route in the same source file.

`action` functions process POST requests to the route.
Often it is also used to process requests that would have used `PUT` or `DELETE`
if those were supported by HTML forms.
As a workaround, an property named "intent" can be used to signal
to the `action` function whether it should perform
a create, update, or delete operation.

For example:

```tsx
import {redirect} from '@remix-run/node';

// Note how the front and back end are implemented in the same file.
export const action: ActionFunction = async ({ request }) => {
  try {
    // No need to use the Fetch API or axios because
    // we are already running in the server.
    let todos = await getTodos();
    const formData = await request.formData();
    const intent = formData.get("intent") as string;

    if (intent === "add") {
      await sleep(1); // to demonstrate "isSubmitting" state
      const id = Date.now()
      const todo = { id, text: formData.get("text") };
      todos.push(todo);
      await saveTodos(todos);
    } else if (intent?.startsWith("delete-")) {
      const id = Number(intent.split("-")[1]);
      const index = todos.findIndex((t: Todo) => t.id === id)
      if (index === -1) {
        return json(
          { message: `No todo with id ${id} found.` },
          { status: 404 }
        );
      }
      todos.splice(index, 1);
      await saveTodos(todos);
    }

    return null; // stays on current page
    // return redirect(path); // redirects to another page
    // In the case of a POST request, this could redirect
    // to a page that displays the newly created resource.
    // For example, return redirect(`/todo/${todo.id}`);
  } catch (e) {
    console.error("todos.tsx action:", e);
  }
};
```

## Prefetching

The `Link` and `NavLink` components can be configured
to prefetch all data they need when the user hovers over them.
This makes rendering the associated routes faster.
To this, add the prop `prefetch="intent"`.

## Nested Routes

Any route can render its own HTML and the HTML of a child route
by including `<Outlet />` in its JSX.

When a nested route is visited, all of its ancestor routes are also rendered.
This includes calling all the `loader` functions in the ancestor routes.

## Resource Routes

Resource routes are routes that expose `loader` and `action` functions,
but do not export a React component.
These are only used for defining API endpoints
and are not associated with a specific page in the UI.

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

## Input Validation

Remix favors validating input after a submit button is pressed.
The `action` function can call custom functions to validate each input.
It constructs an object containing a `fieldErrors` property
whose value is an object containing an error message for each invalid input.
The React component that renders the form can then
display an error message below each invalid input.
The following code demonstrates this approach.

```ts
import { type ActionFunction } from "@remix-run/node";

type ActionData = {
  fields: { text?: string }, // values entered for each field
  fieldErrors: { text?: string }, // error messages for each field
  formError?: string // describes error at form level, not individual input
}

// Define one function like this for each input to be validated.
function validateText(text: string) {
  if (text.length < 3) {
    return "Todo text must be at least three characters."
  }
}

export const action: ActionFunction =
  async ({ request }): Promise<Response | ActionData> => {
    const fieldErrors = {
      // Add a line like this for each form input.
      text: validateText(text)
    };

    // This determines if there are one or more validation errors.
    if (Object.values(fieldErrors).some(Boolean)) {
      return { fieldErrors, fields: { text } };
    }

    // If we reach here then there were no validation errors.
    const id = Date.now()
    const todo = { id, text };
    todos.push(todo);
    await saveTodos(todos);
    return null; // stays on current page
  };

export default function Todos() {
  const [text, setText] = useState("");
  const actionData = useActionData<ActionData>();
  const fieldErrors = actionData?.fieldErrors;

  return (
    <div className="todos">
      <Form method="post" id="todo-form">
        <input
          name="text"
          onChange={e => setText(e.target.value)}
          placeholder="enter new todo here"
          value={text}
        />
        {fieldErrors?.text && (
          <div className="error">Error: {fieldErrors.text}</div>
        )}

        {/* There can be more form inputs here. */}

        <button>Add</button>
      </Form>
    </div>
  );
}
```

## Schema Validation

Consider using {% aTargetBlank "https://zod.dev", "zod" %}
for schema validation.

## Building

To prepare a Remix app for deployment, enter `npm build`.
The generated files for the server are placed in the `build` directory.
The generated files for the client are placed in the `public/build` directory.
