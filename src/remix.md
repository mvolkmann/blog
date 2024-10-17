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

Remix enables many web applications to work with no client-side JavaScript code.

Remix is heavily based on React Router.

The hooks `useState` and `useEffect` can be used in Remix apps,
but they tend to be used much less than in standard React apps.

Remix apps use a database for application state instead of a library like Redux.

Remix automatically provides route-level code splitting.

## Resources

- {% aTargetBlank "https://www.epicweb.dev/", "EpicWeb.dev" %} -
  hosts articles and tutorials on Remix from Kent C. Dodds

## Creating a new project

1. Install Node.js.
1. `cd` to the directory where the project will be created.
1. Enter `npx create-remix@latest`
1. Enter a project directory path (ex. `./dogs`).
1. Press return for "Initialize a new git repository?".
1. Press return for "Install dependencies with npm?".
1. `cd` to the new project directory.
1. See the `README.md` file for instructions.
1. Enter `npm run dev`
1. Browse localhost:5173.

## Project Structure

The `public` directory contains static assets like images.

The `app` directory defines the routes and components of the app.

The file `root.tsx` defines the root component.
By default this:

- loads Tailwind styles
- loads Google fonts,
- renders `meta` tags specified by exported `meta` functions in your routes
- renders `link` tags specified by exported `links` functions in your routes
- renders JSX returned by the default function exported in your routes
- and more

The default export in `root.tsx` is function that renders an `Output` component
which is responsible for rendering the content for each route.
This also exports a `Layout` function
which is the "template" used for rendering all routes.
Content that should appear on every page, such as a top navigation bar,
can be rendered in the `Layout` function.

The file `entry.server.tsx` defines the server code that runs on every request.
Often no changes are needed in this file.

The file `entry.client.tsx` defines the code that runs in the browser.
Often no changes are needed in this file.

The `app/routes` directory defines all the routes of the app
that are mapped to URL paths.
This begins with only the file `_index.tsx`.

To create a new page in the app,
create a new file in the `app/routes` directory.
To add a link to this page in `_index.tsx`,
add an element like `<a href="/demo">Demo</a>`.
This downloads the page from the server.
To perform client-side routing, import the `Link` component with:

```js
import {Link} from '@remix-run/react';
```

and use the following instead of an anchor tag:

```html
<Link to="/demo">Demo Link</Link>
```

Routes can be in deeper subdirectories to require a deeper URL path.

## UI Components

Consider using the React component library
{% aTargetBlank "https://reach.tech", "Reach UI" %}.
It provides many components including `Accordion`, `Alert`, `Combobox`,
`Dialog`, `Portal`, `Slider`, `Tabs`, and `Tooltip`.

The Remix team plans to implement Remix-specific versions of these components
in the future.

## Routes

To define routes, add `.tsx` files in and under the `routes` directory.

A route named "demo" can be defined by the file `demo.tsx`
or by the directory "demo" containing the file `index.tsx`.
The latter approach supports placing additional route-specific files
in the same directory as `index.tsx`.

Subdirectories define URL paths and support nested routes.

File names containing dots in a addition to the one before the file extension
define route paths. For example, the file `foo.bar.baz.tsx`
defines the route `foo/bar/baz`.

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
  );
}

export async function loader({params}) {
  const id = params.id;
  const data = await getMyData(id);
  if (!data) {
    throw json({message: 'Could not find data.'}, {status: 404});
  }
  return data;
}
```

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

A `loader` function can just return a JavaScript object
and Remix will convert it to a JSON response and include
a "Content-Type" header with a value of "application/json; charset=utf-8".
For example, the file `routes/song.tsx` could contain the following:

```ts
export const loader = () => ({
  artist: 'Phoebe Bridgers',
  song: 'Graceland Too'
});
```

## Styling

Global CSS can be defined in a file inside the `app` directory.
This can be included in `app/root.jsx` to make it available to all pages.

Styling that is specific to a page or component can be defined in `.css` files
that are imported into the source file for the page or component.

CSS files for components can be placed in the same directory
as the corresponding `.tsx` file
and imported using the path `./{some-name}.css`.

CSS files for routes should be placed in the `app/styles` directory
and imported using the path `~/styles/{some-name}.css?url`.
The tilde (`~`) at the beginning of the path maps to the `app` directory.

For example:

```ts
import type {LinksFunction} from '@remix-run/node';
import styles from '~/styles/Demo.css?url';

export const links: LinksFunction = () => [
  {rel: 'stylesheet', href: styles, as: 'style'}
];
```

Objects in the returned array can include a "media" property
to specify a media query that must be satisfied to use the styles.
For example, media: "screen and (min-width: 768px)".

Remix only looks for "links" and "meta" functions in route components.
So users of components (not pages) need to import and call those functions
to get arrays and spread them into their own links array.
This pattern is called "surfacing links" in the Remix docs.

For example, the file `app/routes/some-route.tsx` could contain the following:

```ts
import Heading, {links as headingLinks} from '~/components/Heading';

import styles from '~/styles/this-route.css';

export const links = () => [
  {rel: 'stylesheet', href: styles},
  ...headingLinks()
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
These functions process GET requests to routes and are
used to fetch data needed by pages before they are rendered.

These functions only exist and run on the server, never in the browser.
They enable implementing both client and server functionality
for a route in the same source file.
Because these functions run on the server, they never have CORS issues.

The code in `loader` functions can communicate directly with a database.

For example:

```ts
type LoaderData = Todo[];

export function loader({params, request}) {
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

In nested routes, the `loader` function for that route
and the `loader` functions of all ancestor routes are called.

## Actions

Any route can export an `action` function that is optionally `async`.
These are invoked when a `form` on the page is submitted.
Like `loader` functions, these functions only
exist and run on the server, never in the browser.
They enable implementing both client and server functionality
for a route in the same source file.

`action` functions process POST requests to the route.
HTML `form` elements only support two methods, `GET` and `POST`.
Often `action` functions are used to process requests that would have
used `PUT` or `DELETE` if those were supported by HTML forms.
As a workaround, an property named "intent" can be used to signal
to the `action` function whether it should perform
a create, update, or delete operation.

For example:

```tsx
import {redirect} from '@remix-run/node';

// Note how the front and back end are implemented in the same file.
export const action: ActionFunction = async ({request}) => {
  try {
    // No need to use the Fetch API or axios because
    // we are already running in the server.
    let todos = await getTodos();
    const formData = await request.formData();

    // This gets an object whose key/value pairs
    // represent all the data passed in the request.
    const data = Object.fromEntries(formData);

    // This gets the value of a specific piece of data passed in the request.
    const intent = formData.get('intent') as string;

    if (intent === 'add') {
      await sleep(1); // to demonstrate "isSubmitting" state
      const id = Date.now();
      const todo = {id, text: formData.get('text')};
      todos.push(todo);
      await saveTodos(todos);
    } else if (intent?.startsWith('delete-')) {
      const id = Number(intent.split('-')[1]);
      const index = todos.findIndex((t: Todo) => t.id === id);
      if (index === -1) {
        return json({message: `No todo with id ${id} found.`}, {status: 404});
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
    console.error('todos.tsx action:', e);
  }
};
```

In nested routes, only the `action` function for that route is called.
The `action` functions ancestor routes are not called.

A route component can contain multiple `form` elements.
When any of them are submitted, the same `action` function is called.
One way to determine which `form` submit triggered a call to `action`
is to have the submit buttons in each `form` all have `name="intent"`
and different values for their `value` attribute.

## Prefetching

The `Link` and `NavLink` components can be configured
to prefetch all data they need when the user hovers over them.
This makes rendering the associated routes faster.
To this, add the prop `prefetch="intent"`.

## Spinners

Remix automatically displays a spinner in the browser tab
when it is waiting for a `loader` or `action` function to complete.

For spinners on the page, see {% aTargetBlank
"https://github.com/smeijer/spin-delay", "spin-delay" %}.

## Input Validation

Remix favors validating input after a submit button is pressed.
The `action` function can call custom functions to validate each input.
It constructs an object containing a `fieldErrors` property
whose value is an object containing an error message for each invalid input.
The React component that renders the form can then
display an error message below each invalid input.
The following code demonstrates this approach.

```ts
import {type ActionFunction} from '@remix-run/node';

type ActionData = {
  fields: {text?: string}; // values entered for each field
  fieldErrors: {text?: string}; // error messages for each field
  formError?: string; // describes error at form level, not individual input
};

// Define one function like this for each input to be validated.
function validateText(text: string) {
  if (text.length < 3) {
    return 'Todo text must be at least three characters.';
  }
}

export const action: ActionFunction = async ({
  request
}): Promise<Response | ActionData> => {
  const fieldErrors = {
    // Add a line like this for each form input.
    text: validateText(text)
  };

  // This determines if there are one or more validation errors.
  if (Object.values(fieldErrors).some(Boolean)) {
    return {fieldErrors, fields: {text}};
  }

  // If we reach here then there were no validation errors.
  const id = Date.now();
  const todo = {id, text};
  todos.push(todo);
  await saveTodos(todos);
  return null; // stays on current page
};

export default function Todos() {
  const [text, setText] = useState('');
  const actionData = useActionData<ActionData>();
  const fieldErrors = actionData?.fieldErrors;

  return (
    <div className="todos">
      {/* Using Form instead of form prevents full-page reloads. */}
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

## Client Only Code

To prevent code from running on the server and only run it on the client-side,
surround the code with a check like the following:

```js
if (typeof window !== 'undefined') {
  // Do client-only things here.
}
```

For code inside a React component that should only run on the client-side,
place it in a call to `useEffect` as follows:

```js
useEffect(() => {
  // Do client-only things here.
});
```

## Error Boundaries

Each route and `root.tsx` can befine an `ErrorBoundary` function
which returns JSX to be rendered if an unexpected error occurs.
This replaces the content that would otherwise be rendered by <Outlet />.

In the case of nested routes, the closes such function
to where the error occurs is used.
The `ErrorBoundary` function in `root.tsx` serves as a catch-all.

```ts
export function ErrorBoundary({error}) {
  console.log('root.tsx ErrorBoundary: error =', error);
  return (
    <main className="error">
      <h1>An error occurred.</h1>
      <p>{error?.message ?? 'unknown'}</p>
      <p>
        Back to <Link to="/">safety</Link>.
      </p>
    </main>
  );
}
```

## Catch Boundaries

A `CatchBoundary` function is similar to an `ErrorBoundary`,
but is focused on expected rather than unexpected errors.
Like that we can define it in the root component or specific routes.
Remix renders this component when a `Response` object is thrown.
If anything other than an error is thrown
then `ErrorBoundary` is used instead of `CatchBoundary`.

One place a `Response` object can be thrown is in a `loader` function.
For example:

```ts
if (some - condition) {
  throw json(
    // creates a Response object
    {message: 'some-message'},
    {status: 404, statusText: 'some-status-text'}
  );
}
```

The following code demonstrates defining a `CatchBoundary` function:

```ts
import {useCatch} from '@remix-run/react';

export function CatchBoundary({error}) {
  const response = useCatch();
  const message = response.data?.message || 'unspecified error';
  return (
    <main>
      <h1>An error occurred.</h1>
      <p>status: {response.status}</p>
      <p>{message}</p>
    </main>
  );
}
```

If the `CatchBoundary` function throws an error
and an `ErrorBoundary` function is defined,
it will determine what to render.

## Building

To prepare a Remix app for deployment, enter `npm build`.
The generated files for the server are placed in the `build` directory.
The generated files for the client are placed in the `public/build` directory.
