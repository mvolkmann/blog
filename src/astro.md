---
eleventyNavigation:
  key: Astro
layout: topic-layout.njk
---

<style>
    figcaption {
      margin-top: 0.5rem;
      text-align: center;
    }
    img {
        border: 1px solid gray;
    }
    .row {
      display: flex;
      align-items: center;
    }
</style>

<div class="row">
  <figure style="width: 40%">
    <img alt="Astro logo" style="border: 0"
      src="/blog/assets/astro-logo.svg?v={{pkg.version}}">
    <figcaption>Astro logo</figcaption>
  </figure>
  <figure style="width: 20%">
    <img alt="Houston" style="border: 0"
      src="/blog/assets/astro-houston.png?v={{pkg.version}}">
    <figcaption>Houston</figcaption>
  </figure>
</div>

## Overview

{% aTargetBlank "https://astro.build", "Astro" %} is
"the web framework for content-driven websites".
It can be used to generate static sites,
build server-side rendered (SSR) sites,
and define API endpoints.

A major focus of Astro is shipping less JavaScript code to browsers
and doing more work on the server side.

Astro supports using many kinds of UI components including
Astro, Alpine, Lit, Preact, React, SolidJS, Svelte, Vue, WebComponents, and more.

Astro uses the {% aTargetBlank "https://docs.astro.build/en/concepts/islands/",
"Islands architecture" %}.
Jason Miller describes this approach as a way to
"render HTML pages on the server, and inject placeholders or slots
around highly dynamic regions that can then be
hydrated on the client into small self-contained widgets,
reusing their server-rendered initial HTML."
Each island is a bit of JavaScript-enabled interactivity
and the water around them is static HTML.

Astro allows each "island" to use a different web UI frameworks,
combining them into in a single web application.

Astro supports SSR adapters for Cloudflare, Netlify, Node, and Vercel.

Astro provides integrations with Tailwind for CSS styling
and a few other packages.

Astro provides file-based routing that is specified by
the files and directories under the `src/pages` directory.
For example, the URL path `/foo/bar` refers to
the page defined in the file `src/pages/foo/bar.astro`.

Astro was created by
{% aTargetBlank "http://fredkschott.com/about/", "Fred K. Schott" %}.
Fred previously worked on WebComponents at Google and was on the Polymer team.
He also created {% aTargetBlank "https://www.snowpack.dev", "Snowpack" %},
"a lightning-fast frontend build tool", that is no longer maintained.
The functionality of Snowpack was superseded by
{% aTargetBlank "https://vitejs.dev", "Vite" %}
which is used as the build tool in Astro.

## Projects

To create an Astro project, enter `npm create astro@latest`.
This will prompt for the following:

- permission to install create-astro
- "Where should we created your new project?"
  Enter a directory path that can begin with "./".
  Enter just "." to create the project in the current directory.
- "How would you like to start your new project?"
  with the options "Include sample files", "Use blog template", and "Empty".
- "Install dependencies?" Yes or No
- "Do you plan to write TypeScript?" Yes or No
- "How strict should TypeScript be? Strict, Strictest, or Relaxed
- "Initialize a new git repository? Yes or No

Once the project is created, follow the instructions that are output.

- `cd` to the newly created directory.
- Setup Typescript types by entering `npx astro sync`.
- Optionally enter `npx astro add tailwind` to add support for Tailwind CSS styling.
- Enter `npm start` or `npm run dev` to start a local server.
  Both do the same thing.
- Browse localhost:4321 (the default port).

The Astro logo is a rocket.
Astro uses 4321 for the default port because
it is like a countdown sequence for a rocket launch.

Hot reloading is automatically configured so
saved changes are automatically reflected in the browser.

The file `astro.config.mjs` defines all the Astro configuration options
including adapters and extensions (like Tailwind).

The `src/pages` directory contains component source files
that represent complete pages of the app or API endpoints.
Initially this directory will only contain the file `index.astro.`

The `src/components` directory contains component source files
that can be used in page components.
Initially this will only contain the file `index.astro.`

The `src/layouts` directory contains source files
that define the boilerplate HTML used by pages.

The `public` directory holds assets such as audio, images, and video.

To check for issues in the project code, enter `npx astro check`.
This will output errors, warnings, and hints.

To build the site for production, enter `npm run build`.
This creates a `dist` directory containing
all the files needed to deploy the site.

To preview the built site, enter `npm run preview` and browse localhost:4321.

To see a list of the available "astro" commands, enter `npm run astro`.

To open the Astro documentation in the default web browser,
enter `npm run astro docs`.

## Using VSCode

The {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=astro-build.astro-vscode",
"Astro" %} extension from astro.build provides
syntax highlighting, intellisense, and more.

To configure code formatting, open any `.astro` file,
open the Command Palette, and enter "Format Document".
It will prompt for configuring this and automatically do so.

## Imports

Astro supports importing many kinds of file in JavaScript code.
The supported file types include:

- Astro components (.astro)
- CSS (.css)
- CSS Modules (.module.css)
- Images (.svg, .jpg, .png, etc.)
- JavaScript (.js, .mjs)
- JSON (.json)
- JSX (.jsx, .tsx)
- Markdown (.md, .markdown, etc.)
- NPM Packages
- TypeScript (.ts)
- Other kinds of assets (TODO: audio and video?)

For example, the following line imports
a JSON file with the path `src/data/dogs.json`
from a `.astro` file in the `src/pages` directory:

```ts
import dogs from '../data/dogs.json';
console.log('dogs =', dogs);
```

Import paths can be relative to the current file
or be absolute from the root directory of the project.
For example, when inside the file `src/components/shopping/PetShop.astro`,
the file `src/images/animals/dog.png` can be imported in these ways:

```ts
import dogImage from '../../images/animals/dog.png';
import dogImage from '/src/images/animals/dog.png';
```

## Astro Components

Astro components are defined in source files with a `.astro` extension.
These describe HTML that will be rendered on the server.
This can contain three sections:

- optional component script

  This section begins and ends with lines that only contain three dashes,
  referred to as "code fences".
  This is is the same syntax that is used in Markdown files for "front matter".

  Write JavaScript code inside the code fences.
  If TypeScript was enabled for the project, it can be used here.

  Everything in component scripts stays on the server.
  The variables and functions defined in them are not sent to the browser.
  Output from `console.log` calls appear where the server is running,
  not in the browser.
  Place code that should run in the browser inside a `script` tag
  that appears after the component script.

  Code in a component script can:

  - import other files with ESM syntax

  - declare types, such as the type of the props it accepts

    For example:

    ```ts
    export type Props { // TODO: Does this need to use "interface" instead?
      prop1?: string; // optional prop
      prop2: number; // required prop
    }
    ```

  - use destructuring to get the values of props

    For example:

    ```ts
    const {prop1 = 'default value', prop2} = Astro.props as Props;
    ```

  - declare variables that can be used in the HTML that follows

  - declare functions that can be called in the HTML that follows

  - use the Fetch API with top-level `await` to get data from API endpoints

- HTML to be rendered

  This section uses a JSX-like syntax.

  A root element is not required,
  but the fragment syntax from React is supported (`<>...</>`).

  To insert the value of a JavaScript expression, use `{expression}`.
  This can appear in element attribute values and content.

  Conditional logic uses the same syntax as in React.
  For example, `{condition && HTML}` or `{condition ? HTML1 : HTML2}`.

  Iteration also uses the same approach as in React.
  For example, `{collection.map(element => HTML)}`.

  Expressions in curly braces are not reactive.
  `.astro` files are rendered on the server only one time.

- optional `style` tag

  This defines CSS rules that are scoped to this component.

## Naming Conventions

Files under the `src/pages` directory have lowercase names
because their names are used in URL paths.

Files under the `src/components` directory have PascalCase names
because their names become UI component names.

## Styling

As described above, components defined in `.astro` files can include
a `style` tag that defines CSS rules that are scoped to the component.

Global styles can be defined in three ways.

The first option is to add a `link` tag to the `head` section of the HTML
that refers to a CSS file.

The second option is to include a `<style is:global>` tag
in a layout source file that is used by many pages.

The third option is to define a file like `global.css`
in the `src` or `src/styles` directory and include it
in all the page components that wish to use it as follows.

```ts
import '../styles/global.css';
```

TODO: Is the third option the only one that supports hot reload of the browser
when styles are modified?

## Layouts

By convention, the `src/layouts` directory contains `.astro` files
that describe common content that should wrap around the content of pages.
Any number of layout components can be defined.
Each page can choose the layout component it wishes to wrap its content inside.

Layout components can do all the things other components can do including
taking props, importing files, and using other components.

For example, the file `src/layouts/Layout.astro` could contain the following.
Note the use of `<slot />` to specify where
content will be inserted into the layout.
Only one `slot` element can be used
and named slots are not supported.

```js
---
interface Props {
  title: string;
}
const { title } = Astro.props;
---

<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="description" content="Astro description" />
    <meta name="viewport" content="width=device-width" />
    <link rel="icon" type="image/svg+xml" href="/favicon.svg" />
    <meta name="generator" content={Astro.generator} />
    <title>{title}</title>
  </head>
  <body>
    <slot />
  </body>
</html>
<style is:global>
  html {
    font-family: system-ui, sans-serif;
  }
</style>
```

This layout can be used in a page `.astro` file as follows.
Attribute syntax is used to pass props to components.
Note how the `title` prop value is passed to the `Layout` component.

```js
---
import Layout from "../layouts/Layout.astro";
---

<Layout title="My Page">
  <main>
    <h1>My Page</h1>
  </main>
</Layout>
```

Layouts can be nested. For example, a page component `MyPage`
can wrap itself in `LayoutInner` which wraps itself in `LayoutOuter`.

## Images

Images can be placed under the `public` directory, typically in `public/images`.
These can be referenced using a path string
that is relative to the `public` directory.
For example, `<img alt="logo" src="/images/logo.png" />`
searches from the `public` directory.
Astro will server images files placed under the `public` directory as-is
and will not provide image optimization.

In order to take advantage of image optimizations,
place images under the `src/images` directory,
import them into JavaScript code,
and render them using the provided `Image` component.

For example:

```ts
---
import { Image } from "astro:assets";
import logo from ‘../images/logo.png’;
---

<Image alt="logo" src={logo} height={200} width={300} />
```

The `Image` component requires specifying both `height` and `width`
to avoid content layout shift (CLS).

From the documentation at {% aTargetBlank
"https://docs.astro.build/en/guides/images/", "Images" %},
The `Image` component
"can transform a local or authorized remote image's dimensions, file type,
and quality for control over your displayed image.
The resulting `<img>` tag includes `alt`, `loading`, and `decoding` attributes
and infers image dimensions to avoid Cumulative Layout Shift (CLS)."

Astro will also optimize remote images if the following
appears in the `astro.config.mjs` file:

```js
export default defineConfig({
  image: {
    domains: ['astro.build']
  }
});
```

Astro image optimization includes:

- adding `img` element attributes like `decoding="async"` and `loading="lazy"`.
- generating WEBP versions of images to reduce file sizes
- adding attributes required to take advantage of services like Cloudinary

Image optimization is performed by the {% aTargetBlank
"https://github.com/lovell/sharp", "sharp" %} package.

## Event Handling

Code in the component script section is only run on the server-side.
This means functions defined there
cannot be used for client-side event handling.

One way specify client-side JavaScript code is to
place it in a script tag within the HTML section.
For example:

```js
<button id="my-btn">Press Me</button>

<script>
  function handleClick() {
    alert('got click');
  }
  const myBtn = document.getElementById('my-btn');
  myBtn.addEventListener('click', handleClick);
</script>
```

## Collections

Astro supports describing and retrieving collections of data
from Markdown files.

The following steps can be taken to define and render a collection of dogs.

- Create the directory `src/content`.
- Create the file `config.ts` in this directory.
  This file defines each of the collections.
  It uses {% aTargetBlank "https://zod.dev", "Zod" %}
  to describe the schema of each collection.
  The schema defines which front matter properties are valid.
  For example, the following describes a single collection named "dogs".

  ```ts
  import {defineCollection, z} from 'astro:content';

  const dogs = defineCollection({
    type: 'content',
    schema: z.object({
      // The return value from z.string() can be saved in a variable
      // and used on multiple properties to avoid calling it repeatedly.
      name: z.string(),
      breed: z.string()
    })
  });

  export const collections = {dogs};
  ```

- Create the directory `src/content/dogs`.
- Create one Markdown file for each dog inside this directory.
  For example:

  ```md
  ---
  name: 'Comet'
  breed: 'Whippet'
  slug: https://www.akc.org/dog-breeds/whippet/
  ---

  Comet loves pool balls and basketballs.
  ```

  Note the use of front matter to describe properties of this instance.

- Access the collection in a component.
  For example:

  <img alt="Astro Dogs" style="width: 50%"
    src="/blog/assets/astro-dogs.png?v={{pkg.version}}">

  ```html
  ---
  import Layout from "../../layouts/Layout.astro";
  import Dog from "../../components/Dog.astro";
  import { getCollection, type CollectionEntry } from "astro:content";
  
  const dogs: CollectionEntry<"dogs">[] = await getCollection("dogs");
  ---

  <Layout title="Welcome to Astro">
    <main class="m-4">
      { dogs.map((dog) => (
      <Dog
        name="{dog.data.name}"
        breed="{dog.data.breed}"
        slug="{dog.slug}"
        content="{dog.body.trim()}"
      />
      )) }
    </main>
  </Layout>
  ```

- Here is the `Dog` component:

  ```ts
  ---
  interface Props {
  breed: string;
  content: string;
  name: string;
  slug: string;

  const { breed, content, name, slug } = Astro.props;
  ---

  <hr />
  <div class="my-4">
    <p class="font-bold">{name} is a {breed}.</p>
    <p>{content}</p>
    <a href={slug}>American Kennel Club</a>
  </div>
  ```

## API Endpoints

Endpoints are defined by `.js` and `.ts` files under the `src/pages` directory.
Their URLs are defined by file-based routing, just like UI pages.

Endpoints can return data in any format including
JSON and HTML (perhaps for use with HTMX).

The following code in the file `src/pages/pets/dog.json.ts`
demonstrates creating an endpoint that returns JSON created from
data found in the collection defined in the previous section.

```ts
import {getCollection, type CollectionEntry} from 'astro:content';

export async function GET() {
  const dogs: CollectionEntry<'dogs'>[] = await getCollection('dogs');
  const data = dogs.map(dog => dog.data);
  return new Response(JSON.stringify(data));
}
```

To demonstrate defining endpoints that support CRUD operations
we will see code that performs these on a collection of todo objects.
Each object has the properties `id`, `text`, and `completed`.

API endpoints are defined by the functions
`GET`, `POST`, `PUT`, `PATCH`, and `DELETE`.
Each of this take an `APIContext` object
that contains the following properties:

- `params`: an object containing properties
  that match the dynamic segments of the route.
- `props`: an object containing properties
  supplied by the getStaticPaths function
  (only available in server-side rendering)
- `request`: a Request object that contains
  the method, url, headers, and body
- `clientAddress`
- `cookies`
- `generator`
- `locals`
- `redirect`
- `site`
- `url`

We need to share the collection of todos between two source files.
One way to accomplish this is to create a source file
that creates and exports the collection.
This file can be imported by other source files
that need to access the collection.
The following file in `src/pages/todo-state.ts` does this.

```ts
type Todo = {
  id: number;
  text: string;
  completed: boolean;
};

export const todoMap = new Map<number, Todo>();
```

The endpoints defined in `src/pages/todos.ts` do two things:

- retrieve all the todos as a JSON array
- create a new todo, returning its JSON

```ts
import type {APIContext} from 'astro';
import {todoMap} from './todos-state.ts';

let lastId = 0; // used by addTodo and POST functions

// Add some initial todos.
function addTodo(text: string) {
  const todo = {id: ++lastId, text, completed: false};
  todoMap.set(todo.id, todo);
}
addTodo('buy milk');
addTodo('cut grass');

export async function GET() {
  const todos = [...todoMap.values()];
  return new Response(JSON.stringify(todos), {
    headers: {'Content-Type': 'application/json'}
  });
}

export async function POST({request}: APIContext) {
  const todo = await request.json();
  if (todo.completed === undefined) todo.completed = false;
  const id = ++lastId;
  todo.id = id;
  todoMap.set(id, todo);
  return new Response(JSON.stringify(todo), {status: 201});
}
```

The endpoints defined in `src/pages/todos/[id].ts` do four things:

- retrieve a todo as JSON
- update a todo with JSON
- patch a todo with JSON
- delete a todo

Files will square brackets in the name
define dynamic routes where path parameters are used.
In this case the path parameter is the id of a todo.
Dynamic routes require enabling SSR.
To do this, enter `npx astro add node`.

```ts
import type {APIContext} from 'astro';
import {todoMap} from '../todos-state.ts';

export async function GET({params}: APIContext) {
  const {id} = params;
  const idNumber = Number(id);
  const todo = todoMap.get(idNumber);
  return new Response(JSON.stringify(todo), {
    headers: {'Content-Type': 'application/json'}
  });
}
export async function PUT({params, request}: APIContext) {
  const {id} = params;
  const idNumber = Number(id);
  const todo = await request.json();
  todo.id = idNumber; // ensures the id matches the path parameter
  const exists = todoMap.has(idNumber);
  if (exists) todoMap.set(idNumber, todo);
  const status = exists ? 200 : 404;
  return new Response(JSON.stringify(todo), {status});
}

export async function PATCH({params, request}: APIContext) {
  const {id} = params;
  const idNumber = Number(id);
  const updates = await request.json();
  updates.id = idNumber; // ensures the id matches the path parameter
  let todo = todoMap.get(idNumber);
  if (todo) {
    todo = {...todo, ...updates};
    todoMap.set(idNumber, todo);
  }
  const status = todo ? 200 : 404;
  return new Response(JSON.stringify(todo), {status});
}

export async function DELETE({params, request}: APIContext) {
  const {id} = params;
  if (!id) return new Response('missing "id" parameter', {status: 400});

  const idNumber = Number(id);
  const status = todoMap.delete(idNumber) ? 200 : 404;
  return new Response('', {status});
}
```

When starting the server, include the `--host` option
to enable using `localhost` in URLs that hit the endpoints.
For example, the `dev` and `start` scripts in `package.json`
should match the following:

```json
    "dev": "astro dev --host",
    "start": "astro dev --host",
```

For more detail, see {% aTargetBlank
"https://docs.astro.build/en/core-concepts/endpoints/#server-endpoints-api-routes",
"Server Endpoints (API Routes)" %}.

## MDX

The MDX extension enables using components inside Markdown files.
To install this in an Astro project, enter `npx astro add mdx`.

If VS Code is being used, install the {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=unifiedjs.vscode-mdx",
"MDX" %} extension from unified.

Markdown files that wish to render components
must have the file extension `.mdx`.

For example, here is a component definition in `src/components/Greet.astro`:

```html
---
const { name } = Astro.props;
---

<p class="text-bold text-red-500">Hello, {name}!</p>
```

And here is a content file in `src/content/dogs/comet.mdx`.
Note how it imports and uses the `Greet` component:

```html
---
name: 'Comet'
breed: 'Whippet'
website: https://www.akc.org/dog-breeds/whippet/
---

import Greet from "../../components/Greet.astro"; He loves the following: - pool
balls - basketballs - frisbees
![Whippet](https://www.akc.org/wp-content/uploads/2017/11/Whippet-On-White-01.jpg)

<Greet name="Comet" />
```

## View Transitions

Astro supports adding {% aTargetBlank
"https://docs.astro.build/en/guides/view-transitions/", "view transitions" %}
that are applied when navigating from one page to another.
This includes clicking links implemented with `<a>` elements
and triggering the browser forward and back buttons.
For more control over when transitions occur, see {% aTargetBlank
"https://docs.astro.build/en/guides/view-transitions/#router-control",
"router control" %}.

This feature is built on the Web {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/View_Transitions_API",
"View Transitions API" %}.
As of December 2023, the only major web browsers that support this
are Chrome and Edge.

Astro disables all view transitions when
the "prefer-reduce-motion" setting is enabled.
This is a CSS media feature that detects
an operating system specific setting.
For example, in macOS this is configured in the Settings app under
Accessibility ... Display ... Reduce motion.

For basic fade out, fade in transitions between all pages,
modify the layout files used by all the pages as follows.
This specifies the transition that should occur when leaving a page.

```html
---
import { ViewTransitions } from "astro:transitions";
...

---

<html>
  <head>
    ...
    <ViewTransitions />
  </head>
  ...
</html>
```

The built-in transitions include:

- `fade`

  The current page fades out and new page fades in.

- `initial`

  This uses the browser default transition.

- `slide`

  The current page slides out to the left and new page slides in from the right.
  The opposite occurs when navigating back to the previous page.

To specify a transition on a specific element
(which can be the root element of a page),
add the attribute `transition:animate="{transition-type}"`.
For example, `<main transition:animate="slide">`.

To customize the transition,
pass a configuration object to the transition function.
For example, `<main transition:animate={slide({ duration: '2s' })}>`.

To define a custom transition, create an object that
conforms to the `TransitionDirectionalAnimations` interface
which requires `forwards` and `backwards` properties.
Those properties must be objects that
conform to the `TransitionAnimation` interface
which requires `old` and `new` properties.

For example:
TODO: GET THIS TO WORK!

```ts
const spinAnim = {
  old: {
    name: 'spinOut',
    duration: '1s',
    easing: 'linear',
    fillMode: 'forwards'
  },
  new: {
    name: 'spinIn',
    duration: '1s',
    easing: 'linear',
    fillMode: 'backwards'
  }
};

const spin = {
  forwards: spinAnim,
  backwards: spinAnim
};
```

## Internationalization

Astro supports {% aTargetBlank
"https://docs.astro.build/en/guides/internationalization/",
"internationalization" %}.

The Astro approach is it to duplicate each component for each supported language
and store them in directories whose names are language codes.
This is a bad approach because any changes require modifying multiple files.

TODO: Describe how to use this.

## Starlight

{% aTargetBlank "https://starlight.astro.build", "Starlight" %}
is a documentation theme built on Astro.

TODO: Describe how to use this.

## Resources

- {% aTargetBlank "https://astro.build", "Astro Home Page" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=XoIHKO6AkoM",
  "Astro Quick Start Course" %} by Traversy Media

## Unorganized Content

is there a syntax to import files starting from the src directory to avoid using ../ ?
can create the file src/constants.ts that export site-wide constants.
import and use these where needed.
don’t need to export the definition of the Props type in a component.
can you define the Props type using either the interface or type keyword?
to create a custom 404 page, add the file src/pages/404.astro.
This page can also import and use layouts and other components.

you can define collections of data as Markdown files under the src/content directory.
create the file config.ts under the src/content directory.
This file … see email on defining collections.
The schema for collections uses zod.

when do you need to run npx astro sync ?
this sets up typescript types. Does it do anything else?

to create dynamic routes, create files that have square brackets containing a name in the file name. these can be .astro files for components or .ts files for endpoints.

what are the benefits of using the tailwind typography plug-in?

what does this enable? using JavaScript components inside Markdown?
npx astro add mdx
need to change file extensions to .mdx

astro add svelte
enables use of Svelte in .astro files?

learn about enabling SSR so pages are not generated at build time and are instead generated on demand. Is that how it works?

learn about Astro support for pagination.
