---
eleventyNavigation:
  key: Astro
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

<figure style="width: 40%">
  <img alt="Astro logo" style="border: 0"
    src="/blog/assets/astro-logo.svg?v={{pkg.version}}">
</figure>

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

Astro uses the Island architecture which enables combining
the use of multiple web UI frameworks in a single application.
HTML is rendered on the server and with placeholders for dynamic regions.
These placeholders are hydrated on the client-side.

Astro supports SSR adapters for Cloudflare, Netlify, Node, and Vercel.

Astro provides integrations with Tailwind for CSS styling
and a few other packages.

Astro provides file-based routing that is specified by
the files and directories under the `src/pages` directory.
For example, the path `/foo/bar` refers to
the page defined in the file `src/pages/foo/bar.astro`.

Astro was created by
{% aTargetBlank "http://fredkschott.com/about/", "Fred K. Schott" %}.
Fred previously worked on WebComponents at Google and was on the Polymer team.
He also created {% aTargetBlank "https://www.snowpack.dev", "Snowpack" %},
"a lightning-fast frontend build tool", that is no longer maintained.
The functionality of Snowpack was superseded by
{% aTargetBlank "https://vitejs.dev", "Vite" %}
which is used as the build tool in Astro.

## Creating a Project

Enter `npm create astro@latest`.
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
- Enter `npm run dev` or `npm run start` to start a local server.
  Both do the same thing.
- Browse localhost:4321 (the default port).

Hot reloading is automatically configured so
saved changes are automatically reflected in the browser.

The file `astro.config.mjs` defines all the Astro configuration options
including adapters and extensions (like Tailwind).

The `src/pages` directory contains component source files
that represent complete pages of the app.
Initially this will only contain the file `index.astro.`

The `src/components` directory contains component source files
that can be used in page components.
Initially this will only contain the file `index.astro.`

The `src/layouts` directory contains source files
that define the boilerplate HTML used by pages.

The `public` directory holds assets such as audio, images, and video.

To build the site for production, enter `npm run build`.

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

## Styling

As described above, components defined in `.astro` files can include
a `style` tag that defines CSS rules that are scoped to the component.

Global styles can be defined in two ways.
One way is to include a style tag in a layout source file
that is used by many pages.
TODO: Does this need to use `<style is:global>`?
Another way is to define a file like `global.css`
in the `src` or `src/styles` directory and include it
in all the page components that wish to use it as follows.

```ts
import '../styles/global.css';
```

## Layouts

The `src/layouts` directory can contain `.astro` files
that describe common content that should appear in many pages.

For example, the file `src/layouts/Layout.astro` could contain the following.
Note the use of `<slot />` to specify where
content will be inserted into the layout.
TODO: Are named slots supported?

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

## Images

Images can be placed under the `public` directory, typically in `public/images`.
These can be referenced using a path string
that is relative to the `public` directory.
For example, `<img alt="logo" src="/images/logo.png" />`
searches from the public directory.
Astro will not provide image optimization for images in this location.

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

<Image alt="logo" src={logo} width={300} />
```

From the documentation at {% aTargetBlank
"https://docs.astro.build/en/guides/images/", "Images" %},
The `Image` component
"can transform a local or authorized remote image’s dimensions, file type,
and quality for control over your displayed image.
The resulting `<img>` tag includes `alt`, `loading`, and `decoding` attributes
and infers image dimensions to avoid Cumulative Layout Shift (CLS)."

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

Endpoints can return data in any format including JSON and HTML (for HTMX).

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

Endpoints can use path and query parameters.
The following code in the file `src/pages/pets/[id].json.ts`
demonstrates creating an endpoint that returns JSON for a specific dog.

```ts

```

For more detail, see {% aTargetBlank
"https://docs.astro.build/en/core-concepts/endpoints/#static-file-endpoints",
"Static File Endpoints" %}.

## Resources

- {% aTargetBlank "https://astro.build", "Astro Home Page" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=XoIHKO6AkoM",
  "Astro Quick Start Course" %} by Traversy Media

## Unorganized Content

can define multiple layout files to use different ones and specific pages.
is it the case that page components cannot use the slot element and only layout components can do that?
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
