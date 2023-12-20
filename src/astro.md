---
eleventyNavigation:
  key: Astro
layout: topic-layout.njk
---

<figure style="width: 60%">
  <img alt="Astro logo" style="border: 0"
    src="/blog/assets/astro-logo.png?v={{pkg.version}}">
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

Astro was created by
{% aTargetBlank "http://fredkschott.com/about/", "Fred K. Schott" %}.
Fred previously worked on WebComponents at Google and was on the Polymer team.
He also created {% aTargetBlank "https://www.snowpack.dev", "Snowpack" %},
"a lightning-fast frontend build tool", that is no longer maintained.
The functionality of Snowpack was superseded by
{% aTargetBlank "https://vitejs.dev", "Vite" %}.

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

## Astro Components

Astro components are defined in source files with a `.astro` extension.
These describe HTML that will be rendered on the server.
This can contain three sections:

- optional front matter

  This section begins and ends with a line containing only three dashes,
  referred to as "code fences".
  Between these, write JavaScript code.
  If TypeScript was enabled for the project, it can be used here.
  This code can:

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

  To insert the value of a JavaScript expression, use `{expression}`.

  Conditional logic uses the same syntax as in React.
  For example, `{condition && HTML}` or `{condition ? HTML1 : HTML2}`.

  Iteration also uses the same approach as in React.
  For example, `{collection.map(element => HTML)}`.

  Expressions in curly braces are not reactive.
  The `.astro` files are rendered on the server only one time.

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

## Event Handling

Components defined in `.astro` files must configure event handling
in JavaScript code. This is gross! For example:

```js
---
function handleClick() {
  alert('got click');
}
const myBtn = document.getElementById('my-btn');
myBtn.addEventListener('click', handleClick);
---
<button id="my-btn">Press Me</button>
```

## API Endpoints

Endpoints are defined by `.js` and `.ts` files.

For details, see {% aTargetBlank
"https://docs.astro.build/en/core-concepts/endpoints/#static-file-endpoints",
"Static File Endpoints" %}.

## Resources

- {% aTargetBlank "https://astro.build", "Astro Home Page" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=XoIHKO6AkoM",
  "Astro Quick Start Course" %} by Traversy Media

## Unorganized Content
