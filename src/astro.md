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
- Enter `npm run dev` to start a local server.
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

## Resources

- {% aTargetBlank "https://astro.build", "Astro Home Page" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=XoIHKO6AkoM",
  "Astro Quick Start Course" %} by Traversy Media

## Unorganized Content
