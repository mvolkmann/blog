---
eleventyNavigation:
  key: Vite
  parent: Svelte
  order: 9
layout: topic-layout.njk
---

{% aTargetBlank "https://vitejs.dev", "Vite" %} is a build tool
that can be used as a replacement for Webpack, Rollup, and Parcel.
It differs from those in that it uses ES modules to avoid
having to create a new, all-encompassing bundle of JavaScript code
after every code change.
This makes it significantly faster.
However, Vite uses Rollup to create production builds.

The name comes from the French word which means "fast"
and is pronounced "vēt".

Another build to tool that is similar to Vite is
{% aTargetBlank "https://www.snowpack.dev", "Snowpack" %}.

To create a Svelte project that uses Vite,

- Enter `npm init @vitejs/app`
- Enter a project name which will be the name of the created directory.
- Select a framework such as "svelte".
- Select a variant: JavaScript or TypeScript
- `cd` to the project directory.
- Enter `npm install` to install all of the dependencies.

To run in development mode:

- Enter `npm run dev` to start the development server
  that provides watch and hot module reloading.

To build and run in production mode:

- Enter `npm run build`.
- Enter `npm run serve`.
