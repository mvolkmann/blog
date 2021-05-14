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

For production builds, Vite uses Rollup to create a bundle file.

The name comes from the French word which means "fast"
and is pronounced "vēt".

Another build to tool that is similar to Vite is
{% aTargetBlank "https://www.snowpack.dev", "Snowpack" %}.

{% aTargetBlank "https://kit.svelte.dev", "SvelteKit" %}
adds many features on top of Svelte including
the use of Vite, file-based page routing, code splitting,
hot module reloading, and server-side rendering.
If you if you create a SvelteKit project,
by entering `npm init svelte@next {project-name}`,
it will already be configured to use to Vite.
But if you want to create a plain Svelte project that uses Vite,
follow these steps:

- Enter `npm init @vitejs/app`
- Enter a project name which will be the name of the created directory.
- Select "svelte" for the framework.
  Other options include lit-element, preact, react, vanilla, and vue.
- Select a variant: JavaScript or TypeScript
- `cd` to the project directory.
- Enter `npm install` to install all of the dependencies.

To run in development mode:

- Enter `npm run dev` to start the development server
  that provides watch and hot module reloading.

To build and run in production mode:

- Enter `npm run build`
- Enter `npm run serve`
