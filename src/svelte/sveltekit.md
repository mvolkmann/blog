---
eleventyNavigation:
  key: SvelteKit
  parent: Svelte
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://kit.svelte.dev", "SvelteKit" %}
is a framework that leverages Svelte
and is a replacement for previous framework Sapper.
It is similar to Next for React and Nuxt for Vue.

The main features of SvelteKit are:

- file-based page routing
- file-based endpoints (REST services)
- layouts (ex. common page header, footer, and nav)
- error page
- code splitting for JS and CSS
- first page visit is server-rendered for performance
  and remaining pages are rendered in the browser
- page visits only load the JS and CSS they need
- hot reloading (provided by Vite; very fast!)
- can build static sites and individual static pages
- offline support with ServiceWorkers
- CLI tool that provides:
  - setup of TypeScript
  - setup of Sass or Less CSS preprocessors
  - setup of ESLint
  - setup of Prettier
- adapters for specific deployment targets

## Getting Started

To create a new SvelteKit project:

1. Install Node.js

1. Enter `npm create svelte@latest [project-name]`  
   Omit the project name to create a SvelteKit project in the current directory.

1. Answer the following question:

   - Which SvelteKit app template? "Skeleton project" is recommended.
   - Add type checking with TypeScript? "Yes, using TypeScript syntax" is recommended.
   - Select additional options. All are recommended.

1. Following instructions for next steps that are output.

   - Enter `cd project-name`
   - Enter `npm install`

To run a SvelteKit project:

1. `cd` to the project root directory.

1. To run in development mode, enter `npm run dev`  
   This provides watch and live reload.
   Optionally add options after `--`.
   The `--open` or `-o` option opens the app in the default web browser.  
   The `--port` or `-p` option specifies a port listen on
   which defaults to 3000.

1. To run the ESlint linter, enter `npm run lint`

1. To run the Prettier code formatter, enter `npm run format`

## TypeScript

The TypeScript types used by SvelteKit can be found in the files
in the `node_modules/@sveltejs/kit/types` directory.
Routes that need to load data before they are rendered
do so by defining a `load` function in their module context.
The `load` function has a single parameter whose type is `LoadInput`
and it returns a `Promise<LoadOutput>`.

## File-based Page Routing

SvelteKit supports two kinds of routes, pages and endpoints.
This section describes page routes.
Endpoint routes are described in the next section.

The page routes of a SvelteKit app are defined by
files and directories inside the `src/routes` directory.
These can be rendered on the server or in the browser,
depending on configuration.

There are two ways to define a new page route.
Suppose the desired route name is "some-route".
The first approach is to add the file `src/routes/some-route.svelte`
that describes a Svelte component to render.

The second approach is to add the directory `src/routes/some-route`
with the file `index.svelte` inside it
that describes a Svelte component to render.
This approach is often used when there are other route-specific
source files so they can be grouped in the same directory.

In both cases the route is rendered by adding `/some-route`
to the end of the base URL for the app.

Routes that require loading data before they are rendered
can do so by defining a `load` function in their module context.
For example:

```ts
<script context="module" lang="ts">
  import type {LoadInput, LoadOutput} from '@sveltejs/kit/types';

  export async function load({context, fetch, page, session}: LoadInput): Promise<LoadOutput> {
    const dogId = Number(page.params.someId);

    let res = await fetch(`/api/dog/${dogId}`);
    const dog = await res.json();

    return {props: {dog}};
  }
</script>

<script lang="ts">
  import type {Dog} from '$lib/types';

  export let dog: Dog;
</script>

<h2>Dog</h2>

<p>{dog.name} is a {dog.breed}.</p>
```

The `LoadInput` object passed to the `load` function
contains four properties.

The `context` property is a `Context` object.

The `fetch` property is a function used to send an HTTP request.
`fetch: (info: RequestInfo, init?: RequestInit) => Promise<Response>`
This can be called to send an HTTP request to any server,
including the one provided by SvelteKit
where endpoints (described next) are hosted.

The `page` property is a `Page<PageParams>` object.

The `session` property is a `Session` object.

The file `src/routes/index.svelte` defines
the page to render at the `/` route.
To render this page when running the app locally
on the default port, browse `localhost:3000`.

Nested subdirectories under `src/routes` define routes
at URL paths with multiple path parts.
Directories with names that begin with `[` and end with `]`
represent path parameters (a.k.a dynamic parameters).
For example, suppose a page is defined in the file
`src/routes/person/[personId]/dog/[dogId]/index.svelte`.
To render the page for the person with id 19 and the dog with id 2,
browse `localhost:3000/person/19/dog/2`.

## File-based Endpoints (REST services)

Endpoints are defined by creating `.js` or `.ts` files
under the `src/routes` directory.
This code is executed by a provided Node.js server,
or a build time for pre-rendered pages.
It is never executed in a browser.

Endpoints can access data from sources like databases.
They return data as JSON by default,
but other data formats can also be returned.

There are two common conventions for defining endpoints.
One is to create an `api` directory in `src/routes`
and create files in that directory and below.
Another is to create files with a `.json.js` or `.json.ts` extension.
TODO: Try the second option to verify.

TODO: Finish this

## Recommended Directory Structure

This is my opinion on what makes a good directory structure
for a SvelteKit project.

- `.env` defines environment variables with names starting with VITE\_
- `src`
  - `app.html` is the topmost HTML file (import third-party CSS files here)
  - `global.css` defines CSS rules that can affect all components
  - `lib`
    - `env.ts` imports environment variables from `.env` file
      and exports them so Svelte components can use them
    - shared JS functions
    - components used by multiple pages
    - `stores.ts` defines stores used by multiple page
  - `routes`
    - `index.svelte` defines the home page (import `global.css` here?)
    - `api`
      - directories and files for file-based API routes
    - `page-name-1`
      - `stores.ts` defines stores used only by this page
      - `types.ts` defines types used only by this page
      - `index.svelte` defines the top level of the page
      - components used only by this page
    - `page-name-2`
      - `stores.ts` defines stores used only by this page
      - `types.ts` defines types used only by this page
      - `index.svelte` defines the top level of the page
      - components used only by this page

## Layouts

TODO: Finish this

## Error Page

TODO: Finish this

## Code Splitting

TODO: Finish this

## Hot Module Reloading

TODO: Finish this

## Static Pages and Sites

TODO: Finish this

## Tool Setup

Creation of a new SvelteKit project prompts for desired features.
It asks the following questions:

"Use TypeScript in components?" This defaults to no.

"What do you want to use for writing Styles in Svelte components?"
The options are CSS (default), Less, and SCSS.

"Add ESLint for code listing?" This defaults to no.

"Add Prettier for code formatting?" This defaults to no.

## Path Aliases

SvelteKit provides directory aliases.
These can only be used in import path strings.
They are not variables.

`$app` is used to import various modules described below.

`$lib` is an alias to the `src/lib` directory.
It is used to import your own source files
without using relative paths.

## Provided Modules

### `$app/env`

To use this module, import the desired items as follows:

`import { amp, browser, dev } from '$app/env';`

`amp` is a boolean that indicates whether the app uses
{% aTargetBlank "https://developers.google.com/amp",
"Accelerated Mobile Pages (AMP)" %} created by Google.

`browser` is a boolean that indicates whether
the code is running in the browser.
When the value is `false`, the code is running on the server.

`dev` is a boolean that indicates whether
the code is running in development mode.
When the value is `false`, the code is running in production mode.

### `$app/navigation`

To use this module, import the desired items as follows:

`import { goto, prefetch, prefetchRoutes } from '$app/navigation';`

`goto` is a function that programmatically navigates to a new route.
This is preferred over setting `location.href`.

`prefetch` is a function that programmatically
prefetches the page at a given route.
It can be called before calling `goto` to improve performance.
This is appropriate when it can be anticipated
that the user will want to go to the page soon.
It is the same functionality that is triggered by
hovering over a link that uses the `sveltekit:prefetch` attribute.

`prefetchRoutes` is a function that programmatically
prefetches a set of pages at given routes or all pages.

### `$app/paths`

To use this module, import the desired items as follows:

`import { base, assets } from '$app/paths';`

These are directory paths that are relative to the root project directory.
The default to that directory, but can be modified in `svelte.config.cjs`
at the property paths `kit.paths.assets` and `kit.paths.base`.
Changing these doesn't seem very useful.

### `$app/stores`

To use this module, import the desired items as follows:

`import { getStores, navigating, page, session } from '$app/stores';`

`getStores` is a function that returns
an object containing the other three values.
This doesn't seem useful.

`navigating` is a readable store whose value is `null` when not navigating
and an object with `from` and `to` properties when navigating.
TODO: Why does this seem to always be null?

`page` is a readable store that holds the data passed to page `load` functions,
including `host`, `path`, `params`, and `query`
for getting information about a request.

`session` is a writable store that holds session data.
However, setting it doesn't not cause data to be persisted on the server.
If desired, that must be implemented.

### `$service-worker`

To use this module, import the desired items as follows:

`import { build, files, timestamp } from '$service-worker';`

`build` is an array of URLs for the files generated by Vite.
These files can be cached by calling `cache.addAll(build)`.

`files` is an array of URLs for the files in directory specified by
`kit.files.assets` in the `svelte.config.cjs` file which defaults to `static`.

`timestamp` is the value of `Date.now()` (milliseconds) at build time.
It can be used generate a unique cache name which enables
invalidating the caches of subsequent deploys.

## <a name="adapter">Adapters</a>

The provided adapter targets currently include
Node.js, static, Begin, Netlify, and Vercel.
New SvelteKit projects default to using the Node adapter.
To change the adapter, modify the `svelte.config.cjs` file.

### Netlify Example

- cd to the root directory of a SvelteKit project.

- Install the Netlify SvelteKit adapter with
  `npm install -D @sveltejs/adapter-netlify@next`

  This deploys SvelteKit endpoints as serverless functions in AWS
  without requiring an AWS account.

- Edit `svelte.config.cjs`

  - Change the first line to the following:

  ```js
  const adapter = require('@sveltejs/adapter-netlify');
  ```

  - Change the "adapter" line to the following:

    ```js
    adapter: adapter(),
    ```

- Create the file `netlify.toml` in the project root directory
  containing the following:

  ```toml
  [build]
  command = "npm run build"
  functions="functions/"
  publish="build/"
  ```

- Create a GitHub repository to hold the project files,
  add all the files, and push.

- Create a Netlify account if you don't already have one.

  Browse <https://www.netlify.com> and click "Sign up".

- Login to your Netlify account.

- Press the "New site from Git" button.

- Press the "GitHub" button.

- Select the repository created above.

  If you configured your account to only
  provide access to selected repositories,
  click the "Configure the Netlify app on GitHub" link,
  click "Configure", scroll down to "Repository access",
  click the "Select repositories" drop down,
  search for the repository, select it, and press the "Save" button.

- Verify the settings displayed (defaults should be fine)
  and press the "Deploy site" button.

- Wait for deployment to complete
  and note its URL.

- Browse the URL to run the app.

Note: I was expecting each SvelteKit endpoint to
show up as a separate Netlify function.
But instead I see just one Netlify function named "render".
I know it's working because if I add a `console.log`
in the code of each of my endpoints, push the changes,
wait for it to be redeployed to Netlify,
run the new version of app to trigger calls to the endpoints,
and then click that "render" function in Netlify,
I do see the expected output from the `console.log` calls.
It seems odd that all the endpoints are bundled into one Netlify function.
