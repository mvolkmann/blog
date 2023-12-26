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
    <figcaption>Houston,<br />the Astro mascot</figcaption>
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

Development of Astro is managed by "The Astro Technology Company"
which was founded in January 2022 with $7M in seed funding.

## Top Benefits

The top benefits of using Astro include:

- sends less JavaScript code to browsers resulting in faster startup
- file-based routing simplifies mapping pages and endpoints to URLs
- provides image optimization
- makes it easy to generate static sites
- interactive functionality is contained in "dynamic islands"
  to optimize static content
- supports TypeScript for detecting errors while writing code
- can use components implemented in all the popular web frameworks
- can use content collections to easily
  generate static pages from data at build time
- supports implementing API endpoints in JavaScript or TypeScript
- supports many integrations such as
  Alpine, MDX, React, Svelte, Tailwind, and more.

## Projects

To create an Astro project, enter `npm create astro@latest`.
This will prompt for the following:

- permission to install create-astro

- "Where should we created your new project?"

  Enter the name of a new subdirectory to be created.
  To create the project in the current directory, enter only ".".
  To enter a relative path, begin with "./".

- "How would you like to start your new project?"

  The options are "Include sample files", "Use blog template", and "Empty".

- "Install dependencies?" Yes or No

  Press return to accept the default of "Yes".

- "Do you plan to write TypeScript?" Yes or No

  Press return to accept the default of "Yes".

- "How strict should TypeScript be? Strict, Strictest, or Relaxed

  The default is "Strict", but its best to select "Strictest".

- "Initialize a new git repository? Yes or No

Once the project is created, follow the instructions that are output.

- `cd` to the newly created directory.
- Optionally add the Taiwind integration for Tailwind CSS styling
  by entering `npx astro add tailwind`.
- Enter `npm start` or `npm run dev` to start a local server.
  Both do the same thing.
- Browse localhost:4321 (the default port).

The Astro logo is a rocket.
Astro uses 4321 for the default port because
it is like a countdown sequence for a rocket launch.

Hot reloading is automatically configured so
saved changes are automatically reflected in the browser.

To check for issues in the project code,
enter `npx astro check` or `npm run astro check`.
This will output errors, warnings, and hints.

To build the site for production, enter `npm run build`.
This creates a `dist` directory containing
all the files needed to deploy the site.

To preview the built site, enter `npm run preview` and browse localhost:4321.

To see a list of the available "astro" commands, enter `npm run astro`.

To open the Astro documentation in the default web browser,
enter `npm run astro docs`. For help on a specific topic,
click in the search input or press the slash key to move focus there.
Then enter a topic.

### Configuration

The file `astro.config.mjs` defines Astro configuration options
including adapters (like node) and integrations (like Tailwind).

While this file can be manually modified,
it is easier and less error prone to add adapters and integrations
using the `npx astro add {integration-name}` command which
installs the integration package and updates the `astro.config.mjs` file.

The following example configuration file adds the use of
the node adapter and the mdx and tailwind integrations.

```ts
import {defineConfig} from 'astro/config';
import mdx from '@astrojs/mdx';
import node from '@astrojs/node';
import tailwind from '@astrojs/tailwind';

export default defineConfig({
  integrations: [mdx(), tailwind()],
  output: 'server',
  adapter: node({
    mode: 'standalone'
  })
});
```

## VS Code

If you use VS Code as your editor, consider installing these extensions:

- {% aTargetBlank "https://marketplace.visualstudio.com/items?itemName=astro-build.astro-vscode",
  "Astro" %} from astro.build

  This provides language support for `.astro` files
  using the Astro language server.
  It includes syntax highlighting, intellisense code completions,
  Emmet completions in HTML and CSS
  code actions for quick fixes,
  code formatting (using Prettier),
  code folding,
  and more.

- {% aTargetBlank "https://marketplace.visualstudio.com/items?itemName=unifiedjs.vscode-mdx",
  "MDX" %} from unified

  This provides language support for `.mdx` files.

- {% aTargetBlank "https://marketplace.visualstudio.com/items?itemName=astro-build.houston",
  "Houston" %} from astro.build

  Houston is the Astro mascot.
  This provides a color theme using Astro colors which include
  "cool blues, minty greens, and soft purples".
  It also adds a "HOUSTON" section to the Explorer pane
  which becomes visible after restarting VS Code.
  This just displays the mascot.
  If will have a smiley face if the project has no errors,
  a frown if there are minor errors,
  and a sad face if there are more serious errors.

- {% aTargetBlank "https://marketplace.visualstudio.com/items?itemName=bradlc.vscode-tailwindcss",
  "Tailwind CSS Intellisense" %} from tailwindcss.com

  This provides autocomplete, syntax highlighting, and linting
  for Tailwind CSS classes.
  When entering Tailwind class names for colors,
  it provides color preview swatches.

## Prettier

The steps to configure an Astro project to use Prettier are:

- Enter `npm install -D prettier-plugin-astro`
- Create the file `.prettierrc` at the root of the project.

  Some suggested options are shown here.
  The only one that is required is `plugins`.

  ```json
  {
    "arrowParens": "avoid",
    "astroAllowShorthand": true,
    "bracketSpacing": false,
    "singleQuote": true,
    "trailingComma": "none",
    "plugins": ["prettier-plugin-astro"]
  }
  ```

- Add the following script in `package.json`.

  ```json
  "format": "prettier --write '{public,src}/**/*.{astro,css,html,js,ts}'",
  ```

To format all the files in the project, enter `npm run format`.

## Dev Toolbar

When running in dev mode, Astro provides a Dev Toolbar in
a dark gray oval that is centered at the bottom of the browser window.
It is partially hidden from view until the mouse hovers over it.

<img alt="Astro Dev Toolbar" style="border: none; width: 30%"
  src="/blog/assets/astro-dev-toolbar.png?v={{pkg.version}}" />

The toolbar contains the following four buttons:

- Menu (Astro icon)

  This provides options to "Report a Bug", provide "Feedback",
  view "Documentation", and join the "Community" (on Discord).
  It also features some optional integrations.
  The "View all" link opens a browser tab
  for viewing all available integrations.

  The "Copy debug info" button in the upper-right
  copies information to the clipboard that is useful when reporting a bug.
  This includes the version of Astro, version of Node, operating system,
  package manager (ex. npm), the adapter in use (ex. @astrojs/node),
  and a list of the installed integrations.
  The copied text can be pasted into a bug report.

- Inspect (arrow cursor icon)

  This enables inspecting interactive components that are
  marked with a `client:*` directive.

  Click this button to display an outline around
  all the interactive components on the page.
  Hover over one to get a dialog that shows:

  - the specific `client:*` directive that was applied
  - the props that were passed to the component
  - a link that be clicked to open the source file for the component in VS Code

- Audit (document icon)

  This scans the page for accessibility issues.
  Elements that have issues are given a purple outline.
  Hover over them to see a dialog that describes the issues.

- Settings (gear icon)

  This displays a dialog for modifying settings.
  There are currently only two settings,
  "Verbose logging" and "Disable notifications",
  both of which are off by default.

To close any dialog displayed by the Dev Toolbar, press the escape key.

If having access to the Dev Toolbar is not desired,
it can be disabled by entering `npx astro preferences disable devToolbar`.
It can be enabled again by entering `npx astro preferences enable devToolbar`.

## Directory Structure

The `public` directory holds assets such as audio, images, and video
that will be served as-is and not affected by any optimizations.

Common subdirectories of the `src` directory are described in the table below.
The only special names are `pages` and `content`.
There are common names used for the others, but they are not enforced.

| Directory Name   | Purpose                                                                                                                                  |
| ---------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `src/components` | This contains component source files that can be used in page components.                                                                |
| `src/content`    | This holds collections of Markdown files.                                                                                                |
| `src/images`     | This holds images that will be used with the provided `Image` component in order to optimized them.                                      |
| `src/layouts`    | This holds component source files that typically provide boilerplate HTML used by pages.                                                 |
| `src/pages`      | This contains component source files that represent complete pages of the app or API endpoints. Initially only `index.astro` is present. |
| `src/styles`     | This contains CSS files that define global styling.                                                                                      |

It is useful to define path aliases in `tsconfig.json`
to simplify importing files from commonly used directories.
For example:

```json
{
  "extends": "astro/tsconfigs/strictest",
  "compilerOptions": {
    "baseUrl": "./src",
    "paths": {
      "@components/*": ["components/*"],
      "@images/*": ["images/*"],
      "@layouts/*": ["layouts/*"]
    }
  }
}
```

This enables replacing an import like

```js
import Layout from '../../../layouts/Layout.astro';
```

with this:

```js
import Layout from '@layouts/Layout.astro';
```

For site-wide constants, consider creating the file `src/constants.ts`
that export the constants.
In files that need the constants, import them from this file.

## Using VSCode

The {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=astro-build.astro-vscode",
"Astro" %} extension from astro.build provides
syntax highlighting, intellisense, and more.

To configure code formatting, open any `.astro` file,
open the Command Palette, and enter "Format Document".
It will prompt for configuring this and automatically do so.

## Pages

Pages are defined my files in the `src/pages` directory.
They can be described by Astro (`.astro`), Markdown (`.md`),
or MDX (`.mdx`) files.
They cannot be described by components from frameworks like React and Svelte,
but pages can render those kinds of components.

The URL path of a page is determined by its subdirectory path and file name.

For example, the file `src/pages/demo/colors.md`
defines the page at the URL path `/demo/colors/`.

<img alt="Astro colors page" style="width: 25%"
    src="/blog/assets/astro-colors-page.png?v={{pkg.version}}">

```md
---
layout: ../layouts/Layout.astro
---

# Colors

- red
- green
- blue
```

The front matter property `layout` specifies the path to a layout component
that provides the HTML boilerplate for the page.
This can include `link` tags that refer to CSS files used to style the page.

This same page can be implemented as an Astro component
in the file `src/pages/demo/colors.astro` as follows:

```js
---
import Layout from "../../layouts/Layout.astro";

const colors = ["red", "green", "blue"];
---

<Layout>
  <h1>Colors</h1>
  <ul>
    {colors.map((color) => <li>{color}</li>)}
  </ul>
</Layout>
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
    // This can be defined with either "type" or "interface".
    type Props {
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
  Unlike JSX, this content is not pure XML.
  For example, it an contain unclosed elements like `!doctype`
  and self closing elements like `br` and `hr`.

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

### Slots

The HTML of a component can contain a `slot` element
which marks where content will be inserted.
For example, the following component in `src/components/Border.astro`
contains a `slot` element.

```js
---
interface Props {
  color?: string;
}

const { color = "black" } = Astro.props;

const style = {
  borderColor: color,
  boxShadow: `5px 5px 5px ${color}`,
};
---

<div class="border" {style}>
  <slot />
</div>

<style>
  .border {
    border-width: 3px;
    border-radius: 1rem;
    display: inline-block;
    padding: 1rem;
  }
</style>
```

This `Border` component can be used as follows.
Note how content is included.

<img alt="Astro component with slot" style="border: none; width: 30%"
  src="/blog/assets/astro-component-with-slot.png?v={{pkg.version}}" />

```html
<Border color="red">
  What is with this code?
  <br />
  Oh my, looks like I wrote it.
  <br />
  What was I thinking?
</Border>
```

## Shorthand Attributes

Astro supports shorthand syntax for attributes like Svelte.
For example, `<Layout title={title}>` can be written as `<Layout {title}>`.

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

## Dynamic Routes

Dynamic routes are routes defined under the `pages` directory with
directory or file names that contain a variable name inside square brackets.
These can be used for both pages and API endpoints.

For example, the following page defined in `src/pages/index.astro`
contains links to pages that are provided by a dynamic route.

```js
---
import Layout from '../layouts/Layout.astro';

const colors = ["red", "green", "blue"];
---

<Layout>
  {
    colors.map((color) => (
      <div>
        <a href={`/${color}`}>{color}</a>
      </div>
    ))
  }
</Layout>
```

Rather than create a `.astro` file for each color,
we can create the file `[color].astro` shown below.
This file is used to render each of the color pages.

```js
---
export function getStaticPaths() {
  const colors = ["red", "green", "blue"];
  return colors.map((color) => ({ params: { color } }));
}

const { color } = Astro.params;
---

<Layout>
  <h1>
    You selected <span style={`color: ${color}`}>{color}</span>.
  </h1>
</Layout>
```

The `getStaticPaths` function is required when not using SSR
so Astro knows the pages it should generate at build time.
This function is not used when SSR is enabled.

Note how the `colors` array is defined inside the `getStaticPaths` function.
If defined outside that function, it will not be visible.
The reason is that the `getStaticPaths` function gets hoisted into its own scope.
That prevents it from accessing most things outside the function.
This is a limitation that the Astro team hopes remove in the future.

Running `npm run build` generates the `dist` directory which will contain
the following files and more:

- `dist/blue/index.html`
- `dist/green/index.html`
- `dist/red/index.html`

The `getStaticPaths` function is only required if SSR is not enabled.
To enable SSR, install the node adapter by entering `npx astro add node`.

When SSR is enabled, running `npm run build`
will not generate HTML files for dynamic routes.
Instead, the HTML for dynamic routes
will be generated when requested by a client.

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

For static sites, optimized images are generated concurrently at build time.

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
  (All major web browsers support the WEBP format.)
- adding attributes required to take advantage of services like Cloudinary

More image optimization is performed in production builds
than when running in dev mode.

This mostly removes the need to use tools like Squoosh to optimize images.

Image optimization is performed by the {% aTargetBlank
"https://github.com/lovell/sharp", "sharp" %} package.

## Icons

{% aTargetBlank "https://www.astroicon.dev", "Astro Icon" %}
provides access to many icon sets.

To install "Astro Icon" integration, enter `npx astro add astro-icon`.

TODO: This is not currently working!
See https://github.com/natemoo-re/astro-icon/issues/167.
For now, install it with

- Enter `npm install astro-icon@next`.
- Edit `astro.config.mjs`, add `import icon from "astro-icon";`,
  and add `icon()` to the `integrations` array.

To see the available icons, browse {% aTargetBlank
"https://icon-sets.iconify.design", "icon sets" %}.
This includes "Font Awesome Solid", "SVG Spinner", and many more.

To use an icon, import the `Icon` component in the component script with

```js
import {Icon} from 'astro-icon/components';
```

Then render an icon with `<Icon name="{some-icon-name}" />`.

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

## Content Collections

Astro supports describing and retrieving collections of data
from Markdown files that are typically placed under the `src/content` directory.
Each subdirectory represents a different collection.

The following steps can be taken to define and render a collection of dogs.

- Create the directory `src/content`.

- Create the file `config.ts` in this directory.

  This file defines each of the collections.
  It uses {% aTargetBlank "https://zod.dev", "Zod" %}
  to describe and validate the schema of each collection.
  The schema defines which front matter properties are valid
  and provides a type-safe way to use the data.

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
  website: https://www.akc.org/dog-breeds/whippet/
  ---

  Comet loves pool balls and basketballs.
  ```

  Note the use of front matter to describe properties of this instance.

- Access the collection in a component.

  For example:

  <img alt="Astro Dogs" style="width: 50%"
    src="/blog/assets/astro-dogs.png?v={{pkg.version}}">

  ```js
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
        website="{dog.website}"
        content="{dog.body.trim()}"
      />
      )) }
    </main>
  </Layout>
  ```

- Create the `Dog` component:

  ```ts
  ---
  import { type CollectionEntry } from "astro:content";

  interface Props {
    dog: CollectionEntry<"dogs">;
  }

  const { dog } = Astro.props;
  const { breed, image, name } = dog.data;
  const { Content } = await dog.render();
  ---

  <hr />
  <div class="my-4">
    <p class="font-bold">{name} is a {breed}.</p>
    <Content />
    <a href={website}>American Kennel Club</a>
  </div>
  ```

For a more complete example of using content collections,
see the project {% aTargetBlank
"https://github.com/mvolkmann/astro-examples/tree/main/content-collections",
"content-collections" %}.
This displays a component for each NFL team on the initial page.
Clicking the component for any team navigates to a detail page
using view transitions.

<img alt="Astro content collections NFL home" style="width: 49%"
  src="/blog/assets/astro-content-collections-nfl-home.png?v={{pkg.version}}">
<img alt="Astro content collections NFL detail" style="width: 49%"
  src="/blog/assets/astro-content-collections-nfl-detail.png?v={{pkg.version}}">

The file `src/content/config.ts` which defines the schema
for the for collection documents contains the following:

```ts
import {defineCollection, z} from 'astro:content';

const nfl = defineCollection({
  type: 'content',
  schema: z.object({
    city: z.string(),
    name: z.string(),
    conference: z.string(),
    logoUrl: z.string(),
    headCoach: z.string(),
    established: z.number()
  })
});

export const collections = {nfl};
```

The file `src/content/nfl/kansas-city-chiefs.md` is one example
of the content files described by the schema above.
It contains the following:

```md
---
city: Kansas City
name: Chiefs
conference: AFC West
logoUrl: https://res.cloudinary.com/nflleague/image/private/f_auto/league/ujshjqvmnxce8m4obmvs
headCoach: Andy Reid
established: 1960
---

After losing to the Raiders on Christmas Day, the Chiefs ...
```

## MDX

The MDX extension enables using components inside Markdown files.
The components can be implemented in any of the supported frameworks.
These can add interactivity to what would otherwise be static content.

To install the MDX integration in an Astro project, enter `npx astro add mdx`.

Markdown files that wish to render components
must have the file extension `.mdx`.

For example, here is a component definition in `src/components/Greet.astro`:

```js
---
const { name } = Astro.props;
---

<p class="text-bold text-red-500">Hello, {name}!</p>
```

And here is a content file in `src/content/dogs/comet.mdx`.
Note how it imports and uses the `Greet` component:

```js
---
name: 'Comet'
breed: 'Whippet'
website: https://www.akc.org/dog-breeds/whippet/
---

import Greet from "../../components/Greet.astro";
He loves the following:

- pool balls
- basketballs
- frisbees

![Whippet](https://www.akc.org/wp-content/uploads/2017/11/Whippet-On-White-01.jpg)

<Greet name="Comet" />
```

## Other Frameworks

Astro supports using components from many other frameworks including
Alpine, Lit, Preact, React, SolidJS, Svelte, Vue, WebComponents, and more.

### Svelte

Let's walk through the steps to use a
{% aTargetBlank "https://svelte.dev", "Svelte" %} component.

1. Install the Svelte integration by entering `npx astro add svelte`.

1. Define a Svelte component in the `src/components` directory.

   For example, here is the file `Counter.svelte`:

   {% raw %}

   ```html
   <script>
     export let label = '';
     export let start = 0;

     let count = start;
   </script>

   <div class="row">
     {#if label}
       <div>{label}</div>
     {/if}
     <button disabled={count <= 0} on:click={() => count--}>-</button>
     <h1>{count}</h1>
     <button on:click={() => count++}>+</button>
   </div>

   <style>
     button, h1 {
       margin: 0;
     }

     .row {
       display: flex;
       align-items: center;
       gap: 1rem;
     }
   </style>
   ```

   {% endraw %}

1. Use the new component in another component or page.

   ```js
   import Counter from "@components/Counter.svelte";
   ...
   <Counter label="Tally" start={3} client:load />
   ```

   There are five provided `client` directives that tell Astro that
   a component requires client-side JavaScript code to be loaded.
   They differ in when the JavaScript will be loaded.

   | Directive        | When JS is loaded                       |
   | ---------------- | --------------------------------------- |
   | `client:idle`    | when browser is idle                    |
   | `client:load`    | immediately                             |
   | `client:media`   | when a CSS media query condition is met |
   | `client:only`    | after page load with no SSR             |
   | `client:visible` | when component becomes visible          |

   Integrations can add support for custom `client` directives.

For more detail on using Svelte components in Astro, see {% aTargetBlank
"https://docs.astro.build/en/guides/integrations-guide/svelte/",
"Svelte integration" %}.

### Alpine

Now let's walk through the steps to use
{% aTargetBlank "https://alpinejs.dev", "Alpine" %} in Astro.
Alpine is a much lighter weight framework than
frameworks like React, Svelte and Vue.
But Alpine is still quite capable.

1. Install the Alpine integration by entering `npx astro add alpinejs`.

1. Define an Astro component that uses Alpine in the `src/components` directory.

   For example, here is the file `Counter.astro`:

   {% raw %}

   ```js
   ---
   interface Props {
     label?: string;
     start?: number;
   }

   const { label = "", start = 0 } = Astro.props;
   ---

   <div class="row" x-data={`{ count: ${start} }`}>
     {label &&
     <div>{label}</div>
     }
     <button :disabled="count <= 0" @click="count--">-</button>
     <h1 x-text="count"></h1>
     <button @click="count++">+</button>

     <!-- This demonstrates calling client-side JS code
          in Alpine event handling. -->
     <button @click="demo">Click Me</button>
   </div>

   <!-- The is:inline directive opts out of Astro processing
        and includes the script tag as-is. -->
   <script is:inline>
     function demo() {
       alert("Demo time!");
     }
   </script>

   <style>
     button,
     h1 {
       margin: 0;
     }

     .row {
       display: flex;
       align-items: center;
       gap: 1rem;
     }
   </style>
   ```

   {% endraw %}

1. Use the new component in another component or page.

   ```js
   import Counter from "@components/Counter.astro";
   ...
   <Counter label="Tally" start={3} />
   ```

   Unlike with Svelte and other components,
   Astro components that use Alpine do not need a `client:*` directive
   in order to have client-side interactivity.

For more detail on using Alpine in Astro, see {% aTargetBlank
"https://docs.astro.build/en/guides/integrations-guide/alpinejs/",
"Alpine integration" %}.

## Directives

In addition to the `client:` directives described in the previous section,
Astro supports these directives:

| Directive            | Action                                                                                             |
| -------------------- | -------------------------------------------------------------------------------------------------- |
| `class:list={[...]}` | converts an array of CSS class names into a string of class names                                  |
| `define:vars`        | applied to `style` or `script` elements to provide access to the values of front matter variables  |
| `is:inline`          | disables Astro processing of `style` and `script` elements leaving them as-is                      |
| `is:global`          | applied to a `style` element to make the styles global rather than scoped to the current component |
| `is:raw`             | treats descendant elements and interpolations as text                                              |
| `set:html={string}`  | injects a string of HTML into the element                                                          |
| `set:text`           | injects a string of text into the element                                                          |

If a value in the `class:list` array is an object,
the keys must be class names and the values must be Boolean expressions
that determine whether the class name should be included.
If a value is array, it is flattened into the surrounding array.
If a value is `false`, `undefined`, or `null`, it is skipped.

If the string value of `set:html` comes from an untrusted source, use a
sanitizer such as sanitize-html to avoid cross site scripting attacks (XSS).
This directive can be applied to a `Fragment` component
when a wrapping element is not needed.
If the value is a `Promise`, Astro will wait for it to resolve
and use the result as the HTML to insert.

The string value of `set:text` is automatically escaped, replacing
certain characters such as `"` with a character entity such as `&quot;`.

## Custom 404 Page

To create a custom 404 page, add the file `src/pages/404.astro`.
This page can import and use layouts and other components
just like any other page.

The following is an example.

<img alt="Astro 404 page" style="width: 30%"
  src="/blog/assets/astro-404-page.png?v={{pkg.version}}">

```html
---
import Layout from "../layouts/Layout.astro";
---

<Layout>
  <main>
    <p>
      The Web site you seek
      <br />
      cannot be located, but
      <br />
      countless more exist.
    </p>
    <p>
      No content was found at
      <br />
      <span id="path"></span>
    </p>
  </main>
</Layout>

<script>
  const span = document.getElementById('path');
  if (span) span.textContent = window.location.pathname;
</script>

<style>
  main {
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    gap: 3rem;

    height: 100vh;
    width: 100vw;

    background-color: linen;
    color: cornflowerblue;
    font-weight: bold;
  }

  p {
    font-size: 1.5rem;
    margin: 1rem 0;
    text-align: center;
  }

  #path {
    color: red;
  }
</style>
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
are Chrome and Edge. Polyfills are provided to support view transitions
in browsers that do not yet support the View Transitions API.

Astro disables all view transitions when
the CSS media feature "prefer-reduce-motion" is enabled.
This is based on an operating system specific setting.
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

The `ViewTransitions` component must appear as a child of the `head` element
for _any_ transitions to occur, including custom transitions.

The built-in transitions include:

- `fade`

  The current page fades out and new page fades in.

- `initial`

  This uses the browser default transition.

- `slide`

  The current page slides out to the left and new page slides in from the right.
  The opposite occurs when navigating back to the previous page.

- `none`

  This disables transition animations.
  It is typically applied to the `html` element
  to disable all transition animations for the entire page.

To specify a page-level transition other than the default,
add the `transition:animate` directive to the `body` element of each page.
If all pages use a common layout, this can be applied in that layout component.
For example, the following will configure a slide transition where
going forward causes the current page to slide out to the left
and the new page to slide in from the right.
Going back triggers the opposite transitions.:

```html
<body class="p-4" transition:animate={slide({ duration: '1s' })}>
```

To specify a transition on a specific element
(which can be the root element of a page),
add the `transition:animate="{transition-type}"` directive.
For example, `<main transition:animate="slide">`.

To customize the transition,
pass a configuration object to the transition function.
For example, `<main transition:animate={slide({ duration: '2s' })}>`.

When identical `Image` components are on both the current page
and the next page and they have the `transition:name` directive,
one will morph into the other through the view transition.
This is especially useful when the image size of each of the pages differs.

For example:

```html
import whippet from "../images/whippet.webp"; ... --- ...
<image alt="whippet" src="{whippet}" transition:name="whippet" />
...
```

When identical `audio` and `video` elements are on both
the current page and the next page and they are playing
and they have the `transition:name` and `transition:persist` directives,
they will continue playing without interruption through the view transition.

For example:

```html
<audio
  controls
  src="/sample.mp3"
  transition:name="my-audio"
  transition:persist
></audio>
<video controls width="200" transition:name="my-video" transition:persist>
  <source src="/bunny-video.mp4" type="video/mp4" />
</video>
```

To define a custom transition, create an object that
conforms to the `TransitionDirectionalAnimations` interface
which requires `forwards` and `backwards` properties.
Those properties must be objects that
conform to the `TransitionAnimation` interface
which requires `old` and `new` properties.

For example:
TODO: This is not quite working yet.
See astro-examples/view-transitions/src/layouts/SpinLayout.astro.

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

For more detail, see {% aTargetBlank
"https://docs.astro.build/en/guides/view-transitions/", "View Transitions" %}.
Also, check out {% aTargetBlank
"https://github.com/martrapp/astro-vt-bot#readme", "astro-vt-bot" %}.

## API Endpoints

Endpoints are defined by `.js` and `.ts` files under the `src/pages` directory.
Their URLs are defined by file-based routing, just like UI pages.

Endpoints are not UI pages.
It seems odd that they are defined in the `pages` directory
rather than in a dedicated directory like `apis` or `endpoints`.
Perhaps a future version of Astro will make this change.

Endpoints can return data in any format including
JSON and HTML (perhaps for use with HTMX).
However, currently endpoints cannot use Astro components to generate HTML.
That capability is planned for the future.
For now, see the HTMX section below which uses Astro pages as endpoints.

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

Files with square brackets in the name
define dynamic routes where path parameters are used.
In this case the path parameter is the id of a todo.

Dynamic routes can be used for both pages and API endpoints.

Dynamic routes require enabling SSR.
To do this, add the node adapter by entering `npx astro add node`.

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

## HTMX

Astro added support for "page partials" which make it more natural to use HTMX.
These render HTML with only `body` element content.
The `<!doctype html>`, `html`, `head` (and its contents), and `body` elements
are not output.

To indicate that an Astro component should be rendered as a partial,
add the following to its component script:

```js
export const partial = true;
```

For an example app that combines Astro and HTMX, see {% aTargetBlank
"https://github.com/mvolkmann/astro-htmx-todo-app", "astro-htmx-todo-app" %}.
This uses Astro page as endpoints.

<img alt="Astro HTMX Todo app" style="width: 70%"
  src="/blog/assets/astro-htmx-todo-app.png?v={{pkg.version}}" />

HTTP requests can be triggered by interacting with
any HTML element and use any HTTP verb.
These requests can target an Astro page.
The component script in the page can determine
which verb was used and respond accordingly.

## Internationalization

Astro supports {% aTargetBlank
"https://docs.astro.build/en/guides/internationalization/",
"internationalization" %}.

The Astro approach is it to duplicate each component for each supported language
and store them in directories whose names are language codes.
This is a bad approach because any changes require modifying multiple files.

TODO: Describe how to use this.

## Tailwind Typography Plugin

The Tailwind {% aTargetBlank "https://tailwindcss.com/docs/typography-plugin",
"@tailwindcss/typography" %} plugin "provides a set of prose classes
you can use to add beautiful typographic defaults to any vanilla HTML
you don't control, like HTML rendered from Markdown, or pulled from a CMS."

To install and configure this plugin:

- Enter `npx astro add tailwind`
- Enter `npm install -D @tailwindcss/typography`
- Edit `tailwind.config.mjs` and add the following to the `plugins` array:

  ```text
  require('@tailwindcss/typography')
  ```

To use this plugin, add `prose` CSS classes to specific HTML elements
that contain the elements to be styled.

The large screenshot below demonstrates how
many features of Markdown are styled by this plugin.

<img alt="Astro Tailwind Typography" style="width: 70%"
  src="/blog/assets/astro-tailwind-typography.png?v={{pkg.version}}" />

The following files from an Astro project produce the screenshot above.

<i>src/layouts/Layout.astro</i>

Note the use of the CSS class `prose` and
the `slot` element that marks where content will be inserted.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <style>
      body {
        padding: 1rem;
      }
    </style>
  </head>
  <body class="prose">
    <slot />
  </body>
</html>
```

<i>src/pages/index.md</i>

Note the reference to the layout above using a front matter property.

{% raw %}

````md
---
layout: ../layouts/Layout.astro
---

# Tailwind Typography Demo

This demonstrates use of the Tailwind plugin
[@tailwindcss/typography](https://tailwindcss.com/docs/typography-plugin).

> The official Tailwind CSS Typography plugin provides
> a set of prose classes you can use to add beautiful
> typographic defaults to any vanilla HTML you don’t control,
> like HTML rendered from Markdown, or pulled from a CMS.

## Colors

These are some _basic_ colors.

- red
- green
- blue

## Seasons

These are the **seasons** in a year.

1. Winter
1. Spring
1. Summer
1. Fall

## Dogs

These are some dogs I know.

| Name  | Breed                      |
| ----- | -------------------------- |
| Comet | Whippet                    |
| Oscar | German Shorthaired Pointer |

## Tasks

- [x] buy milk
- [ ] cut grass

## Code

```js
function add(n1, n2) {
  return n1 + n2;
}
```
````

{% endraw %}

## Bun instead of Node

Astro provides the following caution about using Bun instead of Node
to execute `astro` commands and run the Astro server:
"Using Bun with Astro may reveal rough edges.
Some integrations may not work as expected."

To create an Astro project using Bun:

- Enter `bunx create-astro@latest {project-name}`.
- `cd` to the new project directory.
- Add Bun type definitions by entering `bun add -d bun-types`.
- Edit `tsconfig.json` and add the following:

  ```json
   "compilerOptions": {
    "types": ["bun-types"]
  }
  ```

- Start the development server with `bunx --bun astro dev`.
- Build the site with `bunx --bun astro build`.
- Preview the built site with `bunx --bun astro preview`.

Using Bun instead of Node provides better performance.
It also enables using all the features of Bun such as SQLite support.
The following Astro page demonstrates this.

```js
---
import { Database } from "bun:sqlite";
import Layout from "../layouts/Layout.astro";

type Todo = {
  id: number;
  text: string;
  completed: number; // 0 or 1 for SQLite compatibility
};

const db = new Database("todos.db", { create: true });
const query = db.query("select * from todos;");
const todos = query.all() as Todo[];
---

<Layout>
  <h1>Astro with Bun</h1>
  { todos.map((todo) => (
    <div>
      <input type="checkbox" checked={todo.completed === 1} />
      <span>{todo.text}</span>
    </div>
  )) }
</Layout>
```

For more details, see {% aTargetBlank
"https://docs.astro.build/en/recipes/bun/", "Using Bun with Astro" %}.

## Starlight

{% aTargetBlank "https://starlight.astro.build", "Starlight" %}
is a documentation theme built on Astro.

TODO: Describe how to use this.

## Resources

- {% aTargetBlank "https://astro.build", "Astro Home Page" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=XoIHKO6AkoM",
  "Astro Quick Start Course" %} by Traversy Media

## Unorganized Content

What are the benefits of using the tailwind typography plug-in?

Learn about enabling SSR so pages are not generated at build time
and are instead generated on demand. Is that how it works?

Learn about Astro support for pagination.

What is the reasonable number of markdown documents to have in a collection
where if you need more then you should use a database?

Learn about and document using the Astro Dev toolbar
that appears at the bottom of the page in the browser when running in dev mode.

Can pages be defined with .html, .md, .mdx, and .svelte files
in addition to .astro files?
