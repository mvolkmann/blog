---
eleventyNavigation:
  key: Tailwind
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Overview

{% aTargetBlank "https://tailwindcss.com/", "Tailwind" %}
is a CSS utility framework.
It provides a large number of pre-built CSS classes
that often only set a single CSS property.
These can be used as values of the `class` attribute on HTML elements.
They can also be customized in each application that uses them.

Other CSS frameworks such as Bootstrap, Bulma, Foundation, and Materialize
define styling for components.
Tailwind takes a different approach,
providing lower level styling that can be combined
in order to create custom looks.

Tailwind is not a replacement for CSS.
It is still necessary for developers to be familiar with
CSS properties and their supported values.

Tailwind is implemented as a PostCSS plugin.
It can be used in conjunction with other PostCSS plugins
such as those that support preprocessors like Sass.

The base styles in Tailwind provide a CSS reset known as "Preflight".

## Pros

- **Collocates markdown and styling**  
  Placing styles with the elements they affect makes it easier
  to visualize the result while looking at the HTML.

- **Less naming required**  
  Tailwind enables styling HTML elements without
  giving them an `id` attribute or CSS `class` name.
  Using custom CSS classes requires assigning a name and
  associating that name with elements using the `class` attribute.
  Understanding the styling that a CSS class applies requires
  looking up the CSS class, often in another source file.
  Another option is to specify CSS properties using the `style` attribute,
  but using Tailwind CSS classes is more concise.

- **More concise**  
  Most Tailwind CSS classes define a single CSS property.
  For example, the CSS property `flex-direction: column;`
  can be replaced by the Tailwind class `flex-col`.

- **Removes need to think about specificity**  
  Specificity is the technique used in CSS to determine which CSS class
  should be applied to an element when multiple classes can apply.
  Because Tailwind directly associates CSS classes with HTML elements,
  those classes are always used regardless of the specificity of other rules.

- **Easier responsive UIs**  
  Tailwind enables creating responsive UIs without writing media queries.
  This is done by prefixing Tailwind class names
  with a breakpoint name followed by a colon.
  For example: `md:flex-col` changes the `flex-direction` to `column`
  when the screen width is at or above the medium breakpoint.

- **Configurable**  
  Tailwind is highly configurable.
  Default values can be overridden in a `tailwind.config.js` file.

- **Supported by tools**  
  There are good, Tailwind-aware extensions for many editors
  including VS Code, Sublime Text, Vim, and Atom.

## Cons

- **Feels like giving up**
  CSS emphasizes defining classes that describe sets of CSS properties
  that can be applied to multiple HTML elements.
  Thought goes into identifying well-named, cohesive sets of these properties.
  Using Tailwind feels like giving up on trying to do this.
  It encourages treating every HTML element as a one-off,
  just applying the styling that each element needs.

- **Learning curve**  
  CSS has a steep learning curve. Tailwind adds to that.
  Effective use of Tailwind requires good knowledge of CSS.
  Developers that are not strong in CSS will struggle with using Tailwind.

- **Clutter**  
  HTML becomes more cluttered when many elements have
  a `class` attribute with a long value.
  This makes it more difficult to understand
  the nested structure of the elements at a glance.
  The VS Code extension "Tailwind Fold" addresses this
  by automatically folding long `class` attribute values
  (and `className` property values in React).

- **Inability to override**  
  When Tailwind classes are used in components of frameworks
  (ex. React, Vue, Svelte, and Angular),
  parent components cannot override their styling.
  For this reason it may be advisable to primarily use Tailwind
  for element layout and not for properties more likely
  to be overridden such as fonts and colors.

## No Build Process

There are two approaches for using Tailwind without a build process.

The easiest approach is to include it from a CDN with this `link` tag:

```html
<script src="https://cdn.tailwindcss.com"></script>
```

This has the downside that it includes every Tailwind CSS class,
not just the ones actually used in the app.

A more involved approach is to generate a CSS file that
only contains the Tailwind CSS classes that are actually used.
The steps to do this are as follows:

1. Install Tailwind by entering `npm install -D tailwindcss`
1. Create the file `tailwind.config.js` by entering `npx tailwindcss init`
1. Edit the value of `content` in `tailwind.config.js`
   to specify the files that might used Tailwind classes.
   For example:

   ```json
   content: ["**/*.{html,tsx}"],
   ```

1. Create the file `global.css` containing the following:

   ```css
   @tailwind base;
   @tailwind components;
   @tailwind utilities;
   ```

   This file can also define custom CSS classes.

1. Generate a CSS file containing only the Tailwind classes used in your app.

   Enter `npx tailwindcss -i ./global.css -o public/tailwind.css --watch`
   to generate `public/tailwind.css` now and
   again every time any of the "content" files are modified.

   Consider adding a `package.json` script for this such as:

   ```json
   "tw": "npx tailwindcss -i ./global.css -o public/tailwind.css --watch"
   ```

1. Include the following `link` element in the base HTML of the app.

   ```html
   <link href="/public/tailwind.css" rel="stylesheet" />
   ```

This also requires enabling serving static files with the following steps:

1. Install a plugin by entering `bun add @elysiajs/static`
1. In the server code, add a call to `app.use(staticPlugin());`

## Basic Build Process

To install Tailwind in a project that has a `package.json` file,
enter `npm install -D tailwindcss`.

A build process is required to use Tailwind.
Many approaches can be taken, but the simplest is to
create a `public` directory and add the following
to the `scripts` section in the `package.json` file:

```json
  "build:css": "tailwind build src/style.css -o public/style.css",
```

The file `src/style.css` should, at a minimum, contain the following:

```css
@tailwind base;
@tailwind components;
@tailwind utilities;
```

To build the required set of Tailwind CSS classes in `public/style.css`,
enter `npm run build:css`.
This will replace `@tailwind` directives above with generated CSS.
The generated CSS file will contain:

- {% aTargetBlank
  "https://github.com/necolas/normalize.css", "normalize.css" %}
  CSS rules
- Tailwind {% aTargetBlank "https://tailwindcss.com/docs/preflight",
  "Preflight" %} CSS rules
- all Tailwind utility class definitions in the categories referenced
  by the `@tailwind` directives in `src/style.css`

Highlights of the Preflight styles include:

- Default margins are removed from headings, paragraphs, and more.
- Headings become unstyled, so they have the same
  `font-size` and `font-weight` as normal text.
- Lists become unstyled, so they have no bullets, numbers, margins, or padding.
- Images become block-level instead of inline.
- Borders become `solid 1px` using the
  default border color of the current theme.
- Preflight styles can be overridden by defining CSS classes
  after `@tailwind base;`.

It is only necessary to rebuild the Tailwind-generated CSS
when Tailwind directives have been modified or
when the build process is configured to purge unused CSS rules
and the Tailwind CSS classes being used have changed.
Avoiding rebuilds speeds development.

For details on configuring various build tools to support Tailwind, see
{% aTargetBlank "https://tailwindcss.com/docs/installation",
"Tailwind installation" %}.

## PostCSS Build Process

An advantage of using PostCSS to build the Tailwind CSS
used by an application is that other PostCSS plugins can also be used.

Building with PostCSS is only slightly more complicated.
The steps to do this are:

1. Install all the PostCSS plugins to be used. For example:

   ```bash
   npm install -D postcss-cli autoprefixer@9.8.6
   ```

1. Create the file `postcss.config.js` containing:

   ```js
   module.exports = {
     // Include a require for each PostCSS plugin to be used here.
     plugins: [require('tailwindcss'), require('autoprefixer')]
   };
   ```

1. Change the `build:css` script in `package.json` to:

   ```json
   "build:css": "postcss src/style.css -o public/style.css"
   ```

## Watching For Changes

To automatically run the `build:css` script
any time a file in the `src` directory changes:

1. Enter `npm install -D watch`

1. Add the following script in `package.json`:

   ```json
   "watch": "watch 'npm run build:css' ./src"
   ```

## <a name="configuration">Configuration</a>

To see all the Tailwind properties that can be customized,
create a new configuration file by entering `npx tailwindcss init --full`.
This creates the file `tailwind.config.js`.
You may wish to save this file for later reference.
Consider renaming it to `tailwind.config-full.js`.

Generate a new `tailwind.config.js` file by entering `npx tailwindcss init`
and add customizations here.
This file will be much shorter than the full version,
making it easier to find your customizations.
Values used by the provided CSS classes
can be customized to change many styling aspects
including colors, breakpoints, fonts, and more.

To change the actual colors used when `white` and `black` are specified,
add a `color` property to the `extend` property as follows:

```js
  theme: {
    extend: {
      colors: {
        black: "#222", // very dark gray
        white: "#ddd" // very light gray
      }
```

To define new, custom colors,
add `color` properties to the `extend` property as follows:

```js
  theme: {
    extend: {
      colors: {
        primary: "cornflowerblue",
        secondary: "orange"
      }
```

With these in place, we can use CSS class names like
`bg-primary` and `text-secondary`.

To change the responsive breakpoints, modify the values below:

```js
  theme: {
    ...
    screens: {
      sm: "640px",
      md: "768px",
      lg: "1024px",
      xl: "1280px"
    },
```

To change the fonts used when `font-sans`, `font-serif`, and `font-mono`
are specified, modify the font name lists in the arrays below:

```js
  theme: {
    ...
    fontFamily: {
      sans: [...],
      serif: [...],
      mono: [...]
    },
```

To register new fonts and add custom font sizes,
specify the `fontFamily` and `fontSize` properties
in the `extend` object.
For example:

```js
  theme: {
    extend: {
      fontFamily: {
        body: ['SomeFont1', 'SomeFont2']
      },
      fontSize: {
        "hero": "6rem"
      },
```

With this in place we can cause a header element to use it as follows:

```html
<h1 class="text-hero">Welcome to My Page</h1>
```

The `tailwind.config.js` file defines a `future` property whose value
is an object with boolean properties that are commented out.
Uncomment these to get warnings about usage of features
that will break in future releases.

## Using With Frameworks

From the website for {% aTargetBlank
"https://github.com/dcastil/tailwind-merge", "tailwind-merge" %},
"If you use Tailwind CSS with a component-based UI renderer like React or Vue,
you're probably familiar with the situation that you want to
change some styles of a component, but only in a one-off case."
tailwind-merge provides the function `twMerge` that takes any number of
strings containing space-separated lists of CSS and Tailwind classes.
It resolves any conflicts and returns a single string of class names.

## Purging Unused CSS Classes

The `purge` configuration property can be modified to purge unused CSS classes.
If this is not done, the generated CSS file will be massive.
As of 10/13/2020, the size is 2413K uncompressed, 1967K minified,
190K gzipped, and 46K compressed with
{% aTargetBlank "https://github.com/google/brotli", "Brotli" %}.

The `purge` property should be set to an array of glob patterns
for file paths that can contain references to CSS classes.
Tailwind will automatically check for
references to Tailwind CSS classes in CSS files.

By default, unused CSS classes are only removed when
the `NODE_ENV` environment variable is set to `production`.
This is desirable.
Otherwise every time a new Tailwind CSS class is used,
another Tailwind build is required, which slows development.

To remove unused CSS classes regardless of the value of `NODE_ENV`,
set the `enabled` property in the `purge` object to `true`.

For example, the `purge` property can be configured as follows::

```json
  purge: {
    content: ['./public/**/*.html', './src/**.*.svelte'],
    enabled: true
  },
```

## Minimizing CSS

To minimize the generated CSS, including removing comments,
in order to make it smaller:

1. Enter `npm install -D cssnano`

1. Change the content of the `postcss.config.js` file to include :the following:

   ```js
   const cssnano = require('cssnano');

   module.exports = {
     plugins: [
       require('tailwindcss'),
       require('autoprefixer'),
       cssnano({
         preset: 'default'
       })
     ]
   };
   ```

It may be desirable to only minimize CSS in production builds.
To modify `postcss.config.js` to do this, see the end of {% aTargetBlank
"https://flaviocopes.com/tailwind-setup/",
"How to setup Tailwind with PurgeCSS and PostCSS" %}.

## Serving Changes

There are many approaches to serve a site from local files and
automatically refresh the browser when changes are detected.
One simple approach is to use {% aTargetBlank
"https://github.com/tapio/live-server", "live-server" %}.

To use live-server:

1. Enter `npm install -g live-server`
1. Enter `live-server public` where `public` contains an `index.html` file.
1. Browse localhost:8080.

## VS Code Support

The VS Code extension {% aTargetBlank
"https://tailwindcss.com/docs/intellisense", "Tailwind CSS IntelliSense" %}
provides autocomplete, syntax highlighting, and linting.

While entering Tailwind CSS class names,
a popup displays all matching names and their definitions.
For example, entering "upp" displays "uppercase - text-transform: uppercase".

To see the definition of a Tailwind CSS class that has already been entered,
hover over its name.

A small color swatch is displayed in front of each
Tailwind class name that represents a color.

The VS Code extension {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=stivo.tailwind-fold",
"Tailwind Fold " %} automatically folds the values of
`class` attributes in HTML and `className` props in JSX.
To see one of these values, move the cursor to its line.
To toggle the folding behavior off and on, press ctrl-option-a.

To enable {% aTargetBlank "https://emmet.io/", "Emmet" %}
completions of Tailwind CSS class names,
add the following in the VS Code `settings.json` file:

```json
  "tailwindCSS.emmetCompletions": true
```

## Icons

The makers of Tailwind created
{% aTargetBlank "https://heroicons.com", "heroicons" %}
which is a website that provides a large collection of free SVG icons.
Hover over an icon to expose buttons for downloading it in SVG or JSX.
Paste this into an HTML file or React component.

The icons come in three variations with a `viewBox` of a specific size:

- Outline icons are 24x24 pixels and use a 1.5px stroke.
- Solid icons are 24x24 pixels and are filled.
- Mini icons are 20x20 and are filled.

By default the icons have the Tailwind classes `w-6` and `h-6`
which makes them a `1.5rem` square.
To change the size of an icon, change these `w` and `h` classes.

To set the color of an icon, set its CSS `color` property.

When using React or Next.js,
another option is to enter `npm install @heroicons/react`
and import specific icons in component source files.
For example:

```js
import {BeakerIcon} from '@heroicons/react/24/solid';
```

## Responsive Breakpoints

By default Tailwind uses the following responsive breakpoints:

| Name | `min-width` |
| ---- | ----------- |
| `sm` | 640px       |
| `md` | 768px       |
| `lg` | 1024px      |
| `xl` | 1280px      |

These specify `min-width` values, so a mobile-first approach is used.

Precede Tailwind CSS class names with a breakpoint name and a colon
to only apply the CSS class when the screen/window width is
greater than or equal to the corresponding `min-width` value.
For example: `lg:m-4` applies a margin of `1rem` only if
the screen/window width is greater than or equal to `1024px`.

Class names with no breakpoint prefix are used when the screen/window width
is smaller than the largest prefix used.
For example `text-red md:text-green` makes the text red for all widths
less than 768px and green for all widths greater than or equal to 768px.

To show an element only when on a mobile device, use `md:hidden`.
To hide an element when on a mobile device, use `hidden md:block`.

This approach for making UIs responsive is
easier than writing CSS media queries.

The breakpoint values can be overridden by modifying the `tailwind.config.js`
file as shown in the [Configuration](#configuration) section.

## Pseudo-class Variants

Tailwind supports many prefixes that can be added before class names,
separated by a colon, that cause the class to only be applied
when a certain condition holds.

| Variant         | Condition                                                                  |
| --------------- | -------------------------------------------------------------------------- |
| `active`        | element is active (ex. a `<button>` is being pressed)                      |
| `checked`       | checkbox or radio button is checked                                        |
| `dark`          | user set OS to use dark mode (default is light mode, no `light` variant)   |
| `disabled`      | form element is disabled                                                   |
| `even-child`    | element is an event-numbered child of its parent (zero-based index is odd) |
| `first-child`   | element is the first child of its parent                                   |
| `focus-visible` | element has focus and the user is using a keyboard                         |
| `focus-within`  | ancestor element has focus                                                 |
| `focus`         | form element (ex. `<input>`) has focus                                     |
| `group-focus`   | ancestor element has the `group` class and has focus                       |
| `group-hover`   | hovering over an ancestor element that has the `group` class               |
| `hover`         | mouse cursor is over the element                                           |
| `last-child`    | element is the last child of its parent                                    |
| `motion-reduce` | `prefers-reduced-motion` media feature matches `reduce`                    |
| `motion-safe`   | `prefers-reduced-motion` media feature matches `no-preference`             |
| `odd-child`     | element is an odd-numbered child of its parent (zero-based index is even)  |
| `visited`       | link (`<a>`) has been visited                                              |

These prefixes can be combined with responsive prefixes.
For example, `md:hover:border`.

TODO: How can you enable a class when one of these conditions is NOT met?

## Tailwind Directives

Tailwind supports several directives described below.

### `@apply`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#apply",
"`@apply`" %} directive includes properties from
any number of Tailwind classes in a custom CSS rule.
This enables a set of Tailwind classes to be reused on several elements
without repeating them.

For example, the following can be added to `src/style.css`
after the `@tailwind` directives:

```css
@layer components {
  .my-alert {
    @apply bg-red-400 inline-block p-4 rounded-full text-2xl text-white;
  }
}
```

After a Tailwind build, the `my-alert` class can be used
to apply the styling of all the Tailwind classes
specified with the `@apply` directive.

For example:

```html
<div class="my-alert">Alert! Something is wrong.</div>
```

### `@layer`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#layer",
"`@layer`" %} directive is used to add custom CSS rules
to a specific bucket of Tailwind CSS rules.

### `@responsive`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#responsive",
"`@responsive`" %} directive generates
responsive variants of custom CSS classes
where the same styling is used for every breakpoint.
To use different styling for some breakpoints,
define media queries that override these.

### `@screen`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#screen",
"`@screen`" %} directive creates a media query that utilizes
a breakpoint value.
For example, instead of writing a media query like this:

```css
@media (min-width: 768px) {
  /* rules go here */
}
```

it can be written as follows to use the `min-width` value
specified for the `md` breakpoint:

```css
@screen md {
  /* rules go here */
}
```

### `@tailwind`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#tailwind",
"`@tailwind`" %} directive is used to inject Tailwind CSS class definitions
into the content of a CSS file.
Earlier we saw this used in the `src/style.css` file.

### `@variants`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#variants",
"`@variants`" %} directive is used to
generate variants of your own CSS classes that are responsive or
are only used when an element is hovered over, has focus, or is active.

## Tailwind Functions

Tailwind currently provides a single function, `theme`.

### `theme` Function

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#theme",
"`theme`" %} function is used obtain values from the Tailwind config
in CSS property values.
For example, to use a shade of yellow from Tailwind
in a custom CSS rule:

```css
.warning {
  background-color: theme('colors.yellow.600');
}
```

Note that the string passed to the `theme` function
is a dot-separated path to a Tailwind configuration value,
not the name of a Tailwind class.

The rule above could also be written using the the `@apply` directive
as follows:

```css
.warning {
  @apply bg-yellow-600;
}
```

The `theme` function is primarily useful when
a value will be used as part of a longer value
or in a calculation using the CSS `calc` function.

## Provided CSS Classes

The following sections list all the provided Tailwind CSS classes.
There are a large number of them!

This same information is available in the official docs at
{% aTargetBlank "https://tailwindcss.com/docs/", "tailwindcss.com/docs" %}.
It is included here in a more compact manner that is
more easily searchable because they are all on a single page.

Many Tailwind class names end with a numeric strength value.
Often this represents a multiple of `0.25rem`.
For example, the class `p-4` adds padding of `1rem` to all sides of an element.
Class names for colors can specify strength values that are multiples of 100.
For example, the class `text-red-400` specifies a certain shade of red.

Many Tailwind class names support specifying
an arbitrary value in square brackets.
For example, the class `p-[19px]`
adds padding of `19px` to all sides of an element.

In class names that include `-color`, `color` should be replaced by one of
`gray`, `red`, `orange`, `yellow`, `green`,
`teal`, `blue`, `indigo`, `purple`, or `pink`.

In the "CSS Property" column of the tables below,
lines where the property name start with `--` are
setting the value of a CSS variables (a.k.a. CSS custom properties).

### Backgrounds

| Class Name          | CSS Property                                                                             |
| ------------------- | ---------------------------------------------------------------------------------------- |
| `bg-fixed`          | `background-attachment: fixed;`                                                          |
| `bg-local`          | `background-attachment: local;`                                                          |
| `bg-scroll`         | `background-attachment: scroll;`                                                         |
|                     |                                                                                          |
| `bg-clip-border`    | `background-clip: border-box;`                                                           |
| `bg-clip-content`   | `background-clip: content-box;`                                                          |
| `bg-clip-padding`   | `background-clip: padding-box;`                                                          |
| `bg-clip-text`      | `background-clip: text;`                                                                 |
|                     |                                                                                          |
| `bg-transparent`    | `background-color: transparent;`                                                         |
| `bg-current`        | `background-color: currentColor;`                                                        |
| `bg-black`          | `background-color: #000;`                                                                |
| `bg-white`          | `background-color: #fff;`                                                                |
| `bg-slate-n`        | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-gray-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-zinc-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-neutral-n`      | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-stone-n`        | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-red-n`          | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-orange-n`       | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-amber-n`        | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-yellow-n`       | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-lime-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-green-n`        | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-emerald-n`      | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-teal-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-cyan-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-sky-n`          | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-blue-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-indigo-n`       | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-violet-n`       | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-purple-n`       | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-fuchsia-n`      | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-pink-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-rose-n`         | `background-color: #X;`<br>where X is 50, 950, or a multiple of 100                      |
| `bg-{color}-{n}`    | `background-color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100      |
|                     |                                                                                          |
| `bg-opacity-{n}`    | `--bg-opacity: {n}/100;`<br>where n = 0, 25, 50, 75, or 100                              |
|                     |                                                                                          |
| `bg-bottom`         | `background-position: bottom;`                                                           |
| `bg-center`         | `background-position: center;`                                                           |
| `bg-left`           | `background-position: left;`                                                             |
| `bg-left-bottom`    | `background-position: left bottom;`                                                      |
| `bg-left-top`       | `background-position: left top;`                                                         |
| `bg-right`          | `background-position: right;`                                                            |
| `bg-right-bottom`   | `background-position: right bottom;`                                                     |
| `bg-right-top`      | `background-position: right top;`                                                        |
| `bg-top`            | `background-position: top;`                                                              |
|                     |                                                                                          |
| `bg-repeat`         | `background-repeat: repeat;`                                                             |
| `bg-no-repeat`      | `background-repeat: no-repeat;`                                                          |
| `bg-repeat-x`       | `background-repeat: repeat-x;`                                                           |
| `bg-repeat-y`       | `background-repeat: repeat-y;`                                                           |
| `bg-repeat-round`   | `background-repeat: round;`                                                              |
| `bg-repeat-space`   | `background-repeat: space;`                                                              |
|                     |                                                                                          |
| `bg-auto`           | `background-size: auto;`                                                                 |
| `bg-contain`        | `background-size: contain;`                                                              |
| `bg-cover`          | `background-size: cover;`                                                                |
|                     |                                                                                          |
| `bg-none`           | `background-image: none;`                                                                |
| `bg-gradient-to-t`  | `background-image: linear-gradient(to top, var(--gradient-color-stops));`                |
| `bg-gradient-to-tr` | `background-image: linear-gradient(to top right, var(--gradient-color-stops));`          |
| `bg-gradient-to-r`  | `background-image: linear-gradient(to right, var(--gradient-color-stops));`              |
| `bg-gradient-to-br` | `background-image: linear-gradient(to bottom right, var(--gradient-color-stops));`       |
| `bg-gradient-to-b`  | `background-image: linear-gradient(to bottom, var(--gradient-color-stops));`             |
| `bg-gradient-to-bl` | `background-image: linear-gradient(to bottom left, var(--gradient-color-stops));`        |
| `bg-gradient-to-l`  | `background-image: linear-gradient(to left, var(--gradient-color-stops));`               |
| `bg-gradient-to-tl` | `background-image: linear-gradient(to top left, var(--gradient-color-stops));`           |
|                     |                                                                                          |
| `from-transparent`  | `--gradient-from-color: transparent`                                                     |
| `from-current`      | `--gradient-from-color: currentColor`                                                    |
| `from-black`        | `--gradient-from-color: #000`                                                            |
| `from-white`        | `--gradient-from-color: #fff`                                                            |
| `from-{color}-{n}`  | `--gradient-from-color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100 |
|                     |                                                                                          |
| `via-transparent`   | `--gradient-via-color: transparent`                                                      |
| `via-current`       | `--gradient-via-color: currentColor`                                                     |
| `via-black`         | `--gradient-via-color: #000`                                                             |
| `via-white`         | `--gradient-via-color: #fff`                                                             |
| `via-{color}-{n}`   | `--gradient-via-color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100  |
|                     |                                                                                          |
| `to-transparent`    | `--gradient-to-color: transparent`                                                       |
| `to-current`        | `--gradient-to-color: currentColor`                                                      |
| `to-black`          | `--gradient-to-color: #000`                                                              |
| `to-white`          | `--gradient-to-color: #fff`                                                              |
| `to-{color}-{n}`    | `--gradient-to-color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100   |

### Borders

In the class names below, `side` can be one of the following:

| Side Abbreviation | Side Name | CSS Properties Affected                        |
| ----------------- | --------- | ---------------------------------------------- |
| `b`               | bottom    | `border-bottom-left` and `border-bottom-right` |
| `l`               | left      | `border-bottom-left` and `border-top-left`     |
| `r`               | right     | `border-bottom-right` and `border-top-right`   |
| `t`               | top       | `border-top-left` and `border-top-right`       |

In the class names below, `corner` can be one of the following:

| Corner Abbreviation | Corner Name  | CSS Property Affected |
| ------------------- | ------------ | --------------------- |
| `bl`                | bottom left  | `border-bottom-left`  |
| `br`                | bottom right | `border-bottom-right` |
| `tl`                | top left     | `border-top-left`     |
| `tr`                | top right    | `border-top-right`    |

&nbsp;

| Class Name                   | CSS Property                                                                    |
| ---------------------------- | ------------------------------------------------------------------------------- |
| `rounded-none`               | `border-radius: 0;`                                                             |
| `rounded-sm`                 | `border-radius: 0.125rem;`                                                      |
| `rounded`                    | `border-radius: 0.25rem;`                                                       |
| `rounded-md`                 | `border-radius: 0.375rem;`                                                      |
| `rounded-lg`                 | `border-radius: 0.5rem;`                                                        |
| `rounded-full:`              | `border-radius: 9999px;`                                                        |
| `rounded-{side/corner}-none` | sets affected properties to `0`                                                 |
| `rounded-{side/corner}-sm`   | sets affected properties to `0.125rem`                                          |
| `rounded-{side/corner}`      | sets affected properties to `0.25rem`                                           |
| `rounded-{side/corner}-md`   | sets affected properties to `0.375rem`                                          |
| `rounded-{side/corner}-lg`   | sets affected properties to `0.5rem`                                            |
| `rounded-{side/corner}-full` | sets affected properties to `9999px`                                            |
|                              |                                                                                 |
| `border`                     | `border-width: 1px;`                                                            |
| `border-{n}`                 | `border-width: {n}px;`<br>where n = 0, 2, 4, or 8                               |
| `border-side`                | sets affected property: to `1px;`                                               |
| `border-side-{n}`            | sets affected property: to `{n}px;`<br>where n = 0, 2, 4, or 8                  |
|                              |                                                                                 |
| `border-transparent`         | `border-color: transparent;`                                                    |
| `border-current`             | `border-color: currentColor;`                                                   |
| `border-black`               | `border-color: #000;`                                                           |
| `border-white`               | `border-color: #fff;`                                                           |
| `border-{color}-{n}`         | `border-color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100 |
|                              |                                                                                 |
| `border-opacity-{n}`         | `--border-opacity: {n}/100;`<br>where n = 0, 25, 50, 75, or 100                 |
|                              |                                                                                 |
| `border-dashed`              | `border-style: dashed;`                                                         |
| `border-dotted`              | `border-style: dotted;`                                                         |
| `border-double`              | `border-style: double;`                                                         |
| `border-none`                | `border-style: none;`                                                           |
| `border-solid`               | `border-style: solid;`                                                          |
|                              |                                                                                 |
| `divide-x`                   | `border-left-width: 1px;`                                                       |
| `divide-x-{n}`               | `border-left-width: {n}px;`<br>where n = 0, 2, 4, or 8                          |
| `divide-x-reverse`           | `--divide-x-reverse: 1;`                                                        |
| `divide-y`                   | `border-top-width: 1px;`                                                        |
| `divide-y-{n}`               | `border-top-width: {n}px;`<br>where n = 0, 2, 4, or 8                           |
| `divide-y-reverse`           | `--divide-y-reverse: 1;`                                                        |
|                              |                                                                                 |
| `divide-transparent`         | `border-color: transparent;`                                                    |
| `divide-current`             | `border-color: currentColor;`                                                   |
| `divide-black`               | `border-color: #000;`                                                           |
| `divide-white`               | `border-color: #fff;`                                                           |
| `divide-{color}-{n}`         | `border-color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100 |
|                              |                                                                                 |
| `divide-opacity-{n}`         | `--divide-opacity: {n}/100;`<br>where n = 0, 25, 50, 75, or 100                 |
|                              |                                                                                 |
| `divide-dashed`              | `border-style: dashed;`                                                         |
| `divide-dotted`              | `border-style: dotted;`                                                         |
| `divide-double`              | `border-style: double;`                                                         |
| `divide-none`                | `border-style: none;`                                                           |
| `divide-solid`               | `border-style: solid;`                                                          |

### <a name="box-alignment">Box Alignment</a>

| Name Prefix             | CSS Property                      |
| ----------------------- | --------------------------------- |
| `content-center`        | `align-content: center;`          |
| `content-start`         | `align-content: flex-start;`      |
| `content-end`           | `align-content: flex-end;`        |
| `content-between`       | `align-content: space-between;`   |
| `content-around`        | `align-content: space-around;`    |
| `content-evenly`        | `align-content: space-evenly;`    |
|                         |                                   |
| `items-baseline;`       | `align-items: baseline;`          |
| `items-center;`         | `align-items: center;`            |
| `items-end;`            | `align-items: flex-end;`          |
| `items-start;`          | `align-items: flex-start;`        |
| `items-stretch;`        | `align-items: stretch;`           |
|                         |                                   |
| `justify-around`        | `justify-content: space-around;`  |
| `justify-between`       | `justify-content: space-between;` |
| `justify-center`        | `justify-content: center;`        |
| `justify-end`           | `justify-content: flex-end;`      |
| `justify-evenly`        | `justify-content: space-evenly;`  |
| `justify-start`         | `justify-content: flex-start;`    |
|                         |                                   |
| `justify-items-auto`    | `justify-items: auto;`            |
| `justify-items-center`  | `justify-items: center;`          |
| `justify-items-end`     | `justify-items: end;`             |
| `justify-items-start`   | `justify-items: start;`           |
| `justify-items-stretch` | `justify-items: stretch;`         |
|                         |                                   |
| `justify-self-auto`     | `justify-self: auto;`             |
| `justify-self-center`   | `justify-self: center;`           |
| `justify-self-end`      | `justify-self: end;`              |
| `justify-self-start`    | `justify-self: start;`            |
| `justify-self-stretch`  | `justify-self: stretch;`          |
|                         |                                   |
| `place-content-around`  | `place-content: space-around;`    |
| `place-content-between` | `place-content: space-between;`   |
| `place-content-center`  | `place-content: center;`          |
| `place-content-end`     | `place-content: end;`             |
| `place-content-evenly`  | `place-content: space-evenly;`    |
| `place-content-start`   | `place-content: start;`           |
| `place-content-stretch` | `place-content: stretch;`         |
|                         |                                   |
| `place-items-auto`      | `place-items: auto;`              |
| `place-items-center`    | `place-items: center;`            |
| `place-items-end`       | `place-items: end;`               |
| `place-items-start`     | `place-items: start;`             |
| `place-items-stretch`   | `place-items: stretch;`           |
|                         |                                   |
| `place-self-auto`       | `place-self: auto;`               |
| `place-self-center`     | `place-self: center;`             |
| `place-self-end`        | `place-self: end;`                |
| `place-self-start`      | `place-self: start;`              |
| `place-self-stretch`    | `place-self: stretch;`            |
|                         |                                   |
| `self-auto`             | `align-self: auto;`               |
| `self-center`           | `align-self: center;`             |
| `self-end`              | `align-self: flex-end;`           |
| `self-start`            | `align-self: flex-start;`         |
| `self-stretch`          | `align-self: stretch;`            |

### Box Sizing

| Name Prefix   | CSS Property               |
| ------------- | -------------------------- |
| `box-border`  | `box-sizing: border-box;`  |
| `box-content` | `box-sizing: content-box;` |

### Container

| Name Prefix | Effect                                                                                 |
| ----------- | -------------------------------------------------------------------------------------- |
| `container` | sets `max-width` to breakpoint size or<br>`width` to `100%` if no breakpoint specified |

For example, `lg:container` or `container`.

### Display

| Name Prefix          | CSS Property                   |
| -------------------- | ------------------------------ |
| `block`              | `display: block;`              |
| `inline`             | `display: inline;`             |
| `inline-block`       | `display: inline-block;`       |
| `flex`               | `display: flex;`               |
| `inline-flex`        | `display: inline-flex;`        |
| `table`              | `display: table;`              |
| `table-caption`      | `display: table-caption;`      |
| `table-cell`         | `display: table-cell;`         |
| `table-column-group` | `display: table-column-group;` |
| `table-footer-group` | `display: table-footer-group;` |
| `table-header-group` | `display: table-header-group;` |

### Effects

The `box-shadow` values set by the `shadow` classes
are somewhat complicated. For details see the {% aTargetBlank
"https://tailwindcss.com/docs/box-shadow", "Box Shadow" %} docs.

| Name Prefix      | CSS Property                                           |
| ---------------- | ------------------------------------------------------ |
| `opacity-n`      | `opacity: {n}/100;`<br>where n = 0, 25, 50, 75, or 100 |
|                  |                                                        |
| `shadow-none`    | `box-shadow: none`                                     |
| `shadow-inner`   | `box-shadow: ...`; see docs                            |
| `shadow-outline` | `box-shadow: ...`; see docs                            |
| `shadow`         | `box-shadow: ...`; see docs                            |
| `shadow-xs`      | `box-shadow: ...`; see docs                            |
| `shadow-sm`      | `box-shadow: ...`; see docs                            |
| `shadow-md`      | `box-shadow: ...`; see docs                            |
| `shadow-lg`      | `box-shadow: ...`; see docs                            |
| `shadow-xl`      | `box-shadow: ...`; see docs                            |
| `shadow-2xl`     | `box-shadow: ...`; see docs                            |

### Flexbox

| Name Prefix         | CSS Property                        |
| ------------------- | ----------------------------------- |
| `flex-1`            | `flex: 1 1 0%;`                     |
| `flex-auto`         | `flex: 1 1 auto;`                   |
| `flex-initial`      | `flex: 0 1 auto;`                   |
| `flex-none`         | `flex: none;`                       |
| `flex-col`          | `flex-direction: column;`           |
| `flex-col-reverse`  | `flex-direction: column-reverse;`   |
| `flex-row`          | `flex-direction: row;`              |
| `flex-row-reverse`  | `flex-direction: row-reverse;`      |
| `flex-wrap`         | `flex-wrap: wrap;`                  |
| `flex-wrap-reverse` | `flex-wrap: wrap-reverse;`          |
| `flex-no-wrap`      | `flex-wrap: nowrap;`                |
| `flex-grow`         | `flex-grow: 1;`                     |
| `flex-grow-0`       | `flex-grow: 0;`                     |
| `flex-shrink`       | `flex-shrink: 1;`                   |
| `flex-shrink-0`     | `flex-shrink: 0;`                   |
| `order-{n}`         | `order: {n};`<br>where n is 1 to 12 |

Also see the classes in the
[Box Alignment](#box-alignment) category.

### Float and Clear

| Name Prefix   | CSS Property                                            |
| ------------- | ------------------------------------------------------- |
| `clear-both`  | `clear: both;`                                          |
| `clear-left`  | `clear: left;`                                          |
| `clear-none`  | `clear: none;`                                          |
| `clear-right` | `clear: right;`                                         |
| `clearfix`    | `&:after { content: ""; display: table; clear: both; }` |
| `float-left`  | `float: left;`                                          |
| `float-none`  | `float: none;`                                          |
| `float-right` | `float: right;`                                         |

### Grid

| Name Prefix           | CSS Property                                                                                 |
| --------------------- | -------------------------------------------------------------------------------------------- |
| `auto-cols-auto`      | `grid-auto-columns: auto`                                                                    |
| `auto-cols-fr`        | `grid-auto-columns: minmax(0, 1fr)`                                                          |
| `auto-cols-max`       | `grid-auto-columns: max-content`                                                             |
| `auto-cols-min`       | `grid-auto-columns: min-content`                                                             |
| `auto-rows-auto`      | `grid-auto-rows: auto`                                                                       |
| `auto-rows-fr`        | `grid-auto-rows: minmax(0, 1fr)`                                                             |
| `auto-rows-max`       | `grid-auto-rows: max-content`                                                                |
| `auto-rows-min`       | `grid-auto-rows: min-content`                                                                |
| `col-auto`            | `grid-column: auto`                                                                          |
| `col-end-{n}`         | `grid-column-end: {n}`<br>where n is 1 to 13                                                 |
| `col-end-auto`        | `grid-column-end: auto`                                                                      |
| `col-span-{n}`        | `grid-column: span {n} / span {n}`<br>where n is 1 to 12                                     |
| `col-span-full`       | `grid-column: 1 / -1`                                                                        |
| `col-start-{n}`       | `grid-column-start: {n}`<br>where n is 1 to 13                                               |
| `col-start-auto`      | `grid-column-start: auto`                                                                    |
| `gap-{n}`             | `gap: {n}`<br>where n is 0 to 8, 10, 12, 16, 20, 24, 32, 40, 48, 56, or 64                   |
| `gap-px`              | `gap: 1px`                                                                                   |
| `gap-x-{n}`           | `column-gap: {n}*0.25rem`<br>where n is 0 to 6, 8, 10, 12, 16, 20, 24, 32, 40, 48, 56, or 64 |
| `gap-x-px`            | `column-gap: 1px`                                                                            |
| `gap-y-{n}`           | `row-gap: {n}*0.25rem`<br>where n is 0 to 6, 8, 10, 12, 16, 20, 24, 32, 40, 48, 56, or 64    |
| `gap-y-px`            | `row-gap: 1px`                                                                               |
| `grid-cols-{n}`       | `grid-template-columns: repeat({n}, minmax(0, 1fr));`<br>where n is 1 to 12                  |
| `grid-cols-none`      | `grid-template-columns: none;`                                                               |
| `grid-flow-col-dense` | `grid-auto-flow: column dense`                                                               |
| `grid-flow-col`       | `grid-auto-flow: column`                                                                     |
| `grid-flow-row-dense` | `grid-auto-flow: row dense`                                                                  |
| `grid-flow-row`       | `grid-auto-flow: row`                                                                        |
| `grid-rows-{n}`       | `grid-template-rows: repeat({n}, minmax(0, 1fr));`<br>where n is 1 to 6                      |
| `grid-rows-none`      | `grid-template-rows: none;`                                                                  |
| `row-auto`            | `grid-row: auto`                                                                             |
| `row-end-{n}`         | `grid-row-end: {n}`<br>where n is 1 to 7                                                     |
| `row-end-auto`        | `grid-row-end: auto`                                                                         |
| `row-span-{n}`        | `grid-row: span {n} / span {n}`<br>where n is 1 to 6                                         |
| `row-start-{n}`       | `grid-row-start: {n}`<br>where n is 1 to 7                                                   |
| `row-start-auto`      | `grid-row-start: auto`                                                                       |

### Interactivity

| Name Prefix           | CSS Property            |
| --------------------- | ----------------------- |
| `appearance-none`     | `appearance: none;`     |
|                       |                         |
| `cursor-auto`         | `cursor: auto;`         |
| `cursor-default`      | `cursor: default;`      |
| `cursor-pointer`      | `cursor: pointer;`      |
| `cursor-wait`         | `cursor: wait;`         |
| `cursor-text`         | `cursor: text;`         |
| `cursor-move`         | `cursor: move;`         |
| `cursor-not-allowed`  | `cursor: not-allowed;`  |
|                       |                         |
| `outline-none`        | `outline: none;`        |
|                       |                         |
| `pointer-events-auto` | `pointer-events: auto;` |
| `pointer-events-none` | `pointer-events: none;` |
|                       |                         |
| `resize-none`         | `resize: none;`         |
| `resize-x`            | `resize: horizontal;`   |
| `resize-y`            | `resize: vertical;`     |
| `resize`              | `resize: both;`         |
|                       |                         |
| `select-none`         | `user-select: none;`    |
| `select-all`          | `user-select: all;`     |
| `select-auto`         | `user-select: auto;`    |
| `select-text`         | `user-select: text;`    |

### Object Fit

| Name Prefix         | CSS Property              |
| ------------------- | ------------------------- |
| `object-contain`    | `object-fit: contain;`    |
| `object-cover`      | `object-fit: cover;`      |
| `object-fill`       | `object-fit: fill;`       |
| `object-none`       | `object-fit: none;`       |
| `object-scale-down` | `object-fit: scale-down;` |

### Object Position

| Name Prefix           | CSS Property                     |
| --------------------- | -------------------------------- |
| `object-bottom`       | `object-position: bottom;`       |
| `object-center`       | `object-position: center;`       |
| `object-left`         | `object-position: left;`         |
| `object-left-bottom`  | `object-position: left bottom;`  |
| `object-left-top`     | `object-position: left top;`     |
| `object-right`        | `object-position: right;`        |
| `object-right-bottom` | `object-position: right bottom;` |
| `object-right-top`    | `object-position: right top;`    |
| `object-top`          | `object-position: top;`          |

### Overflow

| Name Prefix          | CSS Property           |
| -------------------- | ---------------------- |
| `overflow-auto`      | `overflow: auto;`      |
| `overflow-hidden`    | `overflow: hidden;`    |
| `overflow-scroll`    | `overflow: scroll;`    |
| `overflow-visible`   | `overflow: visible;`   |
| `overflow-x-auto`    | `overflow-x: auto;`    |
| `overflow-x-hidden`  | `overflow-x: hidden;`  |
| `overflow-x-scroll`  | `overflow-x: scroll;`  |
| `overflow-x-visible` | `overflow-x: visible;` |
| `overflow-y-auto`    | `overflow-y: auto;`    |
| `overflow-y-hidden`  | `overflow-y: hidden;`  |
| `overflow-y-scroll`  | `overflow-y: scroll;`  |
| `overflow-y-visible` | `overflow-y: visible;` |

### Overscroll Behavior

| Name Prefix            | CSS Property                      |
| ---------------------- | --------------------------------- |
| `overscroll-auto`      | `overscroll-behavior: auto;`      |
| `overscroll-contain`   | `overscroll-behavior: contain;`   |
| `overscroll-none`      | `overscroll-behavior: none;`      |
| `overscroll-x-auto`    | `overscroll-behavior-x: auto;`    |
| `overscroll-x-contain` | `overscroll-behavior-x: contain;` |
| `overscroll-x-none`    | `overscroll-behavior-x: none;`    |
| `overscroll-y-auto`    | `overscroll-behavior-y: auto;`    |
| `overscroll-y-contain` | `overscroll-behavior-y: contain;` |
| `overscroll-y-none`    | `overscroll-behavior-y: none;`    |

### Position

| Name Prefix    | CSS Property                                        |
| -------------- | --------------------------------------------------- |
| `absolute`     | `position: absolute;`                               |
| `fixed`        | `position: fixed;`                                  |
| `relative`     | `position: relative;`                               |
| `static`       | `position: static;`                                 |
| `sticky`       | `position: sticky;`                                 |
|                |                                                     |
| `inset-0`      | `bottom: 0; left: 0; right: 0; top: 0;`             |
| `inset-auto`   | `bottom: auto; left: auto; right: auto; top: auto;` |
|                |                                                     |
| `inset-x-0`    | `left: 0; right: 0;`                                |
| `inset-x-auto` | `left: auto; right: auto;`                          |
|                |                                                     |
| `inset-y-0`    | `bottom: 0; top: 0;`                                |
| `inset-y-auto` | `bottom: auto; top: auto;`                          |
|                |                                                     |
| `bottom-{s}`   | `top: {s};` where s is `0` or `auto`                |
| `left-{s}`     | `left: {s};` where s is `0` or `auto`               |
| `right-{s}`    | `right: {s};` where s is `0` or `auto`              |
| `top-{s}`      | `top: {s};` where s is `0` or `auto`                |

### Sizing

| Name Prefix    | CSS Property                                                   |
| -------------- | -------------------------------------------------------------- |
| `h-{n}`        | `height: {n}*0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20 |
| `w-{n}`        | `width: {n}*0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20  |
|                |                                                                |
| `max-h-full`   | `max-height: 100%;`                                            |
| `max-h-screen` | `max-height: 100vh;`                                           |
|                |                                                                |
| `max-w-none`   | `max-width: none;`                                             |
| `max-w-xs`     | `max-width: 20rem;`                                            |
| `max-w-sm`     | `max-width: 24rem;`                                            |
| `max-w-md`     | `max-width: 28rem;`                                            |
| `max-w-lg`     | `max-width: 32rem;`                                            |
| `max-w-xl`     | `max-width: 36rem;`                                            |
| `max-w-2xl`    | `max-width: 42rem;`                                            |
| `max-w-3xl`    | `max-width: 48rem;`                                            |
| `max-w-4xl`    | `max-width: 56rem;`                                            |
| `max-w-5xl`    | `max-width: 64rem;`                                            |
| `max-w-6xl`    | `max-width: 72rem;`                                            |
| `max-w-full`   | `max-width: 100%;`                                             |
|                |                                                                |
| `min-h-0`      | `min-height: 0;`                                               |
| `min-h-full`   | `min-height: 100%;`                                            |
| `min-h-screen` | `min-height: 100vh;`                                           |
|                |                                                                |
| `min-w-0`      | `min-width: 0;`                                                |
| `min-w-full`   | `min-width: 100%;`                                             |

### Spacing

| Name Prefix   | CSS Properties                                                                   |
| ------------- | -------------------------------------------------------------------------------- |
| `m-{n}`       | `margin: {n}*0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20                   |
| `ml-*`        | `margin-left: {n}*0.25rem; margin-right: {n}*0.25rem;`<br>where n = 0, 1, or 2   |
| `mr-*`        | `margin-right: {n}*0.25rem; margin-right: {n}*0.25rem;`<br>where n = 0, 1, or 2  |
| `mt-*`        | `margin-top: {n}*0.25rem; margin-right: {n}*0.25rem;`<br>where n = 0, 1, or 2    |
| `mx-*`        | `margin-left: {n}*0.25rem; margin-right: {n}*0.25rem;`<br>where n = 0, 1, or 2   |
| `my-*`        | `margin-bottom: {n}*0.25rem; margin-top: {n}*0.25rem;`<br>where n = 0, 1, or 2   |
| `p-{n}`       | `padding: {n}*0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20                  |
| `pl-*`        | `padding-left: {n}*0.25rem; margin-right: {n}*0.25rem;`<br>where n = 0, 1, or 2  |
| `pr-*`        | `padding-right: {n}*0.25rem; margin-right: {n}*0.25rem;`<br>where n = 0, 1, or 2 |
| `px-*`        | `padding-left: {n}*0.25rem; padding-right: {n}*0.25rem;`<br>where n = 0, 1, or 2 |
| `py-*`        | `padding-bottom: {n}*0.25rem; padding-top: {n}*0.25rem;`<br>where n = 0, 1, or 2 |
| `space-x-{n}` | `margin-left: {n}*0.25rem;`<br>where n is 0 to 5                                 |
| `space-y-{n}` | `margin-top: {n}*0.25rem;`<br>where n is 0 to 5                                  |

### SVG

| Name Prefix      | CSS Property                               |
| ---------------- | ------------------------------------------ |
| `fill-current`   | `fill: currentColor;`                      |
| `stroke-current` | `stroke: currentColor;`                    |
| `stroke-{n}`     | `stroke-width: {n};` where n is 0, 1, or 2 |

### Tables

| Name Prefix       | CSS Property                 |
| ----------------- | ---------------------------- |
| `border-collapse` | `border-collapse: collapse;` |
| `border-separate` | `border-collapse: separate;` |
|                   |                              |
| `table-auto`      | `table-layout: auto;`        |
| `table-fixed`     | `table-layout: fixed;`       |

### Transforms

For all the classes in this section, `n` can be
0, 50, 75, 90, 95, 100, 105, 110, 125, or 150.

| Name Prefix           | CSS Property                                                                                             |
| --------------------- | -------------------------------------------------------------------------------------------------------- |
| `scale-{n}`           | `--transform-scale-x: {n}/100; --transform-scale-y: {n}/100;`                                            |
| `scale-x-{n}`         | `--transform-scale-x: {n}/100;`                                                                          |
| `scale-y-{n}`         | `--transform-scale-y: {n}/100;`                                                                          |
|                       |                                                                                                          |
| `rotate-{n}`          | `--transform-rotate: {n}deg;`<br>where n is 0, 45, 90, or 180                                            |
| `-rotate-{n}`         | `--transform-rotate: -{n}deg;`<br>where n is 45, 90, or 180                                              |
|                       |                                                                                                          |
| `translate-x-px`      | `--transform-translate-x: 1px;`                                                                          |
| `-translate-x-px`     | `--transform-translate-x: -1px;`                                                                         |
| `translate-x-{n}`     | `--transform-translate-x: {n}*0.25rem;`<br>where n is 0 to 8, 10, 12, 16, 20, 24, 32, 40, 48, 56, or 64  |
| `-translate-x-{n}`    | `--transform-translate-x: -{n}*0.25rem;`<br>where n is 1 to 8, 10, 12, 16, 20, 24, 32, 40, 48, 56, or 64 |
| `translate-x-full`    | `--transform-translate-x: 100%;`                                                                         |
| `-translate-x-full`   | `--transform-translate-x: -100%;`                                                                        |
| `translate-x-1/2`     | `--transform-translate-x: 50%;`                                                                          |
| `-translate-x-1/2`    | `--transform-translate-x: -50%;`                                                                         |
|                       |                                                                                                          |
| `translate-y-{above}` | `--transform-translate-y: ...;`<br>all the same variations as for `x` above                              |
|                       |                                                                                                          |
| `skew-x-{n}`          | `--transform-skew-x: {n}deg;`<br>where n is 0, 3, 6, or 12                                               |
| `-skew-x-{n}`         | `--transform-skew-x: {n}deg;`<br>where n is 3, 6, or 12                                                  |
| `skew-y-{n}`          | `--transform-skew-y: ...;`<br>all the same variations as for `x` above                                   |
|                       |                                                                                                          |
| `origin-bottom-right` | `transform-origin: bottom right;`                                                                        |
| `origin-bottom`       | `transform-origin: bottom;`                                                                              |
| `origin-center`       | `transform-origin: center;`                                                                              |
| `origin-left`         | `transform-origin: left;`                                                                                |
| `origin-right`        | `transform-origin: right;`                                                                               |
| `origin-top-left`     | `transform-origin: top left;`                                                                            |
| `origin-top-right`    | `transform-origin: top right;`                                                                           |
| `origin-top`          | `transform-origin: top;`                                                                                 |

### Transitions and Animation

The Tailwind `transition-*`, `ease-*`, and `duration-*` classes
can be combined to easily create
transitions from one CSS property value to another.
For example, the following set of Tailwind classes on a button
cause it to transition from having a red border and light red background
to a green border and light green background on hover.
The size of the button also increases slightly on hover.

```css
<button className="
  border-2 px-2 py-1 rounded-full
  border-red-700 bg-red-300
  hover:border-green-700 hover:bg-green-300 hover:scale-110
  transition ease-linear duration-500
">
  Press Me
</button>
```

The `animation` and `@keyframe` values set by the `animate` classes
are somewhat complicated. For details see the {% aTargetBlank
"https://tailwindcss.com/docs/animation", "Animation" %} docs.

| Name Prefix            | CSS Property                                                                                                           |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `transition`           | `transition-property: background-color, border-color, color, fill, stroke, opacity, box-shadow, transform;`            |
| `transition-all`       | `transition-property: all;`                                                                                            |
| `transition-colors`    | `transition-property: background-color, border-color, color, fill, stroke;`                                            |
| `transition-none`      | `transition-property: none;`                                                                                           |
| `transition-opacity`   | `transition-property: opacity;`                                                                                        |
| `transition-shadow`    | `transition-property: box-shadow;`                                                                                     |
| `transition-transform` | `transition-property: transform;`                                                                                      |
|                        |                                                                                                                        |
| `duration-{n}`         | `transition-duration: {n}ms;`<br>where n is 75, 100, 150, 200, 300, 500, 700, or 1000                                  |
|                        |                                                                                                                        |
| `ease-linear`          | `transition-timing-function: linear;`                                                                                  |
| `ease-in`              | `transition-timing-function: cubic-bezier(0.4, 0, 1, 1);`                                                              |
| `ease-out`             | `transition-timing-function: cubic-bezier(0, 0, 0.2, 1);`                                                              |
| `ease-in-out`          | `transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);`                                                            |
|                        |                                                                                                                        |
| `delay-{n}`            | `transition-delay: {n}ms;`<br>where n is 75, 100, 150, 200, 300, 500, 700, or 1000                                     |
|                        |                                                                                                                        |
| `animate-none`         | `animation: none;`                                                                                                     |
| `animate-{effect}`     | `animation: {effect} ...; @keyframes {effect} {...}`<br>where effect is `bounce`, `ping`, `pulse` or, `spin`; see docs |

### Typography/Fonts

In the class name `text-{color}-{n}`, a number is required at the end.
For example, `text-red` is not a valid Tailwind CSS class name.

| Name Prefix               | CSS Property                                                                |
| ------------------------- | --------------------------------------------------------------------------- |
| `font-sans`               | `font-family: boatload of san serif fonts;`<br>includes Arial               |
| `font-serif`              | `font-family: boatload of serif fonts;`<br>includes Times New Roman         |
| `font-mono`               | `font-family: boatload of monospace fonts;`<br>includes Courier New         |
|                           |                                                                             |
| `text-xs`                 | `font-size: 0.75rem;`                                                       |
| `text-sm`                 | `font-size: 0.875rem;`                                                      |
| `text-base`               | `font-size: 1rem;`<br>Why not named `text-md`?                              |
| `text-lg`                 | `font-size: 1.125rem;`                                                      |
| `text-xl`                 | `font-size: 1.25rem;`                                                       |
| `text-2xl`                | `font-size: 1.5rem;`                                                        |
| `text-3xl`                | `font-size: 1.875rem;`                                                      |
| `text-4xl`                | `font-size: 2.25rem;`                                                       |
| `text-5xl`                | `font-size: 3rem;`                                                          |
| `text-6xl`                | `font-size: 4rem;`                                                          |
|                           |                                                                             |
| `antialiased`             | `font-smoothing`<br>with vendor-specific prefixes and values                |
| `subpixel-antialiased`    | `font-smoothing: antialiased;`<br>with vendor-specific prefixes and values  |
|                           |                                                                             |
| `italic`                  | `font-style: italic;`                                                       |
| `not-italic`              | `font-style: normal;`<br>odd class name!                                    |
|                           |                                                                             |
| `font-hairline`           | `font-weight: 100;`                                                         |
| `font-thin`               | `font-weight: 200;`                                                         |
| `font-light`              | `font-weight: 300;`                                                         |
| `font-normal`             | `font-weight: 400;`                                                         |
| `font-medium`             | `font-weight: 500;`                                                         |
| `font-semibold`           | `font-weight: 600;`                                                         |
| `font-bold`               | `font-weight: 700;`                                                         |
| `font-extrabold`          | `font-weight: 800;`                                                         |
| `font-black`              | `font-weight: 900;`                                                         |
|                           |                                                                             |
| `normal-nums`             | `font-variant-numeric: normal;`                                             |
| `ordinal`                 | `font-variant-numeric: ordinal;`                                            |
| `slashed-zero`            | `font-variant-numeric: slashed-zero;`                                       |
| `lining-nums`             | `font-variant-numeric: lining-nums;`                                        |
| `oldstyle-nums`           | `font-variant-numeric: oldstyle-nums;`                                      |
| `proportional-nums`       | `font-variant-numeric: proportional-nums;`                                  |
| `tabular-nums`            | `font-variant-numeric: tabular-nums;`                                       |
| `diagonal-fractions`      | `font-variant-numeric: diagonal-fractions;`                                 |
| `stacked-fractions`       | `font-variant-numeric: stacked-fractions;`                                  |
|                           |                                                                             |
| `tracking-tighter`        | `letter-spacing: -0.05em;`                                                  |
| `tracking-tight`          | `letter-spacing: -0.025em;`                                                 |
| `tracking-normal`         | `letter-spacing: 0em;`                                                      |
| `tracking-wide`           | `letter-spacing: 0.025em;`                                                  |
| `tracking-wider`          | `letter-spacing: 0.05em;`                                                   |
| `tracking-widest`         | `letter-spacing: 0.1em;`                                                    |
|                           |                                                                             |
| `leading-{n}`             | `line-height: {n}*0.25rem;`<br>where n = 3 to 10                            |
| `leading-none`            | `line-height: 1;`                                                           |
| `leading-tight`           | `line-height: 1.25rem;`                                                     |
| `leading-snug`            | `line-height: 1.375rem;`                                                    |
| `leading-normal`          | `line-height: 1.5rem;`                                                      |
|                           |                                                                             |
| `list-none`               | `list-style-type: none;`                                                    |
| `list-disc`               | `list-style-type: disc;`                                                    |
| `list-decimal`            | `list-style-type: decimal;`                                                 |
|                           |                                                                             |
| `list-inside`             | `list-style-position: inside;`                                              |
| `list-outside`            | `list-style-position: outside;`                                             |
|                           |                                                                             |
| `placeholder-transparent` | `color: transparent;`                                                       |
| `placeholder-current`     | `color: currentColor;`                                                      |
| `placeholder-black`       | `color: #000;`                                                              |
| `placeholder-white`       | `color: #fff;`                                                              |
| `placeholder-{color}-{n}` | `color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100    |
|                           |                                                                             |
| `placeholder-opacity-{n}` | `--placeholder-opacity: {n}/100;`<br>where n = 0, 25, 50, 75, or 100        |
|                           |                                                                             |
| `text-center`             | `text-align: center;`                                                       |
| `text-justify`            | `text-align: justify;`                                                      |
| `text-left`               | `text-align: left;`                                                         |
| `text-right`              | `text-align: right;`                                                        |
|                           |                                                                             |
| `text-current`            | `color: currentColor;`                                                      |
| `text-black`              | `color: #000;`                                                              |
| `text-white`              | `color: #fff;`                                                              |
| `text-transparent`        | `color: transparent;`                                                       |
| `text-{color}-{n}`        | `color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100    |
| `text-slate-n`            | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-gray-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-zinc-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-neutral-n`          | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-stone-n`            | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-red-n`              | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-orange-n`           | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-amber-n`            | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-yellow-n`           | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-lime-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-green-n`            | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-emerald-n`          | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-teal-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-cyan-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-sky-n`              | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-blue-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-indigo-n`           | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-violet-n`           | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-purple-n`           | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-fuchsia-n`          | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-pink-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
| `text-rose-n`             | `color: #X;`<br>where X is 50, 950, or a multiple of 100                    |
|                           |                                                                             |
| `text-opacity-{n}`        | `--text-opacity: {n}/100;`<br>where n is 0, 25, 50, 75, or 100              |
|                           |                                                                             |
| `line-through`            | `text-decoration: line-through;`                                            |
| `no-underline`            | `text-decoration: none;`                                                    |
| `underline`               | `text-decoration: underline;`                                               |
|                           |                                                                             |
| `capitalize`              | `text-transform: capitalize;`                                               |
| `lowercase`               | `text-transform: lowercase;`                                                |
| `normal-case`             | `text-transform: none;`                                                     |
| `uppercase`               | `text-transform: uppercase;`                                                |
|                           |                                                                             |
| `align-baseline`          | `vertical-align: baseline;`                                                 |
| `align-bottom`            | `vertical-align: bottom;`                                                   |
| `align-middle`            | `vertical-align: middle;`                                                   |
| `align-text-bottom`       | `vertical-align: text-bottom;`                                              |
| `align-text-top`          | `vertical-align: text-top;`                                                 |
| `align-top`               | `vertical-align: top;`                                                      |
|                           |                                                                             |
| `whitespace-no-wrap`      | `white-space: nowrap;`                                                      |
| `whitespace-normal`       | `white-space: normal;`                                                      |
| `whitespace-pre-line`     | `white-space: pre-line;`                                                    |
| `whitespace-pre-wrap`     | `white-space: pre-wrap;`                                                    |
| `whitespace-pre`          | `white-space: pre;`                                                         |
|                           |                                                                             |
| `break-all`               | `word-break: break-all;`                                                    |
| `break-normal`            | `overflow-wrap: normal; word-break: normal;`                                |
| `break-words`             | `overflow-wrap: break-word;`                                                |
| `truncate`                | `overflow: hidden;`<br>`text-overflow: ellipsis;`<br>`white-space: nowrap;` |

### Visibility

| Name Prefix | CSS Property           |
| ----------- | ---------------------- |
| `invisible` | `visibility: hidden;`  |
| `visible`   | `visibility: visible;` |

### Z-Index

| Name Prefix | CSS Property                                           |
| ----------- | ------------------------------------------------------ |
| `z-{n}`     | `z-index: {n};`<br>where n is 0, 10, 20, 30, 40, or 50 |
| `z-auto`    | `z-index: auto;`                                       |

## HyperUI

{% aTargetBlank "https://www.hyperui.dev", "HyperUI" %}
is a collection of free UI components defined with
Tailwind CSS classes and optionally AlpineJS directives.

There is no need to install anything.
Just find a component to use from the website,
copy its code, and paste in into your project.

Components are defined in a large number of categories including
Auth Forms, Alerts, Badges, Breadcrumbs, Button Groups, Details Lists,
Dividers, Dropdowns, Error Pages, Grids, Header, Inputs, Media, Pagination,
Progress, Radio Groups, Selects, Side Menu, Stats, Steps, Tables, Tabs,
Textareas, Toggles, and Vertical Menu.

There are also components that are specialized for eCommerce and Marketing.

Here is an example of a Toggle that only uses Tailwind classes:

```html
<label
  for="AcceptConditions"
  class="relative h-8 w-14 cursor-pointer [-webkit-tap-highlight-color:_transparent]"
>
  <input type="checkbox" id="AcceptConditions" class="peer sr-only" />

  <span
    class="absolute inset-0 rounded-full bg-gray-300 transition peer-checked:bg-green-500"
  ></span>

  <span
    class="absolute inset-y-0 start-0 m-1 h-6 w-6 rounded-full bg-white transition-all peer-checked:start-6"
  ></span>
</label>
```

This code can be placed in a component for the specific framework being used
(such as Astro or Svelte) to avoid pasting the same code in multiple places.
