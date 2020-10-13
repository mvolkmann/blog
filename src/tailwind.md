---
eleventyNavigation:
  key: Tailwind
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://tailwindcss.com/", "Tailwind" %}
is a CSS utility framework.
It provides a large number of pre-built CSS classes
that typically set a single CSS property.
These can be used as values of the `class` attribute on HTML elements.
They can also be customized in each application that uses them.

Other CSS frameworks such as Bootstrap, Bulma, Foundation, and Material-UI
define styling for components.
Tailwind takes a different approach,
providing lower level styling that can be combined
in order to create custom looks.

Tailwind is not a replacement for CSS.
It is still necessary for developers to be familiar with
CSS properties and their supported values.

## Pros

- Tailwind enables styling HTML elements without
  giving them an `id` attribute or CSS `class` name.
  This could already be done with the `style` attribute,
  but using Tailwind CSS classes is much more concise.
- Placing styles with the elements they affect makes it easier
  to visualize the result while looking at the HTML.
  Using custom CSS classes requires assigning a name and
  looking up the CSS properties, often in another source file
- Responsive UIs can be created without writing any media queries.
  This is done by prefixing Tailwind class names
  with a breakpoint name followed by a colon.
  For example: `md:flex-col` changes the `flex-direction` to `column`
  when the screen width is at or above the medium breakpoint.

## Cons

- The HTML becomes more cluttered.
- Developers that haven't been exposed to Tailwind will have to
  learn about it in order to contribute to UI development.
- When Tailwind classes are used in components of frameworks
  like React, Vue, Svelte, and Angular, parent components
  cannot override the styling.
  For this reason it may be advisable to primary use Tailwind
  for element layout and not for properties more likely
  to be overridden such as fonts and colors.

When Tailwind classes are used in component definitions,
parent components that use those components
cannot override the styling they specify.
For this reason, some developers tend to only use Tailwind classes
for layout, not styling such as fonts and colors.

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

The build process will replace these directives with generated CSS.
The generated CSS file will contain:

- the contents of {% aTargetBlank
  "https://github.com/necolas/normalize.css", "normalize.css" %}
- Tailwind custom reset styles
- all Tailwind utility class definitions in the categories referenced
  by the `@tailwind` directive in `src/styles.css`

For details on configuring various build tools to support Tailwind
{% aTargetBlank "https://tailwindcss.com/docs/installation",
"Tailwind installation" %}

TODO: How can you use Tailwind in a Svelte app?

## PostCSS Build Process

Building with PostCSS is only slightly more complicated.

1. Enter `npm install -D postcss-cli autoprefixer@9.8.6`

1. Create the file `postcss.config.js` containing:

   ```js
   module.exports = {
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

## Configuration

To configure aspects of Tailwind,
generate the file `tailwind.config.js` by entering `npx tailwindcss init`.
For example, the provided classes can be customized to change
colors, border sizes, breakpoints, font weights, shadows, and more.

This file will define a `future` property whose value is an object
with boolean properties that are commented out.
Uncomment these to get warnings about usage of features
that will break in future releases.

## Purging Unused CSS

The `purge` configuration property can be modified to purge unused CSS classes.
It should be set to an array of glob patterns
for file paths that can contain references to CSS classes.
For example, `purge: ['./public/**/*.html']`.
Tailwind will automatically check for
references to Tailwind CSS classes in CSS files.

By default, unused CSS classes are only removed when
the`NODE_ENV`environment variable is set to`production`.
To remove them regardless of this setting,
change the `purge` property in`tailwind.config.js` to the following:

```json
  purge: {
    content: ['./public/**/*.html'],
    enabled: true
  },
```

## Minimizing CSS

To minimize the generated CSS, including removing comments,
in order to make it smaller:

1. Enter `npm install -D cssnano @fullhuman/postcss-purgecss`

1. Change the content of the `postcss.config.js` file to the following::

   ```js
   const cssnano = require('cssnano');
   const purgecss = require('@fullhuman/postcss-purgecss');:

   module.exports = {
     plugins: [
       require('tailwindcss'),
       require('autoprefixer')
       cssnano({
         preset: 'default'
       }),
       purgecss({
         content: ['./layouts/**/*.html', './src/**/*.vue', './src/**/*.jsx'],
         defaultExtractor: content => content.match(/[\w-/:]+(?<!:)/g) || []
       });
     ]
   };
   ```

It may be desirable to only minimize CSS in production.
To modify `postcss.config.js` to do this, see the end of {% aTargetBlank
"https://flaviocopes.com/tailwind-setup/",
"How to setup Tailwind with PurgeCSS and PostCSS" %}.

## VS Code Support

The VS Code extension "Tailwind CSS IntelliSense"
provides autocomplete, syntax highlighting, and linting.

While entering Tailwind CSS class names,
a popup displays all matching names and their definitions.
For example, entering "upp" displays "uppercase text-transform: uppercase".

To see the definition of a Tailwind CSS class that has already been entered,
hover over its name.

A small color swatch is displayed in front of each
Tailwind class name that represents a color.

To enable Emmett completions of Tailwind CSS class names,
add the following in the VS Code `settings.json` file:

```json
  "tailwindCSS.emmetCompletions": true
```

## Breakpoints

By default Tailwind uses the following responsive breakpoints:

| Name | Width  |
| ---- | ------ |
| `sm` | 640px  |
| `md` | 768px  |
| `lg` | 1024px |
| `xl` | 1280px |

These can be overridden by modifying the `tailwind.config.js` file.

Precede Tailwind CSS class names with a breakpoint name and colon
to only apply the CSS class with the screen width matches
a given breakpoint or is larger.

## Directives

Tailwind supports several directives.

### `@apply`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#apply",
"`@apply`" %} directive is used in a CSS rule to
include properties from any number of Tailwind classes
in a custom CSS rule.
This enables a set of Tailwind classes to be reused on several elements
without repeating them.

For example:
TODO: Test this!

````css
.my-alert {
  @apply bg-red, rounded, text-white;
}

### `@layer`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#layer",
"`@layer`" %} directive is used to add custom CSS rules
to a specific bucket of Tailwind CSS rules.
TODO: Why is this useful?

### `@responsive`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#responsive",
"`@responsive`" %} directive is used ...

### `@screen`

The {% aTargetBlank
"https://tailwindcss.com/docs/functions-and-directives#screen",
"`@screen`" %} directive is used ...

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

## Functions

### `theme` function

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
````

The rule above could also be written using the the `@apply` directive
as follows:

```css
.warning {
  @apply bg-yellow-600;
}
```

The `theme` function is primarily useful when
a value will be used in a calculation using the CSS `calc` function.

## Provided CSS Classes

The following sections list all the provided Tailwind CSS classes.
There are a large number of them!

This same information is available in the official docs at
{% aTargetBlank "https://tailwindcss.com/docs/", "tailwindcss.com/docs" %}.
It is included here in a more compact manner that is
more easily searchable because they are all on a single page.

In class names that include `-color`, `color` should be replaced by one of
`gray`, `red`, `orange`, `yellow`, `green`,
`teal`, `blue`, `indigo`, `purple`, or `pink`.

### Backgrounds

| Name Prefix         | Description                                                                              |
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

| Corner Abbreviation | Corner Name  | CSS Properties Affected |
| ------------------- | ------------ | ----------------------- |
| `bl`                | bottom left  | `border-bottom-left`    |
| `br`                | bottom right | `border-bottom-right`   |
| `tl`                | top left     | `border-top-left`       |
| `tr`                | top right    | `border-top-right`      |

| Name Prefix                  | Description                                                                     |
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

### Box Alignment

| Name Prefix             | Description                       |
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

| Name Prefix   | Description                |
| ------------- | -------------------------- |
| `box-border`  | `box-sizing: border-box;`  |
| `box-content` | `box-sizing: content-box;` |

### Container

| Name Prefix | Description                                                                      |
| ----------- | -------------------------------------------------------------------------------- |
| `container` | sets max-width to breakpoint size or<br>width to 100% if no breakpoint specified |

For example, `lg:container`.

### Display

| Name Prefix          | Description                    |
| -------------------- | ------------------------------ |
| `block`              | `display: block;`              |
| `inline-block`       | `display: inline-block;`       |
| `inline`             | `display: inline;`             |
| `flex`               | `display: flex;`               |
| `inline-flex`        | `display: inline-flex;`        |
| `table`              | `display: table;`              |
| `table-caption`      | `display: table-caption;`      |
| `table-cell`         | `display: table-cell;`         |
| `table-column-group` | `display: table-column-group;` |
| `table-footer-group` | `display: table-footer-group;` |
| `table-header-group` | `display: table-header-group;` |

### Effects

| Name Prefix      | Description                                            |
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

| Name Prefix         | Description                         |
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

### Floats and Clear

| Name Prefix   | Description                                             |
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

| Name Prefix           | Description                                                                 |
| --------------------- | --------------------------------------------------------------------------- |
| `col-auto`            | `grid-column: auto`                                                         |
| `col-span-{n}`        | `grid-column: span {n} / span {n}`<br>where n is 1 to 11                    |
| `gap-{n}`             | `gap: {n}`<br>where n is 0 to 8, 10, 12, 16, or 20                          |
| `grid-cols-{n}`       | `grid-template-columns: repeat({n}, minmax(0, 1fr));`<br>where n is 1 to 12 |
| `grid-rows-{n}`       | `grid-template-rows: repeat({n}, minmax(0, 1fr));`<br>where n is 1 to 6     |
| `grid-rows-none`      | `grid-template-rows: none;`                                                 |
| `grid-flow-col`       | `grid-auto-flow: column`                                                    |
| `grid-flow-col-dense` | `grid-auto-flow: column dense`                                              |
| `grid-flow-row`       | `grid-auto-flow: row`                                                       |
| `grid-flow-row-dense` | `grid-auto-flow: row dense`                                                 |
| `row-auto`            | `grid-row: auto`                                                            |
| `row-span-{n}`        | `grid-row: span {n} / span {n}`<br>where n is 1 to 6                        |
| `row-start-{n}`       | `grid-row-start: {n}`<br>where n is 1 to 5                                  |

### Interactivity

| Name Prefix           | Description             |
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

| Name Prefix         | Description               |
| ------------------- | ------------------------- |
| `object-contain`    | `object-fit: contain;`    |
| `object-cover`      | `object-fit: cover;`      |
| `object-fill`       | `object-fit: fill;`       |
| `object-none`       | `object-fit: none;`       |
| `object-scale-down` | `object-fit: scale-down;` |

### Object Position

| Name Prefix           | Description                      |
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

| Name Prefix          | Description            |
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

| Name Prefix            | Description                       |
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

| Name Prefix    | Description                                         |
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
| `top-0`        | `top: 0;`                                           |

### Sizing

| Name Prefix    | Description                                                    |
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

| Name Prefix   | Description                                                                      |
| ------------- | -------------------------------------------------------------------------------- |
| `m-{n}`       | `margin: {n}*0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20                   |
| `mx-*`        | `margin-left: {n}*0.25rem; margin-right: {n}*0.25rem;`<br>where n = 0, 1, or 2   |
| `my-*`        | `margin-bottom: {n}*0.25rem; margin-top: {n}*0.25rem;`<br>where n = 0, 1, or 2   |
| `p-{n}`       | `padding: {n}*0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20                  |
| `px-*`        | `padding-left: {n}*0.25rem; padding-right: {n}*0.25rem;`<br>where n = 0, 1, or 2 |
| `py-*`        | `padding-bottom: {n}*0.25rem; padding-top: {n}*0.25rem;`<br>where n = 0, 1, or 2 |
| `space-x-{n}` | `margin-left: {n}*0.25rem;`<br>where n is 0 to 5                                 |
| `space-y-{n}` | `margin-top: {n}*0.25rem;`<br>where n is 0 to 5                                  |

### SVG

| Name Prefix      | Description                                |
| ---------------- | ------------------------------------------ |
| `fill-current`   | `fill: currentColor;`                      |
| `stroke-current` | `stroke: currentColor;`                    |
| `stroke-{n}`     | `stroke-width: {n};` where n is 0, 1, or 2 |

### Tables

| Name Prefix       | Description                  |
| ----------------- | ---------------------------- |
| `border-collapse` | `border-collapse: collapse;` |
| `border-separate` | `border-collapse: separate;` |
|                   |                              |
| `table-auto`      | `table-layout: auto;`        |
| `table-fixed`     | `table-layout: fixed;`       |

### Transforms

In all the classes in this section, `n` can be
0, 50, 75, 90, 95, 100, 105, 110, 125, or 150.

| Name Prefix           | Description                                                                                              |
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
| `translate-y-{above}` | `--transform-translate-y: ...;`;<br>all the same variations as for `x` above                             |
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

| Name Prefix            | Description                                                                                                            |
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

### Typography

| Name Prefix               | Description                                                              |
| ------------------------- | ------------------------------------------------------------------------ |
| `font-sans`               | `font-family: boat load of san serif fonts;`<br>includes Arial           |
| `font-serif`              | `font-family: boat load of serif fonts;`<br>includes Times New Roman     |
| `font-mono`               | `font-family: boat load of monospace fonts;`<br>includes Courier New     |
|                           |                                                                          |
| `text-xs`                 | `font-size: 0.75rem;`                                                    |
| `text-sm`                 | `font-size: 0.875rem;`                                                   |
| `text-base`               | `font-size: 1rem;`<br>Why not named `text-md`?                           |
| `text-lg`                 | `font-size: 1.125rem;`                                                   |
| `text-xl`                 | `font-size: 1.25rem;`                                                    |
| `text-2xl`                | `font-size: 1.5rem;`                                                     |
| `text-3xl`                | `font-size: 1.875rem;`                                                   |
| `text-4xl`                | `font-size: 2.25rem;`                                                    |
| `text-5xl`                | `font-size: 3rem;`                                                       |
| `text-6xl`                | `font-size: 4rem;`                                                       |
|                           |                                                                          |
| `antialiased`             | `font-smoothing` with vendor-specific prefixes and values                |
| `subpixel-antialiased`    | `font-smoothing: antialiased;` with vendor-specific prefixes and values  |
|                           |                                                                          |
| `italic`                  | `font-style: italic;`                                                    |
| `not-italic`              | `font-style: normal;`<br>weird class name!                               |
|                           |                                                                          |
| `font-hairline`           | `font-weight: 100;`                                                      |
| `font-thin`               | `font-weight: 200;`                                                      |
| `font-light`              | `font-weight: 300;`                                                      |
| `font-normal`             | `font-weight: 400;`                                                      |
| `font-medium`             | `font-weight: 500;`                                                      |
| `font-semibold`           | `font-weight: 600;`                                                      |
| `font-bold`               | `font-weight: 700;`                                                      |
| `font-extrabold`          | `font-weight: 800;`                                                      |
| `font-black`              | `font-weight: 900;`                                                      |
|                           |                                                                          |
| `normal-nums`             | `font-variant-numeric: normal;`                                          |
| `ordinal`                 | `font-variant-numeric: ordinal;`                                         |
| `slashed-zero`            | `font-variant-numeric: slashed-zero;`                                    |
| `lining-nums`             | `font-variant-numeric: lining-nums;`                                     |
| `oldstyle-nums`           | `font-variant-numeric: oldstyle-nums;`                                   |
| `proportional-nums`       | `font-variant-numeric: proportional-nums;`                               |
| `tabular-nums`            | `font-variant-numeric: tabular-nums;`                                    |
| `diagonal-fractions`      | `font-variant-numeric: diagonal-fractions;`                              |
| `stacked-fractions`       | `font-variant-numeric: stacked-fractions;`                               |
|                           |                                                                          |
| `tracking-tighter`        | `letter-spacing: -0.05em;`                                               |
| `tracking-tight`          | `letter-spacing: -0.025em;`                                              |
| `tracking-normal`         | `letter-spacing: 0em;`                                                   |
| `tracking-wide`           | `letter-spacing: 0.025em;`                                               |
| `tracking-wider`          | `letter-spacing: 0.05em;`                                                |
| `tracking-widest`         | `letter-spacing: 0.1em;`                                                 |
|                           |                                                                          |
| `leading-{n}`             | `line-height: {n}*0.25rem;`<br>where n = 3 to 10                         |
| `leading-none`            | `line-height: 1;`                                                        |
| `leading-tight`           | `line-height: 1.25rem;`                                                  |
| `leading-snug`            | `line-height: 1.375rem;`                                                 |
| `leading-normal`          | `line-height: 1.5rem;`                                                   |
|                           |                                                                          |
| `list-none`               | `list-style-type: none;`                                                 |
| `list-disc`               | `list-style-type: disc;`                                                 |
| `list-decimal`            | `list-style-type: decimal;`                                              |
|                           |                                                                          |
| `list-inside`             | `list-style-position: inside;`                                           |
| `list-outside`            | `list-style-position: outside;`                                          |
|                           |                                                                          |
| `placeholder-transparent` | `color: transparent;`                                                    |
| `placeholder-current`     | `color: currentColor;`                                                   |
| `placeholder-black`       | `color: #000;`                                                           |
| `placeholder-white`       | `color: #fff;`                                                           |
| `placeholder-{color}-{n}` | `color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100 |
|                           |                                                                          |
| `placeholder-opacity-{n}` | `--placeholder-opacity: {n}/100;`<br>where n = 0, 25, 50, 75, or 100     |
|                           |                                                                          |
| `text-center`             | `text-align: center;`                                                    |
| `text-justify`            | `text-align: justify;`                                                   |
| `text-left`               | `text-align: left;`                                                      |
| `text-right`              | `text-align: right;`                                                     |
|                           |                                                                          |
| `text-black`              | `color: #000;`                                                           |
| `text-current`            | `color: currentColor;`                                                   |
| `text-{color}-{n}`        | `color: {color-hex-code};`<br>where n is 100 to 900 in increments of 100 |
| `text-transparent`        | `color: transparent;`                                                    |
| `text-white`              | `color: #fff;`                                                           |
|                           |                                                                          |
| `text-opacity-{n}`        | `--text-opacity: {n}/100;`<br>where n is 0, 25, 50, 75, or 100           |
|                           |                                                                          |
| `line-through`            | `text-decoration: line-through;`                                         |
| `no-underline`            | `text-decoration: none;`                                                 |
| `underline`               | `text-decoration: underline;`                                            |
|                           |                                                                          |
| `capitalize`              | `text-transform: capitalize;`                                            |
| `lowercase`               | `text-transform: lowercase;`                                             |
| `normal-case`             | `text-transform: none;`                                                  |
| `uppercase`               | `text-transform: uppercase;`                                             |
|                           |                                                                          |
| `align-baseline`          | `vertical-align: baseline;`                                              |
| `align-bottom`            | `vertical-align: bottom;`                                                |
| `align-middle`            | `vertical-align: middle;`                                                |
| `align-text-bottom`       | `vertical-align: text-bottom;`                                           |
| `align-text-top`          | `vertical-align: text-top;`                                              |
| `align-top`               | `vertical-align: top;`                                                   |
|                           |                                                                          |
| `whitespace-no-wrap`      | `white-space: nowrap;`                                                   |
| `whitespace-normal`       | `white-space: normal;`                                                   |
| `whitespace-pre-line`     | `white-space: pre-line;`                                                 |
| `whitespace-pre-wrap`     | `white-space: pre-wrap;`                                                 |
| `whitespace-pre`          | `white-space: pre;`                                                      |
|                           |                                                                          |
| `break-all`               | `word-break: break-all;`                                                 |
| `break-normal`            | `overflow-wrap: normal; word-break: normal;`                             |
| `break-words`             | `overflow-wrap: break-word;`                                             |
| `truncate`                | `overflow: hidden; text-overflow: ellipsis; white-space: nowrap;`        |

| Name Prefix | Description            |
| ----------- | ---------------------- |
| `invisible` | `visibility: hidden;`  |
| `visible`   | `visibility: visible;` |

### Z-Index

| Name Prefix | Description                                            |
| ----------- | ------------------------------------------------------ |
| `z-{n}`     | `z-index: {n};`<br>where n is 0, 10, 20, 30, 40, or 50 |
| `z-auto`    | `z-index: auto;`                                       |

## Responsive Variants

Tailwind class names can be prefixes with a breakpoint name
to only apply the class when the screen/window width
matches that breakpoint or larger.
The breakpoint names are sm, md, lg, and xl.
For example: `lg:m-16`.

```

```
