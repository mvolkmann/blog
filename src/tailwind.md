---
eleventyNavigation:
  key: Tailwind
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://tailwindcss.com/", "Tailwind" %}
is a CSS utility framework.
It provides pre-built CSS classes that typically set a single CSS property.
These can be listed as the values of the `class` attribute on HTML elements.

Other CSS frameworks such as Bootstrap, Bulma, Foundation, and Material-UI define
styling for components rather than the low level approach used by Tailwind.

Tailwind is not a replacement for CSS.
It is still necessary for developers to be familiar with
CSS properties and their supported values.

## Pros

## Cons

When Tailwind classes are used in component definitions,
parent components that use those components
cannot override the styling they specify.
For this reason, some developers tend to only use Tailwind classes
for layout, not styling such as fonts and colors.

## Alphabetical List

| Name Prefix          | Description                                                                      | Values | Example        |
| -------------------- | -------------------------------------------------------------------------------- | ------ | -------------- |
| `box-border`         | `box-sizing: border-box;`                                                        | none   |                |
| `box-content`        | `box-sizing: content-box;`                                                       | none   |                |
| `container`          | sets max-width to breakpoint size or<br>width to 100% if no breakpoint specified | none   | `lg:container` |
| `mx-*`               | margin left and right                                                            |        |
| `my-*`               | margin top and bottom                                                            |        |
| `px-*`               | padding left and right                                                           |        |
| `py-*`               | padding top and bottom                                                           |        |
| `m-*`                | margin on all sides                                                              |        |
| `p-*`                | padding on all sides                                                             |        |
| `text-*`             | text color (sets CSS `color` property)                                           |        |
| `block`              | `display: block;`                                                                | none   |
| `inline-block`       | `display: inline-block;`                                                         | none   |
| `inline`             | `display: inline;`                                                               | none   |
| `flex`               | `display: flex;`                                                                 | none   |
| `inline-flex`        | `display: inline-flex;`                                                          | none   |
| `table`              | `display: table;`                                                                | none   |
| `table-caption`      | `display: table-caption;`                                                        | none   |
| `table-cell`         | `display: table-cell;`                                                           | none   |
| `table-column-group` | `display: table-column-group;`                                                   | none   |
| `table-footer-group` | `display: table-footer-group;`                                                   | none   |
| `table-header-group` | `display: table-header-group;`                                                   | none   |
| `name`               | description                                                                      | none   |                |

## Categorized Lists

The following sections list all the provided Tailwind CSS classes.
This same information is available in the official docs at
{% aTargetBlank "https://tailwindcss.com/docs/", "tailwindcss.com/docs" %}.
It is included here in a more compact manner that is
more easily searchable because they are all on a single page.

### Accessibility

### Backgrounds

### Borders

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

| Name Prefix | Description                                                                      | Values | Example        |
| ----------- | -------------------------------------------------------------------------------- | ------ | -------------- |
| `container` | sets max-width to breakpoint size or<br>width to 100% if no breakpoint specified | none   | `lg:container` |

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

### Flexbox

| Name Prefix         | Description                       |
| ------------------- | --------------------------------- |
| `flex-1`            | `flex: 1 1 0%;`                   |
| `flex-auto`         | `flex: 1 1 auto;`                 |
| `flex-initial`      | `flex: 0 1 auto;`                 |
| `flex-none`         | `flex: none;`                     |
| `flex-col`          | `flex-direction: column;`         |
| `flex-col-reverse`  | `flex-direction: column-reverse;` |
| `flex-row`          | `flex-direction: row;`            |
| `flex-row-reverse`  | `flex-direction: row-reverse;`    |
| `flex-wrap`         | `flex-wrap: wrap;`                |
| `flex-wrap-reverse` | `flex-wrap: wrap-reverse;`        |
| `flex-no-wrap`      | `flex-wrap: nowrap;`              |
| `flex-grow`         | `flex-grow: 1;`                   |
| `flex-grow-0`       | `flex-grow: 0;`                   |
| `flex-shrink`       | `flex-shrink: 1;`                 |
| `flex-shrink-0`     | `flex-shrink: 0;`                 |
| `order-n`           | `order: n;`<br>where n is 1 to 12 |

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

| Name Prefix           | Description                                                               |
| --------------------- | ------------------------------------------------------------------------- |
| `col-auto`            | `grid-column: auto`                                                       |
| `col-span-n`          | `grid-column: span n / span n`<br>where n is 1 to 11                      |
| `gap-n`               | `gap: n`<br>where n is 0 to 8, 10, 12, 16, or 20                          |
| `grid-cols-n`         | `grid-template-columns: repeat(n, minmax(0, 1fr));`<br>where n is 1 to 12 |
| `grid-rows-n`         | `grid-template-rows: repeat(n, minmax(0, 1fr));`<br>where n is 1 to 6     |
| `grid-rows-none`      | `grid-template-rows: none;`                                               |
| `grid-flow-col`       | `grid-auto-flow: column`                                                  |
| `grid-flow-col-dense` | `grid-auto-flow: column dense`                                            |
| `grid-flow-row`       | `grid-auto-flow: row`                                                     |
| `grid-flow-row-dense` | `grid-auto-flow: row dense`                                               |
| `row-auto`            | `grid-row: auto`                                                          |
| `row-span-n`          | `grid-row: span n / span n`<br>where n is 1 to 6                          |
| `row-start-n`         | `grid-row-start: n`<br>where n is 1 to 5                                  |

### Interactivity

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
| `h-n`          | `height: n * 0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20 |
| `w-n`          | `width: n * 0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20  |
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

| Name Prefix | Description                                                     |
| ----------- | --------------------------------------------------------------- |
| `m-n`       | `margin: n * 0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20  |
| `p-n`       | `padding: n * 0.25rem;`<br>where n is 0 to 8, 10, 12, 16, or 20 |
| `space-x-n` | `margin-left: n * 0.25rem;`<br>where n is 0 to 5                |
| `space-y-n` | `margin-top: n * 0.25rem;`<br>where n is 0 to 5                 |

### SVG

### Tables

### Transforms

### Transitions and Animation

### Typography

| Name Prefix               | Description                                                             |
| ------------------------- | ----------------------------------------------------------------------- |
| `font-sans`               | `font-family: boat load of san serif fonts;`<br>includes Arial          |
| `font-serif`              | `font-family: boat load of serif fonts;`<br>includes Times New Roman    |
| `font-mono`               | `font-family: boat load of monospace fonts;`<br>includes Courier New    |
|                           |                                                                         |
| `text-xs`                 | `font-size: 0.75rem;`                                                   |
| `text-sm`                 | `font-size: 0.875rem;`                                                  |
| `text-base`               | `font-size: 1rem;`<br>Why not named `text-md`?                          |
| `text-lg`                 | `font-size: 1.125rem;`                                                  |
| `text-xl`                 | `font-size: 1.25rem;`                                                   |
| `text-2xl`                | `font-size: 1.5rem;`                                                    |
| `text-3xl`                | `font-size: 1.875rem;`                                                  |
| `text-4xl`                | `font-size: 2.25rem;`                                                   |
| `text-5xl`                | `font-size: 3rem;`                                                      |
| `text-6xl`                | `font-size: 4rem;`                                                      |
|                           |                                                                         |
| `antialiased`             | `font-smoothing` with vendor-specific prefixes and values               |
| `subpixel-antialiased`    | `font-smoothing: antialiased;` with vendor-specific prefixes and values |
|                           |                                                                         |
| `italic`                  | `font-style: italic;`                                                   |
| `not-italic`              | `font-style: normal;`<br>weird class name!                              |
|                           |                                                                         |
| `font-hairline`           | `font-weight: 100;`                                                     |
| `font-thin`               | `font-weight: 200;`                                                     |
| `font-light`              | `font-weight: 300;`                                                     |
| `font-normal`             | `font-weight: 400;`                                                     |
| `font-medium`             | `font-weight: 500;`                                                     |
| `font-semibold`           | `font-weight: 600;`                                                     |
| `font-bold`               | `font-weight: 700;`                                                     |
| `font-extrabold`          | `font-weight: 800;`                                                     |
| `font-black`              | `font-weight: 900;`                                                     |
|                           |                                                                         |
| `normal-nums`             | `font-variant-numeric: normal;`                                         |
| `ordinal`                 | `font-variant-numeric: ordinal;`                                        |
| `slashed-zero`            | `font-variant-numeric: slashed-zero;`                                   |
| `lining-nums`             | `font-variant-numeric: lining-nums;`                                    |
| `oldstyle-nums`           | `font-variant-numeric: oldstyle-nums;`                                  |
| `proportional-nums`       | `font-variant-numeric: proportional-nums;`                              |
| `tabular-nums`            | `font-variant-numeric: tabular-nums;`                                   |
| `diagonal-fractions`      | `font-variant-numeric: diagonal-fractions;`                             |
| `stacked-fractions`       | `font-variant-numeric: stacked-fractions;`                              |
|                           |                                                                         |
| `tracking-tighter`        | `letter-spacing: -0.05em;`                                              |
| `tracking-tight`          | `letter-spacing: -0.025em;`                                             |
| `tracking-normal`         | `letter-spacing: 0em;`                                                  |
| `tracking-wide`           | `letter-spacing: 0.025em;`                                              |
| `tracking-wider`          | `letter-spacing: 0.05em;`                                               |
| `tracking-widest`         | `letter-spacing: 0.1em;`                                                |
|                           |                                                                         |
| `leading-3`               | `line-height: 0.75rem;`                                                 |
| `leading-4`               | `line-height: 1rem;`                                                    |
| `leading-5`               | `line-height: 1.25rem;`                                                 |
| `leading-6`               | `line-height: 1.5rem;`                                                  |
| `leading-7`               | `line-height: 1.75rem;`                                                 |
| `leading-8`               | `line-height: 2rem;`                                                    |
| `leading-9`               | `line-height: 2.25rem;`                                                 |
| `leading-10`              | `line-height: 2.5rem;`                                                  |
| `leading-none`            | `line-height: 1;`                                                       |
| `leading-tight`           | `line-height: 1.25rem;`                                                 |
| `leading-snug`            | `line-height: 1.375rem;`                                                |
| `leading-normal`          | `line-height: 1.5rem;`                                                  |
|                           |                                                                         |
| `list-none`               | `list-style-type: none;`                                                |
| `list-disc`               | `list-style-type: disc;`                                                |
| `list-decimal`            | `list-style-type: decimal;`                                             |
|                           |                                                                         |
| `list-inside`             | `list-style-position: inside;`                                          |
| `list-outside`            | `list-style-position: outside;`                                         |
|                           |                                                                         |
| `placeholder-transparent` | `color: transparent;`                                                   |
| `placeholder-current`     | `color: currentColor;`                                                  |
| `placeholder-black`       | `color: #000;`                                                          |
| `placeholder-white`       | `color: #fff;`                                                          |
| `placeholder-gray-n`      | `color: shade-of-gray;`<br>where ne is 100 to 800 in increments of 100  |
|                           |                                                                         |
| `placeholder-opacity-n`   | `--placeholder-opacity: n*0.25;`<br>where n = 0, 25, 50, 75, or 100     |

| Name Prefix | Description            |
| ----------- | ---------------------- |
| `invisible` | `visibility: hidden;`  |
| `visible`   | `visibility: visible;` |

### Z-Index

| Name Prefix | Description                                          |
| ----------- | ---------------------------------------------------- |
| `z-n`       | `z-index: n;`<br>where n is 0, 10, 20, 30, 40, or 50 |
| `z-auto`    | `z-index: auto;`                                     |

## Responsive Variants

Tailwind class names can be prefixes with a breakpoint name
to only apply the class when the screen/window width
matches that breakpoint or larger.
The breakpoint names are sm, md, lg, and xl.
For example: `lg:m-16`.
