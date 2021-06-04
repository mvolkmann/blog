---
eleventyNavigation:
  key: Web Development Tips
layout: topic-layout.njk
---

This is a collection of tips related to web development
divided into categories.
It assumes that you already know HTML, CSS, and JavaScript to some extent,
but perhaps have not encountered all the tips shared here.
This is very much a work in progress!

## HTML

### Semantic elements

HTML5 introduced many "semantic" elements.
These better describe the intent of certain kinds of markup
than using a more generic element such as `div`.
They also provide more context to assistive technologies
such as screen readers.
Some of the most commonly used semantic elements are
`article`, `aside`, `details`, `figcaption`, `figure`, `footer`, `header`,
`main`, `mark`, `nav`, `section`, `summary`, and `time`.

In most cases the paragraph element `<p>` should be used instead of `<div>`
when the content is only text to be rendered.

In most cases `click` events should only be associated
with `<button>` elements, not with generic elements like `<div>`.

### Values of the type attribute on input elements

HTML5 added by values for the `input` element `type` attribute.
These can change the way the element is rendered
and provide additional input validation.
The values include `checkbox`, `color`, `date`, `datetime-local`,
`email`, `file`, `image`, `month`, `number`, `password`, `radio`,
`range`, `search`, `tel`, `text`, `time`, `url`, `week`,
and a few more less commonly used values.

### Form validation

HTML provides good form validation with nice error messages.
Consider using this before reaching for a form library
that is specific to a given web framework.
Often what works well is to:

- Enclose all the form elements such as
  `button`, `input`, `textarea`, and `select` in a `form` element.
- Include only one "submit" button.
  This is the default type of a `button` element.
  To include `button` elements to do not submit the form,
  set their `type` attribute to `"button"`.
- Mark required form elements with the `require` attribute.
- Use the appropriate `input` `type` attribute values
  such as `"email"` and `"tel"`.
- Use other `input` attributes such as:

  - `min` and `max` for allowed numeric ranges
  - `minlength` and `maxlength` for allowed text lengths
  - `pattern` for regular expressions to be matched

Use CSS pseudo-classes to style form elements based on their validity.
These include `:required`, `:optional`, `:valid`, `:invalid`, `:user-invalid`,
`:blank`, `:placeholder-shown`, `:in-range`, `:out-of-range`,

### Events fired by input elements

The `input` element fires many events, two of which are `change` and `input`.
A `change` event is fired when a user changes the value
AND focus leaves the element.
An `input` event is fired after every change to the value.
For example, if a user types "abc" into an `input`,
an `input` event will be fired after each character
whereas a `change` event will only be fired once
after focus leaves the `input`.

### Value of input elements

The value displayed in an `input` element is specified by an attribute
that is determined by the value of its `type` attribute.
For most `input` types the `value` attribute is used for this purpose.
But when the `type` is `"checkbox"` or `"radio"`,
the value is specified using the `checked` attribute.

An `input` element can have an associated `datalist`.
This cause the `input` to act like an "auto-complete".
TODO: Provide an example.
TODO: Can the datalist be dynamic?

## SVG

SVG is a great markup language for drawing and rendering text and images.
The basics steps to use SVG are:

- Start with `<svg viewBox="0 0 maxX maxY" xmlns="http://www.w3.org/2000/svg">`
  where `maxX` and `maxY` are the maximum values in those dimensions.
  The minimum values do not have to be zero, but those are common values.
- Include child elements for rendering specific kinds of things.
  Commonly used elements include:

  - `circle`
  - `ellipse`
  - `image`
  - `line`
  - `path`
  - `polygon`
  - `polyline`
  - `rect`
  - `text` and `tspan`

- Wrap a `g` element around groups of children
  that need to be positioned or manipulated as a group.
- End with `</svg>`.
  basics

The size of an SVG element can be specified
using the CSS `width` and `height` properties.

The color of lines, paths, and shapes can be specified
using the CSS `stroke` and `fill` properties.

## CSS

### Size Units

CSS supports many units for expressing sizes.
Absolute units include:

- `cm` for centimeters
- `in` for inches
- `mm` for millimeters
- `Q` for quarter millimeters
- `pc` for picas
- `pt` for points
- `px` for pixels

Of these only `px` is commonly used.

Relative units include:

- `ch` for width of "0" in current font
- `em` for parent font size
- `ex` for height of "x" in current font
- `lh` for line height of current element
- `rem` for root element font size
- `vh` for 1% of viewport height
- `vmin` for 1% of smallest viewport dimension
- `vmax` for 1% of largest viewport dimension
- `vw` for 1% of viewport width

Of these the mostly commonly used are `rem`, `vh`, and `vw`.

An important benefit of using `rem` as the unit for nearly all sizes
is that it enables scaling everything in the web site
by simply changing the font size of the `html` element.
A web app can allow each user to modify this size,
perhaps saving their preference in `localStorage`.

### Flex layout

Flex layout is a powerful way to control
the layout of HTML elements in one dimension.
{% aTargetBlank "https://flexboxfroggy.com", "Flexbox Froggy" %}
provides a great way to learn about this.

There are many CSS properties that affect flex layout.
The most commonly used properties are described here.

- `display`

  Set this to `flex`.

- `flex-direction`

  This defaults to `row` for horizontal layout,
  but can be set to `column` for vertical layout.
  It defines the "major axis" and the opposite direction
  is considered to be the "minor axis".

- `justify-content`

  This controls layout on the major axis.
  The most commonly used values are
  `flex-start` (default, pushing children toward start),
  `flex-end (pushing children toward end)`, `center`,
  and `space-between` (spreading children).

- `align-items`

  This controls layout on the minor axis.
  The most commonly used values are `stretch` (default),
  `start` (pushing children toward start),
  `end` (pushing children toward end), and `center`.

- `flex-grow`

  This is applied to children, not the container.
  It affects the amount of space allocated to the element.
  The default value is zero.
  The `flex-grow` values of all the children are added to obtain a total.
  Then the `flex-grow` value of each child is
  divided by the total to get a percentage.
  That percentage of the unused space in the parent container
  is added to the size of the child element.

- `gap`

  This controls the space between elements when
  `justify-content` is not one of the values that begins with `space-`.

Child elements can override their minor axis alignment
described by the `align-items` property on the container
by setting their `align-self` property.

### Grid layout

Grid layout is a powerful way to control
the layout of HTML elements in two dimensions.
{% aTargetBlank "https://cssgridgarden.com", "Grid Garden" %}
provides a great way to learn about this.

There are many CSS properties that affect grid layout.
The most commonly used properties applied to a container element
are described here.

- `display`

  Set this to `grid`.

- `grid-template-columns`

  This specifies the number of columns and their widths.
  Widths can be specified using any CSS unit.
  They can also use `fr` fractional units which work similarly
  to the `flex-grow` CSS property described earlier.
  If consecutive columns have the same width,
  they can be described with the `repeat` function.
  For example, `grid-template-columns: 3rem repeat(3, 1fr) 4rem;`
  describes five columns where the middle three
  all have a width that is one third of the remaining space after
  `3rem` is given to the first child and `4rem` is given to the last child.

- `grid-template-rows`

  This specifies the number of rows and their heights.
  It accepts the same values as `grid-template-columns`.

- `grid-template: {columns} / {rows}`

  This combines the data from `grid-template-columns` and `grid-template-rows`
  into a single property.
  The result is a bit harder to read than using separate properties.

- `grid-template-areas`

  This gives names to collections of grid cells
  allowing them to span multiple columns and rows.
  Child elements then assign themselves to the grid names
  using the `grid-area` CSS property.
  For example, the following describes the layout of a container
  that has a header across the top, a footer across the bottom,
  and a left nav followed by a main area in the center.
  Note the lack of commas between the string values.

  ```css
  grid-template-areas:
    'header header'
    'left-nav main'
    'footer footer';
  ```

- `gap`

  This controls the space between grid areas.

The most commonly used properties applied to child elements
are described here.

- `grid-column-start`: This specifies the column number in which the child begins.
- `grid-column-end`: This specifies the column number in which the child ends.
- `grid-row-start`: This specifies the row number in which the child begins.
- `grid-row-end`: This specifies the row number in which the child ends.
- `grid-column: {start} / {end}` specifies the start and end columns.
- `grid-column: {start} / span {columns}` specifies the start and # of columns.
- `grid-row: {start} / {end}` specifies the start and end rows.
- `grid-row: {start} / span {rows}` specifies the start and # of rows.
- `grid-area: {grid-template-area-name}` specifies a grid template area name
  where the component should be rendered.

Negative values for `end` count from the last column or row.

The default alignment of elements within their grids is specified setting
the `justify-content` and `align-items` properties on the container.
Child elements can override this by setting their
`justify-self` and `align-self` properties.

### Selectors

### Pseudo classes

### pseudo selectors

### Combinators

### Variables

CSS variables (a.k.a custom properties) are useful for
storing and referring to values that are used in multiple places.
Changing the value of a variable updates all the places where it is used.
Common uses include storing the values of colors and sizes.

Here are examples of the syntax for defining and referring to a CSS variable:

````css
:root { /* This matches the root element "html". */
  /* All elements in the document can use variables defined here. */
  --primary-color: cornflowerblue;
}

p {
  color: var(--primary-color);
}

.circle {
  --size: 4rem;
  border: 1px solid calc(var(--size) / 2);
  height: var(--size);
  width: var(--size);
}
```

### CSS position

Elements whose CSS `position` property set to `absolute`
are positioned relative to the nearest ancestor element
that has its CSS `position` property set to `relative`.
If none is found then it is relative to the browser window.
Positioning is specified using the CSS properties
`top`, `bottom`, `left` and `right`.

### window.matchMedia

The CSS Object Model (CSSOM) `window.matchMedia` function
can be used to support light and dark modes.
For example, the following line of JavaScript code
determines if the user has configured their operating system
to prefer dark mode:

```js
const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
````

This can then be used to add a CSS class name such as "dark-mode"
to the body element.

CSS for the `body` element can define CSS variables
that specify colors to be used in light mode.
A separate rule with the selector `body.dark-mode` can override the
values of those CSS variables with the values to be used in dark mode.

### box-sizing

box-sizing: border-box

100% versus 100 VH or 100 VW

position fixed and position sticky

using normalize reset

CSS box model basics

CSS specificity

ways to center things including using absolute positioning and translate -50%

## JavaScript

Review of array methods including reduce

### CSS variables from JavaScript

JavaScript code can access the values of CSS variables.
For example, suppose we have found a DOM element
whose CSS defines the variable `--size`.
We can get the value with the following:

```js
const size = getComputedStyle(domElement).getPropertyValue('--size');
```

JavaScript code can modify the values of CSS variables.
This updates the use of all CSS properties that reference them.
For example, we can change the value of the `--size` variable
with the following:

```js
domElement.style.setProperty('--size', '5rem');
```

## Node

### npm scripts

"npm scripts" are custom command strings
that are defined in `package.json` files.
Nearly all JavaScript-based web development projects use these.
To add one, editing a `package.json` file,
find or add a section that begins with `"scripts": {`,
and add a line with the syntax `"some-name": "some-command-string"`.
For example, the following npm scripts run the Prettier and ESLint tools
on all the applicable files in a Svelte project.

```json
    "format": "prettier --write '{public,src}/**/*.{css,html,js,svelte,ts}'",
    "lint": "eslint --fix --quiet src --ext .js,.svelte,.ts",
```

Web development tooling is often executed using npm scripts
that are defined in a `package.json` file using the `npm run` command.
This is such a frequent activity that it is
useful to define the alias `nr` to do this.
The details differ based on the shell being used.

- bash: `alias nr="npm run"
- zsh: same as bash
- fish: `abbr --add nr npm run`
- nushell: `alias nr = npm run`

## Browsers

Create bookmarks in the bookmark bar to `http://localhost:{port-number}`
for ports that are commonly used by web frameworks.
Common ports include 3000, 5000, and 8080.
These make it easy to test web apps that being developed and run locally.
I learned this from Bill Odom.

## Editors

### Emmet

Emmet is an editor plugin for quickly entering HTML, XML, and CSS.
It also supports many "actions" that operate on HTML and XML elements.
The most commonly used action is to expand an abbreviation or snippet.
Emmet is built into the VS Code editor and
can be installed in most other editors and IDEs.

For example, to create a `div` element with CSS class of `pizza`,
enter `.pizza` and press the tab key.
This expands to `<div class="pizza"></div>` with the cursor
before the end tag ready for you to enter content.

Here are examples of generating HTML:

| Syntax                             | Expands to                           |
| ---------------------------------- | ------------------------------------ |
| `.foo`                             | `<div class="foo"></div>`            |
| `.foo.bar`                         | `<div class="foo bar"></div>`        |
| `#foo`                             | `<div id="foo"></div>`               |
| `p.foo`                            | `<p class="foo"></p>`                |
| `p.foo.bar`                        | `<p class="foo bar"></p>`            |
| `p#foo`                            | `<p id="foo"></p>`                   |
| `ul>li`                            | `<ul><li></li></ul>`                 |
| `p{Hello, World!}`                 | `<p>Hello, World!</p>`               |
| `a:link`                           | `<a href="http://"></a>`             |
| `c`                                | `<!-- -->`                           |
| `img`                              | `<img src="" alt="">`                |
| `img[alt="dog" src="whippet.png"]` | `<img src="whippet.png" alt="dog">`  |
| `input:email`                      | `<input type="email" name="" id="">` |
| `link`                             | `<link rel="stylesheet" href="">`    |

Here are examples of generating CSS.
While it seems like there is a lot to memorize,
if you guess an abbreviation you will usually be correct.

| Syntax | Expands to                        |
| ------ | --------------------------------- |
| `aic`  | `align-items: center;`            |
| `aie`  | `align-items: end;`               |
| `asc`  | `align-self: center;`             |
| `ase`  | `align-self: end;`                |
| `b`    | `bottom: ;`                       |
| `bd`   | `border: ;`                       |
| `bgc`  | `background-color: #fff;`         |
| `c`    | `color: #000;`                    |
| `curp` | `cursor: pointer;`                |
| `df`   | `display: flex;`                  |
| `dg`   | `display: grid;`                  |
| `dib`  | `display: inline-block;`          |
| `dn`   | `display: none;`                  |
| `fsi`  | `font-style: italic;`             |
| `fwb`  | `font-weight: bold;`              |
| `fxdc` | `flex-direction: column;`         |
| `fz`   | `font-size ;`                     |
| `h`    | `height: ;`                       |
| `jcc`  | `justify-content: center;`        |
| `jcsp` | `justify-content: space-between;` |
| `l`    | `left: ;`                         |
| `m`    | `margin: ;`                       |
| `o`    | `outline: ;`                      |
| `p`    | `padding: ;`                      |
| `r`    | `right: ;`                        |
| `t`    | `top: ;`                          |
| `tac`  | `text-align: center;`             |
| `tar`  | `text-align: right;`              |
| `ttc`  | `text-transform: capitalize;`     |
| `ttu`  | `text-transform: uppercase;`      |
| `w`    | `width: ;`                        |
| `z`    | `z-index: ;`                      |

For more detail, see my {% aTargetBlank
"https://objectcomputing.com/resources/publications/sett/march-2018-emmet-editor-plugin?v=1.0.15",
"Emmet article" %}.

## Miscellaneous

Frequently when starting a server locally to test a web app
an error message is output stating that
the port the server tried to use is being used by another process.
One option is to start the server so it listens on a different port.
Another option is to kill the process currently listening on the port.
But how do you find and kill that process?

Here is a bash script that does this.
You can create an alias named "klp" to this script.
Then you can enter "klp 5000" to
kill the process currently listening on port 5000.

```bash
#!/usr/bin/env bash
# Kills the process listening on a given port.

if [[ $# -ne 1 ]]; then
  echo usage: kill-listening-process {port}
  exit 1
fi

port=$1
pid=$(lsof -n -iTCP:$port -sTCP:LISTEN -t)

if [[ $pid ]]; then
  kill $pid
  echo killed process $pid
else
  echo no process is listening on port $port
fi
```

TODO: Add Windows version from Adam Mitz.
