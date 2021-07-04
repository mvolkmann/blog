---
eleventyNavigation:
  key: Web Development Tips
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

Whether you are fairly new to web development or
you've been practicing for a while,
it's always exciting to come across a shortcut or a tip
for simplifying your code that you hadn't been aware of before.
In this article, you'll find a variety of suggestions that will
help you improve your craft and your productivity.

This collection of web development tips assumes that you
already know HTML, CSS, and JavaScript to some extent,
but perhaps have forgot or never seen some of the tips shared here.

All of the examples here use vanilla JavaScript
to avoid appealing only to users of specific web frameworks.
To make the experience more interactive, most of the examples
run live in this web page.
You can also copy the code into a `.html` file and open it in a web browser.

For information about fundamental web topics like
HTML, CSS, and JavaScript that are not covered here,
or to get more detail on topics that are covered here,
your first stop should be the {% aTargetBlank
"https://developer.mozilla.org/en-US/",
"Mozilla Developer Network (MDN) Web Docs" %}.
Start a web search with "MDN"; for example, "MDN CSS transform".

Another great resource is
"{% aTargetBlank "https://caniuse.com", "Can I use" %}"
which provides details on browser support for
all fundamental web development features.

Many of the tip descriptions contain links to MDN pages and
other resources to make it easy to access more detailed information.

## HTML

### Semantic elements

HTML5 introduced many "semantic" elements.
These more specifically describe the intent of certain kinds of markup
than a more generic element, such as `div`.
They also provide more context to assistive technologies,
such as screen readers.

Some of the most commonly used semantic elements include:

- `article`
- `aside`
- `details`
- `figcaption`
- `figure`
- `footer`
- `header`
- `main`
- `mark`
- `nav`
- `section`
- `summary`
- `time`

For descriptions of each of these elements, see the {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/HTML/Element",
"MDN HTML elements reference" %}.

Two key tips to keep in mind:

- In most cases, the paragraph element `<p>` should be used
  instead of `<div>` when the content is only text to be rendered.
- In most cases, `click` events should be associated only with
  `<button>` elements, not with generic elements like `<div>`.

### `input` element `type` attribute

HTML5 added many values for the `input` element `type` attribute.
These can change the way the element is rendered
and provide additional input validation.
The values include:

- `checkbox`
- `color`
- `date`
- `datetime-local`
- `email`
- `file`
- `image`
- `month`
- `number`
- `password`
- `radio`
- `range`
- `search`
- `tel`
- `text`
- `time`
- `url`
- `week`
- ... and a few more less commonly used values

For details on these attribute values, see the {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input",
"MDN input element" %} page.

### Form validation

HTML provides good form validation with nice error messages.
Consider using this before reaching for a form library
that is specific to a given web framework.
Often what works well is to:

- Enclose all form elements, such as
  `button`, `input`, `select`, and `textarea`, in a `form` element.
- Include only one "submit" button.
  This is the default `type` of a `button` element.
  To include `button` elements that do not submit the form,
  set the `type` attribute to `button`.
- Mark required form elements with the `require` attribute.
- Use appropriate `input` `type` attribute values,
  such as `email` and `tel`.
- Use other `input` attributes such as:

  - `min` and `max` for allowed numeric ranges
  - `minlength` and `maxlength` for allowed text lengths
  - `pattern` for regular expressions to be matched

Use CSS pseudo-classes to style form elements based on their validity.
These include:

- `:required`
- `:optional`
- `:valid`
- `:invalid`
- `:user-invalid`
- `:blank`
- `:placeholder-shown`
- `:in-range`
- `:out-of-range`

### Events fired by input elements

The `input` element fires many events, two of which are `change` and `input`.

- A `change` event is fired when a user changes the value
  AND focus leaves the element.
- An `input` event is fired after every change to the value.

For example, if a user types "abc" into an `input`,
an `input` event will be fired after each character,
whereas a `change` event will only be fired once
after focus leaves the `input`.

### Value of `input` elements

The value displayed in an `input` element is specified by an attribute
that is determined by the value of its `type` attribute.
For most `input` types, the `value` attribute is used for this purpose.
But when the `type` is `checkbox` or `radio`,
the value is specified using the `checked` attribute.

### `input` and `datalist`

An `input` element can have an associated `datalist`.
This causes the `input` to act like an "auto-complete".
New values can be added to the `datalist`,
and existing values can be deleted.
This will change the options displayed in the `input`.

Here is an example where the user can select a color.
Initially the only values in the `datalist` are blue, green, and red.
But the user can add more colors.
Try adding more colors and then type into the input
to see the list filtered to only colors that match.

{% include "_datalist.html" %}

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>input/datalist Demo</title>
    <style>
      #datalist-demo form {
        margin-bottom: 1rem;
      }
    </style>
    <script>
      const INITIAL_COLORS = ['blue', 'green', 'red'];

      window.onload = () => {
        const form = document.querySelector('#datalist-demo form');
        const addBtn = form.querySelector('button');
        const newColorInput = form.querySelector('input');
        const colorInput = document.getElementById('color');
        const colorsDataList = document.getElementById('colors');
        const report = document.getElementById('report');

        function addColor(color) {
          // Create a new option element to add to the datalist.
          const option = document.createElement('option');
          option.setAttribute('value', color);

          // Insert the new option in the datalist
          // so they remain in alphabetical order.
          for (const child of colorsDataList.children) {
            const thisColor = child.getAttribute('value');
            if (color < thisColor) {
              colorsDataList.insertBefore(option, child);
              return;
            }
            if (color === thisColor) return; // avoids duplicates
          }
          colorsDataList.appendChild(option);
        }

        INITIAL_COLORS.forEach(addColor);

        // When the form is submitted, add a color to the datalist.
        form.addEventListener('submit', event => {
          event.preventDefault();
          addColor(newColorInput.value);
          newColorInput.value = '';
        });

        // Enable the "Add Color" button only
        // if a new color has been entered.
        newColorInput.addEventListener('input', event => {
          if (event.target.value) {
            addBtn.removeAttribute('disabled');
          } else {
            addBtn.setAttribute('disabled', 'disabled');
          }
        });

        // Report color selections.
        colorInput.addEventListener('change', event => {
          report.textContent = 'You selected ' + event.target.value + '.';
        });
      };
    </script>
  </head>
  <body>
    <section id="datalist-demo">
      <form>
        <label for="new-color">New Color:</label>
        <input id="new-color" />
        <button disabled>Add Color</button>
      </form>

      <label for="color">Color:</label>
      <input id="color" list="colors" bind:value="{color}" />
      <datalist id="colors"></datalist>

      <p id="report">No color has been selected.</p>
    </section>
  </body>
</html>
```

{% endraw %}

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

Of these, only `px` is commonly used.

Relative units include:

- `ch` for the width of "0" in current font
- `em` for the parent font size
- `ex` for the height of "x" in current font
- `lh` for the line height of current element
- `rem` for the root element (`html`) font size
- `vh` for 1% of the viewport height
- `vmin` for 1% of the smallest viewport dimension
- `vmax` for 1% of the largest viewport dimension
- `vw` for 1% of the viewport width

Of these, the mostly commonly used are `rem`, `vh`, and `vw`.

An important benefit of using `rem` as the unit for nearly all sizes
is that it enables scaling everything in the web site
by simply changing the font size of the `html` element,
which defaults to `16px`.
A web app can allow each user to modify this size,
perhaps saving their preference in `localStorage`.

### Viewport units

There are four CSS viewport units.

| Unit   | Description               |
| ------ | ------------------------- |
| `vh`   | 1% of the viewport height |
| `vw`   | 1% of the viewport width  |
| `vmax` | larger of `vh` and `vw`   |
| `vmin` | smaller of `vh` and `vw`  |

The size `100vw` is the full viewport width, including the `body` margin.

The size `100%` is the full width of the parent element.
For children of the `body` element, this does not include the `margin`.
If the `body` `margin` is zero, `100vw` and `100%` are equivalent.
If the `body` `margin` is not zero,
setting the width of a top-level element to `100vw`
will cause it to overlap the right edge of the viewport,
but setting the width to `100%` will not.
For this reason `100%` is often preferred over `100vw`.

The example below demonstrates the difference between `100vw` and `100%`.
Notice the right borders and the use of `box-sizing: border-box;`.

{% include "_viewport-vs-percent.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Viewport Units Demo</title>
    <style>
      body {
        margin: 1rem;
      }

      div {
        box-sizing: border-box;
        display: flex;
        font-size: 1rem;
        outline: 1px solid black;
        padding: 1rem;
      }

      div > div {
        flex-grow: 1;
      }

      .percent100 {
        width: 100%;
      }

      .vw100 {
        width: 100vw;
      }
    </style>
  </head>
  <body>
    <div class="vw100">100vw</div>

    <div class="percent100">100%</div>

    <div class="percent100">
      <div>
        <div class="percent100">left</div>
      </div>
      <div>
        <div class="percent100">right</div>
      </div>
    </div>
  </body>
</html>
```

### Resets

A CSS reset is a set of CSS rules that attempt to
set the CSS properties of standard HTML elements
so they render the same across all popular web browsers.
There are many such resets available.
The most popular is {% aTargetBlank
"https://necolas.github.io/normalize.css/", "normalize.css" %}.

To use normalize.css:

1. Download it from the link above.
2. Add the following in the `head` element of HTML files:

```html
<link rel="stylesheet" href="normalize.css" />
```

### CSS variables

CSS variables (a.k.a. custom properties) are useful for
storing and referring to values that are used in multiple places.
Changing the value of a variable updates
all the property values where it is used.
Common uses include storing the values of colors and sizes.

To define a CSS variable, add a line in any CSS rule
with the syntax:

```css
--some-var-name: some-value;
```

The variable is available for use in the rule where it is define
and in any rules that apply to descendant elements
of elements matched by this rule.

To refer to a CSS variable in the value of a CSS property,
use the following syntax:

```css
var(--some-var-name)
```

To add a default value to be used if the variable is not defined,
use the following syntax:

```css
var(--some-var-name, default-value)
```

The following example defines and uses a couple of CSS variables.

```css
:root {
  /* This matches the root element "html". */
  /* All other CSS rules can use variables defined here. */
  --primary-color: cornflowerblue;
}

p {
  color: var(--primary-color);
}

.circle {
  /* This variable can only be used in this rule
     and in rules that match descendant elements */
  --size: 4rem;

  border: 1px solid calc(var(--size) / 2);
  height: var(--size);
  width: var(--size);
}
```

Later we will see how to get and set
the values of CSS variables in JavaScript code.

### Box model

The CSS box model defines how padding, border, and margin
are added to elements.

- Padding is outside the content and inside the border can be set to `0`.
- The border is optional.
- Margin is outside the border and can also be set to `0`.

<img alt="CSS box model" style="width: 50%"
  src="/blog/assets/css-box-model.png?v={{pkg.version}}"
  title="CSS box model">

Padding, border, and margin can be specified to be the same on all four sides
or to be different on each side.

### `box-sizing` property

When an element has a specified `width` and `height`,
by default those apply to the content and
do not include the `padding`, `border`, and `margin`.
This is because the CSS `box-sizing` property defaults to `content-box`.

If `box-sizing` is set to `border-box`, the `width` and `height`
include the content, `padding`, and `border`, but not the `margin`.
There are no other supported values for the `box-sizing` property.

### `box-shadow` property

The `box-shadow` property adds shadows to
one or more sides of an element.
It does not add to the size of the element,
so it does not affect element layout.

The most common examples of using shadows are to:

- Make the element appear raised above the page
  by adding outset shadows on the right and bottom sides.
- Make the element appear sunken into the page
  by adding inset shadows on the top and left sides.
- Frame the element by adding outset shadows on all four sides.

The `box-shadow` property takes 3 to 5 values.

| # of Values | Meaning                                           |
| ----------- | ------------------------------------------------- |
| 3           | offset-x offset-y color                           |
| 4           | offset-x offset-y blur-radius color               |
| 5           | offset-x offset-y blur-radius spread-radius color |

Additionally, the `inset` keyword can precede the color
to cause the shadow to be drawn on the inside of the element
rather than on the outside (referred to here as "outset")
and on the opposite side of the element.

Setting offset-x to a non-zero value
adds shadow on the left or right side,
depending on whether the `inset` keyword is present.
Likewise, setting offset-y to a non-zero value
adds shadow on the top or bottom side,
depending on whether the `inset` keyword is present.

The blur-radius value specifies the width over which
the shadow changes from 100% opacity to zero opacity.
The default value is `0`, which results in a solid colored shadow.

Setting a positive spread-radius value
increases the shadow width on all four sides.
This is typically used only when offset-x and offset-y are both zero
in order to add a shadow on all four sides of an element.
The default value is `0`.

Here are the value patterns to add a shadow
on a single side of an element.
Note that a small amount of shadow is also added to the adjacent sides
in order to achieve a 3D effect, and this cannot be prevented.

| Side          | offset-x offset y inset? |
| ------------- | ------------------------ |
| top outset    | 0 negative               |
| top inset     | 0 positive inset         |
| bottom outset | 0 positive               |
| bottom inset  | 0 negative inset         |
| right outset  | positive 0               |
| right inset   | negative 0 inset         |
| left outset   | negative 0               |
| left inset    | positive 0 inset         |

Multiple shadows can be added to an element
by specifying comma-separated lists of values.
See the `.two-shadows` selector below for an example.

The following example demonstrates many uses of shadows.

{% include "_css-box-shadow.html" %}

```html
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS box-shadow Demo</title>
    <style>
      .all-inset {
        box-shadow: 0 0 10px 10px inset var(--color);
      }

      .all-outset {
        box-shadow: 0 0 10px 10px var(--color);
      }

      .bottom-inset {
        box-shadow: 0 -10px 10px inset var(--color);
      }

      .bottom-outset {
        box-shadow: 0 10px 10px var(--color);
      }

      .box {
        --color: gray;
        --size: 4rem;
        height: var(--size);
        width: var(--size);

        border: 1px solid var(--color);
        font-family: sans-serif;
        padding: 1rem;
        margin: 1.5rem;
      }

      .left-inset {
        box-shadow: 10px 0 10px inset var(--color);
      }

      .left-outset {
        box-shadow: -10px 0 10px var(--color);
      }

      .right-bottom-outset {
        box-shadow: 10px 10px 10px var(--color);
      }

      .right-inset {
        box-shadow: -10px 0 10px inset var(--color);
      }

      .right-outset {
        box-shadow: 10px 0 10px var(--color);
      }

      .row {
        display: flex;
      }

      .top-inset {
        box-shadow: 0 10px 10px inset var(--color);
      }

      .top-left-inset {
        box-shadow: 10px 10px 10px inset var(--color);
      }

      .top-outset {
        box-shadow: 0 -10px 10px var(--color);
      }

      .two-shadows {
        box-shadow: 10px 0 10px red, 0 10px 10px blue;
      }
    </style>
  </head>
  <body>
    <div class="row">
      <div class="box top-outset">top outset</div>
      <div class="box top-inset">top inset</div>
      <div class="box bottom-outset">bottom outset</div>
      <div class="box bottom-inset">bottom inset</div>
    </div>
    <div class="row">
      <div class="box left-outset">left outset</div>
      <div class="box left-inset">left inset</div>
      <div class="box right-outset">right outset</div>
      <div class="box right-inset">right inset</div>
    </div>

    <div class="row">
      <div class="box right-bottom-outset">right and bottom outset</div>
      <div class="box top-left-inset">top and left inset</div>
    </div>

    <div class="row">
      <div class="box all-outset">outset all sides</div>
      <div class="box all-inset">inset all sides</div>
    </div>

    <div class="box two-shadows">two shadows</div>
  </body>
</html>
```

### Centering

There are many ways to center content using CSS.
Four common ways are:

- `text-align` property set to `center`
  and `line-height` property set to element height,
  possibly also setting `box-sizing` to `border-box`
- `margin` property set to `auto` when `display` is `block`
- `display` property set to `flex` and setting the
  `justify-content` and/or `align-items` properties to `center`
- `position` property set to `absolute` or `fixed`
  and setting the `transform` property

Each of these approaches is demonstrated in the code below.

{% include "_centering.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Centering Demo</title>
    <style>
      .box {
        color: white;
        height: var(--box-size);
        line-height: var(--box-size); /* causes vertical centering of text */
        text-align: center; /* centers text horizontally */
        width: var(--box-size);
      }

      .box1 {
        background-color: red;
        display: block;
        margin: 0 auto; /* centers box horizontally when display is block */
      }

      .box2 {
        background-color: blue;
        display: inline-block;
      }

      .box3 {
        background-color: green;
        opacity: 0.5;
        position: absolute;
        left: 50%; /* of width of position relative ancestor */
        top: 50%; /* of height of position relative ancestor */
        transform: translate(-50%, -50%); /* of box size */
      }

      #centering-demo {
        --box-size: 100px;
        --size: 550px;
        height: var(--size);
        outline: 1px dashed gray;
        /* Absolute positioning of .box3 is relative to this. */
        position: relative;
        width: var(--size);
      }

      .container {
        display: flex;
        justify-content: center;
        margin-top: 1rem;
        width: 100%;
      }

      p {
        text-align: center;
      }
    </style>
  </head>
  <body>
    <section id="centering-demo">
      <p>This is a paragraph.</p>
      <div class="box box1">Box #1</div>
      <div class="container">
        <div class="box box2">Box #2</div>
      </div>
      <!-- This box is centered in the section. -->
      <div class="box box3">Box #3</div>
    </section>
  </body>
</html>
```

### Flex layout

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Learn/CSS/CSS_layout/Flexbox",
"Flex layout" %} is a powerful way to control
the layout of HTML elements in one dimension.
{% aTargetBlank "https://flexboxfroggy.com", "Flexbox Froggy" %}
provides a great way to learn about this.

Many CSS properties affect flex layout.
The most commonly used properties applied to container elements
are described here.

- `display`

  Set this to `flex`.

- `flex-direction`

  This defaults to `row` for horizontal layout,
  but can be set to `column` for vertical layout.
  It defines the "major axis" and the opposite direction
  is considered to be the "minor axis".

- `justify-content`

  This controls layout on the major axis.
  The values are:

  - `flex-start` (default) pushes children toward the start.
  - `flex-end` pushes children toward the end.
  - `center` centers children.
  - `space-between` spreads children with
    no space before the first or after the last.
  - `space-evenly` spreads children with
    equal space before the first and after the last.
  - `space-around` spreads children with
    half space before the first and after the last.

- `align-items`

  This controls layout on the minor axis.
  The most commonly used values are:

  - `stretch` (default)
  - `flex-start` pushes children toward start
  - `flex-end` pushes children toward end
  - `center`

- `flex-grow`

  This is applied to children, not to the container.
  It affects the amount of space allocated to the element.
  The default value is zero.
  The `flex-grow` values of all the children are added to obtain a total.
  Then the `flex-grow` value of each child is
  divided by the total to get a percentage.
  That percentage of the **unused space** in the parent container
  is added to the size of the child element.

- `gap`

  This controls the space between elements when
  `justify-content` is not one of the values that begins with `space-`.

The table below demonstrates combinations of values
for the `justify-content` and `align-items` properties.
This is not an image. It is generated in your web browser.

{% include "_flex-layout.html" %}

Child elements can override their minor axis alignment
described by the `align-items` property on the container
by setting their `align-self` property.

### Grid layout

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Learn/CSS/CSS_layout/Grids",
"Grid layout" %} is a powerful way to control
the layout of HTML elements in two dimensions.
{% aTargetBlank "https://cssgridgarden.com", "Grid Garden" %}
provides a great way to learn about this.

Many CSS properties affect grid layout.
The most commonly used properties applied to container elements
are described here.

- `display`

  Set this to `grid`.

- `grid-template-columns`

  This specifies the number of columns and their widths.
  Widths can be specified using any CSS unit.
  They can also use `fr` fractional units, which work similarly
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

  This gives names to collections of grid cells,
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
    'nav main'
    'footer footer';
  ```

- `gap`

  This controls the space between grid areas.

The most commonly used properties applied to child elements
are described here.

- `grid-column-start` specifies the column number in which the child begins.
- `grid-column-end` specifies the column number in which the child ends.
- `grid-row-start` specifies the row number in which the child begins.
- `grid-row-end` specifies the row number in which the child ends.
- `grid-column: {start} / {end}` specifies the start and end columns.
- `grid-column: {start} / span {columns}` specifies the start and # of columns.
- `grid-row: {start} / {end}` specifies the start and end rows.
- `grid-row: {start} / span {rows}` specifies the start and # of rows.
- `grid-area: {grid-template-area-name}` specifies a grid template area name
  where the component should be rendered.

Negative values for `end` count from the last column or row.

Since a grid area already knows the columns and rows that it occupies,
specifying the `grid-area` property makes the other properties unnecessary.

The default alignment of elements within their grids is specified by setting
the `justify-content` and `align-items` properties on the container.
Child elements can override this by setting their
`justify-self` and `align-self` properties.

In the example below, grid layout is used to layout
a header, footer, left nav and main area of a page.

{% include "_grid-layout.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Grid Layout Demo</title>
    <style>
      body {
        --footer-height: 3rem;
        --header-height: 5rem;
        --nav-width: 10rem;

        display: grid;
        grid-template-columns: var(--nav-width) 1fr;
        grid-template-rows: var(--header-height) 1fr var(--footer-height);
        grid-template-areas:
          'header header'
          'nav main'
          'footer footer';

        height: 100vh;
        width: 100vw;
        margin: 0;
      }

      footer {
        grid-area: footer;
        background-color: lightgreen;
      }

      header {
        grid-area: header;
        background-color: orange;
      }

      main {
        grid-area: main;
        background-color: lightblue;
      }

      nav {
        grid-area: nav;
        background-color: lightcoral;
      }
    </style>
  </head>
  <body>
    <header>This is the header.</header>
    <nav>This is the nav.</nav>
    <main>This is the main area.</main>
    <footer>This is the footer.</footer>
  </body>
</html>
```

### `display` `inline-block` property

The default value of the CSS `display` property for many HTML elements,
including `div`, is `block`.
This prevents them from appearing on the same "row" as other elements,
unless flex or grid layout is specified for their parent element.

Another way to allow multiple block elements to appear on the same row
is to set the CSS `display` property to `inline-block`.

### Selectors

CSS selectors appear at the beginning of CSS rules.
They specify the elements to which a rule applies.
The basic selectors are:

- universal: selects every element; ex. `*`
- type: specifies an element name; ex. `table`
- id: specifies the value of an `id` attribute; ex. `#my-id`
- class: specifies a CSS class name; ex. `.my-class`
- attribute: specifies an attribute that:

  - must be present: ex. `[href]`
  - must be present with a given value: ex. `[href="https://foo.com"]`
  - must be present containing a given value: ex. `[href*="foo"]`
  - must be present beginning with a given value: ex. `[href^="https"]`
  - must be present ending with a given value: ex. `[href$=".com"]`
  - ... and other less commonly used options

Selectors can be combined to be more specific.
For example,
`button.primary` selects `button` elements that have a class of `primary` and
`.dog.active` selects elements with both the `dog` and `active` classes.

A rule can begin with a comma-separated list of selectors to match any of them.

### Combinators

Selectors can be combined to specify the following
relationships between elements:

- descendant: `a b` applies to `b` elements that have an `a` ancestor.
- child: `a > b` applies to `b` elements that have an `a` parent.
- sibling: `a ~ b` applies to `b` elements that have
  a preceding `a` sibling.
- sibling: `a + b` applies to `b` elements that have
  an immediately preceding `a` sibling.

These can be combined to any depth. For example,
`table tr > img` matches all `img` elements whose parent is a `tr` element
that is anywhere inside a `table` element.
They could appear inside the `thead` or `tbody` elements
that are children of the `table` element.

### Pseudo classes

CSS pseudo classes are added to selectors to indicate
that matching elements must be in a specific state.
They begin with a single colon.
Categorized lists of commonly used pseudo classes are described below.

**Location**

- `:link` matches links (`a` elements) that have not yet been visited.

- `:visited` matches links that have been visited.

**User action**

- `:focus` matches the element that currently has focus.

- `:focus-within` matches the element that currently has focus
  and elements that contain an element that currently has focus.

- `:hover` matches elements being hovered over.

**Input**

- `:blank` matches form elements that are empty.

- `:placeholder-shown` matches form elements that
  are currently displaying their placeholder text.

- `:checked` matches checkboxes and radio buttons that are toggled on.

- `:enabled` matches elements that are not disabled.

- `:disabled` matches elements that are disabled.

- `:required` matches form elements that are required to have a value.

- `:optional` matches form elements that are not required to have a value.

- `:valid` matches form elements that contain a valid value.

- `:invalid` matches form elements that contain an invalid value.

- `:read-only` matches elements that cannot be modified by the user.

- `:read-write` matches elements that can be modified by the user.

**Tree-structural**

- `:first-child` matches the first sibling.

- `:first-of-type` matches the first sibling with the same element name.

- `:last-child` matches the last sibling.

- `:last-of-type` matches the last sibling with the same element name.

- `:nth-child` matches sibling elements whose 1-based index matches `an+b`.
  For example, `:nth-child(3)` matches the third sibling,
  `:nth-child(even)` matches even siblings,
  `:nth-child(odd)` matches odd siblings,
  and `:nth-child(2n+3)` matches siblings 3, 5, 7, and so on.

- `:nth-of-type` is like `:nth-child`, but only
  considers siblings with the same element name.

- `:nth-last-child` matches sibling elements in a similar way to `:nth-child`,
  but counting from the end of the list of siblings.

- `:nth-last-of-type` is like `:nth-last-child`, but only
  considers siblings with the same element name.

- `:only-child` matches the element only if it has no siblings.

- `:only-of-type` matches the element only if
  it has no siblings with the same element name.

The following example demonstrates a few of the pseudo-classes.
If an invalid email address is entered in the input, the border turns red.
The "Baseball" button is red because it is disabled.
The other buttons change from blue to green when hovering over them.

{% include "_pseudo-classes.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Pseudo Classes Demo</title>
    <style>
      button {
        background-color: lightblue;
        border: 3px solid blue;
        border-radius: 0.5rem;
        font-size: 1rem;
        padding: 0.5rem;
      }

      button:hover {
        background-color: lightgreen;
        border-color: green;
      }

      /* Placing this after the :hover rule makes it take precedence. */
      button:disabled {
        background-color: pink;
        border-color: red;
      }

      form > div {
        margin-bottom: 0.5rem;
      }

      input {
        border: 2px solid gray;
        border-radius: 0.5rem;
        padding: 0.5rem;
      }

      input:invalid {
        border-color: red;
      }
    </style>
  </head>
  <body>
    <form id="pseudo-classes-demo">
      <div>
        <label for="email">Email</label>
        <input id="email" type="email" />
      </div>
      <div>
        <button disabled>Baseball</button>
        <button>Basketball</button>
        <button>Football</button>
        <button>Hockey</button>
      </div>
    </form>
  </body>
</html>
```

### Pseudo selectors

CSS pseudo elements are added to selectors to style a part of an element
or add content.
They begin with a double colon.
Commonly used pseudo selectors are described below.
Most supported pseudo selectors are rarely used.

- `::after` is used in conjunction with the `content` property
  to add content after the matching element.
- `::before` is used in conjunction with the `content` property
  to add content before the matching element.

Content can be added only to elements that allow content,
such as `label` and `p` elements.
Content cannot be added to empty elements like `input`.

The following example adds an asterisk
after the label of required form elements.

{% include "_pseudo-selectors.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Pseudo Selectors Demo</title>
    <style>
      form > div {
        margin-bottom: 0.5rem;
      }

      label {
        display: inline-block;
        margin-right: 0.5rem;
        text-align: right;
        width: 5rem;
      }

      .required label::after {
        color: red;
        content: '*';
      }
    </style>
    <script>
      window.onload = () => {
        // Add the "required" attribute to all inputs inside
        // container elements with the "required" CSS class.
        const requiredDivs = document.querySelectorAll('form > .required');
        for (const div of requiredDivs) {
          const input = div.querySelector('input');
          input.setAttribute('required', 'required');
        }
      };
    </script>
  </head>
  <body>
    <form id="pseudo-selectors-demo">
      <div class="required">
        <label for="name">Name</label>
        <input id="name" />
      </div>
      <div class="required">
        <label for="email">Email</label>
        <input id="email" type="email" />
      </div>
      <div>
        <label for="phone">Telephone</label>
        <input id="phone" type="tel" />
      </div>
      <button>Submit</button>
    </form>
  </body>
</html>
```

### Specificity

CSS specificity determines the precedence of conflicting CSS rules.
For example, consider the following HTML:

```html
<div class="parent">
  I am the parent.
  <div id="me" class="child" style="color: red">I am the child.</div>
</div>
```

The following CSS rules set the color of
the text “I am the child.” to different values.
The color used depends on the specificity of the selectors.
The scores are represented by lists of four numbers
and are explained after this example.

```css
/* score is 0,1,1,0 */
.parent > #me {
  color: pink;
}

/* same score as the previous selector */
.parent #me {
  color: red;
}

/* score is 0,1,0,0 */
#me {
  color: orange;
}

/* score is 0,0,2,0 */
.parent > .child {
  color: yellow;
}

/* same score as the previous selector */
.parent .child {
  color: green;
}

/* score is 0,0,1,0 */
.child {
  color: blue;
}

/* same score as the previous selector;
   It applies the color purple to the
   element with the class “parent”,
   but the previous rule applies the color blue
   to the element with the class “child”. */
.parent {
  color: purple;
}
```

The precedence order of these rules happens to be the order
in which they are listed here, with the exception that
when there are ties, the last one wins.
Using a `style` attribute on the inner `div` has the highest specificity.

There is a formula for computing the specificity score
of any CSS rule that results in a list of four numbers.
Considering the four numbers from left to right,

- The first is 1 for inline styles and 0 otherwise.
- The second is the number of `id` values in the selector.
- The third is the number of class names in the selector.
- The fourth is the number of element name references in the selector.

Since each matching rule can specific a different set of CSS properties,
properties from multiple rules can be applied to an element.

The score of a selector is determined by removing the commas
and treating it as a single 4-digit number.
For example, treat 1,2,3,4 as 1234.

For each property in a matching rule, the one specified in the
rule with the highest scoring selector wins.
In the case of a tie, one in the last tieing rule wins.

This means that inline styling specified with a
`style` attribute on an HTML element always wins.
After this, `id` attributes are more important than class names,
which are more important than element names.

It also means that the order in which `id` values,
class names, and element names appear in a selector
does not affect its specificity calculation.

For more details on CSS specificity, see
{% aTargetBlank "https://css-tricks.com/specifics-on-css-specificity/",
"Specifics on CSS Specificity" %} on the CSS-Tricks site.

### `position` property

The CSS `position` property supports many values.
This tip distinguishes between three of them.

| position value | Description                                                                   |
| -------------- | ----------------------------------------------------------------------------- |
| `absolute`     | relative to the document; removes element from document flow                  |
| `fixed`        | relative to the viewport; removes element from document flow                  |
| `sticky`       | relative to the document; element remains in document flow, but can be offset |

All of these use the `top`, `right`, `bottom`, and `left` properties
to specify the actual position of the element.

The `absolute` value causes elements to be positioned relative to the nearest
ancestor element that has its CSS `position` property set to `relative`.
If none is found, they are positioned relative to the browser window.

The `sticky` value is often used to keep `table` headings in view
when a table is scrolled vertically.

The following example demonstrates using `absolute` to
position elements at the four corners of an ancestor element
that uses a `position` value of `relative`.

{% include "_css-position.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS position absolute Demo</title>
    <style>
      #position-demo {
        display: inline-block;
        border: 1px solid black;
        height: 5rem;
        width: 8rem;
        position: relative;
      }

      #position-demo > div {
        position: absolute;
      }

      #first {
        top: 0;
        left: 0;
      }

      #second {
        top: 0;
        right: 0;
      }

      #third {
        bottom: 0;
        left: 0;
      }

      #fourth {
        bottom: 0;
        right: 0;
      }
    </style>
  </head>
  <body>
    <h4>CSS position absolute demo</h4>
    <div id="position-demo">
      <div id="first">First</div>
      <div id="second">Second</div>
      <div id="third">Third</div>
      <div id="fourth">Fourth</div>
    </div>
  </body>
</html>
```

<a name="css-position-fixed-sticky"></a>
The next example shows the differences between the CSS `position` property
values `absolute`, `fixed`, and `sticky`.

To run the demo, click the "Show Demo" button.
After viewing it, click the "Back" button to return to this location.
The demo will scroll this page to the top and display three boxes.
The first box uses a `position` value of `absolute`,
the second uses `fixed`, and the third uses `sticky`.
Scroll vertically to see that the `fixed` box remains in place
while the `absolute` and `sticky` boxes scroll with the page.
Scroll vertically inside the list of fruits to see how
using `position` `sticky` causes the "Fruit" line to remain in view.

{% include "_css-position-fixed-sticky.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS position Demo</title>
    <style>
      #absolute {
        position: absolute;
        top: 1rem;
        left: 1rem;
      }

      body {
        margin: 0;
      }

      #container {
        background-color: linen;
        height: 500px; /* so contents can scroll vertically */
        width: 100%;
      }

      .demo {
        background-color: white;
        border: 1px solid black;
        display: inline-block;
        height: 150px;
        width: 100px;
      }

      #fixed {
        position: fixed;
        top: 1rem;
        left: 8rem;
      }

      #scrolling {
        overflow: scroll;
        position: absolute;
        top: 1rem;
        left: 15rem;
      }

      #sticky {
        position: sticky;
        top: 0;
        left: 0;
        background-color: cornflowerblue;
        color: white;
      }
    </style>
  </head>
  <body>
    <div id="container">
      <div id="absolute" class="demo">absolute</div>
      <div id="fixed" class="demo">fixed</div>
      <div id="scrolling" i class="demo">
        <div id="sticky">Fruit</div>
        <div>apple</div>
        <div>banana</div>
        <div>blueberry</div>
        <div>cherry</div>
        <div>grape</div>
        <div>kiwi</div>
        <div>peach</div>
        <div>raspberry</div>
        <div>strawberry</div>
        <div>watermelon</div>
      </div>
    </div>
  </body>
</html>
```

### Vendor prefixes

Some CSS properties require "vendor prefixes" that indicate they are
vendor-specific extensions or are considered experimental.
Vendor prefixes include `-moz-` for Firefox,
`-ms-` for Internet Explorer and Edge, and
`-webkit-` for Chrome and Safari.

Over time the need for these prefixes has diminished,
but many CSS properties still require them.
For a list of CSS properties that still require vendor prefixes in 2021,
see the section "Prefixing in 2021" in this {% aTargetBlank
"https://css-tricks.com/is-vendor-prefixing-dead/#prefixing-in-2021",
"CSS Tricks article" %}.

It is not necessary to manually write CSS properties with vendor prefixes.
Tools such as {% aTargetBlank "https://postcss.org", "PostCSS" %} and
{% aTargetBlank "https://sass-lang.com", "Sass" %} can generate these for you.
Here are the steps to setup and use PostCSS and the
{% aTargetBlank "https://github.com/postcss/autoprefixer", "autoprefixer" %}
plugin to do this:

1. For projects that do not have a `package.json` file,
   create one by entering `npm init` and answering the questions it asks.
1. Enter `npm install -D postcss postcss-cli autoprefixer`
1. Add the following npm script in `package.json`:

   ```json
   "postcss": "postcss --use autoprefixer --dir build src/**/*.css"
   ```

1. Enter `npm run postcss` to generate new CSS files in the `build` directory.
1. Use the CSS files in the `build` directory
   instead of those in the `src` directory.

This changes CSS like the following:

```css
select {
  appearance: none;
}
```

to this:

```css
select {
  -webkit-appearance: none;
  -moz-appearance: none;
  appearance: none;
}
```

The `autoprefixer` npm package is one of many plugins available for PostCSS.
Other popular plugins include:

- {% aTargetBlank "https://github.com/csstools/postcss-preset-env",
  "postcss-preset-env" %}

  This "lets you convert modern CSS into something most browsers can understand,
  determining the polyfills you need based on
  your targeted browsers or runtime environments."

- {% aTargetBlank "https://github.com/hudochenkov/postcss-sorting",
  "postcss-sorting" %}

  This sorts properties within rules, not rules based on selectors.

- {% aTargetBlank "https://stylelint.io/user-guide/usage/postcss-plugin",
  "stylelint" %}

  This is a "linter that helps you avoid errors and enforce conventions."

### `pointer-events` and `appearance`

The CSS property `pointer-events` can be set to `none`
so that click events pass through an element to
elements that are behind it in stacking order or `z-index`.

The CSS property `appearance`, along with vendor prefix variants,
can be set to `none` to disable the default rendering for form controls
such as `button`, `input`, `textarea`, and `select`.
Other CSS properties can then be used to provide custom rendering
while retaining the functionality of the underlying form control.

The following example uses both of these on a `select` element.
The goal is to control the size and color of the
downward pointing triangle on the right side of the `select`.

{% include "_pointer-events.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS pointer-events and appearance Demo</title>
    <style>
      .select-wrapper {
        --color: blue;
        display: inline-block;
        position: relative;
      }

      /* The ::after pseudo selector doesn't work on select elements,
         but it does work on div elements.
         This is why the select is wrapped in a div below. */
      .select-wrapper::after {
        content: '▼';
        color: var(--color);
        font-size: 1.2rem;

        /* Position this over the small triangle
           provided by the select element.
           If appearance is set to none on the select,
           no small triangle will be rendered. */
        position: absolute;
        right: 2px;
        top: 6px;

        /* When this is clicked, allow the click to be processed
           by the select element, not this triangle. */
        pointer-events: none;
      }

      select {
        margin-left: 0.5rem;
        width: 5rem;

        /* This works in Chrome and Firefox, but Safari ignores it. */
        padding: 0.5rem;

        /* To get this to look nice in all the browsers, including Safari,
           we can just draw it ourselves. */
        appearance: none;
        -moz-appearance: none;
        -webkit-appearance: none;
        border: 1px solid var(--color);
        border-radius: 0; /* Safari has a non-zero default border radius. */
      }
    </style>
    <script>
      const prompt = 'Select a color.';
      window.onload = () => {
        const report = document.getElementById('report');
        report.textContent = prompt;

        const select = document.getElementById('color-select');
        select.addEventListener('change', event => {
          const {value} = event.target;
          report.textContent = value ? `You selected ${value}.` : prompt;
        });
      };
    </script>
  </head>
  <body>
    <section>
      <label for="color-select">Color</label>
      <div class="select-wrapper">
        <select id="color-select">
          <option></option>
          <option>Red</option>
          <option>Green</option>
          <option>Blue</option>
        </select>
      </div>
      <p id="report"></p>
    </section>
  </body>
</html>
```

### Transitions

The CSS {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/transition",
"transition properties" %} cause changes to specific CSS properties
to be applied over a given time duration, resulting in animation.
Note that not all CSS properties can be animated.
For a list of those that can, see {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_animated_properties",
"Animatable CSS properties" %}.

| CSS Property                 | Description                                                        |
| ---------------------------- | ------------------------------------------------------------------ |
| `transition-delay`           | time to wait before beginning transition                           |
| `transition-duration`        | time over which to spread the transition                           |
| `transition-property`        | property to transition or "all" (default)                          |
| `transition-timing-function` | name of an easing function                                         |
| `transition`                 | shorthand property that can specify multiple transition properties |

The `transition-timing-function` property specifies an easing function.
Built-in ones include:

- `ease` (default)
- `ease-in`
- `ease-out`
- `ease-in-out`
- `linear`
- `step-start`
- `step-end`
- `steps(n, jump-term)`

It is also possible to define custom easing functions using
`cubic-bezier(4-values)`. To determine the four values,
inspect an element that specifies a timing function
in the DevTools of Chrome, Firefox, or Safari.
Click the icon representing a cubic bezier curve and
drag the two handles that are displayed to obtain the desired curve.
This updates the timing function property to match
and takes effect immediately so the effect can be viewed.
Copy this property value and paste into the source file where it is specified.
For example, the following values cause the animation to
bounce backward in the middle: `0, 2.06, 1, -1.22`.

The `transition` property is a shorthand property whose value is
a list of values for the other transition properties.
If a list contains two time values, the first must be for
`transition-duration` and the second must be for `transition-delay`.

To demonstrate this, we will implement a toggle component
that is an oval containing a circle that represents a "thumb".
Clicking anywhere in the oval causes the thumb to move left or right,
indicating some application-specific option being disabled or enabled.
The CSS property `left` has a transition duration of 0.3 seconds,
so any change to that property takes place gradually over that duration.

{% include "_css-transition.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS transition Demo</title>
    <style>
      .toggle {
        --height: 2rem;

        display: inline-block;
        background-color: cornflowerblue;
        border-radius: calc(var(--height) / 2);
        height: var(--height);
        position: relative;
        width: calc(var(--height) * 2);
      }

      .thumb {
        --inset: 3px;
        --size: calc(var(--height) - 2 * var(--inset));

        position: absolute;
        left: var(--inset);
        top: var(--inset);

        background-color: orange;
        border-radius: calc(var(--size) / 2);
        height: var(--size);
        transition: left 0.3s;
        width: var(--size);
      }
    </style>
    <script>
      let selected = false;

      window.onload = () => {
        const toggle = document.querySelector('.toggle');
        const thumb = document.querySelector('.toggle > .thumb');

        // See the "Get element size" top in the "JavaScript" section
        // for details on the getBoundingClientRect method.
        const toggleWidth = toggle.getBoundingClientRect().width;
        const thumbWidth = thumb.getBoundingClientRect().width;
        const inset = getComputedStyle(thumb).getPropertyValue('--inset');
        const selectedLeft = toggleWidth - thumbWidth - parseInt(inset) + 'px';

        toggle.addEventListener('click', () => {
          selected = !selected;
          thumb.style.left = selected ? selectedLeft : inset;
        });
      };
    </script>
  </head>
  <body>
    <div class="toggle">
      <div class="thumb" />
    </div>
  </body>
</html>
```

### Transforms

The CSS {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/transform",
"transform property" %} translates, rotates, scales, and skews DOM elements.

The example below renders a button containing a finger pointing emoji.
When the button is pressed, a `rotate` `transform` is applied
to rotate the emoji 180 degrees.
When pressed again, the original rotation of zero degrees is restored.

{% include "_css-transform.html" %}

```html
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS transform Demo</title>
    <style>
      button {
        --size: 3rem;

        display: inline-block;
        background-color: cornflowerblue;
        border: none;
        border-radius: calc(var(--size) / 2);
        font-size: 2rem;
        height: var(--size);
        transition: transform 0.7s;
        width: var(--size);
      }

      button.rotate {
        transform: rotate(180deg);
      }
    </style>
    <script>
      window.onload = () => {
        const button = document.querySelector('button');
        button.addEventListener('click', () => {
          button.classList.toggle('rotate');
        });
      };
    </script>
  </head>
  <body>
    <p>Click the button to rotate it.</p>
    <button>☝</button>
  </body>
</html>
```

The next example renders a Pokemon card that starts face-down.
Clicking the card flips it over.
It uses a CSS transform to provide a nice 3D effect.

This works in Chrome, Firefox, and Safari
but flashes a bit in Safari, making the effect feel less polished.

{% include "_css-card-flip.html" %}

```html
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Card Flip Demo</title>

    <style>
      .card {
        height: 403px;
        width: 291px;

        /* This determines the distance between the z=0 plane and the user
           in order to give a 3D-positioned element some perspective. */
        perspective: 40rem;
      }

      .card-container {
        /* Absolute positioning of the img children are relative to this. */
        position: relative;

        /* This allows elements to have a sense of front and back faces. */
        transform-style: preserve-3d;

        /* If the transform property is modified,
           transition the change over this time duration. */
        transition: transform 0.7s;
      }

      .card-container > img {
        /* Stack the images on top of each other. */
        position: absolute;
        top: 0;
        left: 0;

        /* Hide the image when it is flipped. */
        backface-visibility: hidden;
      }

      /* The card front is always flipped 180 degrees from the card back. */
      .card-front {
        transform: rotateY(180deg);
      }

      .flipped {
        /* Making this negative flips right to left,
           whereas positive would flip left to right. */
        transform: rotateY(-180deg);
      }
    </style>
    <script>
      window.onload = () => {
        const cards = document.querySelectorAll('.card');
        for (const card of Array.from(cards)) {
          card.addEventListener('click', () => {
            card.firstElementChild.classList.toggle('flipped');
          });
        }
      };
    </script>
  </head>
  <body>
    <div class="card">
      <div class="card-container">
        <img alt="card back" src="pokemon-back.png" />
        <img alt="card front" src="pokemon-front.png" class="card-front" />
      </div>
    </div>
  </body>
</html>
```

### Animation with keyframes

We have seen how animations can be implemented
using the CSS `transition` and `transform` properties.
Another approach is to use the {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Animations/Using_CSS_animations",
"animation properties" %} along with `@keyframes`.

Keyframes specify changes to CSS properties
that should be animated over some time duration
using an easing function to control the rate of change.

Each keyframe specifies the properties to be applied
at a given percentage through the duration.
It is not necessary to specify a value for every property being animated
for every percentage that is specified.
For example, we could animate `color` changing from
`red` to `yellow` to `blue` and back to `red` at `0%`, `33%`, `67%`, and `100%`
while animating `font-size` changing from
`1rem` to `3rem` and back to `1rem` at `0%`, `50%`, and `100%`.

Animations created this way can do several things
that transitions cannot including:

1. repeating indefinitely
1. executing several sets of property changes
   in parallel, sequentially, or overlapped in any way
1. running animations in reverse
1. returning CSS properties to their original values at the end

The starting values of the properties to be animated
default to their current values.
However, alternate starting values can be specified with `from` or `0%`.
The ending values of these properties also default to their current values.
However, alternate ending values can be specified with `to` or `100%`.
Additional property values can be specified for
other percentages of time through the animation.

| CSS Property                | Description                                                           |
| --------------------------- | --------------------------------------------------------------------- |
| `animation-delay`           | time to wait before beginning animation                               |
| `animation-direction`       | `normal` (default), `reverse`, `alternate`, or `alternate-reverse`    |
| `animation-duration`        | time over which to spread the animation                               |
| `animation-fill-mode`       | determines the styles that are applied before and after the animation |
| `animation-iteration-count` | number of times to execute; defaults to 1; can specify `infinite`     |
| `animation-name`            | name of an `@keyframe`                                                |
| `animation-play-state`      | pauses a running animation or resumes a paused animation              |
| `animation-timing-function` | name of an easing function                                            |
| `animation`                 | shorthand property that can specify multiple animations               |

The `animation-direction` values that begin with "alternate" are
useful when `animation-iteration-count` is greater than 1.
They cause the animation to alternate between
going forward (`normal`) and backward (`reverse`).

The `animation-fill-mode` property determines the styles
that are applied before the animation begins and after it completes.
By default the current styles remain applied at the beginning
and are reapplied at the end.
Other than accepting this default, the most common value is "forwards"
which retains the property values set in the last keyframe.

The `animation-play-state` property can be set to
"paused" to pause a running animation or
"running" to resume a paused animation from where it stopped.

The `animation-timing-function` property specifies an easing function.
Built-in ones include:

- `ease` (default)
- `ease-in`
- `ease-out`
- `ease-in-out`
- `linear`
- `step-start`
- `step-end`
- `steps(n, jump-term)`

It is also possible to define custom easing functions
using `cubic-bezier(values)`.
For details, see the "Transitions" section above.

The `animation` property is a shorthand property whose value is
comma-separated lists of values for the other animation properties
that supports "chained" animations.
If a list contains two time values, the first must be for
`animation-duration` and the second must be for `animation-delay`.
Note that the `animation-iteration-count` value can be a number
that is distinguishable from a time value.

The following example demonstrates many of the animation properties.
It moves a square around the window when the "Start" button is pressed.
The background color of the square changes from
red to yellow to green to blue and back to red.
The "Start" button changes to "Pause" and a "Stop" button appears.
Pressing the "Pause" button pauses the animation
and changes the button to "Resume".
Pressing the "Resume" button resumes the animation.
Pressing the "Stop" button stops the animation,
which returns the square to its starting position
and changes the first button back to "Start".

{% include "_css-animation.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Animation Demo</title>
    <style>
      body {
        margin: 0;
      }

      #box {
        --size: 4rem;
        height: var(--size);
        width: var(--size);

        --start-distance: 2rem;
        --end-distance: 20rem;
        position: absolute;
        top: var(--start-distance);
        left: var(--start-distance);

        background-color: red;
        border: 3px solid black;
        color: white;
        padding: 1rem;
      }

      .jump {
        animation-duration: 3s;
        animation-fill-mode: forwards;
        animation-iteration-count: infinite;
        animation-name: jump-around;
        animation-timing-function: linear;
        /* animation-timing-function: cubic-bezier(0, 2.06, 1, -1.22); */
      }

      @keyframes jump-around {
        /* We can specify "from" or "0%" (same thing) properties here.
          They default to the starting property values
          which is what we want in this case. */
        25% {
          background-color: yellow;
          top: var(--start-distance);
          left: var(--end-distance);
        }
        50% {
          background-color: green;
          top: var(--end-distance);
          left: var(--end-distance);
        }
        75% {
          background-color: blue;
          top: var(--end-distance);
          left: var(--start-distance);
        }
        /* We can specify "to" or "1000%" (same thing) properties here.
          They default to the starting property values
          which is what we want in this case.
          The box will move from the lower left to the upper left
          and the color will change from blue to red. */
      }

      #stop-btn {
        display: none;
      }
    </style>
    <script>
      window.onload = () => {
        const startBtn = document.getElementById('start-btn');
        const stopBtn = document.getElementById('stop-btn');
        const box = document.getElementById('box');

        startBtn.addEventListener('click', () => {
          if (startBtn.textContent === 'Start') {
            box.classList.add('jump');
            box.style.animationPlayState = 'running';
            startBtn.textContent = 'Pause';
            stopBtn.style.display = 'inline-block';
          } else {
            const {style} = box;
            const running = box.style.animationPlayState === 'running';
            box.style.animationPlayState = running ? 'paused' : 'running';
            startBtn.textContent = running ? 'Resume' : 'Pause';
          }
        });

        stopBtn.addEventListener('click', () => {
          box.classList.remove('jump');
          startBtn.textContent = 'Start';
          stopBtn.style.display = 'none';
        });

        // This is useful when animation-iteration-count
        // is not set to infinite.
        // When the animation ends, it removes the "jump" class.
        // This allows the animation to be repeated
        // when the "jump" class is added again.
        /*
        box.addEventListener('animationend', () => {
          box.classList.remove('jump');
        });
        */
      };
    </script>
  </head>
  <body>
    <button id="start-btn">Start</button>
    <button id="stop-btn">Stop</button>
    <div id="box">I'm a box.</div>
  </body>
</html>
```

The following example demonstrates using the `animation` shorthand property
and chaining animations.

{% include "_css-animation-chain.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>keyframes Demo</title>
    <style>
      div {
        --common-options: 500ms alternate 2;
        font-size: 1rem;
        position: absolute;
        left: 0.5rem;
      }

      #text1 {
        top: 2.5rem;
      }

      #text2 {
        top: 3.5rem;
      }

      .zoom-down {
        /* This chains two animations. */
        animation: font-zoom var(--common-options), translate-down var(--common-options);
      }

      .zoom-right {
        /* This chains two animations. */
        animation: font-zoom var(--common-options), translate-right var(--common-options);
      }

      @keyframes font-zoom {
        to {
          font-size: 4rem;
        }
      }

      @keyframes translate-right {
        to {
          transform: translateX(400px);
        }
      }

      @keyframes translate-down {
        to {
          transform: translateY(200px);
        }
      }
    </style>
    <script>
      window.onload = () => {
        const startBtn = document.getElementById('start-btn');
        const text1 = document.getElementById('text1');
        const text2 = document.getElementById('text2');

        startBtn.addEventListener('click', () => {
          text1.classList.add('zoom-right');
          text2.classList.add('zoom-down');
        });

        text1.addEventListener('animationend', () => {
          text1.classList.remove('zoom-right');
        });

        text2.addEventListener('animationend', () => {
          text2.classList.remove('zoom-down');
        });
      };
    </script>
  </head>
  <body>
    <button id="start-btn">Start</button>
    <div id="text1">Hello</div>
    <div id="text2">Animation!</div>
  </body>
</html>
```

### Media queries

CSS {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries",
"media queries" %} have many uses, but the most common is to
apply different CSS properties based on the window width.

The following example lays out elements
horizontally when the browser window is wider than 768 pixels (desktop)
and vertically otherwise (mobile).
This treats the default styling as being for desktop
and the media query styling as being for mobile devices.
The opposite can be done with a media query that specifies `min-width`.

{% include "_media-queries.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Media Queries Demo</title>
    <style>
      .container {
        display: flex;
        justify-content: space-between;
      }

      .container > div {
        border: 1px solid gray;
        border-radius: 0.5rem;
        font-size: 1rem;
        padding: 1rem;
      }

      @media (max-width: 768px) {
        .container {
          align-items: flex-start;
          flex-direction: column;
        }

        .container > div {
          margin-bottom: 1rem;
        }
      }
    </style>
  </head>
  <body>
    <div class="container">
      <div id="first">First</div>
      <div id="second">Second</div>
      <div id="third">Third</div>
      <div id="fourth">Fourth</div>
    </div>
  </body>
</html>
```

### `window.matchMedia` method

The CSS Object Model (CSSOM) {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Window/matchMedia",
"window.matchMedia" %} method can be used to support light and dark modes.
For example, the following line of JavaScript code
determines if the user has configured their operating system
to prefer dark mode:

```js
const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
```

This can then be used to add a CSS class name such as "dark-mode"
to the `body` element.

CSS for the `body` element can define CSS variables
that specify colors to be used in light mode.
A separate rule with the selector `body.dark-mode` can override the
values of those CSS variables with the values to be used in dark mode.
The following example demonstrates this.

{% include "_prefers-color-scheme.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>window.matchMedia Demo</title>
    <style>
      body {
        --bg-color: white;
        --fg-color: purple;

        background-color: var(--bg-color);
        color: var(--fg-color);
        padding: 0 1rem 1rem 1rem;
      }

      body.dark-mode {
        --bg-color: purple;
        --fg-color: white;
      }
    </style>
    <script>
      window.onload = () => {
        // Get the light/dark preference from operating system (OS)
        // and use it for the initial setting.
        const prefersDark = window.matchMedia(
          '(prefers-color-scheme: dark)'
        ).matches;
        if (prefersDark) document.body.classList.add('dark-mode');

        // Allow the user switch modes without modifying the OS setting.
        const toggleBtn = document.getElementById('toggle-btn');
        toggleBtn.addEventListener('click', () => {
          document.body.classList.toggle('dark-mode');
        });
      };
    </script>
  </head>
  <body>
    <p>Try changing the light/dark preference in your operating system.</p>
    <p>
      In macOS, open System Preferences and select "General". After
      "Appearance", select "Light" or "Dark".
    </p>
    <p>
      In Windows, select Start...Settings...Personalization...Colors. Under
      "Choose your color", select "Light" or "Dark".
    </p>
    <button id="toggle-btn">Toggle Light/Dark Mode</button>
  </body>
</html>
```

### Linting

The {% aTargetBlank "https://stylelint.io", "stylelint" %} linter
is a great option for detecting errors and formatting issues in CSS.
It can validate CSS rules found in many file types including
CSS, HTML, JavaScript, Markdown, PostCSS,
{% aTargetBlank "https://sass-lang.com", "Sass" %},
Svelte, TypeScript, and Vue.

To add this to a project:

1. cd to the root directory of the project.
1. Enter `npm install -D stylelint stylelint-config-standard`
1. Create the file `.stylelintrc.json` containing the following:

   ```json
   {
     "extends": "stylelint-config-standard"
   }
   ```

To run stylelint, enter `npx stylelint **/*.css` in the root project directory.
Any number of glob patterns can be specified to process multiple file types.
Alternatively, add an npm script like the following in the `package.json` file
and run it by entering `npm run stylelint`:

```json
"stylelint": "stylelint **/*.{css,svelte}"
```

There is a VS Code extension for stylelint ({% aTargetBlank
"https://github.com/stylelint/vscode-stylelint#readme",
"stylelint.vscode-stylelint" %})
that automatically validates CSS and identifies issues.

## JavaScript

### Embrace booleans

Boolean is a fundamental data type.
Many JavaScript expressions evaluate to a boolean value
and can be used in a direct way as shown below.

```js
// Bad
if (total >= 100) {
  return true;
} else {
  return false;
}

// Only slightly better.
return total >= 100 ? true : false;

// Best
return total >= 100;
```

### Variables

JavaScript supports three keywords for declaring variables,
`var`, `const`, and `let`.

| Keyword | Modifiable | Scope                        | Hoisted |
| ------- | ---------- | ---------------------------- | ------- |
| `var`   | yes        | enclosing function or global | yes     |
| `const` | no         | enclosing block or module    | no      |
| `let`   | yes        | enclosing block or module    | no      |

Hoisted variables can be used in the scope of the variable before
their declaration, which is odd from a code readability standpoint.

Use of `var` is generally discouraged.

Using `const` provides useful documentation to readers of the code
that a variable will not be modified.
Keep in mind that when a `const` variable holds an object or array,
its contents can be modified, but the variable cannot be modified
to refer to a different object or array.

Most JavaScript developers reach for `const` first and
only use the `let` keyword for variables whose values need to be modified.

### Function declarations

JavaScript supports two syntaxes for declaring functions,
function expressions (older style) and arrow functions (newer style).
There are important differences between these,
and there are good reasons to use both.

| Can Use Keyword  | Function Expression | Arrow Function |
| ---------------- | ------------------- | -------------- |
| can use `this`?  | yes                 | no             |
| can use `super`? | yes                 | no             |
| can use `yield`? | yes                 | no             |

The `this` and `super` keywords are used in functions
that are methods of some object, so arrow functions
cannot be used to define methods that use these keywords.

The `yield` keyword is used in {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function*",
"generator functions" %}, so arrow functions cannot be used to define them.

There are a few other differences that rarely come up in practice.

Arrow functions are ideal for defining functions
that only need to be passed to another function,
such as the `Array` methods described later.
They are also ideal for functions that only
need to return the value of a single expression
because those can omit the curly braces around the function body
and the `return` keyword.

The following example uses such an arrow function to sort an array of numbers.

```js
const numbers = [7, 1, 8, 4, 3];

// Sorting using a function expression ... verbose.
numbers.sort(function (n1, n2) {
  return n1 - n2;
});

// Sorting using an arrow function ... compact.
numbers.sort((n1, n2) => n1 - n2);
```

There are two ways to define a named function,
as shown in the examples below.

```js
// Function expression
function average(numbers) {
  const sum = numbers.reduce((acc, n) => acc + n, 0);
  return sum / numbers.length;
}

// Arrow function
const average = numbers => {
  const sum = numbers.reduce((acc, n) => acc + n, 0);
  return sum / numbers.length;
};
```

Using an arrow function does not save typing because
the `function` keyword is shorter than the combination of
the `const` keyword, the `=` operator, the `=>` arrow,
and the spaces around them.

Some developers prefer using function expressions to define named functions
because the first line of code immediately makes it clear
that a function is being defined.
When an arrow function is used, readers of the code don't know
that a function is being defined until they
scan enough of the first line to find the `=>` arrow.

### Nested ternaries

A common opinion is that nested ternaries are hard to read.
But this is only true if they are formatted poorly.
Consider this example.

{% raw %}

```text
let assessment;
if (temperature &lt;= 32) {
  assessment = 'cold';
} else if (temperature &lt;= 80) {
  assessment = 'mild';
} else {
  assessment = 'hot';
}

// This uses nested ternaries to achieve the same result.
const assessment =
  temperature &lt;= 32 ? 'cold' :
  temperature &lt;= 80 ? 'mild' :
  'hot';
```

{% endraw %}

### Array methods

The JavaScript `Array` class supports many methods for operating on elements.
Some of the most useful methods are described below.
The examples use an array of objects that describe dogs.

```js
const comet = {name: 'Comet', breed: 'Whippet', weight: 31};
const snoopy = {name: 'Snoopy', breed: 'Beagle', weight: 40};
const dogs = [
  {name: 'Maisey', breed: 'Treeing Walker Coonhound', weight: 55},
  {name: 'Ramsay', breed: 'Native American Indian Dog', weight: 85},
  {name: 'Oscar', breed: 'German Shorthaired Pointer', weight: 70},
  comet
];
```

- `forEach` takes a function and iterates over the elements.
  It is useful in cases where it is desirable to
  call a named function on each element.
  In other cases, it can be more clear to use a `for of` loop.

  ```js
  function report(dog) {
    console.log(`${dog.name} is a ${dog.breed}.`);
  }

  dogs.forEach(dog => report(dog));
  dogs.forEach(report); // same as the previous line

  for (const dog of dogs) {
    report(dog);
  }
  ```

  However, `forEach` has the advantage that
  it makes the current index available.
  For example:

  ```js
  dogs.forEach((dog, index) => {
    console.log(`${index + 1}) ${dog.name}`);
  });
  ```

  This outputs:

  ```text
  1) Maisey
  2) Ramsay
  3) Oscar
  4) Comet
  ```

- `filter` takes a function and
  creates a new array containing a subset of the elements.

  ```js
  const bigDogs = dogs.filter(dog => dog.weight >= 70);
  // [object for Ramsay, object for Oscar]
  ```

- `map` takes a function and creates a new array with the same length.

  ```js
  const dogNames = dogs.map(dog => dog.name);
  // ['Maisey', 'Ramsay', 'Oscar', 'Comet']
  ```

- `reduce` takes a function and an initial value
  and creates a single value from the elements.
  The parameter name `acc` is short for "accumulator" and is a
  common name for the first parameter of the function passed to `map`.

  ```js
  // The initial value is zero.
  const totalWeight = dogs.reduce((acc, dog) => acc + dog.weight, 0);

  // Create an object where the keys are dog names
  // and the values are the dog objects.
  // This doesn't gracefully handle having
  // multiple dogs with the same name.
  // The initial value is an empty object.
  const nameToDogMap = dogs.reduce((acc, dog) => {
    acc[dog.name] = dog;
    return acc;
  }, {});
  ```

- `some` takes a function and
  determines if some element meets the criteria specified by the function.

  ```js
  const haveWhippet = dogs.some(dog => dog.breed === 'Whippet'); // true
  ```

- `every` takes a function and
  determines if every element meets the criteria specified by the function.

  ```js
  const allWhippets = dogs.every(dog => dog.breed === 'Whippet'); // false
  ```

- `find` takes a function and
  finds the first element that meets the criteria specified by the function.

  ```js
  const firstBigDog = dogs.find(dog => dog.weight >= 70);
  // object for Ramsay
  ```

- `findIndex` takes a function and finds the index of the first element
  that meets the criteria specified by the function.

  ```js
  const firstBigDogIndex = dogs.findIndex(dog => dog.weight &gt;= 70);
  // 1 which is the index for Ramsay
  ```

- `includes` determines if at least one of the elements
  matches a given value.

  ```js
  const haveComet = dogs.includes(comet); // true
  const haveSnoopy = dogs.includes(snoopy); // false
  ```

- `join` creates a string by joining the string representation
  of the elements with a given separator string.

  ```js
  const combined = dogNames.join(', ');
  // 'Maisey, Ramsay, Oscar, Comet'
  ```

- `sort` sorts the elements in place rather than creating a new array.
  If no comparator function is passed, the elements are compared as strings.
  A comparator function that takes a pair of elements can be passed.
  It must return a negative number if they are in the correct order,
  a positive number if they are not,
  and zero if they are the same or the order doesn't matter.
  Also see `Intl.Collator` described later.

  ```js
  // Sort dogs by weight in ascending order.
  dogs.sort((d1, d2) => d1.weight - d2.weight);

  // Sort dogs by weight in descending order.
  dogs.sort((d1, d2) => d2.weight - d1.weight);

  // Sort dogs by name.
  // Using "localeCompare" sorts in a language-specific way.
  dogs.sort((d1, d2) => d1.name.localeCompare(d2.name));
  ```

- `push` adds any number of values to the end
  by passing any number of arguments.

  ```js
  dogs.push(snoopy); // Adds Snoopy at the end.
  ```

- `pop` removes the last element.

  ```js
  dogs.pop(snoopy); // Removes Snoopy from the end.
  ```

- `unshift` adds any number of values to the beginning
  by passing any number of arguments.

  ```js
  dogs.unshift(snoopy); // Adds Snoopy at the beginning.
  ```

- `shift` removes the first element.

  ```js
  dogs.shift(snoopy); // Removes Snoopy from the beginning.
  ```

### CSS properties from JavaScript

HTML elements are represented by
Document Object Model (DOM) objects in memory.
DOM objects have a `style` property whose value is an object.
The keys of this object are camelCased CSS property names,
and the values are the values of those CSS properties.

Using JavaScript, new properties can be added to the `style` objects
and existing ones can be modified or deleted.

In the example below the CSS `color` property of the
element with an id of "greeting" is set to "red" in a CSS rule.
When the "Toggle Color" button is pressed,
a function is called that gets the current value of that style property
and modifies it based on its current value.

{% include "_change-css-style-properties.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Properties Demo</title>
    <style>
      #greeting {
        color: red;
      }
    </style>
    <script>
      window.onload = () => {
        const greeting = document.getElementById('greeting');
        const toggleBtn = document.getElementById('toggle-btn');
        toggleBtn.onclick = () => {
          // This is initially unset, not populated from the style tag.
          const {color} = greeting.style;
          greeting.style.color = color === 'blue' ? 'red' : 'blue';
        };
      };
    </script>
  </head>
  <body>
    <p id="greeting">Hello, World!</p>
    <button id="toggle-btn">Toggle Color</button>
  </body>
</html>
```

### CSS variables from JavaScript

JavaScript code can access the values of CSS variables using a
combination of the {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle",
"getComputedStyle" %} and {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration/getPropertyValue",
"getPropertyValue" %} methods.
JavaScript code can also modify the values of CSS variables
using the {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration/setProperty",
"setProperty" %} method.
This updates the use of all CSS properties that reference them.

These method names are a bit inconsistent as there is
no method named `getProperty` or `setPropertyValue`.

In the example below the CSS variable `--color`
is initially set to "red" and used to
set the CSS `color` property of `p` elements.
When the "Toggle Color" button is pressed,
a function is called that gets the current value of the CSS variable
and modifies it based on its current value.

{% include "_change-css-variable.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Variables Demo</title>
    <style>
      p {
        --color: red;
        color: var(--color);
      }
    </style>
    <script>
      window.onload = () => {
        const greeting = document.getElementById('greeting');
        const toggleBtn = document.getElementById('toggle-btn');
        toggleBtn.onclick = () => {
          const color = getComputedStyle(greeting)
            .getPropertyValue('--color')
            .trim();
          greeting.style.setProperty(
            '--color',
            color === 'blue' ? 'red' : 'blue'
          );
        };
      };
    </script>
  </head>
  <body>
    <p id="greeting">Hello, World!</p>
    <button id="toggle-btn">Toggle Color</button>
  </body>
</html>
```

### Getting element size

DOM elements support the method {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect",
"getBoundingClientRect" %} that returns a `DOMRect` object.
This object contains the properties `width` and `height`,
which provide the size of the element including the border.
It also contains the following properties that describe its position:

| Property | Description                         |
| -------- | ----------------------------------- |
| `left`   | distance from left side of viewport |
| `top`    | distance from top side of viewport  |
| `right`  | `left` + `width`                    |
| `bottom` | `top` + `height`                    |
| `x`      | synonym for `left`                  |
| `y`      | synonym for `top`                   |

In the example below a `div` element is rendered with CSS properties
that give it absolute positioning with a fixed width and height.
The `getBoundingClientRect` method is called on this element
and the properties of the object returned are output as a JSON string
in a read-only `textarea`.

{% include "_getBoundingClientRect.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>getBoundingClientRect Demo</title>

    <style>
      #box {
        position: absolute;
        left: 160px;
        top: 50px;

        background-color: orange;
        box-sizing: border-box;
        height: 200px;
        padding: 1rem;
        width: 300px;
      }

      textarea {
        display: block;
        margin-top: 1rem;
        width: 130px;
      }
    </style>
  </head>
  <body>
    <div id="box">I'm a box.</div>
    <textarea id="report" readonly rows="10"></textarea>

    <script>
      const box = document.getElementById('box');
      const report = document.getElementById('report');
      const rect = box.getBoundingClientRect();
      report.value = JSON.stringify(rect, null, 2);
    </script>
  </body>
</html>
```

### Promises

The JavaScript {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise",
"Promise" %} class provides an alternative to callback functions
for dealing with asynchronous operations.
Many JavaScript functions return `Promise` objects.
One example is the `fetch` function described in the next section.

A promise can be in one of three states:

- pending
- fulfilled with a value (a.k.a. resolved)
- rejected with a reason or error

To create a `Promise` object, call the `Promise` constructor,
passing it a function that has one or two parameters.
The first parameter is a function to call
to put the promise in a resolved state.
The second parameter (optional) is a function to call
to put the promise into a rejected state.
For example:

```js
const myPromise = new Promise((resolve, reject) => {
  // Do some work and eventually call either resolve or reject.
});
```

There are two syntaxes for waiting for a promise to be fulfilled or rejected.

One syntax is to use chains of calls to the
`then`, `catch`, and `finally` methods.
Callback functions are passed to each of these methods.

- The `then` method can be passed one or two callback functions.
  If the promise resolves, the first callback
  is called with the resolved value.
  If the promise rejects, the second callback is called with the error.
- The `catch` method is passed one callback function
  that is called with the error only if the promise rejects.
- The `finally` method is passed one callback function
  that is called last with no arguments
  after the promise either resolves or rejects.

The other syntax is to use the `await` keyword,
often inside a `try` block that has a corresponding `catch` block.
Many developers find this syntax easier to read.
Note that when the `await` keyword is used inside a function,
the function definition must begin with the `async` keyword.

The following example demonstrates using a `Promise`
to implement a `sleep` function.
This promise eventually resolves and never rejects.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Promises Demo #1</title>
    <script>
      function sleep(ms) {
        return new Promise(resolve => setTimeout(resolve, ms));
      }

      async function greetLater(name, ms) {
        await sleep(ms);
        alert('Hello, ' + name);
      }

      window.onload = () => {
        const button = document.getElementById('greet-btn');
        // When the button is clicked, it waits 3 seconds and
        // then displays a greeting in an alert dialog.
        button.addEventListener('click', () => greetLater('Mark', 3000));
      };
    </script>
  </head>
  <body>
    <button id="greet-btn">Greet Later</button>
  </body>
</html>
```

All functions that are marked as `async` return a `Promise` object,
even if they do not explicitly return anything.
In that case they return a `Promise` that resolves to the value `undefined`.

The following example demonstrates creating a `Promise`
that can resolve or reject.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Promises Demo #2</title>

    <!-- Specifying type="module" enables use of top-level await,
         which is using the await keyword outside of any function. -->
    <script type="module">
      // This takes a function and a number of milliseconds.
      // It returns a promise that waits for the
      // number of milliseconds and then calls the function.
      // If the function doesn't throw,
      // the promise resolves to the return value of the function.
      // If the function throws, the promise rejects with the error.
      function callLater(fn, ms) {
        return new Promise((resolve, reject) => {
          setTimeout(() => {
            try {
              resolve(fn());
            } catch (e) {
              reject(e);
            }
          }, ms);
        });
      }

      // This prints given text a specified number of times.
      // If times is zero or negative, it throws.
      function printTimes(text, times) {
        if (times <= 0) {
          throw new Error('printTimes requires times > 0');
        }

        for (let i = 0; i < times; i++) {
          console.log(text);
        }
      }

      try {
        // This waits 2 seconds and then prints "yes" three times.
        await callLater(() => printTimes('yes', 3), 2000);

        // This waits 1 second and then prints an error message
        // because times (-2) is not greater than zero.
        await callLater(() => printTimes('no', -2), 1000);

        // This line is not reached because the previous line throws.
        console.log('finished');
      } catch (e) {
        console.error(e);
      }
    </script>
  </head>
  <body>
    <p>See output in the DevTools console.</p>
  </body>
</html>
```

The `try`/`catch` above can be replaced with the following code
that uses the `then` and `catch` methods.
However, this code is somewhat less readable.

```js
// This prints "yes" three times.
callLater(() => printTimes('yes', 3), 2000)
  // Note that this callback function returns a promise
  // which is required to chain another "then" call.
  .then(() => callLater(() => printTimes('no', -2), 1000))
  .then(() => {
    // This line is not reached because the promise
    // returned by the callback of the previous "then" rejects.
    console.log('finished'); // not reached
  })
  .catch(e => {
    console.error(e);
  });
```

The `Promise` class defines four static methods
that take an `Iterable` of `Promise` objects, typically an array,
and return a new promise.
These are summarized in the table below.

| Static Method        | Resolves When                      | Rejects When            |
| -------------------- | ---------------------------------- | ----------------------- |
| `Promise.all`        | all the promises resolve           | any promise rejects     |
| `Promise.allSettled` | all the promises resolve or reject | never                   |
| `Promise.any`        | any promise resolves               | all the promises reject |
| `Promise.race`       | any promise resolves               | any promise rejects     |

If the promise returned by `Promise.all` resolves,
the value is an array of the resolved values
in the same order as the promises that were passed in.
If it rejects, the value is that of the first promise that rejects.

If the promise returned by `Promise.allSettled` resolves,
the value is an array of the resolved and rejected values
in the same order as the promises that were passed in.

If the promise returned by `Promise.any` resolves,
the value is that of the first promise that resolved.
If it rejects, the value is an {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AggregateError",
"AggregateError" %} object that holds a collection of the errors.

If the promise returned by `Promise.race` resolves,
the value is that of the first promise that resolves.
If it rejects, the value is that of the first promise that rejects.

Sometimes it is convenient to create a `Promise` that immediately
resolves or rejects and return it from a function.
To create such a promise, call the static methods
`Promise.reject(reason)` and `Promise.resolve(value)`.

### Fetch API

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API", "Fetch API" %},
defined by the {% aTargetBlank "https://fetch.spec.whatwg.org",
"Web Hypertext Application Working Group (WHATWG)" %}
provides a JavaScript API for fetching resources, typically using HTTP.
It defines a single function, {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch",
"fetch" %}, that takes a URL that
identifies a resource (or a {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Request",
"Request" %} object) and an optional options object.
The options object can include the following properties:

- `method`: This is an HTTP method name, such as
  `GET`, `POST`, `PUT`, or `DELETE`.
- `headers`: This is an object whose
  keys are request header names and whose values are their values.
- `body`: This specifies content to be passed in the request body.
  Methods such as `POST` and `PUT` use this.
  The value can be a string, a `FormData` object, a `Blob` object,
  or one of several other less commonly used types.
- ... and several more less commonly used properties

The `fetch` function returns a `Promise` object.
This promise resolves to a response object that
can contains many properties including:

- `status`: This is an HTTP status code such as 200 (OK),
  201 (Created), 400 (Bad Request), 401 (Unauthorized),
  403 (Forbidden), 404 (Not Found), 500 (Internal Server Error),
  and many more less commonly used values.
- `statusText`: This is the status message
  corresponding to the status code.
- `ok`: This is a boolean that indicates
  whether the request was successfully processed,
  indicated by the `status` being in the range 200 to 299.
  Note that other status values do not cause
  the returned promise to reject,
  so it is important to check this boolean value
  along with the status code and handle unsuccessful responses.
- `headers`: This is an object whose
  keys are response header names and whose values are their values.
- `body`: This is the response data,
  which can be text (including JSON) or binary data.
  The `Content-Type` response header indicates the format of this data.

Here are examples of using the `fetch` function to
create (POST), retrieve (GET), update (PUT), and delete (DELETE)
resources using a fictional set of {% aTargetBlank
"https://en.wikipedia.org/wiki/Representational_state_transfer",
"REST" %} services.

```js
const DOG_SERVICE_URL = 'https://dogs.com/manage';

async function createDog(breed, name) {
  const dog = {breed, name};
  const body = JSON.stringify(dog);
  const headers = {
    'Content-Length': body.length,
    'Content-Type': 'application/json'
  };
  const res = await fetch(
    DOG_SERVICE_URL,
    {method: 'POST', headers, body }
  );
  if (!res.ok) throw new Error(await res.text());
  return res.json();
}

async function retrieveDog(id) {
  const res = await fetch(DOG_SERVICE_URL + '/' + id);
  if (!res.ok) throw new Error(await res.text());
  return res.json();
}

async function updateDog(dog) {
  const body = JSON.stringify(dog);
  const headers = {
    'Content-Length': body.length,
    'Content-Type': 'application/json'
  };
  const res = await fetch(
    DOG_SERVICE_URL + '/' + dog.id,
    {method: 'PUT', headers, body }
  );
  if (!res.ok) throw new Error(await res.text());
  return res.json();
}

async function deleteDog(id) {
  const res = await fetch(
    DOG_SERVICE_URL + '/' + id
    {method: 'DELETE'}
  );
  if (!res.ok) throw new Error(await res.text());
}
```

Here is example code that uses these functions:

```js
async function demo() {
  try {
    const dog1 = await createDog('Whippet', 'Comet');
    console.log('created dog is', dog1);

    // This assumes that the JSON object returned by
    // the createDog function includes an id property.
    const dog2 = await getDog(dog1.id);
    console.log('retrieved dog is', dog2);

    dog2.name = 'Fireball';
    const dog3 = await updateDog(dog2);
    console.log('updated dog is', dog3);

    await deleteDog(dog3.id);
    console.log('The dog is gone.');
  } catch (e) {
    console.error(e);
  }
}

demo();
```

Browsers limit the total number of open connections to other hosts
and the number of open connections per host.
As of 2021, the limits for popular browsers are those shown below.
When the number of concurrent requests exceeds these limits,
requests wait to be processed, which can make their time to complete
much longer than the actual processing time.

| Browser | Total Open Connections | Open Connections per Host |
| ------- | ---------------------- | ------------------------- |
| Chrome  | 10                     | 6                         |
| Edge    | same as IE11           | same as IE11              |
| Firefox | 17                     | 6                         |
| IE11    | 17                     | 11                        |
| Safari  | 17                     | 6                         |

### Intersection Observer API

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API",
"Intersection Observer API" %} enables executing JavaScript code
when an element intersections an ancestor element or the browser viewport.
Often the goal is to perform some processing when the user scrolls the page,
bringing certain elements into view. Examples include:

- loading images on when they come in to view
- fetching data only when it needs to be displayed
- adding animation to certain elements when they scroll into view

The last use case is implemented on the page you are reading.
Notice how the color of section headings
temporarily changes when they come into view.

{% include "_intersection-observer.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Intersection Observer Demo</title>
    <style>
      .target {
        --size: 10rem;
        height: var(--size);
        width: var(--size);

        animation-delay: 100ms;
        animation-direction: alternate;
        animation-duration: 500ms;
        animation-iteration-count: 2;
        background-color: cornflowerblue;
        color: white;
        font-family: sans-serif;
        font-size: 2rem;
        margin: 1rem;
        padding: 1rem;
      }

      @keyframes pulse {
        100% {
          color: red;
          font-size: 3rem;
        }
      }
    </style>
    <script>
      window.onload = () => {
        function callback(entries) {
          for (const entry of entries) {
            entry.target.style.animationName = entry.isIntersecting
              ? 'pulse'
              : 'none';
          }
        }
        //const options = {threshold: 0.5}; // half visible
        const options = {threshold: 1}; // fully visible
        const observer = new IntersectionObserver(callback, options);

        const targets = document.querySelectorAll('.target');
        for (const target of targets) {
          observer.observe(target);
        }
      };
    </script>
  </head>
  <body>
    <div class="target">First</div>
    <div class="target">Second</div>
    <div class="target">Third</div>
    <div class="target">Fourth</div>
    <div class="target">Fifth</div>
  </body>
</html>
```

## Drawing

Both {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/SVG", "SVG" %}
and the {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API",
"Canvas API" %} can be used to draw things on a web page.

SVG stands for "Scalable Vector Graphics".
It is a markup language similar to HTML, which makes it declarative.
You tell it what to draw, not how to draw it.
SVG elements appear in the DOM and can be styled with CSS.
SVG draws using vectors, which allows it
to render sharp images at nearly any size.
SVG is a great choice for rendering icons.

The Canvas API draws on a `canvas` element using a JavaScript API,
which makes it imperative. You tell it how to draw.
The Canvas API is a great choice for implementing games.

For everything in between rendering icons and implementing games,
the choice is less clear.
For many developers SVG is the default choice until
they need to do something that performs poorly when using SVG.

### SVG

The basics steps to use SVG are:

1. Start with the following element.

   ```html
   <svg viewBox="0 0 maxX maxY" xmlns="http://www.w3.org/2000/svg">
     <!-- Add other elements here. -->
   </svg>
   ```

   The `viewBox` defines the coordinate system.
   Replace `maxX` and `maxY` with
   the maximum values in those dimensions.
   The minimum values do not have to be zero,
   but those are common values.

1. Include child elements for rendering specific kinds of things.
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

1. Wrap a `g` element around groups of children
   that need to be positioned or manipulated as a group.
1. Add a CSS rule for the `svg` element
   that defines its actual `width` and `height`.

The color of lines, paths, and shapes can be specified
using the CSS `stroke`, `stroke-width`, and `fill` properties.

The following example demonstrates all of
the commonly used SVG elements.

{% include "_svg.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>SVG Demo</title>
    <style>
      svg {
        height: 230px;
        width: 180px;
      }
      svg > * {
        stroke-width: 8px;
      }
      circle {
        fill: pink;
        stroke: red;
      }
      ellipse {
        fill: lightgreen;
        stroke: green;
      }
      line {
        stroke: blue;
        stroke-linecap: round;
      }
      path {
        fill: none;
        stroke: orange;
        stroke-linecap: round;
        stroke-linejoin: round;
      }
      polygon {
        fill: lightblue;
        stroke: purple;
        stroke-linecap: round;
        stroke-linejoin: round;
      }
      rect {
        fill: lightblue;
        stroke: blue;
      }
      text {
        fill: blue;
      }
    </style>
  </head>
  <body>
    <section id="svg-demo">
      <svg viewBox="0 0 800 600" xmlns="http://www.w3.org/2000/svg">
        <circle cx="30" cy="30" r="25" />
        <ellipse cx="120" cy="30" rx="50" ry="25" />
        <image x="0" y="60" width="50" height="50" href="./duck.png" />
        <line x1="60" y1="60" x2="170" y2="100" />
        <path d="M 10,120 L 70,160 L 70,120" />
        <polygon points="90,120 160,160 160,120" />
        <rect x="5" y="170" width="80" height="50" rx="10" />
        <text x="95" y="180">
          <tspan>Hello</tspan>
          <tspan x="95" dy="15">World!</tspan>
        </text>
      </svg>
    </section>
  </body>
</html>
```

### Canvas

The basics steps to use the Canvas API are:

1. Create a `canvas` element.
1. Get a reference to the `canvas` DOM element.
1. Get a context object from the `canvas` DOM element
   by calling the {% aTargetBlank
   "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext",
   "getContext" %} method.
1. Call methods on the context object to specify styling,
   draw paths, and fill paths.
1. Call the context `beginPath` method to start a new path.
   This avoids drawing a line from the end of the previous path.

The following example demonstrates all the commonly used
`CanvasRenderingContext2D` methods.
It produces similar output to the SVG example above.
Note that the duck image is quite pixelated compared to the SVG version.

{% include "_canvas.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Canvas API Demo</title>
    <script>
      const TWO_PI = 2 * Math.PI;

      window.onload = () => {
        const canvas = document.getElementById('my-canvas');
        const ctx = canvas.getContext('2d');

        ctx.lineWidth = 8;

        // Circle
        // ctx.arc arguments are centerX, centerY, radius,
        // startAngle, endAngle, and counterClockwise (defaults to false).
        ctx.arc(30, 30, 25, 0, 2 * TWO_PI);
        ctx.fillStyle = 'pink';
        ctx.fill();
        ctx.strokeStyle = 'red';
        ctx.stroke();

        // Ellipse
        ctx.beginPath();
        // ctx.ellipse arguments are centerX, centerY, radiusX, radiusY,
        // rotation, startAngle, endAngle, and
        // counterClockwise (defaults to false).
        ctx.ellipse(120, 30, 50, 25, 0, 0, TWO_PI);
        ctx.fillStyle = 'lightgreen';
        ctx.fill();
        ctx.strokeStyle = 'green';
        ctx.stroke();

        // Image
        // ctx.drawImage arguments are image, x, y, width, and height.
        const image = new Image();
        // Wait for the image to load before
        // asking the canvas context to draw it.
        image.onload = () => {
          ctx.drawImage(image, 0, 60, 50, 50);
        };
        image.src = './duck.png';

        // Line
        ctx.beginPath();
        ctx.moveTo(60, 60);
        ctx.lineTo(170, 100);
        ctx.lineCap = 'round';
        ctx.strokeStyle = 'blue';
        ctx.stroke();

        // Path
        ctx.beginPath();
        ctx.moveTo(10, 120);
        ctx.lineTo(70, 160);
        ctx.lineTo(70, 120);
        ctx.lineCap = 'round';
        ctx.lineJoin = 'round';
        ctx.strokeStyle = 'orange';
        ctx.stroke();

        // Polygon
        ctx.beginPath();
        ctx.moveTo(90, 120);
        ctx.lineTo(160, 160);
        ctx.lineTo(160, 120);
        ctx.closePath();
        ctx.lineCap = 'round';
        ctx.lineJoin = 'round';
        ctx.lineWidth = 16; // Why must this be doubled?
        ctx.strokeStyle = 'purple';
        ctx.stroke();
        ctx.fillStyle = 'lightblue';
        ctx.fill();

        // Rectangle
        ctx.beginPath();
        ctx.fillStyle = 'lightblue';
        // ctx.fillRect arguments are image, x, y, width, and height.
        const args = [5, 170, 80, 50];
        ctx.fillRect(...args);
        ctx.strokeStyle = 'blue';
        ctx.lineWidth = 8;
        ctx.strokeRect(...args);
        // To get rounded corners, use the polygon approach above.

        // Text
        ctx.font = '18px sans-serif';
        ctx.lineWidth = 1;
        ctx.fillStyle = 'blue';
        const x = 95;
        const y = 180;
        ctx.fillText('Hello', x, y);
        ctx.fillText('World!', x, y + 15);
      };
    </script>
  </head>
  <body>
    <section id="svg-demo">
      <canvas id="my-canvas" width="180" height="230" />
    </section>
  </body>
</html>
```

## `Intl`

The ECMAScript Internationalization API defines the {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl",
"Intl" %} namespace, which defines constructor functions and
associated methods for "language sensitive string comparison,
number formatting, and date and time formatting."
A locale can optionally be passed to the constructor functions.
If no locale is specified, the browser default locale is used.

This section provides examples of the most commonly used features in this API.

### `Intl.Collator`

This {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Collator/Collator",
"constructor function" %} returns an object with a `compare` function that
is suitable for use as a comparator function for the `Array` `sort` method.
It performs language-sensitive string comparison.

Here is an example of using this constructor function:

```js
const names = ['Mark', 'Tami', 'Amanda', 'Jeremy', 'mark'];
const options = {caseFirst: 'upper'}; // sorts uppercase before lower
const collator = new Intl.Collator([], options);
names.sort(collator.compare);
console.log('names =', names);
// ["Amanda", "Jeremy", "Mark", "mark", "Tami"]
```

### `Intl.DateTimeFormat`

This {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/DateTimeFormat",
"constructor function" %} returns an object with a `format` method
that can be passed a `Date` object or a number of milliseconds since the epoch.
The constructor takes an options object which supports many options.
The most commonly used options are:

- `dateStyle`: values are `full`, `long`, `medium`, and `short` (default)
- `timeStyle`: values are `full`, `long`, `medium`, and `short` (no default value)
- `hour12`: values are `true` (default) and `false`

To output only the date, do not specify a value for the `timeStyle` option.
To output only the time, do not specify `dateStyle` and do specify `timeStyle`.
To output both, specify both `dateStyle` and `timeStyle`.

Here are some examples of using this constructor function.

```js
let formatter = new Intl.DateTimeFormat();
const date = new Date(1961, 3, 16, 14, 19, 37);
console.log(formatter.format(date)); // 4/16/1961

formatter = new Intl.DateTimeFormat(locales, {
  dateStyle: 'short',
  timeStyle: 'short'
});
console.log(formatter.format(date)); // 4/16/61 2:19 PM

const dateStyles = ['full', 'long', 'medium', 'short'];
for (const dateStyle of dateStyles) {
  formatter = new Intl.DateTimeFormat(locales, {dateStyle});
  console.log(formatter.format(date));
}
// full -> Sunday, April 16, 1961
// long -> April 16, 1961
// medium -> Apr 16, 1961
// short (default) -> 4/16/61

const timeStyles = ['full', 'long', 'medium', 'short'];
for (const timeStyle of timeStyles) {
  formatter = new Intl.DateTimeFormat(locales, {timeStyle});
  console.log(formatter.format(date));
}
// full -> 2:19:37 PM GMT-06:00
// long -> 2:19:37 PM GMT-6
// medium -> 2:19:37 PM
// short -> 2:19 PM
```

Custom formats can specify the parts that should be present and
their formatting, but not their order or the punctuation between them.
Implementations are only required to
support specific subsets of the following options.
Example values are provided for each.

- `weekday`: values are `long` (Thursday; default), `short` (Thu), and `narrow` (T)
- `year`: values are `numeric` (1961) and `2-digit` (61)
- `month`: values are `numeric` (2), `2-digit` (02), `long` (March),
  `short` (Mar), and `narrow` (M)
- `day`, `hour`, `minute`, and `second`: values are `numeric` (7) and `2-digit` (07)

Here is an example of defining and using a custom format.

```js
formatter = new Intl.DateTimeFormat(locales, {
  weekday: 'short',
  year: 'numeric',
  month: 'short',
  day: 'numeric'
});
console.log('custom =', formatter.format(date)); // Sun, Apr 16, 1961
```

### `Intl.DisplayNames`

This {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DisplayNames/DisplayNames",
"constructor function" %} returns an object with an `of` method.
One use is to pass a region identifier and obtain the region name.
For example:

```js
const displayNames = new Intl.DisplayNames('en', {type: 'region'});
const regions = ['AU', 'CA', 'FR', 'GB', 'US'];
for (const region of regions) {
  console.log(region, '=', displayNames.of(region));
}
// AU = Australia
// CA = Canada
// FR = France
// GB = United Kingdom
// US = United States
```

### `Intl.ListFormat`

This {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/ListFormat/ListFormat",
"constructor function" %} returns an object with a `format` method
that takes an array of strings and returns a string
that is a specific concatenation of the values.

Here are some examples of using this constructor function.

```js
const colors = ['red', 'green', 'blue'];
let formatter = new Intl.ListFormat('en', {
  style: 'long',
  type: 'conjunction' // "and" in English
});
console.log(formatter.format(colors)); // red, green, and blue
formatter = new Intl.ListFormat('en', {
  style: 'long',
  type: 'disjunction' // "or" in English
});
console.log(formatter.format(colors)); // red, green, or blue
```

### `Intl.NumberFormat`

This {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/NumberFormat/NumberFormat",
"constructor function" %} returns an object with a `format` method
that takes a number and returns a formatted string.
It supports many options. The most commonly used options are:

- `currency`: a currency identifier such as `USD` or `EUR`
- `currencyDisplay`: `symbol`, `narrowSymbol`, `code`, or `name`
- `notation`: `standard`, `scientific`, `engineering`, or `compact`
- `signDisplay`: `auto`, `never`, `always`, or `exceptZero`
- `style`: `decimal`, `currency`, `percent`, or `unit`
- `unit`: options include `day`, `degree`, `gigabyte`, `hour`, and `meter`
- `unitDisplay`: `long`, `short`, or `narrow`

Here are some examples of using this constructor function.

```js
formatter = new Intl.NumberFormat();
console.log(formatter.format(1234567)); // 1,234,567

formatter = new Intl.NumberFormat('en', {style: 'percent'});
console.log(formatter.format(0.19)); // 19%

const currencyDisplays = ['symbol', 'narrowSymbol', 'code', 'name'];
for (const currencyDisplay of currencyDisplays) {
  formatter = new Intl.NumberFormat('en', {
    style: 'currency',
    currency: 'USD',
    currencyDisplay
  });
  console.log(currencyDisplay, '=', formatter.format(1234.56));
}
// symbol = $1,234.56
// narrowSymbol = $1,234.56
// code = USD 1,234.56
// name = 1,234.56 US dollars

const units = ['day', 'degree', 'gigabyte', 'hour', 'meter'];
const unitDisplays = ['long', 'short', 'narrow'];
for (const unit of units) {
  for (const unitDisplay of unitDisplays) {
    formatter = new Intl.NumberFormat('en', {
      style: 'unit',
      unit,
      unitDisplay
    });
    console.log(unit, unitDisplay, '=', formatter.format(1234));
  }
}
// day long = 1,234 days
// day short = 1,234 days
// day narrow = 1,234d
// degree long = 1,234 degrees
// degree short = 1,234 deg
// degree narrow = 1,234°
// gigabyte long = 1,234 gigabytes
// gigabyte short = 1,234 GB
// gigabyte narrow = 1,234GB
// hour long = 1,234 hours
// hour short = 1,234 hr
// hour narrow = 1,234h
// meter long = 1,234 meters
// meter short = 1,234 m
// meter narrow = 1,234m
```

### `Intl.RelativeTimeFormat`

This {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat/RelativeTimeFormat",
"constructor function" %} returns an object with a `format` method
that takes a numeric value and a unit name such as `day` or `month`.
The most commonly used options are:

- `numeric`: `always` (default) or `auto` (seems best)
- `style`: `long` (default), `short`, or `narrow`

Here are some examples of using this constructor function.

```js
const formatter = new Intl.RelativeTimeFormat('en', {numeric: 'auto'});
const values = [-2, -1, 0, 1, 2];

for (const value of values) {
  console.log(value, '=', formatter.format(value, 'day'));
}
// -2 = 2 days ago
// -1 = yesterday
// 0 = today
// 1 = tomorrow
// 2 = in 2 days

for (const value of values) {
  console.log(value, '=', formatter.format(value, 'month'));
}
// -2 = 2 months ago
// -1 = last month
// 0 = this month
// 1 = next month
// 2 = in 2 months
```

## Node

### npm scripts

"npm scripts" are custom command strings
that are defined in `package.json` files.
Nearly all JavaScript-based web development projects use these.
To add one, edit a `package.json` file and find or add a
top-level `scripts` property whose value is a JSON object.
The keys of this object are script names,
and the values are the corresponding command strings.
For example, the following npm scripts run the Prettier and ESLint tools
on all the applicable files in a Svelte project.

```json
    "format": "prettier --write '{public,src}/**/*.{css,html,js,svelte,ts}'",
    "lint": "eslint --fix --quiet src --ext .js,.svelte,.ts",
```

To execute these scripts use the `npm run` command followed by a script name.
This is such a frequent activity that it is
useful to define the alias `nr` to do this.
The details on creating this alias differ based on the shell being used.
Some examples include:

- bash: `alias nr="npm run"
- zsh: same as bash
- fish: `abbr --add nr npm run`
- nushell: `alias nr = npm run`

## Browsers

It is useful to create bookmarks in the bookmark bar
of the web browsers you use to `http://localhost:{port-number}`
for ports that are commonly used by web frameworks.
Common ports include 3000, 5000, and 8080.
These make it easy to test web applications that are
being developed and run locally.
I learned this from
{% aTargetBlank "http://www.billodom.com", "Bill Odom" %}.

## Editors

### Emmet

Emmet is an editor plugin for quickly entering HTML, XML, and CSS.
It also supports many "actions" that operate on HTML and XML elements.
The most commonly used action is to expand an abbreviation or snippet.
Emmet is built into the VS Code editor and
can be installed in most other editors and IDEs.

For example, to create a `div` element with CSS class of `pizza`,
enter `.pizza` and press the tab or return key.
This expands to `<div class="pizza"></div>` with the cursor
before the end tag ready for you to enter content.

Here are examples of snippets for generating HTML:

| Syntax                             | Expands to                                |
| ---------------------------------- | ----------------------------------------- |
| `!`                                | HTML template including `head` and `body` |
| `.foo`                             | `<div class="foo"></div>`                 |
| `.foo.bar`                         | `<div class="foo bar"></div>`             |
| `#foo`                             | `<div id="foo"></div>`                    |
| `p.foo`                            | `<p class="foo"></p>`                     |
| `p.foo.bar`                        | `<p class="foo bar"></p>`                 |
| `p#foo`                            | `<p id="foo"></p>`                        |
| `ul>li`                            | `<ul><li></li></ul>`                      |
| `p{Hello, World!}`                 | `<p>Hello, World!</p>`                    |
| `a:link`                           | `<a href="http://"></a>`                  |
| `c`                                | `<!-- -->`                                |
| `img`                              | `<img src="" alt="">`                     |
| `img[alt="dog" src="whippet.png"]` | `<img src="whippet.png" alt="dog">`       |
| `input:email`                      | `<input type="email" name="" id="">`      |
| `link`                             | `<link rel="stylesheet" href="">`         |

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
| `jcsb` | `justify-content: space-between;` |
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

Frequently, when starting a server locally to test a web app,
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
  echo usage: klp {port}
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

Here is a version of this script for Windows.
Place this in a file named `klp.bat` that is in
a directory listed in your `PATH` environment variable.

{% raw %}

```bash
@echo off

rem %1 is the first command line argument including quotes.
rem %~1 is the same without the quotes.
set port=%~1
if "%port%" == "" goto noport

rem The /f flag is for reading a file a line at a time.
rem In this case the output of the netstat command is treated as a file.
rem The fifth token in the netstat output is the process id.
rem This is placed in the variable %%p whose name must be a single letter.
for /f "tokens=5" %%p in ('netstat -anop tcp ^| findstr /r :%port%.*LISTENING') do (
  taskkill /f /pid %%p
  goto end rem Exit if a match is found.
)

echo no process is listening on port %port%
goto end

:noport
  echo usage: klp {port}

:end
```

{% endraw %}

## Wrap up

That's it for now!
I will be continually refining and adding web development tips here,
so check back occasionally for new content.

If you have ideas for additional tips that should be included
or corrections to what is already here, please
<a href="mailto:r.mark.volkmann@gmail.com?subject=Web Dev Tips suggestion">
email</a> them to me.

{% include "_intersection-observer-headings.html" %}
