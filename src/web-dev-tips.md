---
eleventyNavigation:
  key: Web Development Tips
layout: topic-layout.njk
---

This is a collection of tips related to web development
divided into categories.
It assumes that you already know HTML, CSS, and JavaScript to some extent,
but perhaps have not encountered all the tips shared here.
All of the examples use vanilla JavaScript
to avoid appealing only to users of specific web frameworks.

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

HTML5 added many values for the `input` element `type` attribute.
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

### input and datalist

An `input` element can have an associated `datalist`.
This causes the `input` to act like an "auto-complete".
New values can be added to the `datalist`
and existing values can be deleted.
This will change the options displayed in the `input`.

Here is an example where the user can select a color.
Initially the only values in the `datalist` are blue, green, and red.
But the user can add more colors.
This uses Svelte, but the concepts
should easily translate to other web frameworks and
you shouldn't need to know much about Svelte to understand this.

In this screenshot the user has already added the color "orange"
and has enter "n" into the "Color" input
which causes the matching values "green" and "orange" to be suggested.

{% include "_datalist.html" %}

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
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

        // Enable the "Add Color" button only if a new color has been entered.
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

## SVG

SVG is a markup language for drawing and rendering text and images.
The basics steps to use SVG are:

- Start with `<svg viewBox="0 0 maxX maxY" xmlns="http://www.w3.org/2000/svg">`.
  The `viewBox` defines the coordinate system.
  Replace `maxX` and `maxY` with the maximum values in those dimensions.
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
- Add a CSS rule for the `svg` element
  that defines its actual `width` and `height`.

The color of lines, paths, and shapes can be specified
using the CSS `stroke`, `stroke-width`, and `fill` properties.

The following example demonstrates all the commonly used SVG elements.

{% include "_svg.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
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

### CSS resets

A CSS reset is a set of CSS rules that attempt to
set the CSS properties of standard HTML elements
so they render the same across all the popular web browsers.
There are many such resets available.
The most popular is {% aTargetBlank
"https://necolas.github.io/normalize.css/", "normalize.css" %}.

To use normalize.css:

1. Download it from the URL above.
2. Add the following in the `head` element of HTML files:

```html
<link rel="stylesheet" href="normalize.css" />
```

### CSS box model

The CSS box model defines how padding, border, and margin are added to elements.
Padding is outside the content and inside the border.
The border is optional.
Margin is outside the border.

<img alt="CSS box model" style="width: 50%"
  src="/blog/assets/css-box-model.png?v={{pkg.version}}"
  title="CSS box model">

Padding, border, and margin can be specified to be the same on all four sides
or be different on each side.

When an element has a specified `width` and `height`,
by default those apply to the content and
do not include the `padding`, `border`, and `margin`.
This is because the CSS `box-sizing` property defaults to `content-box`.
If `box-sizing` is set to `border-box` then the `width` and `height`
include the content, `padding`, and `border`, but not the `margin`.
There are no other supported values for the `box-sizing` property.

### CSS specificity

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
Scores represented by a list of four numbers
are explained after this example.

```css
/* score is 0,1,0,0 */
#me {
  color: orange;
}

/* score is 0,0,2,0 */
.parent > .child {
  color: yellow;
}

/* This has the same specificity as the rule before: 0,0,2,0.
   The last one wins. */
.parent .child {
  color: green;
}

/* score is 0,0,1,0 */
.child {
  color: blue;
}

/* This has the same specificity as the rule before: 0,0,1,0.
   It applies the color purple to the
   element with the class “parent”,
   but the previous rule applies the color blue
   to the element with the class “child”. */
.parent {
  color: purple;
}
```

The precedence order of these rules happens to be
the order in which they are listed here.
Using a `style` attribute on the inner `div` has the highest specificity.

There is a formula for computing the specificity score
of any CSS rule that results in a list of four numbers.
Considering the four numbers from left to right,

- The first is 1 for inline styles and 0 otherwise.
- The second is the number of `id` values in the selector.
- The third is the number of class names in the selector.
- The fourth is the number of element name references in the selector.

The selector with the highest combined number,
obtained by removing the commas and treating it as a single 4-digit number,
wins. For example, treat a score of 1,2,3,4 as 1234.

This means that inline styling specified with a
`style` attribute on an HTML element always wins.
After this, `id` attributes are more important than class names,
which are more important than element names.

It also means that the order in which `id` values,
class names, and element names appear in a selector
does not affect its specificity calculation.

Here are some examples of applying this scoring:

- The CSS selector `.parent > #me` has a score of 0,1,1,0.
- The CSS selector `.parent #me` has the same score.
- The CSS selector `.parent .child` has a score of 0,0,2,0
  and so has a lower score than selectors that use an `id`.

For more details on CSS specificity, see
{% aTargetBlank "https://css-tricks.com/specifics-on-css-specificity/",
"Specifics on CSS Specificity" %} on the CSS-Tricks site.

### Centering

There are many ways to center content using CSS.
Four common ways are:

- `text-align` property
- `margin` property set to `auto` when `display` is `block`
- using flex layout
- absolute positioning using translate

Each of these approaches are demonstrated in the code below.

{% include "_centering.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
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
        position: relative; /* absolute positioning of .box3 is relative to this */
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
      <div class="box box3">Box #3</div>
    </section>
  </body>
</html>
```

### Variables

CSS variables (a.k.a custom properties) are useful for
storing and referring to values that are used in multiple places.
Changing the value of a variable updates all the places where it is used.
Common uses include storing the values of colors and sizes.

Here are examples of the syntax for defining and referring to a CSS variable:

```css
:root {
  /* This matches the root element "html". */
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
  The values are:

  - `flex-start` (default) pushes children toward the start
  - `flex-end` pushes children toward the end
  - `center` centers children
  - `space-between` spreads children with
    no space before the first or after the last
  - `space-evenly` spreads children with
    equal space before the first and after the last
  - `space-around` spreads children with
    half space before the first and after the last

- `align-items`

  This controls layout on the minor axis.
  The most commonly used values are `stretch` (default),
  `flex-start` (pushes children toward start),
  `flex-end` (pushes children toward end), and `center`.

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

{% include "_flex-layout.html" %}

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
    'nav main'
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

In the example below grid layout is used to layout
a header, footer, left nav and main area of a page.

The table below demonstrates combinations of
`justify-content` and `align-items` values.
The `justify-content` values appear in the column headings
and the `align-items` values appear in the row headings.

<img alt="grid layout" style="width: 100%"
  src="/blog/assets/css-grid-layout.png?v={{pkg.version}}"
  title="grid layout">

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
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

### inline-block

The default value of the CSS `display` property for many HTML elements,
including `div`, is "block".
This prevents them from appearing on the same "row" as other elements,
unless flex or grid layout is specified for the parent element.
Another way to allow multiple block elements to appear on the same row
is to set the CSS `display` property to "inline-block".

### pointer-events

To enable clicking an element that is behind another in `z-index`,
set the `pointer-events` CSS property of the top element to `none`.
TODO: Need an example of when this is useful.

### appearance

To take control of how a form control is rendered
set the `appearance` CSS property to `none`.
TODO: Add example of a custom checkbox?

### Selectors

### Pseudo classes

### pseudo selectors

### Combinators

### CSS position

Elements whose CSS `position` property set to `absolute`
are positioned relative to the nearest ancestor element
that has its CSS `position` property set to `relative`.
If none is found then it is relative to the browser window.
Positioning is specified using the CSS properties
`top`, `bottom`, `left` and `right`.

### CSS transitions

A CSS transition causes a change to specific CSS properties
to be applied over a given time duration.
To demonstrate this we will implement a toggle component
that is an oval containing a circle that represents a "thumb".
Clicking anywhere in the oval causes the thumb to move left or right
indicating some application-specific option being disable or enabled.
The CSS property `left` has a transition duration of 0.3 seconds
so any change to that property takes place gradually over that duration.

{% include "_css-transition.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
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

### CSS transforms

CSS transforms translate, rotate, scale, and skew DOM elements.

The example below renders a button containing a finger pointing emoji.
When the button is pressed, a `rotate` `transform` is applied
to rotate the emoji 180 degrees.
When pressed again, the original rotation of zero degrees is restored.

```html
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
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

The next example renders a Pokemon card which is initially face-down.
Clicking the card flips it over.
It uses a CSS transform to provide a nice 3D effect.

This works in Chrome, Firefox, and Safari,
but flashes a bit in Safari making the effect feel less polished.

{% include "_css-card-flip.html" %}

```html
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>

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
    <p>Click card to flip it over.</p>
    <div class="card">
      <div class="card-container">
        <img alt="card back" src="pokemon-back.png" />
        <img alt="card front" src="pokemon-front.png" class="card-front" />
      </div>
    </div>
  </body>
</html>
```

### Media queries

CSS media queries have many uses, but the most common is to
apply different CSS properties based on the window width.

### window.matchMedia

The CSS Object Model (CSSOM) `window.matchMedia` function
can be used to support light and dark modes.
For example, the following line of JavaScript code
determines if the user has configured their operating system
to prefer dark mode:

```js
const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
```

This can then be used to add a CSS class name such as "dark-mode"
to the body element.

CSS for the `body` element can define CSS variables
that specify colors to be used in light mode.
A separate rule with the selector `body.dark-mode` can override the
values of those CSS variables with the values to be used in dark mode.

### Viewport units

100% versus 100 VH or 100 VW

### Fixed vs sticky position

position fixed and position sticky
Show example of a table where thead is sticky.

## JavaScript

### Embrace booleans

```js
// Bad
if (total >= 100) {
  return true;
} else {
  return false;
}

// Better
return total >= 100 ? true : false;

// Best
return total >= 100;
```

### Nested ternaries

A common opinion is that nested ternaries are hard to read.
But are they? Consider this example.

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
// It's not confusing when formatted nicely.
let assessment =
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

- `forEach` iterates of the elements.
  It is useful in cases where it is desirable to
  call a named function on each element.
  In other cases it can be more clear to use a `for of` loop.

  ```js
  function report(dog) {
    console.log(`${dog.name} is a ${dog.breed}.`);
  }

  dogs.forEach(dog => report(dog));
  dogs.forEach(report); // same as previous line

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

- `filter` creates a new array containing a subset of the elements.

  ```js
  const bigDogs = dogs.filter(dog => dog.weight &gt;= 70);
  // [object for Ramsay, object for Oscar]
  ```

- `map` creates a new array with the same length.

  ```js
  const dogNames = dogs.map(dog => dog.name);
  // ['Maisey', 'Ramsay', 'Oscar', 'Comet']
  ```

- `reduce` creates a single value from the elements and an initial value.

  ```js
  // Here the initial value is zero.
  const totalWeight = dogs.reduce((acc, dog) => acc + dog.weight, 0);

  // Create an object where the keys are dog names
  // and the values are the dog objects.
  // This doesn't gracefully handle having multiple dogs with the same name.
  // Here the initial value is an empty object.
  const nameToDogMap = dogs.reduce((acc, dog) => {
    acc[dog.name] = dog;
    return acc;
  }, {});
  ```

- `some` determines if some element meets specified criteria.

  ```js
  const haveWhippet = dogs.some(dog => dog.breed === 'Whippet'); // true
  ```

- `every` determines if every element meets specified criteria.

  ```js
  const allWhippets = dogs.every(dog => dog.breed === 'Whippet'); // false
  ```

- `find` finds the first element that meets specified criteria.

  ```js
  const firstBigDog = dogs.find(dog => dog.weight &gt;= 70);
  // object for Ramsay
  ```

- `findIndex` finds the index of the first element
  that meets specified criteria.

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

  ```js
  // Sort dogs by weight in ascending order.
  dogs.sort((d1, d2) => d1.weight - d2.weight);

  // Sort dogs by weight in descending order.
  dogs.sort((d1, d2) => d2.weight - d1.weight);

  // Sort dogs by name.
  dogs.sort((d1, d2) => d1.name.localeCompare(d2.name));
  ```

- `push` adds any number of values to the end.

  ```js
  dogs.push(snoopy); // Adds Snoopy at the end.
  ```

- `pop` removes the last element.

  ```js
  dogs.pop(snoopy); // Removes Snoopy from the end.
  ```

- `unshift` adds any number of values to the beginning.

  ```js
  dogs.unshift(snoopy); // Adds Snoopy at the beginning.
  ```

- `shift` removes the first element.

  ```js
  dogs.shift(snoopy); // Removes Snoopy from the beginning.
  ```

### Modifying CSS properties from JavaScript

HTML elements are represented by Document Object Model (DOM) objects in memory.
DOM objects have a style property whose value is an object.
The keys of this object are camelCased CSS property names
and the values the values of those CSS properties.
New properties can be added to the style objects
and existing ones can be modified or deleted.

In the example below the CSS variable `--color` is initially set to "red"
and used to set the CSS `color` property of `p` elements.
When the "Toggle Color" button is pressed a function is called that
gets the current value of the CSS variable
and modifies it based on its current value.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
  </head>
  <body>
    <p id="greeting">Hello, World!</p>
    <button id="toggle-btn">Toggle Color</button>

    <!--
    Placing the script tag here causes it to
    run after the HTML above is rendered.
    Another option is to move the script tag to the head section and
    wrap the code in a function that is assigned to window.onload.
    -->
    <script>
      const greeting = document.getElementById('greeting');
      const toggleBtn = document.getElementById('toggle-btn');
      toggleBtn.onclick = () => {
        // This is initially unset, not populated from the style tag.
        const {color} = greeting.style;
        greeting.style.color = color === 'blue' ? 'red' : 'blue';
      };
    </script>
  </body>
</html>
```

### CSS variables from JavaScript

JavaScript code can access the values of CSS variables using a
combination of the `getComputedStyle` and `getPropertyValue` methods.
JavaScript code can also modify the values of CSS variables
using the `setProperty` method.
This updates the use of all CSS properties that reference them.

In the example below the CSS variable `--color` is initially set to "red"
and used to set the CSS `color` property of `p` elements.
When the "Toggle Color" button is pressed a function is called that
gets the current value of the CSS variable
and modifies it based on its current value.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>
    <style>
      p {
        --color: red;
        color: var(--color);
      }
    </style>
  </head>
  <body>
    <p id="greeting">Hello, World!</p>
    <button id="toggle-btn">Toggle Color</button>

    <script>
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
    </script>
  </body>
</html>
```

### Getting element size

DOM elements support the method `getBoundingClientRect`
that returns a `DOMRect` object.
This object contains the properties `width` and `height`
which provide the size of the element.
It also contains the following properties that describe its position:
`x`, `y`, `left`, `top`, `right`, `bottom`.

In the example below a div with many CSS properties is rendered.
When the "Query" button is pressed a function is called that
gets the bounding rectangle of the div and outputs its properties.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Demo</title>

    <style>
      #box {
        position: absolute;
        left: 100px;
        top: 100px;
        height: 200px;
        width: 300px;
        border: 1px solid red;
      }
    </style>
  </head>
  <body>
    <div id="box">I'm a box.</div>
    <button id="query-btn">Query</button>

    <script>
      const box = document.getElementById('box');
      const queryBtn = document.getElementById('query-btn');
      queryBtn.onclick = () => {
        const rect = box.getBoundingClientRect();
        console.log('get-rect.html: rect =', rect);
        /*
        bottom: 302 - includes border
        height: 202 - includes border
        left: 100
        right: 402 - includes border
        top: 100
        width: 302 - includes border
        x: 100
        y: 100
        */
      };
    </script>
  </body>
</html>
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

Here is a version of this script for Windows.
It lacks error handling.
TODO: Add the missing error handling to match the bash script.

{% raw %}

```bash
@echo off
for /f "tokens=5" %%p in ('netstat -anop tcp ^| findstr /r :%1.*LISTENING') do (
  taskkill /f /pid %%p
)
```

{% endraw %}
