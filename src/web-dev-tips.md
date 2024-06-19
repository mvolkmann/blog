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

### Accessible Rich Internet Applications (ARIA)

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA", "ARIA" %}
defines a set of HTML attributes that make certain HTML elements
more accessible to people with disabilities.
This is a large topic and only the surface is scratched here.

Sometimes `button` elements do not contain text that makes their purpose clear.
They may only include an emoji, `img` or `svg` element.
In this case the `aria-label` attribute should be added
so devices such as screen readers can describe the purpose of the button.
For example:

```html
<button aria-label="Fire the missiles">🚀</button>
```

The table below summarizes the ARIA attributes.
For more detail, see the {% aTargetBlank
"https://www.w3.org/TR/wai-aria-1.0/states_and_properties", "W3C documentation" %}.

TODO: Are these only needed when using HTML elements in a non-standard way?
For example, is `role="button"` only needed on non-button elements (like a `div`)?
It seems like we should just always using the `button` element for buttons.
Is `aria-valuemax` only recommended when creating a custom "range widget"
rather than using something like `<input type="range" max="50" />`.

| ARIA Attribute          | Description                                                                                                                                                                                    |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `aria-activedescendant` | identifies the currently active descendant of a composite widget                                                                                                                               |
| `aria-atomic`           | indicates whether assistive technologies will present all, or only parts of, the changed region based on the change notifications defined by `aria-relevant`                                   |
| `aria-autocomplete`     | indicates whether input completion suggestions are provided                                                                                                                                    |
| `aria-busy`             | indicates whether an element, and its subtree, are currently being updated                                                                                                                     |
| `aria-checked`          | indicates the current "checked" state of checkboxes, radio buttons, and other widgets                                                                                                          |
| `aria-controls`         | identifies the elements whose contents or presence are controlled by the current element                                                                                                       |
| `aria-describedby`      | identifies the elements that describe the object                                                                                                                                               |
| `aria-disabled`         | indicates that the element is perceivable but disabled, so it is not editable or otherwise operable                                                                                            |
| `aria-dropeffect`       | indicates what functions can be performed when the dragged object is released on the drop target                                                                                               |
| `aria-expanded`         | indicates whether the element, or another grouping element it controls, is currently expanded or collapsed                                                                                     |
| `aria-flowto`           | identifies the next elements in an alternate reading order which allows assistive technology to override the default reading order                                                             |
| `aria-grabbed`          | indicates an element's "grabbed" state in a drag-and-drop operation                                                                                                                            |
| `aria-haspopup`         | indicates that the element has a popup context menu or sub-level menu                                                                                                                          |
| `aria-hidden`           | indicates that the element and all of its descendants are not visible or perceivable to any user as implemented by the author                                                                  |
| `aria-invalid`          | indicates the entered value does not conform to the format expected by the application                                                                                                         |
| `aria-label`            | defines a string value that labels the current element                                                                                                                                         |
| `aria-labelledby`       | identifies the elements that label the current element                                                                                                                                         |
| `aria-level`            | defines the hierarchical level of an element within a structure                                                                                                                                |
| `aria-live`             | indicates that an element will be updated, and describes the types of updates the user agents, assistive technologies, and user can expect                                                     |
| `aria-multiline`        | indicates whether a text box accepts multiple lines of input or only a single line                                                                                                             |
| `aria-multiselectable`  | indicates that the user may select more than one item from the current selectable descendants                                                                                                  |
| `aria-orientation`      | indicates whether the element orientation is horizontal or vertical                                                                                                                            |
| `aria-owns`             | identifies elements in order to define a visual, functional, or contextual parent/child relationship between DOM elements where the DOM hierarchy cannot be used to represent the relationship |
| `aria-posinset`         | defines an element's position in the current set of listitems or treeitems (not required if all elements in the set are present in the DOM)                                                    |
| `aria-pressed`          | indicates the current "pressed" state of toggle buttons                                                                                                                                        |
| `aria-readonly`         | indicates that the element is not editable, but is otherwise operable                                                                                                                          |
| `aria-relevant`         | indicates what user agent change notifications (additions, removals, etc.) assistive technologies will receive within a live region                                                            |
| `aria-required`         | indicates that user input is required on the element before a form may be submitted                                                                                                            |
| `aria-selected`         | indicates the current "selected" state of various widgets                                                                                                                                      |
| `aria-setsize`          | defines the number of items in the current set of listitems or treeitems (not required if all elements in the set are present in the DOM)                                                      |
| `aria-sort`             | indicates if items in a table or grid are sorted in ascending or descending order                                                                                                              |
| `aria-valuemax`         | defines the maximum allowed value for a range widget                                                                                                                                           |
| `aria-valuemin`         | defines the minimum allowed value for a range widget                                                                                                                                           |
| `aria-valuenow`         | defines the current value for a range widget                                                                                                                                                   |
| `aria-valuetext`        | defines the human readable text alternative of aria-valuenow for a range widget                                                                                                                |

In addition to these, a `role` attribute can be added to an HTML element
with one of the values described at {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques",
"Using ARIA: Roles, states, and properties" %}.

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

### Dialogs

See <a href="/blog/html-dialog-element/" target="_blank">HTML dialog Element</a>

### Popovers

An HTML `button` element can trigger showing another element in a popup.
The popup is dismissed by clicking outside it or by pressing the escape key.
As shown here, you can also add a close button.

<img alt="HTML popover" style="width: 70%"
  src="/blog/assets/html-popover.png?v={{pkg.version}}"
  title="HTML popover">

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>HTML Popover Demo</title>
    <style>
      button {
        background-color: cornflowerblue;
        border: 1px solid gray;
        border-radius: 0.5rem;
        padding: 0.5rem;
      }

      .popover {
        border: none;
        font-family: sans-serif;
        position: relative;
        padding: 1rem;
        padding-top: 2.5rem;

        & .close {
          position: absolute;
          top: 0px;
          right: 0px;

          background: none;
          border: none;
          font-size: 2rem;
          line-height: 2rem;
          margin-right: 0.5rem;
          padding: 0;
          z-index: 1;
        }

        & .close:hover {
          cursor: pointer;
        }

        &::backdrop {
          background: rgb(0 0 0 / 50%);
        }
      }
    </style>
  </head>
  <body>
    <button popovertarget="my-popover">Toggle Popover</button>
    <div class="popover" id="my-popover" popover>
      <!-- Values for popovertargetaction are "hide", "show", and "toggle". -->
      <button
        class="close"
        popovertarget="my-popover"
        popovertargetaction="hide"
      >
        ×
      </button>
      Popover content goes here.
    </div>
  </body>
</html>
```

### figure element

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure",
"figure element" %} associates a caption with any content.
Typically the content is a single `img` element,
but it can be any content such as a `code` or `picture` element.
The caption is specified with a `figcaption` element.
Typically there is only one and it appears
either before or after all the content.

The following example adds a caption to an image:

{% include "_figure.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Figure Demo</title>
    <style>
      img {
        height: 200px;
      }
    </style>
  </head>
  <body>
    <figure>
      <img src="./comet.jpg" alt="Comet" />
      <figcaption>Comet, the Whippet</figcaption>
    </figure>
  </body>
</html>
```

### meter element

The `meter` element represents a measurement within a known range
using a filled bar.
It supports the following attributes:

- `min`: defaults to 0
- `max`: defaults to 1
- `low`: defaults to `min`
- `high`: defaults to `max`
- `optimum`: number between `min` and `max`
- `value`: number between `min` and `max`

By default, the bar will be orange if the value is
less than `low` or greater than `high`.
Otherwise it will be green.
The `optimum` value seems to have no effect.

For example:

```html
<label for="temperature">Temperature:</label>
<meter
  id="temperature"
  min="0"
  max="100"
  low="20"
  high="70"
  optimum="60"
  value="55"
>
  55
</meter>
```

### Lazy Loading

To avoid loading `img` and `iframe` elements that are not scrolled into view,
add the `loading="lazy"` attribute.
As of July 2021 this is not yet supported in Safari
and Firefox does not support this on `iframe` elements.

Browsers can implement this to begin loading
just before the associated elements come in to view.
They can even take network speed into consideration in order to
load the elements even sooner when a slow connection is detected.

It is recommended to only add this attribute to `img` and `iframe` elements
that do not require scrolling after the initial page load
in order to view them.

## CSS

### CSS property sources

CSS property values come from the following four sources:

1. CSS specification defaults
1. Browser (user agent) defaults
1. User stylesheets (not widely supported)
1. Author stylesheets (site-specific)

User stylesheets allow users to apply custom styling to any site they browse.
They are supported in Safari and IE, but not in Chrome or Edge.
In Firefox they are not supported by default, but support can be enabled
by setting `toolkit.legacyUserProfileCustomizations.stylesheets` to `true`.
To enable user stylesheets in Safari, select Preferences ... Advanced,
click the drop-down labelled "Style sheet", and select a CSS file.

For example, to hide the Twitter left and right sidebars in Safari,
create a `.css` file containing the following,
make it the user stylesheet, and restart Safari.

```css
/* Twitter left sidebar */
#react-root header[role='banner'] {
  /* !important is needed here because
     the author stylesheet sets display to flex. */
  display: none !important;
}

/* Twitter right sidebar */
[data-testid='sidebarColumn'] {
  /* !important is needed here because
     the author stylesheet sets display to flex. */
  display: none !important;
}
```

An issue with user stylesheets is that they do not target specific web sites.
They affect any visited site containing elements
that are targeted by the CSS rule selectors.

The precedence order from lowest to highest
of CSS rules found in the sources above is:

1. CSS specification
1. Browser (user agent)
1. User
1. Author
1. animations (using `@keyframes`)
1. Author with `!important`
1. User with `!important`
1. Browser (user agent) `!important`
1. transitions

Note how the order of sources with `!important` is
opposite from the order of sources without it.

### Inheritable CSS properties

The value of some CSS properties for an element
is inherited from its parent element by default.
It would be undesirable this to be the case for all properties.
For example, if `border` was inherited by default,
adding a border to an element would add
the same border to all of its descendant elements.

The following CSS properties are inheritable by default:

- `border-collapse`
- `border-spacing`
- `caption-side`
- `color`
- `cursor`
- `direction`
- `empty-cells`
- `font-family`
- `font-size`
- `font-style`
- `font-variant`
- `font-weight`
- `font-size-adjust`
- `font-stretch`
- `font`
- `letter-spacing`
- `line-height`
- `list-style-image`
- `list-style-position`
- `list-style-type`
- `list-style`
- `orphans`
- `quotes`
- `tab-size`
- `text-align`
- `text-align-last`
- `text-decoration-color`
- `text-indent`
- `text-justify`
- `text-shadow`
- `text-transform`
- `visibility`
- `white-space`
- `widows`
- `word-break`
- `word-spacing`
- `word-wrap`

### CSS global values

In the CSS specification, all CSS properties support the global values
`inherit`, `initial`, `unset`, and `revert`.
All modern browsers support these, but IE only supports `inherit`.

| CSS Global Value | Description                                                                             |
| ---------------- | --------------------------------------------------------------------------------------- |
| `inherit`        | inherits the computed value from the parent element                                     |
| `initial`        | uses the CSS specification default value                                                |
| `unset`          | same as `inherit` for inheritable properties and `initial` for others                   |
| `revert`         | same as `inherit` for inheritable properties and browser (user agent) values for others |

Specifying a value of `inherit` on a non-inheritable property
causes it to inherit from its parent element,
overriding the fact that it is not inheritable by default.

The CSS specification does not define a default value for all properties.
When `initial` is used as the value of a property
for which the CSS specification does not specify a default,
the browser default is used instead.

The following CSS rule resets all properties on all elements
to the CSS specification defaults:

```css
* {
  all: initial;
}
```

Specifying a value of `revert` causes a property to use the value
it would have if it wasn't set by an author stylesheet.
This means the value could come from the CSS specification,
a browser stylesheet, or a user stylesheet.

### Size units

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

- `ch` - width of "0" in current font
- `em` - parent font size in `font-size` property; current font size in other properties
- `ex` - height of "x" in current font
- `lh` - line height of current element
- `rem` - root element (`html`) font size; stands for "root em"
- `vh` - 1% of viewport height
- `vmin` - 1% of smallest viewport dimension
- `vmax` - 1% of largest viewport dimension
- `vw` - 1% of viewport width

Of these, the mostly commonly used are `rem`, `vh`, and `vw`.

An important benefit of using `rem` as the unit for nearly all sizes
is that it enables scaling everything in the web site
by simply changing the font size of the `html` element,
which defaults to `16px`.
A web app can allow each user to modify this size,
perhaps saving their preference in `localStorage`.

When the value is zero, a unit is not required.
CSS linters encourage not supplying a value in that case.
For example, `0` is preferred over `0px`.

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
Notice the right borders and the use of `box-sizing: border-box;`
which is explained later.

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

```text
var(--some-var-name)
```

To add a default value to be used if the variable is not defined,
use the following syntax:

```text
var(--some-var-name, default-value)
```

The default value can be specified with another `var`.
For example, the following uses `--border-color` if it is defined.
Otherwise it will be `--accent-color` if it is defined
or `black` if neither are defined.

```text
var(--border-color, var(--accent-color, black));
```

The following example defines and uses several CSS variables.

```css
/* This matches the root element "html". */
/* All other CSS rules can use variables defined here. */
:root {
  --accent-color: blue;
  --border-color: red;
  --primary-color: cornflowerblue;
}

p {
  color: var(--primary-color);
}

.demo {
  display: inline-block;
  border: 5px solid var(--border-color, var(--accent-color, black));
  padding: 1rem;
}

.circle {
  /* This variable can only be used in this rule
     and in rules that match descendant elements */
  --size: 4rem;

  border: 1px solid var(--primary-color);
  border-radius: calc(var(--size) / 2); /* 50% is preferred */
  height: var(--size);
  width: var(--size);
}
```

Later we will see how to get and set
the values of CSS variables in JavaScript code.

I have still have some questions.

Why can't we reference CSS variables with just their name
instead of using the `var` function?
I understand that has the advantage of being able to specify a value to use
if the variable is not defined, but often we know it will be defined.

Why did they choose to start variable names with `--` instead of `$`
like Sass variables? That is one character shorter
and was already a familiar convention.

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

Many CSS properties that can effect the four sizes of the box differently
have a shorthand version that supports
specifying a different value for each side.
The values are listed in clockwise order starting with the top.
Also, the mnemonic "TRouBLe" serves as a reminder
that the order is top, right, bottom, and left.
When only two values are specified,
the first affects the top and bottom (vertical)
and the second affects the left and right (horizontal).

### `box-sizing` property

When an element has a specified `width` and `height`,
by default those apply to the content and
do not include the `padding`, `border`, and `margin`.
This is because the CSS `box-sizing` property defaults to `content-box`.

If `box-sizing` is set to `border-box`, the `width` and `height`
include the content, `padding`, and `border`, but not the `margin`.
Frequently this is the desired value.
There are no other supported values for the `box-sizing` property.

### `border-radius` property

The `border-radius` property changes the corners of an element.
to be curved instead of square.
Specify one value for circular corners (same for each corner),
two values for elliptical corners, or
four values for a different curve in each corner.
The possible values are quite complex.
See {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius", "MDN" %}
for details.

To change the shape of an element to an oval,
set `border-radius` to a large pixel value such as `999px`.
This actually uses a value that is half the length of the shortest side.

### `box-shadow` property

The `box-shadow` property adds shadows to
one or more sides of an element.
Like the `outline` property,
it does not add to the size of the element,
so it does not affect element layout.
Unlike the `outline` property, its width can be specified.

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
This is useful to render a "border" that does not
affect the size of the element as a real border does.

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
See the `.two-shadows`, `.target`, and .multiple-borders` selectors
below for examples.
An alternative to defining multiple, nested borders
is to use nested divs that each have a single border,
but that requires more HTML elements and
more CSS properties to center each of the divs.

The following examples demonstrate many uses of shadows.

{% include "_css-box-shadow.html" %}

{# pragma warning disable format #}

{% raw %}

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

      .bottom-border {
        box-shadow: 0 10px 0 red;
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

      .left-border {
        box-shadow: -10px 0 0 red;
      }

      .left-inset {
        box-shadow: 10px 0 10px inset var(--color);
      }

      .left-outset {
        box-shadow: -10px 0 10px var(--color);
      }

      .multiple-borders {
        box-shadow: 0 -10px 0 red, 10px 0 0 green, 0 10px 0 blue, -10px 0 0
            purple;
      }

      .nested {
        border: none;
        border-radius: 50%;
        box-shadow: 0 0 0 20px red, 0 0 0 40px green, 0 0 0 60px blue;
        margin: 4rem;
      }

      .right-border {
        box-shadow: 10px 0 0 red;
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

      .target {
        --red: #cb0000;
        --size: 2rem;

        height: var(--size);
        width: var(--size);

        background-color: var(--red);
        border: none;
        border-radius: 50%;
        box-shadow: 0 0 0 20px white, 0 0 0 40px var(--red);
        margin: 3rem;
      }

      .top-border {
        box-shadow: 0 -10px 0 red;
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

    <div class="row">
      <div class="box two-shadows">two shadows</div>
      <div class="box nested">nested shadows</div>
      <div class="target"></div>
    </div>

    <div class="row">
      <div class="box top-border">top border</div>
      <div class="box right-border">right border</div>
      <div class="box bottom-border">bottom border</div>
      <div class="box left-border">left border</div>
      <div class="box multiple-borders">multiple borders</div>
    </div>
  </body>
</html>
```

{% endraw %}

{# pragma warning enable format #}

### Gradients

CSS supports three kinds of gradients: linear, radial, and conic.
Each of these can occur one time over an area or repeat multiple times.
These are supported by the CSS functions
`linear-gradient`, `repeating-linear-gradient`,
`radial-gradient`, `repeating-radial-gradient`,
`conic-gradient`, and `repeating-conic-gradient`.
Each of these functions produce an image which is appropriate for
the value of the `background-image` property.
They cannot be used as the value of the `background-color` property.

The color keyword "transparent" can be used in gradients to allow
the `background-color` of an area to show through a portion of the gradient.

The code below demonstrates using each of these gradient functions.
See the comments in the code for details.

{% include "_gradients.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Gradients Demo</title>
    <style>
      .container {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
      }

      .container > div {
        --size: 5rem;

        display: inline-block;
        font-size: 2rem;
        height: var(--size);
        line-height: var(--size);
        margin-right: 0.5rem;
        text-align: center;
        width: var(--size);
      }

      p {
        font-weight: bold;
      }

      /* By default linear gradients go from top to bottom.
         Any number of colors through which to transition can be specified. */
      .lg1 {
        background-image: linear-gradient(red, yellow, blue);
      }

      /* The area filled with the gradient is not required to be rectangular.
         Here it is a circle. */
      .lg2 {
        background-image: linear-gradient(red, yellow, blue);
        border-radius: 50%;
      }

      /* Each color can optionally be followed by the percentage
         across the area at which the full color is reached.
         This is referred to as a "color stop".
         In the previous example, yellow is reached at 50%
         because it is the middle color.
         Here yellow is reached at 20%. */
      .lg3 {
        background-image: linear-gradient(red, yellow 20%, blue);
      }

      /* If a color is followed by two percentages, they indicate
         a range over which a solid color is displayed.
         These are also referred to as color stops.
         Here the first 20% is solid red, the last 20% is solid yellow,
         and the middle 60% transitions from red to yellow. */
      .lg4 {
        background-image: linear-gradient(red 0% 20%, yellow 80% 100%);
      }

      /* An optional first argument specifies the angle of the gradient
         which defaults to zero.
         It can be specified in degrees, radians, or turns.
         For example, 45deg, 0.785rad, and 0.125turn are equivalent */
      .lg5 {
        background-image: linear-gradient(45deg, red, yellow, blue);
      }

      /* The transition angle can also be specified with
         the "to" keyword followed by
         "left" or "right" and/or "top" or "bottom" in any order.
         Using "to right top" is the same as 45deg. */
      .lg6 {
        background-image: linear-gradient(to right top, red, yellow, blue);
      }

      /* Multiple, comma-separated gradients can be specified,
         but only the first will be visible unless
         the colors have less than full opacity.
         Here we have red with 80% opacity starting in the left bottom
         and moving toward the right top that changes to zero opacity
         70% across the area.
         We also have blue with 80% opacity starting in the right bottom
         and moving toward the left top that changes to zero opacity
         70% across the area.
         The colors combine in the middle to create shades of purple. */
      .lg7 {
        background-image: linear-gradient(
            to right top,
            rgba(255, 0, 0, 0.8),
            rgba(255, 0, 0, 0) 70%
          ), linear-gradient(to left top, rgba(0, 0, 255, 0.8), rgba(
                0,
                0,
                255,
                0
              ) 70%);
      }

      /* To repeat a linear gradient multiple times across an area,
         use the repeating-linear-gradient function.
         Each color can be followed by the length
         or percentage across the area at which it ends.
         The length or percentage of the entire gradient is specified
         by the length after the last color.
         Adding lengths after the other colors is optional
         and just specifies where they reach the full color.
         To repeat the gradient three times,
         add 33.33% after the last color.
         The angle of the gradient can be specified with the same
         optional first argument as in the linear-gradient function. */
      .lg8 {
        background-image: repeating-linear-gradient(red, yellow, blue 33.33%);
      }

      /* Stripes can be created by specifying two color stops for each color. */
      .lg9 {
        background-image: repeating-linear-gradient(
          0.25turn,
          red 0 10px,
          yellow 10px 20px,
          blue 20px 30px
        );
      }

      /* Radial gradients start in the center of an area by default
         and change color as they work outward. */
      .rg1 {
        background-image: radial-gradient(red, yellow, blue);
      }

      /* The area filled with the gradient is not required to be rectangular.
         Here it is a circle.
         A percentage less that 100% is specified after the last color
         because otherwise that color would only appear in the corners
         which are clipped by the border-radius. */
      .rg2 {
        background-image: radial-gradient(red, yellow, blue 75%);
        border-radius: 50%;
      }

      /* The gradient shape can be a circle (default) or ellipse.
         Note that an ellipse will be a circle if
         the height and width of the area are equal. */
      .rg3 {
        background-image: radial-gradient(ellipse, red, yellow, blue);
        width: 8rem;
      }

      /* To change the center position of a radial gradient,
         specify the shape (circle or ellipse) followed by the keyword "at",
         and the center location using the same values
         supported by the CSS background-position property. */
      .rg4 {
        background-image: radial-gradient(
          circle at left top,
          red,
          yellow,
          blue
        );
      }

      /* The distance from the center where the full ending color is reached
         can be specified with the keywords "closest-side", "closest-corner",
         "farthest-side" and "farthest-corner".
         The "closest" and "farthest" only differ when
         the center is not in the center of the area. */
      .rg5 {
        background-image: radial-gradient(
          circle closest-side at 25% 50%,
          red,
          yellow,
          blue
        );
      }
      .rg6 {
        background-image: radial-gradient(
          circle closest-corner at 25% 50%,
          red,
          yellow,
          blue
        );
      }
      .rg7 {
        background-image: radial-gradient(
          circle farthest-side at 25% 50%,
          red,
          yellow,
          blue
        );
      }
      .rg8 {
        background-image: radial-gradient(
          circle farthest-corner at 25% 50%,
          red,
          yellow,
          blue
        );
      }

      /* Color stops can be applied to radial-gradient colors
         in the same way as in linear-gradient colors. */

      /* To repeat a radial gradient multiple times across an area,
         use the repeating-radial-gradient function
         which is similar to repeating-linear-gradient. */
      .rg9 {
        background-image: repeating-radial-gradient(red, yellow, blue 33.33%);
      }

      /* Conic gradients transition colors through
         angles of rotation around a center point. */
      .cg1 {
        background-image: conic-gradient(red, yellow, blue);
      }

      /* The area filled with the gradient is not required to be rectangular.
         Here it is a circle. */
      .cg2 {
        background-image: conic-gradient(red, yellow, blue);
        border-radius: 50%;
      }

      /* The starting angle defaults to zero degrees which,
         unlike what you learned in Geometry class,
         is at the top of the circle.
         To change this, use the "from" keyword. */
      .cg3 {
        background-image: conic-gradient(from 0.25turn, red, yellow, blue);
      }

      /* The center of the gradient can be moved from the center of the area. */
      .cg4 {
        background-image: conic-gradient(at 35% 50%, red, yellow, blue);
      }

      /* Color stops are specified as angles or percentages, not distances.
         Here the first 90 degrees (or 0.25turn) transitions from red to yellow
         and the final 270 degrees (or 0.75turn) transitions from yellow to blue. */
      .cg5 {
        background-image: conic-gradient(red, yellow 90deg, blue);
      }

      /* Pairs of color stops produce solid colors. */
      .cg6 {
        background-image: conic-gradient(
          red 0 120deg,
          yellow 120deg 240deg,
          blue 240deg 360deg
        );
        border-radius: 50%;
      }

      /* To repeat a conic gradient multiple times around an area,
         use the repeating-conic-gradient function
         which is similar to repeating-linear-gradient.
         Here the gradient is repeated three times,
         each occupying 1/3 (33.33%) of a circle. */
      .cg7 {
        background-image: repeating-conic-gradient(red, yellow, blue 33.33%);
      }
    </style>
  </head>
  <body>
    <p>Linear Gradients</p>
    <div class="container">
      <div class="lg1">1</div>
      <div class="lg2">2</div>
      <div class="lg3">3</div>
      <div class="lg4">4</div>
      <div class="lg5">5</div>
      <div class="lg6">6</div>
      <div class="lg7">7</div>
      <div class="lg8">8</div>
      <div class="lg9">9</div>
    </div>

    <p>Radial Gradients</p>
    <div class="container">
      <div class="rg1">1</div>
      <div class="rg2">2</div>
      <div class="rg3">3</div>
      <div class="rg4">4</div>
      <div class="rg5">5</div>
      <div class="rg6">6</div>
      <div class="rg7">7</div>
      <div class="rg8">8</div>
      <div class="rg9">9</div>
    </div>

    <p>Conic Gradients</p>
    <div class="container">
      <div class="cg1">1</div>
      <div class="cg2">2</div>
      <div class="cg3">3</div>
      <div class="cg4">4</div>
      <div class="cg5">5</div>
      <div class="cg6">6</div>
      <div class="cg7">7</div>
    </div>
  </body>
</html>
```

### Gradient Borders

It's easy to create gradient borders using `border-image-source`.
For example:

<img alt="CSS gradient border" style="width: 30%"
  src="/blog/assets/css-gradient-border.png?v={{pkg.version}}"
  title="CSS gradient border">

{% raw %}

```html
<html>
  <head>
    <style>
      body {
        background-color: black;
        font-family: sans-serif;
        padding: 1rem;
      }

      .bordered {
        border-image-source: linear-gradient(to bottom right, red, yellow, red);
        border-image-slice: 1;
        border-radius: 0.5rem;
        border-style: solid;
        border-width: 1px;
        color: white;
        display: inline-block;
        padding: 1rem;
      }
    </style>
  </head>
  <body>
    <div class="bordered">Some Text</div>
  </body>
</html>
```

{% endraw %}

Unfortunately, this approach doesn't work if a `border-radius` is needed.
But the somewhat more complicated approach below does work.
See {% aTargetBlank "https://dev.to/afif/border-with-gradient-and-radius-387f",
"Border with gradient and radius" %}.

<img alt="CSS gradient border rounded" style="width: 30%"
  src="/blog/assets/css-gradient-border-rounded.png?v={{pkg.version}}"
  title="CSS gradient border rounded">

{% raw %}

```html
<html>
  <head>
    <style>
      body {
        background-color: black;
        font-family: sans-serif;
        padding: 1rem;
      }

      .bordered {
        color: white;
        display: inline-block;
        font-size: 1rem;
        padding: 1rem;
        position: relative;
      }

      .bordered::before {
        content: '';
        position: absolute;
        inset: 0;

        border-radius: 0.5rem;
        border: 1px solid transparent;
        background: linear-gradient(to bottom right, red, yellow, red) border-box;
        -webkit-mask: linear-gradient(white 0 0) padding-box, linear-gradient(
            white 0 0
          );
        -webkit-mask-composite: xor;
        mask-composite: exclude;
      }
    </style>
  </head>
  <body>
    <div class="bordered">Some Text</div>
  </body>
</html>
```

{% endraw %}

### Centering

There are many ways to center content using CSS.
Common ways include:

1. To center lines of text in an element,
   set `text-align` to `center` to center horizontally, and
   set `line-height` to the element height
   divided by the number of lines to center vertically
   (see `.box` below).

1. To center an element with `display` set to `block`
   horizontally in its parent,
   set `margin-left` and `margin-right` to `auto`
   (see `.box1` below).

1. To center text or child elements within their parent,
   on the parent element set `display` to `flex`,
   set `justify-content` to `center` (to center along major axis),
   and set `align-items` to `center` (to center along minor axis)
   (see `.container` and `.box2` below).

1. To center an element in its parent element,
   on the parent element set `position` to `relative` and
   on the child element set `position` to `absolute`,
   `inset` to `0`, and `margin` to `auto`.
   Alternatively, set `left` and `top` to `50%`, and
   `transform` to `translate(-50%, -50%)`
   (see `.box3` below).

1. To center an element in the browser window,
   set `position` to `fixed`,
   set `inset` to `0`, and set `margin` to `auto`
   Alternatively, set `left` and `top` to `50%`, and
   `transform` to `translate(-50%, -50%)`
   (see `.box4` below).

Each of these approaches is demonstrated in the code below.

{% include "_centering.html" %}

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Centering Demo</title>
    <style>
      .box {
        display: inline-block;
        color: white;
        height: var(--box-size);
        line-height: var(--box-size); /* centers 1 line of text vertically */
        text-align: center; /* centers text horizontally */
        width: var(--box-size);
      }

      .box1 {
        display: block;
        background-color: red;
        line-height: calc(var(--box-size) / 3);
        margin: 0 auto; /* centers box horizontally when display is block */
      }

      .box2 {
        background-color: blue;
      }

      .box3 {
        background-color: green;
        opacity: 0.5;
        position: absolute;
        left: 50%; /* of width of position relative ancestor */
        top: 50%; /* of height of position relative ancestor */
        transform: translate(-50%, -50%); /* of box size */
      }

      .box4 {
        display: none;
        justify-content: center;
        align-items: center;

        background-color: purple;
        line-height: unset; /* override from .box */

        /* This centers in browser window. */
        position: fixed;

        /* This centers in its parent, overlapping .box3 */
        /* position: absolute; centers in parent */

        inset: 0; /* doesn't center without this */
        margin: auto; /* doesn't center without this */
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

    <script>
      window.onload = () => {
        const box4 = document.querySelector('.box4');
        const showBtn = document.getElementById('show-btn');
        box4.addEventListener('click', () => (box4.style.display = 'none'));
        showBtn.addEventListener(
          'click',
          () => (box4.style.display = 'inline-flex')
        );
      };
    </script>
  </head>
  <body>
    <button id="show-btn">Show Box #4</button>
    <section id="centering-demo">
      <p>This is a paragraph.</p>

      <div class="box box1">Box #1<br />line 2<br />line 3</div>

      <div class="container">
        <div class="box box2">Box #2</div>
      </div>

      <!-- This box is centered in the section. -->
      <div class="box box3">Box #3</div>

      <!-- This box is centered in the browser window. -->
      <div class="box box4">Box #4<br />Click to hide.</div>
    </section>
  </body>
</html>
```

{% endraw %}

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

  This specifies the grid cells that will be occupied by each named area,
  allowing them to span multiple columns and rows.
  Child elements must assign themselves to the grid names
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

{% raw %}

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

{% endraw %}

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

- `:has` matches an element that has specific descendant elements
  (see `:has` section below)

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

{% raw %}

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

{% endraw %}

### :has Pseudo Class

This matches an element that has specific descendant elements.

The following example uses a hidden checkbox to
toggle the opacity of another element between 0 and 1.
When the `label` wrapped around the checkbox is clicked,
the state of the checkbox is toggled.
Note the use of `:has` to change `opacity` to 1.

```html
<html>
  <head>
    <style>
      #greeting {
        font-size: 3rem;
        margin-top: 4px;
        opacity: 0; /* initially not visible */
        transition: opacity 1s;
      }

      #show-checkbox {
        display: none;
      }

      /* This styles the label to look like a button. */
      #toggle-btn {
        background-color: lightgray;
        border: 1px solid black;
        cursor: pointer;
        padding: 2px 8px;
      }

      body:has(#toggle-btn > :checked) #greeting {
        opacity: 1;
      }
    </style>
  </head>
  <body>
    <label id="toggle-btn">
      <input id="show-checkbox" type="checkbox" />
      Toggle
    </label>
    <div id="greeting">Hello, World!</div>
  </body>
</html>
```

The following example demonstates many uses of `:has`.
A lot is achieved without writing any JavaScript code!

<img alt="CSS :has pseudo class" style="width: 70%"
  src="/blog/assets/css-has-pseudo-class.png?v={{pkg.version}}"
  title="CSS :has pseudo class">

```html
<!DOCTYPE html>
<html>
  <head>
    <title>CSS :has Pseudo Class</title>
    <style>
      body {
        --border-width: 5px;
        font-family: sans-serif;
      }

      /* The background color of section elements
         is based on their descendant elements. */
      section {
        background-color: lightyellow;
        border: var(--border-width) solid transparent;
        margin-bottom: 1rem;
        padding: 1rem;
      }
      section:has(img) {
        background-color: lightblue;
      }
      section:has(img):not(:has(p)) {
        background-color: pink;
      }

      /* If the "like" checkbox is checked then
         all section elements get a red border. */
      body:has(#like-checkbox:checked) section {
        border: var(--border-width) solid red;
      }

      /* Add text after the "like" checkbox
         based on whether it is checked. */
      body:has(#like-checkbox:checked) #like-text::before {
        content: 'Unlike';
      }
      body:not(:has(#like-checkbox:checked)) #like-text::before {
        content: 'Like';
      }

      /* If the next sibling of a p is an img, change the p styling. */
      p:has(+ img) {
        background-color: white;
        padding: 0.5rem;
      }

      .error-msg {
        color: red;
        font-weight: bold;
        visibility: hidden;
      }

      input[type='number'] {
        border: 2px solid gray;
        border-radius: 0.5rem;
        padding: 0.5rem;
      }

      /* If a label contains an input with an invalid value,
         make the error message that follows the label visible.
         ~ selects all subsequent siblings if
         they match the selector after it. */
      label:has(input:user-invalid) ~ .error-msg {
        visibility: visible;
      }

      /* Set the border color of the number input
         based on whether its value is valid.
         Focus must be moved out of the input (blur)
         to apply the change. */
      #rating-input:user-valid {
        border-color: green;
      }
      #rating-input:user-invalid {
        border-color: red;
      }

      button {
        background-color: lightgray;
        border: none;
        border-radius: 0.5rem;
        padding: 0.5rem;
      }

      button:hover {
        background-color: lightgreen;
      }

      .buttons {
        margin-top: 1rem;
      }

      /* If one of the buttons is hovered, dim all the other buttons. */
      .buttons:has(button:hover) button:not(:hover) {
        opacity: 0.3;
      }

      .post {
        border: 2px solid gray;
        margin-top: 1rem;
        padding: 1rem;
      }

      /* Change the border around posts that contain any empty elements. */
      .post:has(> *:empty) {
        border: 2px dashed red;
      }
    </style>
  </head>
  <body>
    <section>
      <p>This is a paragraph.</p>
    </section>
    <section>
      <p>This is the CSS logo.</p>
      <img
        alt="CSS Logo"
        src="https://1000logos.net/wp-content/uploads/2020/09/CSS-Logo.png"
        width="100"
      />
    </section>
    <section>
      <img
        alt="CSS Logo"
        src="https://1000logos.net/wp-content/uploads/2020/09/CSS-Logo.png"
        width="100"
      />
    </section>

    <div>
      <input type="checkbox" id="like-checkbox" />
      <span id="like-text"></span>
    </div>

    <div>
      <label
        >Rating
        <input id="rating-input" type="number" min="0" max="10" />
      </label>
      <span class="error-msg">invalid rating</span>
    </div>

    <div class="buttons">
      <button>First</button>
      <button>Second</button>
      <button>Third</button>
    </div>

    <section class="post">
      <p>This is the first paragraph.</p>
      <p>This is the second paragraph.</p>
    </section>
    <section class="post">
      <p>This is the first paragraph.</p>
      <!-- The next element is empty. -->
      <p></p>
    </section>
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

### Truncating text

The CSS property `text-overflow` can be used to truncate long text.
However, it doesn't work on its own.
The `overflow` and `whitespace` properties must also be specified.
See the CSS rule for `.truncate-text` below.

Toggle the checkbox in the example below to switch between
non-truncated and truncated text.

{% include "_truncate-text.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Text Ellipsis Demo</title>
    <style>
      #demo-text {
        outline: 1px dashed red;
        width: 100px;
      }

      .truncate-text {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
    </style>

    <script>
      window.onload = () => {
        text = document.getElementById('demo-text');
        checkbox = document.getElementById('truncate-cb');
        checkbox.addEventListener('change', () => {
          text.classList.toggle('truncate-text');
        });
      };
    </script>
  </head>
  <body>
    <div>
      <input id="truncate-cb" type="checkbox" />
      <label for="truncate-cb">Truncate Text</label>
    </div>
    <div id="demo-text">This is a large amount of text.</div>
  </body>
</html>
```

### `scroll-behavior` property

The CSS {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-behavior",
"scroll-behavior property" %} causes the page to
smoothly scroll rather than jump to a new page location
when the user clicks a link that
navigates to a different part of the same page.
To enable this behavior, add the following CSS rule:

```css
html {
  scroll-behavior: smooth;
}
```

It is not possible to control the speed at which the page scrolls.

Smooth scrolling is implemented in the page you are viewing.
Click a link in the table of contents at the top to see this in action.
Click the browser back button to scroll back to the table of contents.
Safari does not yet implement this property and simply ignores it.

Some users prefer reduced motion.
This is indicated through an OS setting.
To set this in macOS, open System Preferences, select Accessibility,
select Display, and check the checkbox for "Reduce Motion".

To disable smooth scrolling for these uses, add the following CSS:

```css
@media (prefers-reduced-motion) {
  html {
    scroll-behavior: auto;
  }
}
```

### `position` property

The CSS `position` property supports many values.
This tip distinguishes between three of them.

| `position` value | Description                                                                   |
| ---------------- | ----------------------------------------------------------------------------- |
| `absolute`       | relative to the document; removes element from document flow                  |
| `fixed`          | relative to the viewport; removes element from document flow                  |
| `sticky`         | relative to the document; element remains in document flow, but can be offset |

All of these use the `top`, `right`, `bottom`, and `left` properties
to specify the actual position of the element.

The `inset` property provides a shorter alternative to specifying these.

| `inset` usage    | Equivalent to                          |
| ---------------- | -------------------------------------- |
| `inset: a`       | `top: a; right: a; bottom: a; left: a` |
| `inset: a b`     | `top: a; right: b; bottom: a; left: b` |
| `inset: a b c d` | `top: a; right: b; bottom: c; left: d` |

The `absolute` value causes elements to be positioned relative to the nearest
ancestor element that has its CSS `position` property set to `relative`.
If none is found, they are positioned relative to the browser window.

The `sticky` value is often used to keep `table` headings in view
when a table is scrolled vertically.
To do this, add the following CSS rules:

```css
table {
  position: relative;
}

table > thead th {
  position: sticky;
  top: 0;
}
```

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

{% raw %}

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

{% endraw %}

### `calc` function

CSS property values can use the `calc` function to compute a value.
It takes an expression that can include arithmetic expressions
involving values with different units.
It can also use variables.

The unary minus operator cannot be applied to a variable
to produce a negative value, but a variable can be
multiplied by a negative number to achieve the same effect.

For example:

```css
.some-class {
  height: calc(3rem + 5px);
  width: calc(var(--some-size) * 2);
  margin: calc(var(--some-size) * -1);
}
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

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS transition Demo</title>
    <style>
      .thumb {
        --inset: 3px;
        --size: calc(var(--height) - 2 * var(--inset));

        position: absolute;
        left: var(--inset);
        top: var(--inset);

        background-color: orange;
        border-radius: 50%;
        height: var(--size);
        transition: left 0.3s;
        width: var(--size);
      }

      .toggle {
        --height: 2rem;

        display: inline-block;
        background-color: cornflowerblue;
        border-radius: calc(var(--height) / 2);
        height: var(--height);
        position: relative;
        width: calc(var(--height) * 2);
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

{% endraw %}

Here is another example that transitions the CSS `outline-offset` property
to focus the attention of the user when focus moves to a new form control.
Click in the the username `input`, enter a value,
press tab to move focus to the password `input`, enter a value,
and press tab to move focus to the "Login" `button`.
The focused form control will be indicated by a green outline
that starts with an offset that transitions to zero.

In Safari and Firefox on macOS,
the tab key only moves focus between input fields.
This can be changed in System Preferences by selecting
Keyboard ... Shortcuts and checking the checkbox for
"Use keyboard navigation to move focus between controls".

{% include "_css-transition-outline.html" %}

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS transition outline-offset Demo</title>

    <style>
      body {
        font-family: sans-serif;
      }

      form {
        display: flex;
        flex-direction: column;
        align-items: center;
      }

      form button:disabled {
        background-color: gray;
        color: white;
      }

      form > div {
        margin-bottom: 0.5rem;
      }

      form button,
      form input {
        border: 1px solid gray;
        outline: 0 solid green;
        outline-color: green;
        padding: 0.5rem;
        transition: outline-offset 1s;
      }

      form button:focus,
      form input:focus {
        outline-offset: 0;
        outline-width: 3px;
      }

      form label {
        display: inline-block;
        margin-right: 0.5rem;
        text-align: right;
        width: 4.5rem;
      }
    </style>

    <script>
      window.onload = () => {
        const form = document.getElementById('login-form');
        const usernameInput = document.getElementById('username');
        const passwordInput = document.getElementById('password');
        const submitBtn = document.querySelector('form button[type="submit"]');
        submitBtn.disabled = true;

        let username = '';
        let password = '';

        usernameInput.addEventListener('input', e => {
          username = e.target.value;
          updateSubmitBtn();
        });

        passwordInput.addEventListener('input', e => {
          password = e.target.value;
          updateSubmitBtn();
        });

        form.addEventListener('submit', event => {
          event.preventDefault();
          alert('Logging in with ' + username + ' and ' + password);
        });

        function updateSubmitBtn() {
          submitBtn.disabled = !username || !password;
        }
      };
    </script>
  </head>
  <body>
    <form id="login-form">
      <div>
        <label for="username">Username</label>
        <input id="username" />
      </div>
      <div>
        <label for="password">Password</label>
        <input id="password" type="password" />
      </div>
      <button type="submit">Login</button>
    </form>
  </body>
</html>
```

{% endraw %}

### Transforms

The CSS {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/transform",
"transform property" %} translates, rotates, scales, and skews DOM elements.

The example below renders a button containing a finger pointing emoji.
When the button is pressed, a `rotate` `transform` is applied
to rotate the emoji 180 degrees.
When pressed again, the original rotation of zero degrees is restored.

{% include "_css-transform.html" %}

{% raw %}

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
        border-radius: 50%;
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

{% endraw %}

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

### Triangles

CSS borders can be used to render triangles.
This is made possible by the fact that borders meet on diagonal lines,
even when some of the borders are transparent.
Once the desired triangle is created, it can be
translated, scaled, and rotated using the CSS `transform` property.

The code below demonstrates creating triangles in this way.

Some alternatives to consider include
describing triangles with SVG,
drawing triangles with Canvas, and
using the many triangle Unicode characters
such as <code>&amp;#9650;</code> (&#9650;).

{% include "_css-triangles.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS Triangles Demo</title>
    <style>
      .bottom {
        border-color: transparent;
        border-bottom-color: blue;
        outline: 1px dashed gray;
      }

      .frame {
        height: 4rem;
        width: 4rem;
        border: 2rem solid;
        border-color: red green blue purple;
      }

      .isosceles {
        border-width: 2rem 4rem;
      }

      .left {
        border-color: transparent;
        border-left-color: purple;
        outline: 1px dashed gray;
      }

      .no-content {
        height: 0;
        width: 0;
      }

      .right {
        border-color: transparent;
        border-right-color: green;
        outline: 1px dashed gray;
      }

      .rotate {
        transform: rotate(45deg);
        outline: 1px dashed gray;
      }

      .row {
        display: flex;
        align-items: center;
        margin-bottom: 1rem;
      }

      .row > .frame {
        margin-left: 1rem;
      }

      .top {
        border-color: transparent;
        border-top-color: red;
        outline: 1px dashed gray;
      }
    </style>
  </head>
  <body>
    <p>Borders meet on diagonal lines.</p>
    <div class="frame"></div>

    <p>When the content size is zero, the borders become triangles.</p>
    <div class="frame no-content"></div>

    <p>Making all but one border transparent results in a single triangle.</p>
    <div class="row">
      top
      <div class="frame no-content top"></div>
    </div>
    <div class="row">
      right
      <div class="frame no-content right"></div>
    </div>
    <div class="row">
      bottom
      <div class="frame no-content bottom"></div>
    </div>
    <div class="row">
      left
      <div class="frame no-content left"></div>
    </div>

    <p>A triangle can be rotated by any amount.</p>
    <div class="row">
      45 degrees
      <div class="frame no-content left rotate"></div>
    </div>

    <p>
      To create non-right, isosceles triangles, use differing border widths.
    </p>
    <div class="frame no-content isosceles"></div>
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
When each keyframe is executed,
a transition is performed for each CSS property that is specified,
going from the previous value to a new value.

Each keyframe specifies the properties to be applied
at a given percentage through the duration.
It is not necessary to specify a value for every property being animated
for every percentage that is specified.
For example, we could animate `color` changing from
`red` to `yellow` to `blue` and back to `red` at `0%`, `33%`, `67%`, and `100%`
while animating `font-size` changing from
`1rem` to `3rem` and back to `1rem` at `0%`, `50%`, and `100%`.

Animations created this way can do several things
that transitions cannot, including:

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

- To retain the current properties at the beginning and end of the animation,
  set this to `none`, which is the default value.
- To apply different styles at `0%` (or `from`), set this to `backwards`.
- To return the properties set after `100%` (or `to`),
  set this to `forwards`.
- To do both, set this to `both`.

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
The "Start" button changes to "Pause", and a "Stop" button appears.
Pressing the "Pause" button pauses the animation
and changes the button to "Resume".
Pressing the "Resume" button resumes the animation.
Pressing the "Stop" button stops the animation,
which returns the square to its starting position
and changes the first button back to "Start".

{% include "_css-animation.html" %}

{% raw %}

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

{% endraw %}

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

      /* mobile view */
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

If separate media queries are defined for mobile and non-mobile widths,
be careful that both are not applied at the breakpoint.
In the following example, if the screen width is exactly `800px`,
both of these media queries will be applied
and `h1` elements will be both bold and red.

```css
/* Mobile - width <= 800px */
@media (max-width: 800px) {
  h1 {
    font-weight: bold;
  }
}

/* Desktop - width >= 800px */
@media (min-width: 800px) {
  h1 {
    color: red;
  }
}
```

### Container Query Length Units

Container query length units specify sizes relative to
the size of a container element.
The following units are supported:

- `cqw`: 1% of container width (independent of writing direction)
- `cqh`: 1% of container height (independent of writing direction)
- `cqi`: 1% of container inline size (width considering writing direction)
- `cqb`: 1% of container block size (width AND height considering writing direction)
- `cqmin`: smallest of `cqi` or `cqb`
- `cqmax`: largest of `cqi` or `cqb`

The following example demonstrates rendering playing cards
where the rank and suit are centered on each card.
The font size is relative to the card width.
The card width changes based on the window width
and the card heights are controlled by an aspect ratio.

<img alt="CSS container query units" style="width: 80%"
  src="/blog/assets/css-container-query-units.gif?v={{pkg.version}}"
  title="CSS container query units">

{% raw %}

```html
<html>
  <head>
    <meta charset="UTF-8" />
    <title>CSS Container Queries</title>
    <style>
      body {
        background-color: tan;
        margin: 0;
        padding: 2vw;
      }

      .card {
        display: inline-flex;
        justify-content: center;
        align-items: center;

        width: 100%; /* take as much width as possible */
        flex-grow: 1; /* shrink so all are same width */
        aspect-ratio: 0.7143; /* playing cards are 2.5" by 3.5" */

        container-type: inline-size;

        background-color: white;
        border-radius: 3vw;
        padding: 3vw;
      }

      .hand {
        display: flex;
        gap: 3vw;
        width: 100%;
      }

      .name {
        font-size: 50cqw;
      }

      .diamond,
      .heart {
        color: red;
      }

      .club,
      .spade {
        color: black;
      }
    </style>
  </head>
  <body>
    <div class="hand">
      <div class="card heart">
        <div class="name">J♥</div>
      </div>
      <div class="card spade">
        <div class="name">Q♠</div>
      </div>
      <div class="card diamond">
        <div class="name">K♦</div>
      </div>
      <div class="card club">
        <div class="name">A♣</div>
      </div>
    </div>
  </body>
</html>
```

{% endraw %}

### Container Queries

Media queries style elements based the window size.
Container queries style elements based on their container size.

Specific elements can be identified as containers.
Container queries can then define CSS rules that
only apply to elements inside those containers.

To declare an element to be a container,
use the CSS property `container-type`.
The value must be `inline-size` (sizes based on width; most common),
`size` (sizes based on width AND height), or `normal` (not a query container).
Containers can optionally be given names.

For example:

```css
.card {
  container-name: card;
  container-type: inline-size;

  /*
  The previous two lines can be replaced by this shorthand:
  container: card / inline-size;
  */
}
```

To specify CSS rules that only apply to elements inside a container,
define an `@container` query which is similar to a media query.
For example:

```css
/* This only applies to elements inside containers named "card"
   that are at least 800px wide.
   To apply to elements inside all containers that are at least 800px wide,
   omit the container name. */
@container card (min-width: 800px) {
  .suit {
    border: 1px solid red;
  }
}
```

The following example uses a container query to change the
`flex-direction` CSS property based on the width of a container.

<img alt="CSS container query" style="width: 80%"
  src="/blog/assets/css-container-query.gif?v={{pkg.version}}"
  title="CSS container query">

{% raw %}

```html
<html>
  <head>
    <title>CSS Container Queries</title>
    <style>
      body {
        display: flex;
        margin: 0;

        font-family: sans-serif;
        font-size: 2rem;
      }

      h1 {
        margin-top: 0;
      }

      main {
        container: main / inline-size;

        flex-grow: 1;

        background-color: cornflowerblue;
        color: white;
        padding: 2rem;
      }

      nav {
        flex-grow: 0;
        background-color: tan;
        padding: 1rem;
        max-width: 10rem;
        min-width: 10rem;
      }

      nav > ul {
        list-style-type: none;
        margin: 0;
        padding: 0;
      }

      .story {
        display: flex;
        flex-direction: column;
        gap: 2rem;
      }

      .story > p {
        background-color: white;
        border-radius: 1rem;
        color: black;
        margin: 0;
        padding: 1rem;
      }

      @container main (min-width: 800px) {
        .story {
          flex-direction: row;
        }
      }
    </style>
  </head>
  <body>
    <nav>
      <ul>
        <li>Item #1</li>
        <li>Item #2</li>
        <li>Item #3</li>
        <li>Item #4</li>
      </ul>
    </nav>
    <main>
      <h1>Story</h1>
      <p>
        An interesting story is presented below.
      </p>
      <section class="story">
        <p>
          This is the first paragraph. It contains enough text to require
          wrapping. More text is required to achieve this, so I add a lot more.
          I hope this is enough.
        </p>
        <p>
          This is the second paragraph. It contains enough text to require
          wrapping. More text is required to achieve this, so I add a lot more.
          I hope this is enough.
        </p>
      </div>
    </main>
  </body>
</html>
```

{% endraw %}

### Light and dark modes

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

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>window.matchMedia Demo</title>
    <style>
      body {
        --bg-color: white;
        --fg-color: black;

        background-color: var(--bg-color);
        color: var(--fg-color);
        padding: 0 1rem 1rem 1rem;
      }

      body.dark-mode {
        --bg-color: black;
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

{% endraw %}

### Font size preference

Using the CSS size unit `rem` for most sizes
has the advantage that it is easy to allow users to
adjust the size of those properties across the entire page.

The example below shows this in action.
Clicking the "+" button increases the font size of the `h1` and `p` elements.
Clicking the "-" button decreases those font sizes.

An enhancement idea is to save the users preferred font size
in `localStorage` and restore it each time they revisit the site.

{% include "_font-size-change.html" %}

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Font Size Change Demo</title>
    <style>
      button:disabled {
        background-color: pink;
      }

      h1 {
        font-size: 2rem;
        margin-top: 0;
      }

      header {
        display: flex;
        align-items: center;
        /* The sizes in this element do not use rems
           because we don't want zooming to change them. */
        font-size: 16px;
        gap: 8px;
        margin-bottom: 16px;
      }

      p {
        font-size: 1rem;
      }
    </style>
    <script>
      let zoomFactor = 1;
      let zoomFactorDiv;
      let zoomInBtn;
      let zoomOutBtn;

      function changeFontSize(delta) {
        zoomFactor += delta;
        zoomFactorDiv.textContent = zoomFactor.toFixed(1);
        zoomInBtn.disabled = zoomFactor >= 4;
        zoomOutBtn.disabled = zoomFactor <= 0.4;
        /* document.documentElement is the html element. */
        document.documentElement.style.fontSize = zoomFactor + 'rem';
      }

      window.onload = () => {
        zoomFactorDiv = document.getElementById('zoom-factor');
        zoomInBtn = document.getElementById('zoom-in-btn');
        zoomOutBtn = document.getElementById('zoom-out-btn');

        zoomInBtn.addEventListener('click', () => changeFontSize(0.2));
        zoomOutBtn.addEventListener('click', () => changeFontSize(-0.2));
      };
    </script>
  </head>
  <body>
    <header>
      <button id="zoom-in-btn">➕</button>
      <button id="zoom-out-btn">➖</button>
      <div id="zoom-factor">1</div>
    </header>
    <h1>Font Size Demo</h1>
    <p>
      Click the plus and minus buttons above to change the font size used in the
      entire page for elements whose size is specified using
      <code>rem</code> units.
    </p>
  </body>
</html>
```

{% endraw %}

### Custom fonts with `@font-face`

To use a custom font in a web app:

1. Download a font file in one of the supported formats
   (includes "opentype", "truetype", and "woff")
   and place it in a directory where public files are served.

1. Add the `@font-face` at-rule outside any CSS rule
   in a global CSS file, not one specific to a single component.
   For example:

   ```css
   @font-face {
     font-family: 'Ebrima';
     src: url('/fonts/ebrima.ttf') format('truetype');
   }
   ```

1. Refer to the new font family using the `font-family` CSS property.
   For example:

   ```css
   h1,
   h2,
   h3,
   h4,
   h5,
   h6 {
     font-family: 'Ebrima';
   }
   ```

### clip-path property

The CSS {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path",
"clip-path property" %} defines a clipping region
that determines the parts of an element that should be visible.
It is often used with images, but is not limited to those.

The clip path is defined by one of the following functions,
`inset`, `circle`, `ellipse`, `polygon`, or `path`.
Each of these is demonstrated below.

The `clip-path` property can be used with the `transition` property
so changes are animated.
For example, the `clip-path` can become larger or smaller on hover.

{% include "_css-clip-paths.html" %}

{% raw %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>CSS clip-path Demo</title>
    <style>
      body {
        background-color: rebeccapurple;
      }

      .circle {
        /* radius "at" x-center y-center */
        clip-path: circle(35% at 55% 50%);
      }

      .diamond {
        clip-path: polygon(50% 0%, 100% 50%, 50% 100%, 0% 50%);
      }

      .ellipse {
        /* x-radius y-radius "at" x-center y-center */
        clip-path: ellipse(50px 75px at 60% 50%);
      }

      .heart {
        clip-path: path(
          'M15,45 A30,30,0,0,1,75,45 A30,30,0,0,1,135,45 Q135,90,75,130 Q15,90,15,45 Z'
        );
      }

      img {
        display: inline-block;
        height: 200px;
        width: unset;
      }

      .inset {
        /* top right bottom left "round" border-radius */
        clip-path: inset(30px 20px 40px 50px round 10px);
      }

      .star {
        clip-path: polygon(
          50% 0%,
          61% 35%,
          98% 35%,
          68% 57%,
          79% 91%,
          50% 70%,
          21% 91%,
          32% 57%,
          2% 35%,
          39% 35%
        );
        /* This works best on square elements.
           The following properties cause on the top, square
           portion of the image to be rendered. */
        aspect-ratio: 1; /* not supported by IE or Safari */
        object-fit: cover;
      }
    </style>
  </head>
  <body>
    <img class="inset" src="./comet.jpg" alt="Comet" />
    <img class="circle" src="./comet.jpg" alt="Comet" />
    <img class="ellipse" src="./comet.jpg" alt="Comet" />
    <img class="heart" src="./comet.jpg" alt="Comet" />
    <img class="diamond" src="./comet.jpg" alt="Comet" />
    <img class="star" src="./comet.jpg" alt="Comet" />
  </body>
</html>
```

{% endraw %}

### filter property

The CSS {% aTargetBlank "https://developer.mozilla.org/en-US/docs/Web/CSS/filter",
"filter property" %} applies special effects to elements.
Each effect is described by a function call
that is the value of the `filter` property.
For example, `filter: sepia(100%);`

The supported effects are demonstrated below.
Try changing the input values to vary the effects.
In Safari, changes to the drop-shadow values do not take effect
until the window width is made smaller or the window height is changed.
This seems to be a bug in Safari.

The only advantage of using the `filter` property
with the `drop-shadow` function over using the `box-shadow` property
is that some browsers utilize hardware acceleration for it.

One effect not covered here is `url(some-url)` where
the `some-url` refers to a file that defines an SVG filter.

{% include "_css-filters.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Figure Demo</title>
    <style>
      .blur {
        filter: blur(var(--blur-size));
      }

      body {
        font-family: sans-serif;
      }

      .brightness {
        filter: brightness(var(--brightness-percent));
      }

      .contrast {
        filter: contrast(var(--contrast-percent));
      }

      .drop-shadow {
        filter: drop-shadow(
          var(--drop-shadow-offset-x) var(--drop-shadow-offset-y) var(
              --drop-shadow-blur-radius
            ) var(--drop-shadow-color)
        );
      }

      .filter {
        font-family: monospace;
      }

      table input {
        border: 1px solid gray;
        padding: 0.5rem;
      }

      .grayscale {
        filter: grayscale(var(--grayscale-percent));
      }

      .hue-rotate {
        filter: hue-rotate(var(--hue-rotate-angle));
      }

      img {
        display: inline-block;
        height: 200px;
      }

      .invert {
        filter: invert(var(--invert-percent));
      }

      .opacity {
        filter: opacity(var(--opacity-percent));
      }

      .saturate {
        filter: saturate(var(--saturate-percent));
      }

      .sepia {
        filter: sepia(var(--sepia-percent));
      }

      table {
        --blur-size: 1px;
        --brightness-percent: 50%;
        --contrast-percent: 50%;
        --drop-shadow-blur-radius: 5px;
        --drop-shadow-color: #0000ff; /* This color format is required. */
        --drop-shadow-offset-x: 5px;
        --drop-shadow-offset-y: 5px;
        --grayscale-percent: 100%;
        --hue-rotate-angle: 180deg;
        --invert-percent: 100%;
        --opacity-percent: 50%;
        --saturate-percent: 50%;
        --sepia-percent: 100%;

        border-collapse: collapse;
      }

      td,
      th {
        border: 1px solid gray;
        padding: 0.5rem;
      }

      td > div:not(:first-of-type) {
        margin-top: 0.5rem;
      }
    </style>

    <script>
      window.onload = () => {
        const table = document.getElementById('filter-table');

        function setupInput(id, suffix) {
          const input = document.getElementById(id);
          const value = getComputedStyle(table)
            .getPropertyValue('--' + id)
            .trim();
          input.value = suffix ? parseInt(value) : value;

          input.addEventListener('input', e => {
            let {value} = e.target;
            table.style.setProperty(
              '--' + id,
              suffix ? parseInt(value) + suffix : value
            );
          });
        }

        setupInput('blur-size', 'px');
        setupInput('brightness-percent', '%');
        setupInput('contrast-percent', '%');
        setupInput('drop-shadow-blur-radius', 'px');
        setupInput('drop-shadow-color');
        setupInput('drop-shadow-offset-x', 'px');
        setupInput('drop-shadow-offset-y', 'px');
        setupInput('grayscale-percent', '%');
        setupInput('hue-rotate-angle', 'deg');
        setupInput('invert-percent', '%');
        setupInput('opacity-percent', '%');
        setupInput('saturate-percent', '%');
        setupInput('sepia-percent', '%');
      };
    </script>
  </head>
  <body>
    <table id="filter-table">
      <thead>
        <tr>
          <th>Filter</th>
          <th>Demo</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>
            <div class="filter">blur(size)</div>
            <div>
              <label for="blur-size">blur size in px</label>
              <input id="blur-size" min="0" max="20" type="number" />
            </div>
          </td>
          <td>
            <img class="blur" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">brightness(percent)</div>
            <div>
              <label for="brightness-percent">percent</label>
              <input id="brightness-percent" min="0" max="200" type="number" />
            </div>
          </td>
          <td>
            <img class="brightness" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">contrast(percent)</div>
            <div>
              <label for="contrast-percent">percent</label>
              <input id="contrast-percent" min="0" max="200" type="number" />
            </div>
          </td>
          <td>
            <img class="contrast" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">
              drop-shadow(<br />
              offset-x offset-y blur-radius color<br />)
            </div>
            <div>
              <label for="drop-shadow-offset-x">offset-x in px</label>
              <input id="drop-shadow-offset-x" min="0" max="20" type="number" />
            </div>
            <div>
              <label for="drop-shadow-offset-y">offset-y in px</label>
              <input id="drop-shadow-offset-y" min="0" max="20" type="number" />
            </div>
            <div>
              <label for="drop-shadow-blur-radius">blur radius in px</label>
              <input
                id="drop-shadow-blur-radius"
                min="0"
                max="20"
                type="number"
              />
            </div>
            <div>
              <label for="drop-shadow-color">color</label>
              <input id="drop-shadow-color" type="color" />
            </div>
          </td>
          <td>
            <img class="drop-shadow" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">grayscale(percent)</div>
            <div>
              <label for="grayscale-percent">percent</label>
              <input id="grayscale-percent" min="0" max="200" type="number" />
            </div>
          </td>
          <td>
            <img class="grayscale" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">hue-rotate(angle)</div>
            <div>
              <label for="hue-rotate-angle">angle in degrees</label>
              <input id="hue-rotate-angle" min="0" max="360" type="number" />
            </div>
          </td>
          <td>
            <img class="hue-rotate" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">invert(percent)</div>
            <div>
              <label for="invert-percent">percent</label>
              <input id="invert-percent" min="0" max="200" type="number" />
            </div>
          </td>
          <td>
            <img class="invert" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">opacity(percent)</div>
            <div>
              <label for="opacity-percent">percent</label>
              <input id="opacity-percent" min="0" max="200" type="number" />
            </div>
          </td>
          <td>
            <img class="opacity" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">saturate(percent)</div>
            <div>
              <label for="saturate-percent">percent</label>
              <input id="saturate-percent" min="0" max="200" type="number" />
            </div>
          </td>
          <td>
            <img class="saturate" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
        <tr>
          <td>
            <div class="filter">sepia(percent)</div>
            <div>
              <label for="sepia-percent">percent</label>
              <input id="sepia-percent" min="0" max="200" type="number" />
            </div>
          </td>
          <td>
            <img class="sepia" src="./comet.jpg" alt="Comet" />
          </td>
        </tr>
      </tbody>
    </table>
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

## Images

### Image Formats

The table below summarizes the most common image formats used on the web
and the features they support.

| Format                                  | Transparency | Animation | Maximum Colors     | Primary Usage                |
| --------------------------------------- | ------------ | --------- | ------------------ | ---------------------------- |
| Graphics Interchange Format (GIF)       | ✓            | ✓         | 8-bit, 256         | animation                    |
| Joint Photographic Experts Group (JPEG) | ✖            | ✖         | 24-bit, 16 million | photos                       |
| Portable Network Graphics (PNG)         | ✓            | ✖         | 24-bit, 16 million | icons and text               |
| Animated PNG (APNG)                     | ✓            | ✓         | 24-bit, 16 million | animation                    |
| WebP                                    | ✓            | ✓         | 24-bit, 16 million | replacement for JPEG and PNG |
| High Efficiency Image File (HEIF)       | ✓            | ✓         | 16-bit, 65,536     | Apple apps, not browsers     |

GIF, JPEG, PNG, and APNG are widely supported by web browsers.
WebP images are typically preferred over JPEG and PNG images
because they are much smaller that equivalent images in those formats.
WebP is supported by Chrome, Edge, Firefox, and Safari.
However, Safari 14 does not support animated WebP images.
Safari 15 does, but it only runs in macOS 11 (Big Sur) and later.
HEIF is not supported by any web browsers.

TODO: Discuss image compression and options to optimize each format.

### object-fit and background-size CSS properties

When the size of an image doesn't match
the size of the area where it will be rendered,
the `object-fit` CSS property can be used
to specify how the image should be resized.

| `object-fit` value | Effect                                                         |
| ------------------ | -------------------------------------------------------------- |
| `contain`          | scaled to fit inside, so may not completely cover              |
| `scale-down`       | same as `contain`, but only if it would make the image smaller |
| `cover`            | scaled to cover container, so can be cropped                   |
| `fill`             | resized to cover container by stretching, losing aspect ratio  |
| `none`             | not scaled or resized                                          |

When an image is used as a background
by specifying the `background-image` CSS property,
use `background-size` instead of `object-fit`
to specify how it should be resized.
The value can be `contain`, `cover`, a width, or a width and height.
Other values that can be used with `object-fit` are not supported.

{% include "_object-fit.html" %}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>object-fit and background-size Demo</title>
    <style>
      .bg-contain {
        background-image: url('comet.jpg');
        background-position: center; /* defaults to 0% 0% */
        background-repeat: no-repeat; /* defaults to repeat */
        background-size: contain; /* defaults to auto auto */
      }

      .bg-cover {
        background-image: url('comet.jpg');
        background-position: center; /* defaults to 0% 0% */
        background-size: cover; /* defaults to auto auto */
      }

      .contain {
        object-fit: contain; /* defaults to fill */
      }

      .cover {
        object-fit: cover; /* defaults to fill */
      }

      div,
      img {
        --size: 200px;
        height: var(--size);
        width: var(--size);

        margin: 0;
        outline: 1px solid red;
      }

      div {
        color: #0c0;
        font-family: monospace;
        font-size: 1.2rem;
      }

      div,
      figure {
        display: inline-block;
        margin: 1rem;
      }

      figcaption {
        font-family: monospace;
        font-size: 1rem;
      }

      .fill {
        object-fit: fill;
      }

      .scale-down {
        object-fit: scale-down;
      }
    </style>
  </head>
  <body>
    <section>
      <figure>
        <img class="contain" alt="Comet" src="comet.jpg" />
        <figcaption>object-fit: contain;</figcaption>
      </figure>
      <figure>
        <img class="cover" alt="Comet" src="comet.jpg" />
        <figcaption>object-fit: cover;</figcaption>
      </figure>
      <figure>
        <img class="fill" alt="Comet" src="comet.jpg" />
        <figcaption>object-fit: fill;</figcaption>
      </figure>
      <figure>
        <img class="scale-down" alt="Comet" src="comet.jpg" />
        <figcaption>object-fit: scale-down;</figcaption>
      </figure>
    </section>
    <section>
      <div class="bg-contain">background-size: contain</div>
      <div class="bg-cover">background-size: cover</div>
    </section>
  </body>
</html>
```

### Generating Images

To generate a variety of images sizes in macOS,
open an image file in the Preview app.
By default this can be done by double-clicking an image file in the Finder.
For each image size to be created:

1. Select File...Duplicate.
1. Change the file name in the upper-left corner.
1. Select Tools...Adjust Size...
1. Enter the desired width in pixels.
1. Press the "OK" button.

There are web sites that convert JPEG images to the WebP format for free.
One is {% aTargetBlank "https://squoosh.app/", "Squoosh" %}
which is an experimental project from Google Chrome Labs.
It "make images smaller using best-in-class codecs, right in the browser."
Another option is {% aTargetBlank "https://convertio.co/jpg-webp/", "Convertio" %}
from {% aTargetBlank "https://softo.co", "softo" %}.

These images can also be created with different aspect ratios
and have different parts of the image cropped out.

In the examples below we started with a full-size JPEG image
with a resolution of 4032x3024 and created other images from it.
These include JPEG images with widths of 1200, 800, and 400 pixels.
We also created a WebP image from the full-size JPEG image.

### img element

The `img` element supports using the `srcset` and `sizes` attributes
to specify a list of image files can be used.
The `srcset` value is a comma-separated list of image file paths followed by their pixel width (with a `w` after the number).
along with the pixel width of each.
The `sizes` value is a comma-separated list of media queries
that specify when to use the corresponding image file.
When the `sizes` attribute is omitted, an image is selected based on the width of the area in which it will be rendered.
When the `sizes` attribute is specified, an image is selected based on the first media query that is satisfied.
TODO: Can this also specify pixel density values like 1x, 2x, and 3x?

When `srcset` is specified, the `src` attribute should
also be included to provide a default image to render
in browsers that do not yet support the `srcset` attribute.

In the example below, the browser will select the smallest image
that is at least as wide as the area in which it will be rendered.
If the largest available image size is needed,
and the browser supports the WebP format,
that will be preferred over the JPEG format
due to the order in which the options are listed.

Resizing the browser window to a smaller width
will not necessarily cause it to switch to a smaller image.
Since an image that is wider than necessary has already been loaded,
the browser can simply scale that image
rather than download a new, smaller image.

Resizing the browser window to a larger width
will cause it to switch to a larger image
if that is called for my the `srcset` and `size` attribute values.

{# pragma warning disable format #}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width" />
    <title>img Element Demo</title>
    <style>
      body {
        padding: 1rem;
      }

      .container {
        display: flex;
        align-items: center;
        gap: 1rem;

        box-sizing: border-box;
        width: 100%;
      }

      img {
        box-shadow: 0.5rem 0.5rem 0.5rem gray;
        box-sizing: border-box;
        flex-grow: 1;
        min-width: 0; /* needed to allow image to shrink */
      }

      p {
        text-align: center;
        width: 300px;
      }
    </style>
  </head>
  <body>
    <div class="container">
      <p>This is the Grand Prismatic Spring in Yellowstone National Park.</p>
      <img
        alt="Grand Prismatic Spring"
        src="./grand-prismatic-spring-1200.jpg"
        srcset="
          ./grand-prismatic-spring-400.jpg    400w,
          ./grand-prismatic-spring-800.jpg    800w,
          ./grand-prismatic-spring-1200.jpg  1200w,
          ./grand-prismatic-spring-4032.webp 4032w,
          ./grand-prismatic-spring-4032.jpg  4032w
        "
        sizes="
          (max-width: 800px) 400px,
          (max-width: 1200px) 800px,
          (max-width: 1600px) 1200px,
          4032px
        "
      />
    </div>
  </body>
</html>
```

{# pragma warning enable format #}

Note that the `400w` image above is never used, even when
the browser window is resized to its smallest width
and the page is refreshed.
This is because Chrome, Firefox, and Safari
do not allow a browser window to be resized
to a width that is less than 400 pixels.
Edge allows a minimum width of 149px when the DevTools are open
and 500px when they are not.

When the DevTools are set to simulate a specific mobile device
that has a screen width that is less than 400 pixels,
it typically has a pixel density (DPI) that is 2x or 3x,
so the width is treated as double or triple
which is also not less than 400 pixels.

An alternative to specifying image widths after
the image file paths in the `srcset` attribute value
is to specify pixel densities such as `2x` and `3x`.
Chrome DevTools refers to this value as the Device Pixel Ratio (DPR).

For reference, the table below provides the pixel densities
of some common mobile devices.

| Mobile Device      | Pixel Density or DPR |
| ------------------ | -------------------- |
| iPhone 2-3         | 1x                   |
| iPhone 4-9         | 2x                   |
| iPhone 10+         | 3x                   |
| iPad 1-2           | 1x                   |
| iPad 3-7           | 2x                   |
| iPad Air 1-3       | 2x                   |
| iPad Pro 2015-2020 | 2x                   |
| Galaxy S5          | 3x                   |
| Nexus 5X           | 2.6x                 |
| Pixel 2            | 2.6x                 |
| Pixel 2 XL         | 3.5x                 |

The following mobile devices have a pixel density of `2x`:
iPhone 4 through 9, iPad
The following mobile devices have a pixel density of `2x`:
iPhone 4 through 9, iPad
The following mobile devices have a pixel density of `3x`:
iPhone 10 and above,

To verify the image that is downloaded for various browser widths,
open the browser DevTools, click the "Network" tab,
disable caching, size the browser window, and refresh the page.
The image that was downloaded will appear in the Network tab.
Note that the Firefox DevTools Network tab does not show
local files (with a `file:` protocol).

### picture element

Like the `img` element, the `picture` element
can be used to specify a set of images to be considered.
But it can also specify multiple sets of images
consider based on additional media queries.

The next example similar to the one above,
but uses the `picture` element instead of the `img` element.
It also specifies different sets of images to use
based on device orientation.
This is a uncommon need.
In most cases the `img` element can be used
instead of the `picture` element, and is somewhat simpler.

{# pragma warning disable format #}

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width" />
    <title>picture Element Demo</title>
    <style>
      body {
        padding: 1rem;
      }

      .container {
        display: flex;
        align-items: center;
        gap: 1rem;

        box-sizing: border-box;
        width: 100%;
      }

      img {
        box-shadow: 0.5rem 0.5rem 0.5rem gray;
        width: 100%;
      }

      p {
        text-align: center;
        /*TODO: Why can't these be replaced with just width: 225px;? */
        max-width: 225px;
        min-width: 225px;
      }

      .right {
        flex-grow: 1;
      }
    </style>
  </head>
  <body>
    <div class="container">
      <p>This is the Grand Prismatic Spring in Yellowstone National Park.</p>
      <!--TODO: Why can't flexbox size the image unless
          the picture element is wrapped in a div? -->
      <div class="right">
        <picture>
          <source
            media="(orientation: landscape)"
            srcset="
              ./grand-prismatic-spring-400.jpg    400w,
              ./grand-prismatic-spring-800.jpg    800w,
              ./grand-prismatic-spring-1200.jpg  1200w,
              ./grand-prismatic-spring-4032.webp 4032w,
              ./grand-prismatic-spring-4032.jpg  4032w
            "
            sizes="
              (max-width: 800px) 400px,
              (max-width: 1200px) 800px,
              (max-width: 1600px) 1200px,
              4032px
            "
          />
          <source
            media="(orientation: portrait)"
            srcset="
              ./grand-prismatic-spring-400-portrait.jpg    400w,
              ./grand-prismatic-spring-800-portrait.jpg    800w,
              ./grand-prismatic-spring-1200-portrait.jpg  1200w,
              ./grand-prismatic-spring-4032-portrait.webp 4032w,
              ./grand-prismatic-spring-4032-portrait.jpg  4032w
            "
            sizes="
              (max-width: 800px) 400px,
              (max-width: 1200px) 800px,
              (max-width: 1600px) 1200px,
              4032px
            "
          />
          <img
            alt="Grand Prismatic Spring"
            src="./grand-prismatic-spring-1200.jpg"
          />
        </picture>
      </div>
    </div>
  </body>
</html>
```

{# pragma warning enable format #}

## JavaScript

### console methods

The global `console` object supports many methods for writing to
the browser DevTools console or `stdout` in Node.js.

There are four levels of output.
From lowest to highest severity they are
`verbose`, `info`, `warning`, and `error`.
Browser DevTools allow selecting the levels of messages to be output.
By default, all are enabled.

In Chrome, the levels to output are specified in a drop-down
that initially has the value "All Levels ▼".
In Firefox, there are toggle buttons for
"Errors", "Warnings", "Logs", "Info", and "Debug"
where any number of them can be selected.
In Safari, there are toggle buttons for
"All", "Evaluations", "Errors", "Warnings", and "Logs"
where only one of them can be selected.

| Method                              | Level   | Description                                                                                                                             |
| ----------------------------------- | ------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| `console.assert(condition, values)` | error   | if `condition` is false, this is similar to `console.error`<br>but includes a stack trace;<br>if `condition` is true, nothing is output |
| `console.clear()`                   | N/A     | clears all previous console output                                                                                                      |
| `console.count(label)`              | info    | outputs the number of times it has been called with a given label                                                                       |
| `console.countReset(label)`         | N/A     | resets the count for a given label to zero                                                                                              |
| `console.debug(values)`             | verbose | similar to `console.log` but at debug level                                                                                             |
| `console.dir(object)`               | info    | outputs a single object with disclosure triangles like `console.log` (no reason to prefer over that)                                    |
| `console.dirxml(element)`           | info    | outputs a DOM element with disclosure triangles for viewing descendant elements                                                         |
| `console.error(values)`             | error   | similar to `console.log` but outputs an icon, red text, pink background, and a stack trace                                              |
| `console.group(label)`              | N/A     | starts a collapsable group of output that is initially expanded; can create nested groups                                               |
| `console.groupCollapsed(label)`     | N/A     | same as `console.group`, but initially collapsed                                                                                        |
| `console.groupEnd()`                | N/A     | ends the most recent group                                                                                                              |
| `console.info(values)`              | info    | similar to `console.log`                                                                                                                |
| `console.log(values)`               | info    | outputs string representation of each value separated by single spaces                                                                  |
| `console.profile(name)`             | N/A     | non-standard                                                                                                                            |
| `console.profileEnd(name)`          | N/A     | non-standard                                                                                                                            |
| `console.table(obj-or-arr)`         | info    | outputs properties of objects or arrays in a table; often passed an array of objects                                                    |
| `console.time(name)`                | N/A     | starts timer with a given name                                                                                                          |
| `console.timeEnd(name)`             | info    | ends timer with a given name and outputs the name and number of milliseconds that elapsed                                               |
| `console.timeLog(name)`             | info    | outputs the name and number of milliseconds that elapsed for a given timer without ending it                                            |
| `console.timeStamp(values)`         | N/A     | non-standard                                                                                                                            |
| `console.trace(values)`             | info    | outputs the supplied values, if any, and a disclosure triangle expanded to show a stack trace                                           |
| `console.warn(values)`              | warning | similar to `console.log` but outputs brown text, yellow background, and stack trace                                                     |

The `log`, `info`, `debug`, `warn`, and `error` methods
are similar in that they can be passed:

1. any number of values to be output with a single space between each
2. a template string containing type-specific placeholders
   followed by values to be substituted into the placeholders

The supported placeholders are:

- `%d` or `%i` for an integer
- `%f` for a floating point value
- `%s` for a string
- `%o` or `%O` for an object

In browser DevTools the object placeholder is replaced by
a disclosure triangle (▶) which can be clicked to expand
so each property in the object is displayed on a separate line.
Clicking it again contracts the display to a single line.

When outputting descriptive text followed by an object,
it is best to separate them with a comma
instead of concatenating them with the `+` operator.
This will cause it to output a disclosure triangle for
expanding and contracting the object properties.
When the `+` operator is used, the string representations are concatenated
and then the result is output.
The string representation of an object typically looks like
`[object Object]`, which is unhelpful.

One way to output multiple variables with their names and values
is to pass an object literal containing them to a `console` method.
For example:

```js
const name = 'Mark';
const address = {street: '123 Some Street', city: 'Somewhere', state: 'MO'};
console.log({name, address});
```

This outputs `▶ {name: "Mark", address: {…}}`.

When the disclosure triangle is clicked it displays:

```text
▼ {name: "Mark", address: {…}}
  ▶ address: {street: "123 Some Street", city: "Somewhere", state: "MO"}
    name: "Mark"
```

Clicking the disclosure triangle before "address" displays:

```text
▼ {name: "Mark", address: {…}}
  ▼ address:
      city: "Somewhere"
      state: "MO"
      street: "123 Some Street"
    name: "Mark"
```

The `console.table` method can be passed an array of objects.
In this case the column headings are property names
and each object is output on its own row.
An optional second argument is an array of the property names
to output in the order in which the columns should appear.
Clicking a column heading sorts the rows on the corresponding property value
(except in Firefox).

The following code example demonstrates many of the `console` methods
described in the table above.

```js
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <script>
      function doWork() {
        let sum = 0;
        for (let n = 1; n < 100000; n++) {
          sum += n;
        }
        console.log('doWork: sum =', sum);
      }

      window.onload = () => {
        const b = true;
        const n = 19;
        const p = Math.PI;
        const t = 'some text';
        const o = {b, n, p, t};

        console.assert(n > 20, 'n is too low');
        // outputs "Assertion failed: n is too low"

        console.count('demo'); // outputs "demo: 1"

        console.debug('debug', b, n, p, t);
        // outputs "debug true 19 3.141592653589793 some text"

        console.dir(o);
        // outputs "▶ Object" which can be expanded

        console.dirxml(document.body);
        // outputs "▶<body>...</body>" which can be expanded

        console.error('error', b, n, p, t);
        // outputs "error true 19 3.141592653589793 some text"
        // in pink and red

        console.group('animals');
        console.groupCollapsed('domestic');
        console.log('dog');
        console.log('cat');
        console.groupEnd();
        console.group('wild');
        console.log('lion');
        console.log('tiger');
        console.log('bear');
        console.groupEnd();
        console.groupEnd();
        // outputs the following:
        // ▼ animals (already expanded)
        //   ▶ domestic (can be expanded)
        //   ▼ wild (already expanded)
        //       lion
        //       tiger
        //       bear

        console.info('info', b, n, p, t);
        // outputs "info true 19 3.141592653589793 some text"

        console.log('o = ' + o); // o = [object Object]; not useful

        console.log('o =', o);
        // outputs the following:
        // o = ▶{b: true, n: 19, p: 3.141592653589793, t: "some text"}

        console.log('log', b, n, p, t);
        // outputs "log true 19 3.141592653589793 some text"

        console.log('b=%s, n=%d, pi=%f, t=%s, o=%o', b, n, p, t, o);
        // outputs the following:
        // b=true, n=19, pi=3.141592653589793, t=some text, o=
        // ▶{b: true, n: 19, p: 3.141592653589793, t: "some text"}

        console.table(o);
        // outputs the following table:
        // | (index) | Value      |
        // |---------|------------|
        // |b        |true        |
        // |n        |19          |
        // |p        |3.14159...  |
        // |t        |"some text" |
        // ▶Object (can expand)

        console.count('demo'); // outputs "demo: 2"

        const fruits = [
          {name: 'apple', color: 'red', price: 50},
          {name: 'banana', color: 'yellow', price: 25}
        ];
        // When passed an array of objects,
        // the column headings are property names and
        // each object is output in a different row.
        // The optional second argument limits the
        // properties to output and their order.
        // Click a column heading to sort the rows on that column.
        console.table(fruits, ['price', 'name']);
        // outputs the following table:
        // | (index) | price | name   |
        // |---------|-------|--------|
        // |0        |50     |"apple" |
        // |1        |25     |"banana"|

        console.time('groups');
        doWork();
        // outputs "doWork: sum = 4999950000"
        console.timeLog('groups');
        // outputs "groups: 8.5400390625 ms"
        doWork();
        // outputs "doWork: sum = 4999950000"
        console.timeEnd('groups');
        // outputs "groups: 12.336181640625 ms"

        console.trace('foo', 1, true);
        // outputs "▼foo 1 true" followed by a stack trace

        console.warn('warn', b, n, p, t, o);
        // outputs "warn true 19 3.141592653589793 some text"
        // in yellow and brown

        console.countReset('demo'); // no output

        console.count('demo'); // outputs "demo: 1"
      };
    </script>
  </head>
  <body>
    <p>See output in the DevTools console.</p>
  </body>
</html>
```

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

{% raw %}

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

{% endraw %}

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

### Downloading a File

There is no built-in function for downloading a file
containing generated text.
This is useful in situations like generating CSV.

Here is a function that does this by creating an anchor tag,
configuring it to download a file with a given name,
adding it to the DOM, clicking it, and removing it from the DOM.

```js
function downloadFile(filename, content) {
  const element = document.createElement('a');
  element.setAttribute(
    'href',
    'data:text/plain;charset=utf-8,' + encodeURIComponent(content)
  );
  element.setAttribute('download', filename);
  element.style.display = 'none';
  document.body.appendChild(element);
  element.click();
  document.body.removeChild(element);
}
```

### Intersection Observer API

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API",
"Intersection Observer API" %} enables executing JavaScript code
when an element intersect (or stops intersecting)
an ancestor element (or the browser viewport).
Often the goal is to perform some processing when the user scrolls the page,
bringing certain elements into view.
Use cases include:

- adding animation to certain elements when they scroll into view
- fetching data only when it needs to be displayed
- loading images on when they come in to view

The first use case, adding animation,
is implemented on the page you are reading.
Notice how the color of section headings
temporarily changes when they come into view.

The last use case, lazy loading images, is better achieved in modern browsers
by adding `loading="lazy"` to all `img` elements.
See the "Lazy Loading" section above.

To use the Intersection Observer API:

1. Create an `IntersectionObserver` object by invoking the constructor function
   and passing it a callback function and an options object.
2. Call the `observe` method on the `IntersectionObserver` object,
   passing it a DOM element to observe.

The options object passed to the `IntersectionObserver` constructor function
can contain the following properties:

| Option Property | Description                                                                      |
| --------------- | -------------------------------------------------------------------------------- |
| `root`          | DOM element to use as the viewport                                               |
| `rootMargin`    | distance by which to grow or shrink the viewport used to determine intersections |
| `threshold`     | percentage of an element that must be visible to trigger a change                |

The `root` property defaults to the browser viewport.
Typically this is the desired viewport.

The `rootMargin` property takes the same length values as the `margin` property.
It defaults to `0` on all four sides, but can be specified as:

- a single length to specify the same margin on all four sides
- two lengths to specify equal top and bottom margins
  followed by equal left and right margins
- four lengths to specify different margins on all four sides

The `threshold` property defaults to `0`, which means a change is triggered
when a single pixel becomes visible or all pixels become invisible.
A value of `1` means the entire element must be visible
and a value of `0.5` means half of the element must be visible.
This can also be set to an array of numbers to trigger the callback function
at various percentages of visibility.

The callback function will called whenever the intersection status
of a DOM element being observed changes.
It is passed an array of entry objects and the observer object.

Each entry object in the array contain the following properties:

| Entry Object Property | Description                                                                |
| --------------------- | -------------------------------------------------------------------------- |
| `target`              | DOM element being observed                                                 |
| `isIntersecting`      | boolean indicating whether there is any intersection                       |
| `time`                | `DOMHightResTimeStamp` value indicating when the intersection was detected |
| `boundingClientRect`  | `DOMRect` object describing the `target`                                   |
| `rootBounds`          | `DOMRect` object describing the `root`                                     |
| `intersectionRect`    | `DOMRect` object describing the visible area of the `target`               |
| `intersectionRatio`   | ratio of `intersectionRect` area to `rootBounds` area                      |

Getting the observer object enables a callback
that is used for more than one `IntersectionObserver`
to determine which observer detected a change.
It can also be used to stop observing changes to a given element
by calling `observer.unobserve(domElement)`.

In the following example, scrolling vertically
causes the light blue boxes to come into view and leave view.
When a box comes completely into view,
the text size temporarily increases
and the text color temporarily changes to red.

{% include "_intersection-observer.html" %}

{% raw %}

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

{% endraw %}

### Carousels

Implementing a carousel component is not a difficult as it might seem.
Here is a simple implementation inspired by
{% aTargetBlank "https://www.youtube.com/watch?v=VYsVOamdB0g",
"Kevin Powell's YouTube videos" %}.

{% include "_carousel.html" %}

{% raw %}

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Carousel Demo</title>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style>
      button {
        background-color: transparent;
        border: none;
        cursor: pointer;
        font-size: 3rem;
      }

      .carousel {
        display: flex;
        align-items: center;
        gap: 1rem;
      }

      .center {
        display: flex;
        flex-direction: column;
        align-items: center;
        flex-grow: 1;
      }

      .container {
        --height: 400px;
        background-color: lightgray;
        height: var(--height);
        margin: 0 auto;
        overflow: hidden;
        position: relative;
        width: 100%;
      }

      .dot {
        --size: 1.5rem;

        background-color: lightgray;
        border: none;
        border-radius: 50%;
        height: var(--size);
        width: var(--size);
      }

      .dot.selected {
        background-color: gray;
      }

      img {
        max-height: var(--height);
        position: absolute;
        transition: left 0.5s;
        transform: translate(-50%, 0);
        visibility: hidden; /* initially */
      }

      .left-btn {
        visibility: hidden; /* initially */
      }

      nav {
        display: flex;
        gap: 1rem;
        margin-top: 1rem;
      }
    </style>

    <script>
      const images = [
        'german-shorthaired-pointer.jpg',
        'native-american-indian-dog.jpg',
        'treeing-walker-coonhound.jpg',
        'whippet.jpg'
      ];

      let containerWidth = 0;
      let itemWidth = 0;
      let currentDot;
      let lastIndex = images.length - 1;
      let leftBtn;
      let nav;
      let rightBtn;
      let showIndex = 0;
      let imgElements = [];

      function showNext() {
        if (showIndex === lastIndex) return;
        showIndex++;
        update();
      }

      function showPrevious() {
        if (showIndex === 0) return;
        showIndex--;
        update();
      }

      function update() {
        const maxWidth = Math.max(containerWidth, itemWidth);

        // Slide the images to their new position.
        imgElements.forEach((img, index) => {
          const offset = index - showIndex + 0.5;
          // For the image being shown, we want to center it in the container.
          // For the other images, we need to ensure they are out of view.
          const width = index === showIndex ? containerWidth : maxWidth;
          img.style.left = offset * width + 'px';
        });

        // Update the visibility of the left and right buttons.
        leftBtn.style.visibility = showIndex === 0 ? 'hidden' : 'visible';
        rightBtn.style.visibility =
          showIndex === lastIndex ? 'hidden' : 'visible';

        // Select the correct dot.
        currentDot.classList.remove('selected');
        currentDot = nav.querySelector(`button:nth-of-type(${showIndex + 1})`);
        currentDot.classList.add('selected');
      }

      window.onload = () => {
        const carousel = document.querySelector('.carousel');

        leftBtn = carousel.querySelector('.left-btn');
        leftBtn.addEventListener('click', showPrevious);

        rightBtn = carousel.querySelector('.right-btn');
        rightBtn.addEventListener('click', showNext);

        nav = carousel.querySelector('nav');

        const container = carousel.querySelector('.container');
        containerWidth = container.getBoundingClientRect().width;

        let loadCount = 0;

        images.forEach((image, index) => {
          // Create and add a dot for each image.
          const dot = document.createElement('button');
          dot.classList.add('dot');
          nav.appendChild(dot);
          if (index === 0) currentDot = dot;
          dot.addEventListener('click', () => {
            showIndex = index;
            update();
          });

          // Create and add an img element for each image.
          const img = document.createElement('img');
          img.setAttribute('src', 'images/' + image);
          container.appendChild(img);
          imgElements.push(img);

          // Wait for the image to load so we can get its width.
          img.onload = () => {
            const {width} = img.getBoundingClientRect();
            if (width > itemWidth) itemWidth = width;

            // After the last image has loaded,
            // slide them into position and make them visible.
            loadCount++;
            if (loadCount === images.length) {
              update();
              imgElements.forEach(img => (img.style.visibility = 'visible'));
            }
          };
        });
      };
    </script>
  </head>
  <body>
    <div class="carousel">
      <button class="left-btn">&lt;</button>
      <div class="center">
        <div class="container"></div>
        <nav></nav>
      </div>
      <button class="right-btn">&gt;</button>
    </div>
  </body>
</html>
```

{% endraw %}

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
A locale and an options object can optionally
be passed to the constructor functions.
If neither is passed, the browser default locale is used.
If options are required then a locale must be specified.

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

const locale = 'en-US';
formatter = new Intl.DateTimeFormat(locale, {
  dateStyle: 'short',
  timeStyle: 'short'
});
console.log(formatter.format(date)); // 4/16/61 2:19 PM

const dateStyles = ['full', 'long', 'medium', 'short'];
for (const dateStyle of dateStyles) {
  formatter = new Intl.DateTimeFormat(locale, {dateStyle});
  console.log(formatter.format(date));
}
// full -> Sunday, April 16, 1961
// long -> April 16, 1961
// medium -> Apr 16, 1961
// short (default) -> 4/16/61

const timeStyles = ['full', 'long', 'medium', 'short'];
for (const timeStyle of timeStyles) {
  formatter = new Intl.DateTimeFormat(locale, {timeStyle});
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
formatter = new Intl.DateTimeFormat(locale, {
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

- bash: `alias nr="npm run"`
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

### VS Code

{% aTargetBlank "https://code.visualstudio.com", "VS Code" %}
is a very popular, free code editor from Microsoft.
It has many built-in features and more can be added through extensions.

Recommended VS Code extensions include:

- {% aTargetBlank "https://github.com/CoenraadS/Bracket-Pair-Colorizer-2", "Bracket Pair Colorizer 2" %}

  This colorizes matching parentheses, square brackets, and curly braces
  to make it easier to find matching pairs.

- {% aTargetBlank "https://github.com/streetsidesoftware/vscode-spell-checker", "Code Spell Checker" %}

  This performs spell checking in source code considering the parts of camelCase names separately.
  It enables adding words to the user dictionary and workspace dictionaries.
  Spell checking can be disabled and enabled within a file
  and a list of words to ignore can be specified with special comments.

- {% aTargetBlank "https://github.com/kamikillerto/vscode-colorize", "colorize" %}

  This adds color swatches before all CSS colors to visualize them.

- {% aTargetBlank "https://github.com/Microsoft/vscode-eslint", "ESLint" %}

  This integrates the
  {% aTargetBlank "https://eslint.org", "ESLint" %} tool
  for reporting problems in JavaScript and TypeScript files.

- {% aTargetBlank "https://github.com/eamodio/vscode-gitlens", "GitLens" %}

  This adds Git capabilities including:

  - revision navigation,
  - current line blame in faint text at the end of the current line
    (Hover over this text for more detail.)
  - new views in the "SOURCE CONTROL" sidebar including
    COMMITS, REPOSITORIES, FILE HISTORY, BRANCHES, REMOTES, STASHES, and TAGS.
  - git command palette
  - interactive rebase editor
  - and much more

- {% aTargetBlank "https://github.com/ritwickdey/vscode-live-server", "Live Server" %}

  This launches a local development server with live reload.
  It is described in more detail below.

- {% aTargetBlank "https://github.com/prettier/prettier-vscode", "Prettier" %}

  This integrates the
  {% aTargetBlank "https://prettier.io", "Prettier" %} code formatter
  and is typically configured to run when file changes are saved.

- {% aTargetBlank "https://github.com/stylelint/vscode-stylelint", "stylelint" %}

  This integrates the
  {% aTargetBlank "https://stylelint.io", "stylelint" %} tool
  for reporting problems in CSS and Sass files.

- {% aTargetBlank "https://github.com/rangav/thunder-client-support", "Thunder Client" %}

  This is a lightweight Rest API client, similar to
  {% aTargetBlank "https://www.postman.com", "Postman" %}.
  For more detail, see this [blog page](/blog/thunder-client/).

- {% aTargetBlank "https://github.com/Gruntfuggly/todo-tree", "Todo Tree" %}

  This searches your workspace for comment tags like "TODO" and "FIXME".
  It then displays them in a tree view in the EXPLORER pane.
  To navigate to the source file and line of a TODO, click a line in the tree.

- {% aTargetBlank "https://gitlab.com/versionlens/vscode-versionlens", "Version Lens" %}

  When viewing dependencies in a `package.json` file, this shows the latest version of each and enables updating to that version.

- {% aTargetBlank "https://github.com/VSCodeVim/Vim", "Vim" %}

  This provides {% aTargetBlank "https://www.vim.org", "Vim" %} emulation, including support for Vim macros.
  Use this if you value efficient text editing!

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

### Live Server VS Code extension

{% aTargetBlank "https://ritwickdey.github.io/vscode-live-server/",
"Live Server" %} is a VS Code extension that
starts a local file server and watches files for changes,
reloading a browser page when detected.
This is great for experimenting with HTML, CSS, and JavaScript
changes to a site that does not require a build step.

To install it, click the extensions icon in the left nav,
enter "live server" in the search input, and
click the install button in the entry for "Live Server" by Ritwick Dey.

To use it, open a folder/directory in VS Code
that includes an HTML file and open an HTML file.
Then use one of these approaches to start the server:

1. Right-click the pane displaying the HTML and select "Open with Live Server".
1. Open the command palette and select "Live Server: Open with Live Server".

Each of these will launch a local file server,
open the file in your default web browser, and
add "⍉ Port: 5500" to the status bar
at the bottom of the VS Code window.

To stop the server, use one of these approaches:

1. Click the area added to the status bar described above.
1. Right-click the pane displaying the HTML and select "Stop Live Server".
1. Open the command palette and select "Live Server: Stop Live Server".

The status bar area will change to "Go Live"
which can then be clicked to restart the server.

To change the default port that is used (5500)
specify the port in the `liveServer.settings.port` setting.

To use a web browser other than your default,
specify it in the `liveserver.settings.CustomBrowser` setting.
Options include `chrome`, `chrome:PrivateMode`,
`firefox`, `firefox:PrivateMode`, `microsoft-edge`, and `blisk`.

{% aTargetBlank "https://blisk.io", "Blisk" %}
is a Chromium-based, developer-oriented web browser
for testing various device types including desktop, tablet, and mobile.

Live Server supports many more {% aTargetBlank
"https://github.com/ritwickdey/vscode-live-server/blob/master/docs/settings.md", "settings" %}.

## Miscellaneous

### Local HTTP File Server

There are many tools for serving local files over HTTP.
This useful for testing HTML, CSS, and JavaScript in a web browser.
One option is {% aTargetBlank "https://github.com/http-party/http-server", "http-server" %}
in npm.

To use this, enter `npx http-server` and note the port number it selects.
Then browse `localhost:{port-number}`.

### Kill Listening Port

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
