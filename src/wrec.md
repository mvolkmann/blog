---
eleventyNavigation:
  key: Web Reactive Components (wrec)
layout: topic-layout.njk
---

<script src="/blog/js/basic-wrec.js" type="module"></script>
<script src="/blog/js/color-demo.js" type="module"></script>
<script src="/blog/js/color-picker.js" type="module"></script>
<script src="/blog/js/counter-wrec.js" type="module"></script>
<script src="/blog/js/data-binding.js" type="module"></script>
<script src="/blog/js/hello-world.js" type="module"></script>
<script src="/blog/js/radio-group.js" type="module"></script>
<script src="/blog/js/select-list.js" type="module"></script>
<script src="/blog/js/number-slider.js" type="module"></script>

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="wrec logo" style="border: 0"
    src="/blog/assets/wrec-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://www.npmjs.com/package/wrec", "wrec" %} is a small,
zero dependency library that greatly simplifies building web components.
Its main features are that it automates
wiring event listeners and implementing reactivity.

To install it in one of your projects, enter `npm install wrec`.

## Getting Started

To define a web component using wrec:

1. Define a class that extends the `Wrec` class.
1. Optionally define a static property named `css`
   whose value is a string containing CSS rules.
1. Define a static property named `html`
   whose value is a string containing the HTML to render.
1. Register the class as a custom element definition
   by calling the `register` method.

For example:

```js
import Wrec, {css, html} from './wrec.js';

class BasicWrec extends Wrec {
  static css = css`
    span {
      font-family: fantasy;
      font-size: 2rem;
    }
  `;
  static html = html`<span>Hello, World!</span>`;
}

BasicWrec.register();
```

The `css` and `html` properties above use tagged template literals.
This allows the text to span multiple lines.
The tags `css` and `html` are optional.
They trigger the VS Code extension Prettier to format the code
and the es6-string-html extension to add syntax highlighting.

The `register` method registers a custom HTML element
whose name is the kebab-case version of the class name.
For `BasicWrec`, the element name is `basic-wrec`.

To use this in a web page or Markdown file, include the following:

```html
<script src="some-path/basic-wrec.js" type="module"></script>
<basic-wrec></basic-wrec>
```

Here it is in action.

<basic-wrec></basic-wrec>

## Properties

Web components defined with wrec can define and use properties.
Properties are automatically mapped to attributes in the custom element.
Here's a simple example that enables specifying a name.

```js
import Wrec, {html} from './wrec.js';

class HelloWorld extends Wrec {
  static properties = {
    name: {type: String, value: 'World'}
  };

  static html = html`<div>Hello, <span>this.name</span>!</div>`;
}

HelloWorld.register();
```

We can use this custom element as follows:

```html
<hello-world></hello-world>

<hello-world name="wrec"></hello-world>
```

This will render the following:

<hello-world></hello-world>
<hello-world name="wrec"></hello-world>

Use your browser DevTools to inspect the
last instance of the `hello-world` custom element.
Double-click the value of the `name` attribute and change it to your name.
Press the return key or tab key, or click away from the value
to commit the change.
Note how the page updates to greet you.

## JavaScript Expressions

The attributes and text content of the HTML to be rendered
can contain raw JavaScript expressions.
By "raw" we mean that the expressions
are not surrounded by noisy syntax like `${...}`.

If the expressions contain references to properties in the form
`this.propertyName`, wrec automatically watches them for changes.
When changes are detected, wrec automatically reevaluates the expressions
and replaces the attribute values or text contents with new values.

Here's an example of a counter component that takes advantage of this feature:

```js
import Wrec, {css, html} from './wrec.js';

class CounterWrec extends Wrec {
  static properties = {
    count: {type: Number}
  };

  static css = css`
    :host {
      display: block;
    }
    button {
      background-color: lightgreen;
    }
    button:disabled {
      background-color: gray;
    }
  `;

  static html = html`
    <button disabled="this.count === 0" onClick="this.count--" type="button">
      -
    </button>
    <span>this.count</span>
    <button onClick="this.count++" type="button">+</button>
  `;
}

CounterWrec.register();
```

Here it is in action.

<counter-wrec></counter-wrec>

Click the "+" and "-" buttons to try it.

It is highly unlikely that an attribute value or element text content
will ever need to render the word "this", followed by a period,
followed by a valid JavaScript identifier.
But if that need arises, just escape the period by using two.
Wrec will render only a single period.

To follow the word "this" with an ellipsis,
include a space before it as in "this ... and that".

## Two-way Data Binding

Wrec provides two-way data binding for
HTML `input`, `textarea`, and `select` elements.
When the user changes their value,
an associated property is automatically updated.
When code changes the value of an associated property,
the element is automatically updated.

The following web component demonstrates this.
Ignore the CSS variable `--label-width` for now.
We will discuss how that works in the "Dynamic CSS" section below.

```js
import Wrec, {css, html} from './wrec.js';

class NumberSlider extends Wrec {
  static properties = {
    label: {type: String},
    labelWidth: {type: String},
    max: {type: Number, value: 100},
    min: {type: Number, value: 0},
    value: {type: Number}
  };

  static css = css`
    :host {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    input[type='number'] {
      width: 6rem;
    }

    label {
      --label-width: this.labelWidth;
      font-weight: bold;
      text-align: right;
      width: var(--label-width);
    }
  `;

  static html = html`
    <label>this.label</label>
    <input type="range" min="this.min" max="this.max" value="this.value" />
    <span>this.value</span>
  `;
}

NumberSlider.register();
```

Here it is in action.

<number-slider label="Rating" max="10"></number-slider>

Drag the slider thumb to change the value.

## Dynamic CSS

Wrec also supports JavaScript expressions in CSS,
but there's a wrinkle.
When the browsers built-in HTML parser sees a JavaScript expression
in an attribute or the text content of an element, it is ignored.
This allows wrec to process the expressions.
But when the browsers built-in CSS parser sees a JavaScript expression
in a property value, it sees it as invalid and strips it out.
This prevents wrec from processing the expressions.

A workaround is to use CSS properties (a.k.a. CSS variables).

The following color picker component demonstrates this.
It also defines a computed property whose value
can be any valid JavaScript expression.

```js
import Wrec, {css, html} from '../wrec.js';

class ColorPicker extends Wrec {
  static properties = {
    labelWidth: {type: String, value: '3rem'},
    red: {type: Number},
    green: {type: Number},
    blue: {type: Number},
    color: {
      type: String,
      computed: '`rgb(${this.red}, ${this.green}, ${this.blue})`'
    }
  };

  static css = css`
    :host {
      display: flex;
      gap: 0.5rem;
    }

    #sliders {
      display: flex;
      flex-direction: column;
      justify-content: space-between;
    }

    #swatch {
      --color: this.color;
      background-color: var(--color);
      height: 5rem;
      width: 5rem;
    }
  `;

  static html = html`
    <div id="swatch"></div>
    <div id="sliders">
      <!-- Prettier formatted these lines poorly. -->
      ${this.makeSlider('Red')} ${this.makeSlider('Green')} ${this.makeSlider(
        'Blue'
      )}
    </div>
  `;

  static makeSlider(label) {
    return html`
      <number-slider
        label=${label}
        label-width="this.labelWidth"
        max="255"
        value="this.${label.toLowerCase()}"
      ></number-slider>
    `;
  }
}

ColorPicker.register();
```

Here it is in action.

<color-picker></color-picker>

Drag the sliders to change the color of the swatch on the left.

## Nested Web Components

Let's define a web component that uses `color-picker`
to change the color of some text.
It also uses a `number-slider` to change the size of the text.

```js
import Wrec, {css, html} from './wrec.js';

class ColorDemo extends Wrec {
  static properties = {
    color: {type: String},
    size: {type: Number, value: 18}
  };

  static css = css`
    :host {
      --color: this.color;
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
      font-family: sans-serif;
    }
    p {
      --size: this.size;
      color: var(--color);
      font-size: calc(var(--size) * 1px);
    }
  `;

  static html = html`
    <color-picker color="this.color"></color-picker>
    <number-slider
      label="Size"
      max="48"
      min="12"
      value="this.size"
    ></number-slider>
    <p>This is a test.</p>
  `;
}

ColorDemo.register();
```

Here it is in action.

<color-demo></color-demo>

## Kicking it up a Notch

For this demo we need to define three more custom elements which are:

- `radio-group`: renders a set of radio buttons which are `input` elements with `type="radio"`
- `select-list`: renders a `select` element with `option` children
- `data-binding`: renders elements that tie everything together

We will also use `number-slider` which was defined above.

Here is the class that defines the `radio-group` custom element:

```js
import Wrec, {css, html} from './wrec.js';

class RadioGroup extends Wrec {
  static formAssociated = true;

  static properties = {
    labels: {type: String},
    name: {type: String, required: true},
    values: {type: String, required: true},
    value: {type: String}
  };

  static css = css`
    :host > div {
      display: flex;
      gap: 0.5rem;

      > div {
        display: flex;
        align-items: center;
      }
    }
  `;

  static html = html`
    <div>
      this.values.split(",").map((value, index) => this.makeRadio(value, index,
      this.labels )).join("")
    </div>
  `;

  connectedCallback() {
    super.connectedCallback();
    if (!this.value) this.value = this.values.split(',')[0];
    this.#fixValue();
  }

  attributeChangedCallback(attrName, oldValue, newValue) {
    super.attributeChangedCallback(attrName, oldValue, newValue);
    if (attrName === 'value') {
      // Update the checked state of the radio buttons.
      const inputs = this.shadowRoot.querySelectorAll('input');
      for (const input of inputs) {
        input.checked = input.value === newValue;
      }
    } else if (attrName === 'values') {
      this.#fixValue();
    }
  }

  // This handles the case when the specified value
  // is not in the list of values.
  #fixValue() {
    requestAnimationFrame(() => {
      const values = this.values.split(',');
      if (!values.includes(this.value)) this.value = values[0];
    });
  }

  handleChange(event) {
    this.value = event.target.value;
  }

  makeRadio(value, index) {
    let label = this.labels.split(',')[index];
    if (!label) return '';
    value = value.trim();
    return html`
      <div>
        <input
          type="radio"
          id="${value}"
          name="${this.name}"
          onchange="handleChange"
          value="${value}"
          ${value === this.value ? 'checked' : ''}
        />
        <label for="${value}">${label}</label>
      </div>
    `;
  }
}

RadioGroup.register();
```

Here is the class that defines the `select-list` custom element:

```js
import Wrec, {html} from './wrec.js';

class SelectList extends Wrec {
  static formAssociated = true;

  static properties = {
    name: {type: String, required: true},
    labels: {type: String},
    values: {type: String, required: true},
    value: {type: String}
  };

  static html = html`
    <select name="${this.name}" value="this.value">
      this.values.split(",").map((value, index) => this.makeOption(value, index,
      this.labels)).join("")
    </select>
  `;

  connectedCallback() {
    super.connectedCallback();

    // Wait for the DOM to update.
    requestAnimationFrame(() => {
      const values = this.values.split(',');
      if (!values.includes(this.value)) this.value = values[0];
    });
  }

  makeOption(value, index) {
    let label = this.labels.split(',')[index];
    if (!label) return '';
    value = value.trim();
    return html`<option value="${value}">${label}</option>`;
  }
}

SelectList.register();
```

Here is the class that defines the `data-binding` custom element.

The `label` property is a computed property that
calls a method in the class to obtain its value.
Since the method uses properties that don't appear in the expression,
we need to let wrec know which properties the function uses.
The `uses` property value is a comma-separated list of properties names.
When the value of any of those properties changes,
the expression is reevaluated and
a new value is assigned to the computed property.

```js
import Wrec, {css, html} from './wrec.js';

const capitalize = str =>
  str ? str.charAt(0).toUpperCase() + str.slice(1) : str;

class DataBinding extends Wrec {
  static properties = {
    color: {type: String},
    colors: {type: String, required: true},
    labels: {
      type: String,
      computed: 'this.getLabels()',
      uses: 'colors'
    },
    size: {type: Number, value: 18}
  };

  static css = css`
    :host {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
      font-family: sans-serif;
    }
    p {
      --color: this.color;
      --size: this.size;
      color: var(--color);
      font-size: calc(var(--size) * 1px);
      margin: 6px 0;
    }
  `;

  static html = html`
    <div>
      <label>Color Options (comma-separated):</label>
      <input value="this.colors" />
    </div>
    <radio-group
      name="color1"
      labels="this.labels"
      value="this.color"
      values="this.colors"
    ></radio-group>
    <select-list
      name="color2"
      labels="this.labels"
      value="this.color"
      values="this.colors"
    ></select-list>
    <number-slider
      label="Size"
      max="48"
      min="12"
      value="this.size"
    ></number-slider>
    <p>You selected the color <span id="selected-color">this.color</span>.</p>
  `;

  getLabels() {
    return this.colors
      .split(',')
      .map(color => capitalize(color))
      .join(',');
  }
}

DataBinding.register();
```

Finally, here it is in action.

<data-binding color="blue" colors="red,green,blue"></data-binding>

Select one of the radio buttons and
note how the color of the text at the bottom updates.
Also, the corresponding `option` is selected in the `select` element.

Select a different color in the `select-list` and
note how the color of the text at the bottom updates.
Also, the corresponding radio button is selected.

Drag the "Size" slider to change the size of the text at the bottom.

For the most amazing part,
change the comma-separated list of colors in the input at the top.
Notice how the radio buttons and the select options update.
The first color in the list is selected by default.
Selecting other colors via the radio buttons or the `select` works as before.

Take a moment to review the code above that implements these web components.
Consider how much code would be required to reproduce this
using another library or framework and
how much more complicated that code would be!
