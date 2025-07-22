---
eleventyNavigation:
  key: wrec
layout: topic-layout.njk
---

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

<script src="/blog/js/basic-wrec.js" type="module"></script>

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

To use this in a web page or Markdown file, include the following:

```html
<script src="some-path/hello-world.js" type="module"></script>
<hello-world></hello-world>
<hello-world name="wrec"></hello-world>
```

Here it is in action.

<script src="/blog/js/hello-world.js" type="module"></script>

<hello-world></hello-world>
<hello-world name="wrec"></hello-world>

Using your browser DevTools,
inspect the last instance of the `hello-world` custom element.
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

<script src="/blog/js/counter-wrec.js" type="module"></script>

<counter-wrec></counter-wrec>

Click the "+" and "-" buttons to try it.

## Two-way Data Binding

Wrec provides two-way data binding for
HTML `input`, `textarea`, and `select` elements.
When the user changes their value,
an associated property is automatically updated.
When code changes the value of an associated property,
the element is automatically updated.

The following web component demonstrates this.
Ignore the CSS variable `--label-width` for now.
We will discuss how that works later.

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

<script src="/blog/js/number-slider.js" type="module"></script>

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

The following color picker component demonstrates this,
along with defining a computed property.

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

<script src="/blog/js/color-picker.js" type="module"></script>

<color-picker></color-picker>

Drag the sliders to change the color of the swatch on the left.

## Nested Web Components

Let's take this a step farther and define a web component
that uses `color-picker` to change the color of some text.
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

<script src="/blog/js/color-demo.js" type="module"></script>

<color-demo></color-demo>
