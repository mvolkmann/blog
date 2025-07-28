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
<script src="/blog/js/number-slider.js" type="module"></script>
<script src="/blog/js/radio-group.js" type="module"></script>
<script src="/blog/js/rectangle-area.js" type="module"></script>
<script src="/blog/js/select-list.js" type="module"></script>
<script src="/blog/js/temperature-eval.js" type="module"></script>
<script src="/blog/js/toggle-switch.js" type="module"></script>

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

If you're new to web components, see my slides
from a one-hour talk on web components at {% aTargetBlank
"https://github.com/mvolkmann/talks/blob/master/web-components.key.pdf",
"Web Components" %}.
A video of the talk is at
<a href="https://drive.google.com/file/d/16rGM2L8psBGlQ-Zhu6EBXN1GMO30YdRF/view"
target="_blank">OCI Tech Lunch - July 2025</a>.
Also see my series of
<a href="https://www.youtube.com/playlist?list=PLGhglgQb4jVk3-_wc8srORlGalSRFMEpR"
target="_blank">YouTube videos</a> on web components and the wrec library.

Wrec was inspired by {% aTargetBlank "https://lit.dev", "Lit" %}.
It has the following advantages over Lit:

- Wrec is simpler ... just a single class to extend (Wrec).
- Wrec is smaller ... 7K (`wrec.min.js`) versus
  16K (lit-core.min.js`) minified.
- Wrec has a cleaner syntax ... no need to
  surround JS expressions with `${...}`.
- Wrec provides automatic 2-way data binding ...
  no need to dispatch custom events and listen for them.
- Wrec performs direct DOM manipulation, which is
  more efficient than the virtual DOM approach used in Lit
- Wrec doesn't require a special syntax for Boolean attributes.
- Wrec enables specifying the content of a `textarea` element
  with a JavaScript expression in its text content.

Wrec components have many of the features provided by Alpine.js.

To install wrec in one of your projects, enter `npm install wrec`.

## Getting Started

To define a web component using wrec:

1. Copy the file `wrec.min.js` from the wrec
   [GitHub](https://github.com/mvolkmann/wrec/tree/main/dist)
   repository.
   (Alternatively, install wrec using npm and use a bundler like Vite.)
1. Define a class that extends the `Wrec` class.
1. Optionally define a static property named `css`
   whose value is a string containing CSS rules.
1. Define a static property named `html`
   whose value is a string containing the HTML to render.
1. Register the class as a custom element definition
   by calling the `register` method.

For example:

```js
import Wrec, {css, html} from './wrec.min.js';

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
import Wrec, {html} from './wrec.min.js';

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

## Event Listeners

To wire event listeners,
Wrec looks for attributes whose name begins with "on".
It assumes the remainder of the attribute name is an event name.
It also assumes that the value of the attribute is either
a method name that should be called or code that should be executed
when that event is dispatched.
For example, with the attribute `onclick="increment"`,
if `increment` is a method in the component, wrec will
add an event listener to the element containing the attribute
for "click" events and call `this.increment(event)`.
Alternatively, the attribute `onclick="this.count++"`
adds an event listener that increments `this.count`
when the element is clicked.

The case of the event name within the attribute name
does not matter because Wrec lowercases the name.
So the attributes in the previous examples
can be replaced by `onClick="increment"`.

## JavaScript Expressions

The attributes and text content of the HTML to be rendered
can contain raw JavaScript expressions.
By "raw" we mean that the expressions
are not surrounded by noisy syntax like `${...}`.

If the expressions contain references to properties in the form
`this.propertyName`, wrec automatically watches them for changes.
In this context, `this` always refers to the parent web component.
When changes are detected, wrec automatically reevaluates the expressions
and replaces the attribute values or text contents with new values.
Wrec does not rerender the entire web component.

Here's an example of a counter component that takes advantage of this feature:

```js
import Wrec, {css, html} from './wrec.min.js';

class CounterWrec extends Wrec {
  static properties = {
    label: {type: String},
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
      opacity: 0.8;
    }
  `;

  static html = html`
    <label>this.label</label>
    <button onClick="this.count--" type="button" disabled="this.count === 0">
      -
    </button>
    <span>this.count</span>
    <button onClick="this.count++" type="button">+</button>
  `;
}

CounterWrec.register();
```

When the value of an attribute is a Boolean,
wrec adds the attribute to the element with no value
or removes the attribute from the element.
This is commonly used for attributes like `disabled`.

Here it is in action.

```html
<counter-wrec label="Score" count="0"></counter-wrec>
```

<counter-wrec label="Score" count="0"></counter-wrec>

Click the "+" and "-" buttons to try it.

It is highly unlikely that an attribute value or element text content
will ever need to render the word "this", followed by a period,
followed by a valid JavaScript identifier.
But if that need arises, just escape the period by using two.
Wrec will render only a single period.

To follow the word "this" with an ellipsis,
include a space before it as in "this ... and that".

## Unchanging Expressions

In insert the value of an expression
that does not use properties of the web component,
into an HTML template string,
surround the expression with the syntax `${...}`.
For example, assuming `DAYS` is a variable
whose value is an array of month names:

```html
<p>The month is ${DAYS[new Date().getDay()]}.</p>
```

## Conditional and Iterative HTML Generation

Wrec supports conditional and iterative generation of HTML.

The following web component demonstrates conditional generation
using the ternary operator.

```js
import Wrec, {html} from './wrec.min.js';

class TemperatureEval extends Wrec {
  static properties = {
    temperature: {type: Number}
  };

  static html = html`
    <p>this.temperature < 32 ? "freezing" : "not freezing"</p>
  `;
}

TemperatureEval.register();
```

Here it is in action.

```html
<temperature-eval temperature="100"></temperature-eval>
```

<temperature-eval temperature="100"></temperature-eval>

Use your browser DevTools to inspect the
instance of the `temperature-eval` custom element.
Double-click the value of the `temperature` attribute and change it "20".
Note how the rendered output changes from "not freezing" to "freezing".

For an example of a web component that iterates over values
in a comma-delimited attribute value to determine what to render,
see the `RadioGroup` and `SelectList` classes
in the "Kicking it up a Notch" section below.

## Form Elements

Wrec supports two-way data binding for HTML form elements.

- `input` and `select` elements can have a `value` attribute
  whose value is "this.somePropertyName".
  An event listener for "change" events will be added.
  To instead listen for "input" events, use the attribute "value:input".
- `textarea` elements can have text content
  that is "this.somePropertyName".
  An event listener for "change" events will be added.

When the user changes the value of these form elements,
the associated property is automatically updated.
When code changes the value of an associated property,
the form element is automatically updated.

The following web component demonstrates this.
Ignore the CSS variable `--label-width` for now.
We will discuss how that works in the "Reactive CSS" section below.

```js
import Wrec, {css, html} from './wrec.min.js';

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
    <input
      type="range"
      min="this.min"
      max="this.max"
      value:input="this.value"
    />
    <span>this.value</span>
  `;
}

NumberSlider.register();
```

Here it is in action.

```html
<number-slider label="Rating" max="10"></number-slider>
```

<number-slider label="Rating" max="10"></number-slider>

Drag the slider thumb to change the value.

Data binding in Lit is not two-way like in wrec.
A Lit component cannot simply pass one of its properties to
a child Lit component and have the child can update the property.
The child must dispatch custom events that
the parent listens for so it can update its own state.
For an example of this, see
[wrec-compare](https://github.com/mvolkmann/lit-examples/blob/main/wrec-compare/binding-demo.ts).

## Computed Properties

The value of a property can be computed using the values of other properties.
To do this, add the `computed` attribute to the description of the property.

The example component below has a computed property
that compute the area of a rectangle.
It shows three ways to accomplish this, with the first two commented out.

```js
import Wrec, {css, html} from './wrec.min.js';

class RectangleArea extends Wrec {
  static properties = {
    width: {type: Number, value: 10},
    height: {type: Number, value: 5},
    /*
    area: {
      type: Number,
      computed: "this.width * this.height",
    },
    area: {
      type: Number,
      computed: "this.rectangleArea(this.width, this.height)",
    },
    */
    area: {
      type: Number,
      computed: 'this.rectangleArea()',
      uses: 'width,height'
    }
  };

  static css = css`
    .area {
      font-weight: bold;
    }
  `;

  static html = html`
    <number-slider label="Width" value="this.width"></number-slider>
    <number-slider label="Height" value="this.height"></number-slider>
    <div class="area">Area: <span>this.area</span></div>
  `;

  /*
  rectangleArea(width, height) {
    return width * height;
  }
  */
  rectangleArea() {
    return this.width * this.height;
  }
}

RectangleArea.register();
```

Since the `rectangleArea` method uses properties
that don't appear in the expression,
we need to let wrec know which properties the method uses.
The `uses` property value is a comma-separated list of properties names.
When the value of any of those properties changes,
the expression is reevaluated and
a new value is assigned to the computed property.

Here it is in action:

```html
<rectangle-area></rectangle-area>
```

<rectangle-area></rectangle-area>

Drag the "Width" and "Height" sliders.
Note how the "Area" is automatically updated.

## Reactive CSS

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
import Wrec, {css, html} from './wrec.min.js';

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
      <!-- prettier-ignore -->
      ${this.makeSlider('Red')}
      ${this.makeSlider('Green')}
      ${this.makeSlider('Blue')}
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

```html
<color-picker></color-picker>
```

<color-picker></color-picker>

Drag the sliders to change the color of the swatch on the left.

## Nested Web Components

Let's define a web component that uses `color-picker`
to change the color of some text.
It also uses a `number-slider` to change the size of the text.

```js
import Wrec, {css, html} from './wrec.min.js';

class ColorDemo extends Wrec {
  static properties = {
    color: {type: String},
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

```html
<color-demo></color-demo>
```

<color-demo></color-demo>

CSS variable values can be any valid JavaScript expression.
The example above can be changed to double the size as follows:

```css
--size: this.size * 2;
```

## Kicking it up a Notch

For this demo we need to define three more custom elements which are:

- `radio-group`: renders a set of radio buttons which are `input` elements with `type="radio"`
- `select-list`: renders a `select` element with `option` children
- `data-binding`: renders elements that tie everything together

We will also use `number-slider` which was defined above.

Here is the class that defines the `radio-group` custom element.
Note how properties that are mapped to required attributes,
such as `name` and `values` below, specify that with `required: true`.

```js
import Wrec, {css, html} from './wrec.min.js';

class RadioGroup extends Wrec {
  static formAssociated = true;

  static properties = {
    labels: {type: String, required: true},
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
      <!-- prettier-ignore -->
      this.values
        .split(",")
        .map(this.makeRadio.bind(this))
        .join("")
    </div>
  `;

  #labelArray = [];

  connectedCallback() {
    super.connectedCallback();
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
    } else if (attrName === 'labels') {
      this.#labelArray = this.labels.split(',');
    } else if (attrName === 'values') {
      this.#fixValue();
    }
  }

  // This handles the case when the specified value
  // is not in the list of values.
  #fixValue() {
    requestAnimationFrame(() => {
      const values = this.values.split(',');
      if (this.value) {
        if (!values.includes(this.value)) this.value = values[0];
      } else {
        this.value = values[0];
      }
    });
  }

  // This method cannot be private because it is called when
  // a change event is dispatched from a radio button.
  handleChange(event) {
    this.value = event.target.value;
  }

  // This method cannot be private because it is
  // called from the expression in the html method.
  makeRadio(value, index) {
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
        <label for="${value}">${this.#labelArray[index]}</label>
      </div>
    `;
  }
}

RadioGroup.register();
```

Here is the class that defines the `select-list` custom element:

```js
import Wrec, {html} from './wrec.min.js';

class SelectList extends Wrec {
  static formAssociated = true;

  static properties = {
    name: {type: String, required: true},
    labels: {type: String, required: true},
    values: {type: String, required: true},
    value: {type: String}
  };

  static html = html`
    <select name="${this.name}" value="this.value">
      <!-- prettier-ignore -->
      this.values
        .split(",")
        .map(this.makeOption.bind(this))
        .join("")
    </select>
  `;

  #labelArray = [];

  connectedCallback() {
    super.connectedCallback();
    this.#fixValue();
  }

  attributeChangedCallback(attrName, oldValue, newValue) {
    super.attributeChangedCallback(attrName, oldValue, newValue);
    if (attrName === 'labels') {
      this.#labelArray = this.labels.split(',');
    }
  }

  // This handles the case when the specified value
  // is not in the list of values.
  #fixValue() {
    requestAnimationFrame(() => {
      const values = this.values.split(',');
      if (this.value) {
        if (!values.includes(this.value)) this.value = values[0];
      } else {
        this.value = values[0];
      }
    });
  }

  // This method cannot be private because it is
  // called from the expression in the html method.
  makeOption(value, index) {
    return html`
      <option value="${value.trim()}">${this.#labelArray[index]}</option>
    `;
  }
}

SelectList.register();
```

Here is the class that defines the `data-binding` custom element.

The `label` property is a computed property that
calls a method in the class to obtain its value.

```js
import Wrec, {css, html} from './wrec.min.js';

const capitalize = str =>
  str ? str.charAt(0).toUpperCase() + str.slice(1) : str;

class DataBinding extends Wrec {
  static properties = {
    color: {type: String},
    colors: {type: String, required: true},
    labels: {
      type: String,
      //computed: "this.colors.split(',').map(color => this.capitalize(color)).join(',')",
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
    return this.colors.split(',').map(capitalize).join(',');
  }
}

DataBinding.register();
```

Finally, here it is in action.

```html
<data-binding color="blue" colors="red,green,blue"></data-binding>
```

<data-binding color="blue" colors="red,green,blue"></data-binding>

Select one of the radio buttons and
note how the color of the text at the bottom updates.
Also, the corresponding `option` is selected in the `select` element.

Select a different color in the `select-list` and
note how the color of the text at the bottom updates.
Also, the corresponding radio button is selected.

Drag the "Size" slider to change the size of the text at the bottom.

For the most amazing part,
change the comma-separated list of colors in the input at the top
and press the return key to commit the change.
Notice how the radio buttons and the select options update.
The first color in the list is selected by default.
Selecting other colors via the radio buttons or the `select` works as before.

Take a moment to review the code above that implements these web components.
Consider how much code would be required to reproduce this
using another library or framework and
how much more complicated that code would be!

## Property Change Events

Wrec components will dispatch "change" events whenever
a property configured with `dispatch: true` changes.
For an example of this,
see the `checked` property in `examples/toggle-switch.js`.
The component defined in `examples/binding-demo.js`
listens for that event, as does the `script` in `examples/index.html`.

The following web component implements a toggle switch.
The code was generated by ChatGPT using the "o3 pro" model,
and then modified.

A "change" event is dispatched each time
the value of the `checked` property changes.

```js
import Wrec, {css, html} from './wrec.min.js';

class ToggleSwitch extends Wrec {
  static properties = {
    checked: {type: Boolean, dispatch: true}
  };

  static css = css`
    :host {
      --padding: 2px;
      --thumb-size: 22px;
      --height: calc(var(--thumb-size) + var(--padding) * 2);
      --checked-x: calc(var(--thumb-size) - var(--padding) * 2);
    }

    div {
      cursor: pointer;
      display: inline-block;
      position: relative;
      width: calc(var(--thumb-size) * 2);
      height: var(--height);
      outline: none;
    }

    .track {
      position: absolute;
      inset: 0;
      background: #ccc;
      border-radius: calc(var(--height) / 2);
      transition: background 160ms;
    }

    .thumb {
      position: absolute;
      top: var(--padding);
      left: var(--padding);
      width: var(--thumb-size);
      height: var(--thumb-size);
      background: #fff;
      border-radius: 50%;
      box-shadow: 0 0 2px rgb(0 0 0 / 0.4);
      transition: transform 160ms;
    }

    .checked .track {
      background: #4caf50;
    }

    /* thumb slides with a CSS transition */
    .checked .thumb {
      transform: translateX(var(--checked-x));
    }
  `;

  // The tabindex attribute is required to make the div focusable.
  static html = html`
    <div
      aria-checked="this.checked"
      class="this.checked ? 'checked' : ''"
      onClick="toggle"
      onKeyDown="handleKey"
      role="switch"
      tabindex="0"
    >
      <span class="track"></span>
      <span class="thumb"></span>
    </div>
  `;

  handleKey(e) {
    if (e.code === 'Space' || e.code === 'Enter') {
      e.preventDefault();
      this.toggle();
    }
  }

  toggle() {
    this.checked = !this.checked;
  }
}

ToggleSwitch.register();
```

Here it is in action.

```html
<toggle-switch checked></toggle-switch>
```

<toggle-switch checked></toggle-switch>

## Form Submissions

Web components that extend `Wrec` can contribute values to
form submissions by adding the following line to their class definition.
Wrec looks for this automatically does the rest of the work.

```js
static formAssociated = true;
```

## Error Checking

Wrec checks for many kinds of errors and throws an `Error` when they are found.
Look for messages in the DevTools console.
The kinds of errors that are detected include:

- attribute names in web component instances
  with no matching property declaration
- attribute values with a type that differs from the declared property type
- event handling function names that
  don't match any method name in the web component
- expressions in attribute values or element text content
  that reference undeclared web component properties
- expressions in element text content
  that do not evaluate to a string or number

## Security

Wrec uses the JavaScript `eval` function to evaluate JavaScript expressions
that are placed in attribute values and the text content of elements.
This has security implications if those expressions
can come from untrusted sources, so it is best avoid
creating web components that use untrusted content in those ways.

Perhaps the most dangerous thing the use of `eval` allows
is sending HTTP requests to other servers.
Such requests could contain data scraped from your web app
in order to share it with unscrupulous sites.

The easiest way to prevent this is to add a
Content Security Policy (CSP) to your web app.
Simply adding the following element as a child of the
`head` element in each page blocks sending HTTP requests
to any domain except that of your web app:

```html
<meta http-equiv="Content-Security-Policy" content="connect-src 'self'" />
```

## More Examples

Check out the `examples` directory in the
[wrec GitHub repository](https://github.com/mvolkmann/wrec).
This contains many example web components that are defined using wrec.

Compare the files `counter-vanilla.js` and `counter-wrec.js`
to get a feel for how much using wrec
simplifies the code required to define a web component.

To try the examples, clone the repository, cd to that directory,
enter `npm install`, enter `npm run dev`, and browse localhost:5173.
Also try browsing other `.html` files besides `index.html`.

## Tests

wrec has an extensive set of Playwright tests.
To run them:

1. Clone the wrec repository.
1. cd to the `examples` directory.
1. Enter `npm install`.
1. Enter `npm run testui`.
1. Click the right pointing triangle.

If there is no "Action" tab which displays a browser view of the running tests,
reset the Playwright UI settings by entering one of these commands:

```bash
# macOS
rm -rf ~/Library/Caches/ms-playwright/.settings

# Windows
del %LOCALAPPDATA%\ms-playwright\.settings /s /q
```
