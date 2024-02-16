---
eleventyNavigation:
  key: Lit
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 50%">
  <img alt="Lit logo" style="border: 0"
    src="/blog/assets/lit-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://lit.dev", "Lit" %}
is library that simplifies developing
{% aTargetBlank "https://www.webcomponents.org/", "web components" %}.

Lit components are native web components.

Web components define custom elements that
can be used just like standard HTML elements.

Custom element names must include at least one hyphen.
This avoids conflicting with the names of standard HTML elements.

Tags for custom elements cannot be self-closing,
even when they have no content.
For example, `<my-element></my-element>` is valid, but `<my-element />` is not.

Web components can be used in any web page,
with any web framework, and in Markdown files.
This gives them much broader applicability than components
implemented using a specific frameworks such as React.

Web components are more future-proof than other kinds of components
because they are likely to be usable
in applications built with future web frameworks.

## Installing

The easiest way to get started using Lit is to
get the library from this {% aTargetBlank
"https://cdn.jsdelivr.net/gh/lit/dist@3/core/lit-core.min.js", "CDN" %}.
This single file can be saved locally to avoid depending on the CDN.
The file is 17 KB.

Another approach is to use {% aTargetBlank "https://vitejs.dev/", "Vite" %}.
This has many advantages including:

- configuration to use TypeScript
- running in watch mode so code changes are automatically recompiled
- automatic browser updates

The following steps create a new project that uses Vite and Lit.

1. Enter `npm create vite@latest`.
1. If prompted with "OK to proceed?", press the return key.
1. After the "Project name" prompt, enter a project name.
1. For the "Select a framework" prompt, select "Lit".
1. For the "Select a variant" prompt, select "TypeScript".
1. cd to the new project directory.
1. Install dependencies by entering `npm install` or `bun install`.
1. Run the project by entering `npm run dev` or `bun dev`.
1. Type "h" and press the return key for help.
1. Type "o" and press the return key to open a browser tab for localhost:5173.

## Basic Example

The following code from the file `src/greet-message.ts`
implements a basic custom element using Lit and TypeScript.
Using TypeScript provides type checking in code editors like VS Code.

Using TypeScript also allows the use of
decorators like `@customElement` and `@property`.
The `@customElement` decorator assigns a name to the custom element.
The `@property` decorator can declare attributes
that can be applied to the custom element.

```ts
import {css, html, LitElement} from 'lit';
import {customElement, property} from 'lit/decorators.js';

@customElement('greet-message')
export class GreetMessage extends LitElement {
  // This value will come from an HTML attribute.
  @property() name = '';

  render() {
    // Checking for required attributes is optional.
    if (!this.name) throw new Error('name is a required attribute');
    return html`<div>Hello, ${this.name}!</div>`;
  }

  // The `styles` property can be defined anywhere in
  // the class definition, not just at the bottom.
  // :host refers to outermost element rendered by this custom element.
  static styles = css`
    :host {
      color: purple;
    }
  `;
}
```

The following code from the file `src/greet-message.js` implements
the same custom element using JavaScript instead of TypeScript.
JavaScript-based custom elements cannot use decorators,
so they use a different approach to
declare the custom element name and properties (attributes).

```js
import {css, html, LitElement} from 'lit';

export class GreetMessage extends LitElement {
  static properties = {
    name: {type: String}
  };

  render() {
    if (!this.name) throw new Error('name is a required attribute');
    return html`<div>Hello, ${this.name}!</div>`;
  }

  static styles = css`
    :host {
      color: purple;
    }
  `;
}
customElements.define('greet-message', GreetMessage);
```

The following HTML renders the custom element defined above.

<img alt="Lit greet-message" style="width: 20%"
  src="/blog/assets/lit-greet-message.png?v={{pkg.version}}">

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Lit Demo</title>
    <!-- Change .ts to .js to use the JavaScript version. -->
    <script type="module" src="/src/greet-message.ts"></script>
  </head>
  <body>
    <greet-message name="World"></greet-message>
  </body>
</html>
```

## Properties

Lit uses the `@property` decorator to declare the attributes
that can be specified in instances of custom elements.

These values are reactive, meaning that changing the value
causes the `render` method to be called.

The `@property` decorator takes a `PropertyDeclaration` object.

One of the supported options is `type`, which can have the value
`String`, `Number`, `Boolean`, `Array`, or `Object`.
These are Lit type hints, not TypeScript types.
The default `type` is `String`.

When the type is `Number`, the value is passed to the `Number` constructor
which results in `NaN` for non-numeric strings.

When the type is `Boolean`, all values except `null` and `undefined`
are treated as `true`.

When the type is `Array` or `Object`, the value is passed to `JSON.parse`
to get the property value.

Only string values can be specified in HTML.
JavaScript must be used to set attributes to other types of values.

Lit does not provide a way to specify that an attribute is required.
All `@property` declarations must be initialized,
but they can be initialized to an empty string.
The render method or any lifecycle method can check for
required attributes and throw an error if not found.
See the example in the "Basic Example" section above.

## State

Lit uses the `@state` decorator to declare instance properties
that are set via attributes.

These values are reactive, meaning that changing the value
causes the `render` method to be called.

Instance properties that are declared without the
`@property` or `@state` decorator are not reactive.

The following custom Lit element demonstrates the reactivity
of `@property` and `@state` variables.

<img alt="Lit reactivity" style="width: 30%"
  src="/blog/assets/lit-reactivity.png?v={{pkg.version}}">

```ts
import {html, LitElement} from 'lit';
import {customElement, property, state} from 'lit/decorators.js';

@customElement('state-changes')
export class StateChanges extends LitElement {
  @property() p = '';
  @state() s = 'initial';

  changeP() {
    this.p = 'changed';
  }

  changeS() {
    this.s = 'changed';
  }

  render() {
    return html`
      <div>
        p: ${this.p}
        <button @click=${this.changeP}>Change</button>
      </div>
      <div>
        s: ${this.s}
        <button @click=${this.changeS}>Change</button>
      </div>
    `;
  }
}
```

An instance of this custom element can be created as follows.

```html
<state-changes p="initial"></state-changes>
```

## Decorators

Lit supports many decorators.
We have already seen `@customElement`, `@property`, and `@state`.

Other supported decorators include:

- `@eventOptions`
- `@query`
- `@queryAsync`
- `@queryAssignedElements`
- `@queryAssignedNodes`

TODO: Add descriptions to the decorators listed above.

## Lifecycle Methods

Lit supports five lifecycle methods that are automatically called
at specific points in the lifecycle of a custom element.

### adoptedCallback

This lifecycle method is called when the instance is moved to a new document.

This method is rarely used.

### attributeChangedCallback

This lifecycle method is called when the value of an observed attribute changes.

### connectedCallback

This lifecycle method is called after an instance is added to the DOM.
It can be used to add event listeners
to elements outside this custom element.
Typically anything done in this method
is undone in the `disconnectedCallback` method.

### constructor

This lifecycle method is called when an instance is initially created
and again if the custom element definition is modified.
It is commonly used for one time initializations
such as computing property values.

### disconnectedCallback

This lifecycle method is called after an instance is removed from the DOM.
It can be used to remove event listeners
from elements outside this custom element.
Typically anything done in the `connectedCallback` method
is undone in this method.

The following code demonstrates each of the lifecycle methods
except the rarely used `adoptedCallback` method.

```js
import {html, LitElement} from 'lit';
import {customElement, property} from 'lit/decorators.js';

@customElement('lifecycle-demo')
export class LifecycleDemo extends LitElement {
  @property() text = '';

  changeText() {
    this.text = 'changed';
  }

  constructor() {
    super();
    console.log('constructor entered');
  }

  override attributeChangedCallback(
    name: string,
    oldValue: string | null,
    newValue: string | null
  ): void {
    console.log(`${name} changed from ${oldValue} to ${newValue}`);
    super.attributeChangedCallback(name, oldValue, newValue);
  }

  override connectedCallback(): void {
    super.connectedCallback();
    console.log('connectedCallback entered');
  }

  override disconnectedCallback(): void {
    super.disconnectedCallback();
    console.log('disconnectedCallback entered');
  }

  render() {
    return html`
      <div>
        text: ${this.text}
        <button @click=${this.changeText}>Change</button>
      </div>
    `;
  }
}
```

The following HTML demonstrates using the `lifecycle-demo` custom element.
It uses {% aTargetBlank "/blog/topics/#/blog/alpine", "Alpine" %} to
manage state and determine whether the custom element should be in the DOM.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Lifecycle Demo</title>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <script type="module" src="/src/lifecycle-demo.ts"></script>
  </head>
  <body x-data="{text: 'initial', show: false}">
    <button @click="show = !show">Toggle Show</button>
    <label>Text: <input type="text" x-model="text" /></label>
    <template x-if="show">
      <lifecycle-demo :text="text"></lifecycle-demo>
    </template>
  </body>
</html>
```

Every time `show` changes from `false` to `true`,
the `constructor` and `connectedCallback` methods are called.

Every time `show` changes from `true` to `false`,
the `disconnectedCallback` method is called.

When the "Change" button is clicked,
the value of the `text` property changes,
but this does NOT trigger a call to the `attributeChangedCallback` method.

Every time the value of the `input` element is changed by the user,
the value of the Alpine `text` variable changes.
That causes a new value to be passed to the `lifecycle-demo` element
through its `text` attribute.
This DOES trigger a call to the `attributeChangedCallback` method.

The `render` method is called every time
an `@property` or `@state` value changes.
It's not necessary to implement the `attributeChangedCallback` method
to make this happen.

## Event Handling

Lit supports registering event handling functions with attributes
whose names begin with `@`, followed by an event name.
The value must be a function object,
not a call to a function or JavaScript code.
String interpolation (`${fn}`) is used to insert the function
into the `html` tagged template literal.

For example:

```ts
import {css, html, LitElement} from 'lit';
import {customElement} from 'lit/decorators.js';

function handleClick() {
  alert('in handleClick function');
}

@customElement('alert-on-click')
export class AlertOnClick extends LitElement {
  handleClick() {
    alert('in handleClick method');
  }

  render() {
    return html`
      <button @click=${this.handleClick}>Click Me</button>
      <button @click=${handleClick}>Click Me</button>
    `;
  }

  static styles = css`
    :host {
      border: 1px dashed red;
      padding: 1rem;
    }

    button {
      border-radius: 0.5rem;
      border: 3px solid cornflowerblue;
      padding: 0.5rem;
      background-color: lemonchiffon;
      cursor: pointer;
    }
    button:hover {
      background-color: orange;
    }
  `;
}
```

## Type Inference

When an instance of a custom element is created
using the DOM `createElement` method, TypeScript cannot infer
the specific type and assumes it is `HTMLElement`.

For example, in the following code TypeScript will
infer the type of `el` to be `HTMLElement`.

```ts
const el = document.createElement('alert-on-click');
```

For many custom elements, this is not a concern
because element instances will be created in this way.
When this is a concern, enable inferring a more specific type
by adding an entry to the `HTMLElementTagNameMap` as follows.

```ts
declare global {
  interface HTMLElementTagNameMap {
    'alert-on-click': AlertOnClick;
  }
}
```

With this in place, the type of `el` will be inferred to be `AlertOnClick`.

## Errors Detected

Lit error detection includes the following:

- defining multiple custom elements with the same name

The following kinds of errors are not detected.

- Mistyping the name of a custom element in HTML.

  The element will be treated as if it were a `div` element.

## Unorganized Content

To force a custom element instance to update, call its `requestUpdate` method.
