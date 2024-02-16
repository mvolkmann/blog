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
The following code demonstrates this.

```ts
  @property({type: String}) name = '';

  render() {
    if (!this.name) throw new Error('name is a required attribute');
    return html`<div>Hello, ${this.name}!</div>`;
  }
```

## State

Lit uses the `@state` decorator to declare instance properties
that are set via attributes.

These values are reactive, meaning that changing the value
causes the `render` method to be called.

Instance properties that are declared without the
`@property` or `@state` decorator are not reactive.

## Lifecycle Methods

## Event Handling

Lit supports registering event handling functions with attributes
whose names begin with `@`, followed by an event name.
The value must be a function object,
not a call to a function or JavaScript code.
String interpolation (`${fn}`) is used to insert the function
into the `html` tagged template literal.

For example:

```ts
import {LitElement, css, html} from 'lit';
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
