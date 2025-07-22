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
