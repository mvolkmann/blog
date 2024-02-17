---
eleventyNavigation:
  key: Web Components
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="Web Components logo" style="border: 0"
    src="/blog/assets/web-components-logo.svg?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://www.webcomponents.org/introduction", "Web components" %}
"are a set of web platform APIs that allow you to create
new custom, reusable, encapsulated HTML tags to use in web pages and web apps."

Web components encapsulate their markup, styles, and functionality
by using a "shadow DOM".

Implementing a web component requires a bit more effort
than implementing components using a framework like Svelte.
The extra effort is worthwhile for components that will
be used in multiple apps written using multiple frameworks.
Even if you are only using one web framework today,
that may change in the future.
The investment in creating high quality, reusable web components
will pay off in the long run.

While you could implement every UI component of a web app as a web component,
that is a bit harder than using other web framework like Svelte.
Consider only implementing web components for general purpose UI components
that will be used in multiple apps.
The {% aTargetBlank "/blog/topics/#/blog/shoelace", "Shoelace" %}
web components are great examples.

## Basic Example

The following file `src/greet-message.js` defines a web component.

```js
export class GreetMessage extends HTMLElement {
  constructor() {
    super();

    const name = this.getAttribute('name');
    if (!name) throw new Error('name is a required attribute');

    const shadow = this.attachShadow({mode: 'open'});
    const div = document.createElement('div');
    div.textContent = `Hello, ${name}!`;
    div.style.color = 'purple';
    shadow.appendChild(div);
  }
}
customElements.define('greet-message', GreetMessage);
```

The following HTML renders an instance of the web component defined above.

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Web Components Demo</title>
    <script type="module" src="/src/greet-message.js"></script>
  </head>
  <body>
    <greet-message name="World"></greet-message3>
  </body>
</html>
```

## Custom Elements

{% aTargetBlank "https://html.spec.whatwg.org/multipage/custom-elements.html",
"Custom Elements" %} provide a way to define and use custom HTML elements.

## Shadow DOM

The {% aTargetBlank "https://dom.spec.whatwg.org/#interface-shadowroot",
"Shadow DOM" %} provides a way to encapsulate the content and styling
of a custom element.

## ES Modules

{% aTargetBlank "https://html.spec.whatwg.org/multipage/webappapis.html#integration-with-the-javascript-module-system",
"ES Modules" %} define the mechanisms for
exporting and importing JavaScript modules.

## HTML Template

The HTML {% aTargetBlank
"https://html.spec.whatwg.org/multipage/scripting.html#the-template-element",
"template element" %} provides a way to define an HTML fragment
that can be cloned and inserted multiple times into a DOM tree.
