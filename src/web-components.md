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

Implementing a reusable library of UI components that can only be used
in web applications that are implemented with a specific framework
is not a good time investment.
It is much better to implement them as web components so they can be
used in all web applications, regardless of the web framework they use.
The {% aTargetBlank "/blog/topics/#/blog/shoelace", "Shoelace" %}
library of web components is a great example.

While you could implement every UI component of a web app as a web component,
that is a bit harder than using other web framework like Svelte.
Consider only implementing web components for general purpose UI components
that will be used in multiple apps.

## Basic Example

The following file `src/greet-message.js` defines a web component.

```js
export class GreetMessage extends HTMLElement {
  constructor() {
    super();

    const name = this.getAttribute('name');
    if (!name) throw new Error('name is a required attribute');

    const div = document.createElement('div');
    div.textContent = `Hello, ${name}!`;
    div.style.color = 'purple';

    const shadow = this.attachShadow({mode: 'open'});
    shadow.appendChild(div);
    // Using shadow DOM is not required.  We can replace
    // the previous two lines with the following.
    // this.appendChild(div);
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

Many standard HTML elements utilize a shadow DOM.
Examples include `input`, `audio`, `video`, and `detail`.
This can be seen by examining such elements
in the "Elements" tab of the Chrome DevTools.
Click the settings gear icon, scroll to the "Elements" section,
and check "Show user agent shadow DOM".

## ES Modules

{% aTargetBlank "https://html.spec.whatwg.org/multipage/webappapis.html#integration-with-the-javascript-module-system",
"ES Modules" %} define the mechanisms for
exporting and importing JavaScript modules.

## HTML Template

The HTML {% aTargetBlank
"https://html.spec.whatwg.org/multipage/scripting.html#the-template-element",
"template element" %} provides a way to define an HTML fragment
that can be cloned and inserted multiple times into a DOM tree.

Templates can contain slots that are used to insert content.
Each template can contain one unnamed slot and any number of named slots.

The following code demonstrates
using the `template` element inside a web component.
This approach has the advantage that the content of a web component
can be described with HTML instead of using
DOM methods like `createElement` and `appendChild`.
However, it has the disadvantage that the web component definition
must assume that a `template` with a given `id` has been defined outside it.

HTML does not define a mechanism for including one HTML file into another.
So we can't define templates in a separate file that is included.

<img alt="Web Components and templates" style="width: 40%"
  src="/blog/assets/web-component-templates.png?v={{pkg.version}}">

```js
<!DOCTYPE html>
<html>
  <head>
    <title>Template Demo</title>
    <script>
      window.onload = () => {
        class Card extends HTMLElement {
          constructor() {
            super();
            const shadowRoot = this.attachShadow({mode: 'open'});
            const template = document.getElementById('card');
            // Passing true creates a deep clone.
            shadowRoot.appendChild(template.content.cloneNode(true));
          }
        }
        customElements.define('my-card', Card);
      };
    </script>
  </head>
  <body>
    <h1>Template Demo</h1>

    <template id="card">
      <!-- This styling is only scoped when the
           template is used in a web component. -->
      <style>
        .card {
          display: inline-block;
          background-color: cornflowerblue;
          border: 3px solid blue;
          border-radius: 0.5rem;
          padding: 0.5rem;
        }
        h2 {
          color: orange;
          margin-top: 0;
        }
      </style>
      <section class="card">
        <h2><slot name="title">Untitled</slot></h2>
        <h3><slot name="header" /></h3>
        <p><slot name="body" /></p>
        <h3><slot name="footer" /></h3>
      </section>
    </template>

    <my-card>
      <span slot="title">Title #1</span>
      <span slot="header">header #1</span>
      <span slot="body">body #1</span>
      <span slot="footer">footer #1</span>
    </my-card>

    <my-card>
      <span slot="title">Title #2</span>
      <span slot="body">body #2</span>
    </my-card>
  </body>
</html>
```
