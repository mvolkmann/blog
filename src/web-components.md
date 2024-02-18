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

    // Approach #1
    // Using shadow DOM is not required.
    /*
    const div = document.createElement('div');
    div.textContent = `Hello, ${name}!`;
    div.style.color = 'purple';
    this.appendChild(div);
    */

    // Approach #2
    /*
    const div = document.createElement('div');
    div.textContent = `Hello, ${name}!`;
    div.style.color = 'purple';

    // The "mode" option is required and the recommended value is "open".
    // When set to "open", `this.shadowRoot` is set.
    // When set to "closed", it is not.
    this.attachShadow({mode: 'open'});
    this.shadowRoot.appendChild(div);
    */

    // Approach #3
    this.attachShadow({mode: 'open'});
    this.render();
  }

  static get observedAttributes() {
    return ['name'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === 'name') this.render();
  }

  render() {
    const name = this.getAttribute('name');
    this.shadowRoot.innerHTML = `
      <div style="color: purple;">Hello, ${name}!</div>
    `;
  }
}
customElements.define('greet-message', GreetMessage);
```

Calling `this.attachShadow` sets `this.shadowRoot` which is a {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot",
"ShadowRoot" %} object.
This can be accessed in other lifecycle methods such as `connectedCallback`.
Reasons include:

- setting its `innerHTML`
- modifying its descendant elements
- registering listeners for `slotchange` events

The following HTML renders an instance of the web component defined above.
It uses {% aTargetBlank "https://alpinejs.dev/", "Alpine" %}
to add a bit of interactivity.

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Web Components Demo</title>
    <script type="module" src="/src/greet-message.js"></script>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <body>
  <body x-data="{name: 'World'">
    <greet-message :name="name"></greet-message3>
    <button @click="name = 'Earth'">Change Name</button>
  </body>
</html>
```

## Libraries That Simplify

- {% aTargetBlank "/blog/topics/#/blog/lit/", "Lit" %}
- {% aTargetBlank "https://stenciljs.com", "Stencil" %}

## Available Web Components

- {% aTargetBlank "https://shoelace.style", "Shoelace" %}

  "A forward-thinking library of web components."

- {% aTargetBlank "https://github.com/davatron5000/awesome-standalones", "Awesome Standalones" %}
  from Dave Rupert

  "A curated list of awesome framework-agnostic standalone web components."

For more, see {% aTargetBlank
"https://open-wc.org/guides/community/component-libraries/",
"Community: Component Libraries" %}.

## Custom Elements

{% aTargetBlank "https://html.spec.whatwg.org/multipage/custom-elements.html",
"Custom Elements" %} provide a way to define and use custom HTML elements.

A custom element is defined by a JavaScript class that extends `HTMLElement`.
The following code in the file `hello-world.js`
demonstrates a very basic web component.

```js
class HelloWorld extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({mode: 'open'});
    this.shadowRoot.textContent = 'Hello, World!';
  }
}
```

A tag name can be associated with this class as follows:

```js
customElements.define('hello-world', HelloWorld);
```

The name of the class is not required to correspond to the tag name in any way.

The names of custom elements must be all lowercase
and contain at least one hyphen.
This avoids name conflicts with standard HTML elements.

The following HTML demonstrates using this custom element.

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Custom Element Demo</title>
    <script type="module" src="/hello-world.js"></script>
  </head>
  <body>
    <hello-world></hello-world>
  </body>
</html>
```

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

The CSS `display` property for the root element of custom elements
defaults to "inline".
Often it is desirable to change this to "block" or "inline-block".
To do this, use the `:host` CSS pseudo-class.
For example:

```html
<style>
  :host {
    display: inline-block;
  }
</style>
```

### Piercing the Shadow DOM

By default, styling used by web components
cannot be modified by users of the web components.
A web component can allow specific overrides
by exposing the use of CSS variables.

For example, suppose we want to allow users of the `greet-message`
custom element to select the color which defaults to "purple".
To achieve this we can change the `render` method to the following.

```js
  render() {
    const name = this.getAttribute('name');
    this.shadowRoot.innerHTML = `
      <style>
        div { color: var(--greet-message-color, purple); }
      </style>
      <div>Hello, ${name}!</div>
    `;
  }
```

The CSS used by web components that create a shadow DOM is scoped to them.
So the CSS rule above only affects the `div` element that it renders.
The `var` above specifies that the `color` should be the value of the
`--greet-message-color` CSS variable if it is set, and "purple" otherwise.

Now users of this custom element can override the color with the following.

```html
<style>
  :root {
    --greet-message-color: green;
  }
</style>
```

## ES Modules

The {% aTargetBlank "https://html.spec.whatwg.org/multipage/webappapis.html#integration-with-the-javascript-module-system",
"ES Modules" %} specification define the mechanisms for
exporting and importing JavaScript modules.

## HTML Template

The HTML {% aTargetBlank
"https://html.spec.whatwg.org/multipage/scripting.html#the-template-element",
"template element" %} provides a way to define an HTML fragment
that can be cloned and inserted multiple times into a DOM tree.

`template` elements have their CSS `display` property set to `none`
so their content is not rendered.

Templates can contain slots that are used to insert content.
Each template can contain one unnamed slot and any number of named slots.
This is primarily useful when `template` elements are cloned by web components.

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
        // The class name is not required to match the custom element name.
        class Card extends HTMLElement {
          constructor() {
            super();
            this.attachShadow({mode: 'open'});
            const template = document.getElementById('card');
            // Passing true creates a deep clone.
            this.shadowRoot.appendChild(template.content.cloneNode(true));
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

## Lifecycle Methods

Custom elements have lifecycle methods that are automatically called
at specific points during their lifetime or when specific things occur.
These include the following.

### adoptedCallback

This lifecycle method is called automatically
when the instance is moved to a new document.

This method is rarely used.

### attributeChangedCallback(name, oldValue, newValue)

This lifecycle method is called automatically
when the value of an observed attribute changes.
To define the attributes that are observed, implement the following:

```js
static get observedAttributes() {
  return ['name1', 'name2', ...];
}

attributeChangedCallback(name, oldValue, newValue) {
  this.render(); // assumes this method exists and updates the shadow DOM
}
```

### connectedCallback

This lifecycle method is called automatically
after an instance is added to the DOM.
It can be used to add event listeners
to elements outside this custom element.
Typically anything done in this method
is undone in the `disconnectedCallback` method.

### constructor

This lifecycle method is called automatically
when an instance is initially created and
jagain if the custom element definition is modified.
It is commonly used for one time initializations
such as computing property values.

### disconnectedCallback

This lifecycle method is called automatically
after an instance is removed from the DOM.
It can be used to remove event listeners
from elements outside this custom element.
Typically anything done in the `connectedCallback` method
is undone in this method.

## Attributes

Attributes can be passed to custom elements
in the same way that attributes are passed to standard HTML elements.

The class of a custom element can:

- get the value of an attribute

  ```js
  const value = this.getAttribute('some-name');
  ```

- set the value of an attribute

  ```js
  this.setAttribute('some-name', someValue);
  ```

## Elements

The class of a custom element can:

- create new elements

  ```js
  const el = document.createElement('element-name');
  ```

- find elements within its shadow DOM

  ```js
  const el = this.querySelector('element-name');
  const els = this.querySelectorAll('element-name');
  ```

- attach new elements to its shadow DOM

  ```js
  el.appendChild(otherEl);
  ```

- set text content

  ```js
  el.textContent = 'some text';
  ```

- set inner HTML

  ```js
  el.innerHTML = 'some HTML';
  ```
