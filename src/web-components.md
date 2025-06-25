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
define custom HTML elements that can be used just like standard HTML elements.
They are defined by a set of standards that include the
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_custom_elements"
target="_blank">Custom Elements</a>,
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_shadow_DOM"
target="_blank">Shadow DOM</a>,
<a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/template"
target="_blank">HTML Templates</a>, and
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules"
target="_blank">JavaScript Modules</a>.

Web components can be used in any web page,
with any web framework, and in Markdown files.
This gives them much broader applicability than components
implemented using a specific frameworks such as React.
Those components can typically only be used in
applications that use the same framework.

Web components are more future-proof than other kinds of components
because they are likely to be usable
in applications built with future web frameworks.

Implementing a web component requires a bit more effort
than implementing components using a framework like Svelte.
The extra effort is worthwhile for components that will
be used in multiple apps written using multiple frameworks.
Even if you are only using one web framework today,
that may change in the future.
The investment in creating high quality, reusable web components
is likely to pay off in the long run.

Web components can encapsulate their markup, styles, and functionality
by using a "shadow DOM".

Tags for custom elements cannot be self-closing,
even when they have no content.
For example, `<my-element></my-element>` is valid, but `<my-element />` is not.
HTML parsers only handle a fixed set of
"void elements" that can be self-closing.
Commonly used examples include `br`, `hr`, `img`, `input`, `link`, and `meta`.

## DOM Terminology

The term "shadow DOM" refers to DOM nodes that
have an ancestor that is a "shadow root".
A shadow root is created by calling the `HTMLElement` `attachShadow` method.

The term "shadow host" refers to the regular DOM node
to which the shadow root is attached.

The term "light DOM" refers to DOM nodes that
are supplied as slot content to a web component.

The term "regular DOM" refers to all DOM nodes
that are not in a shadow DOM or light DOM.

## Pros and Cons

Some of the pros of using web components include:

- only uses web standards
- very portable ... can be used in all web frameworks
- no build tools are required unless using TypeScript

Some of the cons of using web components include:

- more tedious to implement than components in frameworks like Svelte
- need to use another library like Lit to simplify

## Open UI

{% aTargetBlank "https://open-ui.org", "Open UI" %}
is a W3C Community Group whose goal is "to allow web developers to
style and extend built-in web UI components and controls,
such as select dropdowns, checkboxes, radio buttons, and date/color pickers."

The components proposed by this group could eventually
work their way into the HTML specification.
This would provide native alternatives to
some of the web components we might build and use today.

See the list of components being explored at the Open UI link above.

## Migrating to Web Components

A recommended way to begin using web components in existing web applications
is to identify their UI components that would be useful in other applications
and one-by-one rewrite them as web components.

## Basic Example

The following file `src/greet-message.js` defines a web component.

```js
export class GreetMessage extends HTMLElement {
  // This constructor can be omitted for approach #1.
  constructor() {
    super();
    this.attachShadow({mode: 'open'});
  }

  connectedCallback() {
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

    this.shadowRoot.appendChild(div);
    */

    // Approach #3
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
    // Setting innerHTML removes the need to use low-level
    // DOM methods like `createElement` and `appendChild`.
    this.shadowRoot.innerHTML = `
      <div style="color: purple;">Hello, ${name}!</div>
    `;
  }
}

customElements.define('greet-message', GreetMessage);
```

It is common to use a template literal (in backticks) to construct
the string used as the value of the `innerHTML` property.

When the string contains an optional HTML element,
the following pattern can be used to specify it.

```js
element.innerHTML = `
  <always-present></always-present>
  ${value ? `<optionally-present></optionally-present>` : ''}
  <always-present></always-present>
`;
```

When the string contains an HTML element with an optional attribute,
the following pattern can be used to specify it.

```js
element.innerHTML = `
  <some-element ${value ? `some-attr="${value}"` : ''}></some-element>
`;
```

The following HTML renders an instance of the web component defined above.
It uses {% aTargetBlank "https://alpinejs.dev/", "Alpine" %}
to add a bit of interactivity.

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Web Components Demo</title>
    <script type="module" src="greet-message.js"></script>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <body>
  <body x-data="{name: 'World'}">
    <greet-message :name="name"></greet-message>
    <button @click="name = 'Earth'">Change Name</button>
  </body>
</html>
```

## Options

There are three options for implementing web components:

1. Do not use a shadow DOM.
1. Use an "open" shadow DOM.
1. Use a "closed" shadow DOM.

Regardless of the option selected,
there are two ways to specify the component DOM:

1. Set the `innerHTML` property of some object.
1. Call the `appendChild` method on some object.

When not using a shadow DOM, "some object" is `this`.

When using an open shadow DOM, "some object" is `this.shadowRoot`.

When using a closed shadow DOM, "some object" is
the return value of the `attachShadow` method.

There are two places where you might consider
specifying the DOM of the web component,
in its constructor or in its `connectedCallback` method.
While doing this in the constructor sometimes works, it is discouraged.
The reason is that when the constructor runs,
the attributes and child nodes of the custom element are not yet known.

To create a shadow DOM,
call `this.attachShadow({mode: "open"})`
or `this.attachShadow({mode: "closed"})`.
The recommended mode is "open", which causes `this.shadowRoot`
to be set to a {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot",
"ShadowRoot" %} object.
When the "mode" is "closed", `this.shadowRoot` is not set.
Instead it must be captured via the return value of the `attachShadow` method.
This allows setting its `innerHTML` property
or calling its `appendChild` method,
but it prevents access to the shadow DOM from outside the component.

## Libraries That Simplify

- {% aTargetBlank "/blog/topics/#/blog/lit/", "Lit" %}
- {% aTargetBlank "https://stenciljs.com", "Stencil" %}
- {% aTargetBlank "https://fast.design", "Microsoft FAST" %}

## Available Web Components

- {% aTargetBlank "https://backers.webawesome.com", "Web Awesome" %}

- {% aTargetBlank "https://shoelace.style", "Shoelace" %}

  "A forward-thinking library of web components."

- {% aTargetBlank "https://lion-web.netlify.app", "Lion" %}

  "Fundamental white label web components for building your design system."

- {% aTargetBlank "https://github.com/davatron5000/awesome-standalones", "Awesome Standalones" %}
  from Dave Rupert

  "A curated list of awesome framework-agnostic standalone web components."

- {% aTargetBlank "https://genericcomponents.netlify.app", "generic-components" %}

- {% aTargetBlank "https://lottiefiles.github.io/lottie-player/", "Lottie Player" %}

  This web component enables playing Lottie animations.
  It can be included from a CDN with the following:

  ```html
  <script src="https://unpkg.com/@lottiefiles/lottie-player@0.2.0/dist/lottie-player.js"></script>
  ```

  To render an animation, add the `lottie-player` custom element in HTML
  as follows:

  ```html
  <lottie-player
    autoplay
    loop
    src="https://assets3.lottiefiles.com/packages/lf20_UJNc2t.json"
    style="width: 250px"
  >
  </lottie-player>
  ```

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

The names of custom elements must be all lowercase
and contain at least one hyphen (dash).
This avoids name conflicts with standard HTML elements.

The name of the class that implements a custom element
is not required to correspond to the tag name in any way.
However, a common convention is for class names to use CamelCase
and element names to use kebab-case.

Often the part before the first hyphen serves as a namespace.
For example, all the custom elements provided by
{% aTargetBlank "https://shoelace.style", "Shoelace" %} begin with "sl-".

The following HTML demonstrates using the custom element defined above.

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Custom Element Demo</title>
    <script type="module" src="hello-world.js"></script>
  </head>
  <body>
    <hello-world></hello-world>
  </body>
</html>
```

## Shadow DOM

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_shadow_DOM",
"Shadow DOM" %} provides a way to encapsulate the content and styling
of a custom element.

Many standard HTML elements utilize a shadow DOM.
Examples include `input`, `audio`, `video`, and `detail`.
This can be seen by examining such elements
in the Chrome DevTools "Elements" tab.
The "shadow-root" of standard HTML elements
can only be viewed in the DevTools when enabled.
To enable this, click the settings gear icon, scroll to the "Elements" section,
and check "Show user agent shadow DOM".
The "shadow-root" of custom elements is always visible.

To use a shadow DOM in a web component,
add a line line the following in the constructor
where the value of `mode` is `'open'` or `'closed'`:

```js
this.attachShadow({mode: 'open'});
```

To add content to the shadow DOM in a web component,
add a line like the following in the `connectedCallback` method:

```js
this.shadowRoot.appendChild(someElement);
```

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

### Forms

Instances of web components that are nested in a `form` element
cannot by default contribute to the set of name/value pairs
that are submitted by the form.
They are prevented from doing so by the shadow DOM.
{% aTargetBlank "https://dev.to/steveblue/form-associated-custom-elements-ftw-16bi",
"Form-associated custom elements" %} provide a solution.

TODO: Try this and add an example here.

### Piercing the Shadow DOM

The CSS defined in web components that create a shadow DOM is scoped to them.
It does not "leak out" to affect HTML outside it.

By default, web component styling
cannot be modified by users of the web components.
There are two workarounds for this.

Inheritable CSS properties, of which there are many,
can be used by web components.
These include `color`, `cursor`, `font`,
`font-family`, `font-size`, `font-style`, `font-variant`, `font-weight`,
`letter-spacing`, `line-height`, `text-align`, `text-indent`, `text-transform`,
`visibility`, and `white-space`, `word-spacing`.

For example, suppose we want to set the color used for
`label` elements in a custom element named `dog-data`.
In the `head` element of the main HTML file, add the following:

```html
<style>
  dog-data {
    color: green;
  }
</style>
```

Then in the web component that defines the custom element, add the following:

```html
<style>
  label {
    color: inherit;
  }
</style>
```

Web components can also allow specific style overrides by using CSS variables.

For example, suppose we want to allow users of the `dog-data`
custom element to select the label color which defaults to "purple".
In the web component that defines the custom element, add the following:

<style>
  label {
    color: var(--dog-data-label-color, purple);
  }
</style>

The `var` above specifies that the `color` should be the value of the
`--dog-data-label-color` CSS variable if it is set, and "purple" otherwise.

Then in the `head` element of the main HTML file, add the following:

```html
<style>
  dog-data {
    --dog-data-label-color: red;
  }

  /* OR */

  :root {
    --dog-data-label-color: red;
  }
</style>
```

Another way to share styles across components is
to have each refer to the same `.css` file.
For example, the following main page and two web components
all use the file `share.css` to get
the same styling for all `button` elements.

In `share.css`:

```css
button {
  background-color: cornflowerblue;
  color: orange;
}
```

In `index.html`:

```html
<html>
  <head>
    <link rel="stylesheet" href="share.css" />
    <script src="wc-one.js"></script>
    <script src="wc-two.js"></script>
  </head>
  <body>
    <button>Main</button>
    <wc-one></wc-one>
    <wc-two></wc-two>
  </body>
</html>
```

In `wc-one.js`:

```js
class WCOne extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({mode: 'open'});
  }

  connectedCallback() {
    this.shadowRoot.innerHTML = /*html*/ `
      <link rel="stylesheet" href="share.css" />
      <button>WC One</button>
    `;
  }
}

customElements.define('wc-one', WCOne);
```

In `wc-two.js`:

```js
class WCTwo extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({mode: 'open'});
  }

  connectedCallback() {
    this.shadowRoot.innerHTML = /*html*/ `
      <link rel="stylesheet" href="share.css" />
      <button>WC Two</button>
    `;
  }
}

customElements.define('wc-two', WCTwo);
```

## JavaScript Modules

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules",
"JavaScript Modules" %} support exporting and importing
functions, classes, objects, constants, and variables.

To use the `export` and `import` keywords in a JavaScript source file,
it must be included with a `script` element like the following:

```html
<script src="{path-to-js-file}" type="module"></script>
```

## HTML Templates

The HTML {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_templates_and_slots",
"template element" %} provides a way to define an HTML fragment
that can be cloned and inserted multiple times into a DOM tree.
This is fast because the content of a `template` element in parsed only once.
The resulting DOM root element is assigned to its `content` property.

All `template` elements have their CSS `display` property set to `none`,
so their content is not rendered.
Changing the `display` property to another value does not cause it to render.

To render the contents of a template,
create a deep clone and append the clone to another element.
This is faster than parsing an HTML string again.
For example:

```js
document.body.appendChild(myTemplate.content.cloneNode(true));
```

Templates can contain slots that are used to insert content.
Each template can contain one unnamed slot and any number of named slots.
This is primarily useful when `template` elements are cloned by web components.

The following code demonstrates
using a `template` element inside a web component.
This approach has the advantage that the content of a web component
can be described with HTML instead of creating it using
DOM methods like `createElement` and `appendChild`.
However, it has the disadvantage that the web component definition
must assume that a `template` with a given `id` has been defined outside it.

HTML does not define a mechanism for including one HTML file into another.
So we can't define templates in a separate file that is included.

<img alt="Web Components and templates" style="width: 40%"
  src="/blog/assets/web-component-templates.png?v={{pkg.version}}">

```js
class MyCard extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({mode: 'open'});
  }

  connectedCallback() {
    const template = document.getElementById('my-card');
    this.shadowRoot.appendChild(template.content.cloneNode(true));
  }
}

customElements.define('my-card', MyCard);
```

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Template Demo</title>
    <script src="my-card.js"></script>
  </head>
  <body>
    <h1>Template Demo</h1>

    <template id="my-card">
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

## Slots and Parts

A web component can render HTML that includes slots and parts.
Slots are locations where content can be inserted.
Each web component can have one default slot (unnamed)
and any number of named slots.

A web component can identify some of the elements it renders as "parts".
This enables the parts to be styled from outside of the shadow DOM.
It is an alternative to using CSS variables to "pierce" the shadow DOM,
which was described earlier.

The following example demonstrates a web component
that uses both slots and parts.

```js
export class VanillaWC extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({mode: 'open'});
    const root = this.shadowRoot;

    root.innerHTML = `
      <style>
        /* This targets the shadow host. */
        :host {
          display: inline-block;

          border: 1px solid blue;
          padding: 1rem;
          width: 30%;
        }

        /* This targets any top-level child placed in the slot named "nav". */
        slot[name="nav"]::slotted(*) {
          border-bottom: 1px solid blue;
        }

        /* This could be used in place of the previous rule.
        nav {
          border-bottom: 1px solid blue;
        } */

        /* This targets any top-level p elements placed in any slot.
          ::slotted only supports single-element selectors. */
        ::slotted(p) {
          color: green;
          font-style: italic;
        }
      </style>
      <div>
        <h2>Vanilla Web Component</h2>
        <nav><slot name="nav"></slot></nav>
        <div part="header">header</div>
        <p><slot></slot></p>
      </div>
    `;
  }
}
customElements.define('vanilla-wc', VanillaWC);
```

The `::slotted` pseudo-element styles light DOM elements.
It only works in CSS specified inside a shadow DOM.
It applies to actual elements in slots, not text nodes.

The following HTML demonstrates using the web component defined above.

<img alt="Web Component slots and parts" style="border: 0; width: 60%"
  src="/blog/assets/web-component-slots-parts.png?v={{pkg.version}}">

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Web Component Demo</title>
    <style>
      /* This specifies inheritable styles that will be
         used by web components unless overridden. */
      body {
        color: blue;
        font-family: sans-serif;
      }

      /* This targets the part named "header" in the web component. */
      vanilla-wc::part(header) {
        color: purple;
        font-size: 2rem;
        font-weight: bold;
        text-transform: uppercase;
      }

      /* This targets all a elements in light DOM
         which includes the elements inserted into slots. */
      vanilla-wc a {
        color: red;
      }
    </style>
    <script type="module" src="/src/vanilla-wc.js"></script>
  </head>
  <body>
    <vanilla-wc>
      <div slot="nav">
        <a href="/home">Home</a>
        <a href="/about">About</a>
      </div>
      <p>
        Come and listen to a story about a man named Jed,<br />
        a poor mountaineer, barely kept his family fed.<br />
        Then one day he was shootin at some food<br />
        and up through the ground came a bubblin crude.
      </p>
    </vanilla-wc>
  </body>
</html>
```

## Lifecycle Methods

Custom elements have lifecycle methods that are automatically called
at specific points during their lifetime or when specific things occur.
These include the following.

### constructor

This lifecycle method is called automatically
when an instance is initially created and
again if the custom element definition is modified.
It is commonly used for one time initializations
such as computing property values.

This method can be omitted if no initializations are required.
If a constructor is included, it must begin with a call to `super`
which executes the constructor in the superclass `HTMLElement`.

### connectedCallback

This lifecycle method is called automatically
after an instance is added to the DOM.
This method is typically used to
add elements to the DOM of the web component
and add event listeners.

An event listener can be a specific method in a web component, or simply `this`.
When `this` is specified and the event is dispatched,
the `handleEvent` method is called, passing it an `Event` object.
For example:

```js

connectedCallback() {
  const decrementBtn = this.shadowRoot.querySelector('#decrement-btn');
  decrementBtn.addEventListener('click', this.decrement);

  this.incrementBtn = this.shadowRoot.querySelector('#increment-btn');
  this.incrementBtn.addEventListener('click', this);
}

decrement() {
  this.count--;
}

handleEvent(event) {
  if (event.target === this.incrementBtn) {
    this.count++;
  }
}
```

### attributeChangedCallback(name, oldValue, newValue)

This lifecycle method is called automatically
when the value of an observed attribute changes.

To define the attributes that are observed,
implement the following static method:

```js
static get observedAttributes() {
  return ['name1', 'name2', ...];
}

attributeChangedCallback(name, oldValue, newValue) {
  // Do something with the new attribute value.
  this.render(); // assumes this method exists and updates the shadow DOM
}
```

In Lit, all properties with the `@property` and `@state` decorators
are automatically treated as observed properties.
Lit refers to them as "reactive properties".

### disconnectedCallback

This lifecycle method is called automatically
after an instance is removed from the DOM.
It can be used to remove event listeners
and clean up anything done in `connectedCallback`
if that is needed.

### adoptedCallback

This lifecycle method is called automatically
when the instance is moved to a new document.
This method is rarely used.

## Attributes

Attributes can be passed to custom elements
in the same way that attributes are passed to standard HTML elements.

The class of a custom element can:

- get the names of all provided attributes

  ```js
  const attrNames = this.getAttributeNames();
  ```

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
  el.appendChild(otherEl); // appends a single element

  el.append(node1, node2, ...); // appends any number of nodes
  ```

- set text content

  ```js
  el.textContent = 'some text';
  ```

- set inner HTML

  ```js
  el.innerHTML = 'some HTML';
  ```

## Avoiding Flash Of Undefined Custom Elements (FOUCE)

When a page containing web components is initially rendered,
the CSS and JavaScript for the components may not be loaded yet.
This can cause FOUCE and layout shift.
To avoid this, include the following CSS
which prevents rendering components that are not yet "defined".

```css
:not(:defined) {
  visibility: hidden;
}
```

A more involved approach using `customElements.whenDefined`
is described in {% aTargetBlank
"https://shoelace.style/getting-started/usage#waiting-for-components-to-load",
"Waiting for Components to Load" %}.

## Counter Example

This section shows four ways to implement a counter web component
that contains a minus button, the current count value, and a plus button.

### Vanilla with No Shadow DOM

```js
const counterTemplate = document.createElement('template');
counterTemplate.innerHTML = /*html*/ `
  <style>
    .counter {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    button {
      background-color: lightgreen;
    }

    button:disabled {
      background-color: gray;
    }
  </style>
  <div>
    <button id="decrement-btn">-</button>
    <span>${this.count}</span>
    <button id="increment-btn">+</button>
  </div>
`;

class CounterNoShadow extends HTMLElement {
  static get observedAttributes() {
    return ['count'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (this.isConnected) this.update();
  }

  connectedCallback() {
    this.appendChild(counterTemplate.content.cloneNode(true));

    this.decrementBtn = this.querySelector('#decrement-btn');
    this.decrementBtn.addEventListener('click', () => {
      this.decrement();
    });
    this.querySelector('#increment-btn').addEventListener('click', () => {
      this.increment();
    });

    this.span = this.querySelector('span');
    this.update();
  }

  // Treat the count attribute as the source of truth
  // rather than adding a property.
  get count() {
    return this.getAttribute('count') || 0;
  }

  set count(newCount) {
    return this.setAttribute('count', newCount);
  }

  decrement() {
    if (this.count === 0) return;

    this.count--;
    // this.count gets converted to a string,
    // so we have to use == instead of === on the next line.
    if (this.count == 0) {
      this.decrementBtn.setAttribute('disabled', 'disabled');
    }
    this.update();
  }

  increment() {
    this.count++;
    this.decrementBtn.removeAttribute('disabled');
    this.update();
  }

  update() {
    this.span.textContent = this.count;
  }
}

customElements.define('counter-no-shadow', CounterNoShadow);
```

### Vanilla with Shadow DOM "open"

This uses the same template used in the previous example.

```js
class CounterShadowOpen extends HTMLElement {
  static get observedAttributes() {
    return ['count'];
  }

  constructor() {
    super();
    this.attachShadow({mode: 'open'});
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (this.isConnected) this.update();
  }

  connectedCallback() {
    const root = this.shadowRoot;
    root.appendChild(counterTemplate.content.cloneNode(true));

    this.decrementBtn = root.querySelector('#decrement-btn');
    this.decrementBtn.addEventListener('click', () => {
      this.decrement();
    });
    root.querySelector('#increment-btn').addEventListener('click', () => {
      this.increment();
    });

    this.span = root.querySelector('span');
    this.update();
  }

  // Treat the count attribute as the source of truth
  // rather than adding a property.
  get count() {
    return this.getAttribute('count') || 0;
  }

  set count(newCount) {
    return this.setAttribute('count', newCount);
  }

  decrement() {
    if (this.count === 0) return;

    this.count--;
    // this.count gets converted to a string,
    // so we have to use == instead of === on the next line.
    if (this.count == 0) {
      this.decrementBtn.setAttribute('disabled', 'disabled');
    }
    this.update();
  }

  increment() {
    this.count++;
    this.decrementBtn.removeAttribute('disabled');
    this.update();
  }

  update() {
    this.span.textContent = this.count;
  }
}

customElements.define('counter-shadow-open', CounterShadowOpen);
```

### Vanilla with Shadow DOM "closed"

This uses the same template used in the previous example.

```js
class CounterShadowClosed extends HTMLElement {
  static get observedAttributes() {
    return ['count'];
  }

  constructor() {
    super();
    // When the mode is "closed", there is no shadowRoot property.
    // This prevents access to the DOM of this component from outside.
    // To access the DOM from inside this component,
    // we must capture the return value of the attachShadow method.
    this.root = this.attachShadow({mode: 'closed'});
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (this.isConnected) this.update();
  }

  connectedCallback() {
    this.root.appendChild(counterTemplate.content.cloneNode(true));

    this.decrementBtn = this.root.querySelector('#decrement-btn');
    this.decrementBtn.addEventListener('click', () => {
      this.decrement();
    });
    this.root.querySelector('#increment-btn').addEventListener('click', () => {
      this.increment();
    });

    this.span = this.root.querySelector('span');
    this.update();
  }

  // Treat the count attribute as the source of truth
  // rather than adding a property.
  get count() {
    return this.getAttribute('count') || 0;
  }

  set count(newCount) {
    return this.setAttribute('count', newCount);
  }

  decrement() {
    this.count--;
    // this.count gets converted to a string,
    // so we have to use == instead of === on the next line.
    if (this.count == 0) {
      this.decrementBtn.setAttribute('disabled', 'disabled');
    }
    this.update();
  }

  increment() {
    this.count++;
    this.decrementBtn.removeAttribute('disabled');
    this.update();
  }

  update() {
    this.span.textContent = this.count;
  }
}

customElements.define('counter-shadow-closed', CounterShadowClosed);
```

### Lit

In `package.json`:

```json
{
  "name": "lit-demo",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "vite build",
    "preview": "vite preview"
  },
  "dependencies": {
    "lit": "^3.0.0"
  },
  "devDependencies": {
    "typescript": "^5.0.0",
    "vite": "^5.0.0"
  }
}
```

In `tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ES2022",
    "moduleResolution": "node",
    "lib": ["ES2022", "dom"],
    "experimentalDecorators": true,
    "useDefineForClassFields": false
  }
}
```

In `counter-lit.ts`:

```ts
import {LitElement, css, html} from 'lit';
import {customElement, property} from 'lit/decorators.js';

@customElement('counter-lit')
export class CounterLit extends LitElement {
  static styles = css`
    .counter {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    button {
      background-color: lightgreen;
    }

    button:disabled {
      background-color: gray;
    }
  `;

  @property({type: Number}) count = 0;

  private decrement() {
    if (this.count > 0) this.count--;
  }

  private increment() {
    this.count++;
  }

  render() {
    return html`
      <div class="counter">
        <button ?disabled=${this.count === 0} @click=${this.decrement}>
          −
        </button>
        <span>${this.count}</span>
        <button @click=${this.increment}>+</button>
      </div>
    `;
  }
}
```

In `index.html`:

```html
<html>
  <head>
    <script src="counter-lit.ts" type="module"></script>
  </head>
  <body>
    <counter-lit count="3"></counter-lit>
  </body>
</html>
```
