---
eleventyNavigation:
  key: Alpine
layout: topic-layout.njk
---

<figure style="width: 50%">
  <img alt="Alpine logo" style="border: 0"
    src="/blog/assets/alpine-logo.png?v={{pkg.version}}">
</figure>

## Overview

"{% aTargetBlank "https://alpinejs.dev", "Alpine" %}
is a lightweight JavaScript framework that
uses custom HTML attributes to add dynamic behavior.
It adds support for 18 attributes (aka directives),
6 properties, and 2 methods.

The minified Alpine library for version 3.13.3,
which is the latest as of December 2023, is only 43K.

To use this, add the following `script` tag:

```html
<script
  defer
  src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
></script>
```

Alternatively, install Alpine with
`npm install alpine.js` or `bun add alpine.js`.
Then import, register, and start Alpine with the following
once in each distinct web page:

```js
import Alpine from 'alpinejs';
window.Alpine = Alpine; // optional for DevTools access
Alpine.start();
```

## Basic Example

The following code renders a button that toggles whether a `div` is visible.
The `x-data` attribute defines state that is
available on that element and its descendants.
The `x-show` attribute determines whether that element should be shown
based on the value of the `open` state property.
When `open` is false, the attribute `style="display: none;"`
is added to the `div`.
That attribute is removed when `open` is true.

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <body>
    <div x-data="{open: false}">
      <button @click="open = !open">Toggle</button>
      <div x-show="open">Hello, World!</div>
    </div>
  </body>
</html>
```

## Directives

Alpine 3.13.3 supports 18 directives that are each described below.
Some accept a string of JavaScript code as their value.
The JavaScript code can call builtin and custom JavaScript functions.

### x-bind

The `x-bind` attribute dynamically sets another attribute,
typically using the ternary operator.
For example:

```html
<button
  x-bind:class="canSave ? bg-green-500 : bg-red-500"
  x-bind:disabled="canSave"
>
  Save
</button>
```

A shorthand for `x-bind:` is just `:`.

### x-cloak

The `x-cloak` attribute hides an element until Alpine finishes processing it.
For example:

```html
<style>
  [x-cloak] {
    display: none;
  }
</style>

<div x-cloak ...>...</div>
```

### x-data

The `x-data` attribute declares an HTML element to be an Alpine component
and optionally declares associated state that Alpine watches for changes.
The state is scoped to this element and its descendant elements.
It can include properties and functions.

For example:

```html
<div x-data="{ fruit: 'apple', count: 0 }">...</div>
```

From {% aTargetBlank "https://alpinejs.dev/essentials/state#data-less-alpine",
"Data-less Alpine" %}, "Sometimes you may want to use Alpine functionality,
but don't need any reactive data.
In these cases, you can opt out of passing an expression to x-data."
For example: `<div x-data>`.

Another way to declare state is with the `Alpine.store` function.

The following code prompts for an image query
and displays an image from unsplash.

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <body>
    <div
      x-data="{
        imgUrl: '',
        query: '',
        getImage() {
            this.imgUrl = `https://source.unsplash.com/featured/?${this.query}`;
            this.query = '';
        }
    }"
    >
      <form @submit.prevent="getImage">
        <input type="text" x-model="query" />
        <button>Search</button>
      </form>
      <div x-show="imgUrl">
        <img :src="imgUrl" style="width: 300px" />
      </div>
    </div>
  </body>
</html>
```

### x-effect

The `x-effect` attribute executes JavaScript code
every time a variable it uses changes.
For example:

```html
<div x-effect="console.error('Problem:', problem)">...</div>
```

### x-for

The `x-for` attribute repeats this element and all its contents
once for each item in an array.
For example:

```html
<div x-for="todo in todos">...</div>
```

### x-html

The `x-html` attribute sets the inner HTML of this element
to the result of a given JavaScript expression which is typically HTML text.
For example:

```html
<div x-html="await fetch('https://foo.com/bar').json()">...</div>
// TODO: Can you chain a call to "json" like this?
```

### x-id

The `x-id` attribute ...?
For example:

```html

```

### x-if

The `x-if` attribute conditionally includes the contents of a `template` element.
It can only be used in `template` elements.
For example:

```html
<template x-if="score >= 21">
  <div>Winner!</div>
</template>
```

The `x-show` directive is similar.

### x-ignore

The `x-ignore` attribute prevents this element and its descendants
from being initialized by Alpine.
For example:

```html
<div x-ignore>...</div>
```

### x-init

The `x-init` attribute executes given JavaScript code
when this element is initialized.
For example:

```html
<div x-init="now = Date.now()">...</div>
```

### x-model

The `x-model` attribute binds an input value to an `x-data` variable.
For example:

```html
<div x-data="{ name: '' }">
  <input type="text" x-model="name" />
  <div>Hello, <span x-text="search"></span>!</div>
</div>
```

TODO: See all the modifiers that can be applied to x-model like debounce.

### x-modelable

The `x-modelable` attribute ...?
For example:

```html

```

### x-on

The `x-on` attribute executes given JavaScript code when a given event occurs.
For example:

```html
<button x-on:click="like = !like">Toggle</button>
```

A shorthand for `x-on:` is just `@`.

### x-ref

The `x-ref` attribute adds a reference name to this element
so other elements can access its value with `$refs.{name}`.
For example:

```html
<input type="text" x-ref="name" />
<div>Hello, <span x-text="$refs.name.value"></span>!</div>
```

### x-show

The `x-show` attribute determines whether this element should be visible.

When the condition is false, the attribute `style="display: none;"` is added.
That attribute is removed when the condition is true.
For example:

```html
<h2 x-show="score == 21">Blackjack!</h2>
```

The `x-if` directive is similar.

### x-teleport

The `x-teleport` attribute ...?
For example:

```html

```

### x-text

The `x-text` attribute specifies the text content of this element.
For example:

```html
<div x-text="temperature >= 80 ? 'hot' : 'cold'"></div>
```

It's a shame we can't use a more terse syntax like `{var}`
instead of `<span x-text="var"></span>`.

### x-transition

The `x-transition` attribute causes this element to transition in and out
which it is shown and hidden.
TODO: Can the transition be customized?
For example:

```html
<h2 x-show="score == 21" x-transition>Blackjack!</h2>
```

## Properties

## Methods

## Magics

### $data

This magic property ...

### $dispatch

This magic property is a function that can be called
to dispatch a standard or custom DOM event.
It must be passed an event name.
It can optionally be passed data that can be accessed with `$event.detail`.

If elements that listen for the event are not
ancestors of the elements that dispatch the event,
add the `.window` modifier when listening.
For example, `x-on="my-event.window="{code}"`.

The following code demonstrates dispatching and listening for a custom event.

```html
<div x-data="{eventData: {}}" x-on:my-event="eventData = $event.detail">
  <button x-on:click="$dispatch('my-event', {foo: 'bar'})">Send Event</button>

  <!-- After the button is clicked, this will render {"foo":"bar"}. -->
  <div x-text="JSON.stringify(eventData)"></div>

  <!-- This element is not an ancestor of the button that dispatches the event,
       so the "window" modifier is needed. -->
  <div x-on:my-event.window="console.log($event)"></div>
</div>
```

### $el

This magic property provides access to the DOM node of the current element.
For example:

```html
<button @click="$el.innerHTML = 'Clicked'">Press</button>
```

### $event

This magic property holds data associated with an event
that was dispatched using the `$dispatch` function.
It is used to access the event inside an event handling function.
See the example under `$dispatch`.

### $id

This magic property ...

### $nextTick

This magic property ...

### $refs

This magic property provides access to DOM nodes
whose elements have the `x-ref` attribute.
For example:

```html
<p x-ref="status">Pending</span>
<button @click="$refs.status.innerHTML = 'Processing'">Process</button>
```

### $root

This magic property ...

### $store

To create a named, global store, use JavaScript code like the following:

```js
document.addEventListener('alpine:init', () => {
  Alpine.store('profile', {
    username: '',
    role: '',
    someFunction(args) { ... }
  });
});
```

To access a store in the value of an Alpine directive,
use the `$store` magic property.
For example, `$store.profile.role`.

To access a store in a JavaScript function,
use code like `Alpine.store('profile')`.

### $watch

This magic property ...

## Global Methods

### Alpine.bind

### Alpine.data

### Alpine.store

The `Alpine.store` function defines a named, global store.
It takes a name string and an object with properties and optional methods.
This function can be called any number of times
to define multiple global stores.
For example:

```js
import Alpine from 'alpinejs';
Alpine.store('profile', {
  id: 0,
  username: '',
  role: ''
});
Alpine.store('todos', []);
```

Stores are accessed with the magic property `$store`.
For example:

```js
$store.profile.role = 'admin';
```

## Plugins

Alpine plugins add functionality.
Each plugin used requires a new script tag.

### Anchor

### Collapse

### Focus

### Intersect

### Mask

The {% aTargetBlank "https://alpinejs.dev/plugins/mask", "Mask" %} plugin
formats text inputs in specific ways using a mask whose characters
indicate the set of characters that can be entered.
The supported mask characters are `*` for any character,
`a` for alphabetic characters (a-z and A-Z),
and `9` for numeric characters (0-9).

For example:

```html
<html>
  <head>
    <!-- The plugin must be loaded before Alpine.js. -->
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/@alpinejs/mask@3.x.x/dist/cdn.min.js"
    ></script>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <script>
    // This returns a string like "April 16, 1961".
    const formatDate = date =>
      new Date(date).toLocaleDateString('default', {
        month: 'long',
        day: 'numeric',
        year: 'numeric'
      });
  </script>
  <body x-data="{date: '', placeholder: 'MM/DD/YYYY'}">
    <input
      :placeholder="placeholder"
      size="12"
      x-mask="99/99/9999"
      x-model="date"
    />
    <div x-show="date.length == placeholder.length">
      You entered <span x-text="formatDate(date)"></span>.
    </div>
  </body>
</html>
```

The `x-mask:dynamic` directive takes a JavaScript function name
that is called to determine the mask based what has been entered so far.
For entering currency amounts, use the value `$money($input)`.

### Morph

### Persist

## Miscellaneous Details

To specify Alpine attributes that are not tied to a rendered element,
add them to a `template` element.
These are somewhat like fragments in React.
For example:

```html
<div x-data="{colors: ['red', 'green', 'blue']}">
  <template x-for="color in colors">
    <div x-text="color"></div>
  </template>
</div>
```

## Advanced Topics

### Async

### CSP

### Extending

### Reactivity

## TO DOs

- Implement your Svelte todo app using only Alpine and add that to the blog page.
