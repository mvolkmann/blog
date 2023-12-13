---
eleventyNavigation:
  key: Alpine
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

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

The design of Alpine is somewhat based on Vue.
Vue uses `v-` for its attribute prefixes.
During the initial implementation of Alpine, it did not yet have a name,
so `x-` was chosen for its prefixes.
That is why the prefix is not something like `a-` or `alp-`.

## Using TypeScript

To use TypeScript in an Alpine project, see {% aTargetBlank
"https://dev.to/wtho/get-started-with-alpinejs-and-typescript-4dgf",
"Getting started with Alpine.js and TypeScript" %}.

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

The `x-bind` directive dynamically sets another attribute.
In some cases such as the `class` attribute it is possible to specify
a value both without and with `x-bind` and both values will be used.

A shorthand for `x-bind:` is just `:`.

For example:

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <style>
      .primary {
        border: none;
        background-color: cornflowerblue;
        color: white;
        padding: 0.5rem;
      }
      .shout {
        font-weight: bold;
        text-transform: uppercase;
      }
      .whisper {
        color: lightgray;
        text-transform: lowercase;
      }
    </style>
  </head>
  <body>
    <div x-data="{important: false}">
      <button
        class="primary"
        :class="important ? 'shout' : 'whisper'"
        @click="important = !important"
      >
        Toggle
      </button>
    </div>
  </body>
</html>
```

The value of `:class` can be a JavaScript object
whose keys are class names and whose values are boolean expressions.
Each of the class names whose corresponding boolean expression
evaluates to true will be applied.

In the following example, if the value of `score` is greater than 15,
both the "shout" and "large" CSS classes will be applied.

```html
<p :class="{shout: score > 10, large: score > 15}">Hello, World!</p>
```

Another common use of `x-bind` is conditionally disabling a button.
For example:

```html
<button :disabled="username === '' || password === ''" @click="login">
  Login
</button>
```

### x-cloak

The `x-cloak` directive hides an element until Alpine finishes processing it.
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

The `x-data` directive declares an HTML element to be an Alpine component
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

Alpine uses a mutation observer to watch for changes in element trees
whose root element contains the `x-data` directive.
This avoids wasting time watching for changes to all elements,
some of which are not affected by Alpine directives.
It does however mean that if you forget to use the `x-data` directive,
but use other Alpine directives, they will be ignored.

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

The `x-effect` directive executes JavaScript code
every time a variable it uses changes.
For example:

```html
<body x-data x-effect="updateStatus($store.data.todos)">
  <div x-effect="console.error('Problem:', problem)">...</div>
</body>
```

### x-for

The `x-for` directive repeats the contents of a `template` element
once for each item in an array.
It can only be used in `template` elements.
For example:

```html
<div x-data="{colors: ['red', 'green', 'blue']}">
  <template x-for="color in colors">
    <div x-text="color"></div>
  </template>

  <!-- This iterates over the colors AND their indexes. -->
  <template x-for="color, index in colors">
    <div x-text="`${index + 1}) ${color}`"></div>
  </template>
</div>
```

### x-html

The `x-html` directive sets the inner HTML of this element
to the result of a given JavaScript expression which is typically HTML text.
For example:

```html
<div x-html="await fetch('https://foo.com/bar').json()">...</div>
// TODO: Can you chain a call to "json" like this?
```

### x-id

The `x-id` directive is used in conjunction with the `$id` function.
See the section below describing `$id`.

### x-if

The `x-if` directive conditionally includes the contents of a `template` element.
It can only be used in `template` elements.
For example:

```html
<template x-if="score >= 21">
  <div>Winner!</div>
</template>
```

The `x-show` directive is similar.

### x-ignore

The `x-ignore` directive prevents this element and its descendants
from being initialized by Alpine.
For example:

```html
<div x-ignore>...</div>
```

### x-init

The `x-init` directive executes given JavaScript code
when this element is initialized.
For example:

```html
<div x-data="{ dogs: [] }" x-init="dogs = await (await fetch('/dogs')).json()">
  <template x-for="dog of dogs"> ... </template>
</div>
```

### x-model

The `x-model` directive creates a two-way binding between
an input value to an `x-data` variable.
Supported inputs include the HTML elements
`input`, `textarea`, and `select`.
For example:

```html
<div x-data="{ name: '' }">
  <input type="text" x-model="name" />
  <div>Hello, <span x-text="search"></span>!</div>
</div>
```

The `x-model` directive can be applied to a set of related checkboxes
to populate an array with the values of the selected checkboxes.
For example:

```html
<div x-data="{colors: ['red', 'green', 'blue'], selectedColors: []}">
  <template x-for="color in colors">
    <div style="display: flex; align-items: center;">
      <input type="checkbox" :value="color" x-model="selectedColors" />
      <span x-text="color"></span>
    </div>
  </template>
  <div x-show="selectedColors.length">
    You selected <span x-text="selectedColors"></span>.
  </div>
</div>
```

The `x-model` directive can be applied to a set of related radio buttons
to populate a variable with the value of the selected radio button.
For example:

```html
<div x-data="{colors: ['red', 'green', 'blue'], selectedColor: ''}">
  <template x-for="color in colors">
    <div style="display: flex; align-items: center">
      <input type="radio" :value="color" x-model="selectedColor" />
      <span x-text="color"></span>
    </div>
  </template>
  <div x-show="selectedColor">
    You selected <span x-text="selectedColor"></span>.
  </div>
</div>
```

The `x-model` directive can be applied to a `select` element
that allows either one or multiple selections.
For example:

```html
<div x-data="{colors: ['red', 'green', 'blue']}">
  <div x-data="{selectedColor: ''}">
    <select x-model="selectedColor">
      <option value="">Select a color</option>
      <template x-for="color in colors">
        <!-- <option :value="color" x-text="color"></option> -->
        <!-- The value of each option defaults to its text. -->
        <option x-text="color"></option>
      </template>
    </select>
    <span x-show="selectedColor">
      You selected <span x-text="selectedColor"></span>.
    </span>
  </div>
  <br />
  <div x-data="{selectedColors: []}">
    <!-- After selecting the first color, hold down
        cmd or shift before clicking another. -->
    <select multiple x-model="selectedColors">
      <template x-for="color in colors">
        <option x-text="color"></option>
      </template>
    </select>
    <span x-show="selectedColors.length">
      You selected <span x-text="selectedColors"></span>.
    </span>
  </div>
</div>
```

The following modifiers can be applied to the `x-model` directive:

- `.boolean`

  This converts the string value of the input to a boolean
  when setting the model variable.

- `.debounce`

  This delays updating the model variable until typing stops
  for some period of time (defaults to 250ms).

- `.fill`

  This uses `value` attribute to set initial value of model variable.

- `.lazy`

  This waits to update the model variable until focus leaves the input.

- `.number`:

  This converts the string value of the input to a number
  when setting the model variable.

- `.throttle`

  This updates the model variable on the first value change
  and then repeatedly after some period of time (defaults to 250ms)
  if the value has changed.

### x-modelable

The `x-modelable` directive exposes state "to the outside".
It is useful when using certain templating frameworks such as Laravel Blade.

### x-on

The `x-on` directive executes given JavaScript code when a given event occurs.
For example:

```html
<button x-on:click="like = !like">Toggle</button>
```

If the value of event handler is a function name
instead of a call to a function or other JavaScript code,
the function will be called with no arguments.
So these are equivalent:

```html
<button x-on:click="someFunction()">Do Something</button>
<button x-on:click="someFunction">Do Something</button>
```

A shorthand for `x-on:` is just `@`.

The event name must be composed of lowercase letters and dashes.
To handle events with names that contain uppercase letters or periods,
see the modifiers `camel` and `dot` described below.

The event name can be followed by the following modifiers:

- `.camel`

  HTML attributes do not support camelCasing.
  This modifier enables listening for events whose name is camelCased.
  For example, `@some-name.camel="..."` listens for `someName` events.

- `.capture`

  This executes event handling during the capture phase
  instead of the bubbling phase.

- `.debounce`

  This delays processing the event until activity stops
  for some period of time (defaults to 250ms).

- `.document`

  This causes the event listener to be registered on the `document` object
  instead of on the current element.

- `.dot`

  This modifier enables listening for events that have dots in their name.
  For example, `@some-name.camel="..."` listens for `some.name` events.

- `.once`

  This causes only the first matching event that is dispatched to be handled.

- `.outside`

  This is used on `click` events to listen for
  clicks outside the current element.
  An example where this is useful is closing a modal dialog
  when a user clicks outside it.

- `.passive`

  This can improve scrolling performance when touch events are supported.

- `.prevent`

  This calls `$event.preventDefault()` when an event is handled.

- `.self`

  This restricts the event handling to only events that were
  dispatched from the current element, not from descendant elements.

- `.stop`

  This calls `$event.stopPropagation()` when an event is handled.

- `.throttle`

  This processes the event immediately and then again after some period of time
  (defaults to 250ms) even if activity has not stopped.

- `.window`

  This causes the event listener to be registered on the `window` object
  instead of on the current element.

When listening for key events, modifiers can specify a key
that must be pressed or held down in order to trigger event handling.
These include `.alt`, `.caps-lock`, `.cmd`, `.ctrl`, `.down` (arrow key),
`.enter`, `.equal`, `.escape`, `.left` (arrow key),
`.meta` (command key in macOS, Windows key in Windows),
`.period`, `.right` (arrow key), `.shift`, `.slash`, `.space`,
`.tab`, and `.up` (arrow key).

### x-ref

The `x-ref` directive adds a reference name to this element
so other elements can access its value with `$refs.{name}`.
For example:

```html
<input type="text" x-ref="name" />
<div>Hello, <span x-text="$refs.name.value"></span>!</div>
```

### x-show

The `x-show` directive determines whether this element should be visible.

When the condition is false, the attribute `style="display: none;"` is added.
That attribute is removed when the condition is true.
For example:

```html
<h2 x-show="score == 21">Blackjack!</h2>
```

The `x-if` directive is similar.

### x-teleport

The `x-teleport` directive ...?
For example:

```html

```

### x-text

The `x-text` directive specifies the text content of this element.
For example:

```html
<div x-text="temperature >= 80 ? 'hot' : 'cold'"></div>
```

It's a shame we can't use a more terse syntax like `{var}`
instead of `<span x-text="var"></span>`.

### x-transition

The `x-transition` directive causes this element to transition in and out
when it is shown and hidden using the `x-show` directive.
By default it changes the opacity between 0 and 1
and changes the scale between 0% and 100%.

For example:

```html
<h2 x-show="score == 21" x-transition>Blackjack!</h2>
```

To change the transition duration, add the `duration` modifier.
The duration must be specified in milliseconds, not seconds.
For example:

```html
<h2 x-show="score == 21" x-transition.duration.1000ms>Blackjack!</h2>
```

To only apply only the opacity transition, add the `opacity` modifier.
Note that multiple modifiers can be applied.
For example:

```html
<h2 x-show="score == 21" x-transition.opacity.duration.1000ms>Blackjack!</h2>
```

To only apply only the scale transition, add the `scale` modifier.
For example:

```html
<h2 x-show="score == 21" x-transition.scale.duration.1000ms>Blackjack!</h2>
```

For more detail, see {% aTargetBlank
"https://alpinejs.dev/directives/transition", "x-transition" %}.

## Progress Bar Example

The following example combines some of the features we have seen so far.

<img alt="Alpine progress bar" style="width: 50%"
  src="/blog/assets/alpine-progress-bar.png?v={{pkg.version}}">

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <style>
      .progress-bg {
        background-color: gray;
        height: 2rem;
        position: relative;
        width: 20rem;
      }
      .progress-bar {
        height: 100%;
        background-color: green;
      }
      .progress-value {
        color: white;
        font-family: sans-serif;
        /* centers text in .progress-bg */
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }
    </style>
  </head>
  <body>
    <div
      x-data="{
        percent: 0,
        increment() {
          this.percent = Math.min(100, this.percent + 5);
        }
    }"
    >
      <div class="progress-bg">
        <div class="progress-bar" :style="`width: ${percent}%`"></div>
        <div class="progress-value" x-text="`${percent}%`"></div>
      </div>
      <button @click="increment" style="margin-top: 1rem">Increment</button>
    </div>
  </body>
</html>
```

## Properties

## Methods

## Magic Properties and Functions

### $data

This magic property provides access to the
`x-data` object on the nearest ancestor element.
It can be used to pass the entire object to a JavaScript function
instead of passing individual properties.

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

This magic property is a function that generates an element id
that will not conflict with other generated ids.
It is useful for defining reusable components.

The `$id` function takes a string argument
that is used as the prefix for the generated ids.
Each id generated with the same prefix uses a suffix of
a dash followed by an incrementing integer starting from 1.

For example, the following buttons will be given
the ids "abc-btn-1" and "abc-btn-2":

```html
<button :id="$id('abc-btn')">First</button>
<button :id="$id('abc-btn')">Second</button>
```

For more detail, including use of the `x-id` directive for grouping ids,
see {% aTargetBlank "https://alpinejs.dev/magics/id", "$id" %}.

### $nextTick

This magic property ...

### $refs

This magic property provides access to DOM nodes
whose elements have the `x-ref` directive.
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

The value passed as the second argument to the `Alpine.store` function
can be a boolean, number, string, array, or object.

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

To specify Alpine directives that are not tied to a rendered element,
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

## Common Mistakes

Did you forget to add the `x-data` directive on an ancestor element
of elements that use other Alpine directives?

Did you apply the `x-if` or `x-for` directives
to an element other than `template`?

## TO DOs

- Implement your Svelte todo app using only Alpine and add that to the blog page.

```

```
