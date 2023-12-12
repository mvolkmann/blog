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

To use this, add the following `script` tag:

```html
<script
  defer
  src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
></script>
```

Alternatively, install Alpine with
`npm install alpine.js` or `bun add alpine.js`.
Then import, register, and start Alpine with the following:

```js
import Alpine from 'alpinejs';
window.Alpine = Alpine;
Alpine.start();
```

## Attributes

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

### x-cloak

The `x-cloak` attribute hides an element until Alpine finishes processing it.
For example:

```html
<div x-cloak ...>...</div>
```

### x-data

The `x-data` attribute declares an HTML element to be an Alpine component
and declares its associated data.
For example:

```html
<div x-data={ fruit: 'apple', count: 0 }>...</div>
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

The `x-if` attribute conditionally includes this element.
For example:

```html
<div x-if="score >= 21">...</div>
```

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

### x-ref

The `x-ref` attribute adds a reference name to this element
so other elements can access its value with `$refs.{name}`.
For example:

```html
<input type="text" x-ref="name" />
<div>Hello, <span x-text="$refs.name.value"></span>!</div>
```

### x-show

The `x-show` attribute determines whether this element is visible.
For example:

```html
<h2 x-show="score == 21">Blackjack!</h2>
```

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

### $dispatch

### $el

### $id

### $nextTick

### $refs

### $root

### $store

### $watch

## Global Methods

### Alpine.bind

### Alpine.data

### Alpine.store

## Plugins

### Anchor

### Collapse

### Focus

### Intersect

### Mask

### Morph

### Persist

## Advanced Topics

### Async

### CSP

### Extending

### Reactivity
