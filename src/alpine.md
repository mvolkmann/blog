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

Alpine was created by Caleb Porzio.
He also created {% aTargetBlank "https://laravel-livewire.com", "Livewire" %},
a full stack framework for Laravel which uses PHP.
Quoting Caleb, "Alpine.js offers you the reactive and declarative nature
of big frameworks like Vue or React at a much lower cost.
You get to keep your DOM, and sprinkle in behavior as you see fit."

The minified Alpine library for version 3.13.3,
which is the latest as of December 2023, is only 17K.

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
Vue uses `v-` for its directive prefixes.
During the initial implementation of Alpine, it did not yet have a name,
so `x-` was chosen for its prefixes.
That is why the prefix is not something like `a-` or `alp-`.
Many Alpine directives have the same name suffix as a Vue directives.

## Using TypeScript

To use TypeScript in an Alpine project, see {% aTargetBlank
"https://dev.to/wtho/get-started-with-alpinejs-and-typescript-4dgf",
"Getting started with Alpine.js and TypeScript" %}.

The following steps create a new project that uses Alpine, TypeScript, and Vite.
Vite provides a local HTTP server with hot reload.

- Enter `npm init vite@latest`

  - Enter a project name.
  - For the framework, select "Vanilla".
  - For the variant, select "TypeScript".

- cd to the newly created project directory
- Enter `npm install alpinejs`
- Enter `npm install -D @types/alpinejs`
- Replace the contents of `src/main.ts` with the following:

  ```ts
  import Alpine from 'alpinejs';
  window.Alpine = Alpine;
  Alpine.start();
  ```

- Create the file `src/global.d.ts` containing the following:

  ```ts
  import {Alpine as AlpineType} from 'alpinejs';

  declare global {
    var Alpine: AlpineType;
  }
  ```

- Edit `index.html` which already contains a `script` tag for `/src/main.ts`.
  Add HTML that uses Alpine directives here.

- Enter `npm run dev`

- Browse localhost:5173.

## Basic Example

The following code renders a button that toggles whether a `div` is visible.
The `x-data` directive defines state that is
available on that element and its descendants.
The `x-show` directive determines whether that element should be shown
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

Elements can access state properties on any of their ancestor elements.
When the same property name is used at multiple nesting levels,
the value of the nearest one is used.

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

If array items will be added, deleted, or reordered,
the `template` element should include the `key` attribute
with a unique value for each array item.
This enables Alpine to better managing updating the DOM.
For example:

```html
<div
  x-data="{
    addDog() {
      this.dogs.push({
        id: this.dogs.length + 1,
        name: 'Goofy',
        breed: 'Bloodhound',
      })
    },
    dogs: [
      {id: 1, name: 'Maisey', breed: 'Treeing Walker Coonhound'},
      {id: 2, name: 'Ramsay', breed: 'Native American Indian Dog'},
      {id: 3, name: 'Oscar', breed: 'German Shorthaired Pointer'},
      {id: 4, name: 'Comet', breed: 'Whippet'},
    ],
    getSortedDogs() {
      return this.dogs.sort((a, b) => a.name.localeCompare(b.name))
    }
  }"
>
  <template x-for="dog in getSortedDogs()" :key="dog.id">
    <div x-text="`${dog.name} is a ${dog.breed}.`"></div>
  </template>
  <button @click="addDog">Add Dog</button>
</div>
```

### x-html

The `x-html` directive sets the inner HTML of this element
to the result of a given JavaScript expression which is typically HTML text.
For example:

```html
<div x-html="await (await fetch('https://foo.com/bar')).json()">...</div>
```

The official documentation provides this warning:

"Only use on trusted content and never on user-provided content.
Dynamically rendering HTML from third parties
can easily lead to XSS vulnerabilities."

### x-id

The `x-id` directive is used in conjunction with the `$id` function.
See the section below describing `$id`.

### x-if

The `x-if` directive conditionally includes the contents of a `template` element.
It can only be used in `template` elements.
When the condition is false, the element will not be included in the DOM.

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
  To change the time period, add a period and a number of milliseconds,
  optionally followed by "ms". For example, `.debounce.500`.

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
  To change the time period, add a period and a number of milliseconds,
  optionally followed by "ms". For example, `.throttle.500`.

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
  To change the time period, add a period and a number of milliseconds,
  optionally followed by "ms". For example, `.debounce.500`.

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
  when a user clicks outside it or presses the escape key.

  For example:

  ```html
  <html lang="en">
    <head>
      <script
        defer
        src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
      ></script>
      <style>
        .modal {
          border: 1px solid red;
          padding: 1rem;
          position: absolute;
          left: 50%;
          top: 50%;
          transform: translate(-50%, -50%);
      </style>
    </head>
    <body>
      <div x-data="{show: false}">
        <button @click="show = true">Show</button>
        <p
          class="modal"
          x-show="show"
          @click.outside="show = false"
          @keyup.escape.window="show = false"
        >
          Click outside to hide.
        </p>
      </div>
    </body>
  </html>
  ```

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
  To change the time period, add a period and a number of milliseconds,
  optionally followed by "ms". For example, `.throttle.500`.

- `.window`

  This causes the event listener to be registered on the `window` object
  instead of on the current element.

When listening for key events, modifiers can specify a key
that must be pressed or held down in order to trigger event handling.
These include `.alt`, `.caps-lock`, `.cmd`, `.ctrl`, `.down` (arrow key),
`.enter` (return key), `.equal`, `.escape`, `.left` (arrow key),
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

The following example demonstrates using `x-ref` and `$refs`
to manage a modal dialog created with the HTML `dialog` element.
It can be closed by clicking the "OK" button,
clicking outside the dialog, or pressing the escape key.
The `dialog` element provides checking for the escape key.

```html
<div
  x-data
  // This handles all dialogs.
  @click="if ($event.target.nodeName === 'DIALOG') $event.target.close()"
  // This only handles a specific dialog.
  // @click="if ($event.target === $refs.myDialog) $event.target.close()"
>
  <button @click="$refs.myDialog.showModal()">Open Dialog</button>
  <!-- We are adjusting margins and padding so the
        form entirely fills the dialog, making all clicks
        inside the dialog be seen as clicks on the form. -->
  <dialog style="padding: 0" x-ref="myDialog">
    <!-- When a form with method="dialog" is inside a
          dialog element, submitting the form closes the dialog. -->
    <form method="dialog" style="margin: 0; padding: 1rem">
      <p style="margin-top: 0">This is the dialog content.</p>
      <button>OK</button>
    </form>
  </dialog>
</div>
```

The following example demonstrates using `x-ref` and `$refs`
to allow users to copy the value of an `input` element to the system clipboard.
This is done by clicking a button to the right of the `input`.
After the button is clicked, a green checkmark is displayed for one second.
Note how `$data` is passed to the `copy` function
so it can modify the value of the `copied` property
which is used to determine whether the green checkmark should be displayed.

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <style>
      button.plain {
        background-color: transparent;
        border: none;
        cursor: pointer;
        padding: none;
      }
      .copied {
        color: green;
      }
      .row {
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
    </style>
    <script>
      function copy(input, data) {
        navigator.clipboard.writeText(input.value);
        data.copied = true;
        setTimeout(() => (data.copied = false), 1000);
      }
    </script>
  </head>
  <body>
    <div class="row" x-data="{copied: false}">
      <input size="20" type="text" x-ref="input" />
      <button class="plain" @click="copy($refs.input, $data)">📋</button>
      <div class="copied" x-show="copied">✓</div>
    </div>
  </body>
</html>
```

### x-show

The `x-show` directive determines whether this element should be visible.
The element will be present in the DOM regardless.
When the condition is false, the attribute `style="display: none;"` is added.
That attribute is removed when the condition is true.

For example:

```html
<h2 x-show="score == 21">Blackjack!</h2>
```

The `x-if` directive is similar.
If constructing the element is expensive
(for example, sending an API request to fetch data),
it is preferred to use `x-if` in order to avoid
taking the time to construct an element that will not be visible.

### x-teleport

The `x-teleport` directive transports a `template` element
to another part of the DOM.
One use is to attach a modal dialog to the `body` element.

### x-text

The `x-text` directive specifies the text content of this element.
For example:

```html
<div x-text="temperature >= 80 ? 'hot' : 'cold'"></div>
```

{% raw %}
It's a shame we can't use a more terse syntax like `{{expression}}`
instead of `<span x-text="var"></span>`.
In the Alpine Discord channel, kwoka said
"A plugin to make {{}} work within a part of a tree would be possible."
TODO: Try to write this plugin!
{% endraw %}

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

## Custom Directives

In addition to the provided directives, custom directives can be implemented.
The following code demonstrates defining
a custom directive named `x-weather-feel`.

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <script>
      document.addEventListener('alpine:init', () => {
        Alpine.directive(
          'weather-feel', // Alpine prepends "x-" to the name.
          (el, {expression}, {effect, evaluate}) => {
            // "el" is a reference to the DOM element
            // where the directive is applied.
            // "expression" is JavaScript expression assigned to the directive.
            // "effect" is passed a function to run every time
            // the value of the expression changes.
            // "evaluate" is a function that evaluates a JavaScript expression
            // and returns the result.
            effect(() => {
              const value = evaluate(expression);
              el.textContent =
                value > 80 ? 'hot' : value < 50 ? 'cold' : 'warm';
            });
          }
        );
      });
    </script>
  </head>
  <body x-data="{ temperature: 50 }">
    <input type="range" min="0" max="100" x-model="temperature" />
    <div>
      The temperature is <span x-text="temperature"></span> and I feel
      <span x-weather-feel="temperature"></span>.
    </div>
  </body>
</html>
```

For more detail, see {% aTargetBlank
"https://alpinejs.dev/advanced/extending#custom-directives",
"Custom Directives" %}.

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

## Todo List Example

<img alt="Alpine todo list" style="width: 50%"
  src="/blog/assets/alpine-todo-list.png?v={{pkg.version}}">

```html
<html>
  <head>
    <link rel="stylesheet" href="todo-list.css" />
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <script>
      let data;
      let lastId = 0;

      document.addEventListener('alpine:init', () => {
        Alpine.store('data', {status: '', todos: []});
        data = Alpine.store('data');
        addTodo('buy milk');
        addTodo('cut grass');
      });

      function addTodo(text) {
        data.todos.push({id: ++lastId, text: text.trim(), done: false});
      }

      // This keeps only the todos that are not done.
      function archiveCompleted() {
        data.todos = data.todos.filter(t => !t.done);
      }

      function deleteTodo(todoId) {
        data.todos = data.todos.filter(t => t.id !== todoId);
      }

      function updateStatus(todos) {
        const uncompletedCount = todos.filter(t => !t.done).length;
        data.status = `${uncompletedCount} of ${todos.length} remaining`;
      }
    </script>
  </head>
  <body x-data x-effect="updateStatus(data.todos)">
    <h1>To Do List</h1>
    <div>
      <span x-text="data.status"></span>
      <button @click="archiveCompleted()">Archive Completed</button>
    </div>
    <form x-data="{text: ''}" @submit.prevent="addTodo(text); text = ''">
      <input
        autofocus
        placeholder="enter new todo here"
        size="30"
        type="text"
        x-model="text"
      />
      <button :disabled="text.trim().length === 0">Add</button>
    </form>
    <ul>
      <template x-for="todo in data.todos">
        <li class="todo-row">
          <input type="checkbox" x-model="todo.done" />
          <span :class="{done: todo.done}" x-text="todo.text"></span>
          <button @click="deleteTodo(todo.id)">Delete</button>
        </li>
      </template>
    </ul>
  </body>
</html>
```

## Properties

## Methods

## Magic Properties and Functions

### $data

This magic property provides access to the `x-data` object
on the nearest ancestor element that uses that directive.
It can be used to pass the entire object to a JavaScript function
instead of passing individual properties.

### $dispatch

This magic property is a function that can be called
to dispatch a standard or custom DOM event.
It must be passed an event name.
It can optionally be passed any kind of data
that can be accessed with `$event.detail`.

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

This magic property provides access to the current DOM element.
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

This magic property is a function that takes a function to be executed
after Alpine finishes updating the DOM.
It is useful for inspecting the DOM or making additional changes to it.

This function returns a `Promise`, so the `await` keyword
can be used to wait for it to settle.

### $refs

This magic property provides access to DOM elements
to which the `x-ref` directive is applied.
Its value is an object.
The keys are the string values specified by all the `x-ref` directives.
The values are the corresponding DOM element objects.

For example:

```html
<p x-ref="status">Pending</span>
<button @click="$refs.status.innerHTML = 'Processing'">Process</button>
```

### $root

This magic property holds a reference to
the root DOM element of the current component which is
the nearest ancestor element on which the `x-data` directive is applied.

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

This magic property is a function that can be
called to watch any component property.
It is passed a property name and a function
that is called when the property value changes.
The function is passed the current value.

The following example demonstrates using `$watch`
to watch primitive, object, and array properties.
The "Change Dog" and "Change Scores" buttons
only trigger displaying an alert dialog once because
after the first click they do not change a value.

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <script>
      const data = {
        dog: {name: 'Comet', breed: 'Whippet'},
        password: '',
        scores: [7, 10]
      };

      function configureWatches(watch) {
        watch('dog', value => {
          alert('dog is now ' + JSON.stringify(value));
        });

        watch('password', value => {
          if (value.startsWith('pass')) {
            alert('Bad password!');
          }
        });

        watch('scores', value => {
          alert('scores are now ' + JSON.stringify(value));
        });
      }
    </script>
  </head>
  <body>
    <div x-data="data" x-init="configureWatches($watch)">
      <label>
        Password
        <input autofocus size="20" type="password" x-model="password" />
      </label>
      <button @click="dog.breed = 'Greyhound'">Change Dog</button>
      <button @click="scores[0] = 19">Change Score</button>
    </div>
  </body>
</html>
```

The following is another example of using `$watch`.
It is based on the YouTube video {% aTargetBlank
"https://youtu.be/uc6D2yocRsI?si=ZecBbjHha7r0ugus",
"Learn Alpine.js: Project - Hackernews search" %}
by {% aTargetBlank "https://www.youtube.com/@codecourse", "Codecourse" %}.
This allows users to enter a query which is used to search for matching
articles on {% aTargetBlank "https://news.ycombinator.com", "HackerNews" %}.

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/gh/mvolkmann/alpine-plugins@v0.0.5/interpolate.js"
    ></script>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <script>
      async function search(query, data) {
        if (query === '') {
          data.results = [];
          return;
        }

        try {
          // This endpoint limits the number of hits returned to 20.
          const url = 'https://hn.algolia.com/api/v1/search?query=' + query;
          const response = await fetch(url);
          const json = await response.json();
          data.results = json.hits;
        } catch (e) {
          data.error = e.message;
        }
      }
    </script>
  </head>
  <body>
    <div
      x-data="{error: undefined, query: '', results: []}"
      x-init="$watch('query', query => search(query, $data))"
      x-interpolate
    >
      <label>
        Query
        <input autofocus size="20" type="text" x-model.debounce.300="query" />
      </label>
      <div style="color: red" x-show="error">Error: {error}</div>
      <div x-show="results.length">
        The search for "{query}" returned {results.length} results.
      </div>
      <template x-for="result of results" :key="result.objectID">
        <div>
          <h4 style="margin-bottom: 0">{result.title}</h4>
          <a :href="result.url" target="_blank">{result.url}</a>
        </div>
      </template>
    </div>
  </body>
</html>
```

In addition to the provided magic properties,
custom magic properties can be implemented.
See {% aTargetBlank "https://alpinejs.dev/advanced/extending#custom-magics",
"Custom Magics" %}.

## Global Functions

### Alpine.bind

The `Alpine.bind` function provides a way to define a named set of
attributes and directives that can be applied to multiple elements
to avoid repetition and allow changes to be made in one place.

This is likely rarely needed.

For a contrived example, see {% aTargetBlank
"https://alpinejs.dev/globals/alpine-bind", "Alpine.bind" %}.

### Alpine.data

The `Alpine.data` function provides an alternate way
to specify the value of an `x-data` property.
It is useful for long lists of properties
and properties that are function definitions.
While these can be defined using the `x-data` property,
long definitions can feel out of place in HTML.
Defining them in a `script` tag is sometimes preferable.

For example:

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <script defer>
      document.addEventListener('alpine:init', () => {
        Alpine.data('counter', () => ({
          count: 5,
          decrement() {
            if (this.count > 0) this.count--;
          },
          increment() {
            if (this.count < 10) this.count++;
          }
        }));
      });
    </script>
  </head>
  <body>
    <div style="display: flex; gap: 1rem" x-data="counter">
      <button @click="decrement" :disabled="count === 0">-</button>
      <div x-text="count"></div>
      <button @click="increment" :disabled="count === 10">+</button>
    </div>
  </body>
</html>
```

For more detail, see {% aTargetBlank
"https://alpinejs.dev/globals/alpine-data", "Alpine.data" %}.

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

The {% aTargetBlank "https://alpinejs.dev/plugins/anchor", "Anchor" %} plugin
anchors the position of an element to another using CSS absolute positioning.

In most cases this functionality can be achieved with HTML nesting and CSS.

### Collapse

The {% aTargetBlank "https://alpinejs.dev/plugins/collapse", "Collapse" %}
plugin enables expanding and collapsing elements with smooth animations.

For example:

```html
<html>
  <head>
    <!-- The plugin must be loaded before Alpine.js. -->
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/@alpinejs/collapse@3.x.x/dist/cdn.min.js"
    ></script>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <body x-data="{show: true}">
    <button @click="show = !show">Toggle</button>
    <p x-show="show" x-collapse>
      Out of memory.<br />
      We wish to hold the whole sky,<br />
      But we never will.
    </p>
  </body>
</html>
```

### Focus

The {% aTargetBlank "https://alpinejs.dev/plugins/focus", "Focus" %}
plugin manages focus within a page containing form elements.

### Intersect

The {% aTargetBlank "https://alpinejs.dev/plugins/intersect", "Intersect" %}
plugin enables reacting to an element entering the browser viewport.

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

The {% aTargetBlank "https://alpinejs.dev/plugins/morph", "Morph" %} plugin
enables moving an element into a provided HTML template.
It is useful for updating the DOM from HTML received from an API request
without losing page state.

### Persist

The {% aTargetBlank "https://alpinejs.dev/plugins/persist", "Persist" %}
plugin saves state across page loads, including browser refreshes.
By default the data is saved in `localStorage`.

To use this plugin, add the following `script` tag before the one for Alpine:

```html
<script
  defer
  src="https://cdn.jsdelivr.net/npm/@alpinejs/persist@3.x.x/dist/cdn.min.js"
></script>
```

Alternatively, install the plugin from NPM and initialize it in code.

To cause any `x-data` property to be persisted,
wrap the value in a call to the `$persist` function.

The following code demonstrates persisting the `name` data property.

```html
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/@alpinejs/persist@3.x.x/dist/cdn.min.js"
    ></script>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <body x-data="{ name: $persist('World') }">
    <input autofocus size="20" type="text" x-model="name" />
    <div>Hello, <span x-text="name"></span>!</div>
  </body>
</html>
```

If the type of a persisted property is changed,
clear `localStorage` before running the code again.

Inside `localStorage`, persisted property names are prepended with `_x_`.

If there are multiple pages at the same domain that persist
properties with the same name, they will share the `localStorage` value.
If this is not desired, specify a different name with the `as` modifier.
For example, `x-data="{ $persist(name).as('company-name') }"`
When this modifier is used, the name will not be prepended with `_x_`.

Data saved in `localStorage` remains across sessions of the app.
To avoid this, the data can instead be saved in `sessionStorage`
by applying the `using modifier`.
For example, `x-data="{ $persist(name).using(sessionStorage) }"`

It is also possible to define a custom storage location. For details, see
<a href="https://alpinejs.dev/plugins/persist#custom-storage"
target="_blank">Using a custom storage</a>.

To persist data in a store, use the `Alpine.$persist` function.
This seems to require removing `defer` from the plugin script tag.
For example:

```js
Alpine.store('data', {
  status: '',
  todos: Alpine.$persist([]).as('todos')
});
```

## Components

One issue with Alpine is that it doesn't support defining components
in the sense that SPA frameworks like React do.
For example, we can't define a "ProgressBar" component and then
render it with HTML like `<ProgressBar value={value} max={100} />`.

We can approximate this though with a bit of JavaScript code
that searches the DOM for elements that have an attribute
that specifies a file to load that defines a component.
We can make data available to an instance of a component
using the standard `x-data` Alpine attribute.

Here is the JavaScript code from a file named "x-include.js":

```js
const {href} = location;
const lastSlashIndex = href.lastIndexOf('/');
const urlPrefix = href.substring(0, lastSlashIndex + 1);

function includeHTML() {
  const attribute = 'x-include';
  // Find the first element that contains the include attribute.
  const element = document.querySelector(`[${attribute}]`);
  if (!element) return; // no more found

  const xhr = new XMLHttpRequest();
  xhr.onload = () => {
    element.innerHTML = xhr.responseText;
    element.removeAttribute(attribute);
    // Make a recursive call to process remaining elements
    // with the w3-include-html attribute.
    includeHTML();
  };

  const file = element.getAttribute(attribute);
  xhr.open('GET', urlPrefix + file, true);
  xhr.send();
}

window.onload = includeHTML;
```

Here is an example of using "x-include.js" in a file named "demo.html":

```html
<html lang="en">
  <head>
    <title>Include Demo</title>
    <!-- All included "components" can also use Alpine. -->
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
    <script src="x-include.js"></script>
  </head>
  <body x-data="{show: true}">
    <h1>Include Demo</h1>
    <div style="display: flex; gap: 1rem">
      <button @click="show = !show">Toggle</button>
      <span x-show="show">
        <!-- The file greeting.html defines a component.
             We can pass scoped data to it using x-data. -->
        <span x-include="greeting.html" x-data="{name: 'World'}"></span>
      </span>
    </div>

    <!-- The file colors.html defines a component.
         We can pass scoped data to it using x-data.
         The file will only be loaded once even though
         we "include" it multiple times. -->
    <div x-include="colors.html"></div>
    <div x-include="colors.html" x-data="{upper: true}"></div>
  </body>
</html>
```

Here is the contents of "greeting.html":

```html
<p style="margin: 0">Hello, <span x-text="name"></span>!</p>
```

Here is the contents of "colors.html":

```html
<div x-data="{colors: ['red', 'green', 'blue']}">
  <ol>
    <template x-for="color in colors">
      <!-- Using $data.upper instead of just upper
           works when upper is not defined. -->
      <li x-text="$data.upper ? color.toUpperCase() : color"></li>
    </template>
  </ol>
</div>
```

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

### AJAX

{% aTargetBlank "https://alpine-ajax.js.org", "Alpine AJAX" %}
is an Alpine plugin that
"enables your HTML elements to request remote content from your server."
It provides functionality that is similar to {% aTargetBlank
"https://htmx.org", "HTMX" %}, but is simpler and provides fewer features.

Alpine AJAX was not created by the team that created Alpine.

### Async

The `await` keyword can be used in directives that take a function
to wait for the result of an `async` function.
For example, `<span x-text="await getHighScore()"></span>`.

### CSP

For applications that must adhere to the Content Security Policy (CSP),
see {% aTargetBlank "https://alpinejs.dev/advanced/csp", "CSP" %}.

### Reactivity

Alpine monitors state defined in `x-data` directives and stores and
automatically updates parts of the UI that use the data when the values change.
This is implemented by the functions `Alpine.reactive` and `Alpine.effect`.
For more detail, see {% aTargetBlank
"https://alpinejs.dev/advanced/reactivity", "Reactivity" %}.

## Common Mistakes

Did you forget to add the `x-data` directive on an ancestor element
of elements that use other Alpine directives?

Did you apply the `x-if` or `x-for` directives
to an element other than `template`?

## Resources

- {% aTargetBlank "https://alpinejs.dev", "Alpine home page" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=4c8dpZN0rqM&t=1s", "Say No To Complexity With AlpineJS" %} YouTube video by Caleb Porzio
- {% aTargetBlank "https://mvolkmann.github.io/blog/topics/#/blog/alpine/?v=1.0.22", "Mark Volkmann's Alpine blog page" %}
- {% aTargetBlank "https://www.alpinetoolbox.com", "Alpine Toolbox" %}
