---
eleventyNavigation:
  key: JavaScript Signals
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="JavaScript Signals logo" style="border: 0"
    src="/blog/assets/signals-logo.png?v={{pkg.version}}">
</figure>

### Overview

The <a href="https://github.com/tc39/proposal-signals"
target="_blank">JavaScript Signals standard proposal</a>
aims to provide a standard way to manage state in JavaScript applications
that is reactive.
This is useful in both user interface and server-side code.

The design is the result of collaboration between the teams from
Angular, Ember, MobX, Preact, Qwik, RxJS, Solid, Svelte, Vue, and more.
Noticably absent from this list is the React team.
The goal is for signals to be usable in multiple frameworks.

The proposal defines a new JavaScript namespace named "Signal".
The Signal namespace:

- provides a method to define state with an initial value
- provides a method to define state that is computed from other state
- performs lazy evaluation of state so it is not computed until needed
- memoizes the last computated value of each piece of state
  to avoid repeating computations that will result in the same value

A <a href="https://github.com/tc39/proposal-signals/tree/main/packages/signal-polyfill"
target="_blank">Signal Polyfill</a> is available for use now.

Perhaps when this proposal is approved and becomes part of JavaScript,
browser vendors will implement new DevTools
that track and display the state maintained by signals.

## Define State

The following code defines a piece of state with an initial value:

```js
const counter = new Signal.State(0);
```

## Define Computed State

The following code defines two pieces of state
that are computed from other state:

```js
const isEven = new Signal.Computed(() => (counter.get() & 1) == 0);
const parity = new Signal.Computed(() => (isEven.get() ? 'even' : 'odd'));
```

## Retrieve State

The following code gets the value of a piece of state:

```js
const value = counter.get();
```

## Modify State

The following code modifies a piece of state:

```js
counter.set(10);
```

## effect Function

The provided polyfill does not provide the `effect` function.
However, a suggested implementation is provided at
<a href="https://github.com/tc39/proposal-signals/tree/main/packages/signal-polyfill#creating-a-simple-effect"
target="_blank">Creating a simple effect</a> which is
similar to the following code that I placed in the file `effect.ts`:

```js
import {Signal} from 'signal-polyfill';

let needsEnqueue = true;

function processPending() {
  needsEnqueue = true;
  for (const s of watcher.getPending()) {
    s.get();
  }
  watcher.watch();
}

const watcher = new Signal.subtle.Watcher(() => {
  if (needsEnqueue) {
    needsEnqueue = false;
    queueMicrotask(processPending);
  }
});

type Cleanup = () => void;
type Callback = () => Cleanup | void;
export function effect(callback: Callback) {
  // The callback function passed to the effect function
  // can optionally return a "cleanup" function.
  // If it does then the cleanup function is called every time
  // a piece of state used in the callback function changes,
  // and again if the function returned by this one is called.
  let cleanup: Cleanup | undefined;

  const computed = new Signal.Computed(() => {
    if (typeof cleanup === 'function') cleanup();
    cleanup = callback() || undefined;
  });

  watcher.watch(computed);
  computed.get();

  // The caller of "effect" can call this returned function
  // to stop watching for state changes.
  return () => {
    watcher.unwatch(computed);
    if (typeof cleanup === 'function') cleanup();
  };
}
```

## Execute Code When State Changes

The following code executes a function
every time the value of any state it uses changes:

```js
// This prints the value of the parity state in the DevTools console
// initially, and again every time it changes.
effect(() => console.log(parity.get()));

effect(() => {
  // The setInnerText function is defined below.
  // n1 and n2 are Signal.State objects that hold numbers.
  // Assume that there is a span element with an id of "sum".
  // The first argument to setInnerText is a CSS selector.
  setInnerText('#sum', n1.get() + n2.get());
});
```

## Utility Functions

The following are examples of utility functions
that make working with signals easier.
I placed this code in the file `utilities.ts`:

```js
import {effect} from './effect';
import {Signal} from 'signal-polyfill';

/** Creates a two-way binding between an input element and a state. */
export function bindNumberInput(selector: string, state: Signal.State<any>) {
  const element = getElement(selector) as HTMLInputElement;
  element.onchange = () => state.set(Number(element.value));
  effect(() => (element.value = state.get()));
}

export function getElement(selector: string): HTMLElement {
  const element = document.querySelector(selector) as HTMLElement;
  if (!element) throw new Error(`No element found for selector: ${selector}`);
  return element;
}

export function setInnerText(selector: string, value: string | number) {
  const element = getElement(selector) as HTMLInputElement;
  element.innerText = String(value);
}
```

## Demo App

For a web app that uses the Signals API, see
<a href="https://github.com/mvolkmann/js-signals-demo"
target="_blank">js-signals-demo</a>.

- The root directory contains the file `index.html`.
- The `public` directory contains the file `styles.css`.
- The `src` directory contains the files
  `index.ts`, `effect.ts`, and `utilities.ts`.

<img alt="JavaScript Signals demo app" style="width: 60%"
  src="/blog/assets/js-signals-demo.png?v={{pkg.version}}">
