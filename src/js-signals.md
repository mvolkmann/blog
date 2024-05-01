---
eleventyNavigation:
  key: JavaScript Signals
layout: topic-layout.njk
---

<figure style="width: 30%">
  <img alt="OCaml logo" style="border: 0"
    src="/blog/assets/signals-logo.png?v={{pkg.version}}">
</figure>

### Overview

The <a href="https://github.com/tc39/proposal-signals"
target="_blank">JavaScript Signals standard proposal</a>
aims to provide a standard way to manage state in JavaScript applications
that is "reactive".
This is useful in both user interface and server-side code.

The design is the result of collaboration between the teams from
Angular, Ember, MobX, Preact, Qwik, RxJS, Solid, Svelte, Vue, and more.
Noticable absent from this list is the React team.
The goal is for signals to be usable in multiple frameworks.

The proposal defines a new JavaScript namespace named "Signal".
The Signal namespace:

- provides a method to define state with initial values
- provides a method to define state that is computed from other state
- performs lazy evaluation of state so it is not computed until needed
- memoizes the last computation of each piece of state to avoid
  repeating computations that will result in the same value

A polyfill is available for use now.

There is a potential for browsers to implement new DevTools
that track and display the state of signals.

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

## Execute Code When State Changes

The following code executes a function when any state it uses changes:

```js
// This prints the value of the parity state in the DevTools console
// initially, and again every time it changes.
effect(() => console.log(parity.get()));

// This waits for the DOM to load before trying to find elements.
window.onload = () => {
  const count = document.getElementById('count');
  const target = document.getElementById('target');
  effect(() => {
    if (count) count.innerText = String(counter.get());
    if (target) target.innerText = parity.get();
    // Optionally return a cleanup function.
    // return () => {
    //   console.log("index.ts: performing cleanup");
    // };
  });
};
```
