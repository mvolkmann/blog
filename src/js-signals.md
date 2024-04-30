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
The goal is the signals to be useful in multiple frameworks.

The proposal defines a new JavaScript namespace named "Signal".
The Signal namespace defines methods to:

- define state with initial values
- define state that is computed from other state
- lazy evaluation of state so it is not computed until needed
- memoize the last computation of state to avoid
  repeating computations that will result in the same value

A polyfill is available for use now.

...

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
