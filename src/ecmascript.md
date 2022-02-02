---
eleventyNavigation:
  key: ECMAScript
layout: topic-layout.njk
---

## 2020 Highlights

- dynamic imports

  ```js
  import('/some/name.js').then(module => {
    // use the module
  });

  const module = await import('/some/name.js');
  ```

- namespace re-exporting

  ```js
  export {foo, bar as baz} from 'some-module';
  export _ from 'some-module';
  export _ as newName from 'some-module';
  ```

- optional chaining

  ```js
  const zip = person?.address?.zipCode;
  // sets to undefined if person or person.address is undefined

  const bloodType = person?.getHealthData?.().getBloodType?.();
  // sets to undefined if person has no getHealthData method or if
  // the object returned by getHealthData has no getBloodType method
  ```

- nullish coalescing

  ```js
  const age = person.age || 21;
  // sets to 21 if expression on left
  // evaluates to a falsey value, including zero

  const age = person.age ?? 21;
  // sets to 21 if expression on left
  // evaluates to undefined or null
  ```

- `BigInt` type

  supports arbitrarily large integer values

- `String` `matchAll` method

  returns iterator over all matches of a
  regular expression that must have "g" flag

  ```js
  const someText = 'fooA1barB2bazC3';
  const matches = someText.matchAll(/[A-Z][0-9]/g);
  for (const match of matches) {
    console.log(match[0], 'begins at', match.index);
  }
  ```

  outputs:
  A1 begins at 3
  B2 begins at 8
  C3 begins at 13

- `Promise.allSettled`

  - takes an `Iterable` of promises
  - resolves after all either resolve or reject
  - resolved value is an array of object that each have a `status` property
  - if status is `'fulfilled'`, a `value` property is also present
  - if status is `'rejected'`, a `reason` property is also present

- `globalThis`

  - provides a standard way to access
    the "global object" in all environments
  - alternative to `window`, `self`, or `frames` on the web
  - alternative to `self` in Web Workers
  - alternative to `global` in Node.js

## 2021 Highlights

- numeric separators for readability (underscore)

  ex. `1_234_567.89`

- `String` `replaceAll` method

  ```js
  s.replace(/foo/g, 'bar')
  can be written as
  s.replaceAll('foo', 'bar')
  ```

- `Promise.any`

  - takes an iterable of promises
  - resolves with the first one resolves
  - if all reject, rejects with an `AggregateError` exception
    that holds an error from each promise in order
  - alternative to `Promise.race` that resolves or rejects
    based on the first promise to complete

- private class members

  - begin name of fields and methods with #
  - applies to both instance and static members

## 2022 Highlights

- expected to be finalized in June 2022

- can declare public fields of a class (instance or static)
  inside class, not in constructor

  ```js
  class Dog {
    static count = 0;
    breed = '';

    constructor(breed) {
      this.breed = breed;
      Dog.count++;
    }
  }
  ```

- `static` blocks

  - used to initialize static fields in a class
    when their values must be computed
  - syntax: `static { code }`
  - can have more than one in the same class

- top-level `await`

  can use `await` keyword outside of `async` functions

- error cause

  `Error` objects can have a `cause` property
  that refers to another `Error` object

- `at` method added to `Array` and `String`

  - takes an index which can be negative to count from the end
  - `myArr.at(0)` returns the first element
  - `myArr.at(-1)` returns the last element

- `RegExp` match indices

  Adding the `/d` flag to a regular expression causes it to
  record the start and end index of each capture group.
