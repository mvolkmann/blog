---
eleventyNavigation:
  key: TypeScript Effect
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="TypeScript Effect logo" style="border: 0"
    src="/blog/assets/ts-effect-logo.jpg?v={{pkg.version}}">
</figure>

## Overview

<a href="https://effect.website/" target="_blank">Effect</a>
is a TypeScript library with a large set of features that:

- maximizes type safety
- improves error handling
- makes code more composable, reusable, and testable

## Recent Changes

Several significant changes where made to the Effect library in early 2024
which make most tutorial videos somewhat wrong.

- The order of the type parameters for the generic type `Effect`
  changed from `<Requirements, ErrorType, SuccessType>`
  to `<SuccessType, ErrorType, Requirements>`.
- The schema types had changes.
- The operation of the `pipe` function changed.
- The generator no longer requires wrapper functions.

## Effects

An `Effect` is an immutable value that
describes an operation (a.k.a a program) to be run later.
It is a type that is generic over three values:

1. A success type (`void` if there is no result)
2. An error type (`never` if no errors can occur)
3. An optional set of requirements that
   must be fulfilled in order for the code to run
   (`never` if there are none, `Error` type is the most general).

The result of an `Effect` is only computed when it is run.

To create an `Effect` from a value,
use the `Effect.succeed(value)` or `Effect.fail(error-object)` functions.

There are four ways to create an `Effect`.

If the function to be run never throws ...

- If the function is synchronous, use `Effect.sync(fn)`.
- If the function is asynchronous and never throws, use `Effect.promise(fn)`.

In both cases, `fn` is the function to be run.

If the function can throw ...

- If the function is synchronous, use `Effect.try(obj)`.
- If the function is asynchronous, use `Effect.tryPromise(obj)`.

In both cases, `obj` is an object with `try` and `catch` properties.
The `try` property value is a function that can throw.
A `Promise` is returned.

When an `Effect` is run, it produces a value or an error.
It is similar to the `Result` type in languages like OCaml.

To run an `Effect`:

- If it never throws, use `effect.runSync`.
- If it can throw, use `Effect.runPromise`.

## Pipelines

Pipelines pass values through a series of functions.
Each function take one value from the previous function, and
produces one result that is passed to the next function in the pipeline.

A pipeline is defined by calling the `Effect.pipe` function
or the `effect.pipe` method.
The `Effect.pipe` function takes an initial value and a series of functions,
and passes the initial value to the first function.
The `effect.pipe` method takes a series of functions,
and passes the value of the `Effect` on which it is called
to the first function.

The value returned by a function can be transformed
with the `Effect.map` and `Effect.flatMap` functions.

Side effects can be triggered wtih the `Effect.tap` function.

The values from multiple effects can be combined with the `Effect.all` function.

## Error Handling

TODO: Discuss creating objects with a `_tag` property
that is used to identify an error type.

To handle all kinds of errors, use the `Effect.catchAll` function.

To handle a specific kind of error, use the `Effect.catchTag` function.

To handle multiple specific kinds of errors, use the `Effect.catchTags` function.

TODO: Is there also an `Effect.catch` function?

TODO: What does `Effect.provideService` do?

## Logging

The Effect library provides a set of functions for logging.

- `Effect.logDebug`
- `Effect.logInfo`
- `Effect.logWarning`
- `Effect.logError`
- `Effect.logFatal`

By default, output from all of these is disabled.
To specify a logging level where all logging at that level and above is output,
call the `Effect.withMinimumLogLevel(level)` function.

To include timing information in log output,
call the `Effect.withLogSpan` function.

## Generators

Generators can simplify Effect code in a way that is
similar to using the JavaScript `async` and `await` keywords.
This utilizes the JavaScript `yield*` keyword.
