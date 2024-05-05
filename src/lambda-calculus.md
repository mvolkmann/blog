---
eleventyNavigation:
  key: Lambda Calculus
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

## Overview

Lambda calculus (sometimes written as λ-calculus)
was defined by Alonzo Church in the 1930s.
It describes concepts that are fundamental to functional programming.
It's purpose is to study how functions can interact with each other
not to calculate results in a useful way.

Despite lacking many features found in programming languages,
λ-calculus is Turing complete.
It only defines three concepts, referred to as lambda terms:

- variable: gives a name to a value
- lambda extraction: defines an anonymous function
- application: calls a function

It does not define values such as booleans, numbers, or strings.
It also does not define operators on these types or any built-in functions.
However, these can be defined using only the concepts listed above.

There are two kinds of variables, bound and free.
Bound variables appear as function parameters and represent in input value.
Free variables appear in function definitions and can represent any value.

Throughout this post examples are shown in both
λ-calculus and the closest equivalent in JavaScript.

## Function Syntax

Here is an identity function that just returns the value passed in.

| λ-calculus | JavaScript |
| ---------- | ---------- |
| λx.x       | x => x     |

Function defintions begin with the λ character,
followed by a single parameter name, a period, and
the lambsda term to which the function evaluates.

Here is an example of calling the identity function
which results in the value `y`.
Parentheses are used to surround function defintions
and enable them to be applied.
They are also used to define the order of operations.

| λ-calculus | JavaScript  |
| ---------- | ----------- |
| (λx.x) y   | (x => x)(y) |

Functions in λ-calculus can only have a single parameter.
To support the concept of multi-parameter functions,
a function can be passed to another function.
This is commonly used in programming languages for callback functions.

To demonstrate this we will write a function that adds two numbers.
Recall that we said λ-calculus does not define numbers or operators.
But for demonstration purposes, assume that it does define the `+` operator
which adds two numbers.

| λ-calculus  | JavaScript      |
| ----------- | --------------- |
| λx.λy.x + y | (x, y) => x + y |

A shorthand way of writing the λ-calculus function above is `λxy.x + y`.
