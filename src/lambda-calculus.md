---
eleventyNavigation:
  key: Lambda Calculus
layout: topic-layout.njk
---

## Overview

<a href="https://en.wikipedia.org/wiki/Lambda_calculus"
target="_blank">Lambda calculus</a> (sometimes written as λ-calculus) was
defined by <a href="https://en.wikipedia.org/wiki/Alonzo_Church"
target="_blank">Alonzo Church</a> (1903-1985) in the 1930s.
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
Recall that λ-calculus does not define numbers or operators.
But for demonstration purposes, assume that it does define the `+` operator
which adds two numbers held in variables.

| λ-calculus  | JavaScript      |
| ----------- | --------------- |
| λx.λy.x + y | (x, y) => x + y |

A shorthand way of writing the λ-calculus function above is `λxy.x + y`.

This is referred to as "currying" which is a nod to the mathematician
<a href="https://en.wikipedia.org/wiki/Haskell_Curry"
target="_blank">Haskell Curry</a> (1900-1982) whose used the concept extensively.
However, the concept was initially defined by Gottlob Frege in 1893
before Haskell Curry was born.

## Some Rules

The names of function parameters are irrelevant.
For example, the function `λx.x` is equivalent to the function `λy.y`.

The result of a function application is determined by substituting
the argument value for all occurrences of the function parmameter.
For example, `(λx.x + 3) 2` evaluates to `2 + 3` which evaluates to `5`.
