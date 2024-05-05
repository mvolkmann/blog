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

- variable: gives a single-letter name to a value
- lambda extraction: defines an anonymous function
- application: calls a function with arguments

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

| λ-calculus     | JavaScript      |
| -------------- | --------------- |
| λx. (λy.x + y) | (x, y) => x + y |

Shorthand ways of writing the λ-calculus function above are
`λx.λy.x + y` (parentheses not necessary) and `λxy.x + y`.

This is referred to as "currying" which is a nod to the mathematician
<a href="https://en.wikipedia.org/wiki/Haskell_Curry"
target="_blank">Haskell Curry</a> (1900-1982)
who used the concept extensively in his research.
However, the concept was initially defined by Gottlob Frege in 1893
before Haskell Curry was born.

The JavaScript function above could be defined and called
as follows to support currying.
This uses a function that returns another function.

```js
function add(x) {
  return y => x + y;
}
const sum = add(2)(3); /* 5 */
```

In many functional programming languages including Haskell and OCaml,
all functions automatically support currying.
Passing fewer arguments to a function than it has parameters
results in a new function that expects the remaining parameters.

## Some Rules

The names of function parameters are irrelevant.
For example, the function `λx.x` is equivalent to the function `λy.y`.

The result of a function application is determined by substituting
the argument value for all occurrences of the function parmameter.
For example, `(λx.x + 3) 2` evaluates to `2 + 3` which evaluates to `5`.

## Church Encoding of Numbers

While λ-calculus does not define numbers,
we can select λ terms to represent each natural number.
We can define functions that take another function `f` and a number `x`
whose result is that of `f` being applied `x` times.
Is is not the result of these functions that represent numbers,
but rather the functions themselves.

In the table below, note the number of times the function `f` is applied
on the right side of the period.

| Number | λ term         |
| ------ | -------------- |
| 0      | `λfx.x`        |
| 1      | `λfx.fx`       |
| 2      | `λfx.f(fx)`    |
| 3      | `λfx.f(f(fx))` |

## Addition

Let's see how we can add 2 and 3 to get 5.
See the representations for 2 and 3 in the table above.

Pass the arguments `f` and `x` into the λ term for 3
and perform substitution to get the result of the function application.

```text
λfx.f(f(fx)) f x
f(f(fx))
```

Now pass `f` and the previous result into the λ term for 3
and perform substitution to get the result of the function application.
The λ term for 3 is substituted for every occurrence of `x` in the λ term for 2.

```text
λfx.f(fx) f f(f(fx))
f(f(f(f(fx))))
```

The full definiton of the add function
can be written as `λfxmn. (m f) (n f x)`
where `m` and `n` are the two numbers to be added.

For example, here are the steps to use this function to add 2 and 3.

```text
λfxmn. (m f) (n f x) 2 3
-- Substitute 2 for m and 3 for n.
λfx. (2 f) (3 f x)
-- Substiture the λ terms for 2 and 3.
λfx. (λfx.f(fx) f) (λfx.f(f(fx)) f x)
-- Apply the arguments f and x in the last term.
λfx. (λfx.f(fx) f) f(f(fx))
-- Substitute the last term for the argument x in the first term function.
λfx. f(f(f(f(fx))))
-- This is the definition of the number 5.
```

## Multiplication

Let's see how we can multiply 2 and 3 to get 6.
See the representations for 2 and 3 in the table above.

The λ term for 3 is a function that has two parameters.
Pass only `f` into this to get the single parameter function `λx.f(f(f(x)))`.
Pass this result and `x` into the λ term for 2 which results in
`λx.(λx.f(f(f(x)))) (λx.f(f(f(x))) x)`.
Simplify the expression on the right to get
`λx.f(f(f(x))) f(f(f(x)))`.
Substitute the expression on the right as the
value for `x` in the function on the left to get
`f(f(f(f(f(f(x))))))`.
This is the λ term for 6 which is the expected result.

The full definiton of the multiply function
can be written as `λfxmn. m (n f) x`
where `m` and `n` are the two numbers to be multiplied.

For example, here are the steps to use this function to multiply 2 and 3.

```text
(λfxmn. m (n f) x) 2 3
-- Substitute 2 for m and 3 for n.
λfx. 2 (3 f) x
-- Apply the argument x.
λf. 2 (3 f)
-- Substiture the λ terms for 2 and 3.
λf. λfx.f(fx) (λfx.f(f(fx)) f)
-- Apply the last term as the argument x in the first term.
λf. λf.f(f(λfx.f(f(fx)) f))

λf. f(f(λfx.f(f(fx))))

-- This is the definition of the number 6.
```

## Exponentiation

## Boolean Logic
