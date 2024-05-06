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

  This uses the syntax `λ<parameter>.<expression>`
  where `<parameter>` is a single variable.

- application: calls a function with arguments

  This uses the syntax `(λ<parameter>.<expression>) <arguments>`
  where `<arguments>` is a whitespace-separated list of expressions.

λ-calculus does not define a syntax for values
such as booleans, numbers, and strings.
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
But for demonstration purposes, we can pretend that it does define
numbers, basic math operators such as `+`,
and the Boolean values `true` and `false`.

| λ-calculus     | JavaScript      |
| -------------- | --------------- |
| λx. (λy.x + y) | (x, y) => x + y |

Shorthand ways of writing the λ-calculus function above are
`λx.λy.x + y` (parentheses not necessary) and `λxy.x + y`.
This is also sometimes written with a prefix operator as `λxy.(+ x y)`.

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

TODO: Cover all of these topics:

- Bound vs. Free Variables

  Variables that appear as a function parameter are "bound variables"
  because they are bound to a specific value
  when the function is applied to an argument.
  Variables in function expressions that are not parameters are free variables.

- δ-rule (delta)

  This is used to evaluate functions that are assumed to be built-in.
  For example, `(+ 1 2)` can be evaluated to `3`.

- β-reduction (beta)

  This is used to apply arguments to a function.
  For example, consider the function `(λfx.f (+ x 1))`
  which takes two arguments, a function and a number.
  We can apply two arguments with `(λfx.f (+ x 1)) (λx.(* x 2)) 3`.
  The result is `(λx.(* x 2)) (+ 3 1)`.
  The second term becomes just `4` using the δ-rule.
  We can apply a β-reduction again to obtain `(* 4 2)`.
  One more use of the δ-rule, gives `8`.

- 𝛼-conversion (alpha)
- η-conversion (eta)

## Church Encoding of Numbers

While λ-calculus does not define numbers,
we can select λ terms to represent each natural number.
We can define functions that take another function `f` and a number `x`
whose result is that of `f` being applied `x` times.
Is is not the result of these functions that represent numbers,
but rather the functions themselves.

In the table below, note the number of times the function `f` is applied
on the right side of the period.

| Number | λ term            |
| ------ | ----------------- |
| 0      | `λfx.x`           |
| 1      | `λfx.f x`         |
| 2      | `λfx.f (f x)`     |
| 3      | `λfx.f (f (f x))` |

## Addition

Let's see how we can add 2 and 3 to get 5.
See the representations for 2 and 3 in the table above.

Apply the arguments `f` and `x` to the λ term for 3,
to get the result of the function application.

```text
(λfx.f (f (f x))) f x
f (f (f x))
```

Substitute the term above for `x` on the right side of the λ term for 2.

```text
(λfx.f (f x            )) -- term for 2
(λfx.f (f (f (f (f x))))) -- term for 5
```

An add function can be written as `λfxmn. (m f) (n f x)`
where `m` and `n` are the two numbers to be added.

For example, here are the steps to use this function to add 2 and 3.

```text
(λfxmn. (m f) (n f x)) 2 3
-- Substitute 2 for m and 3 for n.
λfx. (2           f) (3               f x)
-- Substitute the λ terms for 2 and 3.
λfx. (λfx.f (f x) f) (λfx.f (f (f x)) f x)
-- Apply the arguments f and x in the last term.
λfx. (λfx.f (f x) f) (f (f (f x)))
-- Simplify the first term by applying its argument f.
λfx. (λx.f (f x))    (f (f (f x)))
-- Apply the argument `(f (f (f x)))` to the function on its left.
λfx. f (f (f (f (f x)))) -- term for 5
```

## Multiplication

Let's see how we can multiply 2 and 3 to get 6.
See the representations for 2 and 3 in the table above.

The λ term for 3, `λfx.f (f (f x))`, is a function that has two parameters.
Apply `f` to this to get the single parameter function
`λx.f (f (f x))`.

The λ term for 2, `λfx.f (f x)`, is also a function that has two parameters.
Apply the previous result as the value of the `f` parameter in this.
Two substitutions are required, resulting in:

```text
λx.(λx.f (f (f x))) (λx.f (f (f x)) x)
```

Simplify the expression on the right to get:

```text
λx.f (f (f x)) (f (f (f x)))
```

Substitute the term on the right as value for `x` in the term on the left to get

```text
λx.f (f (f (f (f (f x))))) -- term for 6
```

A multiply function can be written as `λfxmn. m (n f) x`
where `m` and `n` are the two numbers to be multiplied.

For example, here are the steps to use this function to multiply 2 and 3.

```text
(λfxmn. m (n f) x) 2 3
-- Substitute 2 for m and 3 for n.
λfx. 2 (3 f) x
-- Apply the argument x.
λf. 2 (3 f)
-- Substitute the λ terms for 2 and 3.
λf. (λfx.f (f x)) ((λfx.f (f (f x))) f)
- Apply the argument f in the last term to get a function that only takes x.
  It seems either argument can be supplied, not just the last.
λf. (λfx.f (f x)) (λx.f (f (f x)))
- Apply the last term as the value of the parameter f in the first term
  to get a function that only takes x.
λf. λx.(λx.f (f (f x))) ((λx.f (f (f x))) x)
-- Simplify the last term by applying the argument x.
λf. λx.(λx.f (f (f x))) (f (f (f x)))
-- Apply the last term as the value of the parameter x in the first term.
λf. (λx.f (f (f (f (f (f x))))))
-- Combine these single argument functions into a two-argument function.
λfx.f (f (f (f (f (f x))))) -- term for 6
```

## Exponentiation

Let's see how we can compute 2 to the power 3.
See the representations for 2 and 3 in the table above.

Pass the term for 2 into the function represented by the term for 3.

```text
λfx.f (f x) -- term for 2
λfx.f (f (f x)) -- term for 3
λfx.f         (f         (f x)) -- whitespace added
-- Substitute the term for 2 in place of
-- all occurrences of f in the term for 3.
λfx.(f (f x)) ((f (f x)) ((f (f x)) x))
-- Apply ?
-- Need to get 8!
```

An exponentiation function can be written as `λmn. n m`
where `m` is the base and `n` is the exponent.

For example, here are the steps to raise 2 to the 3rd power.

```text
λmn. n m
-- Substitute 2 for m and 3 for n.
2 3
-- Substitute the λ terms for 2 and 3.
(λfx.f (f x)) (λfx.f (f (f x)))
-- Simplify the right term by applying the arguments f and x.
(λfx.f (f x)) (f (f (f x)))
-- Add whitespace.
(λfx.f            (f             x)) (f (f (f x)))
-- Apply the right term as the parameter x in the first term function.
(λf.(f (f (f x))) ((f (f (f x))) x))

-- TODO: How can this ever evaluate to 8?
```

## Boolean Logic

## Recursion

TODO: Cover the y-combinator function.
