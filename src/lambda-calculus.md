---
eleventyNavigation:
  key: Lambda Calculus
layout: topic-layout.njk
---

## Overview

<a href="https://en.wikipedia.org/wiki/Lambda_calculus"
target="_blank">Lambda calculus</a> (sometimes written as λ-calculus)
describes concepts that are fundamental to functional programming.
It's purpose is to study how functions can interact with each other,
not to calculate results in a useful way.

Despite lacking many features found in programming languages,
λ-calculus is Turing complete.

Throughout this post examples are shown in both
λ-calculus and the closest equivalent in JavaScript.

## History

<a href="https://en.wikipedia.org/wiki/Gottlob_Frege"
target="_blank">Gottlob Frege</a> (1848-1925)
studied the use of functions in logic in 1893.

<a href="https://en.wikipedia.org/wiki/Moses_Schönfinkel"
target="_blank">Moses Schönfinkel</a> (1888-1942)
studied how combinators can be applied to formal logic in the 1920s.
The term "combinator" has two meanings, both which describe a kind of function.
The first describes functions that have no free variables.
The second describes functions that take other functions
and combine them to create new functions.

<a href="https://en.wikipedia.org/wiki/Alonzo_Church"
target="_blank">Alonzo Church</a> (1903-1985)
invented Lambda Calculus in the 1930s.
He was the PhD advisor of <a href="https://en.wikipedia.org/wiki/Alan_Turing"
target="_blank">Alan Turing</a> (1912-1954).

## Concepts

λ-calculus only defines three concepts, referred to as lambda terms:

- variable: gives a single-letter name to a value

- lambda abstraction: defines an anonymous function that has exactly one parameter

  This uses the syntax `λ<parameter>.<body>`
  where `<parameter>` is a single variable.

  The body of a function extends as far right as possible.
  So `λx.a b c x` is evaluated as `λx.(((a b) c) x)`
  which is different from `((λx.a b) c) x`.
  The expression `λx.λy.λz.a b c` is evaluated as `λx.(λy.(λz.a b c))`.

- application: calls a function with arguments

  This uses the syntax `(λ<parameter>.<expression>) <arguments>`
  where `<arguments>` is a whitespace-separated list of expressions.
  Function application is left associative.
  So `a b c d` is evaluated as `((a b) c) d`.

λ-calculus does not define a syntax for values
such as booleans, numbers, and strings.
It also does not define operators on these types or any built-in functions.
However, these can be defined using only the concepts listed above.

There are two kinds of variables, bound and free.
Bound variables are bound by a specific abstraction (function).
They appear as function parameters and represent an input value.
Free variables appear in function definitions and can represent any value.
The following tables contains examples.

| Expression        | Bound Variables               | Free Variables |
| ----------------- | ----------------------------- | -------------- |
| `λx.(+ x 1)`      | `x`                           | none           |
| `λx.(+ y 1)`      | none                          | `y`            |
| `λx.x λx.(+ x 1)` | rename 2nd `x` as shown below |                |
| `λx.x λy.(+ y 1)` | `x` and `y`                   | none           |

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
  The result of a function application is determined by substituting
  the argument value for all occurrences of the function parmameter.
  For example, `(λx.x + 3) 2` evaluates to `2 + 3` which evaluates to `5`.
  Consider the function `(λfx.f (+ x 1))` which takes two arguments,
  a function and a number.
  We can apply two arguments with `(λfx.f (+ x 1)) (λx.(* x 2)) 3`.
  The result is `(λx.(* x 2)) (+ 3 1)`.
  The second term becomes just `4` using the δ-rule.
  We can apply a β-reduction again to obtain `(* 4 2)`.
  One more use of the δ-rule, gives `8`.

- 𝛼-conversion (alpha)

  This changes the names of a function parameters,
  resulting in equivalent functions.
  For example, the function `λx.x` is equivalent to the function `λy.y`
  and `(λfx.f (+ x 1))` is equivalent to `(λgy.g (+ y 1))`.

- η-conversion (eta)

  This replaces a function that has an explicit parameter
  with one that takes an implicit parameter.
  For example, `λx.(+ x 1)` is equivalent to `(+ x)`
  because `+` is a a function that takes two arguments,
  but only one is supplied.

## Church Numberals

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

The Boolean value true is represented by the function `(λt. λf. t)`.
This takes two arguments and returns the first.

The Boolean value false is represented by the function `(λt. λf. f)`.
This takes two arguments and returns the second.
This is the same as the function that represents the number zero.

A function to return "not" of a Boolean value is `λb. b false true`.
where `b` is either the true or false function.
For example,
`(λb. b false true) (λt. λf. t)` evaluates to false.
`(λb. b false true) (λt. λf. f)` evaluates to true.

A function to return the result of and'ing two Boolean values is
`λx. (λy. x y false)`.

```text
(λx. (λy. x y false)) true true
true true false
(λt. λf. t) true false
true

(λx. (λy. x y false)) true false
true false false
(λt. λf. t) false false
false

(λx. (λy. x y false)) false true
false true false
(λt. λf. f) true false
false

(λx. (λy. x y false)) false false
false false false
(λt. λf. f) false false
false
```

A function to return the result of or'ing two Boolean values is
`λx. (λy. x true y)`.

```text
(λx. (λy. x true y)) true true
true true true
(λt. λf. t) true true
true

(λx. (λy. x true y)) true false
true true false
(λt. λf. t) true false
true

(λx. (λy. x true y)) false true
false true true
(λt. λf. f) true true
true

(λx. (λy. x true y)) false false
false true false
(λt. λf. f) true false
false
```

## Recursion

TODO: Cover the y-combinator function.
