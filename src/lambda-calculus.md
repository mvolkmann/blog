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

The word "calculus" has four meanings, the last of which applies here.

1. In mathematics it is a method of calculation
   known as differential or integral calculus.
1. In pathology it is a stone or concretion
   often formed in the gallbladder or kidneys.
1. In dentistry it is a hard, yellowish or brownish
   deposit on teeth, also known as tartar.
1. A method of calculating, judging, or deciding something
   in a complicated situation.

Despite lacking many features found in programming languages,
λ-calculus is Turing complete.

Throughout this post examples are shown in both
λ-calculus and the closest equivalent in JavaScript.
TODO: Show more JavaScript examples.

## History

<a href="https://en.wikipedia.org/wiki/Gottlob_Frege"
target="_blank">Gottlob Frege</a> (1848-1925)
studied the use of functions in logic in 1893.

<a href="https://en.wikipedia.org/wiki/Moses_Schönfinkel"
target="_blank">Moses Schönfinkel</a> (1888-1942)
studied how combinators can be applied to formal logic in the 1920s.
The term "combinator" has two meanings, both of which describe a kind of function.
The first describes functions that have no free variables.
It combines only its arguments to produce a result.
The second describes functions that take other functions
and combine them to create a new function.

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
  where `<parameter>` is a single variable
  that is also referred to as a meta-variable.

  The body of a function extends as far right as possible.
  So `λx.a b c x` is evaluated as `λx.(((a b) c) x)`
  which is different from `((λx.a b) c) x`.
  The expression `λx.λy.λz.a b c` is evaluated as `λx.(λy.(λz.a b c))`.

  Expressions like `λx.λy.λz.a b c` are sometimes
  written in the shorter form `λxyz.a b c`
  despite the fact that λ-calculus functions only have a single parameter.

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
Free variables appear in function definitions, are not parameters,
and can represent any value.
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

## Evaluation Rules

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

  This changes the names of bound variables (which match function parameters),
  resulting in equivalent functions.
  For example, the function `λx.x` is equivalent to the function `λy.y`
  and `(λfx.f (+ x 1))` is equivalent to `(λgy.g (+ y 1))`.

- η-conversion (eta)

  This replaces a function that has an explicit parameter
  with one that takes an implicit parameter,
  creating a point-free version of the function.
  For example, `λx.(+ x 1)` is equivalent to `(+ x)`
  because `+` is a a function that takes two arguments,
  but only one is supplied.

## Boolean Logic

### True and False

The Boolean value true is represented by the function `(λt. λf. t)`.
This takes two arguments and returns the first.

The Boolean value false is represented by the function `(λt. λf. f)`.
This takes two arguments and returns the second.
This is the same as the function that represents the number zero.

### Not Function

A function to return "not" of a Boolean value is `λb. b false true`.
where `b` is either the true or false function.
For example,
`(λb. b false true) (λt. λf. t)` evaluates to false.
`(λb. b false true) (λt. λf. f)` evaluates to true.

### And Function

A function to return the result of and'ing two Boolean values is
`λx. (λy. x y false)`.
If the first argument is false, that is the result.
Otherwise the second argument is the result.

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

### Or Function

A function to return the result of or'ing two Boolean values is
`λx. (λy. x true y)`.
If the first argument is true, that is the result.
Otherwise the second argument is the result.

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

### If Expressions

An if expression can be implemented as follows where
`c` is the condition to be tested,
`x` is the result if `c` evaluates to true, and
`y` is the result if `c` evaluates to false.

```text
λcxy.c x y
```

This works because the function that represents true returns its first argument
and the function that represents false returns its second argument.

## Arithmetic

### Church Numerals

While λ-calculus does not define numbers, we can select
λ terms to represent each natural number (non-negative integers).
We can define functions that take another function `f` and a number `x`
whose result is that of `f` being applied `x` times.
It is not the result of these functions that represent numbers,
but rather the functions themselves.

In the table below, note the number of times the function `f` is applied
on the right side of the period.

| Number | λ term            |
| ------ | ----------------- |
| 0      | `λfx.x`           |
| 1      | `λfx.f x`         |
| 2      | `λfx.f (f x)`     |
| 3      | `λfx.f (f (f x))` |

The successor function (succ) `λn (λf. λx. f (n f x))`
returns the number that follows a given number.
For example, the successor of 1 is 2.

```text
(λn (λf. λx. f (n f x))) 1
(λn (λf. λx. f (n f x))) (λfx.f x)
λf. λx. f ((λfx.f x) f x)
λf. λx. f (f x)
λfx.f (f x)
2
```

The predecessor function (pred) `λn (λf. λx. n (λg.λh. h (g f)) (λu.x) (λu.u))`
returns the number that precedes a given number.
For example, the predecessor of 2 is 1.

TODO: Finish demonstrating that this works. It works in your JS code!

```text
(λn (λf. λx. n (λg.λh. h (g f)) (λu.x) (λu.u))) 2
(λf. λx. 2 (λg.λh. h (g f))) (λu.x) (λu.u)
(λf. λx. 2 (λg.λh. h (g f))) (λu.x) (λu.u)
(λx. 2 (λg.λh. h (g (λu.x)))) (λu.x)
2 (λg.λh. h (g (λu.x)))) (λu.x)
1
```

### Is Zero

The following function tests whether its argument represents zero.

```text
ISZERO = λn.n (λx.FALSE) TRUE
```

Let's verify that this works for 0 and 1.

```text
(λn.n (λx.FALSE) TRUE) λfx.x
λfx.x (λx.FALSE) TRUE -- f is not used in the body
(λx.x) TRUE
TRUE

(λn.n (λx.FALSE) TRUE) (λfx.f x)
λfx.f x (λx.FALSE) TRUE -- f is used in the body
(λx.FALSE) TRUE
FALSE
```

### Addition

Addition can be seen as iterated succession.
An add function can be written as `λmn. (m succ) n`.
For example, here are the steps to use this function to add 2 and 3.
See the representations for 2 and 3 in the table above.

```text
(λmn. (m succ) n) 2 3
(2 succ) 3
((λfx.f (f x)) succ) 3
(λx.succ (succ x)) 3
succ (succ 3)
succ (succ (λfx.f (f (f x))))
succ (λfx.f (f (f (f x))))
λfx.f (f (f (f (f x))))
5
```

### Subtraction

Substraction can be seen as iterated predecessors.
A subtraction function can be written as `λmn. (n pred) m`.
This returns `zero` if `m` is less than `n`
because we don't have a way to represent negative numbers.

### Multiplication

Multiplication can be seen as iterated addition.
A multiply function (mul) can be written as `λmn. m (add n) 0`
where `m` and `n` are the two numbers to be multiplied.
For example, here are the steps to use this function to multiply 2 and 3.
See the representations for 2 and 3 in the table above.

```text
(λmn. m (add n) 0) 2 3
2 (add 3) 0
(λfx.f (f x)) (add 3) 0
(add 3) ((add 3) 0)
add 3 3
-- We can assume the add function works and skip to the result of 6.
(λmn. (m succ) n) 3 3
(3 succ) 3
(λfx.f (f (f x))) succ 3
succ (succ (succ 3))
succ (succ 4)
succ 5
6 which is represented by λfx.f (f (f (f (f (f x)))))
```

### Division

TODO: Add this.

### Exponentiation

Exponentiation can be seen as iterated multiplicaation.
An exponentiation function (exp) can be written as `λmn. n (mul m) 1`
where `m` is the base and `n` is the exponent.
For example, here are the steps to raise 2 to the 3rd power.
See the representations for 2 and 3 in the table above.

```text
(λmn. n (mul m) 1) 2 3
3 (mul 2) 1
(λfx.f (f (f x))) (mul 2) 1
-- We can assume the mull function works.
(mul 2) ((mul 2) ((mul 2) 1))
(mul 2) ((mul 2) 2)
(mul 2) 4
8
```

Does this defintion also work? `λmn. n m`

```text
(λmn. n m) 2 3
3 2
(λfx.f (f (f x))) 2
λx.2 (2 (2 x))
2 = λfx.f (f x), so (2 x) = (λfy.f (f y)) x = λy.x (x y)
TODO: What can be done from here to arrive at 8?
```

## Equality

A function to determine if two Boolean values are equal can be writen as

```text
λab. (or (and a b) (and (not a) (not b)))
```

A function to determine if two numbers are equal can be writen as

```text
λmn.and (iszero (sub m n)) (iszero (sub n m))
```

We have to test both because our `sub` function returns zero
when the first number is less than the last number.

## Function Composition

A function to compose two functions can be written as λfgx.f (g x).

## Recursion

Functions in lambda calculus do not have names.
This leaves no way for a function to refer to itself
which makes implementing recursion difficult.

The Y Combinator, invented by Haskell Curry, is a function that
implements recursion and provides a way of implementing loops.
It is defined as `λf.(λx.f (x x)) (λx.f (x x))`.
Note that the body contains two identical terms.

Consider the expression `(λx.x x) (λx.x x)`.
Substituting the second term for x in the first term
yields the exact same expression.
This repeats forever, creating an infinite loop.

The Y Combination adds a function parameter `f` to this
which represents the computation to be performed in each iteration.

We can attempt to write the factorial function as

```text
fact = λn.if (iszero n) 1 (mult n (fact (pred n)))
```

But this assumes that functions have names that can be used to call themselves,
but they do not.

For example, the factorial function can be defined as
TODO: Finish this.

The Z Combinator is similar to the Y Combinator,
but it defers calculations until they are needed.
It is defined as

```text
λf.M (λx.f (λv.M x v)) -- where M is the Mockingbird function
```

TODO: Try to implement this in JavaScript.
