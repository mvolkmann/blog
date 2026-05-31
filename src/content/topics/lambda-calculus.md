---
navigation:
  key: Lambda Calculus
layout: topic-layout.njk
---

## Overview

<a href="https://en.wikipedia.org/wiki/Lambda_calculus"
target="_blank">Lambda calculus</a> (sometimes written as λ-calculus)
describes concepts that are fundamental to functional programming
such as first-class functions and currying.
A first-class function can take other functions as arguments
and can return a function.
Currying implements a function with multiple parameters
as a sequence of functions that each have a single parameter.

The purpose of λ-calculus is to study
how functions can interact with each other,
not to calculate results in a useful way.

Learning λ-calculus can be compared to learning about slide rules.
Calculators are an excellent replacement for slide rules,
and it's not necessary to understand how to use a slide rule
in order to use a calculator.
But it's fascinating to learn how slide rules work (logarithms).
Likewise, it's not necessary to understand λ-calculus
in order to be productive in modern programming languages.
But it's fascinating to learn how much can be accomplished
within the constraints of λ-calculus.

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
JavaScript implementations of all the functions described here
can be found in this
<a href="https://github.com/mvolkmann/lambda-calculus/blob/main/lambda-calculus.test.ts"
target="_blank">GitHub repository</a>.

## Resources

- <a href="https://en.wikipedia.org/wiki/Lambda_calculus"
    target="_blank">Lambda calculus</a> page on Wikipedia
- <a href="https://learnxinyminutes.com/docs/lambda-calculus/"
  target="_blank">Learn X in Y minutes Where X=Lambda Calculus</a>
- <a href="https://www.lesswrong.com/posts/D4PYwNtYNwsgoixGa/intro-to-hacking-with-the-lambda-calculus"
  target="_blank">Intro to hacking with the lambda calculus</a>
  blog post by L Rudolf L
- <a href="https://www.youtube.com/watch?v=3VQ382QG-y4"
  target="_blank">Fundamentals of Lambda Calculus & Functional Programming in JavaScript</a>
  YouTube talk by Gabriel Lebec

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

- variable: placeholder for a term represented by a single-letter name

  There are two kinds of variables, bound and free, that are discussed later.

- lambda abstraction: defines an anonymous function that has exactly one parameter

  | λ-calculus            | JavaScript              |
  | --------------------- | ----------------------- |
  | `λ<parameter>.<body>` | `<parameter> => <body>` |

  `<parameter>` is a single variable that is
  also referred to as a meta-variable.

  The body of a function extends as far right as possible.  
  So `λx.a b c x` is evaluated as `λx.(((a b) c) x)`  
  which is different from `((λx.a b) c) x`.  
  The expression `λx.λy.λz.a b c` is evaluated as `λx.(λy.(λz.a b c))`.

  Expressions like `λx.λy.λz.a b c` are sometimes
  written in the shorthand form `λxyz.a b c` despite the fact that
  λ-calculus functions only have a single parameter.

- application: calls a function with arguments

  | λ-calculus                          | JavaScript                             |
  | ----------------------------------- | -------------------------------------- |
  | `(λ<parameter>.<body>) <arguments>` | `(<parameter> => <body>)(<arguments>)` |
  | `(λxyz.<body>) a b c`               | `(x => y => z => <body>)(a)(b)(c)`     |

  `<arguments>` is a whitespace-separated list of expressions.
  Function application is left associative.
  So `a b c d` is evaluated as `((a b) c) d`.

λ-calculus does not define a syntax for values
such as booleans, numbers, and strings.
It also does not define operators on these types or any built-in functions.
However, these can be defined using only the concepts listed above.

## Variables

There are two kinds of variables, bound and free.
Bound variables are bound by a specific abstraction (function).
They appear as function parameters and represent an input value.
Free variables appear in function definitions, are not parameters,
and can represent any value.
The following tables contains examples.

| λ-calculus        | JavaScript             | Bound Variables               | Free Variables |
| ----------------- | ---------------------- | ----------------------------- | -------------- |
| `λx.(+ x 1)`      | `x => x + 1`           | `x`                           | none           |
| `λx.(+ y 1)`      | `x => y + 1`           | none                          | `y`            |
| `λx.x λx.(+ x 1)` | `x => x((x => x + 1))` | rename 2nd `x` as shown below |                |
| `λx.x λy.(+ y 1)` | `x => x((y => y + 1))` | `x` and `y`                   | none           |

The last two examples above assume that `x` is a function
that does not necessarily correspond to a number.

## Function Syntax

An identity function that just returns the value passed in can be defined as:

| λ-calculus | JavaScript |
| ---------- | ---------- |
| `λx.x`     | `x => x`   |

Function definitions begin with the λ character,
followed by a single parameter name, a period, and
the lambda term to which the function evaluates.

Here is an example of calling the identity function
which results in the value `y`.
Parentheses are used to surround function definitions
and enable them to be applied.
They are also used to define the order of operations.

| λ-calculus | JavaScript    |
| ---------- | ------------- |
| `(λx.x) y` | `(x => x)(y)` |

Functions in λ-calculus can only have a single parameter.
To support the concept of multi-parameter functions,
a function can be passed to another function.
This is commonly used in programming languages for callback functions.

To demonstrate this we will write a function that adds two numbers.
Recall that λ-calculus does not define numbers or operators.
But for demonstration purposes, we can pretend that it does define
numbers, basic math operators such as `+`,
and the Boolean values `true` and `false`.

| λ-calculus     | JavaScript        |
| -------------- | ----------------- |
| `λx. λy.x + y` | `(x, y) => x + y` |

A shorthand way of writing the λ-calculus function above is `λxy.x + y`.
This is also sometimes written with a prefix operator as `λxy.(+ x y)`.

Chaining functions that each take a single argument
is referred to as "currying".
This is a nod to the mathematician
<a href="https://en.wikipedia.org/wiki/Haskell_Curry"
target="_blank">Haskell Curry</a> (1900-1982)
who used the concept extensively in his research.
However, the concept was initially defined by Gottlob Frege in 1893
before Haskell Curry was born.

The λ-calculus function above can be defined in JavaScript to support currying.
This uses a function that returns another function.

```js
function add(x) {
  return y => x + y;
}
// OR more compactly as:
const add = x => y => x + y;
// It can be called as follows:
const sum = add(2)(3); /* 5 */
```

In many functional programming languages including Haskell and OCaml,
all functions automatically support currying.
Passing fewer arguments to a function than it has parameters
results in a new function that expects the remaining parameters.

## Evaluation Rules

- 𝛼-conversion (alpha)

  This changes the names of bound variables (which match function parameters),
  resulting in equivalent functions.
  For example, the function `λx.x` is equivalent to the function `λy.y`
  and `λfx.f (+ x 1)` is equivalent to `λgy.g (+ y 1)`.

- β-reduction (beta)

  This is used to apply arguments to a function.
  The result of a function application is determined by substituting
  the argument values for all occurrences of the function parameter.
  For example, `(λx.x + 3) 2` evaluates to `2 + 3` which evaluates to `5`.
  Consider the function `(λfx.f (+ x 1))` which takes two arguments,
  a function and a number.
  We can apply two arguments with `(λfx.f (+ x 1)) (λx.(* x 2)) 3`.
  The result is `(λx.(* x 2)) (+ 3 1)`.
  The second term becomes just `4` using the δ-rule.
  We can apply a β-reduction again to obtain `(* 4 2)`.
  One more use of the δ-rule, gives `8`.

- δ-rule (delta)

  This is used to evaluate functions that are assumed to be built-in.
  For example, `(+ 1 2)` can be evaluated to `3`.

- η-conversion (eta)

  This replaces a function that has an explicit parameter
  with one that takes an implicit parameter,
  creating a point-free version of the function.
  For example, `λx.(+ x 1)` is equivalent to `(+ x)`
  because `+` is a a function that takes two arguments,
  but only one is supplied.

## Testing

When implementing λ-calculus functions in programming languages,
it is useful to be able to convert λ-calculus representations
to actual boolean values and numbers.

In JavaScript this can be accomplished with the following functions
where `b` is a λ-calculus function that represents true or false
and `n` is a λ-calculus function that represents a natural number:

```js
const jsbool = b => b(true)(false);
const jsnum = n => n(x => x + 1)(0);
```

## Boolean Logic

### True and False

The Boolean value true is represented by:

| λ-calculus  | JavaScript    |
| ----------- | ------------- |
| `λt. λf. t` | `t => f => t` |

This takes two arguments and returns the first.

In the examples that follow, the JavaScript function above is represented
by `true_` to avoid conflicting with the JavaScript keyword `true`.

The Boolean value false is represented by:

| λ-calculus  | JavaScript    |
| ----------- | ------------- |
| `λt. λf. f` | `t => f => f` |

This takes two arguments and returns the second.
It is the same as the function that represents the number zero (below).

In the examples that follow, the JavaScript function above is represented
by `false_` to avoid conflicting with the JavaScript keyword `false`.

### Not Function

A function to return "not" of a Boolean value,
where `b` is either the true or false function,
can be defined as:

| λ-calculus        | JavaScript              |
| ----------------- | ----------------------- |
| `λb.b false true` | `b => b(false_)(true_)` |

For example,
`(λb.b false true) (λt. λf. t)` evaluates to false.
`(λb.b false true) (λt. λf. f)` evaluates to true.

### And Function

A function to return the result of and'ing two Boolean values can be defined as:

| λ-calculus          | JavaScript               |
| ------------------- | ------------------------ |
| `λx. λy. x y false` | `x => y => x(y)(false_)` |

If the first argument is `false`, that is the result.
Otherwise the second argument is the result.
Replacing `false` with `x` results in an equivalent definition
because the that is only returned when `x` evaluates to `false`.

In a lazily evaluated programming language like Haskell,
the second argument is only evaluated if the first argument evaluates to true.
In a strictly evaluated programming language like JavaScript,
both arguments are always evaluated.

```text
(λx.  λy. x y false) true true
true true false
(λt. λf. t) true false
true

(λx. λy. x y false) true false
true false false
(λt. λf. t) false false
false

(λx. λy. x y false) false true
false true false
(λt. λf. f) true false
false

(λx. λy. x y false) false false
false false false
(λt. λf. f) false false
false
```

### Or Function

A function to return the result of or'ing two Boolean values can be defined as:

| λ-calculus         | JavaScript              |
| ------------------ | ----------------------- |
| `λx. λy. x true y` | `x => y => x(true_)(y)` |

If the first argument is `true`, that is the result.
Otherwise the second argument is the result.
Replacing `true` with `x` results in an equivalent definition
because the that is only returned when `x` evaluates to `true`.

In a lazily evaluated programming language like Haskell,
the second argument is only evaluated if the first argument evaluates to false.
In a strictly evaluated programming language like JavaScript,
both arguments are always evaluated.

```text
(λx. λy. x true y) true true
true true true
(λt. λf. t) true true
true

(λx. λy. x true y) true false
true true false
(λt. λf. t) true false
true

(λx. λy. x true y) false true
false true true
(λt. λf. f) true true
true

(λx. λy. x true y) false false
false true false
(λt. λf. f) true false
false
```

### If Expressions

A function that models an "if expression" can be defined as follows where
`c` is the condition to be tested,
`x` is the result if `c` evaluates to true, and
`y` is the result if `c` evaluates to false:

| λ-calculus         | JavaScript              |
| ------------------ | ----------------------- |
| `λx. λy. x true y` | `x => y => x(true_)(y)` |

This works because
the function that represents `true` returns its first argument and
the function that represents `false` returns its second argument.

In strictly-evaluated languages like JavaScript, this definition assumes
it is acceptable to evaluate both the `x` and `y` expressions.
This can be problematic when the evaluations are expensive
or when recursion is involved.
To solve this, the `if` function can be implemented as follows where
`b` is a Boolean value,
`t` is a function that can be called to get the true value, and
`f` is a function that can be called to get the false value.

```js
const if_ = b => t => f => b(t)(f)(); // λbtf.(b t f)(_)
```

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

| Number | λ term            | JavaScript             |
| ------ | ----------------- | ---------------------- |
| 0      | `λfx.x`           | `f => x => x`          |
| 1      | `λfx.f x`         | `f => x => f(x)`       |
| 2      | `λfx.f (f x)`     | `f => x => f(f(x))`    |
| 3      | `λfx.f (f (f x))` | `f => x => f(f(f(x)))` |

The function that represents zero is the same
as the function that represents false (above).

These functions that represent numbers take another function and a value.
They call the given function some number of times,
initially passing it the value and
subsequently passing it what the function returns.
For example, the following JavaScript outputs
the five numbers 3, 6, 12, 24, and 48.

```js
const demo = x => {
  console.log(x);
  return x + x;
};
five(demo)(3);
```

### Successor

The successor function (succ)
returns the number that follows a given number.
It can be defined as:

| λ-calculus               | JavaScript                  |
| ------------------------ | --------------------------- |
| `λn (λf. λx. f (n f x))` | `n => f => x => f(n(f)(x))` |

For example, the successor of 1 is 2.

```text
(λn (λf. λx. f (n f x))) 1
(λn (λf. λx. f (n f x))) (λfx.f x)
λf. λx. f ((λfx.f x) f x)
λf. λx. f (f x)
λfx.f (f x)
2
```

### Predecessor

The predecessor function (pred)
returns the number that precedes a given number.
It is one of the most complicated functions in λ-calculus.

Alonzo Church was unable to discover a definition for the predecessor function.
A student of his, Stephen Kleene (1909-1994), arrived at a solution
while in a dentist chair for wisdom teeth removal.
Kleene is most known for his research into recursion theory.

Suppose the goal is to find the predecessor of 3 which is 2.  
Start with the pair (0, 0).  
Apply a function to the pair three times whose result is a new pair
composed of the second number and the successor of the second number.
This yields (0, 1), (1, 2), and (2, 3).
Take the first number of the final pair.

A function to represent a pair can be defined as:

| λ-calculus       | JavaScript               |
| ---------------- | ------------------------ |
| `λx.λy.λf.f x y` | `x => y => f => f(x)(y)` |

A function to get the first element of a pair `p` can be defined as:

| λ-calculus  | JavaScript      |
| ----------- | --------------- |
| `λp.p TRUE` | `p => p(true_)` |

A function to get the second element of a pair `p` can be defined as:

| λ-calculus   | JavaScript       |
| ------------ | ---------------- |
| `λp.p FALSE` | `p => p(false_)` |

Let's use the name "phi" to refer to a function that
takes a pair and returns a new pair composed of
the second element and the successor of the second element.
This can be defined as:

| λ-calculus                       | JavaScript                        |
| -------------------------------- | --------------------------------- |
| `λp.pair (snd p) (succ (snd p))` | `p => pair(snd(p))(succ(snd(p)))` |

The term "`n phi`" represents n applications of the phi function.

We can now define the predecessor function as:

| λ-calculus                        | JavaScript                           |
| --------------------------------- | ------------------------------------ |
| `λn.fst (n phi (pair zero zero))` | `n => fst(n(phi)(pair(zero)(zero)))` |

The Wikipedia page for "Lambda Calculus" defines the predecessor function as:

| λ-calculus                                      | JavaScript                                            |
| ----------------------------------------------- | ----------------------------------------------------- |
| `λn (λf. λx. n (λg.λh. h (g f)) (λu.x) (λu.u))` | `n => f => x => n(g => h => h(g(f)))(u => x)(u => u)` |

### Is Zero

A function to determine whether its argument is the zero function
can be defined as:

| λ-calculus             | JavaScript                   |
| ---------------------- | ---------------------------- |
| `λn.n (λx.FALSE) TRUE` | `n => n(x => false_)(true_)` |

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

Addition can be seen as iterated successions.
An add function can be defined as;

| λ-calculus        | JavaScript             |
| ----------------- | ---------------------- |
| `λmn. (m succ) n` | `m => n => m(succ)(n)` |

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

Subtraction can be seen as iterated predecessors.
A subtraction function can be defined as:

| λ-calculus        | JavaScript             |
| ----------------- | ---------------------- |
| `λmn. (n pred) m` | `m => n => n(pred)(m)` |

This returns `zero` if `m` is less than `n`
because we don't have a way to represent negative numbers.

### Multiplication

Multiplication can be seen as iterated addition.
A multiply function (mul) can be written as the following
where `m` and `n` are the two numbers to be multiplied:

| λ-calculus         | JavaScript                  |
| ------------------ | --------------------------- |
| `λmn. m (add n) 0` | `m => n => m(add(n))(zero)` |

The functions above are correct, but multiplication
is also the same as the composition of two numbers.
See the "Function Composition" section below.

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

Division of integer numbers usually results in a floating point result
and there isn't an easy way to represent floating point numbers in λ-calculus.
We could define division as iterated subtraction
and make the result be the number of times that one integers goes into another.

TODO: Add this.

### Exponentiation

Exponentiation can be seen as iterated multiplication.
An exponentiation function (exp) can be defined as the following
where `m` is the base and `n` is the exponent:

| λ-calculus         | JavaScript                 |
| ------------------ | -------------------------- |
| `λmn. n (mul m) 1` | `m => n => n(mul(m))(one)` |

For example, here are the steps to raise 2 to the 3rd power.
See the representations for 2 and 3 in the table above.

```text
(λmn. n (mul m) 1) 2 3
3 (mul 2) 1
(λfx.f (f (f x))) (mul 2) 1
-- We can assume the mul function works.
(mul 2) ((mul 2) ((mul 2) 1))
(mul 2) ((mul 2) 2)
(mul 2) 4
8
```

An exponentiation function can be defined even more simply
as reverse function composition.

| λ-calculus | JavaScript       |
| ---------- | ---------------- |
| `λmn. n m` | `m => n => n(m)` |

```text
(λmn. n m) 2 3
3 2
(λfx.f (f (f x))) 2
λx.2 (2 (2 x))
which is 2 times 2 times 2 which is 8.
```

## Equality

A function to determine if two Boolean values are equal can be defined as:

| λ-calculus                                  | JavaScript                                     |
| ------------------------------------------- | ---------------------------------------------- |
| `λab. (or (and a b) (and (not a) (not b)))` | `a => b => or(and(a)(b))(and(not(a))(not(b)))` |

A function to determine if two numbers are equal can be defined as:

| λ-calculus                                      | JavaScript                                            |
| ----------------------------------------------- | ----------------------------------------------------- |
| `λmn.and (iszero (sub m n)) (iszero (sub n m))` | `m => n => and(iszero(sub(m)(n)))(iszero(sub(n)(m)))` |

We have to test both argument orders because our `sub` function returns zero
when the first number is less than the last number.

## Function Composition

A function to compose two functions can be defined as:

| λ-calculus     | JavaScript               |
| -------------- | ------------------------ |
| `λfgx.f (g x)` | `f => g => x => f(g(x))` |

## Recursion

Functions in λ-calculus do not have names.
This leaves no way for a function to refer to itself
which makes implementing recursion difficult.

### Omega Combinator

A value `v` is a "fixed point" of a function `f` if `f(v)` is equal to `v`.

The function `λx.x x` has a "fixed point" of `λx.x x`
which happens to be the same as the definition of the function.
The expression `(λx.x x) (λx.x x)` is called the omega (Ω) combinator.
Substituting this fixed point value for each `x` in the function
yields the exact same expression.
Evaluating this repeats forever, creating an infinite loop.

### Y Combinator

The Y combinator, invented by Haskell Curry, is a function that
implements recursion and provides a way of implementing loops.
It adds use of the function parameter `f` to the previous definition,
which represents the computation to be performed in each iteration.
It is defined as `λf.(λx.f (x x)) (λx.f (x x))`.
Note that the body contains two identical terms.

The Y combinator is intended to be used in
lazily evaluated languages like Haskell.
When used in a strictly evaluated language like JavaScript, it will
loop endless until the heap space is exhausted and the program crashes.

We can attempt to define a factorial function as:

```text
fact = λn.if (iszero n) 1 (mult n (fact (pred n)))
```

This assumes that functions have names that can be used to call themselves,
but they do not.

Instead we can use the Y combinator to define a factorial function.
Note how this differs slightly from the definition above
in order to work in a strictly evaluated language.
TODO: Is the modified version equivalent to the Z combinator?

```js
const Y = f => (x => x(x))(x => f(y => x(x)(y))); // λf.(λx.x x) (λx.f (x x))
```

Next, we need a function to compute a single result in a factorial sequence.
This function is not recursive because
λ-calculus doesn't support named functions.
The function `f` is a parameter of this function
and is used to simulate recursion.

```js
const facgen = f => n => iszero(n)(() => one)(() => mul(n)(f(pred(n))))();
```

Breaking this down, `iszero(n)` selects one of the following functions:

- If true then `(() => one)` is selected.
- if false then `(() => mul(n)(f(pred(n))))` is selected.

The selected function is then invoked with `()`.

Finally, we can define a function that combines
the Y combinator and the `facgen` function.

```js
const factorial = Y(facgen);
```

`factorial(zero)` returns the function for `one`.  
`factorial(one)` returns the function for `one`.  
`factorial(two)` returns the function for `two`.  
`factorial(three)` returns the function for `six`.  
and so on.

### Z Combinator

The Z combinator is similar to the Y combinator,
but it provides lazy evaluation,
deferring function applications until their results are needed.
It is an eta-expansion of the Y combinator which can be defined as:

```text
Z = λf.(λx.f(λv.x x v))(λx.f (λv.x x v))
```

It can also be defined as the following
where `M` is the Mockingbird function:

```text
λf.M (λx.f (λv.M x v))
```

A JavaScript implementation can be defined as:

```js
// Note that the two terms at the end are identical.
const Z = f => (x => f(y => x(x)(y)))(x => f(y => x(x)(y)));
```

This can be used in place of the `Y` function above
to define a factorial function.

## Linked Lists

We can simulate linked lists with "cons cells".
The function `cons`, short of "construct",
creates a cons cell which holds a pair of values.
The function `car` takes a cons cell and returns its first element.
The function `cdr` takes a cons cell and returns its last element.

```js
const cons = a => b => f => f(a)(b);
const car = p => p(true_);
const cdr = p => p(false_);
const nil = f => x => null; // used to mark the end of a linked list
```

The following code demonstrates using these functions:

```js
const pair = cons(one)(two);
expect(car(pair)).toBe(one);
expect(cdr(pair)).toBe(two);

const list = cons(one)(cons(two)(cons(three)(nil)));
expect(car(list)).toBe(one);
expect(car(cdr(list))).toBe(two);
expect(car(cdr(cdr(list)))).toBe(three);
expect(cdr(cdr(cdr(list)))).toBe(nil);
```

## Bird Combinators

<a href="https://en.wikipedia.org/wiki/Raymond_Smullyan"
target="_blank">Raymond Smullyan</a> (1919-2017)
assigned bird names to many of the combinators used in λ-calculus
in his book "To Mock a Mockingbird" which contains many logic puzzles.

TODO: Add detail on these.

- Idiot (I)
- Mockingbird (M)
- Kestrel (K)
- Kite (KI)
- Cardinal (C)
- Bluebird (B)
- Thrush (Th)
- Vireo (V)
- Blackbird (B1)
