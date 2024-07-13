---
eleventyNavigation:
  key: Blocks
  order: 1.3
  parent: Smalltalk
layout: topic-layout.njk
---

A block is closure (anonymous function) that can have parameters
and contain many expressions.
They are represented by the class `BlockClosure`.
The value of the block is the value of its last expression.

Blocks take zero or more positional arguments,
which is something methods cannot do.
Argument names appear at the beginning of a block
and each name is preceded by a colon.
The argument list is separated from the expressions by a vertical bar.

Blocks can be saved in variables,
passed as arguments to methods and other blocks,
and can be evaluated multiple times. For example:

```smalltalk
noArgBlock := [2 * 3].
singleArgBlock := [:a | a * 3].
multipleArgBlock := [:a :b | a * b].
```

Blocks support several messages that evaluate the block
whose names begin with `value`.
These message enable providing zero to four arguments.
For blocks with more than four parameters, send the message
`#valueWithArguments:` and an array holding all the arguments.
For example:

```smalltalk
noArgBlock value.
singleArgBlock value: 1.
multiArgBlock value: 1 value: 2.
multiArgBlock value: 1 value: 2 value: 3.
multiArgBlock value: 1 value: 2 value: 3 value: 4.
multiArgBlock valueWithArguments: #(1 2 3 4 5).
```

A block must be passed the same number of arguments as it has parameters.
If a block is passed fewer or more arguments than it accepts,
a Debug window will open.

Blocks can declare and use temporary (local) variables just like a method.

For example:

```smalltalk
average := [:a :b |
    | sum |
    sum := a + b.
    sum / 2.0
```

Blocks are closures, meaning that they can
access variables defined outside them. For example:

```smalltalk
n := 19.
b := [:a | a + n].
b value: 2 "result is 21"
```

To use a block as an iteration condition,
use the methods `whileTrue`, `whileFalse`, `whileNotNil`, and `whileNil`
that are defined in the `BlockClosure` class.
Note that these are not methods on the `Boolean` class.

For example, the following code prints the integer values 1 through 10
in the Transcript:

```smalltalk
| counter |
counter := 1.
[counter <= 10] whileTrue: [
    counter print.
    counter := counter + 1.
].
```

A block can call itself if it passes itself in as an argument.
For example:

```smalltalk
fact := [:block :n |
    n = 1
        ifTrue: 1
        ifFalse: [n * (block value: block value: n - 1)]
].

fact value: fact value: 5 "gives 120"
```

If a block uses the caret symbol (`^`) to return a value,
the containing method will exit and return that value.
