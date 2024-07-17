---
eleventyNavigation:
  key: Control Flow
  order: 22
  parent: Smalltalk
layout: topic-layout.njk
---

Smalltalk does not provide special syntax for conditional logic and iteration.
Instead, control flow is provided through message passing.

### Conditional Logic

The `Boolean` class in the `Kernel-Objects` category contains the methods
`#ifTrue`, `#ifFalse`, `#ifTrue:ifFalse`, and `#ifFalse:ifTrue`.
For example:

```smalltalk
result := a < b ifTrue: ['less'] ifFalse: ['more'].
```

The values for `ifTrue` and `ifFalse` can be
literal values, variables, or blocks with no parameters.
The messages listed above just send the `value` message to the argument value.
Typically the `value` message is used to evaluate a no-arg block.
However, the `Object` class defines the `value` method to just return `self`,
which is what enables any kind of object to be used.

When blocks are used, the compiler is able to optimized the code by
inlining the code within the block and
avoiding the need to send the `value` message.
So it is more efficient to use blocks.

The `Object` instance method `caseOf` is similar to
the `switch` statement in other programming languages.

For example:

```smalltalk
color := 'blue'.
assessment := color caseOf: {
    ['red'] -> ['hot'].
    ['green'] -> ['warm'].
    ['blue'] -> ['cold']
}
```

When a block uses the `^` operator to return a value,
the containing method exits and returns that value.
For example, the following method returns
a `String` based on the `Number` passed in:

```smalltalk
assessTemperature: aNumber
    aNumber > 80 ifTrue: [^ 'hot'].
    aNumber < 40 ifTrue: [^ 'cold'].
    ^ 'warm'
```

### Iteration

The `Integer` class described in the "Basic Data Types" section
provides several methods for iteration.

The `timesRepeat:` method evaluates a block a given number of times.
For example, the following code prints the string `'Ho'`
three times on separate lines:

```smalltalk
3 timesRepeat: ['Ho' print]
```

The `to:do:` and `to:by:do:` methods evaluate a block
for each `Integer` value in a range with an optional step (`by:`) size.
For example, the following code prints
the numbers 1, 3, 5, 7, and 9 on separate lines.

```smalltalk
1 to: 10 by: 2 do: [:n | n print]
```

An alternative is to first create an `Interval` object and
then send it the `#do:` message to evaluate a block argument
for each value in the `Interval`.

See the "Blocks" section for messages that
use a block as an iteration condition.

See the "Collections" section for messages that iterate over a collection.
