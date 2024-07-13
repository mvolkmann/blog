---
eleventyNavigation:
  key: Control Flow
  order: 1.3
  parent: Smalltalk
layout: topic-layout.njk
---

Control flow is provided through message passing.

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

### Iteration

The `timesRepeat` method in the `Integer` class can be used
to evaluate a block a given number of times.
For example:

```smalltalk
3 timesRepeat: ['Ho' print]
```

See the "Collections" section for messages that iterate over a collection.

See the "Blocks" section for messages that
use a block as an iteration condition.
