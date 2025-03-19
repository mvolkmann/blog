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
`#ifTrue:`, `#ifFalse:`, `#ifTrue:ifFalse:`, and `#ifFalse:ifTrue:`.
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
TODO: But does it make a difference if the value is a literal like a number or string?

The `Object` instance method `caseOf:` is similar to
the `switch` statement in other programming languages.
The argument for `caseOf:` is a dynamic array of `Association` elements
where the keys and values are both blocks.
It can optionally take an `otherwise:` argument
whose value is a block that evaluates to the
value to use if none if the cases are matched.
If `otherwise:` is not used and no match is found, the error
"Case not found, and not otherwise clause" is raised.

For example:

```smalltalk
color := #blue.

assessment := color caseOf: {
    [#red] -> ['hot'].
    [#green] -> ['warm'].
    [#blue] -> ['cold']
}.

assessment := color caseOf: {
    [#red] -> ['hot'].
    [#green] -> ['warm'].
    [#blue] -> ['cold']
} otherwise: ['unknown'].
```

Using `caseOf:` requires a sequential search for a matching value.
If there are a large number of cases, consider using a `Dictionary`
that is initialized one time outside the methods that use it.
The values can be blocks containing arbitrary code.
For example:

```smalltalk
colorToAssessment := Dictionary newFrom: {
    #red -> ['hot'].
    #green -> ['warm'].
    #blue -> ['cold']
}.
...
assessment := colorToAssessment at: color.
```

The `Association` objects in the collection passed to the
`caseOf:` method must have keys and values that are both blocks.
Often the blocks just contain a single literal value
such as a number, symbol, or string.
We can define new instance methods in the `Object` class
that are similar to `caseOf:` and `caseOf:otherwise:`,
but do not require the `Association` keys and values to be blocks.

```smalltalk
switchOn: assocColl
    "The elements of assocColl are associations. Answer the evaluated value
    of the first association in assocCol whose evaluated key equals the receiver.
    If no match is found, signal an error.
    This differs from caseOf: in that the keys and values
    of the associations are not required to be blocks."

    ^ self switchOn: assocColl otherwise: [ self caseError ]

switchOn: assocColl otherwise: aBlock
    "The elements of assocColl must be associations. Answer the evaluated value
    of the first association in assocCol whose evaluated key equals the receiver.
    If no match is found, answer the result of evaluating aBlock.
    This differs from caseOf: in that the keys and values
    of the associations are not required to be blocks."

    assocColl associationsDo:
        [ :assoc | (assoc key value = self) ifTrue: [ ^assoc value value ] ].
    ^ aBlock value
```

The following code demonstrates using the `switchOn:otherwise:` method.

```smalltalk
command := #pause.

message := command switchOn: {
    #start -> 'Starting the system'.
    #pause -> 'Pausing the system'.
    #resume -> 'Resuming the system'.
    #stop -> 'Stopping the system'.
} otherwise: 'Unsupported command'.

message print.
```

The "switch" methods defined above are defined in the package
<a href="https://github.com/mvolkmann/Cuis-Smalltalk-Switch"
target="_blank">Cuis-Smalltalk-Switch</a>.

Using nested `#ifTrue:ifFalse:` messages can be verbose and
requires having the correct number of closing square brackets at the end.
For example, the following assigns a string to a variable
based on the temperature:

```smalltalk
assessment :=
    temperature > 80 ifTrue: 'hot' ifFalse: [
    temperature < 40 ifTrue: 'cold' ifFalse: [
    'warm']]
```

Another option is to send the message `#caseOf:` to `true`.
This avoids using nested `ifFalse:` messages and counting square brackets.
For example:

```smalltalk
assessment := true caseOf: {
    [temperature > 80] -> ['hot'].
    [temperature < 40] -> ['cold'].
} otherwise: ['warm'].
```

Another option, suggested by Lauren Pullen, is to add an
instance method like `cond:` to the `ArrayedCollection` class.
It can be defined as follows:

```smalltalk
cond: defaultBlock
    "Mimic the Common Lisp/Scheme COND with a Smalltalk twist."
    self associationsDo: [:assoc |
        assoc key value ifTrue: [^assoc value value]
    ].
    ^ defaultBlock value.
```

The following code demonstrates using the `cond:` method.

```smalltalk
condition := {
    [temperature > 80] -> ['hot'].
    [temperature < 40] -> ['cold']
} cond: ['warm'].
```

Moving the logic to its own function, perhaps in the "private" category,
can make the code less verbose and more clear.
When a block uses the `^` operator to return a value,
the containing method exits and returns that value.
For example:

```smalltalk
assessTemperature: aNumber
    aNumber > 80 ifTrue: [^'hot'].
    aNumber < 40 ifTrue: [^'cold'].
    ^'warm'
```

Many classes define methods that return a `Boolean` value
that is useful for conditional logic. Some examples include:

| Class              | Instance Method   |
| ------------------ | ----------------- |
| `Character`        | `isAlphaNumeric`  |
| `Character`        | `isDigit`         |
| `Character`        | `isLetter`        |
| `Character`        | `isLineSeparator` |
| `Character`        | `isLowercase`     |
| `Character`        | `isPathSeparator` |
| `Character`        | `isSeparator`     |
| `Character`        | `isUppercase`     |
| `Character`        | `isVowel`         |
| `Collection`       | `isEmpty`         |
| `Color`            | `isDark`          |
| `Color`            | `isLight`         |
| `Color`            | `isTransparent`   |
| `Date`             | `isLeapYear`      |
| `Exception`        | `isResumable`     |
| `Integer`          | `isPowerOfTwo`    |
| `Integer`          | `isPrime`         |
| `Interval`         | `isEmpty`         |
| `Number`           | `isDivisibleBy:`  |
| `Number`           | `isEven`          |
| `Number`           | `isNaN`           |
| `Number`           | `isOdd`           |
| `Number`           | `isZero`          |
| `Object`           | `isArray`         |
| `Object`           | `isBlock`         |
| `Object`           | `isCharacter`     |
| `Object`           | `isCollection`    |
| `Object`           | `isFloat`         |
| `Object`           | `isFraction`      |
| `Object`           | `isInteger`       |
| `Object`           | `isInterval`      |
| `Object`           | `isMemberOf`      |
| `Object`           | `isNumber`        |
| `Object`           | `isPinned`        |
| `Object`           | `isPoint`         |
| `Object`           | `isString`        |
| `Object`           | `isSymbol`        |
| `Point`            | `isZero`          |
| `ProtoObject`      | `isNil`           |
| `String`           | `isEmpty`         |
| `SystemDictionary` | `isHeadless`      |
| `SystemVersion`    | `isCuis`          |
| `SystemVersion`    | `isPharo`         |
| `SystemVersion`    | `isSqueak`        |

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
For example:

```smalltalk
interval := 1 to: 10 by: 2.
interval do: [:n | n print]
```

See the "Blocks" section for messages that
use a `BlockClosure` as an iteration condition.
The methods include `whileTrue`, `whileTrue:`, `whileFalse`, `whileFalse:`,
`whileNotNil:`, and `whileNil:`.

See the "Collections" section for messages that iterate over a collection.
