---
eleventyNavigation:
  key: Exception Handling
  order: 12
  parent: Smalltalk
layout: topic-layout.njk
---

Exceptions are described by a specific kind of exception object
that can hold data describing the problem.
To throw/raise an exception object, sent it the
message `#signal:` with a `String` argument description.

Exceptions that are thrown by code in a block can be caught and handled.
Unhandled exceptions result in a Debug window being opened
that contains a stack trace.

Smalltalk seems to use the words "exception" and "error" interchangably.

The following code demonstrates catching an exception
that is thrown by code in a block that divides a number by zero.
It opens an Inspector window to examine details of the exception object.

```smalltalk
[3 / 0] on: ZeroDivide do: [:ex | ex inspect].
```

The class of the exception object is `ZeroDivide`.
Its instance variables include:

- `messageText`: `nil`
- `receiver`: `3`
- `selector`: `#/`
- `arguments`: `#(0)`

To throw a generic `Exception`:

```smalltalk
Error signal: 'some message'
```

Smalltalk provides many subclasses of the `Exception` class.
Examples include `ArithmeticError`, `AssertionFailure`, `Error`, `Halt`,
`MessageNotUnderstood`, `NotYetImplemented`, and `ZeroDivide`.

To define a custom exception, create a class that is a subclass
of the `Exception` class or one of its subclasses such as `Error`.
The custom class can include instance variables and methods
that are specific to that exception.
For example:

```smalltalk
Error subclass: #OutOfBoundsException
    instanceVariableNames: 'lowerBound upperBound'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

The following class method is used to create an instance:

```smalltalk
lower: lowerNumber upper: upperNumber
    ^self new setLower: lowerNumber upper: upperNumber
```

The following instance method is used by the class method above:

```smalltalk
setLower: lowerNumber upper: upperNumber
    super initialize.
    lowerBound := lowerNumber.
    upperBound := upperNumber
```

If an exception subclass defines class methods that create a new instance,
make sure to call `super initialize` as shown above.

To throw a custom exception send the class, or an instance of it,
the `#signal:` message.
For example, the following instance method can be defined
in a class that has a `score` instance variable:

```smalltalk
score: aNumber
    | ex |
    ex := OutOfBoundsException lower: lowerBound upper: upperBound.
    aNumber <  lowerBound ifTrue: [ ex signal: 'too low' ].
    aNumber > upperBound ifTrue: [ ex signal: 'too high' ].
    score := aNumber
```

To catch an exceptions that may be thrown by a method,
send a message that invokes the method inside a block
and send the `#on:do:` message to the block.
Sending the message `#messageText` to an exception object
returns the message text of the exception.

For example:

```smalltalk
[s := Game new score: 5] on: OutOfBoundsException do: [ :ex |
    ex messageText print.
]
```

## Context

TODO: Supposedly the value of `thisContext` is captured in exeception objects,
TODO: but I don't see this happening. Is it supposed to be the value of the
TODO: `signalContext` instance variable? I see that getting set to `nil`.
