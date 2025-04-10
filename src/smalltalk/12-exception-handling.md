---
eleventyNavigation:
  key: Exception Handling
  order: 12
  parent: Smalltalk
layout: topic-layout.njk
---

Exceptions are described by a specific kind of exception object
that can hold data describing the problem.
To throw/raise an exception object, send it the
message `#signal:` with a `String` argument description.

Exceptions that are thrown by code in a block can be caught and handled.
Unhandled exceptions result in a Debug window being opened
that contains a stack trace, also referred to as a "walkback".

Smalltalk seems to use the words "exception" and "error" interchangeably.

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

The methods `on:do:on:do:` and `on:do:on:do:on:do:`
are used to handle two or three different kinds of exceptions.

The method `onDNU:do:` is used to handle `MessageNotUnderstood` exceptions.

The method `ensure:` is used to specify code to execute
after the code in the receiver block executes,
regardless of whether it raises an exception.

To throw a generic `Exception`:

```smalltalk
Error signal: 'some message'
```

Smalltalk provides many subclasses of the `Exception` class.
Examples include `ArithmeticError`, `AssertionFailure`, `Error`, `Halt`,
`MessageNotUnderstood`, `NotYetImplemented`, and `ZeroDivide`.

To define a custom exception, create a subclass of the `Error` class
or one of its subclasses such as `Error`.
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
    aNumber <  lowerBound ifTrue: [ex signal: 'too low'].
    aNumber > upperBound ifTrue: [ex signal: 'too high'].
    score := aNumber
```

To catch an exceptions that may be thrown by a method,
send a message that invokes the method inside a block
and send the `#on:do:` message to the block.
Sending the message `#messageText` to an exception object
returns the message text of the exception.

For example:

```smalltalk
[s := Game new score: 5] on: OutOfBoundsException do: [:ex |
    ex messageText print.
]
```

## Custom Error Handling

When creating an image for an app that non-developers will use,
it is desirable to prevent Debug windows from appearing
when an unhandled error occurs.
One way to do this is to modify the `defaultAction` method
in the `UnhandledError' class.

The default implementation of this method is the following:

```smalltalk
defaultAction
    Smalltalk isDevelopmentEnvironmentPresent
        ifTrue: [self devDefaultAction]
        ifFalse: [self standaloneAppDefaultAction]"
```

This can be changed to the following:
TODO: This is not working yet!

```smalltalk
defaultAction
    | message receiverName selector |
    receiverName := self exception receiver class name.
    selector := self exception message selector.
    message := '{1}: receiver {2}, selector {3}' format: {self description. receiverName. selector }.
    self explore.
    AlertDialog message: message.
```

## Context

TODO: Supposedly the value of `thisContext` is captured in exception objects,
TODO: but I don't see this happening. Is it supposed to be the value of the
TODO: `signalContext` instance variable? I see that getting set to `nil`.
