---
eleventyNavigation:
  key: Does Not Understand
  order: 23
  parent: Smalltalk
layout: topic-layout.njk
---

The `Object` class defines the `doesNotUnderstand` method
which opens a `MessageNotUnderstood` window.
This can be overridden in specific classes to provide specialized processing
of messages that do not have corresponding methods.

Let's demonstrate this by defining an `Accessible` class that enables
sending messages to set and get any instance variable
in classes that inherit from this class.

Here is the class definition:

```smalltalk
Object subclass: #Accessible
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Here is its one and only instance method:

```smalltalk
doesNotUnderstand: aMessage
    "gets or sets an instance variable"

    | argCount getters index key setters |

    "We are only processing messages with 0 or 1 arguments.
    If there are 2 or more arguments, fall back to the default behavior."
    argCount := aMessage numArgs.
    argCount > 1 ifTrue: [ ^super doesNotUnderstand: aMessage ].

    key := aMessage keywords first.

    "If the message is a getter, return the
    corresponding instance variable value."
    getters := self class allInstVarNames.
    index := getters indexOf: key.
    index ifNotZero: [^self instVarAt: index].

    "If the message is a setter, set the
    corresponding instance variable value."
    setters := getters collect: [ :name | name, ':' ].
    index := setters indexOf: key.
    index ifNotZero: [^self instVarAt: index put: aMessage arguments first ].

    "Otherwise fall back to the default behavior."
    ^super doesNotUnderstand: aMessage
```

Here is an example subclass of `Accessible`:

```smalltalk
Accessible subclass: #Person
    instanceVariableNames: 'birthdate firstName lastName'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Here is an example of using this class:

```smalltalk
p := Person new.
p firstName: 'Mark'.
p firstName print. "Mark"
```

Getting and setting instance variables in this way is quite inefficient.
A better approach is to generate accessor methods
by right-clicking the `Person` class in a System Browser
and selecting "more...create inst var accessors"
to generate accessor methods for each instance variable.
