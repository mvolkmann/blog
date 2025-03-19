---
eleventyNavigation:
  key: Dependency Mechanism
  order: 26
  parent: Smalltalk
layout: topic-layout.njk
---

## Overview

Any object in Smalltalk can depend on another object
and be notified when by it when something significant occurs.
For example, multiple objects may want to be notified
when a particular property in some object is modified.

This is supported by the "dependency mechanism"
which is implemented by a set of methods in the `Object` class.

Each object maintains a collection of other objects that depend on it.

## Messages

To add a dependent to an object, send it the message `#addDependent:`
with an argument that is the dependent object to be added.

To query an object for a collection of its dependents,
send it the message `#dependents`.

To remove a dependent from an object, send it the message `#removeDependent:`
with an argument that is the dependent object to be removed.

To signal that a "change" has occurred in an object
and trigger each of its dependents to be notified,
set it the message `#changed`, `#changed:` or `#changed:with:`.

The `changed` method only indicates that some unspecified change occurred.
This method is implemented as `self changed: self`,
which calls the method described below.

The `changed:` method takes an argument named `aspectSymbol`
that specifies the kind of change that occurred.
Typically the argument is a symbol.
In Cuis Smalltalk this method is implemented as
`self triggerEvent: #changed: with: aspectSymbol`.

The `changed:with:` method takes an argument that
specifies the kind of change that occurred and an object describes the change.
Cuis Smalltalk does not support this.

Each of the "changed" methods causes the message `#update:`
to be sent to each of the dependents.

## Example

Let's walk though a simple example.
First we will define a `Temperature` class
that maintains a Celsius temperature
and notifies dependents when the temperature changes.

```smalltalk
Object subclass: #Temperature
    instanceVariableNames: 'celsius'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Demo'

initialize
    celsius := 0

celsius: aNumber
    celsius := aNumber.
    self changed: aNumber.

celsius
    ^ celsius
```

Next we will define a `Water` class
that maintains the state of the water (`#ice`, `#liquid`, or `#gas`)
depending on the temperature.
We could define many classes whose instances depend on the temperature.
The `update:` method is responsible for updating the `state`
when it is informed of a temperature change.

```smalltalk
Object subclass: #Water
    instanceVariableNames: 'state'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Demo'

initialize
    state := #liquid

state
    ^ state

update: temperature
    state := true caseOf: {
        [temperature <= 0] -> [#ice].
        [temperature >= 100] -> [#gas]
    } otherwise: [#liquid]
```

Finally, here is some example code that demonstrates
using the `Temperature` and `Water` classes.

```smalltalk
t := Temperature new.
w := Water new.
w state print. "#liquid"
t addDependent: w.
t celsius: -10.
w state. "#ice"
```
