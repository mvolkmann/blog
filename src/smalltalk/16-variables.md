---
eleventyNavigation:
  key: Variables
  order: 16
  parent: Smalltalk
layout: topic-layout.njk
---

Smalltalk supports five kinds of variables:

- Pseudo-variables are provided by the system and cannot be modified.
- Instance variables are associated with a specific instance of a class.
- Class variables are associated with a class
  and all subclasses share the same value.
- Class instance variables are similar to class variables, but
  they allow subclasses to have different values for the same variable name.
- Temporary (or local) variables are accessible only within
  the method or block where they are declared.

## Pseudo-variables

Pseudo-variables are variables whose value
is provided automatically and cannot be modified.
There are six of these with the names
`true`, `false`, `nil`, `self`, `super`, and `thisContext`.
These names are reserved words, meaning
they cannot be used for other kinds of variables.

`true` and `false` represent Boolean values.
They refer to singleton instances of the `True` and `False` classes.

`nil` represents the lack of a real value.

`self` can be used in instance methods to refer to the current object.
It can also be used in class methods to refer to the current class.

`super` can be used in instance methods
to refer to the superclass of the current object.
For example, the superclass of the `SmallInteger` class is `Integer`
and the superclass of the `Integer` class is `Number`.

`self` and `super` are often used as the receiver of messages.
For example, is typical for the instance method `initialize`
to begin with `super initialize`.
We will have more to say about the `initialize` method later.

From the
<a href="https://cuis-smalltalk.github.io/TheCuisBook/Pseudo_002dvariables.html"
target="_blank">Cuis book</a>, "`thisContext` ...
represents the top frame of the run-time stack. ...
It is essential for implementing development tools like the Debugger and
it is also used to implement exception handling and continuations."

## Instance Variables

Instance variables are associated with a specific instance of a class.
They are declared in a space-separated string that is
the value of the `instanceVariableNames:` keyword in a class definition.

Instance variables are always private, which means they can only be accessed by
instance methods in the class that defines them, and in subclasses.
To expose an instance variable value to methods in other classes,
define an instance method that returns it.

When a new instance of a class is created,
its instance method `initialize` is called.
This is, as the name suggests, a perfect place to
assign an initial value to each instance variable.

## Class Variables

Class variables are associated with a class
and the same value is shared with all subclasses.
They are declared in a space-separated string that is
the value of the `classVariableNames:` keyword in a class definition.

Class variables are described in the same way as instance variables,
but their names must begin uppercase.
It is common for a class to not have any class variables.

Like instance variables, class variables are always private.
To expose a class variable value to methods in other classes,
define a class method that returns it.

To assign initial values to the class variables of a class,
define the class method `initialize`
and explicitly send that message to the class.

## Class Instance Variables

Class instance variables are defined as an
instance variables in the metaclass of a given class.
Unlike class variables, subclasses can have a different value
for the variable than that used by the class where it is defined.
Class instance variables are not commonly used.

TODO: Describe how to define a class instance variable
and set different values in the class and subclasses.
Perhaps an example could be an Animal class with a legs variable
that is set to 0 in the Animal class and 4 in the Dog subclass.

## Temporary Variables

Temporary variables are declared in a space-separated string
between vertical bars inside a method or block definition.
Method parameters are also considered to be temporary variables.
These can only be accessed within the method or block that declares them.

In the following example, `numbers` and `sum` are temporary variables.

```smalltalk
average: numbers
    | sum |
    sum := numbers inject: 0 into: [ :acc :each | acc + each ].
    ^ sum / numbers size
```

## Global Variables

While Smalltalk does not support global variables,
the `Smalltalk` `SystemDictionary` object can be used for this purpose.
The following code adds the key "color" with the value "yellow"
to that `Dictionary`, and then retrieves the value for that key:

```smalltalk
Smalltalk at: 'color' put: 'yellow'.
color := Smalltalk at: 'color' ifAbsent: 'none'.
```
