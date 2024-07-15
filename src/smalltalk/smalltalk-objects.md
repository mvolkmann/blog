---
eleventyNavigation:
  key: Objects
  order: 1.3
  parent: Smalltalk
layout: topic-layout.njk
---

Code and data are both represented by objects.
Code can be described by a method or block, both of which are kinds of objects.

Objects are created by sending a message to a class.
In addition, some kinds of objects, such as numbers, strings, and arrays,
can be created from a literal syntax.

Every class supports the class method `new`,
which creates and returns a new instance of the class.
If the class defines the instance method `initialize`,
the `new` method will call it.
The `initialize` method typically initializes
each of the instance variables of the object.

Let's look at an example class named `Rect`
with instance variables `height` and `width`.
We chose the name `Rect` because the name `Rectangle` is used
by a provided class in the "Graphics-Primitives" class category.

We can define the class method `height:width:`
that provides an alternate way to create objects as follows:

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth
```

We can then define the following instance methods:

```smalltalk
initialize
    super initialize.
    height := 1.
    width := 1

setHeight: aHeight width: aWidth
    height := aHeight.
    width := aWidth

area
    ^height * width
```

The `setHeight:width:` method should be in the "private" category
to indicate that it is not meant to invoked from outside this class.

We can use the `Rect` class as follows:

```smalltalk
r1 := Rect new.
Transcript show: r1 area. "1"

r2 := Rect height: 2 width: 3.
Transcript show: r2 area. "6"
```

To determine the class of an object, send it the `#class` unary message.
For example, `19 class` returns `SmallInteger`.

Variables defined in a Workspace hold references to their object values.
Closing a Workspace removes those references,
which makes it possible for them to be garbage collected.

Some classes such as `Morph` implement a `delete` method.
To delete all instances of such classes,
enter the following in Workspace and "Do it":

```smalltalk
SomeClass allInstancesDo: [ :obj | obj delete ]
```

## Object Creation Detail

New objects can be created by
sending the message `#new` or `#basicNew` to a class.

The diagram below shows the classes involved
when sending the message `#new` to a class.

`Circle` is a subclass of `Shape`
which is a subclass of `Object`
which is a subclass of `ProtoObject`.
The superclass of `ProtoObject` is `nil`.

The asterisks in the diagram are placeholders
for the class associated with an arrow.
For example, sending the message `#superclass` to `Circle` gives `Shape`
and sending the message `#class` to `Circle` gives `Circle class`.

<img alt="Smalltalk class hierarchy:" style="width: 100%"
  src="/blog/assets/SmalltalkClassHierarchy.png?v={{pkg.version}}">

Here are some important facts about the diagram above:

- Every class is represented by an object
  which is an instance of its own metaclass.
- The instance methods of a metaclass are
  the class methods of the corresponding class.
- When a class is selected in a System Browser, clicking the "class" button
  causes it to display the metaclass instance variables
  which are the class methods of the class.
- The name of each metaclass is the name of its corresponding class
  followed by " class". For example, the name of the
  metaclass of the `Circle` class is `Circle class`.
- There is only one instance of each metaclass.
- The superclass of all metaclasses it `Metaclass`.
- `Metaclass` is subclass of `ClassDescription`
  which is a subclass of `Behavior`
  which is a subclass of `Object`.
- The `Behavior` class defines the `new` method and many others including
  `allInstances`, `allSubclasses`, `allSuperclasses`, `instVarNames`,
  and `methodDict`.
- The `Behavior` `new` method returns `self basicNew initialize`.
- The `Object` metaclass implements the instance method `initialize`
  which does nothing, leaving all the instance variables initialized to `nil`.
- Any class can implement the instance method `initialize`
  to set its instance variables to values other than `nil`.

Let's walk through the steps to find the `new` method
when evaluating `circle := Circle new`.

- Look in `Circle class`.
- Look in `Shape class`.
- Look in `Object class`.
- Look in `ProtoObject class`.
- Look in `Class class`.
- Look in `ClassDescription class`.
- Look in `Behavior class` where it is found.

The `basicNew` method does not call the instance method `initialize`.
The `basicNew` method cannot be overridden in subclasses
to do something different, but the `new` method can be overridden.

TODO: Describe why the class `ProtoObject` exists.

`Object` is a superclass of nearly every other class.
There are a small number of classes that are subclasses of `ProtoObject`
and not subclasses of `Object`.
These include `BreakingMethodWrapper`, `MessageCatcher`, and `ProtoCatcher`.

## Method Dictionaries

Method dictionaries are used to process message sends.
Their keys are symbols that are message selectors
and their values are `CompiledMethod` objects.
The processing for finding a matching method
searches the `methodDict` instance variables
found in the inheritance hierarchy of the receiver object.

The instance methods of a class are stored in
the provided instance variable `methodDict` of the class.
This can be viewed by selecting the class name and pressing cmd-i (Inspect it).
The value of `methodDict` is an instance of `MethodDictionary`
which is a subclass of `Dictionary`.

The class methods of a class are stored in
the provided instance variable `methodDict` of the metaclass of the class.
This can be viewed by selecting the class name followed by " class"
and pressing cmd-i (Inspect it).
It is also an instance of `MethodDictionary`.
One way to find a metaclass name so it can be selected
is to select the class in a System Browser and click the "class" button.

## Immutability

A class can enforce the immutability of its object by
simply not implementing any methods that change its instance variable.

Another option is to use the "Immutability" package
so attempts to change any instance variable of a given object
wll result in a "ModificationForbidden" window opening that
contains a stack trace which indicates where the attempt was made.

To install the "Immutability" package,
enter `Feature require: 'Immutability'` and "Do it".
Among other things, this package adds
the instance method `beImmutable` to the `Object` class.

To enforce a specific object to be immutable,
send it the `#beImmutable` message.
For example the `Rect` class described above
could have the following class method for creating new instances:

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth; beImmutable; yourself
```

Note the use of `yourself` to return the current object
rather than the return value of the `beImmutable` method.
