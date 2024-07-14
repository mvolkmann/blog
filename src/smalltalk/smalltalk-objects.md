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

Classes are also represented by objects.
Every class object inherits from `Class`,
which inherits from `ClassDescription`,
which inherits from `Behavior`.
The `Behavior` class defines the instance method `new`
which contains the following:

```smalltalk
    ^ self basicNew initialize.
```

Every class also supports the method `basicNew` which is similar to
the `new` method, but does not call the instance method `initialize`.

Let's look at an example `Rect` class
with instance variables `height` and `width`.

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

We can use the `Rectangle` class as follows:

```smalltalk
r1 := Rect new.
Transcript show: r1 area. "1"

r2 := Rect height: 2 width: 3.
Transcript show: r2 area. "6"
```

To determine the class of an object, send it the `#class` unary message.
For example, `19 class` returns `SmallInteger`.

Variables defined in a Workspace hold references to their object values.
It may be necessary to close a Workspace in order to
trigger garbage collection of those objects.

Some classes such as `Morph` implement a `delete` method.
To delete all instances of such classes,
enter the following in Workspace and "Do it":

```smalltalk
SomeClass allInstancesDo: [ :obj | obj delete ]
```

## Creating Objects

TODO: This section repeats a lot of information that was covered above!

New objects can be created by
sending the message `#new` or `#basicNew` to a class.
Those methods are defined in the `Behavior` class
and are available on every object because:

- `Object` is a subclass of `ProtoObject`.
- The metaclass of `ProtoObject` is `Class` (`ProtoObject class superclass -> Class`).
- `Class` is a subclass of `ClassDescription`.
- `ClassDescription` is a subclass of `Behavior`.

The diagram below shows the classes involved
when sending the message `#new` to a class.
`Circle` is a subclass of `Shape`
which is a subclass of `Object`
which is a subclass of `ProtoObject`.
The asterisks in the diagram are placeholders
for the class associated with an arrow.
For example, sending the message `#superclass` to `Circle` gives `Shape`
and sending the message `#class` to `Circle` gives `Circle class`.

<img alt="Smalltalk class hierarchy:" style="width: 100%"
  src="/blog/assets/SmalltalkClassHierarchy.png?v={{pkg.version}}">

Here are some important facts about the diagram above:

- Every class is represented by an object
  which is an instance of its own metaclass.
- Metaclasses do not have names are referred to as "{class-name} class".
  For example, the metaclass of the `Circle` class
  is referred to as `Circle class`.
- There is only one instance of each metaclass.
- The superclass of all metaclasses it `Metaclass`.
- `Metaclass` is subclass of `ClassDescription`
  which is a subclass of `Behavior`
  which is a subclass of `Object`.
- The `Behavior` class defines the `new` method and many others including
  `allInstances`, `allSubclasses`, `allSuperclasses`, `instVarNames`,
  and `methodDictionary`.

Let's walk through the steps to find the `new` method
when evaluating `circle := Circle new`.

- Look in `Circle class`.
- Look in `Shape class`.
- Look in `Object class`.
- Look in `ProtoObject class`.
- Look in `Class class`.
- Look in `ClassDescription class`.
- Look in `Behavior class` where it is found.

TODO: Discuss what a "metaclass" is and how there methods become available.

The `new` method in `Behavior` sends the `#basicNew`
to the class from which an instance is being created,
which initializes all the instance variables to `nil`.
Then it sends the `#initialize` message to the new instance.

The `basicNew` method cannot be overridden in subclasses
to do something different, but the `new` method can be overridden.

TODO: Describe why the class `ProtoObject` exists.

The class `ProtoObject` is a subclass of itself.
TODO: What does this mean?

`Object` is a superclass of nearly every other class.
There are a small number of classes that are subclasses of `ProtoObject`
and not subclasses of `Object`.
These include `BreakingMethodWrapper`, `MessageCatcher`, and `ProtoCatcher`.

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
For example the `Rectangle` class described above
could have the following class method for creating new instances:

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth; beImmutable; yourself
```

Note the use of `yourself` to return the current object
rather than the return value of the `beImmutable` method.
