---
eleventyNavigation:
  key: Object-Oriented Programming
  order: 3
  parent: Smalltalk
layout: topic-layout.njk
---

Before diving into the details of Smalltalk
which is a pure object-oriented programming language,
let's review the basics of object-oriented programming.

Alan Kay, one of the primary creators the Smalltalk,
coined the term "object-oriented programming" in 1967.

An object encapsulates related data and methods that operate on the data.
For example, an object can hold the `center` and `radius` of a circl.
In Smalltalk, these are referred to as instance variables.

A class is often used as template for creating
instances of the class which are referred to as objects.
For example, a `Circle` class can be defined.

A class can define:

- class methods that are used to create instances

  For example, as class method can take a name and breed
  and return a new object that uses those.

- class variables whose values are shared by all instances

  For example, a class variable can hold
  the timestamp at which the last `Circle` object was created.

- class methods that operate on class variables

  For example, a class method can return a formatted string representation
  of the timestamp held in a class variable.

- instance variables whose values are unique to each instances

- instance methods that operate on instance variables

Classes can inherit from other classes to define a "kind of" relationship
and share both data and methods.
For example, the classes `Square` and `Circle`
can inherit from the class `Shape`.

Most OO languages, such as Smalltalk,
only allow a class to inherit from one other class.
But there are languages that support multiple inheritance.

When a class `A` inherits from a class `B`, we say that
`A` is a subclass of `B` and `B` is a superclass of `A`.

When a class `A` is a subclass of a class `B`:

- Instance methods in `A` can access all the instance variables in `B`.
- Instance methods in `A` can invoke all the instance methods in `B`.
- Class methods in `A` can access all the class variables in `B`.
- Class methods in `A` can invoke all the class methods in `B`.

Polymorphism is the ability to select the implementation of a method to invoke
based on the kind of object on which it is invoked.
For example, the `Shape` class can require its subclasses
to implement an `area` method.
The classes `Square` and `Circle` can each define the `area` method
to compute the value differently.

A variable can hold a reference to a `Shape` object
which can be an instance of any subclass.
When the `area` method is invoked on that variable,
the actual method invoked is determined by the type of the object.

Some languages, such as Smalltalk, determine the method to invoke
at run-time instead of at compile-time.
This is referred to as "late binding".

A somewhat unique feature of Smalltalk is that
methods are invoked through message passing.
Rather that directly invoking a method,
a message is passed to an object and
a lookup process is used to find a corrsponding method.
Details about this lookup process are presented later.
