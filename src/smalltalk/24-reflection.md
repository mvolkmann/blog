---
eleventyNavigation:
  key: Reflection
  order: 24
  parent: Smalltalk
layout: topic-layout.njk
---

## Overview

Smalltalk provides many methods for
getting information about classes and objects.
The following table lists some of them.

| Method                                                  | Answers                                                                                                                      |
| ------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `object class`                                          | the class of `object`                                                                                                        |
| `object instVarNamed: #varName`                         | the value of an instance variable in `object`                                                                                |
| `object instVarNamed: #varName` put: newValue           | sets value of an instance variable in `object` and answers new value                                                         |
| `object class = SomeClass`                              | answers `Boolean` indicating if `object` is an instance of the class `SomeClass` (not one of its subclasses)                 |
| `object isKindOf: SomeClass`                            | answers `Boolean` indicating if `object` is an instance of the class `SomeClass` or one of its subclasses                    |
| `object respondsTo: #selector`                          | answers `Boolean` indicating if the object responds to a given method selector                                               |
| `SomeClass name`                                        | the name of the class as a `String`                                                                                          |
| `Smalltalk allClasses`                                  | an `Array` of all classes defined in the current image                                                                       |
| `Smalltalk allClassesImplementing: #selector`           | an `Array` of all classes that implement a given selector                                                                    |
| `SystemOrganization categoryOfElement: #SomeClass`      | name of the class category to which a given class belongs                                                                    |
| `SomeClass allClassVarNames`                            | a `Set` of class variable names defined in this class                                                                        |
| `SomeClass allSelectors`                                | an `IdentitySet` of all message selectors (as `Symbol` objects) supported by this class, including selectors in superclasses |
| `SomeClass lookupSelector: #selector`                   | the matching `CompiledMethod`, indicating where it is found                                                                  |
| `SomeClass allInstances`                                | an `Array` of all existing instances of this class                                                                           |
| `SomeClass allInstVarNames`                             | an `Array` of instance variable names defined in this class                                                                  |
| `SomeClass allInstVarNamesEverywhere`                   | an `Array` of instance variable names defined in this class and inherited classes                                            |
| `SomeClass allMethodsInCategory: 'some-category'`       | an `Array` of instance methods in a given category, including those defined in this class and inherited                      |
| `SomeClass allSubclasses`                               | an `OrderedCollection` of subclasses                                                                                         |
| `SomeClass superclass`                                  | a `Class` that is the superclass of this class                                                                               |
| `SomeClass withAllSuperclasses`                         | an `OrderedCollection` containing all superclasses of this class going up the hierarchy                                      |
| `SomeClass allSuperclasses`                             | an `OrderedCollection` of superclasses                                                                                       |
| `SomeClass selectors`                                   | an `Array` of all instance method selectors (as `Symbol` objects) of this class                                              |
| `SomeClass class`                                       | the `Metaclass` subclass of this class                                                                                       |
| `SomeClass class selectors`                             | an `Array` of all class method selectors (as `Symbol` objects) of this class                                                 |
| `SomeClass class allMethodsInCategory: 'some-category'` | an `Array` of class methods in a given category, including those defined in this class and inherited                         |
| `CodeListPackages installedPackages`                    | an `Array` of `CodePackage` objects (appear in System Browser class category pane)                                           |
| `thisContext sender`                                    | a `MethodContext` describing the method that invoked the current one                                                         |
| `someMethodContext receiver`                            | the object that sent the message that invoked the `MethodContext`                                                            |
| `someMethodContext selector`                            | the selector `Symbol` of the message that invoked the `MethodContext`                                                        |
| `someSelector keywords`                                 | an `Array` of the keywords in the selector                                                                                   |

## Getting Keywords from Selectors

The following examples demonstrate getting keywords from a message selector:

```smalltalk
#foo keywords. "#('foo')"
#foo:bar:baz: keywords. "#('foo:' 'bar:' 'baz:')"
```

## Operating on All Instances

To run code on every instance of a given class,
send the `allInstancesDo:` message to the class.

For example, to delete all instances of a given class, run
`SomeClass allInstancesDo: [:obj | obj delete]`.

## Sending Messages

See the "Dynamic Messages" section in the "Messages" page.
This describes how to use `perform:` to send messages to objects
where the selector is a `Symbol`.

## Finding the Method for a Selector

There isn't a provided method that finds the nearest class in the
inheritance hierarchy that implements a method with a given selector.
I added the following method to do that.
It is in the `Behavior` class in the method category "\*TypeCheck",
so it is saved in the TypeCheck package.

```smalltalk
lookupClassImplementingSelector: selectorSymbol
    "Look up the given selector in my methodDictionary.
    Return the class that implements it if found.
    Otherwise chase the superclass chain and try again.
    Return nil if no implementing class is found."
    | class |

    class := self.
    [class == nil] whileFalse: [
        class includesSelector: selectorSymbol :: ifTrue: [^ class].
        class := class superclass
    ].
    ^ nil
```

## Finding Methods by Example

The Squeak development environment provides a way to find methods by example
using the "Method Finder" tool (a.k.a. Selector Browser).

<img alt="Squeak Selector Browser" style="width: 85%"
  src="/blog/assets/squeak-selector-browser.png?v={{pkg.version}}">

I implemented similar functionality in Cuis, minus the GUI.
See <a href="https://github.com/mvolkmann/Cuis-Smalltalk-FindByExample"
target="_blank">Cuis-Smalltalk-FindByExample</a>.
It's amazing how fast this is!

<img alt="Cuis Smalltalk Method Finder" style="width: 85%"
  src="/blog/assets/cuis-smalltalk-method-finder.png?v={{pkg.version}}">

For example, evaluating the following in a Workspace:

```smalltalk
Finder methodsByExample: #(#(1 2 3 4) 2.5)
```

prints the following in the Transcript:

```text
The following methods on #(1 2 3 4) with no arguments return 2.5:
- average
- mean
```

There are more examples in the README.

## Questions

TODO: Why does `allClassVarNames` return a `Set` when `allInstVarNames` returns an `Array`?
TODO: Is there a way to get all the message categories used by a class?
