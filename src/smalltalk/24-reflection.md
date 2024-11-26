---
eleventyNavigation:
  key: Reflection
  order: 24
  parent: Smalltalk
layout: topic-layout.njk
---

Smalltalk provides many methods for
getting information about classes and objects.
The following table lists some of them.

| Method                                                  | Answers                                                                                                      |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `object class`                                          | the class of `object`                                                                                        |
| `object instVarNamed: #varName`                         | the value of an instance variable in `object`                                                                |
| `object instVarNamed: #varName` put: newValue           | sets value of an instance variable in `object` and answers new value                                         |
| `SomeClass name`                                        | the name of the class as a `String`                                                                          |
| `Smalltalk allClasses`                                  | an `Array` of all classes defined in the current image                                                       |
| `Smalltalk allClassesImplementing: #selector`           | an `Array` of all classes that implement a given selector                                                    |
| `SystemOrganization categoryOfElement: #SomeClass`      | name of the class category to which a given class belongs                                                    |
| `SomeClass allClassVarNames`                            | a `Set` of class variable names defined in this class                                                        |
| `SomeClass allSelectors`                                | an `IdentitySet` of all message selectors supported by this class, including selectors for inherited methods |
| `SomeClass lookupSelector: #selector`                   | the matching `CompiledMethod`, indicating where it is found                                                  |
| `SomeClass allInstances`                                | an `Array` of all existing instances of this class                                                           |
| `SomeClass allInstVarNames`                             | an `Array` of instance variable names defined in this class                                                  |
| `SomeClass allInstVarNamesEverywhere`                   | an `Array` of instance variable names defined in this class and inherited classes                            |
| `SomeClass allMethodsInCategory: 'some-category'`       | an `Array` of instance methods in a given category, including those defined in this class and inherited      |
| `SomeClass allSubclasses`                               | an `OrderedCollection` of subclasses                                                                         |
| `SomeClass superclass`                                  | a `Class` that is the superclass of this class                                                               |
| `SomeClass withAllSuperclasses`                         | an `OrderedCollection` containing all superclasses of this class going up the hierarchy                      |
| `SomeClass allSuperclasses`                             | an `OrderedCollection` of superclasses                                                                       |
| `SomeClass selectors`                                   | an `Array` of all instance method selectors of this class                                                    |
| `SomeClass class`                                       | the `Metaclass` subclass of this class                                                                       |
| `SomeClass class selectors`                             | an `Array` of all class method selectors of this class                                                       |
| `SomeClass class allMethodsInCategory: 'some-category'` | an `Array` of class methods in a given category, including those defined in this class and inherited         |
| `CodeListPackages installedPackages`                    | an `Array` of `CodePackage` objects (appear in System Browser class category pane)                           |
| `thisContext sender`                                    | a `MethodContext` describing the method that invoked the current one                                         |
| `someMethodContext receiver`                            | the object that sent the message that invoked the `MethodContext`                                            |
| `someMethodContext selector`                            | the selector `Symbol` of the message that invoked the `MethodContext`                                        |
| `someSelector keywords`                                 | an `Array` of the keywords in the selector                                                                   |

The following examples demonstrate getting keywords from a message selector:

```smalltalk
#foo keywords. "#('foo')"
#foo:bar:baz: keywords. "#('foo:' 'bar:' 'baz:')"
```

TODO: Why does `allClassVarNames` return a `Set` when `allInstVarNames` returns an `Array`?
TODO: Is there a way to get all the message categories used by a class?

To run code on every instance of a given class,
send the `allInstancesDo:` message to the class.

For example, to delete all instances of a given class, run
`SomeClass allInstancesDo: [:obj | obj delete]`.

This isn't a provided method that finds the nearest class in the
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

The class `Class` inherits from `ClassDescription`
which inherits from `Behavior`.
The `Behavior` class implements the method `sourceCodeAt:`
which returns a `UnicodeString` containing the source code
for the method with a given selector `Symbol`.
For example, the following code gets a string of source code
from the `Dictionary` `#at:put:` method:

```smalltalk
code := Dictionary sourceCodeAt: #at:put:.
```

The `Parser` class can parse a string of source code
that describes an existing method in a given class.
It returns a `MethodNode` object describing the method.
For example, the following code parses the code from the previous example:

```smalltalk
parser := Parser new.
methodNode := parser parse: code class: Dictionary.
```

The following code gets the names of the arguments that follow the keywords:

```smalltalk
argNodes := methodNode arguments.
argNames := argNodes collect: [:node | node name].
```
