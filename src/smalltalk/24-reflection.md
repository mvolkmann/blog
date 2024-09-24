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

TODO: Why does `allClassVarNames` return a `Set` when `allInstVarNames` returns an `Array`?
TODO: Is there a way to get all the message categories used by a class?

To run code on every instance of a given class,
send the `allInstancesDo:` message to the class.

For example, to delete all instances of a given class, run
`SomeClass allInstancesDo: [:obj | obj delete]`.
