---
eleventyNavigation:
  key: Classes
  order: 15
  parent: Smalltalk
layout: topic-layout.njk
---

A class defines a set of associated class variables, instance variables,
class methods, and instance methods.

A class is defined by sending the message
`#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:`
to a superclass which can be `Object` or any other class.
This message is handled by an instance method of the class `Class`.

The `subclass:` keyword takes a symbol.
The remaining keywords all take strings.
All the keywords must be supplied, even if their value is an empty string.
The following is an example class definition.

```smalltalk
Object subclass: #Dog
    instanceVariableNames: 'breed name'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Programming languages use many terms to describe data
that is encapsulated by objects created from a class.
Examples include "attribute", "property", and "field".
Smalltalk calls these "instance variables".

Instance variables are described by a single `String`
that is the value of the `instanceVariableNames:` argument.
The names are separated by spaces and must begin lowercase.
Their initial value is `nil`.

Class variables are described by a single `String`
that is the value of the `classVariableNames:` argument.
The names are separated by spaces and must begin uppercase.
Their initial value is `nil`.

Each class is a associated with a category.
Classes provided by the image are in categories such as
"Collections", "Kernel", "Morphic", "System", "Tools", "UI", and many more.
If a new class definition is saved with an empty string category,
it will be changed to 'as yet unclassified'.

Classes in the "Kernel" category are considered to be fundamental classes.
Examples include `Date`, `BlockClosure`, `Integer`, `Object`, and `String`.

Pool dictionaries enable sharing data between related classes.
They reside in the `Smalltalk` dictionary.
To create a pool dictionary named "MyPool",
enter the following in a Workspace and "Do it":
`Smalltalk at: #MyPool put: (Dictionary new)`.
Then refer to it from any number of classes with `poolDictionaries: 'MyPool'`.
It is common for a class not have any pool dictionaries.

All classes are global and there is no namespacing.
Class names are added to the global variable `Smalltalk`
which is a `SystemDictionary`.
This requires all class names to be unique.
Typically a common prefix, perhaps 2 or 3 uppercase letters,
is added to a set of related class names in order to make the unique.
Lack of namespacing is seen by some as a weakness of Smalltalk.

All classes inherit from one other class,
except `ProtoObject` which is the highest superclass of all classes.

To get the superclass of a class, send it the `#superclass` method.
For example, `Integer superclass` returns `Number`.

Instance variables can only be directly accessed by methods in the same class.
To expose them outside the class, add getter and setter (optional) methods.
For example, if `score` is an instance variable
then the following is a getter method for it.
By convention, the name of getter and setter methods is the same as
the name of the associated instance variable, but this is not required.
Setter method names end in a colon because they take an argument.

```smalltalk
score
    ^score

score: aNumber
    score := aNumber
```

As shown above, another convention is for variables associated with
keyword messages to indicate their expected type.

Setter methods do not explicitly return a value
and instead implicitly return `self`.
This enables chaining setter messages.
For example, an instance of a `Dog` class
with setters for its `name` and `breed` instance variables
can be created with the following:

```smalltalk
dog := Dog new name: 'Comet' :: breed: 'Whippet'
```

To find a class without needing to know its class category:

- Press shift-return OR
  hover over the top, first column in a System Browser and press cmd-f.
- Enter part of the class name.
- Select a class name from the popup list that appears.
- If shift-return was pressed, a new System Browser
  will open to display the selected class.
  If cmd-f was pressed in an existing System Browser,
  the selected class will be displayed there.
