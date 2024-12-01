---
eleventyNavigation:
  key: Parsing Code
  order: 24
  parent: Smalltalk
layout: topic-layout.njk
---

The `Compiler` class can compile strings of Smalltalk code.
For example, `Compiler evaluate: '1+2'` answers `3`.

The class `Class` inherits from `ClassDescription`
which inherits from `Behavior`.
The `Behavior` class implements the method `sourceCodeAt:`
which returns a `UnicodeString` containing the source code
for the method with a given selector `Symbol`.
The following code gets a string of source code
from the `Dictionary` `#at:put:` method:

```smalltalk
code := Dictionary sourceCodeAt: #at:put:.
```

The `code` variable will hold the following in a single string:

```smalltalk
at: key put: anObject
    "Set the value at key to be anObject.
    If key is not found, create a new entry for key and set is value to anObject.
    If key is found, update the existing association.
    Answer anObject."

    | index assoc |
    index := self findElementOrNil: key.
    assoc := array at: index.
    assoc
        ifNil: [ self atNewIndex: index put: (Association key: key value: anObject) ]
        ifNotNil: [ assoc value: anObject ].
    ^ anObject
```

The `Parser` class parses a string of source code
that describes an existing method in a given class.
TODO: Is it required to be code from an existing method?
It returns a `MethodNode` object that is the
root of the parse tree (a.k.a abstract syntax tree).
The following code parses the code from the previous example:

```smalltalk
parser := Parser new.
methodNode := parser parse: code class: Dictionary.
```

The following code gets the names of the arguments that follow the keywords:

```smalltalk
argNodes := methodNode arguments.
argNames := argNodes collect: [:node | node name].
```

`MethodNode` objects include the following instance varaibles and more:

- `block` - a `BlockNode` object (described below)
- `comment` - an `OrderedCollection` of `UnicodeString` objects
- `properties` - an `AdditionalMethodState` object (has `selector` property)
- `sourceText` - a `UnicodeString` containing all the code for the method
- `temporaries` - an `OrderedCollection` of `TempVariableNode` objects

`BlockNode` objects include the following instance varaibles:

- `arguments` - an `Array` of ?
- `returns` - a `Boolean` that indicates whether the method explicitly returns a value?
- `statements` - an `OrderedCollection` of nodes

The class `ParseNode` is the superclass of all nodes produced by the `Parser`.
This class is provided in Cuis and Squeak Smalltalk, but not in Pharo.
It provides the child `comment` to all its subclasses.

They include:

- `AssignmentNode` - assignment to a variable

  The children are `variable` and `value`.

- `BacktickNode` - compound literal evaluated by compiler (surrounded by backticks)

  The only child is `expression`.

  This class is specific to Cuis Smalltalk.

- `BraceNode` - dynamic array with the syntax `{expr1. expr2. expr3}`

  The children are in `elements`.

- `CascadeNode` - cascade of messages using `;`

  The children are in `receiver` and `messages`.

- `CodeNode` - abstract class that provides shared methods to subclasses

  This class is specific to Cuis Smalltalk,
  but it's subclasses `BlockNode` and `MethodNode`
  are also provided in Squeak Smalltalk.

  - `BlockNode` - block containing expressions to be evaluated later

    The children are `arguments` and `statements`.

  - `MethodNode` - method definition

    The children are `temporaries`, `arguments`, and `block`.

- `LeafNode` - abstract class that provides shared methods to subclasses

  - `LiteralNode` - literal number or string

  - `SelectorNode` - message selector

    The `key` property holds the selector `String`.

    - `SpecialSelectorNode` - special message selectors that have their own bytecode for efficiency

  - `VariableNode` - variable of any kind

    This has a `name` instance varaible.

    - `InstanceVariableNode` - instance variable

      - `MaybeContextInstanceVariableNode` - performance optimization

    - `LiteralVariableNode` - constants and global identifiers

      These include class names, global variables, and pool dictionary entries.

    - `TempVariableNode` - temporary (local) variable

      - `RemoteTempVariableNode` - collection of temporary variables shared between closures

    - `UndeclaredVariableNode` - undeclared variable, perhaps used in a "Do it"

  - `MessageNode` - message sent to a receiver with optional arguments

    The children are `receiver`, `selector`, and `arguments`.

    The `precedence` instance variable is set to 1 for unary messages,
    2 for binary messages, and 3 for keyword messages.

    - `MessageAsTempNode`

- `NewArrayNode` - created by the `genPushNewArray:` opcode optimization

- `ReturnNode` - caret return of a specified value or implicit return of `self`

  This has an `expr` instance variable.

- `TemporariesDeclarationNode` - temporary variable declarations (plural) found at the start of a method or block

  This class is specific to Cuis Smalltalk.

- `TemporaryDeclarationNode` - temporary variable declaration (singular) found at the start of a method or block

  This class is specific to Cuis Smalltalk.

Squeak Smalltalk adds the `ParseNode` subclasses `CommentNode`, `FieldNode`,
and `FutureNode` which are not present in Cuis Smalltalk.

For an example of parsing Smalltalk code and
printing a representation of the parse tree, see the GitHub repository
<a href="https://github.com/mvolkmann/Cuis-Smalltalk-CodeParse"
target="_blank">CodeParse</a>.
