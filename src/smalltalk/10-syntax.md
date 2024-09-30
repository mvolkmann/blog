---
eleventyNavigation:
  key: Syntax
  order: 10
  parent: Smalltalk
layout: topic-layout.njk
---

Before diving into the functionality provided
by the Smalltalk development environment,
it's important to understand the syntax of the Smalltalk programming language.

## Special Tokens

The assignment operator (`:=`), pronounced "gets",
can be rendered as a left pointing arrow.
The return operator (`^`) which is used in methods to return a value
can be rendered as an upward pointing arrow.

To enable those renderings, open the World menu and
select Preferences...Show ST-80 Assignments.
The next time code is modified, all the `:=` and `^` operators
will be rendered as arrows.

To disable these renderings, open the World menu and
select Preferences...Show ANSI Assignments.

Typing an underscore is a shorthand way to type `:=` for variable assignments.
Assignment underscores are changed to `:=` and rendered as left pointing arrows
regardless of whether "Show ST-80 Assigments" is selected.

The following table summarizes all the special tokens.

| Token | Meaning                                                        |
| ----- | -------------------------------------------------------------- |
| `'`   | delimits a `String`                                            |
| `#`   | begins a `Symbol`                                              |
| `:`   | ends a keyword in a keyword message                            |
| `.`   | separates statements                                           |
| `\|`  | delimits local variables in a method or block                  |
| `"`   | delimits a comment                                             |
| `(`   | begins a subexpression                                         |
| `)`   | ends a subexpression                                           |
| `[`   | begins a block                                                 |
| `]`   | ends a block                                                   |
| `^`   | returns an object from a method                                |
| `;`   | cascades statements to send multiple messages to same receiver |
| `::`  | chains messages to remove need for parentheses                 |
| `:=`  | assigns result of expression on right to variable on left      |

## Syntax Summary

The following table summarizes all the syntax.
The term "receiver" describes the object to which a message is sent.

| Item                                              | Example                                                    |
| ------------------------------------------------- | ---------------------------------------------------------- |
| comment                                           | `"some text"` (can span multiple lines)                    |
| temporary (local) variable with private scope     | `myTemp` (camelCase)                                       |
| global variable with shared scope                 | `MyGlobal` (CamelCase)                                     |
| pseudo variable (cannot assign)                   | `self`, `super`, `nil`, `true`, `false`, and `thisContext` |
| integer                                           | `123`                                                      |
| float                                             | `3.14`                                                     |
| exponential notation                              | `1.23e4`                                                   |
| character                                         | `$a`                                                       |
| string                                            | `'text'` (use double ' to include)                         |
| string concatenation (comma message)              | `'foo', 'bar', 'baz'`                                      |
| symbol (globally unique string)                   | `#name`                                                    |
| static array (elements are literal values)        | `#(1 4 8)`                                                 |
| dynamic array (elements are computed at run time) | `{1. 2 * 2. 2 raisedTo: 3}`                                |
| array concatenation (comma message)               | `#(1 2), #(3 4)`                                           |
| assignment                                        | `<variable> := <expression>`                               |
| method and block variable declarations            | `\| foo bar baz \|`                                        |
| block with no arguments                           | `[<expressions>]`                                          |
| block with arguments                              | `[:a :b \| a + b]`                                         |
| unary message send                                | `<object> <message>` such as `5 factorial`                 |
| binary message send (look like operators)         | `<object> <message> <argument>` such as `4 * 5`            |
| keyword message send                              | `2 raisedTo: 4 modulo: 3`                                  |
| message cascade - sends to initial receiver       | `Transcript show: 'foo'; newLine; show: 'bar'`             |
| message chaining - sends to previous result       | `2 * 3 :: squared` (36)                                    |
| method return value                               | `^<expression>` such as ^42                                |
| expression separator (period)                     | `'foo print'. 'bar' print`                                 |
| parentheses to control evaluation order           | `a * (b + c)`                                              |
| reference to current object in a method           | `self`                                                     |
| compound literal evaluated by compiler            | `` `expression` ``                                         |

There is no syntax for describing a class as a whole.
Instead, a class is described by sending a message to its superclass
and each class/instance method is described separately.

Whitespace in Smalltalk code is insignificant.
Spaces, tabs, and newline characters can be freely inserted.

There is no syntax for defining constants.
A common way to provide a constant value is
to define a class method that return it.
For example, `pi` is a class method in the `Float` class.

An expression can have one of the following forms:

- a pseudo-variable `true`, `false`, or `nil`
- a literal value such as `3`, `'Hello'`, or `#(1 2 3)`
- a message send such as `5 factorial`, `a + b`, or `colors at: 2`
- a block such as `[:a :b | a + b]`

The same value can be assigned to multiple variables.
For example: `score1 := score2 := 0`.

The caret (^) in a return expression can be followed by a space.
The pretty printer includes a space, but many developers prefer to omit it.

Expressions in a method are separated by periods.

In static arrays the elements are separated by spaces.  
In dynamic arrays the expressions are separated by periods.

Compound literals evaluate their expression at compile-time
instead of run-time for better run-time performance.

The "cascade" delimiter enables sending multiple messages
to the same receiver object.
For example, `Transcript show: 'foo'; newLine; show: 'bar'`.
The return value is that of the final message.
To instead cause the return value to be the receiver object,
end the cascade with `; yourself`.

The "chain" delimiter `::` is a Cuis-specific extension to Smalltalk-80 syntax.
It can remove the need to surround the preceding expression with parentheses.
It sends a message to the result of the previous message send.
It is useful for sending a unary message to
the result of a binary or keyword message OR
for sending a binary message to the result of a keyword message.
For example:

```smalltalk
1 + 2 squared. "5"
(1 + 2) squared. "9"
1 + 2 :: squared. "9"

15 rem: 4 squared. "15"
(15 rem: 4) squared. "9"
15 rem: 4 :: squared. "9"

(15 rem: 4) * 2. "6"
15 rem: 4 :: * 2. "6"
```

## Naming Conventions

Names of classes, methods, and variables use camelCase.
They consist of letters and digits.
They start with a letter that is uppercase for classes and class variables,
and lowercase for all other names.
The only special character allowed is the underscore character,
but that is rarely used because camelCase is preferred.
