---
eleventyNavigation:
  key: Syntax
  order: 1.2
  parent: Smalltalk
layout: topic-layout.njk
---

Before diving into the functionality provided
by the Smalltalk development environment,
it's important to understand the syntax of the Smalltalk programming language.

## Summary

The following table summarizes all the syntax.

| Item                                              | Example                                                    |
| ------------------------------------------------- | ---------------------------------------------------------- |
| comment                                           | `"some text"`                                              |
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
| block with no arguments                           | `[ <expressions> ]`                                        |
| block with arguments                              | `[:a :b \| a + b]`                                         |
| unary message send                                | `<object> <message>` such as `5 factorial`                 |
| binary message send (look like operators)         | `<object> <message> <argument>` such as `4 * 5`            |
| keyword message send                              | `2 raisedTo: 4 modulo: 3`                                  |
| message cascade - sends to initial receiver       | `Transcript show: 'foo'; newLine; show: 'bar'`             |
| message chaining - sends to previous result       | `2 * 3 :: squared :: negated` (-36)                        |
| method return value                               | `^<expression>` such as ^42                                |
| expression separator (period)                     | `'foo print'. 'bar' print`                                 |
| parentheses to control evaluation order           | `a * (b + c)`                                              |
| reference to current object in a method           | `self`                                                     |

The caret (^) in a return expression can be followed by a space.
The pretty printer includes a space, but many developers prefer to omit it.

Expressions in a method are separated by periods.

In static arrays the elements are separated by spaces.  
In dynamic arrays the expressions are separated by periods.

## Naming Conventions

Names of classes, methods, and variables use camelCase.
They consist of letters and digits.
They start with a letter that is uppercase for classes and class variables,
and lowercase for all other names.
The only special character allowed is the underscore character,
but that is rarely used because camelCase is preferred.

## Special Characters

The assignment operator (`:=`) can be rendered as a left pointing arrow.
The return operator (`^`) which is used in methods to return a value
can be rendered as an upward pointing arrow.

To enable those renderings, open the World menu and
select Preferences...Show ST-80 Assignments.
The next time code is modified, all the `:=` and `^` operators
will be rendered as arrows.

To disable these renderings, open the World menu and
select Preferences...Show ANSI Assignments.

Typing an underscore is a shorthand way to type `:=` for variable assignments.
Assignment underscores are changed to left pointing arrows
regardless of whether "Show ST-80 Assigments" is selected.
