---
eleventyNavigation:
  key: Hyperscript
layout: topic-layout.njk
---

<figure style="width: 60%">
  <img alt="_hyperscript logo" style="border: 0"
    src="/blog/assets/hyperscript-logo.png?v={{pkg.version}}">
</figure>

## Foreword

This page is incomplete.

I'm giving up on \_hyperscript for now.
The language feels a bit too hard to write because it is
significantly different from most programming languages, including JavaScript.
I had difficulty getting some basic examples to work.
My questions on the htmx Discord channel went unanswered.

## Overview

{% aTargetBlank "https://hyperscript.org", "_hyperscript" %} is
small programming language that can be used in HTML files
to implement interactive features such as event handling.
It also supports asynchronous operations such as
fetching data from a server by sending an HTTP request.

\_hyperscript is based on the
{% aTargetBlank "https://en.wikipedia.org/wiki/HyperTalk", "HyperTalk" %}
language which was used in Apple's
{% aTargetBlank "https://en.wikipedia.org/wiki/HyperCard", "HyperCard" %}.
From Wikipedia, HyperCard "is among the first successful hypermedia systems
predating the World Wide Web."

Like HyperTalk, \_hyperscript uses an English-like syntax.
It emphasizes readability, but may feel more difficult to write at first
because the syntax is quite different from typical programming languages.

\_hyperscript is similar to {% aTargetBlank "https://alpinejs.dev", "Alpine" %}
and {% aTargetBlank "https://htmx.org", "HTMX" %}
in that they add attributes to HTML.
But hyperscript only adds one attribute, the underscore.

Placing code on HTML elements favors locality of behavior
over separation of concerns much like
{% aTargetBlank "https://tailwindcss.com", "Tailwind" %}, Alpine, and HTMX.

\_hyperscript was created by Carson Gross who also created
{% aTargetBlank "https://htmx.org", "HTMX" %}.
As of March 2024 it had not yet reached version 1.0.

## Installing

To use \_hyperscript, just including the following `script` tag
in each HTML page that needs it.

```html
<script src="https://unpkg.com/hyperscript.org@0.9.12"></script>
```

Version 0.9.12 was released in October 2023.

\_hyperscript cannot be installed from npm.

## Using \_hyperscript

To use \_hyperscript, add an attribute to HTML elements
whose name is an underscore.
The names `script` and `data-script` can also be used.
For example:

```html
<html lang="en">
  <head>
    <script src="https://unpkg.com/hyperscript.org@0.9.12"></script>
  </head>
  <body>
    <button _="on click alert('got click')">Click Me</button>
  </body>
</html>
```

## Using VS Code

The {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=dz4k.vscode-hyperscript-org",
"_hyperscript" %} extension from dz4k provides
syntax highlighting of \_hyperscript code embedded in HTML files
and in standalone files.
Unfortunately, it often messes up the syntax highlighting of HTML that follows.
This issue was reported in April 2022, so it seems it will not be fixed.

## Underscore Attribute

The value of the `_` attribute is a string of \_hyperscript commands.
It must begin with stating when it should run.
Typically this is the `on` keyword followed by an event name.
It can also be `init` to state that it should run
when the associated element is initialized.

## Commands

\_hyperscript is a programming language
with its own set of commands and keywords.

Each \_hyperscript command is described in the following table.

| Command      | Description                                                                                            |
| ------------ | ------------------------------------------------------------------------------------------------------ |
| `add`        | adds an attribute, CSS class, or CSS property to an element                                            |
| `append`     | appends a string to another string, a value to an array, or element to another element                 |
| `async`      | executes a command or block of commmands asynchronously                                                |
| `beep!`      | prints the source, result, and type of an expression to the console                                    |
| `break`      | exits a `repeat` loop                                                                                  |
| `call`       | evaluates an expression and places the result in the `it` variable                                     |
| `continue`   | continues to the next iteration of a `repeat` loop                                                     |
| `decrement`  | decrements a variable, property, or attribute (see `by` keyword)                                       |
| `default`    | sets the default value of a variable or property                                                       |
| `exit`       | exits a function or event handler without returning a value                                            |
| `fetch`      | fetches text, JSON, HTML, or raw data from an HTTP endpoint and places the result in the `it` variable |
| `for`        | iterates over items in an expression; specifies target of `take`                                       |
| `from`       | specifies source of `take`                                                                             |
| `get`        | an alias for `call`, used when it reads better                                                         |
| `go`         | navigates to a URL, back to the previous page, or scrolls an element into view                         |
| `halt`       | prevents an event from bubbling                                                                        |
| `hide`       | hides an element by changing its CSS `display`, `visibility`, or `opacity` property                    |
| `if`         | provides conditional control flow                                                                      |
| `increment`  | increments a variable, property, or attribute (see `by` keyword)                                       |
| `js`         | embeds JavaScript code; terminated by `end`                                                            |
| `log`        | writes using `console.log` unless another variant is specified after `with` keyword                    |
| `make`       | creates an instance of a DOM class (an element)                                                        |
| `measure`    | gets measurements from an element                                                                      |
| `on`         | specifies events (separated by `or`) that trigger the commands that follow                             |
| `pick`       | gets array elements using the `slice` method                                                           |
| `put`        | inserts content into a variable, property, or the DOM                                                  |
| `remove`     | removes an element from the DOM or a class/property from an element                                    |
| `render`     | clones a `template` element and populates it; result goes in `result` and `it`                         |
| `repeat`     | iterates over items in an expression, a number of times, or `forever`                                  |
| `return`     | returns a value from a function or exits from an event handler                                         |
| `send`       | sends an event to a target element                                                                     |
| `set`        | sets a variable or element properties                                                                  |
| `settle`     | synchronizes on a CSS transition of an element                                                         |
| `show`       | shows an element by changing its CSS `display`, `visibility`, or `opacity` property                    |
| `take`       | removes a class or attribute from elements and adds it to another element                              |
| `tell`       | temporarily changes the default target for a command                                                   |
| `throw`      | throws an exception                                                                                    |
| `toggle`     | toggles CSS classes, an attribute, or the visibility of an element                                     |
| `transition` | transitions properties on an element from one value to another                                         |
| `trigger`    | alias for `send`                                                                                       |
| `wait`       | blocks until an event occurs or for a given amount of time                                             |

## Keywords

Each \_hyperscript keyword is described in the following table.
Many of the keywords are used in "pseudo-commands"
that treat an object method as a top-level command.

| Keyword      | Description                                                                        |
| ------------ | ---------------------------------------------------------------------------------- |
| `and`        | used in logical expressions                                                        |
| `at`         | used in pseudo-commands                                                            |
| `back`       | modifier for the `go` command                                                      |
| `bottom`     | indicates a relative position                                                      |
| `by`         | modifier for the `decrement` and `increment` commands which default to `1`         |
| `center`     | indicates a relative position                                                      |
| `character`  | specifies getting a single character with the `pick` command                       |
| `characters` | specifies getting multiple characters with the `pick` command                      |
| `do`         | begins a block of commmands                                                        |
| `else`       | optionally used with `if`                                                          |
| `empty`      | comparison value                                                                   |
| `end`        | ends a block of commmands                                                          |
| `for`        | used in `repeat` commands                                                          |
| `forever`    | used in `repeat` commands                                                          |
| `from`       | used in pseudo-commands                                                            |
| `in`         | used in `repeat` commands                                                          |
| `into`       | used in pseudo-commands                                                            |
| `is`         | comparison operator                                                                |
| `item`       | specifies the kind of result to get with the `pick` command                        |
| `items`      | specifies the kind of result to get with the `pick` command                        |
| `its`        | possessive that refers to another element                                          |
| `left`       | indicates a relative position                                                      |
| `match`      | specifies getting a regular expression match with the `pick` command               |
| `matches`    | specifies getting a regular expression matches with the `pick` command             |
| `me`         | possessive that refers to the current element                                      |
| `middle`     | indicates a relative position                                                      |
| `my`         | alias for `me`                                                                     |
| `next`       | finds the next element of a given type                                             |
| `not`        | used in logical expressions                                                        |
| `of`         | makes commands more readable                                                       |
| `on`         | used in pseudo-commands                                                            |
| `or`         | used in logical expressions                                                        |
| `otherwise`  | alias for `else`; used with `if`                                                   |
| `previous`   | finds the previous element of a given type                                         |
| `right`      | indicates a relative position                                                      |
| `the`        | makes commands more readable                                                       |
| `then`       | separates multiple commands; optionally used with `if`                             |
| `times`      | indicates the number of times a `repeat` block will execute                        |
| `to`         | used with `append`, `go`, and pseudo-commands                                      |
| `top`        | indicates a relative position                                                      |
| `until`      | used in `repeat` commands                                                          |
| `while`      | used in `repeat` commands                                                          |
| `with`       | specifies the `console` method that `log` should use; also used in pseudo-commands |

## Syntax

Single-line comments begin with `--` (preferred) or `//`.
Multi-line comments are delimited by `/*` and `*/`.

## Terminology

- script: composed of features

- feature: a series of commands

  One kind of feature is an event handler.

- command: a series of expressions

  Multiple commands can be separated by
  the `then` keyword or a newline character.

  Commands with bodies, such as `if` and `on`
  are terminated by the `end` keyword,
  but that is not required if the script ends or another feature starts.
  In practice the `end` keyword is rarely used.

- expressions: can include three kinds of values

  - numbers
  - strings in single or double quotes
  - array literals with the syntax `[value1, value2, ...]`
  - element id references: `#some-id`
  - CSS class references: `.some-class`
  - attribute references: `@some-attribute`
  - query references: `<somename />`

- variables

  To set a variable, use `set {name} to {value}`.
  The `put` keyword can be used in place of `set`, but `set` is preferred.

  There are three variable scopes:

  - `local`: scoped to a feature within a script
  - `element`: scoped to the element associated with the script
  - `global`: available to all scripts

  To specify the scope of a variable,
  precede its name with one of these keywords and space.
  Alternative, precede the name with
  a `$` for global scope or a `:` for element scope.
  If no scope is specified, the variable defaults to `local` scope.

## Errors

When syntax errors are encountered,
error messages are written to the DevTools Console.
They are quite descriptive.

## Logging

To write to the DevTools Console, use `log {value}`

## Comments

Single-line comments begin with `--`
followed by at least one whitespace character.

For now, `//` for single-line comments and `/* ... */` for multi-line comments
are also supported, but those may not be supported in the future.

## Attributes

To get the value of an attribute on the current element,
use `@attr-name`. The value will always be a string.

To set the value of an attribute on the current element,
use `set @attr-name to {value}`.
If the value is not a string, it will be converted to a string.

## Strings

Literal strings are delimited with single or double quotes.
Strings can be concatenated with the `+` operator.
Interpolation is performed in the same way as in JavaScript.
For example:

```text
set fullName to `${firstName} ${lastName}`
```

## Arrays

The following code demonstrates operating on arrays.

```text
set scores to [7, 19, 12]
log the first of scores -- 7
log the last of scores -- 12
log random in scores -- one of the values
```

## Objects

Objects are created with the same literal syntax as in JavaScript.
For example:

```text
set dog to {name: "Comet", breed: "Whippet"}
log `${dog.name} is a ${dog.breed}`
```

There are multiple ways to access a property of an object.

- `dog.name`
- `dog['name']`
- `dog's name`
- `the name of dog`

## DOM Access

Possessive expressions can be used to get and set DOM content.
For example:

```text
get the first <li/> then
set my innerHTML to its innerHTML
```

## Operators

### Comparison Operators

\_hyperscript supports all the comparison operators in JavaScript.
It also supports many comparison keywords.

- `x is y` is the same as `x == y`
- `x is not y` is the same as `x != y`
- `no x` and `x exists` are the same as
  `x == null`, `x == undefined`, and `[[x.length]] == 0`?
- `x is less then y` is the same as `x < y`
- `x is greater then y` is the same as `x > y`

Other operators with no direct comparison to JavaScript include the following:

- `{collection} is empty`
- `{element} matches {selector}`

### Math Operators

\_hyperscript supports most JavaScript math operators.
It does not support the `%` operator for modulo,
but the `mod` keyword can be used instead.

\_hyperscript evaluates all operators from left to right
and doesn't apply operator precedence.
Parentheses must be used when an expression uses multiple operators.

The `increment` and `decrement` keywords modify the value of a number
as their name implies.

## Conversions

To convert a string to a number, follow it with `as an Int`.

## Conditional Logic

```text
if {condition}
  ...
end
```

## Iteration

\_hyperscript supports many kinds of loops.
The `break` and `continue` keywords can be used in all of them.

```text
for {var} in {collection}
  ...
end

for {var} in {collection} index {i}
  ...
end

repeat forever
  ...
end

repeat in {collection} -- sets the "it" variable
  ...
end

repeat while {condition}
  ...
end

repeat until {condition}
  ...
end

repeat {n} times
  ...
end
```

Some commands automatically operate on all items in a collection.
For example, `add .some-class to <li/>`
adds the CSS class `some-class` to all `li` elements.

## Resources

- {% aTargetBlank "https://hyperscript.org", "_hyperscript home page" %}
- {% aTargetBlank "https://hyperscript.org/docs/", "_hyperscript Documentation" %}
- {% aTargetBlank "https://hyperscript.org/cookbook/", "_hyperscript Cookbook" %}
- {% aTargetBlank "https://hyperscript.org/reference/", "_hyperscript Reference" %}
- {% aTargetBlank "", "" %}

## TODO

Organize this content:

can combine with tailwind in order to use provided classes like hidden.
how good are the error messages if there is something wrong in the syntax?
Do error messages appear in the Dev tools console?
Do error messages appear on page load or only when hyperscript is triggered?
see the control flow section in the docs for conditional logic and iteration.
maybe Alpine is better for this because it allows using JavaScript syntax.
The then keyword allows specifying a sequence of hyperscript statements.
what is the difference between if else and if otherwise?

on click toggle .some-class on selector
click is an event.
toggle is an action

List all the supported events. click, load, …
List all the supported keywords and describe each one. toggle, transition, on, my, over to, seconds, then, …
is there a keyword that runs two things at once unlike the then keyword that runs them serially?
selectors can use the keyword me.

make sure you understand what the defer attribute does on a script tag.
resetting a form can be done in a single attribute with hyper script, but requires three things with Alpine, x-data, x-on, and an id on the form.
