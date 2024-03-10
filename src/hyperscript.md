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

## Using

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
