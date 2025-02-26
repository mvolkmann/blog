---
eleventyNavigation:
  key: Nearley
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

{% aTargetBlank "https://nearley.js.org", "nearley" %}
is a parsing toolkit with many features including:

- Handles all BNF grammars, including those with left recursion.
- Can produce abstract syntax trees, text output, or simply validate input.
- Provides a testing tool (`nearley-test`).
- Provides a railroad diagram generator that creates HTML files
  that include SVG-based diagrams (`nearley-railroad`).
- Works with many lexers including
  {% aTargetBlank "https://github.com/no-context/moo", "Moo" %}.

The nearley library:

- can be used in both server-side and browser JavaScript code.
- uses the Earley algorithm
- implements right recursion optimizations from Joop Leo
- can produce random strings that match a given grammar
- has editor plug-ins that provide syntax highlighting
  for VS Code (from Pouya Kary), Sublime Text, and Vim
- has been maintained by volunteers since 2014

## Installing

To install nearley global so its tools can be used from the command line,
enter `npm install -g nearley`.

To install nearley in a node project, enter `npm install nearley`.

Installing nearley also installs the Moo lexer library.

## Builtins

Nearley provides files that define commonly used grammar rules and functions.
These are found at https://github.com/kach/nearley/tree/master/builtin.

`number.ne` defines the grammar rules:

- `unsigned_int` matches zero or positive integers
- `int` matches any integer
- `unsigned_decimal` matches zero or positive floating point numbers
- `decimal` matches any floating point number
- `percentage` matches a decimal followed by %
- `jsonfloat` matches same as decimal, but adds scientific notation matching

`postprocessors.ne` defines the following functions that are used
inside the postprocesssor code associated with grammar rules:

- `nth` returns the nth element from a data array
- `$` ???
- `delimited` ???

`string.ne` defines the grammar rules:

- `dqstring` matches strings delimited by double quotes
- `sqstring` matches strings delimited by single quotes
- `btquote` matches strings delimited by backticks

`whitespace.ne` defines the grammar rules:

- `_` matches zero or more whitespace characters.
- `__` matches one or more whitespace characters.

To include these files in `.ne` grammar file, use the `@builtin` directive.
For example, `@builtin "whitespace.ne"`.

## Creating a Grammar

Grammars are defined in text files with a `.ne` file extension.
The first grammar rule defines the starting point.
The remaining rules can appear in any order, including alphabetical.

The following is a fairly simple grammar defined in the file `arithmetic.ne`.
It defines rules for arithmetic expressions
that use the following operators:

- `+` addition
- `-` subtraction
- `*` multiplication
- '/' division

This grammar supports standard operator precedence
and using parentheses to override that.

```js
@builtin "number.ne" # using decimal rule
@builtin "whitespace.ne" # using _ rule

start -> additive

additive
  -> multiplicative _ "+" _ additive
   | multiplicative _ "-" _ additive
   | multiplicative

multiplicative
  -> term _ "*" _ multiplicative
   | term _ "/" _ multiplicative
   | term

term
  -> decimal
   | "(" additive ")"
```

## Compiling a Grammar

To compile a grammar to JavaScript code, use the `nearleyc` command.
For example:

```bash
nearlyc arithmetic.ne -o arithmetic.js
```

## Testing a Grammar

To test a grammar with specific input,
use the `nearley-test` command.
For example:

```bash
nearly-test arithmetic.js -i '2 * 3 + (5 + 1) / 2 - 4'",
```

This outputs the following nested array which represents the parse tree.
For this example, each occurrence of `null`
represents whitespace which is not being captured.

```text
[
  [
    [
      [
        [ 2 ], null, "*", null, [
          [ 3 ]
        ]
      ], null, "+", null, [
        [
          [ "(", [
              [
                [ 5 ]
              ], null, "+", null, [
                [
                  [ 1 ]
                ]
              ]
            ], ")" ], null, "/", null, [
            [ 2 ]
          ]
        ], null, "-", null, [
          [
            [ 4 ]
          ]
        ]
      ]
    ]
  ]
]
```

This output is not particularly helpful.
It does demonstrate that our grammar is correct.

Here is an example of supplying input that does not match the grammar.

```bash
nearley-test arithmetic-default.js -i '1 + two'
```

And here is the output with some parts elided because it is quite long.

```text
/usr/local/lib/node_modules/nearley/lib/nearley.js:346
                throw err;
                ^
Error: Syntax error at line 1 col 5:

1 1 + two
      ^
Unexpected "t". Instead, I was expecting to see one of the following:

A character matching /[ \t\n\v\f]/ based on:
    ...
A "(" based on:
    ...
A "-" based on:
    ...
A character matching /[0-9]/ based on:
    ...
  offset: 4,
  token: { value: 't' }
```

## Postprocessors

Each rule can be followed by JavaScript code
that is executed when the rule is matched.
The code must be delimited by `{%` and `%}`.

It must contain the name of a predefined function or
a function definition (typically written as an arrow function).

The function is passed three values:

- `data` - array containing the parsed result for each matching token
- `location` - zero-based index into the input string where the match began
- `reject` - object that can be returned to indicate that the rule should not match

Typically only the first argument, `data` is used
and often the name is shortened to just `d`.

## Postprocessor Rules for Evaluating

Let's add postprocessing the previous grammar so that it
evaluates each rule to a number.
The value of the starting rule will be the value of the entire input expression.

```js
@builtin "number.ne" # using decimal rule
@builtin "whitespace.ne" # using _ rule

start -> additive {% id %}

additive
  -> multiplicative _ "+" _ additive {% d => d[0] + d[4] %}
   | multiplicative _ "-" _ additive {% d => d[0] - d[4] %}
   | multiplicative {% id %}

multiplicative
  -> term _ "*" _ multiplicative {% d => d[0] * d[4] %}
   | term _ "/" _ multiplicative {% d => d[0] / d[4] %}
   | term {% id %}

term
  -> decimal {% id %}
   | "(" additive ")" {% d => Number(d[1]) %}
```

Running the following command:

```bash
nearly-test arithmetic.js -i '2 * 3 + (5 + 1) / 2 - 4'",
```

outputs the expected value in an array which is `[ 5 ]`.

## Postprocessor Rules for AST Building

Let's modify the postprocessing so the result is an abstract syntax tree (AST).
This can be useful for compiling one syntax into another.
For example, we could parse code written in Smalltalk
and output corresponding JavaScript code.

Arbitrary JavaScript code can be included in a grammar
by delimiting it with `@{%` and `%}`.
Often this is used to define functions that are used in postprocessing rules.
It can also be used to customize the lexer,
which is the code that converts the input string into tokens
that are matched by the parser rules.

The provided `id` function returns the first element from the data array.
It is equivalent to `d => d[0]`.

```js
@{%
function binaryOperation(data) {
  return {
    type: "binary operation",
    operator: data[2],
    left: data[0],
    right: data[4],
  };
}
%}

@builtin "number.ne" # using decimal rule
@builtin "whitespace.ne" # using _ rule

start -> additive {% id %}

additive
  -> multiplicative _ [+-] _ additive {% binaryOperation %}
   | multiplicative {% id %}

multiplicative
  -> term _ [*/] _ multiplicative {% binaryOperation %}
   | term {% id %}

term
  -> decimal {% id %}
   | "(" additive ")" {% data => data[1] %}
```

Running the following command:

```bash
nearly-test arithmetic.js -i '2 * 3 + (5 + 1) / 2 - 4'",
```

outputs the following AST:

```text
[
  {
    type: "binary operation",
    operator: "+",
    left: {
      type: "binary operation",
      operator: "*",
      left: 2,
      right: 3,
    },
    right: {
      type: "binary operation",
      operator: "-",
      left: {
        type: "binary operation",
        operator: "/",
        left: {
          type: "binary operation",
          operator: "+",
          left: 5,
          right: 1
        },
        right: 2
      }
      right: 4,
    },
  }
]
```

The following JavaScript function takes
a node from this AST and computes its result.
If it is passed the root node above, it returns the expected result of `5`.

```js
function evaluateAstNode(node) {
  let {left, operator, right} = node;
  if (typeof left !== 'number') left = evaluateAstNode(left);
  if (typeof right !== 'number') right = evaluateAstNode(right);
  switch (operator) {
    case '+':
      return left + right;
    case '-':
      return left - right;
    case '*':
      return left * right;
    case '/':
      return left / right;
  }
}
```

## Using a Grammar from JavaScript Code

The compiled parser code can be used from a JavaScript program.
This works in both server-side code and browser code.

The following example uses the previous grammar to produce an AST.
It then uses the `evaluateAstNode` function above to compute the result.

```js
import nearley from 'nearley';
import grammar from './arithmetic.js'; // compiled grammar

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

const input = '2 * 3 + (5 + 1) / 2 - 4'; // expect 5

try {
  parser.feed(input);
  console.log(parser.results);
  console.log(evaluateAstNode(parser.results[0]));
} catch (e) {
  console.error(e.message);
}
```

## Producing ASTs

Abstract syntax trees (ASTs) describe the results of parsing input text.
They have many uses, including generating code in some programming language.

{% raw %}
Each grammar rule can be followed by postprocessing code
that is delimited by `{%` and `%}`.
{% endraw %}
The code must be the name of a predefined function or a function definition.
The function is passed a data argument commonly named `d`
whose value is an array.
The function can return an AST node.

## Railroad Diagrams

To generate a railroad diagram from a grammar,
use the `nearley-railroad` command.
For example, `nearly-railroad my-grammar.ne -o my-grammar.html`.
To view the diagram, open the generated `.html` in any web browser.

<img alt="nearley Railroad Diagram" style="width: 60%"
  src="/blog/assets/nearley-railroad-diagram.png?v={{pkg.version}}">

## Unparsing

TODO: What does the `nearly-unparse` command do?
Does in generate input that produces given output?

## Example Grammars

See https://github.com/kach/nearley/tree/master/examples.
