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

- `unsigned_int`
- `int`,
- `unsigned_decimal`
- `decimal`
- `percentage`
- `jsonfloat`

`postprocessors.ne` defines the following functions that are used
inside the postprocesssor code associated with grammar rules:

- `nth`
- `$`
- `delimited`

`string.ne` defines the grammar rules:

- `dqstring` for strings delimited by double quotes
- `sqstring` for strings delimited by single quotes
- `btquote` for strings delimited by backticks

`whitespace.ne` defines the grammar rules:

- `_` matches zero or more whitespace characters.
- `__` matches one or more whitespace characters.

To include these files in `.ne` grammar file, use the `@builtin` directive.
For example, `@builtin "whitespace.ne"`.

## Creating a Grammar

Grammars are defined in text files with a `.ne` file extension.
The first grammar rule defines the starting point.
The remaining rules can appear in any order, including alphabetical.

## Compiling a Grammar

To compile a grammar to JavaScript code, use the `nearleyc` command.
For example, `nearlyc my-grammar.ne -o my-grammar.js`.

## Testing a Grammar

To test a grammar with specific input,
use the `nearley-test` command.
For example, `nearly-test my-grammar.js -i 'some test input'.

## Using a Grammar from JavaScript Code

```js
import nearley from 'nearley';
import grammar from './grammar2.js';

const input = `
a := 1.
b := 2.
c := a + b.
`;

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

try {
  const output = parser.feed(input);
  const nodes = output.results[0];
  console.log('nodes =', nodes);
} catch (e) {
  console.error(e.toString());
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

## Unparsing

TODO: What does the `nearly-unparse` command do?
Does in generate input that produces given output?
