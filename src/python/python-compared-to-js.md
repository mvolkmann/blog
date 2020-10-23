---
eleventyNavigation:
  key: Python compared to JavaScript
  order: 1
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

This document provides a comparison of the most commonly used features
of Python and JavaScript. Lesser used features are omitted.

## Overview

| Topic                   | JavaScript                                                                                                | Python                                                                                                                                 |
| ----------------------- | --------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| standard                | {% aTargetBlank "https://www.ecma-international.org/publications/standards/Ecma-262.htm", "ECMAScript" %} | {% aTargetBlank "https://docs.python.org/3/", "Python 3 documentation" %}                                                              |
| evaluation              | dynamic                                                                                                   | dynamic                                                                                                                                |
| performance             | fast                                                                                                      | slow unless libraries call C/C++ functions                                                                                             |
| style guide             | {% aTargetBlank "https://prettier.io/", "Prettier" %}                                                     | {% aTargetBlank "https://www.python.org/dev/peps/pep-0008/", "PEP 8" %}, {% aTargetBlank "https://pypi.org/project/black/", "Black" %} |
| most common indentation | 2 spaces                                                                                                  | 4 spaces (not tabs)                                                                                                                    |
| type coercion           | implicit                                                                                                  | explicit except between number types                                                                                                   |

Note: PEP stands for Python Enhancement Proposal.

Some oddities about Python code formatting include:

- leaving two blank lines before and after every function and class definition
- leaving two spaces before every comment that appears on the same line as code

## Pros and cons

### JavaScript

pros:

- ability to run in web browsers (clients)
  and from command line (servers) using Node.js
- great support for asynchronous code
- performance
- compact syntax for functional programming (ex. `reduce` vs. functools)
- can use TypeScript, a superset of JavaScript, to add type checking

cons:

- still in transition from `require` to `import` syntax in Node.js
- type coercions can result in surprising results if not familiar with them

### Python

pros:

- targeted at scripting and rapid application development
- quantity and maturity of libraries for data analysis and machine learning
- multiple number types, not just double-precision float
- some syntax is easier for beginners
  - no curly braces or semicolons, and fewer parentheses
  - `and` instead of `&&`, `or` instead of `||`, and `not` instead of `!`
  - `print` vs. `console.log`
- can add functions implemented in C/C++ or any language callable from C
- can use type hints and tools like mypy to add type checking
- oddly satisfying to type fewer characters
  (ex. comment with `#` instead of `//`)

cons:

- Even though Python convention is to separate words in multi-word
  variable, function, and method names with an underscore,
  there are many examples where no separation is used.
  This makes it difficult to guess the correct name.
- Dictionary references for retrieving and setting values
  are more verbose than JavaScript object references.
  Rather than using dot syntax such as `d.key`,
  we must use `d['key']` or `d.get('key')`
  even if the key is a single word.
- Anonymous functions are limited to a single expression.
- Lambda functions are more verbose than JavaScript arrow functions
  (`lambda` vs. `=>`).
- The classic ternary operator using a `?` and `:` is not supported.
  See the [Conditional logic](#conditional-logic) section for an example.
- There is no syntax for multi-line comments,
  so commenting out a block of code is tedious.
- Magic methods such as `__init__` use "dunder" names (for double underscore)
  which is an odd and verbose way to distinguish special values.
  Other programming languages typically use a single special character prefix.
  See a list in the [Python magic methods](#magic-methods) section.
- Use of operator overloading (supported by magic methods) can be confusing.
- These was no built-in support for asynchronous code
  until the `asyncio` module was added in Python 3.4.
  Some features require Python 3.7+.
- Python programs have poor performance.
  For examples, see {% aTargetBlank
  "https://benchmarksgame-team.pages.debian.net/benchmarksgame/fastest/python.html",
  "The Computer Language Benchmark Game" %}).
- V3 contains breaking changes, so care must be taken when
  reading documentation and examples to verify version compatibility.

## Running scripts

JavaScript source files have an extension of `.js`
or `.mjs` (for ECMAScript modules).

To run a JavaScript script outside a web browser:

- install {% aTargetBlank "https://nodejs.org/", "Node.js" %}.
- enter `node {name}.js`

Python source files have an extension of `.py`.
Multiple words in file names should be separated by underscores
rather than hyphens because the file name becomes the module name
and hyphens are not valid in module names.
Avoid using file names that match that of an existing module
because doing so will prevent being able to import the existing module.

To run a Python script:

- install the Python interpreter from
  {% aTargetBlank "https://www.python.org/downloads/", "python.org" %}
- enter `python3 {name}.py` or `python {name}.py`

In both cases, command line arguments can be passed to the script.

A JavaScript script running in Node.js
can get them from the array `process.argv`.
The first element is the path to the `node` executable,
the second is the path to the script that is running,
and the remaining elements are the command line arguments.

A Python script can get them from `sys.argv`.
The first element is the path to the script that is running,
and the remaining elements are the command line arguments.

To make a Python source file directly executable in UNIX systems:

- Add this as the first line: `#!/usr/bin/env python3`
- Make the file executable by entering `chmod a+x {file-name}.py`
- To run it from a terminal, enter `./{name}.py`

To automatically restart a script when it or a file it imports is modified:

1. Install Node.js.
1. `npm install -g nodemon`
1. If running a JavaScript script, enter `nodemon {name}.js`
1. If running a Python script, enter `nodemon --exec python3 {name}.py`.

## Getting help

In JavaScript, perform web searches that begin with "MDN"
(for the Mozilla Developer Network) followed by a JavaScript search term.
For example, "mdn regexp".

For Python help, see the list of resources at the end.
You can also enter the `python` command to start the REPL
and enter `help()` to start the help utility.
Once inside, enter the name of a module to get help on it.
To exit the help utility and return to the Python REPL,
enter `quit` or just `q`.

Alternatively, once a module has been imported, help on it
can be obtained by passing it to the `help` function.
For example:

```python
import re # for regular expressions
help(re)
```

## Comments

| Type        | JavaScript  | Python |
| ----------- | ----------- | ------ |
| single-line | `//`        | `#`    |
| multi-line  | `/* ... */` | none   |

## Naming conventions

| Kind                        | JavaScript     | Python          |
| --------------------------- | -------------- | --------------- |
| constant                    | `UNDER_SCORES` | same            |
| variable                    | `camelCase`    | `under_scores`  |
| function                    | `camelCase`    | `under_scores`  |
| class                       | `CamelCase`    | same            |
| method                      | `camelCase`    | `under_scores`  |
| public instance properties  | `camelCase`    | `under_scores`  |
| private instance properties | no convention  | `_under_scores` |

While Python uses a naming convention to identify constants (all uppercase),
they can still be modified.
And the naming convention for private instance variables
(start with an underscore), doesn't prevent access from outside the class.

Many Python libraries, including the standard library,
deviate from the Python naming conventions.
In particular, it is common to find multi-word function and method names
that are all lowercase with no separator between words.
For example, the `functools` standard library defines the functions
`partial_method` (follows convention) and `singledispatchmethod` (does not).

## Built-in types

| Type                 | JavaScript                                                              | Python                                                                                     |
| -------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| Boolean              | `true`, `false`                                                         | `True`, `False`                                                                            |
| number               | default is double precision float, `BigInt`                             | `int`, `float`, `complex`                                                                  |
| character            | use strings                                                             | same                                                                                       |
| string               | `'text'` or `"text"`                                                    | same                                                                                       |
| multi-line string    | `` `text` ``                                                            | `"""text"""` or `'''text'''`                                                               |
| string interpolation | `` `prefix${expr1}suffix${expr2}` ``                                    | `f'prefix{expr1}suffix{expr2}'`                                                            |
| array                | `Array` class, literal syntax `[v1, v2, ...]`                           | `array` module in standard library                                                         |
| list                 | use `Array` class                                                       | `list` class with literal syntax `[v1, v2, ...]`<br>mutable and typically homogeneous      |
| tuple                | no equivalent                                                           | `tuple` class with literal syntax `(v1, v2, ...)`<br>immutable and typically heterogeneous |
| range                | no equivalent                                                           | `range` class<br>`range(start, stop[, step])`                                              |
| key/value pairs      | `Object` class with literal syntax<br>`{k1: v1, k2: v2, ...}` and `Map` | `dict` class with literal syntax<br>`{'k1': v1, 'k2': v2, ...}`                            |
| set                  | `Set` class;<br>create with `new Set()`                                 | `set` class with literal syntax `{v1, v2, ...}`<br>or `set(v1, v2, ...)`                   |
| function             | see "Functions" section below                                           | see "Functions" section below                                                              |
| class                | `class Name { ... }`<br>see "Classes" section below                     | `class Name:`<br>see "Classes" section below                                               |
| no value             | `undefined` or `null`                                                   | `None`                                                                                     |

Everything is an object in Python, even values that are
primitives in JavaScript like Booleans, numbers, and strings.

Python has "sequences" whereas JavaScript has arrays.
Kinds of sequences include string, list, tuple, range, and buffer.
A list is a mutable sequence of values that typically have the same type.
A tuple is an immutable sequence of values that can have varying types.
A range is an immutable sequence of numbers that is often be used for looping.

JavaScript object keys must be strings, but `Map` keys can be any kind of value.
Python dict keys can be any immutable type.

The values that are treated as false when used in a Boolean context
are listed below:

| Language   | False Values                                                                    |
| ---------- | ------------------------------------------------------------------------------- |
| Python     | `False`, `0`, `0.0`, `0j` (complex), empty strings, empty sequences, and `None` |
| JavaScript | `false`, `0`, `NaN`, empty strings, `undefined`, and `null`                     |

In JavaScript empty collections evaluate to `true`.
These include arrays, objects, `Map` instances, and `Set` instances.

In Python the "not a number" value `math.nan` evaluates to `true`.

## Modules

In both JavaScript and Python, modules are defined by a single source file.
A source file can import other modules and those can import more modules.

In both JavaScript and Python each module is only imported once.
If its code is modified, the script must be re-run to interpret the changes.

JavaScript modules that are imported with a relative file path
are only searched for in that location.
JavaScript modules that are imported with only a name
are searched for in the project `node_modules` directory
and the global `node_modules` directory.

Python searches for modules in the following order:

- built-in modules
- directory relative to importing file (using dotted module names)
- directories listed in the `PYTHONPATH` environment variable
- installation-specific directories

To see the directories that will be searched,
`import sys` and execute `print(sys.path)`.

Several module-related topics are described in the following table:

| Topic                      | JavaScript                                                                                              | Python                                                                                               |
| -------------------------- | ------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| a module is defined by ... | content of file                                                                                         | same                                                                                                 |
| export                     | `export name = value;`                                                                                  | everything is automatically exported;<br>indicate private values by starting name with an underscore |
| default export             | `export default name = value;`                                                                          | not supported                                                                                        |
| import default             | `import name from 'path';`                                                                              | not supported                                                                                        |
| import entire module       | `const name from 'modname';`                                                                            | `import modname` or <br>`import modname as other` or<br>`from modname import *`                      |
| import specific values     | `const {name1, name2} from 'modname';` or<br>`const {name1 as other1, name2 as other2} from 'modname';` | `from modname import name1, name2` or<br>`from modname import name1 as other1, name2 as other2`      |
| import default and named   | `import name, {name1, name2} from 'path';`                                                              | not supported                                                                                        |
| open source catalog        | {% aTargetBlank "https://www.npmjs.com/", "https://www.npmjs.com/" %}                                   | {% aTargetBlank "https://pypi.org/", "https://pypi.org/" %}                                          |
| tool to install            | `npm` (installed with Node.js)                                                                          | `pip` (installed with Python) or<br>`conda` (installed with Anaconda)                                |

In Python aliases are typically assigned to commonly used packages.
The community has landed on using the aliases shown in the imports below.

| Package                            | Recommended Import                     |
| ---------------------------------- | -------------------------------------- |
| collections<br>in standard library | `import collections as co`             |
| functools<br>in standard library   | `import functools as ft`               |
| itertools<br>in standard library   | `import itertools as it`               |
| matplotlib                         | `from matplotlib import pyplot as plt` |
| NumPy                              | `import numpy as np`                   |
| pandas                             | `import pandas as pd`                  |
| TensorFlow                         | `import tensorflow as tf`              |

## Packages

### JavaScript packages

JavaScript "packages" are managed using the `npm` tool
which is install with Node.js is installed.
To allow each project to use different versions of packages
and make it easy for other developers to install the same set of packages,
create a `package.json` file in each project
by entering `npm init` and answering some questions.

### Python packages

Python "packages" are managed using the `pip` tool
which is installed when Python is installed.
The name is an acronym for "Pip Installs Packages".
It installs packages from the
{% aTargetBlank "https://pypi.org/", "Python Package Index" %} (pypi:).
To upgrade the version of `pip` being used, enter
`python -m pip install --upgrade pip`.

To allow each project to use different versions of packages
and make it easy for other developers to install the same set of packages,
create a virtual environment.
There are several tools that can be used to do this.
Options include <a href="../anaconda/">Anaconda</a>,
{% aTargetBlank "https://virtualenv.pypa.io/en/latest/", "`virtualenv`" %}
(primarily for Python 2), and
{% aTargetBlank "https://docs.python.org/3/library/venv.html", "`venv`" %}
(a subset of `virtualenv` for Python 3).

To create a virtual environment using `venv`,
enter `python -m venv env`
while in the project root directory.
This creates an `env` subdirectory.

To activate this virtual environment, run the `activate` script
in the `env/bin` directory.
In Windows, enter `env\bin\activate.bat`.
In UNIX environments, enter `source env/bin/activate`.
(When using the Fish shell, add the `.fish` extension.)

Activating a virtual environment changes the environment to use versions of
tools and libraries found in the project `env` directory instead of global ones.
Note that rather than including a copy of a specific version of
the Python interpreter, a symbolic link to it is created.
This also changes the shell prompt to indicate the
environment directory being used, `env` in this case.

To deactivate a virtual environment and return to
using global versions of tools and libraries, enter `deactivate`.

Note that Python virtual environments must be activated to take effect.
This differs from Node.js where simply changing to the directory
of a project causes its dependencies versions to be used
based on the contents of the `node_modules` subdirectory.

For details on using Anaconda to manage virtual environments,
see [here](/blog/python/anaconda).

### Using packages

| Operation                                      | JavaScript                                                                         | Python                                                                       |
| ---------------------------------------------- | ---------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| prepare a project                              | `npm init [-y]`                                                                    | `python -m venv env`<br>where `env` is the directory name used by convention |
| install a package globally                     | `npm install -g {pkg-name}`                                                        | with no environment activated,<br>`pip install {pkg-name}`                   |
| install a package locally                      | `npm install {pkg-name}`                                                           | with an environment activated,<br>`pip install {pkg-name}`                   |
| install a specific version of a package        | `npm install {pkg-name}@{version}`                                                 | `pip install {pkg-name}=={version}`                                          |
| update to latest version of a specific package | `npm update {pkg-name}`                                                            | `pip install --upgrade {pkg-name}`                                           |
| see where global packages are installed        | `npm -g root`                                                                      | with no environment activated,<br>`pip list -v`                              |
| see where local packages are installed         | `npm root`                                                                         | with an environment activated,<br>`pip list -v`                              |
| location of local packages                     | `{project-dir}/node_modules`                                                       | `{project-dir}/lib/python{version}/site-packages`                            |
| see list of locally installed packages         | `npm ls` or<br>open `package.json` and see<br>`dependencies` and `devDependencies` | with environment activated, `pip list`                                       |
| create list of project package versions        | automatically maintained in `package.json`                                         | `pip freeze > requirements.txt`                                              |
| install project package versions               | `npm install`                                                                      | `pip install -r requirements.txt`                                            |

### Project Python packages

Project Python packages enable importing modules from subdirectories.
These are subdirectories whose names are a package name.
The subdirectories must contain a `__init__.py` file
that can be empty or contain initialization code for the package.

Add `.py` files in the package directories that define modules.
Then in `.py` files that wish to use a module in a package
(located in ancestor directories),
use `from package-name import module-name`.
To import specific things from package modules,
use `from package-name.module-name import name1, name2`.

The directory structure can be as deep as you like
with each subdirectory containing a `__init__.py` file.
When there are nested packages, imports look like
`from pkg1.pkg2 import module-name` or
`from pkg1.pkg2.module-name import name1, name2`.

For more information on Python packages, see the
{% aTargetBlank "https://docs.python.org/3/tutorial/modules.html#packages",
"Packages" %} in the Python Tutorial.

## Printing/Logging

| Operation                              | JavaScript                                                                                  | Python                                                |
| -------------------------------------- | ------------------------------------------------------------------------------------------- | ----------------------------------------------------- |
| write space-separated values to stdout | `console.log(v1, v2, ...);`                                                                 | `print(v1, v2, ..)`                                   |
| write space-separated values to stderr | `console.error(v1, v2, ...);`                                                               | `import sys`<br>`print(v1, v2, ..., file=sys.stderr)` |
| write to stdout with interpolation     | `` console.log(`Hello ${name}, today is ${dayOfWeek}.`); ``                                 | `print(f'Hello {name}, today is {dayOfWeek}.')`       |
| write to stdout without newline        | in Node.js<br>`const process = require('process');`<br>`process.stdout.write(v1, v2, ...);` | `print(v1, v2, ..., end='')`                          |

## Variables and assignment

JavaScript variables should be declared
using either the `const` or `let` keyword.
Python variables are not declared and
are created when a value is assigned to them.

JavaScript variable assignments can appear inside expressions
such as an `if` or loop condition, which many developers find confusing.
This was not allowed in Python until version 3.8
which adds the "walrus operator" for assigning values to variables
inside larger expressions. For example:

```python
from calendar import day_name
from datetime import date

def get_day():
    index = date.today().weekday()
    return day_name[index]

if (day := get_day()) == 'Tuesday':
    print('Tacos!') # Tacos! if it's a Tuesday

print('day =', day) # Tuesday

# Typically the code is easier to read when
# the walrus operator is not used.  For example:
day = get_day()
if day == 'Tuesday':
    print('Tacos!') # Tacos! if it's a Tuesday
```

The pylint Python linting tool treats module-level variables as constants.
It will output warnings if functions modify their values.
To avoid this, list all such variables to be modified after
the `global` keyword inside functions that modify them.
The related keyword `nonlocal` enables functions to
access variables in ancestor scopes that are not global.

| Topic                         | JavaScript                                                                     | Python                                                    |
| ----------------------------- | ------------------------------------------------------------------------------ | --------------------------------------------------------- |
| constant declaration          | `const NAME = value;`                                                          | `NAME = value`                                            |
| variable declaration          | `let name = value;`                                                            | `name = value`                                            |
| get type of value in variable | `typeof v` and `v.constructor.name`                                            | `type v:`                                                 |
| multiple assignment           | `const [a, b] = [1, 2]`                                                        | `a, b = 1, 2`                                             |
| destructure sequence          | `const [v1, v2, ...] = array;`<br># of variables can differ from # of elements | `v1, v2 = seq`<br># of variables must match # of elements |
| destructure object            | `const {k1, k2, ...} = object;`                                                | not supported                                             |
| un-declare variable           | `name = undefined` - just removes value                                        | `del name`                                                |
| addition                      | `name += expr`                                                                 | same                                                      |
| subtraction                   | `name -= expr`                                                                 | same                                                      |
| multiplication                | `name *= expr`                                                                 | same                                                      |
| division                      | `name /= expr`                                                                 | same                                                      |
| exponentiation                | `name **= expr`                                                                | same                                                      |
| mod (remainder)               | `name %= expr`                                                                 | same                                                      |
| logical and                   | `name &&= expr`                                                                | not supported                                             |
| logical or                    | `name ||= expr`                                                                | not supported                                             |
| logical xor                   | `name ^= expr`                                                                 | not supported                                             |
| bitwise and                   | `name &= expr`                                                                 | same                                                      |
| bitwise or                    | `name |= expr`                                                                 | same                                                      |
| bitwise xor                   | `name ^= expr`                                                                 | same                                                      |
| signed bit shift              | `<<=` (left), `>>=` (right)                                                    | same                                                      |
| unsigned bit shift            | `<<<=` (left), `>>>=` (right)                                                  | not supported                                             |

JavaScript destructuring can capture multiple values in one array variable.
For example:

```js
const arr = [1, 2, 3, 4];
[a, b, ...rest] = arr; // a=1, b=2, rest=[3, 4]
```

The `rest` variable above is set to an array.
The "rest operator" `...` must appear before the last variable.

Python refers to this operation as "unpacking" and it's even more capable.

```python
seq = (1, 2, 3, 4) # using a tuple, but could also be a list
a, b, *rest = seq # a=1, b=2, rest=[3, 4]
a, *rest, b = seq # a=1, rest=[2, 3], b=4
a, b, *rest = seq # a=1, b=2, rest=[3, 4]
```

The `rest` variable above is set to a list.

## Comparison

| Topic                 | JavaScript                              | Python                  |
| --------------------- | --------------------------------------- | ----------------------- |
| equal                 | `==` (with coercion) or `===` (without) | `==` (without coercion) |
| not equal             | `!=` (with coercion) or `!==` (without) | `!=` (without coercion) |
| same object           | `===`                                   | `is`                    |
| different object      | `!==`                                   | `is not`                |
| less than             | `<`                                     | same                    |
| less than or equal    | `<=`                                    | same                    |
| greater than          | `>`                                     | same                    |
| greater than or equal | `>=`                                    | same                    |

Python comparisons can be chained, but JavaScript comparisons cannot.
For example, to determine whether the value of a variable
is between 10 and 20 inclusive:

- in JavaScript, `10 <= n && n <= 20`
- in Python, `10 <= n <= 20`

## Code blocks

JavaScript code blocks are surrounded by curly brackets (`{ code }`).
Long statements can be split over multiple lines without
including any special character at the ends of initial lines.

Python uses leading whitespace (indentation) to determine
which lines are part of the same block.
All consecutive lines with the same indentation, ignoring empty lines,
are considered to be in the same block.
For the purpose of this determination, tab characters are replaced by spaces
so that the number of spaces is a multiple of eight.
This is significant when a mixture of tabs and spaces are used,
which is not recommended.
Python style guides recommend using multiples of four spaces for indentation
and not using tab characters.
Long statements can be split over multiple lines
by adding a backslash (`\`) at the end of all but the last line
when it is not clear that a statement is continuing.

## <a name="conditional-logic">Conditional logic</a>

In the JavaScript syntax below, `sOrB` is short for "statement or block".
It can be a single statement or a set of statements surrounded by curly braces.

A JavaScript `if` statement can contain any number of `else if` parts.
A Python `if` statement can contain any number of `elif` parts.
Python blocks must start on a new line and be indented.

| Topic           | JavaScript                                          | Python                                            |
| --------------- | --------------------------------------------------- | ------------------------------------------------- |
| if              | `if (cond) sOrB`                                    | `if cond: block`                                  |
| if/else         | `if (cond) sOrB1 else sOrB2`                        | `if cond: block1 else: block2`                    |
| if/else if/else | `if (cond1) sOrB1 else if (cond2) sOrB2 else sOrB3` | `if cond: block1 elif cond2: block2 else: block3` |
| ternary         | `cond ? trueValue : falseValue`                     | `trueValue if cond else falseValue`               |

Here's an example of using a Python ternary statement.

```python
import sys

name = sys.argv[1] if len(sys.argv) > 1 else 'World'

# This is a nice alternative.
name = sys.argv[1] or 'World'

# This syntax is NOT supported.
name = len(sys.argv) > 1 ? sys.argv[1] : 'World'
```

## Iteration

As we will see in the "Key/value collections" section later,
JavaScript can store key/value pairs in plain objects
or in instances of the `Map` class.
Python uses "dictionaries" (or dicts) to store key/value pairs.

| Topic                            | JavaScript                                                           | Python                                    |
| -------------------------------- | -------------------------------------------------------------------- | ----------------------------------------- |
| classic for loop                 | `for (let index = start; index < stop; index += step)`               | `for value in range(start, stop, step?):` |
| over collection                  | `for (const value of iterable)`                                      | `for value in iterable:`                  |
| over object/dict keys            | `for (const key of Object.keys(obj))`<br>or `for (const key in obj)` | `for key in dict.keys():`                 |
| over object/dict values          | `for (const value of Object.values(obj))`                            | `for value in dict.values():`             |
| over object/dict keys and values | `for (const [key, value] of Object.entries(obj))`                    | `for key, value in dict.items():`         |
| top-tested                       | `while (cond)`                                                       | `while cond:`                             |
| bottom-tested                    | `do { ... } while (cond);`                                           | `while True: ... if !cond: break`         |
| break out of closest loop        | `break`                                                              | same                                      |
| continue to next iteration       | `continue`                                                           | same                                      |

## Functions

In JavaScript, functions can be defined in two ways.

```js
// Named function
function myFn(p1, p2, ...) {
  body
}

// Anonymous function (a.k.a. arrow function)
const myFn = (p1, p2, ...) => {
  body
};
```

If an anonymous function has exactly one named argument,
the parentheses around it are optional.
If an anonymous function simply returns the value of a single expression,
The curly braces around the body and the `return` keyword are optional.

In Python, functions can also be defined in two ways.

```python
# Named function
def myFn:
  body

# Lambda function
lambda args: expression
```

Python lambda functions can only return the value of a single expression.
They cannot contain additional statements.

Python named functions can have a docstring as their first line.
This is used by tools that generate documentation from code.
It is typically delimited by triple quotes.
For guidelines on the content of docstrings, see the
{% aTargetBlank
  "https://docs.python.org/3/tutorial/controlflow.html#documentation-strings",
  "Documentation Strings" %} in the Python Tutorial and
{% aTargetBlank
  "https://www.python.org/dev/peps/pep-0008/#documentation-strings",
  "PEP-8 documentation strings" %}.
A good docstring for a function looks like:

```python
    """Return the average of a sequence of numbers."""
```

JavaScript does not support function overloading
where the same function name can be defined multiple times
with different numbers and/or types of arguments.
Python supports this in a limited sense using the `singledispatch` decorator
defined in the [`functools`](/blog/python/python-functools) standard library.

| Topic                                                                  | JavaScript                                                                           | Python                                                                                                              |
| ---------------------------------------------------------------------- | ------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------- |
| define named                                                           | `function fnName(params) { ... }`                                                    | `def fnName(params): ...`                                                                                           |
| define anonymous                                                       | `const fnName = (params) => definition`                                              | `lambda params: expression`                                                                                         |
| define anonymous w/ single parameter                                   | `const fnName = param => {...}`                                                      | same as above                                                                                                       |
| define anonymous w/ single expression                                  | `const fnName = (params) => expr`                                                    | same as above                                                                                                       |
| specify default parameter values                                       | `function fnName(p1=v1, p2=v2) {...}`                                                | `def fnName(p1=v1, p2=v2): ...`                                                                                     |
| gather variable number of arguments                                    | `function fnName(p1, p2, ...rest) {...}`<br>`rest` is set to an `Array`              | `def fnName(p1, p2, *rest): ...`<br>`rest` is set to a tuple                                                        |
| gather arguments as key/value pairs                                    | not supported                                                                        | `def fnName(**args): ...`<br>call with `fnName(p1=v2, p2=v2)`<br>or `fnName(**dict)`                                |
| use named/keyword arguments                                            | `function fnName({p1, p2}) {...}`<br>pass an "options" object                        | same as above;<br>any parameter can be specified by name;<br>important feature!<br>call with `fnName(p1=v2, p2=v2)` |
| return a value                                                         | `return value;`                                                                      | `return value`                                                                                                      |
| default return value when no `return`                                  | `undefined`                                                                          | `None`                                                                                                              |
| call                                                                   | `fnName(args)`                                                                       | same                                                                                                                |
| get required argument count                                            | `fnName.length`                                                                      | `from inspect import getfullargspec`<br>`len(getfullargspec(fn).args)`                                              |
| passing fewer arguments than<br>positional parameters                  | remaining are assigned `undefined`                                                   | results in an error                                                                                                 |
| passing more arguments than<br>positional parameters with no gathering | all arguments are available in `arguments` array-like object                         | results in an error                                                                                                 |
| get name                                                               | `fnName.name`                                                                        | `fnName.__name__`                                                                                                   |
| get implementation code                                                | `fnName.toString()`                                                                  | `from inspect import getsource`<br>`getsource(fn)`                                                                  |
| create partial                                                         | `const newFn = fnName.bind(thisValue, arg1, arg2, ...)`<br>`thisValue` can be `null` | `from functools import partial`<br>`newFn = partial(fn, arg1, arg2, ...)`                                           |
| call                                                                   | `fnName.call(thisValue, arg1, arg2, ...)`<br>`thisValue` can be `null`               | `ClassName.methodName(obj, arg1, arg2, ...)`                                                                        |
| apply                                                                  | `fnName.apply(thisValue, argArray)`<br>`thisValue` can be `null`                     | `ClassName.methodName(obj, *argList)`                                                                               |
| spread array to positional arguments                                   | `fnName(...arr)`                                                                     | `fnName(*seq)`                                                                                                      |
| spread object to keyword arguments                                     | not supported                                                                        | `fnName(**dict)`                                                                                                    |

In Python:

- Function parameters with a default value must follows those without one.
- Function parameters listed after one that begins with `*`
  must be specified by name.
- The `partial` function (shown in the table above)
  can only be used on functions.
  For methods of a class, use `partial_method`.

## Execute later or at intervals

In JavaScript, the `setTimeout` function registers a function to be
called after some number of milliseconds in the future. For example:

```js
function myFunction(p1, p2) {
  console.log('myFunction: p1 =', p1, 'p2 =', p2);
}

// Call function above after one second.
const id1 = setTimeout(() => myFunction('arg1', 'arg2'), 1000);
if (tiredOfWaiting) clearTimeout(id1);
```

In Python the same can be done as follows:

```python
import threading

def my_function(p1, p2):
    print('my_function: p1 =', p1, 'p2 =', p2)

# The Timer function takes the number of seconds to wait,
# the function to call, and a tuple of arguments to be passed.
t = threading.Timer(1.0, my_function, ('arg1', 'arg2'))
t.start()
if tired_of_waiting:
    t.cancel()
```

In JavaScript, the `setInterval` function registers a function to be
called repeatedly every some number of milliseconds. For example:

```js
// Call the function above once every two seconds.
const id2 = setInterval(() => myFunction('arg1', 'arg2'), 2000);
if (tiredOfWaiting) clearInterval(id2);

// Cancel interval after five seconds.
setTimeout(() => clearInterval(id2), 5000);
```

In Python there is no simple equivalent.
But the same can be done if we define and use a class as follows:

```python
import threading
import time

class SetInterval:
    def __init__(self, seconds, fn, args):
        self.seconds = seconds
        self.fn = fn
        self.args = args
        self.event = threading.Event()
        threading.Thread(target=self.__set_interval).start()

    def __set_interval(self):
        next_time = time.time() + self.seconds
        while not self.event.wait(next_time - time.time()):
            next_time += self.seconds
            self.fn(*self.args)

    def cancel(self):
        self.event.set()

def my_function(p1, p2):
    print('my_function: p1 =', p1, 'p2 =', p2)

interval = SetInterval(2, my_function, ('arg1', 'arg2'))
if tired_of_waiting:
    interval.cancel()

# Cancel interval after five seconds.
threading.Timer(5, lambda: interval.cancel()).start()
```

## Asynchronous functions

In Python 3.4+, asynchronous functions are supported by the `asyncio` library.

| Topic                              | JavaScript                                                                                     | Python                      |
| ---------------------------------- | ---------------------------------------------------------------------------------------------- | --------------------------- |
| define async named function        | `async function fnName(params) { ... }`                                                        | `async def fnName(params):` |
| define async anonymous function    | `const fnName = async (params) => { ... }`                                                     | not supported               |
| async call with `await`            | `const result = await name(args);`<br>often wrapped in a `try` block                           | `result = await name(args)` |
| async call with `then` and `catch` | `name(args).`<br>&nbsp;&nbsp;`then(result => { ... }).`<br>&nbsp;&nbsp;`catch(err => { ...});` | n/a                         |

In JavaScript, async functions return a `Promise` object.
Here is an example of running tasks that
take a simulated amount of time to complete.
The first takes 3 seconds, the second takes 2, and the third takes 1.
Each tasks outputs its "starting" message immediately.
The "ending" messages appear in reverse order
due to their differing sleep durations.

```js
const sleep = async ms => new Promise(resolve => setTimeout(resolve, ms));

async function doIt(name, sleepMs) {
  console.log('starting', name);
  await sleep(sleepMs);
  console.log('ending', name);
}

async function main() {
  const task1 = doIt('alpha', 3000);
  const task2 = doIt('beta', 2000);
  const task3 = doIt('gamma', 1000);
  await Promise.all([task1, task2, task3]);
  console.log('finished');
}

main();
```

The output is:

```text
starting alpha
starting beta
starting gamma
ending gamma
ending beta
ending alpha
finished
```

In Python 3.4, the
{% aTargetBlank "https://docs.python.org/3.8/library/asyncio.html", "asyncio" %}
library was added.
It can be used to create coroutines which are similar to JavaScript Promises.
The Python language doesn't provide the equivalent of the JavaScript promises,
but {% aTargetBlank "https://pypi.org/project/promise/", "libraries" %} do.

Here is an implementation of the previous JavaScript example in Python
that produces the same output:

```python
from asyncio import create_task, gather, run, sleep

async def doIt(name, sleepMs):
    print('starting', name)
    await sleep(sleepMs / 1000)
    print('ending', name)

async def main():
    task1 = create_task(doIt('alpha', 3000))
    task2 = create_task(doIt('beta', 2000))
    task3 = create_task(doIt('gamma', 1000))
    await gather(task1, task2, task2)
    print('finished')

run(main())
```

## Classes

| Topic                             | JavaScript                                                                | Python                                                                |
| --------------------------------- | ------------------------------------------------------------------------- | --------------------------------------------------------------------- |
| define                            | `class CName { ... }`                                                     | `class CName: ...`                                                    |
| inheritance                       | `class Sub extends Super { ... }`<br>only single inheritance is supported | `class Sub(Super1, Super2, ...)`<br>multiple inheritance is supported |
| constructor                       | `constructor(params) { ... }`                                             | `def __init__(self, params):`                                         |
| instantiate (create instance)     | `const instance = new CName(args);`                                       | `instance = CName(args)`                                              |
| instance property declaration     | not declared; set in constructor on `this` object                         | not declared; set in \_\_init\_\_ on `self` object                    |
| instance property reference       | `this.propName`                                                           | `self.propName`                                                       |
| class/static property declaration | `static propName = value;`                                                | `propName = value;`                                                   |
| class/static property reference   | `CName.propName`                                                          | `CName.propName` or `instance.propName`                               |
| instance method                   | `name(params) { ... }`                                                    | `def name(self, params): ...`                                         |
| class/static method declaration   | `static name(params) { ... }`                                             | `@staticmethod`<br>`def name(params): ...`                            |
| class/static method call          | `CName.name(params)`                                                      | `CName.name(params)` or `instance.name(params)`                       |

In addition to the `@staticmethod` decorator, Python also supports the
`@classmethod` decorator. The difference is that methods defined with
the latter are passed the class as the first argument.

Here is an example of a JavaScript class:

```js
class Statistics {
  constructor(...numbers) {
    this.numbers = numbers;
    this.min = Math.min(...this.numbers);
    this.max = Math.max(...this.numbers);
    this.sum = this.numbers.reduce((acc, n) => acc + n, 0);
  }

  add(number) {
    this.numbers.push(number);
    this.sum += number;
    if (number < this.min) {
      this.min = number;
    } else if (number > this.max) {
      this.max = number;
    }
  }

  mean() {
    return this.sum / this.numbers.length;
  }

  report() {
    console.log('min =', this.min);
    console.log('max =', this.max);
    console.log('mean =', this.mean());
  }
}

const stats = new Statistics(1, 2, 3, 4);
stats.report();
stats.add(5);
stats.report();
```

The output is:

```text
min = 1
max = 4
mean = 2.5
min = 1
max = 5
mean = 3
```

Here is the same class implemented in Python:

```python
class Statistics:
    def __init__(self, *numbers):
        self.numbers = list(numbers)
        self.min = min(*self.numbers)
        self.max = max(*self.numbers)
        self.sum = sum(numbers)

    def add(self, number):
        self.numbers.append(number)
        self.sum += number
        if number < self.min:
            self.min = number
        elif number > self.max:
            self.max = number

    def mean(self):
        return self.sum / len(self.numbers)

    def report(self):
        print('min =', self.min)
        print('max =', self.max)
        print('mean =', self.mean())

stats = Statistics(1, 2, 3, 4)
stats.report()
stats.add(5)
stats.report()
```

The output is the same as above.

Note how in Python the first parameter in all instance methods must be `self`.

Here is a JavaScript function that
takes a class and prints its inheritance hierarchy:

```js
function printClassTree(cls, level = 0) {
  const name = cls.name || 'Object';
  console.log(' '.repeat(2 * level) + name);
  if (name !== 'Object') printClassTree(cls.__proto__, level + 1);
}
```

Here is a Python function to do the same:

```python
def print_class_tree(cls, level = 0):
    print(' ' * 2 * level + cls.__name__)
    for base in cls.__bases__:
        print_class_tree(base, level + 1)
```

## Built-in Functions

JavaScript provides a small number (9) of built-in functions.
Python provides many more (68).
The tables below summarize these.

Often one of the languages does not have an equivalent function to the other,
so the closest alternative is shown instead.

Descriptions below that begin with "determines if"
mean that a Boolean value is returned.

| Python built-in function                         | Description                                                                                            | Closest JavaScript equivalent                                        |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------- |
| `abs(x)`                                         | returns absolute value                                                                                 | `Math.abs(x)`                                                        |
| `all(iterable)`                                  | determines if all elements are `True`<br>in a Boolean context                                          | `arr.every(predicate)`                                               |
| `any(iterable)`                                  | determines if any element is `True`<br>in a Boolean context                                            | `arr.some(predicate)`                                                |
| `ascii(obj)`                                     | like `repr`, but escapes non-ASCII characters                                                          | not supported                                                        |
| `bin(x)`                                         | converts integer to binary string                                                                      | not supported                                                        |
| `bool(x)`                                        | converts value to Boolean                                                                              | `Boolean(x)`                                                         |
| `breakpoint()`                                   | breaks execution and<br>drops into debugger                                                            | `debugger`                                                           |
| `bytearray(source)`                              | returns a new array of bytes                                                                           | use `ArrayBuffer` with typed array classes                           |
| `bytes(source)`                                  | returns a `bytes` object                                                                               | use `ArrayBuffer` with typed array classes                           |
| `callable(x)`                                    | determines if x is callable (a function)                                                               | `typeof x === 'function'`                                            |
| `chr(code_point)`                                | returns string representation<br>of a Unicode code point                                               | `String.fromCodePoint(codePoint)`                                    |
| `compile(source, filename, mode)`                | compiles source into a code/AST object<br>which can be passed to `exec` or `eval`                      | not supported                                                        |
| `complex(real, imag)`                            | creates a complex number<br>from real and imaginary parts                                              | not supported                                                        |
| `delattr(obj, name)`                             | deletes an attribute from an object                                                                    | `delete obj[name]`                                                   |
| `dict([data])`                                   | creates a dictionary                                                                                   | `{}` or `new Map()`                                                  |
| `dir([obj])`                                     | returns a list of defined names<br>in current scope or an object                                       | `Object.keys(obj)`                                                   |
| `divmod(a, b)`                                   | return tuple of quotient and remainder<br>of `a` divided by `b`                                        | `Math.floor(a / b)` and `a % b`                                      |
| `enumerate(iterable)`                            | return list of tuples each containing<br>an index and value from an iterable                           | `Object.entries(arr)`                                                |
| `eval(code)`                                     | evaluates a single code expression                                                                     | `eval(code)`                                                         |
| `exec(code)`                                     | execute any number of lines of code                                                                    | `eval(code)`                                                         |
| `filter(predicate, iterable)`                    | returns iterator over values in iterable<br>where predicate function returns true                      | `arr.filter(predicate)`                                              |
| `float(x)`                                       | returns floating point number<br>created from a number or string                                       | `parseFloat(x)`                                                      |
| `format(value, format)`                          | returns string created by formatting a value<br>using a format string                                  | use template literals                                                |
| `frozenset(iterable)`                            | returns `frozenset` (immutable set) object<br>created from iterable                                    | `Object.freeze(obj)`                                                 |
| `getattr(obj, name [, default])`                 | returns attribute value                                                                                | `obj[name] || default`                                               |
| `globals()`                                      | returns dictionary containing the<br>current global symbol table                                       | not supported                                                        |
| `hasattr(obj, name)`                             | determines if object has a given attribute                                                             | `name in obj`                                                        |
| `hash(obj)`                                      | returns hash value of object                                                                           | not supported                                                        |
| `help([topic])`                                  | invoke the built-in Python help system,<br>typically in the REPL                                       | not supported                                                        |
| `hex(n)`                                         | converts integer to hex                                                                                | `n.toString(16)`                                                     |
| `id(obj)`                                        | returns identity of an object                                                                          | not supported                                                        |
| `input([prompt])`                                | read from stdout with optional prompt                                                                  | use the Node `readline` module<br> `question` method                 |
| `int(x)`                                         | returns an integer created from<br>a number or string                                                  | `parseInt(x[, radix])`                                               |
| `isinstance(obj, ClassName)`                     | determines if an object is<br>an instance of a given class                                             | `obj instanceof ClassName`                                           |
| `issubclass(ClassA, ClassB)`                     | determines if `ClassB` is<br>a subclass of `ClassA`                                                    | `ClassB.prototype instanceof ClassA`                                 |
| `iter(collection)`                               | returns an iterator over the elements<br>of a collection (see `next`)                                  | `arr[Symbol.iterator]()`                                             |
| `len(obj)`                                       | returns number of items in a collection<br>or characters in a string                                   | `obj.length`                                                         |
| `list(iterable)`                                 | constructs a list from an iterable                                                                     | `Array.from(arrayLike)`                                              |
| `locals():`                                      | returns dictionary containing the<br>current local symbol table                                        | not supported                                                        |
| `map(fn, iterable)`                              | returns iterator over values returned by<br>calling function on each iterable element                  | `arr.map(fn)`                                                        |
| `max(v1, v2, ...)` or<br>`max(iterable)`         | returns largest value of arguments<br>or in an iterable                                                | `Math.max(v1, v2, ...)` or<br>`Math.max(...arr)`                     |
| `memoryview(obj)`                                | returns a `memoryview` for an object<br>that support the buffer protocol                               | not supported                                                        |
| `min(v1, v2, ...)` or<br>`min(iterable)`         | returns largest value of arguments<br>or in an iterable                                                | `Math.min(v1, v2, ...)` or<br>`Math.min(...arr)`                     |
| `next(iterator)`                                 | get next item from an iterator                                                                         | `iterator.next()`                                                    |
| `object()`                                       | create an empty, featureless object;<br>can't add properties; Why is this useful?                      | `{}` is similar, but CAN add properties                              |
| `oct(n)`                                         | converts integer to octal                                                                              | `n.toString(8)`                                                      |
| `open(file, mode, ...)`                          | opens a file for<br>reading, writing, or appending                                                     | see the Node fs module                                               |
| `ord(s)`                                         | returns the Unicode code point<br>for a Unicode character                                              | `s.charCodeAt([index])`                                              |
| `base ** exp` or `pow(base, exp)`                | return base raised to exp power                                                                        | `base ** exp` or `Math.pow(base, exp)`                               |
| `print(v1, v2, ...)`                             | print space-separated expression values                                                                | `console.log(v1, v2, ...)`                                           |
| `property(getFn, setFn, delFn, doc)`             | returns a property attribute that encapsulates<br>get, set and delete functions                        | not supported                                                        |
| `range(stop)` or<br>`range(start, stop[, step])` | returns a `range` object<br>which is an immutable sequence                                             | not supported                                                        |
| `repr(obj)`                                      | returns a string representation<br>of an object for developers                                         | `obj.toString()`                                                     |
| `reversed(seq)`                                  | returns an iterator for iterating<br>over a sequence in reverse order                                  | not supported                                                        |
| `round(n[, digits])`<br>returns `float` or `int` | returns a number rounded to<br>some number of decimal points                                           | `n.toFixed(digits)`<br>returns string                                |
| `set([iterable])`                                | creates a `set` object                                                                                 | `new Set()`                                                          |
| `setattr(obj, name, value)`                      | sets an attribute of an object                                                                         | `obj[name] = value`                                                  |
| `slice(stop)` or<br>`slice(start, stop[, step])` | returns a `slice` object that<br>describes a set of indexes;<br>used to retrieve data at those indexes | not supported                                                        |
| `sorted(iterable[, key])`                        | returns a sorted version<br>of an iterable as a `list`                                                 | `arr.sort([compareFn])`<br>sorts in place                            |
| `str(obj)`                                       | returns a human-readable string<br>representation of an object                                         | `obj.toString()`                                                     | : |
| `sum(iterable)`                                  | returns the sum of<br>numbers in an iterable                                                           | `arr.reduce((acc, n) => acc + n)`                                    |
| `super()`                                        | returns a proxy object for<br>calling superclass methods                                               | `super` keyword                                                      |
| `tuple([iterable])`                              | creates a tuple, optionally<br>populated from an iterable                                              | not supported                                                        |
| `type()` returns class object                    | returns the type of a value                                                                            | `typeof v` returns string<br>`v.constructor` is constructor function |
| `vars(obj)`                                      | returns a `dict` view of<br>the attributes in an object                                                | not supported                                                        |  |  |
| `zip(iterables)`                                 | returns an iterator that aggregates<br>elements from multiple iterables                                | not built-in;<br>can use Lodash `zip` function                       |

<br>

| JavaScript built-in function | Description                                                                 | Closest Python equivalent        |
| ---------------------------- | --------------------------------------------------------------------------- | -------------------------------- |
| `decodeURI(s)`               | decodes a URL (opposite of `encodeURI`)                                     | `urllib.unquote(s)`              |
| `decodeURIComponent(s)`      | decodes a component of a URI<br>(opposite of `encodeURIComponent`)          | `urllib.unquote(s)`              |
| `encodeURI(s)`               | encodes a URI, replacing certain characters<br>(not /, #, ?, =, and others) | `urllib.quote(s, safe='chars1')` |
| `encodeURIComponent(s)`      | encodes a component of a URI,<br>replacing certain characters               | `urllib.quote(s, safe='chars2')` |
| `eval(code)`                 | execute any number of lines of code                                         | `exec(code)`                     |
| `isFinite(x)`                | determines if x is a finite number                                          | `math.isfinite(x)`               |
| `isNaN(x)`                   | determines of x in the "not a number" value                                 | `math.isnan(x)`                  |
| `parseFloat(x)`              | returns floating point number<br>created from a number or string            | `float(x)`                       |
| `parseInt(x[, radix])`       | returns an integer created from<br>a number or string                       | `int(x)`                         |

The JavaScript `parseFloat` and `parseInt` functions can process strings
that contain additional characters after those in the number.
For example, `parseFloat('3.14pi')` returns the number `3.14`.
The Python `float` and `int` functions do not support this.

## Boolean operations

| Operation   | JavaScript | Python      |
| ----------- | ---------- | ----------- |
| and         | `b1 && b2` | `b1 and b2` |
| or          | `b1 || b2` | `b1 or b2`  |
| not         | `!b`       | `not b`     |
| bitwise and | `b1 & b2`  | same        |
| bitwise or  | `b1 | b2`  | same        |
| bitwise not | `~b`       | same        |
| bitwise xor | `b1 ^ b2`  | same        |

## Numeric operations

| Operation                                     | JavaScript                             | Python               |
| --------------------------------------------- | -------------------------------------- | -------------------- |
| basic                                         | `+`, `-`, `*`, `/`                     | same                 |
| exponentiation                                | `**`                                   | same                 |
| increment                                     | `++v` (pre) or `v++` (post)            | `v += 1`             |
| decrement                                     | `--v` (pre) or `v--` (post)            | `v -= 1`             |
| mod (remainder)                               | `%`                                    | same                 |
| convert to string                             | `n.toString()`                         | `str(n)`             |
| convert from string to integer                | `Number(s)` or `parseInt(s)`           | `int(s)`             |
| convert from string to float                  | `Number(s)` or `parseFloat(s)`         | `float(s)`           |
| convert to string with fixed decimals (ex. 2) | `n.toFixed(2)`                         | `'{:.2f}'.format(n)` |
| convert to hex                                | `n.toString(16)`                       | `hex(n)`             |
| convert from hex                              | `parseInt(hexString, 16)`              | `int(hexString, 16)` |
| constants                                     | see `Math` and `Number` global objects | see `math` module    |
| functions                                     | see `Math` and `Number` global objects | see `math` module    |

## Math operations

Lesser used constants and functions are omitted from the table below.

To use the Python functions, `import math`.

| Operation                      | JavaScript                    | Python                                       |
| ------------------------------ | ----------------------------- | -------------------------------------------- |
| absolute value                 | `Math.abs(x)`                 | `math.fabs(x)`                               |
| arc cosine                     | `Math.acos(x)`                | `math.acos(x)`                               |
| arc sine                       | `Math.asin(x)`                | `math.asin(x)`                               |
| arc tangent                    | `Math.atan(x)`                | `math.atan(x)`                               |
| ceiling                        | `Math.ceil(x)`                | `math.ceil(x)`                               |
| close                          | not built-in                  | `math.isclose(x, y, rel_tol=rt, abs_tol=at)` |
| combinations of k items from n | not built-in; see below       | `math.comb(n, k)`                            |
| convert degrees to radians     | `degrees * (Math.PI / 180)`   | `math.degrees(radians)`                      |
| convert radians to degrees     | `radians * (180 / Math.PI)`   | `math.radians(degrees)`                      |
| cosine                         | `Math.cos(x)`                 | `math.cos(x)`                                |
| cube root                      | `Math.cbrt(x)`                | `x ** (1. / 3)`                              |
| e constant                     | `Math.E`                      | `math.e`                                     |
| e to power                     | `Math.exp(x)`                 | `math.exp(x)`                                |
| factorial                      | not built-in; see below       | `math.factorial(n)`                          |
| floor                          | `Math.floor(x)`               | `math.floor(x)`                              |
| greatest common denominator    | not built-in                  | `math.gcd(n1, n2, ...)`                      |
| hypotenuse                     | `Math.hypot(x, y, ...)`       | `math.hypot(x)`                              |
| least common multiple          | not built-in                  | `math.lcm(n1, n2, ...)`                      |
| log base 10                    | `Math.log10(x)`               | `math.log10(x)`                              |
| logarithm to any base          | not built-in; see below       | `math.log(x, y)`                             |
| maximum                        | `Math.max(n1, n2, ...)`       | `max(n1, n2, ...)`                           |
| mean (average)                 | not built-in; see below       | `statistics.mean(seq)`                       |
| median                         | not built-in; see below       | `statistics.median(seq)`                     |
| maximum                        | `Math.max(n1, n2, ...)`       | `max(n1, n2, ...)`                           |
| mode                           | not built-in; see below       | `statistics.mode(seq)`                       |
| natural log (base e)           | `Math.log(x)`                 | `math.log(x)`                                |
| not a number                   | `Number.isNaN(x)`             | `math.isnan(x)`                              |
| not a number constant          | `Number.NaN`                  | `math.nan`                                   |
| permutations of k items from n | not built-in; see below       | `math.perm(n, k)`                            |
| pi constant                    | `Math.PI`                     | `math.pi`                                    |
| power (x to y)                 | `Math.pow(x, y)`              | `math.pow(x)`                                |
| product                        | use `Array reduce`; see below | `math.prod(iterable)`                        |
| random [0, 1)                  | `Math.random()`               | `random.random()`                            |
| random integer [a, b]          | not built-in; see below       | `random.randint(a, b)`                       |
| round                          | `Math.round(x)`               | `round(x, decimal_places=0)`                 |
| sign (-1, 0, or 1)             | `Math.sign(x)`                | `0 if x == 0 else math.copysign(1, x)`       |
| sine                           | `Math.sin(x)`                 | `math.sin(x)`                                |
| square root                    | `Math.sqrt(x)`                | `math.sqrt(x)`                               |
| square root of 1/2             | `Math.SQRT1_2`                | `math.sqrt(0.5)`                             |
| square root of 2               | `Math.SQRT2`                  | `math.sqrt(2)`                               |
| sum                            | use `Array reduce`; see below | `math.fsum(iterable)`                        |
| tangent                        | `Math.tan(x)`                 | `math.tan(x)`                                |
| truncate                       | `Math.trunc(x)`               | `math.trunc(x)`                              |

JavaScript can use the following functions to compute
some of the values marked as "not built-in" in the table above:

```js
function combinations(n, k) {
  // n take k
  return factorial(n) / (factorial(k) * factorial(n - k));
}

function factorial(n) {
  if (n < 0) return undefined;
  let f = 1;
  while (n > 1) {
    f *= n--;
  }
  return f;
}

// Compute the logarithm of x using a given base.
const logBase = (x, base) => Math.log(x) / Math.log(base);

const mean = nums => sum(nums) / nums.length;

function median(nums) {
  nums.sort(); // may not want to sort in place
  const len = nums.length;
  const index = Math.floor(len / 2);
  return len % 2 === 1 ? nums[index] : (nums[index - 1] + nums[index]) / 2;
}

function mode(nums) {
  let result = undefined;
  let resultCount = 0;
  const counts = nums.reduce((acc, n) => {
    if (acc[n] === undefined) {
      acc[n] = 1;
    } else {
      acc[n]++;
    }
    if (acc[n] > resultCount) {
      result = n;
      resultCount = acc[n];
    }
    return acc;
  }, {});
  return result;
}

function permutations(n, k) {
  // n take k
  return factorial(n) / factorial(n - k);
}

const product = nums => (nums.length ? nums.reduce((acc, n) => acc * n, 1) : 0);

// Generate a random integer in the range [a, b].
const randint = (a, b) => a + Math.floor(Math.random() * (b - a + 1));

const sum = nums => (nums.length ? nums.reduce((acc, n) => acc + n) : 0);
```

The Python `random` module also provides:

- `random.choice(seq)` returns a random element from a sequence
- `random.shuffle(seq)` shuffles a sequence in place
- and more

## String operations

| Operation           | JavaScript                                      | Python                                                                             |
| ------------------- | ----------------------------------------------- | ---------------------------------------------------------------------------------- |
| literal single line | `'text'` or `"text"`                            | same                                                                               |
| literal multi-line  | `` `text` ``                                    | `"""text"""` or `'''text'''`                                                       |
| length              | `s.length`                                      | `len(s)`                                                                           |
| concatenate         | `s1 + n1`                                       | `s1 + str(n1)` or<br>`s1 str(n1)` with only a space between them                   |
| lowercase           | `s.toLowerCase()`                               | `s.lower()`                                                                        |
| uppercase           | `s.toUpperCase()`                               | `s.upper()`                                                                        |
| substring           | `s1.substring(start, end?)`                     | `s[start:end]` or `s[start:]` or `s[:end]`                                         |
| slice               | like `substring`, but supports negative indexes | same as above                                                                      |
| split               | `s.split(delimiter)` returns array              | `s.split(delimiter)` returns list                                                  |
| starts with         | `s.startsWith(sub)` returns Boolean             | `s.startswith(sub)` returns Boolean                                                |
| ends with           | `s.endsWith(sub)` returns Boolean               | `s.endswith(sub)` returns Boolean                                                  |
| contains            | `s.includes(sub)` returns Boolean               | `sub in s` returns Boolean                                                         |
| index of            | `s.indexOf(sub)` returns number                 | `s.index(sub, start?, end?)` returns int                                           |
| last index of       | `s.lastIndexOf(sub)` returns number             | `s.rindex(sub, start?, end?)` returns int                                          |
| compare             | `s1.localeCompare(s2)` returns -1, 0, or 1      | `import locale`<br>`locale.strcoll(s1, s2)`<br>returns negative, zero, or positive |
| remove prefix       | not supported                                   | `s.removeprefix(p)`                                                                |
| remove suffix       | not supported                                   | `s.removesuffix(p)`                                                                |
| replace first       | `s.replace(oldSub, newSub)`                     | `s.replace(old, new, 1)`                                                           |
| replace all         | `s.replaceAll(oldSub, newSub)`                  | `s.replace(old, new)`                                                              |
| trim start          | `s.trimStart()`                                 | `s.lstrip()`                                                                       |
| trim end            | `s.trimEnd()`                                   | `s.rstrip()`                                                                       |
| trim both ends      | `s.trim()`                                      | `s.strip()`                                                                        |
| repeat n times      | `s.repeat(n)`                                   | `s * n` or `n * s`                                                                 |

## <a name="sequences">Sequences</a>

JavaScript stores sequences of values in arrays.
Python primarily uses the four sequence types
`list`, `tuple`, `range`, and `set` for this purpose.

Python lists are mutable and are typically homogeneous
(elements have the same type).
Python tuples are immutable and are typically heterogeneous
(elements can have different types).
Python ranges are immutable sequences of numbers
and are often used in `for` loops.
Python sets are mutable sequences that do not allow duplicate values
and are typically homogeneous.

To create a JavaScript array:

```js
const myArray = [element1, element2, ...];
```

To create a Python `list`:

```python
myList = [element1, element2, ...]
```

To create a Python `tuple`:

```python
# Parentheses around a tuple are optional.
myTuple = (element1, element2, ...)
```

To create a Python `range`:

```python
myRange = range(end) # 0 to end-1
myRange = range(start, end, step?) # start to end-1 where step defaults to 1
```

To create a Python `set`:

```python
mySet = {element1, element2, ...}
```

All of these types can be nested within each other.
For example, the elements of a list can be tuples
and the elements of a tuple can be ranges.
An exception is that the elements of a range can only be integers.

A named tuple gives a name to a tuple type and
supports accessing elements in instances by name and index.
For example:

```python
from collections import namedtuple
# Internally, this generate a class named Dog.
Dog = namedtuple('Dog', 'name breed')
dog = Dog('Comet', 'Whippet')
print(dog.name) # Comet
print(dog[0]) # Comet
print(dog.breed) # Whippet
print(dog[1]) # Whippet
print(len(dog)) # 2
```

| Operation                    | JavaScript                                                                                          | Python                                                                                                                                             |
| ---------------------------- | --------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| is array/sequence            | `Array.isArray(expr)`                                                                               | `hasattr(type(expr), '__iter__')`<br>`isinstance(expr, list)`<br>`isinstance(expr, tuple)`<br>`isinstance(expr, range)`<br>`isinstance(expr, set)` |
| add to end                   | `arr.push(v1, v2, ...);`                                                                            | `seq.append(v)` to add one and<br>`seq.extend(iterable)` to add more than one                                                                      |
| remove from end              | `const value = arr.pop();`                                                                          | `value = seq.pop()`                                                                                                                                |
| add to start                 | `arr.unshift(value);`                                                                               | `seq.insert(0, value)`                                                                                                                             |
| remove from start            | `const value = arr.shift();`                                                                        | `del seq[0]`                                                                                                                                       |
| insert                       | `arr.splice(index, delCount, v1, v2, ...)`                                                          | `seq.insert(index, value)`                                                                                                                         |
| remove item at index         | `arr.splice(index, 1)`                                                                              | `del seq[index]` - only for lists                                                                                                                  |
| remove items at index range  | `arr.splice(start, count)`                                                                          | `del seq[start:start+count]` - only for lists                                                                                                      |
| remove value                 | `arr.splice(arr.findIndex(value), 1)`                                                               | `seq.remove(value)` - error if not found                                                                                                           |
| remove all                   | `arr = [];`                                                                                         | `seq.clear()`                                                                                                                                      |
| change                       | `arr.splice(start, delCount, v1, v2, ...);`                                                         | combine `del` and `insert` above                                                                                                                   |
| length                       | `arr.length`                                                                                        | `len(seq)`                                                                                                                                         |
| lookup                       | `const value = arr[index];`                                                                         | `value = seq[index]`                                                                                                                               |
| subset                       | `arr.slice(start, end)`<br>can omit end and start and<br>can use negative indexes to count from end | `seq[start:end]`<br>can omit start and/or end and<br>can use negative indexes to count from end                                                    |
| concatenate                  | `const newArr = arr1.concat(arr2, arr3, ...);`                                                      | `newSeq = seq1 + seq2 + seq3`                                                                                                                      |
| copy (shallow)               | `[...arr]` or `arr.slice()`                                                                         | `list.copy()` - only for lists                                                                                                                     |
| find                         | `arr.find(predicate);`                                                                              | `next(filter(predicate, seq))` - see note below this table                                                                                         |
| find index                   | `arr.findIndex(predicate);`                                                                         | `index = seq.index(value, start?, end?)` - see note below this table                                                                               |
| iterate over                 | `for (const value of arr)` or<br>`arr.forEach((value, index) => { ... });`                          | `for item in seq:` or<br>`for index, item in enumerate(seq):`                                                                                      |
| iterate over in reverse      | iterate over `arr.reverse()`                                                                        | `for item in reversed(seq):`                                                                                                                       |
| iterate over in sorted order | create a sorted copy and iterate over it                                                            | `for item in sorted(seq):`                                                                                                                         |
| includes (Boolean)           | `arr.includes(value)`                                                                               | `value in seq`                                                                                                                                     |
| not includes (Boolean)       | `!arr.includes(value)`                                                                              | `value not in seq`                                                                                                                                 |
| index of                     | `arr.indexOf(value[, fromIndex])`                                                                   | `seq.index(value[, start[, end]])`                                                                                                                 |
| last index of                | `arr.lastIndexOf(value[, fromIndex])`                                                               | not built-in; have to reverse list                                                                                                                 | TODO |
| count occurrences            | `arr.reduce((acc, v) => v === value ? acc + 1 : acc, 0)`                                            | `seq.count(value)`                                                                                                                                 |
| join                         | `arr.join(delimiter)` returns string                                                                | `delimiter.join(seq)`                                                                                                                              |
| map                          | `const newArr = arr.map(value => newValue);`                                                        | `iterator = map(function, seq)`                                                                                                                    |
| filter                       | `const newArr = arr.filter(predicate);`                                                             | `iterator = filter(predicate, seq)`                                                                                                                |
| reduce                       | `const value = arr.reduce((acc, value) => { ... });`                                                | `from functools import reduce`<br>`value = reduce(lambda acc, item: ..., seq, initial)`                                                            |
| some/any (Boolean)           | `arr.some(predicate)` - short circuits                                                              | `any(map(predicate, seq))` - no short circuit                                                                                                      |
| every/all (Boolean)          | `arr.every(predicate)` - short circuits                                                             | `all(map(predicate, seq))` - no short circuit                                                                                                      |
| sort                         | `arr.sort(comparator);`<br>`comparator` is a function that compares two elements                    | `list.sort(key=k, reverse?)`<br>`k` is an attribute name or a function that takes<br>an element and returns a value to sort on                     |
| reverse                      | `arr.reverse()`                                                                                     | `list.reverse()` - only for lists                                                                                                                  |
| destructure/unpack           | `const v1, v2, v2 = arr;`<br># of variables on left can differ from # of array elements             | `v1, v2, v3 = seq`<br># of variables on left must match # of sequence elements<br>which limits usefulness                                          |

Python doesn't have a simple, built-in way to find the first item in a list
that matches some criteria. This naive approach is probably the most efficient.

```python
def index(predicate, seq):
  for index in range(0, len(seq)):
      if predicate(seq[index]):
          return index
  return None
```

The Python `filter` and `map` functions are lazy
which means they are not executed until their results are needed.
To get values from them, repeatedly pass the result to `next` or
pass the result to a function like `list` or `set` to get all the values.
For example:

```python
numbers = [1, 2, 3]

iter = map(lambda n: n * 2, numbers) # multiplies each by 2
print(next(iter)) # 2
print(next(iter)) # 4

print(list(map(lambda n: n * 2, numbers))) # [2, 4, 6]
```

The string `join` method takes an iterable over strings.
To join non-string values, use `map` and the `str` function
to convert values to strings. For example:

```python
'-'.join(map(str, numbers)) # '1-2-3'
```

JavaScript can implement lazy functions using generator functions
(see the "List comprehension" section),
but no built-in generator functions are provided.

## Sorting

Suppose we have a sequence of object that represent people and we wish to
sort the sequence on their last name following by their first name.

Here is how this can be done in JavaScript:

```js
const people = [
  {firstName: 'Tami', lastName: 'Volkmann'},
  {firstName: 'Mark', lastName: 'Volkmann'},
  {firstName: 'Brendan', lastName: 'Eich'},
  {firstName: 'Guido', lastName: 'van Rossum'}
];
people.sort((p1, p2) => {
  const compare = p1.lastName.localeCompare(p2.lastName);
  return compare ? compare : p1.firstName.localeCompare(p2.firstName);
});
console.log(people);
```

Here is how this can be done in Python:

```python
from operator import itemgetter

people = [
  {'firstName': 'Tami', 'lastName': 'Volkmann'},
  {'firstName': 'Mark', 'lastName': 'Volkmann'},
  {'firstName': 'Brendan', 'lastName': 'Eich'},
  {'firstName': 'Guido', 'lastName': 'van Rossum'}
]

# This sort is case-sensitive.
#people.sort(key=itemgetter('lastName', 'firstName'))
# This sort is case-insensitive.
getter = itemgetter('lastName', 'firstName')
person_key = lambda p: tuple(map(str.casefold, getter(p)))
people.sort(key=person_key)
print(people)
```

## List comprehensions

Python supports list comprehensions that create a list, but JavaScript does not.
Here are some examples.

```python
squares = [n**2 for n in range(5)] # [0, 1, 4, 9, 16]

multiplesOf3 = [n for n in range(10) if n % 3 == 0] # [0, 3, 6, 9]
```

JavaScript generator functions can be used to do the same thing,
but some utility generator functions must be written.

```js
function* range(n) {
  for (i = 0; i < n; i++) yield i;
}

function* map(fn, iter) {
  for (const element of iter) yield fn(element);
}

const squares = map(n => n ** 2, range(5)); // [0, 1, 4, 9, 16 ]

function* filter(predicate, obj) {
  for (const element of obj) {
    if (predicate(element)) yield element;
  }
}

const multiplesOf3 = filter(n => n % 3 === 0, range(10)); // [ 0, 3, 6, 9 ]
```

Python also supports generator functions and the `yield` keyword.
The JavaScript example above could be implemented as follows in Python:

```python
def map(fn, iter):
    for element in iter:
        yield fn(element)

squares = map(lambda n: n**2, range(5)) # [ 0, 1, 4, 9, 16 ]

def filter(predicate, seq):
    for element in seq:
        if predicate(element):
            yield element

multiplesOf3 = filter(lambda n: n % 3 == 0, range(10)) # [ 0, 3, 6, 9 ]
```

## <a name="sets">Sets</a>

Sets are unordered collections with no duplicate values.

| Operation             | JavaScript                                       | Python                                  |
| --------------------- | ------------------------------------------------ | --------------------------------------- |
| is set                | `expr instanceof Set`                            | `isinstance(expr, set)`                 |
| create                | `const s = new Set();` - cannot specify elements | `s = {elements}` or `s = set(elements)` |
| add                   | `s.add(value)`                                   | same                                    |
| remove                | `s.delete(value);`                               | `s.remove(value)`                       |
| remove all            | `s.clear()`                                      | same                                    |
| length                | `s.size`                                         | `len(s)`                                |
| includes              | `s.has(value)`                                   | `value in s`                            |
| iterate over          | `s.forEach(value => { ... });`                   | `for value in set: ...`                 |
| convert to array/list | `a = s.values();`                                | `l = list(s)`                           |

## Set comprehensions

Python supports set comprehensions that create a set, but JavaScript does not.
Here is an example.

```python
from random import randint

# Pick 10 random integers from 1 to 10
# and keep only the unique values.
# Placing the values in a set enforces unique values.
numbers = {randint(1, 11) for n in range(10)}
```

## <a name="key-value-collections">Key/value collections</a>

JavaScript uses plain objects or instances of the `Map` class
to store associations between keys and values.
Keys in JavaScript objects must be must be strings, integers, or symbols,
but keys in `Map` instances can be any type.

| Operation           | JavaScript Object                                                           | JavaScript Map                                                     |
| ------------------- | --------------------------------------------------------------------------- | ------------------------------------------------------------------ |
| is object/dict      | `typeof expr === 'object'`                                                  | `expr instanceof Map`                                              |
| create              | `const obj = {};`<br>can include initial key/value pairs                    | `const map = new Map();`<br>cannot specify initial key/value pairs |
| length              | `Object.keys(obj).length`                                                   | `map.size`                                                         |
| set value of key    | `obj.key = value` or `obj[key] = value`                                     | `map.set(key, value)`                                              |
| get value of key    | `obj.key` or `obj[key]`<br>use 2nd form if key contains special characters  | `map.get(key)`                                                     |
| get all keys        | `Object.keys(obj)`                                                          | `map.keys()`                                                       |
| get all values      | `Object.values(obj)`                                                        | `map.values()`                                                     |
| get all pairs       | `Object.entries(obj)`<br>returns array of arrays containing a key and value | `map.entries()`<br>returns the same                                |
| test if key present | `key in obj` or `obj.hasOwnProperty(key)`                                   | `map.has(key)`                                                     |
| delete pair         | `delete obj.key` or `delete obj[key]`                                       | `map.delete(key)`                                                  |
| delete all pairs    | `obj = {}`                                                                  | `map.clear()`                                                      |
| iterate over keys   | `for (const key in obj)` or<br>`for (const key of Object.keys(obj))`        | `map.forEach((value, key) => { ... });`                            |

Python uses dictionaries to store associations between keys and values.
The keys must be immutable types like strings, numbers, and tuples containing these.

| Operation               | Python                                                                                                 |
| ----------------------- | ------------------------------------------------------------------------------------------------------ |
| is dict                 | `isinstance(expr, dict)`                                                                               |
| create                  | `dict = {}`<br>`dict = {k1: v1, k2: v2, ...}`<br>`dict([(k1, v1), (k2, v2), ...])`                     |
| length                  | `len(dict)`                                                                                            |
| set value of key        | `dict[key] = value`                                                                                    |
| get value of key        | `dict[key]` or `dict.get(key)`<br>error if key doesn't exist                                           |
| get all keys            | `dict.keys()` or `list(dict)` or<br>`sorted(dict)` to get keys in sorted order                         |
| get all values          | `dict.values()`                                                                                        |
| get all pairs           | `dict.items()`<br>returns a live view that provides a<br>sequence of tuples containing a key and value |
| test if key present     | `key in dict`                                                                                          |
| test if key not present | `key not in dict`                                                                                      |
| delete pair             | `del dict[key]`                                                                                        |
| delete all pairs        | `dict.clear()`                                                                                         |
| iterate over            | `for item in dict.items(): ...`                                                                        |

Python 3.9 added the `|` and `|=` operators for merging dict objects.

### Creating a dictionary object

There are four ways to create a Python dictionary:

```python
# 1) Use the literal syntax.
person = {'name': 'Mark', 'hungry': True}
# person = {'name': 'Mark', 'hungry': True}

# 2) Pass keyword arguments to the `dict` function.
# This has the advantage that single word keys
# do not need to be enclosed in quotes.
person = dict(name='Mark', hungry=True)
# person = {'name': 'Mark', 'hungry': True}

# 3) Pass a list of key/value tuples to the `dict` function.
kv_tuples = [('name', 'Mark'), ('hungry', True)]
person = dict(kv_tuples)
# person = {'name': 'Mark', 'hungry': True}

# 4) Use a dictionary comprehension.
def get_initials(name):
  return ''.join(map(lambda s: s[0], name.split(' ')))

names = ['Mark Volkmann', 'Dorian Yeager']
my_dict = {name: get_initials(name) for name in names}
# {'Mark Volkmann': 'MV', 'Dorian Yeager': 'DY'}
```

## Regular expressions

In JavaScript, regular expressions are a built-in type.
An instance can be created in two ways:

```js
// This pattern matches Canadian postal codes.
const re = /^[A-Z]\d[A-Z] \d[A-Z]\d$/;
// Alternative
//const re = new RegExp('^[A-Z]\\d[A-Z] \\d[A-Z]\\d$');

const pc = 'A1B 2C3';
if (!re.test(pc)) console.log('not a Canadian postal code');
```

In Python, import the `re` module to use regular expressions.
Those that will be used multiple times should be compiled.
Otherwise they can be used inline.
The following example demonstrate the two ways in which
every regular expression method can be used,
calling it on the `re` module or on a compiled regular expression.

```python
import re

# This pattern matches Canadian postal codes.
# Using the string literal prefix "r" prevents the "\" character
# from being treated as an escape character inside a regular expression.
pattern = r'^[A-Z]\d[A-Z] \d[A-Z]\d$'

pc = 'A1B 2C3'

# For one-time use ...
if (not re.search(pattern, pc)):
    print('not a Canadian postal code')

# For repeated usage ...
canadianPostalCode = re.compile(pattern)
if (not canadianPostalCode.search(pc)):
    print('not a Canadian postal code')
```

| Operation                | JavaScript                                                                | Python                                                                 |
| ------------------------ | ------------------------------------------------------------------------- | ---------------------------------------------------------------------- |
| create                   | `const re = /pattern/flags` or<br>`const re = new RegExp(pattern, flags)` | `import re`<br>`regex = re.compile(pattern)`                           |
| test if a string matches | `if (re.test(str))`                                                       | `regex.search(str)`<br>returns a match object or `None` if not matched |
| get first match          | `str.match(re)`                                                           | same as above                                                          |
| get all matches          | `str.matchAll(re)` or `re.exec(str)`                                      | `regex.finditer(str)`<br>returns an iterable over match objects        |
| split string on re       | `str.split(re)`<br>returns an array of strings                            | `regex.split(str)`<br>returns a list of strings                        |

Python match objects support the following methods:

- `group()` - returns the matching string
- `start()` - returns the start index of the match (inclusive)
- `end()` - returns the end index of the match (exclusive)
- `span()` - returns a tuple containing the start and end indexes

For more information on regular expression support in Python, see the
{% aTargetBlank "https://docs.python.org/3/library/re.html",
"Python Standard Library Documentation" %}.

## Error handling

Python refers to errors as exceptions.

| Operation | JavaScript                                      | Python                                                |
| --------- | ----------------------------------------------- | ----------------------------------------------------- |
| throw     | `throw new Error(message);`                     | `raise ExClass(args)`                                 |
| catch     | `try { ... } catch (e) { ... } finally { ... }` | `try: ... except ExClass: ... else: ... finally: ...` |
| rethrow   | `throw e;`                                      | `raise e`                                             |

In JavaScript:

```js
try {
  // code to try executing
} catch (e) {
  // code to handle all exceptions
} finally {
  // optional
  // code to run at end regardless of whether an exception was thrown
}
```

In Python:

```python
try:
  # code to try executing
except ExClass1 as e:
  # code to handle a specific exception class
except (ExClass2, ExClass3):
  # code to handle other exception classes listed in a tuple
else: # optional
  # code to run if no exception was thrown
  # Placing code here means that exceptions it throws
  # will not be handled by the "except" blocks above.
finally: # optional
  # code to run at end regardless of whether an exception was thrown
```

To ignore an exception in Python,
include a `pass` statement in an `except` block.

There are many built-in Python exception classes.
The base class of all of them is `Exception`.
Names of built-in exception classes end in "Error".
For a list of them, see {% aTargetBlank
"https://docs.python.org/3/library/exceptions.html", "Built-in Exceptions" %}.

Names of custom exception classes should also end in "Error".
Here is an example of defining one:

```python
class MyCustomError(Exception):
    pass
```

## Exit with status

Both JavaScript programs running in Node.js and Python programs
can explicitly exit and set a status code.

In Node.js, the `exit` method of the `process` global object is used.

```js
process.exit(statusCode);
```

In Python, the `exit` method of the `sys` module is used.

```python
import sys

sys.exit(status_code)
```

In both cases if no status code is supplied, it defaults to zero.

## JSON

In Python, in order to use JSON methods include `import json`.

| Operation | JavaScript                                 | Python                           |
| --------- | ------------------------------------------ | -------------------------------- |
| create    | `const jsonString = JSON.stringify(expr);` | `jsonString = json.dumps(expr)`  |
| parse     | `const value = JSON.parse(jsonString);`    | `value = json.loads(jsonString)` |

## Writing and reading Files

Node.js and Python can both read and write files containing text or binary data.
For details on how this works in Node.js, see the
{% aTargetBlank "https://nodejs.org/dist/latest-v14.x/docs/api/fs.html",
"Node.js File system docs" %}.
For details on how this works in Python, see
{% aTargetBlank "https://realpython.com/read-write-files-python/",
"Reading and Writing Files in Python" %}.

One case to consider is when the file can easily fit in memory.
We will see an example using JSON that demonstrates this.
Another case is when it cannot and therefore
must be processed using streams, perhaps one line at a time.
We will see an example using CSV that demonstrates this.

### Writing and reading small files

Let's write a JSON file and then read it back in to verify that it worked.

Here is a JavaScript version:
Note that it uses top-level awaits
which requires the code to be in an ES module.
One way to satisfy this is to give the file a `.mjs` file extension.

```js
import fs from 'fs/promises';

const dog = {
  breed: 'Whippet',
  name: 'Comet'
};
const filePath = 'dog.json';

await fs.writeFile(filePath, JSON.stringify(dog));

const buffer = await fs.readFile(filePath);
const newDog = JSON.parse(buffer.toString());
console.log(newDog);
```

And here is a Python version:

```python
import json

dog = {
  "breed": 'Whippet',
  "name": 'Comet'
}
file_path = 'dog.json'

with open(file_path, 'w') as writer:
    writer.write(json.dumps(dog))

with open('x' + file_path, 'r') as reader:
    new_dog = json.loads(reader.read())
    print(new_dog)
```

### Writing and reading large files

Let's write a CSV file and then read it back in to verify that it worked.
Rather than write the entire file at once, we will write one line at a time.
Likewise we will read the file one line at a time.

Here is a JavaScript version:

```js
import fs from 'fs';
import readline from 'readline';

const filePath = 'dogs.csv';

const ws = fs.createWriteStream(filePath);
ws.write('Maisey,Treeing Walker Coonhound\n');
ws.write('Ramsay,Native American Indian Dog\n');
ws.write('Oscar Wilde,German Shorthaired Pointer\n');
ws.write('Comet,Whippet\n');
ws.close();

const rl = readline.createInterface({
  input: fs.createReadStream('dogs.csv')
});

for await (const line of rl) {
  console.log(line);
}

rl.close();
```

And here is a Python version:

```python
file_path = 'dog.csv'

with open(file_path, 'w') as writer:
    writer.write('Maisey,Treeing Walker Coonhound\n');
    writer.write('Ramsay,Native American Indian Dog\n');
    writer.write('Oscar Wilde,German Shorthaired Pointer\n');
    writer.write('Comet,Whippet\n');

with open(file_path, 'r') as reader:
    while True:
        line = reader.readline()
        if not line:
            break
        print(line, end='')
```

## Shell commands

JavaScript code running in Node.js can execute shell commands,
provide input to them, and capture output written to stdout and stderr.

In the example code below we execute the `ps` command
to get the status of all currently running processes and then
output the process id (pid) and running time of all "node" processes.

```js
import {exec} from 'child_process';
import {promisify} from 'util';
const pexec = promisify(exec);

function node_report(lines) {
  for (const line of lines) {
    const tokens = line.trimStart().split(/ +/);
    if (tokens.length >= 9) {
      const command = tokens[7];
      if (command == 'node') {
        const pid = tokens[1];
        const time = tokens[6];
        console.log('process id', pid, 'has run for', time);
      }
    }
  }
}

// Approach #1 - using a callback function
exec('ps -ef', (error, stdout, stderr) => {
  if (error || stderr) {
    console.error(error || stderr);
  } else {
    node_report(stdout.split('\n'));
  }
});

// Approach #2 - awaiting a Promise
const {stdout, stderr} = await pexec('ps -ef');
if (stderr) {
  console.error(stderr);
} else {
  node_report(stdout.split('\n'));
}
```

Here is a Python version that does the same:

```python
import os, re, subprocess

# Create a regular expression that matches two or more spaces.
spaces_re = re.compile(r' {2,}')

# Print the process id and running time of all "node" processes.
def node_report(lines):
    for line in lines:
        # Replaces all occurrences of multiple spaces with one.
        line = spaces_re.sub(' ', line.lstrip())
        tokens = line.split(' ')
        if len(tokens) >= 9:
            command = tokens[7]
            if command == 'node':
                pid = tokens[1]
                time = tokens[6]
                print('process id', pid, 'has run for', time)

# Approach #1 - reading output into a single string
stream = os.popen('ps -ef')
output = stream.read() # reads all lines into a single string
node_report(output.split('\n'))

# Approach #2 - reading output into an array of strings, one per line
stream = os.popen('ps -ef')
node_report(stream.readlines())

# Approach #3 - using a CompletedProcess instance
process = subprocess.run(['ps', '-ef'], capture_output=True, text=True)
if process.stderr:
    print('error:', process.stderr, file=sys.stderr)
else:
    node_report(process.stdout.split('\n'))
```

## Decorators

Python supports decorators which are annotations placed before
functions and classes to alter their behavior.
The TC39 committee that controls the ECMAScript standard for JavaScript
has been discussing adding decorators for many years,
but they have not yet been added.

Here is a simple example of a decorator that logs the
arguments and return value of every invocation value
of functions to which it is applied:

```python
import logging
logging.basicConfig(level=logging.DEBUG)

def log(fn):
    def wrapper(*args):
        result = fn(*args)
        logging.debug(
            f'{fn.__name__} was passed {str(args)} and returned {result}')
        return result

    return wrapper

@log
def add(n1, n2):
    return n1 + n2

add(1, 2) # DEBUG:root:add was passed (1, 2) and returned 3
add(2, 3) # DEBUG:root:add was passed (2, 3) and returned 5
```

The built-in Python decorators include:

- `@classmethod`  
  This transforms a method into a class method
  which receives a class object as its first parameter
  and can use it to access class state.
- `@property`  
  This defines getter, setter, and deleter methods
  for a class instance property.
- `@staticmethod`  
  This transforms a method into a static method
  which does not receive a class object as its first parameter
  and cannot access class state.
  It is useful for utility functions.

For more information, see {% aTargetBlank
"https://realpython.com/primer-on-python-decorators/#decorating-classes",
"Real Python Primer on Python Decorators" %}.

## Check for running as main

Some source files can be used as both the starting point of a script
and a module imported by others.
To include code that is only run when the file is executed as a script,
wrap it in one of the following `if` statements.

In Node.js, use `if (require.main === module) { ... }`

In Python, use `if __name__ == '__main__': ...`

## HTTP servers

HTTP servers can be implemented in both Node.js and Python.
In Node.js, a popular option is to use the
{% aTargetBlank "https://expressjs.com/", "Express" %} package.
In Python, popular options include
{% aTargetBlank "https://flask.palletsprojects.com/", "Flask" %} and
{% aTargetBlank "https://fastapi.tiangolo.com/", "FastAPI" %}.

To demonstrate these options, we will implement HTTP servers that:

- serve static files in a "public" directory
- implement REST services that provide CRUD operations
  on a collection of dogs

The collection of dogs could be persisted to a database,
but we will just hold them in memory in a key/value collection
where the keys are dog ids and the values are
dog objects that have id, breed, and name properties.

We want the servers to:

- provide request logging
- support cross-origin resource sharing (CORS) so web apps
  that are served from a different domain can invoke them
- handle `GET /dog` requests by returning all the dogs as JSON
- handle `POST /dog` requests by adding the dog described in the request body
- handle `PUT /dog/id` requests by updating the dog with the given id
  using the data in the request body
- handle `DELETE /dog/id` requests by deleting the dog with the given id

### JavaScript Express REST server

1. Create a directory for the project and cd to it.
1. Create a `package.json` file for the project by
   entering `npm init` and answering the questions it asks.
1. Install the required dependencies by entering
   `npm install cors express pino-http` and
   `npm install -D nodemon`.
1. Replace the "test" script in `package.json` with
   `"start": "nodemon server.js"`.
   The `nodemon` command provides automatic file watch and server restart.
1. Create the file `server.js` containing the following:

   ```js
   const express = require('express');
   const cors = require('cors');
   const path = require('path');
   const pino = require('pino-http')();

   const dogs = {
     1: {id: 1, breed: 'Whippet', name: 'Comet'}
   };

   const app = express();
   app.use(pino); // for logging
   app.use(cors()); // for cross-origin resource sharing
   app.use(express.json()); // to parse JSON bodies

   // Serve static files found in the public directory.
   app.use(express.static(path.resolve(__dirname, 'public')));

   app.get('/dog', (req, res) => {
     res.end(JSON.stringify(dogs));
   });

   app.get('/dog/:id', (req, res) => {
     const {id} = req.params;
     const dog = dogs[id];
     if (dog) {
       res.end(JSON.stringify(dog));
     } else {
       res.status(404).end('dog not found');
     }
   });

   app.post('/dog', (req, res) => {
     const dog = req.body;
     dog.id = Date.now();
     dogs[dog.id] = dog;
     res.end(String(dog.id));
   });

   app.put('/dog/:id', (req, res) => {
     const {id} = req.params;
     if (dogs[id]) {
       const dog = req.body;
       dog.id = id;
       dogs[id] = dog;
       res.end();
     } else {
       res.status(404).end('dog not found');
     }
   });

   app.delete('/dog/:id', (req, res) => {
     const {id} = req.params;
     if (dogs[id]) {
       delete dogs[id];
       res.end();
     } else {
       res.status(404).end('dog not found');
     }
   });

   const PORT = 1919;
   app.listen(PORT, () => console.log('ready'));
   ```

1. Run the server by entering `npm start`.

### Python Flask REST server

Key benefits of
{% aTargetBlank "https://flask.palletsprojects.com/", "Flask" %} are:

- ability to implement REST APIs
- server-side HTML generation
- provided request logging
- provided file watch and server restart

1. Install the required dependencies by entering `pip install flask flask-cors`.
   {% aTargetBlank "https://flask-cors.readthedocs.io/en/latest/", "Flask-CORS" %}
   supports enabling CORS in Flask servers.
1. If running in a UNIX environment, create the script file `start` shown below
   and make it executable by entering `chmod a+x start`:

   ```bash
   #!/usr/local/bin/bash
   export FLASK_APP=server.py
   export FLASK_ENV=development
   flask run --port=1919
   ```

   Setting `FLASK_ENV` to `development`
   provides automatic file watch and server restart.

   If running in Windows, create a similar `start.bat` file.

1. Create the file `server.py` containing the following:

   ```py
   from flask import Flask, abort, request
   from flask_cors import CORS

   import time

   # Serve static files found in the public directory.
   app = Flask(__name__, static_folder='public', static_url_path='')
   CORS(app) # for cross-origin resource sharing

   dogs = {
       1: {
           'id': 1, 'breed': 'Whippet', 'name': 'Comet'
       }
   }

   @app.route('/dog')
   def all_dogs():
       return dogs

   @app.route('/dog', methods=['POST'])
   def create_dog():
       dog = request.get_json() # from body
       print('dog =', dog)
       id = round(time.time() * 1000)
       print('id =', id)
       dog['id'] = id
       dogs[id] = dog
       return str(id)

   @app.route('/dog/<id>', methods=['PUT'])
   def update_dog(id):
       id = int(id)
       if id in dogs:
           dog = request.get_json() # from body
           dog['id'] = id
           dogs[id] = dog
           return ''
       else:
           abort(404)

   @app.route('/dog/<id>', methods=['DELETE'])
   def delete_dog(id):
       id = int(id)
       if id in dogs:
           del dogs[id]
           return ''
       else:
           abort(404)
   ```

1. Run the server by entering `./start`.

### Python FastAPI REST server

Key benefits of
{% aTargetBlank "https://fastapi.tiangolo.com/", "FastAPI" %} are:

- type validation of request/response bodies and path/query parameters
- automatic generation of Open API documentation
- provided request logging
- provided CORS middleware
- provided file watch and server restart

1. Install the required dependencies by entering
   `pip install fastapi pydantic uvicorn`.

1. Create the file `server.py` containing the following:

   ```py
   from fastapi import FastAPI
   from fastapi.middleware.cors import CORSMiddleware
   from pydantic import BaseModel
   from typing import Optional
   import time

   # JSON in request bodies of POST and PUT requests
   # is validated against this type definition.
   # When validation fails, the response status
   # is set to 422 Unprocessable Entity.
   class Dog(BaseModel):
       id: Optional[int] = None
       breed: str
       name: str

   dogs = {
       1: {
           'id': 1, 'breed': 'Whippet', 'name': 'Comet'
       }
   }

   app = FastAPI()
   app.add_middleware(CORSMiddleware, allow_origins='*')

   @app.get('/dog')
   def all_dogs():
       return dogs

   @app.post('/dog', response_model=str)
   def create_dog(dog: Dog):
       id = round(time.time() * 1000)
       #dog['id'] = id # Why can't the dog object be modified?
       dict = dog.dict()
       dict['id'] = id
       dogs[id] = dict
       return str(id)

   @app.put('/dog/{id}', response_model=str)
   def update_dog(dog: Dog, id: int):
       if id in dogs:
           #dog['id'] = id # Why can't the dog object be modified?
           dict = dog.dict()
           dict['id'] = id
           dogs[id] = dict
           return ''
       else:
           abort(404)

   @app.delete('/dog/{id}')
   def delete_dog(id: int):
       id = int(id)
       if id in dogs:
           del dogs[id]
           return ''
       else:
           abort(404)
   ```

1. Run the server by entering `uvicorn server:app --port 1919 --reload`.
   Including the `--reload` option provides
   automatic file watch and server restart.

1. Check out the Open API docs that are provided for free!
   Browse localhost:1919/docs to see API documentation
   and try each API from the browser.

## HTTP clients

JavaScript applications often use the Fetch API to send HTTP requests.
This is built into modern web browsers and
can be used in Node applications by installing the `node-fetch` package
with `npm install node-fetch`.

Here is an example were we fetch an image of a given dog breed.

```js
const fetch = require('node-fetch');

const breed = 'whippet';
const url = `https://dog.ceo/api/breed/${breed}/images/random/1`;

async function doIt() {
  try {
    const res = await fetch(url);
    if (!res.ok) throw new Error(await res.text());
    const obj = await res.json();
    const [imageUrl] = obj.message; // get first array element
    console.log('image url =', imageUrl);
  } catch (e) {
    console.error(e);
  }
}

doIt();
```

Python applications often use the
{% aTargetBlank "https://requests.readthedocs.io/en/master/", "`requests`" %}
package to send HTTP requests.
It can be installed by entering `pip install requests`.

Here is the same example using Python.

```python
import requests
from requests.exceptions import ConnectionError
import sys

breed = 'whippet'
url = f'https://dog.ceo/api/breed/{breed}/images/random/1'

try:
    res = requests.get(url)
    if res.status_code != 200:
        raise Exception('bad status ' + str(res.status_code))

    obj = res.json() # a dict
    image_url = obj['message'][0] # get first array element
    print('image url =', image_url)
except ConnectionError as e:
    print('failed to connect to', url)
except Exception as e:
    print(e)
```

## <a name="magic-methods">Python magic methods</a>

Python magic methods support many operations on classes and class instances.
These include operator overloading.
The following table provides a categorized, partial list of
the magic methods that a Python class can be implement.

| Method                                | Parameters       | Purpose                                                                        |
| ------------------------------------- | ---------------- | ------------------------------------------------------------------------------ |
| **Object Lifecycle**                  |
| `__new__`                             | cls, ...         | creates a new object                                                           |
| `__init__`                            | self, ...        | initializes a new object                                                       |
| `__del__`                             | self             | destroys an object                                                             |
| `__getattr__`                         | self, name       | can be used to implement the "method missing" pattern<br>(see example below)   |
| **String Representation**             |                  |                                                                                |
| `__repr__`                            | self             | returns a string representations useful to developers                          |
| `__str__`                             | self             | returns a string representation useful to users                                |
| **Comparisons**                       |                  |                                                                                |
| `__eq__`                              | self, other      | determines if this object is equal to another                                  |
| `__ne__`                              | self, other      | determines if this object is not equal to another                              |
| `__lt__`                              | self, other      | determines if this object is < another                                         |
| `__le__`                              | self, other      | determines if this object is <= to another                                     |
| `__gt__`                              | self, other      | determines if this object is > another                                         |
| `__ge__`                              | self, other      | determines if this object is >= to another                                     |
| also see `functools.total_ordering()` |
| **List-like Operations**              |
| `__getitem__`                         | self, key        | gets an item from a list by index                                              |
| `__setitem__`                         | self, key, value | sets an item in a list by index                                                |
| `__delitem__`                         | self, key        | deletes an item from a list by index                                           |
| `__iter__`                            | self             | returns an iterator                                                            |
| `__contains__`                        | self, item       | determines if a given item is contained                                        |
| **Dict Operations**                   |
| `__missing__`                         | self, key        | returns value to use when key is not present<br>class must inherit from `dict` |
| **Math Operations**                   |
| `__add__`                             | self, other      | adds an object to another                                                      |
| `__sub__`                             | self, other      | subtracts an object from another                                               |
| `__mul__`                             | self, other      | multiplies an object by another                                                |
| `__div__`                             | self, other      | divides an object by another                                                   |
| `__mod__`                             | self, other      | mods an object by another                                                      |
| **Pickling (serialization)**          |
| `__getstate__`                        | self             | pickles an object                                                              |
| `__setstate__`                        | self             | unpickles an object                                                            |
| **Other**                             |
| `__call__`                            | self, ...        | treats an object as a function; can change state                               |

The "method missing" pattern supports calling non-existent methods
on an object and inferring meaning at runtime.
This is frequently used in the "Ruby on Rails" framework
in the implementation of "Active Record".

For example, suppose we want to implement a class whose objects
support methods whose names begin with "add" and end with a number.
These methods accept another number as an argument
and return the sum of the numbers.
This could be implemented as follows:

```python
class MethodMissingDemo:

    def __getattr__(self, method_name):
        prefix = 'add'
        if method_name.startswith(prefix):
            n1 = int(method_name[len(prefix):])
            return lambda n2: n1 + n2
        else:
            class_name = self.__class__.__name__
            raise AttributeError(f"{class_name} object has no method '{method_name}'")

demo = MethodMissingDemo()
print(demo.add3(4)) # 7
print(demo.add19(1)) # 20
print(demo.subtract5(7)) # AttributeError: MethodMissingDemo object has no method 'subtract5'
```

In most cases, using normal methods and parameters
instead of the `__getattr__` method results in
code that is easier to understand and maintain.

JavaScript can do something similar using "proxies".

```js
const demo = new Proxy(
  {},
  {
    get(object, methodName) {
      const prefix = 'add';
      if (methodName.startsWith(prefix)) {
        n1 = Number(methodName.substring(prefix.length));
        return n2 => n1 + n2;
      } else {
        throw new ReferenceError('object has no method ' + methodName);
      }
    }
  }
);

console.log(demo.add3(4)); // 7
console.log(demo.add20(1)); // 20
console.log(demo.subtract5(7)); // ReferenceError: object has no method subtract5
```

## Types

To gain type checking for JavaScript, use the TypeScript compiler.
TypeScript is a superset of JavaScript that adds types.

Two popular tools that provide type checking on Python source files
are {% aTargetBlank "http://mypy-lang.org/", "mypy" %} and
{% aTargetBlank "https://github.com/microsoft/pyright", "Pyright" %}.

The remainder of this section focuses on Python type checking.

Python type specifications are referred to as "type hints".
The `python` interpreter ignores type hints,
but they make startup time take slightly longer.
They are useful as documentation even without using a type checker.
IDEs can use them to flag type issues.

The primitive types supported are:

| Type Name | Meaning                                              |
| --------- | ---------------------------------------------------- |
| `bool`    | Boolean                                              |
| `bytes`   | immutable sequence of integers from 0 to 255         |
| `complex` | complex number with `float` real and imaginary parts |
| `float`   | double precision floating point number               |
| `int`     | unlimited precision integer                          |
| `str`     | string                                               |

The collection types supported are:

| Type Name            | Meaning                                               |
| -------------------- | ----------------------------------------------------- |
| `Dict[KT, VT]`       | dict with keys of type KT and values of type VT       |
| `List[T]`            | list with elements of type T                          |
| `Sequence[T]`        | any kind of sequence whose elements are all of type T |
| `Set[T]`             | set with elements of type T                           |
| `Tuple[T1, T2, ...]` | tuple whose elements have specified types             |

Other types supported are:

| Type Name                                           | Meaning                                                                 |
| --------------------------------------------------- | ----------------------------------------------------------------------- |
| `Any`                                               | any value                                                               |
| any class name                                      | instance of the class or instance of a subclass                         |
| `Callable[[P1, P2, ...], RT]`                       | function that takes parameters of types P1, P2, ... and returns type RT |
| `Callable[..., RT]`                                 | function that takes any parameters and returns type RT                  |
| `Generator[YieldType, SendType, ReturnType]`        | generator function;<br>`SendType` and `ReturnType` can be `None`        |
| `NamedType('Name', [('name1', T1), ('name2', T2)])` | named tuple where elements have types T1, T2, ...                       |
| `Optional[T]`                                       | matches `None` or the type T<br>same as `Union[None, T]`                |
| `Type[C]`                                           | matches a class object for class C or a subclass                        |
| `Union[T1, T2, ...]`                                | matches any of the specified types                                      |

All types above whose names begin with a capital letter
must be imported from the "typing" module.
For example, `from typing import Any, List`.
Python 3.9 is supposed to make this unnecessary,
but perhaps mypy does not yet support the new type syntax.

The `Union` type can be used in collection types
to allow elements to have a set of types.

Aliases can be defined for long type descriptions.
This is useful when the same type description is used in many places.
For example, `IntToStrMap = Dict[int, str]`.

To add a "type hint" to a variable or function parameter,
follow its name with a colon, a space, and the type.

To add a return type hint to a function,
follow the argument list right parenthesis with `->` and the type.

For example, if `IceCream` is a class we have defined:

```python
def order_ice_cream(flavor: str, scoops: int, add_sprinkles: bool) -> IceCream:
```

### mypy

{% aTargetBlank "http://mypy-lang.org/", "mypy" %}
is a Python type checking tool that is implemented in Python.
Development began in 2014.

To install mypy, enter `pip install mypy`.
On a Mac, add the following directory to the `PATH` environment variable:
`/Library/Frameworks/Python.framework/Versions/3.8/bin`.

To run mypy on a source file and all the files it imports,
enter `mypy {filename}.py`.

mypy cannot perform type checking on function arguments that correspond to
parameters with default values or
parameters that collect variadic arguments in a tuple or dict.

### Pyright

{% aTargetBlank "https://github.com/microsoft/pyright", "Pyright" %}
is a Python type checking tool that is implemented in TypeScript.
Development began in 2019.
It is used by the VS Code extension Pylance
and can also be run from the command line.

To install Pyright, install Node.js and enter `npm install -g pyright`.

To run Pyright on a source file and all the files it imports,
enter `pyright {filename}.py`.
To run in watch mode, enter `pyright -w`.
See {% aTargetBlank
"https://github.com/microsoft/pyright/issues/330", "this issue" %}.

To configure options for Pyright, create a `pyrightconfig.json` file
in the project root directory. For example, the following content
configures strict type checking for all files in the current directory.

```json
{
  "strict": ["."]
}
```

In answer to the question "What is the long-term plan for Pyright?"
the Pyright FAQ says:

"Pyright is a side project with no dedicated team.
There is no guarantee of continued development on the project.
If you find it useful, feel free to use it and contribute to the code base."

### Stub files

Types can be specified in "stub files" with a `.pyi` extension
instead of directly in `.py` files.

Stub files for popular Python libraries can be downloaded from
{% aTargetBlank "https://github.com/python/typeshed", "typeshed" %}.
These are included as a submodule of mypy.
See the typeshed link for instructions on installing them.

Note that the number of libraries represented here is currently small
and it does not contain stub files for many popular libraries
including mathplotlib, numpy, and pandas.
Type stub files for matplotlib, numpy, and pandas can be found at
{% aTargetBlank "https://pypi.org/project/data-science-types/",
"data-science-types" %}.

When creating your own stub files, create `.pyi` files
with the same names as the `.py` files whose types they describe.
Stub files can be placed in the same directory as the module definition
or in a separate directory with a name like "stubs".
If they are in a separate directory,
define the `MYPYPATH` environment variable to point to the directory.

If a stub file for a module is present,
its type definitions override any found in the corresponding `.py` file.

To define the type of a variable in a stub file,
use the syntax `name: type`.

To define types for a function in a stub file,
use the syntax `def name(p1: type1, p2: type2, ...) -> return_type: ...`.
Note that the `...` is actual syntax.

A stub files can be automatically generated from `.py` files
using the `stubgen` command that is installed with mypy.
By default it places the generated stub files in a subdirectory named `out`,
but this can be changes using the `-o` option.
For example, `stubgen -o . math.py`
generates `math.pyi` in the current directory.
Many of the types will be `Any`, so manual editing of the
generated files is desirable to make the types more strict.

Here is a example module in `math.py` that omits type hints:

```python
PI = 3.1415928

def add(n1, n2):
    return n1 + n2
```

Here is a stub file in `math.pyi` that provides type hints:

```python
PI: float

def add(n1: float, n2: float) -> float: ...
```

Here is code that uses this module:

```python
from math import PI, add

print(add('2', PI))
```

Running the `mypy` command reports ...

```text
demo.py:3: error: Argument 1 to "add" has incompatible type "str"; expected "float"
Found 1 error in 1 file (checked 1 source file)
```

## Testing

See [Python testing](/blog/python/python-testing/).

## Popular tools/libraries/frameworks

| Topic            | JavaScript                          | Python                                        |
| ---------------- | ----------------------------------- | --------------------------------------------- |
| command line     | Node.js `node` command              | `python` command                              |
| utilities        | Lodash, Ramda                       | pydash                                        |
| web server       | Express                             | Flask, FastAPI                                |
| web framework    | React, Svelte, Vue                  | Flask                                         |
| dates and times  | date.fns, Moment.js, Temporal       | datetime (in standard library)                |
| linting          | ESLint                              | pylint, flake8                                |
| unit tests       | Jest, Mocha, Chai, @testing-library | pytest, unittest (in standard library), nose2 |
| end-to-end tests | Cypress                             | same                                          |
| math             | mathjs                              | math (in standard library)                    |
| popular editors  | VS Code                             | VS Code, PyCharm (JetBrains)                  |

## Linting

To lint JavaScript using {% aTargetBlank "https://eslint.org/", "ESLint" %}:

- `npm install eslint eslint-plugin-import`
- `eslint *.js`

To configure the rules used by ESLint,
create the file `.eslintrc.json` in project directories
or in a user home directory.

For example:

```json
{
  "env": {
    "browser": true,
    "es6": true,
    "jest": true,
    "node": true
  },
  "extends": ["eslint:recommended", "plugin:import/recommended"],
  "parserOptions": {
    "ecmaVersion": 2019,
    "sourceType": "module"
  },
  "plugins": ["import"]
}
```

To lint Python code using
{% aTargetBlank "https://www.pylint.org/", "pylint" %}:

- `pip install pylint`
- `pylint *.py`

To configure the rules used by pylint,
create the project file `.pylintrc` or a file in a user home directory
(`~/.pylintrc` or `~/.config/pylintrc`).
To generate a commented version of this file
in order learn about the format and available options,
enter `pylint --generate-rcfile > {file-path}`.
This file can then be modified or just used for reference.
For example:

```text
[MESSAGES CONTROL]
disable=
    invalid-name,
    missing-function-docstring,
    missing-module-docstring,
    redefined-outer-name,
    too-few-public-methods
```

In addition, rules can be disable in source files by adding special comments.
For example:

```python
# This rule treats all module-level variables as constants.
# invalid-name: Constant name doesn't conform to UPPER_CASE naming style
# pylint: disable=C0103

# global-statement: Using the global statement
# pylint: disable=W0603
```

To lint Python code using
{% aTargetBlank "https://flake8.pycqa.org/", "flake8" %}:

- `pip install flake8`
- `flake8`

To configure the rules used by flake8,
create the project file `.flake8` or a file in a user home directory
(`~/.flake8` for Windows and `~/.config/flake8` for macOS and Linux).
For example:

```text
[flake8]
ignore = E261, E265, E302, E305, E731
```

## VS Code

The VS Code extension "Python" from Microsoft adds
"IntelliSense, linting, debugging, code navigation, code formatting,
Jupyter notebook support, refactoring, variable explorer, test explorer,
snippets, and more".

Other useful VS Code extensions for Python code include
autopep8 and pylint.

## Resources

### JavaScript resources

- {% aTargetBlank "https://nodejs.org/dist/latest-v14.x/docs/api/", "Node API docs" %}
- {% aTargetBlank "https://npmjs.com/", "npm" %}
- {% aTargetBlank "https://developer.mozilla.org/en-US/", "Mozilla Developer Network" %}

### Python resources

- {% aTargetBlank "https://www.python.org/", "Python home page" %}
- {% aTargetBlank "https://docs.python.org/3/tutorial/", "Python Tutorial" %}
- {% aTargetBlank "https://docs.python.org/3/library/", "Python Standard Library" %}
- {% aTargetBlank "https://docs.python.org/3/reference/", "Python Language Reference" %}
- {% aTargetBlank "https://docs.python.org/3/howto/", "Python HOWTOs" %}
- {% aTargetBlank "https://docs.python.org/3/faq/", "Python FAQ" %}
- {% aTargetBlank "https://realpython.com/start-here/", "Real Python" %}
- {% aTargetBlank "https://www.tutorialspoint.com/python/", "tutororialspoint Learn Python 3" %}
