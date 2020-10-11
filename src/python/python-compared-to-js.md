---
eleventyNavigation:
  key: Python Compared to JavaScript
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
| most common indentation | 2 spaces                                                                                                  | 4 spaces                                                                                                                               |
| type coercion           | implicit                                                                                                  | explicit except between number types                                                                                                   |

Note: PEP stands for Python Enhancement Proposal.

Some oddities about Python code formatting include:

- leaving two blank lines before and after every function and class definition
- leaving two spaces before every comment that appears on the same line as code

## Pros and Cons

### JavaScript

pros:

- ability to run in web browsers (clients) and from command-line (servers)
- great support for asynchronous code
- performance
- compact syntax for functional programming (ex. functools vs. `reduce`)
- can use TypeScript, a superset of JavaScript, to add type checking

cons:

- still in transition from require to import syntax in Node.js
- type coercions can result in surprising results if not familiar with them

### Python

pros:

- targeted at scripting and rapid application development
- quantity and maturity of libraries for machine learning
- multiple number types
- some syntax is easier for beginners
  - no curly braces or semicolons, and fewer parentheses
  - ex. `and` vs. `&&`.
  - ex. `print` vs. `console.log`
- can add functions implemented in C/C++ or any language callable from C
- can use type hints and tools like mypy to add type checking

cons:

- poor performance -
  For one example of benchmark results, see {% aTargetBlank
  "https://benchmarksgame-team.pages.debian.net/benchmarksgame/fastest/python.html",
  "The Computer Language Benchmark Game" %}).
  Python does well with regular expressions.
- magic methods such as `__init__` use "dunder" names (for double underscore)
  which is an odd and verbose way to distinguish special values
  (Other programming languages typically use a single special character prefix.
  See a list in the "Python Magic Methods" section.)
- use of operator overloading (supported by magic methods) can be confusing
- anonymous functions are limited to a single expression
- no built-in support for asynchronous code
  until the asyncio module was added in Python 3.4
  (some features require Python 3.7+)
- single-word dictionary keys require quotes unlike JavaScript object keys
- lambda functions are more verbose than JavaScript arrow functions
  (lambda vs. ->)
- even though Python convention is to separate words in multi-word
  variable, function, and method names with an underscore,
  there are many examples where no separate is used
- the classic ternary operator using a `?` and `:` is not supported;
  for example:

  ```python
  name = len(sys.argv) > 1 ? sys.argv[1] : 'World' # not supported
  name = sys.argv[1] || 'World' # doesn't work

  # The Python version of ternary does work.
  name = sys.argv[1] if len(sys.argv) > 1 else 'World'
  ```

- lots of documentation and examples are still for V2 instead of V3

## Running Scripts

JavaScript source files have an extension of `.js`
or `.mjs` (for ECMAScript modules).

To run a JavaScript script outside a web browser:

- install {% aTargetBlank "https://nodejs.org/", "Node.js" %}.
- enter `node {name}.js`

Python source files have an extension of `.py`.
Multiple words in file names should be separated by underscores
rather than hyphens because the file name becomes the module name
and hyphens are not valid in module names.

To run a Python script:

- install the Python interpreter from
  {% aTargetBlank "https://www.python.org/downloads/", "python.org" %}
- enter `python {name}.py`

In both cases, command-line arguments can be passed to the script.

To make a Python source file directly executable in UNIX systems:

- Add this as the first line: `#!/usr/bin/env python3`
- Make the file executable by entering `chmod a+x {file-name}.py`
- To run it from a terminal, enter `./{name}.py`

To automatically restart a script when it or a file it imports is modified:

1. Install Node.js.
1. `npm install -g nodemon`
1. If running a JavaScript script, enter `nodemon {name}.js`
1. If running a Python script, enter `nodemon --exec python3 {name}.py`.

## Getting Help

For Python, see the list of resources at the end.
You can also enter the `python` command to start the REPL and enter `help`.
To get help on a particular library, import it and pass the name to help.
For example:

```python
import re
help(re)
```

In JavaScript, perform web searches that begin with "MDN"
(for the Mozilla Developer Network) followed by a JavaScript search term.
For example, "mdn regexp".

## Comments

| Type        | JavaScript  | Python |
| ----------- | ----------- | ------ |
| single-line | `//`        | `#`    |
| multi-line  | `/* ... */` | none   |

## Naming Conventions

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

## Builtin Types

| Type                 | JavaScript                                           | Python                                                                  |
| -------------------- | ---------------------------------------------------- | ----------------------------------------------------------------------- |
| boolean              | `true`, `false`                                      | `True`, `False`                                                         |
| number               | default is double precision float, `BigInt`          | `int`, `float`, `complex`                                               |
| character            | use strings                                          | same                                                                    |
| string               | `'text'` or `"text"`                                 | same                                                                    |
| multi-line string    | `` `text` ``                                         | `"""text"""` or `'''text'''`                                            |
| string interpolation | `` `prefix${expr1}suffix${expr2}` ``                 | `f'prefix{expr1}suffix{expr2}'`                                         |
| array                | `Array` class, literal syntax `[v1, v2, ...]`        | `array` module                                                          |
| list                 | use `Array` type                                     | literal syntax `[v1, v2, ...]`<br>mutable and typically homogeneous     |
| tuple                | no equivalent                                        | literal syntax `(v1, v2, ...)`<br>immutable and typically heterogeneous |
| range                | no equivalent                                        | `range(start, stop[, step])`                                            |
| key/value pairs      | Object in the form `{k1: v1, k2: v2, ...}` and `Map` | dictionary (a.k.a. dict) literal syntax<br>`{'k1': v1, 'k2': v2, ...}`  |
| set                  | `new Set()`                                          | literal syntax `{v1, v2, ...}`<br>or `set(v1, v2, ...)`                 |
| function             | see "Functions" section below                        | see "Functions" section below                                           |
| class                | `class Name { ... }`<br>see "Classes" section below  | `class Name:`<br>see "Classes" section below                            |
| no value             | `undefined` or `null`                                | `None`                                                                  |

Everything is an object in Python, even values that are
primitives in JavaScript like booleans, numbers, and strings.

Python has "sequences" whereas JavaScript has arrays.
There are three kinds of sequences: list, tuple, and range.
A list is a mutable sequence of values that typically have the same type.
A tuple is an immutable sequence of values that can have varying types.
A range is an immutable sequence of numbers that is often be used for looping.

JavaScript object keys must be strings.
Python dict keys can be any immutable type.

In Python, the following values are treated as false when used
in a boolean context: `False`, `None`, `0`, empty strings, and empty sequences.

In JavaScript, the following values are treated as false when used
in a boolean context: `false`, `0`, empty strings, `undefined`, and `null`.

## Modules

In both JavaScript and Python, modules are defined by a single source file.
A source file can import other modules and those can import more modules.

In both JavaScript and Python each module is only imported once.
If its code is modified, the script must be re-run to interpret the changes.

Python searches for modules in this order

- built-in modules
- directory relative to importing file (using dotted module names)
- directories listed in the `PYTHONPATH` environment variable
- installation-specific directories

To see the directories that will be searched,
`import sys` and execute `print(sys.path)`.

| Topic                    | JavaScript                                                                                              | Python                                                                                               |
| ------------------------ | ------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| define                   | content of file                                                                                         | same                                                                                                 |
| export                   | `export name = value;`                                                                                  | everything is automatically exported;<br>indicate private values by starting name with an underscore |
| default export           | `export default name = value;`                                                                          | not supported                                                                                        |
| import default           | `import name from 'path';`                                                                              | not supported                                                                                        |
| import entire module     | `const name from 'modname';`                                                                            | `import modname` or <br>`import modname as other` or<br>`from modname import *`                      |
| import specific values   | `const {name1, name2} from 'modname';` or<br>`const {name1 as other1, name2 as other2} from 'modname';` | `from modname import name1, name2` or<br>`from modname import name1 as other1, name2 as other2`      |
| import default and named | `import name, {name1, name2} from 'path';`                                                              | not supported                                                                                        |
| open source catalog      | {% aTargetBlank "https://www.npmjs.com/", "https://www.npmjs.com/" %}                                   | {% aTargetBlank "https://pypi.org/", "https://pypi.org/" %}                                          |
| tool to install          | `npm` (installed with Node.js)                                                                          | `pip` (installed with Python)                                                                        |

In Python it is common to assign aliases to commonly used packages.
The community has landed on using the aliases shown in the imports below.

| Package                            | Recommended Import                     |
| ---------------------------------- | -------------------------------------- |
| collections<br>in standard library | `import collections as co`             |
| functools<br>in standard library   | `import functools as ft`               |
| itertools<br>in standard library   | `import itertools as it`               |
| matplotlib                         | `from matplotlib import pyplot as plt` |
| NumPy                              | `import numpy as np`                   |
| pandas                             | `import pandas as pd`                  |

## Packages

### JavaScript Packages

JavaScript "packages" are managed using the `npm` tool
which is install with Node.js is installed.
To allow each project to use different versions of packages
and make it easy for other developers to install the same set of packages,
create a `package.json` file in each project
by entering `npm init` and answering some questions.

### Python Packages

Python "packages" are managed using the `pip` tool
which is installed when Python is installed.
The name is an acronym for "Pip Installs Packages".
It installs packages from the
{% aTargetBlank "https://pypi.org/", "Python Package Index" %}.
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
tools and libraries found in the `env` directory instead of global ones.
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

### Package Comparisons

| Operation                                      | JavaScript                                                                         | Python                                                                       |
| ---------------------------------------------- | ---------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| prepare a project                              | `npm init [-y]`                                                                    | `python -m venv env`<br>where `env` is the directory name used by convention |
| install a package globally                     | `npm install -g {pkg-name}`                                                        | with no environment activated,<br>`pip install {pkg-name}`                   |
| install a package locally                      | `npm install {pkg-name}`                                                           | with an environment activated,<br>`pip install {pkg-name}`                   |
| install a specific version of a package        | `npm install {pkg-name}@{version}`                                                 | `pip install {pkg-name}=={version}`                                          |
| update to latest version of a specific package | `npm update {pkg-name}@{version}`                                                  | `pip install --upgrade {pkg-name}`                                           |
| see where global packages are installed        | `npm -g root`                                                                      | with no environment activated,<br>`pip list -v`                              |
| see where local packages are installed         | `npm root`                                                                         | with an environment activated,<br>`pip list -v`                              |
| location of local packages                     | `{project-dir}/node_modules`                                                       | `{project-dir}/lib/python{version}/site-packages`                            |
| see list of locally installed packages         | `npm ls` or<br>open `package.json` and see<br>`dependencies` and `devDependencies` | with environment activated, `pip list`                                       |
| create list of project package versions        | automatically maintained in `package.json`                                         | `pip freeze > requirements.txt`                                              |
| install project package versions               | `npm install`                                                                      | `pip install -r requirements.txt`                                            |

### Project Python Packages

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
"Python Tutorial" %}.

## Printing

| Operation                              | JavaScript                                                                                  | Python                                                |
| -------------------------------------- | ------------------------------------------------------------------------------------------- | ----------------------------------------------------- |
| print space-separated values to stdout | `console.log(v1, v2, ...);`                                                                 | `print(v1, v2, ..)`                                   |
| print space-separated values to stderr | `console.error(v1, v2, ...);`                                                               | `import sys`<br>`print(v1, v2, ..., file=sys.stderr)` |
| print with interpolation               | `` console.log(`Hello ${name}, today is ${dayOfWeek}.`); ``                                 | `print(f'Hello {name}, today is {dayOfWeek}.')`       |
| print without newline                  | in Node.js<br>`const process = require('process');`<br>`process.stdout.write(v1, v2, ...);` | `print(v1, v2, ..., end='')`                          |

## Variables and Assignment

JavaScript variables should be declared
using either the `const` or `let` keyword.
Python variables are not declared and
are created when a value is assigned to them.

JavaScript variable assignments can appear inside expressions
such as an `if` or loop condition, which many developers find confusing.
This is not allowed in Python.

The pylint Python linting tool treats module-level variables as constants.
It will output warnings if functions modify their values.
To avoid this, list all such variables to be modified after
the `global` keyword inside functions that modify them.

| Topic                         | JavaScript                                                                     | Python                                                    |
| ----------------------------- | ------------------------------------------------------------------------------ | --------------------------------------------------------- |
| constant declaration          | `const NAME = value;`                                                          | `NAME = value`                                            |
| variable declaration          | `let name = value;`                                                            | `name = value`                                            |
| get type of value in variable | `typeof name` and `name.constructor.name`                                      | `type name`                                               |
| multiple assignment           | `const [a, b] = [1, 2]`                                                        | `a, b = 1, 2`                                             |
| destructure of array/list     | `const [v1, v2, ...] = array;`<br># of variables can differ from # of elements | `v1, v2 = seq`<br># of variables must match # of elements |
| destructure of object         | `const {k1, k2, ...} = object;`                                                | not supported                                             |
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

Python comparisons can be chained, but JavaScript comparison cannot.
For example, to determine whether the value of a variable
is between 10 and 20 inclusive:

- in JavaScript, `10 <= n && n <= 20`
- in Python, `10 <= n <= 20`

## Conditional Logic

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

## Iteration

As we will see in the "Key/Value Collections" section later,
JavaScript can store key/value pairs in plain objects
or in instances of the `Map` class.
Python uses "dictionaries" (or dicts) to store key/value pairs.

| Topic                            | JavaScript                                                           | Python                                    |
| -------------------------------- | -------------------------------------------------------------------- | ----------------------------------------- |
| classic for loop                 | `for (let value = initial; cond; statements)`                        | `for value in range(start, stop, step?):` |
| over collection                  | `for (const value of iterable)`                                      | `for value in sequence:`                  |
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
  "Python Tutorial" %} and
{% aTargetBlank
  "https://www.python.org/dev/peps/pep-0008/#documentation-strings",
  "PEP-8 documentation strings" %}.

Neither JavaScript nor Python support function overloading
where the same function name can be defined multiple times
with different numbers and/or types of arguments.

| Topic                                                               | JavaScript                                                                           | Python                                                                                                              |
| ------------------------------------------------------------------- | ------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------- |
| define named                                                        | `function fnName(params) { ... }`                                                    | `def fnName(params): ...`                                                                                           |
| define anonymous                                                    | `const fnName = (params) => definition`                                              | `lambda params: expression`                                                                                         |
| define anonymous w/ single parameter                                | `const fnName = param => {...}`                                                      | same as above                                                                                                       |
| define anonymous w/ single expression                               | `const fnName = (params) => expr`                                                    | same as above                                                                                                       |
| specify default parameter values                                    | `function fnName(p1=v1, p2=v2) {...}`                                                | `def fnName(p1=v1, p2=v2): ...`                                                                                     |
| gather variable number of arguments                                 | `function fnName(p1, p2, ...rest) {...}`<br>`rest` is set to an `Array`              | `def fnName(p1, p2, *rest): ...`<br>`rest` is set to a tuple                                                        |
| gather arguments as key/value pairs                                 | not supported                                                                        | `def fnName(**args): ...`<br>call with `fnName(p1=v2, p2=v2)`<br>or `fnName(**dict)`                                |
| use named/keyword arguments                                         | `function fnName({p1, p2}) {...}`<br>pass an object                                  | same as above;<br>any parameter can be specified by name;<br>important feature!<br>call with `fnName(p1=v2, p2=v2)` |
| return a value                                                      | `return value;`                                                                      | `return value`                                                                                                      |
| default return value when no `return`                               | `undefined`                                                                          | `None`                                                                                                              |
| call                                                                | `fnName(args)`                                                                       | same                                                                                                                |
| get required argument count                                         | `fnName.length`                                                                      | `from inspect import getfullargspec`<br>`len(getfullargspec(fn).args)`                                              |
| passing fewer arguments than positional parameters                  | remaining are assigned `undefined`                                                   | results in an error                                                                                                 |
| passing more arguments than positional parameters with no gathering | all arguments are available in `arguments` array-like object                         | results in an error                                                                                                 |
| get name                                                            | `fnName.name`                                                                        | `fnName.__name__`                                                                                                   |
| get implementation code                                             | `fnName.toString()`                                                                  | `from inspect import getsource`<br>`getsource(fn)`                                                                  |
| create partial                                                      | `const newFn = fnName.bind(thisValue, arg1, arg2, ...)`<br>`thisValue` can be `null` | `from functools import partial`<br>`newFn = partial(fn, arg1, arg2, ...)`                                           |
| call                                                                | `fnName.call(thisValue, arg1, arg2, ...)`<br>`thisValue` can be `null`               | `class.method(obj, arg1, arg2, ...)`                                                                                |
| apply                                                               | `fnName.apply(thisValue, argArray)`<br>`thisValue` can be `null`                     | `class.method(obj, *argList)`                                                                                       |
| spread array to positional arguments                                | `fnName(...arr)`                                                                     | `fnName(*seq)`                                                                                                      |
| spread object to keyword arguments                                  | not supported                                                                        | `fnName(**dict)`                                                                                                    |

In Python:

- Function parameters with a default value must follows those without one.
- Function parameters listed after one that begins with `*`
  must be specified by name.
- The `partial` function (shown in the table above)
  can only be used on functions, not methods of a class.

## Asynchronous Functions

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
| define                            | `class Name { ... }`                                                      | `class Name: ...`                                                     |
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
from functools import reduce

class Statistics:
    def __init__(self, *numbers):
        self.numbers = list(numbers)
        self.min = min(*self.numbers)
        self.max = max(*self.numbers)
        self.sum = reduce(lambda acc, n: acc + n, self.numbers, 0)

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

Here is a function takes a class and prints its inheritance hierarchy:

```python
def print_inheritance(cls, level = 0):
    print(' ' * 2 * level, cls.__name__)
    for base in cls.__bases__:
        print_class_tree(base, level + 1)
```

## Boolean Operations

| Operation   | JavaScript | Python      |
| ----------- | ---------- | ----------- |
| and         | `b1 && b2` | `b1 and b2` |
| or          | `b1 || b2` | `b1 or b2`  |
| not         | `!b`       | `not b`     |
| bitwise and | `b1 & b2`  | same        |
| bitwise or  | `b1 | b2`  | same        |
| bitwise not | `~b`       | same        |
| bitwise xor | `b1 ^ b2`  | same        |

## Numeric Operations

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

## String Operations

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
| starts with         | `s.startsWith(sub)` returns boolean             | `s.startswith(sub)` returns boolean                                                |
| ends with           | `s.endsWith(sub)` returns boolean               | `s.endswith(sub)` returns boolean                                                  |
| contains            | `s.includes(sub)` returns boolean               | `sub in s` returns boolean                                                         |
| index of            | `s.indexOf(sub)` returns number                 | `s.index(sub, start?, end?)` returns int                                           |
| last index of       | `s.lastIndexOf(sub)` returns number             | `s.rindex(sub, start?, end?)` returns int                                          |
| compare             | `s1.localeCompare(s2)` returns -1, 0, or 1      | `import locale`<br>`locale.strcoll(s1, s2)`<br>returns negative, zero, or positive |
| replace first       | `s.replace(oldSub, newSub)`                     | `s.replace(old, new, 1)`                                                           |
| replace all         | `s.replaceAll(oldSub, newSub)`                  | `s.replace(old, new)`                                                              |
| trim start          | `s.trimStart()`                                 | `s.lstrip()`                                                                       |
| trim end            | `s.trimEnd()`                                   | `s.rstrip()`                                                                       |
| trim both ends      | `s.trim()`                                      | `s.strip()`                                                                        |
| repeat n times      | `s.repeat(n)`                                   | `s * n` or `n * s`                                                                 |

## Sequences

JavaScript stores sequences of values in arrays.
Python primarily uses the three sequence types list, tuple, and range
for this purpose.

Python lists are mutable and are typically homogeneous
(elements have the same type).
Python tuples are immutable and are typically heterogeneous
(elements can have different types).
Python ranges are immutable sequences of numbers
and are often used in `for` loops.

To create a JavaScript array:

```js
const myArray = [element1, element2, ...];
```

To create a Python list:

```python
myList = [element1, element2, ...]
```

To create a Python tuple:

```python
# Parentheses around a tuple are optional.
myTuple = (element1, element2, ...)
```

To create a Python range:

```python
myRange = range(end) # 0 to end-1
myRange = range(start, end, step?) # start to end-1 where step defaults to 1
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

| Operation                    | JavaScript                                                                                          | Python                                                                                                                         |
| ---------------------------- | --------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| is array/sequence            | `Array.isArray(expr)`                                                                               | `hasattr(type(expr), '\_\_iter\_\_')`<br>`isinstance(expr, list)`<br>`isinstance(expr, tuple)`<br>`isinstance(expr, range)`    |
| add to end                   | `arr.push(v1, v2, ...);`                                                                            | `seq.append(v)` to add one and<br>`seq.extend(iterable)` to add more than one                                                  |
| remove from end              | `const value = arr.pop();`                                                                          | `value = seq.pop()`                                                                                                            |
| add to start                 | `arr.unshift(value);`                                                                               | `seq.insert(0, value)`                                                                                                         |
| remove from start            | `const value = arr.shift();`                                                                        | `del seq[0]`                                                                                                                   |
| insert                       | `arr.splice(index, delCount, v1, v2, ...)`                                                          | `seq.insert(index, value)`                                                                                                     |
| remove item at index         | `arr.splice(index, 1)`                                                                              | `del seq[index]` - only for lists                                                                                              |
| remove items at index range  | `arr.splice(start, count)`                                                                          | `del seq[start:start+count]` - only for lists                                                                                  |
| remove value                 | `arr.splice(arr.findIndex(value), 1)`                                                               | `seq.remove(value)` - error if not found                                                                                       |
| remove all                   | `arr = [];`                                                                                         | `seq.clear()`                                                                                                                  |
| change                       | `arr.splice(start, delCount, v1, v2, ...);`                                                         | combine `del` and `insert` above                                                                                               |
| length                       | `arr.length`                                                                                        | `len(seq)`                                                                                                                     |
| lookup                       | `const value = arr[index];`                                                                         | `value = seq[index]`                                                                                                           |
| subset                       | `arr.slice(start, end)`<br>can omit end and start and<br>can use negative indexes to count from end | `seq[start:end]`<br>can omit start and/or end and<br>can use negative indexes to count from end                                |
| concatenate                  | `const newArr = arr1.concat(arr2, arr3, ...);`                                                      | `newSeq = seq1 + seq2 + seq3`                                                                                                  |
| copy (shallow)               | `[...arr]` or `arr.slice()`                                                                         | `list.copy()` - only for lists                                                                                                 |
| find                         | `arr.find(predicate);`                                                                              | `next(filter(predicate, seq))` - see note below this table                                                                     |
| find index                   | `arr.findIndex(predicate);`                                                                         | `index = seq.index(value, start?, end?)` - see note below this table                                                           |
| iterate over                 | `for (const value of arr)` or<br>`arr.forEach((value, index) => { ... });`                          | `for item in seq:` or<br>`for index, item in enumerate(seq):`                                                                  |
| iterate over in reverse      | iterate over `arr.reverse()`                                                                        | `for item in reversed(seq):`                                                                                                   |
| iterate over in sorted order | create a sorted copy and iterate over it                                                            | `for item in sorted(seq):`                                                                                                     |
| includes (boolean)           | `arr.includes(value)`                                                                               | `value in seq`                                                                                                                 |
| not includes (boolean)       | `!arr.includes(value)`                                                                              | `value not in seq`                                                                                                             |
| index of                     | `arr.indexOf(value[, fromIndex])`                                                                   | `seq.index(value[, start[, end]])`                                                                                             |
| last index of                | `arr.lastIndexOf(value[, fromIndex])`                                                               | not builtin; have to reverse list                                                                                              | TODO |
| count occurrences            | `arr.reduce((acc, v) => v === value ? acc + 1 : acc, 0)`                                            | `seq.count(value)`                                                                                                             |
| join                         | `arr.join(delimiter)` returns string                                                                | `delimiter.join(seq)`                                                                                                          |
| map                          | `const newArr = arr.map(value => newValue);`                                                        | `iterator = map(function, seq)`                                                                                                |
| filter                       | `const newArr = arr.filter(predicate);`                                                             | `iterator = filter(predicate, seq)`                                                                                            |
| reduce                       | `const value = arr.reduce((acc, value) => { ... });`                                                | `from functools import reduce`<br>`value = reduce(lambda acc, item: ..., seq, initial)`                                        |
| some/any (boolean)           | `arr.some(predicate)` - short circuits                                                              | `any(map(predicate, seq))` - no short circuit                                                                                  |
| every/all (boolean)          | `arr.every(predicate)` - short circuits                                                             | `all(map(predicate, seq))` - no short circuit                                                                                  |
| sort                         | `arr.sort(comparator);`<br>`comparator` is a function that compares two elements                    | `list.sort(key=k, reverse?)`<br>`k` is an attribute name or a function that takes<br>an element and returns a value to sort on |
| reverse                      | `arr.reverse()`                                                                                     | `list.reverse()` - only for lists                                                                                              |
| destructure/unpack           | `const v1, v2, v2 = arr;`<br># of variables on left can differ from # of array elements             | `v1, v2, v3 = seq`<br># of variables on left must match # of sequence elements<br>which limits usefulness                      |

Python doesn't have a simple, builtin way to find the first item in a list
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

iter = map(lambda n: n * 2, numbers)
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
(see the "List Comprehension" section),
but no builtin generator functions are provided.

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

# Issue: This sorts lowercase last names like "van Rossum" to the end!
# TODO: What is the easiest fix for this?
people.sort(key=itemgetter('lastName', 'firstName'))
print(people)
```

## List Comprehensions

Python supports list comprehensions that create a list, but JavaScript does not.
Here are some examples.

```python
squares = [n**2 for n in range(5)] # [0, 1, 4, 9, 16]

multipleOf3 = [n for n in range(10) if n % 3 == 0] # [0, 3, 6, 9]
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

const squared = map(n => n ** 2, range(5)); // [0, 1, 4, 9, 16 ]

function* filter(predicate, obj) {
  for (const element of obj) {
    if (predicate(element)) yield element;
  }
}

const multipleOf3 = filter(n => n % 3 === 0, range(10)); // [ 0, 3, 6, 9 ]
```

Python also supports generator functions and the `yield` keyword.
The JavaScript example above could be implemented as follows in Python:

```python
def map(fn, iter):
    for element in iter:
        yield fn(element)

squared = map(lambda n: n**2, range(5)) # [ 0, 1, 4, 9, 16 ]
squared = [n**2 for n in range(5)] # same using list comprehension

def filter(predicate, seq):
    for element in seq:
        if predicate(element):
            yield element

multipleOf3 = filter(lambda n: n % 3 == 0, range(10)) # [ 0, 3, 6, 9 ]
multipleOf3 = [n for n in range(10) if n % 3 == 0] # same using list comprehension
```

## Sets

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

## Set Comprehensions

Python supports set comprehensions that create a set, but JavaScript does not.
Here is an example.

```python
from random import randint

# Pick 10 random integers from 1 to 10
# and keep only the unique values.
# Placing the values in a set enforces unique values.
numbers = {randint(1, 11) for n in range(10)}
```

## Key/Value Collections

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

## Regular Expressions

In JavaScript, regular expressions are a builtin type.
An instance can be created in two ways:

```js
// This pattern matches Canadian postal codes.
const re = /^[A-Z]\d[A-Z] \d[A-Z]\d$/;
// Alternative
//const re = new RegExp('^[A-Z]\\d[A-Z] \\d[A-Z]\\d$');

const pc = 'A1B 2C3';
if (!re.test(pc)) console.log('not a Canadian postal code');
```

In Python, import the `re` library to use regular expressions.
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

Python match objects have the following methods:

- `group()` - returns the matching string
- `start()` - returns the start index of the match (inclusive)
- `end()` - returns the end index of the match (exclusive)
- `span()` - returns a tuple containing the start and end indexes

For more information on regular expression support in Python, see the
{% aTargetBlank "https://docs.python.org/3/library/re.html",
"Python Standard Library Documentation" %}.

## Error Handling

Python refers to errors as exceptions.

| Operation | JavaScript                                      | Python                         |
| --------- | ----------------------------------------------- | ------------------------------ |
| throw     | `throw new Error(message);`                     | `raise ExClass(args)`          |
| catch     | `try { ... } catch (e) { ... } finally { ... }` | `try: ... except ExClass: ...` |
| rethrow   | `throw e;`                                      | `raise e`                      |

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
except ExClass1:
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

There are many builtin Python exception classes.
The base class of all of them is `Exception`.
Names of builtin exception classes end in "Error".
Custom exception classes should do the same.

## JSON

In Python, in order to use JSON methods include `import json`.

| Operation | JavaScript                                 | Python                           |
| --------- | ------------------------------------------ | -------------------------------- |
| create    | `const jsonString = JSON.stringify(expr);` | `jsonString = json.dumps(expr)`  |
| parse     | `const value = JSON.parse(jsonString);`    | `value = json.loads(jsonString)` |

## Writing and Reading Files

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

### Writing and Reading Small Files

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

### Writing and Reading Large Files

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

The builtin Python decorators include:

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

## HTTP Servers

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

### JavaScript Express REST Server

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

### Python Flask REST Server

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

### Python FastAPI REST Server

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

## HTTP Clients

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

    obj = res.json()
    print('is dict?', type(obj) is dict)
    #image_url = obj.message[0] # TODO: Why doesn't this work?
    image_url = obj['message'][0] # get first array element
    print('image url =', image_url)
except ConnectionError as e:
    print('failed to connect to', url)
except Exception as e:
    print(e)
```

## Python Magic Methods

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
            n1 = int(method_name[len(prefix)])
            return lambda n2: n1 + n2
        else:
            class_name = self.__class__.__name__
            raise AttributeError(f"{class_name} object has no method '{method_name}'")

demo = MethodMissingDemo()
print(demo.add3(4)) # 7
print(demo.add4(1)) # 5
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
console.log(demo.add4(1)); // 5
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
| `Callable[...], RT]`                                | function that takes any parameters and returns type RT                  |
| `Generator[YieldType, SendType, ReturnType]`        | generator function;<br>`SendType` and `ReturnType` can be `None`        |
| `NamedType('Name', [('name1', T1), ('name2', T2)])` | named tuple where elements have times T1, T2, ...                       |
| `Optional[T]`                                       | matches `None` or the type T<br>same as `Union[None, T]`                |
| `Type[C]`                                           | matches a class object for class C or a subclass                        |
| `Union[T1, T2, ...]`                                | matches any of the specified types                                      |

All types above whose names begin with a capital letter
must be imported from the "typing" module.
For example, `from typing import Any, List`.

The `Union` type can be used in collection types
to allow elements to have one of a set of types.

Aliases can be defined for long type descriptions.
This is useful when the same type description is used in many places.
For example, `IntToStringMap = Dict[int, str]`.

To add a "type hint" to a variable or function parameter,
follow its name with a colon, a space, and the type.

To add a return type hint to a function,
follow the argument list right parenthesis with `->` and the type.

For example, if `IceCream` is a class we have defined:

```python
def order_ice_cream(flavor: str, scoops: int, add_sprinkles: bool) -> IceCream:
```

### mypy

{% aTargetBlank "http://mypy-lang.org/", "mypy" %} is implemented in Python.
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
is implemented in TypeScript. Development began in 2019.
It is used by the VS Code extension Pylance
and can also be run from the command-line.

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

### Stub Files

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

Packages that support implementing tests for Python code include
pytest, codecov, and coverage (Jordan Walker uses these).

## Popular Tools/Libraries/Frameworks

| Topic            | JavaScript                          | Python                                        |
| ---------------- | ----------------------------------- | --------------------------------------------- |
| command-line     | Node.js `node` command              | `python` command                              |
| utilities        | Lodash, Ramda                       | pydash                                        |
| web server       | Express                             | Flask                                         |
| web framework    | React, Svelte, Vue                  | Flask                                         |
| dates and times  | date.fns, Moment.js, Temporal       | datetime (in standard library)                |
| unit tests       | Jest, Mocha, Chai, @testing-library | unittest (in standard library), nose2, pytest |
| end-to-end tests | Cypress                             | same                                          |
| math             | mathjs                              | math (in standard library)                    |

## VS Code

The VS Code extension "Python" from Microsoft adds
"IntelliSense, linting, debugging, code navigation, code formatting,
Jupyter notebook support, refactoring, variable explorer, test explorer,
snippets, and more".

Other useful VS Code extensions for Python code include
autopep8 and pylint.

## Resources

### JavaScript Resources

- {% aTargetBlank "https://nodejs.org/dist/latest-v14.x/docs/api/", "Node API docs" %}
- {% aTargetBlank "https://npmjs.com/", "npm" %}
- {% aTargetBlank "https://developer.mozilla.org/en-US/", "Mozilla Developer Network" %}

### Python Resources

- {% aTargetBlank "https://www.python.org/", "Python home page" %}
- {% aTargetBlank "https://docs.python.org/3/tutorial/", "Python Tutorial" %}
- {% aTargetBlank "https://docs.python.org/3/library/", "Python Standard Library" %}
- {% aTargetBlank "https://docs.python.org/3/reference/", "Python Language Reference" %}
- {% aTargetBlank "https://docs.python.org/3/howto/", "Python HOWTOs" %}
- {% aTargetBlank "https://docs.python.org/3/faq/", "Python FAQ" %}
- {% aTargetBlank "https://realpython.com/start-here/", "Real Python" %}
