---
eleventyNavigation:
  key: Python Compared to JavaScript
  parent: Python
layout: topic-layout.njk
---

This compares the most commonly used features of Python and JavaScript.
Lesser used features are omitted.

## Overview

| Topic                   | Python                                                                                                                                 | JavaScript                                                                                                |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| standard                | {% aTargetBlank "https://docs.python.org/3/", "Python 3 documentation" %}                                                              | {% aTargetBlank "https://www.ecma-international.org/publications/standards/Ecma-262.htm", "ECMAScript" %} |
| evaluation              | dynamic                                                                                                                                | dynamic                                                                                                   |
| performance             | slow                                                                                                                                   | fast                                                                                                      |
| style guide             | {% aTargetBlank "https://www.python.org/dev/peps/pep-0008/", "PEP 8" %}, {% aTargetBlank "https://pypi.org/project/black/", "Black" %} | {% aTargetBlank "https://prettier.io/", "Prettier" %}                                                     |
| most common indentation | 4 spaces                                                                                                                               | 2 spaces                                                                                                  |
| type coercion           | must be explicit                                                                                                                       | implicit                                                                                                  |

Once source of performance benchmarks can be found at
{% aTargetBlank
"https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fastest.html",
"The Computer Language Benchmarks Game" %}.

## Pros and Cons

### JavaScript

pros:

- performance
- ability to run in web browsers (clients) and from command-line (servers)
- great support for asynchronous code
- more compact syntax for functional programming (ex. functools vs. `reduce`)

cons:

- still in transition from require to import syntax in Node.js
- type coercions can result in surprising results if not familiar with them

### Python

pros:

- targeted at scripting and rapid application development
- quantity and maturity of libraries for machine learning
- multiple number types
- some syntax is easier for beginners
  - ex. `and` vs. `&&`.
  - ex. `print` vs. `console.log`
  - fewer parentheses and no curly braces or semicolons

cons:

- poor performance
- magic methods (a.k.a. "dunder" for double underscore) such as `__init__`
  (see list in "Python Magic Methods" section)
- operator overloading (supported by magic methods)
- lots of documentation and examples are still for V2 instead of V3
- anonymous functions are limited to a single expression
- no built-in support for asynchronous code
  until the asyncio module was added in Python 3.4
  (some features require Python 3.7+)

## Getting Help

In Python, enter the `python` command to start the REPL and enter `help`.
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

| Type        | Python | JavaScript |
| ----------- | ------ | ---------- |
| single-line | #      | //         |
| multi-line  | none   | /\* \*/    |

## Types

| Type                 | Python                                             | JavaScript                                           |
| -------------------- | -------------------------------------------------- | ---------------------------------------------------- |
| boolean              | `True`, `False`                                    | `true`, `false`                                      |
| number               | `int`, `float`, `complex`                          | default is double precision float; also `BigInt`     |
| character            | use string type                                    | use string type                                      |
| string               | 'text', "text", '''text''', or """text"""          | 'text' or "text"                                     |
| string interpolation | f'prefix{expr}suffix'                              | \`prefix\${expr}suffix\`                             |
| array                | see list, tuple, and range                         | `Array`, `[v1, v2, ...]`                             |
| list                 | `[v1, v2, ...]`; mutable and homogeneous           | see `Array`                                          |
| tuple                | `(v1, v2, ...)`; immutable and heterogeneous       | no equivalent                                        |
| range                | range range(start, stop[, step])                   | no equivalent                                        |
| key/value pairs      | dictionary in the form `{'k1': v1, 'k2': v2, ...}` | Object in the form `{k1: v1, k2: v2, ...}` and `Map` |
| set                  | `set(v1, v2, ...)` or `{v1, v2, ...}`              | `new Set()`                                          |
| function             | see "Function" section below                       | see "Function" section below                         |
| class                | `class Name:`                                      | `class Name { ... }`                                 |
| regular expression   | `re.compile(pattern)`                              | `/pattern/flags` or `new RegExp(pattern)`            |
| no value             | `None`                                             | `undefined` or `null`                                |

In Python, the following values are treated as false when used
in a boolean context: False, None, 0, '', and empty sequences.

In JavaScript, the following values are treated as false when used
in a boolean context: false, 0, '', undefined, null.

Python has sequences whereas JavaScript has arrays.
There are three kinds of sequences: list, tuple, and range.
A list is a mutable sequence of values that have the same type.
A tuple is an immutable sequence of values that have varying types.
A range is an immutable sequence of numbers that can be used for looping.

JavaScript object keys must be strings.
Python dict keys can e any immutable type.

## Variables and Assignment

| Topic    | Python         | JavaScript            |
| -------- | -------------- | --------------------- |
| constant | `NAME = value` | `const NAME = value;` |
| variable | `name = value` | `let name = value;`   |

Python uses a naming convention (all uppercase) to identify constants,
but they can still be modified.

## More Assignments

| Topic                | Python                      | JavaScript                      |
| -------------------- | --------------------------- | ------------------------------- |
| spread of array/list | `v1, v2 = array`            | `const [v1, v2, ...] = array;`  |
| spread of object     | not supported               | `const {k1, k2, ...} = object;` |
| addition             | `name += expr`              | same                            |
| subtraction          | `name -= expr`              | same                            |
| multiplication       | `name \*= expr`             | same                            |
| division             | `name /= expr`              | same                            |
| exponentiation       | `name \*\*= expr`           | same                            |
| mod (remainder)      | `name %= expr`              | same                            |
| logical and          | not supported               | `name &&= expr`                 |
| logical or           | not supported               | `name \|\|= expr`               |
| logical xor          | not supported               | `name ^= expr`                  |
| bitwise and          | `name &= expr`              | same                            |
| bitwise or           | `name \|= expr`             | same                            |
| bitwise xor          | `name ^= expr`              | same                            |
| signed bit shift     | `<<=` (left), `>>=` (right) | same                            |
| unsigned bit shift   | not supported               | `<<<=` (left), `>>>=` (right)   |

## Comparison

| Topic                 | Python   | JavaScript                              |
| --------------------- | -------- | --------------------------------------- |
| equal for non-objects | `==`     | `==` (with coercion) or `===` (without) |
| equal of objects      | `is`     | `===`                                   |
| not equal of objects  | `is not` | `!==`                                   |
| not equal             | `!=`     | `!=` (with coercion) or `!==` (without) |
| less than             | `<`      | same                                    |
| less than or equal    | `<=`     | same                                    |
| greater than          | `>`      | same                                    |
| greater than or equal | `>=`     | same                                    |

## Conditional Logic

| Topic   | Python                              | JavaScript                                   |
| ------- | ----------------------------------- | -------------------------------------------- |
| if      | `if cond:`                          | `if (cond) stmtOrBlock`                      |
| if/else | `if cond: else:`                    | `if (cond) { trueBlock } else { falseBlock}` |
| ternary | `trueValue if cond else falseValue` | `cond ? trueValue : falseValue`              |

## Iteration

Python "dictionaries" (or dicts) are used to store key/value pairs.
In JavaScript this is done with plain objects
or the `Map` class (described later).

| Topic                            | Python                                   | JavaScript                                                           |
| -------------------------------- | ---------------------------------------- | -------------------------------------------------------------------- |
| classic                          | `for var in range(start, stop[, step]):` | `for (let var = initial; cond; statements)`                          |
| over collection                  | `for value in sequence:`                 | `for (const value of iterable)`                                      |
| over object/dict keys            | `for key in dict.keys():`                | `for (const key of Object.keys(obj))`<br>or `for (const key in obj)` |
| over object/dict values          | `for value in dict.values():`            | `for (const value of Object.values(obj))`                            |
| over object/dict keys and values | `for key, value in dict.items():`        | `for (const [key, value] of Object.entries(obj))`                    |
| top-tested                       | `while cond:`                            | `while (cond)`                                                       |
| bottom-tested                    | `while True: ... if !cond: break`        | `do { ... } while (cond);`                                           |

## Functions

| Topic                       | Python                                                                 | JavaScript                              |
| --------------------------- | ---------------------------------------------------------------------- | --------------------------------------- |
| named definition            | `def name(params):`                                                    | `function name(params) { definition }`  |
| anonymous definition        | `lambda params: expression`                                            | `const name = (params) => definition`   |
| anonymous single parameter  | same as above                                                          | `const name = param => { ... }`         |
| anonymous single expression | same as above                                                          | `const name = (params) => expr`         |
| variable arguments          | `def name(p1, p2, \*rest):`                                            | `function name(p1, p2, ...rest) { ...}` |
| return type                 | not specified; return a single value<br>that can be an object or array | same as Python                          |
| calling                     | `name(args)`                                                           | `name(args)`                            |

Note that unlike JavaScript arrow functions, Python lambdas
can only use a single expression, not a block of code.

## Classes

| Topic                             | Python                                                  | JavaScript                                 |
| --------------------------------- | ------------------------------------------------------- | ------------------------------------------ |
| defining                          | `class Name:`                                           | `class Name { ... }`                       |
| inheritance                       | `class Sub(Super1, Super2, ...)`                        | `class Sub extends Super { ... }`          |
| constructor                       | `def \_\_init\_\_(self, params):`                       | `constructor(params) { ... }`              |
| instance property declaration     | not declared; set in \_\_init\_\_ on `self`             | not declared; set in constructor on `this` |
| instance property reference       | `self.propName`                                         | `this.propName`                            |
| class/static property declaration | `propName = value;`                                     | `static propName = value;`                 |
| class/static property reference   | `CName.propName` or `instance.propName`                 | `CName.propName`                           |
| instance method                   | `def name(params):`                                     | `name(params) { ... }`                     |
| class/static method declaration   | `@staticmethod`<br>`def methodName(params):`            | `static methodName(params) { ... }`        |
| class/static method call          | `CName.methodName(params)` or `inst.methodName(params)` | `CName.methodName(params)`                 |
| instantiating                     | `object = CName(args)`                                  | `const object = new CName(args);`          |

JavaScript does not support multiple inheritance, but Python does.
In addition to the `@staticmethod` decorator, Python also supports the
`@classmethod` decorator. The difference is that methods defined with
the latter are passed the class as the first argument.

## Asynchronous Operations

In Python 3.4+, asynchronous functions are supported by the asyncio library.

| Topic                    | Python                      | JavaScript                               |
| ------------------------ | --------------------------- | ---------------------------------------- |
| async named function     | `async def name(params):`   | `async function name(params) { ... }`    |
| async anonymous function | not supported               | `const name = async (params) => { ... }` |
| async call with await    | `result = await name(args)` | `const result = await name(args);`       |
| async call with then     | n/a                         | `name(args).then(result => { ... });`    |

In JavaScript, async functions return a Promise.
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

In Python 3.4, the `asyncio` library was added.
It can be used to create coroutines which are similar to JavaScript Promises.
See {% aTargetBlank "https://docs.python.org/3.8/library/asyncio.html", "asyncio docs" %}
Python doesn't seem to have to equivalent of
the JavaScript Promise methods `then` and `catch`.

Here is an implementation of the previous JavaScript example in Python.
It produces the same output.

```python
import asyncio

async def doIt(name, sleepMs):
    print('starting', name)
    await asyncio.sleep(sleepMs / 1000)
    print('ending', name)

async def main():
    task1 = asyncio.create_task(doIt('alpha', 3000))
    task2 = asyncio.create_task(doIt('beta', 2000))
    task3 = asyncio.create_task(doIt('gamma', 1000))
    await asyncio.gather(task1, task2, task2)
    print('finished')

asyncio.run(main())
```

## Modules

| Topic          | Python                               | JavaScript                                 |
| -------------- | ------------------------------------ | ------------------------------------------ |
| defining       | content of file                      | content of file                            |
| export         | everything is automatically exported | `export name = value;`                     |
| default export | not supported                        | `export default name = value;`             |
| import default | not supported                        | `import name from 'path';`                 |
| import named   | from moduleName import name1, name2  | `import {name1, name2} from 'path';`       |
| import both    | n/a                                  | `import name, {name1, name2} from 'path';` |
| where to find  | pip                                  | npm                                        |

## Boolean Operations

| Operation   | Python      | JavaScript   |
| ----------- | ----------- | ------------ |
| and         | `b1 and b2` | `b1 && b2`   |
| or          | `b1 or b2`  | `b1 \|\| b2` |
| not         | `not b`     | `!b`         |
| bitwise and | `b1 & b2`   | same         |
| bitwise or  | `b1 \| b2`  | same         |
| bitwise not | `~b`        | same         |
| bitwise xor | `b1 & b2`   | `b1 ^ b2`    |

## Numeric Operations

| Operation                                     | Python               | JavaScript                         |
| --------------------------------------------- | -------------------- | ---------------------------------- |
| basic                                         | `+`, `-`, `\*`, `/`  | same                               |
| exponentiation                                | `*\*`                | same                               |
| increment                                     | `v += 1`             | `++n1` (pre) or `n1++` (post)      |
| decrement                                     | `v -= 1`             | `--n1` (pre) or `n1--` (post)      |
| mod (remainder)                               | `%`                  | same                               |
| convert to string                             | `str(n)`             | `n.toString()`                     |
| convert to string with fixed decimals (ex. 2) | `"{:.2f}".format(n)` | `n.toFixed(2)`                     |
| convert to hex                                | `hex(n)`             | `n.toString(16)`                   |
| convert from hex                              | `int(hexString, 16)` | `parseInt(hexString, 16)`          |
| constants                                     | see math module      | see Math and Number global objects |
| functions                                     | see math module      | see Math and Number global objects |

## String Operations

| Operation     | Python                                      | JavaScript                                      |
| ------------- | ------------------------------------------- | ----------------------------------------------- |
| concatenation | `s1 + str(n1)`                              | `s1 + n1`                                       |
| lowercase     | `s.lower()`                                 | `s.toLowerCase()`                               |
| uppercase     | `s.upper()`                                 | `s.toUpperCase()`                               |
| substring     | `s[start:end]` or `s[start:]` or `s[:end]`  | `s1.substring(start[, end])`                    |
| slice         | same as above                               | like `substring`, but supports negative indexes |
| split         | `s.split(delimiter)` returns list           | `s.split(delimiter)` returns array              |
| starts with   | `s.startswith(sub)` returns boolean         | `s.startsWith(sub)` returns boolean             |
| ends with     | `s.endswith(sub)` returns boolean           | `s.endsWith(sub)` returns boolean               |
| contains      | `sub in s` returns boolean                  | `s.includes(sub)` returns boolean               |
| index of      | `s.index(sub[, start[, end]])` returns int  | `s.indexOf(sub)` returns number                 |
| last index of | `s.rindex(sub[, start[, end]])` returns int | `s.lastIndexOf(sub)` returns number             |
| compare       | not supported                               | `s.localeCompare(sub)` returns -1, 0, or 1      |
| replace first | `s.replace(old, new, 1)`                    | `s.replace(oldSub, newSub)`                     |
| replace all   | `s.replace(old, new)`                       | `s.replaceAll(oldSub, newSub)`                  |
| trim start    | `s.lstrip()`                                | `s.trimStart()`                                 |
| trim end      | `s.rstrip()`                                | `s.trimEnd()`                                   |
| trim both     | `s.strip()`                                 | `s.trim()`                                      |

## Array/Sequence Operations

Some Python sequence operations apply to all three of kinds of sequences
(list, tuple, and range).

| Operation         | Python                                                                                  | JavaScript                                           |
| ----------------- | --------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| is array/sequence | `hasattr(type(obj), '\_\_iter\_\_')`                                                    | `Array.isArray(expression)`                          |
| length            | `len(seq)`                                                                              | `arr.length`                                         |
| lookup            | `value = seq[index]`                                                                    | `const value = arr[index];`                          |
| subset            | `newSeq = seq[startIndex:endIndex]`                                                     | `const newArr = arr.slice(startIndex[, endIndex]);`  |
| concat            | `newSeq = seq1 + seq2`                                                                  | `const newArr = arr1.concat(arr2, arr3, ...);`       |
| find              | `next(filter(predicate, iterable))`                                                     | `const value = arr.find(predicate);`                 |
| find index        | see note below this table                                                               | `const index = arr.findIndex(predicate);`            |
| for each          | `for item in seq:`                                                                      | `arr.forEach(value => { ... });`                     |
| includes          | `value in seq`                                                                          | `arr.includes(value)` returns boolean                |
| not includes      | `value not in seq`                                                                      | `!arr.includes(value)` returns boolean               |
| index of          | `seq.index(value[, start[, end]])`                                                      | `const index = arr.indexOf(value[, fromIndex])`      |
| last index of     | not builtin; have to reverse list                                                       | `const index = arr.lastIndexOf(value[, fromIndex])`  |
| join              | `delimiter.join(iterable)`                                                              | `arr.join(delimiter)` returns string                 |
| map               | `iterator = map(function, iterable)`                                                    | `const newArr = arr.map(value => newValue);`         |
| filter            | `iterator = filter(predicate, iterable)`                                                | `const newArr = arr.filter(predicate);`              |
| reduce            | `from functools import reduce`<br>`value = reduce(lambda acc, item: ..., seq, initial)` | `const value = arr.reduce((acc, value) => { ... });` |
| any/some          | `any(map(predicate, iterable))`                                                         | `arr.some(predicate)` returns boolean                |
| all/every         | `all(map(predicate, iterable))`                                                         | `arr.every(predicate)` returns boolean               |
| add to end        | `seq.append(value)`                                                                     | `arr.push(value);`                                   |
| remove from end   | `seq.pop()`                                                                             | `const value = arr.pop();`                           |
| add to start      | `seq.insert(0, item)`                                                                   | `arr.unshift(value);`                                |
| remove from start | `del seq[0]`                                                                            | `const value = arr.shift();`                         |
| remove all        | `seq.clear()`                                                                           | `arr = [];`                                          |
| sort              | `list.sort(key=fn)` where `fn` returns a value for the key                              | `arr.sort(comparator);`                              |
| change            | combine `del` and `insert` above                                                        | `arr.splice(start, delCount, v1, v2, ...);`          |

In the Python list `sort` method, "vef" is short for value extract function.

JavaScript generators can be used to implement lazy evaluations.
The Python `filter` and `map` functions are lazy.
To get values from them, pass the result to a function like `list` or `set`.
For example:

```python
numbers = [1, 2, 3]
doubled = list(map(lambda n: n * 2, numbers))
```

The string `join` method takes an iterable over strings.
To join non-string values, use `map`. For example:

```python
'-'.join(map(str, numberList))
```

Python doesn't have a simple, builtin way to find the first item in a list
that matches some criteria. This naive approach is probably the most efficient.

```python
def index(aList, predicate):
  for index in range(0, len(aList) - 1):
      if predicate(aList[index]):
          return index
  return None
```

## List Comprehensions

Python supports list comprehensions, but JavaScript does not.
Here are some examples.
TODO: Add these!

## Function Operations

| Operation                | Python                                                                    | JavaScript                                        |
| ------------------------ | ------------------------------------------------------------------------- | ------------------------------------------------- |
| name                     | `fn.__name__`                                                             | `fn.name`                                         |
| required parameter count | `from inspect import getfullargspec`<br>`len(getfullargspec(fn).args)`    | `fn.length`                                       |
| get implementation code  | `from inspect import getsource`<br>`getsource(fn)`                        | `fn.toString()`                                   |
| bind                     | `from functools import partial`<br>`newFn = partial(fn, arg1, arg2, ...)` | `const newFn = fn.bind(thisArg, arg1, arg2, ...)` |
| call                     | `method(obj, arg1, arg2, ...)`                                            | `fn.call(thisArg, arg1, arg2, ...)`               |
| apply                    | `method(obj, *argList)`                                                   | `fn.apply(thisArg, argArray)`                     |

The Python `partial` function cannot be used on methods, only functions.

## Error Handling

Python refers to errors as exceptions.

| Operation   | Python                         | JavaScript                                      |
| ----------- | ------------------------------ | ----------------------------------------------- |
| throw error | `raise exClass(args)`          | `throw new Error(message);`                     |
| catch error | `try: ... except exClass: ...` | `try { ... } catch (e) { ... } finally { ... }` |

## JSON Operations

| Operation | Python                           | JavaScript                                 |
| --------- | -------------------------------- | ------------------------------------------ |
| create    | `jsonString = json.dumps(expr)`  | `const jsonString = JSON.stringify(expr);` |
| parse     | `value = json.loads(jsonString)` | `const value = JSON.parse(jsonString);`    |

In Python, you must `import json`.
There are many builtin Python exception classes.
The base class of all of them is Error.

## Dict/Object/Map Operations

To store associations between keys and values, Python uses "dictionaries".

| Operation               | Python                         |
| ----------------------- | ------------------------------ |
| create                  | `dict = {}`                    |
| get length              | `len(dict)`                    |
| set value of key        | `dict[key] = value`            |
| get value of key        | `dict[key]` or `dict.get(key)` |
| get all keys            | `dict.keys()` or `list(dict)`  |
| get all values          | `dict.values()`                |
| get all keys and values | `dict.items()`                 |
| test if key present     | `key in dict`                  |
| delete key              | `del dict[key]`                |
| delete all keys         | `dict.clear()`                 |
| iterate over            | `for item in dict.items():`    |

JavaScript uses plain objects or instances of the `Map` class
to store associations between keys and values.
The keys in JavaScript objects must be must be strings, integers, or symbols,
but keys in `Map` instances can be any type.

| Operation               | JavaScript Object                         | JavaScript Map                          |
| ----------------------- | ----------------------------------------- | --------------------------------------- |
| create                  | `const obj = {};`                         | `const map = new Map();`                |
| get length              | `Object.keys(obj).length`                 | `map.size`                              |
| set value of key        | `obj.key = value` or `obj[key] = value`   | `map.set(key, value)`                   |
| get value of key        | `obj.key` or `obj[key]`                   | `map.get(key)`                          |
| get all keys            | `Object.keys(obj)`                        | `map.keys()`                            |
| get all values          | `Object.values(obj)`                      | `map.values()`                          |
| get all keys and values | `Object.entries(obj)`                     | `map.entries()`                         |
| test if key present     | `key in obj` or `obj.hasOwnProperty(key)` | `map.has(key)`                          |
| delete key              | `delete obj.key` or `delete obj[key]`     | `map.delete(key)`                       |
| delete all keys         | `obj = {}`                                | `map.clear()`                           |
| iterate over            | `for (const prop in obj)`                 | `map.forEach((value, key) => { ... });` |

## Set Operations

| Operation             | Python                              | JavaScript                     |
| --------------------- | ----------------------------------- | ------------------------------ |
| create                | `s = {values}` or `s = set(values)` | `const s = new Set();`         |
| length                | `len(s)`                            | `s.size`                       |
| includes              | `value in s`                        | `s.has(value)`                 |
| add                   | `s.add(value)`                      | same                           |
| remove                | `s.remove(value)`                   | `s.delete(value);`             |
| remove all            | `s.clear()`                         | same                           |
| iterate over          | `for value in set:`                 | `s.forEach(value => { ... });` |
| convert to list/array | `l = list(s)`                       | `a = s.values();`              |

## Regular Expression Operations

In Python, import the `re` library. It supports the following methods:

- match: Match a regular expression pattern to the beginning of a string.
- fullmatch: Match a regular expression pattern to all of a string.
- search: Search a string for the presence of a pattern.
- sub: Substitute occurrences of a pattern found in a string.
- subn: Same as sub, but also return the number of substitutions made.
- split: Split a string by the occurrences of a pattern.
- findall: Find all occurrences of a pattern in a string.

| Operation                | Python                                       | JavaScript                                                                |
| ------------------------ | -------------------------------------------- | ------------------------------------------------------------------------- |
| create                   | `import re`<br>`regex = re.compile(pattern)` | `const re = /pattern/flags` or<br>`const re = new RegExp(pattern, flags)` |
| test if a string matches | `if pattern.search(str):`                    | `if (re.test(str))`                                                       |
| get first match          | `pattern.search(str)`                        | `str.match(re)`                                                           |
| get all matches          | `pattern.finditer(str)`                      | `str.matchAll(re)` or `re.exec(str)`                                      |

## Printing

| Operation    | Python                                              | JavaScript                  |
| ------------ | --------------------------------------------------- | --------------------------- |
| print values | `print(v1, v2, ..)`                                 | `console.log(v1, v2, ...);` |
| print error  | `import sys`<br>print(v1, v2, ..., file=sys.stderr) | `console.error(message);`   |

## Check for running as main

In Python, use `if __name__ == '__main__':`.
In Node.js, use `if (require.main === module) {`.

## Popular Tools/Libraries/Frameworks

| Topic            | Python                                        | JavaScript                          |
| ---------------- | --------------------------------------------- | ----------------------------------- |
| command-line     | `python` interpreter                          | Node.js                             |
| utilities        | pydash                                        | Lodash, Ramda                       |
| web server       | Flask                                         | Express                             |
| web framework    | Flask                                         | React, Vue, Svelte                  |
| dates and times  | datetime (in standard library)                | date.fns, Moment.js, Temporal       |
| unit tests       | unittest (in standard library), nose2, pytest | Jest, Mocha, Chai, @testing-library |
| end-to-end tests |                                               | Cypress                             |
| math             | math (in standard library)                    | mathjs                              |

## Python Magic Methods

Python magic methods support operator overloading for custom classes.
This is a partial list of the magic methods that a Python class can be implement.

| Method                              | Parameters       | Purpose                                               |
| ----------------------------------- | ---------------- | ----------------------------------------------------- |
| object lifecycle                    |                  |                                                       |
| `__new__`                           | cls, ...         | creates a new object                                  |
| `__init__`                          | self, ...        | initializes a new object                              |
| `__del__`                           | self             | destroys an object                                    |
| string representation               |                  |                                                       |
| `__repr__`                          | self             | returns a string representations useful to developers |
| `__str__`                           | self             | returns a string representation useful to users       |
| comparisons                         |                  |                                                       |
| `__cmp__`                           | self, other      | removed in Python 3                                   |
| `__ne__`                            | self, other      | determines if this object is not equal to another     |
| `__eq__`                            | self, other      | determines if this object is equal to another         |
| `__lt__`                            | self, other      | determines if this object is < another                |
| `__le__`                            | self, other      | determines if this object is <= to another            |
| `__gt__`                            | self, other      | determines if this object is > another                |
| `__ge__`                            | self, other      | determines if this object is >= to another            |
| also see functools.total_ordering() |                  |                                                       |
| list-like operations                |                  |                                                       |
| `__getitem__`                       | self, key        | gets an item from a list by index                     |
| `__setitem__`                       | self, key, value | sets an item in a list by index                       |
| `__delitem__`                       | self, key        | deletes an item from a list by index                  |
| `__iter__`                          | self             | returns an iterator                                   |
| `__contains__`                      | self, item       | determines if a given item is contained               |
| dict operations                     |                  |                                                       |
| `__missing__`                       | self, key        | returns value to use when key is not present          |
| math operations                     |                  |                                                       |
| `__add__`                           | self, other      | adds an object to another                             |
| `__sub__`                           | self, other      | subtracts an object from another                      |
| `__mul__`                           | self, other      | multiplies an object by another                       |
| `__div__`                           | self, other      | divides an object by another                          |
| `__mod__`                           | self, other      | mods an object by another                             |
| pickling (serialization)            |                  |                                                       |
| `__getstate__`                      | self             | pickles an object                                     |
| `__setstate__`                      | self             | unpickles an object                                   |
| other                               |                  |                                                       |
| `__call__`                          | self, ...        | treats an object as a function; can change state      |
