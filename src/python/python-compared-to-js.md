---
eleventyNavigation:
  key: Python Compared to JavaScript
  parent: Python
layout: topic-layout.njk
---

This compares the most commonly used features of Python and JavaScript.
Lesser used features are omitted.

## Overview

| Topic                   | Python                                             | JavaScript |
| ----------------------- | -------------------------------------------------- | ---------- |
| standard                | <https://docs.python.org/3/>                       | ECMAScript |
| evaluation              | dynamic                                            | dynamic    |
| performance             | slow                                               | fast       |
| style guide             | <https://www.python.org/dev/peps/pep-0008/>, Black | Prettier   |
| most common indentation | 4 spaces                                           | 2 spaces   |
| type coercion           | must be explicit                                   | implicit   |

Once source of performance benchmarks can be found at
<https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fastest.html>.

## Pros and Cons

### JavaScript

pros:

- performance
- ability to run in web browsers (clients) and from command-line (servers)
- great support for asynchronous code
- more compact syntax for functional programming (ex. functools vs. reduce)

cons:

- still in transition from require to import syntax in Node.js
- type coercions can result in surprising results if not familiar with them

Python:

pros:

- quantity and maturity of libraries for machine learning
- multiple number types
- some syntax is easier for beginners
  - ex. "and" vs. "&&".
  - ex. "println" vs. "console.log"
  - fewer parentheses and no curly braces or semicolons

cons:

- poor performance
- magic methods such as ...
- operator overloading
- lots of documentation and examples are still for V2 instead of V3
- anonymous functions are limited to a single expression
- no built-in support for asynchronous code

## Comments

| Type        | Python | JavaScript |
| ----------- | ------ | ---------- |
| single-line | #      | //         |
| multi-line  | none   | /\* \*/    |

## Types

| Type                 | Python                                    | JavaScript                                  |
| -------------------- | ----------------------------------------- | ------------------------------------------- |
| boolean              | True, False                               | true, false                                 |
| number               | int, float, complex                       | default is dbl precision float; also BigInt |
| character            | use string type                           | use string type                             |
| string               | 'text', "text", '''text''', or """text""" | 'text' or "text"                            |
| string interpolation | f'prefix{expr}suffix'                     | \`prefix\${expr}suffix\`                    |
| array                | see list, tuple, and range                | [v1, v2, ...]                               |
| list                 | [v1, v2, ...]                             | see array                                   |
| tuple                | (v1, v2, ...)                             | no equivalent                               |
| range                | range range(start, stop[, step])          | no equivalent                               |
| object / dict        | {'k1': v1, 'k2': v2, ...}                 | {k1: v1, k2: v2, ...}                       |
| function             | see "Function" section below              | see "Function" section below                |
| class                | class Name:                               | class Name { ... }                          |
| regular expression   | re.compile(pattern)                       | /pattern/flags or new RegExp(pattern)       |
| no value             | None                                      | undefined or null                           |

In Python, the following values are treated as false when used
in a boolean context: False, None, 0, '', and empty sequences.
In JavaScript, the following values are treated as false when used
in a boolean context: false, 0, '', undefined, null.

Python has sequences instead of arrays.
There are three kinds of sequences: list, tuple, and range.
A list is a mutable sequence of values that have the same type.
A tuple is an immutable sequence of values that have varying types.
A range is an immutable sequence of numbers that can be used for looping.

JS object keys must be strings.
Python dict keys can e any immutable type.

## Variables and Assignment

| Topic    | Python       | JavaScript          |
| -------- | ------------ | ------------------- |
| constant | NAME = value | const NAME = value; |
| variable | name = value | let name = value;   |

Python uses a naming convention (all uppercase) to identify constants,
but they can still be modified.

## More Assignments

| Topic                | Python                  | JavaScript                    |
| -------------------- | ----------------------- | ----------------------------- |
| spread of array/list | v1, v2 = array          | const [v1, v2, ...] = array;  |
| spread of object     | not supported           | const {k1, k2, ...} = object; |
| addition             | name += expr            | same                          |
| subtraction          | name -= expr            | same                          |
| multiplication       | name \*= expr           | same                          |
| division             | name /= expr            | same                          |
| exponentiation       | name \*\*= expr         | same                          |
| mod (remainder)      | name %= expr            | same                          |
| logical and          | not supported           | name &&= expr                 |
| logical or           | not supported           | name \|\|= expr               |
| logical xor          | not supported           | name ^= expr                  |
| bitwise and          | name &= expr            | same                          |
| bitwise or           | name \|= expr           | same                          |
| bitwise xor          | name ^= expr            | same                          |
| signed bit shift     | <<= (left), >>= (right) | same                          |
| unsigned bit shift   | not supported           | <<<= (left), >>>= (right)     |

## Comparison

| Topic                 | Python | JavaScript                          |
| --------------------- | ------ | ----------------------------------- |
| equal for non-objects | ==     | == (with coercion) or === (without) |
| equal of objects      | is     | ===                                 |
| not equal of objects  | is not | !==                                 |
| not equal             | !=     | != (with coercion) or !== (without) |
| less than             | <      | same                                |
| less than or equal    | <=     | same                                |
| greater than          | >      | same                                |
| greater than or equal | >=     | same                                |

## Conditional Logic

| Topic   | Python                            | JavaScript                                 |
| ------- | --------------------------------- | ------------------------------------------ |
| if      | if cond:                          | if (cond) stmtOrBlock                      |
| if/else | if cond: else:                    | if (cond) { trueBlock } else { falseBlock} |
| ternary | trueValue if cond else falseValue | cond ? trueValue : falseValue              |

## Iteration

| Topic                            | Python                                 | JavaScript                                      |
| -------------------------------- | -------------------------------------- | ----------------------------------------------- |
| classic                          | for var in range(start, stop[, step]): | for (let var = initial; cond; statements)       |
| over collection                  | for value in sequence:                 | for (const value of iterable)                   |
| over object/dict keys            | for key in dict.keys():                | for (const key of Object.keys(obj))             |
| over object/dict values          | for value in dict.values():            | for (const value of Object.values(obj))         |
| over object/dict keys and values | for key, value in dict.items():        | for (const [key, value] of Object.entries(obj)) |
| top-tested                       | while cond:                            | while (cond)                                    |
| bottom-tested                    | while True: ... if !cond: break        | do { ... } while (cond);                        |

## Functions

| Topic                       | Python                                                                 | JavaScript                            |
| --------------------------- | ---------------------------------------------------------------------- | ------------------------------------- |
| named definition            | def name(params):                                                      | function name(params) { definition }  |
| anonymous definition        | lambda params: expression                                              | const name = (params) => definition   |
| anonymous single parameter  | same as above                                                          | const name = param => { ... }         |
| anonymous single expression | same as above                                                          | const name = (params) => expr         |
| variable arguments          | def name(p1, p2, \*rest):                                              | function name(p1, p2, ...rest) { ...} |
| return type                 | not specified; return a single value<br>that can be an object or array | same as Python                        |
| calling                     | name(args)                                                             | name(args)                            |

Note that unlike JS arrow functions, Python lambdas
can only use a single expression, not a block of code.

## Classes

| Topic                             | Python                                              | JavaScript                               |
| --------------------------------- | --------------------------------------------------- | ---------------------------------------- |
| defining                          | class Name:                                         | class Name { ... }                       |
| inheritance                       | class Sub(Super1, Super2, ...)                      | class Sub extends Super { ... }          |
| constructor                       | def \_\_init\_\_(self, params):                     | constructor(params) { ... }              |
| instance property declaration     | not declared; set in \_\_init\_\_ on self           | not declared; set in constructor on this |
| instance property reference       | self.propName                                       | this.propName                            |
| class/static property declaration | propName = value;                                   | static propName = value;                 |
| class/static property reference   | CName.proName or instance.propName                  | CName.propName                           |
| instance method                   | def name(params):                                   | name(params) { ... }                     |
| class/static method declaration   | @staticmethod<br>def methodName(params):            | static methodName(params) { ... }        |
| class/static method call          | CName.methodName(params) or inst.methodName(params) | CName.methodName(params)                 |
| instantiating                     | object = CName(args)                                | const object = new CName(args);          |

JS does not support multiple inheritance, but Python does.
In addition to the `@staticmethod` decorator, Python also supports the
`@classmethod` decorator. The difference is that methods defined with
the latter are passed the class as the first argument.

## Asynchronous Operations

| Topic                    | Python                    | JavaScript                             |
| ------------------------ | ------------------------- | -------------------------------------- |
| async named function     | async def name(params):   | async function name(params) { ... }    |
| async anonymous function | not supported             | const name = async (params) => { ... } |
| async call with await    | result = await name(args) | const result = await name(args);       |
| async call with then     | n/a                       | name(args).then(result => { ... });    |

In JS, async functions return a Promise.
In Python, async function return a coroutine which is similar.
Python doesn't seem to have to equivalent of
the JS Promise methods `then` and `catch`.

## Modules

| Topic          | Python                               | JavaScript                               |
| -------------- | ------------------------------------ | ---------------------------------------- |
| defining       | content of file                      | content of file                          |
| export         | everything is automatically exported | export name = value;                     |
| default export | not supported                        | export default name = value;             |
| import default | not supported                        | import name from 'path';                 |
| import named   | from moduleName import name1, name2  | import {name1, name2} from 'path';       |
| import both    | n/a                                  | import name, {name1, name2} from 'path'; |
| where to find  | pip                                  | npm                                      |

## Boolean Operations

| Operation   | Python    | JavaScript |
| ----------- | --------- | ---------- |
| and         | b1 and b2 | b1 && b2   |
| or          | b1 or b2  | b1 \|\| b2 |
| not         | not b     | !b         |
| bitwise and | b1 & b2   | b1 & b2    |
| bitwise or  | b1 \| b2  | b1 \| b2   |
| bitwise not | ~b        | ~b         |
| bitwise xor | b1 & b2   | b1 ^ b2    |

## Numeric Operations

| Operation                             | Python             | JavaScript                         |
| ------------------------------------- | ------------------ | ---------------------------------- |
| basic                                 | +, -, \*, /        | +, -, \*, /                        |
| exponentiation                        | \*\*               | \*\*                               |
| increment                             | v += 1             | ++n1 (pre) or n1++ (post)          |
| decrement                             | v -= 1             | --n1 (pre) or n1-- (post)          |
| mod (remainder)                       | %                  | %                                  |
| convert to string                     | str(n)             | n.toString()                       |
| convert to string with fixed decimals | "{:.2f}".format(n) | n.toFixed(decimals)                |
| convert to hex                        | hex(n)             | n.toString(16)                     |
| convert from hex                      | int(hexString, 16) | parseInt(hexString, 16)            |
| constants                             | see math module    | see Math and Number global objects |
| functions                             | see math module    | see Math and Number global objects |

## String Operations

| Operation     | Python                               | JavaScript                                    |
| ------------- | ------------------------------------ | --------------------------------------------- |
| concatenation | s1 + str(n1)                         | s1 + n1                                       |
| lowercase     | s.lower()                            | s.toLowerCase()                               |
| uppercase     | s.upper()                            | s.toUpperCase()                               |
| substring     | s[start:end] or s[start:] or s[:end] | s1.substring(start[, end])                    |
| slice         | same as above                        | like substring, but supports negative indexes |
| split         | s.split(delimiter) returns list      | s.split(delimiter) returns array              |
| starts with   | s.startswith(sub)                    | s.startsWith(sub) returns boolean             |
| ends with     | s.endswith(sub)                      | s.endsWith(sub) returns boolean               |
| contains      | sub in s                             | s.includes(sub) returns boolean               |
| index of      | s.index(sub[, start[, end]])         | s.indexOf(sub) returns number                 |
| last index of | s.rindex(sub[, start[, end]])        | s.lastIndexOf(sub) returns number             |
| compare       | not supported                        | s.localeCompare(sub) returns -1, 0, or 1      |
| replace first | s.replace(old, new, 1)               | s.replace(oldSub, newSub)                     |
| replace all   | s.replace(old, new)                  | s.replaceAll(oldSub, newSub)                  |
| trim start    | s.lstrip()                           | s.trimStart()                                 |
| trim end      | s.rstrip()                           | s.trimEnd()                                   |
| trim both     | s.strip()                            | s.trim()                                      |

## Array/Sequence Operations

Some Python sequence operations apply to all three of kinds of sequences
(list, tuple, and range).

| Operation         | Python                                                                              | JavaScript                                         |
| ----------------- | ----------------------------------------------------------------------------------- | -------------------------------------------------- |
| is array/sequence | hasattr(type(obj), '\_\_iter\_\_')                                                  | Array.isArray(expression)                          |
| length            | len(seq)                                                                            | arr.length                                         |
| lookup            | value = seq[index]                                                                  | const value = arr[index];                          |
| subset            | newSeq = seq[startIndex:endIndex]                                                   | const newArr = arr.slice(startIndex[, endIndex]);  |
| concat            | newSeq = seq1 + seq2                                                                | const newArr = arr1.concat(arr2, arr3, ...);       |
| find              | next(filter(predicate, iterable))                                                   | const value = arr.find(predicate);                 |
| find index        | see note below this table                                                           | const index = arr.findIndex(predicate);            |
| for each          | for item in seq:                                                                    | arr.forEach(value => { ... });                     |
| includes          | value in seq                                                                        | arr.includes(value) returns boolean                |
| not includes      | value not in seq                                                                    | !arr.includes(value) returns boolean               |
| index of          | seq.index(value[, start[, end]])                                                    | const index = arr.indexOf(value[, fromIndex])      |
| last index of     | not builtin; have to reverse list                                                   | const index = arr.lastIndexOf(value[, fromIndex])  |
| join              | delimiter.join(iterable)                                                            | arr.join(delimiter) returns string                 |
| map               | iterator = map(function, iterable)                                                  | const newArr = arr.map(value => newValue);         |
| filter            | iterator = filter(predicate, iterable)                                              | const newArr = arr.filter(predicate);              |
| reduce            | from functools import reduce<br>value = reduce(lambda acc, item: ..., seq, initial) | const value = arr.reduce((acc, value) => { ... }); |
| any/some          | any(map(predicate, iterable))                                                       | arr.some(predicate) returns boolean                |
| all/every         | all(map(predicate, iterable))                                                       | arr.every(predicate) returns boolean               |
| add to end        | seq.append(value)                                                                   | arr.push(value);                                   |
| remove from end   | seq.pop()                                                                           | const value = arr.pop();                           |
| add to start      | seq.insert(0, item)                                                                 | arr.unshift(value);                                |
| remove from start | del seq[0]                                                                          | const value = arr.shift();                         |
| remove all        | seq.clear()                                                                         | arr = [];                                          |
| sort              | list.sort(key=vef)                                                                  | arr.sort(comparator);                              |
| change            | combine del and insert above                                                        | arr.splice(start, delCount, v1, v2, ...);          |

In the Python list `sort` method, "vef" is short for value extract function.

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

## Object Operations

| Operation           | Python                     | JavaScript                            |
| ------------------- | -------------------------- | ------------------------------------- |
| get value of key    | dict[key] or dict.get(key) | obj.key or obj[key]                   |
| get keys            | dict.keys() or list(dict)  | Object.keys(obj)                      |
| get values          | dict.values()              | Object.values(obj)                    |
| get keys and values | dict.items()               | Object.entries(obj)                   |
| test if key present | key in dict                | key in obj or obj.hasOwnProperty(key) |
| delete key          | del dict[key]              | delete obj.key or delete obj[key]     |
| delete all keys     | dict.clear()               | obj = {}                              |

## Function Operations

| Operation                | Python                                                                | JavaScript                                      |
| ------------------------ | --------------------------------------------------------------------- | ----------------------------------------------- |
| name                     | fn.\_\_name\_\_                                                       | fn.name                                         |
| required parameter count | from inspect import getfullargspec<br>len(getfullargspec(fn).args)    | fn.length                                       |
| get implementation code  | from inspect import getsource<br>getsource(fn)                        | fn.toString()                                   |
| bind                     | from functools import partial<br>newFn = partial(fn, arg1, arg2, ...) | const newFn = fn.bind(thisArg, arg1, arg2, ...) |
| call                     | method(obj, arg1, arg2, ...)                                          | fn.call(thisArg, arg1, arg2, ...)               |
| apply                    | method(obj, \*argList)                                                | fn.apply(thisArg, argArray)                     |

The Python `partial` function cannot be used on methods, only functions.

## Error Handling

Python refers to errors as exceptions.

| Operation   | Python                       | JavaScript                                    |
| ----------- | ---------------------------- | --------------------------------------------- |
| throw error | raise exClass(args)          | throw new Error(message);                     |
| catch error | try: ... except exClass: ... | try { ... } catch (e) { ... } finally { ... } |

## JSON Operations

| Operation | Python                         | JavaScript                               |
| --------- | ------------------------------ | ---------------------------------------- |
| create    | jsonString = json.dumps(expr)  | const jsonString = JSON.stringify(expr); |
| parse     | value = json.loads(jsonString) | const value = JSON.parse(jsonString);    |

In Python, you must `import json`.
There are many builtin Python exception classes.
The base class of all of them is Error.

## Set Operations

| Operation        | Python      | JavaScript                     |
| ---------------- | ----------- | ------------------------------ |
| create           | {} or set() | new Set();                     |
| length           |             | set.size                       |
| includes         |             | set.has(value)                 |
| add              |             | set.add(value)                 |
| remove           |             | set.delete(value)              |
| remove all       |             | set.clear()                    |
| iterate over     |             | set.forEach(value => { ... }); |
| convert to array |             | set.values()                   |

## Map Operations

| Operation                    | Python | JavaScript                            |
| ---------------------------- | ------ | ------------------------------------- |
| create                       |        | const map = new Map();                |
| length                       |        | map.size                              |
| includes                     |        | map.has(key)                          |
| add                          |        | map.set(key, value)                   |
| remove                       |        | map.delete(key)                       |
| remove all                   |        | map.clear()                           |
| iterate over                 |        | map.forEach((value, key) => { ... }); |
| convert to array             |        | map.values()                          |
| get value for key            |        | map.get(key)                          |
| get array of keys            |        | map.keys()                            |
| get array of values          |        | map.values()                          |
| get array of keys and values |        | map.entries()                         |

## Regular Expression Operations

| Operation                | Python | JavaScript                                   |
| ------------------------ | ------ | -------------------------------------------- |
| create                   |        | /pattern/flags or new RegExp(pattern, flags) |
| test if a string matches |        | re.test(str)                                 |
| get first matches        |        | str.match(re)                                |
| get all matches          |        | str.matchAll(re) or re.exec(str)             |

## Printing

| Operation    | Python            | JavaScript                |
| ------------ | ----------------- | ------------------------- |
| print values | print(v1, v2, ..) | console.log(v1, v2, ...); |
| print error  |                   | console.error(message);   |

## Popular Frameworks

| Topic        | Python | JavaScript         |
| ------------ | ------ | ------------------ |
| command-line |        | Node.js            |
| web          |        | React, Vue, Svelte |

## Libraries

| Topic            | Python | JavaScript                          |
| ---------------- | ------ | ----------------------------------- |
| utilities        |        | Lodash, Ramda                       |
| web server       | Flask  | Express                             |
| dates and times  |        | date.fns, moment                    |
| unit tests       |        | Jest, Mocha, Chai, @testing-library |
| end-to-end tests |        | Cypress                             |
| math             |        | mathjs                              |

## Python Magic Methods

Python magic methods support operator overloading for custom classes.
This is a partial list of the magic methods that a Python class can be implement.

| Method                              | Parameters       | Purpose                                               |
| ----------------------------------- | ---------------- | ----------------------------------------------------- |
| object lifecycle                    |                  |                                                       |
| \_\_new\_\_                         | cls, ...         | creates a new object                                  |
| \_\_init\_\_                        | self, ...        | initializes a new object                              |
| \_\_del\_\_                         | self             | destroys an object                                    |
| string representation               |                  |                                                       |
| \_\_repr\_\_                        | self             | returns a string representations useful to developers |
| \_\_str\_\_                         | self             | returns a string representation useful to users       |
| comparisons                         |                  |                                                       |
| \_\_cmp\_\_                         | self, other      | removed in Python 3                                   |
| \_\_ne\_\_                          | self, other      | determines if this object is not equal to another     |
| \_\_eq\_\_                          | self, other      | determines if this object is equal to another         |
| \_\_lt\_\_                          | self, other      | determines if this object is < another                |
| \_\_le\_\_                          | self, other      | determines if this object is <= to another            |
| \_\_gt\_\_                          | self, other      | determines if this object is > another                |
| \_\_ge\_\_                          | self, other      | determines if this object is >= to another            |
| also see functools.total_ordering() |                  |                                                       |
| list-like operations                |                  |                                                       |
| \_\_getitem\_\_                     | self, key        | gets an item from a list by index                     |
| \_\_setitem\_\_                     | self, key, value | sets an item in a list by index                       |
| \_\_delitem\_\_                     | self, key        | deletes an item from a list by index                  |
| \_\_iter\_\_                        | self             | returns an iterator                                   |
| \_\_contains\_\_                    | self, item       | determines if a given item is contained               |
| dict operations                     |                  |                                                       |
| \_\_missing\_\_                     | self, key        | returns value to use when key is not present          |
| math operations                     |                  |                                                       |
| \_\_add\_\_                         | self, other      | adds an object to another                             |
| \_\_sub\_\_                         | self, other      | subtracts an object from another                      |
| \_\_mul\_\_                         | self, other      | multiplies an object by another                       |
| \_\_div\_\_                         | self, other      | divides an object by another                          |
| \_\_mod\_\_                         | self, other      | mods an object by another                             |
| pickling (serialization)            |                  |                                                       |
| \_\_getstate\_\_                    | self             | pickles an object                                     |
| \_\_setstate\_\_                    | self             | unpickles an object                                   |
| other                               |                  |                                                       |
| \_\_call\_\_                        | self, ...        | treats an object as a function; can change state      |
