---
eleventyNavigation:
  key: Python Compared to JavaScript
  parent: Python
layout: topic-layout.njk
---

This compares the most commonly used features of Python and JavaScript.
Lesser used features are omitted.

## Overview

| Topic       | Python               | JavaScript |
| ----------- | -------------------- | ---------- |
| standard    | no official standard | ECMAScript |
| evaluation  | dynamic              | dynamic    |
| performance | slow                 | fast       |

Once source of performance benchmarks can be found at
<https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fastest.html>.

## Types

| Type               | Python              | JavaScript                                    |
| ------------------ | ------------------- | --------------------------------------------- |
| boolean            | True, False         | true, false                                   |
| number             | int, float, complex | default is dbl precision float; also BigInt   |
| character          | use string type     | use string type                               |
| string             |                     | 'text' or "text" or `text with interpolation` |
| array / sequence   | list, tuple, range  | [v1, v2, ...]                                 |
| object             |                     | {k1: v1, k2: v2, ...}                         |
| function           |                     | see "Function" section below                  |
| class              |                     | class Name { ... }                            |
| regular expression |                     | /pattern/flags or new RegExp(pattern)         |
| no value           |                     | undefined or null                             |

In Python, the following values are treated as false when used
in a boolean context: False, None, 0, '', and empty sequences.
In JavaScript, the following values are treated as false when used
in a boolean context: false, 0, '', undefined, null.

## Assignment

| Topic              | Python | JavaScript                    |
| ------------------ | ------ | ----------------------------- |
| constants          |        | const name = value;           |
| variables          |        | let name[ = value];           |
| spread of array    |        | const [n1, n2, ...] = array;  |
| spread of object   |        | const {n1, n2, ...} = object; |
| addition           |        | name += expr                  |
| subtraction        |        | name -= expr                  |
| multiplication     |        | name \*= expr                 |
| division           |        | name /= expr                  |
| exponentiation     |        | name \*\*= expr               |
| mod (remainder)    |        | name %= expr                  |
| logical and        |        | name &&= expr                 |
| logical or         |        | name \|\|= expr               |
| logical xor        |        | name ^= expr                  |
| bitwise and        |        | name &= expr                  |
| bitwise or         |        | name \|= expr                 |
| bitwise xor        |        | name ^= expr                  |
| signed bit shift   |        | <<= (left), >>= (right)       |
| unsigned bit shift |        | <<<= (left), >>>= (right)     |

## Comparison

| Topic                 | Python | JavaScript                          |
| --------------------- | ------ | ----------------------------------- |
| equal for non-objects | ==     | == (with coercion) or === (without) |
| equal of objects      | is     | ===                                 |
| not equal of objects  | is not | !==                                 |
| not equal             | !=     | != (with coercion) or !== (without) |
| less than             | <>     | <                                   |
| less than or equal    | <=     | <=                                  |
| greater than          | >      | >                                   |
| greater than or equal | >=     | >=                                  |

## Conditional Logic

| Topic   | Python | JavaScript                                 |
| ------- | ------ | ------------------------------------------ |
| if      |        | if (cond) stmtOrBlock                      |
| if/else |        | if (cond) { trueBlock } else { falseBlock} |
| ternary |        | cond ? trueValue : falseValue              |

## Iteration

| Topic           | Python                 | JavaScript                                        |
| --------------- | ---------------------- | ------------------------------------------------- |
| classic         |                        | for (let var = initial; cond; statements) { ... } |
| over collection | for value in sequence: | for (const value of interable) { ... }            |
| top-tested      |                        | while (cond) { ... }                              |
| bottom-tested   |                        | do { ... } while (cond);                          |

## Functions

| Topic                       | Python | JavaScript                                                          |
| --------------------------- | ------ | ------------------------------------------------------------------- |
| named definition            |        | function name(params) { ... }                                       |
| anonymous definition        |        | const name = (params) => { ... }                                    |
| anonymous single parameter  |        | const name = param => { ... }                                       |
| anonymous single expression |        | const name = (params) => expr                                       |
| arguments                   |        | see above                                                           |
| variable arguments          |        | function name(p1, p2, ...rest) { ...}                               |
| return type                 |        | not specified; return a single value that can be an object or array |
| calling                     |        | name(args)                                                          |

## Classes

| Topic                             | Python | JavaScript                                |
| --------------------------------- | ------ | ----------------------------------------- |
| defining                          |        | class Name { ... }                        |
| inheritance                       |        | class SubClass extends SuperClass { ... } |
| constructor                       |        | constructor(params) { ... }               |
| instance property declaration     |        | not declared                              |
| instance property reference       |        | this.propertyName                         |
| class/static property declaration |        | static propertyName = value;              |
| class/static property reference   |        | ClassName.propertyName                    |
| instance method                   |        | name(params) { ... }                      |
| class/static method declaration   |        | static methodName(params) { ... }         |
| class/static method all           |        | ClassName.methodName(params) { ... }      |
| instantiating                     |        | const object = new ClassName(args);       |

## Asynchronous Operations

| Topic                    | Python | JavaScript                             |
| ------------------------ | ------ | -------------------------------------- |
| async named function     |        | async function name(params) { ... }    |
| async anonymous function |        | const name = async (params) => { ... } |
| async call with await    |        | const result = await name(args);       |
| async call with then     |        | name(args).then(result => { ... });    |

## Modules

| Topic          | Python | JavaScript                               |
| -------------- | ------ | ---------------------------------------- |
| defining       |        | content of file                          |
| export         |        | export name = value;                     |
| default export |        | export default name = value;             |
| import default |        | import name from 'path';                 |
| import named   |        | import {name1, name2} from 'path';       |
| import both    |        | import name, {name1, name2} from 'path'; |
| where to find  |        | npm                                      |

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

| Operation     | Python | JavaScript                                    |
| ------------- | ------ | --------------------------------------------- |
| concatenation |        | s1 + s2                                       |
| lowercase     |        | s.toLowerCase()                               |
| uppercase     |        | s.toUpperCase()                               |
| substring     |        | s1.substring(startIndex, endIndex)            |
| slice         |        | like substring, but supports negative indexes |
| split         |        | s.split(delimiter) returns array              |
| starts with   |        | s.startsWith(sub) returns boolean             |
| ends with     |        | s.endsWith(sub) returns boolean               |
| contains      |        | s.includes(sub) returns boolean               |
| index of      |        | s.indexOf(sub) returns number                 |
| last index of |        | s.lastIndexOf(sub) returns number             |
| compare       |        | s.localeCompare(sub) returns -1, 0, or 1      |
| replace first |        | s.replace(sub)                                |
| replace all   |        | s.replaceAll(sub)                             |
| trim start    |        | s.trimStart(sub)                              |
| trim end      |        | s.trimEnd(sub)                                |
| trim both     |        | s.trim(sub)                                   |

## Array/Sequence Operations

Python has sequences instead of arrays.
There are three kinds of sequences: list, tuple, and range.
A list is a mutable sequence of values that have the same type.
A tuple is an immutable sequence of values that have varying types.
A range is an immutable sequence of numbers that can be used for looping.
Some sequence operations apply to all three of these types.

| Operation         | Python                            | JavaScript                                         |
| ----------------- | --------------------------------- | -------------------------------------------------- |
| is array/sequence | hasattr(type(obj), '**iter**')    | Array.isArray(expression)                          |
| length            | len(seq)                          | arr.length                                         |
| lookup            | value = seq[index]                | const value = arr[index];                          |
| subset            | newSeq = seq[startIndex:endIndex] | const newArr = arr.slice(startIndex[, endIndex]);  |
| concat            | newSeq = seq1 + seq2              | const newArr = arr1.concat(arr2, arr3, ...);       |
| find              |                                   | const value = arr.find(predicateFn);               |
| find index        |                                   | const index = arr.findIndex(predicateFn);          |
| for each          |                                   | arr.forEach(value => { ... });                     |
| includes          | value in seq                      | arr.includes(value) returns boolean                |
| not includes      | value not in seq                  | !arr.includes(value) returns boolean               |
| index of          |                                   | const index = arr.indexOf(value[, fromIndex])      |
| last index of     |                                   | const index = arr.lastIndexOf(value[, fromIndex])  |
| join              |                                   | arr.join(delimiter) returns string                 |
| map               |                                   | const newArr = arr.map(value => newValue);         |
| filter            |                                   | const newArr = arr.filter(predicateFn);            |
| reduce            |                                   | const value = arr.reduce((acc, value) => { ... }); |
| some              |                                   | arr.some(predicateFn) returns boolean              |
| every             |                                   | arr.every(predicateFn) returns boolean             |
| add to end        | seq.append(value)                 | arr.push(value);                                   |
| remove from end   |                                   | const value = arr.pop();                           |
| add to start      |                                   | arr.unshift(value);                                |
| remove from start |                                   | const value = arr.shift();                         |
| remove all        | seq.clear()                       | arr = [];                                          |
| sort              | list.sort(key=vef)                | arr.sort(comparator);                              |
| change            |                                   | arr.splice(start, delCount, v1, v2, ...);          |

In the Python list sort method, "vef" is short for value extract function.

## Object Operations

| Operation           | Python | JavaScript          |
| ------------------- | ------ | ------------------- |
| get keys            |        | Object.keys(obj)    |
| get values          |        | Object.values(obj)  |
| get keys and values |        | Object.entries(obj) |

## Function Operations

| Operation                | Python | JavaScript                                      |
| ------------------------ | ------ | ----------------------------------------------- |
| name                     |        | fn.name                                         |
| required parameter count |        | fn.length                                       |
| get implementation code  |        | fn.toString()                                   |
| bind                     |        | const newFn = fn.bind(thisArg, arg1, arg2, ...) |
| call                     |        | fn.call(thisArg, arg1, arg2, ...)               |
| apply                    |        | fn.apply(thisArg, argArray)                     |

## Error Handling

Python refers to errors as exceptions.

| Operation   | Python | JavaScript                                    |
| ----------- | ------ | --------------------------------------------- |
| throw error |        | throw new Error(message);                     |
| catch error |        | try { ... } catch (e) { ... } finally { ... } |

## JSON Operations

| Operation | Python | JavaScript                               |
| --------- | ------ | ---------------------------------------- |
| create    |        | const jsonString = JSON.stringify(expr); |
| parse     |        | const value = JSON.parse(jsonString);    |

## Set Operations

| Operation        | Python | JavaScript                     |
| ---------------- | ------ | ------------------------------ |
| create           |        | const set = new Set();         |
| length           |        | set.size                       |
| includes         |        | set.has(value)                 |
| add              |        | set.add(value)                 |
| remove           |        | set.delete(value)              |
| remove all       |        | set.clear()                    |
| iterate over     |        | set.forEach(value => { ... }); |
| convert to array |        | set.values()                   |

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
