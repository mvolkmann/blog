---
eleventyNavigation:
  key: Python operator module
  order: 1.3
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

The {% aTargetBlank "https://docs.python.org/3/library/operator.html",
"`operator`" %} module in the Python standard library
that provides that implement standard Python operators.
These are useful for passing to higher-order functions.
For example, ..
TODO

The tables below show standard Python operators
and their equivalent function from the `operator` module.

## Relational

| Operator | Function   |
| -------- | ---------- |
| `a < b`  | `lt(a, b)` |
| `a <= b` | `le(a, b)` |
| `a == b` | `eq(a, b)` |
| `a != b` | `ne(a, b)` |
| `a >= b` | `ge(a, b)` |
| `a > b`  | `gt(a, b)` |

## Arithmetic

| Operator | Function         |
| -------- | ---------------- |
| `-a`     | `neg(a)`         |
| `+a`     | `pos(a)`         |
| `a + b`  | `add(a, b)`      |
| `a - b`  | `sub(a, b)`      |
| `a * b`  | `mul(a, b)`      |
| `a / b`  | `truediv(a, b)`  |
| `a // b` | `floordiv(a, b)` |
| `a % b`  | `mod(a, b)`      |
| `a ** b` | `pow(a, b)`      |

## Bitwise

| Operator | Function                | Description                                      |
| -------- | ----------------------- | ------------------------------------------------ |
| `a & b`  | `and_(a, b)`            | bitwise and;<br>note trailing underscore in name |
| `a | b`  | `or_(a, b)`             | bitwise or;<br>note trailing underscore in name  |
| `a ^ b`  | `xor(a, b)`             | bitwise exclusive or                             |
| `~a`     | `inv(a)` or `invert(a)` | bitwise inversion                                |
| `a << b` | `lshift(a, b)`          | left shift                                       |
| `a >> b` | `rshift(a, b)`          | right shift                                      |

## Indexing

| Operator     | Function             | Description        |
| ------------ | -------------------- | ------------------ |
| `obj[k]`     | `getitem(obj, k)`    | indexed retrieval  |
| `obj[k] = v` | `setitem(obj, k, v)` | indexed assignment |
| `del obj[k]` | `delitem(obj, k)`    | indexed deletion   |

## Creating retrieval functions

| Operator   | Function                             | Description                                                            |
| ---------- | ------------------------------------ | ---------------------------------------------------------------------- |
| `obj.name` | `attrgetter(name)(obj)`              | returns attribute value from object<br>`attrgetter` returns a function |
| n/a        | `attrgetter(name1, name2, ...)(obj)` | returns tuple of attribute values from object                          |
| `seq[i]`   | `itemgetter(i)(seq)`                 | returns i'th value from sequence<br>`itemgetter` returns a function    |
| n/a        | `itemgetter(i1, i2, ...)(obj)`       | returns tuple of values from sequence                                  |
| n/a        | `methodcaller(name, args)(obj)`      | calls a method with given arguments on an object                       |

## Slicing

| Operator            | Function                            | Description      |
| ------------------- | ----------------------------------- | ---------------- |
| `seq[i:j]`          | `getitem(seq, slice(i, j))`         | slice retrieval  |
| `seq[i:j] = values` | `setitem(seq, slice(i, j), values)` | slice assignment |
| `del seq[i:j]`      | `delitem(seq, slice(i, j))`         | slice deletion   |

## Other

| Operator                            | Function           | Description                                                                              |
| ----------------------------------- | ------------------ | ---------------------------------------------------------------------------------------- |
| `a + b`                             | `concat(a, b)`     | returns new sequence created by concatenating two others                                 |
| `v in seq`                          | `contains(v, seq)` | determine if `v` is in `seq`                                                             |
| n/a                                 | `countOf(a, b)`    | returns number of occurrences of b in a;<br>note camelCase name                          |
| `a is b`                            | `is_(a, b)`        | determines if a and b are the same object in memory;<br>note trailing underscore in name |
| `a is not b`                        | `is_not(a, b)`     | determines if a and b are not the same object in memory                                  |
| `a @ b`                             | `matmul(a, b)`     | matrix multiplication                                                                    |
| `s % obj`                           | `mod(s, obj)`      | string formatting (SEEMS WRONG!)                                                         |
| `v` evaluated in<br>Boolean context | `truth(v)`         | returns the boolean value of v;<br>same as `bool(v)`                                     |
| `not v`                             | `not_(v)`          | returns the negated boolean value of v;<br>note trailing underscore in name              |
| n/a                                 | `length_hint(seq)` | returns estimated number of elements, not length in bytes;<br>Why estimated?             |
