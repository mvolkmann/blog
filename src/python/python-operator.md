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
For example, ...

THIS IS A WORK IN PROGRESS!

## Comparison

| Operator | Function   |
| -------- | ---------- |
| `<`      | `lt(a, b)` |
| `<=`     | `le(a, b)` |
| `==`     | `eq(a, b)` |
| `!=`     | `ne(a, b)` |
| `>=`     | `ge(a, b)` |
| `>`      | `gt(a, b)` |

## Arithmetic

| Operator | Function        |
| -------- | --------------- |
| `+`      | `add(a, b)`     |
| `-`      | `sub(a, b)`     |
| `*`      | `mul(a, b)`     |
| `/`      | `truediv(a, b)` |
| `%`      | `mod(a, b)`     |
| `**`     | `pow(a, b)`     |

## Bitwise

| Operator | Function     |
| -------- | ------------ |
| `&`      | `and_(a, b)` |
| `|`      | `or_(a, b)`  |
| `^`      | `xor(a, b)`  |
| `~`      | `invert(a)`  |

## Other

| Operator | Function       | Description                                          |
| -------- | -------------- | ---------------------------------------------------- |
| `@`      | `matmul(a, b)` | matrix multiplication                                |
|          | `is_(a, b)`    | determines if a and b are the same object in memory  |
|          | `is_not(a, b)` | determines if a and b are the same object in memory  |
| `not`    | `not(v)`       | negates the boolean value of v                       |
|          | `truth(v)`     | returns the boolean value of v;<br>same as `bool(v)` |
