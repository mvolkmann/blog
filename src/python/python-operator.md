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
| `a < b`  | `lt(a, b)` |
| `a <= b` | `le(a, b)` |
| `a == b` | `eq(a, b)` |
| `a != b` | `ne(a, b)` |
| `a >= b` | `ge(a, b)` |
| `a > b`  | `gt(a, b)` |

## Arithmetic

| Operator | Function         |
| -------- | ---------------- |
| `a + b`  | `add(a, b)`      |
| `a - b`  | `sub(a, b)`      |
| `a * b`  | `mul(a, b)`      |
| `a / b`  | `truediv(a, b)`  |
| `a // b` | `floordiv(a, b)` |
| `a % b`  | `mod(a, b)`      |
| `a ** b` | `pow(a, b)`      |

## Bitwise

| Operator | Function       | Description          |
| -------- | -------------- | -------------------- |
| `a & b`  | `and_(a, b)`   | bitwise and          |
| `a | b`  | `or_(a, b)`    | bitwise or           |
| `a ^ b`  | `xor(a, b)`    | bitwise exclusive or |
| `~a`     | `invert(a)`    | bitwise inversion    |
| `a << b` | `lshift(a, b)` | left shift           |
| `a >> b` | `rshift(a, b)` | right shift          |

## Other

| Operator                            | Function             | Description                                                                             |
| ----------------------------------- | -------------------- | --------------------------------------------------------------------------------------- |
| `a + b`                             | `concat(a, b)`       | concatenation of sequences                                                              |
| `v in seq`                          | `contains(v, seq)`   | determine if `v` is in `seq`                                                            |
| `a @ b`                             | `matmul(a, b)`       | matrix multiplication                                                                   |
| `a is b`                            | `is_(a, b)`          | determines if a and b are the same object in memory;<br>not trailing underscore in name |
| `a is not b`                        | `is_not(a, b)`       | determines if a and b are not the same object in memory                                 |
| `not v`                             | `not(v)`             | negates the boolean value of v                                                          |
| `v` evaluated in<br>Boolean context | `truth(v)`           | returns the boolean value of v;<br>same as `bool(v)`                                    |
| `obj[k] = v`                        | `setitem(obj, k, v)` | indexed assignment                                                                      |
| `del obj[k]`                        | `delitem(obj, k)`    | indexed deletion                                                                        |
| `obj[k]`                            | `getitem(obj, k, v)` | indexed retrieval                                                                       |
