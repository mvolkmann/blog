---
layout: layout.njk
tags: navItem
title: YAML
---

[YAML](https://yaml.org/) stands for "YAML Ain't Markup Language".
In is an alternative to markup languages like JSON and XML.

YAML supports the same basic data types as JavaScript,
booleans, numbers, strings, arrays, and objects (a.k.a hash or dictionary).
Boolean values are `true` and `false`.
Number values are written as expected.
Strings containing no spaces or special characters do not require quotes.
When quotes are needed, they can be single or double quotes.

Arrays are specified by listing each element preceded by a dash and a space.
For example:

```yaml
colors:
  - red
  - green
  - blue
```

Objects (a.k.a dicts) are written as key/value pairs
on separate, indented lines where keys are followed by a colon.
For example:

```yaml
address:
  street: 123 Some Lane
  city: Somewhere
  state: CA
  zip: 12345
```

Arrays can hold objects and other arrays..
Object property values can be arrays and other objects.

Comments begin with `#` and extend to the end of the line.
Multi-line comments are not supported.
