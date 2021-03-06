---
eleventyNavigation:
  key: YAML
layout: topic-layout.njk
---

[YAML](https://yaml.org/) stands for "YAML Ain't Markup Language".
In is an alternative to markup languages like JSON and XML.

YAML supports the same basic data types as JavaScript,
booleans, numbers, strings, arrays, and objects (a.k.a hash or dictionary).

**Booleans** are `true` and `false`.

**Numbers** are written as expected.

**Strings** containing no spaces or special characters do not require quotes.
When quotes are needed, they can be single or double quotes.
Multi-line strings start with `>` and continue until the end of their indentation.
For example:

```yaml
lifeStory: >
  Come and listen to my story `bout a man named Jed.
  A poor mountaineer, barely kept his family fed.
```

**Arrays** are specified by listing each element preceded by a dash and a space.
For example:

```yaml
colors:
  - red
  - green
  - blue
```

**Objects** (a.k.a dicts) are written as key/value pairs
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

**Comments** begin with `#` and extend to the end of the line.
Multi-line comments are not supported.
