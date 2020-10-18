---
eleventyNavigation:
  key: Python versions
  order: 1.1
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

This page documents the most significant additions and changes
in each version of Python starting with version 3.0.
If I have omitted changes that you feel are significant, please let
<a href="mailto:r.mark.volkmann@gmail.com?subject=Python version changes"
target="_blank">me</a> know and I will add them.

## Python 3.0 - December 3, 2008

- Strings use Unicode instead of ASCII.
- The `print` keyword was replaced by the `print` function.
- The `<>` operator was removed. Use `!=` instead.
- Some functions and methods now return "views" (like an iterator)
  instead of a `list` object.
- The `long` type was renamed to `int`.
- Expressions like `1 / 2` now return a `float` instead of an `int`.
- The `__cmp__` method is no longer supported
  and the `cmp` function should not be used.
- The default encoding for source code is now UTF-8.
- Non-ASCII letters are allowed in identifiers.
- The `StringIO` module is now in`io.StringIO`.
- Annotations can be added to function arguments
  to functions to describe the return value.
  The goal is to enable future tooling to use the annotations
  for purposes such as type checking and documentation generation.
- The `nonlocal` statement was added to allow functions to
  access non-local, non-global values.
- Iterable unpacking was added. For example:

  ```python
  a, b *rest = data
  a, *rest, b = data
  *rest, a, b = data
  ```

- Support for tuple unpacking in parameter lists was removed.
- List comprehension syntax changed slightly.
- Dictionary comprehensions were added.
- Support for `set` literals was added.
  For example, `{'one', 'two', 'three'}`
- Set comprehensions were added.
- Exception catching syntax changed from
  `exception ErrorName, var_name` is now
  `exception ErrorName as var_name`

## Python 3.1 - June 27, 2009

- The `OrderedDict` class was added in the `collections` module.
- New features were added to `unittest` including
  test skipping and new assert methods.
- tile support was added to tkinter
- Support for a new syntax for nested `with` statements.

## Python 3.2 - February 20, 2011

- The `concurrent.futures` module was added to support concurrent programming.
- Additions were made to the `shutil` module.
- The `sysconfig` module was added.
- The `argparse` command-line parsing module was added.
- Improvements were made to the `pdb` debugger.

## Python 3.3 - September 29, 2012

- The `unittest.mock` module was added to
  support implementing mock objects used by tests.
- The `venv` command for managing virtual environments was added.
- Support for generator delegation was added via `yield from` expressions.

## Python 3.4 - March 16, 2014

- The `asyncio` module was added.
- The `enum` module was added.
- The `pathlib` module was added.
- The `selectors` module was added.
- The `statistics` module was added.

## Python 3.5 - September 13, 2015

- The `typing` module for type hints was added.
- A new exception class, `RecursionError`, was added.
- Support for coroutines with `async` and `await` was added.
- Support for the matrix multiplication operator `@` was added.
  It not supported by built-in data types such as nested lists,
  but is supported by NumPy.

## Python 3.6 - December 23, 2016

- Support for formatting string literals (a.k.a. f-strings)
  with the syntax (`f'...'`) was added.
- Support for underscores in numeric literals was added.
  Single underscores can be added anywhere inside a literal number
  for readability and they are ignored.
- Support for variable annotations was added.
- Support for async comprehensions was added.
- Support for async generators was added.
- The `secrets` module was added.

## Python 3.7 - June 27, 2018

- The `contextvar` module was added.
- The `dateclasses` module was added.
- The `breakpoint` function was added.
- Preserving insertion order in `dict` objects was made official.
- The `time` module gained functions for nanosecond resolution.

## Python 3.8 - October 14, 2019

- Support for the `:=` "walrus operator"
  for assignments inside expressions was added.
- Support for position-only parameters with `/` syntax was added.
- Support for the `=` specifier was added to f-strings.

## Python 3.9 - October 5, 2020

- The string methods `removeprefix` and `removesuffix` were added.
- Support for the union operators `|` and `|=`
  for merging `dict` objects was added.
- The ability to add generics type hinting in standard collections was added.
- Python code is now parsed by a new PEG parser.
- An IANA Time Zone Database was added in the `zoneinfo` module.
- The `graphlib` module was added, with support for topological sorting.
- CPython will adopt an annual release cycle (like ECMAScript).
