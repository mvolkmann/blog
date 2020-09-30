---
eleventyNavigation:
  key: NumPy
  order: 5
  parent: Python
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://numpy.org/", "NumPy" %}
is a Python library for scientific computing.

NumPy is a dependency of the pandas package,
so if you have installed that, you already have NumPy.
Otherwise, install it by entering `pip install numpy`.

A primary feature of NumPy is that it uses NumPy arrays
to store and operate on data.
These have many advantages over Python lists including:

- Type checking of each element is not needed
  because NumPy arrays are homogeneous.
- Memory requirements are smaller because the representation of each
  data value requires less bytes than the equivalent Python representation.
- NumPy array elements are stored in contiguous memory, so access is faster.
- Some operations on element values can be performed in parallel by taking
  advantage of Single Instruction, Multiple Data (SIMD) vector processing.
- Memory caching can be more effective utilized
  which results in faster element access.

NumPy supports many operations on matrices.
SciPy supports even more. Is it seen as a superset of NumPy?

## Using NumPy

```python
import numpy as np
```

## Creating a NumPy Array

Many function described below accept a `dtype` argument
to specify the data type of the array elements.
When this is not specified, the data type is inferred or has a default.

In all function below that accept a number of rows and columns,
any number of dimension sizes can be specified, including one.

| To create ...                                                | Use ...                                                                                                                                                                                           |
| ------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1D array from a list                                         | `a = np.array([v1, v2, ...])`                                                                                                                                                                     |
| 1D array from a tuple                                        | `a = np.array((v1, v2, ...))`                                                                                                                                                                     |
| 2+D array from lists                                         | `a = np.array([[v1, v2, ...], [w1, w2, ...]])`                                                                                                                                                    |
| 2+D array from tuples                                        | `a = np.array(((v1, v2, ...), (w1, w2, ...)))`                                                                                                                                                    |
| array with all zero values                                   | `a = np.zeros((rows, cols), dtype=np.type)`                                                                                                                                                       |
| array with all one values                                    | `a = np.ones((rows, cols), dtype=np.type)`                                                                                                                                                        |
| array with all a specific value                              | `a = np.full((rows, cols), value, dtype=np.type)`                                                                                                                                                 |
| array with same shape as another<br>and all a specific value | `a = np.full_like(other_array, v), value, dtype=np.type)` or<br>`a = np.full(other_aray.shape, v, dtype=np.type)`                                                                                 |
| array with uninitialized values                              | `a = np.empty((rows, cols), dtype=np.type)`<br>Values come from whatever happens to be at the memory location.                                                                                    |
| 1D array with range of values                                | `a = np.arange(start, end, step, dtype=np.type)`<br>Values can be integer or float.<br>`start` is inclusive and defaults to 0.<br>`end` is exclusive and has no default.<br>`step` defaults to 1. |
| 1D array with evenly spaced values                           | `a = np.linspace(start, end, count, dtype=np.type)`<br>`end` is inclusive.<br>`count` is the number values to produce.                                                                            |
| array with random float values                               | `a = np.random.rand(rows, cols)`<br>Values are between 0 (inclusive) and 1 (exclusive).                                                                                                           |
| array with random integer values                             | `a = np.random.randint(min, max, size=(rows, cols))`<br>`min` defaults to zero and is inclusive.<br>`max` has no default and is exclusive.                                                        |
| identity matrix (creates n x n array)                        | `a = np.identity(n, dtype=np.type)`                                                                                                                                                               |
| copy of existing array                                       | `b = a.copy()`                                                                                                                                                                                    |
| repeated copies elements in existing array                   | `b = np.repeat(a, times, axis={0|1})`<br>axis is 0 for vertical and 1 for horizontal.                                                                                                             |

## Getting Information About an Array

| Information                                     | Attribute    |
| ----------------------------------------------- | ------------ |
| number of dimensions                            | `a.dim`      |
| shape (size of each dimension)                  | `a.shape`    |
| data type of elements                           | `a.dtype`    |
| number of elements (product of dimension sizes) | `a.size`     |
| size of each element in bytes                   | `a.itemsize` |
| total size (`a.size * a.itemsize`)              | `a.nbytes`   |

## Element Access

In all function below that accept row and column indexes,
any number of indexes can be specified, including one.

In the tables that follow, some abbreviations are used for arguments.

- Row index values below are abbreviated as `ri`.
- Column index values below are abbreviated as `ci`.
- Row start indexes (`rsi`) and column start indexes (`csi`)
  are inclusive and default to zero.
- Row end indexes (`rei`) and column end indexes (`cei`)
  are exclusive and default to length.
- Negative index values can be used to count from the end of a dimension
  where `-1` is the last element.

| Operation                                   | Code                                                  |
| ------------------------------------------- | ----------------------------------------------------- |
| get element value                           | `a[ri, ci]`                                           |
| get entire row                              | `a[ri, :]`                                            |
| get entire column                           | `a[:, ci]`                                            |
| get specific columns from a row             | `a[ri, [ci1, ci2, ...]]`                              |
| get range of columns from a row             | `a[ri, csi:cei:step]`                                 |
| set element value                           | `a[ri, ci] = v`                                       |
| set all elements in a row to a value        | `a[ri, :] = v`                                        |
| set all elements in a column to a value     | `a[:, ci] = v`                                        |
| set all elements in a row to values         | `a[ri, :] = [v1, v2, ...]`<br>must provide all values |
| set all elements in a column to values      | `a[:, ci] = [v1, v2, ...]`<br>must provide all values |
| set range of elements in a row to values    | `a[ri, csi:cei] = [v1, v2, ...]`                      |
| set range of elements in a column to values | `a[rsi:rei, ci] = [v1, v2, ...]`                      |

More detail is coming soon!
