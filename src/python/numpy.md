---
eleventyNavigation:
  key: NumPy library
  order: 5
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Overview

{% aTargetBlank "https://numpy.org/", "NumPy" %}
is a Python library for scientific computing.
It is primarily implemented in optimized C code.
It is an alternative to MatLab for use in Python.

NumPy supports many operations on matrices.
{% aTargetBlank "https://www.scipy.org/", "SciPy" %}
builds on NumPy to add support for even more mathematical operations.
NumPy is also a dependency of the pandas package.

If you have installed either SciPy or pandas, you already have NumPy.
Otherwise, install it by entering `pip install numpy`.
To use it, `import numpy as np`.

A primary feature of NumPy is that it uses NumPy arrays
to store and operate on data.
Typically the data in these arrays is numbers,
but it can also be any other type.
NumPy arrays have many advantages over Python lists including:

- Type checking of each element is not needed
  because NumPy arrays are homogeneous.
- Memory requirements are smaller because the representation of each
  data value requires less bytes than the equivalent Python representation.
- NumPy array elements are stored in contiguous memory, so access is faster.
- Some operations on element values can be performed in parallel by taking
  advantage of Single Instruction, Multiple Data (SIMD) vector processing.
- Memory caching can be more effective utilized
  which results in faster element access.

## Creating a NumPy Array

Many function described below accept a `dtype` argument
to specify the data type of the array elements.
When this is not specified, the data type is inferred or has a default.

In all function below that accept a number of rows and columns,
any number of dimension sizes can be specified, including one.

| To create ...                                                | Use ...                                                                                                                                                                                           |
| ------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| array from CSV file                                          | `a = np.genfromtxt(file_path, delimiter=',')`                                                                                                                                                     |
| 1D array from a list                                         | `a = np.array([v1, v2, ...])`                                                                                                                                                                     |
| 1D array from a tuple                                        | `a = np.array((v1, v2, ...))`                                                                                                                                                                     |
| 2+D array from lists                                         | `a = np.array([[v1, v2, ...], [w1, w2, ...]])`                                                                                                                                                    |
| 2+D array from tuples                                        | `a = np.array(((v1, v2, ...), (w1, w2, ...)))`                                                                                                                                                    |
| array with all zero values                                   | `a = np.zeros((rows, cols), dtype=np.type)`                                                                                                                                                       |
| array with all one values                                    | `a = np.ones((rows, cols), dtype=np.type)`                                                                                                                                                        |
| array with all a specific value                              | `a = np.full((rows, cols), value, dtype=np.type)`                                                                                                                                                 |
| array with same shape as another<br>and all a specific value | `a = np.full_like(other_array, v), value, dtype=np.type)` or<br>`a = np.full(other_array.shape, v, dtype=np.type)`                                                                                |
| array with uninitialized values                              | `a = np.empty((rows, cols), dtype=np.type)`<br>Values come from whatever happens to be at the memory location.                                                                                    |
| 1D array with range of values                                | `a = np.arange(start, end, step, dtype=np.type)`<br>Values can be integer or float.<br>`start` is inclusive and defaults to 0.<br>`end` is exclusive and has no default.<br>`step` defaults to 1. |
| 1D array with evenly spaced values                           | `a = np.linspace(start, end, count, dtype=np.type)`<br>`end` is inclusive.<br>`count` is the number values to produce.                                                                            |
| array with random float values                               | `a = np.random.rand(rows, cols)`<br>Values are between 0 (inclusive) and 1 (exclusive).                                                                                                           |
| array with random integer values                             | `a = np.random.randint(min, max, size=(rows, cols))`<br>`min` defaults to zero and is inclusive.<br>`max` has no default and is exclusive.                                                        |
| identity matrix (creates n x n array)                        | `a = np.identity(n, dtype=np.type)`                                                                                                                                                               |
| copy of existing array                                       | `b = a.copy()`                                                                                                                                                                                    |
| copy of existing array, but use specified data type          | `b = a.astype(np.type)`<br>truncates if necessary                                                                                                                                                 |
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

## Operations on One Array

NumPy supports many operations that produce new arrays
from the elements in existing arrays.

Here is a sampling of some that create a new array
from the elements of an existing array.

| Operation                                                         | Code                                                                                                                                          |
| ----------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| add and create new array                                          | `b = a + v`                                                                                                                                   |
| add in place                                                      | `a += v`                                                                                                                                      |
| subtract                                                          | `b = a - v`                                                                                                                                   |
| multiply                                                          | `b = a \* v`                                                                                                                                  |
| divide                                                            | `b = a / v`                                                                                                                                   |
| reciprocal                                                        | `b = np.reciprocal(a)`<br>works with floats, but not integers                                                                                 |
| exponentiation                                                    | `b = a \*\* v`                                                                                                                                |
| square                                                            | `b = np.square(a)`                                                                                                                            |
| square root                                                       | `b = np.sqrt(a)`                                                                                                                              |
| sine                                                              | `b = np.sin(a)`                                                                                                                               |
| cosine                                                            | `b = np.cos(a)`                                                                                                                               |
| tangent                                                           | `b = np.tan(a)`                                                                                                                               |
| absolute value                                                    | `b = np.absolute(a)` or<br>`b = np.fabs(a)`                                                                                                   |
| round to nearest integer                                          | `b = np.rint(a)`                                                                                                                              |
| floor                                                             | `b = np.floor(a)`                                                                                                                             |
| ceiling                                                           | `b = np.ceil(a)`                                                                                                                              |
| truncate                                                          | `b = np.trunc(a)`                                                                                                                             |
| sign (-1, 0, or 1)                                                | `b = np.sign(a)`                                                                                                                              |
| convert degrees to radians                                        | `b = np.radians(a)` or `b = np.deg2rad(a)`                                                                                                    |
| convert radians to degrees                                        | `b = np.degrees(a)` or `b = np.rad2deg(a)`                                                                                                    |
| create array of same shape<br>containing boolean values           | `b = {condition involving a}`<br>can use all relational and logical operators<br>example: `b = a > 50`<br>example: `b = (a > 50) & (a < 100)` |
| create 1D array of elements that meet criteria                    | `b = a[{condition involving a}]`<br>example: `b = a[a > 50]`                                                                                  |
| create 1D array of elements<br>in a 1D array at specified indexes | `b = a[[i1, i2, ...]]`                                                                                                                        |
| determine if any value in a row meets criteria                    | `b = np.any({condition involving a}, axis=1)`<br>returns 1D array of booleans for each row                                                    |
| determine if any value in a column meets criteria                 | `b = np.any({condition involving a}, axis=0)`<br>returns 1D array of booleans for each column                                                 |
| determine if all values in a row meets criteria                   | `b = np.all({condition involving a}, axis=1)`<br>returns 1D array of booleans for each row                                                    |
| determine if all values in a column meets criteria                | `b = np.all({condition involving a}, axis=0)`<br>returns 1D array of booleans for each column                                                 |

## Operations on Two Arrays

Here is a sampling of operations that
combine corresponding elements of two arrays
to create elements in a new array.

| Operation               | Code                                                                                    |
| ----------------------- | --------------------------------------------------------------------------------------- |
| add elements            | `b = np.add(a1, a2)`                                                                    |
| subtract elements       | `b = np.subtract(a1, a2)`                                                               |
| multiply elements       | `b = np.multiply(a1, a2)`                                                               |
| divide elements         | `b = np.divide(a1, a2)`                                                                 |
| maximum                 | `b = np.maximum(a1, a2)` or<br>`b = np.fmax(a1, a2)`<br>element-wise only for 1D arrays |
| minimum                 | `b = np.minimum(a1, a2)` or<br>`b = np.fmin(a1, a2)`<br>element-wise only for 1D arrays |
| greatest common divisor | `b = np.gcd(a1, a2)`<br>can also pass two numbers                                       |
| lowest common multiple  | `b = np.lcm(a1, a2)`<br>can also pass two numbers                                       |

Here is a sampling of other operations on two arrays.

| Operation             | Code                                                                                               |
| --------------------- | -------------------------------------------------------------------------------------------------- |
| minimum element       | `v = np.min(a)`                                                                                    |
| maximum element       | `v = np.max(a)`                                                                                    |
| sum of elements       | `v = np.sum(a)`                                                                                    |
| determinant           | `v = np.linalg.det(a)`                                                                             |
| matrix multiplication | `b = np.matmul(a1, a2)`<br>The number of columns in `a1` must<br>equal the number of rows in `a2`. |

The `np.linalg` package provides many more functions in addition to `det`.
See {% aTargetBlank
"https://numpy.org/doc/stable/reference/routines.linalg.html",
"Linear algebra (numpy.linalg)" %}.
