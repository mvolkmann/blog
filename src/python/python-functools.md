---
eleventyNavigation:
  key: Python functools module
  order: 1.3
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

The `functools` module in the Python standard library
that provides"higher-order" functions
which are functions that accept or return other functions.
The most commonly used of these functions are demonstrated below.

In each of the code snippets that follow,
the following import is needed:

```python
from datetime import {function-name}
```

## `cache`

This is a decorator that memoized a function,
which causes it to cache return values
so repeated calls with the same arguments return the cached value
rather that execute the function again.
For example:

```python
@cache
def add(n1, n2):
  print(f'computing {n1} + {n2}')
  return n1 + n2

print(add(1, 2))
print(add(2, 3))
print(add(1, 2))
```

## `lru_cache`

This is a decorator that is similar to `@cache`,
but only caches results for the `maxsize` most recent argument lists.
When the cache becomes full, the least recently used cached value is evicted.
The `maxsize` argument defaults to 128.
For example:

```python
from functools import lru_cache

@lru_cache(maxsize=3)
def add(n1, n2):
    print(f'computing {n1} + {n2}')
    return n1 + n2

print(add(1, 2)) # computes
print(add(1, 2)) # uses cache
print(add(2, 3)) # computes
print(add(2, 3)) # uses cache
print(add(3, 4)) # computes
print(add(3, 4)) # uses cache
print(add(4, 5)) # computes
print(add(4, 5)) # uses cache
print(add(1, 2)) # computes because previously cached value was evicted

print(add.cache_info())
# CacheInfo(hits=4, misses=5, maxsize=3, currsize=3)
```

## `total_ordering`

This adds comparison methods to a class based on an existing
`eq` method and at least one of the comparison methods
`le`, `lt`, `gt`, or `ge`.
For example:

```python
from enum import Enum
from functools import total_ordering
import math

M_TO_KM = 1.60934

class DUnit(Enum):
    KM = 1
    MILE = 2

def km(d):
    return d.mag if d.unit == DUnit.KM else d.mag * M_TO_KM

@total_ordering
class Distance:
    # unit must be a DUnit.
    def __init__(self, mag, unit):
        self.mag = mag
        self.unit = unit

    def __eq__(self, other):
        return math.isclose(km(self), km(other))

    def __lt__(self, other):
        return km(self) < km(other)

    def __str__(self):
        return str(self.mag) + ('km' if self.unit == DUnit.KM else ' mile')

k5 = Distance(5, DUnit.KM)
k10 = Distance(10, DUnit.KM)
m3 = Distance(3, DUnit.MILE)
m10 = Distance(10, DUnit.MILE)

races = [m10, k5, m3, k10]
races.sort()

for race in races:
    print(race)

assert races[0] == m3
assert races[-1] == m10
assert m3 < k5
assert m3 <= k5
assert k5 > m3
assert k5 >= m3
assert m3 != k5
```

## `partial`

```python

```

## `partial_method`

```python

```

## `reduce`

```python

```

## `singledispatch`

```python

```

## `singledispatchmethod`

```python

```

## `update_wrapper`

```python

```

## `wraps`

```python

```
