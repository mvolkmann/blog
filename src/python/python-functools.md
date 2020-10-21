---
eleventyNavigation:
  key: Python functools module
  order: 1.3
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

The `functools` module in the Python standard library
that provides "higher-order" functions
which are functions that accept or return other functions.
The most commonly used of these functions are demonstrated below.

In each of the code snippets that follow,
the following import is needed:

```python
from datetime import {function-name}
```

## `cache`

This is a decorator that memoizes a function,
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

## `partial`

This creates a `partial` object that is callable like a function
and calls a given function with provided initial arguments.
For example:

```python
from functools import partial

def relationship(feeling, name1, name2):
  return f'{name1} {feeling} {name2}.'

print(relationship('loves', 'Joanie', 'Chachi'))

# Provide one initial argument.
loves = partial(relationship, 'loves')
print(loves('Joanie', 'Chachi'))
print(loves('Mark', 'Tami'))

# Provide two initial arguments.
grinch_hates = partial(relationship, 'hates', 'The grinch')
print(grinch_hates('Christmas'))
print(grinch_hates('noise'))
```

## `partial_method`

This is similar to `partial`, but it creates
a new class method based on an existing class method.

## `reduce`

This reduces a sequence of values to a single value.
The result can be any kind of value including a primitive, object, or sequence.
For example:

```python
from functools import reduce
import math

# This is a simple example, but
# the built-in function "sum" and the "math.prod" function
# can be used instead.
numbers = [1, 7, 13, 21]
my_sum = reduce(lambda a, b: a + b, numbers)
my_product = reduce(lambda a, b: a * b, numbers)
print(my_sum, my_product) # 42, 1911
print(sum(numbers), math.prod(numbers)) # 42, 1911

# This is an example where results are accumulated in a dict.
items = [
    {'name': 'apples', 'price': 3.00},
    {'name': 'bananas', 'price': 2.50},
    {'name': 'grapes', 'price': 5.99},
    {'name': 'oranges', 'price': 4.50}
]

def find_extremes(acc, item):
    price = item['price']
    lowest = acc.get('lowest')
    highest = acc.get('highest')
    if not lowest or price < lowest['price']:
        acc['lowest'] = item
    if not highest or price > highest['price']:
        acc['highest'] = item
    return acc

results = reduce(find_extremes, items, {})
print(results)
# {
#     'lowest': {'name': 'bananas', 'price': 2.5},
#     'highest': {'name': 'grapes', 'price': 5.99}
# }
```

## `singledispatch`

This is a decorator that enables implementing multiple version of a function
that differ based on the type of the first argument.
For example:

```python
from functools import singledispatch

class MyClass:
    def __str__(self):
        return 'MyClass object'

@singledispatch
def log_me(arg):
    print('unsupported type', type(arg))

@log_me.register
def _(arg: bool):
    print('boolean', arg)

@log_me.register
def _(arg: int):
    print('int', arg)

@log_me.register
def _(arg: float):
    print('float', arg)

@log_me.register
def _(arg: str):
    print('str', arg)

@log_me.register
def _(arg: list):
    print('list', arg)

@log_me.register
def _(arg: tuple):
    print('tuple', arg)

@log_me.register
def _(arg: set):
    print('set', arg)

@log_me.register
def _(arg: dict):
    print('dict', arg)

@log_me.register
def _(arg: MyClass):
    print('MyClass', arg)

log_me(True) # boolean True
log_me(1) # int 1
log_me(1.2) # float 1.2
log_me('text') # str text
log_me([1, 2, 3]) # list [1, 2, 3]
log_me((1, 2, 3)) # tuple (1, 2, 3)
log_me({1, 2, 3}) # set {1, 2, 3}
log_me({'alpha': 1, 'beta': 2}) # dict {'alpha': 1, 'beta': 2}
obj = MyClass()
log_me(obj) # MyClass MyClass object
log_me(None) # unsupported type <class 'NoneType'>
log_me(range(3)) # unsupported type <class 'range'>
```

## `singledispatchmethod`

This is a decorator that is similar to the `singledispatch` decorator,
but it enables implementing multiple version of a method in a class.

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

k5 = Distance(5, DUnit.KM) # ~ 3.1 miles
k10 = Distance(10, DUnit.KM) # ~ 6.2 miles
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
