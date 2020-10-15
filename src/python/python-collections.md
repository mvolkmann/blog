---
eleventyNavigation:
  key: Python Collections
  order: 1.5
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

Python provides
four collection classes that do not need to import and
nine more that can be imported from
the collections module in the standard library.
The table below summarizes these:

## Built-in Collection Classes

Python provides four collection classes that do not need to be imported to use.

| Collection Name | Mutable? | Ordered? | Allows Duplicates? | Can Index | Syntax                  |
| --------------- | -------- | -------- | ------------------ | --------- | ----------------------- |
| `list`          | yes      | yes      | yes                | yes       | `[e1, e2, ...]`         |
| `tuple`         | no       | yes      | yes                | yes       | `(e1, e2, ...)`         |
| `set`           | yes      | no       | no                 | no        | `{e1, e2, ...}`         |
| `dict`          | yes      | yes      | yes                | no        | `{k1: v1, k2: v2, ...}` |

Why do these class names start with a lowercase letter?

For details on the `list` and `tuple` classes,
see [here](/blog/python/python-compared-to-js/#sequences).

For details on the `set` class,
see [here](/blog/python/python-compared-to-js/#sets).

For details on the `dict` class,
see [here](/blog/python/python-compared-to-js/#key-value-collections).

## Collection Classes in `collections` module

The collections module in the Python standard library
defines nine addition collection classes that most be imported to use.

| Collection Name | Description                                                                           |
| --------------- | ------------------------------------------------------------------------------------- |
| `namedtuple`    | function that creates a `tuple` subclass with named fields                            |
| `deque`         | function that creates a `list`-like container with fast appends and pops on both ends |
| `Chainmap`      | a `dict`-like view of multiple mappings                                               |
| `Counter`       | `dict` subclass for counting hashable objects                                         |
| `OrderedDict`   | `dict` subclass that remembers the order in which entries were added                  |
| `defaultdict`   | `dict` subclass that calls a function to get missing values                           |
| `UserDict`      | wrapper around `dict` objects for easier subclassing                                  |
| `UserList`      | wrapper around `list` objects for easier subclassing                                  |
| `UserString`    | wrapper around `string` objects for easier subclassing                                |

TODO: Is OrderedDict deprecated since dict now remembers insertion order?

### `namedtuple`

A {% aTargetBlank
"https://docs.python.org/3/library/collections.html#collections.namedtuple",
"`namedtuple`" %} has all the features of a `tuple`,
but differs in that each position has an associated name.

Here is an example of creating and using `namedtuple` instances:

```python
from collections import namedtuple

# Define a namedtuple.
DogNT = namedtuple('Dog', 'breed, name')

# Create an instance with positional arguments.
dog1 = DogNT('Whippet', 'Comet')
print(dog1)

# Create an instance with named arguments.
dog2 = DogNT(name='Oscar', breed='German Shorthaired Pointer')
print(dog2)

# Access a field by name.
print(dog1.name) # Comet

# All methods from the tuple class are inherited.

# Get the name of a namedtuple instance.
print(type(dog1).__name__) # Dog

# Make a new instance from an iterable.
data = ['Native American Indian Dog', 'Ramsay']
dog3 = DogNT._make(data) # Why start with underscore?
print(dog3)

# Get a dict from a namedtuple.
print(dog1._asdict())

# Create a new instance from an existing one
# where some values are replaced.
print(dog2._replace(name='Oscar Wilde'))

# Get tuple of field names from an instance.
print(dog1._fields) # ('breed', 'name')
```

### `deque`

A {% aTargetBlank
"https://docs.python.org/3/library/collections.html#collections.deque",
"`deque`" %} (pronounced "deck") is a double-ended queue.
Unlike the `list` class, elements can be efficiently
appended to and popped from both ends.

Here is an example of creating and using `deque` instances:

```python
from collections import deque

# Create an empty deque and then append values to both ends.
dogs = deque()
dogs.append('Oscar')
dogs.appendleft('Ramsay')
dogs.append('Comet')
dogs.appendleft('Maisey')
print(dogs) # deque(['Maisey', 'Ramsay', 'Oscar', 'Comet'])

# Create a deque with initial values.
dogs = deque(['Maisey', 'Ramsay', 'Oscar', 'Comet'])
print(dogs) # deque(['Maisey', 'Ramsay', 'Oscar', 'Comet'])

# Remove the first and last elements.
dogs.pop()
dogs.popleft();
print(dogs) # deque(['Ramsay', 'Oscar'])

# Add multiple elements at the beginning (left) and end (right).
dogs.extendleft(['Snoopy', 'Spot']) # added in reverse order
dogs.extend(['Speed', 'Trixie'])
print(dogs) # deque(['Spot', 'Snoopy', 'Ramsay', 'Oscar', 'Speed', 'Trixie'])

# Insert an element before a given index.
index = 3
dogs.insert(index, 'Reece')
print(dogs) # deque(['Spot', 'Snoopy', 'Ramsay', 'Reece', 'Oscar', 'Speed', 'Trixie'])

# Remove the element that was just inserted.
del dogs[index]
print(dogs) # deque(['Spot', 'Snoopy', 'Ramsay', 'Oscar', 'Speed', 'Trixie'])

# Remove all elements.
dogs.clear();
print(dogs) # deque([])
```

### `Chainmap`
