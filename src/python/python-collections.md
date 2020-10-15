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
| `Counter`       | `dict` subclass for counting hashable values                                          |
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

A {% aTargetBlank
"https://docs.python.org/3/library/collections.html#collections.ChainMap",
"`ChainMap`" %} provides a view over multiple dict objects.

Here is an example of creating and using a `ChainMap`:

```python
from collections import ChainMap

dict1 = {
    'Maisey': 'Treeing Walker Coonhound',
    'Ramsay': 'Native American Indian Dog'
}
dict2 = {
    'Oscar': 'German Shorthaired Pointer',
    'Comet': 'Whippet'
}

# Create a ChainMap that provides a view of the two dicts above.
cm = ChainMap(dict1, dict2)
print(cm) # ChainMap({'Maisey': 'Tree...', 'Ramsay': 'Native...'}, {'Oscar': 'German...', 'Comet': 'Whippet'})

# Get values of some keys from the ChainMap.
print(cm['Ramsay']) # Native American Indian Dog
print(cm['Comet']) # Whippet

# Modify one of the dicts and verify that the ChainMap sees the change.
dict2['Oscar'] = 'GSP'
print(cm['Oscar']) # GSP

# Add data to one of the dicts and verify that the ChainMap sees it.
dict1['Snoopy'] = 'Beagle'
print(cm['Snoopy']) # Beagle

# Get the dicts used by the ChainMap.
dicts = cm.maps # returns list containing dict1 and dict2
print(dicts)

# Add a key/value pair to the ChainMap
# which adds it to the first associated dict.
# It's better to add to associated dicts.
cm['Reece'] = 'Whippet'

# Remove a key/value pair from the ChainMap
# which only removes from the first associated dict
# and raises KeyError if not found there.
# It's better to remove from to associated dicts.
del cm['Reece']
```

### `Counter`

A {% aTargetBlank
"https://docs.python.org/3/library/collections.html#collections.Counter",
"`Counter`" %} is a special `dict` that
counts occurrences of unique values in an iterable.

Here is an example of creating and using a `Counter`:

```py
from collections import Counter

fruits = [
    'apple', 'banana', 'cherry', 'grape', 'orange', 'strawberry',
    'banana', 'apple', 'apple', 'grape', 'banana', 'banana'
]

counter = Counter(fruits)

print(counter)

# Get the number of occurrences of banana.
print(counter['banana']) # 4

# Remove all occurrences of grape.
del counter['grape']

# Get a list of all the elements in sorted order.
print(list(counter.elements()))
# ['apple', 'apple', 'apple', 'banana', 'banana', 'banana', 'banana',
#  'cherry', 'orange', 'strawberry']

# Get a list of tuples where each contains a unique value
# and its count (number of occurrences)
# where the tuples are sorted by descending count.
print(counter.most_common())
# [('banana', 4), ('apple', 3), ('cherry', 1), ('orange', 1), ('strawberry', 1)]

# Remove some occurrences.
counter.subtract({'banana': 2, 'strawberry': 1})
print(list(counter.elements()))
# ['apple', 'apple', 'apple', 'banana', 'banana', 'cherry', 'orange']
print(counter.most_common())
# [('apple', 3), ('banana', 2), ('cherry', 1), ('orange', 1), ('strawberry', 0)]
```

### `OrderedDict`

### `defaultdict`

### `UserDict`

### `UserList`

### `UserString`
