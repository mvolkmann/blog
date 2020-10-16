---
eleventyNavigation:
  key: Python collections module
  order: 1.5
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Built-in Collection Classes

Python provides four collection classes that do not need to be imported to use.

| Collection<br>Name | Mutable? | Ordered? | Allows<br>Duplicates? | Can<br>Index? | Syntax                  |
| ------------------ | -------- | -------- | --------------------- | ------------- | ----------------------- |
| `list`             | yes      | yes      | yes                   | yes           | `[e1, e2, ...]`         |
| `tuple`            | no       | yes      | yes                   | yes           | `(e1, e2, ...)`         |
| `set`              | yes      | no       | no                    | no            | `{e1, e2, ...}`         |
| `dict`             | yes      | yes      | yes                   | no            | `{k1: v1, k2: v2, ...}` |

For details on the `list` and `tuple` classes,
see [here](/blog/python/python-compared-to-js/#sequences).

For details on the `set` class,
see [here](/blog/python/python-compared-to-js/#sets).

For details on the `dict` class,
see [here](/blog/python/python-compared-to-js/#key-value-collections).

## `collections` Module Classes

The `collections` module in the Python standard library defines
nine additional collection classes that must be imported to use.

| Collection Name | Description                                                   |
| --------------- | ------------------------------------------------------------- |
| `namedtuple`    | `tuple` subclass with named fields                            |
| `deque`         | `list`-like container with fast appends and pops on both ends |
| `Chainmap`      | `dict`-like view of multiple mappings                         |
| `Counter`       | `dict` subclass for counting hashable values                  |
| `OrderedDict`   | `dict` subclass that remembers insertion order                |
| `defaultdict`   | `dict` subclass that calls a function to get missing values   |
| `UserDict`      | wrapper around a `dict` object for easier subclassing         |
| `UserList`      | wrapper around a `list` object for easier subclassing         |
| `UserString`    | wrapper around a `string` object for easier subclassing       |

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
# Counter({'banana': 4, 'apple': 3, 'grape': 2,
#          'cherry': 1, 'orange': 1, 'strawberry': 1})

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

An {% aTargetBlank
"https://docs.python.org/3/library/collections.html#collections.OrderedDict",
"`OrderedDict`" %} is a special `dict` that
preserves the order in which keys were added.
Starting in Python 3.5, the `dict` class also does this.
One reason to continue using `OrderedDict` instead of `dict`
when order must be preserved is that
assuming `dict` objects will preserve order will cause
code to break when run in versions of Python before 3.5.

Here is an example of creating and using an `OrderedDict`:

```python
from collections import OrderedDict

# Create and populate an OrderedDict.
dogs = OrderedDict()
dogs['Maisey'] = 'Treeing Walker Coonhound'
dogs['Ramsay'] = 'Native American Indian Dog'
dogs['Oscar'] = 'German Shorthaired Pointer'
dogs['Comet'] = 'Whippet'

print(dogs)
# OrderedDict([
#  ('Maisey', 'Treeing Walker Coonhound'),
#  ('Ramsay', 'Native American Indian Dog'),
#  ('Oscar', 'German Shorthaired Pointer'),
#  ('Comet', 'Whippet')])

# Change the value of a key,
# noting that its position does not change.
dogs['Oscar'] = 'GSP'

# Move "Comet" to the beginning.
dogs.move_to_end('Comet', last=False)

# Move "Ramsay" to the end.
dogs.move_to_end('Ramsay')

print(dogs)
# OrderedDict([
#  ('Comet', 'Whippet'),
#  ('Maisey', 'Treeing Walker Coonhound'),
#  ('Oscar', 'GSP'),
#  ('Ramsay', 'Native American Indian Dog')])

# Remove the first key/value pair.
dogs.popitem(last=False)

# Remove the last key/value pair.
dogs.popitem()

print(dogs)
# OrderedDict([
#  ('Maisey', 'Treeing Walker Coonhound'),
#  ('Oscar', 'GSP')])
```

### `defaultdict`

A {% aTargetBlank
"https://docs.python.org/3/library/collections.html#collections.defaultdict",
"`defaultdict`" %} is a special `dict` that can
provide a value for missing keys rather than raise a `KeyError`.
This is useful because it enables writing code
that doesn't need to check for missing values.

The `defaultdict` function is passed a function that
must return the default value to use for missing keys.
This can be a constructor function from a built-in class,
a custom class, a lambda function, or a normal function.
Unfortunately the function is not passed the key that is missing,
so that cannot be used to determine the value to return.

```python
# Each of the examples below indicate
# the value that is used for missing keys.
my_dict = defaultdict(bool) # False
my_dict = defaultdict(int) # 0
my_dict = defaultdict(float) # 0.0
my_dict = defaultdict(str) # empty string
my_dict = defaultdict(list) # empty list
my_dict = defaultdict(tuple) # empty tuple
my_dict = defaultdict(dict) # empty dict
my_dict = defaultdict(set) # empty set
my_dict = defaultdict(MyClass) # an instance of MyClass
my_dict = defaultdict(lambda: 'missing') # the string "missing"
my_dict = defaultdict(lambda: 7) # the number 7

# This function is only called once for each missing key.
# The missing key and the value this returns are
# added to the defaultdict so it is no longer missing.
def get_missing():
    print('in get_missing')
    return 'mixed'

dogs = defaultdict(get_missing) # returns "mixed" for missing keys
```

Here is an example of creating and using a `defaultdict`:

```python
from collections import defaultdict

names_by_breed = defaultdict(list)

def add_dog(name, breed):
    # Don't need to check for missing breed key
    # because an empty list will be supplied.
    names_by_breed[breed].append(name)

add_dog('Comet', 'Whippet')
add_dog('Maisey', 'Treeing Walker Coonhound')
add_dog('Reece', 'Whippet')

print('Whippet names are', names_by_breed['Whippet']) # ['Comet', 'Reece']
print('Beagle names are', names_by_breed['Beagle']) # []

fruit_counts = defaultdict(int)

def add_fruit(name):
    # Don't need to check for missing fruit key
    # because zero will be supplied.
    fruit_counts[name] += 1

add_fruit('banana')
add_fruit('apple')
add_fruit('banana')

print('banana count is', fruit_counts['banana']) # 2
print('grape count is', fruit_counts['grape']) # 0
```

### `UserDict`, `UserList`, and `UserString`

These classes are useful as the base class of custom classes
that need the functionality of a `dict`, `list`, or `string`.
In older versions of Python it is not possible to directly
inherit from `dict`, `list`, and `string`,
but doing so is now supported.
However, it can still be easier to inherit from these "User" classes
instead because the underlying `dict`, `list`, or `string`
will be available as an attribute.
