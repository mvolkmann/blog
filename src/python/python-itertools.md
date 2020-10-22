---
eleventyNavigation:
  key: Python itertools module
  order: 1.3
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

The {% aTargetBlank
"https://docs.python.org/3/library/itertools.html",
"`itertools`" %} module in the Python standard library
provides functions for creating iterators.
The most commonly used of these functions are demonstrated below.

## Infinite iterators

The following functions return an iterator that never stop.
This means they never raise the `StopIteration` error.

### `count`

This function returns an iterator that produces integers
starting from a given value, in steps of one or a specified step.
For example:

```python
from itertools import count

for n in count(10, 5):
    print(n)
    if n >= 30:
        break
```

### `cycle`

This function returns an iterator that produces provided values
in an endless cycle.
For example:

```python
from itertools import cycle

count = 0
for letter in cycle(('alpha', 'beta', 'gamma')):
    print(letter)
    count += 1
    if count == 6:
        break
```

### `repeat`

This function returns an iterator that produces an endless sequence
of a single value, or optionally only a given number of times.
For example:

```python
from itertools import repeat

count = 0
for result in repeat('win'):
    print(result) # win 3 times
    count += 1
    if count == 3:
        break

for result in repeat('lose', 2):
    print(result) # lose 2 times
```

## Iterators that terminate based on a sequence

### `accumulate`

This function is similar to the `reduce` function in the `functools` module
but returns an iterator that makes intermediate results available.
By default it adds numbers in an iterable (using `operator.add`),
but a different function can be supplied.
By default the initial value is `None`,
but a different initial value can be specified.
For example:

```python
from itertools import accumulate

items = [
    {'name': 'apples', 'price': 3.00},
    {'name': 'bananas', 'price': 2.50},
    {'name': 'grapes', 'price': 5.99},
    {'name': 'oranges', 'price': 4.50}
]

def add_price(acc, item):
    return acc + item['price']

for total in accumulate(items, func=add_price, initial=0):
    print(total) # 0, 3.0, 5.5, 11.49, 15.99
```

### `chain`

This function creates an iterator over multiple iterables.
For example:

```python
from itertools import chain

primary = ('red', 'yellow', 'blue')
secondary = ('orange', 'green', 'purple')
other = ('magenta', 'cyan')

for color in chain(primary, secondary, other):
    print(color) # each color in each tuple
```

### `chain.from_iterable`

This function is similar to `chain`,
but its input is an iterable containing iterables.
For example:

```python
from itertools import chain

primary = ('red', 'yellow', 'blue')
secondary = ('orange', 'green', 'purple')
other = ('magenta', 'cyan')
colors = (primary, secondary, other)

for color in chain.from_iterable(colors):
    print(color) # each color in each tuple
```

### `compress`

This function creates an iterator that iterates over
the values in on iterable called "data" whose
corresponding value in another iterable called "selectors"
evaluates to `True`.
For example:

```python
from itertools import compress

colors = ['red', 'orange', 'yellow', 'green', 'blue', 'purple']
like = [False, True, True, False, True, False]

for color in compress(colors, like):
    print(color) # orange, yellow, blue
```

### `dropwhile`

This function creates an iterator that iterates over a given iterable,
but skips all the initial elements
for which a predicate function evaluates to `True`.
It is the opposite of `takewhile`.
For example:

```python
from itertools import dropwhile
from operator import itemgetter

teams = [
    {'name': '49ers', 'wins': 3, 'losses': 3},
    {'name': 'Bears', 'wins': 5, 'losses': 1},
    {'name': 'Cardinals', 'wins': 4, 'losses': 2},
    {'name': 'Lions', 'wins': 2, 'losses': 3},
    {'name': 'Packers', 'wins': 4, 'losses': 1},
    {'name': 'Rams', 'wins': 4, 'losses': 2},
    {'name': 'Seahawks', 'wins': 5, 'losses': 0},
    {'name': 'Vikings', 'wins': 1, 'losses': 5}
]

# Sort teams on their records with best teams first.
teams.sort(key=itemgetter('wins', 'losses'), reverse=True)

def winning_record(team):
    return team['wins'] > team['losses']

# Iterate over teams that do not have a winning record.
for team in dropwhile(winning_record, teams):
    print(team['name']) # 49ers, Lions, Vikings
```

### `filterfalse`

This function creates an iterator that
iterates over the elements of a given iterable
for which a predicate function returns false.
For example:

```python
from itertools import filterfalse

teams = [
    {'name': '49ers', 'wins': 3, 'losses': 3},
    {'name': 'Bears', 'wins': 5, 'losses': 1},
    {'name': 'Cardinals', 'wins': 4, 'losses': 2},
    {'name': 'Lions', 'wins': 2, 'losses': 3},
    {'name': 'Packers', 'wins': 4, 'losses': 1},
    {'name': 'Rams', 'wins': 4, 'losses': 2},
    {'name': 'Seahawks', 'wins': 5, 'losses': 0},
    {'name': 'Vikings', 'wins': 1, 'losses': 5}
]

def winning_record(team):
    return team['wins'] > team['losses']

# Iterate over teams that do not have a winning record.
print('Losing')
for team in filterfalse(winning_record, teams):
    print('-', team['name']) # 49ers, Lions, Vikings

# Iterate over teams that have a winning record.
print('Winning')
for team in filter(winning_record, teams):
    print('-', team['name']) # Bears, Cardinals, Packers, Rams, Seahawks
```

### `groupby`

This function creates an iterator that iterates over tuples that
each contain a unique key value and an iterable of objects that match that key.
The example below groups football teams by their division.
So the tuples will contain a division value
and an iterable over the teams in that division.
The data must be sorted so that
all the elements with the same key value are together.

```python
from itertools import groupby
from operator import itemgetter

teams = [
    {'division': 'NFC East', 'name': '49ers', 'wins': 3, 'losses': 3},
    {'division': 'NFC North', 'name': 'Bears', 'wins': 5, 'losses': 1},
    {'division': 'NFC East', 'name': 'Cardinals', 'wins': 4, 'losses': 2},
    {'division': 'NFC North', 'name': 'Lions', 'wins': 2, 'losses': 3},
    {'division': 'NFC North', 'name': 'Packers', 'wins': 4, 'losses': 1},
    {'division': 'NFC East', 'name': 'Rams', 'wins': 4, 'losses': 2},
    {'division': 'NFC East', 'name': 'Seahawks', 'wins': 5, 'losses': 0},
    {'division': 'NFC North', 'name': 'Vikings', 'wins': 1, 'losses': 5}
]

# Sort teams on their division.
teams.sort(key=itemgetter('division'))

# The teams list must be sorted on the groupby key,
# "division" in this case.
for division, div_teams in groupby(teams, key=lambda team: team['division']):
    print(division)
    for team in div_teams:
        print('-', team['name'])
# Output is:
# NFC East
# - 49ers
# - Cardinals
# - Rams
# - Seahawks
# NFC North
# - Bears
# - Lions
# - Packers
# - Vikings
```

### `islice`

This function creates an iterator that iterates over a subset of an iterable.
In the examples below, we want to iterate over
just some of the values in the `colors` list.

```python
from itertools import islice

colors = ['red', 'orange', 'yellow', 'green', 'blue', 'purple']

# Iterate over the first two elements.
for color in islice(colors, 2):
    print(color) # red, orange

print()

# Iterate over elements from index 2 inclusive to index 5 exclusive.
for color in islice(colors, 2, 5):
    print(color) # yellow, green, blue

print()

# Iterate oer every other element starting from index 1
# and going to the end of the list.
for color in islice(colors, 1, len(colors), 2):
    print(color) # orange, green, purple
```

### `starmap`

This function creates an iterator over the results of calling a given function,
using the tuple elements from an iterable as the arguments.
For example:

```python
from itertools import starmap

boxes = [
  (2, 3, 4),
  (3, 4, 5),
  (4, 2, 6)
]

def compute_volume(width, depth, height):
  return width * depth * height

for volume in starmap(compute_volume, boxes):
  print(volume) # 24, 60, 48

# Alternate approach, iterating over boxes.
for box in boxes:
  print(compute_volume(*box)) # 24, 60, 48
```

### `takewhile`

This function creates an iterator that iterates over a given iterable,
but skips all the initial elements
for which a predicate function evaluates to `True`.
It is the opposite of `dropwhile`.
For example:

```python
from itertools import takewhile
from operator import itemgetter

teams = [
    {'name': '49ers', 'wins': 3, 'losses': 3},
    {'name': 'Bears', 'wins': 5, 'losses': 1},
    {'name': 'Cardinals', 'wins': 4, 'losses': 2},
    {'name': 'Lions', 'wins': 2, 'losses': 3},
    {'name': 'Packers', 'wins': 4, 'losses': 1},
    {'name': 'Rams', 'wins': 4, 'losses': 2},
    {'name': 'Seahawks', 'wins': 5, 'losses': 0},
    {'name': 'Vikings', 'wins': 1, 'losses': 5}
]

# Sort teams on their records with best teams first.
teams.sort(key=itemgetter('wins', 'losses'), reverse=True)

def winning_record(team):
    return team['wins'] > team['losses']

# Iterate over teams that have a winning record.
for team in takewhile(winning_record, teams):
    print(team['name']) # Bears, Seahawks, Cardinals, Rams, Packers
```

### `zip_longest`

This function creates an iterator that
combines corresponding elements from multiple iterables.
For example:

```python
from itertools import zip_longest

months = ['January', 'February', 'March', 'April']
numbers = range(1, len(months) + 1)
people = ['Laura', 'Pat', 'unknown', 'Mark']
for list in zip_longest(numbers, months, people):
  print(list)

# Output is:
# (1, 'January', 'Laura')
# (2, 'February', 'Pat')
# (3, 'March', 'unknown')
# (4, 'April', 'Mark')
```

## Combinatoric iterators

### `combinations`

This function creates an iterator over all the combinations
of n items from an iterable taken k at a time (referred to as n choose k)
where order does not matter.
From the docs, "Elements are treated as unique
based on their position, not on their value."
For example:

```python
from itertools import combinations

colors = ['red', 'green', 'blue']
for color in combinations(colors, 2):
  print(color)

# Output:
# ('red', 'green')
# ('red', 'blue')
# ('green', 'blue')
```

### `combinations_with_replacement`

This function is similar to `combinations`,
but allows items to be repeated.

```python
from itertools import combinations_with_replacement

colors = ['red', 'green', 'blue']
for color in combinations_with_replacement(colors, 2):
  print(color)

# Output:
# ('red', 'red')
# ('red', 'green')
# ('red', 'blue')
# ('green', 'green')
# ('green', 'blue')
# ('blue', 'blue')
```

### `permutations`

This function creates an iterator over all the combinations
of n items from an iterable taken k at a time (referred to as n choose k)
where order matters.
From the docs, "Elements are treated as unique
based on their position, not on their value."
If the second argument (k) is not supplied,
it defaults to the length of the first argument iterable.
Note that the second argument is required in the `combinations` function.
For example:

```python
from itertools import permutations

colors = ['red', 'green', 'blue']
for color in permutations(colors, 2):
  print(color)

# Output:
# ('red', 'green')
# ('red', 'blue')
# ('green', 'red')
# ('green', 'blue')
# ('blue', 'red')
# ('blue', 'green')
```

### `product`

This function creates an iterator over the cartesian product
of a set of iterables.
This is every combination of one item taken from each of the iterables.
For example:

```python
from itertools import product

rank = ['Ace', 'King', 'Queen', 'Jack']
suit = ['diamonds', 'clubs', 'hearts', 'spades']

for card in product(rank, suit):
  print(card)

# Output:
# ('Ace', 'diamonds')
# ('Ace', 'clubs')
# ('Ace', 'hearts')
# ('Ace', 'spades')
# ('King', 'diamonds')
# ('King', 'clubs')
# ('King', 'hearts')
# ('King', 'spades')
# ('Queen', 'diamonds')
# ('Queen', 'clubs')
# ('Queen', 'hearts')
# ('Queen', 'spades')
# ('Jack', 'diamonds')
# ('Jack', 'clubs')
# ('Jack', 'hearts')
# ('Jack', 'spades')
```
