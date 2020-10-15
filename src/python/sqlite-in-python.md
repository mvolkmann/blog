---
eleventyNavigation:
  key: SQLite in Python
  order: 9
  parent: Python
layout: topic-layout.njk
---

For details on SQLite, see [here](/blog/sqlite).

Python has built-in support for
{% aTargetBlank "https://www.sqlite.org/", "SQLite" %} databases
provided by the package `sqlite3`.
Here is an example program that performs many common operations.

```python
import sqlite3  # included with Python

# Connect to a database and create it if it doesn't exist.
# The database is in a binary file and is not human-readable.
conn = sqlite3.connect('pets.db')

# All database operations occur on a cursor.
c = conn.cursor()

# Uncomment this to start with an empty table.
c.execute('drop table if exists dogs')

# Create a table.
# An object id column named "oid" is automatically added.
c.execute('''
    create table if not exists dogs (
      name text,
      breed text,
      age integer
    )
''')

def add_dog(dog):
    # Names preceded by a colon are keys in
    # the dict passed as the second argument.
    # The value of the "oid" column is auto-incremented.
    c.execute('insert into dogs values (:name, :breed, :age)', dog)

def get_dogs():
    # Selecting * does not get the "oid" column.
    # It must be explicitly requested.
    c.execute('select *, oid from dogs')
    return c.fetchall()  # returns list of tuples, one per record

def print_dogs(label, dogs):
    print(label)
    if len(dogs):
        print(f"  {'Name':12}  {'Breed':30}  {'Age':3}  {'OID':3}")
        print(f"  {'-'*12:12}  {'-'*30:30}  {'-'*3:3}  {'-'*3:3}")
        for dog in dogs:
            name, breed, age, oid = dog
            print(f'  {name:12}  {breed:30}  {age:3}  {oid:3}')
    else:
        print('  empty')
    print()

dogs = get_dogs()
print_dogs('initial dogs', dogs)

# If the table is empty, add some rows.
if len(dogs) == 0:
    add_dog({'name': 'Maisey', 'breed': 'Treeing Walker Coonhound', 'age': 11})
    add_dog({'name': 'Ramsay', 'breed': 'Native American Indian Dog', 'age': 8})
    add_dog({'name': 'Oscar', 'breed': 'German Shorthaired Pointer', 'age': 3})
    add_dog({'name': 'Comet', 'breed': 'Whippet', 'age': 0})
    print_dogs('dogs after inserts', get_dogs())

# Get the oid for Oscar.
c.execute('select oid from dogs where name="Oscar"')
tuple = c.fetchone()  # returns tuple where first element is oid
if tuple != None:  # If None is returned, Oscar was not found.
    oid = tuple[0]
    # Update Oscar's name and age.
    c.execute('update dogs set name="Oscar Wilde", age=4 where oid=' + str(oid))
    print_dogs('dogs after update', get_dogs())

    # Delete Oscar.
    c.execute('delete from dogs where oid=' + str(oid))
    print_dogs('dogs after delete', get_dogs())

conn.commit()  # saves changes
conn.close()
```
