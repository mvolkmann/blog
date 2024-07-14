---
eleventyNavigation:
  key: SQLite
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.sqlite.org/", "SQLite" %}
claims to be "the most used database engine in the world".
It is a relational database implemented in C.
It runs on all common operating systems and mobile devices.
Its creator, D. Richard Hipp, pronounces it "es queue el ite".

Pros of SQLite include:

- open source and free
- no external dependencies
- small code footprint
- zero configuration required
- full SQL support
- databases stored in a single, binary file which makes them easy to back up
- includes a command-line interface (CLI)
- simple API
- atomic transactions
- fast performance
- wide operating system support
- wide programming language support
- targeted at small databases (great for mobile applications)
- supports multiple, concurrent readers

Cons of SQLite include:

- doesn't support databases larger than 281 terabytes
  (There are other limits on things like string length,
  number of columns in a table, and number of tables,
  but these are all quite high.)
- only supports a single writer (locks entire database during writes)
  so not good for multi-user applications
- no support for stored procedures
- no built-in data encryption

See {% aTargetBlank "https://www.sqlite.org/whentouse.html",
"Appropriate Uses For SQLite" %}.
This contains the following advice and much more:

- "Avoid using SQLite in situations where the same database
  will be accessed directly (without an intervening application server)
  and simultaneously from many computers over a network."
- "SQLite supports an unlimited number of simultaneous readers,
  but it will only allow one writer at any instant in time.
  For many situations, this is not a problem. Writers queue up.
  Each application does its database work quickly and moves on,
  and no lock lasts for more than a few dozen milliseconds.
  But there are some applications that require more concurrency,
  and those applications may need to seek a different solution."

## Installing

SQLite can be downloaded from
{% aTargetBlank "https://www.sqlite.org/download.html", "here" %}.
In macOS it can be installed using Homebrew by entering `brew install sqlite`.

For help, enter `sqlite3 --help`.

## CLI Tool

To start the SQLite CLI, enter `sqlite3`
optionally followed by a file path to a database file.
This displays a `sqlite>` prompt where you can enter
SQL commands (terminated by a semicolon) and
"dot commands" that begin with a period.

For example, here is a sample session that creates a `pets` database
that contains tables for `dogs` and `cats`.

```text
$ sqlite3 pets.db
sqlite> create table dogs(id integer primary key autoincrement, name string, breed string, age integer);
sqlite> create table cats(id integer primary key autoincrement, name string, breed string, remaining_lives integer);
sqlite> .schema (displays the schema for each table)
sqlite> insert into dogs values('Comet', 'Whippet', 1);
sqlite> select * from dogs;
sqlite> .exit
$ ls *.db
```

The file `pets.db` exist now as long as at least one table was defined.

## File Input

SQL commands can be read from a file.
For example, if the file `schema.txt` commands SQL commands
then the command `sqlite3 demo.db < schema.txt`
will execute the commands using the database `demo.db`.

## Dot Commands

The most commonly used dot commands are described in the following table:

| Action                                            | Command                                                                                                      |
| ------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| change working directory                          | `.cd {directory}`                                                                                            |
| close current database and open new one           | `.open {file-path}.db`                                                                                       |
| attach an additional database                     | `attach database "{file-path}" as {name};`                                                                   |
| list names and paths of attached databases        | `.databases`                                                                                                 |
| list tables in current database                   | `.tables`                                                                                                    |
| drop a table                                      | `drop table if exists {table-name}`                                                                          |
| upsert                                            | `insert into table-name (col1, col2) values (val1, val2) on conflict(name) do update set col2 = val2;`       |
| show database schema                              | `.schema`                                                                                                    |
| show table schema                                 | `.schema {table-name}`                                                                                       |
| configure showing/hiding # of rows changed by SQL | `.changes {on \| off}`                                                                                       |
| export table to CSV file                          | `.headers on`<br>`.mode csv`<br>`.once {file-path}`<br>`select * from {table-name}`<br>`.system {file-path}` |
| import CSV file into a table                      | `.import {file-path} {table-name}`                                                                           |
| execute shell command                             | `.shell {command}`                                                                                           |
| show current configuration settings               | `.show`                                                                                                      |
| exit                                              | `.exit` or `.quit`                                                                                           |

When multiple databases are attached,
the tables in all of them are accessible and
reporting commands such as `.tables` report on all the databases.

By default, output from `select` statements uses `list` mode
where each record is output on a single line
with `|` characters separating column values.

Here is an example of `list` mode output:

```text
Maisey|Treeing Walker Coonhound|11
Ramsay|Native American Indian Dog|8
```

The `.mode {mode}` command can be used to change this output.
Supported modes include `list`, `csv`, `line`, and `column`.

Here is an example of `csv` mode output:

```text
Maisey,"Treeing Walker Coonhound",11
Ramsay,"Native American Indian Dog",8
```

Here is an example of `line` mode output:

```text
 name = Maisey
breed = Treeing Walker Coonhound
  age = 11

 name = Ramsay
breed = Native American Indian Dog
  age = 8
```

Here is an example of `column` mode output:

```text
id name   breed                    age
-- ------ ------------------------ ---
1  Maisey Treeing Walker Coonhound 11
2  Ramsay Native American Indian D 8
```

## Column Types

SQLite supports a small set of column types that include:

- integer
- real
- numeric (for boolean, decimal, date, and datetime values)
- text
- blob

Use the numeric type for boolean values where 0 is false and 1 is true.

Use the text type for ISO date strings.

Use the integer type for storing seconds or milliseconds
since the epoch (1970-01-01 00:00:00 UTC).

## Write-Ahead Logging (WAL)

By default SQLite performs atomic commits
and rollbacks using a "rollback journal".
At any point in time there is either one writer or none.
In WAL mode, readers block the single writer and the writer block readers.

WAL is a new option that is significantly faster.
Readers and the writer no longer block each other.

Some of the disadvantages of WAL include:

- The virtual file system must support shared-memory primitives,
  so WAL is not an option on every computer.
- All processes that access the database must
  run on the same computer as the database.

To enable WAL mode, start the server as follows:

```bash
sqlite3 {name}.db 'PRAGMA journal_mode=WAL;'
```

For more detail, see {% aTargetBlank "https://www.sqlite.org/wal.html",
"Write-Ahead Logging" %}.

## JavaScript Support

There are many client libraries in npm for working with SQLite databases.
A recommended library is {% aTargetBlank
"https://github.com/JoshuaWise/better-sqlite3", "better-sqlite3" %}.
To install this, enter `npm install better-sqlite3`.
To use TypeScript types, enter `npm install @types/better-sqlite3`.

The example code below demonstrates using this with TypeScript.

```js
import sqlite from 'better-sqlite3';
import type {RunResult, Statement} from 'better-sqlite3';

// Open existing database file or creating if missing.
const db = sqlite('gift-track.db');

// This pragma must be enabled in order to
// check foreign key constraints and perform cascading deletes.
db.pragma('foreign_key = true');

// Delete the tables if they already exist.
db.exec('drop table if exists gifts');
db.exec('drop table if exists people');
db.exec('drop table if exists occasions');

// Create tables.

db.exec(
  'create table people (' +
    'id integer primary key autoincrement, ' +
    'name string, ' +
    'month integer, ' +
    'day integer, ' +
    'year integer, ' +
    'unique (name collate nocase))' // case insensitive
);

db.exec(
  'create table occasions (' +
    'id integer primary key autoincrement, ' +
    'name string, ' +
    'month integer, ' +
    'day integer, ' +
    'year integer, ' +
    'unique (name collate nocase))' // case insensitive
);

db.exec(
  'create table gifts (' +
    'id integer primary key autoincrement, ' +
    'description string, ' +
    'location string, ' +
    'name string, ' +
    'occasionId integer, ' +
    'personId integer, ' +
    'price numeric, ' +
    'url string, ' +
    'unique (name collate nocase), ' + // case insensitive
    // If an occasion is deleted, also delete gifts associated with it.
    'constraint fk_occasion foreign key ' +
    '  (occasionId) references occasions(id) on delete cascade ' +
    // If a person is deleted, also delete gifts their gifts.
    'constraint fk_person foreign key ' +
    '  (personId) references people(id) on delete cascade ' +
    ')'
);

// Create prepared statements.

insertPersonPS = db.prepare(
  'insert into people (name, month, day, year) values (?, ?, ?, ?)'
);
getAllPeoplePS = db.prepare('select * from people');
getPersonPS = db.prepare('select * from people where id = ?');
updatePersonPS = db.prepare(
  'update people set name=?, month=?, day=?, year=? where id = ?'
);
deletePersonPS = db.prepare('delete from people where id = ?');

// Execute prepared statements.

type Person = {
  id?: number;
  name: string;
  month?: number;
  day?: number;
  year?: number;
};

// Insert a person.
const person: Person = {
  name: 'Mark',
  month: 4,
  day: 16,
  year: 1961
};

try {
  const result: RunResult = insertPersonPS.run( // synchronous
    person.name,
    person.month,
    person.day,
    person.year
  );
  person.id = result.lastInsertRowid as number;
} catch (e) {
  const isDuplicate = e.toString().includes('UNIQUE constraint failed');
  throw isDuplicate
    ? new Error('duplicate person name ' + person.name)
    : e;
}

// Get all the people.
const people = (getAllPeoplePS.all() as unknown) as Person[];

// Update a person.
updatePersonPS.run(
  person.name,
  person.month,
  person.day,
  person.year,
  person.id
);

// Delete a person.
deletePersonPS.run(person.id);
```

## Python Support

Python has built-in support for SQLite.
For details, see [here](/blog/python/sqlite-in-python/).

## VS Code Support

See the VS Code extension {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=alexcvzz.vscode-sqlite", "SQLite" %}.
To use this:

- Open the Command Palette and enter "SQLite: Open Database".
- Select a database from the drop-down list.
- In the Explorer view, expand the "SQLite Explorer" section.
- Select a database to see a list of tables inside it.
- Select a table to see its column names and types.
- Click the triangle to the right a table name to see the rows inside it.
- Click one of the buttons in the upper-right to export the table data as
  CSV, HTML, or JSON.
- Click the new document icon in the "SQLite Explorer" section
  to enter a query.
- Open the Command Palette and enter "SQLite: Run Query" to run the query.
