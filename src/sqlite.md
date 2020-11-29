---
eleventyNavigation:
  key: SQLite
layout: topic-layout.njk
---

## Overview

SQLite claims to be "the most used database engine in the world".
It runs on all common operating systems and mobile devices.

Features of SQLite include:

- atomic transactions
- zero configuration required
- full SQL support
- databases stored in a single, binary file which makes them easy to back up
- small code footprint
- simple API
- fast performance
- implemented in C
- no external dependencies
- wide operating system support
- public domain source code
- includes a command-line interface (CLI)

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

The most commonly used dot commands are described in the following table:

| Action                                            | Command                                                                                                      |
| ------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| change working directory                          | `.cd {directory}`                                                                                            |
| close current database and open new one           | `.open {file-path}.db`                                                                                       |
| attach an additional database                     | `attach database "{file-path}" as {name};`                                                                   |
| list names and paths of attached databases        | `.databases`                                                                                                 |
| list tables in current database                   | `.tables`                                                                                                    |
| show database schema                              | `.schema`                                                                                                    |
| show table schema                                 | `.schema {table-name}`                                                                                       |
| configure showing/hiding # of rows changed by SQL | `.changes {on|off}`                                                                                          |
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
Maisey      Treeing Walker Coonhound  11
Ramsay      Native American Indian D  8
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

## Python Support

Python has built-in support for SQLite.
For details, see [here](/blog/python/sqlite-in-python/).
