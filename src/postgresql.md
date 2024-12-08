---
eleventyNavigation:
  key: PostgreSQL
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.postgresql.org", "PostgreSQL" %}
claims to be "the world's most advanced open source relational database".
It is implemented in C.
PostgreSQL runs on all common operating systems.
It was originally created by Michael Stonebraker and his team
at the University of California, Berkeley, in the 1980s.

Pros of PostgreSQL include:

- open source and free
- advanced SQL compliance
- very extensible through custom functions, operators,
  data types, and procedural languages
- supports arrays, JSON, XML, and hstore (key/value pairs)
- strong ACID (Atomicity, Consistency, Isolation, Durability) compliance
- excellent performance for read-heavy and write-heavy workloads
- extensive documentation

Cons of PostgreSQL include:

- can be resource intensive
- configuration and management can be somewhat complex
- doesn’t natively support automatic sharding
  or scaling across distributed nodes

## Installing

To install PostgreSQL in macOS, use Homebrew with the commands
`brew uninstall libpq` and `brew install postgresql@14`.
Verify the install by entering `psql --version`.

If running in macOS with an Apple silicon processor and
PostgreSQL has been previously install while using a non-silicon processor,
enter the following commands to delete the previously installed commands.
This will allow the newly installed versions
in the `/opt/homebrew/bin` directory to be used instead.

````bash
rm /usr/local/bin/clusterdb
rm /usr/local/bin/createdb
rm /usr/local/bin/createuser
rm /usr/local/bin/dropdb
rm /usr/local/bin/dropuser
rm /usr/local/bin/initdb
rm /usr/local/bin/pg*
rm /usr/local/bin/postgres
rm /usr/local/bin/psql
rm /usr/local/bin/reindexdb
rm /usr/local/bin/vacuumdb
rm -rf /usr/local/var/postgres

Modify your shell startup script to set the `PGDATA` environment variable.
In zsh, modify `~/.zshenv` to include the following:

```bash
export PGDATA="/opt/homebrew/var/postgres"
````

## Getting Started

To create the initial files required by the database server, enter

```bash
initdb
```

This creates the directory `/opt/homebrew/var/postgres`.

To start the database server, enter

```bash
brew services start postgresql@14
```

OR

```bash
pg_ctl -D /opt/homebrew/var/postgres start
```

To stop the database server, enter

```bash
brew services stop postgresql@14
```

OR

```bash
pg_ctl -D /opt/homebrew/var/postgres stop
```

To create a database, enter `createdb {db-name}`.

To delete a database, enter `dropdb {db-name}`.

To list the existing databases, enter `psql -l`.

To enter interactive mode for a given database,
enter art for a given database `psql -d {db-name}`.

In interactive mode:

- To list all the current roles (users),
  enter `select * from pg_roles;`

- To list all the tables in the current database, enter `\d`.

- To create a table within a database, enter

  ```sql
  create table {table-name} ({col-name} {col-type}, ...);
  ```

  For example:

  ```sql
  create table dog (id bigserial primary key, name text, breed text);

  create table todos (id serial primary key, description text unique, completed boolean);
  ```

- To add a row to a table, enter an `insert` command like
  `insert into dogs (name, breed) values ('Comet', 'whippet');`

- To execute a query, enter a command like:

  ```sql
  select \* from pets where breed = 'whippet';
  ```

  Don't forget the semicolon at the end.

- To quit interactive mode, enter `\q`.

To execute a single SQL statement, enter `psql -d {db-name} -c '{stmt}'`
For example, `psql -d pets -c 'select \* from dogs'`

To execute SQL statements in a file,
enter `psql -d {db-name} -f {file-path}`

## pgAdmin

pgAdmin is a popular GUI tool for working with PostgreSQL databases.
It can be found at <https://www.pgadmin.org/>.
For macOS on Apple Silico (AMD), I downloaded the installer
`pgadmin4-8.12-arm64.dmg` and double-clicked it to install.

## Supported Column Types

- boolean or bool
- char(n)
- varchar(n)
- text
- smallint
- int
- serial (integer with autoincrement)
- bigserial (longer integer with autoincrement)
- float(n)
- real or float8
- numeric or numeric(p, s) where
  p is the total number of digits and
  s is the number after the decimal point
- date
- time
- timestamp
- timestamptz - a timezone-aware timestamp
- interval - a period of time
- array (ex. an array of int values is int[])
- json (text)
- jsonb (binary)
- uuid
- for geometric data: box, line, point, lseg, polygon
- inet
- macaddr

## Unorganized Content

To create a user/role

- createuser
  - will prompt for name of role to add and
    whether the role should be a superuser

To change the password of a user

- psql -Upostgres -dtemplate1
- alter user {username} with password '{new-password}';
- \q

To make the "postgres" user able to create tables

- psql -d some-database
- \password postgres
- Enter new password like "postgres".
- alter role postgres with superuser;
- alter role postgres with createdb;
- ctrl-d

To start interactive mode

- psql -U{username} {database-name}
  - username can be "postgres"

To delete a database

- dropdb -U{username} {db-name}

To use a database in interactive mode

- psql -U{username} {db-name}
  - for example, XAdemo

To list all the public (non-system) tables

- select \* from pg_tables where schemaname='public';

More on interactive mode

- prompt
  - begins with the database name
  - ends in =# if you are the database superuser, otherwise ends in =>
- see schemas that will be searched - show search_path;
- change schemas that will be searched -
  set search_path to {comma-separated-schema-list}
  - for example, set search_path=sal;
- help - \? or \h{command}
- include - \i{file}
  - reads SQL from the given file
- list all databases - \l
- list public tables - \d or \z
- list system tables - \dS
  - for example, the system table pg_tables
- list columns in a table - \d {table-name}
- version
  - to see version of PostgreSQL, enter "select version();"
- exit - \q
- see http://www.postgresql.org/docs/6.4/static/app-psql.htm for more

Auto-incrementing keys

- approach #1
  CREATE TABLE foo (
  id SERIAL PRIMARY KEY,
  ... other columns ...
  )
- approach #2
  CREATE SEQUENCE fooSeq;
  CREATE TABLE foo (
  id INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('fooSeq'),
  ... other columns ...
  )

To dump the database to a file:

- from a terminal window
  - pg_dump -Upostgres {dbname} > {outfile}
    - postgres password is postgres
  - outfile will contain all SQL commands necessary to
    recreate the database in its current state
  - to recreate the database elsewhere
    - use dropdb to delete the previous database
      (see "To delete a database")
    - use createdb to recreate an empty database
      (see "To create a database")
    - use psql to run the commands in outfile
      (see "Run commands in a file")
- from pgAdmin III
  - right-click a table name
  - select "Backup..."
  - press "..." to specify the file where it will be saved
  - select "PLAIN"
  - select "Only data"
  - press "OK"
  - press "Done"
