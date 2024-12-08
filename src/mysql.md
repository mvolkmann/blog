---
eleventyNavigation:
  key: MySQL
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.mysql.com", "MySQL" %}
is an open-source relational database.
It is implemented in C++.
MySL runs on all common operating systems.
It was originally created by the Swedish company "MySQL AB",
which was later bought by Sun, which was later bought by Oracle.

Pros of MySQL include:

- open source and free
- TODO: add more

Cons of MySQL include:

- TODO: add some

## Installing

To install MySQL in macOS, use Homebrew with the command
`brew install mysql`.

By default there is no root password.

## Getting Started

To start the database server, enter `brew services start mysql`.

To stop the database server, enter `brew services stop mysql`.

# Interactive Mode

To enter interactive mode, enter `/opt/homebrew/bin/mysql -u root`.

The following list includes commonly used interactive commands:

- `show databases;`
- `create database {database-name};`
- `drop database {database-name};`
- `use {database-name};`
- `show tables;`
- `drop table if exists {table-name};`
- `create table {table-name} ({col-name} {col-type}, ...);`
  - for example,
    `create table todos (id int auto_increment primary key, description text, completed boolean, unique (description));`
- `describe {table-name};`
- `insert into {table-name} ({col1}, {col2}, ...) values ({v1}, {v2}, ...);`
- `select \* from {table-name};`
- `source {file-name}.sql;`
- `exit`

## Supported Column Types

- bit, bool, boolean
- tinyint, smallint, int, integer, mediumint, bigint
- decimal, numeric
- float, double

- char, varchar, text
- enum, set
- binary, varbinary
- json
- blob

- date, datetime, time, timestamp, year

## ODBC

To access a MySQL database using ODBC,
download an ODBC driver for your operating system.
The ODBC driver for MariaDB is compatible with MySQL.
It can be installed with `brew install mariadb-connector-odbc.
This is FAR easier that attempting to install and configure an ODBC driver
that is specifically for MySQL.
from <https://dev.mysql.com/downloads/connector/odbc/>.
To try that, see <https://dev.mysql.com/doc/connector-odbc/en/connector-odbc-installation-binary-macos.html#connector-odbc-macos-iodbc>.
I never could get this option to work.

Modify `/opt/homebrew/etc/odbcinst.ini`
to refer to `/usr/local/lib/libmyodbc9w.so`
by adding the following lines:

```text
[MySQL]
Description = MySQL ODBC Driver
Driver = /opt/homebrew/lib/mariadb/libmaodbc.dylib
```

Modify `~/.odbc.ini` to add a data source name like the following:

```text
[TodosDSN]
Description = MySQL database for a Todo app
Driver = MySQL
Database = todos
User = root
```
