---
eleventyNavigation:
  key: Turso
layout: topic-layout.njk
---

<figure style="width: 30%">
  <img alt="Turso logo" style="border: 0"
    src="/blog/assets/turso-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://turso.tech", "Turso" %} is a cloud database
that uses SQLite.

## Steps to Use

- Create a free account.
- Create your first database.

  The website provides a button that can be clicked to do this.

- Create a replica for your database in a specific city.

  The website provides a button that can be clicked to do this.
  I chose Atlanta, Georgia USA.

- Install the Turso CLI.

  In macOS, enter `brew install tursodatabase/tap/turso`

- Sign up by entering `turso auth signup`

- Create a database by entering `turso db create {db-name}`

- View information about the database by entering `turso db show {db-name}`

  This will output the URL, primary location (ex. ord),
  replica location (ex. atl), and size (ex. 4.1 kB).

- Connect to the database shell by entering `turso db shell {db-name}`

- Create a table by entering `create table todos(id integer primary key autoincrement, text string, done integer);`

- List the existing tables by entering `.tables`

- Output the schema for all tables or a specific table by entering `.schema [table-name]`

- Insert a row by entering `insert into todos (text, done) values('cut grass', 0);`

- Update the row by entering `update todos set done=1 where id=1;`

- Query the table by entering `select * from todos;`

- Quit the shell by entering `.quit`
