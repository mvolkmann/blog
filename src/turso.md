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

Databases can be created and modified in the web UI or using their CLI.

Turso uses Drizzle ORM.

## Setup

- Create a free account.

- Create your first database.

  The website provides a button that can be clicked to do this.

- Create a replica for your database in a specific city.

  The website provides a button that can be clicked to do this.
  I chose Atlanta, Georgia USA.

- Install the Turso CLI.

  In macOS, enter `brew install tursodatabase/tap/turso`

- Sign up by entering `turso auth signup`

## CLI

- Create a database by entering `turso db create {db-name}`

- View information about the database by entering `turso db show {db-name}`

  This will output the URL, primary location (ex. ord for Chicago),
  replica location (ex. atl for Atlanta), and size (ex. 4.1 kB).

- Connect to the database shell by entering `turso db shell {db-name}`

- Create a table by entering `create table todos(id integer primary key autoincrement, text string, done integer);`

- List the existing tables by entering `.tables`

- Output the schema for all tables or a specific table by entering `.schema [table-name]`

- Insert a row by entering `insert into todos (text, done) values('cut grass', 0);`

- Update the row by entering `update todos set done=1 where id=1;`

- Query the table by entering `select * from todos;`

- Quit the shell by entering `.quit`

  Unlike in SQLite, `.exit` does not work.

## App Access

- Create a directory for the app and `cd` to it.

- Create the initial files by entering `bun init`.

- Install the client library by entering `bun add @libsql/client`

- Create an auth token by entering `turso db tokens create {db-name}`

- Edit `.env` and add `TURSO_AUTH={auth-token}`.

- Get the database URL by entering `turso db show {db-name} --url`

- Edit `index.ts` and replace the contents with the following:

  ```ts
  import {createClient} from '@libsql/client';

  const client = createClient({
    url: '{db-url}',
    authToken: Bun.env.TURSO_TOKEN
  });

  const resultSet = await client.execute('select * from todos');
  const {columns, rows} = resultSet;
  for (const row of rows) {
    console.log('---');
    for (const column of columns) {
      console.log(column, '=', row[column]);
    }
  }
  ```

- Edit `package.json` and add the following:

  ```json
  "scripts": {
    "dev": "bun run index.ts"
  },
  ```

- Run the app by entering `bun dev`
