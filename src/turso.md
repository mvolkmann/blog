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

Compute services can be moved to "the edge"
so they are closer to users and perform better.
But this provides limited benefits if the services
need to access databases that are hosted in
one location rather than also being on the edge.

Turso addresses this problem by moving databases to the edge.
Each Turso logical database has one primary database
and zero or more replica databases.

Turso uses <a href="https://turso.tech/libsql" target="_blank">libSQL</a>
databases.
libSQL is an open-source fork of SQLite
that adds native replication and an HTTP mode.
This allows it to replicate and scale with low resource usage.
libSQL can support having one logical database per tenant
or even one logical database per user.

Primary databases handle both read and write operations.
Replica databases handle read operations,
but delegate write operations to their primary database.
Turso handles making all the replicas eventually consistent.

Databases can be created and modified in the web UI or using their CLI.

Turso uses Drizzle ORM.

## Setup

- Create a free account.

- Create your first database.

  The website provides a button that can be clicked to do this.

- Optionally create replicas of your database
  in a specific cities that are near your users.

  The website provides a button that can be clicked to do this.
  I chose Atlanta, Georgia USA.

- Install the Turso CLI.

  In macOS, enter `brew install tursodatabase/tap/turso`

- Sign up by entering `turso auth signup`

## Available Locations

To see a list of all the available locations for primary and replica databases,
enter `turso db locations`. The current list includes the following:

```text
ID   LOCATION
---  --------
ams  Amsterdam, Netherlands
arn  Stockholm, Sweden
atl  Atlanta, Georgia (US)
bog  Bogotá, Colombia
bom  Mumbai, India
bos  Boston, Massachusetts (US)
cdg  Paris, France
den  Denver, Colorado (US)
dfw  Dallas, Texas (US)
ewr  Secaucus, NJ (US)
eze  Ezeiza, Argentina
fra  Frankfurt, Germany
gdl  Guadalajara, Mexico
gig  Rio de Janeiro, Brazil
gru  São Paulo, Brazil
hkg  Hong Kong, Hong Kong
iad  Ashburn, Virginia (US)
jnb  Johannesburg, South Africa
lax  Los Angeles, California (US)
lhr  London, United Kingdom
mad  Madrid, Spain
mia  Miami, Florida (US)
nrt  Tokyo, Japan
ord  Chicago, Illinois (US)  [default]
otp  Bucharest, Romania
phx  Phoenix, Arizona (US)
qro  Querétaro, Mexico
scl  Santiago, Chile
sea  Seattle, Washington (US)
sin  Singapore, Singapore
sjc  San Jose, California (US)
syd  Sydney, Australia
waw  Warsaw, Poland
yul  Montreal, Canada
yyz  Toronto, Canada
```

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
