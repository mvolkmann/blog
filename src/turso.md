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

The logo depicts Iku-Turso which is a mythological Finnish sea creature.

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
that adds a server, native replication, and an HTTP mode.
This allows it to replicate and scale with low resource usage.
libSQL can support having one logical database per tenant
or even one logical database per user.

Primary databases handle both read and write operations.
Replica databases handle read operations,
but delegate write operations to their primary database.
Turso handles making all the replicas eventually consistent.

Databases can be created and modified in the web UI or using their CLI.

Turso uses Drizzle ORM.

Astro uses Turso for "Astro DB".

## Pricing

As of March 2024, Turso offers three plans described in the following table.

|                  | STARTER    | SCALER      | ENTERPRISE PARTNER |
| ---------------- | ---------- | ----------- | ------------------ |
| Price            | free       | $29/month   | $2,999/month       |
| Storage          | 9 GB       | 24 GB       | unlimited          |
| Databases        | 500        | 10,000      | unlimited          |
| Locations        | 3          | 6           | unlimited          |
| Row reads/month  | 1 billion  | 100 billion | unlimited          |
| Row writes/month | 25 million | 100 million | unlimited          |

The primary limitation of the STARTER and SCALER plans
is having a maximum of 3 or 6 locations (primary and replicas).
This prevents having fast access all over the world.
Depending on the location of users,
this may be acceptable for many web applications.

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

## Write Details

When a record is written from a replica,
the `last_replication_index` is immediately returned from the primary.
At this point the current replica has not yet been updated.

When a record is read from a replica,
the `last_replication_index` value is added to the read request.
If the replica has a lower `last_replication_index`
then the request is forwarded to the primary.
This can result in increased latency for reads until the replica catches up.
But it guarantees that reads from a replica will always
get the data that was most recently written from the same replica.

Replication tends to be very fast, so this is not typically an issue.

## Setup

- Create a free account.

- Create your first database.

  The website provides a button that can be clicked to do this.
  It will create the primary database in the supported location
  that is nearest to your current location.

- Optionally create replicas of your database
  in a specific cities that are near your users.

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

## Importing Exising Databases

Existing SQLite databases can be imported into Turso using the
`turso db create {db-name} --from-file {file-path}` command.

See other options at <a href="https://docs.turso.tech/cli/db/create"
target="_blank">db create</a>.

## Web-based Dashboard

Browse <a href="https://turso.tech/" target="_blank">Turso web site</a>
and click the "Login" button to access your dashboard.
This provides a web-based view that show:

- the number of databases you have created
- the amount of storage used
- the number of locations used for primary and replica databases
- the number or rows read and written

To access a specific database, click "Databases" in the left nav.
A table at the bottom lists the database names.
Click one to see its URL, ID, and basic statistics.
Click the "Edit Tables" button to
view the existing tables and create new tables.
From here you can view/modify existing records and add new records.

## App Access

Turso currently provides API for interacting with databases using
TypeScript, Go, and Rust.

- Create a directory for the app and `cd` to it.

- Create the initial files by entering `bun init`.

- Install the client library by entering `bun add @libsql/client`

- Create an auth token by entering `turso db tokens create {db-name}`

- Edit `.env` and add the following lines:

  ```text
  TURSO_AUTH={auth-token}
  TURSO_URL={db-url}
  ```

- Get the database URL by entering `turso db show {db-name} --url`

- Edit `index.ts` and replace the contents with the following:

  ```ts
  import {createClient} from '@libsql/client';

  const client = createClient({
    url: Bun.env.TURSO_URL as string,
    authToken: Bun.env.TURSO_TOKEN
  });

  const resultSet = await client.execute('select * from todos');
  // The client.execute method can also run
  // commands like insert, update, and delete.
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

## ORMs

Using an ORM can simplify the code for interacting with Turso databases.
The <a href="/blog/topics/#/blog/drizzle/" target="_blank">Drizzle</a> ORM
has great support for Turso.

```javascript
import {eq} from 'drizzle-orm';
import {drizzle} from 'drizzle-orm/libsql';
import {sqliteTable, integer, text} from 'drizzle-orm/sqlite-core';
import {createClient} from 'libsql/client';

const client = createClient({url, authToken});
const db = drizzle(client);

// Map ORM object properties to table columns.
export const todos = sqliteTable('todos', {
  id: integer('id').primaryKey(),
  text: text('text', {length: 256}),
  done: integer('done')
});

function createTodo(text: string, done: int) {
  return db.insert(todos).values({text, done}).run();
}

function getTodos() {
  return db.select().from(todos);
}
```

## TODOs

Create a demo app that uses Bun, Hono, Turso, and Cloudflare.
