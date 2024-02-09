---
eleventyNavigation:
  key: Drizzle
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

<img alt="Drizzle logo" style="border: 0; width: 20%"
  src="/blog/assets/drizzle-logo.png?v={{pkg.version}}">
<img alt="Drizzle mascot" style="border: 0; width: 20%"
  src="/blog/assets/drizzle-mascot.png?v={{pkg.version}}">

{% aTargetBlank "https://orm.drizzle.team", "Drizzle" %} is a TypeScript-based
Object Relational Mapper (ORM) library that is free and open-source.
It competes with other popular ORMs such as
{% aTargetBlank "https://www.prisma.io", "Prisma" %}.

Drizzle is designed "to be a thin layer on top of SQL
and introduce minimal runtime overhead".

Database table schemas are defined entirely in TypeScript.
These are used to create/migrate tables AND provide type checking in code.

## Pros

The Drizzle methods for interacting with databases are SQL-like,
so it doesn't feel like learning a new syntax.

Queries created with the Drizzle Query API always result in one SQL query.
This helps with performance and minimizes round trips to the server.

The Drizzle library is lightweight (32K minified).

Drizzle is easy to use.

Drizzle does not require any code generation.

Drizzle has no dependencies.

Drizzle supports many databases. These include
LiteFS, MySQL, Neon, PlanetScale, PostgreSQL,
SQLite, Supabase, Turso, Vercel Postgres, Web SQLite, and Xata.
Notable exceptions include Microsoft SQL Server and MongoDB.

Drizzle can generate TypeScript schema definitions
from existing database tables.

Drizzle supports schema migrations.

Drizzle supports many edge platforms. These include:

- {% aTargetBlank "https://bun.sh", "Bun" %}
- {% aTargetBlank "https://developers.cloudflare.com/workers/", "Cloudflare Workers" %}
- {% aTargetBlank "https://deno.com/deploy", "Deno Deploy" %}
- {% aTargetBlank "https://www.electronjs.org", "Electron" %}
- {% aTargetBlank "https://fly.io", "Fly.io" %}
- {% aTargetBlank "https://supabase.com/docs/guides/functions", "Supabase functions" %}
- {% aTargetBlank "https://vercel.com/docs/functions/serverless-functions", "Vercel functions" %}

## Cons

While Drizzle supports many kinds of databases,
switching the configuration and code that works with one type
to work with another is fairly tedious.
There are differences in the configuration and code required for each.

The current documentation is better for some databases than others.
It can be difficult to determine how to correctly configure Drizzle
to work with some databases that it claims to support.

Drizzle would benefit greatly from having a GitHub repo of example projects,
one for each supported database/driver combination, that all show the
same basic example of configuring access to a one-to-many relationship.
This would help developers get started with using Drizzle.

It would be great if there was a tool that could modify an existing project
to switch from one database/driver combination to another.

## Example Project

All the example code in this page assumes the use of a PostgreSQL database.

Two npm packages that can be used to connect to the database are
{% aTargetBlank "https://github.com/brianc/node-postgres", "pg" %}
(aka node-postgres) and {% aTargetBlank
"https://github.com/porsager/postgres#connection", "postgres" %}.

When I tried to call Drizzle methods with a database connection
that was created with "pg", I got the error {% aTargetBlank
"https://github.com/brianc/node-postgres/issues/3120",
"TypeError: client.unsafe is not a function" %}.
But they work with the "postgres" package.

When I tried to run drizzle-kit commands using the dialog "postgres",
I got "error: unknown command '\*:postgres'".
But they work with the "pg" package.

What a mess! It seems I need to install and use both packages.

### Create Database

For PostgreSQL:

- Install PostgreSQL.

  In macOS this can be done using
  the Homebrew command `brew install postgresql`.
  If it has already been installed, it can be upgraded to the latest version
  by entering `brew upgrade postgresql`.

- Start the database server by entering `pg_ctl -D /usr/local/pgsql/data start`

- Create a database by entering `createdb {db-name}`.
  For this example, the database name is "drizzle-demo".

For SQLite, see <a href="/blog/topics/#/blog/sqlite" target="_blank">SQLite</a>
for information on installing it and creating a database.

### Create Project

- Install Node or Bun.
- Create a directory for the project and cd to it.
- Enter `npm init` or `bun init`
- Create a `src` directory.
- If using Node, create the file `src/index.mjs`
- If using Bun, move `index.ts` to the `src` directory.

### Install Drizzle and a Database Client

- Enter `npm install drizzle-orm` or `bun add drizzle-orm`
- Enter `npm install -D drizzle-kit` or `bun add -d drizzle-kit`
- If using PostgreSQL, enter `npm install postgres` or `bun add postgres`

### Add NPM Scripts

Add the following in `package.json`:

```json
  "scripts": {
    "demo": "node src/index.mjs",
    "migrations:generate": "drizzle-kit generate:pg",
    "migrations:pull": "drizzle-kit introspect:pg",
    "migrations:push": "drizzle-kit push:pg",
    "migrations:drop": "drizzle-kit drop --config=drizzle.config.ts",
    "studio": "drizzle-kit studio"
  },
```

When using Bun instead of Node, change the `demo` script to:

```json
"demo": "bun run src/index.ts",
```

### Describe Tables

Create the file `src/schema.mjs` (Node) or `src/schema.ts` (Bun)
containing the following:

```ts
import {relations} from 'drizzle-orm';
import {integer, pgTable, serial, text} from 'drizzle-orm/pg-core';

export const dogs = pgTable('dogs', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  breed: text('breed'),
  ownerId: integer('owner_id')
});

export const owners = pgTable('owners', {
  id: serial('id').primaryKey(),
  name: text('name').notNull()
});

// Drizzle Studio requires relations to be specified in both directions.
// In this case that is owners to dogs and dogs to owners.
export const ownersRelations = relations(owners, ({many}) => ({
  dogs: many(dogs)
}));

export const dogsRelations = relations(dogs, ({one}) => ({
  owner: one(owners, {
    fields: [dogs.ownerId],
    references: [owners.id]
  })
}));
```

### Configure Drizzle

Create the file `drizzle.config.ts` containing the following:

```ts
import type { Config } from "drizzle-kit";

export default {
  schema: "./src/schema.mjs",
  out: "./src/migrations",
  dbCredentials: {
    host: "localhost",
    database: "drizzle-demo",
  },
} satisfies Config;
```

When using Bun instead of Node, change the `schema` value to `./src/schema.ts`.

### Generate Initial Migration

Enter `npm run migrations:generate`  
or `bun run migrations:generate`.  
This will create a `.sql` file in the `src/migrations` directory.

### Create Tables

Enter `npm run migrations:push`  
or `bun run migrations:push`.  
This will create the tables
"dogs", "dogs_id_seq", "owners", and "owners_id_seq".

### Database Connection

Create the file `src/db.mjs` (Node) or `src/db.ts` (Bun)
containing the following.
This creates a database connection and can be imported into
any source file that needs to access the database.

```ts
import {drizzle} from 'drizzle-orm/postgres-js';
import postgres from 'postgres';
import * as schema from './schema.mjs';

const dbName = 'drizzle-demo';
const dbPrefix = 'postgres://postgres:adminadmin@0.0.0.0:5432';
const connectionString = dbPrefix + '/' + dbName;
const client = postgres(connectionString);

export const db = drizzle(client, {schema});
```

When using Bun instead of Node, change the schema import path to `./schema.ts`.

### Perform CRUD Operations

Change the contents of `src/index.mjs` (Node) or `src/index.ts` (Bun)
to the following and run it by entering `npm run demo`.

```js
import {eq} from 'drizzle-orm';
import {dogs, owners} from './schema.mjs';
import {db} from './db.mjs';

// Delete all records from the tables.
await db.delete(dogs);
await db.delete(owners);

// Insert new rows in the owners table.
let results = await db
  .insert(owners)
  .values({name: 'Tami'})
  .returning({id: owners.id});
const tamiId = results[0].id;

results = await db
  .insert(owners)
  .values({name: 'Amanda'})
  .returning({id: owners.id});
const amandaId = results[0].id;

// Insert new rows in the dogs table.
await db
  .insert(dogs)
  .values({name: 'Comet', breed: 'Greyhound', ownerId: tamiId});
await db.insert(dogs).values([
  {name: 'Maisey', breed: 'Treeing Walker Coonhound', ownerId: amandaId},
  {name: 'Ramsay', breed: 'Native American Indian Dog'},
  {name: 'Oscar', breed: 'German Shorthaired Pointer', ownerId: amandaId}
]);

// Modify a row in the dogs table.
await db.update(dogs).set({breed: 'Whippet'}).where(eq(dogs.name, 'Comet'));

// Delete a row from the dogs table.
await db.delete(dogs).where(eq(dogs.name, 'Ramsay'));

// Get all dogs.
results = await db.select().from(dogs);
console.log('All Dogs');
for (const result of results) {
  console.log('-', result.name);
}

// Get dogs owned by Amanda.
results = await db
  .select()
  .from(dogs)
  .innerJoin(owners, eq(owners.id, dogs.ownerId))
  .where(eq(owners.name, 'Amanda'));
console.log("\nAmanda's Dogs");
for (const result of results) {
  console.log('-', result.dogs.name);
}

process.exit(); // Why needed?
```

## Queries

Drizzle provides three methods for querying tables.

- `db.select([options]).from({table}).where({filter})`
- `db.query.{table}.findFirst([options])`
- `db.query.{table}.findMany([options])`

## Query Filtering

The `drizzle-orm` module provides the following methods
that can be used to filter query results:

- `and`
- `between` - numeric range
- `eq`
- `gt`
- `gte`
- `inArray`
- `isNotNull`
- `isNull`
- `like` - pattern match
- `lt`
- `lte`
- `ne`
- `not`
- `notBetween` - numeric range
- `notInArray`
- `notLike` - pattern match
- `or`
- `references` - for foreign keys

For example, the following query gets all users whose
full name includes "olk" and are at least 50 years old.

```js
const result = await
  .db
  .select()
  .from(users)
  .where(and(like(users.fullName, "%olk%"), gte(users.age, 50)));
```

## Generating Drizzle Schema

When there is an existing database, a Drizzle schema file that describes
all the tables in it can be generated with the command `drizzle-kit introspect`.
The path to the generated file is `src/migrations/schema.ts`.

The generated code does not include definitions of relations between tables.
Unfortunately those need to be added manually.

This command is often run from a script in `package.json` that is defined as:

```json
"migrations:pull": "drizzle-kit introspect:pg",
```

Enter `npm run migrations:pull` to run this.

## Schema Changes

To modify the schema for any of the tables:

1. Modify `src/lib/schemas.mjs`.
1. Enter `npm run migrations:generate` to generate a new `.sql` file
   in `src/migrations` directory.
1. Enter `npm run migrations:push` to apply the schema changes to the database.

## Dropping Migrations

The `drizzle-kit drop` command prompts for a `.sql` file
in the `src/migrations` directory to delete and then deletes it.
This is preferable to manually deleting the file
because doing so can break subsequent `drizzle-kit` commands.

WARNING: This does not undo changes made by the migration!

Drizzle does not provide a mechanism for rolling back applied migrations.
To do that, create a new migration that reverses the changes and apply it.

To iterate on schema changes in a development environment
without creating a new migration `.sql` file for the changes,
skip the use of `npm run migrations:generate`
and just use `npm run migrations:push`.
This syncs your schema with the database schema directly.

## Drizzle Studio

Drizzle Studio is a web-based database browser.
To view it:

1. Enter `npm run studio`  
   or `bun run studio`
1. Browse https://local.drizzle.studio

As of January 2024 this was still in beta.
It was fine for browsing data, but had issues with updating and adding data.
