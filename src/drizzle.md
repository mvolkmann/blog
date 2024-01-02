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

{% aTargetBlank "https://orm.drizzle.team", "Drizzle" %} is a TypeScript-based
Object Relational Mapper (ORM) library that is free and open-source.
It competes with other popular ORMs such as
{% aTargetBlank "https://www.prisma.io", "Prisma" %}.

Drizzle was designed "to be a thin layer on top of SQL
and introduce minimal runtime overhead".

The methods supported by Drizzle are SQL-like
so it doesn't feel like learning a new syntax.

Queries created with the Drizzle Query API always result in one SQL query.
This helps with performance and minimizes round trips to the server.

The key features of Drizzle include:

- lightweight (32K minified)
- simple to use
- good performance
- support for schema migrations
- no code generation required
- zero dependencies
- supports many databases

  Supported databases include LiteFS, MySQL, Neon, PlanetScale, PostgreSQL,
  SQLite, Supabase, Turso, Vercel Postgres, Web SQLite, and Xata.
  Notable exceptions include Microsoft SQL Server and MongoDB.

- edge support

  This includes support for:

  - {% aTargetBlank "https://bun.sh", "Bun" %}
  - {% aTargetBlank "https://developers.cloudflare.com/workers/", "Cloudflare Workers" %}
  - {% aTargetBlank "https://deno.com/deploy", "Deno Deploy" %}
  - {% aTargetBlank "https://www.electronjs.org", "Electron" %}
  - {% aTargetBlank "https://fly.io", "Fly.io" %}
  - {% aTargetBlank "https://supabase.com/docs/guides/functions", "Supabase functions" %}
  - {% aTargetBlank "https://vercel.com/docs/functions/serverless-functions", "Vercel functions" %}

## Postgres Example

Drizzle database table schemas are defined entirely in TypeScript.
These are used to create/migrate tables AND provided type checking in code.

- Install PostgreSQL.

  In macOS this can be done using
  the Homebrew command `brew install postgresql`.
  If it has already been installed, it can be upgraded to the latest version
  by entering `brew upgrade postgresql`.

- Start the database server by entering `pg_ctl -D /usr/local/pgsql/data start`

- Create a database by entering `createdb {db-name}`.
  For this example, the database name is "drizzle-demo".

- Install Node or Bun.

- Create a Node or Bun project.

  - Create a directory for the project and cd to it.
  - Enter `npm init` or `bun init`

- Enter `npm install drizzle-orm`  
  or `bun add drizzle-orm`
- Enter `npm install pg`  
  or `bun add pg`
- If on macOS and using Bun, enter `bun add @esbuild/darwin-x64`
- Enter `npm install -D drizzle-kit`  
  or `bun add -d drizzle-kit`
- Add the following scripts in `package.json`:

  ```json
  "build": "drizzle-kit build",
  "migrations:generate": "drizzle-kit generate:pg",
  "migrations:push": "drizzle-kit push:pg",
  "migrations:drop": "drizzle-kit drop --config=drizzle.config.ts",
  "start": "drizzle-kit start",
  "test": "drizzle-kit test"
  ```

- Create the file `src/lib/schema.mjs` containing the following:

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

  export const dogsConfig = relations(owners, ({one}) => ({
    dogs: one(dogs)
  }));
  ```

- Create the file `drizzle.config.ts` containing the following:

  ```ts
  import type { Config } from "drizzle-kit";
  import "dotenv/config";

  export default {
    schema: "./src/lib/schema.mjs",
    out: "./src/migrations",
    driver: "pg",
    dbCredentials: {
      host: "localhost",
      database: "drizzle-demo",
    },
  } satisfies Config;
  ```

- Generate the first migration by  
  entering `npm run migrations:generate`  
  or `bun run migrations:generate`.  
  This will create a `.sql` file in the `src/migrations` directory.

- Create the tables described in the schema by  
  entering `npm run migrations:push`  
  or `bun run migrations:push`.  
  This will create the tables "dogs", "dogs_id_seq",
  "owners", and "owners_id_seq".

- Perform CRUD operations on the database  
  by creating the file `index.mjs` and  
  entering `node src/index.mjs`  
  or `bun run src/index.mjs`.

  ```js
  import {eq} from 'drizzle-orm';
  import {drizzle} from 'drizzle-orm/postgres-js';
  import postgres from 'postgres';
  import * as schema from './lib/schema.mjs';

  // Get a connection to the database.
  const dbName = 'drizzle-demo';
  const dbPrefix = 'postgres://postgres:adminadmin@0.0.0.0:5432';
  const connectionString = dbPrefix + '/' + dbName;
  const poolSize = 1;
  const client = postgres(connectionString, {max: poolSize});
  const db = drizzle(client, {schema});

  // Get a reference to the tables.
  const {dogs, owners} = schema;

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

## Schema Changes

To modify the schema for any of the tables:

1. Modify schemas defined in `src/lib/schemas.mjs`.
1. Enter `npm run migrations:generate` to generate a new `.sql` file
   in `src/migrations` directory.
1. Enter `npm run migrations:push` to apply the schema changes to the database.

## Drizzle Studio

TODO: Document this.
