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

- lightweight
- simplicity
- performance
- migrations
- no code generation
- zero dependencies
- database support

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

The schema is entirely defined in TypeScript.
It is used to create/migrate tables AND provided type checking in code.

- Install PostgreSQL.

  In macOS this can be done using
  the Homebrew command `brew install postgresql`.
  If it has already been installed, it can be upgraded to the latest version
  by entering `brew upgrade postgresql`.

- Start the database server by entering `pg_ctl -D /usr/local/pgsql/data start`

- Create a database by entering `createdb drizzle-demo`

- Create a Node.js or Bun project.

  - Install Node or Bun.
  - Create a directory for the project and cd to it.
  - Enter `npm init` or `bun init`

- Enter `npm install drizzle-orm` or `bun add drizzle-orm`
- Enter `npm install pg` or `bun add pg`
- Enter `npm install postgres` or `bun add postgres`
- If on macOS and using Bun, enter `bun add @esbuild/darwin-x64`
- Enter `npm install -D drizzle-kit` or `bun add -d drizzle-kit`
- Enter `npm install dotenv`.
- Add the following scripts in `package.json`:

  ```json
    "build": "drizzle-kit build",
    "migrations:generate": "drizzle-kit generate:pg",
    "migrations:push": "drizzle-kit push:pg",
    "migrations:drop": "drizzle-kit drop --config=drizzle.config.ts",
    "start": "drizzle-kit start",
    "test": "drizzle-kit test"
  ```

- Create the file `src/lib/schema.ts` containing the following:

  ```ts
  import {relations} from 'drizzle-orm';
  import {integer, pgTable, serial, text} from 'drizzle-orm/pg-core';

  // To generate migrations for these, enter `bun run migrations:generate`.
  // To push a migration into the database, enter `bun run migrations:push`.

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

  export const dogsConfig = relations(owners, ({many}) => ({
    dogs: many(dogs)
  }));
  ```

- Create the file `drizzle.config.ts` containing the following:

  ```ts
  import type {Config} from 'drizzle-kit';
  import 'dotenv/config';

  export default {
    schema: "./src/lib/schema.ts",
    out: "./src/migrations",
    driver: "pg",
    dbCredentials: {
      connectionString: process.env.DATABASE_URL,
      host: "localhost",
      database: "drizzle-demo",
    },
  } satisfies Config;

  ```

- Generate the first migration by entering `npm run migrations:generate`.
  This will create a `.sql` file in the `src/migrations` directory.

- Create the tables described in the schema by entering
  `npm run migrations:push`.
  This will create the tables "dogs", "dogs_id_seq",
  "owners", and "owners_id_seq".

## Drizzle Studio

TODO: Document this.

```

```
