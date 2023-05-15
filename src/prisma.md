---
eleventyNavigation:
  key: Prisma
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.prisma.io", "Prisma" %} is the
"next-generation Node.js and TypeScript ORM".

Prisma supports managing and interacting with the following databases:

- CockroachDB
- Microsoft SQL Server
- MongoDB
- MySQL
- PostgreSQL
- SQLite

Prisma can be used in the following JavaScript/TypeScript environments:

- AdonisJs
- Express
- koa
- Fastify
- Feathers
- Foal TS
- hapi
- Micro
- NestJS
- Next.js
- Polka
- Sails

Prisma consists of three parts:

- "Prisma Client" is an "auto-generated and type-safe query builder
  for Node.js & TypeScript".
  It allows interacting with databases using an API rather than SQL statements.
- "Prisma Migrate" is a tool for migrating database schemas
  when changes are required.
- "Prisma Studio" is a "GUI to view and edit data in your database".

## Steps to Use

1. Define a Prisma schema for a database.
   This has it's own syntax which is similar to DDL, but not identical.
   This can be generated from the schema of an existing database.
1. Install Prisma Client by entering "npm install @prisma/client".
1. Generate code for accessing the database by entering "prisma generate".
1. Create an instance of the generated `PrismaClient` with the following code:

   ```js
   import { PrismaClient } from '@prisma/client'
   const prisma = new PrismaClient()
   ```

## Creating a Record

The following example creates a User record and an associated Address record.

```js
const user = await prisma.user.create({
  data: {
    // Can ids be auto-generated?
    name: 'Mark Volkmann',
    email: 'mark@gmail.com',
    address: {
      create: {
        street: '123 Some Street'
        city: 'Somewhere',
        state: 'CA',
        zip: 123456
      },
    },
  },
});
```

## Querying Records

The following query retrieves all users have a Gmail email address
and live in California.

```js
const users = await prisma.user.findMany({
  where: {
    email: { endsWith: '@gmail.com' },
    address: { state: 'CA' }
  }
}
```

## Updating Records

The following code updates the email address of a User with a given id.

```js
const user = await prisma.user.update({
  where: { id: 12345678 },
  data: { email: 'richard@gmail.com' }
});
```

## Deleting Records

The following code deletes one User record with a given id.

```js
const user = await prisma.user.deleteMany({
  where: { id: 12345678 },
});
```

The following code deletes all User records with a state of "CA".

```js
const users = await prisma.user.deleteMany({
  where: { state: 'CA' },
});
```


