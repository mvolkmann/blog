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
  To run this, enter `npx prisma studio`

## Installing

Enter the following commands to create a project
that uses Prisma, TypeScript, and SQLite.
Slightly different steps are needed to add use of Prisma to an existing project
and select different options.

1. `mkdir my-project`
1. `cd my-project`
1. `npm init -y`
1. `npm install -D typescript ts-node @types/node`
1. `npm tsc --init`
1. `npm install -D prisma`
1. `npx prisma init --datasource-provider sqlite`
1. Add your schema definition in the file `prisma/schema.prisma`.
   This has it's own syntax which is similar to DDL, but not identical.
   This can also be generated from the schema of an existing database.
   See "Example Schema" below.
1. `npx prisma migrate dev --name init`
1. `npm install @prisma/client`
1. `npx prisma generate`

## Example Schema

This example comes from the Prisma {% aTargetBlank
"https://www.prisma.io/docs/getting-started/quickstart", "Quickstart" %} page.

```
model User {
  id    Int     @id @default(autoincrement())
  email String  @unique
  name  String?
  posts Post[]
}

model Post {
  id        Int     @id @default(autoincrement())
  title     String
  content   String?
  published Boolean @default(false)
  author    User    @relation(fields: [authorId], references: [id])
  authorId  Int
}  id    Int     @id @default(autoincrement())
```

## Creating Client Instance

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
    // id is an autoincrement property.
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

## Pagination

Prisma supports {% aTargetBlank
"https://www.prisma.io/docs/concepts/components/prisma-client/pagination",
"pagination" %} with the `skip` and `take` properties
of the object passed to the `findMany` method.
