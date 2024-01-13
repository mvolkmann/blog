---
eleventyNavigation:
  key: Hono
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="Hono logo" style="border: none; width: 20%"
  src="/blog/assets/hono-logo.png?v={{pkg.version}}"
  title="Hono logo">

## Overview

{% aTargetBlank "https://hono.dev/", "Hono" %}
is a JavaScript HTTP server library that runs in any JavaScript runtime.
This includes AWS Lambda, Bun, Cloudflare Pages, Cloudflare Workers,
Deno, Fastly, Lagon, Netlify, NextJS, Node.js, and Vercel.

{% aTargetBlank "https://hono.dev/", "ElysiaJS" %} is a competitor to Hono.
It has slightly better performance than Hono, but only runs in Bun.

## Projects

To create a new project that uses Hono,
enter `npm create hono@latest` or `bunx create-hono`.
This will prompt for:

- Target directory
- Which template do you want to use?

It will then run for almost a minute. Why so long?
When it finishes, the target directory will be created
and will contain the following files:

- `README.md`
- `package.json`
- `src/index.ts`
- `tsconfig.json`

Install the dependencies by entering `npm install` or `bun install`.

Run the project by entering `npm run dev` or `bun run dev`.

By default, the server listens on port 3000 and provide hot reloading.

## JSX

Hono supports using JSX to generate HTML responses.
This is useful in conjunction with
{% aTargetBlank "https://htmx.org", "htmx" %}.

IMPORTANT: Source files that use JSX must have a file extension of `.tsx`.

Verify that the following `compilerOptions` properties
are present in `tsconfig.json`.
They are by default in new Hono projects.

```json
"jsx": "react-jsx",
"jsxImportSource": "hono/jsx"
```

## Example

This example demonstrates implementing endpoints for
CRUD operations on a collection of dogs.
This code does not require any additional packages to be installed.

<img alt="Hono dog app" style="width: 50%"
  src="/blog/assets/hono-dog-app.png?v={{pkg.version}}">

### src/index.ts

```ts
import {Hono} from 'hono';
import {serveStatic} from 'hono/bun';
import {logger} from 'hono/logger';
import dogRouter from './dog-router';

const app = new Hono();

// This logs all HTTP requests to the terminal where the server is running.
app.use('/*', logger());

// This serves static files from the public directory.
app.use('/*', serveStatic({root: './public'}));

app.route('/dog', dogRouter);

export default app;
```

### public/styles.css

```css
body {
  font-family: sans-serif;
}
```

### src/dog-router.tsx

```ts
import {Hono, type Context} from 'hono';
import type {FC} from 'hono/jsx';

const router = new Hono();

interface NewDog {
  name: string;
  breed: string;
}

interface Dog extends NewDog {
  id: number;
}

let lastId = 0;

// The dogs are maintained in memory.
const dogMap: {[id: number]: Dog} = {};

function addDog(name: string, breed: string): Dog {
  const id = ++lastId;
  const dog = {id, name, breed};
  dogMap[id] = dog;
  return dog;
}

addDog('Comet', 'Whippet');
addDog('Oscar', 'German Shorthaired Pointer');

// This provides HTML boilerplate for any page.
const Layout: FC = props => {
  return (
    <html>
      <head>
        <link rel="stylesheet" href="/styles.css" />
        <title>{props.title}</title>
      </head>
      <body>{props.children}</body>
    </html>
  );
};

// This returns JSX for a page that
// list the dogs passed in a prop.
const DogPage: FC = ({dogs}) => {
  const title = 'Dogs I Know';
  return (
    <Layout title={title}>
      <h1>{title}</h1>
      <ul>
        {dogs.map((dog: Dog) => (
          <li>
            {dog.name} is a {dog.breed}.
          </li>
        ))}
      </ul>
    </Layout>
  );
};

// This gets all the dogs as either JSON or HTML.
router.get('/', (c: Context) => {
  const accept = c.req.header('Accept');
  if (accept && accept.includes('application/json')) {
    return c.json(dogMap);
  }

  const dogs = Object.values(dogMap).sort((a, b) =>
    a.name.localeCompare(b.name)
  );
  return c.html(<DogPage dogs={dogs} />);
});

// This gets one dog by its id as JSON.
router.get('/:id', (c: Context) => {
  const id = Number(c.req.param('id'));
  const dog = dogMap[id];
  c.status(dog ? 200 : 404);
  return c.json(dog);
});

// This creates a new dog.
router.post('/', async (c: Context) => {
  const data = (await c.req.json()) as unknown as NewDog;
  const dog = addDog(data.name, data.breed);
  return c.json(dog);
});

// This updates the dog with a given id.
router.put('/:id', async (c: Context) => {
  const id = Number(c.req.param('id'));
  const data = (await c.req.json()) as unknown as NewDog;
  const dog = dogMap[id];
  if (dog) {
    dog.name = data.name;
    dog.breed = data.breed;
  }
  c.status(dog ? 200 : 404);
  return c.json(dog);
});

// This deletes the dog with a given id.
router.delete('/:id', async (c: Context) => {
  const id = Number(c.req.param('id'));
  const dog = dogMap[id];
  if (dog) delete dogMap[id];
  c.status(dog ? 200 : 404);
  return c.text('');
});

export default router;
```

### src/index.test.ts

This tests all the CRUD functionality.
To run all the tests, enter `bun test`.

```ts
import {describe, expect, it} from 'bun:test';
import app from '.';

const comet = {id: 1, name: 'Comet', breed: 'Whippet'};

const URL_PREFIX = 'http://localhost:3000/dog';

async function getAllDogs() {
  const req = new Request(URL_PREFIX);
  req.headers.set('Accept', 'application/json');
  const res = await app.fetch(req);
  return res.json();
}

async function getDogById(id: number) {
  const req = new Request(`${URL_PREFIX}/${id}`);
  const res = await app.fetch(req);
  return res.status === 200 ? res.json() : undefined;
}

describe('dog endpoints', () => {
  it('should get all dogs', async () => {
    const dogs = await getAllDogs();
    expect(Object.keys(dogs).length).toBe(2);
    expect(dogs[1]).toEqual(comet);
  });

  it('should get one dog', async () => {
    const req = new Request(URL_PREFIX + '/1');
    const res = await app.fetch(req);
    const dog = await res.json();
    expect(dog).toEqual(comet);
    expect(res.status).toBe(200);
  });

  it('should add a dog', async () => {
    const dog = {
      name: 'Ramsay',
      breed: 'Native American Indian Dog'
    };
    const req = new Request(URL_PREFIX, {
      method: 'POST',
      body: JSON.stringify(dog),
      headers: {
        'Content-Type': 'application/json'
      }
    });
    const res = await app.fetch(req);

    expect(res.status).toBe(200);
    const newDog = await res.json();

    // Verify that the dog was added.
    const dogs = await getAllDogs();
    const foundDog = dogs[newDog.id];
    expect(newDog.name).toBe(foundDog.name);
    expect(newDog.breed).toBe(foundDog.breed);
  });

  it('should update a dog', async () => {
    const dog = {
      name: 'Fireball',
      breed: 'Greyhound'
    };
    const req = new Request(URL_PREFIX + '/1', {
      method: 'PUT',
      body: JSON.stringify(dog),
      headers: {
        'Content-Type': 'application/json'
      }
    });
    const res = await app.fetch(req);

    expect(res.status).toBe(200);
    const updatedDog = await res.json();
    expect(updatedDog.name).toBe(dog.name);
    expect(updatedDog.breed).toBe(dog.breed);

    // Verify that the dog was updated.
    const dogs = await getAllDogs();
    const foundDog = dogs[updatedDog.id];
    expect(updatedDog.name).toBe(foundDog.name);
    expect(updatedDog.breed).toBe(foundDog.breed);
  });

  it('should delete a dog', async () => {
    const id = 1;
    const req = new Request(`${URL_PREFIX}/${id}`, {
      method: 'DELETE'
    });
    const res = await app.fetch(req);

    expect(res.status).toBe(200);

    // Verify that the dog was deleted.
    const dog = await getDogById(id);
    expect(dog).toBeUndefined();
    const dogs = await getAllDogs();
    expect(dogs[id]).toBeUndefined();
  });
});
```

## Validation

Hono supports validating the following kinds of request data
passed to endpoints:

- `cookie` to validate cookies
- `form` to validate form submissions
- `header` to validate request headers
- `json` to validate JSON in request bodies
- `param` to validate path parameters
- `query` to validate query parameter

When validation fails,
the response status code will be 400, indicating a bad request,
and the response body will be a JSON object similar to the following
that describes the validation error:

```json
{
  "success": false,
  "error": {
    "issues": [
      {
        "message": "{description-of-error}",
        "path": ["{name-of-invalid-property}"]
        ... more properties describing the error ...
      }
    ],
    "name": "ZodError"
  }
}
```

The support for specifying validation criteria is quite basic.
However, integration with {% aTargetBlank "https://zod.dev", "Zod" %}
can be installed for much better support.

To install a library that integrates Zod validation with Hono,
enter `npm install @hono/zod-validator` or `bun add @hono/zod-validator`.

To make this library available in code, add the following imports:

```ts
import {z} from 'zod';
import {zValidator} from '@hono/zod-validator';
```

To add validation to a route, add any number of calls to `zValidator`
as additional arguments after the route path and before the handler function.
Each call can validate a different kind of request data.
For example, one can be for validating path parameters
and another can be for validating the JSON request body.

The following code adds Zod validation to the dog endpoints
that were defined earlier.
Look for lines that contain "Schema" or "Validator".

### src/dog-router.tsx Improved

```ts
import {Hono, type Context} from 'hono';
import type {FC} from 'hono/jsx';
import {z} from 'zod';
import {zValidator} from '@hono/zod-validator';

const router = new Hono();

interface NewDog {
  name: string;
  breed: string;
}

interface Dog extends NewDog {
  id: number;
}

let lastId = 0;

// The dogs are maintained in memory.
const dogMap: {[id: number]: Dog} = {};

function addDog(name: string, breed: string): Dog {
  const id = ++lastId;
  const dog = {id, name, breed};
  dogMap[id] = dog;
  return dog;
}

addDog('Comet', 'Whippet');
addDog('Oscar', 'German Shorthaired Pointer');

// This provides HTML boilerplate for any page.
const Layout: FC = props => {
  return (
    <html>
      <head>
        <link rel="stylesheet" href="/styles.css" />
        <title>{props.title}</title>
      </head>
      <body>{props.children}</body>
    </html>
  );
};

// This returns JSX for a page that
// list the dogs passed in a prop.
const DogPage: FC = ({dogs}) => {
  const title = 'Dogs I Know';
  return (
    <Layout title={title}>
      <h1>{title}</h1>
      <ul>
        {dogs.map((dog: Dog) => (
          <li>
            {dog.name} is a {dog.breed}.
          </li>
        ))}
      </ul>
    </Layout>
  );
};

// This gets all the dogs as either JSON or HTML.
router.get('/', (c: Context) => {
  const accept = c.req.header('Accept');
  if (accept && accept.includes('application/json')) {
    return c.json(dogMap);
  }

  const dogs = Object.values(dogMap).sort((a, b) =>
    a.name.localeCompare(b.name)
  );
  return c.html(<DogPage dogs={dogs} />);
});

// This gets one dog by its id as JSON.
const idSchema = z.object({
  id: z.coerce.number()
});
const idValidator = zValidator('param', idSchema);
router.get('/:id', idValidator, (c: Context) => {
  const id = Number(c.req.param('id'));
  const dog = dogMap[id];
  c.status(dog ? 200 : 404);
  return c.json(dog);
});

// This creates a new dog.
const dogSchema = z
  .object({
    name: z.string().min(1),
    breed: z.string().min(2)
  })
  .strict(); // no extra properties allowed
const dogValidator = zValidator('json', dogSchema);
router.post('/', dogValidator, async (c: Context) => {
  const data = (await c.req.json()) as unknown as NewDog;
  const dog = addDog(data.name, data.breed);
  return c.json(dog);
});

// This updates the dog with a given id.
router.put('/:id', idValidator, dogValidator, async (c: Context) => {
  const id = Number(c.req.param('id'));
  const data = (await c.req.json()) as unknown as NewDog;
  const dog = dogMap[id];
  if (dog) {
    dog.name = data.name;
    dog.breed = data.breed;
  }
  c.status(dog ? 200 : 404);
  return c.json(dog);
});

// This deletes the dog with a given id.
router.delete('/:id', idValidator, async (c: Context) => {
  const id = Number(c.req.param('id'));
  const dog = dogMap[id];
  if (dog) delete dogMap[id];
  c.status(dog ? 200 : 404);
  return c.text('');
});

export default router;
```

## RPC

Hono can export the type definitions for endpoints
that are based on the specified validators.
These definitions can be used in client code
to add type checking to endpoint calls
that are made using the Hono client.

The following is an example of client code that uses the Hono client.

```ts
import {UpdateType} from './dog-router';
import {hc} from 'hono/client';

const URL_PREFIX = 'http://localhost:3000/dog';

const client = hc<UpdateType>(URL_PREFIX);
```

For more detail, see {% aTargetBlank
"https://hono.dev/guides/rpc#rpc-client", "RPC/Client" %}.
