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

"Honō" is the Japanese word for "flame" or "blaze" which explains its logo.

Hono is significantly faster than
{% aTargetBlank "https://expressjs.com", "Express" %}
which is the most popular HTTP library for Node.js.

{% aTargetBlank "https://elysiajs.com", "Elysia" %} is a competitor to Hono.
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

Run the project by entering `npm run dev` or `bun dev`.

By default, the server listens on port 3000.
It also provides hot reloading.

## Routes

To define routes, create a `Hono` object and call the methods
`get`, `post`, `put`, `patch`, `delete`, and `all` on it.
Each of these methods take a URL path and
a function that is passed a `Context` object.

For example:

```js
const app = new Hono();

app.get('/some/path', (c: Context) => {
  ...
});
```

Path parameters are described in URL path strings as a name preceded by a colon.
Optional parameters are followed by `?`.
For example: `'/population/:state/:year?'`.

Path parameter names can also include a regular expression.
For example: `'/population/:state{[0-9]*}'`
TODO: When is this useful?

The callback functions passed to routes must return a `Response` object.
This is typically done by returning the result of a call to
`c.text`, `c.json`, or `c.html`.

It is also possible to override the route methods to simplify the code.
For example instead of having the callback functions return a `Response` object,
they could return a string (which could automatically use `c.text`),
JSX (which could automatically use `c.html`), or
any other kind of value (which could automatically use `c.json`).
This idea is implemented in code found in the issue {% aTargetBlank
"https://github.com/honojs/hono/issues/2037#issuecomment-1910957072",
"endpoints returning raw values" %}.

## Context

The `Context` object that is passed to routes has many properties and methods.
The most useful properties are `req` (a `HonoRequest` object)
and `res` (a `Response` object defined by the Fetch API).

The most useful methods on the `c.req` object
related to extracting data from a request
are described in the following table.

| Action                       | Code                                                          |
| ---------------------------- | ------------------------------------------------------------- |
| get value of request header  | `c.req.header('Some-Name')`                                   |
| get value of path parameter  | `c.req.param('some-name')`                                    |
| get value of query parameter | `c.req.query('some-name')`                                    |
| get value of text body       | `const text = await c.req.text();`                            |
| get `FormData` from body     | `const formData = await c.req.formData();`                    |
| get property from `formData` | `const value = (formData.get('property') as string) \|\| '';` |
| get value of JSON body       | `const object = await c.req.json();`                          |

The most useful methods on the `Context` object
related to creating a response
are described in the following table.

| Action                       | Code                                   |
| ---------------------------- | -------------------------------------- |
| set value of response header | `c.header('Some-Name', 'some value');` |
| set status code              | `c.status(someCode);`                  |
| return text response         | `return c.text('some text');`          |
| return JSON response         | `return c.json(someObject);`           |
| return HTML response         | `return c.html(someHTML);`             |
| return "Not Found" error     | `return c.notFound();`                 |
| redirect to another URL      | `return c.redirect('someURL');`        |

In the `c.html` method, `someHTML` is a string of HTML or JSX.

See the {% aTargetBlank "https://github.com/honojs/hono/issues/2037",
"endpoints returning raw values" %} proposal.

To share values between routes,
set them in one route with `c.set('someName', 'some value');`
and get them other routes with `c.get('someName')`.

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

Remove the `jsxFragmentFactory` property if it is present.

## Routers

Hono includes {% aTargetBlank "https://hono.dev/concepts/routers",
"five routers" %}.
Typically the default router is fine and this information can be ignored.

The provided routers are:

- `RegExpRouter`

  This performs regular expression matching for all routes.
  It is typically the fastest of the routers, but
  performance will be degrade if there are a large number defined routes.

- `TrieRouter`

  This router is much faster than Express,
  but not quite as fast as `RegExpRouter`.
  It supports some route path patterns that `RegExpRouter` does not support.

- `SmartRouter`

  This router selects between other routers at startup
  based on what it thinks will be the fastest for the configured routes.

- `LinearRouter`

  This router is best for environments such as edge functions
  where the application is initialized for every request
  because it performs route registration faster than `RegExpRouter`.

- `PatternRouter`

  This router is best for memory constrained environments.

The default router is `SmartRouter` configured to
select between `RegExpRouter` and `TrieRouter`.

To use a different router, pass one to the `Hono` constructor.
For example:

```ts
import {Hono} from 'hono';
import {LinearRouter} from 'hono/router/linear-router';

const app = new Hono({router: new LinearRouter()});
```

Hono provides presets for selecting a router based on how `Hono` is imported.
For example, `import {Hono} from '{somewhere}';`

| Import Hono From | Router Used                                            |
| ---------------- | ------------------------------------------------------ |
| `'hono'`         | `SmartRouter` selecting `RegExpRouter` or `TrieRouter` |
| `'hono/quick'`   | `SmartRouter` selecting `LinearRouter` or `TrieRouter` |
| `'hono/tiny'`    | `PatternRouter`                                        |

## CRUD Example

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

The port on which the server listens defaults to 3000.
To listen on a different port, say 1234, change the last line to the following.

```ts
export default {port: 1234, fetch: app.fetch};
```

### public/styles.css

```css
body {
  font-family: sans-serif;
}
```

### src/dog-router.tsx

```ts
import {Context, Hono} from 'hono';
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
  c.status(201);
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

  it('should create a dog', async () => {
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

    expect(res.status).toBe(201);
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

## File-based Routing

Hono does not support file-based routing, but the addon package
{% aTargetBlank "https://github.com/honojs/honox", "honox" %} does.

The following steps demonstrate using file-based routing with honox.

1. Create a new project by entering `bun create hono@latest`.

   For the template type, choose "bun".

1. cd to the new project directory.
1. Enter `bun install`.
1. Enter `bun add honox`.
1. Enter `bun add -d vite`.
1. Create the file `vite.config.ts` containing the following:

   ```ts
   import honox from 'honox/vite';
   import {defineConfig} from 'vite';

   export default defineConfig({
     plugins: [honox()]
   });
   ```

1. Rename the `src` directory to `app`.
1. Renamed `app/index.ts` to `app/server.ts`.
1. Replace the contents of `app/server.ts` with the following:

   ```ts
   import {showRoutes} from 'hono/dev';
   import {createApp} from 'honox/server';

   const app = createApp();
   showRoutes(app);
   export default app;
   ```

1. Edit `package.json`.

   Change `src/index.ts` to `app/server.ts` in the "dev" script.

   TODO: Maybe this needs to use the `vite` command.

1. Create the directory `src/routes`.
1. Inside the new directory, create the files
   `_404.tsx`, `_error.tsx`, `_renderer.tsx`, and `index.tsx`.

1. Add the following in `src/routes/_404.tsx`:

   ```ts
   import {NotFoundHandler} from 'hono';

   const handler: NotFoundHandler = c => {
     return c.render(<h1>Sorry, Not Found...</h1>);
   };

   export default handler;
   ```

1. Add the following in `src/routes/_error.tsx`:

   ```ts
   import {ErrorHandler} from 'hono';

   const handler: ErrorHandler = (e, c) => {
     return c.render(<h1>Error! {e.message}</h1>);
   };

   export default handler;
   ```

1. Add the following in `src/routes/_renderer.tsx`:

   ```ts
   import {jsxRenderer} from 'hono/jsx-renderer';

   export default jsxRenderer(({children}) => {
     return (
       <html lang="en">
         <head>
           <meta charset="UTF-8" />
           <meta
             name="viewport"
             content="width=device-width, initial-scale=1.0"
           />
         </head>
         <body>{children}</body>
       </html>
     );
   });
   ```

1. Add the following in `src/routes/index.tsx`:

   ```ts
   export default function Home() {
     return <h1>Welcome!</h1>;
   }
   ```

1. Start the server by entering `bun dev`.

   This gives me the error described in this {% aTargetBlank
   "https://github.com/honojs/honox/issues/38", "issue" %}.

1. Browse localhost:3000.

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
However, integration with
<a href="/blog/topics/#/blog/zod" target="_blank">Zod</a>,
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

This code also captures the route objects and exports their types
so they can be used in client code to call the endpoints
using the RPC approach described in the next section.

### src/dog-router.tsx Improved

```ts
import {Context, Hono} from 'hono';
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
const getAllRoute = router.get('/', (c: Context) => {
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
  id: z.coerce.number().positive()
});
const idValidator = zValidator('param', idSchema);
const getOneRoute = router.get('/:id', idValidator, (c: Context) => {
  const id = Number(c.req.param('id'));
  const dog = dogMap[id];
  c.status(dog ? 200 : 404);
  return c.json(dog);
});

// This creates a new dog.
const dogSchema = z
  .object({
    id: z.number().positive().optional(),
    name: z.string().min(1),
    breed: z.string().min(2)
  })
  .strict(); // no extra properties allowed
const dogValidator = zValidator('json', dogSchema);
const createRoute = router.post('/', dogValidator, async (c: Context) => {
  const data = (await c.req.json()) as unknown as NewDog;
  const dog = addDog(data.name, data.breed);
  c.status(201);
  return c.json(dog);
});

// This updates the dog with a given id.
const updateRoute = router.put(
  '/:id',
  idValidator,
  dogValidator,
  async (c: Context) => {
    const id = Number(c.req.param('id'));
    const data = (await c.req.json()) as unknown as NewDog;
    const dog = dogMap[id];
    if (dog) {
      dog.name = data.name;
      dog.breed = data.breed;
    }
    c.status(dog ? 200 : 404);
    return c.json(dog);
  }
);

// This deletes the dog with a given id.
const deleteRoute = router.delete('/:id', idValidator, async (c: Context) => {
  const id = Number(c.req.param('id'));
  const dog = dogMap[id];
  if (dog) delete dogMap[id];
  c.status(dog ? 200 : 404);
  return c.text('');
});

export default router;
export type CreateType = typeof createRoute;
export type DeleteType = typeof deleteRoute;
export type GetAllType = typeof getAllRoute;
export type GetOneType = typeof getOneRoute;
export type UpdateType = typeof updateRoute;
```

## RPC

Hono can export the type definitions for routes
that are based on the specified validators.
These definitions can be used in client code
to add type checking to endpoint calls
that are made using the Hono client.

Route types were exported in the code in the previous section.

The following is an example of client code that uses the Hono client.

TODO: Why don't I get any type information in VS Code when using these routes?

```ts
import {CreateType, DeleteType, GetAllType, UpdateType} from './dog-router';
import {hc} from 'hono/client';

const URL_PREFIX = 'http://localhost:3000/';

const createClient = hc<CreateType>(URL_PREFIX);
const deleteClient = hc<DeleteType>(URL_PREFIX);
const getAllClient = hc<GetAllType>(URL_PREFIX);
const updateClient = hc<UpdateType>(URL_PREFIX);

async function demo() {
  // Create a dog.
  let res = await createClient.dog.$post(
    {
      json: {
        name: 'Ramsay',
        breed: 'Native American Indian Dog'
      }
    },
    {
      headers: {
        Accept: 'application/json'
      }
    }
  );

  // Update a dog.
  res = await updateClient.dog[':id'].$put({
    param: {id: 1},
    json: {
      // id: 1,
      name: 'Fireball',
      breed: 'Greyhound'
    }
  });

  // Delete a dog.
  res = await deleteClient.dog[':id'].$delete({param: {id: 2}});

  // Get all the dogs.
  res = await getAllClient.dog.$get(
    {},
    {
      headers: {
        Accept: 'application/json'
      }
    }
  );
  const dogs = await res.json();
  console.log('dogs =', dogs);
}

demo();
```

The output from the code above is:

```text
dogs = {
  "1": {
    id: 1,
    name: "Fireball",
    breed: "Greyhound",
  },
  "3": {
    id: 3,
    name: "Ramsay",
    breed: "Native American Indian Dog",
  },
}
```

For more detail, see {% aTargetBlank
"https://hono.dev/guides/rpc#rpc-client", "RPC/Client" %}.

## Tests

The following code in the file `src/index.test.ts`
tests all the routes defined above.
To run it, enter `bun test`.

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

  it('should create a dog', async () => {
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

    expect(res.status).toBe(201);
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

## Performance

The following sites provide relevant benchmarks:

- {% aTargetBlank
  "https://medium.com/deno-the-complete-reference/deno-express-vs-fastify-vs-oak-vs-hono-whos-runs-fastest-0657d791c17a",
  "Deno — Express vs Fastify vs Oak vs Hono: Who’s runs fastest?" %}

  This shows Hono to be significantly faster than Fastify, Oak, and Express.

- {% aTargetBlank
  "https://medium.com/deno-the-complete-reference/bun-elysia-js-vs-hono-hello-world-b655a20f1f2b",
  "Bun: Elysia.js vs Hono" %}

  This concludes that "Elysia is marginally faster than Hono,
  but the winning margin is negligible."

- {% aTargetBlank "https://github.com/delvedor/find-my-way", "find-my-way" %}
  is another HTTP library to consider. Some benchmarks
  show this to be considerably faster than Elysia and Hono.
