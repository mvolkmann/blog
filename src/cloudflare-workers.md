---
eleventyNavigation:
  key: Cloudflare Workers
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="Hono logo" style="border: none; width: 30%"
  src="/blog/assets/cloudflare-workers-logo.svg?v={{pkg.version}}"
  title="Hono logo">

## Overview

{% aTargetBlank "https://workers.cloudflare.com", "Cloudflare Workers" %}
host edge functions.

Features provided include:

- automatic scaling

  There is no need to configure auto-scaling or load balancers.

- high performance global network

  Cloudflare workers run in a network of data centers that use V8 isolates
  that have very low latency (approximately 25ms in my testing).

- write in a variety of programming languages,
  including JavaScript, TypeScript, Rust, C, and C++.

- run instantly without cold starts

- affordable

  The first 100,000 requests each day are free.
  After that the cost is $5 USD per 10 million requests.

- no servers to maintain

- provides edge storage of static assets using "Workers KV"

- can generate assets at runtime,
  including images, SVGs, PDFs, and more

## Projects

To create a new project that uses Hono,
enter `npm create cloudflare -- {app-name}`.
This will prompt for:

- Need to install the following packages: ... Ok to proceed?
- In which directory do you want to create you application?
- What type of application do you want to execute?

  - "Hello World" Worker
      - Website or web app
       - Example router & proxy Worker
       - Scheduled Worker (Cron Trigger)
       - Queue consumer & producer Worker
       - ChatGPT plugin
       - OpenAPI 3.1

- Do you want to use TypeScript?
- Do you want to use git for version control?
- Do you want to deploy your application?

  This will open a browser window where you can sign up for an account
  or log in if you already have an account.
  You will also be prompted in a browser window to
  allow "wrangler" to make changes in your account.

If you chose to deploy the application,
it will be opened in a new browser window.

When it finishes, the target directory will be created
and will contain the following files:

- `node_modules` directory
- `package.json`
- `package-lock.json`
- `src/index.ts`
- `tsconfig.json`
- `wrangler.toml`

To start a local server, enter `npm run dev`.

To see the app, browse localhost:8787.
The server provides hot reloading of only itself, not the web browser.

To deploy app again after making local changes, enter `npm run deploy`.

## Wrangler

The `wrangler` command supports creating, testing,
and deploying Cloudflare Worker apps.
To install it globally, enter `npm install -g wrangler`.

TODO: Is direct use of this command deprecated?

## Using Hono

This section walks through creating a Cloudflare Worker project that
uses {% aTargetBlank "https://hono.dev/", "Hono" %} for request routing.

- Create a new project named "hono-dogs" as described above.

- Install Hono dependencies by entering the following commands:

  ```bash
  npm install hono`
  npm install @hono/zod-validator
  ```

- Make the following modifications in `tsconfig.json`:

  ```json
  "jsx": "react-jsx",
  "jsxImportSource": "hono/jsx",
  ```

- Create the two files described below:

- Test the site locally.

  Start a local server by entering `npm run dev`.
  Browse localhost:8787/dog to see a JSON object describing dogs.

- Deploy the app by entering `npm run deploy`.

  The URL will be output after the line that begins with "Published".

### src/index.ts

```ts
import {Hono} from 'hono';
import dogRouter from './dog-router';

const app = new Hono();

app.get('/', c => c.redirect('/dog'));

app.route('/dog', dogRouter);

export default app;
```

### src/dog-router.ts

```ts
import {Hono, type Context} from 'hono';
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

// This gets all the dogs as either JSON or HTML.
const getAllRoute = router.get('/', (c: Context) => {
  return c.json(dogMap);
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

## Viewing Workers

To view your workers, browse https://cloudflare.com.

Login to see the dashboard page.

Click "Workers & Pages" in the left nav
to display a list of the deployed workers.

For each worker, the number of requests sent to it
and the number of errors logged will be displayed.

Click a worker to see detail about it, including its deployed URL.

## KV Stores

TODO: Document how to use these.
TODO: Can you store the map of dogs for your demo in a KV store?

See https://developers.cloudflare.com/workers/wrangler/migration/v1-to-v2/wrangler-legacy/commands/#kv.

## htmx

A complete web app developed with htmx could be deployed to a Cloudflare Worker!
TODO: Try this!
