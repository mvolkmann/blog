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

<img alt="Cloudflare Workers logo" style="border: none; width: 30%"
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

- ability write in a variety of programming languages,
  including JavaScript, TypeScript, Rust, C, and C++.

- run instantly without cold starts

  This may not be true when using the free tier.

- affordable

  The first 100,000 requests each day are free.
  After that the cost is $5 USD per 10 million requests.

- no servers to maintain

- provides edge storage of static assets using "Workers KV"

- can generate assets at runtime,
  including images, SVGs, PDFs, and more

## Projects on Web

A Cloudflare Worker can be created, edited, tested, and deployed
directly from the website without downloading any code.

- Browse {% aTargetBlank "https://www.cloudflare.com", "cloudflare.com" %}.
- If you do not yet have an account, click the "Sign up" button.
- If you do have an account, click the "Log in" button.
- Click "Workers & Pages" in the left nav.
- Click the "Create application" button.
- Click the "Create Worker" button.
- Change the name of the worker.
- Click the "Deploy" button. This must be done before the code can be edited.
- Click the "Edit code" button. For example:

  ```js
  export default {
    async fetch(request, env, ctx) {
      return new Response('<h1>Hello, Mark!</h1>', {
        headers: {
          'Content-Type': 'text/html'
        }
      });
    }
  };
  ```

- Edit the contents of the file "worker.js" directly in the browser.
- Click the "HTTP" tab.
- Click the "Send" button to see the response in the right pane.
- To save changes, click the "Save and deploy" button.

  A confirmation dialog will appear.
  Click the "Save and deploy" button inside the dialog.

- To view deployed app in a new browser tab, click the "workers.dev" link.

## Wrangler

The Wrangler command-line interface (CLI) supports creating, testing,
and deploying Cloudflare Worker apps locally.

To install it globally, enter `npm install -g wrangler`.

To authenticate Wrangler with your Cloudflare account,
enter `wrangler login`.
This opens a browser window that prompts
"Allow Wrangler to make changes to your Cloudflare account?"
Click the "Allow" button.
When the message "You have granted authorization to Wrangler!" appears,
close the browser window.

To continue development of a worker created in the Web UI, enter
`npm create cloudflare@2 {worker-name} -- --type pre-existing --existing-script {worker-name}`
This will ask "Do you want to use git for version control?"
It will then download the project and install the dependencies.

To start a local server for the app,
cd to the new project directory and enter `npm run dev`.

To test the app, browse localhost:8787.

## Local Projects

To create a new Cloudflare Worker project from the command-line,
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

To start a local server for the app,
cd to the new project directory and enter `npm run dev`.

To test the app, browse localhost:8787.

The local server provides hot reloading of only itself, not the web browser.

To deploy app again after making local changes, enter `npm run deploy`.

An alternative to creating a project with `npm create cloudflare -- {app-name}`
is to use `npm create hono@latest` and select the "cloudflare-workers" template.
TODO: How does this approach differ?

## Using Hono

This section walks through creating a Cloudflare Worker project that
uses {% aTargetBlank "https://hono.dev/", "Hono" %} for request routing.

- Create a new project named "hono-dogs" as described above.

- Install Hono dependencies by entering the following commands:

  ```bash
  npm install hono
  npm install @hono/zod-validator
  ```

- Make the following modifications in `tsconfig.json`:

  ```json
  "jsx": "react-jsx",
  "jsxImportSource": "hono/jsx",
  ```

- Add the following in `wrangler.toml` to support
  serving static files from the `public` directory:

  ```toml
  [site]
  bucket = "./public"
  ```

- Create the three files described below.

- Test the site locally.

  Start a local server by entering `npm run dev`.
  Browse localhost:8787/dog to see the following:

  <img alt="Cloudflare Workers Hono dogs" style="width: 60%"
    src="/blog/assets/cloudflare-workers-hono-dogs.png?v={{pkg.version}}"
    title="Cloudflare Workers Hono dogs">

- Deploy the app by entering `npm run deploy`.

  The URL will be output after the line that begins with "Published".

### src/index.ts

```ts
import {Hono} from 'hono';
import {serveStatic} from 'hono/cloudflare-workers';
import dogRouter from './dog-router';

const app = new Hono();

// This serves static files from the
// [site] bucket directory specified in wrangler.toml.
app.get('/*', serveStatic({root: './'}));

app.get('/', c => c.redirect('/dog'));

app.route('/dog', dogRouter);

export default app;
```

### src/dog-router.ts

```ts
import {Context, Hono} from 'hono';
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

### static/styles.css

```css
body {
  font-family: sans-serif;
}

li {
  color: purple;
}
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

"{% aTargetBlank "https://developers.cloudflare.com/kv/get-started/",
"Workers KV" %} provides low-latency, high-throughput global storage
to your Cloudflare Workers applications.
Workers KV is ideal for storing user configuration data, routing data,
A/B testing configurations, authentication tokens,
and is well suited for read-heavy workloads."

To enable use of KV in a worker:

- Browse the {% aTargetBlank "https://dash.cloudflare.com/",
  "Cloudflare Dashboard" %} and log in if not already logged in.
- Click "Workers & Pages" in the left nav.
- Click the subcategory "Plans" in the left nav.

  The free plan offers up to 100,000 read operations per day
  and up to 1,000 write, delete, list operations per day.
  For more, click the "Purchase Workers Paid" button.

- Create a KV namespace.

  Enter `wrangler kv:namespace create {namespace}`.
  This will output lines to be added to `wrangler.toml`.

- Add the lines output by previous command in `wrangler.toml`.

TODO: Document the remaining instructions.

TODO: Can you store the map of dogs for your demo in a KV store?

## htmx

A complete web app developed with htmx can be deployed to a Cloudflare Worker!
TODO: Try this!

## Astro

To deploy an Astro app to Cloudflare, see {% aTargetBlank
"https://docs.astro.build/en/guides/integrations-guide/cloudflare/",
"@astrojs/cloudflare" %}.
