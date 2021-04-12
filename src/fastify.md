---
eleventyNavigation:
  key: Fastify
layout: topic-layout.njk
---

{% aTargetBlank "https://www.fastify.io", "Fastify" %}
is a web server framework for Node.js that is an alternative to Express.

Benefits include:

- one of the fastest Node-based web servers
- supports JSON Schema for validating requests
- uses Pino for logging
- maintains a TypeScript type declaration file
  so usage can be type checked
- provides a good developer experience

To install Fastify in a project, enter `npm install fastify`.

To enable serving static files, enter `npm install fastify-static`.

To run the server in watch mode,
install `nodemon` by entering `npm install -g nodemon`.
Then run the app using the `nodemon` command instead of `node`.

To use ECMAScript Modules (ESM),
edit `package.json` and add the line `"type": "module",`.
This enables using the following syntax to
import a JavaScript file from another one:

```js
import './some-name.js'; // file extension is required
```

Here is code for an example server.

```js
import {fastify} from 'fastify';
import fastifyStatic from 'fastify-static';
import path from 'path';
import {fileURLToPath} from 'url';

// Normally these names are defined by Node.js.
// But when "type" is set to "module" in package.json,
// these go away.
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PORT = 1919;

const app = fastify();

async function startApp() {
  try {
    // Serve static files from the "public" directory.
    app.register(fastifyStatic, {
      root: path.join(__dirname, 'public')
    });

    // Define "/" route.
    app.get('/', {}, (request, reply) => {
      reply.send({foo: 'bar'}); // sets Content-Type to application/json
    });

    await app.listen(PORT);
    console.log('listening on port', PORT);
  } catch (e) {
    console.error(e);
  }
}

startApp();
```
