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

To enable accessing environment-specific environment files,
enter `npm install dotenv`.
Then create a `.env` file that defines environment variables
with lines like `MONGO_URL=some-url`.
A different version of this file can be created for each environment,
such as dev, test, stage, and production.

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

Set MONGO_URL to
mongodb+srv://{username}:{password}@cluster0.3efbd.mongodb.net/myFirstDatabase?retryWrites=true&w=majority

Here is code for an example server.

```js
import dotenv from 'dotenv';
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

// Load environment variables from the .env file into process.env.
// For example, process.env.MONGO_URL.
dotenv.config();

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
