---
eleventyNavigation:
  key: Bun
layout: topic-layout.njk
---

<figure style="width: 33%">
  <img alt="Ziggy, the Zig mascot" style="border: 0"
    src="/blog/assets/bun-logo.svg?v={{pkg.version}}">
</figure>

## Overview

"{% aTargetBlank "https://bun.sh", "Bun" %} is a fast JavaScript all-in-one toolkit."
It includes a JavaScript runtime, package manager, bundler, and test runner.
All of this is is free and open source.

Bun can be used as a drop-in replacement for npm and Node.js.
The Bun runtime supports nearly all Node.js builtin modules (around 40 of them).
Rarely used modules such as dgram, http2, inspector, and repl
are not yet supported.
The v8 module is specific to Webkit which Bun does not use.

## Benefits Over npm and Node.js

The benefits of using Bun over npm and Node.js include:

- Bun provides significantly better performance.
- Bun simplifies the current state of JS/TS tooling
  (transpilers, bundlers, testing tools, and so on).
  There is less to install and configure and it is easier to use.
- Bun supports both CommonJS (`require`) and ESM (`import`)
  in the same source files.
- Bun supports TypeScript out of the box.
- Bun supports JSX/TSX files for generating HTML using React.
- Bun has builtin support for SQLite databases using the `bun:sqlite` module.
  It is 2 times faster than Deno and 4 times faster than Node.js
  in queries per second.
  Future support for MySQL and Postgres is planned.
- Bun has builtin support for Jest-compatible unit tests
  using the `bun:test` module.
- Bun has better support for Web APIs including
  `fetch`, `Request`, `Response`, `ReadableStream`, `WebSocket`, and more.
  There is no need to install dependencies such as `node-fetch` and `ws`.
  Bun's native implementation of these tends to be
  faster than third-party libraries used with Node.js.
- Bun provides native hot reloading using the `--hot` flag
  which updates a running server without restarting it.
  This allows preserving application state
  and connections such as HTTP and WebSockets.
  Hot reloading in Node.js currently requires either using
  an external tool like nodemon or the experimental `--watch` flag.
- Bun provides bun-specific APIs that are alternatives to many npm modules.
  These are highly optimized and perform much better than their Node.js equivalents.
  - `Bun.file` returns a `File` object with the same properties as
    DOM `File` objects.  The contents are lazily loaded.
    It provides many `async` methods for reading content in different formats
    including text, json, stream, and ArrayBuffer.
    These read files up to 10 times faster than Node.js equivalents.
  - `Bun.write` writes many kinds of values including strings, buffers, files,
    and responses from HTTP requests.
    It writes 3 times faster than Node.js equivalents.
  - `Bun.serve` starts an HTTP server based on web APIs.
    It handles 4 times as many requests per second as the Node.js equivalent.
  - `Bun.env` provides access to environment variables
    without installing and using the `dotenv` package.
  - Builtin Websocket support handles 5 times as many messages per second
    as the Node.js equivalent and is easier to use.
  - Builtin password hashing using Bcrypt and Argon is provided.
  - Bun provides many more APIs!
- Bun has a plugin API that is similar to esbuild.
  Many esbuild plugins work in Bun without modification.

The Bun team makes the following additional performance claims:

- Bun is 4 times faster than Node.js for a "hello world" program.
- The time to build and run TS code in Bun is 4 times faster than esbuild,
  15 times faster than TSX, and 43 times faster than TSC + Node.
- `bun install` is 29 times faster than npm and 17 times faster than pnpm;
- `bun run` can be used in place of `npm run` and is 5 times faster.
- `bun test` is 8 times faster than Vitest and 13 times faster than Jest.

## History

Development of Bun started in 2022 by Jarred Sumner with VC funding.
Bun became stable (1.0) in September 2023.

Bun was created by the startup company
{% aTargetBlank "https://oven.sh/", "Oven.sh" %}
because that’s what buns come out of.
“Oven has decided to adopt a strategy twinned with Deno Company.
They will launch sales of a cloud architecture based on Bun.”

The name "Bun" came from a friend who suggested it
because she has a bunny named "Bun".
The initial reaction was "I'm not going to name it after your bunny.
And then I thought about it more and it made some sense."
It also makes sense because
"it’s a bundling of the JavaScript ecosystem and a bundler".

## Implementation

Bun is built on JavaScriptCore (JSC) from Safari
rather than V8 which is used by Chrome.
JSC has faster startup time and lower memory usage than V8,
but slightly less overall performance.

Bun is implemented in Zig and some C++.
"Zig is sort of similar to writing C, but with better memory safety features
in debug mode and modern features like defer (sort of similar to Go's)
and arbitrary code can be executed at compile-time via comptime.
It's really good for writing performant low-level software."
Zig "has very few keywords so it's a lot easier to learn than,
for example, C++ or Rust."

Oven will donate to the Zig Software Foundation to keep the project going.

## Framework Support

Bun works with nearly all major web frameworks including
Next.js, SvelteKit, Astro, Nuxt, and Fastify.

## Installing

To install Bun for macOS, Linux, or Windows Subsystem for Linux (WSL),
enter `curl -fsSL https://bun.sh/install | bash`.
This adds the `bun` executable.

To verify the installation and see the version that is installed,
enter `bun --version` or `bun -v`.

To upgrade to the latest version, enter `bun upgrade`.

"Bun provides a limited, experimental native build for Windows.
At the moment, only the Bun runtime is supported."

## REPL

To start a Read-Evaluate-Print-Loop, enter `bun repl`.
This takes a few seconds to install the first time it is run,
but is much faster in subsequent runs.

## Projects

To create a new Bun project, create a directory, cd to it.
For a vanilla Bun project, enter `bun init`.
This creates the following files:

- `README.md`

  This provides basic information about using Bun.
  The contents should be replaced with project-specific information.

- `bun.lockb`

  This is the Bun equivalent of `package-lock.json`, but it is in a binary format.

- `index.ts`

  This is the entry point source file.

- `package.json`

  This describes dependencies, scripts, and more.

- `tsconfig.json`

  This configures TypeScript.

It also creates the directory `node_modules` that holds code for dependencies.
Initially it contains the directories `.bin`, `bun-types`, and `typescript`.
There is no need to run `npm install` to get started.

The file `index.ts` only contains the following:

```js
console.log("Hello via Bun!");
```

To run the project, enter `bun run index.ts`.
Note that TypeScript source files can be executed using `bun run`
without installing additional packages or configuring anything.

It is somewhat common to create a `src` directory
and move `index.ts` and other source files into it.

To create a project from a template,
enter `bun create {template} {destination}`.
For example, to create a project that uses Svelte,
enter `bun create svelte@latest my-svelte-app` (or `bun c`).
Some templates will ask questions about your preferences
and some will output instructions about what to do next.

## Help

To see all the subcommands supported by the `bun` command, enter `bun`.
The subcommands include `init`, `run`, `test`, `repl`, `upgrade`, and more.

To see all the options supported by the `bun` command, enter `bun --help`.

To see a list of all scripts defined in `package.json`, enter `bun.run`.

## Package Manager

The Bun package manager works with npm packages.
It uses `package.json` files just like npm,
but it uses a binary `bun.lockb` file instead of `package-lock.json`.

The `bun` command can be used as a replacement for the `npm`
even when not using the bun runtime.
A motivation for doing this is that `bun` is significantly faster than `npm`.

Just like in Node.js, Bun tracks dependencies in a `package.json` file
and installs dependencies in the `node_modules` directory.

Adding dependencies with the `bun` command is
much faster than adding them with the `npm` command.

To add a runtime dependency to a Bun project,
enter `bun add {package-name}` (or `bun a`).

To add a development dependency to a Bun project,
enter `bun add -d {package-name}`.

To update a runtime or development dependency in a Bun project,
enter `bun update {package-name}`.

To remove a runtime or development dependency to a Bun project,
enter `bun remove {package-name}` (or `bun rm`).

To install all the dependencies listed in `package.json`,
enter `bun install` (or `bun i`).

All downloaded packages are stored in the global cache directory
`~/.bin/install/cache`.
When a project adds a dependency to a package version
that is already in the global cache directory,
it is "copied" from there instead of downloading it again.
In Linux this uses hardlinks.
In MacOS, this uses "clonefile" which implements "copy on write".
In both cases, the operation is extremely fast
because the files are not actually copied.

## Linting

Bun does not include support for code linting.
To add this using ESLint:

1. Enter `npm init @eslint/config`.
   Note that this uses `npm` and not `bun`.
   This will ask a series of questions, install the required dependencies,
   and create an ESLint configuration file.

1. Add the following script in `package.json`:

   ```json
   "lint": "eslint 'src/**/*.{css,html,ts,tsx}'",
   ```

To lint all the source files in the current project,
enter `bun run lint`.

## Code Formatting

Bun does not include support for code formatting.
To add this using Prettier:

1. Install Prettier with `bun add -d prettier`

1. Create the file `.prettierrc` with content similar to the following:

   ```text
   {
     "arrowParens": "avoid",
     "bracketSpacing": false,
     "singleQuote": true,
     "trailingComma": "none"
   }
   ```

1. Add the following script in `package.json`:

   ```json
   "format": "prettier --write 'src/**/*.{css,html,ts,tsx}'",
   ```

To format all the source files in the current project,
enter `bun run format`.

## Unit Tests

Bun has builtin support for implementing and running unit tests
that are mostly compatible with Jest.

Suppose we have the following code in the file `math.ts`:

```ts
export function add(n1: number, n2: number): number {
  return n1 + n2;
}
```

To implement a unit test for the module defined above,
add the following code in the file `math.test.ts`:

```ts
import { expect, test } from "bun:test";
import { add } from "./math";

test("add", () => {
  expect(add(2, 2)).toBe(4);
});
```

To run all the unit tests in the current project,
including those found in subdirectories, enter `bun test`.

To only run tests whose name matches a given pattern,
add the `--test-name-pattern {pattern}` option.

To automatically rerun tests when code changes are saved,
enter `bun --watch test`.

Other functions that can be imported from `bun:test` include
`describe`, `beforeAll`, `afterAll`, `beforeEach`, and `afterEach`.

By default tests timeout and are considered failed after five seconds.
To change this, add the `--timeout {seconds}` option.

## Bundling

Bun can bundle all the files for an application into a single JavaScript file.

To demonstrate this:

1. Create a new project by entering `bun init`.

1. Create the file `bar.ts` containing the following:

   ```ts
   export function bar() {
     console.log('in bar.ts');
   }
   ```

1. Create the file `foo.ts` containing the following:

   ```ts
   import {bar} from './bar';

   export function foo() {
     console.log('in foo.ts');
     bar();
   }
   ```

1. Modify the file `index.ts` to contain the following:

   ```ts
   import {foo} from './foo';

   console.log('in index.ts');
   foo();
   ```

1. Bundle these three `.ts` files by entering
   `bun build index.ts --outdir build`.

1. Run the bundled file by entering `bun run build/index.js`.
   This should produce the following output:

   ```text
   in index.ts
   in foo.ts
   in bar.ts
   ```

As an alternative to the last step,
create the file `bundle.ts` containing the following
and run it by entering `bun run bundle.ts`:

```ts
Bun.build({
  entrypoints: ['index.ts'],
  outdir: 'build'
});
```


## Bun Global Variable

The `Bun` global variable has many properties, most of which are functions.
Highlights include:

- `Bun.build`: compiles a collection of source files into a single source file
- `Bun.serve`: function that starts an HTTP server

## HTTP Server

To implement a basic HTTP server,
replace the contents of `index.ts` with the following:

```js
const server = Bun.serve({
  port: 3000,
  fetch(req) {
    return new Response('Hi! This is my first Bun server!');
  },
});


console.log('Server started on port ', server.port);
```

To run this, enter `bun run index.js` and browse `localhost:3000`.

To simplify starting the server, add the following in `package.json`:

```json
  "scripts": {
    "start": "bun run index.ts"
  }
```

Now the server can be started by entering `bun start` or `bun run start`.

## Watch Mode

To automatically restart a server when code changes are detected,
add the following script in `package.json`:

```json
    "dev": "bun --watch index.ts"
```

Then start the server with `bun run dev`.

To patch the code in place when code changes are detected
and not restart the server, use the `--hot` flag in place of `--watch`.
Technically this updates the internal module cache with the new code.
This will not refresh a web browser that is
displaying content obtained from a Bun server.

For more detail, see {% aTargetBlank "https://bun.sh/docs/runtime/hot",
"Watch mode" %}.

This will not automatically refresh browsers that browsing the server URL.

## Environment Variables

Bun has builtin support for getting environment variables from the file `.env`.
For example, suppose this file contains `FOO=bar`.
The value `bar` can be obtained with `process.env.FOO` or `Bun.env.FOO`.

Changes to the `.env` after a server is started
are only available if the server is restarted.

## Executable Files

To allow a JS/TS source file to be executed directly:

1. Add the line `#!/usr/bin/env bun` at the beginning of the file.
1. Compile the file with `bun build ./file-name.ts --outfile file-name --compile`
1. Enter `file-name`.

## Executing Without Installing

The `npx` command can be used to execute a Node package
that may not already be installed.
For example, `npx cowsay Hello Cow!` outputs ASCII text
that depicts a cow with a speech bubble.

The same can be done with Bun using `bun x` (or `bunx`).
The main difference is that the Bun approach is much faster.

## Writing and Reading Files

The following code demonstrates how to write and read text files
using the Bun APIs.

```ts
import {expect, test} from 'bun:test';

test('write/read file', async () => {
  const filePath = './data.txt';
  const content = 'Hello, World!';
  // The second argument to the write function can be a string,
  // Buffer, file, or the awaited result of a fetch call.
  await Bun.write(filePath, content);

  const file = Bun.file(filePath);
  const actual = await file.text();
  // Other methods on File objects include arrayBuffer, blob, and json.
  expect(actual).toBe(content);
});
```

## WebSockets

Bun has builtin support for WebSockets.

The following code in the file `ws-server.ts`
demonstrates writing a WebSocket server.
It replies to all text messages with the same text converted to uppercase.
To run this, enter

```ts
const server = Bun.serve({
  port: 1919,
  fetch(req, server) {
    if (server.upgrade(req)) return;
    return new Response('WebSocket upgrade failed', {status: 500});
  },
  websocket: {
    open(ws) {
      console.log('WebSocket opened');
    },
    message(ws, data) {
      console.log('received:', data);
      console.log('type:', typeof data);
      if (typeof data === 'string') {
        console.log('sending response');
        ws.send(data.toUpperCase());
      }
    },
    close(ws, code, reason) {
      console.log(`WebSocket closed with code ${code} and reason "${reason}"`);
    }
  }
});

console.log('WebSocket server is listening on port', server.port);
```

The following code in the file `ws-client.html`
demonstrates writing a WebSocket client.
After starting the server above, open this in a web browser.
Enter any text in the input and click the "Send" button.

```ts
<html lang="en">
  <head>
    <script>
      let input, message, output;
      window.onload = () => {
        message = document.getElementById('message');
        input = document.getElementById('input');
        output = document.getElementById('output');
      };

      const ws = new WebSocket('ws://localhost:1919');
      ws.addEventListener('open', event => {
        send('opened connection');
      });
      ws.addEventListener('message', event => {
        output.innerHTML = event.data;
      });

      function send(text) {
        input.innerHTML = text;
        ws.send(text);
      }
    </script>
  </head>
  <body>
    <h1>WebSocket Client</h1>
    <input id="message" />
    <button onclick="send(message.value)">Send</button>
    <div>Input: <span id="input" /></div>
    <div>Output: <span id="output" /></div>
  </body>
</html>
```

## SQLite

Bun has builtin support for SQLite databases.
However, it does not install SQLite.
For macOS, SQLite can be installed using Homebrew.

After installing SQLite, a database containing a table with records
can be created with the following steps:

```bash
sqlite3 todos.db
sqlite> create table todos(id string primary key, text string, completed numeric);
sqlite> .schema
CREATE TABLE todos(id string primary key, text string, completed numeric);
sqlite> insert into todos values('t1', 'cut grass', 0);
sqlite> insert into todos values('t2', 'buy milk', 1);
sqlite> select * from todos;
t1|cut grass|0
t2|buy milk|1
sqlite> .exit
```

This creates the file `todos.db` that contains all the tables for this database
(only one in this case).

To access this database from code, write something like the following
in the file `database.ts`:

```ts
import {Database} from 'bun:sqlite';

const db = new Database('todos.db', {create: true});
const query = db.query('select * from todos;');
const todos = query.all(); // get();
console.log(todos);
```

Run this with `bun run database.ts`.
It will output the following:

```text
[
  {
    id: "t1",
    text: "cut grass",
    completed: 0
  }, {
    id: "t2",
    text: "buy milk",
    completed: 1
  }
]```

## HTTP Server

Bun has builtin support for implementing HTTP servers
using the `Bun.serve` function.

See the example at {% aTargetBlank
"https://github.com/mvolkmann/bun-http-server", "bun-http-server" %}
GitHub repository.

## ElysiaJS

{% aTargetBlank "https://elysiajs.com", "ElysiaJS" %} is a
"TypeScript framework supercharged by Bun with End-to-End Type Safety,
unified type system and outstanding developer experience."

To create a new Bun project that uses ElysiaJS,
enter `bun create elysia {destination}`.
The destination is used as the name of a directory to create
and can be `.` to create the project in the current directory.

To run the starter project, enter `bun run dev`.
Browsing localhost:3000 will display "Hello Elysia".

To define new routes in the project ...

See the example at {% aTargetBlank
"https://github.com/mvolkmann/bun-elysia-demo", "bun-elysia-demo" %}
GitHub repository.

## CLEANUP UP REMAINDER OF THIS CONTENT

Compare performance of bun, deno, and node on your rush-hour code. Implement your rush-hour program in Zig. Compare performance.

Create code examples that demonstrate the most important bun-specific APIs.

Does Bun share module code across projects like pnpm?

Concerns

- Were the Node APIs implemented to have identical characteristics with no bugs?

More

- Elysia is a bun-specific server framework which is 18 times faster than Express
  - `bun create elysia project-name` generates a scaffolded app
  - `cd project-name`
  - `bun run dev` provides hot reloading
  - browse localhost:3000
  - bunx prisma init —datasource-provider mysql
- Hono is a close competitor to Elysia
