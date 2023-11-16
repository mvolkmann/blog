---
eleventyNavigation:
  key: Bun
layout: topic-layout.njk
---

## Overview

"{% aTargetBlank "https://bun.sh", "Bun" %} is a fast JavaScript all-in-one toolkit."
It includes a JavaScript runtime, package manager, bundler, and test runner.
Bun is free and open source.

Development of Bun started in 2022 by Jarred Sumner with VC funding.
Bun became stable (1.0) in September 2023.

Bun was created by the startup company
{% aTargetBlank "https://oven.sh/", "Oven.sh" %}
because that’s what buns come out of.
Oven will donate Zig to the Zig Software Foundation to keep the project going.

Bun is built on JavaScriptCore from Safari
which has faster startup times than V8.
Bun supports TypeScript out of the box.

Bun is implemented in Zig and some C++.
"Zig is sort of similar to writing C, but with better memory safety features
in debug mode and modern features like defer (sort of similar to Go's)
and arbitrary code can be executed at compile-time via comptime.
It's really good for writing performant low-level software."
"It has very few keywords so it's a lot easier to learn than,
for example, C++ or Rust."

Bun works with nearly all major web frameworks including
Next.js, SvelteKit, Astro, Nuxt, and Fastify.

The Bun package manager works with npm packages.

The name "Bun" came from a friend who suggested it
because she has a bunny named "Bun".
The initial reaction was "I'm not going to name it after your bunny.
And then I thought about it more and it made some sense."
It also makes sense because
"it’s a bundling of the JavaScript ecosystem and a bundler".

## Installing

To install Bun for macOS, Linux, or Windows Subsystem for Linux (WSL),
enter `curl -fsSL https://bun.sh/install | bash`.
This adds the `bun` executable.

To upgrade to the latest version, enter `bun upgrade`.

"Bun provides a limited, experimental native build for Windows.
At the moment, only the Bun runtime is supported."

## REPL

To start a Read-Evaluate-Print-Loop, enter `bun repl`.
This takes a few seconds to install the first time it is run,
but is much faster in subsequent runs.

## Projects

To create a new Bun project, create a directory, cd to it, and enter `bun init`.
This creates the files:

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

## Help

To see all the subcommands supported by the `bun` command, enter `bun`.
The subcommands include `init`, `run`, `test`, `repl`, `upgrade`, and more.

To see all the options supported by the `bun` command, enter `bun --help`.

To see a list of all scripts defined in `package.json`, enter `bun.run`.

## Unit Tests

Bun has builtin support for implementing and running unit tests.

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

To automatically rerun tests when code changes are saved,
enter `bun --watch test`.

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

Now the server can be started by entering `bun start`.

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

## CLEANUP UP REMAINDER OF THIS CONTENT

These notes are currently in a very rough form!

Compare performance of bun, deno, and node on your rush-hour code. Implement your rush-hour program in Zig. Compare performance.

Is the only plan for profit to provide hosting?
“Oven has decided to adopt a strategy twinned with Deno Company. They will launch sales of a cloud architecture based on Bun.”

Create code examples that demonstrate the most important bun-specific APIs.

Does it share module code across projects like pnpm?

Why?

- simplifies current state of JS/TS tooling (transpilers, bundlers, testing tools, …); less to install and configure; easier to use
- provides significantly better performance
- supports both CommonJS (require) and ESM (import) in the same source files

Features

- The bun runtime replaces Node.js and supports nearly all Node.js builtin modules (around 40 of them; rarely used modules such as dgram, http2, inspector, and repl are not yet supported; v8 module is specific to Webkit which Bun does not use)
- Supports JSX/TSX files.
- hot reloading with “bun —hot {file} (watch mode equivalent of nodemon?)
- has a plugin API that is similar to that of esbuild, so many esbuild plugins work in bun without modification
- bun:test a Jest-compatible test runner; 8 times faster than Vitest and 13 times faster than Jest
- SQLite database built into the bun runtime; see bun:sqlite; 2 times faster than Deno and 4 times faster than Node in queries per second
- bun-specific, highly-optimized APIs that perform better than Node.js equivalents
  - Bun.file returns a File object with the same properties as DOM File objects; contents are lazily loaded; many async methods for reading content in different formats including text, json, stream, and ArrayBuffer; reads files up to 10 times faster
  - Bun.write to write many kinds of values including strings, buffers, files, and responses from HTTP requests; writes 3 times faster than Node.js
  - Bun.serve to start an HTTP server based on web APIs; handles 4 times as many requests per second as Node.js
  - Bun.env provides access to environment variables without using the dotenv package
  - Websocket support handles 5 times as many messages per second as Node.js and is easier to use
  - password hashing (uses Bcrypt and Argon)
  - many more APIs!

Performance

- 4 times faster than Node.js for a hello world program
- time to build and run TS code is 4 times faster than esbuild, 15 times faster than TSX, and 43 times faster than TSC + Node
- “bun run” can be used in place of “npm run” and is 5 times faster
- “bun install” is 29 times faster than npm and 17 times faster than pnpm; uses package.json files just like npm; uses a binary bun.lockb file instead of package-lock. json; can be used as a replacement for npm even when not using the bun runtime
- “bunx” is the equivalent of “npx”

Concerns

- Was it a good idea to build with Zig, a new, not yet popular language?
- We’re the Node APIs implemented to have identical characteristics with no bugs?

More

- Elysia is a bun-specific server framework which is 18 times faster than Express
  - `bun create elysia project-name` generates a scaffolded app
  - `cd project-name`
  - `bun run dev` provides hot reloading
  - browse localhost:3000
  - bunx prisma init —datasource-provider mysql
- Hono is a close competitor to Elysia
