---
eleventyNavigation:
  key: Bun
layout: topic-layout.njk
---

<figure style="width: 33%">
  <img alt="Bun logo" style="border: 0"
    src="/blog/assets/bun-logo.svg?v={{pkg.version}}">
</figure>

## Overview

"{% aTargetBlank "https://bun.sh", "Bun" %} is a fast JavaScript all-in-one toolkit."
It includes a JavaScript runtime, package manager, bundler, and test runner.
All of this is free and open source under the MIT license.

Bun can be used as a drop-in replacement for npm and Node.js.
The Bun runtime supports nearly all Node.js built-in modules (around 40 of them).
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
- Bun supports JSX/TSX files for generating HTML.
- Bun has built-in support for SQLite databases using the `bun:sqlite` module.
  It is 2 times faster than Deno and 4 times faster than Node.js
  in queries per second.
  Future support for MySQL and Postgres is planned.
- Bun has built-in support for Jest-compatible unit tests
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
- Bun has a plugin API that is similar to esbuild.
  Many esbuild plugins work in Bun without modification.

The Bun team makes the following additional performance claims:

- Bun is 4 times faster than Node.js for a "hello world" program.
- The time to build and run TS code in Bun is 4 times faster than esbuild,
  15 times faster than TSX, and 43 times faster than TSC + Node.
- `bun install` is 29 times faster than npm and 17 times faster than pnpm.
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

## Bun APIs

Bun provides bun-specific APIs that are alternatives to many npm modules.
These are highly optimized and perform much better than their Node.js equivalents.

`Bun.file` returns a `File` object with the same properties as
DOM `File` objects. The contents are lazily loaded.
It provides many `async` methods for reading content in different formats
including text, json, stream, and ArrayBuffer.
These read files up to 10 times faster than Node.js equivalents.

`Bun.write` writes many kinds of values including strings, buffers, files,
and responses from HTTP requests.
It writes 3 times faster than Node.js equivalents.

`Bun.serve` starts an HTTP server based on web APIs.
It handles 4 times as many requests per second as the Node.js equivalent.

`Bun.env` provides access to environment variables
without installing and using the `dotenv` package.

`Bun.spawn` and `Bun.spawnSync` enable spawning child processes.

`Bun.listen` and `Bun.connect` enable sending and receiving TCP requests.

Builtin Websocket support handles 5 times as many messages per second
as the Node.js equivalent and is easier to use.

Builtin password hashing using Bcrypt and Argon is provided.

Bun provides many more APIs!

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
console.log('Hello via Bun!');
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
The destination defaults to the current directory.
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
`~/.bun/install/cache`.
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

## Unit Tests

Bun has built-in support for implementing and running unit tests
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
import {expect, test} from 'bun:test';
// Can also import describe, beforeAll, beforeEach, afterAll, and afterEach.
import {add} from './math';

test('add', () => {
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

## Global Variables

Bun provides the following global variables.
For details, see {% aTargetBlank "https://bun.sh/docs/api/globals", "Globals" %}.

- Bun-specific

  - BuildMessage
  - Bun
  - ResolveMessage

  Add the following in `tsconfig.json` so VS Code
  doesn't complain about the `Bun` global variable.

  ```json
  {
    "compilerOptions": {
      ...
      "types": [
        "bun-types" // add Bun global
      ]
      ...
    }
  }
  ```

- Cloudflare

  - HTMLRewriter

- Cross-platform

  - globalThis

- Node.js

  - \_\_dirname
  - \_\_filename
  - Buffer
  - exports
  - global
  - module
  - process
  - require()

- Web
  - AbortController
  - AbortSignal
  - alert
  - Blob
  - ByteLengthQueuingStrategy
  - confirm
  - atob()
  - btoa()
  - clearImmediate()
  - clearInterval()
  - clearTimeout()
  - console
  - CountQueuingStrategy
  - Crypto
  - crypto
  - CryptoKey
  - CustomEvent
  - Event
  - EventTarget
  - fetch
  - FormData
  - Headers
  - JSON
  - MessageEvent
  - performance
  - prompt
  - queueMicrotask()
  - ReadableByteStreamController
  - ReadableStream
  - ReadableStreamDefaultController
  - ReadableStreamDefaultReader
  - reportError
  - Response
  - Request
  - setImmediate()
  - setInterval()
  - setTimeout()
  - ShadowRealm
  - SubtleCrypto
  - DOMException
  - TextDecoder
  - TextEncoder
  - TransformStream
  - TransformStreamDefaultController
  - URL
  - URLSearchParams
  - WebAssembly
  - WritableStream
  - WritableStreamDefaultController
  - WritableStreamDefaultWriter Web

## Importing Files

A text file can be imported as a string.

The following text file `haiku.txt` contains three lines.

```text
Out of memory.
We wish to hold the whole sky,
But we never will.
```

The following code demonstrates importing and using the text file above.

```ts
import haiku from './haiku.txt';
const lines = haiku.split('\n');
for (const line of lines) {
  const words = line.split(' ');
  console.log(`${words.length} words: ${line}`);
}
```

The output of this code is:

```text
3 words: Out of memory.
7 words: We wish to hold the whole sky,
4 words: But we never will.
```

A JSON or TOML file can be imported as JavaScript object.

The following text file `dogs.json` describes two dogs.

```json
[
  {"breed": "Whippet", "name": "Comet"},
  {"breed": "German Shorthaired Pointer", "name": "Oscar"}
]
```

The following code demonstrates importing and using the JSON file above.

```ts
import dogs from './dogs.json';
for (const dog of dogs) {
  console.log(`${dog.name} is a ${dog.breed}.`);
}
```

The output of this code is:

```text
Comet is a Whippet.
Oscar is a German Shorthaired Pointer.
```

## Bun Global Variable

The `Bun` global variable has many properties, most of which are functions.
Highlights include:

- `Bun.build`: compiles a collection of source files into a single source file
- `Bun.serve`: function that starts an HTTP server

## HTTP Server

Bun has built-in support for implementing HTTP servers
using the `Bun.serve` function.

To implement a basic HTTP server, create a Bun project and
replace the contents of `index.ts` with the following:

```js
const server = Bun.serve({
  port: 3000,
  fetch(req) {
    return new Response('Hi! This is my first Bun server!');
  },
  error(err) {
    // Handle the error.
    console.error(err);
    return new Response(err);
  },
  // optional to support HTTPS
  tls: {
    cert: Bun.file('./cert.pem'),
    key: Bun.file('./key.pem')
  }
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

Then start the server with `bun dev`.

To patch the code in place when code changes are detected
and not restart the server, use the `--hot` flag in place of `--watch`.
Technically this updates the internal module cache with the new code.

For more detail, see {% aTargetBlank "https://bun.sh/docs/runtime/hot",
"Watch mode" %}.

This will not refresh a web browser that is
displaying content obtained from a Bun server.
One way to do that is to use a WebSocket connection.

Add the following in the server code:

```js
import WebSocket from 'ws';
new WebSocket.Server({port: 1920});
```

Add the following in the client-side code:

```js
const ws = new WebSocket('ws://localhost:1920');
ws.addEventListener('close', event => {
  setTimeout(() => {
    window.location.reload();
  }, 500); // gives the server time to restart
});
```

The client will lose the WebSocket connect when the server restarts.
It will wait 500ms and then reload the browser.
By that time the server should be back up again.
Even if it isn't, the browser will wait for it.

## Environment Variables

Bun has built-in support for getting environment variable values
from the file `.env` in the project root directory.
For example, suppose this file contains `NAME=Mark`.
The value `Mark` can be obtained with `process.env.NAME` or `Bun.env.NAME`.

Environment variable values can reference
previously defined environment variables.

For example, suppose the `.env` file contains the following:

```text
NAME=Mark
GREETING="Hello, ${NAME}!"
```

The following test will pass:

```ts
import {expect, test} from 'bun:test';

test('environment variables', () => {
  expect(process.env.NAME).toBe('Mark');
  expect(Bun.env.GREETING).toBe('Hello, Mark!');
});
```

Changes to the `.env` after a server is started
are only available if the server is restarted.

## Executable Files

There are three ways to execute a JS/TS source file.

The first approach is to enter `bun {file-name}`.

The second way is to add the shebang line `#!/usr/bin/env bun`
at the beginning of the file and enter `./{file-name}`.

The third way is to compile the file with
`bun build ./{file-name} --outfile {exe-name} --compile`
and enter `./{exe-name}`.

## Executing Without Installing

The `npx` command can be used to execute a Node package
that may not already be installed.
For example, `npx cowsay Hello Cow!` outputs ASCII text
that depicts a cow with a speech bubble.

The same can be done with Bun using `bun x` (or `bunx`).
The main difference is that the Bun approach is much faster.

## import.meta

The global variable `import` has a `meta` property whose value is an object.
This object contains several properties and one method,
each of which are demonstrated in the following code.

```ts
import {expect, test} from 'bun:test';

test('import.meta', async () => {
  // import.meta.env is just an alias for process.env.
  expect(import.meta.env).toBe(process.env);

  // These get the directory and file name of the current file.
  const dir = import.meta.dir;
  const file = import.meta.file;
  expect(dir).toBe('/Users/volkmannm/Documents/dev/bun/bun-examples');
  expect(file).toBe('import-meta.test.ts');
  expect(import.meta.path).toBe(`${dir}/${file}`);

  // This indicates whether the current file is the one
  // that was directly executed by "bun run" or "bun test".
  expect(import.meta.main).toBe(true);

  // This finds the path of a file relative to the current file.
  const importName = 'dogs.json';
  const resolved = await import.meta.resolve('./' + importName);
  expect(resolved).toBe(`${dir}/${importName}`);
});
```

## Console

The global `console` object in Bun supports
all the same methods as in browsers and Node.js.

It can also be used as an `AsyncIterable` to read from stdin.
The following code demonstrates this.
It repeatedly prompts for text to be entered and
reports the number of words and characters.
Pressing return without entering any text ends the program.

```ts
// Using console.write avoids outputting a newline characters at the end.
const prompt = () => console.write('Enter text: ');

prompt();
for await (const line of console) {
  const trimmed = line.trim();
  if (trimmed.length === 0) {
    process.exit(0); // break does not exit a "for await" loop
  }
  const words = trimmed.split(' ');
  console.log(`words: ${words.length}, characters: ${line.length}`);
  prompt();
}
```

## Utilities

The `Bun` global variable refers to an object with many properties and methods
including:

| Name                                | Description                                                                |
| ----------------------------------- | -------------------------------------------------------------------------- |
| `deepEquals(obj1, obj2 [, strict])` | recursively compares two objects                                           |
| `deflateSync(buffer)`               | compresses using DEFLATE                                                   |
| `env`                               | alias for `process.env`                                                    |
| `escapeHTML(value)`                 | escapes built-in entities in HTML                                          |
| `fileURLToPath(url)`                | converts a `file://` URL string to an absolute path                        |
| `gunzipSync(buffer)`                | decompresses using GUNZIP                                                  |
| `gzipSync(buffer)`                  | compresses using GZIP                                                      |
| `inflateSync(buffer)`               | decompresses using INFLATE                                                 |
| `inspect.custom`                    | symbol that can be defined as a method to customize what `inspect` returns |
| `inspect(obj)`                      | serializes an object to a string like `console.log`                        |
| `main`                              | absolute path to entry point source file                                   |
| `nanoseconds()`                     | returns nanoseconds since this `bun` process started                       |
| `openInEditor(file)`                | opens a file in the default editor                                         |
| `pathToFileURL(path)`               | converts an absolute path to a `file://` URL string                        |
| `peek(promise)`                     | gets a promise result without `await` or `.then` when already settled      |
| `readableStreamTo*(stream)`         | set of functions that asynchronously consume a readable stream             |
| `resolveSync(path, root)`           | resolves a file path or module specifier                                   |
| `revision`                          | string containing git commit id of bun version                             |
| `sleep(ms)`                         | returns Promise that resolves after `ms` milliseconds                      |
| `sleepSync(ms)`                     | blocks for `ms` milliseconds                                               |
| `version`                           | string containing bun version                                              |
| `which(executableName)`             | returns absolute path to an executable                                     |

For more detail see {% aTargetBlank "https://bun.sh/docs/api/utils", "Utils" %}.

## JSX

Bun supports embedding JSX for HTML templating
in source files with `.jsx` or `.tsx` file extensions.

The `compilerOptions` property in `tsconfig.json`
determines how JSX is processed.
For example, the following settings enable using JSX with the Hono framework:

```json
{
  "compilerOptions": {
    ...
    "jsx": "react-jsx",
    "jsxImportSource": "hono/jsx"
    ...
  }
}
```

Depending on the JSX library used, calling `toString`
on a variable set to a JSX value returns an HTML string.
This works with the Hono framework.

Bun supports "prop punning" which is a shorthand syntax
for props that have the same name as an in-scope variable.
For example, the following are equivalent
when the variables `class` and `value` are in scope:

```xml
<input class={class} type="text" value={value} />
<input {class} type="text" {value} />
```

For more detail, see the Bun documentation on
{% aTargetBlank "https://bun.sh/docs/runtime/jsx", "JSX" %}.

## Serializing Objects

The `bun:jsc` module supports serializing and deserializing JavaScript values.
For example:

```ts
import {serialize, deserialize} from 'bun:jsc';
import {expect, test} from 'bun:test';

test('serialize', () => {
  const dogs = [
    {name: 'Comet', breed: 'Whippet'},
    {name: 'Oscar', breed: 'German Shorthaired Pointer'}
  ];
  const buffer = serialize(dogs); // a SharedArrayBuffer
  const newDogs = deserialize(buffer);
  expect(newDogs).toStrictEqual(dogs);
});
```

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

## Globbing

Bun provides a way to iterate over files that match a given glob pattern.

The following code lists all TypeScript source files
in and below the current directory,
excluding any found in `node_modules` directories.

```ts
import {Glob} from 'bun';

const glob = new Glob('**/*.ts');
for await (const file of glob.scan('.')) {
  // also see glob.scanSync
  if (file.startsWith('node_modules/')) continue;
  if (file.includes('/node_modules/')) continue;
  console.log(file);
}
```

For more detail see {% aTargetBlank "https://bun.sh/docs/api/glob", "Glob" %}.

## WebSockets

Bun has built-in support for WebSockets.

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

Bun has built-in support for SQLite databases.
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
in the file `sqlite.test.ts`:

```ts
import {Database} from 'bun:sqlite';
import {expect, test} from 'bun:test';

type Todo = {
  id: number;
  text: string;
  completed: number; // 0 or 1 for SQLite compatibility
};

const db = new Database('todos.db');
// Alternative way to access database in src directory:
// import {Statement} from 'bun:sqlite';
// import db from "./todos.db" with {"type": "sqlite"};
const deleteAllTodosPS = db.prepare('delete from todos');
const deleteTodoPS = db.prepare('delete from todos where id = ?');
const getTodoQuery = db.query('select * from todos where id = ?');
const getAllTodosQuery = db.query('select * from todos;');
const insertTodoQuery = db.query(
  'insert into todos (text, completed) values (?, 0) returning id'
);
const updateTodoPS = db.prepare('update todos set completed=? where id = ?');

test('sqlite', () => {
  deleteAllTodosPS.run();

  const text = 'buy milk';
  const {id} = insertTodoQuery.get(text) as {id: number};
  expect(id).toBeGreaterThan(0);

  let todos = getAllTodosQuery.all() as Todo[];
  expect(todos.length).toBe(1);
  let [todo] = todos;
  expect(todo.text).toBe(text);

  updateTodoPS.run(1, todo.id);

  todo = getTodoQuery.get(todo.id) as Todo;
  expect(todo.completed).toBe(1);

  deleteTodoPS.run(todo.id);

  todos = getAllTodosQuery.all() as Todo[];
  expect(todos.length).toBe(0);
});
```

Run this with `bun test sqlite.test.ts`.

## Foreign Function Interface (FFI)

The `bun:ffi` module supports calling functions implemented in
languages other than JavaScript that support the C ABI.
These include C, C++, Kotlin, Rust, Zig, and more.

The following Zig code defines a function that
computes the average of numbers in an array.
The array must be passed as a pointer to the first element and a length.

```zig
pub export fn average(numbers_ptr: [*]const f32, len: usize) f32 {
    var sum: f32 = 0.0;
    const numbers = numbers_ptr[0..len];
    for (numbers) |number| {
        sum += number;
    }
    const float_len: f32 = @floatFromInt(len);
    return sum / float_len;
}
```

To build this as a library, enter
`zig build-lib average.zig -dynamic -OReleaseFast`.

The following JavaScript code uses `bun:ffi` to call the Zig function.

```ts
import {dlopen, FFIType, ptr, suffix} from 'bun:ffi';

// Open a dynamic library.
const path = `libaverage.${suffix}`;
const lib = dlopen(path, {
  average: {
    args: [FFIType.ptr, FFIType.i32],
    returns: FFIType.f32
  }
});

// Get a reference to the average function.
const average = lib.symbols.average;

// Create and pass a typed array.
const numbers = new Float32Array([1, 2, 3, 4]);
const result = average(ptr(numbers), numbers.length);
console.log('average is', result);
```

To run this, enter `bun run index.ts`.
The expected output is "average is 2.5".

For more detail, see {% aTargetBlank "https://bun.sh/docs/api/ffi", "FFI" %}.
That page includes a list of all the supported FFI types.

## Bun Shell

"The {% aTargetBlank "https://bun.sh/blog/the-bun-shell", "Bun Shell" %}
is a new experimental embedded language and interpreter in Bun
that allows you to run cross-platform shell scripts in JavaScript & TypeScript."

The following code uses Bun Shell to
output the names of all the files in the current directory.

```ts
import {$} from 'bun';

const lines = $`ls .`.lines();
for await (const line of lines) {
  console.log(line);
}
```

## Elysia

{% aTargetBlank "https://elysiajs.com", "Elysia" %} is a
"TypeScript framework supercharged by Bun with End-to-End Type Safety,
unified type system and outstanding developer experience."

Elysia is a bun-specific server framework that is 18 times faster than Express.
{% aTargetBlank "https://hono.dev", "Hono" %} is a close competitor to Elysia.

To create a new Bun project that uses Elysia:

- `bun create elysia {destination}`

  The destination is used as the name of a directory to create
  and can be `.` to create the project in the current directory.

- `cd project-name`
- `bun dev` provides hot reloading
- browse localhost:3000

To define new routes in the project ...

See the example at {% aTargetBlank
"https://github.com/mvolkmann/bun-elysia-demo", "bun-elysia-demo" %}
GitHub repository.

When a URL path for hitting an Elysia server does not end in a file name,
it does not default to rendering the file "index.html".
To do that, end the URL with "/index.html".
See this {% aTargetBlank "https://github.com/elysiajs/elysia/issues/327",
"issue" %}.

For an alternative that runs in Bun and other JavaScript runtimes, see
<a href="/blog/topics/#/blog/hono/" target="_blank">Hono</a>.

## TODOs

- Compare performance of bun, deno, and node on your rush-hour code.
- Implement your rush-hour program in Zig and compare performance.
