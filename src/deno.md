---
eleventyNavigation:
  key: Deno
layout: topic-layout.njk
---

<img alt="Deno logo" style="width: 30%"
  src="/blog/assets/Deno.svg" title="Deno logo">

{% aTargetBlank "https://deno.land/", "Deno" %} is a
"secure runtime for JavaScript and TypeScript".
The name comes from moving the last two letters of "Node" to the front.
Originally it was pronounced "den O", but was later changed to "D no"
like the pet dinosaur in the Flintstones cartoon.

Ryan Dahl created both
{% aTargetBlank "https://nodejs.org/", "Node.js" %} and Deno.
In June 2018, he gave a talk at JSConf EU 2018 titled
"{% aTargetBlank "https://www.youtube.com/watch?v=M3BM9TB-8yA&vl=en",
"10 Things I Regret About Node.js" %}.
A prototype of Deno was presented during this talk.

Deno has builtin support for TypeScript.
It automatically compiles TypeScript code to JavaScript before running it,
making it unnecessary to define a build process to do this.

Both Node.js and Deno are built on the Chrome V8 JavaScript engine
which is implemented in C++.
Node.js is primarily implemented in C++.
Deno was originally implemented in Go and later changed to Rust.
It is also partially implemented in JavaScript.
To learn why Deno internals use JavaScript instead of TypeScript,
see this {% aTargetBlank
"https://docs.google.com/document/d/1_WvwHl7BXUPmoiSeD8G83JmS8ypsTPqed4Btkqkn_-4/preview",
"Design Doc" %}.

Event handling in Node.js is provided by the
{% aTargetBlank "https://libuv.org/", "libuv" %} C++ library.
In Deno this is provided by the
{% aTargetBlank "https://tokio.rs/", "Tokio" %} Rust library.

Version 1.0 of Deno was released on May 13, 2020.
It is open source and uses the MIT license.

Deno is secure by default.
The environment, file system, and network
can only be accessed if explicitly enabled.

Deno supports WebAssembly (WASM).
Many programming languages can be compiled to WASM, including
{% aTargetBlank "https://emscripten.org/", "C" %}, C++, C#,
Go, Java, Kotlin,
{% aTargetBlank "https://github.com/iodide-project/pyodide", "Python" %},
Prolog, Ruby, {% aTargetBlank "https://rustwasm.github.io/", "Rust" %},
Scheme, Swift, and others.
The resulting WASM code can then be called from JavaScript or TypeScript code.
TODO: Try this with Python!

To get started with Deno and find documentation, see the
{% aTargetBlank "https://deno.land/manual", "Deno Manual" %}.

## Differences from Node.js

1. Deno uses ES Modules by default.
   Node.js can use ES Modules, but defaults to CommonJS.
1. Deno uses URLs for loading remote dependencies rather than
   relative paths in the `node_modules` directory.
1. Deno uses built-in resource fetching
   rather than relying on npm to install packages.
1. Deno has built-in TypeScript support with caching of compile source files.
1. Deno has built-in support for web standards
   like the Fetch API and functions from the `window` object.
1. Deno requires file system and network access to be explicitly enabled.
1. Deno APIs utilize Promises, ES6, and TypeScript features.
1. Deno provides a large standard library with no external dependencies.
1. Deno uses message passing channels to invoke privileged system APIs.

## Pros

## Cons

## Installing

To install Deno:

- for Windows, use Chocolately: `choco install deno`
- for macOS, use Homebrew: `brew install deno`
- for Linux, use curl: `curl -fsSL https://deno.land/x/install/install.sh | sh`

For more install options, see the main Deno website.

## Options

To get help on `deno` options, enter `deno --help`.

To get help on a subcommand such as "run", enter `deno run --help`.

To get the version of `deno` that is installed, enter `deno -V`.
To also get the versions of V8 and TypeScript that `deno` uses,
enter `deno --version`.

## Conventions

The `.js` and `.ts` files in the standard library
use underscores rather than dashes to separate multiple words in file names.

## Permission Options

Deno supports the following permission options:

- `-A` or `--allow-all`  
  This allow all permissions, disabling all security.
- `--allow-env`  
  This allows getting and setting environment variables.
- `--allow-hrtime`  
  This allows use of high-resolution time measurement
  which can be used in timing attacks and fingerprinting.
- `--allow-net=<allow-net>`  
  This allows network access.
  A comma-separated list of domains can optionally be specified
  to restrict network access to only those domains.
- `--allow-plugin`  
  This allows plugins to be loaded.
  It is currently an unstable feature.
- `--allow-read=<allow-read>`  
  This allows read access to the file system.
  A comma-separated list of directories or files can optionally be specified
  to restrict reads to only those directories and files.
- `--allow-run`  
  This allow running subprocesses outside of the Deno sandbox,
  which means they do not have security restrictions.
- `--allow-write=<allow-write>`  
  This allows write access to the file system.
  A comma-separated list of directories or files can optionally be specified
  to restrict writes to only those directories and files.

## Command Summary

Deno commands start with `deno` followed by a command name and options.
Here's a summary of the commands. Details will be provided later.

| Command       | Description                                                                  |
| ------------- | ---------------------------------------------------------------------------- |
| `bundle`      | bundles a module and its dependencies into a single file                     |
| `cache`       | caches dependencies                                                          |
| `completions` | generates shell completions                                                  |
| `doc`         | displays documentation for a given module                                    |
| `eval`        | evaluates a string of JS/TS code                                             |
| `fmt`         | formats source files                                                         |
| `help`        | displays help on `deno` commands and environment variables                   |
| `info`        | displays information about the cache or source files, including dependencies |
| `install`     | installs a script as an executable                                           |
| `lint`        | identifies issues in source files                                            |
| `repl`        | starts a Read Eval Print Loop                                                |
| `run`         | runs a script from a file or URL                                             |
| `test`        | runs tests                                                                   |
| `types`       | displays information about built-in types                                    |
| `upgrade`     | upgrades `deno`                                                              |

## Running

To run a Deno program, enter a command like the following:

```bash
deno run src/main.ts
```

Deno also supports running a script at a URL.
For example, the following outputs a welcome message
including a dinosaur unicode character:

```bash
deno run https://deno.land/std/examples/welcome.ts
```

The following command runs a simple HTTP file server
that serves all files in and below the current directory
on port 1234.

```bash
deno run --allow-net --allow-read \
  https://deno.land/std/http/file_server.ts --port 1234
```

Browsing localhost:1234 serves the file `index.html` in the current directory.

To get help on command line options for `file_server.ts`,
enter the following:

```bash
deno run --allow-net --allow-read https://deno.land/std/http/file_server.ts --help
```

## Watching

To run in watch mode so the script is restarted
when any source file in and below the current directory changes,
add the currently unstable `--watch` option.
For example:

```bash
deno run --allow-net --unstable --watch my_server.ts
```

When a file change is detected, the program is restarted,
but no output is generated to indicate that this happened.

Another more full-featured option for watching files
that is similar to {% aTargetBlank "https://nodemon.io/", "nodemon" %} is
{% aTargetBlank "https://github.com/denosaurs/denon", "denon" %}.

To install denon:

- enter `deno install -Af --unstable https://deno.land/x/denon/denon.ts`
- add `$HOME/.deno/bin` to your `PATH` environment variable

Use the `denon` command in place of the `deno` command
with the same options and arguments. For example:

```bash
denon run --allow-net my_server.ts
```

## VS Code

The "Deno" VS Code extension provides:

- intellisense
- import URL completion
- diagnostics with quick fixes
- optional use of `deno fmt` for code formatting
- TypeScript type definitions

After installing, add the following to the user settings JSON:

```json
"deno.enable": true,
"deno.import_intellisense_origins": {
  "https://deno.land": true,
  "https://deno.land/x": true
},
"deno.lint": true,
"deno.unstable": true,
```

Enabling `deno.unstable` is required to use `deno lint`.

To use `deno fmt` for formatting TypeScript files,
regardless of whether they will be run by Deno, add the following:

```json
"[typescript]": {
  "editor.defaultFormatter": "denoland.vscode-deno",
},
```

## Linting

To lint all `.js` and `.ts` files
in and below the current directory, enter `deno lint --unstable`.

To see a list of the rules checked by `deno lint`,
enter `deno lint --rules --unstable`.

To prevent the linter from running on a file,
add the following comment at the top:

```js
// deno-lint-ignore-file
```

To prevent the linter from checking specific rules on a file,
add the following comment at the top:

```js
// deno-lint-ignore-file rule1 rule2 rule3
```

To prevent the linter from checking specific rules on a specific line,
add the following comment before the line:

```js
// deno-lint-ignore rule1 rule2 rule3
```

## Code Formatting

To format all `.js`, `.json`, and `.ts` files
in and below the current directory, enter `deno fmt`.
This uses `dprint` (implemented in Rust) rather than Prettier.
Two noticeable changes it makes are to adds semicolons where missing
and change single quotes to double.

To prevent formatting a statement,
precede it with the comment `// deno-fmt-ignore`.
To prevent formatting an entire file,
add the comment `// deno-fmt-ignore-file` at the top.

There is discussion about making the formatting configurable,
but it seems to not currently be configurable.

If the formatting performed by `deno fmt` is not to your liking,
Prettier can be used instead.

## Built-in Functions

For documentation on the built-in functions, see the
{% aTargetBlank "https://doc.deno.land/builtin/stable", "Runtime API" %}.

## Standard Library

The Deno {% aTargetBlank "https://deno.land/std", "Standard Library" %}
is modeled after that of the Go programming language.
It is maintained by the Deno team.
These libraries typically contain a `mod.ts` file
that defines what is exported.

For example, the `fs` library provides the following functions:

- `copy` copies a file or directory
- `exists` tests whether a file or directory exists
- `move` moves a file or directory
- `walk` recursively walks the files in a directory
- and more

The `fmt` library provides the following functions:

- `printf` prints formatted output
- functions to output text with a specific style including
  `bold`, `dim`, `hidden`, `inverse`, `italic`, `strikethrough`, and `underline`
- functions to output text in a color including
  `black`, `blue`, `cyan`, `gray`, `green`,
  `magenta`, `red`, `white`, `yellow`, and `rgb8` (for custom colors)
- `bg` versions of the color functions to set the background color
- `bright` versions of the color functions
- `bgBright` versions of the color functions to set a bright background color

To write to stdout with colors, similar to the npm package "chalk":

```js
import {
  bgYellow,
  blue,
  green,
  italic,
  red,
  underline
} from 'https://deno.land/std/fmt/colors.ts';
console.log(
  bgYellow(red('one')),
  italic(green('two')),
  underline(blue('three'))
);
```

A version can optionally be specified in URLs. For example:

```js
import {blue, green, red} from 'https://deno.land/std@0.79.0/fmt/colors.ts';
```

The `io` library provides functions for input/output.
For example, to read from a file one line at a time:

```js
import {readLines} from 'https://deno.land/std/io/mod.ts';
import * as path from 'https://deno.land/std/path/mod.ts';

const filename = path.join(Deno.cwd(), 'my-file.txt');
const reader = await Deno.open(filename);
for await (const line of readLines(reader)) {
  console.log(line);
}
```

## Imports

Deno source files import other files using the `import` statement,
not the `require` function like in Node.js.
The file to import can be specified with either a relative file path or a URL.
TODO: Can it be an absolute file path?
In both cases a file, not just a directory, must be specified.
Unlike in Node.js, the filename `index.js` is not treated as
the default filename and a file extension must be included.

Deno does not use npm to install modules.
Instead, `import` statements can specify module URLs
that reference network resources such as those in GitHub.

Imported files are cached by saving them in the directory
specified in the `DENO_DIR` environment variable
rather than in a `node_modules` directory.
If this environment variable is not set,
the files are saved in an OS-specific location.

- Windows: `%LOCALAPPDATA%/deno`
- macOS: `\$HOME/Library/Caches/deno`
- Linux: `\$XDG_CACHE_HOME/deno` or `\$HOME/.cache/deno`

Downloaded dependencies are placed in the `deps` subdirectory.
JavaScript files created by compiling TypeScript files
are placed in the `gen` subdirectory.
The `gen` directory contains
a `file` directory for `.js` files
generated from `.ts` files in the file system and
a `https` directory for `.js` files
generated from downloaded `.ts` files.

Dependencies are not cataloged in a `package.json` file,
which is not used at all.
TODO: How do you script common actions?

Spreading library URLs throughout the source files of a project
can be undesirable, especially of some are repeated in multiple files.
An alternate approach is to create a file like `src/deps.ts` (recommended name)
containing lines like the following:

```ts
export {name1, name2, name3} from 'https://domain1/pkg1@v1/filename1.ts';
export {name4, name5} from 'https://domain2/pkg2@v2/filename2.ts';
```

Then import from this file with lines like the following:

```ts
import {name1, name3, name4} from './deps.ts';
```

TODO: Isn't it odd to then have every source file only import from this one file?

It is useful to record the version of each dependency being used
so the same versions can be downloaded by multiple developers
and in CI/CD environments.
A lock file does just this.
Assuming all dependencies are exported from the file `src/deps.ts`,
enter the following command to create a lock file named `lock.json`.

```bash
deno cache --lock=lock.json --lock-write src/deps.ts
```

Add the lock file to version control.

To install all the dependency versions specified in a lock file,
enter the following command:

```bash
deno cache --reload --lock=lock.json src/deps.ts
```

The cached files continue to be used
without checking whether dependencies have been updated.
To update the cached files to their latest versions,
enter `deno cache --reload`.

To update a single module to a specific version,
enter a command like the following:

```bash
deno cache --reload=https://some-domain/some-module@version main.ts
```

After doing this, the lock file can be updated by
entering the command for creating it that was described earlier.

To run a Deno program using only cached modules,
preventing any new downloads, enter a command like the following:

```bash
deno run --lock=lock.json --cached-only main.ts
```

This will fail if any source file imports a module with a URL
that has not previously been cached.

## deno.land

The site `https://deno.land/x` hosts a curated collection of
third-party modules that work with Deno.
As of 11/23/2020 it contained 1,302 modules.
Examples include lodash, date_fns, ramda, xstate, ky (HTTP client), and
i18next (internationalization framework).

## Deno Global Variable

When a script is run with `deno run`,
the `Deno` global variable is made available.
It has a large number of properties, most of which are functions.
Examples include:

args - array of command-line arguments
Buffer
chdir
chmod
chmodSync
chown
chownSync
close
connect
copy
copyFile
copyFileSync
create
createSync
cwd
execPath
exit
File
inspect
iter
iterSync
listen
mainModule
mkdir
mkdirSync
open
openSync
pid - process id
ppid - parent process id
read
readAll
readAllSync
readDir
readDirSync
readFile
readFileSync
readLink
readLinkSync
readSync
readTextFile
readTextFileSync
realPath
realPathSync
remove
removeSync
rename
renameSync
run
stderr
stdin
stdout
test
watchFs
write
writeAll
writeAllSync
writeFile
writeFileSync
writeSync
writeTextFile
writeTextFileSync

build - an object with `arch`, `env`, `os`, `target`, and `vendor` properties

env - an object with the functions `delete`, `get`, `set`, and `toObject`

errors - an object whose properties are
the following error constructor functions:

- AddrInUse
- AddrNotAvailable
- AlreadyExists
- BadResource
- BrokenPipe
- Busy
- ConnectionAborted
- ConnectionRefused
- ConnectionReset
- Http
- Interrupted
- InvalidData
- NotConnected
- NotFound
- NotSupported
- PermissionDenied
- TimedOut
- UnexpectedEof
- WriteZero

version - an object with the properties `deno`, `typescript`, and `v8`
whose values are all version numbers

To write and read text files:

```ts
const text = 'This is a story\nbout a man named Jed';
await Deno.writeTextFile('demo.txt', text);
const result = await Deno.readTextFile('demo.txt');
```

## Parsing command line arguments

See the {% aTargetBlank "https://deno.land/x/yargs@v16.1.1-deno", "Yargs" %}
library.

## Basic HTTP server

```js
import {serve} from 'https://deno.land/std/http/server.ts';

const port = 6543;
const server = serve({port});
console.log('listening on port', port);

for await (const req of server) {
  //console.log('req is', req)
  console.log('got request for', req.method, req.url);
  req.respond({body: 'Hello World\n'});
}
```

## Implementing a REST Server

There are many HTTP server libraries for Deno.
Currently the most popular is
{% aTargetBlank "https://oakserver.github.io/oak/", "oak" %}.
It is inspired by the Koa package for Node.js.

Here is an example of implementing a REST server in oak
that supports CRUD operations on dog objects.

```js
import {Application, Router} from 'https://deno.land/x/oak/mod.ts';
import {v4} from 'https://deno.land/std/uuid/mod.ts';

const PORT = 1234;
const dogs = {};

async function createDog(context) {
  const body = await context.request.body();
  const {breed, name} = await body.value;
  const id = v4.generate();
  dogs[id] = {id, breed, name};

  context.response.body = id;
  context.response.status = 201;
}

async function deleteAllDogs(context) {
  dogs = {};
}

async function deleteDog(context) {
  const {id} = context.params;
  if (dogs[id]) {
    delete dogs[id];
    context.response.status = 200; // Why necessary?
  } else {
    context.response.status = 404;
  }
}

async function getAllDogs(context) {
  context.response.body = JSON.stringify(dogs);
}

async function getDog(context) {
  const {id} = context.params;
  const dog = dogs[id];
  if (dog) {
    context.response.body = JSON.stringify(dog);
  } else {
    context.response.status = 404;
  }
}

async function updateDog(context) {
  const {id} = context.params;
  const dog = dogs[id];
  if (dog) {
    const body = await context.request.body();
    dogs[id] = await body.value;
  } else {
    console.log('server.js updateDog: not found');
    context.response.status = 404;
  }
}

const router = new Router();
router
  .delete('/dog', deleteAllDogs)
  .delete('/dog/:id', deleteDog)
  .get('/dog', getAllDogs)
  .get('/dog/:id', getDog)
  .post('/dog', createDog)
  .put('/dog/:id', updateDog);
// patch is also supported.

const app = new Application();

// Request logging
app.use(async (ctx, next) => {
  await next();
  console.log(`${ctx.request.method} ${ctx.request.url}`);
});

app.use(router.routes());
app.use(router.allowedMethods());
app.addEventListener('listen', () => {
  console.log('listening on port', PORT);
});
await app.listen({port: PORT});
```

Here is client code that sends requests to this REST server.

```js
const URL_PREFIX = 'http://localhost:1234/dog';

function addDog(dog) {
  const headers = {'Content-Type': 'application/json'};
  const body = JSON.stringify(dog);
  return fetch(URL_PREFIX, {method: 'POST', headers, body});
}

function deleteAllDogs() {
  return fetch(URL_PREFIX, {method: 'DELETE'});
}

function deleteDog(id) {
  return fetch(URL_PREFIX + '/' + id, {method: 'DELETE'});
}

async function getAllDogs() {
  const res = await fetch(URL_PREFIX);
  return res.json();
}

async function getDogById(id) {
  const res = await fetch(URL_PREFIX + '/' + id);
  return res.json();
}

function getDogByName(dogs, name) {
  return Object.values(dogs).find(dog => dog.name === name);
}

function updateDog(dog) {
  const headers = {'Content-Type': 'application/json'};
  const body = JSON.stringify(dog);
  return fetch(URL_PREFIX + '/' + dog.id, {method: 'PUT', headers, body});
}

await deleteAllDogs();
await addDog({name: 'Maisey', breed: 'Treeing Walker Coonhound'});
await addDog({name: 'Ramsay', breed: 'Native American Indian Dog'});
await addDog({name: 'Oscar', breed: 'German Shorthaired Pointer'});
await addDog({name: 'Comet', breed: 'Whippet'});
await addDog({name: 'Snoopy', breed: 'Beagle'});

const res = await fetch(URL_PREFIX);
let dogs = await getAllDogs();
console.log('dogs =', Object.values(dogs));

const oscar = getDogByName(dogs, 'Oscar');
console.log('client.js x: oscar =', oscar);
oscar.name = 'Oscar Wilde';
await updateDog(oscar);
const res2 = await deleteDog(getDogByName(dogs, 'Snoopy').id);
console.log('client.js x: res2 =', res2);

dogs = await getAllDogs();
console.log('after delete, dogs =', Object.values(dogs));

console.log('client.js x: oscar =', await getDogById(oscar.id));
```
