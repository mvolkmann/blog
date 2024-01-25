---
eleventyNavigation:
  key: Deno
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

<img alt="Deno logo" style="width: 30%"
  src="/blog/assets/deno-logo.svg?v={{pkg.version}}"
  title="Deno logo">

## Overview

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

Version 1.0 of Deno was released on May 13, 2020.
It is open source and uses the MIT license.

The Deno core team, in order of commits, consists of
Ryan Dahl (Google),
Bartek Iwańczuk
Bert Belder (StrongLoop),
Kitson Kelly (Thoughtworks), and
Luca Casonato.

Deno is secure by default.
The environment, file system, and network
can only be accessed if explicitly enabled.
Compare this to other programming languages/environments
like Node.js, Python, and Java where there are
no restrictions on what application or library code can do.
All of those are insecure by default.

I believe the Deno security model is its most significant feature!
With Deno you never have to worry that some library you are using,
or a library used by a library,
might do something bad that you didn't want to allow.
You can run an app with no permissions and see what errors are generated.
For example, suppose you see the following error message:

```text
error: Uncaught PermissionDenied: read access to "pets.db",
run again with the --allow-read flag
```

You can then decide whether you app should be reading that file
before running it again with permission granted.
The permission flags are covered in more detail later.

Deno has built-in support for TypeScript.
It automatically compiles TypeScript code to JavaScript before running it,
making it unnecessary to define a build process to do this.

The `deno` command supports many sub-commands for common tasks
including running programs, starting a REPL, linting code, formatting code,
displaying code documentation, getting information on dependencies,
and bundling code.

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
The {% aTargetBlank "https://discord.gg/deno", "Deno Discord channel" %}
is another great resource.

## Differences from Node.js

1. Deno uses ES modules by default.
   Node.js can use ES modules, but defaults to CommonJS.
1. Deno uses URLs for loading remote dependencies rather than
   relative paths in the `node_modules` directory.
   These can come from any URL, not just from npm.
1. Deno uses built-in resource fetching
   rather than relying on npm to install packages.
1. Deno has built-in TypeScript support with caching of compiled source files.
1. Deno has built-in support for Web APIs
   like the Fetch API and functions from the `window` object.
1. Deno requires file system and network access to be explicitly enabled.
1. Deno APIs utilize Promises, ES6, and TypeScript features.
1. All asynchronous actions in Deno return a Promise rather than use callbacks.
1. Deno provides a large standard library with no external dependencies.
1. Deno uses message passing channels to invoke privileged system APIs.
1. Deno programs stop when an uncaught error is encountered,
   such as failing to catch a rejected promise.

## Pros

- Deno has unique support for building secure applications
  that are limited in their ability to read/write files
  and access network resources.
- Deno has built-in support for TypeScript which makes using it easier
  because it removes the need to install the TypeScript compiler
  and configure a build process.
- Deno supports modern JavaScript features that are not yet
  implemented in other JavaScript environments.
  Examples include the {% aTargetBlank
  "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining",
  "optional chaining operator" %} (`?.`),
  the {% aTargetBlank
  "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing_operator",
  "nullish coalescing operator" %} (`??`),
  and {% aTargetBlank "https://github.com/tc39/proposal-top-level-await",
  "top-level `await`" %}.
- Deno includes many development tools to support common activities such as
  linting, code formatting, running tests, and bundling code into a single file.
  This removes the need to select, install, and configure tools for these.

## Cons

- The number of libraries that are compatible with Deno
  is far less that the number of Node.js libraries.
  Many Node.js libraries cannot be used directly by Deno
  because that use APIs that Deno cannot make secure.
- Knowledge of existing Node.js libraries often doesn't help
  when implementing an application in Deno and
  time must be spent finding and learning how to use an alternate library.
- Deno is currently slower than Node.js for some tasks.

## Installing `deno`

Deno is installed as a single executable file.
To install it:

- for Windows, use Chocolately: `choco install deno`
- for macOS, use Homebrew: `brew install deno`
- for Linux, use curl: `curl -fsSL https://deno.land/x/install/install.sh | sh`

For more install options, see the
{% aTargetBlank "https://deno.land/", "main Deno website" %}.

To update to the latest version, enter `deno upgrade`.

## Options

To get help on `deno` options, enter `deno help`, `deno --help`, or `deno -h`.
This includes the Deno version, the Deno manual URL, third party module URLs,
the issue tracker URL, basic command examples, basic options,
a brief description of each subcommand, and
a description of environment variables that affect Deno.

To get help on a subcommand such as "run", enter `deno run --help`.

To get the version of `deno` that is installed, enter `deno -V`.
To also get the versions of V8 and TypeScript that `deno` uses,
enter `deno --version`.

## Conventions

The `.js` and `.ts` files in the standard library
use underscores rather than dashes to separate multiple words in file names.

## Permission Flags

Deno supports the following permission flags:

- `-A` or `--allow-all`  
  This allow all permissions, disabling all security.
- `--allow-env`  
  This allows getting and setting environment variables.
- `--allow-hrtime`  
  This allows use of high-resolution time measurement
  which can be used in timing attacks and fingerprinting.
  Without this permission, functions that return times
  such as `performance.now` return values in milliseconds
  instead of a more precise value.
- `--allow-net=<allow-net>`  
  This allows network access.
  A comma-separated list of domains can optionally be specified
  to restrict network access to only those domains.
  For example, `--allow-net=bitbucket.org,github.com,gitlab.com`.
- `--allow-plugin`  
  This allows plugins implemented as Rust binaries to be loaded.
  JavaScript code can then invoke plugin code using OPs.
  For an example, see {% aTargetBlank
  "https://denolib.gitbook.io/guide/codebase-basics/example-adding-a-deno-api",
  "here" %}.
  Keep in mind that plugins are not limited by
  the normal Deno security restrictions.
  Plugins are currently an unstable feature.
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

What other programming language / runtime can make
the security guarantees that Deno makes?
I don't know of any.

A Deno program can check for permission flags and take different actions
based on whether specific flags and values are present.
For example:

```js
const status = await Deno.permissions.query({name: 'write', path: '/tmp'});
if (status.state === 'granted') {
  console.log('Permission was granted on the command line.');
} else {
  const status = await Deno.permissions.request({name: 'write', path: '/tmp'});
  if (status.state === 'granted') {
    console.log('Permission was granted interactively.');
  } else {
    console.log('Permission was denied interactively.');
  }
}
```

The permissions API is currently experimental,
so this must be run with the `--unstable` flag.

If the program is not also run with `--allow-write=/tmp`,
the user will be prompted with the following and
the program will wait for the user to press the "g" or "d" key:

```text
⚠️ Deno requests write access to "/tmp". Grant? [g/d (g = grant, d = deny)]
```

The `Deno.permissions.revoke` function removes a permission.
This is useful when actions that require the permission have completed
and you wish to ensure that those actions cannot be performed again
for the remainder of the program.

## Example Script

Here's an example Deno script to whet your appetite.
It prompts for a U.S. ZIP code and uses the
{% aTargetBlank "https://openweathermap.org/api", "OpenWeather API" %}
to get the city name, current temperature, and current wind speed.
It was inspired by a
{% aTargetBlank "https://www.youtube.com/watch?v=3MapKMsioTo", "talk" %}
given by Luca Casonato at an ING Tech Meetup on November 5, 2020.

```ts
import {bold, cyan} from 'https://deno.land/std@0.79.0/fmt/colors.ts';

function print(label: string, value: string): void {
  console.log(bold(cyan(label + ':')), value);
}

try {
  // Web browsers implement the "prompt" function
  // to prompt a user for input using a modal dialog.
  // Deno implements this to prompt in a terminal.
  const zipCode = prompt('U.S. ZIP code:');
  if (!zipCode) {
    console.error('A ZIP code must be entered.');
    Deno.exit(1); // exits program with a status code
  }

  // Deno supports top level await.
  const secrets = await Deno.readTextFile('secrets.json');
  const {API_KEY} = JSON.parse(secrets);
  const url =
    'https://api.openweathermap.org/data/2.5/weather' +
    `?zip=${zipCode}&units=imperial&appid=${API_KEY}`;

  const res = await fetch(url);
  const json = await res.json();
  // This API includes JSON error messages in the response body.
  if (!res.ok) throw new Error(json.message);

  print('\nCity', json.name);
  print('Temperature', json.main.temp + '°F');
  print('Wind speed', json.wind.speed + ' mph');
} catch (e) {
  // console methods accept a subset of CSS styling.
  console.error('%c' + e.message, 'color: red');
}
```

To check the code for issues, enter `deno lint`.

To format the code, enter `deno fmt`.

Assuming Deno is installed and you have the file
`secrets.json` that contains your OpenWeather API key,
the following command runs the script.
Note the permission flags that are required.
They only allow reading from a specific file
and sending network requests to a specific domain.

```bash
deno run --allow-read=secrets.json --allow-net=api.openweathermap.org weather.ts
```

Here is a sample session, minus the colors:

```text
U.S. ZIP code: 12345

City: Schenectady
Temperature: 40.64°F
Wind speed: 11.41 mph
```

Repeatedly typing that long command to run this would be tedious.
We could write a shell script to run it, but Deno can do that for us.
Just recall the command, change the "run" subcommand to "install",
and execute it.
This writes a shell script to `$HOME/.deno/bin`
with the same name as your source file.
It will contain the `deno run` command with all its options.
To give the script a different name,
specify the name using the `--name` (or `-n`) flag.
As long as that "bin" directory is in your `PATH`,
it can now be executed by just entering the script name.

## Command Summary

Deno commands start with `deno` followed by a command name and options.
Here's a summary of the commands. Details will be provided later.

| Command       | Description                                                                  |
| ------------- | ---------------------------------------------------------------------------- |
| `bundle`      | bundles a module and its dependencies into a single file                     |
| `cache`       | caches dependencies                                                          |
| `compile`     | creates a standalone executable                                              |
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
| `types`       | displays information about built-in types; can use in your code              |
| `upgrade`     | upgrades `deno`                                                              |

Some commands and flags are considered experimental
and may change in the future.
It in order to use those, the `--unstable` flag
must be included until they become stable.

For an example of using the `install` command, see the "Watching" section.

## REPLs

A Read Eval Print Loop (REPL) tool is great for
learning about the features of a programming language or runtime.
The command `deno repl` or just `deno` starts a terminal-based REPL.
It supports tab completion and syntax highlighting.

There are also web-based REPLs.
One is {% aTargetBlank "https://deno-playground.now.sh/", "Deno Playground" %}.
This supports code formatting and sharing code with others.
Another less attractive option is
{% aTargetBlank "https://deno.town/", "deno.town" %},
which doesn't support code formatting or sharing code.

## Running

To run a Deno program, enter a command like the following,
passing it a JavaScript or TypeScript file:

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
when any source file (`.js`, `.json`, or `.ts`)
in and below the current directory changes,
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
- ES module imports
- import URL completion
- import maps
- diagnostics with quick fixes
- optional use of `deno fmt` for code formatting
- TypeScript type definitions

Install and configure this extension.

Most likely you will want this extension to only run on
`.js` and `.ts` files that contain Deno code.
Rather than configure this extension with global settings,
create a `.vscode` directory at the top of each Deno project directory.
Then create the file `settings.json` inside it with the following content:

```json
{
  "deno.enable": true,
  "deno.import_intellisense_origins": {
    "https://deno.land": true,
    "https://deno.land/x": true
  },
  "deno.lint": true,
  "deno.unstable": true,
  "[javascript]": {
    "editor.defaultFormatter": "denoland.vscode-deno"
  },
  "[typescript]": {
    "editor.defaultFormatter": "denoland.vscode-deno"
  }
}
```

Enabling `deno.unstable` is required to use `deno lint`.

This should use `deno fmt` to format `.js` and `.ts` files in the project,
but this is not working for me.
See this {% aTargetBlank
"https://github.com/denoland/vscode_deno/issues/267", "issue" %}.

## Linting

To lint all `.js` and `.ts` files
in and below the current directory, enter `deno lint --unstable`.

To see a list of the rules checked by `deno lint`,
enter `deno lint --rules --unstable`.
This includes all of the rules in the
{% aTargetBlank "https://eslint.org/docs/rules/", "eslint:recommended" %} set
and many of the rules in the {% aTargetBlank
"https://www.npmjs.com/package/@typescript-eslint/eslint-plugin#supported-rules",
"plugin:@typescript-eslint/recommended" %} set.
See this {% aTargetBlank "https://github.com/denoland/deno_lint/issues/556",
"issue" %}.

The Deno linter is implemented in Rust in order to be
significantly much faster than ESLint.

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
This uses {% aTargetBlank "https://github.com/dprint/dprint", "dprint" %}
(implemented in Rust) rather than Prettier.
The primary reason for this choice is that dprint is significantly faster.

Some noticeable changes dprint makes include:

- adds semicolons where missing
- changes single quotes to double
- adds trailing commas after last items
  when they do not all fit on a single line
  (applies to imports, function parameters, function arguments,
  array literals, and object literals)

While dprint is configurable, the `deno fmt` command
currently ignores dprint configuration files.

To prevent formatting a statement,
precede it with the comment `// deno-fmt-ignore`.
To prevent formatting an entire file,
add the comment `// deno-fmt-ignore-file` at the top.

There is discussion about making the formatting configurable,
but it seems to not currently be configurable.

If the formatting performed by `deno fmt` is not to your liking,
Prettier can be used instead.

## Getting Information

To get the locations of cache directories used by Deno, enter `deno info`.
This provides the cache directory,
the subdirectory where remote modules are cached, and
the subdirectory where the output of the TypeScript compiler is stored.

To install a remote module and see a list of its dependencies,
enter `deno info {module-url}`.

## Generating Documentation

The `deno doc` command writes documentation for built-in types
and local source files to stdout.

For example, to see documentation on the `Deno.read` function,
enter `deno doc --built-in Deno.read`.
To see documentation for the local source file `demo.ts`,
enter `deno doc demo.ts`.

When run on source files, it only generates documentation on things
that are exported, such as variables, functions, classes, and types.
It takes a very simple approach.
For variables, including constants, only their name is output.
For functions, their signature is output.
For classes, their constructor signature,
fields, and method signatures are output.
Each of these are followed by the text
of the `/** comment */` that precedes it.
It does not perform any special parsing or formatting of
{% aTargetBlank "https://jsdoc.app/", "JSDoc" %} comments
and it does not generate HTML files.

## Debugging

Deno programs can be debugged using the Chrome DevTools Debugger.
It supports source maps which enables stepping through TypeScript code
while actually running JavaScript code produced by the compiler.
The steps to use the debugger are:

1. Start the script by entering
   `deno run -A --inspect-brk some_name.ts`
2. Browse `chrome://inspect`
3. Click the "inspect" link at the bottom of the Chrome window
   to open another Chrome window.
4. Typically the source file of a dependency
   rather than the main script will be displayed.
   Click the table for the main script to view its source code.
   If there is no tab for it, use the file explorer on the left
   to navigate to it under "file://".
5. Click line numbers to toggle breakpoints.
6. Click the buttons in the upper-right to
   resume execution (play triangle), step over (curved arrow over dot),
   step in (down arrow), step out (up arrow), step (right arrow),
   deactivate all breakpoints (slash through breakpoint symbol),
   and toggle pausing on exceptions (stop sign with equal sign).
7. Examine local and global variables under the "Scope" section on the right.
8. Create watches under the "Watch" section on the right.
9. Examine the call stack under the "Call Stack" section on the right.
10. To restart execution, click the "reload" link
    back in the original Chrome window.

<img alt="Chrome debugger window #1" style="width: 40%"
  src="/blog/assets/deno-chrome-debugger-1.png?v={{pkg.version}}"
  title="Chrome debugger window #1">
<img alt="Chrome debugger window #2" style="width: 90%"
  src="/blog/assets/deno-chrome-debugger-2.png?v={{pkg.version}}"
  title="Chrome debugger window #2">

VS Code also supports debugging Deno programs.
The steps to do this are:

1. Create a `.vscode` directory in the top project directory.
1. Create the file `.vscode/launch.json` with the following content:

   ```json
   {
     "version": "0.2.0",
     "configurations": [
       {
         "name": "Deno",
         "type": "pwa-node",
         "request": "launch",
         "cwd": "${workspaceFolder}",
         "runtimeExecutable": "deno",
         "runtimeArgs": ["run", "-A", "--inspect-brk", "${file}"],
         "attachSimplePort": 9229,
         "outputCapture": "std"
       }
     ]
   }
   ```

1. Open the main script file in VS Code.
1. Select Run ... Start Debugging or press F5.
1. If prompted to select an environment, select "Deno".
1. Use the debugger control buttons at the top.
1. Use the left-nav debugger content including the sections labelled
   "VARIABLES", "WATCH", "CALL STACK", "LOADED SCRIPTS", and "BREAKPOINTS".
1. To exit from the debugger, click the red square in the debug controls,
   select Run ... Stop Debugging, or press shift-F5.

<img alt="VS Code debugger" style="width: 90%"
  src="/blog/assets/deno-vs-code-debugger.png?v={{pkg.version}}"
  title="VS Code debugger">

Currently it reports the following error when importing modules from URLs:

```text
Could not load source '{file-system-path/some-name.ts}': Unable to retrieve source content.
```

See this {% aTargetBlank "https://github.com/denoland/vscode_deno/issues/233",
"issue" %}.

## Testing

Deno has built-in support for unit tests, documented
{% aTargetBlank "https://deno.land/manual/testing", "here" %}.

Here is a simple module defined in `statistics.ts`:

```ts
export function average(...numbers: number[]): number {
  const {length} = numbers;
  if (length === 0) return 0;
  return sum(...numbers) / length;
}

export function sum(...numbers: number[]): number {
  return numbers.reduce((acc, n) => acc + n, 0);
}
```

Here is a file that provides tests for the `average` function.

```ts
import {assertEquals} from 'https://deno.land/std@0.79.0/testing/asserts.ts';
import {average, sum} from './statistics.ts';

Deno.test('average', () => {
  assertEquals(average(), 0);
  assertEquals(average(1), 1);
  assertEquals(average(1, 2), 1.5);
  assertEquals(average(1, 2, 3), 2);
});

Deno.test('sum', () => {
  assertEquals(sum(), 0);
  assertEquals(sum(1), 1);
  assertEquals(sum(1, 2), 3);
  assertEquals(sum(1, 2, 3), 6);
});
```

To run all the tests in the current directory, enter `deno test`.

The assertion functions provided by the `testing/assert.ts` module include:

- `assert` checks for truthy value
- `assertEquals` and `assertNotEquals` check for deep equality
- `assertExists` checks for not `null` or `undefined`
- `assertMatch` and `assertNotMatch`
- `assertObjectMatch` checks for object being a subset of another deeply
- `assertStrictEquals` and `assertNotStrictEquals` checks for same reference
- `assertArrayIncludes` and `assertStringIncludes`
- `assertThrows` and `assertThrowsAsync`
- `fail`
- `unreachable`

Adding `.only` or `.skip` after `Deno.test` is not supported, but there is
{% aTargetBlank "https://github.com/denoland/deno/issues/5197", "discussion" %}
about adding them.

In addition, test code can import and throw `AssertionError`.

The `equal` function checks for deep equality
and returns a boolean rather than throwing.

To run the tests and report test coverage,
enter `deno test --coverage --unstable`.
This outputs any uncovered source lines.
It would be nice if this supported an option to
only output uncovered line numbers instead of the actual code.

## Bundling

The `deno bundle` command bundles all the dependencies of a program
into a single file.
It takes a `.js` or `.ts` file as input and produces a `.js` file.
To bundle the program `demo.js` into `demob.js`,
enter `deno bundle demo.js demob.js`.
To run the bundled version, enter `deno run demob.js`.
See this {% aTargetBlank "https://github.com/denoland/deno/issues/8211",
"issue" %}.

Some bundles can be run in web browsers.
However, this command does not modify the use of top level `await`,
so code that uses that feature will only run in browsers that support it.

## Installing Programs

The `deno install` command creates an executable shell script
that runs a given Deno program.
Any required permission options must be included in this command.
By default the script is placed in `$HOME/.deno/bin`
and has the same name as the input source file with no file extension.
To change the name of the executable created,
specify the name with the `--name` or `-n` option.
To change the location of the executable,
specify it with the `--root` option or
set the environment variable `DENO_INSTALL_ROOT`.

The generated script uses the `deno run` command to execute the program
just as you would when running it yourself.

Bundling and installing a Deno application
is not a good solution for distributed apps to users.
In order for users to run the app they would need to:

- get the `.js` file created by the `bundle` command
- get the script created by the `install` command
- modify the script to refer to the file path of the copied `.js` file
- install Deno
- run the script using the same shell
  that was specified in the `install` command

A better solution using `deno compile` is coming soon.

## Shell Completions

The `deno completions` command generates shell-specific commands
that define completions for `deno` subcommands and their options.
The supported shells are zsh, bash, fish, powershell, and elvish.

For example, to generate completions for the bash shell
enter the following command:

```bash
deno completions bash > /usr/local/etc/bash_completion.d/deno.bash
```

To cause these completions to be activated in every new bash shell session,
add the following in `~/.bash_profile`:

```bash
source /usr/local/etc/bash_completion.d/deno.bash
```

After starting a new bash shell, type `deno` following by a space
and press the tab key twice to see all the possible completions.
They will include flags like `--help` and `--version`.
They will also include commands like
`fmt`, `install`, `lint`, `run`, and `test`.
After completing the command to `deno run` followed by a space,
press tab twice to see all the possible flags including
`--allow-net`, `--allow-read`, `--allow-write`,
`--inspect-brk`, `--unstable`, and `--watch`.

To generate completions for the fish shell
and enable their use enter the following command:

```bash
deno completions fish > $HOME/.config/fish/completions/deno.fish
```

No additional steps are needed in order for the
completions to be activated in every new fish shell session.
After starting a new fish shell, type `deno` followed by a space
and press the tab key once to see all the possible completions.
Use the down and up arrow keys to select a completion and continue typing.
To see the allowed flags for a command, type one `-` and press tab.
Once again, use the down and up arrow keys to select one.

## Libraries

Deno library code comes from three categories.
The "Runtime API" defines built-in things that are available without importing.
The "Standard Library" defines things that are maintained by the Deno team.
"Third Party Modules" are maintained by other developers.
The main Deno website at {% aTargetBlank "https://deno.land/", "deno.land" %}
has links to documentation for all three categories at the top of the page.

## Built-ins

For documentation on the built-in functions, see the
{% aTargetBlank "https://doc.deno.land/built-in/stable", "Runtime API" %}.

These include the following from the Web API:

- variables

  - `console`: an object with the same methods as in browsers
    including `debug`, `dir`, `error`, `info`, `log`, `table`, and `warn`

    `console.table(object)` draws table border lines in the terminal.

    The other methods can style their output using the following CSS properties:

    - `color` with any CSS color value like a name or hex value preceded by `#`
    - `background-color` with a color value
    - `font-style` with values like `normal` and `italic`
    - `font-weight` with values like `normal` and `bold`
    - `text-decoration-color` with a color value (not widely supported by terminals)
    - `text-decoration-line` with values like `none`, `underline`, and `line-through`

    To use CSS properties, pass two or more strings to the method.
    The first string contains the text to output
    and `%c` at each location where the styling should change.
    Supply one CSS string for each occurrence of `%c` in the first string.

    For example:

    ```js
    console.log(
      '%cHello, %cWorld!',
      'color: red; background-color: yellow; font-style: italic',
      'color: blue; font-weight: bold'
    );
    ```

  - `window` - global object like in web browsers

    The properties of this object include:

    - global variables `console` and `Deno`
    - global constructor functions for various events
    - all the global functions listed below
    - all the global classes listed below

- functions

  - `fetch` for sending HTTP requests

  - `alert`, `confirm`, and `prompt`

    These write to stdout and read from stdin.
    All of them pause execution and wait for the user to reply.
    `alert` and `prompt` wait for the enter/return key.
    `confirm` waits for the "y" or "n" key.

  - `setTimeout` and `clearTimeout`

  - `setInterval` and `clearInterval`

  - `setImmediate` and `clearImmediate` are being added.

  - `addEventListener`, `removeEventListener`, and `dispatchEvent`

  - `atob` and `btoa` for converting to and from Base64 encoding

- classes

  - `Event`
  - `EventTarget`
  - `File`
  - `FormData`
  - `Headers`
  - `Request`
  - `Response`
  - `URL`
  - `URLSearchParams`
  - `WebSocket`
  - `Window`
  - `Worker`

To capture performance data for a section of code,
begin the section with `performance.mark('some-name');`
and end the section with `performance.measure('some-name');`.

Here is an example of dispatching and listening for custom events:

```ts
// ProgressEvent extends EventTarget, not Event.
// But the first parameter of an EventListener is supposed to be an Event.
// @ts-ignore
const listener = (event: ProgressEvent): void => {
  // Report on the progress.
  console.log('loaded =', event.loaded);
  if (event.loaded >= event.total) {
    removeEventListener('myCustomEvent', listener);
    clearInterval(intervalId);
  }
};
addEventListener('myCustomEvent', listener);

let loaded = 0;
const intervalId = setInterval(() => {
  // Do some work.
  loaded += 10; // report that 10% of the work is done
  const event = new ProgressEvent('myCustomEvent', {
    lengthComputable: true,
    loaded,
    total: 100
  });
  dispatchEvent(event);
}, 200);
```

## Imports

Deno source files import other files as ES modules
using the `import` statement, not the `require` function like in Node.js.
The file to import can be specified with
a relative file path, an absolute file path, or a URL.
In all cases a file, not just a directory, must be specified.
Unlike in Node.js, the filename `index.js` is not treated as
the default filename and a file extension must be included.
When a `.ts` file is imported, it is automatically compiled to JavaScript.

Potential issues with importing modules from URLs include the ability to:

- run code without a connection to the internet
- run code if the owner of a remote module removes or breaks it
- be confident the code won't do something malicious

Deno addresses all of these concerns.
The first two are covered by the fact that Deno caches remote modules
when they are initially downloaded.
Subsequent runs of your code use cached copies of the remote modules.
New versions must be explicitly requested with the `--reload` option.
The last concern is addressed by the Deno security model.
No code can read/write files, access network resources,
access environment variables, or run subprocesses
unless given permission using the `--allow-*` flags.

A version can optionally be specified in `import` URLs. For example:

```js
import {blue, green, red} from 'https://deno.land/std@0.79.0/fmt/colors.ts';
```

When a URL with no version is imported, the newest version is used.
But after the import is executed, the program will continue to use that version,
not checking for new versions, unless it is run with the `--reload` flag.

File URLs for libraries maintained in GitHub contain tag names,
so creating tags that are version numbers is great way
to make multiple versions compatible with Deno imports.
To see the URL to use, select a file, select a tag, click the "Raw" button,
and note the URL in the address bar.

Deno does not use `npm` to install modules.
Instead, `import` statements can specify module URLs
that reference network resources such as those in GitHub.

Imported files are cached by saving them in the directory
specified in the `DENO_DIR` environment variable
rather than in a `node_modules` directory.
The directory is shared between all Deno projects
rather and using a separate directory for each project
as is done in Node.js.
This saves disk space and avoids needing to
re-download and re-compile previously used libraries for new projects.

If the `DENO_DIR` environment variable is not set,
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
The many subcommands of the `deno` command
make npm scripts, implemented in a `package.json` file,
less necessary.

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

## Import Maps

Imports maps are currently an experimental feature
that only work if the `--unstable` flag is included.
They define a mapping from keywords to URLs
and allow the keywords to be used in place of URLs in `import` statements.
The mapping is specified in a JSON file
which is referenced by the `--importmap` option.

For example, the file `import_map.json` could contain:

```json
{
  "imports": {
    "date-fns": "https://cdn.deno.land/date_fns/versions/v2.15.0/raw/index.js"
  }
}
```

A script in the file `demo.js` can import these as follows:

```js
// Example function calls take directly from https://date-fns.org/.
import {format, formatDistance, formatRelative, subDays} from 'date-fns';

console.log(format(new Date(), "'Today is a' iiii"));
// example output: Today is a Monday

console.log(formatDistance(subDays(new Date(), 3), new Date()));
// output: 3 days ago

console.log(formatRelative(subDays(new Date(), 3), new Date()));
// example output: last Friday at 7:26 p.m.
```

To run this, enter the following:

```bash
deno run --import-map=import_map.json --unstable demo.js
```

## Lock Files

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

## Standard Library

The Deno {% aTargetBlank "https://deno.land/std", "Standard Library" %}
is modeled after that of the Go programming language.
It is implemented in TypeScript.
The Deno core team maintains and reviews it,
providing more quality assurance than using third-party libraries.
These modules are not installed by default and are
downloaded and cached the first time a script that uses them is run.

Highlights from the Deno standard library
are described in the following table:

| Module      | Description                                                                        |
| ----------- | ---------------------------------------------------------------------------------- |
| archive     | tars and untars files                                                              |
| bytes       | operates on Uint8Array values                                                      |
| datetime    | parses date strings, formats `Date` objects, and extracts data from them           |
| encoding    | deals with many data encodings such as base32, binary, csv, toml, and yaml         |
| flags       | command line argument parser                                                       |
| fmt         | formatted output (`sprintf`) including colors and styles                           |
| fs          | file system manipulation                                                           |
| hash        | creates hashes (ex. md5) and applies them to data                                  |
| http        | for implementing HTTP servers                                                      |
| io          | reading from and writing to streams, including reading by lines                    |
| log         | logging with debug, info, warning, error, and critical levels                      |
| mime        | reads and writes multipart form data                                               |
| node        | compatibility layer for the Node.js standard library (partially implemented)       |
| path        | manipulates file paths; currently on generates RegExps from glob expressions       |
| permissions | gets permission flag strings and grants permissions (see `Deno.permissions` first) |
| signal      | generates and listens for OS signals such as SIGINT (ctrl-c)                       |
| testing     | assertions for implementing unit tests                                             |
| textproto   | `TextProtoReader` class for reading from a `BufReader`                             |
| uuid        | generates and validates unique identifiers                                         |
| wasi        | implements the WebAssembly System Interface                                        |
| ws          | for implementing WebSocket servers                                                 |

TODO: Is `datetime` being renamed to `date`? See https://github.com/denoland/deno/issues/8594.

These libraries typically contain a `mod.ts` file
that defines what is exported.

To see documentation for a standard library source file,
browse {% aTargetBlank "https://deno.land/", "deno.land" %},
click "Standard Library" at the top,
click the name of the library (such as `fs`),
click a `.ts` source file (such as `mod.ts`),
and click "View Documentation".

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

The `io` library provides functions for input/output.
For example, to read from a file one line at a time:

```js
import {readLines} from 'https://deno.land/std@0.79.0/io/mod.ts';
import * as path from 'https://deno.land/std@0.79.0/path/mod.ts';

const filename = path.join(Deno.cwd(), 'my-file.txt');
const reader = await Deno.open(filename);
// The readlines function returns an "async iterable object".
// The "for await" loop iterates over this.
for await (const line of readLines(reader)) {
  console.log(line);
}
```

## Resource Ids

Deno refers to input and output streams as resources
that have resource ids.
Resource ids are useful for streaming reads (see `Deno.read`),
streaming writes (see `Deno.write`),
seeking to specific resource locations (see `Deno.seek`).
and flushing pending data operations (see `Deno.fdatasync` and `Deno.fsync`).

Here is an example of using resources.

```js
import {readLines} from 'https://deno.land/std@0.79.0/io/mod.ts';

const file = await Deno.open('./BeverlyHillbillies.txt');
console.log('file.rid =', file.rid); // 3

// This returns an object where keys are resource ids (integers as strings)
// and the values are resource names.
// Resources include "stdin", "stdout", "stderr", and any opened files.
const resources = Deno.resources();
console.table(resources);

// Determine which resources are associated with a terminal.
for (const rid of Object.keys(resources)) {
  console.log(`Is resource id ${rid} a TTY?`, Deno.isatty(Number(rid)));
  // stdin, stdout, and stderr are ttys.
  // Files are not.
}

const READ_LINE_BY_LINE = true;
if (READ_LINE_BY_LINE) {
  for await (const line of readLines(file)) {
    console.log('line =', line);
  }
} else {
  // Read the entire file at once.
  // buffer is a Uint8Array.
  const buffer = await Deno.readAll(file);
  const text = new TextDecoder().decode(buffer);
  console.log('text =', text);
}

// One way to close a file is to refer to it by its resource id.
// This also removes the file from the list of resources.
//Deno.close(file.rid);

// Here is another way which just calls Deno.close(file.rid).
file.close();
```

While you could read from stdio using its resource id,
it is easier to use the `prompt` function.
While you could write to stdout and stderr using their resource ids,
it is easier to use `console.log` and `console.error`.

## Third Party Modules

Many third party modules are registered at
{% aTargetBlank "https://deno.land/x", "deno.land/x" %}.
The code for these modules originates from a public GitHub repository.
When a module is added to `deno.land/x`,
an immutable copy is made in an S3 bucket and it is served from there.
As of 12/3/2020 there were 1342 modules here.

To add your own modules to `deno.land/x`,
browse the site and click the "Add a module" button
under "How do I add a module".
Apply a git tag to each version.
These tags are used as version numbers in import URLs.

One example is the `case` module which provides many functions
that convert multi-word strings where words are separated by spaces
into another string.
Follow the instructions to add a
"Branch or tag creation" webhook to your repository.

```js
import * as c from 'https://deno.land/x/case@v2.1.0/mod.ts';

const s = 'one FINE day';
console.log('camel:', c.camelCase(s)); // oneFineDay
console.log('constant:', c.constantCase(s)); // ONE_FINE_DAY
console.log('dot:', c.dotCase(s)); // one.fine.day
console.log('header:', c.headerCase(s)); // One-Fine-Day
console.log('lower:', c.lowerCase(s)); // one fine day
console.log('lowerFirst:', c.lowerFirstCase(s)); // one FINE day
console.log('normal:', c.normalCase(s)); // one fine day
console.log('param:', c.paramCase(s)); // one-fine-day
console.log('pascal:', c.pascalCase(s)); // OneFineDay
console.log('path:', c.pathCase(s)); // one/fine/day
console.log('sentence:', c.sentenceCase(s)); // One fine day
console.log('snake:', c.snakeCase(s)); // one_fine_day
console.log('swap:', c.swapCase(s)); // ONE fine DAY
console.log('title:', c.titleCase(s)); // One Fine Day
console.log('upper:', c.upperCase(s)); // ONE FINE DAY
console.log('upperFirst:', c.upperFirstCase(s)); // One FINE day
```

Another site for third party Deno modules is
{% aTargetBlank "https://nest.land/", "nest.land" %}.
This is a free repository of Deno modules that uses
{% aTargetBlank "https://www.arweave.org/", "Arweave" %}
to permanently store them using Blockchain.
As of 11/30/2020 there were 196 registered modules here.

## deno.land

The site `https://deno.land/x` hosts a curated collection of
third-party modules that work with Deno.
As of 11/23/2020 it contained 1,302 modules.
Examples include lodash, date_fns, ramda, xstate, ky (HTTP client), and
i18next (internationalization framework).

## Using Node Packages

Node packages that only use Node.js APIs that have been polyfilled by Deno
have a possibility of being usable in Deno programs. For example,
{% aTargetBlank "https://www.npmjs.com/package/lodash", "lodash" %} can be used.
To do this:

1. Create a `package.json` file by entering `npm init`.
2. Install Node package with the `npm install` command
   to add them in a `node_modules` subdirectory.
3. Require them with the code like the following.

```js
// use_node.js
import {createRequire} from 'https://deno.land/std@0.79.0/node/module.ts';
const require = createRequire(import.meta.url);
const _ = require('lodash');

const scores = [87, 73, 94];
console.log('first score =', _.first(scores)); // 87
console.log('last score =', _.last(scores)); // 94
```

To run this, enter `deno run --allow-env --allow-read --unstable use_node.js`.

Deno provides polyfills for some Node packages.
See the list at {% aTargetBlank
"https://deno.land/std@0.79.0/node#supported-built-ins", "Supported Builtins" %}.
Use the `require` function to load these.
For example, to load the `path` polyfill:

```js
import {createRequire} from 'https://deno.land/std@0.79.0/node/module.ts';
const require = createRequire(import.meta.url);
const path = require('path');
```

## Deno Global Variable

When a script is run with `deno run`,
the `Deno` global variable is made available.
It has a large number of properties, most of which are functions.
Examples include:

TODO: Do something with this list!
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

`build` - an object with `arch`, `env`, `os`, `target`, and `vendor` properties

`env` - an object with the functions `delete`, `get`, `set`, and `toObject`

`errors` - an object whose properties are
the following error constructor functions
which can be used to create errors thrown by your code:

- `AddrInUse`
- `AddrNotAvailable`
- `AlreadyExists`
- `BadResource`
- `BrokenPipe`
- `Busy`
- `ConnectionAborted`
- `ConnectionRefused`
- `ConnectionReset`
- `Http`
- `Interrupted`
- `InvalidData`
- `NotConnected`
- `NotFound`
- `NotSupported`
- `PermissionDenied`
- `TimedOut`
- `UnexpectedEof`
- `WriteZero`

`version` - an object with the properties `deno`, `typescript`, and `v8`
whose values are all version numbers

To write and read text files:

```ts
const text = 'This is a story\nbout a man named Jed';
try {
  await Deno.writeTextFile('demo.txt', text);
  const result = await Deno.readTextFile('demo.txt');
} catch (e) {
  // handle errors
}
```

## Environment Variables

The `Deno.env` variable provides methods to
get, set, and delete environment variables.
Using these requires the `--allow-env` flag.

| Operation | Function                        |
| --------- | ------------------------------- |
| get       | `Deno.env.get("name")`          |
| set       | `Deno.env.set("name", "value")` |
| delete    | `Deno.env.delete("name")`       |

The shell in which the Deno program runs is no affected
by the `set` or `delete` methods.

For example:

```js
Deno.env.set('COLOR', 'yellow');
let color = Deno.env.get('COLOR');
console.log(color); // yellow
Deno.env.delete('COLOR');
color = Deno.env.get('COLOR');
console.log(color); // undefined
```

## Parsing command line arguments

There are several Deno modules for parsing command line arguments.
The standard library provides the
{% aTargetBlank "https://deno.land/std/flags", "flags" %} module.
Third party options include
{% aTargetBlank "https://deno.land/x/yargs@v16.1.1-deno", "yargs" %} and
{% aTargetBlank "https://github.com/c4spar/deno-cliffy", "Cliffy" %}.

The `flags` module provides a `parse` function that gathers
command line flags into an object and returns it.
Default values can be specified for each flag.
Additional values after `--` are gathered into an array stored in the key "\_".

An options method to be called with unsupported flags,
named `unknown`, can be supplied.
These flags will still be added to the returned object
unless this method returns `false`.

Here is an example of using the `flags` module in the file `flags_demo.js`.
It recognizes the flags "alpha", "beta", and "gamma".
The value of the "alpha" flag is coerced to a boolean.
The values of the "beta" and "gamma" flags are treated as strings.
There is no support for treating flag values as numbers,
but they can be converted to numbers after being parsed.

```js
import {parse} from 'https://deno.land/std@0.79.0/flags/mod.ts';

const options = {
  boolean: ['alpha'],
  string: ['beta', 'gamma'],
  default: {
    alpha: false,
    beta: 1,
    gamma: 'Greek'
  },
  unknown(flag) {
    console.error('unsupported flag', flag);
    return false;
  }
};
const args = parse(Deno.args, options);
console.table(args);
```

When this is run with the command
`deno run flags_demo.js --alpha=foo --beta=19 --delta -- foo bar`
it outputs the following:

```text
unsupported flag ---delta
┌───────┬───────┬───────┬─────────┐
│ (idx) │   0   │   1   │ Values  │
├───────┼───────┼───────┼─────────┤
│   _   │ "foo" │ "bar" │         │
│ alpha │       │       │  true   │
│ beta  │       │       │  "19"   │
│ gamma │       │       │ "Greek" │
└───────┴───────┴───────┴─────────┘
```

Here is the same example of using the `yargs` module in the file `yargs_demo.ts`.
It also supports "aliases" which enable shorthand options
(ex. `--alpha` or `-a`).

```ts
import yargs from 'https://deno.land/x/yargs@v16.2.0-deno/deno.ts';
import {Arguments} from 'https://deno.land/x/yargs@v16.2.0-deno/deno-types.ts';

const args = yargs(Deno.args)
  .option('alpha', {
    alias: 'a',
    default: false,
    description: 'first greek',
    type: 'boolean'
  })
  .option('beta', {
    alias: 'b',
    default: 1,
    description: 'second greek',
    type: 'number'
  })
  .option('gamma', {
    alias: 'g',
    default: 'Greek',
    description: 'third greek',
    type: 'string'
  })
  .strict().argv; // only allow described options
console.table(args);
```

When this is run with the command
`deno run yargs_demo.ts --alpha=foo --beta=19 --gamma=delta -- foo bar`
it outputs the following:

```text
┌───────┬───────┬───────┬────────────┐
│ (idx) │   0   │   1   │   Values   │
├───────┼───────┼───────┼────────────┤
│   _   │ "foo" │ "bar" │            │
│ alpha │       │       │   false    │
│   a   │       │       │   false    │
│ beta  │       │       │     19     │
│   b   │       │       │     19     │
│ gamma │       │       │  "delta"   │
│   g   │       │       │  "delta"   │
│  $0   │       │       │ "deno run" │
└───────┴───────┴───────┴────────────┘
```

The yargs module supports using the `--help` flag
to output help on the supported flags.
For example, `deno run yargs_demo.ts --help` outputs the following:

```text
Options:
      --help     Show help                                             [boolean]
      --version  Show version number                                   [boolean]
  -a, --alpha    first greek                          [boolean] [default: false]
  -b, --beta     second greek                              [number] [default: 1]
  -g, --gamma    third greek                         [string] [default: "Greek"]
```

## Databases

The most popular Deno library for working with relational databases is
{% aTargetBlank "https://deno.land/x/cotton", "Cotton" %}.
It supports MySQL, PostgreSQL, and SQLite.

For more details on SQLite, see [here](/blog/sqlite).

Cotton supports {% aTargetBlank
"https://deno.land/x/cotton@v0.7.4/docs/guide/query-builder.md",
"query builder" %} functions that enable
creating SQL statements with function calls
as an alternative to manually creating SQL statement strings.

Cotton also supports {% aTargetBlank
"https://deno.land/x/cotton@v0.7.4/docs/guide/model.md",
"object-relational mapping" %} that maps a JavaScript class to a database table.

Finally, Cotton supports {% aTargetBlank
"https://deno.land/x/cotton@v0.7.4/docs/guide/migrations.md",
"database migrations" %} for making a series of changes to a database schema
in a repeatable way.

Here's an example using
{% aTargetBlank "https://www.sqlite.org/index.html", "SQLite" %}.

1. Install SQLite by downloading a version from the
   {% aTargetBlank "https://www.sqlite.org/download.html", "SQLite Download Page" %}.
1. Create a pets database by entering `sqlite3 pets.db`
   This starts an interactive session, indicated by the `sqlite>` prompt.
1. Create a "dogs" table by entering
   `create table dogs(id integer primary key autoincrement, name string, breed string);`
1. Add a row to the "dogs" table by entering
   `insert into dogs values('Comet', 'Whippet');`
1. Query the "dogs" table by entering `select \* from dogs;`

Here is a Deno program in a file named `cotton_demo.js`
that interacts with the pets database.

```ts
import {connect} from 'https://deno.land/x/cotton@v0.7.4/mod.ts';

const db = await connect({type: 'sqlite', database: 'pets.db'});

// Delete all rows from the dogs table.
//await db.table("dogs").delete().execute();
//await db.table("dogs").delete().where('name', '*').execute();
await db.query('delete from dogs');

// Add new rows to the dogs table.
const initialDogs = [
  {name: 'Maisey', breed: 'Treeing Walker Coonhound'},
  {name: 'Ramsay', breed: 'Native American Indian Dog'},
  {name: 'Oscar', breed: 'German Shorthaired Pointer'},
  {name: 'Comet', breed: 'Whippet'}
];
// Note that building a query is separate from executing it.
const query = db.table('dogs').insert(initialDogs);
console.log('sql =', query.toSQL()); // to see what it will execute
await query.execute();

await db
  .table('dogs')
  .where('name', 'Oscar')
  .update({name: 'Oscar Wilde'})
  .execute();

// Using a SQL string
let dogs = await db.query('select * from dogs order by name');
console.log('dogs =', dogs);

// Using query builder
dogs = await db.table('dogs').order('name').execute();
console.log('dogs =', dogs);

// Using "where"
const [comet] = await db.table('dogs').where('name', 'Comet').execute();
console.log('comet =', comet);

// Using pagination
dogs = await db.table('dogs').order('name').limit(2).offset(2).execute();
console.log('2nd page of dogs with 2 per page =', dogs);

await db.disconnect();
```

To run this, enter the following command which is
very specific about which files should be readable and writable:

```bash
deno run \
  --allow-read=pets.db,pets.db-journal \
  --allow-write=pets.db,pets.db-journal \
  cotton_demo.js
```

An alternative supported by Cotton is to
define model classes that map to tables,
create instances of those classes, and
interact with the database using the model classes and instances.
Each model class must have a property that is mapped to the primary key of its table
and that column must use autoincrement.

Here is an example that mimics most of the previous example using a model class:

```ts
import {
  Column,
  connect,
  DataType,
  Model,
  Primary
} from 'https://deno.land/x/cotton/mod.ts';

const db = await connect({type: 'sqlite', database: 'pets.db'});
const manager = db.getManager();

// If no table name is passed to @Model, it will default to
// the lowercase version of the class name, "dog" in this case.
@Model('dogs')
class Dog {
  // This class cannot have a constructor because Cotton
  // requires being able to create instances with new Dog().

  @Primary()
  id!: number;

  // The docs say "Cotton is smart enough to determine the data type
  // of that particular column using TypeScript types.
  // However, you can still customize your column types
  // by passing the type option."
  // But omitting this gives the error
  // "Column '${propertyKey}' must have a type!"
  @Column({type: DataType.String})
  name!: string;

  @Column({type: DataType.String})
  breed!: string;
}

// Delete all rows from the dogs table.
// The query builder doesn't support deleting all rows from a table
// because it is a dangerous thing to do in production.
await db.query('delete from dogs');

// Add new rows to the dogs table.
const initialDogs = [
  {name: 'Maisey', breed: 'Treeing Walker Coonhound'},
  {name: 'Ramsay', breed: 'Native American Indian Dog'},
  {name: 'Oscar', breed: 'German Shorthaired Pointer'},
  {name: 'Comet', breed: 'Whippet'}
];
for (const {name, breed} of initialDogs) {
  const dog = new Dog();
  dog.name = name;
  dog.breed = breed;
  await manager.save(dog);
}

await manager.query(Dog).where('name', 'Oscar').update({name: 'Oscar Wilde'});

const dogs = await manager.query(Dog).all();
console.log('dogs =', dogs);

const comet = await manager.query(Dog).where('name', 'Comet').first();
console.log('comet =', comet);

await db.disconnect();
```

Because this code uses decorators, the following options
must be present in a `tsconfig.json` file:

```json
{
  "compilerOptions": {
    "experimentalDecorators": true,
    "emitDecoratorMetadata": true
  }
}
```

It seems that Cotton does not currently support
creating or executing for prepared statements?
See this {% aTargetBlank "https://github.com/rahmanfadhil/cotton/issues/32",
"issue" %}.

## MongoDB

One option for working with MongoDB databases in Deno
is {% aTargetBlank "https://deno.land/x/mongo@v0.20.0", "deno_mongo" %}.
Another option is
{% aTargetBlank "https://eveningkid.github.io/denodb-docs/", "denodb" %}
which aims to works with many databases.
It uses deno_mongo under the hood for MongoDB databases,
but I wasn't able to get it to work.
See this {% aTargetBlank "https://github.com/eveningkid/denodb/issues/152",
"issue" %} and this {% aTargetBlank
"https://github.com/eveningkid/denodb/commit/abed3063dd92436ceb4f124227daee5ee6604b2d#commitcomment-44634400",
"temporary fix" %}.

Here is an example of using deno_mongo:

```ts
import {MongoClient} from 'https://deno.land/x/mongo@v0.20.0/mod.ts';

const client = new MongoClient();
await client.connect('mongodb://127.0.0.1:27017');

interface Dog {
  _id: {$oid: string};
  name: string;
  breed: string;
}

const db = client.database('animals');
const dogsColl = db.collection<Dog>('dogs');

// Delete all the documents in the dogs collection.
let count = await dogsColl.deleteMany({_id: {$ne: null}});
console.log('deleted', count);

// Insert one dog.
const id = await dogsColl.insertOne({
  name: 'Snoopy',
  breed: 'Beagle'
});
console.log('inserted id =', id);

// Insert many dogs.
const initialDogs = [
  {name: 'Maisey', breed: 'Treeing Walker Coonhound'},
  {name: 'Ramsay', breed: 'Native American Indian Dog'},
  {name: 'Oscar', breed: 'German Shorthaired Pointer'},
  {name: 'Comet', breed: 'Whippet'}
];
await dogsColl.insertMany(initialDogs);

// Update one of the dogs.
const {matchedCount, modifiedCount, upsertedId} = await dogsColl.updateOne(
  {name: {$eq: 'Oscar'}},
  {$set: {name: 'Oscar Wilde'}}
);

// Get all the documents in the dogs collection.
const dogs = await dogsColl.find({name: {$ne: null}});
console.log('dogs =', await dogs.toArray());

// Get the number of documents in the dogs collection.
count = await dogsColl.count();
console.log('count =', count);
```

## Basic HTTP server

The standard library contains `file_server.ts`
which implements a basic HTTP file server.
To install this and server files in the current directory:

```bash
deno install --allow-net --allow-read https://deno.land/std@0.87.0/http/file_server.ts
file_server .
```

There are many options for implementing a custom HTTP server.
Here is a very basic HTTP server that serves files
and only uses the standard library.

```js
import {serve} from 'https://deno.land/std@0.83.0/http/server.ts';
import {serveFile} from 'https://deno.land/std@0.83.0/http/file_server.ts';

const port = 1234;
const server = serve({port});
console.log('listening on port', port);

for await (const req of server) {
  try {
    const content = await serveFile(req, path);
    req.respond(content);
  } catch (e) {
    const status = e && e instanceof Deno.errors.NotFound ? 404 : 500;
    req.respond({body: e.message, status});
  }
}
```

Here is a more advanced HTTP server written in TypeScript.
Typically one would use a framework such as
{% aTargetBlank "https://oakserver.github.io/oak/", "oak" %} (covered below) or
{% aTargetBlank "https://github.com/asos-craigmorten/opine", "opine" %}
(port of Express) for this,
but it demonstrates what is possible using only the standard library.
It supports two operations, "add" and "multiply",
that are invoked with requests like these:

```text
GET http://localhost:1234/add?n1=2&n2=3 returns 5
GET http://localhost:1234/multiply?n1=3&n2=4 returns 12
```

```ts
import {
  Response,
  serve,
  Server,
  ServerRequest
} from 'https://deno.land/std/http/server.ts';
import {decode} from 'https://deno.land/std@.79.0/encoding/utf8.ts';

type StringToString = {[key: string]: string};

const port = 1234;
const server: Server = serve({port});
console.log('listening on port', port);

async function getBodyJson(req: ServerRequest): Promise<string> {
  const text = await getBodyText(req);
  const contentType = req.headers.get('Content-Type');
  if (contentType !== 'application/json') {
    throw new Error(`Content-Type ${contentType} is not valid for JSON`);
  }
  return JSON.parse(text);
}

async function getBodyText(req: ServerRequest): Promise<string> {
  // Deno makes heavy use of the Uint8Array type.
  const buf: Uint8Array = await Deno.readAll(req.body);
  return decode(buf);
}

function getQueryParams(
  req: ServerRequest,
  ...names: string[]
): StringToString {
  const host = req.headers.get('host');
  // URL and URLSearchParams are Web APIs.
  const url = new URL('http://' + host + req.url);
  const searchParams = new URLSearchParams(url.search);

  const params: StringToString = {};
  for (const name of names) {
    const value = searchParams.get(name);
    if (value === null) {
      throw new Error('missing query parameter ' + name);
    }
    params[name] = value;
  }
  return params;
}

function add(req: ServerRequest): number {
  const {n1, n2} = getQueryParams(req, 'n1', 'n2');
  return Number(n1) + Number(n2);
}

// This just demonstrates getting a JSON body.
async function echoJson(req: ServerRequest): Promise<string> {
  const obj = await getBodyJson(req);
  return JSON.stringify(obj);
}

function multiply(req: ServerRequest): number {
  const {n1, n2} = getQueryParams(req, 'n1', 'n2');
  return Number(n1) * Number(n2);
}

async function shout(req: ServerRequest): Promise<string> {
  const body = await getBodyText(req);
  return body.toUpperCase();
}

type Handler = (req: ServerRequest) => void;

const routeHandlers: Record<string, Handler> = {add, echoJson, multiply, shout};

function getFirstPathPart(url: string): string {
  const [path] = url.split('?');
  return path.split('/')[1];
}

for await (const req: ServerRequest of server) {
  console.info(req.method, req.url);

  // This assumes that we can determine the route
  // based on the first path part and does not
  // support routes that use path parts as parameters.
  const route = getFirstPathPart(req.url);

  const handler = routeHandlers[route];
  if (handler) {
    try {
      const body = String(await handler(req));
      req.respond({body});
    } catch (e) {
      req.respond({body: e.message, status: 400});
    }
  } else {
    req.respond({body: 'unsupported route ' + route, status: 400});
  }
}
```

## REST Server with oak

There are many HTTP server libraries for Deno.
Currently the most popular is
{% aTargetBlank "https://oakserver.github.io/oak/", "oak" %}.
It is inspired by the Koa package for Node.js.

Other HTTP server libraries to consider include
{% aTargetBlank "https://github.com/zhmushan/abc", "abc" %} and
{% aTargetBlank "https://deno.land/x/drash@v1.3.0", "drash" %}.

Here is an example of implementing a REST server in oak
that supports CRUD operations on dog objects.

```js
import {
  Application,
  Context,
  Router
} from 'https://deno.land/x/oak@v6.5.0/mod.ts';
import {v4} from 'https://deno.land/std@0.85.0/uuid/mod.ts';

const PORT = 1234;
const dogs = {};
const id = v4.generate();
dogs[id] = {id, name: 'Comet', breed: 'Whippet'};

function sendJson(context, value) {
  context.response.headers.set('Content-Type', 'application/json');
  context.response.body = JSON.stringify(value);
}

function deleteAllDogs(context) {
  dogs = {};
}

function getAllDogs(context) {
  sendJson(context, Object.values(dogs));
}

function getDog(context) {
  const {id} = context.params;
  const dog = dogs[id];
  if (dog) {
    sendJson(context, dog);
  } else {
    context.response.status = 404;
  }
}

async function createDog(context) {
  const body = await context.request.body();
  const {breed, name} = await body.value;
  const id = v4.generate();
  const dog = {id, breed, name};
  dogs[id] = dog;
  context.response.status = 201;
  sendJson(context, dog);
}

async function updateDog(context) {
  const {id} = context.params;
  if (dogs[id]) {
    const body = await context.request.body();
    const dog = await body.value;
    dogs[id] = dog;
    sendJson(context, dog);
  } else {
    context.response.status = 404;
  }
}

function deleteDog(context) {
  const {id} = context.params;
  const exists = Boolean(dogs[id]);
  if (exists) delete dogs[id];
  context.response.status = exists ? 200 : 404;
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

Deno server libraries do not yet seem to support
generating OpenAPI (Swagger) documentation.
One module that claims to support this is {% aTargetBlank
"https://deno.land/x/deno_swagger_doc@releasev1.0.0", "deno_swagger_doc" %},
but I wasn't able to get it to work.
See this {% aTargetBlank
"https://github.com/singhcool/deno-swagger-doc/issues/8", "issue" %}.

## WebSockets

Deno has built-in support for WebSockets.
To demonstrate this, we'll build a simple app
where multiple browser clients can connect to a WebSocket server.
Each client renders an empty `svg` element.
Clicking on the `svg` adds a circle in that location on every connected client.
This could be the start of a game.

<img alt="Deno WebSocket demo" style="width: 30%"
  src="/blog/assets/deno-websocket-demo.png?v={{pkg.version}}"
  title="Deno WebSocket demo">

Here is the server code:

```ts
import {serve} from 'https://deno.land/std@0.79.0/http/server.ts';
import {
  acceptWebSocket,
  WebSocket
} from 'https://deno.land/std@0.79.0/ws/mod.ts';

const clients: WebSocket[] = [];

async function addClient(sock: WebSocket) {
  clients.push(sock);

  // For every new event coming from this client ...
  for await (const event of sock) {
    // Send the event to all connected clients.
    clients.forEach(client => client.send(event as string));
  }
}

const PORT = '1234';
// For every client connection request ...
for await (const req of serve(':' + PORT)) {
  const {conn, r: bufReader, w: bufWriter, headers} = req;
  try {
    const sock = await acceptWebSocket({conn, bufReader, bufWriter, headers});
    addClient(sock);
  } catch (e) {
    console.error(`failed to accept WebSocket: ${e}`);
    await req.respond({status: 400});
  }
}
```

Here is the client code that can be opened in a web browser:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style>
      body {
        margin: 0;
      }

      circle {
        fill: yellow;
        stroke: blue;
        stroke-width: 5px;
      }

      svg {
        background-color: linen;
        height: 400px;
        width: 600px;
      }
    </style>
    <script>
      function addCircle(x, y, radius) {
        const svg = document.querySelector('svg');
        const circle = document.createElementNS(
          'http://www.w3.org/2000/svg',
          'circle'
        );
        circle.setAttribute('cx', x);
        circle.setAttribute('cy', y);
        circle.setAttribute('r', radius);
        svg.appendChild(circle);
      }

      const ws = new WebSocket('ws://localhost:1234');

      ws.onopen = () => {
        console.log('opened WebSocket');
      };

      ws.onmessage = event => {
        const message = event.data;
        const [shape, x, y, size] = message.split(' ');
        if (shape === 'circle') {
          addCircle(x, y, size);
        } else {
          alert('unsupported shape ' + shape);
        }
      };

      function handleClick(e) {
        const RADIUS = 20;
        const msg = `circle ${event.clientX} ${event.clientY} ${RADIUS}`;
        ws.send(msg);
      }
    </script>
  </head>
  <body>
    <svg xmlns="http://www.w3.org/2000/svg" onclick="handleClick(event)" />
  </body>
</html>
```

## WebAssembly (WASM)

WebAssembly is a binary format for a stack-based virtual machine.
It is a compilation target of many programming languages
including C, C++, C#, F#, Go, JavaScript, Kotlin, Rust, and Swift.
(Perhaps Python cannot yet be compiled to WebAssembly.)
A notable feature is that WebAssembly code
can be run from all the major web browsers.
It can also be run from a command line (terminal),
Node.js, and Deno.

WebAssembly code executes much faster than JavaScript code.
Some benchmarks show it to be an order of magnitude faster.
Deno can execute WebAssembly code.
It does this by utilizing the WebAssembly virtual machine provided by V8.

TODO
To compile Python to WebAssembly ...

To compile Rust to WebAssembly and use it in a Deno program,
see [Rust WebAssembly](/blog/Rust#webassembly).

TODO: Create an example that demonstrates the performance difference
TODO: between JavaScript and WebAssembly. Perhaps compute the total
TODO: and average sales by category of a large number of transactions.

## Tidbits

To determine if the current file was executed as the main script,
check the value of the boolean `import.meta.main`.
