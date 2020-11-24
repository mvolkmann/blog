---
eleventyNavigation:
  key: Deno
layout: topic-layout.njk
---

{% aTargetBlank "https://deno.land/", "Deno" %} is a
"secure runtime for JavaScript and TypeScript".

Ryan Dahl created both
{% aTargetBlank "https://nodejs.org/", "Node.js" %} and Deno.
In June 2018, he gave a talk at JSConf EU 2018
titled "10 Things I Regret About Node.js".
A prototype of Deno was presented during this talk.

Deno has builtin support for TypeScript.
It automatically compiles TypeScript code to JavaScript before running it,
making it unnecessary to define a build process to do this.

Both Node.js and Deno are built on the Chrome V8 JavaScript engine.
Node.js is primarily implemented in C++.
Deno was originally implemented in Go and later changed to Rust.

Version 1.0 of Deno was released on May 13, 2020.
It is open source and uses the MIT license.

Deno is secure by default.
The environment, file system, and network
can only be accessed if explicitly enabled.

To get started and find documentation, see the
{% aTargetBlank "https://deno.land/manual", "Deno Manual" %}.

## Differences from Node.js

1. Deno uses ES Modules by default.
   Node.js can use ES Modules, but defaults to CommonJS.
1. Deno uses URLs for loading remote dependencies rather than
   relative paths in the `node_modules` directory.
1. Deno uses built-in resource fetching
   rather than relying on npm to install packages.
1. Deno has built-in TypeScript support with caching of compile source files.
1. Deno requires file system and network access to be explicitly enabled.
1. Deno APIs utilize Promises, ES6, and TypeScript features.
1. Deno provides a large standard library with no external dependencies.
1. Deno uses message passing channels to invoke privileged system APIs.

## Standard Library

The standard library of Deno is modeled after
that of the Go programming language.

## Pros

## Cons

## Installing

To install Deno:

- for Windows, use Chocolately: `choco install deno`
- for macOS, use Homebrew: `brew install deno`
- for Linux, use curl: `curl -fsSL https://deno.land/x/install/install.sh | sh`

## Linting

## Code Formatting

## Imports

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

## Version

Enter `deno -V` to output the Deno version.

Enter `deno --version` to output the versions of Deno, V8, and TypeScript.

## Running

To run a Deno program, enter a command like the following:

```bash
deno run src/main.ts
```

To run in watch mode, add the currently unstable watch option.
For example:

```bash
deno run --allow-net --unstable --watch my-server.ts
```

When a file change is detected, the program is restarted,
but no output is generated to indicate that this happened.

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

## Implementing a REST Server

TODO: Finish this.
