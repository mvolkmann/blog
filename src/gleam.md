---
eleventyNavigation:
  key: Gleam
layout: topic-layout.njk
---

<figure style="width: 30%">
  <img alt="Gleam logo" style="border: 0"
    src="/blog/assets/gleam-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://gleam.run", "Gleam" %} is
"a friendly language for building type-safe systems that scale!"
It runs on Beam, the Erlang virtual machine.
Gleam programs can be compiled to Erlang or JavaScript.

Gleam has a syntax that is inspired by Elm, OCaml, and Rust.
Like OCaml, Gleam has strong type inference,
making it unnecessary to specify types.

Gleam includes a compiler (implemented in Rust), build tool, package manager,
code formatter, language server, standard library, JavaScript bindings,
and a WASM API.
It was released in 2024.

Gleam programs can use libraries implemented in Gleam, Elixir, and Erlang.

The Gleam logo is a pink starfish named Lucy that glows underwater.

## Installing

To install in macOS, enter `brew update`, `brew install erlang`,
and `brew install gleam`.
For other platforms, see <a href="https://gleam.run/getting-started/installing/"
target="_blank">Installing Gleam</a>.

## VS Code

To edit Gleam code using VS Code,
consider installing the extension Gleam by Gleam.

## Projects

To create a new Gleam project, enter `gleam new {name}`.

This creates the following files:

- `.github/workflows/test.yml`
- `.gitignore`
- `README.md`
- `gleam.toml`
- `src/{name}.gleam`
- `test/{name}_test.gleam`

Gleam source files have the `.gleam` file extension.

To run the project, enter `gleam run`.

This compiles the project into Erlang files (`.erl` extension) that can
be found in the `build/dev/erlang/{name}/_gleam_artefacts` directory.
It then runs the `main` function in the `.gleam` file
within the `src` directory identified by
the `name` property in the `gleam.toml` file.
It will output a log of all the steps it takes,
and finally "Hello from {name}!".

To compile to JavaScript instead of Erlang,
add the `--target` (or `-t`) option
with the value `javascript` (default is `erlang`).
For example, `gleam run -t javascript`.
This compiles the project into JavaScript files
(`.mjs` extension for JavaScript modules) that can
be found in the `build/dev/javascript/{name}` directory.
It will output a log of all the steps it takes,
and finally produce the same output as when using Erlang.

Each source file defines a module.
To run a different module, specify it with the `--module` or `-m` option.
For example, `gleam run -m other` runs
the `main` function in the file `src/other.gleam`.

To run all the tests, enter `gleam test`.

This will output a log of all the steps it takes,
and finally "1 tests, 0 failures".

## Dependencies

To add a dependency, enter `gleam add {name1} {name2} ...`

This updates the `gleam.toml` file and
installs the packages in the `build/packages` directory.

The `build` directory can be deleted.
It will be recreated automatically when
`gleam run` or `gleam test` are executed.
All packages listed as "dependencies" or "dev-dependencies"
in the `gleam.toml` file will be re-installed.

## Importing Packages

To import a package in a Gleam source file:

```ocaml
import {name}
```

Some packages have subpackages.
To import a subpackage:

```gleam
import {name}/{sub-name}
```

The line `import gleam/io` makes the module name `io` available
as a prefix that can be used to access all the functions it defines.
For example, `io.println("Hello, World!")`.

A module can be given an alternate name with the `as` keyword.
This can be used to prevent name conflicts.
For example, `import gleam/string as text`.

Module functions can be imported to enable
using them without a module name qualifier.
For example, `import gleam/io.{debug, println}`.
followed by `println("Hello, World!")`.

## Type Inference

Gleam infers the types of all variables, function parameters,
and function return types.
Types can be specified for documentation purposes,
but they must match the inferred types.

The Gleam language server, used by VS Code Gleam extension,
enables hovering over a variable, function defintion, or function call
to display their inferred types in a popup.

Just like OCaml, Gleam supports different operators
for integer and floating point values.

## Web Development

Gleam web development is supported by many libraries including
gleam_http, lustre, mist, ...
