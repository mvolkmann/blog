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
It runs on BEAM (Björn’s Erlang Abstract Machine)
which is the Erlang virtual machine that is also used by
<a href="https://elixir-lang.org" target="_blank">Elixir</a>.
Gleam programs can be compiled to Erlang or JavaScript.

Gleam was created by <a href="https://lpil.uk" target="_blank">Louis Pilford</a>
who currently resides in London.

Gleam has a syntax that is inspired by Elm, Go, Lua, OCaml, and Rust.
Like OCaml, Gleam has strong type inference,
using the Hindley Milner type system,
making it unnecessary to specify types.

Gleam includes a compiler (implemented in Rust), build tool, package manager,
code formatter, language server, standard library, JavaScript bindings,
and a WASM API.
It was released in 2024.

Gleam programs can use libraries implemented in Gleam, Elixir, and Erlang.

The Gleam logo is a pink starfish named Lucy that glows underwater.

## Resources

- <a href="https://gleam.run/frequently-asked-questions/"
  target="_blank">Gleam FAQs</a>
- <a href="https://tour.gleam.run" target="_blank">Gleam Language Tour</a>
- <a href="https://www.youtube.com/watch?v=yUPMVgvl4vo"
  target="_blank">A Brief Introduction to Gleam</a>
- <a href="https://exercism.org/tracks/gleam" target="_blank">Exercism's Gleam Track</a>

## Installing

To install in macOS, enter `brew update`, `brew install erlang`,
and `brew install gleam`.
For other platforms, see <a href="https://gleam.run/getting-started/installing/"
target="_blank">Installing Gleam</a>.

## VS Code

To edit Gleam code using VS Code,
consider installing the extension Gleam by Gleam.

Code formatting will not be performed on save by default.
To configure it, open the Command Palette,
select Format Document, and choose "Gleam".
After doing this one time, code will be formatted on save.

## Projects

To create a new Gleam project, enter `gleam new {name}`.
Project names can only contain lowercase letters, digits, and underscores.
Names beginning with "gleam\_" are reserved.

This creates the following files:

- `.github/workflows/test.yml`

  This runs all the tests in a GitHub Action every time changes are pushed.

- `.gitignore`
- `README.md`
- `gleam.toml`
- `src/{name}.gleam`
- `test/{name}_test.gleam`

Gleam source files have the `.gleam` file extension.

To format all the `.gleam` files in the project from the command-line
rather than in an editor like VS Code, enter `gleam format`.
The code formatter automatically sorts `import` statements.

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

To run all the tests, enter `gleam test`.

This will output a log of all the steps it takes,
and finally "1 tests, 0 failures".

To delete all the generated files, enter `gleam clean`.
This deletes the `build` directory.

## Watch Mode

It's desirable to have something watch the files in project for changes
and automatically run the main module again or run all the tests again
when a change is detected.
Gleam does not currently support this, but an
<a href="https://github.com/gleam-lang/gleam/issues/2570"
target="_blank">issue</a> has been created for it.

## Modules

Each `.gleam` source file defines a module
that contains definitions for items like functions, types, and so on.

To use the items in a module, import it with the `import` keyword.

To run the `main` function in a module other than the main module of a project,
specify it with the `--module` or `-m` option.
For example, `gleam run -m other` runs
the `main` function in the file `src/other.gleam`.

The Gleam standard library provides the following modules:

- `gleam/bit_array`
- `gleam/bool`
- `gleam/bytes_builder`
- `gleam/dict`
- `gleam/dynamic`
- `gleam/float`
- `gleam/function`
- `gleam/int`
- `gleam/io`
- `gleam/iterator`
- `gleam/list`
- `gleam/option`
- `gleam/order`
- `gleam/pair`
- `gleam/queue`
- `gleam/regex`

  - `check` - returns a `Bool` indicating if a `String` matches a `Regex`
  - `compile` - creates a `Regex` from a `String` and a set of `Options`
  - `from_string` - creates a `Regex` from a `String` and no `Options`
  - `scan` - returns a `List` of all the `Regex` matches found in a `String`
  - `split` - returns a `List` of substrings created by splitting a `String`
    on delimiters that match a `Regex`

- `gleam/result`
- `gleam/set`
- `gleam/string`
- `gleam/string_builder`
- `gleam/uri`

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

```ocaml
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

The `as` keyword can also be used to assign alternate names
to imported functions. For example:

```ocaml
import gleam/int.{square_root as sqrt}
```

The Gleam community discourages the use of function calls
that are not qualified by their module name.

## Naming Rules

The names of functions and variables must start with a lowercase letter
and can only contain lowercase letters, digits, and underscores.

The names of types and type constructors must start with an uppercase letter
and can only contain letters and digits.

## Functions

Functions are defined with the `fn` keyword.
They can be named or anonymous.

TODO: Add more detail on functions.

## Type Inference

Gleam infers the types of all variables, function parameters,
and function return types.
Types can be specified for documentation purposes,
but they must match the inferred types.

The Gleam language server, used by VS Code Gleam extension,
enables hovering over a variable, function defintion, or function call
to display their inferred types in a popup.

For example:

```ocaml
// No types specified.
fn add(a, b) {
  a + b
}

// Types specified.
fn subtract(a: Int, b: Int) -> Int {
  a - b
}
```

Just like OCaml, Gleam supports different operators
for integer and floating point values.

## Custom Types

The `type` keyword is used to define a custom type.

## Variables

Variables are declared with the `let` keyword.
All variables and function parameters are immutable.

## Math

Gleam is currently missing lots of math constants and functions
found of most other programming languages.
See this <a href="https://github.com/gleam-lang/gleam/issues/3188"
target="_blank">issue</a>.

## Control Flow

The only control flow mechanism in Gleam is the `case` expression.
For simple cases this can be a bit verbose because the code formatter
will cause every `case` expression to be at least four lines.

For example, the following JavaScript:

```js
if (a < b) console.log('yes');

let color = score > 5 ? 'green' : 'red';
```

could be written as follows in Gleam:

```ocaml
case a < b {
  True -> io.println("yes")
  False -> Nil
}

let color = case score {
  score if score > 5 -> "green"
  _ -> "red"
}
```

TODO: Add more examples of case expressions including pattern matching and guards.

## Error Handling

Many Gleam functions return a Result type
which holds either an `Ok` or an `Error` value.
There are several ways to handle this that are demonstrated below.
Note that the `square_root` function produces
an error when passed a negative number.

```ocaml
import gleam/float
import gleam/int
import gleam/io.{debug, println}
import gleam/result

pub fn main() {
  // All case branches must have the same result type.
  // Here all case braches have a Float result.
  case int.square_root(25) {
    Ok(result) -> result
    Error(_) -> 0.0
  }
  |> debug

  // Here all case braches have a String result.
  case int.square_root(25) {
    Ok(r) -> float.to_string(r)
    Error(_) -> "No real root"
  }
  |> debug

  // assert causes a crash the function after = results in an Error.
  let assert Ok(result) = int.square_root(25)
  debug(result)

  // result.unwrap takes a value of type Result and
  // returns its OK value or the supplied value on Error.
  -25
  |> int.square_root
  |> result.unwrap(0.0)
  // returns supplied value on Error
  |> debug
}
```

## Currying

Unlike OCaml, Gleam does not provide automatic function currying.
Calling a function with fewer arguments than it has parameters
results in an error rather than returning a new function
that expects the remaining arguments.

A "function capture" creates a new single argument function
from an existing one by calling it with a single "hole".
Using an underscore for one of the arguments specifies the hole.
For example:

```ocaml
import gleam/function
import gleam/io

fn add(a, b) {
  a + b
}

pub fn main() {
  let plus5 = add(5, _)
  debug(plus5(2)) // 7
}
```

The `gleam/function` module defines the functions `curry2` through `curry6`
where the number indicates the number of parameters in a function passed to it.
A new function is returned that is curried, meaning it takes a single argument
and returns another function that takes a single argument,
repeatedly until all arguments are supported.
For example:

```ocaml
import gleam/function
import gleam/io

fn add2(a, b) {
  a + b
}

fn add3(a, b, c) {
  a + b + c
}

pub fn main() {
  // The type of plus10 is fn(Int) -> Int.
  let plus10 = function.curry2(add2)(10)
  debug(plus10(9)) // 19

  // The type of curried_add3 is fn(Int) -> fn(Int) -> fn(Int) -> Int.
  let curried_add3 = function.curry3(add3)
  debug(curried_add3(1)(2)(3)) // 6
}
```

## Packages

<a href="https://packages.gleam.run" target="_blank">Gleam Packages</a>
is the official Gleam package repository.
For example, the "mist" web server package can be found here.

## Web Development

Gleam web development is supported by many libraries including
gleam_http, lustre, mist, wisp, ...

## Foreign Function Interface (FFI)

See
<a href="https://www.jonashietala.se/blog/2024/01/11/exploring_the_gleam_ffi/"
target="_blank">Exploring the Gleam FFI</a>.
