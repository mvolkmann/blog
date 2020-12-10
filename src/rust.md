---
eleventyNavigation:
  key: Rust
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

<img alt="Deno logo" style="width: 20%"
  src="/blog/assets/rust-logo.png" title="Rust logo">

## Overview

{% aTargetBlank "https://www.rust-lang.org/", "Rust" %} is a
programming language for building reliable and efficient software.

Features of Rust include:

- fast
- memory-efficient
- rich, static type system with type inference
- ownership model to guarantee memory-safety and thread-safety

## Why use Rust

Performance:

The best way to get software performance is to
use a "systems" language like C, C++, or Rust.
One reason these languages are fast is because
they do not provide automatic garbage collection
that is slow and can run at unpredictable times.
Systems languages also allow control over
whether data is on the stack or on the heap.

Safety:

Software written in systems languages typically must
take great care to avoid memory and threading issues.
Memory issues include accessing memory after it has been freed,
resulting in unpredictable behavior.
Threading issues include race conditions where the order in which
code runs is unpredictable, resulting in unpredictable results.
Rust addresses both of these issues,
resulting in code that is less likely to contain bugs.

Immutable by default:

A large source of errors in any software involves
incorrect assumptions about where data is modified.
Making variables immutable by default and
requiring explicit indication of functions that are
allowed to modify data significantly reduces these errors.

Control over number sizes:

One way to achieve performance in computationally intensive tasks
is to store collections of numbers in contiguous memory for fast access
and control the number of bytes used by each number.

Ownership model:

Manual garbage collection is error prone.
Rust uses an "ownership model" where code is explicit about
the single scope that owns each piece of data.
When that scope ends, the data can be safely freed
because no other scope can possibly be using the data.

Systems languages tend to be more complex that non-systems languages,
requiring more time to learn and more time to write software in them.
Rust is no exception.
But some developers choose to them in spite of this
in order to gain the benefits described above.

## Installing

Rust is installed using the {% aTargetBlank "", "rustup" %} tool.

To install rustup in macOS, install {% aTargetBlank "", "homebrew" %}
and then enter `brew install rustup`.

To install rustup in Linux (or macOS), enter the following command:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

To install rustup in Windows,
use {% aTargetBlank "https://chocolatey.org/", "Chocolately" %}
or {% aTargetBlank "https://scoop.sh/", "Scoop" %}.

After installing rustup, enter `rustup-init`.
This configures the use of Rust in the bash and zsh shells.
When using the fish shell, add the following in `.config/fish/config.fish`:

```bash
set -x PATH $PATH $HOME/.cargo/bin
```

Verify installation by entering `rustc --version`.

## Online Playground

To try Rust code online, browse the
{% aTargetBlank "https://play.rust-lang.org/", "Rust Playground" %}.
This includes access to the top 100 most downloaded crates from
{% aTargetBlank "https://crates.io/", "crates.io" %}
and crates from the
{% aTargetBlank "https://rust-lang-nursery.github.io/rust-cookbook/",
"Rust Cookbook" %}.

<img alt="Rust Playground" style="width: 100%"
  src="/blog/assets/rust-playground.png" title="Rust Playground">

Press the ellipsis after the "RUN" button
to open a popup with the following options:

- "Run" to build and run the code (`cargo run`)
- "Build" to only build the code (`cargo build`)
- "Test" to build the code and run the tests (`cargo test`)  
  Tests must be preceded by `#[test]` and no `main` function can be present.
- "ASM" to build the code and show the generated assembly code
- "LLVM IR" to build the code and show the generated
  LLVM intermediate representation (IR)
- "MIR" to build the code and show the generated
  mid-level intermediate representation (MIR)
- "WASM" to build a WebAssembly module for use in web browsers

The "RUN" button will change to the last selected option
so it can be re-executed by pressing the button.

Press the "DEBUG" button to open a popup for choosing between
"Debug" and "Release" built modes.

Press the "NIGHTLY" button to open a popup for choosing a Rust version
which can be "Stable channel" (default), "Beta channel", or "Nightly channel".
The button text changes to indicate the selected version.

Press the ellipsis after the version button to open a popup
with the following options:

- "Edition" sets the Rust edition to 2018 (default) or 2015
- "Backtrace" to disable (default) or enable
  display of backtraces when a panic occurs  
  Enabling this slows performance a bit.

Press the "SHARE" button to open a panel on the right side
containing the following links:

- "Permalink to the playground" changes the URL to one which will
  recall the current code set to run with the current version of Rust.
- "Direct link to the gist" navigates to the URL of the GitHub Gist
  where the code is stored. The code cannot be executed from here.
- "Embed code in link" changes the URL to one which includes
  a base 64 encoded copy of the code as a query parameter.
  This is only appropriate for small code samples due to URL length limits.
- "Open a new thread in the Rust user forum" does what the link
  implies, making it easy to ask questions about a code sample.
- "Open an issue on the Rust GitHub repository"
  makes it easy to report a bug in Rust.

Press the "TOOLS" button to open a popup with the following options:

- "Rustfmt" formats the code using the `rustfmt` tool.
- "Clippy" runs the Clippy linter on the code.
- "Miri" runs the program using the
  {% aTargetBlank "https://github.com/rust-lang/miri", "Miri interpreter" %}
  which is an experimental interpreter for Rust's
  mid-level intermediate representation (MIR).
  which detects some bugs not detected by press the "RUN" button?
- "Expand macros" displays the code in the right panel with
  all the macro calls expanded in order to see what they actually do.

Press the "CONFIG" button to open a popup with the following options:

- "Style" to switch between "SIMPLE" (no line numbers)
  and "ADVANCED" (line numbers).
- "Keybinding" to choose between keybindings supported by the
  {% aTargetBlank "https://github.com/ajaxorg/ace", "Ace" %} (Cloud9) editor.
  These include ace, emacs, sublime, vim, and vscode.
- "Theme" to choose from 30+ themes including
  cobalt, github, solarized light, solarized dark.
- "Pair Characters" to ???
- "Orientation" to arrange panes horizontally, vertically,
  or automatically choose based on window size.
- and advanced options to control generated assembly code

There doesn't seem to be a way to select a font for the code.

Configuration options are saved in browser Local Storage
so they can be applied to future sessions.
The most recently entered code is also saved in Local Storage,
but previously entered code is not.

## Compiling and Running

Rust source files have a `.rs` file extension.

To compile a Rust source file,
creating an executable with the same name and no file extension,
and run it:

- open a terminal (or Windows Command Prompt),
- cd to the directory containing a `.rs` file that defines a `main` function
- enter `rustc name.rs`
- in macOS or Linux, enter `./name`
- in Windows, enter `name`

For example, the following is a Rust Hello World program:

```rust
fn main() {
    println!("Hello World!");
}
```

## VS Code

Install the Rust extension which adds:

- syntax highlighting
- code completion
- code formatting
- type documentation on hover
- code snippets
- rename refactoring
- linting with error indicators with ability to apply suggestions
- build tasks

TODO: This extension seems to do nothing!

## Terminology

`cargo`: a command-line utility described later
crate: a binary (executable) or a library
module: a set of related values such as constants and functions
package: a set of related crates described by a `Cargo.toml` file
TOML: a configuration file format; stands for Tom's Obvious, Minimal Language

## TOML Syntax

{% aTargetBlank "https://github.com/toml-lang/toml", "TOML" %}
is a configuration file format that maps to a hash table.

Each key/value pair is described by a line with the syntax `key = value`.
Keys are not surrounded by any delimiters.
Supported value data types include
string, integer, float, boolean, datetime,
array (ordered list of values),
and table (collection of key/value pairs).
String values are surrounded by double quotes.
Datetime values have the format `yyyy-mm-ddThh:mm:ss`.
The time portion can be omitted and it can be followed by a time zone
(`Z` for UTC or `+hh:mm` for an offset).
Array elements are surrounded by square brackets and separated by commas.

Comments begin with `#` character and extend to the end of the line.

Sections and sub-sections are indicated by lines
containing a name enclosed in square brackets.
Think of these like keys whose values are objects.

## Cargo

The `cargo` command is a CLI tool that is installed with Rust.
While using it is not required, it is highly recommended.
For help, enter `cargo --help` or just `cargo`.

The following table describes the `cargo` subcommands:

| Subcommand    | Description                                                                |
| ------------- | -------------------------------------------------------------------------- |
| `bench`       | runs benchmarks for the current project                                    |
| `build`       | builds current project in the `target` directory                           |
| `check`       | verifies current project builds without errors,<br>without generating code |
| `clean`       | deletes `target` directory                                                 |
| `clippy`      | checks current project for errors using the Clippy linter                  |
| `doc`         | generates documentation for the current project                            |
| `init`        | creates a Rust project in the current directory                            |
| `install`     | installs an executable in `~/.cargo/bin` by default                        |
| `new`         | creates a Rust project in a new subdirectory                               |
| `publish`     | publishes package to the registry                                          |
| `run` or `r`  | runs current project                                                       |
| `search`      | searches registry for crates                                               |
| `test` or `t` | runs tests in the current project                                          |
| `uninstall`   | removes executable from `~/.cargo/bin` by default                          |
| `update`      | updates dependencies in `Cargo.lock`                                       |

## Naming Conventions

In general, names of "types" use PascalCase
and names of "value" use snake_case.

| Item            | Naming Convention                  |
| --------------- | ---------------------------------- |
| constants       | SCREAMING_SNAKE_CASE               |
| constructors    | snake_case                         |
| crates          | snake_case or kebab-case           |
| enums           | PascalCase                         |
| enums values    | PascalCase                         |
| features        | no convention                      |
| enums           | PascalCase                         |
| file names      | snake_case or kebab-case           |
| functions       | snake_case                         |
| lifetimes       | 'lowercase                         |
| macros          | snake_case!                        |
| methods         | snake_case                         |
| modules         | snake_case                         |
| statics         | SCREAMING_SNAKE_CASE               |
| structs         | PascalCase                         |
| traits          | PascalCase                         |
| type parameters | PascalCase, but usually one letter |
| types           | PascalCase                         |
| variables       | snake_case                         |

## Comments

Rust supports many comment syntaxes.
"Doc comments" are included in generated HTML documentation
that is generated by entering `cargo doc`.
TODO: How do you generate it?

| Syntax      | Usage                                       |
| ----------- | ------------------------------------------- |
| `//`        | extends to end of current line              |
| `/* ... */` | can span multiple lines                     |
| `///`       | doc comment preceding the item it describes |
| `//!`       | doc comment inside the item it describes    |

Code inside doc comments that is surrounded by "fences"
is run by the `rustdoc --test` command.
For example:

1. Create a project by entering `cargo new doc_test`.

1. Add the file `src/math.rs` containing the following:

   ````rust
   /// ```
   /// assert_eq!(math::average(vec![1.0, 2.0, 3.0, 4.0]), 2.5);
   /// ```
   pub fn average(numbers: Vec<f64>) -> f64 {
       let sum: f64 = numbers.iter().sum();
       sum / numbers.len() as f64 // return value
   }
   ````

1. Compile this to a library by entering
   `rustc --crate-type lib src/math.rs`
   This is needed because doc tests are only run on library crates.

1. Run the doc tests by entering
   `rustdoc -L . --test src/math.rs`

1. To call the `average` function from another source file,
   modify `src/main.js` to match the following:

   ```rust
   mod math;

   fn main() {
       let scores = vec![1.0, 2.0, 3.0, 4.0];
       let avg = math::average(scores);
       println!("average = {}", avg);
   }
   ```

1. To run this, enter `cargo run`

TODO: Are all names that end with `!` macros?

## Formatted Print

The `std::fmt` namespace defines macros that format text.

| Macro Name  | Description                           |
| ----------- | ------------------------------------- |
| `format!`   | writes to a `String`                  |
| `print!`    | writes to stdout                      |
| `println!`  | same as `print!`, but adds a newline  |
| `eprint!`   | writes to stderr                      |
| `eprintln!` | same as `eprint!`, but adds a newline |

All of these macros take a formatting string
followed by zero or more expressions whose values
are substituted into the formatting string
where occurrences of `{}` appear.
For example:

```rust
println!("{} is {}.", "Rust", "interesting"); // Rust is interesting.
```

The curly brackets can contain indexes which allow
the expression values to be inserted in a different order
and be inserted more than once. For example:

```rust
println!("{1} {0} {2} {1}", "red", "green", "blue"); // green red blue green
```

## Formatting Code

The most popular code formatting tool for Rust is
{% aTargetBlank "", "rustfmt" %}.
To install this, enter `cargo install rustfmt`.
TODO: Is this installed by default by rustup?

To run it on all `.rs` files in the current directory,
enter `rustfmt *.rs`.

## Variables

Variables are immutable by default.
For variables that hold non-primitive values,
even their fields cannot be mutated.

The `mut` keyword marks a variable as mutable.

A variable declaration has the syntax `let name: type = value;`
where the value is optional.
However, a value must be assigned before the variable is referenced.
The colon and the type can be omitted if it can be inferred from the value.

There are four ways to declare a "variable".

| Syntax                          | Meaning                                                                                               |
| ------------------------------- | ----------------------------------------------------------------------------------------------------- |
| `let name: type = value`        | immutable variable that must be assigned a value<br>before it is used and is thereafter immutable     |
| `let mut name: type = value`    | mutable variable that must be assigned a value<br>before it is used, but can be modified              |
| `const name = value`            | constant that must be assigned a value when it is declared                                            |
| `static name: type = value`     | immutable variable that lives for the duration of the program; typically `const` is preferred         |
| `static mut name: type = value` | mutable variable that lives for the duration of the program;<br>can only mutate in `unsafe` functions |

TODO: Are statics a way to share data across functions,
TODO: even those defined in separate files, without passing it?

## Rules of Ownership

Memory management in Rust is handle by following these rules:

1. Each value is referred to by a variable that is its owner.
1. Each value has one owner at a time.
1. When the owner goes out of the scope, the value is dropped.

## Built-in Scalar Types

Rust defines four scalar (primitive) types which are
boolean, character, integer, and floating-point.

The boolean type name is `bool`.
Its only values are `true` and `false`.

The character type name is `char`.
Literal values are surrounded by single quotes.
Its values are Unicode values of up to four bytes.
TODO: Does every character use all four bytes?

The signed integer type names are `i{n}` where `{n}`
is the number of bits which can be 8, 16, 32, 64, 128 or `size`
which corresponds to either 32 or 64 depending on the processor architecture.
The default type for literal integers is `i32` regardless of the processor.

Literal integer values can use the underscore character to separate
thousands, millions, and so on. For example,
the population of the U.S. in 2020 was approximately 330_676_544.
Hex values begin with `0x`, octal values begin with `0o`,
and binary values begin with `0b`.

The unsigned integer types are the same, but start with `u` instead of `i`.

Floating-point type names are `f{n}` where `{n}` is 32 or 64.
The default type for literal floats is `f64` regardless of the processor.

## Built-in Compound Types

Rust defines two compound (non-primitive) types which are tuple and array.
These are distinct from the collection types that are described later.

A tuple is a fixed-length list of values that can be of different types.
The syntax for a tuple type is `(type1, type2, ...)`.
The syntax for a tuple value is `(value1, value2, ...)`.
Individual values can be accessed by index or destructuring.

For example:

```rust
TODO: ADD THIS
```

An array is a fixed-length list of values that have the same type.
The syntax for an array type is `[type, length]`.
The syntax for an array value is `[value1, value2, ...]`.
For example:

````rust
let rgb = ["red", "green", "blue"];
// A Rust string is a "compound collection", covered later.
let sevens = [7, 5]; // same as [7, 7, 7, 7, 7]
```

Elements of an array can be accessed using
square brackets and zero-based indexes.
For example, `rgb[1]` is "green".

A vector is a variable-length list of values.
TODO: Do they all have the same type?

## Collections

Rust defines three kinds of collections that hold a variable number of values.
These include strings, vectors, and hash maps.

Strings are collections of characters.
Literal values are surrounded by double quotes.

Vectors are collections of any kind of value.

Hash maps hold key/value pairs where the keys and values can be any type.

There are two kinds of strings used in Rust programs.
The Rust language defines the `str` type
and the standard library defines the `String` type.
TODO: Explain the differences!

## Operators

Rust supports common operators including:

- arithmetic: `+`, `-`, `\*`, `/`, `%` (mod)

## Conditional Logic

## Iteration

## Functions

Functions are defined using the `fn` keyword,
followed by a name, parameter list, return type, and body.
For example:

```rust
fn average(numbers: &Vec<f64>) -> f64 {
  numbers.iter().sum() / numbers.len() as f64
}

fn main() {
  let numbers: Vec<f64> = vec![1.0, 2.0, 3.0, 4.0];
  println!("average = {}", average(&numbers));
}
```

A `return` statement returns the value of an expression.
If the last statement is not terminated by a semicolon, its value is returned.
This means that `return my_result;` is equivalent to `my_result`.

Functions are accessible by default within the same source file,
but they are private by default when defined in a different source file.
For functions that should be visible outside the source file that defines them,
add the `pub` keyword before the `fn` keyword.

## Structs

A struct defines a type that is a set of related fields and methods,
similar to a class in other languages.
The `struct` keyword only defines fields.
The `impl` keyword adds methods to a struct.
Struct names are used to create instances.
For example:

```rust
fn main() {
  struct Point2D {
    x: f64,
    y: f64,
  }

  impl Point2D {
    fn distance_to(self: &Point2D, other: &Point2D) -> f64 {
      let dx = self.x - other.x;
      let dy = self.y - other.y;
      (dx.powf(2.0) + dy.powf(2.0)).sqrt()
    }
  }

  let p1 = Point2D { x: 3.0, y: 4.0 };
  let p2 = Point2D { x: 6.0, y: 8.0 };
  let d = p1.distance_to(&p2);
  println!("distance is {}", d);
}
```

Structs and their fields are accessible by default within the same source file,
but they are private by default when defined in a different source file.
For structs that should be visible outside the source file that defines them,
add the `pub` keyword to both the `struct` and the fields to be exposed.

Structs cannot inherit from (extend) other structs,
but they can nest other structs (composition).

## Traits

A trait describes an interface that structs can implement.
Traits can be generic, including type parameters.
Trait functions can provide default implementations
that are used by implementing types that do not override them.

For example:

```rust
fn main() {
  struct Point2D {
    x: f64,
    y: f64,
  }

  trait Distance<T> {
      fn distance_to(self: &Self, other: &Self) -> T;
  }

  impl Distance<f64> for Point2D {
      fn distance_to(self: &Point2D, other: &Point2D) -> f64 {
          let dx = self.x - other.x;
          let dy = self.y - other.y;
          (dx.powf(2.0) + dy.powf(2.0)).sqrt()
      }
  }

  let p1 = Point2D { x: 3.0, y: 4.0 };
  let p2 = Point2D { x: 6.0, y: 8.0 };
  let d = p1.distance_to(&p2);
  println!("distance is {}", d);
}
```

Traits can specify other traits that must also be implemented
by any structs that implement them.  For example:

```rust
pub trait HockeyPlayer: Athlete + Person {
  // Describe functions unique to hockey players here.
}
```

Now any `struct` that implements `HockeyPlayer`
must also implement `Athlete` and `Person`.

## Custom Types

TODO: Is this different from a struct?

## Modules

Modules define collections of values like constants and functions.
A module can be defined in three ways.

1. Inside a source file that uses it with the `mod` keyword.
1. In a file whose name is the module name.
1. In multiple files within a directory whose name is the module name.

By default, all members of a module are private.
To make a member accessible outside the module,
add the `pub` keyword at the beginning of its definition.

TODO: Show examples of each of these approaches.

To use a module that is defined in another file or directory,
use the `mod` keyword to gain access
and the `use` keyword to specify the values in it that will be used.
TODO: Why are both keywords needed?

Modules can be nested to further segregate the defined names.

## Imports

## Standard Library

## <a name="webassembly">WebAssembly</a>

Tools for compiling Rust code to WebAssembly include
{% aTargetBlank "https://rustwasm.github.io/wasm-pack/", "wasm-pack" %} and
{% aTargetBlank "https://www.secondstate.io/articles/ssvmup/", "ssvmup" %}
The last update to wasm-pack was on February 7, 2020,
so this project may be abandoned.
The ssvmup tool was inspired by wasm-pack and has explicit support for Deno.

To compile a `.rs` file to WebAssembly:

1. Install ssvmup by entering the following command (one time only):

   ```bash
   curl https://raw.githubusercontent.com/second-state/ssvmup/master/installer/init.sh -sSf | sh
   ```

1. Create a new Rust library (referred to as a "crate")
   by entering `cargo new --lib my-crate`

1. `cd my-crate`

1. Edit `src/lib.rs` and add code there.
   For example:

   ```rust
   use wasm_bindgen::prelude::*;

   #[wasm_bindgen]
   pub fn factorial(x: u64) -> u64 {
       match x {
           0 | 1 => 1,
           _ => x * factorial(x - 1),
       }
   }
   ```

1. Edit the generated `Cargo.toml` file
   which is similar to a Node.js `package.json` file.
   For example:

   ```toml
   [package]
   name = "my-crate"
   version = "0.1.0"
   authors = ["R. Mark Volkmann <me@gmail.com>"]
   edition = "2018"
   description = "sample project using ssvmup"
   license = "MIT/Apache-2.0"
   #repository = "https://github.com/mvolkmann/my-crate"

   [lib]
   crate-type = ["cdylib"]

   [dependencies]
   wasm-bindgen = "=0.2.61"
   ```

1. Enter `ssvmup build --target deno`
   This creates a `pkg` directory containing
   `package.json`, a `.wasm` file, and a `.js` file that
   reads the `.wasm` file and prepares it for use in JavaScript code.

1. Copy the generated `pkg` directory to the directory
   containing the Deno code that wishes to use it.

1. Import the exported functions with a line like the following:

   ```js
   import {factorial} from './pkg/my_crate.js';
   ```

1. Call the imported functions.

   ```js
   console.log(factorial(4n)); // "n" suffix makes it BitInt
   ```
````
