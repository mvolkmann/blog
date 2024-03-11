---
eleventyNavigation:
  key: Rust
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

<img alt="Rust logo" style="width: 20%"
  src="/blog/assets/rust-logo.png?v={{pkg.version}}" title="Rust logo">
<img alt="Rust mascot" style="width: 30%"
  src="/blog/assets/rustacean-ferris-crab.png?v={{pkg.version}}"
  title="Rust mascot, Ferris the crab">

## Overview

{% aTargetBlank "https://www.rust-lang.org/", "Rust" %}
is a programming language with the goal of
"empowering everyone to build reliable and efficient software."

Features of Rust include:

- performance on par with C/C++
- memory-efficiency
- ownership model that guarantees memory-safety and thread-safety
- rich, static type system with type inference
- targets {% aTargetBlank "https://llvm.org", "LLVM" %} to
  enable Rust programs to run on a wide variety of platforms
- ability to call and be called by languages that support
  the C Application Binary Interface (ABI) with no overhead
  (includes C, C++, Go, Java, Python, and Ruby)
- functional - Functions can be stored in variables,
  passed to functions, and returned from functions.
- somewhat object-oriented - Structs can have fields and methods (like classes).
  These can be public or private, providing encapsulation.
  Structs can implement traits which are like interfaces.
  Traits can be used as types which achieves polymorphism.
  Structs can have fields whose types are other struct types
  which achieves composition, but they cannot inherit from other structs.
- self-hosted (implemented in itself) since 2011

Rust was created at Mozilla by Graydon Hoare,
with contributions from Dave Herman, Brendan Eich, and others.
It was formally announced in 2010.
Version 1.0 was released in May 2015.
A new point release is made every six weeks.

From {% aTargetBlank "https://doc.rust-lang.org/edition-guide/editions/",
"rust-lang.org" %},
"Every two or three years, we'll be producing a new edition of Rust.
Each edition brings together the features that have landed into
a clear package, with fully updated documentation and tooling."

The {% aTargetBlank "https://foundation.rust-lang.org", "Rust Foundation" %}
was announced in February 2021.
It is "an independent non-profit organization to
steward the Rust programming language and ecosystem."
The initial member companies include
AWS, Google, Huawei, Microsoft, and Mozilla.

Rust developers are referred to as "Rustaceans"
which is derived from the word "crustaceans".
Rust mascot is Ferris the crab, a crustacean.
The name is fitting because ferrous metals are subject to rust.
Images of Ferris can be found at {% aTargetBlank
"https://rustacean.net/", "rustacean.net" %}.

## Why use Rust

**Performance:**

The best way to get software performance is to
use a "systems" language like C, C++, or Rust.
One reason these languages are fast is because
they do not provide automatic garbage collection,
which is slow and can run at unpredictable times.
Systems languages also allow control over whether
data is on the stack (faster, but data must have a fixed size)
or on the heap (slower, but data can vary in size).

**Safety:**

Software written in systems languages typically must
take great care to avoid memory and threading issues.
Memory issues include
dangling pointers (accessing memory after it has been freed),
memory leaks (failing to free memory that is no longer needed),
and double frees (freeing memory more than once).
These result in unpredictable behavior.
Threading issues include race conditions where the order in which
code runs is unpredictable, leading to somewhat random results.
Rust addresses both of these issues,
resulting in code that is less likely to contain bugs.

**Immutable by default:**

A large source of software errors involves
incorrect assumptions about where data is modified.
Making variables immutable by default and
requiring explicit indication of functions that are
allowed to modify data significantly reduces these errors.

**Ownership model:**

Manual garbage collection, where developers are responsibly for
allocating and freeing memory, is error prone.
Automatic garbage collection is relatively slow and
requires runtime support which consumes memory.
Rust does not use either form of garbage collection.
It instead enforces an ownership model where code is explicit
about the single scope that currently owns each piece of data.
Unless ownership is transferred to another scope,
when that scope ends the data is safely freed
because no other scope can possibly be using the data.
The "borrow checker" in the Rust compiler enforces this at compile time.

**Zero-Cost Abstractions:**

Rust strives for zero-cost abstractions characterized by
this quote from Bjarne Stroustrup, the creator of C++:
"What you don't use, you don't pay for.
And further: What you do use, you couldn't hand code any better."
Rust supports many abstractions that make code more clear,
but are optimized by the compiler so there is little to no impact
on performance or the amount of machine code that is generated.
One example is the use of generic functions that are compiled
to separate versions for each concrete type with which they are used.
This eliminates the need for runtime dynamic dispatch
and is referred to as "monomorphism".

**Control over number sizes:**

One way to achieve performance in computationally intensive tasks
is to store collections of numbers in contiguous memory for fast access
and specify the number of bytes used by each number.
Rust supports a wide variety of number types for
integer and floating point values of specific sizes.

**WebAssembly:**

WebAssembly (abbreviated WASM) is a binary instruction format for
a stack-based virtual machine that is supported by modern web browsers
(currently Chrome, Edge, Firefox, and Safari).
WASM code typically executes faster than equivalent code written in JavaScript.
Code from many programming languages can be compiled to WASM.
In 2021 full support is only available for C, C++, and Rust.
Experimental support is available for C#, Go, Java, Kotlin, Python,
Swift, TypeScript, and a few other less commonly used languages.

In order to run WASM code in a web browser,
the runtime of the source language must be included.
Rust is a great choice for targeting WASM because it has a very small runtime
compared to options like Python, so it downloads faster.

**Complexity Tradeoff**
Systems languages tend to be more complex that non-systems languages,
requiring more time to learn and more time to write software in them.
Rust is no exception.
But many developers choose to use Rust in spite of this
in order to gain the benefits described above.
On the positive side, the Rust compiler catches many errors
that would only be discovered at runtime with other systems languages.
The Rust compiler also provides very detailed error messages
that often include suggestions on how to correct the errors.

## Why use another programming language

**Not Performance Critical:**

If programming languages that provided automatic memory management
(such as JavaScript/TypeScript, Python, and Go)
are fast enough for the target application,
and garbage collection pauses are not an issue,
the effort required to learn and use Rust may be hard to justify.
For many developers, this is the case for everything they write.

**Learning Curve:**

The learning curve for Rust is quite high.
It may be too much effort to bring an entire team up to speed on using it.
Just learning how to use strings in Rust is a challenge.
Developers must constantly think about which scope "owns" each piece of data
and decide whether values or references should be passed to functions.
They must think about whether values have sizes that are known at compile-time.
Generic types are used heavily (for example, in error handling),
and often generic types are nested.

**Incompatible Libraries:**

If an application needs to use non-Rust libraries that are difficult to use
from Rust, it may be better to use a more compatible programming language.

**Processor Target:**

If the target platform uses a processor type that is not a target of LLVM,
Rust cannot currently produce code that will run on it.

**Compiler Speed:**

The Rust compiler is notoriously slow, but it has been getting faster.
Slow compile times can negatively affect developer flow because
they make it difficult to quickly try alternate coding approaches.
The introduction of multiple companies participating in the
newly formed "Rust Foundation" will likely lead to
improvements in this area.
Testing new and modified functions with unit tests rather than in the context
of an application that uses them can reduce the time to test changes.

## Installing

It is recommended to install Rust using the
{% aTargetBlank "https://github.com/rust-lang/rustup/blob/master/README.md",
"rustup" %} tool.
This enables having multiple versions of Rust installed
and switching between them.

To install rustup in macOS:

1. Install "Command Line Tools for Xcode" from {% aTargetBlank
   "https://developer.apple.com/downloads/more", "developer.apple.com" %}
   (requires a free Apple ID)
1. Install {% aTargetBlank "", "homebrew" %}.
1. Enter `brew install rustup`

Enter the following command to install rustup in Linux (or macOS):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

To install rustup in Windows,
use {% aTargetBlank "https://chocolatey.org/", "Chocolately" %}
or {% aTargetBlank "https://scoop.sh/", "Scoop" %}.

For more details, see {% aTargetBlank
"https://forge.rust-lang.org/infra/other-installation-methods.html",
"Other Rust Installation Methods" %}.

After installing rustup, enter `rustup-init`.
This installs many toolchain items including `cargo` (build/run tool),
`clippy` (linter), `rust-docs` (documentation), `rust-std` (standard library),
`rustc` (compiler), and `rustfmt` (code formatter).
It also configures the use of Rust in the bash and zsh shells.
When using the fish shell, add the following in `.config/fish/config.fish`:

```bash
set -x PATH $PATH $HOME/.cargo/bin
```

Verify the installation by entering `rustc --version`
which should output the version of the `rustc` command that is installed.

Once installed, to update the versions of all the Rust tools
enter `rustup update`.

## Learning Resources

Resources for learning Rust include:

- {% aTargetBlank "https://cheats.rs", "Rust Language Cheat Sheet" %}

- {% aTargetBlank
  "https://docs.microsoft.com/en-us/learn/paths/rust-first-steps/",
  "Take your first steps with Rust" %} from Microsoft

- `rustup doc` command

  This displays local documentation that is installed along with Rust
  in the default web browser. It can be read even when offline.
  This includes links to the following:

  - API documentation  
    Tip: When reading API documentation, if a "Go to latest version" link
    appears in the page header, indicating you are not looking at the latest version of the documentation, click it to see the latest.
  - "The Rust Programming Language" book
  - "Rust by Example" book
  - "The Rust Reference" book which is more detailed
    than "The Rust Programming Language" book
  - "The Cargo Book" book
  - and much more

- {% aTargetBlank "https://www.rust-lang.org/", "Rust website" %}

- {% aTargetBlank "https://doc.rust-lang.org/book/", "The Rust Programming Language" %} book

  This is a free, open source book.
  A print copy can be purchased from {% aTargetBlank
  "https://nostarch.com/Rust2018", "No Starch Press" %}.

- {% aTargetBlank "https://doc.rust-lang.org/stable/reference/", "The Rust Reference" %}

  This is "the primary reference for the Rust programming language".

- {% aTargetBlank "https://rust-lang-nursery.github.io/rust-cookbook/", "Rust Cookbook" %}

  This is a "collection of simple examples that demonstrate
  good practices to accomplish common programming tasks".

- {% aTargetBlank "https://doc.rust-lang.org/stable/rust-by-example/", "Rust by Example" %}

  This is a free, online set of examples in many categories.

- {% aTargetBlank "https://doc.rust-lang.org/std/index.html", "Rust Standard Library" %}
  API documentation

- {% aTargetBlank "https://www.oreilly.com/library/view/programming-rust-2nd/9781492052586/", "Programming Rust" %} book (O'Reilly)

- {% aTargetBlank "https://www.manning.com/livevideo/rust-in-motion?a_aid=cnichols&a_bid=6a993c2e", "Rust in Motion" %} video course (Manning)

- {% aTargetBlank
  "https://www.youtube.com/watch?v=Az3jBd4xdF4&list=PLLqEtX6ql2EyPAZ1M2_C0GgVd4A-_L4_5",
  "Doug Milford Rust Tutorial series" %} YouTube videos

- {% aTargetBlank "https://www.youtube.com/watch?v=WnWGO-tLtLA&t=2s",
  "Jon Gjengset Crust of Rust" %} YouTube videos

- {% aTargetBlank "https://www.youtube.com/watch?v=WnWGO-tLtLA&t=2s",
  "Ryan Levick Introduction to Rust" %} YouTube videos

- {% aTargetBlank "https://github.com/rust-lang/rustlings", "Rustlings" %}

  This "contains small exercises to get you used to
  reading and writing Rust code".

- {% aTargetBlank "https://exercism.io/tracks/rust", "exercism Rust track" %}

  This provides "code practice and mentorship for everyone".
  The exercism site includes exercises across 52 languages.

- {% aTargetBlank "https://github.com/ctjhoa/rust-learning", "rust-learning" %}

  This includes "a bunch of links to blog posts, articles, videos, etc
  for learning Rust."

## Terminology

TODO: Add more terminology in this section? ownership, borrow checker, ...

- [Cargo](#cargo)
  - a command-line utility for building and running Rust programs
- {% aTargetBlank "https://github.com/rust-lang/rust-clippy", "Clippy" %}
  - a code linter with over 400 checks for
    correctness, style, complexity, an performance
- [crate](#crates)
  - a Rust program (binary) or library
  - contains a tree of modules
- {% aTargetBlank "https://crates.io/", "crates.io" %}
  - repository of Rust crates, similar to npm for JavaScript
- [enum](#enums)
  - a named type whose values come from a list of named variants
  - in Rust, these variants can have associated data
  - a key feature of Rust error handling
- future
  - represents the result of an operation that will complete in the future,
    similar to a JavaScript `Promise`
- generic
  - a parameterized type that enables storing and using multiple types of data
- lifetime
  - the time period during which a variable can be accesses,
    starting when it is created and ending when it is freed (a.k.a dropped)
  - often associated with the scope of a particular code block
- [macro](#macros)
  - a function-like construct whose name ends in `!`
    and generates code at compile-time
- [module](#modules)
  - a set of related values such as constants and functions
- package
  - `cargo` feature for building, testing, and sharing crates
  - a set of crates described by a `Cargo.toml` file
  - contains any number of binaries and 0 or 1 library
- panic
  - represents an unrecoverable error that causes a program to terminate,
    print a stack trace, and print an error message
- [struct](#structs)
  - a named collection of fields similar to a class in other languages
  - can have associated functions and methods
- [trait](#traits)
  - a named collection of constants, function signatures,
    and optional default implementations
  - similar to interfaces in other languages
- [TOML](#toml)
  - a configuration file format used by Cargo
  - stands for Tom's Obvious, Minimal Language

## Rust Playground

To try Rust code online, browse the
{% aTargetBlank "https://play.rust-lang.org/", "Rust Playground" %}.
This includes access to the top 100 most downloaded crates (libraries)
from {% aTargetBlank "https://crates.io/", "crates.io" %}
and crates from the
{% aTargetBlank "https://rust-lang-nursery.github.io/rust-cookbook/",
"Rust Cookbook" %}.

A good use for the Rust Playground is to copy code examples
from this blog post into it and run them.
Then make changes to the code to try variations and expand your knowledge.

The example code shown in the screenshot below
will be more clear after structs and traits are explained.

<img alt="Rust Playground" style="width: 100%"
  src="/blog/assets/rust-playground.png?v={{pkg.version}}"
  title="Rust Playground">

All of the code must be entered in a single editor pane,
simulating being in the single source file `main.rs`.

Press the ellipsis after the "RUN" button
to open a popup with the following options:

- "Run" to build and run the code (`cargo run`)
- "Build" to only build the code (`cargo build`)
- "Test" to build the code and run the tests (`cargo test`)  
  Test functions must be preceded by `#[test]`
  and no `main` function can be present.
- "ASM" to build the code and show the generated assembly code
- "LLVM IR" to build the code and show the generated
  LLVM intermediate representation (IR)
- "MIR" to build the code and show the generated
  mid-level intermediate representation (MIR)
- "WASM" to build a WebAssembly module for use in web browsers

The "RUN" button will change to the last selected option
so it can be re-executed by simply pressing the button.

Press the "DEBUG" button to open a popup for choosing between
"Debug" and "Release" build modes.
A debug build completes in less time because it performs less optimization.

Press the "STABLE" button to open a popup for choosing a Rust version
which can be "Stable channel" (default), "Beta channel", or "Nightly channel".
The button text changes to indicate the selected version.

Press the ellipsis after the version button to open a popup
with the following options:

- "Edition" sets the Rust edition to 2018 (default) or 2015
- "Backtrace" to disable (default) or enable display of backtraces
  when a panic occurs which slows performance a bit.

Press the "SHARE" button to open a panel on the right side
containing the following links:

- "Permalink to the playground" changes the URL to one which will recall
  the current code, set to run with the currently selected version of Rust.
- "Direct link to the gist" navigates to the
  URL of the GitHub Gist where the code is stored.
  The code can be viewed, but not executed from here.
- "Embed code in link" changes the URL to one which includes
  a base 64 encoded copy of the code as a query parameter.
  This is only appropriate for small code samples due to URL length limits.
- "Open a new thread in the Rust user forum" does what the link
  implies, making it easy to ask questions about a code sample.
  Use this frequently while learning!
- "Open an issue on the Rust GitHub repository"
  makes it easy to report a bug in Rust.

Press the "TOOLS" button to open a popup with the following options:

- "Rustfmt" formats the code using the `rustfmt` tool.
- "Clippy" runs the Clippy code linter with over 400 checks for
  correctness, style, complexity, an performance.
- "Miri" runs the program using the
  {% aTargetBlank "https://github.com/rust-lang/miri", "Miri interpreter" %}
  which is an experimental interpreter for Rust's
  mid-level intermediate representation (MIR).
  It can detect some bugs not detected by pressing the "RUN" button.
- "Expand macros" displays the code in the right panel with
  all the macro calls expanded in order to see what they actually do.
  For example, try this with a `main` function that
  just calls the `println!` macro to print "Hello".

Press the "CONFIG" button to open a popup with the following options:

- "Style" enables switching between "SIMPLE" (no line numbers)
  and "ADVANCED" (line numbers).
- "Keybinding" enables choosing between keybindings supported by the
  {% aTargetBlank "https://github.com/ajaxorg/ace", "Ace" %} (Cloud9) editor
  which is used by this tool.
  Options include ace, emacs, sublime, vim, and vscode.
- "Theme" enables choosing between 30+ themes including
  cobalt, github, solarized light, solarized dark.
- "Pair Characters" automatically inserts closing `)`, `}`, and `]` characters
  after the `(`, `{`, and `[` characters are typed.
- "Orientation" enables choosing how panes are arranged.
  Options include Horizontal, Vertical,
  and Automatic which chooses based on window size.
- Advanced options control generated assembly code.

There doesn't seem to be a way to select a font for the code.

Configuration options are saved in browser Local Storage
so they can be applied to future sessions.
The most recently entered code is also saved in Local Storage,
but previously entered code is not retained.

## Compiling and Running

Rust source files have a `.rs` file extension.

Here are the steps to compile a Rust source file that defines a `main` function,
create an executable with the same name and no file extension,
and run the executable:

- open a terminal (or Windows Command Prompt),
- cd to the directory containing the `.rs` file
- enter `rustc name.rs`
- in macOS or Linux, enter `./name`
- in Windows, enter `name`

For example, the following is a Rust Hello World program:

```rust
fn main() {
    println!("Hello World!");
}
```

Calls to names that end in `!`, like `println!`,
are calls to a [macro](#macros) rather than a function.

Typically the `rustc` command is not used directly.
Instead the `cargo` command,
described in the [Cargo](#cargo) section,
is used to run `rustc` and the resulting executable.

## VS Code

If VS Code is being used to edit Rust code,
there are two main extensions to consider: "Rust" and "Rust-analyzer".
Both offer similar features which include:

- syntax highlighting
- code completion
- code formatting
- type documentation on hover
- linting with error indicators and the ability to apply suggestions
- code snippets
- rename refactoring
- debugging
- running build tasks

To enable Rust code formatting, add the following in `settings.json`
where FORMATTER is "rust-lang.rust" for the Rust extension
and "matklad.rust-analyzer" for the "Rust-analyzer" extension.

```json
  "[rust]": {
    "editor.defaultFormatter": FORMATTER,
    "editor.insertSpaces": true,
    "editor.tabSize": 4
  },
```

By default Rust-analyzer displays inferred types inline in code,
which can be beneficial but is also verbose and distracting.
This can be disabled in Settings by searching for "rust analyzer"
and unchecking "Rust-analyzer > Inlay Hints: Type Hints".
Inferred types can still be displayed by hovering over a variable.

These extensions only work properly if the root folder
of a Rust project is opened and it contains a `Cargo.toml` file.
See the [Cargo](#cargo) section for details on creating this.

## <a name="toml">TOML</a>

{% aTargetBlank "https://github.com/toml-lang/toml", "TOML" %}
is a configuration file format that maps to a hash table.
The Rust Cargo tool uses this format for `Cargo.toml` configuration files.

Each key/value pair is described by a line with the syntax `key = value`.
Keys are not surrounded by any delimiters.

Supported value data types include
string, integer, float, boolean, datetime,
array (an ordered list of values),
and table (a collection of key/value pairs).
String values are surrounded by double quotes.
Datetime values have the format `yyyy-mm-ddThh:mm:ss`.
The time portion can be omitted or be followed by a time zone
which is `Z` for UTC or `+hh:mm` for a specific offset.
Array elements are surrounded by square brackets and separated by commas.

Comments begin with the `#` character and extend to the end of the line.

Sections and sub-sections are indicated by lines
containing a name enclosed in square brackets.
Think of these like keys whose values are objects.

Here is the default `Cargo.toml` file that is created by
the command `cargo new project-name`
which we will learn about in the next section:

```toml
[package]
name = "delete-me"
version = "0.1.0"
authors = ["R. Mark Volkmann <r.mark.volkmann@gmail.com>"]
edition = "2018"

# See more keys and their definitions at
# https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
```

## <a name="cargo">Cargo</a>

The `cargo` command is a CLI tool that is installed with Rust.
While using it is not required, it is highly recommended.

For help, enter `cargo --help` or just `cargo`.

The following table describes the `cargo` subcommands:

| Subcommand                      | Description                                                                                     |
| ------------------------------- | ----------------------------------------------------------------------------------------------- |
| `bench`                         | runs benchmarks for the project which are special kinds of tests                                |
| `build`                         | builds the project in the `target` directory                                                    |
| `check`                         | verifies that the project builds successfully,<br>without generating code                       |
| `clean`                         | deletes the `target` directory                                                                  |
| `clippy`                        | runs all project source files through the Clippy linter                                         |
| `doc`                           | generates documentation for the current project                                                 |
| `fmt`                           | formats all project source files using `rustfmt`                                                |
| `init`                          | creates a new Rust project in the current directory                                             |
| `install`                       | installs the project executable, by default in `~/.cargo/bin`                                   |
| `new`                           | creates a new Rust project in a new subdirectory                                                |
| `publish`                       | publishes the project crate in the {% aTargetBlank "https://crates.io", "crates.io" %} registry |
| `run` or `r`                    | builds and runs the project                                                                     |
| <code>search <i>name</i></code> | searches the {% aTargetBlank "https://crates.io", "crates.io" %} registry for matching crates   |
| `test` or `t`                   | runs the project tests                                                                          |
| `uninstall`                     | removes the project executable, by default from `~/.cargo/bin`                                  |
| `update`                        | updates dependencies in the `Cargo.lock` file                                                   |

The most commonly used `cargo` subcommands are described in more detail below.

The `cargo new` command creates a new directory containing a Rust project
that is initialized as a new Git repository.
It contains a `Cargo.toml` configuration file
that specifies the project name, version, authors,
the Rust edition to use, and a list of dependencies.
The created directory also contains a `src` directory containing a single file.
When the `--lib` switch is not included, the file is
`main.rs` which is a simple hello world program.
When the `--lib` switch is included, the file is
`lib.rs` which contains a simple unit test.

In Node.js, project source files can use dependencies listed in their
`package.json` file AND also their dependencies recursively.
But Rust project source files can only use
dependencies listed in their `Cargo.toml` file.
This has the benefit that a dependency can drop one of its dependencies
without breaking apps that use it because a Rust application or library
must explicitly list all of its dependencies.

The `cargo run` command builds and runs the project.
It also downloads dependencies listed in the `Cargo.toml` file,
and their dependencies recursively, that have not yet been downloaded.
This command can be slow when run for the first time in a new project
or if new dependencies have been added since the last time it was run.

To pass command-line arguments to a program, specify them after `--`.  
For example, `cargo run -- arg1 arg2`

To watch project files for changes and
automatically run a `cargo` command when they do,
enter `cargo install cargo-watch` one time
and then enter <code>cargo watch -x <i>subcommand</i></code>.
If the `-x` option is omitted, the subcommand defaults to `check`, not `run`.
Typically the desired subcommand is `run`.

The `cargo build` command creates a non-optimized executable
in the `target/debug` directory.
To create an optimized, production build, enter `cargo build --release`
which creates an executable in the `target/release` directory.

## Naming Conventions

In general, names of types use `PascalCase`
and names of values use `snake_case`.
The compiler outputs warnings when the naming conventions
described in the table below are not followed.

| Item                                                                                           | Naming Convention                    |
| ---------------------------------------------------------------------------------------------- | ------------------------------------ |
| constants                                                                                      | `SCREAMING_SNAKE_CASE`               |
| constructor functions                                                                          | `snake_case`                         |
| crates                                                                                         | `snake_case` or `kebab-case`         |
| {% aTargetBlank "https://doc.rust-lang.org/cargo/reference/features.html", "crate features" %} | no convention                        |
| enums                                                                                          | `PascalCase`                         |
| enum variants                                                                                  | `PascalCase`                         |
| file names                                                                                     | `snake_case` or `kebab-case`         |
| functions                                                                                      | `snake_case`                         |
| lifetimes                                                                                      | `'lowercase`                         |
| macros                                                                                         | `snake_case!`                        |
| methods                                                                                        | `snake_case`                         |
| modules                                                                                        | `snake_case`                         |
| statics                                                                                        | `SCREAMING_SNAKE_CASE`               |
| structs                                                                                        | `PascalCase`                         |
| traits                                                                                         | `PascalCase`                         |
| type parameters (generics)                                                                     | `PascalCase,` but usually one letter |
| type aliases                                                                                   | `PascalCase`                         |
| variables                                                                                      | `snake_case`                         |

## Syntax Highlights

- Rust prefers short keywords. Examples include
  `const` for constant, `fn` for function,
  <code>i<i>size</i></code> for integer types,
  `impl` for implement, `let` for variable declarations, `mod` for module,
  `mut` for mutable, `pub` for public,
  <code>u<i>size</i></code> for unsigned integer types,
  and `use` for imports.
- Strings are delimited by double quotes.
- Single characters are delimited by single quotes.
- Items (like functions, structs, and struct members)
  are made public using the `pub` keyword.
- A dot (`.`) character is used to
  access struct fields and call instance methods.
- A double colon (`::`) is used as a namespace separator (borrowed from C++)
  and to call "associated functions"
  (like class or static methods in other languages).
- Conditions for conditional logic and iteration
  are not surrounded by any delimiter (no parentheses).
- Statements associated with conditional logic and iteration
  must be in blocks surrounded by curly braces.
- The preferred indentation is four spaces.
- Named functions are declared with the `fn` keyword.
- Function return types follow the parameter list and the characters `->`.
- Functions that return nothing omit `->` and the return type.
- Statements must terminated by a semicolon.
- If the last evaluated expression in a function does not end with a semicolon,
  its value is returned.
- Most statements are also expressions and evaluate to a value,
  including the `if` and `match` statements.
- There is no null type or value.
  Instead the `Option` enum `None` variant used.

## Comments

Rust supports regular comments and "doc comments".

| Syntax      | Usage                                                 |
| ----------- | ----------------------------------------------------- |
| `//`        | extends to end of current line                        |
| `/* ... */` | can span multiple lines                               |
| `///`       | doc comment preceding the item it describes           |
| `//!`       | doc comment inside the block of the item it describes |

"Doc comments" are included in HTML documentation
that is generated by entering `cargo doc`.
This creates the directory <code>target/doc/<i>crate-name</i></code>
and writes HTML for the documentation there.

To generate the documentation and open it in the default browser,
enter `cargo doc --open`.
Optionally add the `--no-deps` flag to
avoid building documentation for crate dependencies.

Doc comments optionally include sections with titles that begin with `#`.
Common sections include:

- `# Examples`

  This section provides code examples in Markdown fences.
  It is especially useful to demonstrate calls to functions and methods.
  For library crates (not for binary crates) this code is executed along with
  other test code by the `cargo test` and `rustdoc --test` commands.

- `# Errors`

  This section describes any errors that can be returned by the code
  in a `Result` enum which has `Ok` and `Err` variants.

- `# Panics`

  This section describes scenarios that can cause the code to panic.

- `# Safety`

  For functions marked as `unsafe`, this section explains why.
  It also describes what callers must do to use it safely.

Let's walk through a simple example of code that includes
a doc comment with an "Examples" section.
The `vec!` macro, `iter` method, and `sum` method
are described in more detail later.
For now all you need to know is that the `vec!` macro creates a list of values,
the `iter` method returns an iterator over the values,
and the `sum` method adds all the values provided by the iterator.
In this case all the values have the type `f64`
which is an 8-byte floating point value.

1. Create a library project by entering `cargo new math --lib`.

1. Edit the file `src/lib.rs` and replace its content the following:

   ````rust
   /// # Examples
   ///
   /// ```
   /// let numbers = vec![1.0, 2.0, 3.0, 4.0];
   /// assert_eq!(math::average(&numbers), 2.5);
   /// ```
   pub fn average(numbers: &Vec<f64>) -> f64 {
       let sum: f64 = numbers.iter().sum();
       sum / numbers.len() as f64 // return value
   }
   ````

1. Run the doc comment examples as tests by entering `cargo test`.

## <a name="attributes">Attributes</a>

Rust attributes are like "decorators" in other programming languages.
They annotate an item in order to change its behavior.
An attribute can be specified
immediately before an item with the syntax `#[attr]`,
inside the block of an item with the syntax `#![attr]`,
or at the top level of a source file.
When used at the top level, `#![attr]` specifies a crate-wide attribute.

The following table summarizes commonly used built-in attributes.

| Attribute                        | Description                                                                        |
| -------------------------------- | ---------------------------------------------------------------------------------- |
| `allow(warning1, warning2, ...)` | suppresses the specified linting rule warnings                                     |
| `derive(name1, name2, ...)`      | automatically implements traits or<br>applies "derive macros", often on a `struct` |
| `doc`                            | provides an alternate way to specify and format doc comments                       |
| `should_panic`                   | indicates that a test function is expected to panic                                |
| `test`                           | indicates that a function is a test                                                |

For a list of linting rules that produce warnings, see {% aTargetBlank
"https://doc.rust-lang.org/rustc/lints/listing/warn-by-default.html",
"Warn-by-default lints" %}.
Examples include `dead_code`, `unreachable_code`, `unused_assignment`,
`unused_imports`, and `unused_variables`.
These warnings can be disabled using the `allow` attribute.

The [table of provided traits](#trait-table) in the "Traits" section
indicates those can be automatically implemented using the `derive` attribute.
For more detail, see {% aTargetBlank
"https://doc.rust-lang.org/rust-by-example/trait/derive.html", "Derive" %}.

For more built-in attributes,
see the list in "The Rust Reference" at {% aTargetBlank
"https://doc.rust-lang.org/reference/attributes.html#built-in-attributes-index",
"Attributes" %}.

Custom attributes can be implemented by defining {% aTargetBlank
"https://doc.rust-lang.org/book/ch19-06-macros.html#attribute-like-macros",
"attribute-like macros" %}.

## Formatted Print

The `std` namespace, pronounced "stood", defines many commonly used values.
The `std::fmt` namespace defines macros that format text.

| Macro Name  | Description                           |
| ----------- | ------------------------------------- |
| `print!`    | writes to stdout                      |
| `println!`  | same as `print!`, but adds a newline  |
| `eprint!`   | writes to stderr                      |
| `eprintln!` | same as `eprint!`, but adds a newline |
| `format!`   | writes to a `String`                  |

All of these macros take a formatting string
followed by zero or more expressions whose values
are substituted into the formatting string
where occurrences of `{}` appear.
For example:

```rust
println!("{} is {}.", "Rust", "interesting"); // Rust is interesting.
```

To print a representation of a value for debugging purposes on a single line,
use `{:?}`. To print each field of a struct on separate lines, use `{:#?}`.
Custom structs must implement the `Debug` trait in order to use these.
The easiest way to do this is to add the attribute `#[derive(Debug)]`
before struct definitions.

The following table summarizes what
each of the supported format arguments produce.

| Format Argument | Description                                    |
| --------------- | ---------------------------------------------- |
| `{}`            | value of next argument                         |
| `{:?}`          | debugging value on single line                 |
| `{:#?}`         | debugging value on multiple lines              |
| `{n}`           | value of argument at zero-based index n        |
| `{name}`        | value with a given name                        |
| `{:.n}`         | number with `n` decimal places                 |
| `{:.*}`         | number with specified number of decimal places |
| `{:#X}`         | number as uppercase hexadecimal                |
| `{:#x}`         | number as lowercase hexadecimal                |
| `{:<n}`         | value left justified in a width of n           |
| `{:>n}`         | value right justified in a width of n          |
| `{:^n}`         | value centered in a width of n                 |

Here are some examples:

```rust
let n = 3;
println!("{} {}", n * 2, n * 3); // 6, 9
println!("{2} {1}", n * 2, n * 3); // 9, 6
println!("{double} {triple}", double = n * 2, triple = n * 3); // 6 9

#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

let p = Point2D { x: 1.0, y: 2.0 }; // constructs a struct instance
println!("{:?}", p); // Point2D { x: 1.0, y: 2.0 }
println!("{:#?}", p);
// Point2D {
//     x: 1.0,
//     y: 2.0,
// }

println!("{1} {0} {2} {1}", "red", "green", "blue"); // green red blue green

println!(
    "{red} {green} {blue}",
    blue="00F", green="0F0", red="F00"); // F00 0F0 00F

let pi = std::f64::consts::PI;
println!("{:.4}", pi); // 3.1416
println!("{:.*}", 4, pi); // 3.1416

println!("{:#X}", 15); // 0xF
println!("{:#x}", 15); // 0xf

println!("A{:<5}Z", 123); // A123  Z
println!("A{:>5}Z", 123); // A  123Z
println!("A{:^5}Z", 123); // A 123 Z
```

Here is an example of customizing the way a struct is formatted as a string:

```rust
use std::fmt;

struct Point2D {
    x: f64,
    y: f64
}

impl fmt::Display for Point2D {
    // Using "self" as the name of the first parameter makes this a method.
    // The second parameter "fmt" must be a mutable reference.
    // This method renders a value with the type "fmt::Result".
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "(x:{}, y:{})", self.x, self.y)
    }
}

fn main() {
    let pt = Point2D { x: 1.2, y: 3.4 };
    println!("{}", pt); // (x:1.2, y:3.4)
}
```

For more formatting options, see
{% aTargetBlank "https://doc.rust-lang.org/std/fmt/", "std::fmt" %}.

## <a name="variables">Variables</a>

Rust variables are immutable by default.
For variables that hold non-primitive values
such as arrays, tuples, and structs,
even their elements/fields cannot be mutated.

The `mut` keyword marks a variable or parameter as mutable.
For variables that hold non-primitive values
such as arrays, tuples, and structs,
the variable can be changed to point to a different value
AND their elements/fields can be mutated.

A variable declaration has the syntax
<code>let[ mut] name[: <i>type</i>][ = <i>value</i>];</code>
where optional parts are surrounded by square brackets.
The colon and type can be omitted if
the desired type can be inferred from the value.
A value must be assigned before the variable is referenced.
For example:

```rust
#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

fn main() {
    let p: Point2D = Point2D { x: 1.0, y: 2.0 }; // can assign in declaration
    println!("p = {:?}", p); // Point2D { x: 1.0, y: 2.0 }

    let p: Point2D; // shadows previous declaration (described later)
    p = Point2D { x: 1.0, y: 2.0 }; // can assign after declaration
    println!("p = {:?}", p); // Point2D { x: 1.0, y: 2.0 }
    //p = Point2D { x: 1.2, y: 3.4 }; // cannot change value
    //p.x = 5.6; // cannot change a field in current value

    let mut p = Point2D { x: 1.0, y: 2.0 };
    println!("p = {:?}", p); // Point2D { x: 1.0, y: 2.0 }
    p = Point2D { x: 1.2, y: 3.4 }; // can change value
    println!("p = {:?}", p); // Point2D { x: 1.2, y: 3.4 }
    p.x = 5.6; // can change a field in current value
    println!("p = {:?}", p); // Point2D { x: 5.6, y: 3.4 }
}
```

There are five ways to declare a variable.

| Syntax                           | Meaning                                                                                                            |
| -------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `let name: type = value;`        | immutable variable that must be assigned a value<br>before it is used                                              |
| `let mut name: type = value;`    | mutable variable that must be assigned a value<br>before it is used and can be modified                            |
| `const name: type = value;`      | constant that must be assigned a compile-time expression<br>when it is declared, not the result of a function call |
| `static name: type = value;`     | immutable variable that lives for the duration of the program                                                      |
| `static mut name: type = value;` | mutable variable that lives for the duration of the program;<br>can only access in `unsafe` blocks and functions   |

Variables defined with `static` are given a fixed location in memory
and all references refer to the value at that location.
Their [lifetime](#lifetimes) is `'static` which is the duration of the program.

Variables defined with `const` do not have a location in memory.
The compiler substitutes their value where all references appear,
so the variable doesn't exist at runtime.
In that sense they do not have a lifetime.

Declarations of `const` and `static` variables must be explicitly typed,
rather than inferring a type based on the assigned value.
One rationale is that because their scope can extend to the entire crate,
it is better to be explicit about the desired type.

Constants can be defined using either `const` or `static`.
Using `static` is preferred for values that are larger than a pointer.
This is because the value of a `const` variable is copied everywhere it is used,
unlike the value of a `static` variable that is shared.
In order to assign `static` variables to other variables,
their type must implements the `Copy` trait
because the assignment requires copying.
All of the scalar types like `bool`, `char`, `i32`, and `f64`
implement the `Copy` trait.

Here are examples of using `const` and `static` variables:

```rust
#[derive(Debug)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
}
const PURPLE_C: Color = Color { r: 255, g: 0, b: 255 };
static PURPLE_S: Color = Color { r: 255, g: 0, b: 255 };

static mut SIZE: u8 = 1;

fn main() {
    println!("{:?}", PURPLE_C); // Color { r: 255, g: 0, b: 255 }
    println!("{:?}", PURPLE_S); // Color { r: 255, g: 0, b: 255 }
    let v = PURPLE_C; // allowed
    //let v = PURPLE_S; // error: cannot move out of static item
    println!("{:?}", v); // Color { r: 255, g: 0, b: 255 }b

    unsafe {
        println!("{}", SIZE); // 1
        change_it();
        use_it(); // 2
    }
}

unsafe fn change_it() {
    SIZE = 2; // mutates "static mut" variable
}

unsafe fn use_it() {
    println!("{}", SIZE); // 2
}
```

To print the type of a variable for debugging purposes,
define the following function and pass a reference to it:

```rust
fn print_type<T>(_: &T) {
    // The syntax ::<T> is referred to as the "turbofish" qualifier.
    // It specifies a type in the middle of an expression.
    // In this case it is a generic type.:
    println!("{}", std::any::type_name::<T>())
}

fn main() {
    let v = 19;
    print_type(&v); // i32
}
```

Rust allows variables to be re-declared with a different type in the same block.
This is referred to as "shadowing".
Sometimes this is preferred over coming up with
multiple names for the same concept.
For example:

```rust
let command = "order 3 tacos"; // &str

// The str split_whitespace method returns an Iterator.
// The Iterator nth method returns an Option
// whose value can be obtained in many ways.
// One way is to call unwrap_or, passing it
// a value to return if no value is found.
let quantity = command.split_whitespace().nth(1).unwrap_or(""); // &str

// Parse the string value to create an i32 value.
let quantity = quantity.parse().unwrap_or(0);

if quantity > 2 {
    println!("You must be very hungry!")
} else {
    println!("Perhaps you don't really like tacos.")
}
```

## <a name="ownership-model">Ownership Model</a>

Memory management in Rust is handled by following these rules,
referred to as the ownership model:

1. Each value is referred to by a variable that is its owner.
1. Each value has one owner at a time,
   but the owner can change over its lifetime.
1. When the owner variable goes out of the scope, the value is dropped (freed).

The ownership model provides the following benefits:

- Runtime speed is achieved by
  eliminating the need for a garbage collector (GC).
- Performance is more predictable because there are no GC pauses.
- Memory access is safer since there is no possibility of
  null pointer accesses or dangling pointer accesses
  (accessing memory that has already been freed).
- Parallel and concurrent processing is safer
  because there is no possibility of data races
  causing unpredictable interactions between threads.

Values are stored either in the stack or the heap.
Accessing stack data is faster, but data on the heap can grow and shrink,
and it can live beyond the scope that created it.

Values whose sizes are known at compile time are stored on the stack.
Values of all other types are stored in the heap.
The documentation for types whose size is not known at compile time
indicates this with `?Sized`.
These include:

- slices, not references to them
- string slice types `str` and `OsStr`
- `std::path::Path` for representing and operating on file system paths
- trait objects (<code>dyn <i>TraitName</i></code>)
- structs and tuples for which the last field/item has one of these types

Values of types whose sizes are not known at compile time
can be stored on the heap by using the `Box` type.
This provides a fixed size way to refer to a value
that does not have a fixed size.
An example is returning an error struct whose specific type
is selected at run time based on the kind of error that occurs.
Another example is a recursive type such as linked list.

<aside>
Sometimes Rust chooses to stores `&str` values on the stack
but you cannot control that, so it's best to think of them
as always being on the heap.
</aside>

All code blocks are delimited by a pair of curly braces
and create a new scope.
Variables declared in each new scope add data to the stack
that is freed when that scope exits.
Many keywords have an associated block, including
`fn`, `if`, `loop`, `for`, and `while`.

The `Drop` trait can be implemented for any type
to specify code to execute (in the `drop` method)
when data of that type is dropped.

While it is not typically called directly,
`std::mem::drop` is a function that can be called to explicitly
free the memory owned by a variable before it goes out of scope.

The following table summarizes the options for
assigning a variable to another or
passing a variable an argument to a function.

| Goal               | Syntax      |
| ------------------ | ----------- |
| transfer ownership | `name`      |
| copy               | `name`      |
| borrow immutably   | `&name`     |
| borrow mutably     | `&mut name` |

The difference between the first and second cases is entirely
based on whether the type of the data implements the `Copy` trait.

Having all of these options requires considering the following questions
for every variable being passed to a function or assigned to another variable:

1. Do I want to transfer ownership? Often the answer is "no".
1. Does the receiver need to modify the value? Often the answer is "no".
1. Do I want to avoid making a copy for efficiency? Often the answer is "yes".
1. If I want to pass a copy, will one be made automatically
   or do I need to explicitly clone it?

Here are some examples that demonstrate ownership inside a single function.
See the [Strings](#strings) section for more detail
on the differences between the `String` and `&str` types.

```rust
fn main() {
    let a = 1;

    // Because a has a scalar type (fixed size) that implements the Copy trait,
    // this makes a copy of a and assigns that to b
    // rather than moving ownership from a to b.
    // Both a and b can then be used.
    let b = a;

    println!("{}", a); // 1
    println!("{}", b); // 1

    // This creates a String instance from a &str value.
    let c = String::from("test");

    // Because c is on the heap and String does not implement
    // the Copy trait, this moves ownership from c to d
    // and c can no longer be used.
    let d = c;

    //println!("c = {}", c); // error: value borrowed here after move
    println!("d = {}", d); // test

    // The Copy trait requires also implementing the Clone trait.
    // We can implement these traits manually, but that is more work.
    // The easiest way to implement them is with the derive attribute.
    #[derive(Clone, Copy, Debug)]
    struct Point2D {
        x: f64,
        y: f64
    }
    let e = Point2D { x: 1.0, y: 2.0 };

    // Since we implemented the Copy trait on the Point2D type,
    // this makes a copy of e and assigns it to f.
    // If we hadn't implemented the Copy trait,
    // this would move ownership from e to f.
    let f = e;
    println!("f = {:?}", f); // Point2D { x: 1.0, y: 2.0 }

    // This works because we implemented the Copy trait.
    // If we hadn't, we would get the error "value borrowed here after move".
    println!("e = {:?}", e);
}
```

Ownership of a value can be "borrowed" by any number of variables
by getting a reference to a value.
For example:

```rust
let e = Point2D { x: 1.0, y: 2.0 };
println!("e = {:?}", e); // Point2D { x: 1.0, y: 2.0 }

let f = &e; // an immutable borrow
let g = &e; // another immutable borrow
println!("f={:?}, g={:?}", f, g); // no errors here
```

Any variable whose type acts like a pointer to another type
(for example, `&variable` and smart pointers like
`Box`, `Rc`, and `Arc` that are described later)
can be dereferenced to get the value to which it points.
This can be done with the `*` operator.
Automatic dereferencing is performed by the dot operator
which is used to access a field or method of a type.
Automatic dereferencing also occurs when
a reference type is passed to a function or macro.
This is why we were able to print `f` above
without specifying `*f` which also works.
Automatic dereferencing makes code less "noisy" than it would otherwise be.

Borrowing does not transfer (also referred to as "move") ownership.
A borrowed variable can go out of scope without
freeing the memory associated with the original variable.

When a value is mutable and ownership is borrowed,
the compiler will report an error if the value is mutated
after the borrow is created and before the last use of the borrow.
This is because references expect the data they reference
to remain the same for the duration of the borrow.
For example:

```rust
let mut e = Point2D { x: 1.0, y: 2.0 };
let f = &e; // f borrows a reference rather than taking ownership
println!("f = {:?}", f); // works

// The next line mutates e.
// But f which is a mutable borrow is used after this line.
// So we get the error "cannot assign to `e.x` because it is borrowed".
e.x += 3.0;

println!("f = {:?}", f);
```

Often a borrow only needs to read a value (referred to as
an "immutable borrow").
Any number of immutable borrows can be created.
A mutable borrow allows changing a mutable value through the borrowed variable.
But a mutable borrow can only be created when
there will be no uses of already created immutable borrows
until after the last use of the mutable one.
Also, only one mutable borrow of a given variable can be active at a time.
The original variable cannot be accessed again
until after the last access of the borrowed variable.
For example:

```rust
#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

fn main() {
    let mut pt = Point2D { x: 1.0, y: 2.0 };
    let ref1 = &pt; // immutable borrow
    println!("{:?}", ref1); // Point2D { x: 1.0, y: 2.0 }

    // Can create and use any number of additional immutable borrows.
    let ref2 = &pt;
    println!("{:?}", ref2); // Point2D { x: 1.0, y: 2.0 }

    // Can use earlier immutable borrows again.
    println!("{:?}", ref1); // Point2D { x: 1.0, y: 2.0 }

    // Can create and use a mutable borrow.
    let ref3 = &mut pt;
    ref3.x = 3.0;
    println!("{:?}", ref3); // Point2D { x: 3.0, y: 2.0 }

    // Can't use immutable borrows created before a mutable borrow,
    // even if the mutable borrow isn't used to mutate the value.
    //println!("{:?}", ref1);
    // error: cannot borrow `pt` as mutable
    // because it is also borrowed as immutable

    // Can create and use new immutable borrows because
    // we are finished with the mutable borrow at this point.
    let ref4 = &pt;
    println!("{:?}", ref4); // Point2D { x: 3.0, y: 2.0 }
}
```

An alternative to borrowing is to clone data,
but doing this is often unnecessarily inefficient.
To clone a value, call its `clone` method
which is available for all types that implement the `Clone` trait.
A large number of built-in types implement this including
`String`, arrays, tuples, `Vec`, `HashMap`, and `HashSet`.
To enable cloning a struct, implement the `Clone` trait by adding
the `#[derive(Clone)]` attribute before it.

When variables whose values are on the stack are passed to functions,
the functions are given copies.
For example:

```rust
fn my_function(x: i32) {
    println!("{}", x); // 1
}

fn main() {
    let x = 1;
    my_function(x); // a copy of x is passed
    println!("{}", x); // 1
}
```

When variables (not references) of types that
do not implement the `Copy` trait are passed to functions,
copies are not made and ownership is transferred.
When the function exits, the data is freed.
The calling function can then no longer use variables that were passed.
For example:

```rust
#[derive(Clone, Copy, Debug)]
struct Point2D {
    x: f64,
    y: f64
}

fn take_point(p: Point2D) {
    println!("p = {:?}", p);
}

fn take_vector(v: Vec<u8>) {
    println!("v = {:?}", v);
}

fn main() {
    let pt = Point2D { x: 1.0, y: 2.0 };
    // A copy is passed because the Point2D type implements the copy trait.
    take_point(pt);
    // We can still use pt because ownership was not transferred.
    println!("pt = {:?}", pt);

    let numbers = vec![1, 2, 3];
    // Ownership is transferred because
    // the Vec type does not implement the Copy trait.
    take_vector(numbers); // error: borrow of moved value: `numbers`
    // We cannot use numbers here because ownership was transferred.
    println!("numbers = {:?}", numbers); // value borrowed here after move
}
```

We can fix the error above by changing the function to
return the parameter which returns ownership.
For example:

```rust
fn take_vector(v: Vec<u8>) -> Vec<u8> {
    println!("v = {:?}", v);
    v
}

fn main() {
    let numbers = vec![1, 2, 3];
    let new_numbers = take_vector(numbers);
    println!("new_numbers = {:?}", new_numbers);
}
```

When references to variables are passed to functions,
ownership is borrowed rather than being transferred.
When the function exits, the value is not freed
and the calling function can continue using it.
For example:

```rust
// We could pass the i32 argument by reference,
// but there is no benefit in doing that.
fn my_function(i: i32, s: &String) {
    println!("{}", i); // 1
    println!("{}", s); // "test"
}

fn main() {
    let i = 1;
    let s = String::from("test");
    my_function(i, &s);
    println!("{}", i); // 1
    println!("{}", s); // "test"
}
```

To allow a function to modify data passed to it by reference,
pass and receive mutable references.
For example:

```rust
fn my_function(i: &mut i32, s: &mut String) {
    println!("{}", i); // 1
    *i += 1; // dereferences and increments
    println!("{}", s); // "test"
    s.push_str(" more");
}

fn main() {
    let mut i = 1; // on stack
    let mut s = String::from("test"); // on heap
    // Even though i and s are mutable, the arguments to
    // my_function below only need to be marked as mutable
    // if that function requires them to be mutable.
    my_function(&mut i, &mut s);
    println!("{}", i); // 2
    println!("{}", s); // "test more"
}
```

A function can create a value and return it.
This transfers ownership to the caller
rather than freeing the data when the function exits.
For example:

```rust
#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

fn get_origin() -> Point2D {
    Point2D { x: 0.0, y: 0.0 }
}

fn get_string() -> String {
    String::from("test")
}

fn main() {
    let p = get_origin();
    println!("{:?}", p); // Point2D { x: 0.0, y: 0.0 }

    let s = get_string();
    println!("{}", s); // test
}
```

Early we said that memory for values allocated in a scope
is freed when the scope exits.
However, there is an exception to this
when ownership is transferred outside a block.
For example:

```rust
fn main() {
    let a; // set once inside the block that follows

    {
        // Allocate inside block.
        let b = String::from("test");

        // Move ownership to "a" which lives outside this block.
        a = b;

        // If the previous line is changed to
        // a = &b;
        // we get the error "`b` does not live long enough"
        // because a will no longer get ownership
        // and b will be freed at the end of the block.

        // Memory for b is not freed when this block exits
        // because b no longer owns it.
    }

    // "a" can be used here because its lifetime has yet not ended.
    println!("{}", a);
}
```

Closures are functions that capture values in their environment
so they can be accessed later when the function is executed.
Functions defined with the `fn` keyword are not closures.
Closures are defined as anonymous functions
with a parameter list written between vertical bars
which must be present even if there are no parameters.
While function parameter and return types must be specified,
these can be inferred for closures.

Here is an ownership example that is similar to the previous example,
but uses a closure:

```rust
fn main() {
    // This variable must be initialized in order to access it in the closure.
    // Because it is then modified in the closure, it must be mutable.
    let mut a = String::new(); // an empty string

    // This variable must be `mut` because it captures and mutates
    // the mutable variable "a" in its environment.
    let mut inner = | | { // no parameters
        let b = String::from("test");
        a = b; // Moves ownership from b to a.
    };

    inner();
    println!("{}", a); // test
}
```

Here is an example that concisely summarizes the ownership options
when passing a value to a function:

```rust
#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

fn take(pt: Point2D) {
    println!("in take, {:?}", pt);
}

fn take_and_return(pt: Point2D) -> Point2D {
    println!("in take_and_return, {:?}", pt);
    pt // returns ownership to caller
}

fn borrow_immutably(pt: &Point2D) {
    //pt.x = 3.0; // can't mutate
    println!("in borrow_immutably, {:?}", pt);
}

fn borrow_mutably(pt: &mut Point2D) {
    pt.x = 3.0; // can mutate
    println!("in borrow_mutably, {:?}", pt);
}

fn main() {
    let pt = Point2D { x: 1.0, y: 2.0 };
    take(pt); // in take, Point2D { x: 1.0, y: 2.0 }
    // Can't use pt after ownership was transferred and not returned.
    //println!("after take, pt = {:?}", pt);

    let mut pt = Point2D { x: 1.0, y: 2.0 };
    pt = take_and_return(pt); // in take_and_return, Point2D { x: 1.0, y: 2.0 }
    // Can use after this because ownership is returned.
    println!("{:?}", pt); // Point2D { x: 1.0, y: 2.0 }

    let pt = Point2D { x: 1.0, y: 2.0 };
    borrow_immutably(&pt); // in borrow_immutably, Point2D { x: 1.0, y: 2.0 }
    println!("{:?}", pt); // Point2D { x: 1.0, y: 2.0 }

    let mut pt = Point2D { x: 1.0, y: 2.0 };
    borrow_mutably(&mut pt); // in borrow_mutably, Point2D { x: 3.0, y: 2.0 }
    println!("{:?}", pt); // Point2D { x: 3.0, y: 2.0 }
}
```

Here are some _guidelines_ related to ownership of heap data.
While there are situations in which these do not apply,
often to improve performance,
following these will typically reduce ownership issues in your code.

1. Compound types should own their heap data rather than
   hold references to heap data owned elsewhere.
   For example, struct fields that are strings
   should use the `String` type instead of `&str`.
1. Pass references to heap data to functions
   rather than transferring ownership.
   For example, a parameter that accepts string data
   should have the type `&str` instead of `String`.
   An exception is when the function wants to take ownership
   in order to add items to a compound type that it owns.
1. In functions that create and return heap data,
   transfer ownership to the caller.
   For example, return `String` rather than `&str`.

## <a name="lifetimes">Lifetimes</a>

Lifetimes ensure that memory does not get freed
before a reference to it can use it.

<aside>
This is an advanced topic.
Be prepared to revisit this later and
read it multiple times before it sinks in.
Fortunately for most function definitions
there is no need to explicitly specify lifetimes.
</aside>

Lifetime annotations only apply to references.
All reference parameters and reference return types have a lifetime,
but in most cases the Rust compiler automatically determines them.
It does so using these three simple "lifetime elision" rules:

1. Each parameter with a reference type and no specified lifetime parameter
   is assigned a unique lifetime parameter.
1. If all the reference parameters now have the same lifetime
   and the return type is a reference,
   it is assigned the same lifetime as the reference parameters.
   This occurs when there is only one parameter with a reference type
   or when there is more that one,
   but they all have the same specified lifetime annotation.
1. If the function is a method
   (indicated by the first parameter having the name `self`),
   the `self` parameter is a reference (`&self` or `&mut self`),
   and the return type is a reference,
   the `self` reference and the return type reference
   are given the same lifetime parameter.
   The reason for this is that it is common for such a method
   to return a reference to part of `self`.

If after applying these rules to a function whose return type is a reference
the lifetime of the return type is still unknown,
the compiler outputs a "missing lifetime specifier".
This means that lifetimes must be explicitly specified.
Typically explicit lifetime parameters are only needed when
there are multiple reference parameters and the return type is a reference.
When this is the case,
often the same explicit lifetime parameter is added to
all of the reference parameters AND the return reference type.

Lifetime annotations used in function signatures are
declared by listing them in angle brackets just like generic types.
They are distinguish from generic types by beginning with a single quote.
Lifetime annotations appear in reference types
after the `&` and before type names.
Their names are typically a single letter such as "a".
Lifetime annotations only serve to indicate which items in a function signature
have the same lifetime (or at least as long as another), not an actual duration.

The following example illustrates a case
where lifetime annotations are required:

```rust
// The function signature below results in a
// "missing lifetime specifier" error.
// Additionally, the compiler says "explicit lifetime required"
// for s1, s2, and the return type.
// When more than one reference is passed to a function AND
// one of them can be returned, Rust requires lifetime specifiers.
//fn get_greater(s1: &str, s2: &str) -> &str {

// The next function signature includes lifetime specifiers.
fn get_greater<'a>(s1: &'a str, s2: &'a str) -> &'a str {
    if s1 > s2 {
        s1
    } else {
        s2
    }
}

fn get_surprise(s: &str) -> &str {
    return get_greater(s, "no soup for you");
}

fn main() {
    println!("{}", get_surprise("soup")); // soup
    println!("{}", get_surprise("bread")); // no soup for you
}
```

The name `static` is a reserved lifetime name.
It is the lifetime of `const` and `static` values
which live for the duration of the program.

To use more than one lifetime specifier in a function signature,
list them after the function name inside angle brackets separated by commas.
For example, `fn my_function<'a, 'b>(...)`.

To specify that lifetime `b` is at least as long as lifetime `a`,
use `fn my_function<'a, 'b: 'a>(...)`.

## <a name="enums">Enums</a>

Enums specify a list of allowed values referred to as "variants".
For example:

```rust
#[allow(dead_code)] // suppresses warning about not using all the variants
#[derive(Debug)]
enum PrimaryColor { Red, Green, Blue }

fn process_color(color: PrimaryColor) {
    println!("{:?}", color); // Green
}

fn main() {
    let color: PrimaryColor = PrimaryColor::Green;
    process_color(color);
}
```

Match expressions are similar to `switch` statements in other languages,
but they evaluate to a value.
Instead of `case` statements inside a `switch` they have "match arms".
There must be a match arm for every possible value
of the expression being matched, i.e. they must be exhaustive.
For example:

```rust
#[allow(dead_code)]
#[derive(Debug)]
enum PrimaryColor { Red, Green, Blue }

fn main() {
    use PrimaryColor::*; // brings all values into current scope
    let color = Red;

    // If a match arm for any PrimaryColor variant was missing
    // we would get a "non-exhaustive patterns" error.
    let item = match color {
        Red => "stop sign",
        Green => "grass",
        Blue => "sky",
    };
    println!("{}", item); // stop sign

    let item = match color {
        Blue => "sky",
        _ => "unknown" // wildcard handling all other values
    };
    println!("{}", item); // unknown
}
```

The [Error Handling](#error-handling) section
describes the `Option` and `Result` generic enums
that are provided by the standard library.
These contain variants that hold data,
which is something that enums in most other programming languages cannot do.
The `Option<T>` enum defines the variants `Some(T)` and `None`.
The `Result<T, E>` enum defines the variants `Ok(T)` and `Err<E>`.

Enum variants can hold many kinds of values.
For example:

```rust
use std::fmt::Debug;

#[derive(Debug)]
enum Demo {
    Empty,
    Single(String),
    TupleLike(String, i32, bool), // holds positional items
    StructLike{x: f64, y: f64}, // holds named items
}

fn main() {
    use Demo::*;
    let d1 = Empty;
    let d2 = Single(String::from("Hello"));
    let d3 = TupleLike(String::from("red"), 19, true);
    let d4 = StructLike{x: 1.2, y: 3.4};
    println!("{:?}, {:?}, {:?}, {:?}", d1, d2, d3, d4);
    // prints Empty, Single("Hello"),
    // TupleLike("red", 19, true),
    // StructLike { x: 1.2, y: 3.4 }
}
```

## <a name="error-handling">Error Handling</a>

Unlike many other programming languages,
Rust does not support throwing and catching exceptions.
Instead, functions that can fail typically
return the enum type `Option` or `Result`.

The `Option` enum has two variants,
`Some` which wraps a value and `None` which doesn't.
For example, a function that takes a vector and
returns the first element that matches some criteria
could return `Some` wrapping the element, or `None` if no match is found.
This is similar to the `Maybe` monad in Haskell.

The `Result` enum also has two variants,
`Ok` which wraps a value and `Err` which wraps an error description.
For example, a function that reads all the text in a file
could return `Ok` wrapping the text, or
`Err` wrapping a description of why reading the file failed.
This is similar to the `Either` monad in Haskell.

There are many ways to handle variants from these enum types.

1. Use a `match` statement.

   For example:

   ```rust
   #[derive(Debug)]
   pub enum MathError {
       DivisionByZero // used by divide2 function below
   }

   // Demonstrates returning a Option enum.
   fn divide1(numerator: f64, denominator: f64) -> Option<f64> {
       if denominator == 0.0 {
           None // means there is no result, but doesn't explain why
       } else {
           Some(numerator / denominator)
       }
   }

   // Demonstrates returning a Result enum.
   // Commented lines below show an alternative way
   // to describe the error using a string.
   //const DIV_BY_ZERO: &str = "divide by zero";
   fn divide2(numerator: f64, denominator: f64)
   -> Result<f64, MathError> {
   //fn divide2(numerator: f64, denominator: f64)
   //-> Result<f64, &'static str> {
       if denominator == 0.0 {
           Err(MathError::DivisionByZero)
           //Err(DIV_BY_ZERO)
       } else {
           Ok(numerator / denominator)
       }
   }

   fn main() {
       let n = 5.0;
       let d = 2.0;

       match divide1(n, d) { // returns an Option enum
           Some(result) => println!("{:.2}", result), // 2.50
           None => println!("divide by zero"),
       }

       match divide2(n, d) { // returns a Result enum
           Ok(result) => println!("{:.2}", result), // 2.50
           Err(e) => println!("{:?}", e),
           //Err(msg) => println!("{}", msg),
       }
   }
   ```

2. Use `if let` statement.

   We can replace the `match` statements
   in the previous example with the following:

   ```rust
   if let Some(result) = divide1(n, d) {
       println!("{:.2}", result);
   } else {
       println!("fail")
   }

   if let Ok(result) = divide2(n, d) {
       println!("{:.2}", result);
   } else { // With this approach we lose the detail in the Err variant.
       println!("fail")
   }
   ```

3. Use the `unwrap`, `unwrap_or`, `unwrap_or_default`,
   or `unwrap_or_else` method.

   These extract the value from an `Option` or `Result` enum.

   If the value is a `Some` or `Ok`, these succeed.

   If the value is a `None` or `Err`:

   - `unwrap` panics, exiting the program.  
      If the value is `Err`, the message it wraps will be output.
   - `unwrap_or` uses a specified value.
   - `unwrap_or_default` uses the default value for the type.  
     For custom types, this is specified by implementing the `Default` trait.
   - `unwrap_or_else` executes a closure passed to it
     to compute the value to use.

   For example, we can replace the `match` and `if let` statements above
   with the following:

   ```rust
   let result = divide1(n, d).unwrap();
   println!("{:.2}", result);

   // When d is zero this uses infinity for the value.
   let result = divide2(n, d).unwrap_or(std::f64::INFINITY);
   println!("{:.2}", result);
   ```

4. Use the `expect` method.

   This is nearly the same as the `unwrap` method.
   The only difference is that a custom error message can be supplied.
   We can replace the lines above with the following:

   ```rust
   let result = divide1(n, d).expect("division failed");
   println!("{:.2}", result);

   let result = divide2(n, d).expect("division failed");
   println!("{:.2}", result);
   ```

5. Use the `?` operator which is shorthand for the `try!` macro.

   This allows the caller to handle errors, similar to
   re-throwing an exception in other programming languages.
   It can be applied to functions that return a `Result` or `Option` enum.

   If the value is a `Some` or `Ok` variant then it is unwrapped and returned.
   If the value is a `None` or `Err` variant then it is passed through the
   `from` function (defined in the standard library `From` trait)
   in order to convert it to the return type of the function,
   and that is returned to the caller.

   The function in which this operator is used must
   declare the proper return type and return a value of that type.

   ```rust
   let result = divide1(n, d)?;
   println!("{:.2}", result);

   let result = divide2(n, d)?;
   println!("{:.2}", result);
   ```

   Uses of `?` can be chained in the same statement.
   For example, suppose the function `alpha` returns a `Result`
   whose wrapped value is an object with a method `beta`
   that returns a `Result` whose wrapped value is
   an object with a method `gamma` that returns a `Result`.
   We can get the value of this call sequences with the following:

   ```rust
   let value = alpha()?beta()?gamma()?;
   ```

   In functions that call multiple other functions
   that return `Result` instances with different types of errors
   and wish to return them to callers, consider making the return type
   `Result<SomeOkType, Box<dyn std::error::Error>>`.
   `Box` is needed to accommodate error values of different sizes
   because the size of the error value must be known at compile time.
   A `Box` is a smart pointer with a fixed size that points to another value.
   The `dyn` keyword performs dynamic dispatch to allow a value
   of any type that implements a given trait, `Error` in this case.
   Even the `main` function can be given this return type.

   In the case below the errors that can be returned
   (`std::io::Error` and `std::num::ParseIntError`)
   all implement the `std::error::Error` trait.
   This example comes from the Rustlings exercise `errorsn.rs`.

   ```rust
   fn read_and_validate(
       b: &mut dyn io::BufRead,
   ) -> Result<PositiveNonzeroInteger, Box<dyn std::error::Error>> {
       let mut line = String::new();
       // The read_line method can return Err(std::io::Error).
       b.read_line(&mut line)?;
       // The parse method can return Err(std::num::ParseIntError).
       let num: i64 = line.trim().parse()?;
       let answer = PositiveNonzeroInteger::new(num)?;
       Ok(answer)
   }
   ```

The following example demonstrates several approaches to error handling
for functions that can return multiple error types.
To experiment with this code, download it from the GitHub repo named
{% aTargetBlank "https://github.com/mvolkmann/rust-error-handling",
"rust-error-handling" %}.

```rust
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::fmt;
use std::fs::read_to_string;

// This is a custom error type.
// It enables callers that receive this kind of error
// to handle different error causes differently.
// These must implement the Error trait (done below) which requires
// implementing the Debug (done here) and Display (done below) traits.
#[derive(Debug)]
pub enum GetDogsError {
    BadFile(std::io::Error),
    BadJson(serde_json::error::Error),
}

// Make the variants of this enum directly available.
use GetDogsError::*;

// All of the Error trait methods have default implementations, so
// no body is required here, but we will implement the source method.
impl Error for GetDogsError {
    // Returns the wrapped error, if any.
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match *self {
            // The wrapped error type is implicitly cast to the trait object
            // type &Error because it implements the Error trait.
            BadFile(ref e) => Some(e),
            BadJson(ref e) => Some(e),
        }
    }
}

impl std::fmt::Display for GetDogsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BadFile(ref e) => write!(f, "bad file: {}", e),
            BadJson(ref e) => write!(f, "bad JSON: {}", e),
        }
    }
}

// The "From" trait converts values of one type to another.
// Having the following implementations enables
// using the ? operator in the get_dogs3 function below.
impl From<std::io::Error> for GetDogsError {
    fn from(other: std::io::Error) -> Self {
        BadFile(other)
    }
}
impl From<serde_json::error::Error> for GetDogsError {
    fn from(other: serde_json::error::Error) -> Self {
        BadJson(other)
    }
}

// This struct can be deserialized from JSON and serialized to JSON.
#[derive(Deserialize, Serialize, Debug)]
struct Dog {
    name: String,
    breed: String,
}

// Let's look at three versions of a function that
// reads a JSON file describing dogs and parses it
// to create a vector of Dog instances.

// With this version callers cannot easily distinguish between
// the two types of errors that can occur,
// std::io:Error from failing to read the file and
// serde_json::error::Error from failing to parse the JSON.
// This approach is fine when callers only need to
// know if an error occurred and print an error message.
fn get_dogs1(file_path: &str) -> Result<Vec<Dog>, Box<dyn Error>> {
    let json = read_to_string(file_path)?;
    let dogs: Vec<Dog> = serde_json::from_str(&json)?;
    Ok(dogs)
}

// If we have many functions with this same return type,
// Result<some-type, GetDogsError> {
// we can reduce the repetition by defining a type alias.
pub type MyResult<T> = std::result::Result<T, GetDogsError>;

// With this version callers can distinguish between the
// two types of errors by matching on the GetDogsError variants.
fn get_dogs2(file_path: &str) -> MyResult<Vec<Dog>> {
    match read_to_string(file_path) {
        Ok(json) => match serde_json::from_str(&json) {
            Ok(dogs) => Ok(dogs),
            Err(e) => Err(BadJson(e)),
        },
        Err(e) => Err(BadFile(e)),
    }
}

// This version takes advantage of the fact that
// GetDogsError implements the From trait for
// each of the kinds of errors that can occur.
// This enables using the ? operator because errors of those
// types will automatically be converted to the GetDogsError type.
fn get_dogs3(file_path: &str) -> MyResult<Vec<Dog>> {
    let json = read_to_string(file_path)?;
    let dogs: Vec<Dog> = serde_json::from_str(&json)?;
    Ok(dogs)
}

// If the main function has this return type, it can use the ? operator.
//fn main() -> Result<(), Box<dyn Error>> {
fn main() {
    let file_path = "./dogs.json";

    /*
    // With the first approach we can easily detect
    // that an error has occurred.
    if let Ok(dogs) = get_dogs1(file_path) {
        dbg!(dogs);
    } else {
        eprintln!("failed to get dogs, but don't know why");
    }
    */

    /*
    // But handling different kinds of errors differently is messy.
    match get_dogs1(file_path) {
        Ok(dogs) => println!("{:?}", dogs),
        Err(e) => {
            if let Some(e) = e.downcast_ref::<std::io::Error>() {
                eprintln!("bad file: {:?}", e);
            } else if let Some(e) =
                e.downcast_ref::<serde_json::error::Error>() {
                eprintln!("bad json {:?}", e);
            } else {
                eprintln!("some other kind of error");
            }
        }
    }
    */

    // With the second and third approaches it is much easier
    // to handle different kinds of errors differently.
    //match get_dogs2(file_path) {
    match get_dogs3(file_path) {
        Ok(dogs) => println!("{:?}", dogs),
        Err(BadFile(e)) => eprintln!("bad file: {}", e),
        Err(BadJson(e)) => eprintln!("bad json: {}", e),
    }
}
```

The `quick-error` crate makes implementing custom error types easier.
Here are the changes required to use this approach which also requires
adding `quick-error` as a dependency in `Cargo.toml`.
Note that we no longer need to manually implement the
`Error`, `Display`, and `From` traits for our custom error enum.
To experiment with this code, see the `quick_error` branch of the
{% aTargetBlank "https://github.com/mvolkmann/rust-error-handling",
"rust-error-handling" %} GitHub repo.

```rust
#[macro_use]
extern crate quick_error;

use serde::{Deserialize, Serialize};
use std::fs::read_to_string;

quick_error! {
    #[derive(Debug)]
    pub enum GetDogsError {
        BadFile(e: std::io::Error) {
            display("bad file {}", e)
            from()
        }
        BadJson(e: serde_json::error::Error) {
            display("bad json {}", e)
            from()
        }
    }
}
```

## Generics

Rust makes heavy use of generic types.
They enable implementing functions, structs, and traits that
operate on various types of data instead of only specific types.

Generic types are declared inside angle brackets.
They can specify one or more traits that must be implemented by
concrete types in order to use them in place of the type parameters.
The sections on functions, structs, and traits
contain many examples of using generic types.

## Built-in Types

Rust provides many built-types.
The following diagram summarizes them.

<img alt="Rust Built-in Data Types" style="width: 70%"
  src="/blog/assets/rust-types.png?v={{pkg.version}}"
  title="Rust Built-in Data Types">

### <a name="scalar-types">Built-in Scalar Types</a>

Rust defines many scalar (primitive) types which can be categorized as
boolean, character, integer (6 kinds), or floating point (2 kinds).

The boolean type name is `bool`.
Its only values are `true` and `false`.

The character type name is `char`.
Literal values are surrounded by single quotes.
Its values are Unicode values that each occupy four bytes,
regardless of whether four bytes are actually needed to represent them.
This gives the values a known size at compile time.

The signed integer type names are `i{n}` where `{n}`
is the number of bits which can be 8, 16, 32, 64, 128, or `size`.
The `isize` type matches either `i32` or `i64`
depending on the current processor architecture.
The default type for literal integers is `i32` regardless of the processor.

The unsigned integer types are the same, but start with `u` instead of `i`.
The `usize` type is typically used to index into
collections such as arrays and vectors.

Literal number values can end with these type names to make their type explicit.
For example, the following variable declarations are equivalent:

```rust
let number: i8 = 19;
let number = 19i8;
```

Literal integer values can use the underscore character to separate
thousands, millions, and so on. For example,
the population of the U.S. in 2020 was approximately 330_676_544.

Hex literal values begin with `0x`,
octal literal values begin with `0o`, and
binary literal values begin with `0b`.

The floating point type names are `f{n}` where `{n}` is 32 or 64.
The default type for literal floats is `f64`
regardless of the current processor.
Literal floating point values must include a decimal point
to avoid being treated as integer values,
but it is acceptable to have no digits after the decimal point.
This means that `123.` is treated the same as `123.0`.

The "unit type" represents not having a value,
like `void` in other languages.
It can be thought of as an enum with a single variant which is written as `()`.
It is the return value of functions that do not return a value.
It is also what the `Result` enum `Ok` variant
wraps when there is nothing to return.
The unit type value can follow the `=>` in a `match` arm to take no action.

Rust allows adding methods to any type, even built-in, scalar types.
The following example shows added a method to the `i32` type:

```rust
trait Days<T> {
    fn days_from_now(self) -> String;
}

impl Days<i32> for i32 {
    fn days_from_now(self: i32) -> String {
        let s = match self {
            -1 => "yesterday",
            0 => "today",
            1 => "tomorrow",
            _ => if self > 0 { "future" } else { "past" }
        };
        s.to_string()
    }
}

fn main() {
    let days: i32 = -1;
    println!("{}", days.days_from_now()); // yesterday
    println!("{}", 0.days_from_now()); // today
    println!("{}", 1.days_from_now()); // tomorrow
    println!("{}", 2.days_from_now()); // future
    println!("{}", (-2).days_from_now()); // past
}
```

Here is another way to implement this that works on values
of any type that implements the traits `Eq`, `Ord`, and `From<i8>`
which all the number types do:

```rust
use std::cmp::{ Eq, Ord };

trait Days {
    fn days_from_now(self) -> &'static str;
}

impl<T: Eq + Ord + From<i8>> Days for T {
    fn days_from_now(self) -> &'static str {
        // Can't use match with Self::from(n) values.
        if self == Self::from(0) {
            "today"
        } else if self == Self::from(-1) {
            "yesterday"
        } else if self == Self::from(1) {
            "tomorrow"
        } else if self > Self::from(0) {
            "future"
        } else {
            "past"
        }
    }
}

fn main() {
    let days: i32 = -1;
    println!("{}", days.days_from_now()); // yesterday
    println!("{}", 0.days_from_now()); // today
    println!("{}", 1.days_from_now()); // tomorrow
    println!("{}", 2.days_from_now()); // future
    println!("{}", (-2).days_from_now()); // past
}
```

The [Macros](#macros) section shows one more approach.

### Primitive Compound Types

Rust defines three primitive compound types which are tuple, array, and slice.
These are distinct from the collection types
that are described in the next section.

A tuple is a fixed-length list of values that can be of different types.
The maximum length is 12.
The syntax for a tuple type is `(type1, type2, ...)`.
The syntax for a tuple value is `(value1, value2, ...)`.
Individual values can be accessed by index or destructuring.
It is not possible to iterate over the elements of a tuple.

For example:

```rust
let t = (1, 2, 3, 4);
let (v1, v2, ..) = t; // destructuring; .. means ignore remaining values
println!("{} {}", v1, v2); // 1 2
```

An array is a fixed-length list of values that have the same type.
Once created, it cannot grow or shrink.
The syntax for an array type is `[type; length]`.
The syntax for an array value is `[value1, value2, ...]`.
For example:

```rust
let a = [1, 2, 3, 4]; // inferred type is [i32; 4]
let [v1, v2, ..] = a; // destructuring; .. means ignore remaining values
println!("{} {}", v1, v2); // 1 2

let rgb = ["red", "green", "blue"];
// A Rust string is a "compound collection", covered later.
println!("{:?}", rgb); // ["red", "green", "blue"]

// A literal value (7 here) can be used as a type.
let sevens = [7; 5]; // same as [7, 7, 7, 7, 7]
println!("{:?}", sevens); // [7, 7, 7, 7, 7]
```

Elements of an array can be accessed using
square brackets and zero-based indexes.
For example, `rgb[1]` is "green".

Rust provides the `Vec` (vector) type for creating
variable-length lists of values that have the same type.
These can be used in place of arrays when the number of values varies.
Operating on an array or `Vec` often requires
obtaining an [`Iterator`](#iterators).
For example, that is where the methods `map`, `filter`, and `fold` are found.

A slice is a borrowed reference to a contiguous subset of a collection
that is represented by pointer and a length.
These are described in more detail in the [Slices](#slices) section.

### Collections

TODO: Continue review here.

Rust defines many kinds of collections that hold a variable number of values.
These include strings and collections in the `std::collections` namespace.

The `std::collections` namespace defines the following sequence types:

- `Vec`: a resizable, ordered array of values of the same type
  where items can be efficiently adding and removing items at the end
- `VecDeque`: like a `Vec`, but items
  can also be efficiently adding and removing items at the beginning
- `LinkedList`: like a `VecDeque`, but can be efficiently split
  which enables efficiently adding and removing items in the middle

The `std::collections` namespace defines the following map types:

- `HashMap`: a collection of key/value pairs with efficient value lookup by key
  where keys and values can be any type
- `BTreeMap`: like a `HashMap`, but sorted by key enabling efficient
  retrieval of values corresponding to the smallest key, largest key,
  closest key that is smaller or larger than some key value,
  or range of keys

The `std::collections` namespace defines the following set types:

- `HashSet`: a collection of unique values with
  efficient determination of whether a given value is a member
- `BTreeSet`: similar to storing only the keys in a `BTreeMap`

The `std::collections` namespace defines one more collection type
that doesn't fall into the previous categories:

- `BinaryHeap`: implements a priority queue where
  only the highest priority item is accessible

#### <a name="strings">Strings</a>

Strings are collections of UTF-8 encoded characters
stored as a `Vec` of `u8` byte values.
Literal values are surrounded by double quotes.
Strings are more difficult to work with in Rust than in other languages.
Rust trades simplicity here for better
performance, memory management, and concurrency.

There are two kinds of strings in Rust.
The language defines the "string slice" type `&str`
and the standard library defines the `String` type.
A `&str` value is a reference (or pointer) to data that includes
the length of the string and a pointer to where the character data is stored,
which can be on the stack or the heap.
A `String` value is a {% aTargetBlank "#smart-pointers", "smart pointer" %}
that holds a pointer to the character data on the heap,
a length, and a capacity.
The capacity is the length to which the character data can grow
before additional space must be allocated (handled automatically).
A `&str` value can be a view into the characters of `String`,
sharing its character data.

While one reason to choose the `String` type over the `&str` type is so
the length can change, the most common reason is so a variable can "own" it.
Values of type `&str` cannot be owned by a variable.

Literal characters (just one) are surrounded by single quotes
and have the type `char`.
Literal strings (zero or more characters) are surrounded by double quotes
and have the type `&str`.

Here is a summary of the types that can be used to represent strings:

| Type          | Description                                                             |
| ------------- | ----------------------------------------------------------------------- |
| `str`         | rarely used directly                                                    |
| `&str`        | reference to an immutable string slice; commonly used                   |
| `&mut str`    | rarely user; some `str` methods use it                                  |
| `String`      | immutable if declared with `let`; mutable with `let mut`; commonly used |
| `&String`     | reference to an immutable string; typically `&str` is used instead      |
| `&mut String` | reference to a mutable string; commonly used                            |

So the string types most frequently used are
`&str`, `String`, and `&mut String`.

To get a `&str` from a `String` in the variable `s`,
use `&s` or `s.as_str()` which are equivalent.

To get a `String` from a `&str` in the variable `s`,
use `s.to_string()`, `String::from(s)`, or `s.to_owned()` which are equivalent.
Actually, `to_string` calls `String::from` which calls `to_owned`.
These calls are inlined so they all have the same performance.

When a `String` reference is passed to a function that expects a `&str`
it is automatically coerced to that type.
This is because `String` implements the `Deref` trait with a `Target` of `str`.
For more on this, see the [Smart Pointers](#smart-pointers) section.
For example:

```rust
fn my_function(s: &str) {
    println!("{}", s); // "test"
}

fn main() {
    let s = String::from("test");
    my_function(&s); // ownership is not transferred
    println!("{}", s); // "test"
}
```

To create a `String` from multiple values of types that
implement the `Display` trait, use `format!(fmt_string, v1, v2, ...)`.

Here are examples of declaring, creating, and passing various kinds of strings:

```rust
fn demo(s1: &str, s2: String, s3: &String, s4: &mut String) {
  println!("s1 = {}", s1); // one
  println!("s2 = {}", s2); // two
  println!("s3 = {}", s3); // three
  println!("s4 = {}", s4); // four alpha
  //s3.push_str(" beta"); // error: cannot borrow `*s3` as mutable
  s4.push_str(" beta"); // only s4 can be mutated
}

fn main() {
    let s1 = "one"; // type is inferred as &str
    let s2: String = "two".to_string();
    let s3: String = String::from("three");
    let mut s4: String = "four".to_owned();
    s4.push_str(" alpha");

    demo(s1, s2, &s3, &mut s4);

    println!("main: s1 = {}", s1); // one
    //println!("main: s2 = {}", s2); // error: borrow of moved value
    println!("main: s3 = {}", s3); // three
    println!("main: s4 = {}", s4); // four alpha beta
}
```

Typically variables and parameters for strings that
do not require mutation should use the type `&str`
and those that do should use the type `&mut String`.

In the tables below, assume the following variable types:

- `c` holds a `char` value
- `r` holds a `std::ops::Range` value
- `s` and `t` hold `&str` values
- `u`, `v`, and `w` hold `String` values
- `z` holds a `char` or `&str` value

Everywhere `c` is used, a literal character can be used in its place.
Everywhere `s` and `t` are used, a literal string can be used in their place.

Here are operations on the `str` type:

| Syntax                                                  | Operation                            |
| ------------------------------------------------------- | ------------------------------------ |
| create                                                  | `"text in double quotes"`            |
| get substring                                           | `s[start..end]` (1)                  |
| get iterator over Unicode characters                    | `s.chars()`                          |
| get `char` at index                                     | `s.chars().nth(index)` (2)           |
| determine if contains                                   | `s.contains(z)`                      |
| determine if ends with                                  | `s.ends_with(z)`                     |
| determine if starts with                                | `s.starts_with(z)`                   |
| get substring                                           | `s.get(r)` (3)                       |
| get length                                              | `s.len()`                            |
| get iterator over lines                                 | `s.lines()`                          |
| parse into another type such as specific number type    | `let v = s.parse::<T>()` (4)         |
| create `String` that repeat n times                     | `s.repeat(n)`                        |
| replace all occurrences of z1 with z2                   | `s.replace(z1, z2)`                  |
| replace first n occurrences of z1 with z2               | `s.replacen(z1, z2, n)`              |
| split on a character                                    | `s.split(c)` returns an iterator (5) |
| split on a character n times (last contains rest)       | `s.splitn(n, c)` returns an iterator |
| split at index                                          | `s.split_at(n)` returns tuple        |
| split on any amounts of whitespace                      | `s.split_whitespace()`               |
| remove prefix                                           | `s.strip_prefix(z)` returns `Option` |
| remove suffix                                           | `s.strip_suffix(z)` returns `Option` |
| convert `&str` to `String`                              | `s.to_string()`                      |
| convert `&str` to `String`                              | `String::from(s)`                    |
| convert `&str` to `String`                              | `s.to_owned()`                       |
| get lowercase `String`                                  | `s.to_lowercase()`                   |
| get uppercase `String`                                  | `s.to_uppercase()`                   |
| get `&str` with leading and trailing whitespace removed | `s.trim()`                           |
| get `&str` with trailing whitespace removed             | `s.trim_end()`                       |
| get `&str` with leading whitespace removed              | `s.trim_start()`                     |

1. `start` is inclusive and `end` is exclusive.
1. The `chars` method can be used to iterate over the characters in a string.
   The `nth` method returns a `Option` object because
   the string may be shorter than the index.
   See the [Error Handling](#error-handling) section
   for ways to get the `char` from it.
1. This returns an `Option` object rather than panic on bad indexes.
1. The `::<T>` syntax is called "turbofish" and is described later.
1. The `collect` method can be called on this iterator to get a `Vec<&str>`.

Many `String` methods operate on byte indexes.
This works for strings that only contain ASCII characters,
but is error-prone for strings that contain multi-byte Unicode characters.
Methods on the `str` type are better for working with Unicode characters.

Here are operations on the `String` type.
Note that methods that modify the value
require the `String` to be mutable (`mut`).

| Operation                                | Syntax                           |
| ---------------------------------------- | -------------------------------- |
| create empty                             | `String::new()`                  |
| create from `&str` #1                    | `String::from(s)`                |
| create from `&str` #2                    | `s.to_string()`                  |
| create from multiple `&str` #1           | `let u = [s, t].concat();`       |
| create from multiple `&str` #2           | `let u = format!("{}{}", s, t);` |
| create from `String` and `&str` (1)      | `let u = v + s;`                 |
| create from multiple `String` values (2) | `let u = v + &w;`                |
| get `&str` without copying               | `let s = &t;`                    |
| get `&str` without copying               | `let s = t.as_str();`            |
| append `char`                            | `u.push(c)`                      |
| append `&str`                            | `u += s;`                        |
| append `&str`                            | `u.push_str(s)`                  |
| append `String`                          | `u += v;`                        |
| insert `char`                            | `u.insert(index, c)`             |
| insert `&str`                            | `u.insert_str(v, s)`             |
| get substring                            | `s.get(r)` same as for `&str`    |
| get substring from index to end          | `s[start..]`                     |
| get substring from beginning to index    | `s[..end]`                       |
| get substring where end is exclusive     | `u[start..end]`                  |
| get substring where end is inclusive     | `u[start..=end]`                 |
| get `char` at index                      | `&u.chars().nth(index)`          |
| get length                               | `u.len()`                        |
| remove and return last character         | `u.pop()`                        |

1. The `String` `v` here must precede the `&str` s.
1. All `String` values on the right of `=` after the first
   must be preceded by `&` which converts them to `&str`.

Here's an example of working with Unicode characters:

```rust
let my_string = "Santa 🎅 🎄";
// Get the Santa emoji character.
let letter = &my_string.chars().nth(6);

// Approach #1
if let Some(c) = letter {
    println!("letter is {}", c); // 🎅
}

// Approach #2
match letter {
    Some(c) => println!("letter is {}", c), // 🎅
    None => () // ignores when string is shorter
}
```

Here's a function that returns the first word in a string
that might contain non-ASCII Unicode characters:

```rust
fn first_word(s: &str) -> &str {
    // The chars method returns an iterator.
    // The enumerate method on the iterator returns a new iterator over
    // tuples of indexes and values in the iterator on which it is called.
    for (i, letter) in s.chars().enumerate() {
        if letter == ' ' {
            // Return all the characters up to,
            // but not including the space.
            return &s[..i];
        }
    }
    s
}

fn main() {
    let s = String::from("foo bar baz");
    let word = first_word(&s);
    println!("{}", word); // foo

    let s = String::from("onelongword");
    let word = first_word(&s);
    println!("{}", word); // onelongword

    println!("{}", first_word("")); // empty string
}
```

In many programming languages strings are immutable.
To make a change you create a new string
and assign it back to the same variable.
In Rust the `&mut str` type can be used for this.
If it is desirable to modify a string in place, perhaps for
performance reasons, the `mut String` type can be used instead.
For example:

```rust
let mut s1 = "first";
s1 = "second";

let mut s2 = String::from("first");
s2.replace_range(.., "second"); // range .. is the entire string
```

#### Vectors

A vector is represented by the `Vec` generic type.
It is a smart pointer that holds a pointer to the data on the heap,
a length, and a capacity.
The capacity is the length to which the data can grow
before additional space must be allocated (handled automatically).

Here is a summary of commonly used `Vec` methods:

| Operation                             | Syntax                         |
| ------------------------------------- | ------------------------------ |
| create empty                          | `Vec::new()`                   |
| create empty with capacity            | `Vec::with_capacity(capacity)` |
| create with items                     | `vec![item1, item2, ...]` (1)  |
| append other                          | `v.append(&other_vector)`      |
| search for index of value when sorted | `v.binary_search(value)`       |
| remove all items                      | `v.clear()`                    |
| determine if contains value           | `v.contains(value)`            |
| remove consecutive repeated items     | `v.dedup()`                    |
| remove consecutive repeated items     | `v.dedup_by(fn)` (2)           |
| remove consecutive repeated items     | `v.dedup_by_key(fn)` (3)       |
| get first value                       | `v.first()`                    |
| insert at index                       | `v.insert(index, value)`       |
| determine if empty                    | `v.is_empty()`                 |
| get iterator                          | `v.iter()`                     |
| get iterator that allows mutating     | `v.iter_mut()`                 |
| get last value                        | `v.last()`                     |
| get length                            | `v.len()`                      |
| remove and return last item           | `v.pop()`                      |
| add to end                            | `v.push(value)`                |
| remove and return item at index       | `v.remove(index)`              |
| remove item with value                | `v.remove_item(index)`         |
| reverse in place                      | `v.reverse()`                  |
| sort in place                         | `v.sort()`                     |
| sort in place                         | `v.sort_by(fn)` (2)            |
| sort in place                         | `v.sort_by_key(fn)` (3)        |
| replace items                         | `v.splice(range, iter)` (4)    |
| swap items at indexes                 | `v.swap(index1, index2)`       |

1. `vec!` is a macro, not a method.
1. `fn` is passed two values and returns
   a `bool` indicating if the first should be removed.
1. `fn` is passed a value and returns
   a computed value to be used for comparison.
1. `range` specifies indexes to remove and
   `iter` specifies items to insert in their place.

Also see the [Iterator](#iterators) methods
that include `filter`, `map`, `fold` (like `reduce` in JavaScript),
`min`, `max`, `sum`, `product`, and more.

Here is an example of creating and operating on a vector:

```rust
fn main() {
    let scores = vec![70, 90, 85, 100];

    for score in &scores {
        let grade = match score {
            90..=100 => 'A',
            80..=89 => 'B',
            70..=79 => 'C',
            60..=69 => 'D',
            _ => 'F',
        };
        println!("{}", grade);
    }

    let total: u32 = scores.iter().sum();
    let average = total as f32 / total as f32;
    println!("average = {:.1}", average);
}
```

When a `Vec` reference is passed to a function that expects a slice reference
it is automatically coerced to that type.
This is because `Vec<T>` implements the `Deref` trait with a `Target` of `[T]`.
For more on this, see the [Smart Pointers](#smart-pointers) section.
For example:

```rust
fn average_i32(numbers: &[i32]) -> f32 {
    // The "sum" generic function doesn't know what its output type should be,
    // so that must be specified.  One approach to specify it as the type of a variable that will hold return value.
    // For example:
    // let numerator: i32 = numbers.iter().sum();
    // numerator as f32 / numbers.len() as f32
    // Another approach is to use the turbofish qualifier which is
    // ideal for specifying the type in the middle of a longer expression.
    // For example:
    numbers.iter().sum::<i32>() as f32 / numbers.len() as f32
}

fn main() {
    let scores = vec![70, 90, 85, 100];

    // Print average of all scores.
    println!("average = {:.1}", average_i32(&scores)); // 86.2

    // Print average of all scores except the first.
    println!("average = {:.1}", average_i32(&scores[1..])); // 91.7
}
```

The size of items in a vector must all be the same
and must be known at compile time.
One way to hold items with varying sizes to
place each item in a `Box` and place these in the vector.
For example, this approach can be used to can create
a vector of items that all implement a given trait.

```rust
use std::fmt::Debug;

fn main() {
    let mut items: Vec<Box<dyn Debug>> = Vec::new();

    items.push(Box::new(true)); // bool
    items.push(Box::new('X')); // char
    items.push(Box::new(19)); // i32
    items.push(Box::new(1.23)); // f64
    items.push(Box::new("hello")); // &str
    items.push(Box::new(String::from("world"))); // String
    items.push(Box::new([1, 2, 3])); // array
    items.push(Box::new((1, 2, 3))); // tuple

    for item in items {
        println!("{:?}", item);
    }
}
```

#### Sets

A set is a collection of unique values.

The `std::collections` namespace defines the `HashSet` generic type.

Here is a summary of commonly used `HashSet` methods:

| Operation                                     | Syntax                     |
| --------------------------------------------- | -------------------------- |
| create empty                                  | `let set = HashSet::new()` |
| remove all items                              | `set.clear()`              |
| determine if an item is present               | `set.contains(item)`       |
| get iterator over items not in another set    | `set1.difference(set2)`    |
| get a reference to an item with a given value | `set.get(value)`           |
| insert an item                                | `set.insert(item)` (1)     |
| get iterator over common items in two sets    | `set1.intersection(set2)`  |
| determine if empty                            | `set.is_empty()`           |
| determine if a subset of another set          | `set1.is_subset(set2)`     |
| determine if a superset of another set        | `set1.is_superset(set2)`   |
| get iterator over all items                   | `set.iter()`               |
| get number of items (length)                  | `set.len()`                |
| remove a value                                | `set.remove(value)`        |
| remove elements that do not match a predicate | `set.retain(pred_fn)` (2)  |
| get iterator over unique items in two sets    | `set1.union(set2)`         |

1. returns a `bool` indicating if the item was added; `false` if already present
1. modifies in place

Here is an example of creating and using a `HashSet`
containing `String` elements.
Using the `String` type rather than `&str` allows the `HashSet`
to own the values so they have the same lifetime as the `HashSet`
which is generally desirable.

```rust
use std::collections::HashSet;

fn main() {
    // Element type is inferred from what is inserted.
    let mut colors = HashSet::new();
    colors.insert("red");
    colors.insert("green");
    colors.insert("blue");

    println!("{:?}", colors); // {"red", "green", "blue"}
    println!("{}", colors.len()); // 3
    println!("{}", colors.contains("green")); // true
    println!("{}", colors.contains("orange")); // false

    colors.remove("green");
    println!("{:?}", colors); // {"blue", "red"}

    for color in &colors {
        println!("{}", color); // blue then red
    }
}
```

Here is an example of creating and using a `HashSet`
containing `struct` elements:

```rust
use std::collections::HashSet;

#[derive(Debug, Eq, Hash, PartialEq)]
struct Dog {
    name: String,
    breed: String
}
impl Dog {
    // This is a constructor function where
    // the name "new" is used by convention.
    // The type "Self" here refers to "Dog".
    fn new(name: &str, breed: &str) -> Self {
        Self {
            name: name.to_string(),
            breed: breed.to_string()
        }
    }
}

fn main() {
    let mut dogs = HashSet::new();
    dogs.insert(Dog::new("Maisey", "Treeing Walker Coonhound"));
    dogs.insert(Dog::new("Ramsay", "Native American Indian Dog"));
    dogs.insert(Dog::new("Oscar", "German Shorthaired Pointer"));
    dogs.insert(Dog::new("Comet", "Whippet"));
    println!("dogs = {:#?}", dogs);

    for dog in &dogs {
        println!("{:?}", dog);
    }

    let comet = Dog::new("Comet", "Whippet");
    let spot = Dog::new("Spot", "Beagle");
    println!("{:?}", dogs.contains(&comet)); // true
    println!("{:?}", dogs.contains(&spot)); // false
}
```

#### Maps

Maps are collections of key/value pairs.
Keys can be any type, but they must all be the same type.
The same is true for values.

The `std::collections` namespace defines the `HashMap` generic type.

Here is a summary of commonly used `HashMap` methods:

| Operation                                                 | Syntax                                    |
| --------------------------------------------------------- | ----------------------------------------- |
| create empty                                              | `let map = HashMap::new()`                |
| remove all items                                          | `map.clear()`                             |
| determine if a key is present                             | `set.contains_key(key)`                   |
| get value associated with a key                           | `map.get(key)` (1)                        |
| get mutable value associated with a key                   | `map.get_mut(key)` (1)                    |
| insert key/value pair                                     | `map.insert(key, value)` (2)              |
| determine if empty                                        | `map.is_empty()`                          |
| get iterator over key/value pairs as tuples               | `map.iter()`                              |
| get mutable iterator over key/value pairs as tuples       | `map.iter_mut()`                          |
| get iterator over keys                                    | `map.keys()`                              |
| get number of key/value pairs (length)                    | `map.len()`                               |
| get value for key, inserting default value if not present | `map.entry(key).or_insert(initial-value)` |
| remove a key/value pair                                   | `map.remove(key)` (2)                     |
| remove key/value pairs that do not match a predicate      | `map.retain(pred_fn)` (3)                 |
| get iterator over values                                  | `map.values()`                            |
| get iterator over mutable values                          | `map.values_mut()`                        |

1. returns an `Option`
1. returns `Some(old_value)` if key was already present and `None` otherwise
1. modifies in place

Here is an example of creating and using a `HashMap`
containing `String` keys and `i32` values.
Using the `String` type for keys rather than `&str` allows the `HashMap`
to own the values so they have the same lifetime as the `HashMap`
which is generally desirable.

```rust
use std::collections::HashMap;

fn get_shortest_v1(months: &HashMap<String, i8>) -> Option<&str> {
    let mut shortest: i8 = std::i8::MAX;
    let mut name: Option<&str> = None;
    for (key, &val) in months {
        if val < shortest {
            name = Some(key);
            shortest = val;
        }
    }
    name
}

fn get_shortest_v2(months: &HashMap<String, i8>) -> Option<&str> {
      // The HashMap iter method returns
      // an iterator over (key, value) tuples.
      months.iter()
        // The Iter min_by_key method is passed a closure and returns
        // an Option that wraps the smallest value returned by the closure.
        // The closure is passed each tuple, one at a time,
        // and returns the value to be compared.
        // In this case it is the second element in the tuple (at index 1).
        .min_by_key(|p| p.1)
        // The Option map is called on an Option of one type
        // and returns an Option of another.
        // In this case it is called on an Option wrapping a (key, value) tuple
        // and returns an Option wrapping just the key (at index 0).
        .map(|p| p.0.as_str())
}

fn main() {
    let mut days_in_month: HashMap<String, i8> = HashMap::new();
    days_in_month.insert("January".to_string(), 31);
    days_in_month.insert("February".to_string(), 28);
    days_in_month.insert("March".to_string(), 31);
    days_in_month.insert("April".to_string(), 30);

    println!("daysInMonth = {:#?}", &days_in_month);

    for (month, days) in &days_in_month {
        println!("There are {} days in {}.", days, month);
    }

    println!("entries = {:?}", days_in_month.len()); // 4

    println!("days in March = {:?}", days_in_month.get("March").unwrap());

    // get_shortest_v1 and get_shortest_v2 return the same value.
    if let Some(shortest) = get_shortest_v1(&days_in_month) {
        println!("shortest = {}", shortest); // February
    }

    days_in_month.remove("February");
    println!("entries = {:?}", days_in_month.len()); // 3
    println!("days in February = {:?}", days_in_month.get("February"));

    let month = "April";
    match days_in_month.get(month) {
        Some(days) => println!("There are {} days in {}.", days, month),
        None => println!("No data found for {}.", month)
    }
}
```

Here is an example of creating and using a `HashMap`
with `String` keys and `struct` values:

```rust
use std::collections::HashMap;

#[derive(Debug, Eq, Hash, PartialEq)]
struct Dog {
    name: String,
    breed: String
}
impl Dog {
    fn new(name: &str, breed: &str) -> Self {
        Self {
            name: name.to_string(),
            breed: breed.to_string()
        }
    }
}

fn main() {
    // Key and value types are inferred from what is inserted.
    let mut dogs = HashMap::new();

    // This function must be a closure so it can access dogs.
    let mut add_dog = |name: &str, breed: &str| {
        dogs.insert(name.to_string(), Dog::new(name, breed));
    };

    add_dog("Maisey", "Treeing Walker Coonhound");
    add_dog("Ramsay", "Native American Indian Dog");
    add_dog("Oscar", "German Shorthaired Pointer");
    add_dog("Comet", "Whippet");

    println!("dogs = {:#?}", dogs);
    println!("entries = {:?}", dogs.len()); // 4
    println!("Comet = {:#?}", dogs.get("Comet").unwrap());
    dogs.remove("Comet");
    println!("entries = {:?}", dogs.len()); // 3

    let name = "Oscar";
    match dogs.get(name) {
        Some(dog) => println!("found {:#?}.", dog),
        None => println!("No dog named {} found.", name)
    }

    for (name, dog) in &dogs {
        println!("{} is a {}.", name, dog.breed);
    }
}
```

For a longer example of using a `HashMap` and the `entry` method,
see {% aTargetBlank "https://github.com/mvolkmann/rust-poker", "rust-poker" %}.
The file `src/lib.rs` defines the `evaluate` method of the `Hand` struct.
This uses the `entry` method to determine the number of cards in the hand
from each suit and rank.
This repository also serves as a good example of implementing
doc tests, unit tests, and integration tests.

## Slices

A slice is a borrowed reference to a contiguous subset of a collection
that is represented by pointer and a length.
For example, a slice of a `String` has the type `&str`
which holds a pointer and a length.
Slice are created on a reference (`&`) using a range (`[start..end]`).
Indexes that are out of bounds are caught at runtime, not compile-time.
For example:

```rust
let s = String::from("abcdefgh");
// The & here says we are getting a "reference" to a portion of the string.
let sub = &s[3..6];
println!("{}", sub); // "def"

let colors = ["red", "orange", "yellow", "green", "blue", "purple"];

let slice1 = &colors[1..5];
println!("{:?}", slice1); // ["orange", "yellow", "green", "blue"]

// We can get a slice of a slice.
let slice2 = &slice1[1..3];
println!("{:?}", slice2); // ["yellow", "green"]
```

Many kinds of collections, including arrays and vectors,
support obtaining slices of their items.
This includes strings, arrays, and vectors.

For example:

```rust

```

Mutable slices allow their items to be mutated.
For example:

```rust
fn main() {
    // Create a vector of &str values.
    let original = vec!["red", "orange", "yellow", "green", "blue", "purple"];

    // Create a vector of String values from original.
    // map is lazy and collect forces its evaluation.
    let mut colors = original.iter().map(|c| c.to_string()).collect::<Vec<String>>();

    // Change a subset of the colors to uppercase.
    let subset = &mut colors[1..=3];
    for color in subset {
        color.make_ascii_uppercase();
    }
    println!("{:?}", colors); // ["red", "ORANGE", "YELLOW", "GREEN", "blue", "purple"]
}
```

For details on the syntax for specifying ranges, see [Ranges](#ranges).

## Conditional Logic

The most common way to implement conditional logic is with an `if` expressions.
The expression after the `if` keyword must evaluate to a `bool` value.
It is not surrounded by parentheses and
code to be executed must be surrounded curly braces,
even if there is only one statement or expression.
For example:

```rust
if temperature > 90 {
    println!("hot");
} else if temperature < 40 {
    println!("cold");
} else {
    println!("tolerable");
}
```

An `if` expression can be assigned to a variable and newlines are not required.
For example:

```rust
let color = if temperature > 90 { "red" } else { "blue" };
```

Other ways to implement conditional logic
include `if let` and `match` expressions
which use pattern matching to extract a value.
These were shown in the [Error Handling](#error-handling) section.

A `match` expression can match the following kinds of values:
boolean, integer, &str, String, and enum.
These must be exhaustive, meaning that they account for
every possible value of the expression being matched.
An underscore can be used as a wildcard match.
For example:

```rust
let month = "February";
let holiday = match month {
    "January" => "New Year's Day",
    "February" => "Valentine's Day",
    "July" => "Independence Day",
    "October" => "Halloween",
    "November" => "Thanksgiving",
    "December" => "Christmas",
    _ => "unknown" // matches any other value
};
println!("The holiday in {} is {}.", month, holiday);
```

The lines containing `=>` are referred to as "match arms".
They can match a single pattern or
one of a set of patterns separated by `|` characters.
Patterns can be literal values of the types
`bool`, `char`, `&str`, non-negative integer, wildcard (`_`),
tuple of these types, array of these types,
inclusive range (`m..=n`, not `m..n`), enum variant, or struct.
There are a few more less commonly used patterns that are supported.
For details, see {% aTargetBlank
"https://doc.rust-lang.org/reference/patterns.html", "Patterns" %}.

The part on the right side of `=>` can be an expression or a block.

For example:

```rust
fn get_points(rank: &str) -> i8 {
    match rank {
        "Jack" | "Queen" | "King" => 10,
        "Ace" => 1,
        _ => match rank.parse::<i8>() {
              Ok(points) => points,
              Err(_) => 0
        }
    }
}

fn main() {
    let cards = ["7", "Jack", "Ace", "5", "bad"];
    for rank in &cards {
        println!("Points for {} is {}.", rank, get_points(rank));
    }

    let age = 15;
    let category = match age {
        // The ranges cannot overlap and
        // must use "..=" rather than "..".
        0..=2 => "toddler",
        3..=12 => "child",
        13..=19 => "teen",
        20..=59 => "adult",
        _ => "senior"
    };
    println!("{} is a {}.", age, category);
}
```

The expression being evaluated can be a complex type such as a tuple.
For example:

```rust
let suit = "diamond";
let rank = "queen";
let card = match (suit, rank) {
    ("spade", _) => "a spade",
    (_, "king") => "a king",
    ("diamond", "queen") => "queen of diamonds",
    _ => "something else"
};
println!("Your card is {}.", card);
```

A "match guard" adds an `if` expression to a match pattern.
For example:

```rust
struct Point2D {
    x: i32,
    y: i32
}

fn main() {
    let numbers = vec![3, 8, -1, 12];

    // Suppose the numbers are supposed to be between 1 and 10 inclusive
    // and we want to know whether each is even, odd,
    // or invalid because it is outside that range.
    for n in numbers {
        let describe = match n {
            1..=10 if n % 2 == 0 => "even",
            1..=10 if n % 2 == 1 => "odd",
            _ if n > 0 => "too high",
            _ => "negative" // only other possibility
        };
        println!("n is {}", describe); // odd, even, negative, too high
    };

    let pt = Point2D { x: 3, y: -5 };

    // Structs can be matched, but
    // floating point values cannot be used in match patterns.
    // This is why the Point2D struct here uses integer values.
    let describe = match pt {
        Point2D { x: 0, y: 0 } => "origin",
        Point2D { x: _, y: 0 } => "x-axis",
        Point2D { x: 0, y: _ } => "y-axis",
        Point2D { x, y } if x > 0 && y > 0 => "1st quadrant",
        Point2D { x, y } if x < 0 && y > 0 => "2nd quadrant",
        Point2D { x, y } if x < 0 && y < 0 => "3rd quadrant",
        Point2D { x, y } if x > 0 && y < 0 => "4th quadrant",
        _ => "impossible" // doesn't know about conditions are exhaustive
    };

    println!("{}", describe); // 4th quadrant
}
```

Rust does not support the ternary operator (`? :`)
found in many other programming languages.
Since `if` expressions have a value,
the following can be written to simulate a ternary:

```rust
const color = if temperature > 90 { "red" } else { "blue" };
```

It is possible to write a macro to mimic this,
but it doesn't reduce the expression much.
For example:

```rust
macro_rules! tern {
    ($cond:expr => $true_expr:expr, $false_expr:expr) => {
        if $cond {
            $true_expr
        } else {
            $false_expr
        }
    };
}

fn main() {
    let temperature = 80;
    let color = tern!(temperature > 90 => "red", "blue");
    println!("{}", color); // blue
}
```

## Iteration (Looping)

Rust supports the following loop expressions:

| Name        | Description                                                               |
| ----------- | ------------------------------------------------------------------------- |
| `loop`      | infinite loop that can be exited with a `break`                           |
| `while`     | top-tested loop that repeats as long as an expression evaluates to `true` |
| `while let` | like `while`, but repeats as long as a pattern match succeeds             |
| `for`       | for looping over an iterator                                              |

All these support using the a `break` statement to break out of the loop
and the `continue` statement to advance to the next iteration.
While not commonly used, `break` can be followed by an expression
whose value becomes the value of a `loop` expression.
This is not permitted inside `while`, `while let`, or `for` loops.

See the [Standard IO](#standard-io) section for an example using `loop`.

Here's an example of using a `while` loop:

```rust
struct Item {
    name: String,
    price: u32 // in cents
}

fn main() {
    let items: Vec<Item> = vec![
        Item { name: "milk".to_string(), price: 289 },
        Item { name: "bread".to_string(), price: 349 },
        Item { name: "orange juice".to_string(), price: 479 },
        Item { name: "cheese".to_string(), price: 399 },
        Item { name: "cereal".to_string(), price: 379 },
    ];
    let len = items.len();

    let mut wallet = 1200; // $12

    // Buy items until there isn't enough left in the wallet.
    let mut index = 0;
    while index < len && wallet >= items[index].price {
      wallet -= items[index].price;
      println!("bought {}", items[index].name); // milk, bread, and orange juice
      index += 1;
    }

    // Here is a somewhat nicer approach using a for loop:
    /*
    for item in items {
        if wallet < item.price { break; }
        wallet -= item.price;
        println!("bought {}", item.name); // milk, bread, and orange juice
    }
    */
}
```

A `while let` loop is useful when iterating over
repeated calls to a function that might fail.
The example below uses the `futures` and `rand` crates
which requires adding the dependencies `futures = "0.3.8"` and `rand = "0.8.2"`
to `Cargo.toml`.
For example:

```rust
use futures::executor::block_on;
use rand::Rng;

// Pretend this function makes a REST call that could possibly fail.
async fn get_data() -> Result<i8, &'static str> {
  // rng stands for "random number generator".
  let mut rng = rand::thread_rng();
  let n = rng.gen_range(1..11); // number from 1 to 10
  // Fail if n is greater than 7.
  if n <= 7 { Ok(n) } else { Err("failed") }
}

fn main() {
    // Here is an approach for processing the result of a single call.
    let result = block_on(get_data());
    match result {
        Ok(n) => println!("in single call, n = {}", n),
        Err(msg) => println!("error: {}", msg)
    }

    // Here is an approach for processing calls in a loop
    // that continues until an Err is returned.
    while let Ok(n) = block_on(get_data()) {
        println!("in while let, n = {}", n);
    }
}
```

A `for` loop is used to iterate over any kind of iterator.
It can operate on an explicitly obtained iterator or
it can obtain an iterator from any type that
implements the `IntoIterator` trait.
Collection types such as `Range`, `HashMap`, `HashSet`, and `Vec`
implement this, but arrays and tuples do not.
For example:

```rust
fn main() {
    // We can use range notation to iterate over a range of numbers
    // where the first number inclusive and the last is exclusive.
    for n in 1..4 { // 1, 2, and 3
        println!("loop 1: n = {}", n);
    }
    // Adding = before the second number makes the range inclusive.
    for n in 1..=4 { // 1, 2, 3, and 4
        println!("loop 2: n = {}", n);
    }

    // Iterating over the items in a tuple is not supported, but we
    // can iterate over the items in an array using the "iter" method.
    let num_arr = [1, 7, 5, 2, 9, 6];
    for n in num_arr.iter() {
        println!("loop 3: n = {:?}", n);
    }

    // The iter_mut method allows items to be mutated during iteration.
    let mut num_arr = [1, 7, 5, 2, 9, 6];
    // Double all the numbers during iteration.
    for n in num_arr.iter_mut() {
        *n *= 2;
    }
    for n in num_arr.iter() {
        println!("loop 4: {:?}", n); // values are doubled
    }

    // Another approach is to create a new array of modified values
    // using the array map method that is considered experimental
    // and only available in nightly builds as of 12/13/20.
    //let new_numbers = numbers.map(|n| n * 2);

    // We can call the map method on an iterator
    // to create a new iterator over doubled numbers.
    let new_iter = num_arr.iter().map(|n| n * 2);
    for n in new_iter {
        println!("loop 5: {:?}", n); // values are doubled
    }

    // We can iterate over the items in a vector
    // without calling the "iter" method.
    let num_vec = vec![1, 7, 5, 2, 9, 6];
    for n in num_vec {
        println!("loop 6: n = {:?}", n);
    }
}
```

Let's look at one more iteration example that requires specifying lifetimes.
The function `longest` is passed a reference to an array of strings.
There are three lifetimes to consider, that of the array,
that of the items inside it, and that of the return value.
Rust wants to know that the array items
will live as long as the return value
since one of them will be returned.
We must specify that with lifetime annotations (`'a` below).

```rust
fn longest<'a>(strings: &[&'a str]) -> &'a str {
    strings
        .iter()
        .fold("", |acc, s| if s.len() > acc.len() { s } else { acc })
}

fn main() {
    let fruits = ["apple", "banana", "cherry", "date"];
    let result = longest(&fruits);
    println!("longest is {}", result); // banana
}
```

## Functions

Functions are defined using the `fn` keyword,
followed by a name, parameter list, return type, and body.
Functions that do not return anything omit the return type rather than
specify a type like `void` as is done in some other languages.
Functions that might fail should return a `Result` enum
to allow callers to handle errors.
See the "Error Handling" section for details.

A `return` statement returns the value of an expression.
If the last statement is not terminated by a semicolon, its value is returned.
This means that `return my_result;` is equivalent to `my_result`.

For example:

```rust
fn average(numbers: &Vec<f64>) -> f64 {
    let sum: f64 = numbers.iter().sum();
    sum / numbers.len() as f64 // return value
}

fn greet(name: &str) {
    println!("Hello, {}!", name);
}

fn main() {
    let numbers: Vec<f64> = vec![1.0, 2.0, 3.0, 4.0];
    println!("average = {}", average(&numbers));
    greet("World");
}
```

The documentation for the `Copy` trait says that values of types
that implement it "can be duplicated by simply copying bits".
When such a value is passed to a function, rather than passing a reference,
a copy is created and ownership of the original value is not transferred.
All primitive types such as `bool`, `char`, `i32`, and `f64`
implement `Copy`.
However, structs and collection types like
tuple, array, `Vec`, `HashMap`, and `HashMap` do not.
You can choose to implement the `Copy` trait for custom structs
if all their fields also implement it.

Function parameters of non-Copy types typically use reference types.
This is because usually the function wants to borrow their values
rather than take ownership.
If the caller uses a variable to pass a non-Copy value,
and the function takes ownership, the caller loses ownership and
can no longer use the variable unless the function returns it.

Function return values typically have non-reference types.
This is because usually the function creates the value
and wants to transfer ownership to the caller.
If a function creates a value and returns a reference to it,
the code will not compile because the value goes out of scope and is dropped.

Function parameters that are strings that are not mutated by the function
should almost always have the type `&str`.
This allows many string representations to be passed in including
literal strings, `&str` values, and
references to `String` values (not `String` values).
Functions that create and return strings have the return type `String`
so ownership can be transferred to the caller.

Functions that take data from arrays or vectors
typically should have slice parameters.
This makes them more flexible than taking a reference to an array or vector
because callers can pass an array, vector, or slice of them.

Functions are accessible by default within the same source file,
but they are private by default when defined in a different source file.
For functions that should be visible outside the source file that defines them,
add the `pub` keyword before the `fn` keyword.

In the [Ownership Model](#ownership-model) section we learned that
closures are functions that capture values in their environment
so they can be access later when the function is executed.
Functions defined with the `fn` keyword are not closures.
Closures are defined as anonymous functions
with a parameter list written between vertical bars
which must be present even if there are no parameters.

Rust does not support writing functions that
accept a variable number of arguments (variadic),
but macros can do this.
Functions can instead be passed in an array.
For example:

```rust
// This takes an array of strings and returns one of them.
// Lifetimes must be specified, but why?
fn longest<'a>(strings: &'a [&str]) -> &'a str {
    strings
        .iter()
        .fold("", |acc, s| if s.len() > acc.len() { s } else { acc })
}

fn main() {
    let fruits = ["apple", "banana", "cherry", "date"];
    let result = longest(&fruits);
    println!("longest is {}", result);
}
```

## <a name="iterators">Iterators</a>

Many Rust methods return a `std::iter::Iterator` that
can be used to iterate over the elements of a collection.
Iterators are lazy, meaning that they
do not pre-compute the values they will return.

The commonly used methods for creating an iterator over a collection include:

- `iter` for iterating over immutable references to items
- `iter_mut` for iterating over mutable references to items
- `into_iter` for iterating over items

Here is an example of modifying the items in a collection while iterating over it.

```rust
let mut colors = ["red".to_string(), "orange".to_string(), "yellow".to_string()];
for color in colors.iter_mut() {
    *color = color.to_uppercase();
}
```

The `into_iter` method is described by the `IntoIterator` trait.
Collections that implement this can be iterated over using a `for` loop
without needing to call one of the methods listed above.
For example:

```rust
let colors = ["red", "orange", "yellow"];
for color in &colors {
    println!("{}", color); // red then orange then yellow
}
```

The `Iterator` trait supports methods in the following non-exhaustive list:

| Method                | Description                                                                                                  |
| --------------------- | ------------------------------------------------------------------------------------------------------------ |
| `all(pred_fn)`        | returns `bool` indicating if `pred_fn` returns true for all items                                            |
| `any(pred_fn)`        | returns `bool` indicating if `pred_fn` returns true for any items                                            |
| `chain(iter2)`        | returns `Iterator` that iterates over combined items                                                         |
| `collect()`           | returns a `std::vec::Vec` containing all the items                                                           |
| `count()`             | returns number of items in `Iterator`, consuming it                                                          |
| `enumerate()`         | returns `Iterator` over tuples of indexes and items                                                          |
| `filter(pred_fn)`     | returns `Iterator` over items for which `pred_fn` returns true                                               |
| `fold(fn)`            | returns result of combining items into a single value; like `reduce` in other languages                      |
| `last()`              | returns last item in `Iterator`, consuming it                                                                |
| `map(fn)`             | returns `Iterator` over results of calling a function on each item                                           |
| `max()`               | returns `Option` that wraps the largest item                                                                 |
| `max_by(fn)`          | returns `Option` that wraps the largest result based on passing pairs of items to a function                 |
| `max_by_key(fn)`      | returns `Option` that wraps the largest result of passing each item to a function                            |
| `min()`               | returns `Option` that wraps the smallest item                                                                |
| `min_by(fn)`          | returns `Option` that wraps the smallest result based on passing pairs of items to a function                |
| `min_by_key(fn)`      | returns `Option` that wraps the smallest result of passing each item to a function                           |
| `next()`              | returns `Option` that wraps the next item                                                                    |
| `nth(n)`              | returns `Option` that wraps the nth item                                                                     |
| `partition(pred_fn)`  | returns two collections containing items for which a function returns true or false                          |
| `position(pred_fn)`   | returns `Option` that wraps the index of first item for which `pred_fn` returns true                         |
| `product()`           | returns product of items; panics on overflow                                                                 |
| `rev()`               | returns `Iterator` that iterates in reverse order                                                            |
| `skip(n)`             | returns `Iterator` that begins after n items                                                                 |
| `skip_while(pred_fn)` | returns `Iterator` that begins at first item for which `pred_fn` returns false                               |
| `sum()`               | returns sum of items; panics on overflow                                                                     |
| `take(n)`             | returns `Iterator` that stops after the first n items                                                        |
| `take_while(pred_fn)` | returns `Iterator` that stops at last item for which `pred_fn` returns true                                  |
| `zip(iter2)`          | returns `Iterator` over `Option` objects that wrap<br>references to corresponding items from two `Iterators` |

Here is an example of using the `next` method:

```rust
fn main() {
    let colors = ["red", "yellow", "blue"];
    // Must be mutable because calling methods
    // like "next" on it modifies its state.
    let mut iter = colors.iter();

    match iter.next() {
        Some(color) => println!("{}", color), // red
        None => println!("no more colors")
    }

    if let Some(color) = iter.next() {
        println!("{}", color); // yellow
        if let Some(color) = iter.next() {
            println!("{}", color); // blue
        }
    }
}
```

We can also obtain an iterator that allows the underlying collection
to be modify while iterating over it. For example:

```rust
fn main() {
    let mut colors = ["red".to_string(), "yellow".to_string(), "blue".to_string()];

    for color in colors.iter_mut() {
        color.make_ascii_uppercase();
    }

    println!("{:?}", colors); // ["RED", "YELLOW", "BLUE"]

    // To create a new vector rather than modify one in place ...
    //let new_colors: Vec<String> =
    //    colors.iter().map(|s| s.to_uppercase()).collect();
}
```

Here is an example of using the `fold` method
which is like `reduce` in some other programming languages:

```rust
let a1 = [1, 2, 3];
let sum = a1.iter().fold(0, |acc, n| acc + n);
println!("{}", sum); // 6

// For this use of "fold" we can use the "sum" method instead.
let sum: i32 = a1.iter().sum();
println!("{}", sum); // 6
```

Here is an example of using the `filter` method.

```rust
// This is a predicate function used by the "filter" method.
// It must take a reference to a item in an "Iterator".
// In this case the item type is "&str",
// so the parameter type must be "&&str".
fn is_short(s: &&str) -> bool {
    s.len() <= 4
}

fn main() {
    let months = "January|February|March|April|May|June|July|August";

    // This passes a closure to the filter method.
    let short_names: Vec<&str> =
        months.split('|').filter(|m| m.len() <= 4).collect();
    println!("{:?}", short_names); // ["May", "June", "July"]

    // This passes a function to the filter method.
    let short_names: Vec<&str> =
        months.split('|').filter(is_short).collect();
    println!("{:?}", short_names); // ["May", "June", "July"]
}
```

Here is an example of using the `map` method:

```rust
let a1 = [1, 2, 3];
let iter = a1.iter().map(|n| n * 2); // doubles each number
for n in iter {
    println!("{}", n); // 2 then 4 then 6
}

// We cannot create an array from an iterator,
// but we can create a Vector.
// map is lazy and collect forces its evaluation.
let v1 = vec![1, 2, 3];
let v2: Vec<i32> = v1.iter().map(|n| n * 2).collect();
println!("{:?}", v2); // [2, 4, 6]
```

Here is an example of using the `zip` method:

```rust
let a1 = [1, 2, 3];
let a2 = [4, 5, 6, 7];

// This creates an "Iterator" over "Option" enums.
// The "Some" variant will contain tuples of references.
let mut iter = a1.iter().zip(a2.iter());

assert_eq!(iter.next(), Some((&1, &4)));
assert_eq!(iter.next(), Some((&2, &5)));
assert_eq!(iter.next(), Some((&3, &6)));
assert_eq!(iter.next(), None); // extra item in a2 ignored
```

Here is an example of using the `take` method
to get a certain number of initial values from an iterator:

```rust
#[derive(Debug)]
struct Student {
    name: String,
    scores: Vec<f32>,
}

impl Student {
    fn new(name: &str, scores: &[f32]) -> Self {
        Self {
            name: name.to_string(),
            scores: scores.to_vec(),
        }
    }
}

fn average(numbers: &[f32]) -> f32 {
    numbers.iter().sum::<f32>() / numbers.len() as f32
}

fn main() {
    // This is mutable because the sort_by method sorts it in place.
    let mut students: Vec<Student> = vec![
        Student::new("Alice", &[90.0, 75.0, 80.0]),
        Student::new("Betty", &[85.0, 95.0, 80.0]),
        Student::new("Claire", &[70.0, 80.0, 75.0]),
        Student::new("Dina", &[95.0, 100.0, 90.0]),
        Student::new("Elaine", &[75.0, 90.0, 100.0]),
    ];

    // Sort the students in descending order by their average scores.
    students.sort_by(|a, b| {
        let a_avg = average(&a.scores);
        let b_avg = average(&b.scores);
        b_avg.partial_cmp(&a_avg).unwrap()
    });

    // Print the names of the top 3 students.
    for student in students.iter().take(3) {
      println!("{}", student.name);
    }
}
```

Here is an example of using the `some` and `all` methods:

```rust
let numbers = vec![8, 13, 5, 21, 15, 3, 6, 24];

// Is any number less than 3?
println!("any < 3? {}", numbers.iter().any(|&n| n < 3));

// Is any number greater or equal to 20?
println!("any >= 20? {}", numbers.iter().any(|&n| n >= 20));

// Are all the numbers even?
println!("all even? {}", numbers.iter().all(|&n| n & 2 == 0));

// Are all the numbers less than 30?
println!("all < 30? {}", numbers.iter().all(|&n| n < 30));
```

## Regular Expressions

Regular expressions for string pattern matching are not directly supported.
Instead an external crate such as
{% aTargetBlank "https://crates.io/crates/regex", "regex" %} must be used.
For example:

```rust
use regex::Regex;

fn main() {
    // Determine if a string matches a regular expression.
    // Match 'h' or 's' followed by 1 to 3 digits.
    let re = Regex::new(r"[hs]\d{1,3}").unwrap();
    let s = "The host is h19 and the switch is s257.";
    if re.is_match(s) {
        println!("matched");
    } else {
        println!("mismatch");
    }

    // Get text matching capture groups.
    for cap in re.captures_iter(s) {
        let text = &cap[0];
        println!("{:?}", text); // h19 and s257
    }

    // Split a string on a regular expression
    // and collect the pieces into a vector.
    let pieces: Vec<&str> = re.split(s).into_iter().collect();
    println!("{:?}", pieces); // ["The host is ", " and the switch is ", "."]

    // Split a string on a regular expression
    // and iterate over the pieces.
    let piece_iter = re.split(s).into_iter();
    for piece in piece_iter {
        println!("{}", piece);
        // The host is
        //  and the switch is
        // .
    }
}
```

## Operators

Rust supports most of the common operators found in other programming languages.

The `std::ops` namespace defines overloadable operators.
For example, we can define what it means
to add and subtract to `Point2D` objects by
implementing the `Add` and `Sub` traits defined in `std::ops`.

The operators that can be implemented for custom types include:

- arithmetic: `+`, `+=`, `-`, `-=`, `_`, `_=`, `/`, `/=`
- bit shift: `<<`, `<<=`, `>>`, and `>>=`
- bitwise: `&`, `&=`, `|`, `|=`, `^`, and `^=`
- deref: `\*` for getting and setting a value
- functions: `()` call operator in three forms
- index: `[]` operator to get and set an element
- logical: `!` not, but `&&` and `||` cannot be defined
- mod: `%` and `%=`
- negate: `-` (unary)
- range: `..` and `..=`

## <a name="ranges">Ranges</a>

The `std::ops` namespace defines the following range types:

| Range Type         | Meaning                          | Syntax  |
| ------------------ | -------------------------------- | ------- |
| `RangeFull`        | all items                        | `..`    |
| `Range`            | start inclusive to end exclusive | `s..e`  |
| `RangeInclusive`   | start inclusive to end inclusive | `s..=e` |
| `RangeFrom`        | start inclusive and above        | `s..`   |
| `RangeTo`          | zero to end exclusive            | `..e`   |
| `RangeToInclusive` | zero to end inclusive            | `..=e`  |

These are distinct types and there is not a
provided range type that encompasses all of them.

Instances these types are values that
can be assigned to variables, be members of structs,
be passed to functions, and be returned from functions.

For example:

```rust
use std::ops::{Range, RangeInclusive};

fn print_range(r: &Range<i32>) {
    println!("range = {:?}", r);
}

fn print_range_inclusive(r: &RangeInclusive<i32>) {
    println!("range = {:?}", r);
}

fn main() {
    let a = 3;
    let b = 7;

    let r1 = a..b;
    print_range(&r1); // range = 3..7
    // The Range contains method requires a ref to a number,
    // so even literal values must be passed by reference.
    println!("{}", r1.contains(&5)); // true
    println!("{}", r1.contains(&7)); // false

    let r2 = a..=b;
    print_range_inclusive(&r2); // range = 3..=7
    for n in r2 {
        println!("{}", n); // 3, 4, 5, 6, 7
    }
}
```

## <a name="structs">Structs</a>

A struct defines a type that is a set of related fields and methods,
similar to a class in other languages.

The `struct` keyword only defines a set of fields.
When there a no fields, it is referred to as a "unit struct".
These are used to implement groups of related functionality
that have no state.

The `impl` keyword adds
associated functions (like class or static methods in other languages)
and methods (like instance methods in other languages) to a struct.
Associated functions are typically used to construct instances or a struct.
By convention, many structs have an associated function
named `new` for just this purpose.

The first parameter of methods must be named "self",
like the convention in Python.
Any `fn` definition with no parameters or
a first parameter with a name other than "self"
is an "associated function" rather than a method.
Associated functions are called with the syntax
`StructName::function_name(arguments)`.
Methods are called with the syntax
`instance_variable.method_name(arguments)`.

In an `impl` block the type `Self` refers to the associated struct type.
Using `Self` in place of the struct name enables renaming structs
without needing to change uses of their names inside `impl` blocks.

An instance of a struct can be created using its name.
It is also common to define an associated function named "new"
(by convention) that creates an instance that is initialized in a specific way,
similar to a constructor in other languages.
In addition, we can implement the `Default` trait for a struct
to provide a way to create a "default" instance.

For example:

```rust
use std::default::Default;

#[derive(Debug, Default)]
struct Point2D {
    x: f64,
    y: f64, // comma after last field is optional
}

// We only need to manually implement the "Default" trait
// when one or more fields should be assigned non-default values,
// such as a number other than zero for numeric fields.
// In this case the default fields values work fine,
// so we don't need to write the commented out code below.
// The derive attribute above generates this code for us.
/*
impl Default for Point2D {
    fn default() -> Self {
        Self { x: 0.0, y: 0.0 } // origin
    }
}
*/

impl Point2D {
    fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    // Associated function
    fn distance_between(pt1: &Self, pt2: &Self) -> f64 {
        let dx = pt1.x - pt2.x;
        let dy = pt1.y - pt2.y;
        (dx.powi(2) + dy.powi(2)).sqrt()
    }

    // Method that does not mutate
    fn distance_to(self: &Self, other: &Self) -> f64 {
        Self::distance_between(self, other)
    }

    // Method that mutates
    fn translate(self: &mut Self, dx: f64, dy: f64) {
        self.x += dx;
        self.y += dy;
    }
}

fn main() {
    // Create an instance using the "default" function.
    let origin = Point2D::default();

    // Create an instance using the struct name.
    let p1 = Point2D { x: 3.0, y: 4.0 };

    // Create an instance using the "constructor" function.
    let p2 = Point2D::new(6.0, 8.0);

    println!("{:?}", origin); // Point2D { x: 0.0, y: 0.0 }
    println!("{}", Point2D::distance_between(&p1, &p2)); // 5
    println!("{}", p1.distance_to(&p2)); // 5

    p2.translate(-2.0, 1.0);
    println!("{:?}", p2); // Point2D { x: 4.0, y: 9.0 }
}
```

In general it's best for struct fields to not have reference types.
This makes the lifetime of field values match that of the struct.
This simplifies its usage by removing the need to specify lifetime annotations.
For example, struct fields with a string value should almost always
use the type `String` instead of `&str` or `&String`.

To allow structs to be printed for debugging purposes,
add the following above their definition: `#[derive(Debug)]`.
Then print instances using the `:?` (single line)
or `:#?` (multi-line) format specifier.
For example:

```rust
println!("p1 = {:?}", p1);
println!("p2 = {:#?}", p1);
```

This outputs the following:

```text
p1 = Point2D { x: 3.0, y: 4.0 }
p2 = Point2D {
    x: 3.0,
    y: 4.0,
}
```

Structs are not cloneable, copyable, or printable by default.
Being copyable allows instances to be
passed by value (copy) instead of by reference.
These features add compile time,
so Rust requires implementing them on a case-by-case basis.
The easiest way to implement these features
is the precede a struct definition with the following:

```rust
#[derive(Clone, Copy, Debug)]
```

Structs and their fields are accessible by default within the same source file,
but they are private by default when defined in a different source file.
For structs that should be visible outside the source file that defines them,
add the `pub` keyword to both the `struct` and the fields to be exposed.

Structs cannot inherit from (extend) other structs,
but they can nest other structs (composition).

A `struct` can include the fields of another `struct`
of the same type using the `..` syntax.
This can only appear at the end of the list of values.
It only supplies values that were not specified.
For example:

```rust
struct Point3D {
    x: f64,
    y: f64,
    z: f64
}
let p3 = Point2D { x: 1, y: 2, z: 3 };
let p4 = Point3D { z: 4, ..p3 }; // uses p3.x and p3.y, but not p3.z
```

Note that it wouldn't make sense to allow including the fields
of multiple structs because struct fields are never optional.
Each struct being included would contain all the fields.

A "tuple struct" gives a name to a tuple,
allowing it to be used as a type.
For example:

```rust
#[derive(Debug)]
struct RGB(u8, u8, u8);

const CORNFLOWER_BLUE: RGB = RGB(100, 149, 237);
const REBECCA_PURPLE: RGB = RGB(0x66, 0x33, 0x99);
println!("{:?}", CORNFLOWER_BLUE); // RGB(100, 149, 237)
println!("{:?}", REBECCA_PURPLE); // RGB(102, 51, 153)
```

Tuples whose values have the same types as those in a tuple struct
are not compatible with that type. For example:

```rust
let mut color: RGB = RGB(100, 149, 237);
color = RGB(200, 49, 137); // can change to value of type RGB
color = (100, 149, 237); // error: expected struct `RGB, found tuple
```

One use of a tuple struct is referred to as the "new type" pattern.
It assigns a name to a specific use of an existing type.
For example:

```rust
#[derive(Debug)]
struct Temperature(i16);

fn is_cold(t: &Temperature) -> bool {
    t.0 <= 32 // Fahrenheit
}

fn main() {
    let mut temperature: Temperature = Temperature(20);
    println!("{}", is_cold(&temperature)); // true
    temperature = Temperature(90);
    println!("{}", is_cold(&temperature)); // false
    println!("{}", is_cold(&Temperature(50))); // false
    println!("{}", is_cold(&(50))); // error: expected struct `Temperature`, found integer
}
```

The `impl` keyword can be used multiple times on the same struct.
However, usually all the non-trait methods of a struct
are defined in one `impl` block.
The following example shows using three `impl` blocks
to define methods on the same struct.

File `geometry.rs`

```rust
#[derive(Debug)]
pub struct Point2D {
    pub x: f64,
    pub y: f64,
}

impl Point2D {
    pub fn distance_from_origin(&self) -> f64 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}
```

File `more_geometry.rs`

```rust
use super::geometry::Point2D;

impl Point2D {
    pub fn translate_x(&mut self, dx: f64) {
        self.x += dx;
    }
}

pub fn scoot(p: &mut Point2D) {
    p.translate_x(1.0);
}
```

File `main.rs`

```rust
mod geometry;
use geometry::Point2D;

mod more_geometry;

fn demo(p: &mut Point2D) {
    impl Point2D {
        fn translate_y(&mut self, dy: f64) {
            self.y += dy;
        }
    }

    p.translate_y(1.0);
}

fn main() {
    let mut p = Point2D { x: 1.0, y: 2.0 };
    println!("distance = {}", p.distance_from_origin());

    // The translate_x method is defined in geometry.rs.
    p.translate_x(2.0);

    // The "scoot" function is defined in more.rs.
    // Just because "p" is mutable doesn't mean
    // that all references to it are mutable.
    // The "scoot" function requires a mutable reference
    // and we must specify that when passing a reference.
    more::scoot(&mut p);

    demo(&mut p);

    // The "translate_y" method is defined in the "demo" function above.
    p.translate_y(2.0);

    println!("{:?}", p); // Point2D { x: 4.0, y: 5.0 }
}
```

Structs can use generic types.
For example:

```rust
use num::ToPrimitive;

// T can be any type that implements the "ToPrimitive" trait.
// This is required because the "distance_to_origin" method
// calls the "to_f64" method defined by that trait.
#[derive(Debug)]
struct Point2D<T: ToPrimitive> {
    x: T,
    y: T
}

impl<T: ToPrimitive> Point2D<T> {
    pub fn distance_from_origin(&self) -> f64 {
        let x = self.x.to_f64().unwrap();
        let y = self.y.to_f64().unwrap();
        (x.powi(2) + y.powi(2)).sqrt()
    }
}

fn main() {
    let pt1 = Point2D::<f32> { x: 3.0, y: 4.0 };
    let pt2 = Point2D::<i8> { x: 3, y: 4 };
    println!("pt1 = {:?}", pt1);
    println!("pt1 distance = {:?}", pt1.distance_from_origin());
    println!("pt2 = {:?}", pt2);
    println!("pt2 distance = {:?}", pt2.distance_from_origin());

    /* Can't do this because &str does not implement the ToPrimitive trait.
    let pt3 = Point2D::<&str> { x: "foo", y: "bar" };
    println!("pt3 = {:?}", pt3);
    println!("pt3 distance = {:?}", pt3.distance_from_origin());
    */
}
```

## Dereference

The dereference operator is used to get the value of a reference.
This isn't needed very often because unlike in most programming languages
that support references (or pointers), Rust does not
require different syntax for accessing fields and methods
based on whether an instance or a reference is used.
It supplies "automatic referencing and dereferencing"
in field access and method calls for types that implement the `Deref` trait.
This includes immutable and mutable references to all types.
In the case of method calls, it automatically adds `&`, `&mut`, or `*`
based on the method declaration of the `self` parameter.

For example:

```rust
struct Point2D {
    x: f64,
    y: f64
}

impl Point2D {
    fn is_origin(&self) -> bool {
        self.x == 0.0 && self.y == 0.0
    }
}

fn main() {
    let p = Point2D { x: 1.0, y: 2.0 };
    let p_ref = &p;
    println!("{}", p.x); // 1
    println!("{}", p_ref.x); // 1; same syntax with reference
    println!("{}", p.is_origin()); // false
    println!("{}", p_ref.is_origin()); // false; same syntax with reference
}
```

Here is an example where dereference is needed:

```rust
// Implementing the PartialEq and PartialOrd traits
// enables comparing instances.
#[derive(Debug, PartialEq, PartialOrd)]
struct Point2D {
    x: f64,
    y: f64
}

const ORIGIN: Point2D = Point2D { x: 0.0, y: 0.0 };

fn is_origin(pt: &Point2D) -> bool {
    // We could just check whether x and y are zero,
    // but then we wouldn't need to dereference pt.
    //pt.x == 0.0 && pt.y == 0.0

    // We can't compare a Point2D reference to a Point2D,
    // but we can dereference pt to get the Point2D instance
    // it references and then compare that to ORIGIN.
    *pt == ORIGIN
}

fn main() {
    let p = Point2D { x: 1.0, y: 2.0 };
    let q = Point2D { x: 0.0, y: 0.0 };
    println!("p equal q? {}", p == q); // false
    println!("p is origin? {:?}", is_origin(&p)); // false
    println!("q is origin? {:?}", is_origin(&q)); // true
}
```

## Type Aliases

Aliases for types can be defined using the `type` keyword.
These can be used anywhere a type can be specified.

Here is an example of a type alias for a
`HashMap` with specific key and value types:

```rust
use std::collections::HashMap;

type StringToIntMap = HashMap<String, i32>;

fn main() {
    let mut score_map: StringToIntMap = StringToIntMap::new();
    score_map.insert("Mark".to_string(), 19);
    score_map.insert("Tami".to_string(), 42);
    score_map.insert("Amanda".to_string(), 37);
    score_map.insert("Jeremy".to_string(), 35);

    // Iterating takes ownership of score_map unless a reference is used.
    for entry in &score_map {
        println!("{:?}", entry);
    }

    if let Some(winner) = score_map.iter().max_by_key(|entry| entry.1) {
        println!("winner is {}", winner.0);
    } else {
        println!("no players found");
    }
}
```

Here is an example of using a type alias for a function signature:

```rust
#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

// This is a type alias for a function signature.
type PointFn = fn (pt: &Point2D, input: f64) -> Point2D;

fn rotate(pt: &Point2D, angle: f64) -> Point2D {
    let cos = angle.cos();
    let sin = angle.sin();
    let x = pt.x * cos - pt.y * sin;
    let y = pt.x * sin + pt.y * cos;
    Point2D { x, y }
}

fn translate_x(pt: &Point2D, dx: f64) -> Point2D {
    // "..*pt" gets the rest of the fields in the "pt" object
    // which in this case is just the "y" field.
    Point2D { x: pt.x + dx, ..*pt }
}

fn translate_y(pt: &Point2D, dy: f64) -> Point2D {
    // "..*pt" gets the rest of the fields in the "pt" object
    // which in this case is just the "x" field.
    Point2D { y: pt.x + dy, ..*pt }
}

// The type of the second parameter is the type alias defined above.
fn operate(pt: &Point2D, function: PointFn, input: f64) -> Point2D {
    function(pt, input)
}

fn main() {
    let p = Point2D {x: 3.0, y: 4.0};

    println!("{:?}", operate(&p, translate_x, 4.0));
    // Point2D { x: 7.0, y: 4.0 }

    println!("{:?}", operate(&p, translate_y, 2.0));
    // Point2D { x: 3.0, y: 5.0 }

    let pi = std::f64::consts::PI;
    println!("{:?}", operate(&p, rotate, pi / 2.0));
    // Point2D { x: -4.0, y: 3.0000000000000004 }
}
```

## <a name="traits">Traits</a>

A trait describes an interface that any number of types can implement.

Any trait can be implemented on a type defined in the current crate.
A trait defined in the current crate can be implemented on any type,
even those in the standard library.
But a trait defined outside the current crate cannot be
implemented on a type that is also defined outside the current crate.
For example, the `std::vec::Vec` type does not implement the
`std::fmt::Display` trait and this implementation cannot be added.
This is referred to as the "orphan rule".

A trait defines any number of associated constants (not commonly used),
associated functions, and methods.
The first parameter of methods must be named "self".
Any `fn` definition whose first parameter name is not "self"
is an "associated function" rather than a method.

The first parameter of a method can be written as
`self` (takes ownership or copies),
`&self` (borrows immutably; most common), or
`&mut self` (borrows mutably; needed to modify struct fields).
These are shorthand for `self: Self`, `self: &Self`,
and `self: &mut Self` respectively.
In the first case, if the type implements
the `Copy` trait then the value is copied.
Otherwise ownership is transferred to the method.

Often traits are implemented for structs, but they can also
be implemented for tuples and primitive types like `bool`.

A trait function or method can be described by providing only its signature.
This requires implementing types to define the body.
It can also provide a default implementation that is
used by implementing types that do not override it.

We saw in the [Structs](#structs) section that
we can add methods to a struct without defining a trait.
Using a trait is useful when it is desireable to
implement the same set of functions and methods on many types.
The trait can then be used as a parameter or return type,
enabling any type that implements the trait to be used.
This is how Rust achieves polymorphism.

Many functions provided by the standard library are implementations of traits.
When looking at documentation for a type consider that
some methods may only be described in the documentation
for traits that are implemented for the type.

In this example we define the trait `Item` which is implemented
by specific kinds of items such as `Book` and `Food`.
A `Cart` holds items that implement the `Item` trait.

```rust
use std::fmt;

type Price = u64;

trait Item {
    fn get_description(&self) -> &str;
    fn get_price(&self) -> Price;
}

struct Book {
    price: Price,
    title: String,
}

impl Item for Book {
    fn get_description(&self) -> &str {
        &self.title
    }
    fn get_price(&self) -> Price {
        self.price
    }
}

struct Food {
    calories_per_serving: u32,
    description: String,
    price: Price,
}

impl Item for Food {
    fn get_description(&self) -> &str {
        &self.description
    }
    fn get_price(&self) -> Price {
        self.price
    }
}

#[derive(Default)]
struct Cart {
    // A Cart holds any kind of items that implement the Item trait.
    items: Vec<Box<dyn Item>>,
}

impl Cart {
    // Each Item added to a Cart is guaranteed
    // to live for the duration of the program.
    // There are two ways to specify the item parameter type.
    //fn add<T: Item + 'static>(&mut self, item: T) {
    fn add(&mut self, item: impl Item + 'static) {
        self.items.push(Box::new(item));
    }

    fn get_subtotal(&self) -> Price {
        self.items
            .iter()
            .fold(0, |acc, item| acc + item.get_price())
    }
}

impl fmt::Display for Cart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in &self.items {
            // We don't need to manually extract items from their Box.
            writeln!(f, "{} ${}", item.get_description(), item.get_price())?;
        }
        Ok(())
    }
}

fn main() {
    let mut cart = Cart::default();

    let item = Book {
        title: "Svelte and Sapper in Action".to_string(),
        price: 2000,
    };
    cart.add(item);
    cart.add(Food {
        description: "Snickers bar".to_string(),
        calories_per_serving: 229,
        price: 75,
    });
    cart.add(Food {
        description: "Coke can".to_string(),
        calories_per_serving: 140,
        price: 100,
    });
    println!("subtotal = {}", cart.get_subtotal() as f64 / 100.0);
}
```

Traits can be made generic by including type parameters.
Here is an example of a custom generic trait named `Distance`
that is implemented for the custom type `Point2D`.

```rust
struct Point2D {
    x: f64,
    y: f64,
}

trait Distance<T> {
    // The type Self here refers to the implementing type.
    // In the "impl" below, that is the Point2D type.
    fn distance_to(self: &Self, other: &Self) -> T;
}

impl Distance<f32> for Point2D {
    fn distance_to(&self, other: &Point2D) -> f32 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx.powi(2) + dy.powi(2)).sqrt() as f32
    }
}

impl Distance<f64> for Point2D {
    fn distance_to(&self, other: &Point2D) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx.powi(2) + dy.powi(2)).sqrt()
    }
}

fn main() {
    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };

    // The type of d must be specified because there is more than one
    // implementation of the Distance Trait on the Point2D type.
    let d: f32 = p1.distance_to(&p2);
    println!("{}", d); // 5
    let d: f64 = p1.distance_to(&p2);
    println!("{}", d); // 5
}
```

An alternative to making a trait generic is to use "associated types".
A key distinction is the number of implementations that can be defined
on a given type, many (with generics) or only one (with associated types).
For example, for the `Point2D` type above
if we only need one implementation of the `distance_to` method,
we can use an associated type as follows:

```rust
// We can implement this trait for any number of types like Point2D,
// but each can only specify one output type for its distance_to method.
trait Distance {
    // We can define any number of named associated types here,
    // but only one is needed in this example.
    type Output;

    fn distance_to(&self, other: &Self) -> Self::Output;
}

impl Distance for Point2D {
    // Specify the concrete type of each associated type here.
    type Output = f64;

    fn distance_to(&self, other: &Point2D) -> Self::Output {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx.powi(2) + dy.powi(2)).sqrt()
    }
}
```

A trait must be in scope in order to use its constants and methods.
This avoids ambiguities in cases where a type implements multiple traits
that happen to define constants and/or methods with the same names.
Consider this example, defined by three source files:
TODO: Maybe this example belongs in the "Modules" section instead.

Here is `src/printable.rs` which defines the `Printable` trait:

```rust
pub trait Printable {
    fn print(&self);
}
```

Here is `src/person.rs` which defines the `Person` type:

```rust
// super traverses up a level to the parent of this module
// which is the same parent as the "printable" module.
// It is only aware of the "printable" module
// because src/main.rs contains "mod printable;".
use super::printable::Printable;

pub struct Person {
    pub first_name: String,
    pub last_name: String,
}

impl Printable for Person {
    fn print(&self) {
        println!("{} {}", self.first_name, self.last_name);
    }
}
```

Here is `src/main.rs` which uses the `Person` type:

```rust
// These mod statements tell the compiler to read the corresponding ".rs" files
// and add entries in the module tree which will become
// crate (src/main.rs)
// - crate::person (src/person.rs)
// - crate::printable (src/printable.rs)
// Note that `person` and `printable` are siblings,
// but `main is not their sibling!
// There can be only one "mod" statement per module in the project.
// Each "mod" statement is placed in the highest module
// that encompasses all usages of the module.j
// Often, but not always, this is the top source file (main.rs or lib.rs).
mod person;
mod printable;

use person::Person;

// In order to use methods defined by a trait, the trait must be in scope.
use printable::Printable;

fn main() {
    let p = Person {
        first_name: "Mark".to_string(),
        last_name: "Volkmann".to_string(),
    };
    p.print();
}
```

Traits can specify other traits that must also be
implemented by any types that implement them.
These are referred to as "supertraits".
For example:

```rust
pub trait HockeyPlayer: Athlete + Person {
    // Describe functions unique to hockey players here.
}
```

Now any `struct` that implements `HockeyPlayer`
must also implement the `Athlete` and `Person` traits.
Also see the `Printable` example later in this section.

It is possible for a type to implement multiple traits
that describe constants and/or methods with the same names.
Calling them requires doing so in a form that makes it clear which is desired.
For example:

```rust
trait First {
    const SOME_CONST: i32 = 1; // associated constant
    fn some_method(&self) {
        println!("First some_method {}", Self::SOME_CONST);
    }
}

trait Second {
    const SOME_CONST: i32 = 2; // associated constant
    fn some_method(&self) {
        println!("Second some_method {}", Self::SOME_CONST);
    }
}

// Implement both traits on the built-in bool type.
impl First for bool {} // using default some_method implementation
impl Second for bool {} // using default some_method implementation

fn main() {
    //println!("{}", bool::SOME_CONST); // error; multiple `VALUE` found
    //println!("{}", First::SOME_CONST); // error; type annotations needed
    println!("{}", <bool as First>::SOME_CONST); // 1
    println!("{}", <bool as Second>::SOME_CONST); // 2

    //true.some_method(); // error; multiple applicable items in scope
    // Using a "fully-qualified function call addresses this.
    // Normal function calls are really syntactic sugar for this form.
    // The compiler converts them to this form.
    First::some_method(&true); // First some_method 1
    Second::some_method(&false); // Second some_method 2
}
```

<a name="trait-table"></a>
The following table summarizes the built-in traits.
Those that can be derived (automatically implemented) using the
`#[derive(trait1, trait2, ...)]` attribute are indicted in the "Notes" column.
"Marker traits" are used to indicate a property of a type
without defining any methods and are also indicated in the "Notes" column.
Other traits must be manually implemented.

| Trait Name                | Description                                                                                                                                 | Notes             |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- | ----------------- |
| `std::borrow::Borrow`     | defines `borrow` method that allows an immutable type to be borrowed as a different type<br>(ex. `String` borrowed as `str`)                |                   |
| `std::borrow::BorrowMut`  | defines `borrow_mut` method that allows a mutable type to be borrowed as a different type                                                   |                   |
| `std::borrow::ToOwned`    | defines `to_owned` and `clone_into` methods to construct owned data from a borrow                                                           |                   |
| `std::clone::Clone`       | defines `clone` method that explicitly copies an object                                                                                     | derivable         |
| `std::cmp::Eq`            | defines `eq` method that is used by the `==` and `!=` operators to compare instances                                                        | derivable         |
| `std::cmp::Ord`           | defines `cmp`, `max`, `min`, and `clamp` methods that are used to<br>compare instances using `<`, `<=`, `==`, `!=`, `>=`, and `>` operators | derivable         |
| `std::cmp::PartialEq`     | defines `eq` and `ne` methods for comparing types where some instances are not equal to themselves (2)                                      | derivable         |
| `std::cmp::PartialOrd`    | defines `partial_cmp` method for comparing types where some instances cannot be logically compared to others (3)                            | derivable         |
| `std::convert::AsMut`     | defines `as_mut` method that converts one mutable reference type to another                                                                 |                   |
| `std::convert::AsRef`     | defines `as_ref` method that converts one reference type to another                                                                         |                   |
| `std::convert::From`      | defines `from` associated function that converts one value type to another; ex. `String::from`                                              |
| `std::convert::Into`      | defines `into` method that is the opposite of `From`<br>and is automatically implemented when that is implemented                           |                   |
| `std::convert::TryFrom`   | defines `try_from` method for type conversions that can fail; opposite of `TryInto`                                                         |                   |
| `std::convert::TryInto`   | defines `try_into` method for type conversions that can fail; opposite of `TryFrom`                                                         |                   |
| `std::default::Default`   | defines `default` associated function for getting a default instance of a type                                                              | derivable         |
| `std::fmt::Debug`         | defines `fmt` method that outputs a value for debugging using `{:?}` and `{:#?}` in a format string                                         | derivable         |
| `std::fmt::Display`       | defines `fmt` method that formats a value for output<br>to be seen by a user rather than a developer                                        |                   |
| `std::hash::Hash`         | defines `hash` method for computing the hash value of an instance (1)                                                                       | derivable         |
| `std::io::Read`           | defines `read` method that reads the receiver value into an array of bytes (4)                                                              |                   |
| `std::io::Write`          | defines `write` method that writes data from an array of bytes into the receiver                                                            |                   |
| `std::iter::Extend`       | defines `extend` method that adds items to a collection                                                                                     |                   |
| `std::iter::FromIterator` | defines `from_iter` method that is used to automatically<br>convert an iterator to a collection                                             |                   |
| `std::iter::IntoIterator` | defines `into_iter` method that is used to automatically<br>convert a collection to an iterator over it                                     |                   |
| `std::iter::Iterator`     | defines `next` method for iterating over the data in a value                                                                                |                   |
| `std::iter::Product`      | defines `product` method on an iterator to compute the product of its items                                                                 |                   |
| `std::iter::Sum`          | defines `sum` method for on an iterator to compute the sum of its items                                                                     |                   |
| `std::marker::Copy`       | marks a type whose instances can be implicitly copied by assignment or passing by value                                                     | derivable, marker |
| `std::marker::Send`       | marks a type whose instance ownership can be transferred from one thread to another                                                         | marker            |
| `std::marker::Sized`      | marks a type whose instance sizes are known a compile time                                                                                  | marker            |
| `std::marker::Sync`       | marks a type whose instance references can be shared between threads                                                                        | marker            |
| `std::marker::Unpin`      | marks a type whose instances can be moved after being pinned to a memory location                                                           | marker            |
| `std::ops::Add`           | defines the `+` operator on a type                                                                                                          |                   |
| `std::ops::AddAssign`     | defines the `+=` operator on a type                                                                                                         |                   |
| `std::ops::BitAnd`        | defines the `&` operator on a type                                                                                                          |                   |
| `std::ops::BitAndAssign`  | defines the `&=` operator on a type                                                                                                         |                   |
| `std::ops::BitOr`         | defines the `\|` operator on a type                                                                                                         |                   |
| `std::ops::BitOrAssign`   | defines the `\| =` operator on a type                                                                                                       |                   |
| `std::ops::BitXor`        | defines the `^` operator on a type                                                                                                          |                   |
| `std::ops::BitXorAssign`  | defines the `^=` operator on a type                                                                                                         |                   |
| `std::ops::Deref`         | defines `deref` method that allows smart pointers to be<br>used as immutable references to the data to which they point                     |                   |
| `std::ops::DerefMut`      | defines `deref_mut` method that allows smart pointers to be<br>used as mutable references to the data to which they point                   |                   |
| `std::ops::Div`           | defines the `/` operator on a type                                                                                                          |                   |
| `std::ops::DivAssign`     | defines the `/=` operator on a type                                                                                                         |                   |
| `std::ops::Drop`          | defines `drop` method that is called when a value is dropped, typically to free resources                                                   |                   |
| `std::ops::Fn`            | defines `call` method for closures that borrow values from their environment immutably                                                      |                   |
| `std::ops::FnMut`         | defines `call_mut` method for closures that borrow values from their environment mutably                                                    |                   |
| `std::ops::FnOnce`        | defines `call_once` method for closures that takes ownership of values from their environment;<br>can only be called once                   |                   |
| `std::ops::Index`         | defines `index` method for getting data from a value by index; supports `[index]` syntax                                                    |                   |
| `std::ops::IndexMut`      | defines `index` method                                                                                                                      |                   |
| `std::ops::Mul`           | defines the `*` operator on a type                                                                                                          |                   |
| `std::ops::MulAssign`     | defines the `*=` operator on a type                                                                                                         |                   |
| `std::ops::Neg`           | defines the unary `-` operator on a type                                                                                                    |                   |
| `std::ops::Not`           | defines the unary `!` operator on a type                                                                                                    |                   |
| `std::ops::RangeBound`    | defines the `..` and `..=` operator on a type                                                                                               |                   |
| `std::ops::Rem`           | defines the `%` mod operator on a type                                                                                                      |                   |
| `std::ops::RemAssign`     | defines the `%=` mod operator on a type                                                                                                     |                   |
| `std::ops::Shl`           | defines the `<<` operator on a type                                                                                                         |                   |
| `std::ops::ShlAssign`     | defines the `<<=` operator on a type                                                                                                        |                   |
| `std::ops::Shr`           | defines the `>>` operator on a type                                                                                                         |                   |
| `std::ops::ShrAssign`     | defines the `>>=` operator on a type                                                                                                        |                   |
| `std::ops::Sub`           | defines the `-` operator on a type                                                                                                          |                   |
| `std::ops::SubAssign`     | defines the `-=` operator on a type                                                                                                         |                   |
| `std::str::FromStr`       | defines `from_str` associated function that converts a `&str` value to the implementing type                                                |                   |
| `std::string::ToString`   | defines `to_string` method that is automatically implemented by implementing the `Display` trait                                            |                   |

1. The `hash` method is used by the `HashMap` and `HashSet` collections.
1. This means values are not necessarily reflexive.
   For example, the number value `NaN` is not equal to itself.
1. For example, the number value `NaN` is not
   less than, equal to, or greater than zero.
1. The term "receiver" refers to the value to the left of the dot.

Here is an example of implementing some of the built-in traits
for a custom struct:

```rust
use std::cmp;
use std::default::Default;
use std::ops::{Add, AddAssign, Sub};

#[derive(Clone, Copy, Debug, Default)]
struct Color {
    r: u8,
    g: u8,
    b: u8
}

// Implementing the "Add" trait enables using
// the "+" operator to add "Color" instances.
impl Add for Color {
    // The "Add" trait requires specifying an "associated type"
    // named "Output" that specifies the return type of the "add" method.
    // It could be a type other than "Self" which represents
    // the type on which it is being implemented (Color in this case),
    // but that's what we want here.
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            r: cmp::min(255, self.r + other.r),
            g: cmp::min(255, self.g + other.g),
            b: cmp::min(255, self.b + other.b)
        }
    }
}

// Implementing the "AddAssign" trait enables using the
// "+=" operator to add a "Color" instance to a receiver "Color".
impl AddAssign for Color {
    fn add_assign(&mut self, other: Self) {
        self.r = cmp::min(255, self.r + other.r);
        self.g = cmp::min(255, self.g + other.g);
        self.b = cmp::min(255, self.b + other.b);
    }
}

// Implementing the "Sub" trait enables using the
// "-" operator to subtract "Color" instances.
impl Sub for Color {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            r: cmp::max(0, self.r - other.r),
            g: cmp::max(0, self.g - other.g),
            b: cmp::max(0, self.b - other.b)
        }
    }
}

// Implementing the "SubAssign" trait to enable using the "-=" operator
// would be similar to implementing the "AddAssign" trait.

fn main() {
    let red = Color { r: 255, g: 0, b: 0 };
    let blue = Color { r: 0, g: 0, b: 255 };

    let purple = red + blue; // can add colors
    println!("{:?}", purple); // Color { r: 255, g: 0, b: 255 }

    let blue = purple - red; // can subtract colors
    println!("{:?}", blue); // Color { r: 0, g: 0, b: 255 }

    let mut color = Color::default();
    color += red; // can add a Color to the one on the left side
    color += blue;
    println!("{:?}", color); // Color { r: 255, g: 0, b: 255 }
}
```

Here is an example of implementing the `From` and `Into` traits
on custom structs `Point2D` (a point with x and y coordinates)
and `Point3D` (a point with x, y, and z coordinates).
It implements the following type conversions:

- `Point2D` -> `Point3D` sets `z` to zero.
- `Point2D` -> `f64` calculates the distance from the origin.
- `Point3D` -> `Point2D` discards the `z` coordinate.
- `Point3D` -> `f64` calculates the distance from the origin.

This code can also be found in the GitHub repo {% aTargetBlank
"https://github.com/mvolkmann/rust-from-into", "rust-from-into" %}.

```rust
use std::convert::{From, Into};

#[derive(Debug, Default)]
struct Point2D {
    x: f64,
    y: f64,
}

impl Point2D {
    fn new(x: f64, y: f64) -> Point2D {
        Self { x, y }
    }
}

impl Into<f64> for Point2D {
    fn into(self) -> f64 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

impl From<&Point3D> for Point2D {
    fn from(p3: &Point3D) -> Self {
        Self { x: p3.x, y: p3.y }
    }
}

#[derive(Debug, Default)]
struct Point3D {
    x: f64,
    y: f64,
    z: f64,
}

impl Point3D {
    fn new(x: f64, y: f64, z: f64) -> Point3D {
        Self { x, y, z }
    }
}

impl Into<f64> for Point3D {
    fn into(self) -> f64 {
        (self.x.powi(2) + self.y.powi(2) + self.z.powi(2)).sqrt()
    }
}

impl From<&Point2D> for Point3D {
    fn from(p2: &Point2D) -> Self {
        Self {
            x: p2.x,
            y: p2.y,
            z: 0.0,
        }
    }
}

fn main() {
    let p2 = Point2D::new(3.0, 4.0);

    // One way to convert a Point2D to a Point3D.
    let p3 = Point3D::from(&p2);
    println!("p2 to 3D = {:?}", p3);

    // Another way to convert a Point2D to a Point3D.
    // Parens are required around the reference because the
    // dot operator for the call to "into" has higher precedence.
    let p3: Point3D = (&p2).into();
    println!("p2 to 3D = {:?}", p3);

    let distance: f64 = p2.into();
    println!("p2 distance from origin = {}", distance);

    let p3 = Point3D::new(3.0, 4.0, 5.0);

    // One way to convert a Point3D to a Point2D.
    let p2 = Point2D::from(&p3);
    println!("p3 to 2D = {:?}", p2);

    // Another way to convert a Point3D to a Point2D.
    let p2: Point2D = (&p3).into();
    println!("p3 to 2D = {:?}", p2);

    let distance: f64 = p3.into();
    println!("p3 distance from origin = {}", distance);
}
```

Here is an example of implementing a custom trait
on built-in types, in this case `str` and `String`:

```rust
use itertools::join;

trait Case {
    fn to_camel(&self) -> String;
    fn to_snake(&self) -> String;
}

impl Case for str {
    fn to_camel(&self) -> String {
        let word_tuples = self.split_whitespace().enumerate();
        let word_iter = word_tuples.map(|(index, s)| {
          let word = s.to_lowercase();
          if index == 0 {
              return word;
          }
          // Change first letter to uppercase.
          let mut chars = word.chars(); // iterator over characters
          let first = chars.next().unwrap().to_uppercase();
          let rest = chars.as_str();
          format!("{}{}", first, rest)
        });
        join(word_iter, "")
    }

    fn to_snake(&self) -> String {
        join(self.split_whitespace().map(|s| s.to_lowercase()), "_")
    }
}

impl Case for String {
    fn to_camel(&self) -> String {
        self.as_str().to_camel() // uses str implementation
    }

    fn to_snake(&self) -> String {
        self.as_str().to_snake() // uses str implementation
    }
}

fn main() {
    let s1 = "Foo BAR bAZ";
    let s2= String::from(s1);

    println!("{}", s1.to_camel()); // fooBarBaz
    println!("{}", s2.to_camel()); // fooBarBaz

    println!("{}", s1.to_snake()); // foo_bar_baz
    println!("{}", s2.to_snake()); // foo_bar_baz
}
```

Traits can be used as parameter and return types to
specify that any type which implements them can be substituted.
Specifying traits as a qualifier on a generic type
is referred to a "trait bound".
In the `print_string` function below,
the type of the first parameter is `&str`
and the type of the second is a reference to any type that
implements both the `Debug` and `ToString` traits.
The compiler will generate separate versions of the function
for each concrete type passed as the second argument.

```rust
use std::fmt::Debug;

#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

impl ToString for Point2D {
    fn to_string(&self) -> String {
        format!("({}, {})", self.x, self.y)
    }
}

// There are three ways to write the function signature
// that all specify that the "value" parameter
// must implement both the "Debug" and "ToString" traits:
// 1) fn print_string(label: &str, value: &(impl Debug + ToString)) {
// 2) fn print_string<T: Debug + ToString>(label: &str, value: &T) {
// 3) fn print_string<T>(label: &str, value: &T)
//    where T: Debug + ToString {
// Option 3 is often used when there are multiple generic parameters
// or they have more than one trait bound.
fn print_string<T>(label: &str, value: &T)
where T: Debug + ToString {
  println!("{}: {}", label, value.to_string());
  println!("debug: {:?}", value);
}

fn main() {
  print_string("Name", &"Mark");
  // Name: Mark
  // debug: "Mark"

  print_string("Score", &19); // Score: 19
  // Score: 19
  // debug: 19

  let pt = Point2D { x: 1.2, y: 3.4 };
  print_string("Point", &pt);
  // Point: (1.2, 3.4)
  // debug: Point2D { x: 1.2, y: 3.4 }
}
```

Another approach is to use the `dyn` keyword.
This causes the compiler to generate a single version of a function
that uses runtime dynamic dispatch.
Here is a new version of the `print_string` function that does this:

```rust
// We need to define a new trait that combines the desired traits
// because only one trait name can follow the `dyn` keyword.
trait Printable: Debug + ToString {}
impl<T: Debug + ToString> Printable for T {}

fn print_string(label: &str, value: &dyn Printable) {
  println!("{}: {}", label, value.to_string());
  println!("debug: {:?}", value);
}
```

Writing functions that operate on any numeric type
can be done using trait bounds.
There are three ways to specify them.
One way is shown in this example.

```rust
// The trait bound "Copy + Into<f32>" means that this
// takes a slice of any type that can be copied
// and can be converted to the f32 type.
// This is true of all the primitive numbers types except f64.
// This approach is more flexible than declaring the parameter
// to be "&[i32]" which only accepts a slice of i32 values.
fn sum<T>(numbers: &[T]) -> f32
where T: Copy + Into<f32> {
    // The map part below can also be written as ".map(|x| x.into())".
    numbers.iter().copied().map(Into::into).sum::<f32>()
}

fn average<T>(numbers: &[T]) -> f32
where T: Copy + Into<f32> {
    sum(numbers) / numbers.len() as f32
}

fn main() {
    let numbers = [200u8, 255u8, 3u8];
    let total = sum(&numbers);
    println!("total = {}", total); // 458 which would overflow u8

    let scores: Vec<u8> = vec![70, 90, 85, 100];

    // Print average of all scores.
    println!("average = {:.1}", average(&scores)); // 86.2

    // Print average of all scores except the first.
    println!("average = {:.1}", average(&scores[1..])); // 91.7
}
```

Alternatively we can define a new trait that
combines several others and then use it as a trait bound.
With the following in place, we can use the trait `Number`
in the code above in place of `Copy + Into<f32>`.
We can also use it in functions that need the capabilities
of the additional traits from the `std::ops` namespace.

```rust
use std::ops::*;
trait Number:
    Add + AddAssign + Copy + Div + DivAssign +
    Into<f64> + Mul + MulAssign + Sub + SubAssign {}
impl<T> Number for T where
    T: Add + AddAssign + Copy + Div + DivAssign +
       Into<f64> + Mul + MulAssign + Sub + SubAssign {}
```

A similar approach is used by {% aTargetBlank
"https://docs.rs/num-traits/", "num_traits" %} library crate.

Another approach is to use the {% aTargetBlank
"https://crates.io/crates/num", "num" %} crate.
Add this as a dependency in `Cargo.toml` with a line like `num = "0.3.1"`.

For example:

```rust
extern crate num;
use core::ops::AddAssign;
use num::{Num, ToPrimitive};

// T can be any type that implements the traits
// AddAssign, Copy, Num, and ToPrimitive.
// The built-in primitive number types like i32, u8, and f32
// all implement the AddAssign and Copy traits.
// The num crate adds implementations of
// Num and ToPrimitive to those same types.
// So T can be any built-in numeric type.
fn average<T: AddAssign + Copy + Num + ToPrimitive>(numbers: &[T]) -> f32 {
    // The Num trait requires also implementing the Zero trait
    // which defines the zero function.
    // That returns the zero value for the wrapped primitive type.
    let mut sum = T::zero();

    for n in numbers {
        sum += *n; // requires implementing the AddAssign trait
    }
    let numerator = sum.to_f32().unwrap();
    numerator / numbers.len() as f32
}
```

The following table maps built-in traits
to the commonly used built-in types that implement them.
Knowing this is useful for determining the trait bounds that can be used
to write functions that support arguments of multiple types.
The goal is provide a sense for the use of each trait.
See the official documentation for details.

Recall that Rust supports the following built-in scalar (single value) types:

- `bool`
- `char`
- number: float and integer
- float: `f32` and `f64`
- integer: signed and unsigned
- signed integer: `i8`, `i16`, `i32`, `i64`, `i128`, and `isize`
- unsigned integer: `u8`, `u16`, `u32`, `u64`, `u128`, and `usize`

The compound types (multiple values) with fixed sizes include arrays and tuples.
When these are listed as a implementing type below,
this is only the case if their items implement the trait.

In the lists of implementing types:

- B means all the built-in scalar and compound types.
- C means any type that implements the `Clone` trait or slices of such types.
- S means any type with size known at compile type or slices of such types.
- Often there are restrictions on an implementing type.
  For example, when `HashMap` is listed, there may be restrictions on
  the types of keys and values that can be used.

| Trait                     | Implementing Types                                                                                                                                               |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `std::borrow::Borrow`     | S, scalar, compound, `Arc`, `Box`, `Rc`, `String`, `Vec`                                                                                                         |
| `std::borrow::BorrowMut`  | S, array, `Box`, `String`, `Vec`                                                                                                                                 |
| `std::borrow::ToOwned`    | C, `str`                                                                                                                                                         |
| `std::clone::Clone`       | B, S, `Arc`, `Box`, `Error`, `HashMap`, `HashSet`, `LinkedList`, `Rc`, `Result`, `String`, `Vec`, and many more                                                  |
| `std::cmp::Eq`            | B, arrays, ranges, slices, tuples, `Arc`, `Box`, `Duration`, `Error`, `HashMap`, `HashSet`, `LinkedList`, `Rc`, `Result`, `String`, `Vec`, and many more         |
| `std::cmp::Ord`           | B, S, arrays, slices, tuples, `Arc`, `Box`, `Duration`, `Error`, `LinkedList`, `Option`, `Rc`, `Result`, `String`, `Vec`                                         |
| `std::cmp::PartialEq`     | B, S, arrays, ranges, slices, tuples, `Arc`, `Box`, `Duration`, `HashMap`, `HashSet`, `LinkedList`, `str`, `String`, `Vec`                                       |
| `std::cmp::PartialOrd`    | B, arrays, tuples, `Arc`, `Box`, `Duration`, `Error`, `LinkedList`, `Option`, `Rc`, `Result`, `str`, `String`, `Vec`                                             |
| `std::convert::AsMut`     | arrays, slices, `Box`, `String`, `Vec`                                                                                                                           |
| `std::convert::AsRef`     | arrays, slices, `Arc`, `Box`, `Rc`, `str`, `String`, `Vec`                                                                                                       |
| `std::convert::From`      | B (not `bool`), `Arc`, `Box`, `Error`, `String`, `Vec`                                                                                                           |
| `std::convert::Into`      | any type that implements the `From` trait                                                                                                                        |
| `std::convert::TryFrom`   | B (not `bool`), arrays (and arrays wrapped by `Arc`, `Box`, or `Rc`), `Vec`                                                                                      |
| `std::convert::TryInto`   | any type that implements the `TryFrom` trait                                                                                                                     |
| `std::default::Default`   | B, arrays, ranges, slices, tuples, `Arc`, `Duration`, `Error`, `HashMap`, `HashSet`, `LinkedList`, `Rc`, `String`, `Vec`                                         |
| `std::fmt::Debug`         | B, arrays, ranges, slices, tuples, `Arc`, `Box`, `Duration`, `Error`, `HashMap`, `HashSet`, `Rc`, `str`, `String`                                                |
| `std::fmt::Display`       | B, `Arc`, `Box`, `Error`, `Rc`, `str` `String`                                                                                                                   |
| `std::hash::Hash`         | B, arrays, ranges, slices, tuples, `Arc`, `Box`, `LinkedList`, `Rc`, `Result`, `Vec`                                                                             |
| `std::io::Read`           | `BufReader`, `File`, `Stdin`, `TcpStream`, `UnixStream`                                                                                                          |
| `std::io::Write`          | `BufWriter`, `File` `LineWriter`, `Stderr`, `Stdout`, `TcpStream`, `UnixStream`                                                                                  |
| `std::iter::Extend`       | `HashMap`, `HashSet`, `LinkedList`, `String`, `Vec`                                                                                                              |
| `std::iter::FromIterator` | `Arc`, `Box`, `HashMap`, `HashSet`, `LinkedList`, `Option`, `Rc`, `Result`, `String`, `Vec`                                                                      |
| `std::iter::IntoIterator` | arrays, slices, `HashMap`, `LinkedList`, `Option`, `Result`, `Vec`                                                                                               |
| `std::iter::Iterator`     | ranges and many more                                                                                                                                             |
| `std::iter::Product`      | numbers, `Option`, `Result`                                                                                                                                      |
| `std::iter::Sum`          | numbers, `Option`, `Result`                                                                                                                                      |
| `std::marker::Copy`       | B, arrays, tuples, `Error`, `Result`                                                                                                                             |
| `std::marker::Send`       | B, arrays, ranges, slices, `Arc`, `Box`, `Cell`, `Error`, `HashMap`, `HashSet`, `LinkedList`, `RefCell`, `Vec`                                                   |
| `std::marker::Sized`      | From the docs, "All type parameters have an implicit bound of `Sized`.<br>The special syntax `?Sized` can be used to remove this bound if it's not appropriate." |
| `std::marker::Sync`       | B, arrays, ranges, slices, `Arc`, `Box`, `Duration`, `Error`, `HashMap`, `HashSet`, `LinkedList`, `Result`, `String`, `Vec`                                      |
| `std::marker::Unpin`      | B, arrays, ranges, slices, `Arc`, `Box`, `Cell`, `Duration`, `HashMap`, `HashSet`, `LinkedList`, `Ref`, `RefCell`, `Result`, `String`, `Vec`                     |
| `std::ops::Add`           | numbers, `Duration`, `String`                                                                                                                                    |
| `std::ops::AddAssign`     | numbers                                                                                                                                                          |
| `std::ops::BitAnd`        | `bool`, numbers                                                                                                                                                  |
| `std::ops::BitAndAssign`  | `bool`, numbers                                                                                                                                                  |
| `std::ops::BitOr`         | `bool`, numbers                                                                                                                                                  |
| `std::ops::BitOrAssign`   | `bool`, numbers                                                                                                                                                  |
| `std::ops::BitXor`        | `bool`, numbers                                                                                                                                                  |
| `std::ops::BitXorAssign`  | `bool`, numbers:                                                                                                                                                 |
| `std::ops::Deref`         | `Arc`, `Box`, `Rc`, `Ref`, `RefMut`, `String`, `Vec`                                                                                                             |
| `std::ops::DerefMut`      | `Box`, `String`, `Vec`                                                                                                                                           |
| `std::ops::Div`           | numbers                                                                                                                                                          |
| `std::ops::DivAssign`     | numbers                                                                                                                                                          |
| `std::ops::Drop`          | `Arc`, `Box`, `LinkedList`, `Rc`                                                                                                                                 |
| `std::ops::Fn`            |                                                                                                                                                                  |
| `std::ops::FnMut`         |                                                                                                                                                                  |
| `std::ops::FnOnce`        |                                                                                                                                                                  |
| `std::ops::Index`         | slices, `HashMap`, `str`, `String`, `Vec`                                                                                                                        |
| `std::ops::IndexMut`      | slices, `str`, `String`, `Vec`                                                                                                                                   |
| `std::ops::Mul`           | numbers                                                                                                                                                          |
| `std::ops::MulAssign`     | numbers                                                                                                                                                          |
| `std::ops::Neg`           | numbers                                                                                                                                                          |
| `std::ops::Not`           | `bool`, numbers                                                                                                                                                  |
| `std::ops::RangeBound`    | ranges                                                                                                                                                           |
| `std::ops::Rem`           | numbers                                                                                                                                                          |
| `std::ops::RemAssign`     | ranges                                                                                                                                                           |
| `std::ops::Shl`           | numbers                                                                                                                                                          |
| `std::ops::ShlAssign`     | numbers                                                                                                                                                          |
| `std::ops::Shr`           | numbers                                                                                                                                                          |
| `std::ops::ShrAssign`     | numbers                                                                                                                                                          |
| `std::ops::Sub`           | numbers                                                                                                                                                          |
| `std::ops::SubAssign`     | numbers                                                                                                                                                          |
| `std::str::FromStr`       | B, `String`                                                                                                                                                      |
| `std::string::ToString`   | `char`, `str`, `String`                                                                                                                                          |

## <a name="macros">Macros</a>

Macros generate code at compile-time.
There are two ways to define a macro: "declarative" and "procedural".
There are three ways to use macros: attribute-like, derive, and function-like.
Macros defined with the procedural syntax can be used in any of these ways.
But macros defined with the declarative syntax
can only create function-like macros.

Declarative macros are defined with `macro_rules!`.
They specify patterns to match, similar to a `match` expression,
and code to replace what is matched.
One example is the `vec` macro that is used create instances of the `Vec` type.

Attribute-like macros annotate the code that follows.
Examples include:

- `allow` disables certain warnings (ex. `unused` variables and functions)
- `cfg` conditionally includes an item based on a predicate (ex. configuration value or target OS)
- `deprecated` marks an item as deprecated so usages of it will trigger a warning message
- `derive` derives the implementation of specified traits on a type such as a struct
- `feature` enables unstable or experimental compiler features
- `macro_use` to expand the scope of a macro definition or import macros from another crate
- `should_panic` indicates that a test function is expected to panic
- `test` identifies a test function

Derive macros specify code to be added to implement traits
when the `derive` attribute is applied to a type such as a struct.
For example, `#[derive(Clone, Copy, Debug)]`.

Function-like macros are invoked like Rust functions,
but with `!` at the end of their name.
Unlike functions, they can take a variable number of arguments.
Examples include all the macros in the table below.

| Name              | Description                                                                                                       |
| ----------------- | ----------------------------------------------------------------------------------------------------------------- |
| `assert`          | takes a boolean expression and panics if it is false                                                              |
| `assert_eq`       | takes two arguments and panics if they are not equal                                                              |
| `assert_ne`       | takes two arguments and panics if they are equal                                                                  |
| `dbg`             | takes expressions separated by commas and<br>prints each followed by `=` and its value on separate lines          |
| `debug_assert`    | same as `assert`, but not enabled in release builds                                                               |
| `debug_assert_eq` | same as `assert_eq`, but not enabled in release builds                                                            |
| `debug_assert_ne` | same as `assert_ne`, but not enabled in release builds                                                            |
| `env`             | takes an environment variable name and is replaced by its value                                                   |
| `eprint`          | same as `print`, but prints to stderr instead of stdout                                                           |
| `eprintln`        | same as `println`, but prints to stderr instead of stdout                                                         |
| `file`            | expands to the relative path to the current source file                                                           |
| `format`          | takes a format string and expressions and returns a `String`                                                      |
| `include_bytes`   | takes a relative file path and returns the contents as a byte array                                               |
| `include_str`     | takes a relative file path and returns the contents as a Unicode string                                           |
| `line`            | expands to the line number of the current source line                                                             |
| `matches`         | takes an expression and a pattern and returns a boolean indicating whether it matches the pattern                 |
| `option_env`      | takes an environment variable name and expands to an `Option` enum;<br>`Some(value)` if defined, `None` otherwise |
| `panic`           | exits program and outputs a stack trace and error message                                                         |
| `print`           | takes a format string and expressions and prints them                                                             |
| `println`         | same as `print`, but adds a newline at the end                                                                    |
| `todo`            | panics with the message "not yet implemented"                                                                     |
| `unimplemented`   | panics with the message "not implemented"                                                                         |
| `unreachable`     | panics with the message "entered unreachable code"                                                                |
| `vec`             | creates a `Vec` containing specified items listed in square brackets                                              |
| `write`           | like `print`, but writes to a byte buffer                                                                         |
| `writeln`         | like `write`, but adds a newline at the end                                                                       |

The assert macros are used in tests and to check
pre-conditions that must hold at the beginning of functions.

The `dbg` macro is incredibly useful for debugging!
For example, rather than writing `println!("score = {}", score);`
write `$dbg(score)`.

The following macros all take a format string
and expressions to insert into the format string:
`eprint`, `eprintln`, `format`, `panic`, `println`, `write`, and `writeln`.

The `file` and `line` macros are useful for debugging output.
For example:

```rust
println!("in {} on line {} x = {}", file!(), line!(), x);
```

The `todo` and `unimplemented` macro is called from functions that have
not yet been implemented so that calls to them will be flagged.
From the official documentation, "The difference ... is that
while todo! conveys an intent of implementing the functionality later ...,
unimplemented! makes no such claims."

The `unreachable` macro is called in code paths that should never be reached.
An example is in the last arm of a `match` expression
that uses a wildcard (`_`) on the left side
where all the expected conditions are described in the preceding match arms.

Another source of macros is the external crate
`log` which provides macros for logging.
It defines the macros `debug`, `error`, `info`, `log`, `trace`, and `warn`.

Implementing macros is an advanced topic.
We will just show some basic examples here.
Declarative macros are defined with `macro_rules!`.
They specify code patterns to match and code to replace the match.

Here is a macro that expands on the provided `dbg` macro
by outputting the relative file path and line number of the call.

```rust
macro_rules! debug {
    ($ex:expr) => {
        println!("{}@{}: {} = {}", file!(), line!(), stringify!($ex), $ex);
    };
}

fn main() {
    debug!(2 + 3); // outputs src/main.rs@8: 2 + 3 = 5
}
```

In the [Built-in Scalar Types](#scalar-types) section
we included examples of adding methods to built-in types.
This can also be accomplished with declarative macros as shown below:

```rust
trait Days {
    fn days_from_now(self) -> String;
}

macro_rules! implement_days {
    ($t: ty) => { // "ty" matches a Rust type
        impl Days for $t {
            fn days_from_now(self) -> String {
                let s = match self {
                    -1 => "yesterday",
                    0 => "today",
                    1 => "tomorrow",
                    _ => {
                        if self > 0 {
                            "future"
                        } else {
                            "past"
                        }
                    }
                };
                s.to_string()
            }
        }
    };
}

implement_days! {i8}
implement_days! {i16}
implement_days! {i32}

fn main() {
    let d8: i8 = 1;
    let d16: i16 = -1;
    println!("{}", d8.days_from_now()); // tomorrow
    println!("{}", d16.days_from_now()); // yesterday
    println!("{}", 0.days_from_now()); // today
}
```

In the example above, each call to the `implement_days!` macro
adds the `days_from_now` method to a single type.
To allow adding the the method to multiple types in one call,
change the first line inside the macro to the following:

```rust
    ($($t: ty), *) => { $(
```

Then add the line `)*` after the end of the `impl` block.
Now the macro can be called as follows
to add the `days_from_now` method to several types:

```rust
implement_days! { i8, i16, i32, i64, i128 }
```

The built-in, declarative macro `vec` is defined as follows:

```rust
macro_rules! vec {
    () => (
        $crate::vec::Vec::new()
    );
    ($elem:expr; $n:expr) => (
        $crate::vec::from_elem($elem, $n)
    );
    ($($x:expr),+ $(,)?) => (
        <[_]>::into_vec(box [$($x),+])
    );
}
```

TODO: What is the syntax for defining a procedural macro?

## <a name="tests">Tests</a>

Rust supports three kinds of tests:
doc tests, unit tests, and integration tests.
Examples of each kind of test can be found in the Rust application at
{% aTargetBlank "https://github.com/mvolkmann/rust-poker", "rust-poker" %}.
All of these tests are executed by entering `cargo test`.
To run only test files whose names match a pattern,
enter `cargo test {pattern}`.

By default output to stdout is captured and not displayed.
To run the tests and see this output, enter `cargo test -- --nocapture`.

Doc tests are placed in special comments above the library functions they test.
A nice feature of these tests is that they provide examples of using
the function they test along with other comments about the function.
A downside is that because they appear in a comment,
editors like VS Code do not inspect the code for syntax and usage errors.
Note that placing doc tests above non library functions has no effect.

Here is an example from the `src/lib.js` file in the rust-poker app.
This function takes a `Hand` struct that has
a `cards` field with the type `Vec<Card>`.
It also takes a `rank` that is a `char` representing
a card rank such as `K` for king.
The function returns the suit (a Unicode character)
of the first card found with a matching rank.

````rust
/// ```
/// use std::str::FromStr; // needs to be in scope
/// let hand = poker::Hand::from_str("Q♥ 9♣ J♦ 8♠ 10♥").unwrap();
/// assert_eq!(poker::get_suit(&hand, 'J'), '♦');
/// ```
pub fn get_suit(hand: &Hand, rank: char) -> char {
    if let Some(card) = hand.cards.iter().find(|&c| c.rank == rank) {
        card.suit
    } else {
        '?'
    }
}
````

The file `src/lib.rs` has doc tests above each of the functions it defines.
Examine this file for additional doc test examples.

Unit tests are placed in the source files of the functions they test.
Each unit test is meant to test a single function in isolation from the others,
although nothing enforces this.
Unlike doc tests, these are not placed in comments.
This enables editors like VS Code inspect the code for syntax and usage errors.

Here is an example from the `src/lib.js` file in the rust-poker app.
It tests the `suit_name` function that takes a suit Unicode character
and returns the name of the suit.
It also tests the `Hand::deal` function that deals a set of random cards.

```rust
#[cfg(test)]
mod tests {
    // This makes all the functions in the containing module available.
    use super::*;

    #[test]
    fn it_gets_suit_name() {
        assert_eq!(suit_name('♣'), "clubs");
        assert_eq!(suit_name('♦'), "diamonds");
        assert_eq!(suit_name('♥'), "hearts");
        assert_eq!(suit_name('♠'), "spades");
    }

    #[test]
    //#[ignore] to temporarily ignore a test
    fn it_deals() {
        let size = 5;
        let hand = Hand::deal(size);
        assert_eq!(hand.cards.len(), size);
        // We could make more assertions here, but
        // other tests cover the creation of individual cards.
    }
}
```

Integration tests are placed in separate source files
under the `test` directory.
Unlike unit tests, these are meant to test
multiple functions in conjunction with each other.

Here is an example from the `tests/tests.rs` file in the rust-poker app.
It tests the `evaluate` method of the `Hand` struct which
determines the type of hand such as "full house", "flush", or "straight".

```rust
use poker;
use poker::Hand;
use std::str::FromStr;

#[test]
fn it_evaluates_hand() {
    let hand = Hand::from_str("A♥ K♥ Q♥ J♥ 10♥").unwrap();
    assert_eq!(hand.evaluate(), "royal flush");

    let hand = Hand::from_str("Q♥ J♥ 10♥ 9♥ 8♥").unwrap();
    assert_eq!(hand.evaluate(), "straight flush");

    let hand = Hand::from_str("Q♥ 7♥ Q♣ Q♦ Q♠").unwrap();
    assert_eq!(hand.evaluate(), "4 of a kind of queens");

    let hand = Hand::from_str("Q♥ 7♥ Q♣ Q♦ 7♠").unwrap();
    assert_eq!(hand.evaluate(), "full house");

    let hand = Hand::from_str("Q♥ 7♥ 3♥ A♥ 9♥").unwrap();
    assert_eq!(hand.evaluate(), "flush");

    let hand = Hand::from_str("Q♥ 9♣ J♦ 8♠ 10♥").unwrap();
    assert_eq!(hand.evaluate(), "straight");

    let hand = Hand::from_str("Q♥ 7♥ Q♣ Q♦ J♠").unwrap();
    assert_eq!(hand.evaluate(), "3 of a kind of queens");

    let hand = Hand::from_str("Q♥ 7♥ Q♣ 5♦ 7♠").unwrap();
    assert_eq!(hand.evaluate(), "two pairs");

    let hand = Hand::from_str("Q♥ 7♥ Q♣ 5♦ J♠").unwrap();
    assert_eq!(hand.evaluate(), "pair of queens");

    let hand = Hand::from_str("Q♥ 7♥ J♣ 5♦ A♠").unwrap();
    assert_eq!(hand.evaluate(), "high card ace of ♠");
}
```

## <a name="standard-io">Standard IO</a>

The `std::io` namespace supports many input/output operations.
The members `stdin` and `stdout` are functions that return objects
with methods for operating on the `stdin` and `stdout` streams.

The `stdin` methods like `read_line` and
`stdout` methods like `write` and `flush`
return a `Result` enum value to enable representing
successful operations and errors.
For example:

```rust
// The Write trait must be in scope in order to use the flush method.
use std::io::{stdin, stdout, Write};

fn main() {
    let mut buffer = String::new();

    loop {
        // Output a prompt.
        print!("Command: ");
        // Flush stdio without writing a newline character.
        stdout().flush().unwrap();

        // Read a line of input from the user.
        stdin().read_line(&mut buffer).unwrap();
        // Remove the newline character from the end of the buffer.
        buffer.pop();

        // Break out of the loop if the user entered "quit".
        if buffer == "quit" {
            break;
        }

        println!("You entered {}.", buffer);

        // Prepare to reuse the buffer.
        buffer.clear();
    }
}
```

Here is a modified version of the code above
that uses the `text_io` crate which simplify it.
To use this, add the dependency `text_io = "0.1.8"` in the `Cargo.toml` file.
This version also adds a `print` function to simplify
writing to `stdout` without including a newline.
The `main` function in this version is much simpler!

```rust
use std::io::{self, Write};

use text_io::read;

fn print(text: &str) {
  let mut stdout = io::stdout();
  stdout.write(text.as_bytes()).unwrap();
  stdout.flush().unwrap();
}

fn main() {
  loop {
    print("Command: ");
    let command: String = read!("{}\n"); // reads until newline and omits it
    if command == "quit" {
      break;
    }
    println!("You entered {}.", command);
  }
}
```

## File IO

The `std::fs` and `std::io` modules enable reading and writing from files.
For example:

```rust
use std::fs::{File, read_to_string};
use std::io::{BufReader, BufWriter, Result};
// The following is required to gain access to the
// "lines" and "write" methods.
use std::io::prelude::*;

fn write_file(path: &str) -> Result<()> {
    // To write entire file from one string ...
    //let mut f = File::create(path)?;
    //f.write_all(b"Hello\nWorld\n")

    // To write one line at a time ...
    let f = File::create(path)?;
    let mut writer = BufWriter::new(f);
    // The write method takes a byte slice
    // and returns the number of bytes written.
    // This shows two ways to create a byte slice from a string.
    writer.write(b"Hello\n")?;
    writer.write("World\n".as_bytes())?;
    Ok(())
}

fn read_file(path: &str) -> Result<()> {
    // To read entire file into a string ...
    //let content = read_to_string(path)?;
    //println!("content = {}", content);

    // To read one line at a time ...
    let f = File::open(path)?;
    let reader = BufReader::new(f);
    for line in reader.lines() {
        println!("line = {}", line?);
    }

    Ok(()) // returning unit value
}

fn main() -> Result<()> {
    let path = "demo.txt";
    write_file(path)?;
    read_file(path)
}
```

To read and write JSON files, consider using
{% aTargetBlank "https://github.com/serde-rs/json", "Serde JSON" %}.
The name is short for "<b>ser</b>ializing and <b>de</b>serializing".
This requires adding the following dependencies in `cargo.toml`:

```toml
serde = { version = "1.0.118", features = ["derive"] }
serde_json = "1.0.60"
```

Rust allows libraries to "feature-gate" {% aTargetBlank
"https://doc.rust-lang.org/cargo/reference/features.html",
"feature-gate" %} some their features.
This enables reduced compile times and generated binary sizes
for applications that do not use all the features of a library.
The "features" option above specifies that we wish to use
parts of the "derive" feature of the "serde" library,
which is a gated feature.

Here is an example of writing and reading a JSON file:

```rust
use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Deserialize, Serialize, Debug)]
struct Dog {
    name: String,
    breed: String,
}

fn main() {
    let mut dogs = Vec::new();
    dogs.push(Dog {
        name: "Comet".to_string(),
        breed: "Whippet".to_string(),
    });
    dogs.push(Dog {
        name: "Oscar".to_string(),
        breed: "German Shorthaired Pointer".to_string(),
    });

    // Produce JSON from the dogs vector.
    let json = serde_json::to_string(&dogs).unwrap();
    // To produced pretty-printed JSON, use the following instead:
    // let json = serde_json::to_string_pretty(&dogs).unwrap();
    println!("json = {}", json);

    // Parse the JSON to obtain a vector of Dog instances.
    let dogs: Vec<Dog> = serde_json::from_str(&json).unwrap();
    println!("dogs = {:#?}", dogs);
}
```

The JSON produced by the code above is the following, all on a single line:

```json
[
  {"name": "Comet", "breed": "Whippet"},
  {"name": "Oscar", "breed": "German Shorthaired Pointer"}
]
```

## Command-line Arguments

Command-line arguments can be passed to a Rust program
by listing them after the executable path.
To execute a Rust program using `cargo run` and
pass it command-line arguments, specify them after `--`
as follows: `cargo run -- arg1 arg2 ...`

In either case, the arguments are available in the iterator `std::env::args`.
The `collect` method can be used to create a vector of the arguments.
The first item is the path the executable and
the remaining items are the actual arguments.
For example:

```rust
let args: Vec<String> = std::env::args().collect();
```

Command-line arguments can represent options
that affect what an application does.
A library crate like {% aTargetBlank "https://crates.io/crates/clap", "clap" %}
can be used to simplify parsing of the options and provide help.

The `clap` crate supports many features including the ability to:

- generate help viewed with `--help` (not with `-h`)
- get the program version with `--version` or `-V` (not with `-v`)
  specified in the code or obtained from the `Cargo.toml` file
- parse positional arguments (known as "args")
- parse optional named arguments with no value,
  also known as "flags" (ex. `--quiet`)
- parse named arguments with values,
  also known as "options" (ex. `--color yellow`)
- parse arguments with default values
- define when an argument is allowed or required
  (see the `Arg` methods `required_if`, `required_ifs`,
  `required_unless`, `required_unless_all`, and `required_unless_one`)
- specify options with YAML (However, a benefit of specifying them
  in code is that an IDE can detect and report errors.)
- produce colored error messages
- parse non-string option values (ex. `bool`, `i32`, or `f64`)
- implement custom value validation
- and more

Named arguments can have short (ex. `-q`) and long (ex. `--quiet`) forms.

Flags can be combined.
This means that `-abc` is treated the same as `-a -b -c`.

Each argument must have a name that is used to retrieve its value.
While it typically matches its long name, specifying this name is required
because positional options are not required to have a long or short name.

To demonstrate using `clap` we will create a program that accepts:

- a positional argument with a type of `u8` that specifies a size
- an optional flag that causes output to be minimized (`--quiet`)
- a required option that supplies a color name (`--color`)

To get help on the app, enter `cargo run -- --help` or
build the app with `cargo build` and
enter `./target/debug/clap-demo --help`.

To run the app, enter `cargo run -- 19 --color yellow -q` or
build the app with `cargo build` and
enter `./target/debug/clap-demo -- 19 --color yellow -q`.

```rust
// The following attribute enables using macros defined in the clap crate.
#[macro_use]
extern crate clap;

use clap::Arg;
// Use the following line instead if the
// alternate approach described below is selected.
//use clap::{App, Arg};

// This is a custom validator function that
// is passed to the `validator` method below.
fn validate_color(color: String) -> Result<(), String> {
    if color == "white" {
        Err("white is not a valid choice".to_string())
    } else {
        Ok(())
    }
}

fn main() {
    // The code below gets some values from the Cargo.toml file.
    // To supply those values in the code here, use the following:
    // let matches = App::new("clap-demo) // next line preferred
    // let matches = App::new(crate_name!()) // gets from Cargo.toml
    //     .about("This demonstrates the use of clap.") // next line preferred
    //     .about(crate_description!()) // gets from Cargo.toml
    //     .author("R. Mark Volkmann") // next line preferred
    //     .author(crate_authors!()) // gets from Cargo.toml
    //     .version("1.0") // next line preferred
    //     .version(crate_version!()) // gets from Cargo.toml

    // The preferred approach is to get
    // all possible values from Cargo.toml file.
    // The `app_from_crate!` macro does this by combining
    // the use of all the macros used above.
    let matches = app_from_crate!()
        // This adds text before the generated help text.
        .before_help("Welcome to my demo!") // optional; rarely used
        // This adds text after the generated help text.
        .after_help("Have fun!") // optional; rarely used

        // Good usage text is generated automatically,
        // but it can be overridden as follows:
        //.usage("overridden usage text")

        // This is a named argument that doesn't have a value (a "flag")
        // and is either present or not.
        .arg(
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .help("minimizes output"),
        )

        // This is a named argument that has a value (an "option").
        // The name can be separated from the value with a space or "=".
        // For example, "--color yellow" or "--color=yellow".
        .arg(
            Arg::with_name("color")
                .long("color")
                .short("c")
                .takes_value(true) // makes this an "option"
                .default_value("black")
                .value_name("COLOR")
                .help("your favorite color")
                .validator(validate_color),
        )

        // This argument is positional (an "arg").
        // Positional arguments can come before or after named arguments.
        // Their order is the order in which they are defined here
        // unless the "index" method is called.
        // It specifies the position and is only used on positional arguments.
        // When the "index" method is used, "takes_value" defaults to "true".
        .arg(
            Arg::with_name("size")
                .takes_value(true)
                .help("how big?")
                .required(true),
        )

        .get_matches(); // parses command-line arguments

    // The "value_of" method returns an "Option" enum, with
    // "Some" wrapping the value or "None" when the option is not present.
    // We can safely call "unwrap" for required options or
    // options with a default value, instead of calling "unwrap_or"
    // which supplies a value to use when a parsing error occurs.
    // In this case, "color" is not required and has a default value.
    let color = matches.value_of("color").unwrap();

    // Values are always strings, but clap provides
    // a macro to convert them to other types.
    let size = value_t_or_exit!(matches, "size", u8);

    // The `is_present` method checks whether a flag is present.
    if matches.is_present("quiet") {
        println!("{} {}", size, color);
    } else {
        println!("You ordered size {} in {}.", size, color);
    }
}
```

## <a name="modules">Modules</a>

A module defines a collection of values like constants, functions, and structs.
A module can be defined in many places:

1. in the Rust standard library
1. in a dependency declared in the `Cargo.toml` file
1. in a file whose name is the module name
1. in multiple files within a directory whose name is the module name
1. inside a source file that uses it (with `mod { ... }`)

By default, all members of a module are private.
To make a member accessible outside the module,
add the `pub` keyword at the beginning of its definition.

When a module is defined inside a source file,
it is typically only used by code in that file.
This is useful for teaching Rust concepts,
but is not often used in practice.
For example:

```rust
mod points {
    pub struct Point2D {
        pub x: f64,
        pub y: f64, // comma after last field is optional
    }

    impl Point2D {
        // Associated function
        pub fn distance_between(pt1: &Self, pt2: &Self) -> f64 {
            let dx = pt1.x - pt2.x;
            let dy = pt1.y - pt2.y;
            (dx.powi(2) + dy.powi(2)).sqrt()
        }

        // Method
        pub fn distance_to(self: &Self, other: &Self) -> f64 {
            Self::distance_between(self, other)
        }
    }
}

fn main() {
    use points::Point2D;
    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };
    let d1 = p1.distance_to(&p2);
    println!("{}", d1); // 5
    let d2 = Point2D::distance_between(&p1, &p2);
    println!("{}", d2); // 5
}
```

Moving the module definition into a separate file
de-clutters the source file that uses it
and enables using the module in many source files.

Here is the same code, split into two files.
First up is the file `src/points.rs`:

```rust
pub struct Point2D {
    pub x: f64,
    pub y: f64, // comma after last field is optional
}

impl Point2D {
    // Associated function
    pub fn distance_between(pt1: &Self, pt2: &Self) -> f64 {
        let dx = pt1.x - pt2.x;
        let dy = pt1.y - pt2.y;
        (dx.powi(2) + dy.powi(2)).sqrt()
    }
    // Method
    pub fn distance_to(self: &Self, other: &Self) -> f64 {
        Self::distance_between(self, other)
    }
}
```

In addition to its use in defining a module inside a source file,
the `mod` keyword is used to gain access to modules defined outside.
The statement `mod name;` is equivalent to `mod name { include!("main.rs"); }`
where `main.rs` is the file that contains the module definition
(determined by the compiler).

Values in the `std::prelude` module are automatically made available.
A list of these values can be found {% aTargetBlank
"https://doc.rust-lang.org/std/prelude/", "here" %}
and include `Box`, `Option`, `Result`, `String`, and `Vec`.

Other libraries can also define a `prelude` module,
but the values it defines are not automatically imported.
Users of the library can include all the items defined in a `prelude` module
using `use library_name::prelude::\*;`.
This is most useful for bringing traits into scope.
(See this comment in the {% aTargetBlank
"https://users.rust-lang.org/t/defining-a-prelude-module-for-a-library-crate/54018/2",
"Rust Forum" %}).

The `use` statement binds a module path to a new name for easier access.
For example, `use A::B::C` enables using `C` with just that name
instead of its fully qualified name.
This is also referred to as bringing `C` into scope.
In this example, `C` can be any kind of item
including a module, enum, struct, or function.

When bringing functions into scope, it is idiomatic to just
specify the path to their parent module in a `use` statement
and use that to refer to the function (`ParentModule::fnName`).
This makes it apparent when looking a calls to these functions
that they are not defined locally.

When bringing other items like structs and enums into scope
it is idiomatic to specify their full paths in a `use` statement
and then refer to them using only their name.
However, this approach doesn't work if multiple items are needed
from different modules and they have the same name.
In that case either bring their parent modules into scope
and use those to disambiguate references or
use the `as` keyword in the `use` statement to assign aliases to the names.
For example, `use math::dimension3::Point as Point3D`.

The file `src/main.rs` below uses the `points` module defined above.

```rust
mod points;
use points::Point2D;

fn main() {
    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };
    let d1 = p1.distance_to(&p2);
    println!("{}", d1); // 5
    let d2 = Point2D::distance_between(&p1, &p2);
    println!("{}", d2); // 5
}
```

This approach works well for small modules.
For large modules it is sometimes desirable to
split their definition across multiple source files.
Each `.rs` file defines a module and
placing them in subdirectories creates sub-modules.
A `.rs` file can use the `mod` and `use` statements
to gain access to items in multiple other modules
and re-export those items as their own.

The old way of doing this was to
create a directory with the name of the module,
place the files that define the module functionality inside it,
and create the file `mod.rs` inside the directory
that imports items to be exposed from those files and re-exports them.

The new way is similar, but
a `.rs` file with the name of the module is created instead of `mod.rs`
and this is placed in the same directory as the module directory.

Here is the previous code using this approach.
We'll add a function to the module that is
defined in a different source file than
the one that defines the `Point2D` struct
just to demonstrate using multiple files.

The file `src/points/types.rs` can be
identical to the file `src/points.rs` above,
defining the `Point2D` struct and it's associated function and method.

The file `src/points/functions.rs` defines the function `distance_from_origin`
which returns the distance from a `Point2D` object to the origin.
Note that this is a plain function, not a method or associated function.

```rust
// The super keyword enables finding a module
// (types in this case) in the same directory.
use super::types::Point2D;

pub fn distance_from_origin(pt: &Point2D) -> f64 {
    (pt.x.powi(2) + pt.y.powi(2)).sqrt()
}
```

The file `src/points.rs` ties it all together.

```rust
mod functions;
mod types;
pub use functions::*;
pub use types::*;
```

The file `src/main.rs` below demonstrates
using all the features of the `points` module.

```rust
mod points;
// Note how a "use" statement can access multiple values
// from a module using curly braces.
use points::{distance_to_origin, Point2D};

fn main() {
    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };
    let d1 = p1.distance_to(&p2);
    println!("{}", d1); // 5
    let d2 = Point2D::distance_between(&p1, &p2);
    println!("{}", d2); // 5
    let d3 = distance_to_origin(&p1);
    println!("{}", d3); // 5
}
```

Modules can be nested to further segregate the defined items.

## <a name="crates">Crates</a>

A crate is a tree of modules.
There are two kinds of crates.
Binary (bin) crates produce an executable.
Library (lib) crates provide code used by other crates.
TODO: Can a crate be both types?

The most popular source of open source crates is
{% aTargetBlank "https://crates.io/", "crates.io" %}.
Commonly used crates found here include:

- {% aTargetBlank "https://crates.io/crates/actix-web", "actix-web" %} - web framework
- {% aTargetBlank "https://crates.io/crates/chrono", "chrono" %} - date and time library
- {% aTargetBlank "https://crates.io/crates/clap", "clap" %} - command-line argument parser
- {% aTargetBlank "https://crates.io/crates/futures", "futures" %} -
  "An implementation of futures and streams featuring
  zero allocations, composability, and iterator-like interfaces.
- {% aTargetBlank "https://crates.io/crates/hyper", "hyper" %} - HTTP client library
- {% aTargetBlank "https://crates.io/crates/log", "log" %} - logging API
- {% aTargetBlank "https://crates.io/crates/nom", "log" %} - parser combinators library
- {% aTargetBlank "https://crates.io/crates/num-traits", "num-traits" %} - generic mathematics
- {% aTargetBlank "https://crates.io/crates/rand", "rand" %} - random number generation
- {% aTargetBlank "https://crates.io/crates/rayon", "rayon" %} - "data-parallelism library"
- {% aTargetBlank "https://crates.io/crates/reqwest", "reqwest" %} - HTTP client
- {% aTargetBlank "https://crates.io/crates/rocket", "rocket" %} - web framework
- {% aTargetBlank "https://crates.io/crates/serde", "serde" %} - data structure serialization and deserialization, including JSON
- {% aTargetBlank "https://crates.io/crates/tokio", "tokio" %} - "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed applications."

## Creating and Using a Library

To create a library project, enter `cargo new {name} --lib`

To demonstrate, let's create a library for operating on 2D points.

1. Create the library directory structure by entering
   `cargo new geometry2d --lib`

1. Enter `cd geometry2d`

1. Create `src/geometry2d.rs` containing the following:

   ```rust
   pub struct Point2D {
       pub x: f64,
       pub y: f64,
   }

   impl Point2D {
       // Associated function
       pub fn distance_between(pt1: &Self, pt2: &Self) -> f64 {
           let dx = pt1.x - pt2.x;
           let dy = pt1.y - pt2.y;
           (dx.powi(2) + dy.powi(2)).sqrt()
       }

       // Method
       pub fn distance_to(self: &Self, other: &Self) -> f64 {
           Self::distance_between(self, other)
       }
   }
   ```

1. Modify `src/lib.rs` to contain the following:

   ```rust
   mod geometry2d;

   // Expose the Point2D struct as a root item.
   pub use crate::geometry2d::Point2D;

   #[cfg(test)]
   mod tests {
       use crate::geometry2d::Point2D;

       #[test]
       // To temporarily skip a test, add: #[ignore]
       fn distance_between() {
           let pt1 = Point2D { x: 3.0, y: 4.0 };
           let pt2 = Point2D { x: 6.0, y: 8.0 };
           assert_eq!(Point2D::distance_between(&pt1, &pt2), 5.0);
       }

       #[test]
       fn distance_to() {
           let pt1 = Point2D { x: 3.0, y: 4.0 };
           let pt2 = Point2D { x: 6.0, y: 8.0 };
           assert_eq!(pt1.distance_to(&pt2), 5.0);
       }
   }
   ```

1. Run the tests by entering `cargo test`

1. Build the library by entering `cargo build`

1. Go back up a directory by entering `cd ..`

1. Create the directory structure for an application that will use the library
   by entering `cargo new geometry2d-app`

1. Enter `cd geometry2d-app`

1. Modify `Cargo.toml` to contain the following dependency:

   ```toml
   geometry2d = { path = "../geometry2d" }
   ```

1. Modify `src/main.rs` to contain the following:

   ```rust
   extern crate geometry2d;
   use geometry2d::Point2D;

   fn main() {
       let pt1 = Point2D { x: 3.0, y: 4.0 };
       let pt2 = Point2D { x: 6.0, y: 8.0 };
       println!("{}", Point2D::distance_between(&pt1, &pt2)); // 5
       println!("{}", pt1.distance_to(&pt2)); // 5
   }
   ```

1. Run the app by entering `cargo run`

## Cross-compiling

Rust can build executables for platforms other than the host platform.
For example, follow these steps to build a Windows executable
when not running on a Windows machine:

1. One time, install mingw-w64.
   In macOS this can be done by installing Homebrew
   and entering `brew install mingw-w64`
1. One time, enter `rustup target add x86_64-pc-windows-gnu`
1. One time, enter `rustup toolchain install stable-x86_64-pc-windows-gnu`
1. `cd` to the application root directory.
1. Enter `cargo build --target x86_64-pc-windows-gnu --release`

This creates an executable file in the
`target/x86_64-pc-windows-gnu/release` directory
with the same name as the project and a `.exe` file extension.

## <a name="smart-pointers">Smart Pointers</a>

TODO: Resume review here.
Smart pointers are an alternative to references.
Each is implemented by a struct that holds metadata
and has methods that implement its features.
Many are defined in the standard library
and developers can implement new ones.

| Name         | Description                                                                                                                                      |
| ------------ | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `Arc<T>`     | stands for "atomically reference counted"; enables multiple owners across multiple threads                                                       |
| `Box<T>`     | a pointer stored on the stack to data on the heap                                                                                                |
| `Cell<T>`    | enables having multiple mutable references to a value within a single thread (1)                                                                 |
| `Cow<T>`     | stands for "Clone On Write"; wraps an immutable borrow and<br>provides the `to_mut` method for lazy cloning when mutation or ownership is needed |
| `Rc<T>`      | stands for "reference counted"; enables multiple owners                                                                                          |
| `Ref<T>`     | used with a `RefCell` to enforce immutable borrowing rules at runtime                                                                            |
| `RefCell<T>` | similar to `Cell`, but holds references to values instead of values and supports mutable borrows                                                 |
| `RefMut<T>`  | used with a `RefCell` to enforce mutable borrowing rules at runtime                                                                              |
| `String`     | owns `str` data, holds `capacity` and `length` metadata, and provides methods to operate on the data                                             |
| `Vec<T>`     | similar to `String`, but the data elements can be any specified type                                                                             |

1. This supports "interior mutability" which is described below.

Smart pointers must implement the `std::ops::Deref` and `Drop` traits.
The `Deref` trait requires a single method, `deref`,
that is used by the dereference operator `*`.
The `Drop` trait requires a single method, `drop`,
that is called automatically when an object
from an implementing struct goes out of scope.
It can define what should happen in addition to freeing memory,
including freeing resources like file handles and network connections.

Rust provides "deref coercion" which automatically converts a reference
to any value that implements the `Deref` trait to
a reference to the value returned by the `deref` method.
This enables passing references or smart pointers
to functions that take a reference.
No additional uses of the `&` or `*` operators are required.
This is why references to `String` values
can be passed to functions that take a `&str`.

It is possible to drop (free) a value that implements the `Drop` trait
before it goes out of scope.
This is done by passing the value to the prelude function `std::mem::drop`
rather than calling the `drop` method.

A `Box` smart pointer lives on the stack
and holds a pointer to data on the heap.
It is useful in three scenarios:

1. A value that implements a given trait should be owned,
   but the value can be any type that implements the trait
   (ex. `Box<dyn Error>`).
1. Ownership of a large value should be transferred without copying the data.
1. The size of some data cannot be known at compile time,
   but it must be used in a context that requires a fixed size
   (ex. a recursive type such as linked-list or tree)

An `Rc` smart pointer holds a reference count that starts at one,
is incremented each time it is cloned (by calling `Rc::clone(&my_rc)`),
and is decremented when the original or a clone goes out of scope.
The value they refer to is not dropped until the reference count goes to zero.
This smart pointer is useful when it is not possible to know at compile-time
which scope that uses the data will be the last to do so.

"Interior mutability" is the ability to modify data inside an immutable object.
Normally Rust does not allow this, but the `RefCell` smart pointer enables it.
Often `RefCell` is used in combination with `Rc`.

The `Rc` smart pointer can hold both
strong (owning) and weak (not owning) references.
Instances are dropped when the number of strong references goes to zero,
but not when the number of weak references goes to zero.
This distinction is useful in situations like representing a tree
where parent nodes have strong references to children,
but children have a weak reference to their parent.
The result is that dropping a parent also drops its children
(unless there are other strong references to them),
but dropping a child does not drop its parent.

Smart pointers are needed to implement data structures such as
linked lists, trees, and graphs.
The standard library provides `std::collections::LinkedList`,
but it does not provide structs that implement trees and graphs.
For example:

```rust
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;
use std::vec::Vec;

// This is a common combination of types
// for holding a reference to a value that can be mutated.
// Rc cannot mutate what it holds, but
// RefCell provides "interior mutability" which
// allows a mutable borrow while immutable borrows exist.
// Normally this is not allowed by the compiler, but using
// RefCell moves the checking of correct usage to runtime.
type Wrapper<T> = Rc<RefCell<Node<T>>>;

#[derive(Debug)]
struct Node<T> {
    data: T,
    children: Vec<Wrapper<T>>
}

impl<T: Display> Node<T> {
    fn add_child(&mut self, child: Wrapper<T>) {
        self.children.push(child);
    }

    fn new(data: T) -> Node<T> {
        Node { data, children: vec![] }
    }

    fn depth_first(&self) {
        println!("{}", self.data);
        for child in &self.children {
            // These two lines can be replaced by the one that follows.
            //let child_node = child.borrow();
            //child_node.depth_first();
            child.borrow().depth_first();
        }
    }

    fn wrap(data: T) -> Wrapper<T> {
        Rc::new(RefCell::new(Node::new(data)))
    }
}

fn main() {
    let a = Node::wrap('A');
    let b = Node::wrap('B');
    let c = Node::wrap('C');
    let d = Node::wrap('D');

    a.borrow_mut().add_child(Rc::clone(&b));
    a.borrow_mut().add_child(Rc::clone(&c));
    b.borrow_mut().add_child(Rc::clone(&d));
    a.borrow().depth_first();
}
```

The `Arc` smart pointer is similar to the `Rc` smart pointer,
but can be used by multiple threads to safely share data between them.
Often this is used in conjunction with
`std::sync::Mutex` or `std::sync::RwLock`.

"Mutex" is short for "mutual exclusion".
Instances guarantee that only one thread at a time
will have access to the data it holds.
A thread gains access by calling the `lock` method.
This blocks until the lock can be acquired.
The lock is automatically released when
the variable that holds it goes out of scope.

`RwLock` instances a similar, but distinguish between uses that
need the ability to modify the data and those that merely read the data.
Only one thread at a time can acquire a write lock,
but any number of threads can acquire a read lock
as long as no thread has a write lock.
The `read` and `write` methods are called to acquire a lock.
These block until it can be acquired.
Like with a `Mutex`, the lock is automatically released when
the variable that holds it goes out of scope.

TODO: Also look at use async_std::sync::{Arc, RwLock};
TODO: How does async_std::sync::RwLock differ from std::sync::RwLock?
TODO: Is it better to put RwLock outside Arc instead of Arc outside RwLock?
TODO: Also discuss the versions of these in the parking_lot crate.:

I said:
It would be great if there was a resource that provided guidance for choosing between the std::sync , async_std::sync , tokio::sync , and parking_lot versions of all of these locking mechanisms.

Alice replied:
Use the guide on Tokio's docs to choose between blocking or async lock, then use either Tokio or async-std's lock if you need an async lock, choosing the same as the one you use as runtime, or std/parking_lot if you need a blocking lock.

The shared state chapter in Tokio's tutorial has more details.
See <https://tokio.rs/tokio/tutorial/shared-state>.

## Threads

Rust has built-in support for threads.
It uses native threads, not "green threads".
But support for green threads (or "tasks") is available
in popular crates including async-std and tokio.

When writing to code to execute multiple blocks of code or functions
there are four options to consider.

1. serially: one at a time
1. concurrently: taking turns using a single thread
1. parallel using operating system threads
1. parallel using tasks (a.k.a. green threads)

These options are demonstrated using the `std`, `async_std`, and `tokio` crates.
Note that options 2 and 4 are not possible using only the `std` crate.

| Option           | `std` | `async_std` | `tokio` |
| ---------------- | ----- | ----------- | ------- |
| serial           | ✓     | ✓           | ✓       |
| concurrent       | x     | ✓           | ✓       |
| parallel threads | ✓     | ✓           | ✓       |
| parallel tasks   | x     | ✓           | ✓       |

Here is a simple example that spawns a number of threads
that each sleep for a random duration,
send a value to the main thread using a channel,
and return a value.
The main thread receives and prints the messages sent over the channel.
It also waits for all the threads to finish by calling their `join` method.

The file `src/main.rs` exercises all four options using a single crate.
To use a different crate, uncomment the related code in this file
and comment the previously uncommented part.
This code can be found in the GitHub repo at
{% aTargetBlank "https://github.com/mvolkmann/rust-parallel-options",
"rust-parallel-options" %}.

```rust
use std::error::Error;

mod std_demo;
use std_demo::{concurrent, parallel_tasks, parallel_threads, serial};
fn main() -> Result<(), Box<dyn Error>> {
    let (sum1, sum2) = serial()?;
    println!("serial: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    let (sum1, sum2) = concurrent()?;
    println!("concurrent: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    let (sum1, sum2) = parallel_threads()?;
    println!("threads: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    let (sum1, sum2) = parallel_tasks()?;
    println!("tasks: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    Ok(())
}

mod async_std_demo;
use async_std_demo::{concurrent, parallel_threads, parallel_tasks, serial};
#[async_std::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let (sum1, sum2) = serial()?;
    println!("serial: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    //let (sum1, sum2) = concurrent()?;
    let (sum1, sum2) = concurrent().await?;
    println!("concurrent: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    //let (sum1, sum2) = parallel_threads()?;
    let (sum1, sum2) = parallel_threads().await?;
    println!("threads: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    //let (sum1, sum2) = parallel_tasks()?;
    let (sum1, sum2) = parallel_tasks().await?;
    println!("tasks: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    Ok(())
}

mod tokio_demo;
use tokio_demo::{concurrent, parallel_tasks, parallel_threads, serial};
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let (sum1, sum2) = serial()?;
    println!("serial: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    let (sum1, sum2) = concurrent().await?;
    println!("concurrent: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    let (sum1, sum2) = parallel_threads().await?;
    println!("threads: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    let (sum1, sum2) = parallel_tasks().await?;
    println!("tasks: sum1 = {:?}, sum2 = {:?}", sum1, sum2);

    Ok(())
}
```

TODO: See other mpsc implementations that are in futures::sync::mpsc
TODO: :and tokio::sync::mpsc, but not in async-std!

In addition, "channels" can be used to enable
threads to communicate during their execution
rather than waiting to get a result when they complete.

```rust
use rand::Rng; // stands for "random number generator".
use std::sync::mpsc; // stands for "multiple producer, single consumer"
use std::thread;
use std::time::Duration;

fn main() {
    let (tx, rx) = mpsc::channel();

    const N: i32 = 3;

    // Start N threads and collect their handles.
    let mut handles = Vec::new();
    let mut rng = rand::thread_rng();
    for i in 1..=N {
        let ms = rng.gen_range(500..5000);
        // The "move" keyword below is necessary for
        // the closure to take ownership of i and ms.
        let tx = mpsc::Sender::clone(&tx);
        handles.push(thread::spawn(
            move || -> Result<(i32, u64), mpsc::SendError<_>> {
                println!("thread {} started", i);
                // Generate a random number of milliseconds to sleep.
                thread::sleep(Duration::from_millis(ms));
                let msg = format!("tx from thread {}!", i);
                tx.send(msg)?;
                println!("thread {} finished", i);
                Ok((i, ms)) // tuple of thread number and milliseconds slept
            },
        ));
    }
    // Need to drop the original sender (tx) so the receiver (rx) so
    // the loop below can exit after the last tx clone goes out of scope.
    drop(tx);

    // Listen for channel messages in a different thread.
    // This is a good way to enable processing the results from each thread
    // in the order in which they complete
    // rather than the order in which they were started.
    for msg in rx {
        println!("received {}", msg);
    }

    // Wait for all the threads to finish.
    // Results will be processed in the order the threads were created,
    // not in the order in which the threads complete.
    for handle in handles {
        match handle.join() {
            Ok(result) => println!("result = {:?}", result.unwrap()),
            Err(e) => eprintln!("error: {:?}", e),
        }
    }

    println!("done");
}
```

Here is an example that scrapes web sites listed in a file.
It reports the number of `img` tags found at each site.
First it does this with a single thread and
then spawning a separate tasks for each site
to enable using multiple threads.
The elapsed time for each approach is output
to show the speed benefit of using multiple threads.

The code can be found at {% aTargetBlank
"https://github.com/mvolkmann/rust-web-scrape", "rust-web-scrape" %}.
With 19 web sites listed in `web-sites.txt` and
running on a 2019 MacBook Pro laptop with 32GB of memory,
the single-threaded approach took 16.18 seconds
and the multi-threaded approach took 707 milliseconds.

Note that spawning more threads than there are processor cores
is not an issue for a reasonable number of threads, 19 in this case.
But spawning thousands of concurrent threads
will consume a large amount of memory.
In that case spawning "tasks",
such as those supported by async-std and tokio is preferred.

Here are the dependencies added in `Cargo.toml`:

```toml
futures = "0.3.12"
reqwest = "0.11.0"
tokio = { version = "1.2.0", features = ["full", "rt"] }
```

Here is the `src/main.js` file:

```rust
extern crate reqwest;
// We cannot use async-std in place of tokio because reqwest depends on tokio.
extern crate tokio;

use reqwest::header::USER_AGENT;
use std::boxed::Box;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};
use std::time::Instant;
use tokio::task;
use tokio::task::JoinHandle;

// We need to set the user agent because some sites return 403 Forbidden
// for requests that do not seem to be coming from a web browser.
const UA: &str = "Mozilla/5.0"; // This is enough.

type FileLines = Lines<BufReader<File>>;

type GenericError = Box<dyn std::error::Error + Send + Sync>;
type Result<T> = std::result::Result<T, GenericError>;

async fn get_sites() -> Result<FileLines> {
    let path = "./web-sites.txt";
    let f = File::open(path)?;
    let reader = BufReader::new(f);
    Ok(reader.lines())
}

async fn process_site(url: &str) -> Result<()> {
    // If commented ...
    if url.starts_with("#") {
        return Ok(());
    }

    let client = reqwest::Client::new();
    let res = client.get(url).header(USER_AGENT, UA).send().await?;
    let html = res.text().await?;
    //dbg!(&html);
    let images: Vec<&str> = html.matches("<img ").collect();
    println!("{} has {} img tags", url, images.len());
    Ok(())
}

#[tokio::main] // starts the Tokio runtime
async fn main() -> Result<()> {
    // Single threaded ...
    let sites = get_sites().await?;
    let start = Instant::now();
    for site in sites {
        if let Ok(url) = site {
            process_site(&url).await?;
        }
    }
    println!("single-threaded time: {:?}\n", start.elapsed());

    // Multi-threaded ...
    let sites = get_sites().await?;
    let start = Instant::now();
    let mut handles: Vec<JoinHandle<MyResult<()>>> = Vec::new();
    for site in sites {
        //handles.push(thread::spawn(|| async move {
        handles.push(task::spawn(async {
            if let Ok(url) = site {
                process_site(&url).await?;
            }
            Ok(())
        }));
    }
    for handle in handles {
        if let Err(e) = handle.await? {
            eprintln!("error: {}", e);
        }
    }
    println!("multi-threaded time: {:?}", start.elapsed());

    Ok(())
}
```

{% aTargetBlank "https://github.com/rayon-rs/rayon", "rayon" %}
is a Rust "data parallelism library".
It supports the parallel iterator methods `par_iter` and `par_iter_mut`.
It also supports parallel sorting of slices and vectors.j
The following example shows how rayon can be used to
process of all the items in a vector using multiple threads.
It uses the `rand` crate to generate random numbers.

```rust
use rand::Rng;
use rayon::prelude::*;
use std::time::Instant;

fn main() {
    // Generate a vector of random numbers.
    const N: i32 = 1_000_000_000;
    let mut numbers = Vec::new();
    // rng stands for "random number generator".
    let mut rng = rand::thread_rng();
    for _i in 0..N {
        let n = rng.gen(); // number from 0 inclusive to 1 exclusive
        numbers.push(n);
    }

    // The times given here are from a 2019 MacBook Pro laptop
    // with 32GB of memory and 8 cores.
    // Multiplying all the numbers by two took 2.617 seconds using one thread.
    let start = Instant::now();
    numbers.iter_mut().for_each(|p: &mut f64| *p *= 2.0);
    println!("elapsed time: {:?}", start.elapsed());

    // Using multiple threads took only 554ms.
    let start = Instant::now();
    numbers.par_iter_mut().for_each(|p: &mut f64| *p *= 2.0);
    println!("elapsed time: {:?}", start.elapsed());
}
```

It is often necessary to coordinate access to
data that is shared between threads.
Common ways to do this include using a `Mutex` or `RwLock`.
There are multiple implementations available for each of these.
Consider using those in `std::sync` and the `parking_lot` crate.

The `parking_lot` versions of `Mutex` and `RwLock`
are generally preferred over the `std::sync` versions for reasons described at
{% aTargetBlank "https://docs.rs/parking_lot/0.11.1/parking_lot/type.Mutex.html", "parking_lot::Mutex" %}
and
{% aTargetBlank "https://docs.rs/parking_lot/0.11.1/parking_lot/type.RwLock.html", "parking_lot::RwLock" %}.:w
The reasons include better poison handling, less memory usage,
better fairness of lock sharing, and better performance.

To gain exclusive access to a value wrapped in a `Mutex`,
call its `lock` method. The lock is automatically released
when the value returned by this method goes out of scope.

To gain non-exclusive read access to a value wrapped in a `RwLock`,
call its `read` method. There can be any number of concurrent readers.
To gain exclusive write access to a value wrapped in a `RwLock`,
call its `write` method.
This will block until there are no other readers or writers.
Like with a `Mutex`, the lock is automatically released
when the value returned by the `read` or `write` method goes out of scope.

A mutex is "poisoned" when a thread that holds the lock panics.
This causes attempts to acquire the lock in other threads to fail,
The `lock` method returns a `LockResult`
which enables callers to detect this situation.

A `RwLock` is poisoned when a thread that
holds a write lock (not a read lock) panics.
This causes attempts to acquire the lock in other threads to fail,
The `read` and `write` methods returns a `LockResult`
which enables callers to detect this situation.

## Futures and async/await

`Future` is a trait that enables defining code to be executed in the future.
While it can be used directly, typically the keywords `async` and `await`
are used to provide syntactic sugar that simplifies the code.

The keyword `async` is added to the beginning of function definitions or blocks.
It changes the function to be non-blocking and
return an instance that implements the `Future` trait.

All `async` functions return a `Future` even though
they do not explicitly specify that in their return type.

It is not possible to manually write normal Rust code
that is the equivalent of the code
produced by the `async` and `await` keywords.
The Rust compiler performs some magic to support these.

From user "kornel" in the Rust Forum February 3, 2020,

> `async` can create `Future` objects that are self-referential structs,
> and this is the only place in safe Rust where this is allowed.
> It ends up being a state machine that uses unsafe pointers.
> It's not a straightforward translation,
> and there's no equivalent high-level syntax.
> That unique complexity is the reason why
> `.await` is a built-in syntax.
> Otherwise it would be a crate with a proc_macro.

For example, add the following dependencies in `Cargo.toml`:

```toml
async-std = {version = "1.9.0", features = ["attributes"]}
futures = "0.3.12"
```

Using `async` requires a futures executor like those
provided by the external crates async-std and tokio.
While you could write your own executor or write low-level routines
that return futures and wake them after some hardware event,
in practice typically async-std or tokio is used.

Add the following in `src/main.rs`:

```rust
use async_std::fs::{File};
use async_std::io::{BufReader, Result};
use async_std::prelude::*;

// The return type, in this case a Result, is wrapped in a Future.
async fn sum_file(file_path: &str) -> Result<f64> {
    let f = File::open(file_path).await?;
    let reader = BufReader::new(f);
    let mut sum = 0.0;
    //for line in reader.lines() { // can use this with std::io::BufReader
    let mut stream = reader.lines();
    while let Some(Ok(line)) = stream.next().await {
        if let Ok(n) = line.parse::<f64>() {
            println!("n = {}", n);
            sum += n;
        }
    }
    Ok(sum)
}

#[async_std::main]
async fn main() {
    match sum_file("./numbers.txt").await {
        Ok(sum) => println!("sum = {}", sum),
        Err(e) => eprintln!("error = {}", e)
    }
}
```

`Future`s are lazy meaning they are not executed
until the `await` keyword is applied to them.
The `await` keyword triggers execution of the future,
waits for it to complete, and yields control back to the thread
so it has the opportunity to do other work while waiting.
The `.await` syntax is often placed at the end of a function call
with a dot (period) before it.
This makes it appear that `await` is a property, but it is actually a keyword.
For example: `some_future.await;`.

The `await` keyword can only be used in `async` functions or `async` blocks.
An async block has the syntax `async { ... }`.
The `move` keyword can be added to move ownership of variables
defined outside the block that are used in the block into it.
For example, `async move { ... }`.

An "executor" is required in order to evaluate `Future`s.
The most common way to add an executor is to include the attribute
`#[async_std::main]` or `#[tokio::main]` before `async fn main`.
These are macros that transform the `async fn main()`
into a synchronous `fn main()` that initializes a runtime instance
and executes the code in the function.

From the tokio tutorial, the following:

```rust
#[tokio::main]
async fn main() {
    println!("hello");
}
```

is transformed into:

```rust
fn main() {
    let mut rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        println!("hello");
    })
}
```

Crates like async-std and tokio support "tasks"
which are like lightweight threads that are scheduled
to run in read threads.
A task can be thought of as a collection of futures.

## async-std

The `async-std` crate is a port of the `std` crate that provides provides
asynchronous alternatives to some its functionality .
Programs that wish to use it can replace references
to the `std` namespace with `async_std`.
It utilizes the `async` and `await` keywords.
Form the official documentation, "blocking functions have been replaced with
async functions and threads have been replaced with lightweight tasks."

Prefer using the tokio runtime instead of async-std
because it is very mature and supports more features.
These cannot be mixed, so you must choose one.

To use this runtime, add `async-std` as a dependency in `Cargo.toml`

```toml
async-std = { version = "1.9.0", features = ["attributes"] }
```

Then add the following attribute before the `main` function:

```rust
#[async_std::main]
```

## Tokio

{% aTargetBlank "https://crates.io/crates/tokio", "tokio" %} is a popular crate
that makes implementing asynchronous code even easier.
It was initially created by Carl Lerche and is supported by
many contributors including Alex Crichton, Ivan Petkov, Alice Rhyl,
Taiki Endo, Eliza Weisman, Lucio Franco, Sean McArthur, and more.

Features supported by Tokio include:

- TCP, UDP, Unix sockets, and pipes
- timers
- spawning threads
- communication between threads using channels
- mutex and semaphore
- subprocesses
- signal handlers
- streams
- sync utilities
- work-stealing task scheduling
- and more

From the Tokio tutorial:

> Many of Tokio's types are named the same as their
> synchronous equivalent in the Rust standard library.
> When it makes sense, Tokio exposes the same APIs
> as `std` but using `async fn`.

Here is a very basic example of using `tokio`.
It assumes the following dependency line
has been added in the `Cargo.toml` file.

```toml
tokio = { version = "1.1.1", features = ["full"] }
```

Tokio supports a large number of features.
The resulting binary size for a program can be reduced
by including only the features being used instead of "full".

```rust
use std::time::Duration;
use tokio::time::{sleep, Sleep};

fn sleep_ms(ms: u64) -> Sleep {
    sleep(Duration::from_millis(ms))
}

#[tokio::main]
async fn main() {
    println!("Hello");
    //sleep(Duration::from_millis(1000)).await;
    sleep_ms(1000).await;
    println!("World");
}
```

The following example spawns 10 tasks that each sleep for a random amount of time and then return some data. The results are processed in the order in which the tasks complete.

TODO: Add code from https://github.com/mvolkmann/rust-tokio.

TODO: Add information from <https://github.com/mvolkmann/rust-parallel-options>.

## Standard Library

The {% aTargetBlank "https://doc.rust-lang.org/std/index.html",
"Rust Standard Library" %} is relatively small.
Often commonly needed functionality is instead found
in the collection of crates at
{% aTargetBlank "https://crates.io/", "crates.io" %}.

## Databases

Rust can access many kinds of databases including
PostgreSQL, MySQL, SQLite, and MongoDB.
This is supported by several crates including:

- {% aTargetBlank "https://crates.io/crates/diesel", "diesel" %}
- {% aTargetBlank "https://crates.io/crates/postgres", "postgres" %}
- {% aTargetBlank "https://crates.io/crates/mongodb", "mongodb" %}
- {% aTargetBlank "https://crates.io/crates/mysql", "mysql" %}
- {% aTargetBlank "https://crates.io/crates/rusqlite", "rusqlite" %}

Here is an example of using the `postgres` crate:

1. Start the PostgreSQL server by entering
   `pg_ctl -D /usr/local/pgsql/data -l logfile start`
1. Create the "animals" database by entering `createdb animals`.
1. Create the file `db.ddl` containing the following:

   ```sql
   create table dogs (
     id serial primary key,
     breed text not null,
     name text not null
   )
   ```

1. Create the "dogs" table by entering `psql -d animals -f db.ddl`

1. Create a new Rust project by entering `cargo new postgres-demo`

1. Enter `cd postgres-demo`

1. Edit `Cargo.toml` and add the dependency `postgres = "0.19.0"`.

1. Edit `src/main.rs` to contain the following:

   ```rust
   use postgres::{Client, Error, NoTls, Row};

   fn insert_dog(client: &mut Client, name: &str, breed: &str) -> Result<Vec<Row>, Error> {
       client.query(
           "insert into dogs (name, breed) VALUES ($1, $2) returning id",
           &[&name, &breed],
       )
   }

   fn insert_dogs(client: &mut Client) -> Result<u64, Error> {
       let dogs = [
           ("Maisey", "Treeing Walker Coonhound"),
           ("Ramsay", "Native American Indian Dog"),
           ("Comet", "Whippet"),
       ];
       for dog in &dogs {
           insert_dog(client, dog.0, dog.1)?;
       }
       Ok(dogs.len() as u64) // # of inserted rows
   }

   fn delete_dogs(client: &mut Client) -> Result<u64, Error> {
       client.execute("delete from dogs", &[])
   }

   fn update_dog(client: &mut Client, id: i32, name: &str) -> Result<u64, Error> {
       client.execute("update dogs set name = $2 where id = $1", &[&id, &name])
   }

   fn report_dogs(client: &mut Client) {
       if let Ok(rows) = client.query("select id, name, breed from dogs", &[]) {
           for row in rows {
               let id: i32 = row.get(0);
               let name: &str = row.get(1);
               let breed: &str = row.get(2);
               println!("id={} name={} breed={}", id, name, breed);
           }
       }
   }

   fn main() {
       let username = "mark";
       let password = "";
       let database = "animals";
       let conn_str = format!(
           "postgresql://{}:{}@localhost/{}",
           username, password, database
       );
       let mut client = Client::connect(&conn_str, NoTls).unwrap();

       delete_dogs(&mut client).unwrap();
       insert_dogs(&mut client).unwrap();

       let rows = insert_dog(&mut client, "Oscar", "German Shorthaired Pointer").unwrap();
       let row = rows.first().unwrap();
       if let Some::<i32>(id) = row.get(0) {
           update_dog(&mut client, id, "Oscar Wilde").unwrap();
       }

       report_dogs(&mut client)
   }
   ```

1. Enter `cargo run`

Here is an example of using the `diesel` crate
to access the same PostgreSQL database:
This code can also be found at {% aTargetBlank
"https://github.com/mvolkmann/rust-diesel-demo", "rust-diesel-demo" %}.

1. Enter `cargo new diesel_demo`

1. Enter `cd diesel_demo`

1. Add these dependencies in `Cargo.toml`:

   ```toml
   diesel = { version = "1.4.5", features = ["postgres"] }
   dotenv = "0.15.0"
   ```

1. Enter `cargo install diesel_cli`

1. Create the file `.env` that sets the environment variable `DATABASE_URL`
   to the proper connection string with the syntax
   `postgresql://{username}:{password}@localhost/{database}`.
   For example:

   ```bash
   DATABASE_URL=postgresql://mark:@localhost/animals
   ```

1. Enter `diesel setup`

1. Enter `diesel migration generate dogs`

1. Edit `migrations/dogs/up.sql` to contain:

   ```sql
   create table dogs (
     id serial primary key,
     breed text not null,
     name text not null
   )
   ```

1. Edit `migrations/dogs/down.sql` to contain the following:

   ```sql
   drop table dogs
   ```

1. Enter `diesel migration run`.
   Later, to drop the `dogs` table and recreate it
   enter `diesel migration redo`.

1. Edit `src/models.rs` to contain the following:

   ```rust
   use crate::schema::dogs;

   // The order of the fields in these structs must match
   // the order of the columns in the corresponding table.
   //
   // Implementing the "Identifiable" trait means the struct
   // represents a single row in a database table and, by default,
   // has an "id" field that is its primary key.
   //
   // The associated table name defaults to the lowercase version
   // of the struct name with an "s" on the end.
   // If this is not the table name, specify the "table_name" attribute
   // as shown above the "NewDog" struct that follows.
   #[derive(AsChangeset, Identifiable, Queryable)]
   pub struct Dog {
       pub id: i32,
       pub breed: String,
       pub name: String,
   }

   #[table_name = "dogs"]
   #[derive(Insertable)]
   pub struct NewDog {
       pub breed: String,
       pub name: String,
   }
   ```

1. Edit `src/main.rs` to contain the following:

   ```rust
   #[macro_use]
   extern crate diesel;

   mod models;
   mod schema;

   use diesel::prelude::*;
   use diesel::result::Error;
   use models::*;

   // Deletes all rows from the dogs table.
   fn delete_dogs(conn: &PgConnection) -> Result<usize, Error> {
       diesel::delete(schema::dogs::table).execute(conn)
   }

   // Gets a connection to the "animals" database that contains a "dogs" table.
   fn get_connection() -> ConnectionResult<PgConnection> {
       use dotenv::dotenv;
       use std::env;
       dotenv().ok();
       let url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
       PgConnection::establish(&url)
   }

   // Inserts a row in the "dogs" table and returns its id.
   fn insert_dog(conn: &PgConnection, name: &str, breed: &str) -> Result<i32, Error> {
       let dog = NewDog {
           name: name.to_string(),
           breed: breed.to_string(),
       };
       let id: i32 = diesel::insert_into(schema::dogs::table)
           .values(&dog)
           .returning(schema::dogs::id)
           .get_result(conn)?;
       Ok(id)
   }

   // Outputs information about each row in the "dogs" table.
   fn report_dogs(conn: &PgConnection) {
       let results = schema::dogs::dsl::dogs
           .load::<Dog>(conn)
           .expect("error loading dogs");

       for dog in results {
           println!("{} is a {} (id {}).", dog.name, dog.breed, dog.id);
       }
   }

   // Updates the "dogs" table row with a given id.
   fn update_dog(conn: &PgConnection, id: i32, name: &str, breed: &str) -> Result<usize, Error> {
       let dog = Dog {
           id,
           name: name.to_string(),
           breed: breed.to_string(),
       };
       diesel::update(&dog).set(&dog).execute(conn)
   }

   // Updates the name of a dog with a given id in the "dogs" table.
   fn update_name(conn: &PgConnection, id: i32, name: &str) -> Result<usize, Error> {
       let dog = schema::dogs::dsl::dogs.filter(schema::dogs::id.eq(id));
       let result = diesel::update(dog)
           // can also pass a tuple of column changes
           .set(schema::dogs::name.eq(name))
           .execute(conn);
       if let Ok(count) = result {
           if count == 0 {
               return Err(Error::NotFound);
           }
       }
       result
   }

   // The return type specified here allows using "?" for error handling
   // regardless of the specific kinds of errors that occur.
   fn main() -> Result<(), Box<dyn std::error::Error>> {
       let conn = get_connection()?;

       delete_dogs(&conn)?;

       let dogs = [
           ("Maisey", "Treeing Walker Coonhound"),
           ("Ramsay", "Native American Indian Dog"),
           ("Comet", "Whippet"),
       ];
       for dog in &dogs {
           insert_dog(&conn, dog.0, dog.1)?;
       }

       let id = insert_dog(&conn, "Oscar", "German Shorthaired Pointer")?;

       //update_dog(&conn, id, "Oscar Wilde", "German Shorthaired Pointer")?;
       update_name(&conn, id, "Oscar Wilde")?;

       report_dogs(&conn);

       Ok(())
   }
   ```

Here is an example of using the `mongodb` crate:

1. Edit `Cargo.toml` to include these dependencies:

   ```toml
   futures = "0.3.12"
   mongodb = { version = "1.1.1", default-features = false, features = ["tokio-runtime"] }
   tokio = { version = "1.1.1", features = ["full"] }
   ```

1. Add the following in `src/main.rs`:

   ```rust
   use futures::stream::StreamExt; // needed to call "next" method on cursor
   use mongodb::bson::{doc};
   use mongodb::error::Result;
   use mongodb::Client;

   #[tokio::main]
   async fn main() -> Result<()> {
       let uri = "mongodb://127.0.0.1:27017";
       let client = Client::with_uri_str(uri).await?;

       println!("Databases:");
       for name in client.list_database_names(None, None).await? {
           println!("- {}", name);
       }

       let db = client.database("animals");
       let coll = db.collection("dogs");

       //coll.delete_many(None, None).await?; // does not work
       coll.drop(None).await?;

       let documents = vec![
           doc! {"name": "Maisey", "breed": "Treeing Walker Coonhound"},
           doc! {"name": "Ramsay", "breed": "Native American Indian Dog"},
           doc! {"name": "Comet", "breed": "Whippet"},
       ];
       coll.insert_many(documents, None).await?;

       // An array can be used instead as follows.
       // But the "insert_many" method wants owned documents
       // and this array only contains references to them.
       // One way to get an array of owned documents from an
       // array of references is to call ".iter().cloned()" on it as shown here.
       /*
       let documents: [Document; 3] = [
           doc! {"name": "Maisey", "breed": "Treeing Walker Coonhound"},
           doc! {"name": "Ramsay", "breed": "Native American Indian Dog"},
           doc! {"name": "Comet", "breed": "Whippet"},
       ];
       coll.insert_many(documents.iter().cloned(), None).await?;
       */

       let document = doc! { "name": "Oscar", "breed": "German Shorthaired Pointer" };
       let result = coll.insert_one(document, None).await?;

       coll.update_one(
           doc! {"_id": &result.inserted_id},
           doc! { "$set": { "name": "Oscar Wilde" }},
           None,
       )
       .await?;

       println!("\nDogs:");
       let mut cursor = coll.find(None, None).await?;
       while let Some(result) = cursor.next().await {
           if let Ok(doc) = result {
               dbg!(doc);
           }
       }

       Ok(())
   }
   ```

## Sending HTTP Requests

The {% aTargetBlank "https://crates.io/crates/reqwest", "reqwest" %}
crate is a popular option for sending HTTP requests.
Here is an example of using it along with
{% aTargetBlank "https://crates.io/crates/tokio", "tokio" %}
for asynchronous calls and
{% aTargetBlank "https://crates.io/crates/serde", "serde" %}
for data structure deserialization.

```rust
extern crate reqwest;
extern crate tokio;

use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
struct Todo {
    user_id: i32,
    id: i32,
    title: String,
    completed: bool,
}

// std::error::Error is a trait, not a type.
// Adding the `dyn` keyword before it means the error can be
// described by any type of value that implements that trait.
// The error value must have known size at compile time.
// Since any value of a type that implements the trait can be used,
// that size is not known.
// But the compiler does know the size of a `Box`
// which is what it is used to wrap the error value.
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    //let url = "https://jsonplaceholder.typicode.com/todos/3";
    let url = "https://jsonplaceholder.typicode.com/todos";

    let res = reqwest::get(url).await?;
    let json = res.text().await.unwrap();
    println!("json = {}", json);

    //let todo: Todo = serde_json::from_str(&json).unwrap();
    let todos: Vec<Todo> = serde_json::from_str(&json).unwrap();

    //println!("todo = {:?}", todo);
    println!("todos = {:#?}", todos);

    Ok(())
}
```

Here is an example that uses reqwest to benchmark REST service implementations.
This code can be found at {% aTargetBlank
"https://github.com/mvolkmann/rust-rest-benchmark", "rust-rest-benchmark" %}.

```rust
use serde::{Deserialize, Serialize};
use std::time::Instant;
use uuid::Uuid;

const BASE_URL: &str = "http://localhost:1234/dog";
#[derive(Clone, Debug, Deserialize, Serialize)]
struct Dog {
    id: String,
    breed: String,
    name: String,
}

async fn delete_all_dogs(client: &Client, dogs: &Vec<Dog>) -> Result<(), Box<dyn std::error::Error>> {
    for dog in dogs {
        let url = format!("{}/{}", BASE_URL, dog.id);
        client.delete(&url).send().await?.text().await?;
    }
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let count: usize = 10000;
    let client = reqwest::Client::new();

    let start = Instant::now();

    // Get all the current dogs.
    let dogs = reqwest::get(BASE_URL).await?.json::<Vec<Dog>>().await?;

    // Delete all the current dogs.
    delete_all_dogs(&client, &dogs).await?;

    // Create new dogs.
    for i in 0..count {
        let id = Uuid::new_v4().to_string();
        let dog = Dog {
            id,
            name: format!("name-{}", i),
            breed: format!("breed-{}", i)
        };
        let res = client.post(BASE_URL).json(&dog).send().await?;
        if !res.status().is_success() {
            eprintln!("error creating dog, status = {}", res.status());
        //} else {
        //    let new_dog: Dog = res.json().await?;
        //    println!("created dog {:?}", new_dog);
        }
    }
    let dogs = reqwest::get(BASE_URL).await?.json::<Vec<Dog>>().await?;
    //println!("after creates, dogs = {:#?}", dogs);
    assert_eq!(dogs.len(), count);

    // Update all the dogs.
    for dog in dogs {
        let id = dog.id.clone();
        let new_dog = Dog {
            id: id.clone(),
            name: format!("new-{}", dog.name),
            breed: format!("new-{}", dog.breed)
        };
        let url = format!("{}/{}", BASE_URL, id);
        let res = client.put(&url).json(&new_dog).send().await?;
        if !res.status().is_success() {
            eprintln!("error updating dog, status = {}", res.status());
        }
    }

    // Retrieve all the dogs.
    let dogs = reqwest::get(BASE_URL).await?.json::<Vec<Dog>>().await?;
    //println!("after updates, dogs = {:#?}", dogs);

    delete_all_dogs(&client, &dogs).await?;

    println!("elapsed time: {:?}", start.elapsed());

    Ok(())
}
```

## <a name="http-servers">HTTP Servers</a>

There are many ways to listen for and process HTTP requests.
Popular creates for implementing HTTP servers include:

- {% aTargetBlank "https://crates.io/crates/actix-web", "actix-web" %}
- {% aTargetBlank "https://crates.io/crates/rocket", "rocket" %}
- {% aTargetBlank "https://crates.io/crates/warp", "warp" %}
- {% aTargetBlank "https://crates.io/crates/tide", "tide" %}

In order to demonstrate using each of these frameworks,
we will implement the same set of CRUD REST services
that operate on a collection of dog descriptions.
Typically the data would be persisted to a database, but
to keep the focus on the frameworks it will just be held in memory.
This requires learning how each framework manages application state.

All of this code, along with benchmark results, can be found in GitHub at
{% aTargetBlank "https://github.com/mvolkmann/rust-rest", "rust-rest" %}.

The endpoints exposed will be:

- GET /dog - to retrieve all the dogs
- GET /dog/{id} - to retrieve a specific dog
- POST /dog - to create a dog
- PUT /dog/{id} - to update a dog
- DELETE /dog/{id} - to delete a dog

{% aTargetBlank
"https://www.techempower.com/benchmarks/#section=intro&hw=ph&test=fortune",
"TechEmpower" %} is a company that has been performing benchmarks of
web application frameworks since 2013.
They catalog community-provided code and results for a large number
of frameworks performing the same set of tasks that include
"JSON serialization, database access, and server-side template composition."
The tests are run "on cloud instances and on physical hardware."
They are rerun with the latest versions of each framework every few months.

As of February 2021, five of the top 10 frameworks were implemented in Rust.
Of these the most popular in terms of usage is actix.
The Rust frameworks listed above had the following percentage
performance scores compared to 100% for the fastest framework.
For reference, a few non-Rust frameworks are included.

| Framework      | Language   | Score      |
| -------------- | ---------- | ---------- |
| drogon         | C++        | 100%       |
| actix          | Rust       | 98.0%      |
| vertx          | Java       | 51.0%      |
| greenlightning | Java       | 49.0%      |
| warp           | Rust       | 22.4%      |
| php            | PHP        | 14.1%      |
| nodejs         | JavaScript | 13.7%      |
| micronaut      | Java       | 13.5%      |
| fastify        | JavaScript | 10.9%      |
| express        | JavaScript | 8.9%       |
| rocket         | Rust       | 5.0%       |
| fastapi        | Python     | 7.8%       |
| flask          | Python     | 3.5%       |
| spring         | Java       | 3.5%       |
| grails         | Java       | 1.7%       |
| laravel        | PHP        | 0.7%       |
| tide           | Rust       | not listed |

Interestingly most of the frameworks evaluated
are ones you have probably never heard of.

One reason to care about performance is cloud costs.

One might conclude from this that actix is the only
Rust framework that should be considered.
Why start with one that is significantly slower?

### Actix-web

This crate had a bad reputation for using unsafe code. See {% aTargetBlank
"https://deavid.wordpress.com/2020/01/18/actix-web-is-dead-about-unsafe-rust/",
"Actix-web is dead" %}.
However, as of January 2020, this crate has new maintainers
and perhaps the issues raised will be addressed.
They claim that all of the unsafe code has been carefully analyzed
and should be safe in practice.

Add the following dependencies in `Cargo.toml`:

```toml
actix-web = "3.3.2"
parking_lot = "0.11.1"
serde = "1.0.123"
uuid = { version = "0.8.2", features = ["serde", "v4"] }
```

Add the following code in `src/main.rs`:

```rust
use actix_web::{delete, get, post, put, web, App, HttpRequest, HttpResponse, HttpServer, Result};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

// We need to implement the "Clone" trait in order to
// call the "cloned" method in the "get_dogs" route.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct Dog {
    id: String,
    breed: String,
    name: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct NewDog {
    breed: String,
    name: String,
}

type DogMap = HashMap<String, Dog>;

struct AppState {
    dog_map: DogMap,
}

#[get("/dog")]
async fn get_dogs(state: web::Data<RwLock<AppState>>) -> Result<HttpResponse> {
    let state = state.read();
    let dogs: Vec<Dog> = state.dog_map.values().cloned().collect();
    Ok(HttpResponse::Ok().json(dogs))
}

#[get("/dog/{id}")]
async fn get_dog(req: HttpRequest, state: web::Data<RwLock<AppState>>) -> Result<HttpResponse> {
    let id = req.match_info().get("id").unwrap();
    let state = state.read();
    if let Some(dog) = state.dog_map.get(id) {
        Ok(HttpResponse::Ok().json(dog))
    } else {
        Ok(HttpResponse::NotFound().finish())
    }
}

#[post("/dog")]
async fn create_dog(
    json: web::Json<NewDog>,
    state: web::Data<RwLock<AppState>>,
) -> Result<HttpResponse> {
    let id = Uuid::new_v4().to_string();
    let new_dog = json.into_inner();
    let dog = Dog {
        id: id.clone(),
        name: new_dog.name,
        breed: new_dog.breed,
    };

    let mut state = state.write();
    state.dog_map.insert(id, dog.clone());
    Ok(HttpResponse::Created().json(dog))
}

#[put("/dog/{id}")]
async fn update_dog(
    json: web::Json<Dog>,
    state: web::Data<RwLock<AppState>>,
) -> Result<HttpResponse> {
    let dog = json.into_inner();
    let id = dog.id.clone();
    //println!("updating dog with id {}", id);
    let mut state = state.write();
    state.dog_map.insert(id, dog.clone());
    Ok(HttpResponse::Ok().json(dog))
}

#[delete("/dog/{id}")]
async fn delete_dog(req: HttpRequest, state: web::Data<RwLock<AppState>>) -> Result<HttpResponse> {
    let id = req.match_info().get("id").unwrap();
    let mut state = state.write();
    if let Some(_dog) = state.dog_map.remove(id) {
        Ok(HttpResponse::Ok().finish())
    } else {
        Ok(HttpResponse::NotFound().finish())
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let mut dog_map: HashMap<String, Dog> = HashMap::new();

    // Start with one dog already created.
    let id = Uuid::new_v4().to_string();
    let dog = Dog {
        id: id.clone(),
        name: "Comet".to_string(),
        breed: "Whippet".to_string(),
    };
    dog_map.insert(id, dog);

    let data = web::Data::new(RwLock::new(AppState { dog_map }));

    HttpServer::new(move || {
        App::new()
            .app_data(data.clone())
            .service(get_dog)
            .service(get_dogs)
            .service(delete_dog)
            .service(create_dog)
            .service(update_dog)
    })
    .bind(("127.0.0.1", 1234))?
    .run()
    .await
}
```

### Rocket

An issue with using Rocket is that it currently
requires using a nightly version of Rust.
For details, see {% aTargetBlank
"https://github.com/SergioBenitez/Rocket/issues/19#issuecomment-736637259",
"this issue" %}.
However, the stable version of Rust can be used with the master branch of Rocket
if that is specified in the `rocket` dependency.

Rocket includes built-in support for accessing the following databases:
Memcache, MongoDB, MySQL, Neo4J, PostgreSQL, Redis, and SQLite.

The {% aTargetBlank "https://rocket.rs/v0.4/guide/configuration/",
"Rocket configuration guide" %} says
"Warning: Rocket's built-in TLS is not considered ready for
production use. It is intended for development use only."
Presumably production uses of Rocket place the Rocket server
behind another server such as nginx to get HTTPS support.

Some aspects of Rocket are configured by created a `Rocket.toml` file.
If this file does not exist, default values are used.
Here is example content for this file:

```toml
[debug]
address = "127.0.0.1"
port = 1234
log_level = "normal"

[staging]
address = "0.0.0.0"
port = 80
log_level = "off"

[release]
address = "0.0.0.0"
port = 80
log_level = "critical"
```

Add the following dependencies in `Cargo.toml`:

```toml
parking_lot = "0.11.1"
rocket = { git = "https://github.com/SergioBenitez/Rocket", branch = "master" }
rocket_contrib = { git = "https://github.com/SergioBenitez/Rocket", branch = "master", features = ["json"] }
serde = { version = "1.0.118", features = ["derive", "rc"] }
serde_json = "1.0.60"
uuid = { version = "0.8.2", features = ["serde", "v4"] }
```

Add the following code in `src/main.rs`:

```rust
#[macro_use]
extern crate rocket;

use parking_lot::RwLock;
use rocket::State;
use rocket_contrib::json::Json;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use uuid::Uuid;

// We need to implement the "Clone" trait in order to
// call the "cloned" method in the "get_dogs" function.
#[derive(Clone, Deserialize, Serialize, Debug)]
struct Dog {
    id: String,
    breed: String,
    name: String,
}

#[derive(Deserialize, Serialize, Debug)]
struct NewDog {
    breed: String,
    name: String,
}

struct MyState {
    dog_map: Arc<RwLock<HashMap<String, Dog>>>,
}

#[rocket::main]
async fn main() {
    let mut dog_map: HashMap<String, Dog> = HashMap::new();

    let id = Uuid::new_v4().to_string();
    let dog = Dog {
        id: id.clone(),
        name: "Comet".to_string(),
        breed: "Whippet".to_string(),
    };
    dog_map.insert(id, dog);

    let state = MyState {
        dog_map: Arc::new(RwLock::new(dog_map)),
    };

    #[post("/", format = "json", data = "<json>")]
    fn create_dog(json: Json<NewDog>, state: State<MyState>) -> Json<Dog> {
        let new_dog = json.into_inner();
        let id = Uuid::new_v4().to_string();
        let dog = Dog {
            id: id.clone(),
            name: new_dog.name,
            breed: new_dog.breed,
        };
        //TODO: Find better way to get host and port.
        let url = format!("http://localhost:1234/dog/{}", &id);
        let mut dog_map = state.dog_map.write();
        dog_map.insert(id, dog.clone());

        Created::new(url).body(Json(dog))
    }

    #[delete("/<id>")]
    fn delete_dog(id: String, state: State<MyState>) -> NoContent {
        let mut dog_map = state.dog_map.write();
        dog_map.remove(&id);
        NoContent
    }

    #[get("/<id>", format = "json")]
    fn get_dog(id: String, state: State<MyState>) -> Option<Json<Dog>> {
        let dog_map = state.dog_map.read();
        if let Some(dog) = dog_map.get(&id) {
            Some(Json(dog.clone()))
        } else {
            None
        }
    }

    #[get("/")]
    fn get_dogs(state: State<MyState>) -> Json<Vec<Dog>> {
        let dog_map = state.dog_map.read();
        let dogs = dog_map.values().cloned().collect();
        Json(dogs)
    }

    #[put("/<id>", format = "json", data = "<json>")]
    fn update_dog(id: String, json: Json<Dog>, state: State<MyState>) -> Json<Dog> {
        let dog: Dog = json.into_inner();
        let mut dog_map = state.dog_map.write();
        dog_map.insert(id, dog.clone());
        Json(dog)
    }

    //TODO: Learn how to get this to use TLS/HTTPS.
    // Note that https://rocket.rs/v0.4/guide/configuration/ says
    // "Warning: Rocket's built-in TLS is not considered ready for
    // production use. It is intended for development use only."
    rocket::ignite()
        .manage(state)
        .mount(
            "/dog",
            routes![create_dog, delete_dog, get_dog, get_dogs, update_dog],
        )
        .launch()
        .await
        .expect("failed to start rocket");
}
```

### Tide

Add the following dependencies in `Cargo.toml`:

```toml
async-std = { version = "1.9.0", features = ["attributes"] }
parking_lot = "0.11.1"
serde = "1.0.123"
tide = "0.16.0"
uuid = { version = "0.8.2", features = ["serde", "v4"] }
```

While it may be possible to get Tide to work with tokio instead of async_std,
the author has not provided an easy way to do this
and no examples of doing it were found. See
{% aTargetBlank "https://github.com/http-rs/tide/issues/791", "this issue" %}.

Add the following code in `src/main.rs`:

```rust
use async_std::sync::Arc;
use parking_lot::RwLock;
use std::collections::HashMap;
use tide::prelude::*;
use tide::{Body, Request, Response, StatusCode};
use uuid::Uuid;

// We need to implement the "Clone" trait in order to
// call the "cloned" method in the "get_dogs" route.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct Dog {
    #[serde(default)]
    id: Option<String>,
    breed: String,
    name: String,
}

type DogMap = HashMap<String, Dog>;

#[derive(Clone)]
struct State {
    dog_map: Arc<RwLock<DogMap>>,
}

#[async_std::main]
async fn main() -> tide::Result<()> {
    let mut dog_map: HashMap<String, Dog> = HashMap::new();

    let id = Uuid::new_v4().to_string();
    let dog = Dog {
        id: Some(id.clone()),
        name: "Comet".to_string(),
        breed: "Whippet".to_string(),
    };
    dog_map.insert(id, dog);

    let state = State {
        dog_map: Arc::new(RwLock::new(dog_map)),
    };
    let mut app = tide::with_state(state);

    app.at("/dog")
        // Get all dogs.
        .get(|req: Request<State>| async move {
            let dog_map = req.state().dog_map.read();
            let dogs: Vec<Dog> = dog_map.values().cloned().collect();
            let mut res = Response::new(StatusCode::Ok);
            res.set_body(Body::from_json(&dogs)?);
            Ok(res)
        })
        // Create a dog.
        .post(|mut req: Request<State>| async move {
            let mut dog: Dog = req.body_json().await?;
            let id = Uuid::new_v4().to_string();
            dog.id = Some(id.clone());
            let mut dog_map = req.state().dog_map.write();
            dog_map.insert(id, dog.clone());
            let mut res = tide::Response::new(StatusCode::Created);
            res.set_body(Body::from_json(&dog)?);
            Ok(res)
        });

    app.at("/dog/:id")
        // Get a specific dog.
        .get(|req: Request<State>| async move {
            let id = req.param("id")?;
            let dog_map = req.state().dog_map.read();
            if let Some(dog) = dog_map.get(id.clone()) {
                let mut res = Response::new(StatusCode::Ok);
                res.set_body(Body::from_json(&dog)?);
                Ok(res)
            } else {
                Ok(Response::new(StatusCode::NotFound))
            }
        })
        // Update a dog.
        .put(|mut req: Request<State>| async move {
            let dog: Dog = req.body_json().await?;
            let id = req.param("id")?;
            let mut dog_map = req.state().dog_map.write();
            if let Some(_dog) = dog_map.get(id) {
                dog_map.insert(id.to_string(), dog.clone());
                let mut res = tide::Response::new(StatusCode::Ok);
                res.set_body(Body::from_json(&dog)?);
                Ok(res)
            } else {
                Ok(Response::new(StatusCode::NotFound))
            }
        })
        // Delete a dog.
        .delete(|req: Request<State>| async move {
            let id = req.param("id")?;
            let mut dog_map = req.state().dog_map.write();
            if let Some(_dog) = dog_map.remove(id) {
                Ok(Response::new(StatusCode::Ok))
            } else {
                Ok(Response::new(StatusCode::NotFound))
            }
        });

    app.listen("127.0.0.1:1234").await?;
    Ok(())
}
```

### Warp

The Warp framework was created by Sean McArthur (@seanmonster)
who has created many popular Rust crates including
hyper, reqwest, and pretty-env-logger.
It defines HTTP routes using "filters".

Add the following dependencies in `Cargo.toml`:

```toml
parking_lot = "0.11.1"
serde = { version = "1.0.123", features = ["derive"] }
serde_json = "1.0.61"
tokio = { version = "1.1.1", features = ["full"] }
uuid = { version = "0.8.2", features = ["serde", "v4"] }
warp = "0.3.0"
```

Add the following code in `src/main.rs`:

```rust
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::Infallible;
use std::sync::Arc;
use uuid::Uuid;
use warp::http::StatusCode;
use warp::reply::{json, with_status, Json, Reply};
use warp::{Filter, Rejection};

// We need to implement the "Clone" trait in order to
// call the "cloned" method in the "get_dogs" route.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct Dog {
    id: String,
    breed: String,
    name: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct NewDog {
    breed: String,
    name: String,
}

type DogMap = HashMap<String, Dog>;

type State = Arc<RwLock<DogMap>>;

#[tokio::main]
async fn main() {
    // Add one dog for testing.
    let id = Uuid::new_v4().to_string();
    let dog = Dog {
        id: id.clone(),
        name: "Comet".to_string(),
        breed: "Whippet".to_string(),
    };
    let mut dog_map = HashMap::new();
    dog_map.insert(id, dog);

    let state: State = Arc::new(RwLock::new(dog_map));

    fn with_state(state: State) -> impl Filter<Extract = (State,), Error = Infallible> + Clone {
        warp::any().map(move || state.clone())
    }

    let get_dogs = warp::path!("dog")
        .and(warp::get())
        .and(with_state(state.clone()))
        .and_then(handle_get_dogs);

    // In routes that cannot return an Err,
    // the compiler cannot infer the error type for the Result.
    // This must be an async fn instead of a closure passed to and_then
    // until proper support for async closures is added to Rust.
    async fn handle_get_dogs(state: State) -> Result<Json, Rejection> {
        let dog_map = state.read();
        let dogs: Vec<Dog> = dog_map.values().cloned().collect();
        Ok(warp::reply::json(&dogs))
    }

    let get_dog = warp::path!("dog" / String)
        .and(warp::get())
        .and(with_state(state.clone()))
        .and_then(handle_get_dog);

    async fn handle_get_dog(id: String, state: State) -> Result<impl Reply, Infallible> {
        let dog_map = state.read();
        if let Some(dog) = dog_map.get(&id) {
            Ok(with_status(json(&dog), StatusCode::OK))
        } else {
            Ok(with_status(json(&""), StatusCode::NOT_FOUND))
        }
    }

    let create_dog = warp::path!("dog")
        .and(warp::post())
        .and(warp::body::json())
        .and(with_state(state.clone()))
        .and_then(handle_create_dog);

    // See the comment above the handle_get_dogs function.
    async fn handle_create_dog(new_dog: NewDog, state: State) -> Result<impl Reply, Rejection> {
        let id = Uuid::new_v4().to_string();
        let dog = Dog {
            id: id.clone(),
            name: new_dog.name,
            breed: new_dog.breed,
        };
        let mut dog_map = state.write();
        dog_map.insert(id, dog.clone());
        Ok(with_status(json(&dog), StatusCode::CREATED))
    }

    let update_dog = warp::path!("dog" / String)
        .and(warp::put())
        .and(warp::body::json())
        .and(with_state(state.clone()))
        .and_then(|id: String, dog: Dog, state: State| async move {
            let mut dog_map = state.write();
            if let Some(_dog) = &dog_map.get(&id) {
                dog_map.insert(id, dog.clone());
                Ok(warp::reply::json(&dog))
            } else {
                Err(warp::reject::not_found())
            }
        });

    let delete_dog = warp::path!("dog" / String)
        .and(warp::delete())
        .and(with_state(state.clone()))
        .and_then(|id: String, state: State| async move {
            let mut dog_map = state.write();
            if let Some(_dog) = dog_map.remove(&id) {
                Ok(with_status("", StatusCode::NO_CONTENT))
            } else {
                Err(warp::reject::not_found())
            }
        });

    //TODO: Learn how to get this to use TLS/HTTPS.
    let routes = get_dogs
        .or(create_dog)
        .or(update_dog)
        .or(delete_dog)
        .or(get_dog);
    warp::serve(routes).run(([127, 0, 0, 1], 1234)).await;
}
```
