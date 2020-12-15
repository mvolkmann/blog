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
- targets LLVM, so runs on a wide variety of platforms that targets
- can call and be called by C

Rust was created at Mozilla by Graydon Hoare,
with contributions from Dave Herman, Brendan Eich, and others.
It was formally announced in 2010.
Rust has been self-hosted (implemented in itself) since 2011.
Version 1.0 was released in May 2015.
A new point release is made every six weeks.

From {% aTargetBlank "https://doc.rust-lang.org/edition-guide/editions/",
"rust-lang.org" %},
"Every two or three years, we'll be producing a new edition of Rust.
Each edition brings together the features that have landed into
a clear package, with fully updated documentation and tooling."

## Why use Rust

**Performance:**

The best way to get software performance is to
use a "systems" language like C, C++, or Rust.
One reason these languages are fast is because
they do not provide automatic garbage collection
that is slow and can run at unpredictable times.
Systems languages also allow control over
whether data is on the stack or on the heap.

**Safety:**

Software written in systems languages typically must
take great care to avoid memory and threading issues.
Memory issues include accessing memory after it has been freed,
resulting in unpredictable behavior.
Threading issues include race conditions where the order in which
code runs is unpredictable, resulting in unpredictable results.
Rust addresses both of these issues,
resulting in code that is less likely to contain bugs.

**Immutable by default:**

A large source of errors in any software involves
incorrect assumptions about where data is modified.
Making variables immutable by default and
requiring explicit indication of functions that are
allowed to modify data significantly reduces these errors.

**Control over number sizes:**

One way to achieve performance in computationally intensive tasks
is to store collections of numbers in contiguous memory for fast access
and control the number of bytes used by each number.

**Ownership model:**

Manual garbage collection is error prone.
Rust uses an "ownership model" where code is explicit about
the single scope that owns each piece of data.
When that scope ends, the data can be safely freed
because no other scope can possibly be using the data.

Systems languages tend to be more complex that non-systems languages,
requiring more time to learn and more time to write software in them.
Rust is no exception.
But some developers choose to use Rust in spite of this
in order to gain the benefits described above.
On the positive side, the Rust compiler catches many errors
that would only be discovered at runtime with other systems languages.
The Rust compiler also provides very detailed error messages
that include suggestions on how to correct the errors.

## Installing

Rust is installed using the {% aTargetBlank "", "rustup" %} tool.
This enables having multiple versions of Rust installed
and switching between them.

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

All of the code must be entered in a single editor pane,
simulating all of it being in a single file.

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
  and "ADVANCED" (line numbers)
- "Keybinding" to choose between keybindings supported by the
  {% aTargetBlank "https://github.com/ajaxorg/ace", "Ace" %} (Cloud9) editor  
  These include ace, emacs, sublime, vim, and vscode.
- "Theme" to choose from 30+ themes including
  cobalt, github, solarized light, solarized dark
- "Pair Characters" to automatically insert
  closing `)`, `}`, and `]` character after `(`, `{`, and `[` characters
- "Orientation" to arrange panes horizontally, vertically,
  or automatically choose based on window size
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

See the "Cargo" section for an alternative way
to compile and run a Rust program.

## VS Code

Install the Rust extension which adds:

- syntax highlighting
- code completion
- code formatting
- type documentation on hover
- linting with error indicators with ability to apply suggestions
- code snippets
- rename refactoring
- debugging
- build tasks

Add the following in `settings.json`:

```json
  "[rust]": {
    "editor.defaultFormatter": "rust-lang.rust",
    "editor.insertSpaces": true,
    "editor.tabSize": 4
  },
```

Note that this extension only works if
the opened folder contains a `Cargo.toml` file.

## Terminology

`cargo`: a command-line utility described later
crate: a binary (executable) or a library
module: a set of related values such as constants and functions
package: a set of related crates described by a `Cargo.toml` file
TOML: a configuration file format; stands for Tom's Obvious, Minimal Language

## TOML

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

To watch project files for changes and
automatically run a `cargo` command when they do,
enter `cargo install cargo-watch` one time
and then enter `cargo watch -x subcommand`.
The `-x` flag can be omitted in which case
the subcommand defaults to `check`, not `run`.
Typically you will want the subcommand to be `run`.

## Formatting Code

The most popular code formatting tool for Rust is
{% aTargetBlank "", "rustfmt" %}.
To install this, enter `cargo install rustfmt`.
TODO: Is this installed by default by rustup?

To run it on all `.rs` files in the current directory,
enter `rustfmt *.rs`.

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

The compiler outputs warnings when these naming conventions are not followed.

## Syntax Highlights

- The preferred indentation is four spaces.
- Statements must terminated by a semicolon.
- Strings are delimited by double quotes.
- Single characters are delimited by single quotes.
- Items are made public using the `pub` keyword.
- The dot (`.`) character is used to
  access struct fields and call instance methods.
- The double colon (`::`) is used as
  a namespace separator (borrowed from C++)
  and to call static methods.
- Conditions for conditional logic and iteration are not
  surrounded by any delimiter such as parentheses.
- Statements associated with conditional logic and iteration
  must be in blocks surrounded by curly brackets.
- Named functions are declared with the `fn` keyword.
- Function return types follow the parameter list and `->`.
- Functions that return nothing omit the `->` and return type.
- Most statements are also expressions and evaluate to a value,
  including `if` and `match` statements.
- If the last expression in a function does not end with a semicolon,
  it's value is returned.
- There is no null type or value.
  Instead the wrapper enum types `Option` and `Result` are used.

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

## <a name="attributes">Attributes</a>

Rust attributes are like "decorators" in other programming languages.
They annotate an item in order to change its behavior.
An attribute can be specified
immediately before the declaration of an item with the syntax `#[attr]`
or inside the declaration with the syntax `#![attr]`.

The following table summarizes commonly used attributes.

| Attribute                        | Description                                                  |
| -------------------------------- | ------------------------------------------------------------ |
| `allow(warning1, warning2, ...)` | suppress specified warnings (ex. `dead_code` )               |
| `derive(trait1, trait2, ...)`    | automatically implement a list of traits on a `struct`       |
| `doc`                            | provides an alternate way to specify and format doc comments |
| `should_panic`                   | indicates that a test is expected to panic                   |
| `test`                           | annotates a function as a test                               |

For more, see the list at {% aTargetBlank
"https://doc.rust-lang.org/reference/attributes.html#built-in-attributes-index",
"Attributes" %}.

For the `derive` attribute, traits that can be automatically implemented
are described in the following table:

| Trait Name   | Description                                                                               |
| ------------ | ----------------------------------------------------------------------------------------- |
| `Clone`      | adds ability to explicitly copy an object using the `clone` method                        |
| `Copy`       | adds ability to implicitly copy an object in assignment or pass by value                  |
| `Debug`      | adds ability to output a value for debugging using `{:?}` and `{:#?}` in a format string  |
| `Default`    | adds a `default` static method for getting an empty or default instance of a type         |
| `Eq`         | adds ability to compare instances using `==` and `!=`                                     |
| `Hash`       | adds a `hash` method for computing the hash value of an instance (1)                      |
| `Ord`        | adds ability to compare instances using `<`, `<=`, `==`, `!=`, `>=`, and `>` operators    |
| `PartialEq`  | like `Eq`, but for types where some instances are not equal to themselves (2)             |
| `PartialOrd` | like `Ord`, but for types where some instances cannot be logically compared to others (3) |

1. The `hash` method is used by the `HashMap` and `HashSet` collections.
1. This means values are not necessarily reflexive.
   For example, the number value `NaN` is not equal to itself.
1. For example, the number value `NaN` is not
   less than, equal to, or greater than zero.

For more detail, see {% aTargetBlank
"https://doc.rust-lang.org/rust-by-example/trait/derive.html", "Derive" %}.

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

To print a representation of a value for debugging purposes on a single line,
use `{:?}`. To print each field of a struct on separate lines, use `{:#?}`.
Custom structs must implement the `Debug` trait in order to use these.
This is done by adding the line `#[derive(Debug)]` before their definitions.

The following table summarizes the supported format strings
that can appear inside the curly brackets.

| Format String | Description                                                           |
| ------------- | --------------------------------------------------------------------- |
| `{:?}`        | debugging output on single line                                       |
| `{:#?}`       | debugging output on multiple lines                                    |
| `{n}`         | prints the argument at index n (zero-based)                           |
| `{name}`      | prints the value with a given name                                    |
| `{:.n}`       | prints a number with `n` decimal places                               |
| `{:.*}`       | prints a number with number of decimal places specified in value list |
| `{:#X}`       | prints number as uppercase hexadecimal                                |
| `{:#x}`       | prints number as lowercase hexadecimal                                |
| `{:<n}`       | print left justified in a width of n                                  |
| `{:>n}`       | print right justified in a width of n                                 |
| `{:^n}`       | print centered in a width of n                                        |

Here are some examples:

```rust
#[derive(Debug)]
struct Point2D {
    x: f64,
    y: f64
}

let p = Point2D { x: 1.0, y: 2.0 };
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

For more options, see
{% aTargetBlank "https://doc.rust-lang.org/std/fmt/", "std::fmt" %}.

## Variables

Variables are immutable by default.
For variables that hold non-primitive values such as structs and arrays,
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
| `const name: type = value`      | constant that must be assigned a value when it is declared                                            |
| `static name: type = value`     | immutable variable that lives for the duration of the program; typically `const` is preferred         |
| `static mut name: type = value` | mutable variable that lives for the duration of the program;<br>can only mutate in `unsafe` functions |

Note that `const` declarations must be explicitly typed.
They do not infer a type based on the assigned value.

TODO: Are statics a way to share data across functions,
TODO: even those defined in separate files, without passing it?

## Ownership Model

The Rust ownership model provides the following benefits:

- runtime speed achieved by eliminating the need for a garbage collector (GC)
- more predictable performance since there are no GC pauses
- safer memory access since there is no possibility of
  null pointer accesses or dangling pointer accesses
  (accessing memory that has already been freed)
- safer parallel and concurrent processing
  since there is no possibility of data races
  causing unpredictable interactions between threads

Memory management is handled by following these rules:

1. Each value is referred to by a variable that is its owner.
1. Each value has one owner at a time, the owner can change over its lifetime.
1. When the owner goes out of the scope, the value is dropped.

Variable values are stored either in the stack or the heap.
Accessing stack data is faster, but data on the heap can grow and shrink
and it can live beyond the scope that created it.

Variable values whose sizes are known at compile time are stored on the stack.
This includes booleans (`bool` type), single characters (`char` type), numbers,
tuples, and arrays.
Variable values of all other types are stored in the heap.
This includes:

- strings (`&str` and `String`)
- structs, even those that only contain
  fields with types that have a known size
- collections from the `std::collections` namespace
  which defines sequences (`Vec`, `VecDeque`, and `LinkedList`),
  sets (`HashSet` and `BTreeSet`), and maps (`HashMap` and `BTreeMap`).

A value of these types can be stored on the heap by using the `Box` type.
For example:

```rust
let heap_int: Box<i32> = Box::new(19);
```

Note: Sometimes Rust stores `&str` values on the stack
but you cannot control that, so it's best to think of them
as always being on the heap.

All code blocks are delimited by a pair of curly brackets
and create a new scope.
Each new scope can add data to the stack
that is freed when that scope exits.
Many keywords have an associated block, including
`fn`, `if`, `loop`, `for`, and `while`.

Here are some examples that demonstrate ownership
inside a single function:

```rust
fn main() {
    let a = 1;
    // Because a is a scalar type (fixed size),
    // this makes a copy of a and assigns that to b
    // rather than moving ownership from a to b.
    // Both a and b can then be used.
    let b = a;
    println!("b = {}", b); // 1
    println!("a = {}", a); // 1

    let c = String::from("test");
    // Because c is on the heap and does not implement the Copy trait,
    // this moves ownership from c to d.
    // c can no longer be used.
    let d = c;
    println!("d = {}", d); // test
    //println!("c = {}", c); // error "value borrowed here after move"

    // The Copy trait requires also implementing the Clone trait.
    // We can also implement these traits manually, but that is more work.
    #[derive(Clone, Copy, Debug)]
    struct Point2D {
        x: f64,
        y: f64
    }
    let e = Point2D { x: 1.0, y: 2.0 };
    // If the struct implements the Copy trait, as we have done above,
    // a copy is made.  Otherwise this moves ownership from e to f.
    let f = e;
    println!("f = {:?}", f); // Point2D { x: 1.0, y: 2.0 }
    // This fails if ownership has been moved from e to f.
    println!("e = {:?}", e); // error "value borrowed here after move"
}
```

Ownership of a value can also be "borrowed".
For example:

```rust
let e = Point2D { x: 1.0, y: 2.0 };
let f = &e;
println!("f = {:?}", f); // Point2D { x: 1.0, y: 2.0 }
println!("e = {:?}", e); // Point2D { x: 1.0, y: 2.0 }
```

When a value is mutable and ownership is borrowed,
Rust will flag an error if the value is mutated
after ownership is borrowed and before the last use of the borrow.
This is because references expect the data they reference
to remain the same.
For example:

```rust
let mut e = Point2D { x: 1.0, y: 2.0 };
let f = &e; // f borrows a reference rather than taking ownership
println!("f = {:?}", f); // works
// If f is used after this, the next line triggers the error
// "cannot assign to `e.x` because it is borrowed".
e.x += 3.0;
println!("e = {:?}", e); // Point2D { x: 4.0, y: 2.0 }
println!("f = {:?}", f); // triggers error on mutation above
```

An alternative is to clone data instead of borrowing a reference,
but doing this is often unnecessarily inefficient.
To clone a value whose type implements the `Clone` trait,
call the `clone` method on it.
For example, `let f = e.clone();`

When stack variables are passed to functions,
the functions are given copies.
This is true even if the parameters are declared to be mutable.
For example:

```rust
fn my_function(x: i32) {
    println!("{}", x); // 1
}

fn main() {
    let x = 1;
    my_function(x);
    println!("{}", x); // 1
}
```

When heap variables (not references) are passed to functions,
copies are not made and ownership is transferred.
When the function exits, the data is freed.
The calling function can no longer use the variable that was passed in.
For example:

```rust
// Note that it is preferable to use &str instead of String here
// unless we need a mutable String as demonstrated below.
// However, we want to demonstrate using an argument value
// that is definitely in the heap.
fn my_function(s: String) {
    println!("{}", s); // "test"
}

fn main() {
    let s = String::from("test");
    my_function(s); // error "borrow of moved value: `s`"
    println!("{}", s); // triggers error above
}
```

When references to stack or heap variables are passed to functions,
ownership is borrowed by the function and
is returned to the calling function when the function completes.
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
    *i += 1;
    println!("{}", s); // "test"
    s.push_str(" more");
}

fn main() {
    let mut i = 1; // on stack
    let mut s = String::from("test"); // on heap
    // Even though i and s are mutable, the arguments to
    // my_function below do not need to be marked as mutable
    // unless that function requires them to be mutable.
    my_function(&mut i, &mut s);
    println!("{}", i); // 2
    println!("{}", s); // "test more"
}
```

Early we said that memory allocated in a scope is freed when that scope exits.
However, there is an exception to this
when ownership is transferred outside the block.
For example:

```rust
fn main() {
    let a;

    {
        // Allocate inside block.
        let b = String::from("test");

        // Move ownership to a which lives outside this block.
        a = b;

        // If the previous line is changed to
        // a = &b;
        // we get the error "`b` does not live long enough"
        // because a will no longer get ownership
        // and b will be freed at the end of the block.

        // Memory for b is not freed when this block exits
        // because b no longer owns it.
    }

    // We can use the value here because
    // the lifetime of a has not ended yet.
    println!("{}", a);
}
```

Here is a similar example using a closure:

```rust
fn main() {
    let mut a = String::new();
    let mut inner = | | {
        let b = String::from("test");
        a = b;
    };
    inner();
    println!("{}", a);
}
```

## Dereference

The dereference operator is used to get the value of a reference.
It isn't needed very often.
This is because unlike in most programming languages
that support references (or pointers),
Rust does not require different syntax for accessing fields and methods
based on whether an instance or a reference is used.
For example:

```rust
struct Point2D {
    x: f64,
    y: f64
}

impl Point2D {
    fn is_origin(&self) -> bool {
        self.x == 0.0 &&self.y == 0.0
    }
}

fn main() {
    let p = Point2D { x: 1.0, y: 2.0 };
    let p_ref = &p;
    println!("{}", p.x); // 1
    println!("{}", p_ref.x); // 1
    println!("{}", p.is_origin()); // false
    println!("{}", p_ref.is_origin()); // false
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

## Lifetimes

Lifetimes ensure that memory does not get freed
before a reference to it can use it.
This is only a concern in functions that
take two or more references and return one of them.

All reference parameters and reference return types have a lifetime,
but the Rust compiler automatically determines them in most cases.
When it cannot, you must explicitly specify them.
This is typically only needed when
reference parameters can be returned.
Usually the same lifetime is used on
all of them AND on the return reference type.

Lifetimes are specified appear before type names
are are composed of a single quote followed by a name
which is typically a single letter such as "a".
They only serve to indicate which items in a function signature
have the same lifetime, not an actual duration.

The following code illustrates potential errors
that lifetime checking prevents.

```rust
fn a(s1: &String) -> &String {
    let s2 = String::from("second");
    return b(s1, &s2);
}

// This function signature results in
// "explicit lifetime required" errors for s1 and s2.
// and a "missing lifetime specifier" error on the return type.
// This is because when more than one reference is passed to a function
// AND one of them can be returned, Rust requires lifetime specifiers.
//fn b(s1: &String, s2: &String) -> &String {

// This function signature includes lifetime specifiers.
// Now we get an error on the call to function b above
// because it might return the value of the local variable s2
// which is freed when function a exits.
// "s2" would not be available in the caller ("main" in this case).
fn b<'a>(s1: &'a String, s2: &'a String) -> &'a String {
    if s1 > s2 {
        s1
    } else {
        s2
    }
}

fn main() {
    let s1 = String::from("first");
    println!("greatest is {}", a(&s1));
}
```

To use more than one lifetime specifier in a function signature,
list them after the function name inside angle brackets separated by commas.
For example, `fn my_function<'a, 'b>(...)`.

To specify that lifetime `b` is at least as long as lifetime `a`,
use `fn my_function<'a, 'b: 'a>(...)`.

## Built-in Scalar Types

Rust defines four scalar (primitive) types which are
boolean, character, integer, and floating point.

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

Floating point type names are `f{n}` where `{n}` is 32 or 64.
The default type for literal floats is `f64` regardless of the processor.
Literal floating point values must include a decimal point
to avoid being treated as integer values.

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

```rust
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

Rust defines many kinds of collections that hold a variable number of values.
These include strings and collections in the `std::collections` namespace.

The `std::collections` namespace defines the following sequence types:

- `Vec`: a resizable, ordered array of any kind of value
  where items can be efficiently added at the end
- `VecDeque`: like a `Vec`, but items
  can also be efficiently added at the beginning
- `LinkedList`: like a `Vec`, but it they
  can be efficiently split and appended

The `std::collections` namespace defines the following map types:

- `HashMap`: a collection of key/value pairs with efficient value lookup by key
  where keys and values can be any kind of value
- `BTreeMap`: like a `HashMap`, but sorted by key enabling efficient retrieval
  of values corresponding to the smallest key, largest key,
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

### Strings

Strings are collections of UTF-8 encoded characters.
Literal values are surrounded by double quotes.
Strings are more difficult to work with in Rust than in other languages.
Rust trades simplicity here for better
performance, concurrency, memory management.

There are two kinds of strings in Rust.
The language defines the "string slice" type `&str`
and the standard library defines the `String` type.
A `&str` value has a fixed length and its data
can be stored on the stack or in the heap.
Variables of this type hold a reference to the data wherever it lives.
A `String` value has a variable length and is stored in the heap.

Literal strings (zero or more characters) are surrounded by double quotes
and have the type `&str`.
Literal characters (just one) are surrounded by single quotes
and have the type `char`.

In the table below, assume
the variable `c` holds a `char` value,
the variables `s` and `t` hold `&str` values, and
the variables `u`, `v`, and `w` hold `String` values.
Everywhere `c` is used, a literal character can be used in its place.
Everywhere `s` and `t` are used, a literal string can be used in its place.

| Operation                                         | Syntax                           |
| ------------------------------------------------- | -------------------------------- |
| create `&str`                                     | `"text in double quotes"`        |
| create `String` #1                                | `String::from(s)`                |
| create `String` #2                                | `s.to_string()`                  |
| create empty `String`                             | `String::new()`                  |
| create `String` from multiple `&str` #1           | `let u = [s, t].concat();`       |
| create `String` from multiple `&str` #2           | `let u = format!("{}{}", s, t);` |
| create `String` from `String` and `&str` (1)      | `let u = v + s;`                 |
| create `String` from multiple `String` values (2) | `let u = v + &w;`                |
| convert `&str` to `String`                        | `s.to_string()`                  |
| convert `String` to `&str` without copying        | `let s = &t;`                    |
| concatenate to `&str`                             | cannot be done                   |
| concatenate to `&str` to `String`                 | `u += s;`                        |
| concatenate to `String` to `String`               | `u += v;`                        |
| concatenate to `char` to `String` (3)             | `u.push(c);`                     |
| concatenate to `&str` to `String` (3)             | `u.push_str(s);`                 |
| get substring of `&str`                           | `s[start..end]` (4)              |
| get substring of `String`                         | same as for `&str`               |
| get substring from index to end                   | `s[start..]`                     |
| get substring from beginning to index             | `s[..end]`                       |
| get substring where end is inclusive              | `u[start.. =end]`                |
| get `char` at index from `&str`                   | `s.chars().nth(index)` (5)       |
| get `char` at index from `String`                 | `&u.chars().nth(index)`          |

1. The `String` `u` here must be first.
1. All `String` values on the right of `=` after the first
   must be preceded by `&` which converts it to a `&str`.
1. The `String` `u` must be mutable.
1. `start` is inclusive and `end` is exclusive.
1. The `chars` method can be used to iterate over the characters in a string.
   The `nth` method returns a `Option` object because
   the string may be shorter than the index.
   To get the `char` from it, use one of the approaches below.

```rust
let char5 = &myString.chars().nth(5);

// Approach #1
if let Some(c) = char5 {
    println!("5th char is {}", c);
}

// Approach #2
match char5 {
    Some(c) => println!("5th char is {}", c);
    None => {} // ignores when string is shorter
}
```

In many programming languages strings are immutable.
To make a change you create a new string
and assign it back to the same variable.
In Rust the `&mut str` type can be used for this.
If it is desirable to modify a string in place,
perhaps for performance reasons, the `mut String` type can be used instead.
For example:

```rust
let mut s1 = "first";
s1 = "second";

let mut s2 = String::from("first");
s2.replace_range(.., "second");
```

When a `String` reference is passed to a function that expects a `&str`
it is automatically coerced to that type.
For example:

```rust
fn my_function(s: &str) {
    println!("{}", s); // "test"
}

fn main() {
    let s = String::from("test");
    my_function(&s);
    println!("{}", s); // "test"
}
```

### Vectors

| Operation    | Syntax       |
| ------------ | ------------ |
| create empty | `Vec::new()` |

TODO: Finish this.

### Sets

To use the `HashSet` type by ony its name:

```rust
use std::collections::HashSet;
```

Here is an example of creating and using a `HashSet`
containing `String` elements:

```rust
// Element type is inferred from what is inserted.
let mut colors = HashSet::new();
colors.insert("red");
colors.insert("green");
colors.insert("blue");

println!("colors = {:?}", colors);
println!("color count = {:?}", colors.len()); // 3
println!("contains green? = {:?}", colors.contains("green")); // true
println!("contains orange? = {:?}", colors.contains("orange")); // false

colors.remove("green");
println!("after removing green, colors = {:?}", colors);

for color in &colors {
    println!("{}", color);
}
```

Here is an example of creating and using a `HashSet`
containing `struct` elements:

```rust
#[derive(Debug, Eq, Hash, PartialEq)]
struct Dog {
    name: String,
    breed: String
}
impl Dog {
    fn new(name: &str, breed: &str) -> Self {
        Dog {
            name: name.to_string(),
            breed: breed.to_string()
        }
    }
}

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
println!("contains Comet? = {:?}", dogs.contains(&comet)); // true
println!("contains Spot? = {:?}", dogs.contains(&spot)); // false
```

### Maps

To use the `HashMap` type by ony its name:

```rust
use std::collections::HashMap;
```

Here is an example of creating and using a `HashMap`
with `String` keys and `i32` values:

```rust
// Key and value types are inferred from what is inserted.
let mut days_in_month = HashMap::new();
days_in_month.insert("January", 31);
days_in_month.insert("February", 28);
days_in_month.insert("March", 31);
days_in_month.insert("April", 30);

println!("daysInMonth = {:#?}", days_in_month);
println!("entries = {:?}", days_in_month.len()); // 4
println!("days in March = {:?}", days_in_month.get("March").unwrap());
days_in_month.remove("February");
println!("entries = {:?}", days_in_month.len()); // 3
println!("days in February = {:?}", days_in_month.get("February"));

let month = "April";
match days_in_month.get(month) {
    Some(days) => println!("There are {} days in {}.", days, month),
    None => println!("No data found for {}.", month)
}

for (month, days) in &days_in_month {
    println!("There are {} days in {}.", days, month);
}
```

Here is an example of creating and using a `HashMap`
with `String` keys and `struct` values:

```rust
#[derive(Debug, Eq, Hash, PartialEq)]
struct Dog {
    name: String,
    breed: String
}
impl Dog {
    fn new(name: &str, breed: &str) -> Self {
        Dog {
            name: name.to_string(),
            breed: breed.to_string()
        }
    }
}

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
```

## Conditional Logic

`if` expressions are the most common way to implement conditional logic.
The condition is not surrounded by parentheses.
where blocks require surrounding curly brackets.
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

The expression can be assigned to a variable.
Newlines are not required, so this can be written on a single line.
For example:

```rust
let color = if temperature > 90 { "red" } else { "blue" };
```

Other ways to implement conditional logic
include `if let` and `match` expressions
which use pattern matching to extract a value.
These are often used in conjunction with `Option` and `Result` enum types
which can be the result type of functions that can fail.

Here is an example of using the `Option` type
whose possible values are `Some(value)` and `None`.
This is similar to the `Maybe` monad in Haskell.

```rust
fn divide(numerator: f64, denominator: f64) -> Option<f64> {
    if denominator == 0. {
        None // means there is no result, but doesn't explain why
    } else {
        Some(numerator / denominator)
    }
}

fn main() {
    let n = 5.;
    let d = 2.;

    match divide(n, d) {
        None => println!("divide by zero"),
        Some(result) => println!("{:.2}", result),
    }

    if let Some(result) = divide(n, d) {
        println!("result is {}", result);
    } else {
        println!("fail")
    }
}
```

Here is an example of using the `Result` type
whose possible values are `Ok(value)` and `Err(why)`.
It differs from the `Option` type in that
it can express why a function failed.
This is similar to the `Either` monad in Haskell.

```rust
#[derive(Debug)]
pub enum MathError {
    DivisionByZero
}

// Commented lines show an alternative way
// to describe the error using a string.
//const DIV_BY_ZERO: &str = "divide by zero";

fn divide(numerator: f64, denominator: f64) -> Result<f64, MathError> {
//fn divide(numerator: f64, denominator: f64) -> Result<f64, &'static str> {
    if denominator == 0. {
        Err(MathError::DivisionByZero)
        //Err(DIV_BY_ZERO)
    } else {
        Ok(numerator / denominator)
    }
}

fn main() {
    let n = 5.;
    let d = 0.;

    match divide(n, d) {
        Err(e) => println!("{:?}", e),
        //Err(msg) => println!("{}", msg),
        Ok(result) => println!("result is {:.2}", result),
    }

    if let Ok(result) = divide(n, d) {
        println!("result is {}", result);
    } else {
        println!("fail")
    }
}
```

`match` expressions can be used to match on any kind of value.
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
    _ => "unknown" // underscore matches an other value
};
println!("The holiday in {} is {}.", month, holiday);
```

The part of each match on the left side of `=>` is called a "match arm".
It can list multiple values separated by `|` characters.
It can also specify a numeric range.

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

Rust does not support the ternary operator (`? :`)
found in many other programming languages.
Since `if` forms an expression that has a value,
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

## <a name="standard-io">Standard IO</a>

The `std::io` namespace supports many input/output operations.
The members `stdin` and `stdout` are functions that return objects
with methods for operating on the actual `stdio` and `stdout` streams.

The `stdin` methods like `read_line` and
`stdout` methods like `write` and `flush` return a `Result` enum value.
The `unwrap` methods can be called on this.
If the enum value is `Ok`, this returns the value it contains.
If the enum value is `Err`, this panics.
The `expect` method is similar, but allows specifying an error message.

```rust
// The Write trait is required in order to use the flush method.
use std::io::{stdin, stdout, Write};

fn main() {
  let mut buffer = String::new();

  loop {
    print!("Command: ");
    stdout().flush().unwrap();
    stdin().read_line(&mut buffer).unwrap();
    buffer.pop(); // removes newline from end of buffer

    if buffer == "quit" {
      break;
    }

    println!("You entered {}.", buffer);

    buffer.clear(); // prepares to reuse buffer
  }
}
```

Here is a modified version of the code above that uses the `text_io` crate:
To use this, add the following to the dependency
`text_io = "0.1.8"` in Cargo.toml.
It also adds a `print_flush` function to simplify
writing to `stdout` without including a newline.

```rust
use std::io::{self, Write};

use text_io::read;

fn print_flush(text: &str) {
  let mut stdout = io::stdout();
  stdout.write(text.as_bytes()).unwrap();
  stdout.flush().unwrap();
}

fn main() {
  loop {
    print_flush("Command: ");
    let command: String = read!("{}\n"); // reads until newline and omits it
    if command == "quit" {
      break;
    }
    println!("You entered {}.", command);
  }
}
```

## Iteration (Looping)

Rust supports the following looping expressions:

| Name        | Description                                                               |
| ----------- | ------------------------------------------------------------------------- |
| `loop`      | infinite loop that can be exited with a `break`                           |
| `while`     | top-tested loop that repeats as long as an expression evaluates to `true` |
| `while let` | like `while`, but repeats as long as a pattern match succeeds             |
| `for`       | for looping over an iterator                                              |

For an example using `loop`, see the [Standard IO](#standard-io) section.

TODO: Do something with the following example code.

Here's an example of using a `while` loop:

```rust
let numbers = [1, 7, 5, 2, 9, 6];
let mut i = 0;
while numbers[i] % 2 == 1 {
    println!("{} is odd", numbers[i]);
    i += 1;
}
```

A `while let` loop is useful when iterating over
repeated calls to a function that might fail.
The example below uses the `futures` crate which requires
adding the dependency `futures = "0.3.8"` to `Cargo.toml`.
For example:

```rust
use futures::executor::block_on;
use rand::Rng;

// Pretend this function makes a REST call that could possibly fail.
async fn get_data() -> Result<i8, &'static str> {
  let mut rng = rand::thread_rng();
  let n = rng.gen_range(1, 11); // number from 1 to 10
  println!("get_data: n = {}", n);
  if n <= 7 { Ok(n) } else { Err("failed") }
}

fn main() {
    // Here is an approach for processing the result of a single call.
    let result = block_on(get_data());
    match result {
        Ok(n) => println!("in single call, n = {}", n),
        Err(msg) => println!("get_data error: {}", msg)
    }

    // Here is an approach for processing calls in a loop
    // that continues until an Err is returned.
    while let Ok(n) = block_on(get_data()) {
        println!("in while let, n = {}", n);
    }
}
```

A `for` loop is used to iterate over any kind of iterator.
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

    // Iterating over the items in a tuple is not supported,
    // but we can iterate over the items in an array.
    let num_arr = [1, 7, 5, 2, 9, 6];
    for n in num_arr.iter() {
        println!("loop 3: n = {:?}", n);
    }

    // The iter_mut method allows items to be mutated during iteration.
    let mut mut_num_arr = [1, 7, 5, 2, 9, 6];
    // Double all the numbers during iteration.
    for n in mut_num_arr.iter_mut() {
        *n *= 2;
    }
    for n in mut_num_arr.iter() {
        println!("loop 4: {:?}", n);
    }

    // Another approach is to create a new array of modified values
    // using the array map method that is considered experimental
    // and only available in nightly builds as of 12/13/20.
    //let new_numbers = numbers.map(|n| n * 2);

    // We can call the map method on an iterator
    // to create a new iterator over doubled numbers.
    let new_iter = mut_num_arr.iter().map(|n| n * 2);
    for n in new_iter {
        println!("loop 5: {:?}", n);
    }

    // We can iterate over the items in a vector.
    let num_vec = vec![1, 7, 5, 2, 9, 6];
    for n in num_vec.iter() {
        println!("loop 3: n = {:?}", n);
    }
}
```

Let's look at one more iteration example that requires specifying lifetimes.
The function `longest` is passed a reference to an array of strings.
There are three lifetimes to consider, that of the array,
that of the elements inside it, and that of the return value.
Rust wants to know that the array elements
will live as long as the return value
since one of them will be returned.
We must specify that with lifetime annotations (`'a` below).

```rust
fn longest<'a>(strings: &[&'a str]) -> &'a str {
    strings
        .iter()
        .fold("", |acc, s| if s.len() > acc.len() { s } else { acc })
}

let fruits = ["apple", "banana", "cherry", "date"];
let result = longest(&fruits);
println!("longest is {}", result);
```

## Functions

Functions are defined using the `fn` keyword,
followed by a name, parameter list, return type, and body.
Functions that do not return anything omit the return type rather than
specify a type like `void` as is done in some other languages.

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

TODO: Try to write a generic version of the average function
TODO: that works on any numeric type. But see
TODO: https://users.rust-lang.org/t/passing-generic-vector-of-numbers/52486/7.

Functions are accessible by default within the same source file,
but they are private by default when defined in a different source file.
For functions that should be visible outside the source file that defines them,
add the `pub` keyword before the `fn` keyword.

Named functions in Rust are not closures.
They do not capture variables from their surrounding environment.
However, anonymous functions assigned to variables are closures.
For example:

```rust
fn main() {
    let mut a = "";
    let mut inner = | | {
        a = "test"; // a is visible since we are in a closure
    };
    inner();
    println!("{}", a);
}
```

Rust does not support writing functions that
accept a variable number of arguments (variadic).
They can instead be passed in an array.
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

## Ranges

The `std::ops` namespace defines the range types

| Range Type         | Meaning                          |
| ------------------ | -------------------------------- |
| `Range`            | start inclusive to end exclusive |
| `RangeFrom`        | start inclusive and above        |
| `RangeFull`        | zero and above                   |
| `RangeInclusive`   | start inclusive to end inclusive |
| `RangeTo`          | zero to end exclusive            |
| `RangeToInclusive` | start inclusive to end inclusive |

These are distinct types and there is not a
provided range type that encompasses all of them.

Objects of these types are regular values that
can be assigned to variables, be members of structs,
be passed to functions, and be returned from functions.

## Structs

A struct defines a type that is a set of related fields and methods,
similar to a class in other languages.
The `struct` keyword only defines fields.
The `impl` keyword adds instance and static methods to a struct.
Struct names are used to create instances.
For example:

```rust
fn main() {
    struct Point2D {
        x: f64,
        y: f64, // comma after last field is optional
    }

    impl Point2D {
        // Instance method
        fn distance_to(self: &Self, other: &Self) -> f64 {
            Self::distance_between(self, other)
        }

        // Static method
        fn distance_between(pt1: &Self, pt2: &Self) -> f64 {
            let dx = pt1.x - pt2.x;
            let dy = pt1.y - pt2.y;
            (dx.powf(2.0) + dy.powf(2.0)).sqrt()
        }
    }

    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };
    let d1 = p1.distance_to(&p2);
    println!("d1 = {}", d1);
    let d2 = Point2D::distance_between(&p1, &p2);
    println!("d2 = {}", d2);
}
```

To allow structs to be printed for debugging purposes,
add the following above their definition:
`#[derive(Debug)]`.
Then print using the `:?` (single line) or `:#?` (multi-line) format specifier.
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
is the proceed a struct definition with the following:

```rust
#[derive(Clone, Copy, Debug)]
```

A `struct` can be empty, containing no fields.
This is useful for implementing groups of functionality
that do not require fields.

Structs and their fields are accessible by default within the same source file,
but they are private by default when defined in a different source file.
For structs that should be visible outside the source file that defines them,
add the `pub` keyword to both the `struct` and the fields to be exposed.

A `struct` can include the fields of another `struct` of the same type
using the `..` syntax.
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

A "tuple struct" gives a name to a tuple.
For example:

```rust
#[derive(Debug)]
struct RGB(u8, u8, u8);

const CORNFLOWER_BLUE: RGB = RGB(100, 149, 237);
const REBECCA_PURPLE: RGB = RGB(0x66, 0x33, 0x99);
println!("{:?}", CORNFLOWER_BLUE); // RGB(100, 149, 237)
println!("{:?}", REBECCA_PURPLE); // RGB(102, 51, 153)
```

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
by any structs that implement them. For example:

```rust
pub trait HockeyPlayer: Athlete + Person {
    // Describe functions unique to hockey players here.
}
```

Now any `struct` that implements `HockeyPlayer`
must also implement `Athlete` and `Person`.

The [Attributes](#attributes) section describes the built-in traits
that can be derived (automatically implemented).
Additional built-in traits that must be manually implemented
are described in the following table.

| Trait Name     | Description                                                                                         |
| -------------- | --------------------------------------------------------------------------------------------------- |
| `AsRef`        |                                                                                                     |
| `Borrow`       |                                                                                                     |
| `Display`      | adds a `fmt` method that formats a value for output<br>to be seen by a user rather than a developer |
| `Deref`        |                                                                                                     |
| `DerefMut`     |                                                                                                     |
| `Drop`         |                                                                                                     |
| `From`         |                                                                                                     |
| `FromStr`      |                                                                                                     |
| `Into`         |                                                                                                     |
| `Iterator`     |                                                                                                     |
| `IntoIterator` |                                                                                                     |
| `Read`         |                                                                                                     |
| `Send`         |                                                                                                     |
| `Sized`        |                                                                                                     |
| `Sync`         |                                                                                                     |
| `ToString`     | adds a `to_string` method                                                                           |
| `Write`        |                                                                                                     |

TODO: Finish adding descriptions in this table.

## Futures

TODO: Add this section.

## Modules

A module defines a collection of values like constants, functions, and structs.

A module can be defined in many places:

1. inside a source file that uses it
1. in a file whose name is the module name
1. in multiple files within a directory whose name is the module name
1. in the Rust standard library
1. in a dependency declared in the `Cargo.toml` file

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
        // Instance method
        pub fn distance_to(self: &Self, other: &Self) -> f64 {
            Self::distance_between(self, other)
        }

        // Static method
        pub fn distance_between(pt1: &Self, pt2: &Self) -> f64 {
            let dx = pt1.x - pt2.x;
            let dy = pt1.y - pt2.y;
            (dx.powf(2.0) + dy.powf(2.0)).sqrt()
        }
    }
}

fn main() {
    use points::Point2D;
    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };
    let d1 = p1.distance_to(&p2);
    println!("d1 = {}", d1);
    let d2 = Point2D::distance_between(&p1, &p2);
    println!("d2 = {}", d2);
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
    // Instance method
    pub fn distance_to(self: &Self, other: &Self) -> f64 {
        Self::distance_between(self, other)
    }

    // Static method
    pub fn distance_between(pt1: &Self, pt2: &Self) -> f64 {
        let dx = pt1.x - pt2.x;
        let dy = pt1.y - pt2.y;
        (dx.powf(2.0) + dy.powf(2.0)).sqrt()
    }
}
```

In addition to its use in defining a module inside a source file,
the `mod` keyword is used to gain access to modules defined outside.
The `use` statement binds a full path to a new name for easier access.
For example, `use A::B::C` enables using `C` with just that name
instead of its fully qualified name.

The file `src/main.rs` below uses the `points` module defined above.

```rust
mod points;
use points::Point2D;

fn main() {
    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };
    let d1 = p1.distance_to(&p2);
    println!("d1 = {}", d1);
    let d2 = Point2D::distance_between(&p1, &p2);
    println!("d2 = {}", d2);
}
```

This approach works well for small modules.
For large modules it is sometimes desirable to
split their definition across multiple source files.
Each `.rs` defines a module and
placing them in directories creates sub-modules.
A `.rs` file can use the `mod` and `use` statements
to gain access to the functionality in multiple other modules
and re-export the functionality as its own.

The old way of doing this was to
create a directory with the name of the module,
place the files that define the module functionality inside it,
and create the file `mod.rs` inside the directory
that imports all functionality to be exposed from those files
and re-exports it.

The new way is similar, but
a `.rs` file with the name of the module is created instead of `mod.rs`
and this is placed in the same directory as the module directory.

Here is the previous code using this approach.
We'll add a function to the module that is
defined in a different source file than
the one that defines the `Point2D` struct.

The file `src/points/types.rs` can be
identical to the file `src/points.rs` above.
It defines the `Point2D` struct fields and methods.

The file `src/points/functions.rs` defines the function `distance`
which returns the distance between two `Point2D` objects.
Note that this is a plain function, not an instance or static method.

```rust
// The super keyword enables finding a module
// (types in this case) in the same directory.
use super::types::Point2D;

pub fn distance(pt1: &Point2D, pt2: &Point2D) -> f64 {
    let dx = pt1.x - pt2.x;
    let dy = pt1.y - pt2.y;
    (dx.powf(2.0) + dy.powf(2.0)).sqrt()
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
// Note how a single "use" statement can simplify
// access to multiple values from a module.
use points::{distance, Point2D};

fn main() {
    let p1 = Point2D { x: 3.0, y: 4.0 };
    let p2 = Point2D { x: 6.0, y: 8.0 };
    let d1 = p1.distance_to(&p2);
    println!("d1 = {}", d1);
    let d2 = Point2D::distance_between(&p1, &p2);
    println!("d2 = {}", d2);
    let d3 = distance(&p1, &p2);
    println!("d3 = {}", d3);
}
```

Modules can be nested to further segregate the defined names.

## Crates

A crate is a collection of modules.

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

```

```
