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

## Built-in Scalar Types

Rust defines four scalar (primitive) types which are
boolean, character, integer, and floating-point.

The boolean type name is `bool`.
Its only values are `true` and `false`.

The character type name is `char`.
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
Vectors are collections of any kind of value.
Hash maps hold key/value pairs where the keys and values can be any type.

## Operators

Rust supports common operators including:

- arithmetic: `+`, `-`, `\*`, `/`, `%` (mod)

## Variables

Variables are immutable by default.
For variables that hold non-primitive values,
even their fields cannot be mutated.

The `mut` keyword marks a variable as mutable.

A variable declaration has the syntax `let name: type = value;`
where the value is optional.
However, a value must be assigned before the variable is referenced.
The colon and the type can be omitted if it can be inferred from the value.

## Conditional Logic

## Iteration

## Custom Types

## Traits

## Hello World

The following is a Rust Hello World program:

```rust
fn main() {
    println!("Hello World!");
}
````

## Compiling Code

To compile a `.rs` file, enter `rustc {name}.rs`.
This creates an executable with the same name and no file extension.
For example `rustc hello.rs` creates `hello`.
To run the executable, enter `./` followed by the name.
For example, `./hello`.

## Formatting Code

The most popular code formatting tool for Rust is
{% aTargetBlank "", "rustfmt" %}.
To install this, enter `cargo install rustfmt`.
TODO: Is this installed by default by rustup?

To run it on all `.rs` files in the current directory,
enter `rustfmt *.rs`.

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
