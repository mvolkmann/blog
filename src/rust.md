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
- memory-efficient.
- rich type system.
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

## Hello World

The following is a Rust Hello World program:

```rust
fn main() {
    println!("Hello World!");
}
```

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

## WebAssembly

To compile a `.rs` file to WebAssembly:

1. Enter `cargo install wasm-pack` (one time only).

1. Create a new Rust library (called a "crate")
   by entering `cargo new --lib my-crate`

1. `cd my-crate`

1. Edit `src/lib.rs` and add code there.
   For example:

   ```rust
   use wasm_bindgen::prelude::*;

   #[wasm_bindgen]
   extern {
       pub fn alert(s: &str);
   }

   #[wasm_bindgen]
   pub fn greet(name: &str) {
       alert(&format!("Hello, {}!", name));
   }
   ```

1. Edit `Cargo.toml` which is similar to a Node.js `package.json` file.
   For example:

   ```toml
   [package]
   name = "my-crate"
   version = "0.1.0"
   authors = ["R. Mark Volkmann <r.mark.volkmann@gmail.com>"]
   description = "A sample project with wasm-pack"
   license = "MIT/Apache-2.0"
   repository = "https://github.com/mvolkmann/my-crate"
   edition = "2018"

   [lib]
   crate-type = ["cdylib"]

   [dependencies]
   wasm-bindgen = "0.2"
   ```

1. Enter `wasm-pack build`  
   This creates a `pkg` directory that contains all the files needed
   to run the code from JavaScript.
