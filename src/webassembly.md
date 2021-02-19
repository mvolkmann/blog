---
eleventyNavigation:
  key: WebAssembly
layout: topic-layout.njk
---

## Overview

WebAssembly (abbreviated WASM) is a binary instruction format
for a stack-based virtual machine.

WASM code can be run in modern web browsers including
Chrome, Edge, Firefox, and Safari (not Internet Explorer).
It can also be run outside of web browsers using
WASM3, Wasmtime, or WAMR.

There are two primary reasons to run WASM code from a web browser.
The first is that it typically executes much faster than
equivalent code written in JavaScript.
The second is that it enables writing code in any language
that can be compiled to WASM as an alternative to JavaScript.

The primary reason to run WASM code outside a web browser
is that it enables targeting any platform that supports WASM.
This is similar to the rationale for using Java,
whose virtual machine is supported by many platforms.

WASM can also be compiled to native executables
that run on x86 and ARM processors.

## Choice of Programming Language

Code from many programming languages can be compiled to WASM.
Currently full support is only available for C, C++, and Rust.
Experimental support is available for C#, Go, Java, Kotlin, Python,
Swift, TypeScript, and a few other less commonly used languages.

In order to run WASM code in a web browser,
the runtime of the source language must be included.
Rust is a great choice for targeting WASM because it has a very small runtime
compared to options like Python, so it downloads faster.
One reason Rust is able to ship a small runtime is that
it does not need to include garbage collection code.

A reason to prefer Rust over languages like C and C++
is that the Rust compiler makes certain kinds of error impossible,
such as those related to memory management and access.

Tools for compiling Rust code to WebAssembly include
{% aTargetBlank "https://rustwasm.github.io/wasm-pack/", "wasm-pack" %} and
{% aTargetBlank "https://www.secondstate.io/articles/ssvmup/", "ssvmup" %}
The ssvmup tool was inspired by wasm-pack and has explicit support for Deno.
Also see the support for Rust in the
{% aTargetBlank "https://parceljs.org/rust.html", "Parcel bundler" %}!

From {% aTargetBlank
"https://rustwasm.github.io/book/game-of-life/implementing.html",
"Implementing Conway's Game of Life" %},
"As a general rule of thumb, a good JavaScript/WebAssembly interface design
is often one where large, long-lived data structures are implemented as
Rust types that live in the WebAssembly linear memory,
and are exposed to JavaScript as opaque handles.
JavaScript calls exported WebAssembly functions that take these opaque handles,
transform their data, perform heavy computations, query the data,
and ultimately return a small, copy-able result.
By only returning the small result of the computation,
we avoid copying and/or serializing everything back and forth between
the JavaScript garbage-collected heap and the WebAssembly linear memory."

## VS Code

VS Code has several extensions for working with WASM code.
The most popular is "WebAssembly" with the description
"WebAssembly Toolkit for VSCode".

## Text Format

WASM has a text format that enables directly writing code in `.wat` files
as opposed to writing in the another language such as Rust
and compiling to WASM.
It has a syntax that uses "S-expressions", similar to LISP.
This represents code as a tree of nodes.

Each expression is enclosed in parentheses.
The first value in each expression indicates the node type.
The remaining values are attributes or child nodes.

Every `.wat` file contains a single, top-level S-expression
that defines a module.
Modules define sets of functions.
Function definitions have the syntax
`(func {signature} {locals} {body})`.
The signature defines the function name,
its parameter types, and its return type.
Locals defines local variable names and types.
Body is a list of instructions that implement the function.

Currently only for types are supported.
These match number types from Rust, including `i32`, `i64`, `f32`, and `f64`.
Other types such as strings and structs currently must be
serialized into these number types and deserialized from them.
Tools such as wasm_bindgen for Rust can do this for you.

Parameters and local variables are access by their position in the signature
(zero-based index), not by name.
The `local.get {index}` instruction gets a value and places it on the stack.
To change the value of a local variable, use the instruction
`(local.set {index} {value})`.
Function calls get their arguments from the stack.
When a function exists, its return value is the top value on the stack.

In functions that do not return a value, the return type is omitted.

Here is an example that defines a function that
takes two numbers and returns their sum.

1. Create the file `add.wat` containing the following:

```text
(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add)
  (export "add" (func $add))
)
```

To compile this code ...

1. Install WebAssembly Binary Toolkit (WABT).
   This includes a set of command line tools including
   wat2wasm, wasm2wat, wasm-validate, and wasm-interp.
   In macOS this can be installed using Homebrew
   by entering `brew install wabt`.

1. Enter `wat2wasm` to create the binary file `add.wasm`.

1. Create the JavaScript file `index.js` that loads the `add.wasm`
   and calls the function it defines:

```js
WebAssembly.instantiateStreaming(fetch('add.wasm')).then(m => {
  const sum = m.instance.exports.add(1, 2);
  console.log('sum =', sum);
  document.getElementById('result').textContent = sum;
});
```

1. Create the HTML file `index.html` that includes `index.js`:

```html
<!DOCTYPE html>
<html>
  <head>
    <script src="index.js"></script>
  </head>
  <body>
    <div>sum = <span id="result"></span></div>
  </body>
</html>
```

1. Start a local HTTP file server.

1. Browse localhost:????.

To call this code from JavaScript running in a web browser ...

## WASM Instructions

The table below summarizes the supported WASM instructions.
It uses the following abbreviations for substitutions in instruction names:

- `mm` and `nn` can be `32` or `64`
- `sx` can be `u` or `s`

### Numeric Instructions

| Name       | Description           |
| ---------- | --------------------- |
| `abs`      | absolute value        |
| `add`      | add                   |
| `and`      | and                   |
| `ceil`     | ceiling               |
| `clz`      | ?                     |
| `copysign` | copy sign             |
| `ctz`      | ?                     |
| `div_{sx}` | divide                |
| `div`      | divide                |
| `eq`       | equal                 |
| `eqz`      | ?                     |
| `floor`    | floor                 |
| `ge_{sx}`  | greater than or equal |
| `ge`       | greater than or equal |
| `gt_{sx}`  | greater than          |
| `le_{sx}`  | less than or equal    |
| `le`       | less than or equal    |
| `lt_{sx}`  | less than             |
| `max`      | maximum               |
| `min`      | minimum               |
| `mul`      | multiply              |
| `ne`       | not equal             |
| `nearest`  | round?                |
| `neg`      | negate                |
| `or`       | or                    |
| `popcnt`   | ?                     |
| `rem_{sx}` | remainder             |
| `rotl`     | rotate left           |
| `rotr`     | rotate right          |
| `shl`      | shift left            |
| `shr_{xx}` | shift right           |
| `sqrt`     | square root           |
| `sub`      | subtract              |
| `trunc`    | truncate              |
| `xor`      | exclusive or          |

### ? Instructions

| Name          | Description |
| ------------- | ----------- |
| `convert`     | ?           |
| `demote`      | ?           |
| `extend`      | ?           |
| `promote`     | ?           |
| `reinterpret` | ?           |
| `trunc_sat`   | ?           |
| `trunc`       | truncate    |
| `wrap`        | ?           |

### Parametric Instructions

| Name     | Description |
| -------- | ----------- |
| `drop`   | ?           |
| `select` | ?           |

## Variable Instructions

| Name                    | Description                    |
| ----------------------- | ------------------------------ |
| `local.get {local-id}`  | get local variable onto stack  |
| `local.set {local-id}`  | set local variable from stack  |
| `local.tee {local-id}`  | ?                              |
| `global.get {local-id}` | get global variable onto stack |
| `global.set {local-id}` | set global variable from stack |

## Memory Instructions

| Name                      | Description |
| ------------------------- | ----------- |
| `i{nn}.load {mem}`        |             |
| `i{nn}.load8_{sx} {mem}`  |             |
| `i{nn}.load16_{sx} {mem}` |             |
| `i64.load32_{sx} {mem}`   |             |
| `i{nn}.store {mem}`       |             |
| `i{nn}.store8 {mem}`      |             |
| `i{nn}.store16 {mem}`     |             |
| `i64.store32 {mem}`       |             |
| `f{nn}.load {mem}`        |             |
| `f{nn}.store {mem}`       |             |
| `memory.grow`             |             |
| `memory.size`             |             |

## Control Instructions

| Name                                         | Description                                                                    |
| -------------------------------------------- | ------------------------------------------------------------------------------ |
| `nop`                                        | no operation                                                                   |
| `unreachable`                                |                                                                                |
| `block {block-type} {instr}* end`            | code block                                                                     |
| `loop {block-type} {instr}* end`             | loop                                                                           |
| `if {block-type} {instr}* else {instr}* end` | conditional                                                                    |
| `br {label-id}`                              |                                                                                |
| `br_if {label-id}`                           |                                                                                |
| `br_table vec({label-id}) {label-id}`        |                                                                                |
| `return`                                     | return from function                                                           |
| `call {function-id}`                         | call function                                                                  |
| `call_indirect {type-id}`                    |                                                                                |
| `end`                                        | ends a function body, global initialization, element segment, or data segement |

## wasm-pack

To install wasm-pack in Linux or macOS, enter the following:

```bash
cargo install wasm-pack
```

To create a project that uses wasm-pack::

1. `wasm-pack new my-wasm`

   TODO: Describe what this puts in Cargo.toml.

1. `cd my-wasm`

1. `wasm-pack build --target web`

   TODO: Describe the files this produces.

1. Create the following `index.html` file:

   ```html
   <html>
     <head>
       <meta charset="utf-8" />
       <title>WASM Demo</title>
     </head>
     <body>
       <script type="module">
         import {default as wasm, greet} from './pkg/my_wasm.js';
         wasm().then(module => {
           greet();
         });
       </script>
     </body>
   </html>
   ```

1. Start a local HTTP file server.
   There are many ways to do this, including using Deno.
   TODO: Is there any reason to use wasm-server instead because it supports the WASM MIME type?
   TODO: Maybe this is no longer an issue.
   To run a simple Deno HTTP file server:

   1. Install {% aTargetBlank "https://deno.land/#installation", "Deno" %}.
   1. Enter `deno install --allow-net --allow-read https://deno.land/std@0.83.0/http/file_server.ts`
   1. Enter `file_server .`
   1. Browse `localhost:4507`

## ssvmup

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
