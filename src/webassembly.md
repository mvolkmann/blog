---
eleventyNavigation:
  key: WebAssembly
layout: topic-layout.njk
---

## Overview

WebAssembly (abbreviated WASM) is a binary instruction format
for a stack-based virtual machine.
Other popular stack-based virtual machines include
the Java Virtual Machine (JVM) and the .NET Common Language Runtime (CLR).

WASM code can be run in modern web browsers including
Chrome, Edge, Firefox, and Safari (not Internet Explorer).
It can also be run outside of web browsers using tools such as
{% aTargetBlank "https://github.com/wasm3/wasm3", "WASM3" %},
{% aTargetBlank "https://wasmtime.dev", "Wasmtime" %},
{% aTargetBlank "https://github.com/bytecodealliance/wasm-micro-runtime",
"WebAssembly Micro Runtime (WAMR)" %}.

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

## VS Code

VS Code has several extensions for working with WASM code.
The most popular is "WebAssembly" with the description
"WebAssembly Toolkit for VSCode".

## Only Numbers

Currently WASM only supports the number types `i32`, `i64`, `f32`, and `f64`.
But the {% aTargetBlank "https://github.com/WebAssembly/interface-types",
"Interface Types proposal" %} seeks to change this.
This can be accomplished by enabling supported programming languages
to serialize non-numeric values to linear memory as an array of i32 values.
Other supported languages can then deserialize values from the array.
This enables each language to use its own representation of the data types.

WASM doesn't assume number values are signed.
However, specific instructions performed on them do.
For example, the instruction to add two i64 signed values is `i64.add`
and the for unsigned values is `i64.add_u`.

## Implementing Directly

While code from many programming languages can be compiled to WASM,
it is also possible to directly implement the code.

WASM has a binary format and a text (intermediate form) format.
Files in the binary format have the extension `.wasm`.
Files in the text format have the extension `.wat`.

The text format has two styles, linear and S-expressions.
The linear format places instructions on separate lines.
The S-expression format uses parentheses, similar to LISP,
representing a tree of nodes.
The first value in each expression indicates the node type.
The remaining values are attributes or child nodes.

A `.wat` file can be compile to a `.wasm` file using the `wat2wasm` tool.
A `.wasm` file can be de-compiled to a `.wat` file using the `wasm2wat` tool.
Note that this outputs the linear style.
Also see `.wast` files that are for writing tests.

The `wasm-nm` tool outputs the symbols that are
export from and imported into a `.wasm` file.
To install this tool, enter `cargo install wasm-nm`.
To run it, enter `wasm-nm {file-path}.wasm`.
The names of exported symbols are preceded by "e " and
the names of imported symbols are preceded by "i ".

Every `.wat` file contains a single, top-level S-expression
that defines a module.
Modules can define functions that are callable from JavaScript.
These definitions have the syntax `(func {signature} {locals} {body})`.
The signature defines the function name,
its parameter types, and its return type.
In functions that do not return a value, the return type is omitted.
Locals defines local variable names and their types.
The body is a list of instructions that implement the function.

Currently only four types are supported, `i32`, `i64`, `f32`, and `f64`.
These match number types from Rust.
Other types such as strings and structs currently must be
serialized into these number types and deserialized from them.
Tools such as wasm_bindgen for Rust generate code that does this for you.

Parameters and local variables are accessed by their
position in the signature using zero-based indexes.
The text format also allows functions, parameters, and local variables
to have names that start with `$`.
These can be referenced by name,
but the names are compiled away in favor of indexes.

Operations get their arguments from the top values on the stack.
The `local.get {index | name}` instruction
gets the value of a parameter of local variable and places it on the stack.
The `local.set {index | name} {value}` instruction
sets value of a local variable.
The `{type}.const {value}` instruction pushes a constant value on the stack.
When a function exists, its return value is the top value on the stack.

Single line comments begin with `;;` and extend to the end of the line.
Multi-line comments begin with `(;` and end with `;)`.
This makes it easy to comment out an S-expression
because `;` characters just need to be added
inside the opening and closing parentheses.
It also means that a winking smiley face is the closing delimiter!

Here are examples of functions.
The first takes two numbers and returns their sum.
The rest compute the distance between two points
using the formula `sqrt(dx**2 + dy**2)`.
Three versions are presented to show different approaches.
This code is available in the GitHub repo
{% aTargetBlank "https://github.com/mvolkmann/wasm-demo/", "wasm-demo" %}.

1. Create the file `math.wat` containing the following:

```wasm
(module
  (func $sum (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add
  )
  (export "sum" (func $sum))

  ;; This uses the "linear" format.
  (func $distance
    (param $x1 f64)
    (param $y1 f64)
    (param $x2 f64)
    (param $y2 f64)
    (result f64)

    get_local $x1
    get_local $x2
    f64.sub
    tee_local $x1 ;; reusing $x1 to hold temporary dx value
    get_local $x1
    f64.mul

    get_local $y1
    get_local $y2
    f64.sub
    tee_local $y1 ;; reusing $y1 to hold temporary dy value
    get_local $y1
    f64.mul

    f64.add
    f64.sqrt
  )
  (export "distance" (func $distance))

  ;; This uses S-expressions.
  (func $distance2
    (param $x1 f64)
    (param $y1 f64)
    (param $x2 f64)
    (param $y2 f64)
    (result f64)

    (local $dx f64)
    (local $dy f64)

    (set_local $dx
      (f64.sub
        (local.get $x1)
        (local.get $x2)
      )
    )
    (set_local $dy
      (f64.sub
        (local.get $y1)
        (local.get $y2)
      )
    )

    (f64.sqrt
      (f64.add
        (f64.mul
          ;; There is no instruction to duplicate the value at
          ;; the top of the stack, so we have to do this twice.
          ;; See https://github.com/WebAssembly/design/issues/1365.
          (get_local $dx)
          (get_local $dx)
        )
        (f64.mul
          (get_local $dy)
          (get_local $dy)
        )
      )
    )
  )
  (export "distance2" (func $distance2))

  ;; This uses even more S-expressions.
  (func $distance3
    (param $x1 f64)
    (param $y1 f64)
    (param $x2 f64)
    (param $y2 f64)
    (result f64)

    (f64.sqrt
      (f64.add
        (f64.mul
          (tee_local $x1 ;; reusing $x1 to hold temporary dx value
            (f64.sub
              (get_local $x1)
              (get_local $x2)
            )
          )
          (get_local $x1)
        )
        (f64.mul
          (tee_local $y1 ;; reusing $y1 to hold temporary dy value
            (f64.sub
              (get_local $y1)
              (get_local $y2)
            )
          )
          (get_local $y1)
        )
      )
    )
  )
  (export "distance3" (func $distance3))
)
```

1. Install the {% aTargetBlank "https://github.com/WebAssembly/wabt",
   "WebAssembly Binary Toolkit (WABT)" %}.
   This includes a set of command line tools including
   wat2wasm, wasm2wat, wasm-validate, and wasm-interp.
   In macOS this can be installed using Homebrew
   by entering `brew install wabt`.

1. Enter `wat2wasm math.mat` to create the binary file `math.wasm`.

1. Create the JavaScript file `index.js` that loads the `add.wasm`
   and calls the function it defines:

   ```js
   WebAssembly.instantiateStreaming(fetch('math.wasm')).then(m => {
     const {distance, distance2, distance3, sum} = m.instance.exports;
     document.getElementById('sum').textContent = sum(19, 3);
     document.getElementById('distance').textContent = distance(2, 3, 5, 7);
     console.log('distance1 =', distance(2, 3, 5, 7));
     console.log('distance2 =', distance2(2, 3, 5, 7));
     console.log('distance3 =', distance3(2, 3, 5, 7));
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
       <div>sum = <span id="sum"></span></div>
       <div>distance = <span id="distance"></span></div>
     </body>
   </html>
   ```

1. Start a local HTTP file server.
   One approach is to install [Deno](https://deno.land/)
   and then enter these commands:

   ```bash
   deno install --allow-net --allow-read https://deno.land/std@0.87.0/http/file_server.ts
   file_server .
   ```

1. Browse localhost:{port} where port is
   the port on which the local server is listening.

1. Open the DevTools console to see the `console.log` output.

## WASM Instructions

The tables below summarize the currently supported WASM instructions.
Understanding these is only necessary when
directly writing WASM code in text format or
to understand what compilers for higher level languages like Rust generate.

For more detail, see the {% aTargetBlank
"https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md",
"WASM Reference Manual" %}.

The tables use the following abbreviations
for substitutions in instruction names:

- `mm` and `nn` can be `32` bits or `64` bits
- `sx` can be `u` (unsigned) or `s` (signed)

As mentioned earlier, there is no instruction for
duplicating the top value on the stack.
Adding this has been proposed.
Thomas Lively provided rationale on why this has not been done.

"It takes a surprising amount of work and time to spec new instructions and
get them implemented in every tool and engine out there.
So generally only changes with significant benefits
get all the way through the process.
Unfortunately that means that there are a lot of "nice to have" proposals,
even tiny ones like adding a single dup instruction, that don't make the cut.
I'm not saying we'll never add dup, but if we do it will because it solves
an important problem so lots of folks agree it's important to add
and will be motivated to implement and maintain it throughout the ecosystem.

This is one of the costs of standards-based work.
If WASM were controlled by a single party,
it would be easy to add a single instruction like dup.
Since it's not, you first have to get a lot of different people with
different priorities and opinions to agree that adding dup
is both a good idea and worth their time and effort.
Because of this extra consensus-building work,
the community can have more confidence in the robustness and benefits
of the proposals that do make it through the process."

### Numeric Instructions

These instructions are prefixed by one of the four supported number types.
For example, the instruction to add two `f32` values is `f32.add`.

| Name       | Description                     |
| ---------- | ------------------------------- |
| `abs`      | absolute value                  |
| `add`      | add                             |
| `ceil`     | ceiling                         |
| `copysign` | copy sign                       |
| `div_{sx}` | integer divide                  |
| `div`      | floating point divide           |
| `floor`    | floor                           |
| `max`      | maximum                         |
| `min`      | minimum                         |
| `mul`      | multiply                        |
| `ne`       | not equal                       |
| `nearest`  | round floating point to integer |
| `neg`      | negate                          |
| `rem_{sx}` | remainder                       |
| `sqrt`     | square root                     |
| `sub`      | subtract                        |
| `trunc`    | truncate                        |

### Bitwise Instructions

| Name       | Description                    |
| ---------- | ------------------------------ |
| `clz`      | count leading zeros            |
| `ctz`      | count training zeros           |
| `popcnt`   | population count (# of 1 bits) |
| `rotl`     | rotate left                    |
| `rotr`     | rotate right                   |
| `shl`      | shift left                     |
| `shr_{xx}` | shift right                    |

### Logical Instructions

| Name  | Description  |
| ----- | ------------ |
| `and` | and          |
| `or`  | or           |
| `xor` | exclusive or |

### Comparison Instructions

| Name      | Description                          |
| --------- | ------------------------------------ |
| `eq`      | equal                                |
| `eqz`     | equal to zero                        |
| `ge_{sx}` | integer greater than or equal        |
| `ge`      | floating point greater than or equal |
| `gt_{sx}` | integer greater than                 |
| `gt`      | floating point greater than          |
| `le_{sx}` | integer less than or equal           |
| `le`      | floating point less than or equal    |
| `lt_{sx}` | integer less than                    |
| `lt`      | floating point less than             |

### Conversion Instructions

| Name          | Description                                                         |
| ------------- | ------------------------------------------------------------------- |
| `convert`     | convert integer to floating point                                   |
| `demote`      | convert f64 to f32                                                  |
| `extend`      | convert i32 to i64                                                  |
| `promote`     | convert f32 to f64                                                  |
| `reinterpret` | convert from integer to floating point or floating point to integer |
| `trunc`       | truncate, discarding the least significant bits                     |
| `wrap`        | converts i32 to i64, discarding the most significant bits           |

## Variable Instructions

| Name                    | Description                                      |
| ----------------------- | ------------------------------------------------ |
| `local.get {local-id}`  | push local variable onto stack                   |
| `local.set {local-id}`  | set local variable from stack and pop            |
| `local.tee {local-id}`  | set local variable from stack and leave on stack |
| `global.get {local-id}` | push global variable onto stack                  |
| `global.set {local-id}` | set global variable from stack and pop           |

## Control Instructions

These instructions are expressions, not statements.
They result in placing a value on the stack.

| Name                               | Description                                                                    |
| ---------------------------------- | ------------------------------------------------------------------------------ |
| `block {block-type} {instr}*`      | creates a block of instructions, typically in another instruction such as `if` |
| `loop {block-type} {instr}* end`   | creates a labeled block for implementing a loop                                |
| `if`                               | denotes the true block of a conditional                                        |
| `else`                             | denotes the false block of a conditional                                       |
| `end`                              | marks the end of a block for `block`, `if`, `else`, `loop`, or `function`      |
| `br {depth}`                       | unconditional branch; `br 0` goes to top of loop; `br 1` exits loop            |
| `br_if {depth} {condition}`        | conditional branch                                                             |
| `br_table {table} {default-depth}` | branch based on table entry at depth                                           |
| `return`                           | return from function                                                           |
| `call {function-id}`               | call function                                                                  |
| `call_indirect {type-id}`          | call function at index in table                                                |
| `unreachable`                      | signals an error (trap) if reached                                             |

Even control flow operates on the stack.
For example, the `if` instruction executes its branch
if the value at the top of the stack evaluates to true.

Here is an example of using an `if` instruction
in a function that returns the largest of two values:

```wasm
  (func $max (param $lhs i32) (param $rhs i32) (result i32)
    ;; The first argument specifies the type if expression result.
    '' The second argument is the result of the condition to be tested.
    (if (result i32) (i32.gt_s (local.get $lhs) (local.get $rhs))
      (then (local.get $lhs))
      (else (local.get $rhs))
    )
  )
  (export "max" (func $max))
```

TODO: Demonstrate all the control instructions in a `.wat` file!
TODO: See https://medium.com/leaningtech/solving-the-structured-control-flow-problem-once-and-for-all-5123117b1ee2.
TODO: Maybe implement the Fibonacci function in multiple ways.

### Basic Instructions

| Name            | Description                                                            |
| --------------- | ---------------------------------------------------------------------- |
| `call`          | calls a function                                                       |
| `call_indirect` | calls a function at an index in the default table                      |
| `const`         | pushes a constant value onto the stack                                 |
| `drop`          | pops top value from stack and does nothing with it                     |
| `nop`           | no operation                                                           |
| `select`        | takes two values and a condition; returns 1st if true and 2nd if false |

## Memory Instructions

The `load` instructions load data from the default linear memory.
The `store` instructions store data into the default linear memory.
These instructions are prefixed by the number type to be loaded or stored.
In the table below, `mem` is a memory offset.

| Name                      | Description                                    |
| ------------------------- | ---------------------------------------------- |
| `i{nn}.load {mem}`        | reads integer value into matching size         |
| `i{nn}.load8_{sx} {mem}`  | reads integer value into 8 bits                |
| `i{nn}.load16_{sx} {mem}` | reads integer value into 16 bits               |
| `i64.load32_{sx} {mem}`   | reads i64 value into 32 bits                   |
| `f{nn}.load {mem}`        | reads floating point value                     |
| `i{nn}.store {mem}`       | writes integer value into matching size        |
| `i{nn}.store8 {mem}`      | writes integer value into 8 bits               |
| `i{nn}.store16 {mem}`     | writes integer value into 16 bits              |
| `i64.store32 {mem}`       | writes i64 value into 32 bits                  |
| `f{nn}.store {mem}`       | writes floating point value into matching size |
| `memory.grow`             | increases size of default linear memory        |
| `memory.size`             | returns the size of default linear memory      |

## Higher Level Languages

Code from many programming languages can be compiled to WASM.
Currently full support is only available for C, C++, and Rust.
Experimental support is available for C#, Go, Java, Kotlin, Python,
Swift, TypeScript, and a few other less commonly used languages.

In order to run WASM code that was compiled from another language,
the runtime of the language must be included.
Rust is a great choice for targeting WASM because it has a very small runtime
compared to options like Python, so it loads faster.
One reason Rust is able to ship a small runtime is that
it does not need to include code to perform garbage collection.

A reason to prefer Rust over languages like C and C++
is that the Rust compiler makes certain kinds of error impossible,
such as those related to memory management and access.

Tools for compiling Rust code to WebAssembly include
{% aTargetBlank "https://rustwasm.github.io/wasm-pack/", "wasm-pack" %} and
{% aTargetBlank "https://www.secondstate.io/articles/ssvmup/", "ssvmup" %}
The ssvmup tool was inspired by wasm-pack and has explicit support for Deno.
Also see the support for Rust in the
{% aTargetBlank "https://parceljs.org/rust.html", "Parcel bundler" %}!

## Rust With Numbers

Rust functions that only use numbers
can be compiled to WASM and called from JavaScript
without using tools like wasm-pack or wasm-bindgen.

Let's implement the same `sum` and `distance` functions
we saw earlier, but do so using Rust instead of WAT.
Where are the steps assuming Rust has already been installed.

1. `cargo new --lib rust-math`

1. Edit `Cargo.toml` and add the following:

   ```toml
   [lib]
   crate-type = ['cdylib']
   ```

1. Edit `src/lib.rs` and change the contents to the following:

   ```rust
   #[no_mangle]
   pub fn sum(n1: f64, n2: f64) -> f64 {
       n1 + n2
   }

   #[no_mangle]
   pub fn distance(x1: f64, y1: f64, x2: f64, y2: f64) -> f64 {
       ((x1 - x2).powi(2) + (y1 - y2).powi(2)).sqrt()
   }
   ```

1. To generate a `.wasm` file from the Rust code,
   enter `cargo build --target wasm32-unknown-unknown`

1. To install the `wasm-nm` tool for
   listing the symbols exported by a `.wasm` file,
   enter `cargo install wasm-nm`
   The symbols `sum` and `distance` will be output with an "e"
   in front of them for "export".

1. To see the exported symbols,
   enter `wasm-nm target/wasm32-unknown-unknown/debug/rust_math.wasm`

1. Modify the `index.js` file created earlier to pass
   the file path of this `.wasm` file to the fetch function which is
   `rust-math/target/wasm32-unknown-unknown/debug/rust_math.wasm`.

1. Start a local HTTP file server like before.

1. Browse localhost:{port} where port is
   the port on which the local server is listening.

1. Open the DevTools console to see the `console.log` output.
   There be an error message saying that `distance2` is not a function
   which is expected because the Rust code
   only defines the `sum` and `distance` functions.

## Calling JavaScript functions from Rust

WASM code written in a language other than JavaScript can call custom JavaScript functions.
Let's look at an example.

1. Add the following at the beginning of `index.js`:

   ```js
   function cube(n) {
     return n ** 3;
   }

   function square(n) {
     return n \* n;
   }

   const importObject = {env: {cube, square}};
   ```

1. Pass `importObject` as the second argument to
   `WebAssembly.instantiateStreaming` as follows:

   ```js
   WebAssembly.instantiateStreaming(fetch(wasmPath), importObject).then(m => {
   ```

1. Define the function signatures in `rust_math/src/lib.rs` as follows:

   ```rust
   extern "C" {
     fn cube(n: f64) -> f64;
     fn square(n: f64) -> f64;
   }
   ```

1. Call these functions like normal Rust functions,
   but call them inside an `unsafe` block since
   the Rust compiler cannot guarantee that they are safe.
   For example:

   ```rust
   #[no_mangle]
   pub fn sum_of_square_and_cube(n: f64) -> f64 {
       let result;
       unsafe {
           result = square(n) + cube(n);
       }
       result
   }
   ```

1. Rebuild the `.wasm` file by entering
   `cargo build --target wasm32-unknown-unknown`

1. Verify the symbols that are imported by entering
   `wasm-nm target/wasm32-unknown-unknown/debug/rust_math.wasm`
   The symbols `cube` and `square` will be output with an "i"
   in front of them for "import".

1. Start a local HTTP file server like before.

1. Browse localhost:{port} where port is
   the port on which the local server is listening.

1. Open the DevTools console to see the `console.log` output.

## Rust With More Types

The wasm-bindgen tool makes it possible to compile Rust functions
that use non-numeric types to WASM.
It also enables calling built-in JavaScript functions
such as `alert` and `console.log`.
wasm-bindgen provides a Rust library and a CLI tool.
The Rust library provides macros that generate the Rust code
required to serialize and deserialize Rust data types.
The CLI tool generates JavaScript code that wraps the WASM code as an ES module
which makes it easier to consume in a web app.

Let's implement an example where non-numeric types are
passed from JavaScript to Rust functions and non-numeric types are returned.

The wasm-pack CLI tool makes using wasm-bindgen easier,
so we will also use that. This tool:

- It executes a Cargo command to compile Rust code to WASM.
- It calls wasm-bindgen to generate an ES module
  that wraps usage of the WASM code.
- It can invoke the wasm-opt tool to optimize the WASM code.
- It can generate a `package.json` file needed to
  deploy the ES module and WASM code as an npm package.
- It generates TypeScript type definitions
  for the exported functions and types in a `.d.ts` file
  that can be used by calling TypeScript code to provide type checking.

1. Install wasm-pack by entering the following command:

   ```bash
   curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
   ```

1. Enter `cargo new --lib wasm-bindgen-demo`

1. Enter `cd wasm-bindgen-demo`.

1. Edit `Cargo.toml` and add the following dependency.

   ```toml
   [lib]
   crate-type = ['cdylib']

   [dependencies]
   wasm-bindgen = "0.2.70"
   ```

1. Edit `src/lib.rs` and change the contents to the following:

   ```rust
   use wasm_bindgen::prelude::*;

   #[wasm_bindgen]
   extern "C" {
       fn alert(s: &str);

       #[wasm_bindgen(js_namespace = console)]
       pub fn log(s: &str);
   }

   // This makes functions define in JavaScript available in Rust.
   #[wasm_bindgen(raw_module = "/index.js")]
   extern "C" {
       fn cube(n: f64) -> f64;
       fn square(n: f64) -> f64;
   }


   #[wasm_bindgen]
   pub fn greet(name: &str) {
       log(&format!("Hello, {}!", name));
   }

   #[wasm_bindgen]
   #[derive(Debug)]
   pub struct Color {
       pub red: u8,
       pub green: u8,
       pub blue: u8,
   }

   #[wasm_bindgen(js_name = getColor)]
   pub fn get_color() -> Color {
       let color = Color {
           red: 1,
           green: 2,
           blue: 3,
       };
       log(&format!("color = {:?}", color));
       color
   }

   #[wasm_bindgen(js_name = getPowers)]
   pub fn get_powers(n: u32) -> Vec<u32> {
       alert(&format!("Getting powers of {} ...", n));
       vec![n, n.pow(2), n.pow(3)]
   }


   // This Rust function calls custom JavaScript functions.
   #[wasm_bindgen(js_name = sumOfSquareAndCube)]
   pub fn sum_of_square_and_cube(n: f64) -> f64 {
       square(n) * cube(n)
   }
   ```

1. Enter `wasm-pack build --dev --target web`

1. Create `index.js` with the following content:

   ```js
   import init, {
     Color,
     getColor,
     getPowers,
     greet
   } from './pkg/wasm_bindgen_demo.js';

   export function square(n) {
     return n * n;
   }

   export function cube(n) {
     return n ** 3;
   }

   async function run() {
     await init();
     greet('World');

     const color = getColor();
     console.log('color =', color);
     console.log('color instanceof Color?', color instanceof Color); // true
     console.log('color.red =', color.red); // 1
     console.log('color.green =', color.green); // 2
     console.log('color.blue =', color.blue); // 3

     const powers = getPowers(3); // a UIntArray
     console.log('powers =', powers); // [3, 9, 27]

     console.log('square + cube =', sumOfSquareAndCube(2)); // 12
   }

   run();
   ```

1. Create `index.html` with the following content:

   ```html
   <!DOCTYPE html>
   <html>
     <head>
       <script type="module" src="index.js"></script>
     </head>
     <body>
       <div>See the console.</div>
     </body>
   </html>
   ```

1. Start a local HTTP file server like before.

1. Browse localhost:{port} where port is
   the port on which the local server is listening.

1. Open the DevTools console to see the `console.log` output.

TODO: How can Rust call custom JavaScript functions when using wasm-bindgen
TODO: instead of `WebAssembly.instantiateStreaming`?
TODO: See https://rustwasm.github.io/docs/wasm-bindgen/examples/import-js.html!

## Linear Memory

TODO: Resume here

"Linear memory" can be used to share data across programming languages
without the overhead of copying values.
Linear memory is also used by libraries such as wasm-bindgen
to enable passing non-numeric values to functions
and returning non-numeric values from them.

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

To use this approach, WASM code allocates space and provides functions that
return a pointer to the space and the size of the space.
Languages that wish to use the space call these functions
and map their own data to it.
For example, JavaScript code can create a typed array such as `Float64Array`
that uses the same space.
It can then set and get elements in the array to write and read
the linear memory that is available in the WASM code.
Note that it is not possible to allocate space outside the WASM code
and get WASM code to share it.

Let's walk through an example that demonstrates this.
The code is available in the GitHub repo {% aTargetBlank
"https://github.com/mvolkmann/wasm-rust-linear-memory",
"wasm-rust-linear-memory" %}.
TODO: Why did you need to use wasm-bindgen in this example
TODO: since it only uses numbers?

## Updating DOM from Rust

See https://github.com/mvolkmann/wasm-bind-demo/blob/main/src/lib.rs
which uses the web-sys crate.

## AssemblyScript

{% aTargetBlank "https://www.assemblyscript.org", "AssemblyScript" %}
is a programming language designed to compile to WASM.

AssemblyScript is a variant of TypeScript.
Its source files use the `.ts` file extension.
Semicolons at the ends of statements are optional.

AssemblyScript includes
"a relatively small memory management and garbage collection runtime."

The only supported types are:

- boolean `bool`
- signed integer types `i8`, `i16`, `i32` and `i64`
- unsigned integer types `u8`, `u16`, `u32` and `u64`
- floating point types `f32` and `f64`.
- platform-specific integers `isize` and `usize`
- 128-bit vector `v128`
- opaque host reference `anyref`
- `void` for functions with no return value (cannot omit return type)
- `Array`
- `ArrayBuffer`
- `DataView`
- `Map`
- `Math`
- `Set`
- `string`?
- `String`
- typed arrays `Int{size}Array`, `UInt{size}Array`, and `Float{size}Array`

Macro types

- `indexof<T>`
- `native<T>`
- `returnof<T>`
- `valueof<T>`

Supported math instructions are described {% aTargetBlank
"https://www.assemblyscript.org/stdlib/math.html", "here" %}.

To install the AssemblyScript compiler, install Node.js
and enter `npm install -g assemblyscript`.

To compile an AssemblyScript source file to a `.wat` file:

```bash
asc {file-path}.ts -t {file-path}.wat
```

To compile an AssemblyScript source file to a `.wasm` file:

```bash
asc {file-path}.ts -b {file-path}.wasm -O3
```

Here are the steps to implement a `distance` function in AssemblyScript
that computes the distance between two points and call it from JavaScript:

1. Create the file `math.ts` containing the following:

   ```ts
   export function distance(x1: f64, y1: f64, x2: f64, y2: f64): f64 {
     return Math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2);
   }
   ```

1. Compile this to WASM by entering `asc math.ts -b math.wasm -O3`

1. Create the file `index.js` containing the following:

   ```js
   WebAssembly.instantiateStreaming(fetch('math.wasm')).then(m => {
     const {distance} = m.instance.exports;
     document.getElementById('result').textContent = distance(2, 3, 5, 7);
   });
   ```

1. Create the file `index.html` containing the following:

   ```html
   <!DOCTYPE html>
   <html>
     <head>
       <script src="index.js"></script>
     </head>
     <body>
       <div>result = <span id="result"></span></div>
     </body>
   </html>
   ```

1. Start a local HTTP file server like before.

1. Browse localhost:{port} where port is
   the port on which the local server is listening.

## WebAssembly System Interface (WASI)

The {% aTargetBlank "https://wasi.dev", "WebAssembly System Interface (WASI)" %}
defines a way to communicate with the system
that focuses on portability and security.
This includes things like stdout/stdin, the file system, and network resources.

Adding these features makes WASM useful outside of web browsers.
Motivations for using WASM in this way include performance, safety, and the
ability to combine code compiled from many higher-level programming languages.

WASM code using WASI is portable across operating systems.

Implementations of WASI include
{% aTargetBlank "https://wasmtime.dev", "Wasmtime" %} (developed at Mozila) and
{% aTargetBlank "https://bytecodealliance.github.io/lucet/", "Lucet" %}
(developed at Fastly).
There is a {% aTargetBlank
"https://github.com/bytecodealliance/lucet/issues/607", "plan" %}
to merge Lucet with Wasmtime.

Rust code compiled to WASM can use WASI features
because WASI capabilities are included in Rust standard libraries.
For example, the `println!` macro can be used.
To compile a Rust project to WASM with WASI support,
enter `cargo build --target wasm32-unknown-wasi`.
TODO: What are the options for the three parts of the target string?

C/C++ code compiled to WASM can use WASI features because it will use
wasi-sysroot which is a wasi-core implementation of the libc library.

WASM code using WASI can also be run in web browsers using polyfills.
For example, such a polyfill would turn the WASI version of a Rust `println!`
into a call to `console.log`.

Security in WASI is implemented with sandboxing, similar to Deno.
Perhaps Deno modeled their security after WASI.
When running WASM code that uses WASI features,
file system and network resources to be accessed must be specified.
Access to unspecified resources are not permitted.
This is referred to as "Capability Space Security".

Platform-specific versions of tools like Wasmtime
can execute platform-independent WASM code that uses WASI features.
They handle translation into platform-specific calls.

WASI functions are made available to WASM binaries
through imports that are passed in.
This allows passing in platform-specific versions of these functions.
It also supports limiting what the WASM code can do
by not passing in every function.

Security is also controlled at the module level
by passing allowed functions and file descriptors between them.

Remaining work on WASI includes defining support for
asynchronous I/O, file watching, and file locking.

## Running WASM Outside Browsers

There are currently three tools for running WASM code outside a web browser.
Each is described below.

### {% aTargetBlank "https://github.com/wasm3/wasm3", "WASM3" %}

To install this in macOS, install Homebrew and enter `brew install wasm3`.
Installing for other platforms is more complicated.
For details, visit the WASM3 site linked above.

To call functions defined in a `.wasm` file from a REPL,
enter `wasm3 --repl {path-to-wasm-file}`.
Then enter function names followed by arguments.

To call functions directly, not using a REPL,
enter `wasm3 --func {function-name} {path-to-wasm-file} {arguments}`.

TODO: I can't get either of these approaches to work on a `.wasm` file
TODO: I created from a Rust programing using
TODO: `rustc {path-to-rust-file} --target wasm32-wasi`!

### {% aTargetBlank "https://wasmtime.dev", "Wasmtime" %}

To install this in Linux or macOS:

1. Enter `curl https://wasmtime.dev/install.sh -sSf | bash`
1. Open a new terminal that will have `wasmtime` in `PATH`.

Visit the Wasmtime site linked above for instructions to install in Windows.

One way to demonstrate running this is to compile Rust code to
{% aTargetBlank "https://wasi.dev", "WebAssembly System Interface (WASI)" %}.
To do so, enter `rustc {path-to-rust-file} --target wasm32-wasi`.
This produces a `.wasm` file.
The Rust code can use features such as the `println!` macro to produce output.
For example:

```rust
fn main() {
    println!("Hello, World!");
}
```

To execute a `.wasm` file, enter `wasmtime {path-to-wasm-file}`.

To execute a `.wast` test file, enter `wasmtime wast {path-to-wast-file}`.

Unlike wasm3, wasmtime does not provide a REPL or
support running a specific function from the command-line.

### {% aTargetBlank "https://github.com/bytecodealliance/wasm-micro-runtime", "WebAssembly Micro Runtime (WAMR)" %}

Instructions for installing this tool on various platforms can be found at
{% aTargetBlank
  "https://github.com/bytecodealliance/wasm-micro-runtime/blob/main/doc/build_wamr.md",
  "build_wamr.md" %}.

To install this in macOS:

- Browse {% aTargetBlank
  "https://github.com/bytecodealliance/wasm-micro-runtime",
  "wasm-micro-runtime" %}.
- Click the "Code" button and "Download ZIP".
- Unzip the downloaded file.
- cd into its directory and into `product-mini/platforms/darwin`.
- Install the `cmake` command with `brew install cmake`.
- Enter `mkdir build`
- Enter `cd build`
- Enter `cmake ..`
- Enter `make` to create the executable `iwasm` in the current directory.
- Copy `iwasm` to a directory listed in the `PATH` environment variable.

To run a `.wasm` file, enter `.iwasm {path-to-wasm-file}`

## Demos

- <https://github.com/mvolkmann/wasm-bind-demo>
- <https://github.com/mvolkmann/wasm-demo>
- <https://github.com/mvolkmann/wasm-rust-linear-memory>

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
   # cdylib exports a C-style interface for a Rust dynamic library.
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

TODO: Invent a programming language that translated to WAT
TODO: more directly than a language like Rust.
