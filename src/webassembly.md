---
eleventyNavigation:
  key: WebAssembly
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Overview

WebAssembly (abbreviated WASM) is a binary instruction format
for a stack-based virtual machine.
Other popular stack-based virtual machines include
the Java Virtual Machine (JVM) and the .NET Common Language Runtime (CLR).

WASM code can be run in modern web browsers including
Chrome (including Android), Edge, Firefox (including Android),
Safari (including iOS), Opera, and Android Browser, but not Internet Explorer.
It can also be run outside of web browsers using tools such as
{% aTargetBlank "https://wasmtime.dev", "Wasmtime" %},
{% aTargetBlank "https://github.com/wasm3/wasm3", "WASM3" %},
{% aTargetBlank "https://github.com/bytecodealliance/wasm-micro-runtime",
"WebAssembly Micro Runtime (WAMR)" %}.

There are two primary reasons to run WASM code in a web browser.
The first is that it typically executes much faster than
equivalent code written in JavaScript.
The second is that it enables writing some of the code in any language
that can be compiled to WASM as an alternative to JavaScript.

There are also two primary reasons to run WASM code outside a web browser.
The first is that it enables targeting any platform that supports WASM.
This is similar to the rationale for using Java,
whose virtual machine is supported by many platforms.
The second is that it is secure by default.
WASM code does not have access to the environment,
the file system, or network resources.
The only way it can access those things is if the code
that invokes it passes in functions that have those capabilities.
This is referred to as capability-based security".

where
access to resources such as the file system and network are restricted.
Actually, WASM itself has no access to these and only gains it through the
{% aTargetBlank "https://wasi.dev", "WebAssembly System Interface (WASI)" %}.

WASM code can also be compiled to native executables
that run on x86 and ARM processors.

## VS Code

VS Code has several extensions for working with WASM code.
The most popular is "WebAssembly" with the description
"WebAssembly Toolkit for VSCode".

## Only Numbers

Currently WASM only supports the four data types `i32`, `i64`, `f32`, and `f64`.
These match number types from Rust.
Other types such as strings and structs must be
serialized into these number types and deserialized from them
using linear memory.
Tools such as
{% aTargetBlank "https://github.com/rustwasm/wasm-bindgen", "wasm_bindgen" %}
for Rust and
{% aTargetBlank "https://emscripten.org", "Emscripten" %} for C/C++
generate code that does this.

The {% aTargetBlank "https://github.com/WebAssembly/interface-types",
"Interface Types proposal" %} seeks to change this. It "adds a new set
of interface types to WebAssembly that describe high-level values".
These can be implemented using linear memory
and the standard WASM numeric types.
Added types include additional integer types, characters, lists,
records (structs), and variants (enumerated types).
Strings are represented as lists of characters.
Supported programming languages will be able to
serialize and deserialize these additional data types.
Each language will be able to use its own representation of the data types.

WASM doesn't assume that number values are signed.
However, specific instructions performed on them do.
For example, the instruction to add two i64 signed values is `i64.add`
and for unsigned values is `i64.add_u`.

## WASM Text Format

While code from many programming languages can be compiled to WASM,
it is also possible to directly implement the code.

WASM has a binary format and a text (intermediate form) format.
Files in the binary format have the extension `.wasm`.
Details about this format are provided later.
Files in the text format have the extension `.wat`.

The text format has two styles, linear (or plain) and S-expressions (or folded).
The linear format places instructions on separate lines.
The S-expression format uses parentheses, similar to LISP,
representing a tree of nodes.
The first value in each expression indicates the node type.
The remaining values are attributes or child nodes.

Every `.wat` file contains a single, top-level S-expression
that defines a module.
It is not possible to define more than one module in a source file.
The module instruction does not support assigning a name.

Non-WASM runtimes such as web browsers, Rust, Node.js, Deno, and Python
can import multiple WASM modules,
but a WASM module cannot import another WASM module.

Modules can define many kinds of things including:

- imports from other modules
- exports other modules can import
- function type definitions
- function definitions,
- tables to implement function pointers
- linear memory for storing arbitrary data
- data to be placed in linear memory
- global variables available throughout the module

## Tools

The {% aTargetBlank "https://github.com/WebAssembly/wabt",
"WebAssembly Binary Toolkit (WABT)" %}
includes a set of command line tools including
`wat2wasm`, `wasm2wat`, `wasm-validate`, and `wasm-interp`.
In macOS these can be installed by installing
{% aTargetBlank "https://brew.sh", "Homebrew" %}
and entering `brew install wabt`.

The `wat2wasm` tool compiles a `.wat` file to a `.wasm` file.
The `wasm2wat` tool de-compiles a `.wasm` file to a `.wat` file
that uses the linear style.
Also see `.wast` files that are for writing tests.

The `wasm-nm` tool outputs the symbols that are
export from and imported into a `.wasm` file.
To install this tool, enter `cargo install wasm-nm`.
To run it, enter `wasm-nm {file-path}.wasm`.
The names of exported symbols are preceded by "e " and
the names of imported symbols are preceded by "i ".

## Tests

One way to write unit tests for WASM functions
is to use the WABT tools `wast2json` and `spectest-interp`.
For example, the following code defines an `add` function
and unit tests for it.
To run this, enter `wast2json demo.wat && spectest-interp demo.json`.

Here is the contents of `demo.wat` which defines
a function to be tested and its tests.

```wasm
(module
  (func (export "add") (param i32 i32) (result i32)
    (i32.add (local.get 0) (local.get 1))
  )
)

(assert_return (invoke "add" (i32.const 0) (i32.const 0)) (i32.const 0))
(assert_return (invoke "add" (i32.const 0) (i32.const 1)) (i32.const 1))
(assert_return (invoke "add" (i32.const 1) (i32.const 0)) (i32.const 1))
;; This test is expected to fail.
;; It's purpose to show how failures are reported.
(assert_return (invoke "add" (i32.const 3) (i32.const 4)) (i32.const 6))
```

The output is:

```text
demo.wast:12: mismatch in result 0 of assert_return: expected i32:6, got i32:7
3/4 tests passed.
```

## WASM Functions

Modules can define functions.
Functions that are exported can be called from JavaScript.
These definitions have the syntax `(func {signature} {locals} {body})`.
The signature defines the function name,
its parameter types, and its return type.
In functions that do not return a value, the return type is omitted.
Locals defines local variable names and their types.
The body is a list of instructions that implement the function.

Parameters and local variables are accessed by their
position in the signature using zero-based indexes.
The text format also allows functions, parameters, and local variables
to have names that start with `$`.
These can be referenced by name,
but the names are compiled away in favor of indexes.

WASM functions can be named or unnamed.
Any function can be called by its position (zero-based index) within the module.
Named functions call also be called using their name.

The following code is in the file `demo.wat`.

```wasm
(module
  ;; anonymous function at index 0 that just returns 19
  (func (result i32)
    i32.const 19
  )

  ;; named function at index 1 that just returns 21
  (func $second (result i32)
    i32.const 21
  )

  (func (export "callFirst") (result i32)
    call 0
  )

  (func (export "callSecond") (result i32)
    call $second
    ;; same as call 1
  )
)
```

Compile this code to a `.wasm` file by entering `wat2wasm demo.wat`.

The following JavaScript code is in the file `demo.js`.
It instantiates the WASM code above and calls its exported functions:

```js
async function run() {
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'));
  const {callFirst, callSecond} = m.instance.exports;
  console.log('first =', callFirst()); // 19
  console.log('second =', callSecond()); // 21
}

run();
```

Function parameters can also be named or unnamed.
Unnamed functions are referred to by their
position (zero-based index) within the parameter list.
Parameters are declared using the `param` instruction.
Typically each parameter is described separately so each can be given a name and type.
Alternatively all of their types can be described with a single `param` instruction,
but in that case they cannot be given names.
Functions have a fixed number of parameters
and cannot accept a variable number of them.
For example:

```wasm
  ;; Declaring each parameter separately
  (func (export "percent") (param $amount f32) (param $total f32) (result f32)
    local.get $amount
    local.get $total
    f32.div
    f32.const 100.0
    f32.mul
  )

  ;; Using on param instruction
  (func (export "percent2") (param f32 f32) (result f32)
    local.get 0
    local.get 1
    f32.div
    f32.const 100.0
    f32.mul
  )
```

Functions that return a value must specify its type with `(return {type})`.
This is omitted for functions that do not return a value.

Exporting a function makes it available outside its module,
such as in JavaScript.
There are two ways to export a function.
It can be given both a WASM name and an exported name.
This allows it to be called from both WASM code and outside code.
For example:

```wasm
  ;; This function cannot be called by name in this WASM module.
  (func (export "subtract") (param i32 i32) (result i32)
    (i32.sub (local.get 0) (local.get 1))
  )
```

It call also be given only an exported name.
In this case it can still be called from WASM code,
but only by its position within the module.

```wasm
  (func (export "subtract") (param i32 i32) (result i32)
    (i32.sub (local.get 0) (local.get 1))
  )
```

Instructions get their arguments from the top values on the stack.
The `local.get {index | name}` instruction (old name was `get_local`)
gets the value of a parameter of local variable and places it on the stack.
The `local.set {index | name} {value}` instruction (old name was `set_local`)
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

    local.get $x1
    local.get $x2
    f64.sub
    local.tee $x1 ;; reusing $x1 to hold temporary dx value
    local.get $x1
    f64.mul

    local.get $y1
    local.get $y2
    f64.sub
    local.tee $y1 ;; reusing $y1 to hold temporary dy value
    local.get $y1
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

    (local.set $dx
      (f64.sub
        (local.get $x1)
        (local.get $x2)
      )
    )
    (local.set $dy
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
          (local.get $dx)
          (local.get $dx)
        )
        (f64.mul
          (local.get $dy)
          (local.get $dy)
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
          (local.tee $x1 ;; reusing $x1 to hold temporary dx value
            (f64.sub
              (local.get $x1)
              (local.get $x2)
            )
          )
          (local.get $x1)
        )
        (f64.mul
          (local.tee $y1 ;; reusing $y1 to hold temporary dy value
            (f64.sub
              (local.get $y1)
              (local.get $y2)
            )
          )
          (local.get $y1)
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

TODO: Consider deleting this table if it just duplicates what is explained later.

| Operation                                  | Instruction Syntax                                            |
| ------------------------------------------ | ------------------------------------------------------------- |
| define function                            | `func [{name}] {parameters} {return-type} {body}`             |
| define a function parameter                | `param {name} {type}`                                         |
| define a function return type              | `result {type}`                                               |
| export function to make it available in JS | `export {js-name} (func {name})`                              |
| call function                              | `call {fn-name}`                                              |
| declare local variable                     | `local {name} {type}` (cannot initialize)                     |
| set local variable                         | `local.set {name} {value}`                                    |
| get local variable                         | `local.get {name}`                                            |
| declare global variable                    | `global {name} {type} [{value}]` (can initialize)             |
| set global variable                        | `global.set {name} {value}` (must be mutable)                 |
| get global variable                        | `global.get {name}`                                           |
| conditional logic                          | `if (result {type}) {condition} (then {body}) (else {body}))` |
| select value based on condition            | `select {non-zero-value} {zero-value} {condition}`            |
| define a block                             | `block {body}`                                                |
| loop                                       | `loop {body}` (defines a block)                               |
| break out of block                         | `br {depth}`                                                  |
| compare values                             | see "Comparison Instructions" below                           |
| set data in linear memory                  | `{type}.store{bits} {value}` (ex. `i32.store8`)               |
| get data from linear memory                | `{type}.load{bits}` (ex. `i32.load8_u`)                       |
| grow linear memory                         | `memory.grow {pages}`                                         |
| shrink linear memory?                      | not currently supported, but being discussed (1)              |

(1) {% aTargetBlank "https://github.com/WebAssembly/design/issues/1397", "memory management issue" %}

The tables below summarize the currently supported WASM instructions.
Understanding these is only necessary when
directly writing WASM code in text format or
to understand what compilers for higher level languages like Rust generate.

For more detail, see the {% aTargetBlank
"https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md",
"WASM Reference Manual" %}.

The tables below use the following abbreviations
for substitutions in instruction names:

- `mm` and `nn` can be `32` bits or `64` bits
- `sx` can be `u` (unsigned) or `s` (signed)

As mentioned earlier, there is no instruction for
duplicating the top value on the stack.
Adding this has been proposed.
Thomas Lively provided rationale on why this has not been done.

> > > "It takes a surprising amount of work and time to spec new instructions and
> > > get them implemented in every tool and engine out there.
> > > So generally only changes with significant benefits
> > > get all the way through the process.
> > > Unfortunately that means that there are a lot of "nice to have" proposals,
> > > even tiny ones like adding a single dup instruction, that don't make the cut.
> > > I'm not saying we'll never add dup, but if we do it will because it solves
> > > an important problem so lots of folks agree it's important to add
> > > and will be motivated to implement and maintain it throughout the ecosystem.
> > >
> > > This is one of the costs of standards-based work.
> > > If WASM were controlled by a single party,
> > > it would be easy to add a single instruction like dup.
> > > Since it's not, you first have to get a lot of different people with
> > > different priorities and opinions to agree that adding dup
> > > is both a good idea and worth their time and effort.
> > > Because of this extra consensus-building work,
> > > the community can have more confidence in the robustness and benefits
> > > of the proposals that do make it through the process."

Many WASM instruction names follow the format `{kind-of-thing}.{operation}`
where kinds of things include:

- variables: `local` and `global`
- integers: `i32` and `i64`
- floating point numbers: `f32` and `f64`

Some kind-specific operations include:

- `get` and `set` to get and set the value of a variable
- `const` to place the value of a constant on the stack
- `store` and `load` to copy data into memory and retrieve it

WASM instructions get their arguments from
literal values specified after the instruction
(referred to as "static immediate arguments") and/or
from the stack (referred to as "dynamic operands").
Some instructions use only one of these sources,
while others use both.
The columns in the tables below include the following columns
to provide this information for each instruction:

- I: number of immediate arguments
- Si: number of arguments popped from the stack (input)
- So: number of values pushed onto the stack (output)

### Variable Instructions

Global variables are declared at the module level, not inside functions.
They are immutable by default.
To declare a global variable to be mutable, specify its type as `(mut {type})`.

Local variables are declared inside functions
and the declarations must appear at the beginning of the function
before any other instructions.
Local variables cannot be initialized when they are declared,
so they are always mutable.

The instructions in the table below declare, get, and set
local and global variables.

| Name         |  I  | Si  | So  | Description                                              |
| ------------ | :-: | :-: | :-: | -------------------------------------------------------- |
| `local`      |  2  |  0  |  0  | declares a local variable name and type                  |
| `local.get`  |  1  |  0  |  0  | push local variable onto stack                           |
| `local.set`  |  1  |  1  |  0  | set local variable from stack and pop                    |
| `local.tee`  |  1  |  0  |  0  | set local variable from stack and leave on stack         |
| `global`     |  1  |  0  |  0  | declares a global variable name, type, and initial value |
| `global.get` |  1  |  0  |  0  | push global variable onto stack                          |
| `global.set` |  1  |  1  |  0  | set global variable from stack and pop                   |

The two immediate arguments of the `local` instruction are its name and type.
Local variables default to zero and cannot be initialized to a different value.
See the `$l1` example below.

Older code examples use the deprecated names
`get_local`, `set_local`, `tee_local`, `get_global`, and `set_global`.

All of these instructions take an immediate argument
that identifies the variable on which to operate by index or name.
The `set` instructions get the new value from the stack.

The file `demo.wat` below demonstrates of using these instructions.

```wasm
(module
  ;; Import a global variable named gFromJS from JavaScript,
  ;; give it the WASM name $g_from_js,
  ;; and declare it to hold an immutable i32 value.
  (global $g_from_js (import "js" "gFromJS") i32)

  ;; Define a global variable named $g_here that
  ;; holds a mutable i32 value and is initialized to 19.
  (global $g_here (mut i32) (i32.const 19))

  ;; Define a function that has no parameters and returns a f64 value and
  ;; export it with the name "demo" so it can be called from JavaScript.
  (func (export "demo") (result f64)
    ;; Define a local variable named $l1 that holds a f64 value.
    ;; The value cannot be initialized in this instruction.
    (local $l1 f64)

    ;; Set the local variable to 3.14.
    (local.set $l1 (f64.const 3.14))

    ;; Get the value of $l1, placing it on the stack.
    local.get $l1

    ;; Do it again so two copies are on the stack.
    local.get $l1

    ;; Multiply $l1 by itself and place the f64 result on the stack.
    f64.mul

    ;; Add 1 to $g_here so the value becomes 20.
    (global.set $g_here (i32.add (global.get $g_here) (i32.const 1)))

    ;; Get the value of $g_here, placing it on the stack.
    global.get $g_here

    ;; Convert the i32 value to f64.
    f64.convert_i32_s

    ;; Add $g_here to the previous result from f64.mul.
    f64.add

    ;; Get the value of $g_from_js.
    global.get $g_from_js

    ;; Convert the i32 value to f64.
    f64.convert_i32_s

    ;; Add $g_from_js to the previous result and
    ;; use this as the return value of the function.
    ;; The result is 3.14 * 3.14 + (19 + 1) + 20 = 49.8596.
    f64.add
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {
    js: {
      gFromJS: new WebAssembly.Global({value: 'i32'}, 20)
    }
  };

  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {demo} = m.instance.exports;
  console.log('result =', demo());
}

run();
```

### Numeric Instructions

These instructions are prefixed by one of the four supported number types.
For example, the instruction to add two `f32` values is `f32.add`.

| Name       |  I  | Si  | So  | Description                     |
| ---------- | :-: | :-: | :-: | ------------------------------- |
| `abs`      |  0  |  1  |  1  | absolute value                  |
| `add`      |  0  |  2  |  1  | add                             |
| `ceil`     |  0  |  1  |  1  | ceiling                         |
| `copysign` |  0  |  1  |  1  | copy sign                       |
| `div_{sx}` |  0  |  2  |  1  | integer divide                  |
| `div`      |  0  |  2  |  1  | floating point divide           |
| `floor`    |  0  |  1  |  1  | floor                           |
| `max`      |  0  |  2  |  1  | maximum                         |
| `min`      |  0  |  2  |  1  | minimum                         |
| `mul`      |  0  |  2  |  1  | multiply                        |
| `ne`       |  0  |  1  |  1  | not equal                       |
| `nearest`  |  0  |  1  |  1  | round floating point to integer |
| `neg`      |  0  |  1  |  1  | negate                          |
| `rem_{sx}` |  0  |  2  |  1  | remainder                       |
| `sqrt`     |  0  |  1  |  1  | square root                     |
| `sub`      |  0  |  2  |  1  | subtract                        |
| `trunc`    |  0  |  1  |  1  | truncate                        |

We saw examples of using the `add` and `mul` instructions
in the "Variable Instructions" section.

All of these instructions require a type prefix.
For example, `f64.max` is used in the file `demo.wat` below
which demonstrates using some of these instructions.

```wasm
(module
  (func (export "max") (param f64 f64) (result f64)
    (f64.max (local.get 0) (local.get 1))
  )

  (func (export "min") (param f64 f64) (result f64)
    (f64.min (local.get 0) (local.get 1))
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {};
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {max, min} = m.instance.exports;

  const pi = Math.PI; // 3.14159
  const e = Math.E; // 2.71828

  console.log(max(pi, e)); // pi
  console.log(max(e, pi)); // pi
  console.log(min(pi, e)); // e
  console.log(min(e, pi)); // e
}

run();
```

Many math functions, such as `sin`, `cos`, `tan`, and `log` are missing in WASM.
One way to get these is to import them from JavaScript as shown below.

The file `demo.js` below provides math functions to `demo.wasm`.

```js
async function run() {
  const imports = {
    js: {
      sin: Math.sin,
      cos: Math.cos,
      tan: Math.tan,
      log: Math.log
    }
  };
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {demo} = m.instance.exports;
  console.log('result =', demo(0.7)); // 0.811529
}

run();
```

The file `demo.wat` below demonstrates using functions imported from JavaScript.
Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

```wasm
(module
  ;; This shows describing a function signature inline.
  ;;(import "js" "sin" (func $sin (param f64) (result f64)))

  ;; This shows assigning a name to a function signature
  ;; and reusing it.
  (type $math_fn (func (param f64) (result f64) ))
  (import "js" "sin" (func $sin (type $math_fn)))
  (import "js" "cos" (func $cos (type $math_fn)))
  (import "js" "tan" (func $tan (type $math_fn)))
  (import "js" "log" (func $log (type $math_fn)))

  ;; This returns log(sin($radians) + cos($radians) + tan($radians)).
  (func (export "demo") (param $radians f64) (result f64)
    (call $sin (local.get $radians))
    (call $cos (local.get $radians))
    f64.add
    (call $tan (local.get $radians))
    f64.add
    call $log
  )
)
```

### Bitwise Instructions

| Name       |  I  | Si  | So  | Description                    |
| ---------- | :-: | :-: | :-: | ------------------------------ |
| `clz`      |  0  |  1  |  0  | count leading zeros            |
| `ctz`      |  0  |  1  |  0  | count trailing zeros           |
| `popcnt`   |  0  |  1  |  0  | population count (# of 1 bits) |
| `rotl`     |  0  |  2  |  0  | rotate left                    |
| `rotr`     |  0  |  2  |  0  | rotate right                   |
| `shl`      |  0  |  2  |  0  | shift left                     |
| `shr_{sx}` |  0  |  2  |  0  | shift right                    |

All of these instructions require a type prefix.
For example, `i32.clz` is used in the file `demo.wat` below
which demonstrates using some of these instructions.

```wasm
(module
  (func (export "leadingZeros") (param $value i32) (result i32)
    (i32.clz (local.get $value))
  )

  (func (export "trailingZeros") (param $value i32) (result i32)
    (i32.ctz (local.get $value))
  )

  (func (export "population") (param $value i32) (result i32)
    (i32.popcnt (local.get $value))
  )

  (func (export "rotateLeft") (param $value i32) (param $bits i32) (result i32)
    (i32.rotl (local.get $value) (local.get $bits))
  )

  (func (export "rotateRight") (param $value i32) (param $bits i32) (result i32)
    (i32.rotr (local.get $value) (local.get $bits))
  )

  (func (export "shiftLeft") (param $value i32) (param $bits i32) (result i32)
    (i32.shl (local.get $value) (local.get $bits))
  )

  (func (export "shiftRight") (param $value i32) (param $bits i32) (result i32)
    ;; shr_u is for unsigned.
    (i32.shr_u (local.get $value) (local.get $bits))
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {};
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {
    leadingZeros,
    population,
    rotateLeft,
    rotateRight,
    shiftLeft,
    shiftRight,
    trailingZeros
  } = m.instance.exports;

  // 52 in one byte of binary is 00110100.
  const value = 52;

  // In four bytes there are 3*8 + 2 leading zeros.
  console.log('leading zeros =', leadingZeros(value)); // 26
  console.log('trailing zeros =', trailingZeros(value)); // 2
  console.log('population =', population(value)); // 3 1-bits
  console.log('shiftRight =', shiftRight(value, 1)); // 26
  console.log('shiftLeft =', shiftLeft(value, 1)); // 104

  // Could use shiftRight here instead since no bits are wrapping around.
  console.log('rotateRight =', rotateRight(value, 2)); // 13

  // Could use shiftLeft here instead since no bits are wrapping around.
  console.log('rotateLeft =', rotateLeft(value, 2)); // 208
}

run();
```

### Logical Instructions

| Name  |  I  | Si  | So  | Description  |
| ----- | :-: | :-: | :-: | ------------ |
| `and` |  0  |  2  |  0  | and          |
| `or`  |  0  |  2  |  0  | or           |
| `xor` |  0  |  2  |  0  | exclusive or |

All of these instructions require a type prefix.
For example, `i32.and` is used in the file `demo.wat` below
which demonstrates using some of these instructions.

The file `demo.wat` below demonstrates using each of these instructions.

```wasm
(module
  (func (export "and") (param i32 i32) (result i32)
    (i32.and (local.get 0) (local.get 1))
  )

  (func (export "or") (param i32 i32) (result i32)
    (i32.or (local.get 0) (local.get 1))
  )

  (func (export "xor") (param i32 i32) (result i32)
    (i32.xor (local.get 0) (local.get 1))
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {};
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {and, or, xor} = m.instance.exports;

  // 52 in one byte of binary is 00110100.
  const v1 = 52;

  // 21 in one byte of binary is 00010101.
  const v2 = 21;

  console.log('and =', and(v1, v2)); // 00010100 = 20
  console.log('or =', or(v1, v2)); // 00110101 = 53
  console.log('xor =', xor(v1, v2)); // 00100001 = 33
}

run();
```

### Comparison Instructions

| Name      |  I  | Si  | So  | Description                          |
| --------- | :-: | :-: | :-: | ------------------------------------ |
| `eq`      |  0  |  2  |  0  | equal                                |
| `eqz`     |  0  |  1  |  0  | equal to zero                        |
| `ge_{sx}` |  0  |  2  |  0  | integer greater than or equal        |
| `ge`      |  0  |  2  |  0  | floating point greater than or equal |
| `gt_{sx}` |  0  |  2  |  0  | integer greater than                 |
| `gt`      |  0  |  2  |  0  | floating point greater than          |
| `le_{sx}` |  0  |  2  |  0  | integer less than or equal           |
| `le`      |  0  |  2  |  0  | floating point less than or equal    |
| `lt_{sx}` |  0  |  2  |  0  | integer less than                    |
| `lt`      |  0  |  2  |  0  | floating point less than             |

All of these instructions require a type prefix.
For example, `f64.ge` is used in the file `demo.wat` below
which demonstrates using some of these instructions.
We saw an easier way to implement these functions
in the "Numeric Instructions" section.

```wasm
(module
  (func (export "max") (param f64 f64) (result f64)
    (select (local.get 0) (local.get 1) (f64.ge (local.get 0) (local.get 1)))
  )

  (func (export "min") (param f64 f64) (result f64)
    (select (local.get 0) (local.get 1) (f64.le (local.get 0) (local.get 1)))
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {};
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {max, min} = m.instance.exports;

  const pi = Math.PI; // 3.14159
  const e = Math.E; // 2.71828

  console.log(max(pi, e)); // pi
  console.log(max(e, pi)); // pi
  console.log(min(pi, e)); // e
  console.log(min(e, pi)); // e
}

run();
```

### Conversion Instructions

| Name          |  I  | Si  | So  | Description                                                         |
| ------------- | :-: | :-: | :-: | ------------------------------------------------------------------- |
| `convert`     |  0  |  1  |  0  | convert integer to floating point                                   |
| `demote`      |  0  |  1  |  0  | convert f64 to f32                                                  |
| `extend`      |  0  |  1  |  0  | convert i32 to i64                                                  |
| `promote`     |  0  |  1  |  0  | convert f32 to f64                                                  |
| `reinterpret` |  0  |  1  |  0  | convert from integer to floating point or floating point to integer |
| `trunc`       |  0  |  1  |  0  | truncate, discarding the least significant bits                     |
| `wrap`        |  0  |  1  |  0  | converts i32 to i64, discarding the most significant bits           |

All of these instructions require a type prefix.
We saw examples of using the `f64.convert_i32s` instruction
in the "Variable Instructions" section above.
The type prefix specifies the output type and the
instruction suffix (`_32s` in this case) specifies the input type.

### Control Instructions

These instructions are expressions, not statements.
They result in placing a value on the stack.

| Name                        |  I  | Si  | So  | Description                                                                      |
| --------------------------- | :-: | :-: | :-: | -------------------------------------------------------------------------------- |
| `block [{name}]`            |  0  |  1  |  0  | creates a group of instructions                                                  |
| `loop [{name}]`             |  0  |  1  |  0  | creates a special block for implementing a loop                                  |
| `if {condition}`            |  0  |  1  |  0  | creates a conditional with at `then` part and an optional `else` part            |
| `then`                      |  0  |  0  |  0  | denotes the false block of a conditional                                         |
| `else`                      |  0  |  0  |  0  | denotes the false block of a conditional                                         |
| `end`                       |  0  |  0  |  0  | marks the end of a block for `block`, `if`, `else`, `loop`, or `function`        |
| `br {depth}`                |  1  |  0  |  0  | unconditional branch                                                             |
| `br_if {depth} {condition}` |  1  |  1  |  0  | conditional branch                                                               |
| `br_table {list-of-depths}` | 2+  |  1  |  0  | branch to a depth from a list of them based on the index at the top of the stack |
| `return`                    |  0  |  1  |  0  | return from function, optionally specifying a return value                       |
| `unreachable`               |  0  |  0  |  0  | signals an error (trap) if reached                                               |

Even control flow instructions operate on the stack.
For example, the `if` instruction executes its branch
if the value at the top of the stack evaluates to true.

The `block` instruction creates the equivalent of
an immediately invoked inline function.
It has a result type and a set of instructions.
When in a `block`, branching to depth `0` exits the `block`.

The `loop` instruction creates a different kind of `block`
where branching to depth `0` goes to the beginning of the loop
for another iteration rather than branching out of the block.

The `block` and `loop` instructions can specify a block name
that branch instructions can refer to instead of specifying a depth.

The file `demo.wat` below demonstrates using some of these instructions.

```wasm
(module
  (func $factorial (param $n i32) (result i32)
    (if
      (result i32)
      (i32.le_s (local.get $n) (i32.const 2))
      (then (local.get $n))
      (else
        (i32.mul
          (local.get $n)
          (call $factorial (i32.sub (local.get $n) (i32.const 1))) ;; recursive
        )
      )
    )
  )
  (export "factorial" (func $factorial))

  (func (export "sumRange") (param $start i32) (param $end i32) (result i32)
    (local $sum i32)
    (local $n i32)
    (local.set $n (local.get $start))

    (loop
      ;; Add $n to $sum.
      (local.set $sum (i32.add (local.get $sum) (local.get $n)))

      ;; Add 1 to $n.
      (local.set $n (i32.add (local.get $n) (i32.const 1)))

      ;; Go to top of the loop if $end not reached
      ;; by branching to block level zero.
      ;; Otherwise drop out of loop.
      (br_if 0 (i32.le_s (local.get $n) (local.get $end)))
    )

    (local.get $sum)
  )

  (func (export "blockWithoutResult") (param $n i32) (result i32)
    (local $result i32)

    (block ;; This block has no result.
      (local.set $result (i32.const 1))
      ;; Inside a block, branching to depth zero exits the block.
      (br_if 0 (i32.lt_s (local.get $n) (i32.const 100)))
      (local.set $result (i32.const 2))
      (br_if 0 (i32.lt_s (local.get $n) (i32.const 200)))
      (local.set $result (i32.const 3))
    )

    (local.get $result)
  )

  (func (export "blockWithResult") (param $n i32) (result i32)
    (block (result i32) ;; This block has a result.
      i32.const 1
      ;; Inside a block, branching to depth zero exits the block.
      (br_if 0 (i32.lt_s (local.get $n) (i32.const 100)))

      ;; This is needed so there will only be one value
      ;; on the stack at the end of this function.
      drop
      i32.const 2
      (br_if 0 (i32.lt_s (local.get $n) (i32.const 200)))

      ;; This is needed so there will only be one value
      ;; on the stack at the end of this function.
      drop
      i32.const 3
    )
  )

  (func (export "usingReturn") (param $n i32) (result i32)
    (if (i32.lt_s (local.get $n) (i32.const 100))
      (then (return (i32.const 1)))
    )
    (if (i32.lt_s (local.get $n) (i32.const 200))
      (then (return (i32.const 2)))
    )
    (i32.const 3)
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {};
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {
    blockWithoutResult,
    blockWithResult,
    factorial,
    returnDemo,
    sumRange,
    usingReturn
  } = m.instance.exports;

  console.log(factorial(3)); // 1 * 2 * 3 = 6
  console.log(factorial(5)); // 1 * 2 * 3 * 4 * 5 = 120

  console.log(sumRange(3, 6)); // 3 + 4 + 5 + 6 = 18

  console.log(blockWithoutResult(19)); // 1
  console.log(blockWithoutResult(142)); // 2
  console.log(blockWithoutResult(728)); // 3

  console.log(blockWithResult(19)); // 1
  console.log(blockWithResult(142)); // 2
  console.log(blockWithResult(728)); // 3

  console.log(usingReturn(19)); // 1
  console.log(usingReturn(142)); // 2
  console.log(usingReturn(728)); // 3
}

run();
```

### Memory Instructions

WASM memory is a contiguous array of bytes.
When it is allocated, it is given an initial size in pages (64 KB each)
and optionally a maximum size which cannot exceed 4 GB.
The allocated size can be increased later using the `memory.grow` instruction
up to the maximum size.

Each WASM module can have only one array of linear memory.
But JavaScript can instantiate more than one WASM module
in order to access multiple instances of linear memory.

The `load` instructions load data from the default linear memory.
The `store` instructions store data into the default linear memory.
These instructions are prefixed by the number type to be loaded or stored.
In the table below, `mem` is a memory offset.

| Name                                   |  I  | Si  | So  | Description                                                        |
| -------------------------------------- | :-: | :-: | :-: | ------------------------------------------------------------------ | ----------------------- |
| `i{nn}.load {mem}`                     |  0  |  1  |  1  | reads integer value into matching size                             |
| `i{nn}.load8_{sx} {mem}`               |  0  |  1  |  1  | reads integer value into 8 bits                                    |
| `i{nn}.load16_{sx} {mem}`              |  0  |  1  |  1  | reads integer value into 16 bits                                   |
| `i64.load32_{sx} {mem}`                |  0  |  1  |  1  | reads i64 value into 32 bits                                       |
| `f{nn}.load {mem}`                     |  0  |  1  |  1  | reads floating point value                                         |
| `i{nn}.store {mem}`                    |  0  |  2  |  0  | writes integer value into matching size                            |
| `i{nn}.store8 {mem}`                   |  0  |  2  |  0  | writes integer value into 8 bits                                   |
| `i{nn}.store16 {mem}`                  |  0  |  2  |  0  | writes integer value into 16 bits                                  |
| `i64.store32 {mem}`                    |  0  |  2  |  0  | writes i64 value into 32 bits                                      |
| `f{nn}.store {mem}`                    |  0  |  2  |  0  | writes floating point value into matching size                     |
| `memory {initial-pages} [{max-pages}]` |  1  |  2  |  0  | 0                                                                  | allocates linear memory |
| `memory.grow`                          |  1  |  0  |  1  | increases size of linear memory in pages and returns previous size |
| `memory.size`                          |  0  |  0  |  1  | returns the size of default linear memory                          |

The file `demo.wat` below demonstrates using some of these instructions.

```wasm
(module
  (memory (export "myMemory") 1) ;; initial size 1 page; maximum not specified

  (func $translate (param $offset i32) (param $delta f64)
    (f64.store
      (local.get $offset)
      (f64.add
        (f64.load (local.get $offset))
        (local.get $delta)
      )
    )
  )

  (func (export "translatePoints") (param $length i32) (param $dx f64) (param $dy f64)
    (local $offset i32) ;; starts at zero

    (local $lastOffset i32)
    (local.set $lastOffset
      (i32.mul
        (local.get $length) ;; number of points
        (i32.const 16) ;; 8 bytes for x + 8 bytes for y
      )
    )

    (loop
      (call $translate (local.get $offset) (local.get $dx))

      ;; Advance $offset to get next y value.
      (local.set $offset (i32.add (local.get $offset) (i32.const 8)))

      ;; Translate y value by $dy.
      (f64.store
        (local.get $offset)
        (f64.add
          (f64.load (local.get $offset))
          (local.get $dy)
        )
      )

      ;; Advance $offset to get next x value.
      (local.set $offset (i32.add (local.get $offset) (i32.const 8)))

      (br_if 0 (i32.lt_s (local.get $offset) (local.get $lastOffset)))
    )
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {};
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {myMemory, translatePoints} = m.instance.exports;

  const points = [
    {x: 1.2, y: 2.3},
    {x: 3.4, y: 4.5},
    {x: 5.6, y: 6.7}
  ];

  // Copy the point data into linear memory shared with WASM code.
  const offset = 0;
  const length = points.length * 2;
  const array = new Float64Array(myMemory.buffer, offset, length);
  let index = 0;
  for (const point of points) {
    array[index++] = point.x;
    array[index++] = point.y;
  }

  console.log('untranslated points =', array);
  translatePoints(points.length, 2, 3);
  console.log('translated points =', array);
}

run();
```

### Other Instructions

| Name            |  I  | Si  | So  | Description                                                            |
| --------------- | :-: | :-: | :-: | ---------------------------------------------------------------------- |
| `call`          |  1  | \*  |  1  | calls a function                                                       |
| `call_indirect` |  0  | \*  |  1  | calls a function at an index in the default table                      |
| `const`         |  1  |  0  |  1  | pushes a constant value onto the stack                                 |
| `drop`          |  0  |  0  |  0  | pops top value from stack and does nothing with it                     |
| `nop`           |  0  |  0  |  0  | no operation                                                           |
| `select`        |  0  |  3  |  1  | takes two values and a condition; returns 1st if true and 2nd if false |

TODO: Are `module` and `type` considered to be instructions?

The number of stack values used by `call` and `call_indirect` matches
the number of parameters in the signature of the function being called.

We saw an example of using the `call` instruction in the
`factorial` function defined in the "Control Instructions" section.

We have seen many examples of using the `const` instruction
to push a constant value onto the stack.

We saw an example of using the `drop` instruction
in the `blockWithResult` function
defined in the "Control Instructions" section.

We saw an example of using the `select` instruction
in the `max` and `min` functions
defined in the "Comparison Instructions" section.

The `call_indirect` instruction is used in the file `demo.wat` below.
It works in conjunction with the `table` and `elem` instructions.

```wasm
(module
  (type $transform (func (param f64) (result f64)))

  (func $double (type $transform)
    (f64.mul (local.get 0) (f64.const 2))
  )

  (func $half (type $transform)
    (f64.div (local.get 0) (f64.const 2))
  )

  (func $triple (type $transform)
    (f64.mul (local.get 0) (f64.const 3))
  )

  ;; Create a table of function references named $transforms
  ;; that holds at most three elements.
  ;; Note that this is at the module level, not inside a function.
  (table $transforms 3 funcref)

  ;; Set elements in the table starting at offset zero.
  ;; Note that this is at the module level, not inside a function.
  (elem (i32.const 0) func $half $double $triple)

  (func (export "transform") (param $value f64) (param $fnIndex i32) (result f64)
    (call_indirect (type $transform) (local.get $value) (local.get $fnIndex))
  )
)
```

Compile this file to `demo.wasm` by entering `wat2wasm demo.wat`.

The file `demo.js` below uses `demo.wasm`.

```js
async function run() {
  const imports = {};
  const m = await WebAssembly.instantiateStreaming(fetch('demo.wasm'), imports);
  const {transform} = m.instance.exports;

  const n = 3.14;
  console.log('half =', transform(n, 0)); // using $half function; 1.57
  console.log('double =', transform(n, 1)); // using $double function; 6.28
  console.log('triple =', transform(n, 2)); // using $triple function; 9.42
}

run();
```

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

TODO: Demonstrate using emscripten to compile C code to WASM.

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

## Parallel WASM

TODO: Show how to run multiple WASM functions in parallel in a web browser
TODO: using WebWorkers.

TODO: Can they update the same linear memory in order to divide a large task
TODO: like rotating points?

## WASM Binary Format

WASM binary files have the extension `.wasm`.
They begin with four bytes that identify the file as WASM.
The hex values are `0061736d` which is zero
followed by the ASCII characters "asm".
This is followed by a four byte integer in little endian format
that specifies the WASM version which is currently `01000000` for version 1.

The remainder of the file is divided into 12 sections.

| Section Name | Description                                                                                                  |
| ------------ | ------------------------------------------------------------------------------------------------------------ |
| type         | describes function signatures (parameter and return types)                                                   |
| import       | describes imports from other modules including functions, tables, memory, and global variables               |
| func         | stores a list of indexes into the type section for each function defined in this module                      |
| table        | used by the `call_indirect` instruction for function pointers                                                |
| mem          | holds the lower and upper limits on the number of 64KB pages of linear memory that will be used              |
| global       | holds the type, mutability, and initial value of all global variables                                        |
| export       | describes all the functions, tables, memory, and global variables that are exported for other modules to use |
| start        | holds the index of the main/starting function if there is one (for running outside web browsers)             |
| elem         | holds data used to select a function from a table by the `call_indirect` instruction                         |
| code         | holds the local variables and code for each function defined in the module                                   |
| data         | holds data used to initialize the linear memory used by the module                                           |
| custom       | can store arbitrary data such a debugging information and data used by third party extensions                |

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
{% aTargetBlank "https://wasmtime.dev", "Wasmtime" %} (developed at Mozilla) and
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

TODO: Try reading and writing files using Wasmtime.
TODO: Try sending an HTTP GET request using Wasmtime.

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

TODO: Is this correct!
To execute a `.wast` test file, enter `wasmtime wast {path-to-wast-file}`.

Unlike wasm3, Wasmtime does not provide a REPL or
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

```

```
