---
eleventyNavigation:
  key: OCaml
layout: topic-layout.njk
---

<figure style="width: 30%">
  <img alt="OCaml logo" style="border: 0"
    src="/blog/assets/ocaml-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://ocaml.org", "OCaml" %} is
"an industrial-strength functional programming language
with an emphasis on expressiveness and safety."
It was released in 1996
which is the same year that Java was released.

OCaml uses a variant of the Hindley–Milner type system,
which is a type inference algorithm for
statically typed functional programming languages.
The Hindley–Milner type system has the ability to
infer the most general types of expressions
without requiring explicit type annotations in nearly all cases.

The Caml programming language, released in 1985, is the predecessor of OCaml.
The name is short for "Categorical Abstract Machine Language".
OCaml is short for "Objective Caml".
It adds support for object-oriented programming
and a more expressive type system.

OCaml is a member of the
<a href="https://en.wikipedia.org/wiki/ML_(programming_language)"
target="_blank">ML</a> (short for Meta Language)
family of programming languages.
Other languages that were influenced by ML include Standard ML,
Clojure, Elm, Erlang, F#, Haskell, Rust, and Scala.
F# was heavily inspired by OCaml and runs on the .NET platform.

F# is to C# as Scala and Clojure are to Java.
They are ML-inspired languages that
interoperate with an underlying non-ML language.

OCaml has an interpreter, a compiler that compiles to bytecode,
and a compiler that creates native executables.
OCaml can also be compiled to assembly, C, JavaScript, and WebAssembly.

The OCaml compilers are implemented in OCaml.
They are fast compared to the compilers for Haskell and Rust.

OCaml source files have the extension `.ml` which stands for "meta language".

The performance of OCaml is generally about 50% that of C.

OCaml supports Foreign Function Interface (FFI) mechanisms to call C functions.

Functional programming languages like OCaml are
well-suited for implementing interpreters and compilers.
The initial version of the Rust compiler was implemented in OCaml
until Rust was mature enough to host itself.

The biggest impediment to OCaml adoption is likely the
poor state of library documentation and lack of example code.
There is an attempt being made to address this by adding
cookbook-like material to the "Learn" section of the main web site.

## Notable Features

OCaml has a number of notable features including:

- strong static type checking
- incredible type inference (almost never need to specify types)
- very terse syntax for defining and calling functions
- pattern matching with many ways to match
- automatic function currying
- functional programming (not as pure as Haskell, but more pragmatic)
- object-oriented programming (though not often used)
- variant types (like enums with associated values)
- polymorphic types (OCaml's version of generics)
- interesting implementation of modules
- pragmatic approach to mutability
  (immutable by default, but can opt-in to some mutability)
- automatic garbage collection
- fast compared to other non-systems programming languages
- interoperable with C
- package management with OPAM
- comes with an interpreter and two compilers
  (one to produce bytecode and one to produce native executables)
- Dune build system
- `utop` REPL

## OCaml 5

OCaml version 5 was released in December 2022.
It added domains which map to operating system threads.

It also added "effect handlers" which used to be called "algebraic effects".
These are like a try/catch that can resume execution where the effect occurred.

## Resources

- <a href="https://ocaml.org" target="_blank">OCaml home page</a>

  Click the "Playground" link to experiment with writing OCaml code online.

- <a href="https://pliutau.com/my-first-experience-with-ocaml/" target="_blank">My First Experience With OCaml</a>
- <a href="http://ocamlverse.net/content/ecosystem.html" target="_blank">OCamlverse Ecosystem</a>
- <a href="https://learnxinyminutes.com/docs/ocaml/" target="_blank">Learn X in Y minutes</a>
  Where X=OCaml
- <a href="https://cs3110.github.io/textbook/cover.html"
  target="_blank">OCaml Programming: Correct + Efficient + Beautiful</a>
  book used in Cornell CS 3110 course
- <a href="https://www.youtube.com/watch?v=MUcka_SvhLw&list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU" target="_blank">OCaml Programming</a>
  course YouTube videos from Dr. Michael Ryan Clarkson at Cornell University
- <a href="https://dev.realworldocaml.org" target="_blank">Real World OCaml</a> book
- <a href="https://ocaml-book.com" target="_blank">OCaml from the Very Beginning</a> book
- <a href="https://caml.inria.fr/pub/old_caml_site/humps/" target="_blank">The Caml Humps</a>
  collection of links to Caml-related tools, libraries, code samples, and tips
- <a href="https://en.wikipedia.org/wiki/OCaml" target="_blank">OCaml Wikipedia page</a>
- <a href="https://ilyasergey.net/YSC2229/" target="_blank">Introductory Data Structures and Algorithms</a>
  course from Yale-NUS College

## Usage

Ahrefs uses OCaml in its backend systems and data processing pipelines
for Search Engine Optimization (SEO) tools and data analysis.

Bloomberg created BuckleScript which compiles OCaml code to JavaScript.
In 2022, BuckleScript was renamed to ReScript.

Citrix uses OCaml in the Hypervisor software.

Coq is an interactive theorem prover implemented in OCaml.

Docker uses OCaml in their desktop software for Windows and macOS.

Facebook uses OCaml for many things including:

- Hack programming language (extends PHP with static types)
- Facebook Messenger (the web version)
- Flow static type system for JavaScript
- Infer static analyzer for Java, C, C++, and Objective-C

Haxe is a high-level cross-platform programming language
that can be compiled to run on many platforms.
Its compiler is implemented in OCaml.

Jane Street uses OCaml for all their financial software,
including algorithmic trading.
They are one of the largest users and supporters of OCaml.

LexiFi uses OCaml in their financial software
for derivatives pricing and risk management.

T3 uses OCaml for algorithmic trading, quantitative analysis, risk management,
and other financial software.

Tarides uses OCaml and contributes to the
OCaml compiler, platform, and ecosystem.

## Derived Languages

<a href="https://reasonml.github.io" target="_blank">Reason</a>
(previously called ReasonML)
is a syntax extension and toolchain for OCaml developed by Facebook.
It provides a more JavaScript-like syntax while
retaining full compatibility with the OCaml language and its libraries.
The syntax of Reason is more familiar to JavaScript developers than OCaml.
It also supports JSX.

<a href="https://rescript-lang.org" target="_blank">ReScript</a>
(previously called BuckleScript) is "a robustly typed language
that compiles to efficient and human-readable JavaScript".
It was forked from Reason and is not compatible with OCaml.

<a href="https://melange.re/v3.0.0/" target="_blank">Melange</a>
is a set of tools that work with OCaml and Reason code
to generate and interoperate with JavaScript.
It supports JSX and can be used to generate React components.

<a href="https://coq.inria.fr" target="_blank">Coq</a>
is a formal proof system that is primarily implemented in OCaml.

## Installing

To install the OCaml package manager "opam" for Linux and macOS,
enter the following shell command which completes in a few seconds:

```bash
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
```

To install for windows, see <a href="https://ocaml.org/install"
target="_blank">Install OCaml on Windows</a>.

After installing `opam`, enter `opam init`
which takes over five minutes to complete.

To install tools for development, enter the following command
which takes about four minutes to complete:

```bash
opam install ocaml-lsp-server odoc ocamlformat utop
```

Each of these tools have a dependency on `dune`, so that is also installed.

To begin using the tools in your current shell, enter `eval $(opam env)`.

## Toolchain

The OCaml toolchain consists of the following components
that provide everything needed to
write, compile, test, debug, and profile OCaml programs and libraries.

- The `ocaml` interpreter interactive toplevel executes code interactively
  for testing small code snippets and exploring language features.
- The `ocamlc` compiler compiles source code
  into bytecode that can run on any platform.
  It creates files with the extensions `.cmo` and `.cmi` (for interfaces).
  The `-o` flag typical typically specifies an output file
  with a `.byte` extension that is passed to the `ocamlrun` command.
  The `.cmo` and `.cmi` files are not needed to run the `.byte` file,
  but incremental recompilation relies on them.
- The `ocamlrun` command executes a bytecode file.
- The `ocamlopt` compiler compiles source code
  into an optimized, platform-specific executable.
- The `opam` package Manager installs libraries and tools,
  manages dependency versions, and upgrades them.
- The `ocamlbuild` (predecessor to `dune`) and `dune` (modern) build systems
  simplify building complex and managing dependencies.
- The `ocamldoc` tool generates documentation from OCaml source code annotations.
  This is in maintenance mode. Using `odoc` through Dune is preferred.
- The `ocamldebug` debugger is used to debug OCaml programs.
- The `ocamlprof` tool profiles OCaml programs
  to analyze their performance and identify bottlenecks.
- The standard library provides commonly needed functions and data structures.
  It includes modules for strings, lists, arrays, I/O operations, and more.

Every source file defines a module, which is a collection of named values
that can be constants, functions, and types.
An `open` expression brings the names defined in a module into the current scope.
Circular dependencies between modules are not allowed.

Suppose we have the file `my_module.ml` containing
the function definition `let add a b = a + b`.
The name of this module is the name of the file
with the first letter uppercased.

Suppose the file `main.ml` contains the following:

```ocaml
open My_module

let () =
  let sum = add 1 2 in
  print_int sum
```

The syntax `()` represents the "unit value" and represents not having a value.

Ending a source file that is the starting point of a program
with `let () =` followed by an expression is not required, but
it ensures that the result of the expression will be the unit value.
It also makes it clear that the purpose of the expression
is the side effects it produces.

It is recommended to create a Dune project,
move these files into the project `bin` directory,
and run it with `dune exec {project-name}`.
See the Dune section below for more detail.

However, there are several options for
building and running a project without using Dune.

To compile this to an executable and run it,
enter `ocamlopt my_module.ml main.ml -o main` and `./main`.

To compile this to bytecode and run it,
enter `ocamlc my_module.ml main.ml -o main.byte` and `ocamlrun main.byte`.

To compile this to an executable and run it
by specifying only the main source file,
enter `ocamlbuild main.native` and `./main.native`.
The `ocamlbuild` command creates a `_build` directory containing
many intermediate files that are not needed to run the program.

To compile this to bytecode and run it
by specifying only the main source file,
enter `ocamlbuild -cflags -g main.byte` and `ocamlrun main.byte`.

## Help

For help on OCaml, see:

- <a href="https://tinyurl.com/discord-ocaml" target="_blank">OCaml Discord</a>
- <a href="https://discuss.ocaml.org/" target="_blank">OCaml Discourse</a>
- <a href="https://github.com/lindig/ocaml-style" target="_blank">OCaml style guide</a>

Also use the odig tool.
To install it, enter `oapm install ocaml-manual odig`.
To use it, enter `odig doc`.
The first time this is run it will take
five minutes or so to update the documentation.
After that, running it will open a browser tab.
This links to external documentation pages
that are typically brief and lacking in examples.

## VS Code

Install the "OCaml Platform" extension from OCaml Labs.

Create the file `.ocamlformat` in each project root directory containing
at least the following in order for VS Code to format OCaml code on save.

```text
profile = default
version = 0.26.1
```

For documenation on `ocamlformat` options, see
<a href="https://ocaml.org/p/ocamlformat/latest/doc/manpage_ocamlformat.html"
target="_blank">Manpage: ocamlformat</a>.

To try the options online, see <a href="https://ahrefs.github.io/ocamlformat/"
target="_blank">OCamlFormat configurator</a>.

You may want to add these:

```text
break-infix = fit-or-vertical
if-then-else = fit-or-vertical
parse-docstrings = true
wrap-comments = true
```

Copy the `.ocamlformat` into every subdirectory
that contains its own `dune-project` file.

Sometimes after code changes VS Code flags errors that aren't real.
Running "Developer: Reload Window" from the command palette clears them.
This issue may go away if you run `dune build -w` (for watch mode)
in a terminal window.
Doing this is useful anyway because
there may be errors that `dune build` will flag,
but the Language Server Processor (LSP) used in VS Code will not.

## Expressions

An expression in OCaml can be a:

- literal value - ex. `true`, `3`, `3.14`, or "hello"
- variable - ex. `x`
- `let` variable declaration - ex. `let x = 3`
- `let` function declaration - ex `let double x = x * 2`
- keyword expression - ex. `if ...` or `for ...`
- function call - ex. `double 3`
- sequence of any of the above separated by semicolons

When using a sequence of expressions,
all but the last must evaluate to the unit value.
The `ignore` function can be applied to any expression to ignore its result.
That function takes any value and returns the unit value `()`.

The following code shows two ways to ignore the return value of a function.
Assume the `add_dog` function returns a `dog` record.

```ocaml
ignore (add_dog "Comet" "Whippet");
add_dog "Oscar" "German Shorthaired Pointer" |> ignore;
print_endline "finished adding dogs"
```

## REPL

OCaml has two REPLs. A basic one can be started by entering `ocaml`.
A better REPL is `utop` which is short for "Universal TOPlevel".
The `utop` command provides a more interactive, user-friendly interface that
includes line editing, syntax highlighting, command history, and tab completion.

In either REPL the expressions you enter are only evaluated
when they are terminated by a double semicolon (`;;`).
This allows entering a series of expressions
that are separated from each other by a single colon
and allows them to span multiple lines. For example:

```ocaml
print_int (2 + 3);
print_int (2 * 3);; (* 56 *)
```

Double semicolons are only used in REPL sessions, not in source files
(except when assigning global identifiers).

When an expression is entered, `utop` will
output its name, inferred type, and value.
For example, entering `let x = 7;;` will output `val x : int = 7;`.

If the expression does not have a name,
the part to the left of the colon will be a dash.
For example, entering a raw value like `7;;` or
an anonymous function like `fun x -> x * x;;` will do this.
But entering a named function like `let square x = x * x;;`
will output the name.

Use the left and right arrow keys to move the cursor within the expression
and make edits.

Use the up and down arrow keys to recall previously entered expressions.
These can be edited and executed again.

To show the type of an expression `e`, enter `#show e;;`

To load definitions from an OCaml source file into the REPL,
enter `#use "{file-path}";;`.
This enables using all the types and functions defined in the source file
inside the REPL.

To configure `utop` options that should be used every time it is started,
create the file `~/.config/utop/init.ml`.
For example, the following automatically requires and opens the `Core` module
and makes the output less noisy.

```ocaml
#require "Core";;
open Core;;
#utop_prompt_dummy;;
```

To remove the typeahead hint boxes, add `UTop.set_show_box false`.

To trace the flow of function execution, enter `#trace {fn-name};;`.
Then enter a call to the function.
This is especially useful for recursive functions.
To stop tracing the function, enter `#untrace {fn-name};;`.

To exit the REPL, press ctrl-d or enter #quit.

There is also an iOS app called "OCaml" for evaluating OCaml expressions.

## Comments

Comments in OCaml code begin with `(*` and end with `*)`.
They can span any number of lines and can be nested.
There is no syntax for single-line comments.

Doc comments provide documentation that can be extracted from source files.
They begin with `(**` and end with `*)`.
Their content is similar to JSDoc comments,
including the use of `@param` and `@return` annotations.
There are three variations of doc comments, floating, item, and label.

## Primitive Types

OCaml supports the following primitive types.
Their sizes depend on the CPU.

- unit - one literal value `()`
- `bool` - 1 byte with the literal values `true` and `false`
- `char` - 1 byte ASCII, not Unicode
- `int` - 8 or 4 bytes
- `float` - 8 bytes
- `string` - sequence of bytes, not Unicode characters

Unit represents having no value and is
the return type of functions that don't return anything.

The functions `succ` and `pred` operate on `int` and `float` values
and return the value one higher or one lower.

Single literal characters are delimited by single quotes.

Literal strings are delimited by double quotes.
The `^` operator is used to concatenate strings.

The `Char` and `String` modules provide many functions
for operating on values of these types.

Some libraries assume that sequences of bytes in a `string`
represent Unicode characters.
See the libraries `Uutf`, `Uutf_string`, and `ocaml-unicode`.

Primitive values are expressions that do not require additional evaluation.

OCaml does not support null values.

There is a corresponding standard library module for each of these whose name
begins uppercase. These provide functions for operating on values of the type.

## Primitive Type Conversions

The OCaml standard library provides many functions
for converting a value from one type to another.
Examples include:

- `bool_of_string` - raises `Failure` if conversion fails
- `bool_of_string_opt` - returns `None` if conversion fails
- `char_of_int`
- `float_of_int`
- `float_of_string` - raises `Failure` if conversion fails
- `float_of_string_opt` - returns `None` if conversion fails
- `int_of_char`
- `int_of_float` - `truncate` is an alias
- `int_of_string` - raises `Failure` if conversion fails
- `int_of_string_opt` - returns `None` if conversion fails
- `string_of_bool`
- `string_of_float`
- `string_of_int`

There is no `string_of_char` function.
To convert a `char` expression `c` to a `string` value, use `String.make 1 c`.

## Naming Conventions

It is idiomatic in OCaml for

- variable, function, and class names to use
  snake_case with all lowercase letters
- module names and variant constructors to use CamelCase,
  starting with an uppercase letter

It is idiomatic in OCaml for the name of a function that

- transforms a value of type x to a value of type y to be `y_of_x`
- returns a `Option` value to end in `_opt`
- raises an exception to end in `_exc`

## Keywords

OCaml reserves the use of the following identifiers:
`and`, `as`, `assert`, `asr`, `begin`, `class`,
`constraint`, `do`, `done`, `downto`, `else`, `end`,
`exception`, `external`, `false`, `for`, `fun`, `function`,
`functor`, `if`, `in`, `include`, `inherit`, `initializer`,
`land`, `lazy`, `let`, `lor`, `lsl`, `lsr`,
`lxor`, `match`, `method`, `mod`, `module`, `mutable`,
`new`, `nonrec`, `object`, `of`, `open`, `or`,
`private`, `rec`, `sig`, `struct`, `then`, `to`,
`true`, `try`, `type`, `val`, `virtual`, `when`,
`while`, and `with`.

## Operators

OCaml operators only operate on specific types.
This enables type inference of function return types
because the compiler can determine the types required by its code.
It is a very important way in which OCaml
differs from most other programming languages.

Operators for `float` values are the same as those for `int` values,
but with a `.` added to the end.

The arithmetic operators include:

| Operator | Description          |
| -------- | -------------------- |
| `~-`     | int negation         |
| `+`      | int addition         |
| `-`      | int subtraction      |
| `*`      | int multiplication   |
| `/`      | int division         |
| `~-.`    | float negation       |
| `+.`     | float addition       |
| `-.`     | float subtraction    |
| `*.`     | float multiplication |
| `/.`     | float division       |
| `**`     | float exponentiation |
| `mod`    | modulo               |

The `-` and `-.` operators can also be used for unary negation,
but in some cases their meaning is ambiguous.
That is why the `~-` and `~-.` operators are provided.

There is no exponentiation operator for `int` values in the standard library.

When using the `Base` module:

- the `**` operator performs `int` exponentiation
- the `**.` operator performs `float` exponentiation
- the `%` operator performs modulo

There are no operators like `++` and `--` to increment or decrement a number.
Instead use the functions `succ` and `pred` to get the successor or predecessor.
For refs that hold an `int` value, use the functions `incr` and `decr`.

The string operators include:

| Operator | Description                 |
| -------- | --------------------------- |
| `^`      | string concatenation        |
| `^^`     | format string concatenation |

The relational and logical operators include:

| Operator | Description                              |
| -------- | ---------------------------------------- |
| `==`     | physical equality; same address          |
| `!=`     | physical (inequality); different address |
| `=`      | structural equality; same content        |
| `<>`     | structural inequality; different content |
| `<`      | less than                                |
| `>`      | greater than                             |
| `<=`     | less than or equal                       |
| `>=`     | greater than equal                       |
| `&&`     | boolean and                              |
| `\|\|`   | boolean or                               |
| `not`    | boolean not                              |

The `=` operator is also used with the `let` keyword for assignment.

The remaining operators include:

| Operator | Description                                     |
| -------- | ----------------------------------------------- |
| `!`      | gets ref value (dereferences)                   |
| `:=`     | sets ref value (assigns)                        |
| `@`      | list concatenation                              |
| `@@`     | function application                            |
| `\|>`    | reverse function application (aka pipe forward) |

The function application and reverse function application operators
provide an alternative to surrounding nested function calls with parentheses.
The `@@` operator cannot follow `assert` because that is not a normal function.

For example:

```ocaml
let double x = x * 2
let square x = x * x
let () =
  (* All the remaining expression print 16. *)

  let d = double 2 in
  let s = square d in
  print_int s;

  print_int (square (double 2))

  print_int @@ square @@ double @@ 2

  2 |> double |> square |> print_int
```

Most OCaml operators are implemented as binary functions.
To use them as functions, wrap them in parentheses.
For example, `a + b` is the same as `(+) a b`.
Adding spaces inside the parentheses is optional,
but is required for the `*` operator because
`(*` is interpreted as the beginning of a comment.

Operator functions can be passed to functions like `List.filter`.

The following code demonstrates writing a function that
takes a comparison function, which can be a logical operator.
The `choose` function takes two lists and a comparison function.
It returns a new list where each element
is the element from the first or second list at the same index
where comparing them using the comparison function evaluates to true.

```ocaml
let rec choose list1 list2 cmp_fun =
  (* match is described in the "Conditional Logic" section. *)
  match (list1, list2) with
  | [], [] -> [] (* both lists are empty; return empty list *)
  | [], _ -> list2 (* first list is empty; return second list *)
  | _, [] -> list1 (* second list is empty; return first list *)
  | h1 :: t1, h2 :: t2 ->
      if cmp_fun h1 h2 then
        h1 :: choose t1 t2 cmp_fun (* add h1 to result list *)
      else h2 :: choose t1 t2 cmp_fun (* add h2 to result list *)

let l1 = [ 1; 3; 4; 6; 9; 10 ]
let l2 = [ 7; 1; 0; 8; 6; 4 ]

let () =
  List.iter
    (fun x ->
      print_int x;
      print_string " ")
    (choose l1 l2 ( > )) (* relational operator passed here *)
```

The result is `[7; 3; 4; 8; 9; 10]`.

Custom binary operators can be defined using an allowed set of characters.

The `open` keyword brings all the identifiers in a given module,
`printf` in this case, into scope so they can be used
without prefixing them with their module name.

For example:

```ocaml
open Printf

type point = { x : float; y : float }

(* This operator adds two points. *)
let ( +! ) (p1 : point) (p2 : point) : point =
  { x = p1.x +. p2.x; y = p1.y +. p2.y }

(* This is an alternate way to define the same operator. *)
let ( +! ) { x = x1; y = y1 } { x = x2; y = y2 } =
  { x = x1 +. x2; y = y1 +. y2 }

(* This shadows the built-in + operator to perform subtraction instead,
   but only in the current scope. *)
let ( + ) (n1 : int) (n2 : int) : int = n1 - n2

let () =
  let p1 = { x = 1.; y = 2. } in
  let p2 = { x = 4.; y = 6. } in
  let p3 = p1 +! p2 in
  printf "(%F, %F)\n" p3.x p3.y; (* { x = 5.; y = 8. } *)

  let n1 = 2 and n2 = 3 in
  printf "%d\n" (n1 + n2) (* -1 *)
```

## Variables

Variables are immutable.
An exception is that variables in a REPL can be reassigned.

Identifier names must start with a lowercase letter unless they refer to
a module, variant constructor, or "polymorphic variant tag".
They can contain letters, digits, and the underscore character.
They can also end with single quotes to create names like
like `x'` (for x prime) and `x''` (for x double-prime).
Technically an identifier can contain any number of single quotes
and they can appear anywhere except at the beginning,
but doing this is odd.

A `let` **expression** binds an identifier to the value of an expression
whose scope is the expression that follows the `in` keyword.
The value of a `let` expression is the value of its expression
with all occurrences of the identifier replaced with its value.
For example, here are three ways to write a `let` expression
that evaluates to `3`:

```ocaml
let a = 1 in
let b = 2 in
a + b

let a = 1 and b = 2 in
a + b

let a, b = 1, 2 in
a + b
```

Note how the expression that follows a `let` expression
can be another `let` expression in order to place
multiple identifiers into the scope of the final expression.

It is a common error to write `let variable = expression;`
instead of `let variable = expression in`.
When this is done, your editor should flag it with the message
"Warning 10: this expression should have type unit".
If you compile the code, the message "Error: Syntax error" will be output
and will refer to the last line in the file, which is unhelpful.

Identifiers bound by `let` expressions go out-of-scope
after they are evaluated.

The type of a variable can be specified, but typically
this is omitted because the type can be inferred.

```ocaml
let a : int = 1 in
let b : int = 2 in
a + b
```

While it is not required to include a space on both sides of the colon
when specifying a type, it is customary
and the `ocamlformat` code formatter will add them.

A `let` **definition** omits the `in` keyword.
These are used inside modules to create global definitions
that do not go out of scope after they are evaluated.
They are not expressions, so they do not have a value.

The following example from a REPL session binds three global identifiers.
Note how a double semicolon is used to
terminate the assignments of global identifiers.

In order to call a function that takes no arguments, such as `print_newline`,
it must be passed the unit value `()`.
Without this it is just a reference to the function and not a call to it.

```ocaml
let a = 1
let b = 2
let c = 3;;

print_int (a + b);
print_newline ();
print_int (b + c);
print_newline ()
```

A `let` expression can use pattern matching to bind any number of variables
to elements in various collection types.
This is similar to JavaScript destructuring.

The syntax `let pattern = expression in ...` is the same as
writing `match expression with pattern -> ...`,
but `match` supports multiple patterns.
Types like lists and arrays always need more than one path
to avoid getting a warning for a non-exhaustive match.
For example:

```ocaml
open Printf

let my_tuple = (1, 2, 3)
let my_list = [ 1; 2; 3 ]
let my_array = [| 1; 2; 3 |]

type my_record_type = { first : int; second : int; third : int }

let my_record = { first = 1; second = 2; third = 3 }

let () =
  (* Only one pattern is needed because tuples have a fixed structure.
     The single pattern is "irrefutable". *)
  let a, b, c = my_tuple in
  printf "tuple: a = %d, b = %d, c = %d\n" a b c;

  (* Same for records. *)
  let { first; second; third } = my_record in
  printf "record: first = %d, second = %d, third = %d\n" first second third;

  (* Multiple patterns are needed for types with a variable structure
     like lists, arrays, and variants. *)
  (* We can match on a list that contains at least some number of elements. *)
  (match my_list with
  | a :: b :: c :: _ -> printf "list: a = %d, b = %d, c = %d\n" a b c
  | _ -> print_endline "list doesn't contain at least 3 elements");

  (* We cannot do this for an array.
     Instead we must match on a specific number of elements. *)
  match my_array with
  | [| a; b; c |] -> printf "array: a = %d, b = %d, c = %d\n" a b c
  | _ -> printf "array doesn't contain exactly 3 elements\n"
```

### Type Variables

OCaml supports type variables which are similar to generics in other languages.
Type variables serve as a placeholder for an unknown type.
They are written with a single quote followed by a lowercase name.

Often the name is just `'a` and is pronounced "alpha".
If additional type variables are needed,
it is common to use `'b` (beta) and `'c` (gamma).
Other common type variable names are `'k` for a key type
and `'v` for a value type.

For example, entering `[];;` in `utop` outputs type type `'a list`
because it is a list where the type of the elements is unknown.

The following code demonstrates writing a function that uses
parametric polymorphism to find the largest value in a list of values.

```ocaml
(* The next 2 lines can be replaced by the following
   omits the type specifications:
   let rec max_element = function *)
let rec max_element (ls : 'a list) : 'a option =
  match ls with
  | [] -> None (* for an empty list *)
  | [ x ] ->
      Some x (* for a list with only one element *)
  | hd :: tl -> (
      match max_element tl with
      | None ->
          Some hd (* head is maximum if tail is empty *)
      | Some max_tl -> Some (if hd > max_tl then hd else max_tl))

(* This is a more compact way to write the function above. *)
let max_element = function
  | [] -> None
  | hd :: tl -> Some (List.fold_left max hd tl)

let () =
  let numbers = [ 1; 13; 4; 9 ] in
  let max = max_element numbers in
  match max with
  | None -> print_endline "empty list"
  | Some max -> print_int max (* 13 *)
```

## References

While variables are immutable, they can be
bound to a reference holds a mutable value.
References are created with the `ref` function
which must be given an initial value.
The initial value determines its type.

The `!` prefix operator dereferences a `ref` to obtain its value.
It does not negate a `bool` value. The `not` function is used for that.

The `:=` operator assigns a new value to a `ref`.

For example, the following code sets the variable `score` to a reference to `0`.
The `while` loop increments the value until it reaches 10.

```ocaml
let score = ref 0 in
while !score < 10 do
  score := !score + 1;
  print_int !score;
  print_newline ()
done
```

For `int` refs, the `incr` and `decr` functions
can be used to increment and decrement their value.

Refs are actually single field records with a mutable field named `contents`.
The `ref` operators are just shorthand for accessing that field.
If `r` is a variable bound to a `ref`
then `!r` is short for `r.contents`
and `r := expr` is short for `r.contents <- expr`.

Other kinds of values in OCaml that support mutation include
arrays and record fields (when marked as `mutable`).
Values created with the following standard library modules
also support mutation:
`Atomic`, `Bytes`, `Hashtbl`, `Mutex`, `Queue`, `Semaphore`, and `Stack`.

A function can hold a `ref` in its scope using a closure.
For example, the following function returns
the next `int` value every time it is called.

```ocaml
let next =
  let last = ref 0 in
  fun () ->
    incr last;
    !last
```

Call this repeatedly with `next ()`.

## Algebraic Data Types (ADTs)

OCaml supports algebraic data types which include product types and sum types.

Product types (aka "each-of types") describe a conjunction
which can be thought of as "this AND this AND this".
They can hold multiple pieces of data with differing types.
Examples of product types in OCaml include tuples and records.

Tuples describe a cartesian product of values.
For example, the tuple type `float * float` describes the combination of
every possible float value (say an x-coordinate) with
every possible float value (say a y-coordinate).

Sum types (aka "one-of types") describe alternatives with a disjunction
which can be thought of as "this OR this OR this".
Examples of sum types in OCaml include
the primitive type `int` and variant types (described later).
Lists and trees are implemented as variant types.
The elements in a list are represented as a variant whose value can be
the empty list (`[]`) or a cons cell created with the `::` operator that
represents a value and a list tail.
A tree can be represented as a variant whose value can be `Empty` or `Node`
where nodes have a value and
references to nodes representing left and right subtrees.

Lists are sum types because they can be either an empty list
or a head and tail (where the tail can be an empty list).
Lists are also product types because they hold
a head and a tail which have different types.

## Generalized Algebraic Data Types (GADTs)

GADTs are an extension to ADTs that support more precise type specifications
(more constraints) and enable better type inference in certain situations.

GADTs are a step toward supporting dependent types.
Dependent types enable defining types whose meaning depends on values.
For example a list type can specify a fixed length
and/or that its values must be in ascending order.

TODO: Add more detail on GADTs.

## Type Aliases

The `type` keyword defines a type alias.
It is used to define shorthand name for
a type definition (such as a variant type)
or to define an alias for an existing type.

For example:

```ocaml
type weight = float
type point = float * float (* a tuple *)
```

## Variant Types

Variant types have many uses including enumerated values, error handling,
and representing data structures whose shape can vary.

Each variant name is called a "constructor" (aka "tag")
and must begin with an uppercase letter.
Each constructor can have an associated value
of a type that is specified after the `of` keyword.
The value types of the variants can differ.

To see all the constructors of a variant type in `utop`,
enter `#show {variant-type}`.

Constructor expressions that do not use a value
are referred to as "constant variant expressions".
The following code provides some examples:

```ocaml
type season = Spring | Summer | Fall | Winter
type tense = Spring | Sprung | Sprang

(* The first vertical bar here is optional.
   When pattern matching a sum type, if all variants aren't matched,
   the warning "this pattern-matching is not exhaustive" will appear.
   Since both the season and tense types define a Spring constructor,
   we need to specify which variant type to use. *)
let forecast : season -> string = function
  | Spring -> "rain"
  | Summer -> "sun"
  | Fall -> "nice"
  | Winter -> "snow"

let get_tense : tense -> int = function
  | Spring -> 1
  | Sprang -> 2
  | Sprung -> 3

let () =
  print_endline (forecast Winter);
```

Another option to avoid constructor name conflicts
is to wrap the conflicting variant type in a struct.
It is idiomatic for the main type in a module to be named "t".

```ocaml
module Tense = struct
  type t = Spring | Sprung | Sprang
end

let tense = function Tense.Spring -> 1 | Tense.Sprang -> 2 | Tense.Sprung -> 3
```

One more option is to a prefix to the constructor names
so they don't collide.

```ocaml
type season = SSpring | SSummer | SFall | SWinter
type tense = TSpring | TSprung | TSprang
```

Variants that do use a value are referred to as
"non-constant variant expressions".
These are similar to TypeScript type unions.
The following code provides some examples:

```ocaml
type my_union = BoolVal of bool | IntVal of int | StringVal of string

let get_string = function
| BoolVal b -> string_of_bool b
| IntVal i -> string_of_int i
| StringVal s -> s

let () =
  let b = BoolVal true in
  let i = IntVal 10 in
  let s = StringVal "hello" in
  print_endline (get_string i);
  print_endline (get_string s);
  print_endline (get_string b)
```

The following code uses a variant type to describe shapes.

```ocaml
type point = float * float

type shape =
  | Circle of { center : point; radius : float }
  | Rectangle of { lower_left : point; width : float; height : float }

let area = function
  | Circle { radius = r } -> Float.pi *. r *. r
  | Rectangle { width = w; height = h } -> w *. h

let center shape =
  match shape with
  | Circle { center = c } -> c
  | Rectangle { lower_left = x, y; width = w; height = h } ->
      (x +. (w /. 2.), y +. (h /. 2.))

let () =
  let c = Circle { center = (0., 0.); radius = 10. } in
  let r =
    Rectangle { lower_left = (0., 0.); width = 10.; height = 5. }
  in
  printf "c area = %f\n" (area c); (* 314.159265 *)
  printf "r area = %f\n" (area r); (* 50.0 *)
  let x, y = center c in
  printf "c center = (%f, %f)\n" x y; (* (0.0, 0.0) *)
  let x, y = center r in
  printf "r center = (%f, %f)\n" x y; (* (5.0, 2.5) *)
```

Variant types can be parameterized. For example:

```ocaml
open Printf

(* A tree can be empty or it can have a node
   that holds a left tree, a value, and a right tree. *)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let rec depth_first_in_order tree =
  match tree with
  | Empty -> () (* do nothing *)
  | Node (value, left, right) ->
      depth_first_in_order left;
      print_endline value;
      depth_first_in_order right

(* This holds names in a tree so that when
   printed with depth_first_in_order
   they will appear in sorted order. *)
let family_tree : string tree =
  Node
    ( "Jeremy",
      Node ("Amanda", Empty, Empty),
      Node
        ( "Meghan",
          Node ("Mark", Empty, Empty),
          Node ("RC", Empty, Node ("Tami", Empty, Empty)) ) )

let () = depth_first_in_order family_tree
```

### Option Variant Type

The <a href="https://v2.ocaml.org/api/Option.html" target="_blank">Option</a>
module defines a variant type represents an optional value.
It has the constructors `None` and `Some` defined by
`type 'a option = None | Some of 'a`.

Functions that sometimes do not have a value to return
use this type to represent their return value.
For example, the `List.find_opt` function does this.

```ocaml
open Printf

let colors = [ "red"; "green"; "blue" ]
let color = "green"
let result = List.find_opt (fun x -> x = color) colors

let () =
  match result with
  | Some c -> printf "found %s\n" c
  | None -> print_endline "failed to find green\n"
```

The <a href="https://v2.ocaml.org/api/Option.html" target="_blank">Option</a>
module defines many functions that operate on an `Option` value.
Many of these have operator equivalents.

`Option.value` extracts the value from an `Option`
and provides a default value to use when it is `None`.
`Option.fold` is similar, but it takes a function
that is called to produce the return value when it is a `Some`.
The following code demonstrates using both of these.

```ocaml
type dogt = { name : string; breed : string }

let dog1_opt = None
let dog2_opt = Some { name = "Comet"; breed = "whippet" }

let () =
  let dog = Option.value ~default:{ name = "?"; breed = "?" } dog1_opt in
  print_endline dog.breed;

  let breed =
    Option.fold ~none:"unknown"
      ~some:(fun dog -> String.uppercase_ascii dog.breed)
      dog2_opt
  in
  print_endline breed
```

`Option.map` applies a function to the value inside an `Option`
and return a new `Option`.
If it is `Some v` then the result of passing `v` to the function
is returned in a `Some` variant.
If it is `None` then `None` is returned.
The operator `>>|` is often defined to use this.

For example:

```ocaml
let double x = x * 2
let is_even x = x mod 2 == 0

let () =
  let numbers = [ 1; 4; 7 ] in
  List.find_opt is_even numbers
  |> Option.map double |> Option.map string_of_int
  |> Option.value ~default:"no even found"
  |> print_endline
```

`Option.bind` is used in reverse function application chains
so a function that returns an `Option` can have
the value inside it passed to the next function in the chain.
If any function returns `None`, the remaining functions are not called
and the value of the entire chain is `None`.
The operator `>>=` is often defined to use this.

### Result Variant Type

The <a href="https://v2.ocaml.org/api/Result.html" target="_blank">Result</a>
module defines a variant type represents
the result of a function that can succeed or fail.
It has the constructors `OK` and `Error`.

For example:

```ocaml
let divide numerator denominator =
  if denominator = 0. then Error "cannot divide by zero"
  else Ok (numerator /. denominator)

let () =
  let n = 5. and d = 0. in
  match divide n d with
  | Ok v -> print_endline (string_of_float v)
  | Error e -> print_endline e
```

### Interpreter

Variant types can be used to build interpreters. For example:

```ocaml
type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec eval = function
  | Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2

let () =
  let result = eval (Add (Int 5, Mul (Int 3, Int 2))) in
  print_endline (string_of_int result)
  (* 5 + (3 * 2) = 11 *)
```

## Conditional Logic

Conditional logic is implemented with the `if` and `match` expressions.

### if expression

The syntax is `if expr1 then expr2 else expr3`.

`expr1` must evaluate to a `bool`.
Non-`bool` values are not automatically coerced to a `bool` value.
For example, `0` is not treated as `false`.

`expr3` must evaluate to the same type as `expr2`
so the `if` expression always evaluates to the same type.

For example:

```ocaml
let sign = if result > 0 then "positive"
  else if result < 0 then "negative"
  else "zero"
```

To include multiple expressions after `then` or `else`,
surround them with parentheses and separate them with semicolons.
The value will be the value of the final expression.

As an alternative to parentheses, the `begin` and `end` keywords can be used.
However, OCaml editor extensions may
automatically replace those with parentheses.

Unless the `then` branch produces a unit result, an `else` branch is required.

An `if` expression can be used where other languages use a ternary operator.

### match expression

A `match` expression performs pattern matching.
It takes an expression whose value is to be matched and a set of branches.
Each branch begins with a vertical bar (`|`) followed by a pattern,
the characters `->`, and code to execute when the pattern is matched.
The vertical bar before the first branch is optional.

A `match` expression is somewhat like a
`switch` statement in other programming languages.
You can think of the vertical bar at the beginning of each branch
as being like the `case` keyword in those languages.

The type of a `match` expression is the type of its branch expressions,
which must all evaluate to the same type.

The patterns must be exhaustive, meaning that
there must be a pattern that matches every possible value.
Using the catch-all `_` as the final pattern satisfies this.

The patterns can match:

- a constant (ex. `| 7` or `| "summer"`)
- a range of characters (ex. `| 'a' .. 'f'`)
- a variant type constructor (ex. `| None` or `| Some x`)
- a tuple (ex. `| (_, "summer", temperature)` which means
  we don't care about the first element, the second element must be `"summer"`,
  and we want to capture the value of the third element)
- a list (ex. `| []` or `| ["summer"; other]` or `| first :: second :: rest`)
- an array (ex. `| [||]` or `| [|"summer"; other|]`)
- a record (ex. `| {name; age = a}`)
- multiple match expressions (ex. `| 7 | 8 | 9`)
- a variable to match anything and bind the value to it
- a guard using the `when` keyword (ex. `| n when 7 <= n && n <= 9`)
- the catch-all `_` which doesn't bind the value

The patterns are checked in the order they are specified and
the first matching pattern is used, so their order matters.

The value of a `match` expression is the value after the matched pattern.

There is a warning if the branches are not exhaustive.
At runtime if no branch matches, a `Match_failure` exception is raised.

For example:

```ocaml
let () =
  Random.self_init ();
  let n = Random.int 10 in
  print_endline (string_of_int n);
  match n with
  | 0 | 1 | 2 | 3 -> print_string "small"
  | n when 4 <= n && n <= 7 -> print_string "medium"
  | _ -> print_string "large"
```

The `as` keyword is used to bind a name to the entire matched expression.
For example:

```ocaml
let my_tuple = (true, 3)

let () =
  match my_tuple with
    | (b, n) as t -> Printf.printf "%b, %d, %b, %d\n" b n (fst t) (snd t)
    | _ -> print_endline "will never happen"
```

The `function` keyword defines a function, like the `fun` keyword,
but it's specialized to perform pattern matching on its first argument.
It is useful in functions that immediately `match` on the last parameter.
It simplfies the code by removing the need to list the last parameter
and replaces `match {last-parameter} with` with just `function`.
For example:

```ocaml
type season = Winter | Spring | Summer | Fall

let weather s =
  match s with
  | Winter -> "cold"
  | Spring -> "cool"
  | Summer -> "hot"
  | Fall -> "perfect"

(* same as previous function *)
let weather2 = function
  | Winter -> "cold"
  | Spring -> "cool"
  | Summer -> "hot"
  | Fall -> "perfect"

let () =
  print_endline (weather Fall); (* perfect *)
  print_endline (weather2 Fall); (* perfect *)
```

The following code uses the `function` keyword to
transform a list of `Option` values to a list of unwrapped values
where a default value is used for `None` cases.

```ocaml
let options = [ Some 1; None; Some 2 ]
let values = List.map (function Some x -> x | None -> 0) options
let () = List.iter (Printf.printf "%d\n") values
(* outputs 1, 0, and 2 *)
```

## Iteration

The `for` and `while` keywords support imperative iteration.
It is more typical in OCaml to use recursion for iteration.

A `for` loop specifies start and stop values,
but it cannot specify a step size.

The expression inside a `for` loop must have a unit value,
which means that the loop itself doesn't return a value.

There are no `break` or `continue` statements like in other languages.

The following code demonstrates `for` loops.

```ocaml
open Printf

let () =
  for i = 1 to 5 do
    printf "%d\n" i
  done;

  for i = 5 downto 1 do
    printf "%d\n" i
  done
```

A `while` loop specifies a boolean condition.
This might be an expression involving a `ref` whose value changes
or a function that returns a value to be tested.
The loop terminates when the condition evaluates to `false`.

The following code demonstrates `while` loops.

```ocaml
open Printf

let () =
  let i = ref 1 in
  while !i <= 5 do
    printf "%d\n" !i;
    incr i
  done
```

To iterate over collections like lists and arrays it's best to
use `iter` functions instead of `for` or `while` loops.

```ocaml
open Printf

let () =
  let numbers = [ 1; 2; 3; 4; 5 ] in
  List.iter (fun x -> printf "%d\n" x) numbers;

  let numbers = [| 1; 2; 3; 4; 5 |] in
  Array.iter (fun x -> printf "%d\n" x) numbers
```

## Collections

OCaml has built-in support for many kinds of collections.
The most commonly used are tuples, lists, arrays, and maps.

| Collection | Primary Use Case                                                                    |
| ---------- | ----------------------------------------------------------------------------------- |
| `list`     | unbounded length; access elements by walking from head                              |
| `tuple`    | bounded length (typically only 2 or 3); access elements by position                 |
| `array`    | bounded length; access elements by index                                            |
| `Set`      | immutable collection of values with no duplicates according to a `compare` function |
| `Map`      | immutable collection of key/value pairs; access values by key                       |
| `Hashtbl`  | mutable collection of key/value pairs; access values by key                         |
| `record`   | fixed set of fields accessed by name                                                |

Each of these collection types are described in more detail below.
All but `Set`, `Map` and `Hashtbl` have a literal syntax.
Tuples separate elements with commas and all the others use semicolons.

- `tuple` - `(expr1, expr2, ...)`
- `list` - `[ expr1; expr2; ... ]`
- `array` - `[| expr1; expr2; ... |]`
- `record` - `{ k1 = expr1; k2 = expr2; ... }`

Also see these modules:

- <a href="https://v2.ocaml.org/api/Array.html" target="_blank">Array</a>
- <a href="https://v2.ocaml.org/api/Hashtbl.html" target="_blank">Hashtbl</a>
- <a href="https://v2.ocaml.org/api/List.html" target="_blank">List</a>
- <a href="https://v2.ocaml.org/api/Map.html" target="_blank">Map</a>
- <a href="https://v2.ocaml.org/api/Queue.html" target="_blank">Queue</a>
- <a href="https://v2.ocaml.org/api/Seq.html" target="_blank">Seq</a>
- <a href="https://v2.ocaml.org/api/Set.html" target="_blank">Set</a>
- <a href="https://v2.ocaml.org/api/Stack.html" target="_blank">Stack</a>

### Tuples

A tuple is an immutable, ordered collection of values whose types can differ.

To create a tuple, surround the elements in parentheses
and separate them with commas.
For example:

```ocaml
let t = (true, 3, "blue") in
(* Can use pattern matching to extract the values. *)
let (b, n, c) = t in
printf "b = %b, n = %d, c = %s\n" b n c
```

In practice it is rare for a tuple to contain more than three values.
Tuples with two elements are referred to as a "pair"
and those with three elements are referred to as a "triple".

The parentheses shown above for creating and pattern matching on a tuple
are optional, but are recommended for clarity.

The variable `t` above has the type `bool * int * string`
which is referred to as a "product type"
(based on tuples being similar to cartesian products).

Fun fact: The Greek word "aster" means "star",
so it makes sense to refer to the asterisk character as "star".

For pairs, the `fst` function returns the first element
and the `snd` function returns the second.
There are no built-in functions for operating on longer tuples.

Pattern matching can be used to get a specific element from a tuple.
The following code shows four ways to write a function that
returns the third element of a 4-element tuple.

```ocaml
let third_of_4 t = match t with _, _, c, _ -> c
let third_of_4 = function _, _, c, _ -> c
let third_of_4 t = let _, _, c, _ = t in c
let third_of_4 (_, _, v, _) = v

let t = ("alpha", "beta", "gamma", "delta") in
print_endline (third_of_4 t) (* gamma *)
```

You can think of tuples like records (described later)
where the fields are accessed by position rather than name.

### Lists

A list is an immutable, ordered collection of values
that all have the same type.
Lists are implemented as a variant type with two constructors
whose values form a singly linked list.
The first constructor is `[]` (pronounced "nil"),
which represents an empty list.
The second constructor is `::` (pronounced "cons", short for "construct")
which creates a new list by adding an element
to the beginning of an existing list.

To create an empty list, use the nil constructor.
For example:

```ocaml
let issues = []
```

To create a non-empty list, surround the elements in square brackets
and separate them with semicolons. For example:

```ocaml
let colors = ["red"; "green"; "blue"]
```

Using commas instead of semicolons is a common error.
When that is done, a list containing a single tuple is created.

A non-empty list is represented by a head that holds an element value
and a tail that holds the remainder which is another list that may be empty.

To create a new list by adding an element to the beginning of an existing list,
use the cons constructor.
The right side must be a list and
the left side must be an expression that evaluates to
the same type as elements in the list on the right.
As an operator, this is right-associative.
For example:

```ocaml
let new_list = element :: old_list
```

The `color` list above was written with syntactic sugar for the following
which is evaluated from right to left to construct the final list:

```ocaml
let colors = "red" :: "green" :: "blue" :: []
```

To create a new list by concatenating two lists, use the `@` operator.
For example:

```ocaml
let new_list = list1 @ list2
```

The type of a list is written as `t list` where `t` is the type of the elements.
For example, the type of a list of `float` values is `float list`.
The type of an empty list `[]` is `'a list`
where `'a` is a type variable that represents an unknown type.

List elements can themselves be lists, but all the elements
must then also be lists with the same element type.
The sublists are not required to have the same length.
For example:

```ocaml
let seasons = [
  ["spring"; "rain"];
  ["summer"; "heat"];
  ["fall"; "nice"];
  ["winter"; "cold"]
]
```

The type of `seasons` is `string list list`.

A `match` expression can be used to recursively process a list.
For example, the following code computes the sum of a list of integers.
By convention, the name `hd` is used for the head of a list
and the name `tl` is used for the tail.

Here are three ways to write a function that takes an `int list`
and returns the sum of the elements.

```ocaml
let rec sum list =
  match list with
  | [] -> 0
  | hd :: tl -> hd + sum tl

let rec sum = function
  | [] -> 0
  | hd :: tl -> hd + sum tl

let rec sum = List.fold_left (+) 0
```

The standard library provides many functions that operate on lists. See the
<a href="https://v2.ocaml.org/api/List.html" target="_blank">List</a> module.
Some highlights include the following:

| Function          | Description                                                                                                                   |
| ----------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `List.exists`     | determines if a list contains at least one element that satisfies a predicate function                                        |
| `List.filter`     | creates a new list from the elements in an existing list that satisfy a predicate function                                    |
| `List.find`       | returns the first element that satisfies a predicate or raises `Not-found`                                                    |
| `List.find_opt`   | returns an `Option` that contains the first element that satisfies a predicate function                                       |
| `List.fold_left`  | reduces a list to a single value by applying an accumulator function from left to right                                       |
| `List.fold_right` | reduces a list to a single value by applying an accumulator function from right to left                                       |
| `List.for_all`    | determines if every element in a list satisfies a predicate function                                                          |
| `List.hd`         | returns the head of a list                                                                                                    |
| `List.iter`       | iterates over a list in a way that is useful when the function passed to it has a side effect and a result list is not needed |
| `List.length`     | returns the length of a list                                                                                                  |
| `List.map`        | creates a new list containing elements that are computed by passing each element in an existing list to a given function      |
| `List.map2`       | similar to `List.map`, but it operates on two lists, passing corresponding elements from each to a given function             |
| `List.mem`        | returns a `bool` that indicates whether a given value is a member                                                             |
| `List.nth`        | takes a list and an index and returns the list element at the index                                                           |
| `List.sort`       | returns a sorted list                                                                                                         |
| `List.sort_uniq`  | returns a sorted list with no duplicates                                                                                      |
| `List.tl`         | returns the tail of a list                                                                                                    |

Since tuple elements are separated by commas and list elements are
separated by semicolons, a list of tuples can be written as follows:

```ocaml
["alpha", 1; "beta", 2; "gamma", 3]
```

This evaluates to the following list of tuples:

```ocaml
[("alpha", 1); ("beta", 2); ("gamma", 3)]
```

The following code demonstrates using the
`List` module functions `map`, `filter`, and `fold_left`.
See the section "Pretty Printing" for details on `[@@deriving show]`.

```ocaml
open Printf

type int_list = int list
[@@deriving show]

let numbers = [4; 1; 9; 7; 2]

let doubled = List.map (fun x -> x * 2) numbers

let even_numbers = List.filter (fun x -> x mod 2 = 0) numbers

let sum = List.fold_left (+) 0 numbers

let () =
  print_endline (show_int_list doubled); (* [8; 2; 18; 14; 4] *)
  print_endline (show_int_list even_numbers); (* [4; 2] *)
  printf "sum = %d\n" sum (* sum = 23 *)
```

The following code finds the maximum value in a list of numbers.

```ocaml
let rec list_max (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
  | h :: t -> (
      match list_max t with None -> Some h | Some m -> Some (max h m))

let () =
  let numbers = [ 1; 13; 4; 9 ] in
  let max = list_max numbers in
  match max with
  | None -> print_endline "empty list"
  | Some max -> print_int max
```

Here is an easier way to find the maximum value in a list of numbers
if we can assume the list is not empty.

```ocaml
let max_int x y = if x > y then x else y
let maximum =
  first = List.hd numbers in
  List.fold_left max_int first numbers
```

The `fold_left` and `fold_right` functions both take three arguments.
But the order of the last two arguments differs.
The first argument in both is a function that takes a value and an accumulator,
but the order of those arguments differs.
For example:

```ocaml
let numbers = [ 1; 2; 3 ]

let () =
  let sum = List.fold_left (fun acc n -> acc + n) 0 numbers in
  Printf.printf "fold_left result is %d\n" sum;

  let sum = List.fold_right (fun n acc -> acc + n) numbers 0 in
  Printf.printf "fold_right result is %d\n" sum
```

The following code sorts a list of strings:

```ocaml
["red"; "green"; "blue"] |> List.sort compare
```

The standard library doesn't provide functions to take and drop
a number of elements from a list, but the `Base` module does.
See the `take` and `drop` functions described at
<a href="https://ocaml.org/p/base/latest/doc/Base/List/"
target="_blank">List</a>.

### Association Lists

An association list is list of tuple pairs
where the first value in each tuple is treated as a key
and the second is treated as an associated value.
Association lists are used in place of `Map` and `Hashtbl` collections
when there are a small number of key/value pairs.
This is because lookup in an association list can be slow
where there are a large number of key/value pairs.

The `List` module provides the following functions
for operating on association lists.

| Function       | Description                                                                            |
| -------------- | -------------------------------------------------------------------------------------- |
| `assoc`        | returns the value associated with a given key or raises `Not_found`                    |
| `assoc_opt`    | returns the value associated with a given key in a `Some` or returns `None`            |
| `mem_assoc`    | returns a `bool` indicating if a given key is present                                  |
| `remove_assoc` | returns a new association list where the pair with a given key is removed              |
| `assq`         | same as `assoc` but compares keys with physical rather than structural equality        |
| `assq_opt`     | same as `assoc_opt` but compares keys with physical rather than structural equality    |
| `mem_assq`     | same as `mem_assoc` but compares keys with physical rather than structural equality    |
| `remove_assq`  | same as `remove_assoc` but compares keys with physical rather than structural equality |

For example:

```ocaml
(* Create an association list. *)
let al1 = [ ("red", "FF0000"); ("green", "00FF00"); ("blue", "0000FF") ]

(* Create a new association list from al1
   where the "green" key/value pair is removed. *)
let al2 = List.remove_assoc "green" al1

(* Create a new association list from al1
   where the "yellow" key/value pair is added. *)
let al3 = ("yellow", "FFFF00") :: al2

let () =
  (* Get the value for the "red" key in al1. *)
  assert (List.assoc "red" al1 = "FF0000");
  (* Determine if the key "red" is present in al1. *)
  assert (List.mem_assoc "red" al1);
  (* Determine if the key "purple" is present in al1. *)
  assert (not (List.mem_assoc "purple" al1));
  (* Determine if the key "yellow" is present in al3. *)
  assert (List.mem_assoc "yellow" al3)
```

### Arrays

An array is a mutable, ordered collection of values
that all have the same type.
Its length is fixed.

To create an array, surround the elements in square brackets
that have vertical bars inside them and separate them with semicolons.
For example:

```ocaml
let colors = [| "red"; "green"; "blue" |]
```

To get an element from an array, follow it with a dot and
a zero-based index in parentheses (odd syntax)
or use the `Array.get` function. For example:

```ocaml
let color = colors.(1) (* "green" *)
OR
let color = Array.get colors 1
```

To modify an array element, use the `<-` operator or the `Array.set` function.
For example:

```ocaml
colors.(1) <- "yellow"
OR
Array.set colors 1 "yellow"
```

The type of an array is written as `t array`
where `t` is the type of the elements.
For example, the type of an array of `float` values is `float array`.

An empty array is written as `[||]` and has the type `'a array`
to indicate that the type of its elements is unknown.

The standard library `Array` module provides
many functions that operate on arrays. See the
<a href="https://v2.ocaml.org/api/Array.html" target="_blank">Array</a> module.
Some highlights include the following:

| Function           | Description                                                                                                                                         |
| ------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Array.append`     | creates a new array by concatenating two arrays                                                                                                     |
| `Array.concat`     | creates a new array by concatenating all the arrays in a list                                                                                       |
| `Array.exists`     | determines if an array contains at least one element that matches a given predicate function                                                        |
| `Array.for_all`    | determines if every element in an array matches a given predicate function                                                                          |
| `Array.iter`       | takes a function and an array; iterates over the array, passing each element the function; doesn't return a value, so this is used for side effects |
| `Array.length`     | returns the length of a given array                                                                                                                 |
| `Array.fold_left`  | reduces an array to a single value by applying an accumulator function from left to right                                                           |
| `Array.fold_right` | reduces an array to a single value by applying an accumulator function from right to left                                                           |
| `Array.sub`        | creates a new array that is a subset of an existing array                                                                                           |
| `Array.to_list`    | creates a new list that contains the same elements as a given array                                                                                 |
| `Array.of_list`    | creates a new array that contains the same elements as a given list                                                                                 |
| `Array.map`        | creates a new array containing elements that are computed by passing each element in an existing array to a given function                          |

### Sets

A `Set` is an immutable, ordered collection of values
that all have the same type with no duplicates.
It it implemented with a balanced binary tree.

The following code demonstrates creating and using two set types,
one for `int` values and one for `dog` record values.

```ocaml
open Printf

module IntSet = Set.Make (struct
  type t = int

  (* Int. below can be inferred. *)
  let compare = Int.compare
end)

type dog = { name : string; breed : string }

module DogSet = Set.Make (struct
  type t = dog

  (* String. below can be inferred. *)
  let compare a b = String.compare a.name b.name
end)

let () =
  let intSet = IntSet.of_list [ 3; 5; 3 ] in
  IntSet.iter (fun n -> print_endline (string_of_int n)) intSet;

  let dogSet =
    DogSet.(
      empty (* returns an empty Set *)
      |> add { name = "Comet"; breed = "Whippet" }
      |> add { name = "Oscar"; breed = "GSP" }
      |> add { name = "Comet"; breed = "Greyhound" })
  in
  DogSet.iter (fun dog -> printf "%s is a %s.\n" dog.name dog.breed) dogSet
```

### Records

A record an immutable collections of named fields.
The names of record types, like all type names,
must begin with a lowercase letter.
The fields can have different types, including other record types.
Records are similar to structs in other languages.

The following code defines a record type
that describes an item available for purchase.

```ocaml
type item = {
  description : string;
  mutable price : int;
  weight : float;
}
```

The field names must begin with a lowercase letter.

The field values are immutable by default, but can be made mutable
by adding the `mutable` keywored before their field name.

To create a record, supply values for each of the fields
inside curly braces in any order. For example:

```ocaml
let my_item : item = { description = "milk"; price = 350; weight = 1. }
```

It is not necessary to specify the type of the record variable.
But the field names and values must match some existing record type
and no extra fields can be present.

Field values can be omitted if there is a
variable in scope with the same name as the field.
This is called "field punning".
For example:

```ocaml
let description = "milk" and price = 350 and weight = 1. in
let my_item = { description; price; weight } in
...
```

To access a field value in a record, use dot syntax.
For example:

```ocaml
let p = my_item.price
```

A field name must follow the dot,
not an expression that evaluates to a field name.
To lookup values based on an expression, use a `Map` instead of a record.

Fields can also be pattern matched. For example:

```ocaml
match my_item with
| {description=d; price=p; weight=w} ->
    (* can use d, p, and w here *)
```

Alternatively there is a shorthand for
setting variables whose names match the field names.

```ocaml
match my_item with
| {description; price; weight} ->
    (* can use description, price, and weight here *)
```

To modify a mutable field in a record, use the `<-` operator.
For example:

```ocaml
milk.price <- 400
```

To create a new record by copying fields from an existing one
and using different values for some of its fields,
use the `with` keyword. For example:

```ocaml
let new_milk = { milk with price = 325; weight = 1.5 }
```

The following code demonstrates creating and using a list of records.

```ocaml
open Printf

type item = {
  description : string;
  mutable price : int;
  weight : float;
}

let eggs = { description = "eggs"; weight = 0.4; price = 275 }
let milk : item = { description = "milk"; weight = 1.; price = 350 }
let new_milk = { milk with price = 325; weight = 1.5 }
let items = [ eggs; new_milk ]

let print_item item =
  printf "%s: $%d (%f lbs)\n" item.description item.price item.weight

let () =
  milk.price <- 400;
  List.iter print_item items;
  let total = List.fold_left (fun acc item -> acc + item.price) 0 items in
  printf "Total: $%d\n" total
```

### Maps

A Map is an immutable collection of key/value pairs.

The standard library `Map` module provides
many functions that operate on maps.
Each of them return a new map that uses structual sharing
to avoid making a copy of the whole map. See the
<a href="https://v2.ocaml.org/api/Map.html" target="_blank">Map</a> module.

To create a map type that uses keys of a given type, call `Map.Make`.
For example, the following creates a map type where the keys are strings.

```ocaml
module StringMap = Map.Make (String)
```

Module names, like `StringMap` above, must start with an uppercase letter.

To create an instance of this map type that starts empty,
call the `empty` function on the map type. For example:

```ocaml
let dog_map = StringMap.empty
```

To create a new map from an existing one adding one key/value pair,
call the `add` function on the map type
passing it a key, a value, and an existing map. For example:

This code uses the
<a href="https://erratique.ch/software/uuidm/doc/Uuidm/index.html"
target="_blank">Uuidm</a> module to generate uuids
that are used as keys in the map.
It must be installed with `opam install uuidm`.

```ocaml
let generate_uuid () = Uuidm.(v `V4 |> to_string)
let uuid = generate_uuid () in
let new_dog_map = StringMap.add uuid dog dog_map
```

To create an instance of this map that that starts with some key/value pairs,
use the `of_seq` function on the map type as shown in the code below.

```ocaml
let make_dog name breed =
  let uuid = generate_uuid () in
  { id = uuid; name; breed }
let comet = make_dog "Comet" "whippet"
let oscar = make_dog "Oscar" "GSP"
let dog_map = StringMap.of_seq @@ List.to_seq [
  (comet.id, comet);
  (oscar.id, oscar);
]
```

To find a value in a map by its key,
use the `find_first_opt` function on the map type.
This returns an `Option` because it's possible the key will not be found.
For example:

```ocaml
let dog_opt = StringMap.find_first_opt (fun key -> key = "some-key") dog_map
```

To change the value for a given key, use the `update` function on the map type.

```ocaml
let new_dog_map = dog_map |> StringMap.update "some-key" new_value
```

To create a new map from an existing one where one key/value pair is removed,
use the `remove` function on the map type. For example:

```ocaml
let new_dog_map = dog_map |> StringMap.remove "some-key"
```

The following code demonstates all the operations described above.

```ocaml
module StringMap = Map.Make (String)

let () =
  let map =
    StringMap.empty |> StringMap.add "a" "apple" |> StringMap.add "b" "banana"
    |> StringMap.update "a" (Option.map (fun _ -> "apricot"))
    |> StringMap.remove "b"
  in
  let a_fruit = StringMap.find_opt "a" map in
  match a_fruit with
  | None -> print_endline "No such fruit"
  | Some fruit -> print_endline fruit
```

The following code demonstrates using a `Map` to store a collection
of dog descriptions and prints information about each dog.

```ocaml
open Printf

let generate_uuid () = Uuidm.(v `V4 |> to_string)

module StringMap = Map.Make (String)

type dog = { id : string; name : string; breed : string }

let add_dog map name breed =
  let uuid = generate_uuid () in
  let dog = { id = uuid; name; breed } in
  StringMap.add uuid dog map

let print_dog _ dog = printf "%s) %s is a %s.\n" dog.id dog.name dog.breed

let () =
  let dog_map = StringMap.empty in
  let dog_map = add_dog dog_map "Comet" "whippet" in
  let dog_map = add_dog dog_map "Oscar" "GSP" in
  StringMap.iter print_dog dog_map
```

### Hashtbl

A <a href="https://ocaml.org/docs/hash-tables" target="_blank">Hashtbl</a>
is similar to a `Map`, but it is mutable.

The `Hashtbl` module supports the following functions:

| Function                     | Description                                                       |
| ---------------------------- | ----------------------------------------------------------------- |
| `Hashtbl.clear`              | removes all the key/value pairs and retains the bucket size       |
| `Hashtbl.copy`               | returns a copy of a `Hashtbl`                                     |
| `Hashtbl.create`             | creates a `Hashtbl` instance                                      |
| `Hashtbl.add`                | adds a key/value pair                                             |
| `Hashtbl.filter_map_inplace` | can modify the value for each key or remove it                    |
| `Hashtbl.find`               | returns the first value for a given key or raises `Not-found`     |
| `Hashtbl.find_all`           | returns a list of all values for a given key                      |
| `Hashtbl.find_opt`           | returns an `Option` that contains the first value for a given key |
| `Hashtbl.fold`               | computes a single value from all the key/value pairs              |
| `Hashtbl.iter`               | iterates over all the key/value pairs                             |
| `Hashtbl.length`             | returns the number of key/value pairs                             |
| `Hashtbl.mem`                | returns a boolean indicating whether a given key is a member      |
| `Hashtbl.remove`             | removes one key/value pair for a given key                        |
| `Hashtbl.replace`            | replaces the value for a given key                                |
| `Hashtbl.reset`              | removes all the key/value pairs and shrinks the bucket size       |

Like with any module, if you call `open Hashtbl`
then these function can be called without the `Hashtbl.` prefix.

To create a `Hashtbl` instance, call the `Hashtable.create` function
passing it an estimate for the number of key/value pairs that will added.
It can expand to hold more, but providing a good estimate
can make add entries more efficient.

```ocaml
let my_hash = Hashtbl.create 10
```

Initially the hashtable does not know the type of the keys and values.
Once the first entry is added, those types will be fixed
and all subsequent entries must use the same types for their keys and values.

The first argument to all the remaining functions is a `Hashtbl` instance,
with the exception of the `iter` function where it is the last argument.

The `add` function can add multiple values for the same key,
and can even add the same value multiple times.
The previous values are not overwritten.

```ocaml
Hashtbl.add my_hash "whippet" "Rudy"
Hashtbl.add my_hash "whippet" "Dasher"
Hashtbl.add my_hash "whippet" "Comet"
```

To have only one value for each key, use the `replace` function instead.
The `replace` function adds a new key or replaces an existing one.

```ocaml
Hashtbl.replace my_hash "whippet" "Comet"
```

The `find` function raises a `Not_found` exception if the key is not found.

```ocaml
let name = Hashtbl.find my_hash "whippet"
```

The `find_all` function returns an empty list if the key is not found.

```ocaml
let names = Hashtbl.find_all my_hash "whippet"
```

The `remove` function doesn't complain if the key being removed doesn't exist.

```ocaml
Hashtbl.remove my_hash "whippet"
```

The following code demonstrates creating a `Hashtbl`
that holds a collection of dogs.
The keys are `dog` ids and the values are `dog` records.

```ocaml
open Printf

type dog = { id : string; name : string; breed : string }

(* 10 is an estimate for the number of entries that will be added. *)
let dog_map = Hashtbl.create 10

let generate_uuid () = Uuidm.(v `V4 |> to_string)

let add_dog name breed =
  let id = generate_uuid () in
  Hashtbl.add dog_map id { id; name; breed }

let print_dog dog = printf "%s is a %s (id=%s).\n" dog.name dog.breed dog.id

let () =
  add_dog "Comet" "Whippet";
  add_dog "Oscar" "GSP";
  (* The function passed to `iter` takes a key and value from the Hashtbl. *)
  Hashtbl.iter (fun _ dog -> print_dog dog) dog_map
```

## Functions

OCaml functions are first-class.
They can take other functions as arguments and can return new functions.

Function definitions must appear before any calls to them.

OCaml functions cannot be overridden based on
their number of parameters or parameter types.

The syntax `a b c` means calling function `a` with the arguments `b` and `c`.

Another way to write this is to use the "reverse function application" operator
`|>` which is intended to look like a right pointing triangle.
In fact, in fonts that support ligatures, it is displays as exactly that.
Using this, the equivalent of `a b c` is c |> b |> a.
This operator is typically used with functions that take a single argument.
The following code demonstrates two ways to compute
the squared value of the sine of an angle.

The body of a function definition, the part after the `=`,
is a single expression.
An expression can be a semicolon-separated set of expressions,
but typically there is only one.
When there are more than one, all but the final expression
serve to cause side effects and must have the unit type (no value).
Typically these serve to assign the results of intermediate computations
to variables or perform debug printing.
The result of a function call is the value of the final expression.

```ocaml
let square x = x *. x
let angle = 0.78 (* radians *)
let result1 = square (sin angle)
(* The following is equivalent expression that uses
   the reverse function application operator. *)
let result2 = angle |> sin |> square
```

The OCaml syntax for defining and calling functions
is inspired by lambda calculus.

In order to call a function that takes no arguments, such as `print_newline`,
it must be "passed" the "unit" value `()`.
Without this it is just a reference to the function and not a call to it.

Functions that only produce side effects do not return anything.
Calls to them are expressions with "unit type", represented by `()`.
These are like "statements" in other languages.
Some call them "effectful expressions".

There are two ways to return early from a function ...
using the `exit` (not typically used)
or `raise` keywords (raises an exception).

Anonymous functions (aka lambdas) are defined using the `fun` function.
For example the following function
takes two `int` arguments and returns an `int`.
Note how no parentheses are required and
the parameters are just separated by spaces.

```ocaml
fun a b -> a + b
```

It is not necessary to specify these types.
They are inferred from the function expression `a + b`
based on the fact that the `+` operator only
operates on `int` values and returns an `int` value.

The types can be specified if desired.
The main reason to do so is to add documentation so readers of the code
don't have to examine the function body to determine the types.
To specify the parameter types and the return type,
the function above can be written as the following.

```ocaml
fun (a : int) (b : int) : int -> a + b
```

If this function definition is entered in a REPL, followed by `;;`,
the output will be `- : int -> int -> int = <fun>`.
The dash at the beginning indicates that the value does not have a name.
The first and second occurrences of `int` are the types of the two parameters.
The last `int` is the return type of the function.
The `<fun>` after the `=` represents the bytecode for the function
which cannot be printed.

When a function is called with fewer arguments than it has parameters,
a new function is returned that is the result of partial application.
That is why one arrow (`->`) for each parameter appears in the output.

In fact, the function definition above is just syntactic sugar
for the following which only defines single-parameter function.

```ocaml
fun a -> (fun b -> a + b))
```

Despite not having a name, this function
can be invoked by surrounding it in parentheses.
For example, the following evaluates to `5`.
Note how no parentheses are required around the arguments
which are just separated by a space.

```ocaml
(fun a b -> a + b) 2 3
```

Let's give a name to this function and invoke it in a couple of ways.

```ocaml
let add = (fun a b -> a + b) in
print_int (add 1 2); (* 3 *)

(* Shorthand for previous way of defining this function *)
let add a b = a + b in
print_int (add 1 2); (* 3 *)

(* Even shorter with point-free style! *)
let add = (+)
```

In the call to `print_int` above, parentheses are needed around `add 1 2`
so that is evaluated before the `print_int` function is called.

Let's use partial application to create a new function
that only takes a single number and adds 5 to it.

```ocaml
let add5 = (add 5) in
print_int (add5 2); (* 7 *)
```

In the expression `e1 e2 e3`, `e1` must evaluate to a function
and it is passed the values of `e2` and `e3`.
If `e2` or `e3` are not primitive values or variables,
add parentheses around those expressions so they are
evaluated before the function call to `e1` is evaluated.
For example, our `add` function above can be called as follows:

```ocaml
add (2 * 3) (4 + 5) (* 6 * 9 = 54 *)
```

Labeled parameters allow them to be specified by name in calls.
A function can have a mixture of labeled and unlabeled parameters.
Unlabeled parameters are positional and must be
passed in the order they are specified.
Labeled parameters can be specified in any order
and can be mixed into the unlabeled parameters.

To declare a labelled parameter, use the syntax `~arg_name:param_name`
where `arg_name` is the name used in callsx
and `param_name` is the name used in the function implementation.
If `arg_name` is the same as `param_name`,
this can be shortened to just `~arg_name`.

To pass an labeled argument, use the syntax `~arg_name:value`.
For example, `~maximum:100`.

```ocaml
open Printf

(* This doesn't use labeled parmeters. *)
let rectangle_area1 length width = length *. width

(* This does use labeled parmeters.
   The name between ~ and : is what will be used in calls.
   The name after the : is what will be used in the function body. *)
let rectangle_area2 ~length:l ~width:w = l *. w

let () =
  printf "area1 = %f\n" (rectangle_area1 8.5 11.);
  printf "area2 = %f\n" (rectangle_area2 ~length:8.5 ~width:11.)
```

Labeled parameters can specify default values which makes them optional.
Use the syntax `?arg-name:(param-name = default_value)`.
If `arg_name` is the same as `param_name`,
this can be shortened to just `?(arg_name = default_value)`.
Optional labeled parameters must appear before the non-optional parameters.

Functions that have any optional parameters
must have at least one positional parameter.
This is required because OCaml does not surround arguments with parentheses
and so needs a way to know when the last argument has been reached.
When no positional parameters are needed, specify `()` as the last one.
In this case calls to the function must also end with `()`.

For example:

```ocaml
open Printf

(* When using optional parameters, there must be at least one
   that is not optional.  Adding `()` satisifies this. *)
let greet ?(name = "World") ?(suffix = "!") () =
  printf "Hello, %s%s\n" name suffix

let product ?(a = 1.) b = a *. b

let sum2 ?alpha:(first = 1) ?(beta = 2) () = first + beta

let sum3 ?alpha:(first = 1) ?(beta = 2) ~gamma () = first + beta + gamma

let () =
  (* In calls to functions with no required parameters, include `()`.
     It doesn't matter where it appears in the argument list. *)
  greet ~name:"Mark" ~suffix:"." (); (* Hello, Mark. *)
  greet () ~name:"Mark"; (* Hello, Mark! *)
  greet () (* Hello, World! *)

  printf "%f\n" (product ~a:8.5 2.); (* 17 *)
  printf "%f\n" (product 8.5); (* 8.5 *)

  printf "%d\n" (sum2 ~alpha:3 ~beta:4 ()); (* 7 *)
  printf "%d\n" (sum2 ~alpha:3 ()); (* 5 *)
  printf "%d\n" (sum2 ~beta:4 ()); (* 5 *)
  printf "%d\n" (sum3 ~gamma:5 ()) (* 8 *)
```

Recursive functions must be defined with `let rec`. For example:

```ocaml
let rec factorial n =
  if n < 0 then
    (* raises an exception *)
    failwith "factorial is not defined for negative numbers"
  else if n = 0 then 1
  else n * factorial (n - 1)
```

The `failwith` keyword is sometimes used in place of
code that is not ready to be written.
For example, `failwith "TODO"`.

Function parameters can use pattern matching to extract elements from tuples.
This can also be done for lists and arrays, but those
require special handling due to non-exhastive matching.
The reason is that by definition tuple types have a known size,
but list and array types do not.
Recall that:

- The type of the tuple `(1, 2, 3)` is `int * int * int` which has a length of 3.
- The type of the list `[1; 2; 3]` is `int list` which does not specify a length.
- The type of the array `[|1; 2; 3|]` is `int array` which does not specify a length.

The following code demonstrates ways to use
pattern matching on tuples, lists, and arrays.

```ocaml
open Printf

(* This only handles tuples of length 4. *)
let tuple2of4 (_, e, _, _) = e
(* alternate implementation *)
(* let tuple2of4 tuple = match tuple with a, b, c, d -> b *)

(* This only handles lists of length 4. *)
let list2of4 = function
  | [ _; e; _; _ ] -> e
  | _ -> failwith "list must have length 4"

(* This only handles arrays of length 4. *)
let array2of4 = function
  | [| _; e; _; _ |] -> e
  | _ -> failwith "array must have length 4"

let () =
  let t = ("a", "b", "c", "d") in
  (* This pattern matches a tuple in a variable declaration. *)
  let _, second, third, _ = t in
  printf "second in tuple is %s\n" second;
  printf "third in tuple is %s\n" third;
  printf "second in tuple is %s\n" (tuple2of4 t);

  let l = [ "a"; "b"; "c"; "d" ] in
  printf "second in list is %s\n" (list2of4 l);

  let a = [| "a"; "b"; "c"; "d" |] in
  printf "second in array is %s\n" (array2of4 a)
```

A common OCaml idiom to hide the use of an accumulator parameter
is to nest a function inside another.
For example, see the `visit` function that is
nested inside the `reverse_int_list` function here.
This uses tail recursion so recursion does not cause the call stack to grow.

```ocaml
let reverse_int_list l =
  let rec visit acc l =
    match l with [] -> acc | hd :: tl -> visit (hd :: acc) tl
  in
  visit [] l

let rec print_int_list l =
  match l with
  | [] -> ()
  | hd :: tl ->
      print_int hd;
      print_char ' ';
      print_int_list tl

let () =
  let numbers = [ 1; 2; 3; 4; 5 ] in
  let rev = reverse_int_list numbers in
  print_int_list rev;
  print_newline ()
```

OCaml does not make it easy to write variadic functions,
which are functions that take a variable number of arguments.
One approach is to take a list, but that
requires all the values to have the same type.
Another approach is to use a generalized algebraic data type (GADT),
but that introduces complexity.

### Function Composition

OCaml does not define an operator for function composition,
but you can define one.
The following code demonstrates four ways to
define a new function that composes existing functions.

```ocaml
let add1 x = x + 1
let square x = x * x
let add1square x = x |> add1 |> square
let add1square' x = square @@ add1 x
let ( << ) f g x = f (g x)
let add1square'' = square << add1
let ( >> ) f g x = g (f x)
let add1square''' = add1 >> square

let () =
  assert (add1square 2 = 9);
  assert (add1square' 2 = 9);
  assert (add1square'' 2 = 9);
  assert (add1square''' 2 = 9)
```

The `Base` module defines the `Fn.compose` function
which composes a pair of functions from right to left.
It can be used as follows:

```ocaml
open Base
let add1square = Fn.compose square add1
```

## Modules

A module provides a namespace for a set of related values
that can be types, exceptions, constants, and functions.
Constants and functions are defined with `let` definitions.
Module names always begin with an uppercase letter
and they use CamelCase by convention.

Modules often describe a data type, ways to create instances,
and operations on instances.
Examples of such modules defined in the standard library include
`Array`, `Char`, `Hashtbl`, `List`, `Map`, `Option`,
`Queue`, `Result`, `Set`, `Stack`, and `String`.
Standard library modules that do not describe a data type include
`Printf`, `Random`, `Sys`, and `Unix`.

Modules that define a data type do not
define methods that are called on instances.
Instead they define functions to which
an instance and other arguments are passed.
For example, the `List` module defines the `map` function
that is used as follows:

```ocaml
let numbers = [4; 1; 9; 7; 2]
let doubled = List.map (fun x -> x * 2) numbers
```

Every `.ml` source file defines a module.
For example, the file `demo.ml` defines the module `Demo`.
These can contain the following:

- `open` statements that make the names in another module
  available without a module name prefix
- `include` statements that include values defined in another module
  as if they were defined in the current file.
- `type` definitiions
- `exception` definitions
- `let` definitions that define constants and functions
- `module` definitions that define submodules

A package is a collection of related libraries and
a library is a collection of related modules.

Modules provides a way to identify multiple values
that happen to have the same name.
For example, the `List` and `Array` modules both define the `map` function.

Modules also serve to hide complexity.

A submodule can be defined with the syntax `module ModuleName = struct ... end`.

The following code defines a module that contains functions for
converting temperature values and demonstrates using it.

```ocaml
module Temperature = struct
  let c_of_f fahrenheit = (fahrenheit -. 32.) *. (5. /. 9.)
  let f_of_c celsius = (9. /. 5. *. celsius) +. 32.
end

let () =
  print_float (Temperature.c_of_f 100.);
  (* 37.8. *)
  print_newline ();
  print_float (Temperature.f_of_c 0.);
  (* 32.0 *)
  print_newline ()
```

Another way to define a module is to describe its interface in a `.mli` file
and it's implementation in a `.ml` file with the same name.
This approach is describe in the "Signatures" section below.

Modules cannot be used like values. They cannot be assigned to a variable,
passed to a function, or returned from a function.

Consider the following code which uses
values from the `Hashtbl` module multiple times.
There is no need to "import" the module
as is done in many other programming languages.

```ocaml
let dog_table = Hashtbl.create 10
let () =
  Hashtbl.add dog_table "Comet" "Whippet";
  Hashtbl.add dog_table "Oscar" "GSP";
  Hashtbl.iter
    (fun name breed -> Printf.printf "%s is a %s.\n" name breed)
    dog_table
```

There are multiple ways to avoid repeating a module name
every time the values it defines are referenced.
The options include:

- global `open`

  Adding `open Hashtbl` at the top of a source file
  allows unprefixed names to be used anywhere in the source file.
  This works fine as long is the names don't collide with
  names defined in other modules that also use a global open.

  ```ocaml
  open Hashtbl
  let dog_table = create 10
  let () =
    add dog_table "Comet" "Whippet";
    add dog_table "Oscar" "GSP";
    iter
      (fun name breed -> Printf.printf "%s is a %s.\n" name breed)
      dog_table
  ```

- local `option`

  For example, `let open Hashtbl in` narrows the scope in which
  unprefixed names can be used to the expression that follows.

  ```ocaml
  let dog_table = Hashtbl.create 10
  let () =
    let open Hashtbl in
    add dog_table "Comet" "Whippet";
    add dog_table "Oscar" "GSP";
    iter
      (fun name breed -> Printf.printf "%s is a %s.\n" name breed)
      dog_table
  ```

- dot and parentheses

  For example, `Hashtbl.(...)` narrows the scope in which
  unprefixed names can be used to the expression inside the parentheses.

  ```ocaml
  let dog_table = Hashtbl.create 10
  let () =
    Hashtbl.(
      add dog_table "Comet" "Whippet";
      add dog_table "Oscar" "GSP";
      iter
        (fun name breed -> Printf.printf "%s is a %s.\n" name breed)
        dog_table
    )
  ```

Modules can be nested to create a hierachy of namespaces,
but it seems this is rarely used.

Circular dependencies between modules are not allowed.

### Stdlib Module

There is an implicit `open` for the module `Stdlib`,
so all of its members can be accessed without the module name prefix.

Commonly used members include:

- many operators
- many math functions
- many type conversion functions
- input/output functions
- `ref` and operators on them
- `result` variant type
- `exit` and `at_exit`
- a large number of standard library modules
- tuple functions for pairs, `fst` and `snd`
- `ceil` and `floor`
- `failwith`, `invalid_arg`, and `raise`
- many exceptions
- `compare`
- `max` and `min`
- `nan`
- `not`
- debugging dunderbars (ex. `__FILE__`, `__FUNCTION__`, and `__LINE__`)

The following code demonstrates using the debugging dunderbar values.

```ocaml
let log file fn line =
  Printf.printf "file: %s; function: %s; line: %d\n" file fn line

let demo () =
  print_endline "entered demo";
  log __FILE__ __FUNCTION__ __LINE__;
  print_endline "exiting demo"

let () = demo ()
```

### Printf Module

The Printf module defines many functions that
produce string output using a format string.

Perhaps the most commonly used function of these is `printf`.
It takes a format string and a number of additional arguments
equal to the number of placeholders in the format string.
Commonly used placeholders include

- `%s` for `string` values
- `%d` for `int` values
- `%f` for `float` values including trailing zero decimal places
- `%F` for `float` values excluding training zero decimal places
- `%.2f` for `float` values with two decimal places
- `%B` for `bool` values (outputs "true" or "false")

Ending the format string with `%!` causes it to flush the output buffer.

## Signatures

The concept of "interfaces" in other programming languages
is supported in OCaml with "signatures".
These can be used to specify that multiple modules
support the same set of functions.

A signature is defined with the syntax
`module type ModuleTypeName = sig ... end`.
It can contain the following kinds of specifications:
`type`, `exception`, `val`, and `module type`.

The following code demonstrates defining a signature
and two modules that conform to it.

```ocaml
open Printf

module type Shape = sig
  type t

  val area : t -> float
end

(* float represents radius *)
module Circle : Shape with type t := float = struct
  let area radius = Float.pi *. radius *. radius
  (* Any extra values, including functions, define here
     that are not described in the `Shape` signature
     will be private to this module (not exposed outside).
     We say the module is "sealed". *)
end

(* float * float represents length and width *)
module Rectangle : Shape with type t := float * float = struct
  let area (length, width) = length *. width
end

let () =
  let radius = 5. in
  let circle_area = Circle.area radius in
  printf "Circle area: %.2f\n" circle_area;

  let length = 4. and width = 6. in
  let rectangle_area = Rectangle.area (length, width) in
  printf "Rectangle area: %.2f\n" rectangle_area
```

Files with the `.mli` extension contain signatures
of functions defined in the corresponding `.ml` file.
This pair of files is called a "compilation unit".
Code that uses this unit can only access what the `.mli` file describes.

For example, see the definitions of the standard library modules
in the GitHub repository for OCaml at
<a href="https://github.com/ocaml/ocaml/tree/trunk/stdlib"
target="_blank">ocaml/stdlib</a>.
The file `list.mli` defines the signatures for the `List` module
and the file `list.ml` defines the implementations.

The `is_empty` function has the signature

```ocaml
val is_empty : 'a list -> bool
```

and the implementation

```ocaml
let is_empty = function
  | [] -> true
  | _ :: _ -> false
```

As another example, here is the file `math.mli`:

```ocaml
type point = float * float

(**
Computes the distance from one point to another.
@param p1 the first point
@param p2 the second point
@return the distance between them
*)
val distance : point -> point -> float
```

And here is the file `math.ml`:

```ocaml
(* This defines the "Math" module which is in the "Module_demo2" library. *)
type point = float * float

(* private function *)
let square x = x *. x

(* public function *)
let distance (x1, y1) (x2, y2) =
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  sqrt ((square dx) +. (square dy))
```

Note how the `point` type is define in both the `.mli` and `.ml` files.
This is typical. A way to avoid this duplication is described in
<a href="https://www.craigfe.io/posts/the-intf-trick"
target="_blank">The \_intf trick</a>.

Here is the file `main.ml` that uses the `Math` module defined above:

```ocaml
let () =
  let p1 = (0., 0.) in
  let p2 = (3., 4.) in
  let d = MyLibrary.Math.distance p1 p2 in
  assert (d = 5.)
```

The Language Server Processor (LSP) will display
the help text for the `distance` function
when hovering over the call to the function above
in an editor that is configured to use the LSP such as VS Code.

Files with the `.cmi` extension are compiled versions of `.mli` files and
files with the `.cmo` extension are compiled versions of `.ml` files.

TODO: Can an OCaml class implement a signature?

## Functors

TODO: Describe these.
See <a href="https://ocaml.org/docs/functors" target="_blank">Functors</a>.

## Exception Handling

The built-in type `exn` is an
<a href="https://v2.ocaml.org/manual/extensiblevariants.html"
target="_blank">extensible variant type</a>
that has constructors for all the built-in exceptions.
Examples include `Division_by_zero`, `Failure`,
`Invalid_argument`, and `Not_found`.
For a full list, see <a href="https://v2.ocaml.org/api/index_exceptions.html"
target="_blank">Index of exceptions</a>.

The `exception` keyword creates a custom exception
that is added as a constructor of the `exn` type.
Exceptions can optionally have an associated value of any type.
For example:

```ocaml
exception BadThingHappened
exception TemperatureTooHigh of float
```

The `raise` keyword raises a given exception.
For example:

```ocaml
raise BadThingHappened
raise (TemperatureTooHigh 99.9)
```

There are predefined functions that simplify raising common exceptions.  
`failwith "data not found"` is short for `raise (Failure "data not found")`.  
`invalid_arg "bad 1st arg"` is short for `raise (Invalid_argument "bad 1st arg")`.

The `try` keyword enables catching exceptions raised by an expression.
It is similar to the `match` keyword, but the branches must match on exceptions.
The value of each branch must have the same type.
If no branch matches a raised exception, the exception is re-raised.
For example:

```ocaml
open Printf

exception TemperatureCrazy
exception TemperatureHigh of float

let evaluate_temperature temp =
  if temp >= 120. then raise TemperatureCrazy;
  if temp >= 100. then raise (TemperatureHigh temp);
  if temp < 32. then "cold" else if temp < 75. then "hot" else "nice"

let report_temperature t =
  try printf "%.1f is %s\n" t (evaluate_temperature t) with
  | TemperatureCrazy -> printf "%.1f is a crazy temperature!\n" t
  | TemperatureHigh t -> printf "%.1f is too hot!\n" t

(* Alternative that does the same thing and
   is good for matching both exceptions and other values.
   let report_temperature t =
     match evaluate_temperature t with
     | exception TemperatureCrazy -> printf "%.1f is a crazy temperature!\n" t
     | exception TemperatureHigh t -> printf "%.1f is too hot!\n" t
     | s -> printf "%.1f is %s\n" t s
*)

let () =
  report_temperature 80.;
  (* "80.0 is nice" *)
  report_temperature 50.;
  (* "50.0 is nice" *)
  report_temperature 20.;
  (* "20.0 is cold" *)
  report_temperature 100.;
  (* "100.0 is too hot!" *)
  report_temperature 120. (* "120.0 is a crazy temperature!" *)
```

The built-in function `assert` takes a Boolean expression
and raises an `Assert_failure` if if evaluates to false.
This is good for verify conditions that must hold
in order for the program to run correctly.

## Source Files

The code in a source file is executed just as it would be in a REPL
if it were terminated by a double semicolon.

Every source file defines a module.
The identifiers it defines, including types, constants, and functions,
can be used in other source files.
The name of a module is inferred from its source file name
by uppercasing its first letter.
For example, the file `foo_bar.ml` defines the module `Foo_bar`.

Suppose this file defines the function `baz`.
To call this function in another source file, use `Foo_bar.baz`.
Alternatively, include an `open` statement to
allow all the identifiers in a given module
to be used in the current source file
without prefixing them with their module name.
If we have `open Foo_bar` then the `baz` function
can be used without the `Foo_bar.` prefix.

OCaml source files contain the following kinds of statements:

- `open` statements
- constants defined with `let` definitions
- functions defined with `let` definitions
- an optional "main" expression that begins with `let () =`

Here's an example of a file named `math.ml`
that defines a type, a constant, and some functions
that can be used by other source files.

```ocaml
type point2d = float * float

let pi = Float.pi

let add a b = a + b

let average numbers =
  let sum = List.fold_left (+) 0 numbers in
  let length = List.length numbers in
  float_of_int sum /. float_of_int length

(* This demonstrates adding types to parameters and
   specifying the return type, all of which can be inferred. *)
let distance ((x1, y1) : point2d) ((x2, y2) : point2d) : float =
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  sqrt ((dx *. dx) +. (dy *. dy))
```

Here is a "main" source file, typically named `main.ml`,
that uses things defined in `math.ml`.
It defines a constant, a function, and a expression that is executed
when this file is passed to the `ocaml` command.

```ocaml
open Math
open Printf

let my_constant = 7

let square x = x * x (* a function *)

(* Note the use of semicolons to separate
   the statements and expressions. *)
let () =
  (* This is a verbose way to print a value. *)
  print_string "my_constant = ";
  print_int my_constant;
  print_newline ();

  (* This is a more concise way to print a value. *)
  printf "pi = %f\n" pi; (* defined in math.ml *)

  (* This calls a function defined in this file. *)
  printf "square of %d = %d\n" my_constant (square my_constant);

  (* The remaining examples call functions defined in math.ml. *)
  let a = 1 and b = 2 and c = 3 in
  printf "sum of a and b = %d\n" (add a b);

  let numbers = [a; b; c] in
  let avg = average numbers in
  printf "average of a, b, and c = %f\n" avg;

  let p1 = (0., 0.) and p2 = (1., 1.) in
  let d = distance p1 p2 in print_float d
```

When these files appear in the `bin` directory of a Dune project,
the program can be run by entering `dune exec {project-name}`.

To run this outside of a Dune project, create an executable by entering
`ocamlopt math.ml main.ml -o demo` and run it by entering `./demo`.

The `module` keyword can be used to define a submodule.
For example, the file `math.ml` defines the module `Geometry`.
If it contains `module Geometry = struct ... end`
then the things it defines are in the module `Math.Geometry`.

For example, suppose in a Dune project
the file `lib/math.ml` contains the following:

```ocaml
let average numbers =
  let sum = List.fold_left (+.) 0. numbers in
  let length = List.length numbers in
  sum /. float_of_int length

module Geometry = struct
  let rectangle_area width height = width *. height
  let rectangle_perimeter width height = width *. 2. +. height *. 2.
end
```

In the file `bin/main.ml` this can be used as follows:

```ocaml
open Demo.Math
(* Can also add this: open Demo.Math.Geometry *)
open Printf

let () =
  let avg = average [5.2; 3.5] in
  printf "average = %f\n" avg;
  let area = Geometry.rectangle_area 5.2 3.5 in
  printf "area = %f\n" area
```

## Input/Output

The OCaml standard library provides many functions that read input.

The following functions read from `stdin`:

- `read_line` - raises `End_of_file` if there is no more to read
- `read_int_opt` - returns `None` if conversion fails
- `read_int` - raises `Failure "int_of_string"` if conversion fails
- `read_float_opt` - returns `None` if conversion fails
- `read_float` - raises `Failure "float_of_string"` if conversion fails

The OCaml standard library provides many functions that produce output.

The following functions write to `stdout`:

- `print_bytes`
- `print_char`
- `print_endline` - prints a string followed by a newline
- `print_float`
- `print_int`
- `print_newline` - prints only a newline
- `print_string`

The following functions write to `stderr`:

- `prerr_bytes`
- `prerr_char`
- `prerr_endline` - prints a string followed by a newline
- `prerr_float`
- `prerr_int`
- `prerr_newline` - prints only a newline
- `prerr_string`

The following program prompts for two numbers and outputs their product.

```ocaml
(* Make the printf function available. *)
open Printf;;

print_string "Enter the first number: ";
let num1 = read_float () in

print_string "Enter the second number: ";
let num2 = read_float () in

let product = num1 *. num2 in
printf "The product of %.2f and %.2f is %.2f\n" num1 num2 product;
```

The following program reads and prints all the lines in a text file
using a `while` loop.

```ocaml
let channel = open_in "BeverlyHillbillies.txt" in
try
  while true do
    (* input_line reads from a given channel
       until a newline or the end is reached. *)
    let line = input_line channel in
    print_endline line
  done
with End_of_file -> close_in channel
```

The following program reads and prints all the lines in a text file
using recursion.

```ocaml
let channel = open_in "BeverlyHillbillies.txt" in
let rec loop () =
  try
    let line = input_line channel in
    print_endline line;
    loop ()
  with End_of_file -> close_in channel
in
loop ()
```

The following program creates a text file and writes lines to it.

```ocaml
let channel = open_out "output.txt" in
try
  (* output_string writes to a given channel. *)
  output_string channel "line 1\n";
  output_string channel "line 2\n";
  output_string channel "line 3\n";
  close_out channel
with ex -> close_out channel
```

## Object-oriented Features

Objects and classes offer an alternative to records.
Objects and records have fields, but objects can also have methods.

Most OCaml code uses modules instead of objects and classes.

The following code demonstrates defining and using a class (`point`).
Classes, like types, have lowercase names by convention.

A class definition serves as its single constructor.
A class cannot define additional constructors,
but functions can be written to create instances in additional ways.

The `point` class and the `origin` function are wrapped in a module
to make it clear that they are related.
Modules have names that start uppercase by convention.

```ocaml
open Printf

module Geometry = struct

  (* Arguments passed to a constructor do not require a matching field
     and can be used in methods. *)
  class point (x_init : float) (y_init : float) =
    (* Can omit "(self)" if there are no references to it. *)
    object (self)
      (* Instance variables are immutable by default,
         but can be made mutable. *)
      val mutable x = x_init
      val mutable y = y_init

      (* Getter methods can have the same name as the field they return,
         but often they start with "get_". *)
      method get_x = x
      method get_y = y
      method set_x new_x = x <- new_x
      method set_y new_y = y <- new_y

      (* This is a named constructor. *)
      method origin = new point 0. 0.
      method print = printf "(%f, %f)\n" x y

      method translate dx dy =
        x <- x +. dx;
        y <- y +. dy
    end

  (* This is the OCaml version of a named constructor. *)
  let origin = new point 0. 0.
end

let () =
  (* Use the new keyword to create an instance of the class. *)
  (* let p = new point 0. 0. in *)
  (* This uses a named constructor function instead. *)
  let p = Geometry.origin in

  (* Methods are called with # instead of dot. *)
  p#set_x 1.;
  p#set_y 2.;
  p#translate 3. 4.;

  (* There is no need to pass the unit value `()`
     to call methods that have no parameters. *)
  printf "(%f, %f)\n" p#get_x p#get_y;
  p#print;

  (* The Oo.copy function makes a shallow copy of an object. *)
  let p2 = Oo.copy p in
  p2#set_x 5.;
  p2#print;

  (* Each object is assigned a unique id
     that can be accessed with Oo.id function. *)
  printf "p id = %d\n" (Oo.id p);
  printf "p2 id = %d\n" (Oo.id p2)
```

Abstract classes are defined with "class virtual".
Methods can be defined with "method virtual"
to require subclasses to implement them.

To inherit from another class, add "inherit {class_name} {args}"
inside "object (self)" to call its constructor.

To enable calling superclass methods,
add "inherit {class_name} {args} as super"
and then use "super#{method_name}" to call them.

To coerce a subclass value to a superclass type, use `obj :> {superclass}`.

The following code demonstrates defining an abstract class (`shape`)
and classes that inherit from it (`circle` and `rectangle`).
It also uses the `point` class defined above.

```ocaml
open Printf

module Geometry = struct
  class point (x_init : float) (y_init : float) =
    object (self)
      val mutable x = x_init
      val mutable y = y_init
      method get_x = x
      method get_y = y
      method set_x new_x = x <- new_x
      method set_y new_y = y <- new_y
      method print = printf "(%f, %f)\n" x y

      method translate dx dy =
        x <- x +. dx;
        y <- y +. dy
    end

  let origin = new point 0. 0.

  class virtual shape (name_init : string) =
    object
      val name = name_init
      method name = name
      method virtual area : float
    end

  class circle (center_init : point) (radius_init : float) =
    object
      inherit shape "circle"
      val center = center_init
      val radius = radius_init
      method area = 3.14159 *. radius *. radius
    end

  class rectangle (lower_left_init : point) (width_init : float)
    (height_init : float) =
    object
      inherit shape "rectangle"
      val lower_left = lower_left_init
      val width = width_init
      val height = height_init
      method area = width *. height
    end
end

let () =
  let p = Geometry.origin in
  let c = new Geometry.circle p 5. in
  printf "%s area = %f\n" c#name c#area;
  let r = new Geometry.rectangle p 10. 5. in
  printf "%s area = %f\n" r#name r#area
```

## Dune

<a href="https://dune.build" target="_blank">Dune</a>
is a popular OCaml and Reason build system.
It is used create, build, test, and run OCaml projects.
It can also compile to JavaScript.

To install the `dune` command, enter `opam install dune`.

For help, enter `dune --help`.

### Creating

**To create a project** that uses `dune`,
cd to where the project should be created
and enter the following.

```text
dune init project {project_name}
```

The project name can contain underscores, but not hyphens.

This generates many files including:

- dune configuration file `dune-project`
- opam configuration file `{project_name}.opam`
- `_build`, `bin`, `lib`, and `test` directories
- main source file `bin/main.ml`
- unit test file `test/test_{project_name}.ml`.

The `bin` directory holds source files that will be compiled to executables.
These often use modules defines in the `lib` directory.
It's a bit odd that it hold source files
rather than executables created by a build process.

It is common to have a file in the `bin` directory named `main.ml`,
but that name is not required.
It is also common to end files in the `bin` directory
with an expression that begins with `let () =`.
This is similar to the main function in other languages.

The `lib` directory is the preferred location for source files
that define reusable functions.
One reason is that Dune supports implementing unit tests
for files in the `lib` directory, but not for files in the `bin` directory.

The `test` directory holds test files for tests that
are not included inline with the functions they test.
Inline tests are typically preferred.

When using git for version control,
the `_build` directory should be added in the `.gitignore` file.

The `bin`, `lib`, and `test` directories all contain a `dune` file
that holds configurations options for that directory.
These files use s-expressions (also used in Lisp)
that is an alternative to other data formats such as JSON.
Each option is specified in by a name and value inside parentheses
that is referred to as a
"<a href="https://dune.readthedocs.io/en/stable/dune-files.html"
target="_blank">stanza</a>".

The `lib` directory can have subdirectories that contain `.ml` files
and those can define additional types, constants, and functions.
To make those accessible, add the following
at the bottom of the `lib/dune` file:

```text
(include_subdirs qualified)
```

For example, if the project name is "demo" and
`lib/sub/mod.ml` defines the function `greet`
then `bin/main.ml` can refer to it with `Demo.Sub.Mod.greet`.

An alternative to adding subdirectories in the `lib` directory is to create
additional top-level directories that each define a different library.

For example:

- Create the top-level directory `lib2`.
- Create the file `lib2/dune` containing `(library (name demo2))`
- Create the file `lib2/mod.ml` containing
  `let greet () = print_endline "Hello from lib2!"`
- Update the `libraries` stanza in the `bin/dune` file to `(libraries demo demo2)`
- Add the call `Demo2.Mod.greet ()` in `bin/main.ml`

### Manually Creating

A Dune project can be created by manually creating
a directory containing the three files
`dune-project`, `dune`, and `{project-name}.ml`.

Suppose the project name is "my_dune_project".
Here is a minimal `dune-project` file.
The first line specifies the version of the syntax used in this file
and in the `dune` files, not the version of Dune being used.

```text
(lang dune 3.14)
(package
  (name my_dune_project)
  (depends ocaml dune))
```

Here is a minimal `dune` file.

```text
(executable
  (public_name my_dune_project))
```

Here is a minimal OCaml source file with the same name as the project,
`my_dune_project.ml`.

```ocaml
let () = print_endline "Hello, World!"
```

To run this project, enter `dune exec my_dune_project`
This may take 5 to 10 seconds the first time it is run.
It will be much faster for subsequent runs.

If you prefer to name the main source file `main.ml`,
add the `(name main)` stanza in the `dune` file.

### Building

**To build the project**, enter `dune build` or just `dune b`.
This creates `_build/default/bin/main.exe`.
To automatically rebuild the project
when code changes are detected, add the `--watch` flag.

### Running

**To run the project**, enter `dune exec {executable_name}`.
The executable name is specified in the `public_name` stanza
found in the `bin/dune` file and defaults to the project name.

### Cleaning

**To clean a project**, enter `dune clean`.
This deletes the `_build` directory that contains generated files.

### utop

**To run `utop` with project libraries automatically available**,
enter `dune utop`.
For example, in a project with a library named "demo",
a module named "math_lib", and a function in that module named "add",
the following works: `Demo.Math_lib.add 1 2`.

### Example Project

Let's walk through creating a small OCaml project with Dune.

1. `cd` to the directory where the project should be created.
1. Enter `dune init project demo`
1. `cd demo`
1. Enter `dune exec demo`.
1. Verify that the output is "Hello, World!".
1. Create the file `lib/math.ml` containing the following:

   ```ocaml
   let pi = Float.pi

   let add a b = a +. b

   let average numbers =
     let sum = List.fold_left (+.) 0. numbers in
     let length = List.length numbers in
     sum /. float_of_int length
   ```

1. Create the file `lib/points.ml` containing the following:

   ```ocaml
   type point2d = float * float

   (* This demonstrates adding types to parameters and
      specifying the return type, all of which can be inferred. *)
   let distance ((x1, y1) : point2d) ((x2, y2) : point2d) : float =
     let dx = x2 -. x1 in
     let dy = y2 -. y1 in
     sqrt ((dx *. dx) +. (dy *. dy))

   let%test _ = distance (1., 1.) (4., 5.) = 5.
   ```

1. Modify the file `bin/main.ml` to contain the following:

   ```ocaml
   (* Note how open requires the library name AND the module name. *)
   open Demo.Math
   open Demo.Points
   open Printf

   let my_constant = 7

   let square x = x * x (* a function *)

   (* Note the use of semicolons to separate the
      statements and expressions. *)
   let () =
     (* This is a verbose way to print a value. *)
     print_string "my_constant = ";
     print_int my_constant;
     print_newline ();

     (* This is a more concise way to print a value. *)
     printf "pi = %f\n" pi; (* defined in math.ml *)

     (* This calls a function defined in this file. *)
     printf "square of %d = %d\n" my_constant (square my_constant);

     (* The remaining examples call functions defined in math.ml. *)
     let a = 2. and b = 3. and c = 2.5 in
     printf "sum of a and b = %f\n" (add a b);

     let numbers = [a; b; c] in
     let avg = average numbers in
     printf "average of a, b, and c = %f\n" avg;

     let p1 = (0., 0.) and p2 = (1., 1.) in
     let d = distance p1 p2 in print_float d
   ```

   The `let () =` is required because at the module level,
   everthing needs to be in a binding.
   You could use `let _ =` instead which allows
   the expression to have any kind of value.
   But using `let () =` is preferred because it states
   that the expression does not return a value.

1. Enter `dune exec demo`.
1. Verify that the output is

   ```text
   my_constant = 7
   pi = 3.141593
   square of 7 = 49
   sum of a and b = 5.000000
   average of a, b, and c = 2.500000
   1.41421356237
   ```

Signatures can be used to make some definitions in a module "private".
For example:

1. Create the file `lib/geometry.ml` to define
   the implementation of the `Geometry` module.

   ```ocaml
   type point = float * float

   module type Signatures = sig
     val distance : point -> point -> float
   end

   module Mod : Signatures = struct
     let square x = x *. x
     let distance (x1, y1) (x2, y2) = sqrt (square (x1 -. x2) +. square (y1 -. y2))
   end
   ```

1. Create the file `lib/geometry.mli` to define
   the public interface of the `Geometry` module.

   ```ocaml
   type point = float * float
   val distance : point -> point -> float
   ```

   This does not expose the `square` function.

1. Call the distance function in `bin/main.ml`.

   ```ocaml
   printf "distance = %f\n" (Demo.Geometry.distance (0., 0.) (1., 1.));
   ```

   This assumes the `public_name` of the executable is "demo",

### Unit Tests

Dune supports several kinds of tests,
including inline, expectation, and "cram" tests.
Expection tests are similar to Jest snapshot tests.
Cram tests describe the output of a shell session.
Jest is a JavaScript test framework.

The following steps add tests to the `demo` project above and run them.
It seems that tests can only be used in libraries defined
in the `lib` directory, not in the `bin` directory.

1. Enter `opam install ppx_inline_test` to enable only inline tests or
   enter `opam install ppx_expect` to enable both inline and expectation tests.

   PPX is short for "PreProcessor eXtension".

1. Change `lib/dune` to the following:

   ```text
   (library
     (name demo)
     (inline_tests)
     ; This only enables inline tests, not expectation tests.
     ; (preprocess (pps ppx_inline_test)))
     ; This enables both inline tests and expectation tests.
     (preprocess (pps ppx_expect)))
   ```

   Single line comments in `dune` files begin with `;`.

1. Add the following lines in `lib/math.ml`:

   ```ocaml
   (* This is an inline test for the add function. *)
   let%test _ = add 1.2 2.3 = 3.5

   (* This is an expectation test for the average function. *)
   let%expect_test _ =
     print_float (average [2.; 3.; 2.5]);
     [%expect "2.5"]
   ```

1. Add the following lines in `lib/points.ml`:

   ```ocaml
   let%test _ = distance (1., 1.) (4., 5.) = 5.
   ```

1. Enter `dune test` or `dune test -w` to run in watch mode.

   You'll see `dune runtest` in documentation,
   but `dune test` is shorter and is an alias.

1. Verify that there are no failed tests.

   When all the tests pass, there is no output.
   If any expecation tests fail, but the actual values are correct,
   enter `dune promote` to update all the expected values.

Tests can also be placed in the `test` directory.
The following steps implement the same tests above in this way.

1. Replace the contents of the `test/dune` file with the following:

   ```text
   (tests
     (libraries demo)
     (names add average)
   )
   ```

   If there is only one test,
   `(tests` can be changed to `(test` and
   `(names` can be changed to `(name`.

1. Create the file `test/add.ml` containing the following:

   ```ocaml
   open Demo

   let () = assert ((Math.add 1.2 2.3) = 3.5)
   ```

1. Create the file `test/average.ml` containing the following:

   ```ocaml
   open Demo

   let () = print_float (Math.average [2.; 3.; 2.5]); (* 2.5 *)
   ```

1. Create the file `test/average.expected` containing `2.5`.

1. Enter `dune test` and verify that all the tests pass.

Also see the example project at
<a href="https://github.com/mvolkmann/ocaml-examples/tree/main/odoc_demo"
target="_blank">odoc_demo</a> which implements unit tests for the
`my_math` module that it defines.

### Multiple Executables

A Dune project can host the code for multiple executables.
This is ideal for a collection of sample programs
that demonstrate various features of OCaml.

To create a project like this:

1. `cd` to the directory where the project will be created.

1. Enter `dune init project samples` where "samples" is the project name.

1. `cd samples`

1. Delete all the directories and files that will not be needed.
   These includes the directories `bin`, `lib`, and `test`
   and the file `samples.opam`.

1. Edit the `dune-project` file to only contain the following:

   ```text
   (lang dune 3.14)
   (package
     (name samples)
     (depends ocaml dune))
   ```

1. Create the file `dune` in the root directory of the project
   containing the following:

   ```text
   (executables
     (names program1 program2)
     (libraries lib1 lib2))
   ```

   Replace `program1` and `program2` with the names of your sample `.ml` files.
   Replace `lib1` and `lib2` with the names of any libraries you
   install using `opam` that are used by your sample programs.

1. To run one of your sample programs such as `foo.ml`,
   enter `dune exec ./foo.exe`

## opam Libraries

<a href="https://opam.ocaml.org" target="_blank">opam</a>
is a package manager for OCaml, similar to npm for JavaScript.

1. `dune init project date_formatting`
1. `cd date_formatting`
1. `opam install odate`
1. Edit `bin/dune` and change the `libraries` stanza to
   `(libraries date_formatting odate)`.
1. Edit `bin/main.ml` and replace its contents with the following:

   ```ocaml
   module Date = ODate.Unix

   (* For documentation for date/time format strings, see
      https://github.com/MLstate/opalang/blob/master/lib/stdlib/core/date/duration.opa#L472
      A is day of week, B is month, d is day of month, Y is year.
      a and b give abbreviations.
   *)
   let date_format = "%A, %B %d %Y"

   let date_printer =
     match Date.To.generate_printer date_format with
     | Some p -> p
     | None -> failwith "could not generate printer"

   let () =
     let now = Date.now() in
     let s = Date.To.string date_printer now in
     print_endline s
   ```

1. `dune exec date_formatting`

### Switches

To use specific versions of packages in projects, create and activate switches.
A switch is a collection of packages that are tied to an OCaml version
and can specify versions of packages.

There are two kinds of switches,
global switches that can be used by multiple projects and
local switches that are tied to a specific project directory.

To create a global switch,
enter `opam switch create {switch-name} {ocaml-version}`.
For example, `opam switch my_project 5.1.1`
The OCaml version cannot be omitted.
Creating a new switch takes around four minutes to complete.

To change the current global switch, enter `opam switch {switch-name}`
and `eval $(opam env)` to activate it.

To see the name of the currently activated global switch, enter `opam switch`.

To install packages in the currently activated global switch,
enter `opam install {package-name}.{version}`

To return to the default global switch, use the same commands to
change to any switch, but specify the name "default".

To list all the currently defined global switches, enter `opam switch list`

To delete a global switch, enter `opam switch remove {switch-name}`

To create a local switch, cd to a project directory
and enter `opam switch create .`.
Local switches are automatically selected and activated
when you `cd` to a project directory that has one.
Switches consume a lot of disk space,
so creating a local switch for each project is not recommended.

An alternative to a local switch is to link a global switch to a project directory.
To do this, `cd` to the project directory
and enter `opam switch link {switch-name}`

## Pretty Printing

See the library <a href="https://github.com/ocaml-ppx/ppx_deriving"
target="_blank">ppx_deriving</a>.
This supports a number plugins for deriving functions from types,
including `enum`, `eq`, `fold`, `iter`, `make`, `map`, `ord`,
`protobuf` `show`, and `yojson`.

PPX is short for "PreProcessor eXtension".
These can add support for new syntax and generate/transform code.

To use this in a Dune project:

1. Install it by entering `opam install ppx_deriving`.

1. If using the `yojson` plugin, also enter `opam install ppx_deriving_yojson`.

1. Add the following stanza to the project `dune` file.
   Here we only use the `show` plugin which generates functions
   whose names begin with `show_` that return pretty-printed strings.

   ```text
   (preprocess (pps ppx_deriving.show ppx_deriving.yojson))
   ```

1. Annotate types to be pretty-printed. For example:

   ```ocaml
   type int_list = int list [@@deriving show]
   ```

1. Print instances of that type with the following:

   ```ocaml
   print_endline (show_int_list numbers);
   ```

   This outputs something like `[1; 2; 3]`.

We can write a function to print the elements in a list
as an alternative to using a ppx.

```ocaml
let print_int_list l =
  List.iter (Printf.printf "%d ") l;
  print_newline ()

let () =
  let numbers = [ 1; 2; 3 ] in
  print_int_list numbers
```

This outputs like `1 2 3`.

## Generating Documentation

The odoc tool generates HTML-based documentation
from `(** ... *)` commands in `.ml` source files.
Those commands can contain directives like `@param` and `@result`.

This must be installed by entering `opam install odoc`.

Dune only generates documentation for public libraries.

See the example project at
<a href="https://github.com/mvolkmann/ocaml-examples/tree/main/odoc_demo"
target="_blank">odoc_demo</a>.

To generate HTML documentation, enter `dune build @doc`.
The generated `index.html` file will be in the
`_build/default/_doc/_html` directory.

For more detail, see
<a href="https://ocaml.github.io/odoc/odoc_for_authors.html"
target="_blank">odoc for authors</a>

## HTTP Servers

There are several OCaml libraries for implementing HTTP servers
that provide API endpoints. Popular options include:

- <a href="https://aantron.github.io/dream/" target="_blank">Dream</a> -
  1.5K GitHub stars
- Opium - 747 GitHub stars
- CoHTTP - 676 GitHub stars
- Ocsigen - not in GitHub

### Dream

The steps to use the
<a href="https://aantron.github.io/dream/" target="_blank">Dream</a>
web framework are:

- Install Dream by entering `opam install dream`
- cd to the directory where a new Dune project will be created.
- Enter `dune init project dream_demo`
- `cd dream_demo`
- Edit `bin/dune` and modify the `libraries` stanza to include `dream`.
- Edit `bin/dune` and add the following at the end to enable use of HTML templates:

  ```text
  (rule
    (targets main.ml)
    (deps main.eml)
    (action (run dream_eml %{deps} --workspace %{workspace_root})))
  ```

- Delete `bin/main.ml`.
- Create `bin/main.eml` with the following:

  ```ocaml
  let hello who =
    <html>
      <body>
        <h1>Hello, <%s who %>!</h1>
      </body>
    </html>

  let () =
    Dream.run
    @@ Dream.logger
    @@ Dream.router [
      Dream.get "/" (fun _ -> Dream.html (hello "World"));
    ]
  ```

- Enter `dune exec dream_demo --watch`
- Browse localhost:8080

VS Code isn't able to provide LSP support for files with a `.eml` extension.
Using an extension of `.eml.ml` or `.eml.html` enables LSP support.
But both extensions cause issues because the files
contain a combination of OCaml code and HTML.
Treating them as one or the other will cause VS Code
to flag errors that are not actually errors.

For a more advanced Dream app that
places the HTML templates in separate source files, see
<a href="https://github.com/mvolkmann/ocaml-examples/tree/main/dream_demo"
target="_blank">dream_demo</a>.

Dream Templates are not well-supported in editors like VS Code.
Many errors flagged in them are not real errors.
If the templates are moved to separate files with the `.eml.html` file extension
as recommended, hovering over OCaml values will not display their types.

<img alt="OCaml Dream info" style="border: 0"
  src="/blog/assets/ocaml-dune-dream-dialect.png?v={{pkg.version}}">

For an alternative to Dream templates
which doesn't having tooling support issues, see
<a href="https://github.com/yawaramin/dream-html" target="_blank">dream-html</a>.

## Jane Street Modules

Jane Street created the modules `Base`, `Core_kernel`, and `Core`.
The `Base` module is a minimal replacement for the OCaml standard library.
The `Core_kernal` module extends `Base` and adds features.
The `Core` module extends `Core_kernel` and adds UNIX APIs.

To use the `Base` module, install it with `opam install base`.
In source files that use it, add `open Base`.
This causes all values from the standard library to be marked as deprecated.

To use the `Base` module:

1. `opam install base`
1. Add `base` as a dependency in `dune` files.
1. Add `open Base` in source files.

The use of Jane Street modules somewhat splits the community in two.

## Converting OCaml to JavaScript

Here are the steps to compile an OCaml program to JavaScript using Dune.

1. Install the "js_of_ocaml" compiler.

   Enter `opam install js_of_ocaml-compiler`.

1. Modify the `dune` file.

   It should contain the following where
   the main OCaml source file is `hello.ml`:

   ```text
   (executables
     (names hello)
     (modes js))
   ```

1. Compile to JavaScript.

   Enter `dune build ./hello.bc.js`

   This generates a ridiculous amount of JavaScript code ...
   over 9000 lines for a Hello World program.
   So this is not a serious option!

1. Run the code.

   Enter `node _build/default/hello.bc.js`

## Preprocessor Extensions (ppx)

Preprocessor extensions are programs that are called at compile time
to alter or add source code.
They operate on the abstract syntax tree (AST) of the program.

For example, the syntax `[%get_env "USER"]`
can be replaces by the value of the `USER` environment variable.

See <a href="https://ocaml.org/docs/metaprogramming"
target="_blank">Preprocessors and PPXs</a>

## Serializing Data

OCaml provides good support for serializing data to
s-expressions, JSON, and binary (sequences of bytes).

For s-expressions, see the
<a href="https://ocaml.janestreet.com/ocaml-core/v0.13/doc/sexplib/Sexplib/Sexp/index.html"
target="_blank">Sexplib.Sexp</a> module.

For JSON, see the <a href="https://github.com/ocaml-community/yojson"
target="_blank">yojson</a> library.

For binary, see the <a href="https://ocaml.org/manual/5.1/api/Marshal.html"
target="_blank">Marshal</a> module.

TODO: Add detail on this.

## Suppressing Errors

Sometimes it is desirable to suppress certain errors.
For example, you have have a record type that contains a
field that is not yet being used, but will be used in the future.

There are a couple of ways to do this.
One way is to add `[@warning "-unused-field"]` immediately after
the `type` keyword that defines the record type.
Another way when using Dune is to add the
stanza `(flags (:standard -w -69))` to the `dune` file
where 69 is the code for "unused-field" errors.

To see all the supported error codes, enter `ocaml -warn-help`.

## Concurrency

There are multiple OCaml libraries that support concurrency.
The one that seems most popular is
<a href="https://github.com/ocaml-multicore/eio" target="_blank">eio</a>.

To install eio, enter `opam install eio_main`.

## Debugging

To use `ocamldebug`:

1. Add the following stanzas in the `dune` file of the executable:

   ```text
   (flags (:standard -g))
   (modes byte)
   ```

1. Build the executable with `dune build`.
   This creates a `.bc` bytecode file in the `_build/default` directory.

1. Enter `ocamldbug _build/default/{name}.bd`

1. Enter `ocamldebug` commands like `step`, `next`,
   `print`, `break`, `continue`, and `quit`.

I wasn't able to set a breakpoint or print any expressions.
