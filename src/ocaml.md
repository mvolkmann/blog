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

The Caml programming language is the predecessor of OCaml.
The name is short for "Categorical Abstract Machine Language".
OCaml is short for "Objective Caml".

OCaml is a member of the
<a href="https://en.wikipedia.org/wiki/ML_(programming_language)"
target="_blank">ML</a> (short for Meta Language)
family of programming languages.
Other dialects of ML include Standard ML and F#.
ML influenced the design of many other languages including
Clojure, Elm, Haskell, Erlang, Rust, and Scala.

OCaml has an interpreter, a compiler that compiles to bytecode,
and a compiler that creates native executables.

The OCaml compilers are implemented in OCaml.
They are fast compared to the compilers for Haskell and Rust.

OCaml source files have the extension `.ml` which stands for "meta language".

OCaml supports Foreign Function Interface (FFI) mechanisms to call C functions.

The financial company <a href="https://www.janestreet.com"
target="_blank">Jane Street</a> is one of the
largest users and supporters of OCaml.

## Resources

- <a href="https://ocaml.org" target="_blank">OCaml home page</a>
- <a href="https://learnxinyminutes.com/docs/ocaml/" target="_blank">Learn X in Y minutes</a>
- <a href="https://cs3110.github.io/textbook/cover.html"
  target="_blank">OCaml Programming: Correct + Efficient + Beautiful</a>
  book used in Cornell CS 3110 course
- <a href="https://dev.realworldocaml.org" target="_blank">Real World OCaml</a> book
- <a href="https://ocaml-book.com" target="_blank">OCaml from the Very Beginning</a> book

## Variants

<a href="https://reasonml.github.io" target="_blank">Reason</a>
is a syntax extension and toolchain for OCaml developed by Facebook.
It provides a more JavaScript-like syntax while
retaining full compatibility with the OCaml language.
The syntax of Reason is more familiar to JavaScript developers than OCaml.
Reason can use OCaml libraries.

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
TODO: Does this install dune which is a build tool?

To install tools for development, enter the following shell command
which takes about four minutes to complete:

```bash
opam install ocaml-lsp-server odoc ocamlformat utop
```

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
  with a `.byte` extension that is passed to the `ocamlrun` coommand.
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
- The `ocamldebug` debugger is used to debug OCaml programs.
- The `ocamlprof` tool profiles OCaml programs
  to analyze their performance and identify bottlenecks.
- The standard library provides commonly needed functions and data structures.
  It includes modules for strings, lists, arrays, I/O operations, and more.

A module is a collection of named values
that can be constants, functions, and types.
An `open` expresion brings the names defined in a module into the current scope.
Circular dependencies between modules are not allowed.

Suppose we have the file `my_module.ml` containing `let add a b = a + b`
and we have the file `main.ml` containing the following:

```ocaml
open My_module

(* This is similar to the main function in other languages.
   It is where most side effects should occur. *)
let () =
  let sum = add 1 2 in
  print_int sum
```

It is recommended to create a Dune project,
move these files in the project `bin` directory,
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

## VS Code

Install the "OCaml Platform" extension from OCaml Labs.

Create the file `.ocamlformat` in each project root directory containing
at least the following in order for VS Code to format OCaml code on save.

```text
profile = default
version = 0.26.1
```

Sometimes after code changes VS Code flags errors that aren't real.
Running "Developer: Reload Window" from the command palette clears them.
This issue may go away if you run `dune build -w` (for watch mode)
in a terminal window.

## REPL

OCaml has two REPLs. A basic one can be started by entering `ocaml`.
A better one is `utop` which is short for "Universal Toplevel".
The `utop` command provides a more interactive, user-friendly interface that
includes line editing, syntax highlighting, command history, and tab completion.

In either REPL the expressions you enter are only evaluated
when they are terminated by a double semicolon (`;;`).
This allows entering multiple expressions that are separated by a single colon
and allows them to span multiple lines. For example:

```ocaml
print_int (2 + 3); print_int (2 * 3);; (* 56 *)
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
They can be edited and executed again.

To load definitions in an OCaml source file into the REPL,
enter `#use "{file-path}";;`.
This enables using all the types and functions defined in the source file
inside the REPL.

To exit the REPL, press ctrl-d or enter #quit.

There is also an iOS app called "OCaml" for evaluating OCaml expressions.

## Comments

Comments in OCaml code begin with `(*` and end with `*)`.
They can span any number of lines.

Comments can be nested.

Doc comments provided documentation that can be extracted from source files.
They begin with `(**` and end with `*)`.
Their content is similar to JSDoc comments,
including the use of `@param` and `@return` annotations.
There are three variations of doc comments, floating, item, and label.

## Primitive Types

OCaml supports the following primitive types.
Their sizes depend on the CPU.

- `bool` - 1 byte with the literal values `true` and `false`
- `char` - 1 byte ASCII, not Unicode
- `int` - 8 or 4 bytes
- `float` - 8 bytes
- `string` - sequence of bytes, not Unicode characters

Literal chars are delimited by single quotes.

Literal strings are delimited by double quotes.
The `^` operator is used to concatenate strings.

The `Char` and `String` modules provide many functions
for operating on values of these types.

For Unicode support, see the libraries
`Uutf`, `Uutf_string`, and `ocaml-unicode`.

Primitive values are expressions that do not require additional evaluation.

## Type Conversions

The OCaml standard library provides many functions
for converting a value from one type to another.
Examples include:

- `float_of_int`
- `int_of_float` - `truncate` is an alias
- `int_of_char`
- `char_of_int`
- `string_of_bool`
- `bool_of_string` - raises `Failure` if conversion fails
- `bool_of_string_opt` - returns `None` if conversion fails
- `string_of_int`
- `int_of_string` - raises `Failure` if conversion fails
- `int_of_string_opt` - returns `None` if conversion fails
- `string_of_float`
- `float_of_string` - raises `Failure` if conversion fails
- `float_of_string_opt` - returns `None` if conversion fails

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

The `-` and `-.` operators can also be used for unary negation.
There are no operators like `++` and `--` to increment or decrement a number.
Instead use the functions `succ` and `pred` to get the successor or predecessor.

There is no exponentiation operator for `int` values.

The string operators include:

| Operator | Description                 |
| -------- | --------------------------- |
| `^`      | string concatenation        |
| `^^`     | format string concatenation |

The relational and logical operators include:

| Operator | Description           |
| -------- | --------------------- |
| `==`     | same object           |
| `!=`     | not same object       |
| `=`      | structural equality   |
| `<>`     | structural inequality |
| `<`      | less than             |
| `>`      | greater than          |
| `<=`     | less than or equal    |
| `>=`     | greater than equal    |
| `&&`     | boolean and           |
| `\|\|`   | boolean or            |

The `=` operator is also used for assignment.

The remaining operators include:

| Operator | Description                   |
| -------- | ----------------------------- |
| `!`      | gets ref value (dereferences) |
| `:=`     | sets ref value (assigns)      |
| `@`      | list concatenation            |
| `\|>`    | reverse function application  |

Most OCaml operators are implemented as binary functions.
To use them as functions, wrap them in parentheses.
For example, `a + b` is the same as `(+) a b`.
Adding spaces inside the parentheses is optional,
but is required for the `*` operator because
`(*` is interpreted as the beginning of a comment.

Operator functions can be passed to functions like `List.filter`.

The following code demonstrates writing a function that
takes a comparison function which can be a logical operator.
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

## Variables

Variables are immutable.
An exception is that variables in a REPL can be reassigned.

Identifier names must start with a lowercase letter unless they refer to
a module, constructor, or "polymorphic variant tag".
They can contain letters, digits, and the underscore character.
They can also end with a single quote to create pairs of names
like `x` and `x'` (for x prime).
Technically an indentfier can contain any number of single quotes
and they can appear anywhere except at the beginning,
but doing this is odd!

A `let` expression binds an identifier to the value of an expression
whose scope is the expression that follows.
The value of a `let` expression is the value of its expression
with all occurrences of the identifier replaced with its value.
For example, the value of this `let` expression is `3`:

```ocaml
let a = 1 in
let b = 2 in
a + b
```

The code above can also use the `and` keyword as follows:

```ocaml
let a = 1 and b = 2 in a + b
```

Note how the expression that follows a `let` expression
can be another `let` expression in order to place
multiple identifiers in the scope of the final expression.

Identifers bound by `let` expressions go out-of-scope
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
and the ocamlformat code formatter will add them.

Let definitions omit the `in` keyword.
They create global definitions that
do not go out of scope after they are evaluated.
They are not expressions, so they do not have a value.

The following example binds three global identifiers.
Note how a double semicolon is used to
terminate the assignments of global identifiers.

In order to call a function that takes no arguments, such as `print_newline`,
it must be "passed" the "unit" value `()`.
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

### Type Variables

OCaml supports type variables which are similar to generics in other languages.
Type variables serve as a placeholder for an unknown type.
They are written with a single quote followed by a lowercase name.

Often the name is just `'a` and is pronounced "alpha".
If additional type variables are needed,
it is common to use `'b` (beta) and `'c` (gamma).

For example, entering `[];;` in `utop` outputs type type `'a list`
because it is a list where the type of the elements is unknown.

The following code demonstrates writing a function that uses
parametric polymorphism to find the largest value in a list of values.

```ocaml
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

let () =
  let numbers = [ 1; 13; 4; 9 ] in
  let max = max_element numbers in
  match max with
  | None -> print_endline "empty list"
  | Some max -> print_int max
```

The `function` keyword is useful in functions
that immediate `match` on the last parameter.
It simplfies the code by removing the need to list the last parameter
and replacing `match {last-parameter} with` with just `function`.
For example, the first two lines of the `max_element` function above
can be replaced by `let rec max_element = function`.
The type of `max_element` can be specified as `'a list -> 'a option`
which states that it takes a list of `a'` elements
and returns an `option` of type `'a`.

## References

While variables are immutable, they can be bound to a reference that is mutable.
References are created with the `ref` function
which must be given an initial value.
The initial value determines its type.

The `!` prefix operator dereferences a `ref` to obtain its value.

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

Refs are actually single field records with a mutable field named `contents`.

## Options

The type `option` represents an optional value.
The only valid values of this type are `None` and `Some`.

Functions that sometimes do not have a value to return
use this type to represent their return value.
For example, the `List.find_opt` function does this.

```ocaml
(* The `open` keyword allows all the identifiers in a given module
   (`printf` in this case) to be used in this source file
   without prefixing them with their module name. *)
open Printf

let colors = [ "red"; "green"; "blue" ]
let color = "green"
let result = List.find_opt (fun x -> x = color) colors

let () =
  match result with
  | Some c -> printf "found %s\n" c
  | None -> print_endline "failed to find green\n"
```

## Custom Types

The `type` keyword defines a custom type.

It can be used to simply create an alias for an existing type.
For example:

```ocaml
type weight = float
```

Here is an example of defining and using a type for tree nodes.

```ocaml
open Printf

(* A tree can be empty or it can have a node
   that holds a left tree, a value, and a right tree. *)
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let rec depth_first_in_order tree =
  match tree with
  | Empty -> () (* do nothing *)
  | Node (left, value, right) ->
      depth_first_in_order left;
      print_endline value;
      depth_first_in_order right

(* This holds names in a tree so that when
   printed with depth_first_in_order
   they will appear in sorted order. *)
let family_tree : string tree =
  Node
    ( Node (Empty, "Amanda", Empty),
      "Jeremy",
      Node
        ( Node (Empty, "Mark", Empty),
          "Meghan",
          Node (Empty, "RC", Node (Empty, "Tami", Empty)) ) )

let () = depth_first_in_order family_tree
```

The `type` keyword can also be used to define variant types
which are described in the next section.

## Variant Types

Variant types have many uses including enumerated values, error handling,
and for representing data structures whose shape can vary.

"Sum types" describe alternatives.
Primitive types like `int` and variant types describe below
are examples of sum types.

"Product types" are types that can hold
multiple pieces of data with differing types.
Tuples and records are examples of product types.

Lists are sum types because they can be either an empty list
or a head and tail (where the tail can be an empty list).
Lists are also product types because they hold
a head and a tail which have different types.

Each variant name is called a "constructor".
Each construtor can have an associated value of a specified type.
The value types of the variants can differ.

The following code provides some examples:

```ocaml
type season = Spring | Summer | Fall | Winter

(* The first vertical bar here is optional.
   When pattern matching a sum type, if all variants aren't matched,
   the warning "this pattern-matching is not exhaustive" will appear. *)
let forecast = function
  | Spring -> "rain"
  | Summer -> "sun"
  | Fall -> "nice"
  | Winter -> "snow"

type result = Ok of float | Error of string

let divide x y = if y = 0.0 then Error "Division by zero" else Ok (x /. y)

type my_union = BoolVal of bool | IntVal of int | StringVal of string

let get_string = function
  | IntVal i -> string_of_int i
  | StringVal s -> s
  | BoolVal b -> string_of_bool b

let () =
  print_endline (forecast Winter);

  let numerator = 5.0 and denomiator = 0.0 in
  match divide numerator denomiator with
  | Ok v -> print_endline (string_of_float v)
  | Error e ->
      print_endline e;

  let b = BoolVal true in
  let i = IntVal 10 in
  let s = StringVal "hello" in
  print_endline (get_string i);
  print_endline (get_string s);
  print_endline (get_string b)
```

## Tuples

A tuple is an immutable, ordered collection of values whose types can differ.

To create a tuple, surround the elements in parentheses
and separate them with commas.
For example:

```ocaml
let t = (true, 3, "blue") in
(* Can use destructuring to extract the values. *)
let (b, n, c) = t in
printf "b = %b, n = %d, c = %s\n" b n c
```

The parentheses shown above for creating and destructuring a tuple are optional.

The variable `t` above has the type `bool * int * string`
which is referred to as a "product type"
(based on tuples being similar to cartesian products).

For tuples that only have two elements,
the `fst` function returns the first element
and the `snd` function returns the second.

To get a specific element from a tuple
of a certain length, use pattern matching.
For example, the following function gets
the third element from any 4-element tuple.

```ocaml
let third_of_4 tuple = match tuple with a, b, c, d -> c;;

let t = ("alpha", "beta", "gamma", "delta") in
print_endline (third_of_4 t) (* gamma *)
```

You can think of tuples like records (described later)
where the fields are accessed by position rather than name.

## Conditonal Logic

Conditional logic is implemented with the `if` and `match` expressions.

### if expression

The syntax is `if expr1 then expr2 else expr3`.

`expr1` must evaluate to a `bool`.
Non-`bool` values are not automatically interpreted as a `bool`.
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

Unless the `then` branch produces a unit result, an `else` branch is required.

An `if` expression can be used where other languages use a ternary operator.

### match expression

The `match` keyword performs pattern matching.
It takes an expression whose value is to be matched
and a set of patterns.

The patterns must be exhaustive, meaning that
there must be a pattern that matches every possible value.
Using the catch-all `_` as the final pattern satisfies this.

The patterns can match:

- a specific value (ex. `| 7` or `| "summer"`)
- several values (ex. `| 7 | 8 | 9`)
- a range of characters (ex. `| 'a' .. 'f'`)
- a guard (ex. `| n when 7 <= n && n <= 9`)
- a variant type constructor (ex. `| None` or `| Some x`)
- a tuple (ex. `| (_, "summer", temperature)` which means
  we don't care about the first element, the second element must be `"summer"`,
  and want to capture the third element)
- a list (ex. `| []` or `| ["summer"; other]` or `| first :: second :: rest`)
- an array (ex. `| [||]` or `| [|"summer"; other|]`)
- the catch-all `_`

The value of a `match` expression is the value of the matched pattern.

For example:

```ocaml
open Random

let () =
  Random.self_init ();
  let n = Random.int 10 in
  print_int n;
  print_newline ();
  match n with
  | 0 | 1 | 2 | 3 -> print_string "small"
  | n when 4 <= n && n <= 7 -> print_string "medium"
  | _ -> print_string "large"
```

## Iteration

TODO: Add this content.

## Lists

A list is an immutable, ordered collection of values
that all have the same type.
Lists are implemented as singly linked lists.

To create an empty list, use a pair of square brackets
that is pronounced "nil". For example:

```ocaml
let issues = []
```

To create a non-empty list, surround the elements in square brackets
and separate them with semicolons. For example:

```ocaml
let colors = ["red"; "green"; "blue"]
```

A non-empty list is represented by a head that holds an element value
and a tail that holds the remainder which is another list that may be empty.

To create a new list by adding an element to the beginning of an existing list,
use the `::` operator which is pronounced "cons" (short for "construct").
The right side of this operator must be a list
and the left side must be an expression that evaluates to
the same type as elements in the list on the right. For example:

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

The type of a list is written as `T list` where `T` is the type of the elements.
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

The `match` can be used to recursively process a list.
For example, the following code computes the sum of a list of integers.
By convention, the name `hd` is used for the head of a list
and `tl` is used for the tail.

```ocaml
let rec sum list =
  match list with
  | [] -> empty_value
  | hd :: tl -> hd + sum tl
```

The standard library provides many functions that operate on lists.
See <a href="https://v2.ocaml.org/api/List.html" target="_blank">Module List</a>.
Some highlights include the following:

The `List.exists` function determines if a list contains
at least one element that matches a given predicate function.

The `List.filter` function creates a new list from the elements
in an existing list that satisfy a predicate function.

The `List.fold_left` function reduces a list to a single value
using an accumulator function.

The `List.iter` function iterates over a list
in a way that is useful when the function passed to it
has a side effect and a result list is not needed.

The `List.length` function returns the length of a given list.

The `List.map` function creates a new list containing elements that are
computed by passing each element in an existing list to a given function.

The `List.map2` function is similar to `List.map`, but it operates on two lists,
passing corresponding elements from each to a given function.

The `List.nth` function takes a list and an index.
It returns the list element at the index.

Since tuple elements are separated by commas and list elements are
separated by semicolons, a list of tuples can be written as follows:

```ocaml
["alpha", 1; "beta", 2; "gamma", 3]
```

This evaluates to the following list of tuples:

```ocaml
[("alpha", 1); ("beta", 2); ("gamma", 3)]
```

## Arrays

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
a zero-based index in parentheses (odd syntax). For example:

```ocaml
let color = colors.(1) (* "green" *)
OR
let color = Array.get colors 1
```

To modify an array element, use the `<-` operator. For example:

```ocaml
colors.(1) <- "yellow"
OR
Array.set colors 1 "yellow"
```

The type of an array is written as `T array`
where `T` is the type of the elements.
For example, the type of an array of `float` values is `float array`.

An empty array is written as `[||]` and as the type `'a array`
to indicate that the type of its elements is unknown.

The standard library `Array` module provides
many functions that operate on arrays.
See <a href="https://v2.ocaml.org/api/Array.html" target="_blank">Module Array</a>.
Some highlights include the following:

The `Array.append` function creates a new array by concatenating two arrays.

The `Array.concat` function creates a new array
by concatenating all the arrays in a list.

The `Array.exists` function determines if an array contains
at least one element that matches a given predicate function.

The `Array.length` function returns the length of a given array.

The `Array.fold_left` function reduces an array to a single value
using an accumulator function.

The `Array.sub` function creates a new array
that is a subset of an existing array.

The `Array.to_list` function creates a new list
that contains the same elements as a given array.

The `Array.of_list` function creates a new array
that contains the same elements as a given list.

The `Array.map` function creates a new array containing elements that are
computed by passing each element in an existing array to a given function.

## Records

A record an immutable collections of fields.
The fields can have differents types, including other record types.
Records are similar to structs in other languages.

The following code defines a record type
that describes an item available for purchase:

```ocaml
type item = {
  description : string;
  mutable price : int;
  mutable weight : float;
}
```

The field names must begin with a lowercase letter.

The field values are immutable by default, but can be made mutable
by adding the `mutable` keywored before their field name.

To create a record, just supply values for each of the fields
inside curly braces in any order. For example:

```ocaml
let my_item : item = { description = "milk"; price = 350; weight = 1.0 }
```

It is not necessary to specify the type of the record variable.
But the field names and values must match some existing record type
and no extra fields can be present.

To access a field value in a record, use dot syntax. For example:

```ocaml
let p = my_item.price
```

Fields can also be destructured using a `match`. For example:

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

To create a new record from an existing one
using a different values for some of its fields,
use the `with` keyword. For example:

```ocaml
let new_milk = { milk with price = 325; weight = 1.5 }
```

The following code demonstrates creating and using a list of records.

```ocaml
open Printf

type item = {
  description : string;
  mutable weight : float;
  mutable price : int;
}

let eggs = { description = "eggs"; weight = 0.4; price = 275 }
let milk : item = { description = "milk"; weight = 1.0; price = 350 }
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

## Maps

A Map is an immutable collection of key/value pairs.

The standard library `Map` module provides
many functions that operate on maps.
Each of them return a new map that uses structual sharing
to avoid making a copy of the whole map.
See <a href="https://v2.ocaml.org/api/Map.html" target="_blank">Module Map</a>.

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
let dog_option = StringMap.find_first_opt (fun key -> key = "some-key") dog_map
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

## Functions

OCaml functions are first-class.
They can take other functions as arguments and can return new functions.

The syntax `a b c` means calling function `a` with the arguments `b` and `c`.

Another way to write this is to use the "reverse function application" operator
`|>` which is intended to look like a right pointing triangle.
In fact, in fonts that support ligatures, it is displays as exactly that.
Using this, the equivalent of `a b c` is c |> b |> a.
This operator is typically used with functions that take a single argument.
The following code demonstrates two ways to compute
the squared value of the sine of an angle.

The body of a function definition, the part after the `=`, is an expression.
An expression can be a semicolon-separated set of expressions,
but typically there is only one.
When there are more than one, all but the final expression
serve to cause side effects and must have the unit type (no value).
Often this use used for debug printing.
The result of a function call is the value of the final expression.

```ocaml
let square x = x *. x
let angle = 0.78 (* radians *)
let result1 = square (sin angle)
let result2 = angle |> sin |> square
```

In order to call a function that takes no arguments, such as `print_newline`,
it must be "passed" the "unit" value `()`.
Without this it is just a reference to the function and not a call to it.

Functions that only produce side effects do not return anything.
Calls to them are expressions with "unit type", represented by `()`.
These are like "statements" in other languages.
Some call them "effectful expressions".

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

Labeling parameters allows them to be specified by the labels in calls.
A function can have a mixture of labeled and unlabeled parameters.
Unlabeled parameters are positional and must be
passed in the order they are specified.
Labeled parameters can be specified in any order
and can be mixed into the unlabeled parameters.

Label names are not required to match their coresponding parameter names.

To declare a labelled parameter or pass an labelled argument,
use the syntax `~{name}:{value}`. For example:

```ocaml
open Printf

(* This doesn't use labeled parmeters. *)
let rectangle_area1 length width = length *. width

(* This does use labeled parmeters.
   The name between ~ and : is what will be used in calls.
   The name after the : is what will be used in the function body. *)
let rectangle_area2 ~length:l ~width:w = l *. w

let () =
  printf "area1 = %f\n" (rectangle_area1 8.5 11.0);
  printf "area2 = %f\n" (rectangle_area2 ~length:8.5 ~width:11.0)
```

Labeled parameters can specify default values (which makes them optional)
with the syntax `?{label}:({name}={default-value})`.
A shorthand for this when the label and name match
is `?({name}={default-value})`.
These must appear before the required parameters.

Recursive functions must be defined with `let rec`. For example:

```ocaml
let rec factorial n =
  if n < 0 then
    (* raises an exception *)
    failwith "factorial is not defined for negative numbers"
  else if n = 0 then 1
  else n * factorial (n - 1);;
```

Function parameters can use destructuring to extract elements from tuples.
This can also be done for lists and arrays, but those
require special handling due to non-exhastive matching.
The reason is that by definition tuple types have a known size,
but list and array types do not.
Recall that:

- The type of the tuple `(1, 2, 3)` is `int * int * int` which has a length of 3.
- The type of the list `[1; 2; 3]` is `int list` which does not specify a length.
- The type of the array `[|1; 2; 3|]` is `int array` which does not specify a length.

The following code demonstrates ways to use
destructuring of tuples, lists, and arrays.

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
  (* This destructures a tuple in a variable declaration. *)
  let _, second, third, _ = t in
  printf "second in tuple is %s\n" second;
  printf "third in tuple is %s\n" third;
  printf "second in tuple is %s\n" (tuple2of4 t);

  let l = [ "a"; "b"; "c"; "d" ] in
  printf "second in list is %s\n" (list2of4 l);

  let a = [| "a"; "b"; "c"; "d" |] in
  printf "second in array is %s\n" (array2of4 a)
```

OCaml does not make it easy to write variadic functions,
which are functions that take a variable number of arguments.
One approach is to take a list, but that
requires all the values to have the same type.
Another approach is to use a generalized algebraic data type (GADT),
but that introduces complexity.

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
- an optional "main" expression that typically begins with `let () =`

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

  let p1 = (0.0, 0.0) and p2 = (1.0, 1.0) in
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
  let sum = List.fold_left (+.) 0.0 numbers in
  let length = List.length numbers in
  sum /. float_of_int length

module Geometry = struct
  let rectangle_area width height = width *. height
  let rectangle_perimeter width height = width *. 2.0 +. height *. 2.0
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

## Dune

<a href="https://dune.build" target="_blank">Dune</a>
is a popular OCaml and Reason build system.
It is used create, build, test, and run OCaml projects.
It can also compile to JavaScript.

To install the `dune` command, enter `oam install dune`.

For help, enter `dune --help`.

To create a project that uses `dune`,
cd to where the project should be created
and enter `dune init project {project_name}`.
This generates many files including:

- dune configuration file `dune-project`
- opan configuration file `{project_name}.opam`
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
These files use Lisp-like syntax.
Each option is specified in by a name and value inside parentheses
that is referred to as a
"<a href="https://dune.readthedocs.io/en/stable/dune-files.html"
target="_blank">stanza</a>".

To build the project, enter `dune build`.
This creates `_build/default/bin/main.exe`.
To automatically rebuild the project
when code changes are detected, add the `--watch` flag.

To run the project, enter `dune exec {executable_name}`.
The executable name is specified in the `public_name` stanza
found in the `bin/dune` file and defaults to the project name.

To run `utop` with project libraries automatically available, enter `dune utop`.
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
     let sum = List.fold_left (+.) 0.0 numbers in
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

   let%test _ = distance (1., 1.) (4., 5.) = 5.0
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
     let a = 2.0 and b = 3.0 and c = 2.5 in
     printf "sum of a and b = %f\n" (add a b);

     let numbers = [a; b; c] in
     let avg = average numbers in
     printf "average of a, b, and c = %f\n" avg;

     let p1 = (0.0, 0.0) and p2 = (1.0, 1.0) in
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
     print_float (average [2.0; 3.0; 2.5]);
     [%expect "2.5"]
   ```

1. Add the following lines in `lib/points.ml`:

   ```ocaml
   let%test _ = distance (1., 1.) (4., 5.) = 5.0
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

   let () = print_float (Math.average [2.0; 3.0; 2.5]); (* 2.5 *)
   ```

1. Create the file `test/average.expected` containing `2.5`.

1. Enter `dune test` and verify that all the tests pass.

# HTTP Servers

There are several OCaml libraries for implementing HTTP servers
that provide API endpoints. Popular options include:

- <a href="https://aantron.github.io/dream/" target="_blank">Dream</a> -
  1.5K GitHub stars
- Opium - 747 GitHub stars
- CoHTTP - 676 GitHub stars
- Ocsigen - not in GitHub

TODO: Implement your Dog CRUD htmx app using Dream.

## Converting OCaml to JavaScript

See http://ocsigen.org/js_of_ocaml/latest/manual/overview.
