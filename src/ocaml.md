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

Supposedly the OCaml compiler is much faster than the Haskell compiler.
TODO: Verify this.

OCaml source files have the extension `.ml` which stands for "meta language".

The financial company <a href="https://www.janestreet.com"
target="_blank">Jane Street</a> is one of the
largest users and supporters of OCaml.

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
This allows expressions to span multiple lines.

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
- `int` - 8 or 4 bytes
- `float` - 8 bytes
- `string` - sequence of bytes, not Unicode characters

Literal strings are delimited by double quotes.
The `^` operator is used to concatenate strings.

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
Note how double colons must be used to terminate several of the lines.
Also, functions that take no arguments, like `print_newline`,
must be passed the "unit" value `()`.

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

TODO: Add an example function that has a parameter whose type uses a type variable.

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
(* makes the printf function available
   without writing Printf.printf *)
open Printf

let colors = [ "red"; "green"; "blue" ]
let color = "green"
let result = List.find_opt (fun x -> x = color) colors

let () =
  match result with
  | Some c -> printf "found %s\n" c
  | None -> print_endline "failed to find green\n"
```

## Tuples

A tuple is an immutable, ordered collection of values whose types can differ.

To create a tuple, surround the elements in parentheses
and separate them with commas.
For example:

```ocaml
let t = (true, 3, "blue")
```

This has the type `bool * int * string`
which is referred to as a "product type".

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

```ocaml
let rec sum list =
  match list with
  | [] -> empty_value
  | head :: tail -> head + sum tail
```

The standard library provides a large number of functions that operate on lists.
See <a href="https://v2.ocaml.org/api/List.html" target="_blank">Module List</a>.

The `List.map` function maps over one list.

The `List.map2` function maps over two lists.

The `List.iter` function iterates over a list
in a way that is useful when the function passed to it
has a side effect and a result list is not needed.

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

TODO: Clean up this section.

ordered, fixed length collection of mutable elements that all elements that all have the same type
syntax [| v1; v2; v3 |] creates an array
Why did they choose semicolons instead of commas?
indexed by integers starting from zero

Records
like structs in other languages
to define a record type,
type book = {
series : string;
volume : int;
title : string;
author : string;
mutable stock : int;
}
note how fields can be marked as “mutable”

## Maps

A Map is an immutable collection of key/value pairs.
See https://ocaml.org/docs/maps.

TODO: Add more to this section.

## Functions

OCaml functions are first-class.
They can take other functions as arguments and can return new functions.

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

Functions support partial application.
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

Labelling parameters allows them to be specified by the labels in calls.
Label names are not required to match their coresponding parameter names.

TODO: Finish cleaning up this section.

Unlabelled parameters are positional.
functions can have labelled parameters and they can have default values
to declare a labelled parameter or pass an labelled argument, use ~{name}:{value}
labelled arguments can appear in any order and be mixed with positional arguments
when a function is called with fewer arguments than it has parameters, a new function is returned that is the result of partial application
recursive functions must be defined with “let rec”
optional parameters must be preceded by either ~ (for labelled) or ? (for positional); for example, ?(answer=42)
?(init = 0) is shorthand for ?init:(init = 0).
The first “init” is the argument label and the second is the parameter name. They can differ just like in Swift.

The following code demonstrates two ways to write functions
that extract an element from a tuple.

```ocaml
(* Get the second element of a 4-element tuple using destructuring. *)
let second (_, e, _, _) = e

(* Get the third element of a 4-element tuple using match. *)
let third tuple = match tuple with a, b, c, d -> c
let greek = ("alpha", "beta", "gamma", "delta")

let () =
  print_endline (second greek);
  (* beta *)
  print_endline (third greek)
(* gamma *)
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

When using git for version control,
the `_build` directory should be added in the `.gitignore` file.

The `bin`, `lib`, and `test` directories all contain a `dune` file
that holds configurations options for that directory.
These files using Lisp-like syntax.

To build the project, enter `dune build`.
This creates `_build/default/bin/main.exe`.
To automatically rebuild the project
when code changes are detected, add the `--watch` flag.

To run the project, enter `dune exec {project_name}`.

### Example Project

Let's walk through creating a small OCaml project with Dune.

1. `cd` to the directory where the project should be created.
1. Enter `dune init project demo`
1. `cd demo`
1. Enter `dune exec demo`.
1. Verify that the output is "Hello, World!".
1. Create the file `bin/math_local.ml` containing the following:

   ```ocaml
   let add a b = a + b
   let average a b = float_of_int (add a b) /. 2.0
   ```

1. Modify the file `bin/main.ml` to contain the following:

   ```ocaml
   open Printf

   let () =
     let a = 1 and b = 2 and c = 3 in
     printf "sum = %d\n" (Math_local.add a b);
     printf "average = %f\n" (Math_local.average b c)
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
   sum = 3
   average = 2.500000
   ```

1. Now let's try defining the math functions in a library.
   Copy `lib/math_local.ml` to the `lib` directory
   and rename it to `math_lib.ml`.

1. Edit `bin/main.ml`.

   Add the line `open Demo` at the beginning.  
   Change the two references to `Math_local` to `Math_lib`.

1. Enter `dune exec demo`.
1. Verify that the output is the same.

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

1. Add the following lines in `lib/math_lib.ml`:

   ```ocaml
   (* This is an inline test for the add function. *)
   let%test _ = add 1 2 = 3

   (* This is an expectation test for the average function. *)
   let%expect_test _ =
     print_float (average 2 3);
     [%expect "2.5"]
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

   let () = assert ((Math_lib.add 2 2) = 4)
   ```

1. Create the file `test/average.ml` containing the following:

   ```ocaml
   open Demo

   let () = print_float (Math_lib.average 2 3)
   ```

1. Create the file `test/average.expected` containing `2.5`.

1. Enter `dune test` and verify that all the tests pass.

# HTTP Servers

There are several OCaml libraries for implementing HTTP servers
that provide API endpoints. Popular options include:

- <a href="https://aantron.github.io/dream/" target="_blank">Dream</a> - 1.5K GitHub stars
- Opium - 747 GitHub stars
- CoHTTP - 676 GitHub stars
- Ocsigen - not in GitHub

## Converting OCaml to JavaScript

See http://ocsigen.org/js_of_ocaml/latest/manual/overview.

## Unorganized Content

Type Inference
OCaml infers most types
almost never need to specify the types of variables or function parameters
can declare with a type annotation with the syntax (name : type)

Numeric Operators
+, -, _, and / for int values
+., -., _., and /. for float values
This distinction is made to avoid having operators that are overloaded for multiple types.

Conditional Logic
if-then-else is an expression
if expr1 then expr2 else expr3
expr1 must evaluate to a bool
int and other values are not automatically interpreted as a bool
expr3 must evaluate to the same type as expr2 so the “if” expression always evaluates to the same type
normally an else branch is required (maybe can omit if the then branch can evaluate to the “unit value” ()
The = operator is used for assignment AND for comparison.
can be used where other languages use a ternary operator
ex. let sign = if result > 0 then “positive” else if result < 0 then “negative” else “zero”
pattern matching
let sign = match …