---
eleventyNavigation:
  key: Zig
layout: topic-layout.njk
---

## Overview

<figure style="width: 50%">
  <img alt="Ziggy, the Zig mascot"
    src="/blog/assets/ziggy.svg?v={{pkg.version}}">
  <figcaption>Ziggy, the lizard Zig mascot</figcaption>
</figure>

{% aTargetBlank "https://ziglang.org", "Zig" %}
is a free, open source, high performance, systems programming language.
It is a modern alternative to C with much of the same syntax
such as statements terminated with semicolons and conditions in parentheses.

Zig is suitable for applications that care deeply
about performance and/or binary size
which justifies the tedium and verbosity of manual memory management.

Zig provides a complete toolchain for creating, developing, building,
and testing apps written in Zig, C, and C++.
It supports LLVM cross-compilation to integrate with C and C++.
There are advantages to building apps with the Zig compiler
even if they have not Zig code and only use C and/or C++ code.

Zig includes a package manager, a build system API (used in `build.zig`` files),
cross-compilation support, and a test runner.

Zig was created by Andrew Kelly in 2016.
It is maintained by the Zig Software Foundation (ZSF).

## Used By

- {% aTargetBlank "https://bun.sh", "Bun" %} - a JavaScript/TypeScript
  runtime and toolchain, is primarily written in Zig.
  Bun has many advantages over Node.js and Deno including much better performance.

- {% aTargetBlank "https://www.roc-lang.org", "Roc" %} -
  "a fast, friendly, functional language"
  "Roc's compiler has always been written in Rust.
  Roc's standard library was briefly written in Rust,
  but was soon rewritten in Zig."

- {% aTargetBlank "https://machengine.org", "Mach" %} -
  a game engine and graphics toolkit, is implemented in Zig.

## Installing

To install, download a platform-specific zip or tar file from
the {% aTargetBlank "https://ziglang.org/download/", "Releases" %} page,
expand it, move the directory this creates to a desired location,
set the environment variable `ZIG_PATH` to point to this directory, and
add `ZIG_PATH` to the list of directories in the `PATH` environment variable.

In macOS and easier option is to install Zig
with Homebrew by entering `brew install zig`.
However, this may currently only work on Macs with Intel-based processors.

For more detail on installation options, see {% aTargetBlank
"https://ziglang.org/learn/getting-started/#installing-zig", "Installing Zig" %}.

## Getting Started

The following code, in the file `hello.zig` is a basic hello world program.

```zig
const std = @import("std");

pub fn main() void {
    // s for string, d for decimal
    std.debug.print("Hello {s}! {d}\n", .{"Zig", 2023});
}
```

The print methods expects a certain type of struct as its second argument.
Creating an instance with .{} uses that type.

To build and run this program, enter `zig run hello.zig`.

To create an executable, enter `zig build-exe --name hello hello.zig`.
To create an executable for a different OS, specify the -target option.
For example, `-target x86_64-windows`.

To customize the executable, add one of the following compiler flags.
These cause the executable to be optimized for
some combination of size, speed, and runtime safety.

| Compiler flag   | Runtime safety checks | Optimizations |
| --------------- | --------------------- | ------------- |
| -O Debug        | Yes                   | No            |
| -O ReleaseSafe  | Yes                   | Yes (speed)   |
| -O ReleaseSmall | No                    | Yes (size)    |

To run the executable, enter `./hello`.

To format a `.zig` source file, enter `zig fmt {file-name}.zig`.

There is no linter for Zig, but the Zig compiler
provides more guidance than most compilers.

## Tools

For VS Code, see the extension {% aTargetBlank
"https://github.com/ziglang/vscode-zig", "Zig Language" %}.

## Resources

- {% aTargetBlank "https://ziglang.org", "Zig home page" %}
- {% aTargetBlank "https://blog.logrocket.com/getting-started-zig-programming-language/",
  "Getting started with the Zig programming language" %}
- {% aTargetBlank "https://github.com/ratfactor/ziglings", "Ziglings" %} -
  "A series of tiny broken programs ...
  By fixing them, you'll learn how to read and write Zig code."

## Zig Projects

To create a new project, create a directory for it, cd to the directory,
and enter `zig init-exe`.
This creates the file `build.zig` and
a `src` directory containing the file `main.zig`.

The file `build.zig` is a build script that uses the compiler API.
Modify this file to change the characteristics of executable that is produced.

The file `main.sig` is the starting point of the application.
Like many `.zig` files, this begins by importing the standard library
with `const std = @import("std");`
It also defines the main function with `pub fn main() !void { ... }`.
The `!` means the function can return an error value.

To run the app, enter `zig build run`.

## Comments

Single-line comments begin with `//`.

Zig does not support multi-line comments.

Doc comments begin with `///`.
These are used to document variables and functions.

Top-level comments begin with `//!`.
These are used to document the current module (source file).

## Primitive Types

Zig supports a large number of primitive types.

- signed integers: `i8`, `i16`, `i32`, `i64`, `i128`
- unsigned integers: `u8`, `u16`, `u32`, `u64`, `u128`
  The `u8` type can be used to hold a single character.
  Single character literals are enclosed in single quotes.
- floating point: `f16`, `f32`, `f64`, `f80`, `f128`
- `isize`, `usize`
- C types: `c_char`, `c_short`, `c_ushort`, `c_int`, `c_uint`,
  `c_long`, `c_ulong`, `c_longlong`, `c_ulonglong`, `c_longdouble`
- `bool`
- `anyopaque`
- `void`
- `noreturn`
- `type`
- `anyerror`
- `comptime_int`
- `comptime_float`

In addition to these primitive types, "arbitrary bit-width integers can be
referenced by using an identifier of `i` or `u` followed by digits."
For example, the identifier `u3` refers to an unsigned 3-bit integer.
"The maximum allowed bit-width of an integer type is 65535."

## CLEANUP EVERYTHING BELOW HERE!

- does it always catch when memory is not freed?
- supports low-level memory control using allocators
  - page_allocator, c_allocator, ArenaAllocator, FixedBufferAllocator
- not memory-safe like Rust
- no operator overloading
- no exceptions; functions that can fail must return an error value (typically an enum?)
- comptime blocks run at compile time; takes the place of preprocessor directives and macros
- comptime variables are initialized at compile time
- has a builtin testing framework that uses the `test` keyword followed by a description string and a block of code that implements the test; enter `zig test {file-path}` to run the tests in a given file
- `zig cc` is an alternate C/C++ compiler
  - can build a platform-specific executable for the current platform
  - can build an executable for a specified platform
  - can find bugs that standard C/C++ compilers do not

To create a new library, enter `zip init-lib`.

Data types include u8 (single byte unsigned integer), …
Strings are delimited with double quotes.

The `defer` keyword specifies a function to be called when the function it is inside exits.
This is often used to deallocate memory allocated on the line before.
For example, `var allocator = std.heap.page_allocator; var myList = std.ArrayList(10).init(allocator); defer myList.deinit();`
Also see `errdefer` which specifies a function to call if an error occurs in the current scope.

Zig standard library

- document this

Primitive Values

- null, undefined
- true, false

Strings

- “String literals are constant single-item points to null-terminated byte arrays.”
- var name = “Mark”;
- dereferencing converts to array of characters
- default encoding is UTF-8
- multi-line string literals precede each line with `\\`
- to iterate over the bytes in a string (characters if ASCII), for (name) |byte| { … }
- - operator can be used to create a string by concatenating characters; var name = ‘M’ + ‘a’ + ‘r’ + ‘k’

Arrays

- to create an array from a literal, const numbers = [_]u32{ 19, 21, 7 }; // \_ indicates not specifying size
- to initialize all elements to same value, const numbers = [_]u32{0} \*\* 3; // array of 3 zero elements
- use var instead of const to make them mutable
- have a `len` field
- to get an array element, numbers[1]
- to mutate an array element, numbers[1] = 3;
- ++ operator concatenates arrays, returning a new array
  - applies to strings; var name = “Ma” ++ “rk”;
- \*\* operator multiplies arrays, returning a new array
- multidimensional arrays are created by nesting arrays
  - var matrix = [3][3]f64 {
  - [_]f64{ 1.0, 2.0, 3.0 },
  - [_]f64{ 4.0, 5.0, 6.0 },
  - [_]f64{ 7.0, 8.0, 9.0 }
  - };
  - access elements with matrix[row][column]
  - iterate over with nested for loops
  - for (matrix, 0..) |row, rowIndex| {
  - for (row, 0..) |value, columnIndex| {
  -     // use value, rowIndex, and columnIndex here
  - }
  - }

Slices

- “A slice is a pointer and a length. The difference between an array and a slice is that the array's length is part of the type and known at compile-time, whereas the slice's length is known at runtime.”
- like arrays, slices have a `len` field
- to get a slice from an array, var mySlice = myArray[startIndex..endIndex]; // inclusive?
- access and modify slice elements just like with array elements
- do both perform bounds checking and generate a well-known error when outside?

Vectors

- sets of primitive values or pointers
- can operate on them in parallel using SIMD instructions if available using standard operators that return a new vector
  - const v1 = @Vector(_, f32){ 1.2, 2.3, 3.4 }; // need to specify length instead of using _?
  - const v2 = @Vector(\_, f32){ 9.8, 8.7, 7.6 };
  - const v3 = v1 + v2;
- vectors are compatible with fixed-length arrays with the same length or slices of arrays with the same length; can be assigned to each other

Enumerations

- to define, const Color = enum { red, green, blue, };
- to use, var color = Color.red;
- to make tag values available, specify the tag type
  - for example: const Color = enum(u8) { red, green, blue, }; // assigns 0, 1, and 2
- to define with specific values, const Color = enum(u8) { red = 10, green = 20, blue = 30, };

Standard Library Data Structures

- ArrayList - “a contiguous, growable list of items in memory”
- AutoArrayHashMap
- AutoArrayHashMapUnmanaged
- StringArrayHashMap
- StringArrayHashMapUnmanaged
- ArrayHashMap
- BufMap
- BufSet - a set of strings
- AutoHashMap
- AutoHashMapUnmanaged
- StringHashMap
- StringHashMapUnmanaged
-
- more?
- Set?

Variables

- can declare at file scope or in function scopes
- variables must be initialized, but an initialize to `undefined`; ex. `var score: u32 = 0; const maxScore = 10;`
- declare immutable variables with const or mutable variables with var; prefer const when possible
- does it support type inference?
- variable names cannot shadow those in outer scopes
- names must start with a letter and contain letters, numbers, and underscores
- non-conforming names can be used with `@“some name”`
- variables declared outside any function are referred to as “container-level variables”
  - includes variables declared inside struct, union, enum, and opaque (similar to a struct; used for interacting with C code that doesn’t expose field details) definitions (only top-level ones?)

Optionals

- to allow a value to have a specific type or `null`, precede the type with `?`
- for example, const maybeNumber: ?i32 = null;
- Zig prevents null references by using optional pointers whose usages are checked by the compiler
- the `orelse` operator unwraps optional values and provides a value to use if it is null
  - const ptr = optionalPtr orelse 0; // assumes optionalPtr is a pointer to an integer

Operators

- +, -, \*, /, %
- operator assignment: +=, -=, \*=, /=, %/
- bitwise: <<, >>, &, |, ^, ~
- bitwise assignment: <<=, >>=, &=, |=, ^=
- orelse, .?
- and (not &&), or (not ||), ! (not not; inconsistent!)
- catch
- ==, !=, >, >=, <, <=
- array concatenation with ++
- array multiplication with \*\*
- pointer dereference with .\*
- address of with &
- merge error sets with ||
- many wrapping operators
- many saturating operators
- does not support the ++ and — operators found in C

Structs

- struct types are defined with `const SomeName = struct { f1: type1, f2: type2 ,};`
- struct instances are created with `var someInstance = SomeName { .f1 = v1, .f2 = v2, };`
- anonymous struct syntax alternative: `var someInstance: SomeName = .{ .f1 = v1, .f2 = v2 };`
- can any field value be `undefined`?
- to define a method in a struct, add a `pub` function inside its definition
- to define a constant in a struct, add `pub const NAME = value;`
- structs can be used to implement generic functions
  - “A generic data structure is simply a function that returns a type.”
  - this example from official docs returns an instance of a dynamically defined struct type for a linked list whose nodes hold values of a given type
  - Node is a nested type that is available through the returned type
  - fn LinkedList(comptime T: type) type {
  - return struct {
  -     // This just declares a type used for the struct fields below.
  -     pub const Node = struct {
  -       prev: ?*Node, // optional value
  -       next: ?*Node, // optional value
  -       value: T
  -     };
  -     first: ?*Node,
  -     last: ?*Node,
  -     length: usize,
  - }
  - var numberList = LinkedList(i32) { .first = null, .last = null, .length = 0 };
  - const NumberList = LinkedList(i32);
  - var node = NumberList.Node { .prev = null, .next = null, .value = 19 };
  - var numberList = LinkedList(i32) { .first = @node, .last = &node, .length = 1 };

Tuples

- defined by an anonymous struct with no field names
- fields are given names that are integers starting from 0
- access a field value with [index] syntax
- const myTuple = { true, “hello”, @as(u32, 19) };
- myTuple[1] is “hello”
-

Unions

- defines a set of possible types for a value
- const DataType = union { number: i32, truth: bool };
- var data1 = DataType { .number = 19 };
- var data2 = DataType { .truth = true };
- anonymous union syntax alternative: `var data2: DataType = .{ .number = 19 };`
- tagged union types use an enum for their cases and can be used in switch expressions
- const DataTypeTag = enum { number, truth };
- const DataType = union(DataTagType) { number: i32, truth: bool };
- can get the tag type of a union type with std.meta.Tag(DataType) which returns DataTypeTag

Control Structures

- if statements are just like in C
  - if (cond1) { … } else if (cond2) { … } else { … }
  - but they are expressions
  - the ternary operator is not supported, but an if expression can be used instead
    - const value = if (condition) 19 else 21;
  - to test whether a variable with an optional type is currently set to null, if (variable) |value| { use value here } else { value was null }; else part is optional
  - could use `unreachable` statement in else to intentionally crash; asserts that a code location should never be reached
  - can also just compare to null if value isn’t needed; if (variable == null) …
- switch statements are a bit different than in C
  - cases are referred to as “branches”
  - branches do not fall through
  - switch (expression) {
  -         1…10 => { // range of values
  -             …
  -         },
  -         20, 30 => { // list of values
  -             …
  -         },
  -         else => { // required unless the branches are exhaustive
  -             …
  -         }
  -     }
  - it must be possible to coerce all branch values to a common type
  - can switch on number and enum values; can it switch on string values?
  - switch statements can be used as expressions
    - const result = switch (expression) {
    - 1 => “single”,
    - 2 => “couple”,
    - 3 => “few”,
    - else => “many”
    - }
- while loops are just like in C
  - while (condition) { … }
  - while (condition) : (updates) { … }
    - this uses while like a C for loop
    - for example, var i: i32 = 1; while (i < 10) : (i += 1) { … }
  - if condition is a function call that returns an optional value, the loop exits if null is returned
    - for example, while (iterator.next()) |item| { … }
  - can use break to exit (break :someLabel;)
  - can use continue to skip to the next iteration (continue :someLabel)
  - can break or continue or a labelled outer loop
  - outer: while (condition1) {
  - while (condition2) {
  -     if (condition3) break :outer;
  - }
  - }
- while loops are expressions
  - to specify their value, `break someValue;`
  - if the loop can exit without breaking, add an else clause to specify the value
    - while (condition) { … } else someValue;
  - else is only evaluated if the loop does not break
- while loops can catch errors when the condition is a function call that returns an error union type
  - while (someFunction()) |value| {
  - // Process non-error value here.
  - } else |err| {
  - // Handle err here.
  - }
  - The else block is only evaluated if someFunction returns an error value.
- for loops are different than in C
  - in this example, sequence is an array or a slice
  - for (sequence) |item| {
  -     …
  - }
  - to get both item values and indexes
    - for (sequence, 0..) |item, index| { … }
  - to iterate over a range of integers, for (start..end) |value| { … }
  - to iterate over multiple arrays or slices that have the same length at the same time, for (seq1, seq2) |v1, v2| { … }
  - to iterate over a sequence by value so the items can be mutated, for (&sequence) |_item| { item._ = newValue }
- for loops are expressions
  - to specify their value, `break someValue;`
  - if the loop can exit without breaking, add an else clause to specify the value
    - for (condition) { … } else someValue;
  - else is only evaluated if the loop does not break
- like while loops, nested for loops can be labelled to enable breaking out of or continuing an outer loop

Functions

- parameters are immutable; function bodies cannot modify them
- primitive types are passed by value (copy)
- Zig will decide whether to pass non-primitive types (structs, unions, and arrays) by value or reference based on which it determines is faster
- to force pass by reference, pass a pointer
  - to pass an argument by reference (a pointer), precede with &
  - to accept a parameter as a pointer, precede type with \*
- if there is no return value, the return type is void
- if an error can be returned, precede the return type with !
- to declare, fn someName(p1: type1, p2: type2) returnType { … }
- to make available outside current source file and callable from C, precede with `export`; is this only for creating libraries?
- to allow other source files to import the function, precede it with `pub`
- to define a type that is a pointer to a function and can be used as a argument type, const myFnType = \*const fn (p1: type1, p2: type2) return type;
- variable parameter types and reflection
  - to allow any type of value to be passed as an argument, use `anytype` for the parameter type
  - inside the function, use the builtin function `@TypeOf(variableName)` to refer to the actual type
  - inside the function, use the builtin function `@typeInfo(variableName)` to get information about the actual type
    - @typeInfo(@TypeOf) returns a std.builtin.Type object
    - what are the properties and methods on a Type object?
- can use reflection to get details about the parameters and return type of a given function with `@typeInfo(@TypeOf(someFunction)).Fn` which gives an object with the fields `params` (an array of objects with `type` and `name`? fields?) and `return_type` (a type)

Blocks

- lines of code surrounded by curly braces that define a variable scope
- labelled blocks are expressions that return a value
- const value = myLabel: { add code to compute value here; break :myLabel value }

Errors

- The `try` keyword provides error handling.
- Possible errors cannot be ignored.
- The catch keyword is used to provide a default value for an expression that results in an error. - const number = parseU64(str, 10) catch 0; - can compute value with a labeled block -
  catch blk: {
  // do things - break :blk 13; - }; -
- If no `catch` block is included, the error is returned to the caller
- error sets are similar to enums and describe the errors that can be thrown by functions
- example from official docs
  - const FileOpenError = error { AccessDenied, OutOfMemory, FileNotFound };
  - const String = [_]u8; // TODO: Is this the proper way to describe a string type?
  - fn processFile(filePath: \*String) FileOpenError { … }

Type Coercion and Casting

- Add detail on this!

Memory Management

- Zig does not provide any memory management and has no runtime.
- In C, memory is managed with the functions malloc, free, and realloc. These use a provided memory allocator.
- Zig allows selection of a memory allocation strategy and does not choose a default strategy.
- It is also possible to implement a custom allocator.
- All functions and data structures that allocate memory take an Allocator argument that specifies the strategy to use.
- The C strategy is available as std.heap.c_allocator.
- Other provided allocations include:
  - std.heap.FixedBufferAllocator
  - std.heap.ThreadSafeFixedBufferAllocator
  - std.heap.ArenaAllocator
  - std.testing.Allocator
  - std.testing.FailingAllocator
  - DESCRIBE EACH OF THESE
  - ARE THERE MORE PREDEFINED ALLOCATORS?

Builtin Functions

- list all of these
- Async Functions
- not currently supported, but will be in the future

noreturn

- the type of functions that never finish
- also the type of break, continue, return, unreachable, and the while construct `while (true) { … }`

Zig has an official style guide that is not enforced by the compiler

- 4-space indentation
- open braces on the same line or the last of wrapped lines
- maximum line length is 100
- function names should be camelCase
  - but functions that return a type should be TitleCase
- type names should be TitleCase
- variable names should be snake_case
- names of files that define a struct should have the same name as the struct which uses TitleCase
- all other file names should be snake_case
- directory names should be snake_case
