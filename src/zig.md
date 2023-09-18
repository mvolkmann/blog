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
about performance and/or binary size.
which justifies the tedium and verbosity of manual memory management.
Zig does not provide automated garbage collection.

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
const print = std.debug.print;

pub fn main() void {
    // s for string, d for decimal
    print("Hello {s}! {d}\n", .{"Zig", 2023});
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
- {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std",
  "Zig Standard Library" %}
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

## Variables

## Ranges

Ranges of numbers have an inclusive lower bound
and an upper bound that is either exclusive or inclusive.
For example, the range `5..7` includes the values 5 and 6
and the range `5...7` includes the values 5, 6 and 7.

## Control Structures

Zig supports four control structures, `if`, `switch`, `while`, and `for`.
All of these can be used as expressions that result in a value.

### if Expressions

The syntax for `if` expressions is nearly
identical to that of C `if` statements.
The most basic form follows:

```zig
if (cond1) {
    // code goes here
} else if (cond2) {
    // code goes here
} else {
    // code goes here
}
```

Here is an example that includes random number generation and
simplifies output of strings type is typically `[]const u8`.

```zig
const std = @import("std");
const print = std.debug.print;

// The value of the first argument to print must be known at compile time.
fn log(comptime text: []const u8) void {
    print(text ++ "\n", .{});
}

pub fn main() !void {
    // prng is short for pseudo random number generator.
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        // try can only be used inside a function.
        try std.os.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });

    // Generate a random integer from 1 to 3.
    const value = prng.random().intRangeAtMost(u8, 1, 3);
    print("value = {}\n", .{value});

    if (value == 1) {
        log("one");
    } else if (value == 2) {
        log("two");
    } else {
        log("other");
    }
}
```

Zig does not support the ternary operator from C.
An `if` expression can be used instead.
The following example sets the variable `value` to either `19` or `21`.

```zig
const value = if (condition) 19 else 21;
```

An `if` expression can test whether a variable with an optional type
is currently set to `null`.
If not, the value is unwrapped and made available in the body.

```zig
if (variable) |value| {
    // use value here
} else { // optional part
    // value was null
}
```

Here is an example that demonstrates using an optional integer.

```zig
const std = @import("std");
const print = std.debug.print;

fn report(wrapper: ?i8) void {
    if (wrapper) |value| {
        print("value = {}\n", .{value});
    } else {
        print("value is null\n", .{});
    }
}

pub fn main() void {
    var wrapper: ?i8 = null;
    report(wrapper);
    wrapper = 19;
    report(wrapper);
}
```

The following code demonstrates testing whether a variable with an optional type
is currently set to null without unwrapping the value.

```zig
if (variable == null) {
    // code goes here
}
```

### switch Expressions

The syntax for `switch` expressions is a bit different
than in C `switch` statements.

Cases are referred to as "branches".
Branches can match a single value, a list of values, or a range of values.
These are followed by the `=>` operator which is followed by
an expression, a statement, or a block of code.

It must be possible to coerce all branch values to a common type.
Supported branch value types include numbers and enums.
Switching on strings is not supported.

Here is an example that demonstrates all the branch options:

```zig
const std = @import("std");
const print = std.debug.print;

// The value of the first argument to print must be known at compile time.
fn log(comptime text: []const u8) void {
    print(text ++ "\n", .{});
}

pub fn main() !void {
    // prng is short for pseudo random number generator.
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        // try can only be used inside a function.
        try std.os.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });

    // Generate a random integer from 1 to 3.
    const value = prng.random().intRangeAtMost(u8, 1, 10);
    print("value = {}\n", .{value});

    switch (value) {
        1 => log("one"),

        // Double-dot ranges have an exclusive upper bound.
        // Triple-dot ranges have an inclusive upper bound.
        2...5 => { // braces are only required for multiple statements
            log("two to five");
        },

        6, 8, 10 => {
            log("six, eight, or ten");
        },

        // The else branch must be specified
        // unless the other branches are exhaustive.
        else => log("other")
    }
}
```

Here is an example of a `switch` expression used to obtain a value:

```zig
    const result = switch (value) {
        1 => "single",
        2 => "couple",
        3 => "few",
        else => "many",
    };
    print("result = {s}\n", .{result});
```

Here is an example of switching on an `enum` value.
The `else` branch is not needed because the branches are exhaustive.

```zig
    const Color = enum { red, green, blue };
    var favorite = Color.blue;
    // var favorite: Color = .blue; // alternate syntax
    switch (favorite) {
        .red => log("hot"),
        .green => log("warm"),
        .blue => log("cold"),
    }
```

### while Expressions

The syntax for `while` expressions is nearly identical
to that of C `while` statements.

Here are examples of `while` expressions
that demonstrate using the `break` and `continue` statements
which can optionally be followed by a label to jump to an outer loop.

```zig
const std = @import("std");
const print = std.debug.print;

pub fn main() !void {
    var value: u8 = 0;

    // This loop outputs 1, 2, 3.
    while (true) {
        value += 1;
        print("{}\n", .{value});
        if (value == 3) break;
    }

    value = 0;
    // This loop outputs 1, 2, 4, 5.
    while (value < 5) {
        value += 1;
        if (value == 3) continue;

        print("{}\n", .{value});
    }
}
```

A `while` expression can be used like a `C` `for` loop
where a single update is specified in a second pair of parentheses
that is separated from the first pair with a colon.
Here is an example.

```zig
    value = 1;
    // This loop outputs 1, 2, 3.
    while (value <= 3) : (value += 1) {
        print("{}\n", .{value});
    }
```

If the condition passed to a `while` expression evaluates to `null`,
the loop exits.
This can be used to iterate over the return values
of a function that has a optional return type.
Here is an example:

```zig
const std = @import("std");
const print = std.debug.print;

var counter: u8 = 0;
fn nextCounter() ?u8 {
    counter += 2;
    if (counter > 6) return null;
    return counter;
}

pub fn main() !void {
    // This loop terminates when the nextCounter function returns null.
    // It outputs 2, 4, 6.
    while (nextCounter()) |c| {
        print("{}\n", .{c});
    }
}
```

A `while` expression can be used to obtain a value.
The value is specified by a `break` statement with a value.
If it is possible for the loop to exit without hitting a `break` statement,
add an `else` clause to specify the value.
The `else` clause is only evaluated if the loop does not `break`.
Here is an example:

```zig
    value = 0;
    // result is "triple" if value starts a 1
    // and "not found" if value starts at 0.
    const result = while (value < 10) {
        if (value == 3) break "triple";
        value += 2;
    } else "not found";
    print("result = {s}\n", .{result});
```

A `while` expression can catch errors when its expression evaluates to an error.
The `else` clause is only evaluated if an error occurs.
Here is an example:

```zig
const std = @import("std");
const print = std.debug.print;

// TODO: Can errors contain associated data?
const FetchError = error{TooBig};
var count: u8 = 0;
fn fetchCount() FetchError!u8 {
    count += 2;
    return if (count > 6) FetchError.TooBig else count;
}

pub fn main() !void {
    // This loop terminates when the fetchCount function returns an error.
    // It outputs 2, 4, 6 followed by "error.TooBig".
    while (fetchCount()) |c| {
        print("{}\n", .{c});
    } else |err| {
        print("err = {}\n", .{err});
    }
}
```

### for Expressions

The syntax for `for` expressions is very different from C `for` statements.
They can be used in several ways.

A `for` expression can iterate over the items in an array or slice.
Here is an example.

```zig
const std = @import("std");
const print = std.debug.print;

pub fn main() !void {
    // Create an array of numbers.
    const numbers = [_]u8{ 10, 20, 30, 40, 50 };

    // This loop outputs all the numbers.
    for (numbers) |number| {
        print("{}\n", .{number});
    }

    // Create a slice from an inclusive index to an exclusive index.
    const slice = numbers[1..4];

    // This loop outputs 20, 30, 40
    // which are the numbers at index 1, 2, and 3.
    for (slice) |number| {
        print("{}\n", .{number});
    }
}
```

A `for` expression can iterate over a range of integers.
Here is an example:

```zig
    // Iterate over a range of numbers where
    // the first is included and the last is not.
    // This loop outputs 10, 11, 12, 13, 14, but not 15.
    for (10..15) |number| {
        print("{}\n", .{number});
    }
```

A `for` expression can iterate over any number of arrays and slices
at the same time. Each must have the same length.
Here is an example:

```zig
    const letters = "ABCDE";
    // This loop outputs the ASCII code of each letter
    // followed by the number at the same index.
    // The letters and numbers arrays must have the same length.
    for (letters, numbers) |letter, number| {
        print("{} - {}\n", .{ letter, number });
    }
```

An open-ended range starting from zero can be iterated over
at the same time as other arrays or slices.
Here is an example that outputs the index of each value
in the `numbers`` array along with the corresponding number:

```zig
    for (0.., numbers) |index, number| {
        print("{} - {}\n", .{ index, number });
    }
```

To mutate array or slice items in a loop, iterate over pointers to the items.
Here is an example:

```zig
    var mutable = [_]u8{ 1, 2, 3 };
    for (&mutable) |*item| {
        item.* *= 2; // doubles
    }
    const expected = [_]u8{ 2, 4, 6 };
    // If this fails, the output is "error: TestExpectedEqual"
    // followed by a stack trace.
    // It does not indicate which items are not equal.
    try std.testing.expectEqualSlices(u8, &mutable, &expected);
```

A `for` expression can be used to obtain a value.
The value is specified by a `break` statement with a value.
If it is possible for the loop to exit without hitting a `break` statement,
add an `else` clause to specify the value.
The `else` clause is only evaluated if the loop does not `break`.
Here is an example:

```zig
    // result is "triple" if the range starts a 1
    // and "not found" if the range starts at 0.
    // It seems "for" expressions can use double-dot ranges,
    // but not triple-dot ranges. Why?
    const result = for (1..10) {
        if (value == 3) break "triple";
        value += 2;
    } else "not found";
    print("result = {s}\n", .{result});
```

Like `while` expressions, `for` expressions
can use `break` and `continue` statements
which can optionally be followed by a label to jump to an outer loop.

## Error Handling

Errors are represented by enum values.
TODO: Can they carry additional data?

TODO: Add more detail here!

```zig
const std = @import("std");
const print = std.debug.print;

const DemoError = error{Demo};
fn demo(good: bool) DemoError!u8 {
    return if (good) 19 else DemoError.Demo;
}

// Only need ! in return type if errors are not caught.
pub fn main() !void {
    // Not catching possible errors.
    var result = try demo(true);
    print("result = {}\n", .{result});

    // Catching possible errors.
    // result = demo(false) catch |err| {
    result = demo(false) catch |err| {
        print("err = {}\n", .{err});
        return;
    };
    print("result = {}\n", .{result});
}
```

## Unreachable Code Paths

The `unreachable` statement asserts that a code path should never be reached.
This calls the builtin `panic` function
with the message "reached unreachable code"
which terminates the application and outputs a stack trace.

## Functions

## Standard Library

### ArrayList

The {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:ArrayList",
"ArrayList" %} data structure is "a contiguous, growable list of items in memory."

Instances of `ArrayList` have the fields `items`, `capacity`, and `allocator`.
Instances have the methods `append`, `appendSlice`, `clone`, `deinit`,
`getLast`, `getLastOrNull`, `init`, `insert`, `insertSlice`, `orderedRemove`,
`pop`, `popOrNull`, `replaceRange`, `writer`, and many more.

Here is an example that creates an `ArrayList` instance,
adds items to it, and iterates over them.

```zig
const std = @import("std");
const print = std.debug.print;
const allocator = std.heap.page_allocator;

pub fn main() !void {
    var list = std.ArrayList(i32).init(allocator);
    defer list.deinit(); // frees when function exits
    // The append method can return an error if the allocator
    // cannot allocate enough memory for the item being added.
    try list.append(19);
    try list.append(21);

    // This loop outputs 19 and 21.
    for (list.items) |value| {
        print("{}\n", .{value});
    }
}
```

### HashMap

The {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:HashMap",
"HashMap" %} data structure is a collection of key/value pairs.

TODO: Add an example.

### MultiArrayList

The {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:MultiArrayList",
"MultiArrayList" %} data structure "stores a list of a struct or tagged union type".
"Instead of storing a single list of items, MultiArrayList stores
separate lists for each field of the struct or lists of tags and bare unions."

TODO: Add an example.

## Tests in Code

Unit tests can be included in source files
in order to test the functions they define.

Each test is described by the `test` keyword
followed by a function name or a test description string and
a block of code that uses the `expect` function to make assertions.

Tests are executed by running `zig test {file-name}.zig`.

Here is a basic example:

```zig
const std = @import("std");
const expect = std.testing.expect;

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test add { // uses a function name
    try expect(add(1, 2) == 3); // passes
}

test "add works" { // uses a description string
    try expect(add(1, 2) == 3); // passes
    try expect(add(2, 3) == 50); // fails
}
```

If an `expect` call fails, its test stops, but other tests are still run.
The output includes the following:

- a message of the form
  "Test [{m}/{n}] test.{test-description}... FAIL (TestUnexpectedResult)"
  for each failed test
- a stack trace is output that shows the failing `expect` (only one of them?)
- a summary of the form "{n1} passed; {n2} skipped; {n3} failed"

## CLEANUP EVERYTHING BELOW HERE!

- Functions can specify the type of errors they can return by preceding the
  ! in the return type with an error type or probably an error set.
- Precede function return types with ? if null can be returned.
- Investigate Zig string libraries.
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
