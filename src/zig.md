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

Zig emphasizes:

- No hidden control flow.

  Examples of hidden control flow in other languages include
  exception handling, operator overloading, and destructors.

- No hidden memory allocations.

- No preprocessors or macros.

  In place of these, Zig uses code that runs at compile-time,
  indicated by the `comptime` keyword.

Zig includes:

- a package manager
- a build system that is simpler that the
  combinations of build tools typically used with C and C++
- a build system API (used in `build.zig` files)
- cross-compilation support
- a test runner.

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

The print methods expects a literal array as its second argument.
The syntax for a literal array or struct is `.{}`
where array elements or struct fields appear in
a comma-separated list inside the curly braces.

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
- {% aTargetBlank "https://ziglearn.org", "ziglearn.org" %}

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
It relies on code editors to make it
easy to comment and uncomment ranges of lines.

Top-level comments begin with `//!`.
These are used to document the current module (source file).

Doc comments begin with `///`.
These are used to document variables and functions.

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

The syntax for declaring a variable is:

```zig
{const|var} {name}[: type] = {value};
```

The parts inside curly braces are required and
the parts inside square brackets are optional.

Variable declared with `const` are immutable and
variable declared with `var` are mutable.
Using `const` is preferred when possible.

The convention for variable names is to use snake_case.

The type can be omitted if it can be inferred from the value.

An initial value is required, but can be set to `undefined`.
The compiler does not currently check that
the variable is set to another value before it is used.
This can produce undesirable results or errors,
so it's best not to set variables to `undefined`.
Runtime checks for this may be added in the future.

The builtin function `@as` performs an explicit type coercion.
This can be used to ensure that the initial value is treated as a specific type.
For example:

```zig
const limit = @as(i8, 5);
print("{d} is {s}\n", .{ limit, @typeName(@TypeOf(limit)) }); // 5 is i8
```

Variable shadowing is not allowed.
Variables cannot have the same name as another in an outer scope.

## Pointers

To get a pointer to the data of a variable, use `&variable_name`.

To dereference a pointer, use `variable_name.*`.
This syntax can be used with chaining when the value is a `struct`
to access a struct field.
For example, `dogPtr.*.name`.

A pointer to a non-`const` value can be used to modify the value
regardless of whether the pointer itself is `const`.
A pointer to `const` value cannot be used to modify the value.

Here are examples of obtaining and using pointers.

```zig
const std = @import("std");
const print = std.debug.print;

const Dog = struct { name: []const u8, breed: []const u8, age: u8 };

pub fn main() void {
    var dog = Dog{ .name = "Comet", .breed = "whippet", .age = 3 };
    const dogPtr = &dog;
    print("name = {s}\n", .{dog.name});
    print("name = {s}\n", .{dogPtr.*.name});

    // Pointers can only be used to modify a struct property
    // if the struct instance is not const.
    dogPtr.*.name = "Oscar";
    print("name = {s}\n", .{dog.name});

    var number: u8 = 1;
    print("number = {d}\n", .{number}); // 1

    // Shorthand operators can be used to
    // modify the value referenced by a pointer.
    const numberPtr = &number;
    numberPtr.* += 1;
    print("number = {d}\n", .{number}); // 2
}
```

## Ranges

Ranges of numbers have an inclusive lower bound
and an upper bound that is either exclusive or inclusive.
For example, the range `5..7` includes the values 5 and 6
and the range `5...7` includes the values 5, 6 and 7.

## Arrays

Array types have the syntax `[length]type`.
For example, `[5]i32` is an array of five integers.
The length can be replaced by an underscore when it can be inferred from an initial value.
For example:

```zig
const dice_rolls = [_]u8{ 2, 6, 1, 5 };
```

Arrays have a len field.

## Strings

Strings are represented by arrays of type `[]u8`.
Zig only provides the ability to operator on strings as byte arrays.

For more functionality, use a string library such as
{% aTargetBlank "https://github.com/JakubSzark/zig-string", "zig-string" %} or
{% aTargetBlank "https://codeberg.org/dude_the_builder/zigstr", "zigstr" %}.

## Slices

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

The conditions must evaluate to a `bool` value.
Other types of values are not interpreted as truthy or falsely.

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
where a single update is specified in a
second pair of parentheses (referred to as a "continue expression")
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
    // If a "continue expression" is included,
    // the colon and that come after the capture in pipes.
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

The syntax for defining a function is:

```zig
[pub] fn {name}([parameter-list]) {return-type} {
    {body}
}
```

Marking a function as `pub` makes it available outside its source file.

The parameter list is a comma-separated list of parameter declarations.
The syntax for each parameter declaration is `{name}: {type}`.

The convention for function names is to use camelCase.

The return type syntax is `[error-type][!][return-type]`.
For example:

- `i32` means an integer is returned and no errors can be returned
- `!i32` means an integer or one of the errors inferred from the function body is returned
- `anyerror!i32` means an integer or any kind of error is returned
- `MyErrorSet!i32` means an integer or one of the errors in `MyErrorSet` is returned

Function parameters cannot be modified.

To call a function that returns a value and not use it, assign it to `_`.
For example, `_ = someFn();`

Zig does not support "vararg" functions,
so functions that would have a variable number of arguments
in other languages take a literal array (`.{}`) instead.

## Reflection

The following builtin functions support reflection:

- `@TypeOf` returns the type of a given variable.
- `@typeName` returns the name of a given type as a string.
- `@typeInfo` returns information about a given type.
- `std.meta.fields` returns information about the fields in a struct.
- `std.meta.trait.hasField` methods determine if a struct
  contains a field with a given name.
- `std.meta.trait.hasFn` methods determine if a struct
  contains a function with a given name.
- `std.meta.trait.*` methods determine the kind of a type. These include
  `isConstPtr`, `isContainer`, `isExtern`, `isFloat`, `isIndexable`, `isIntegral`,
  `isManyItemPtr`, `isNumber`, `isPacked`, `isPtrTo`, `isSignedInt`, `isSingleItemPtr`,
  `isSlice`, `isSliceOf`, `isTuple`, `isUnsignedInt`, and `isZigString`.

```zig
const std = @import("std");
const print = std.debug.print;
const trait = std.meta.trait;

const Dog = struct { name: []const u8, breed: []const u8, age: u8 };

fn nextInteger(n: i32, bigger: bool) i32 {
    return if (bigger) n + 1 else n - 1;
}

pub fn main() void {
    const d = Dog{ .name = "Comet", .breed = "whippet", .age = 3 };

    // Struct reflection
    // This for loop must be inline.
    inline for (std.meta.fields(Dog)) |field| {
        const T = field.type;
        print(
            "Dog struct has field \"{s}\" with type {s}\n",
            .{ field.name, @typeName(T) }
        );

        const value = @field(d, field.name);
        // comptime is required here because the compiler needs to know the type.
        if (comptime trait.isNumber(T)) print("value is {d}\n", .{value});
        if (comptime trait.isZigString(T)) print("value is {s}\n", .{value});
    }

    // Function reflection
    const T = @TypeOf(nextInteger);
    print("nextInteger type is {}\n", .{T}); // fn(i32, bool) i32
    const info = @typeInfo(T);
    if (info == .Fn) { // if T is a function type ...
        // This for loop must be inline.
        inline for (1.., info.Fn.params) |number, param| {
            // Can't get parameter name, only type.
            print("parameter {d} type is {any}\n", .{ number, param.type });
        }
        print("return type is {any}\n", .{info.Fn.return_type});
    }
}
```

In the code above, `info` is a `std.builtin.Type` instance.
It is compared to `.Fn` to test whether `T` is a function type.
A `Type` instance can be compared to any of these:
`.AnyFrame`, `.Array`, `.Bool`, `.ComptimeFloat`, `.ComptimeInt`, `.Enum`,
`.EnumLiteral`, `.ErrorSet`, `.ErrorUnion`, `.Float`, `.Fn`, `.Frame`, `.Int`,
`.NoReturn`, `.Null`, `.Opaque`, `.Optional`, `.Pointer`, `.Struct`,
`.Undefined`, `.Union`, `.Vector`, and `.Void`

## Allocators

See https://ziglang.org/documentation/master/#toc-Choosing-an-Allocator.
for guidelines on choosing an allocator.

- `std.heap.page_allocator`

  Your system/Target page allocator, will give you a whole OS page.

- `std.heap.ArenaAllocator`

  Will use an arena to amortize the freeing of memory, you alloc, you free all at once

- `std.heap.GeneralPurposeAllocator`

  Welp, is a configurable allocator that let you as an extra detect certain errors while using heap

- `std.heap.MemoryPool`

  It allocates one and only one type, but is really fast at it

- `std.heap.FixedBufferAllocator`

  Takes a buffer (any buffer) and transform it into an allocator, which is useful to reuse memory while using alloc functions

- `std.heap.c_allocator`

  If you linked C, you get to use malloc covered in zig info

- `std.heap.raw_c_allocator`

  Same than above, but raw

- `std.heap.HeapAllocator`

  Sooooo windows Heap Allocator

- `std.heap.ThreadSafeAllocator`

  Covers an allocator in a way that calling it between threads is safe (not exactly fast tho)

- `std.heap.SbrkAllocator`

  use Sbrk to alloc, I don't like it, search for Sbrk

- `std.heap.WasmAllocator`

  Wasm is your friend, use this to allocate. Generally used as a backing allocator for GeneralPurpose or Arena

- `std.heap.WasmPageAllocator`

  Dumber WasmAllocator, useful for when you actually wanna do your own memory management via "pages"

- `std.heap.LogToWriterAllocator`

  Allocs never where this easy to track, writes to a custom writer alloc information (When they happenend and such)

- `std.heap.LoggingAllocator`

  Allocs are logged into std.log

- `std.heap.ScopedLoggingAllocator`

  Same as the above but it goes directly to the std.log function, this time with our good ol' scope

- `std.heap.StackFallbackAllocator`

  Some stack, some heap: IF the stack is not enough, go to the heap

## Standard Library

TODO: Add detail here.

## Structs

A `struct` is a custom type that holds a collection of fields
and optional methods. Here is an example:

```zig
const std = @import("std");
const sqrt = std.math.sqrt;
const expect = std.testing.expect;

fn square(n: f32) f32 {
    return std.math.pow(f32, n, 2);
}

const Point = struct {
    x: f32,
    y: f32,

    pub fn distanceToOrigin(self: Point) f32 {
        return sqrt(square(self.x) + square(self.y));
    }

    pub fn distanceTo(self: Point, other: Point) f32 {
        const dx = self.x - other.x;
        const dy = self.y - other.y;
        return sqrt(square(dx) + square(dy));
    }
};

test "Point struct" {
    const p1 = Point{ .x = 3, .y = 4 };
    try expect(p1.distanceToOrigin() == 5);

    const p2 = Point{ .x = 6, .y = 8 };
    try expect(p1.distanceTo(p2) == 5);
    try expect(Point.distanceTo(p1, p2) == 5);
}
```

To get information about all the fields in a struct, use `std.meta.fields`.
For example, the following code can be added to the test above.
For each field it prints the name, the type, and its value in the `p1` instance.
TODO: Maybe add a section just on Zig reflection.

```zig
    print("\n", .{});
    // TODO: Why does this only work with "inline"?
    inline for (std.meta.fields(@TypeOf(p1))) |field| {
        print("found field {s} with type {s}\n", .{ field.name, @typeName(field.type) });
        print("value in p1 is {}\n", .{@as(field.type, @field(p1, field.name))});
    }
}

```

A `struct` can be responsible for managing its own memory.

```zig
pub const PathManager = struct {
    paths: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    // ...

    fn appendFilePaths(self: *PathManager, path: []const u8) !void {
        const dir = std.fs.cwd();
        const file = try dir.openFile(path, .{ .mode = .read _only });
        defer file.close();
        const metadata = try file.metadata();
        switch (metadata.kind()) {
            std.fs.File.Kind.file => {
                const abs_path = try dir.realpathAlloc(self.allocator, path);
                errdefer self.allocator.free(abs_path);
                try self.paths.append (abs_path);
            },
            std.fs.File.Kind.directory => {
                var next_dir = try dir.openIterableDir(path, .{});
                defer next_dir.close();
                var iter = next_dir.iterate();
                while (try iter.next()) |entry| {
                    const next_path = try std.fs.path.join(self.allocator, &[_][]const u8{ path, entry. name });
                    defer self.allocator.free(next_path);
                    try self.appendFilePaths(next_path);
                }
            },
            else => return,
        }
    }
};
```

Using the custom struct above:

```zig
pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var manager = try PathManager.init(allocator, &[_][]const u8{ "src", "test" });
    defer manager.deinit();
}
```

TODO: Add an example of a generic struct using `comptime`.

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

Each test is described by the `test` keyword followed by
a function name or a test description string and a block of code.

The block of code uses the `expect` function to make assertions.
The `expect` function takes a single argument
that must be an expression that evaluates to a `bool` value.
It can return an error and so must be preceded by the `try` keyword.

All tests is a source file are executed by running `zig test {file-name}.zig`.

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

## Stack Example

This example is based on the Primeagen video at {% aTargetBlank
"https://www.youtube.com/watch?v=xIPrwrBAU2c", "Zig Data Structure Katas" %}.

```zig
const std = @import("std");
const log = std.debug.print;
const Allocator = std.mem.Allocator; // memory allocator interface
const expect = std.testing.expect;

// This creates a struct that represents
// a stack whose values are a given type.
fn Stack(comptime T: type) type {
    return struct {
        const Node = struct { value: T, next: ?*Node };

        // Gets the type of the struct we are inside.
        const Self = @This();

        length: usize,
        head: ?*Node, // optional pointer
        allocator: Allocator, // passed to init below

        pub fn init(allocator: Allocator) Self {
            return .{ .length = 0, .head = null, .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            while (self.length > 0) _ = self.pop();
            self.* = undefined;
        }

        pub fn push(self: *Self, value: T) !void {
            var node = try self.allocator.create(Node);
            node.value = value;
            node.next = self.head;
            self.length += 1;
            self.head = node;
        }

        pub fn pop(self: *Self) ?T {
            if (self.head) |unwrapped| {
                defer self.allocator.destroy(unwrapped);
                self.length -= 1;
                self.head = unwrapped.next;
                return unwrapped.value;
            }
            return null;
        }

        pub fn print(self: *Self) void {
            log("\nStack length is {}.\n", .{self.length});
            var node = self.head;
            while (node) |unwrapped| {
                log("=> {}\n", .{unwrapped.value});
                node = unwrapped.next;
            }
        }
    };
}

test "stack" {
    const IntStack = Stack(i32);
    var stack = IntStack.init(std.testing.allocator);
    defer stack.deinit();

    try stack.push(19);
    try expect(stack.length == 1);

    try stack.push(20);
    try expect(stack.length == 2);

    stack.print(); // output is suppressed in tests

    var value = stack.pop();
    try expect(stack.length == 1);
    try expect(value == 20);

    value = stack.pop();
    try expect(stack.length == 0);
    try expect(value == 19);

    value = stack.pop();
    try expect(stack.length == 0);
    try expect(value == null);
}
```

## CLEANUP EVERYTHING BELOW HERE!

Can functions be defined like this?
`const theFunc = fn() void { ... }`

Example of a tagged union:

```zig
const Tagged = union (Tag) { a: u8, b: f32, c: bool };

pub fn main() anyerror!void {
    var value = Tagged{ .b = 1.5 };
    switch (value) {
        .a => |×| std.log.info("a: {}", .{x}),
        .b => |x| std.log.info("b: {}", .{x}),
        .c => |x| std.log.info("c: (" , .{x}),
    }
}
```

Do switch branch ranges require three dots and slice ranges require two dots?

Runtime safety includes array bounds checking, …

To use a for loop to iterate over an array (not a slice), do you have to dereference it with & to turn it into a slice?
arr[n..] returns a slice from index n to the end.
Can create a slice from an array, another slice, or a. multi-pointer (define).
Use @enumToInt(enumValue) to get its ordinal value.
Enum definitions can contain const and var variables that become namespaced to the enum type, and not associated with individual instances.
Can enum instances have associated data? I think not.
Generally want to define types like a enums at the global scope instead of inside a function.

Are strings just byte slices?

Each enum ordinal value is the previous plus one unless specified.
enums can define methods that can be called on instances.
Can call on an instance OR pass an instance to it.
An enum type can be inferred; ex. Color.red vs. .red

In a type definition, can refer to the type with @This()

Knows how to print struct instances with an empty format specifier.
Struct Fields can be given default values.
Structs can define methods.

std.debug.print or std.log.info ? What else can output to stdout?

Zig Software Foundation is a non-profit organization dedicated to improving the craft of software engineering as a whole.

Zig is a C/C++ compiler toolchain and build system that can be used to simplify maintenance of your existing projects.

Zig is a simple, powerful programming language that excels in the most demanding environments.

defer allows specifying freeing of memory immediately after it is allocated.
defer runs when its block exits, not only when a function exits

Work on zig started in August 2015 - first commit.

About pointer dereference syntax ...
"""
ident.* does seem weird at first glance, but it allows for much nicer chaining - instead of something like (*foo).bar (or even doing what C does and introducing a whole new operator -> for that special case), you can just do foot.bar, and the nice thing is that it works over multiple levels of indirection (even in C before I've had to do (\*foo)->bar).
Regarding ], I assume you're talking about the calls to std.log.info. Those are creating empty tuples for the format arguments - std.log.info takes two arguments, a format string and a tuple of the values to interpolate into it, much like e.g. printf in C. Quite often in the video he actually uses {×} to specify a format argument. An alternative syntax here would be to use varargs

in an earlier phase of its design Zig did have varargs, and they were used here, but for a few reasons it was decided to remove them from the language and replace them with tuples in cases like these. Yes, it results in a few extra characters when you're logging constant messages, but in practice that's not really an issue, and removing varargs had advantages for the language from the perspective of simplicity.

I don't know why this chaining you consider to be better. To me, it's just different, not better or worse. And bringing confusion without having significan... Read more

how do you consider (_foo) ->bar (or worse, (_(\*foo)).bar) to be better than foo.\*\* bar? This situation can get even worse with more complex structures,
"""

Learn about async/await.
Zig strives for having only one way to accomplish each task.
Learn about arena allocators.

- Functions can specify the type of errors they can return by preceding the
  ! in the return type with an error type or probably an error set.
- Precede function return types with ? if null can be returned.
- Investigate Zig string libraries.
- does it always catch when memory is not freed?
- supports low-level memory control using allocators
  - page_allocator, c_allocator, ArenaAllocator, FixedBufferAllocator, GeneralPurposeAllocator, std.testing.allocator
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

From ziglearn.org:

Blocks in Zig are expressions and can be given labels,
which are used to yield values. Here, we are using a label called blk.
Blocks yield values, meaning that they can be used in place of a value.
The value of an empty block + is a value of the type void

```zig
test "labelled blocks"
    const count = bik: {
        var sum: u32 = 0;
        var i: u32 = 0;
        while (i < 10) : (i += 1) sum += i;
        break blk sum;
    };
    expect (count == 45);
    expect (@Type0f (count) == u32);
}
```

This can be seen as being equivalent to C's i++.

```zig
blk: {
    const tmp = i;
    i += 1
    break :blk tmp;
}
```

Are these the same return types?
!i32
anyerror!i32

Learn about anonymous structs.
Can they be used like JavaScript objects.

Printing type names:

```zig
const m1 = try allocator.alloc(u8, 1);
const m2 = try allocator.alloc(u8, 10);
const m3 = try allocator.alloc(u8, 100);
std.log.info("{s}", .{@typeName (@Type0f(m3))});
```

See zig-arena-allocator.jpg in Downloads.

Lambdas are not supported, but there aren't often needed in Zig.
You can do something like this instead with a struct:

```zig
var map = std.StringArrayHashMap([]const u8).init(allocator);
defer map.deinit();
try map.put("c", "c");
try map.put("b", "b");
try map.put("a", "a");

const SortContext = struct {
    keys: [][]const u8,
    pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
        return std.mem.lessThan(u8, ctx.keys[a_index], ctx.keys[b_index]);
    }
};

map.sort(SortContext{ .keys = map.keys () });
```

See zig-error-with-associated-data.jpg in Downloads.

See zig-struct-with-method.png in Downloads.
See zig-struct-with-method-calling-2-ways.png in Downloads.
See zig-generics.jpg in Downloads.
See zip-enum-and-associated-data.jpg in Downloads.

Demonstrate calling your own C and C++. ode from Zig.
No support for interfaces.
Errors cannot hold associated data. Use out parameters.
Can transpile Zig to C.
having a trailing comma after the last field in a struct changes how zig formats the code. It will keep each field on a separate line. If you remove the trailing comma from the last field, and all the fields will fit on a single line, then the formatter will do that.

To create a new library, enter `zip init-lib`.

Data types include u8 (single byte unsigned integer), …
Strings are delimited with double quotes.

The `defer` keyword specifies a function to be called when the function it is inside exits.
This is often used to deallocate memory allocated on the line before.
For example, `var allocator = std.heap.page_allocator; var myList = std.ArrayList(10).init(allocator); defer myList.deinit();`
Also see `errdefer` which specifies a function to call if an error occurs in the current scope.

Constant and variable declarations must be initialized to some value.
When can the type be inferred from the initial value?
If the initial value is “undefined” (means uninitialized), does the compiler enforce that it is assigned before it is used? I assume this is different from making it nullable with a “?” before the type.

To declare an array,

- can be const or var
- number of elements comes first, then the type, then the initial values in curly braces
- ex. const name = [3]i32{10, 20, 30};
- ex. var name = [_]i32{10, 20, 30}; // length is inferred from initial values

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
