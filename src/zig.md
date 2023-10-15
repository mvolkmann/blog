---
eleventyNavigation:
  key: Zig
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

## Overview

<figure style="width: 50%">
  <img alt="Ziggy, the Zig mascot"
    src="/blog/assets/ziggy.svg?v={{pkg.version}}">
  <figcaption>Ziggy, the lizard Zig mascot</figcaption>
</figure>

{% aTargetBlank "https://ziglang.org", "Zig" %}
is a free, open source, high performance, systems programming language.
It is a modern alternative to C with similar syntax
such as statements terminated with semicolons and conditions in parentheses.

Zig provides a complete, LLVM-based toolchain for creating, developing,
building, and testing apps written in Zig, C, and C++.
There are advantages to building apps with the Zig compiler
even if they have no Zig code and only use C and/or C++ code.

Zig is suitable for applications that care deeply about
performance, binary size, and/or memory usage.
Often these concerns justify the tedium of
manual memory management that is required
due to lack of automated garbage collection.

One appeal of Zig is that it is simpler than C++ and Rust.

Zig emphasizes:

- No hidden control flow

  Examples of hidden control flow in other languages include
  exception handling, operator overloading, destructors, and decorators.

- No hidden memory allocations

  All memory allocation is performed by allocators that the developer selects.
  Each kind of allocation implements a different allocation strategy.
  Zig does not support closures, so allocations to not outlive their scope.

- No preprocessors or macros

  In place of these, Zig uses code that runs at compile-time,
  indicated by the `comptime` keyword.

- Having only one way to accomplish each task.

Zig includes:

- a package manager
- a build system that is simpler that the
  combinations of build tools typically used with C and C++
- a build system API (used in `build.zig` files)
- cross-compilation support
- a test runner
- ability to target all platforms supported by LLVM, including WebAssembly

Zig is not an object-oriented (OO) programming language.
There is no support for defining classes, using inheritance,
or using polymorphism.
However, Zig does support defining structs with methods
and for many applications that is close enough to OO.

Andrew Kelly began work on Zig in August, 2015 (first commit).
The first public release was in February, 2016.

Despite still being in beta, it has been adopted by many projects.
The current version of Zig is 0.11.0
and is expected to reach 1.0 in 2025.

Development of Zig is managed by the Zig Software Foundation (ZSF)
which is a non-profit organization.
"The mission of the Zig Software Foundation is to promote, protect, and advance
the Zig programming language, to support and facilitate
the growth of a diverse and international community of Zig programmers,
and to provide education and guidance to students,
teaching the next generation of programmers to be competent, ethical,
and to hold each other to high standards."

Originally the Zig compiler was implemented in C++.
The 0.10.0 version of Zig changed to a self-hosted compiler
which means it is implemented in Zig.

## Used By

- {% aTargetBlank "https://bun.sh", "Bun" %} - a JavaScript/TypeScript
  runtime and toolchain, is primarily written in Zig.
  Bun has many advantages over Node.js and Deno including much better performance.

- {% aTargetBlank "https://tigerbeetle.com", "TigerBeetle" %} is
  "the world's fastest financial accounting database".

- {% aTargetBlank "https://www.roc-lang.org", "Roc" %} -
  "a fast, friendly, functional language"
  "Roc's compiler has always been written in Rust.
  Roc's standard library was briefly written in Rust,
  but was soon rewritten in Zig."

- {% aTargetBlank "https://machengine.org", "Mach" %} -
  a game engine and graphics toolkit, is implemented in Zig.

- {% aTargetBlank "https://www.uber.com/blog/bootstrapping-ubers-infrastructure-on-arm64-with-zig/",
  "Uber" %} uses Zig to build its C++ applications for x86_64 and arm64.

## Run-time Checks

Zig provides the following run-time checks:

- bounds checking of array and slice indexing
  at compile-time when index is known at compile-time and at run-time otherwise
- pointers cannot be null unless declared to be optional
- optional pointers must be checked for null before they are dereferenced
- tagged unions cannot be accessed without verifying the tag
- detects arithmetic underflow and overflow when casting between numeric types
- checks for correct alignment when casting between pointer types

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
- {% aTargetBlank "https://discord.com/servers/zig-programming-language-605571803288698900",
  "Zig Discord server" %}
- {% aTargetBlank "https://en.wikipedia.org/wiki/Zig_(programming_language)",
  "Wikipedia" %}
- {% aTargetBlank "https://zig.news", "Zig News" %}

## Installing

To install, download a platform-specific zip or tar file from
the {% aTargetBlank "https://ziglang.org/download/", "Releases" %} page,
expand it, move the directory this creates to a desired location,
set the environment variable `ZIG_PATH` to point to this directory, and
add `ZIG_PATH` to the list of directories in the `PATH` environment variable.

Consider downloading a nightly build in order to use the very latest version.

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

The first argument to `print` is a format string
that can contain `{}` placeholders where values are to be inserted.
The second argument to `print` is a literal array that holds
values to be inserted in place of the format string placeholders.
The syntax `.{}` is the literal syntax for
both arrays and structs (more on these later).
This can contain a comma-separated list of either
array items or struct field assignments.

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

To see a list of Zig guiding principles, enter `zig zen`.

## Tools

For VS Code, see the extension {% aTargetBlank
"https://github.com/ziglang/vscode-zig", "Zig Language" %}.

Editor extensions typically handle unused variables
by adding a line that uses them.
For example, if the variable `foo` is unused,
the line `_ = foo;` will be added immediately after its declaration.

## Style

Zig has an official {% aTargetBlank
"https://ziglang.org/documentation/0.11.0/#Style-Guide", "Style Guide" %}
that is not enforced by the compiler.
At a high level it specifies the following:

- 4-space indentation
- open braces on the same line or the last of wrapped lines
- maximum line length is 100
- function names should be camelCase
  - but functions that return a type should be TitleCase
- type names should be TitleCase
- variable names should be snake_case
- names of files that define a struct should have
  the same name as the struct which uses TitleCase
- all other file names should be snake_case
- directory names should be snake_case

## Zig Projects

To create a new project, create a directory for it, cd to the directory,
and enter `zig init-exe`.
This creates the file `build.zig` and
a `src` directory containing the file `main.zig`.

The file `build.zig` is a build script that uses the compiler API.
Modify this file to change the characteristics of executable that is produced.

The file `main.zig` is the starting point of the project.
Like many `.zig` files, this begins by importing the standard library
with `const std = @import("std");`
It also defines the main function with `pub fn main() !void { ... }`.
The `!` means the function can return an error value.
If an error is returned from the `main` function,
it panics and prints a stack trace.

Zig source files can import other source files
in order to access their `pub` values, including functions.

```zig
// In the file my_module.zig:
pub const gretzky = 99;

pub fn double(n: i32) i32 {
    return n * 2;
}

// In the file main.zig:
const std = @import("std");
// mod is a struct instance whose fields
// are the pub values in my_module.zig.
const mod = @import("my_module.zig");

pub fn main() !void {
    std.debug.print("{}\n", .{mod.gretzky}); // 99

    const value = 3;
    const result = mod.double(value);
    std.debug.print("{}\n", .{result}); // 6
}
```

To build and run the app, enter `zig build run`.

The object files produced by the compiler
are stored in the `zig-cache` directory.
This allows subsequent builds to avoid recompiling source files
that have not changed since the last build.

The executable file produced by a build
is stored in `zig-out/bin/{project-name}`.

## Comments

Single-line comments begin with `//`.

Zig does not support multi-line comments.
It relies on code editors to make it
easy to comment and uncomment ranges of lines.

Top-level comments begin with `//!`.
These are used to document the current module (source file).

Doc comments begin with `///`.
These are used to document variables and functions.

## Printing

Zig provides several functions that write to stderr.
Perhaps the most commonly used is `std.debug.print`.
Others include `std.log.info`, `std.log.debug`, `std.log.warn`,
and `std.log.err`, in order from least to most severe.

The `print` function takes a format string
and a possibly empty literal array of values
to be inserted in place of the format string placeholders.
The format string can contain placeholders with the syntax `{specifier}`
that indicate where values in the second argument are to be inserted.

The following format specifiers are supported:

| Specifier | Prints                                                                                                                                          |
| --------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `any`     | value of any type using its default format                                                                                                      |
| `b`       | integer value in binary notation                                                                                                                |
| `c`       | integer as ASCII character (maximum of 8 bits)                                                                                                  |
| `d`       | numeric value in decimal notation                                                                                                               |
| `e`       | floating point value in scientific notation                                                                                                     |
| `o`       | integer value in octal notation                                                                                                                 |
| `s`       | slices of `u8`                                                                                                                                  |
| `s`       | pointer-to-many and C pointers of `u8`                                                                                                          |
| `u`       | integer as UTF-8 sequence (maximum of 21 bits)                                                                                                  |
| `X`       | numeric value in uppercase hexadecimal notation                                                                                                 |
| `x`       | numeric value in lowercase hexadecimal notation                                                                                                 |
| `?`       | optional value as either the unwrapped value or `null`;<br>may be followed by a format specifier for the underlying value                       |
| `!`       | error union value as either the unwrapped value or the formatted error value;<br>may be followed by a format specifier for the underlying value |
| `*`       | address of the value                                                                                                                            |

Often placeholders do not need to specify a format
because the correct formatting is used by default.
In these cases, placeholders can be written as `{}`.

A common error is to pass a single value as the second argument
instead of a literal array. The compiler will output the error
"expected tuple or struct argument, found {type-passed}".

The `std.log.*` functions take the same arguments as `std.debug.print`,
but produce output the begins with their level followed by a colon and a space.
For example, `std.log.warn("{} is too large!", .{score});`
prints a message like "warning: 19 is too large!".
These log functions add their own newline at the end of each output.

To set the logging level, define the public constant `std_options`.
For example, the following suppresses output
from `std.log.info` and `std-log-debug`.

```zig
pub const std_options = struct {
    // This sets the default logging level.
    // Set to .info, .debug, .warn, or .err.
    pub const log_level = .warn;

    // This sets scope-specific logging levels.
    pub const log_scope_levels = &[_]std.log.ScopeLevel{
        // Can have one line like this for each scope.
        .{ .scope = .my_library, .level = .info },
    };
};
```

Log messages can be scoped to a particular part of an project.
This is useful for better identifying log messages and
for filtering log output from specific parts of a project.

Use the following instead of `std.log` to scope the log messages.

```zig
const log = std.log.scoped(.my_library);
log.info("testing", .{}); // output is "info(my_library): testing"
```

TODO: How can you specify scope-specific logging levels?

For more detail, see {% aTargetBlank
"https://gist.github.com/kassane/a81d1ae2fa2e8c656b91afee8b949426",
"A simple overview of Zig's std.log" %}.

To write to stdout instead of stderr, do the following:

```zig
const stdout = std.io.getStdOut();
const sow = stdout.writer();
try sow.print("Hello, {s}!\n", .{"world"});
```

Structs can specify how they should be formatted for printing
by implementing the `format` function.
For example:

```zig
const std = @import("std");
const print = std.debug.print;

const Dog = struct {
    name: []const u8,
    breed: []const u8,
    age: u8,

    const Self = @This();
    pub fn format(
        value: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        return writer.print(
            "{s} is a {d} year old {s}.",
            .{ value.name, value.age, value.breed },
        );
    }
};

pub fn main() !void {
    const dog = Dog{ .name = "Comet", .breed = "whippet", .age = 3 };
    print("{}\n", .{dog}); // Comet is a 3 year old whippet.
}
```

## Types

Types in Zig, such as builtin types like `i32` and custom `struct` types,
are first-class values.
This means they can be assigned to variables, assigned to struct fields,
passed to functions, and returned from functions.

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
Using `const` is preferred when the value will not be modified.

Variable names must begin with a letter and are composed of letters, numbers, and ?.
Variable names cannot match a keyword (listed in the next section).
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

Zig does not allow unused variables.
Editor extensions/plugins such as vscode-zig
can add lines like `_ = my_variable` for each unused variable on save
so they appear to be used.
This feature may be enabled by default, can be disabled.
In vscode-zig, the "Zls: Enable Autofix" option controls this.

## Optionals (aka Nullables)

The types of variables, struct fields, and function parameters can be optional.
This allows them to have the value `null`.
To declare a type to be optional, add a question mark before their type
and give them a default value which may be `null`.

The following code demonstrates many features of working with optionals:

```zig
const std = @import("std");
const print = std.debug.print;
const expectEqual = std.testing.expectEqual;
const String = []const u8;

test "optional" {
    var a: i8 = 0; // not optional
    // Placing "?" before a type makes it optional.
    // Only optional variables can be set to "null".
    var b: ?i8 = null;

    //TODO: Why does Zig require casting the 0 and null values here?
    try expectEqual(a, 0);
    try expectEqual(b, null);

    a = 1;
    b = 2;
    try expectEqual(a, 1);
    try expectEqual(b, 2);

    // This form of "if" statement can only be used with optional values.
    // If non-null, the unwrapped value is placed in value.
    if (b) |value| {
        try expectEqual(value, 2);
    } else {
        unreachable; // verifies that b is non-null
    }

    // The orelse operator unwraps the value if non-null
    // and uses the value that follows if null.
    // This is why the cast here is to i8 instead of ?i8.
    try expectEqual(b orelse 0, 2);

    // "b.?" is equivalent to "b orelse unreachable".
    // It unwraps the value which is why the cast here is to i8 instead of ?i8.
    try expectEqual(b.?, 2);

    b = null;
    try expectEqual(b, null);
    try expectEqual(b orelse 0, 0);
    // _ = b.?; // results in "panic: attempt to use null value"

    if (b) |_| { // not using the unwrapped value
        unreachable; // verifies that b is null
    } else {
        try expectEqual(b, null);
    }
}

// This is a struct with optional fields.
const Dog = struct {
    name: ?String = null,
    breed: ?String = null,
};

// This demonstrates using the orelse operator
// which unwraps the value if non-null
// and uses the value that follows if null.
fn present(dog: Dog) String {
    return dog.name orelse dog.breed orelse "unknown";
}

test "struct with optional fields" {
    const dog1 = Dog{ .name = "Comet", .breed = "Whippet" };
    const dog2 = Dog{ .name = "Oscar" };
    const dog3 = Dog{ .breed = "Beagle" };
    const dog4 = Dog{};
    const dogs = [_]Dog{ dog1, dog2, dog3, dog4 };

    try expectEqual(dog1.name, "Comet");
    try expectEqual(dog1.breed, "Whippet");

    try expectEqual(dog2.name, "Oscar");
    try expectEqual(dog2.breed, null);

    try expectEqual(dog3.name, null);
    try expectEqual(dog3.breed, "Beagle");

    try expectEqual(dog4.name, null);
    try expectEqual(dog4.breed, null);

    //TODO: Why is the cast to String necessary here?
    try expectEqual(present(dog1), "Comet");
    try expectEqual(present(dog2), "Oscar");
    try expectEqual(present(dog3), "Beagle");
    try expectEqual(present(dog4), "unknown");

    // The output from this loop should be:
    // name = Comet
    // breed = Whippet
    // present = Comet
    // name = Oscar
    // present = Oscar
    // breed = Beagle
    // present = Beagle
    // present = unknown
    for (dogs) |dog| {
        if (dog.name) |name| print("name = {s}\n", .{name});
        if (dog.breed) |breed| print("breed = {s}\n", .{breed});
        print("present = {s}\n", .{present(dog)});
    }
}
```

## Keywords

Zig supports the following keywords which cannot be used as
the names of variables, function parameters, or struct fields:

`addrspace`, `align`, `allowzero`, `and`, `anyframe`, `anytype`, `asm`,
`async`, `await`, `break`, `callconv`, `catch`, `comptime`, `const`, `continue`,
`defer`, `else`, `enum`, `errdefer`, `error`, `export`, `extern`, `fn`, `for`,
`if`, `inline`, `linksection`, `noalias`, `noinline`, `nosuspend`, `opaque`,
`or`, `orelse`, `packed`, `pub`, `resume`, `return`, `struct`, `suspend`,
`switch`, `test`, `threadlocal`, `try`, `union`, `unreachable`,
`usingnamespace`, `var`, `volatile`, and `while`.

## Operators

- arithmetic: `+`, `-`, `*`, `/`, `%`
- operator assignment: `+=`, `-=`, `\*=`, `/=`, `%/`
- relational: `==`, `!=`, `>`, `>=`, `<`, `<=`
- logical: `and` (not `&&`), `or` (not `||`), `!` (not `not`)

  The first two provide short-circuiting and therefore can affect control flow.
  Operators that merely produce a value, like `!`, are symbols instead of words.

- bitwise: `<<`, `>>`, `&`, `|`, `^`, `~`
- bitwise assignment: `<<=`, `>>=`, `&=`, `|=`, `^=`
- unwrapped value or other value: `orelse`
- array concatenation: `++`
- array multiplication: `**`
- pointer dereference: `.*`
- address of: `&`
- merge error sets: `||`
- many {% aTargetBlank "https://en.wikipedia.org/wiki/Integer_overflow",
  "wrapping" %} operators
- many {% aTargetBlank
  "https://en.wikipedia.org/wiki/Saturation_arithmetic#:~:text=Saturation%20arithmetic%20is%20a%20version,a%20minimum%20and%20maximum%20value.",
  "saturating" %} operators
- does not support the `++` and `-—` operators found in C

## Pointers

To get a pointer to the data of a variable, use `&variable_name`.

To dereference a pointer, use `variable_name.*`.
This syntax can be used with chaining when the value is a `struct`
to access a struct field.
For example, `dogPtr.*.name`.
But the compiler treats `dogPtr.name` as the same.

A pointer to a non-`const` value can be used to modify the value
regardless of whether the pointer itself is `const`.
A pointer to `const` value cannot be used to modify the value.

The `.*` syntax can be used on the left or right sign of an assignment.
For example:

```zig
const std = @import("std");
const expectEqual = std.testing.expectEqual;

fn touchdown(scorePtr: *u8, extraPoint: bool) !void {
    const current = scorePtr.*;
    scorePtr.* += 6;
    try expectEqual(scorePtr.*, current + 6);
    if (extraPoint) scorePtr.* += 1;
}

test "primitive pointers" {
    var score: u8 = 3;
    // Only need try here because touchdown uses expectEqual.
    try touchdown(&score, true);
    try expectEqual(score, 10);
}
```

Pointer variables cannot be set to `null`
unless they are declared to be optional with `?`.

Zig supports two kinds of pointers, single-item and many-item.
The following table describes several types that involve pointers.
The first three are a single-item pointer and
the rest (ones with square brackets) are many-item pointers.

| Type       | Meaning                                                    |
|------------|------------------------------------------------------------|
| `*T`       | pointer to a T value                                       |
| `?*T`      | optional pointer to a T value                              |
| `*?T`      | pointer to an optional T value                             |
| `[*]T`     | pointer to an unknown number of T values                   |
| `?[*]T`    | optional pointer to an unknown number of T values          |
| `[*]?T`    | pointer to an unknown number of optional T values          |
| `?[*]?T`   | optional pointer to an unknown number of optional T values |

The type `[*]T` is a slice of pointers to values of type T.

Here are examples of obtaining and using pointers.

```zig
const std = @import("std");
const expectEqual = std.testing.expectEqual;

const Dog = struct { name: []const u8, breed: []const u8, age: u8 };

test "struct pointers" {
    var dog = Dog{ .name = "Comet", .breed = "whippet", .age = 3 };
    const dogPtr = &dog; // single-item pointer
    try expectEqual(dog.name, "Comet");
    try expectEqual(dogPtr.*.name, "Comet");
    try expectEqual(dogPtr.name, "Comet"); // automatic dereferencing

    // Pointers can only be used to modify a struct property
    // if the struct instance is not const.
    dogPtr.*.name = "Oscar";
    try expectEqual(dog.name, "Oscar");

    // Create an array of Dog instances.
    var dogs = [_]Dog{
        .{ .name = "Comet", .breed = "whippet", .age = 3 },
        .{ .name = "Oscar", .breed = "whippet", .age = 7 },
    };
    // Iterate over the dogs and increment their age.
    // &dogs gives a many-item pointer.
    for (&dogs) |*d| {
        // d.*.age += 1; // This works.
        d.age += 1; // But this also works and is shorter.
        // In C we could use "d->age++;",
        // but Zig doesn't support the arrow or ++ operators.
    }
    try expectEqual(dogs[0].age, 4);
    try expectEqual(dogs[1].age, 8);
}
```

## Ranges

Ranges of numbers have an inclusive lower bound
and an upper bound that is either exclusive or inclusive.
For example, the range `5..7` with two dots includes the values 5 and 6
and the range `5...7` with three dots includes the values 5, 6 and 7.

Exclusive ranges can be used to create a slice from an array, but not inclusive.

Inclusive ranges can be used in switch branches, but not exclusive.

I was unable to find rationale for these limitations.

## Enumerations

The `enum` keyword is used to define an enumeration.
For example, `const Color = enum { red, yellow, blue };`.

Enum instances like `Color.yellow` have a unique ordinal value.
These values start from zero by default and increment by one.
Enum instances cannot have associated data.

To get the ordinal value of an enum instance,
use the builtin function `@intFromEnum(enumValue)`.
For example, `@intFromEnum(Color.yellow)` returns `1`.

Enums are typically defined at the global scope rather than inside a function.

Enum values can override their default ordinal value.
Subsequent enum values that do not also override their default value
increment from the ordinal value of the previous enum value.

Enums can define methods that can be called on instances.
These methods can be called on an instance or an instance can be passed to them.

Here is a an example that demonstrates all of these features.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

// A type must be specified for an enum
// in order to override its default ordinal values.
const Color = enum(u8) {
    red, // defaults to 0
    yellow, // assigned 1
    blue = 7, // overrides default of 2
    green, // assigned 8

    const favorite = Color.yellow;

    // The type name "Color" is available here when define outside a function,
    // but not when defined inside a function (container-scope).
    // pub fn isPrimary(self: Color) bool {
    //     return self == Color.red or self == Color.yellow or self == Color.blue;
    // }

    // Using "Self" like this works regardless of whether the enum
    // is defined inside or outside of a function.
    const Self = @This();
    pub fn isPrimary(self: Self) bool {
        return self == Self.red or self == Self.yellow or self == Self.blue;
    }
};

test "enum" {
    const c = Color.green;
    print("c = {}\n", .{c}); // enum_demo.main.Color.green
    try expectEqual(@intFromEnum(c), 8);
    try expect(!c.isPrimary());
    try expect(!Color.isPrimary(c));
    try expect(Color.yellow.isPrimary());
}
```

## Arrays

Arrays are contiguous memory with compile-time known, fixed length
and zero-based indexes.

Slices are similar to arrays, but their length is not known until run-time.
A slice is created by referencing (not copying) a subset of
an array or other slice using range syntax.

Arrays own their data, whereas slices are pointers to data they do not own.

Both arrays and slices have a `len` field that holds their length.
The length cannot be changed.
For a dynamically-sized array, consider using
<a href="https://ziglang.org/documentation/master/std/#A;std:ArrayList"
target="_blank">ArrayList</a>.

Array types have the syntax `[length]type`.
For example, `[5]i32` is an array of five integers.

The length can be replaced by an underscore
when it can be inferred from an initial value.
For example:

```zig
const dice_rolls = [_]u8{ 4, 2, 5, 1, 2 };
```

To get a subset of an array, called a "slice", reference a range of its items.
For example, `dice_rolls[2..4]` gives a "slice" of the items at index 2 and 3.
Note that the `..` operator creates a range where the upper bound is exclusive.
The `...` operator, which creates a range where the upper bound is inclusive,
cannot be used to create a slice.

The upper index of the range can be omitted
to get all the items from a given index to the end.
For example, `dice_rolls[2..]`.

The following code demonstrates several operations on arrays.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "arrays" {
    // Create a mutable array so it can be modified later.
    var dice_rolls = [_]u8{ 4, 2, 6, 1, 2 };
    try expectEqual(dice_rolls.len, 5);

    // Use a for loop to iterate over the items in an array or slice.
    // A for loop can iterate over multiple arrays at the same time.
    // This is being used to iterate over the array elements AND their indices.
    for (dice_rolls, 0..) |roll, index| {
        try expectEqual(roll, dice_rolls[index]);
    }

    // Copy an array.
    const copy: [5]u8 = dice_rolls;
    try expectEqual(copy[0], dice_rolls[0]);
    try expect(&copy != &dice_rolls);

    // Get a slice of an array.
    const subset = dice_rolls[2..4];
    var expected_subset = [_]u8{ 6, 1 };
    try expectEqualSlices(u8, &expected_subset, subset);

    // Modify array items in-place.
    for (&dice_rolls) |*roll| {
        roll.* += 1;
        if (roll.* > 6) roll.* = 1;
    }
    // print("{any}\n", .{dice_rolls});
    const expected_modifications = [_]u8{ 5, 3, 1, 2, 3 };
    try expectEqualSlices(u8, &expected_modifications, &dice_rolls);

    // Concatenate two arrays.
    const more_rolls = [_]u8{ 1, 2, 3 };
    const combined_rolls = dice_rolls ++ more_rolls;
    const expected_combined = [_]u8{ 5, 3, 1, 2, 3, 1, 2, 3 };
    try expectEqualSlices(u8, &expected_combined, &combined_rolls);
}
```

Multidimensional arrays are created by nesting single-dimension arrays.

## Slices

A slice is an array-like collection of values
whose length is not known until run-time.
A slice references (doesn't copy) a range of indexes
from an array or another slice.
The range must be specified with indexes separated by two dots
which means the start index is inclusive and the end index is exclusive.

Slice types have the syntax `[]type`
with no length specified in the square brackets.
For example, `[]u8` is a slice of `u8` values.

The following code demonstrates creating, accessing, and modifying slices:

```zig
const std = @import("std");
const expectEqual = std.testing.expectEqual;

test "slice" {
    var array = [_]u8{ 1, 2, 3, 4, 5 };
    try expectEqual(array.len, 5);

    // This slice is immutable.
    const slice = array[2..4]; // use [0..] to include all elements
    try expectEqual(slice.len, 2);
    try expectEqual(slice[0], array[2]);
    try expectEqual(slice[1], array[3]);

    // This slice is mutable because it was
    // created from a pointer to an array.
    const arrayPtr = &array;
    const slice2 = arrayPtr[2..4];
    // The slice and array share memory,
    // so modifying one also modifies the other.
    slice2[0] = 30;
    try expectEqual(array[2], 30);
    array[3] = 40;
    try expectEqual(array[3], 40);
}
```

## defer and errdefer

The `defer` keyword specifies an expression to evaluate
when the containing block exits.
This is often used to deallocate memory allocated on the preceding line
or perform some other kind of cleanup.
For example:

```zig
var allocator = std.heap.page_allocator;
var myList = std.ArrayList(10).init(allocator);
defer myList.deinit();
```

Defer expressions cannot use the "return" keyword.

The `errdefer` keyword specifies an expression to evaluate
if an error is returned from the current scope.
See the example in the "Error Handling" section.

## Strings

Strings are represented by arrays of type
`[]u8` (mutable) or `[]const u8` (immutable).
This treats strings like a collection of bytes rather than Unicode characters.

It is convenient to define an alias for this type with the following:

```zig
const String = []const u8;
```

Literal strings are delimited by double quotes.

Zig only provides the ability to operate on strings as byte arrays.
There are Zig libraries that provide additional capabilities
such as operating on Unicode characters.

The following string operations are supported using byte arrays.

| Operation          | Example                       |
| ------------------ | ----------------------------- |
| assign to variable | var name: []u8 = "Mark";      |
| get a byte         | const letter2 = name[1]; // a |
| modify a byte      | name[1] = 'o'; // now "Mork"  |

One way to create an array of strings is to use an array initializer as follows:

```zig
const colors = [_]String{ "red", "green", "blue" };
```

To write to a string, use `std.fmt.bufPrint` or `std.io.fixedBufferStream`.
Using `bufPrint` is good when all the content can be specified in one call.
Using `fixedBufferStream` allows content to be collected over multiple calls.
The following code demonstrates both approaches.

```zig
const std = @import("std");
const expectEqualStrings = std.testing.expectEqualStrings;

test "basic" {
    // s is a pointer to the string.
    const s = "Hello, world!";
    const T = @TypeOf(s);
    // 13 is the length and 0 is the sentinel (terminator) value.
    try expectEqualStrings(@typeName(T), "*const [13:0]u8");
}

test "bufPrint" {
    var buffer: [20]u8 = undefined;
    const result = try std.fmt.bufPrint(
        &buffer,
        "{d} {s} {d}",
        .{ 'A', "Hello", 19 },
    );
    try expectEqualStrings("65 Hello 19", result);
}

test "fixedBufferStream" {
    var buffer: [20]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buffer);
    var writer = fbs.writer();
    _ = try writer.write("one"); // returns # of bytes written
    try writer.print("-{s}", .{"two"});
    try writer.print("-{s}", .{"three"});
    try expectEqualStrings("one-two-three", fbs.getWritten());
}
```

The `std.io` namespace provides several functions for split/tokenize strings
based on specified delimiters.
Split returns a slice from each delimiter
whereas tokenize treats consecutive delimiters as one.
For example, splitting `"red,green,,,blue"` on the comma delimiter
gives `"red"`, `"green"`, `""`, `""`, and `"blue"` and
tokenizing the same string gives `"red"`, `"green"`, and `"blue"`.

The following code demonstrates each of these.

```zig
const std = @import("std");
const expectEqualStrings = std.testing.expectEqualStrings;
const String = []const u8;

test "split" {
    const expected = [_]String{ "red", "green", "", "", "blue" };

    const colors1 = "red,green,,,blue";
    // This returns an iterator that provides values obtained by
    // splitting on a single delimiter that is a single value.
    var iter1 = std.mem.splitScalar(u8, colors1, ',');
    var index: u8 = 0;
    while (iter1.next()) |color| {
        try expectEqualStrings(expected[index], color);
        index += 1;
    }

    const colors2 = "red;-)green;-);-);-)blue";
    // This returns an iterator that provides values obtained by
    // splitting on a single delimiter that is a sequence of values.
    var iter2 = std.mem.splitSequence(u8, colors2, ";-)");
    index = 0;
    while (iter2.next()) |color| {
        try expectEqualStrings(expected[index], color);
        index += 1;
    }

    var colors3 = "red,green,; blue";
    // This returns an iterator that provides values obtained by
    // splitting on any one of the given delimiters.
    var iter3 = std.mem.splitAny(u8, colors3, ",; ");
    index = 0;
    while (iter3.next()) |color| {
        try expectEqualStrings(expected[index], color);
        index += 1;
    }
}

test "tokenize" {
    const expected = [_]String{ "red", "green", "blue" };

    const colors1 = "red,green,,,blue";
    // This returns an iterator that provides values obtained by
    // splitting on a single delimiter that is a single value.
    var iter1 = std.mem.tokenizeScalar(u8, colors1, ',');
    var index: u8 = 0;
    while (iter1.next()) |color| {
        try expectEqualStrings(expected[index], color);
        index += 1;
    }

    const colors2 = "red;-)green;-);-);-)blue";
    // This returns an iterator that provides values obtained by
    // splitting on a single delimiter that is a sequence of values.
    var iter2 = std.mem.tokenizeSequence(u8, colors2, ";-)");
    index = 0;
    while (iter2.next()) |color| {
        try expectEqualStrings(expected[index], color);
        index += 1;
    }

    var colors3 = "red,green,; ,; blue";
    // This returns an iterator that provides values obtained by
    // splitting on any one of the given delimiters.
    var iter3 = std.mem.tokenizeAny(u8, colors3, ",; ");
    index = 0;
    while (iter3.next()) |color| {
        try expectEqualStrings(expected[index], color);
        index += 1;
    }
}
```

TODO: Finish from https://www.huy.rocks/everyday/01-04-2022-zig-strings-in-5-minutes

The following code demonstrates using the zig-string library.

For more functionality, including Unicode support, use a string library such as
{% aTargetBlank "https://github.com/JakubSzark/zig-string", "zig-string" %} or
{% aTargetBlank "https://codeberg.org/dude_the_builder/zigstr", "zigstr" %}.

```zig
// This demonstrates using the zig-string library
// at https://github.com/JakubSzark/zig-string.
// You can just copy the file zig-string.zig.
const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const String = @import("./zig-string.zig").String;

test "strings" {
    const allocator = std.testing.allocator;

    var myString = String.init(allocator);
    defer myString.deinit();

    // Use functions provided
    try myString.concat("abc");
    _ = myString.pop();
    assert(myString.cmp("ab"));
    try myString.concat("cde");

    assert(myString.cmp("abcde"));
    assert(myString.len() == 5);

    // TODO: This is not working!
    // const mySubstr = myString.substr(1, 3);
    // print("mySubstr = {any}\n", .{mySubstr});

    myString.toUppercase(); // modifies in place
    assert(myString.cmp("ABCDE"));

    myString.toLowercase(); // modifies in place
    assert(myString.cmp("abcde"));

    var copy = try myString.clone();
    defer copy.deinit();
    copy.reverse(); // modifies in place
    assert(copy.cmp("edcba"));

    assert(!myString.isEmpty());
    myString.clear();
    assert(myString.isEmpty());

    var santa = try String.init_with_contents(allocator, "Ho");
    defer santa.deinit();
    assert(santa.cmp("Ho"));
    try santa.repeat(2); // will have 3 occurrences after this
    assert(santa.cmp("HoHoHo"));

    // TODO: Why must this be var and not const?
    var colors = try String.init_with_contents(allocator, "red,green,blue");
    defer colors.deinit();
    // Splits into []u8 slices.  This works.
    if (colors.split(",", 0)) |c1| {
        assert(std.mem.eql(u8, c1, "red"));
        if (colors.split(",", 4)) |c2| {
            assert(std.mem.eql(u8, c2, "green"));
        }
    }

    var padded = try String.init_with_contents(allocator, "  foo ");
    padded.trim(); // trims in place
    // Also see trimStart and trimEnd.
    assert(padded.cmp("foo"));

    // Splits into String slices.  This does not work!
    // var color1 = try colors.splitToString(",", 0);
    // if (color1) |c1| {
    //     defer c1.deinit();
    //     assert(c1.cmp("red"));
    //     const color2 = try colors.splitToString(",", 4);
    //     if (color2) |c2| {
    //         defer c2.deinit();
    //         assert(c2.cmp("green"));
    //     }
    // }

    // This demonstrates splitting a []u8 instead of a zig-string String.
    const colorsArray = "red,green,blue";
    var splits = std.mem.split(u8, colorsArray, ",");
    while (splits.next()) |chunk| {
        print("chunk = {s}\n", .{chunk});
    }
}
```

## Structs

A `struct` is a custom type that holds a collection of fields
and optional methods.
Struct fields and methods cannot be made private.
They are always visible outside the `struct` definition.

The following code demonstrates defining and using a `struct`:

```zig
const std = @import("std");
const sqrt = std.math.sqrt;
const expect = std.testing.expect;

fn square(n: f32) f32 {
    return std.math.pow(f32, n, 2);
}

const Point = struct {
    x: f32 = 1, // default value
    y: f32 = 2, // default value

    pub fn distanceToOrigin(self: Point) f32 {
        return sqrt(square(self.x) + square(self.y));
    }

    pub fn distanceTo(self: Point, other: Point) f32 {
        const dx = self.x - other.x;
        const dy = self.y - other.y;
        return sqrt(square(dx) + square(dy));
    }
};

// Typically this would be a method on the Point struct,
// but we want to demonstrate passing a pointer to a struct
// to enable modifying fields.
fn translate(pt: *Point, dx: f32, dy: f32) void {
    pt.x += dx;
    pt.y += dy;
}

test "Point struct" {
    var p1 = Point{}; // modified later
    try expectEqual(p1.x, 1);
    try expectEqual(p1.y, 2);

    const p2 = Point{ .y = 3 };
    try expectEqual(p2.x, 1);
    try expectEqual(p2.y, 3);

    const p3 = Point{ .x = 3, .y = 4 };
    try expectEqual(p3.distanceToOrigin(), 5);

    const p4 = Point{ .x = 6, .y = 8 };
    try expectEqual(p3.distanceTo(p4), 5);
    try expectEqual(Point.distanceTo(p3, p4), 5);

    // This iterates over all the fields of the Point struct,
    // prints the name, the type, and the value in the p1 instance.
    print("\n", .{});
    // TODO: Why does this only work with "inline"?
    inline for (std.meta.fields(@TypeOf(p1))) |field| {
        print("found field {s} with type {s}\n", .{ field.name, @typeName(field.type) });
        print("value in p1 is {}\n", .{@as(field.type, @field(p1, field.name))});
    }

    // Passing a pointer so the struct instance can be modified.
    translate(&p1, 2, 3);
    try expectEqual(p1.x, 3);
    try expectEqual(p1.y, 5);
}
```

The fields of a `struct` can be given default values that are used
when instances are created without specifying a value for each field.
For example, the declaration of the field `x` in the `Point` struct above
can be replaced with `x: f32 = 1`.
We can then create a `Point` instances without specifying a value for `x`.
For example, `const my_point = Point{.y = 2}` creates
an instance where the `x` field has the value `1`.

The `std.debug.print` function does a reasonable job
of printing `struct` instances without any format specifier.
For example, after setting the `p1` variable in the code above,
we could add `print("p1 = {}\n", .{p1});` to produce the following output:

```text
p1 = struct_demo.Point{ .x = 3.0e+00, .y = 4.0e+00 }
```

The syntax for a literal struct is the same as the syntax for a literal array.
`.{}` can contain a comma-separated list of either
array items or struct field assignments.
This syntax can be used to assign a struct instance to a variable
or pass one to a function.
A struct definition specified with the `struct` keyword is not required.
In this sense it is similar to creating objects in JavaScript.

The following example creates a `struct` instance with four properties.

```zig
    const instance = .{
        .key1 = true, // type is bool
        .key2 = 19, // type is comptime_int
        .key3 = 'x', // type is comptime_int (value is 120)
        .key4 = "text", // type is *const [4:0]u8; 0 is the alignment
    };

    try expectEqual(bool, @TypeOf(instance.key1));
    try expectEqual(true, instance.key1);

    try expectEqual(comptime_int, @TypeOf(instance.key2));
    try expectEqual(19, instance.key2);

    try expectEqual(comptime_int, @TypeOf(instance.key3));
    try expectEqual('x', instance.key3);

    try expectEqual(*const [4:0]u8, @TypeOf(instance.key4));
    try expectEqual("text", instance.key4);
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

## Tuples

Tuples are anonymous structs without specified field names.
The field names default to indexes.

The following code demonstrates using a tuple:

```zig
const std = @import("std");
const print = std.debug.print;
const trait = std.meta.trait;
const expectEqual = std.testing.expectEqual;

test "tuple" {
    // Casting the integer and float literal values
    // to specific types is optional.
    // The compiler will know the element types in this tuple
    // and can use them in the "inline for" loop below.
    // This is important because the types passed to
    // "trait.isZigString" must be known at compile-time.
    const tuple = .{ true, @as(u8, 19), @as(f32, 3.14), 'A', "hello" };

    try expectEqual(tuple.len, 5);
    try expectEqual(tuple[0], true);
    try expectEqual(tuple[1], 19);
    try expectEqual(tuple[2], 3.14);
    try expectEqual(tuple[3], 'A');
    try expectEqual(tuple[4], "hello");
    try expectEqual(tuple.@"4", "hello"); // alternate way to index

    // This loop must be "inline".
    inline for (tuple) |value| {
        const T = @TypeOf(value);
        print("type of {any} is {}\n", .{ value, T });
        // comptime must be used here because the argument
        // to isZigString must be comptime-known.
        if (comptime trait.isZigString(T)) {
            print("value is {s}\n", .{value});
        } else {
            print("value is {any}\n", .{value});
        }
    }

    // Destructuring can be used to get the elements of a tuple,
    // but all the elements must be matched.
    // Zig only supports destructuring of tuples.
    const e1, const e2, const e3, const e4, const e5 = tuple;
    _ = e3;
    _ = e4;
    _ = e5;
    try expectEqual(e1, true);
    try expectEqual(e2, 19);
}
```

## Unions

A bare `union` defines a set of fields that a value can have
where only one is active at a time.
Each field can have a different type.

A tagged `union` adds the use of an `enum` that
lists the possible field names s in a union.
This allows the union to be used in a `switch` statement.

The following code demonstrates using both kinds of unions:

```zig
const std = @import("std");
const print = std.debug.print;
const expectEqual = std.testing.expectEqual;

test "union" {
    const Identifier = union {
        name: []const u8,
        number: i32,
    };
    const id1 = Identifier{ .name = "top secret" };
    const id2 = Identifier{ .number = 1234 };
    try expectEqual(id1.name, "top secret");
    try expectEqual(id2.number, 1234);
}

test "tagged union" {
    const IdentifierTag = enum { name, number };
    const Identifier = union(IdentifierTag) {
        name: []const u8,
        number: i32,
    };

    const ids = [_]Identifier{
        Identifier{ .number = 1234 },
        Identifier{ .name = "top secret" },
    };

    for (ids) |id| {
        switch (id) {
            .name => |name| {
                print("got Identifier named \"{s}\"\n", .{name});
            },
            .number => |number| print("got Identifier #{d}\n", .{number}),
        }
    }
}
```

## Blocks

Blocks are lines of code surrounded by curly braces that define a variable scope.

Labelled blocks are expressions that return a value.
For example:

```zig
const value = myLabel: {
    // Add code to compute a value here.
    break :myLabel some_value;
}
```

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
        // Only triple-dot ranges are allowed here.
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

### Labeled Blocks

Labeled blocks turn a block into an expression with a value.
They can be used to compute the value of a variable or a switch branch.

The syntax for labeled blocks was borrowed from {% aTargetBlank
"https://doc.rust-lang.org/reference/expressions/loop-expr.html#labelled-block-expressions",
"Rust" %}.

The value of a labeled block is specified using a `break` statement
that includes the block label.

By convention, must block labels in Zig are "blk".

For example:

```zig
const std = @import("std");
const expectEqual = std.testing.expectEqual;

test "labeled block" {
    const s1 = blk: {
        const ms = std.time.milliTimestamp();
        const s = @divFloor(ms + 500, 1000);
        break :blk s;
    };

    // Often labeled blocks can be replaced by a single expression.
    const s2 = @divFloor(std.time.milliTimestamp() + 500, 1000);

    try expectEqual(s1, s2);
}
```

## Error Handling

Errors are represented by enum values that cannot carry additional data.
For example, the following defines an error set with two possible values:

```zig
const EvalError = error { Negative, TooHigh };
```

An error can be returned from a function in the same way as returning a value.
For example:

```zig
return EvalError.TooHigh;
```

Functions that can return errors must indicate this
by preceding their return type with `!`.
They can optionally specify an error set before the `!`
that describes the possible errors that can be returned.
If no error set is specified, the compiler determines
the possible errors from the function body.

When calling a function that can return an error,
preceding the call with the `try` keyword causes the calling function
to return the error that is returned by the called function.
Note that `try someFn();` is equivalent to `someFn() catch |e| return e;`.

The following code demonstrates defining a function
that specifies an error set:

```zig
const EvalError = error{ Negative, TooHigh };

fn double(n: i8) EvalError!i8 {
    if (n < 0) return EvalError.Negative;
    if (n > 100) return EvalError.TooHigh;
    return n * 2;
}

pub fn main() !void {
    var result = try double(4); // 8
    print("result = {d}\n", .{result});
    result = try double(-1); // panics with error: .Negative
    print("result = {d}\n", .{result});
}
```

Omitting the error set from a function return type
is not the same as specifying the error set `anyerror`.
Using `anyerror` means that absolutely any kind of error can be returned,
whereas omiting the error set means
the compiler will determine the possible errors.

When the error set is omitted from the function return type,
the function can return single error values
that are no part of a predefined error set.

The following code demonstrates defining a function
that does not specify an error set:

```zig
fn double(n: i32) !i32 {
    if (n < 0) return error.Negative; // note use of error keyword
    if (n > 100) return error.TooHigh;
    return n * 2;
}

pub fn main() !void {
    var result = try double(4); // 8
    print("result = {d}\n", .{result});
    result = try double(-1); // panics with error: Negative
    print("result = {d}\n", .{result});
}
```

The following code demonstrates many features of Zig error handling:

```zig
const std = @import("std");
const print = std.debug.print;

// The "expectEqual" function has the following signature:
// fn expectEqual(expected: anytype, actual: @TypeOf(expected)) !void
// So the second argument is cast to the type of the first.
// If the expected value is a literal value,
// it must be cast with "@as" if it is the first argument,
// but not if it is the second.
const expectEqual = std.testing.expectEqual;

const expectError = std.testing.expectError;

const EvalError = error{ Negative, TooHigh };

fn double(n: i8) EvalError!i8 {
    if (n < 0) return EvalError.Negative;
    if (n > 100) return EvalError.TooHigh;
    return n * 2;
}

test "error handling" {
    // The "try" before the call to "double"
    // causes any error returned by "double" to be returned,
    // which would cause this test to fail.
    // "try someFn();" is equivalent to "someFn() catch |err| return err;"
    // try expectEqual(@as(i8, 4), try double(2)); // requires cast
    try expectEqual(try double(2), 4); // does not require cast

    try expectError(EvalError.Negative, double(-1));

    try expectError(EvalError.TooHigh, double(101));

    // "catch" provides a value to use if *any* error is returned.
    var result = double(-1) catch @as(i8, 0);
    try expectEqual(result, 0);

    result = double(101) catch @as(i8, 100);
    try expectEqual(result, 100);

    // We can test for specific errors.
    try expectEqual(double(-1), EvalError.Negative);
    try expectEqual(double(101), EvalError.TooHigh);
}

fn safeDouble(n: i8) i8 {
    return double(n) catch |err| {
        print("safeDouble caught {}\n", .{err});
        if (err == EvalError.Negative) return 0;
        if (err == EvalError.TooHigh) return 100;
        return 0;
    };
}

test "catch" {
    try expectEqual(safeDouble(2), 4);
    try expectEqual(safeDouble(-1), 0);
    try expectEqual(safeDouble(101), 100);
}

// This function differs from "double" in that in uses "errdefer".
fn doubleErrdefer(n: i8) EvalError!i8 {
    errdefer print("double returned an error for {d}\n", .{n});
    return double(n);
}

test "errdefer" {
    try expectEqual(doubleErrdefer(2), 4);

    // This prints "double returned an error for -1".
    try expectEqual(doubleErrdefer(-1), EvalError.Negative);

    // This prints "double returned an error for 101".
    try expectEqual(doubleErrdefer(101), EvalError.TooHigh);
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

Anonymous functions (lambdas) are not supported.
For the rationale, see {% aTargetBlank
"https://github.com/ziglang/zig/issues/1717", "issue 1717" %}.

Marking a function as `pub` makes it available
to be imported from outside its source file.

The convention for function names is to use camelCase.

The parameter list is a comma-separated list of parameter declarations.
The syntax for each parameter declaration is `{name}: {type}`.

Zig does not support "vararg" functions,
so functions that would have a variable number of arguments
in other languages take a literal array (`.{}`) instead.

Function parameters cannot be modified.

The return type syntax is `[error-type][!][return-type]`.
For example:

- `i32` means an integer is returned and no errors can be returned
- `!i32` means an integer or one of the errors inferred from the function body is returned
- `anyerror!i32` means an integer or any kind of error is returned
- `MyErrorSet!i32` means an integer or one of the errors in `MyErrorSet` is returned

When an error occurs in a function, an error `enum` value is returned.
Zig does not support throwing exceptions.

To call a function that returns a value and not use it, assign it to `_`.
For example, `_ = someFn();`

When a function returns, all stack memory allocations
made in the function are freed.
A common way to allow a function to modify data
that is not freed when the function returns is to
pass a pointer to data owned by the caller and modify that inside the function.

Literal strings are known and compile-time and
stored in memory that is not part of the stack or the heap.
A function can create a literal string and return it
since it will not be freed when the function exits.

## Duck Typing with anytype

Functions can have parameters with the type `anytype`.
As the name implies, this allows any kind of value to be passed.
However, the compiler will verify that the value
can be used correctly by the function body.
This supports "duck typing".

The following code demonstrates using `anytype` for duck typing.

```zig
const std = @import("std");
const expectApproxEqAbs = std.testing.expectApproxEqAbs;

const String = []const u8;

const Animal = struct {
    name: String,
    top_speed: u32, // miles per hour
};

const Car = struct {
    make: String,
    model: String,
    year: u16,
    top_speed: u32, // miles per hour
};

const Wrong = struct {
    top_speed: f32, // not expected type of u32
};

// The first argument must be a struct with
// a top_speed field that is an integer.
fn travelTime(thing: anytype, distance: u32) !f32 {
    // We could use @TypeOf(thing) and functions like
    // std.meta.trait.hasField and std.meta.trait.isIntegral
    // to verify that "thing" meets our criteria.
    // However, there is no need to do that because the compiler will
    // verify that "thing" has a "top_speed" field that is an integer
    // just because it is used that way here.
    const s: f32 = @floatFromInt(thing.top_speed);

    // We can't eliminate the local variable d because
    // @floatFromInt requires that we specify the result type.
    const d: f32 = @floatFromInt(distance);

    return d / s;
}

test "anytype" {
    const cheetah = Animal{
        .name = "cheetah",
        .top_speed = 75,
    };
    const distance = 20; // miles
    const tolerance = 0.001;
    try expectApproxEqAbs(
        try travelTime(cheetah, distance),
        0.2667,
        tolerance,
    );

    const ferrari = Car{
        .make = "Ferrari",
        .model = "F40",
        .year = 1992,
        .top_speed = 201,
    };
    try expectApproxEqAbs(
        try travelTime(ferrari, distance),
        0.0995,
        tolerance,
    );

    // This results in a compile error which is good because
    // the first argument is struct whose top_speed field is not an integer.
    // const wrong = Wrong{ .top_speed = 1.0 };
    // _ = try travelTime(wrong, distance);

    // This results in a compile error which is good because
    // the first argument is not a struct with a "top_speed" field.
    // _ = try travelTime("wrong", distance);
}
```

## comptime Keyword

The `comptime` keyword marks items that must be known at compile-time.
It can be applied to:

- function parameters
- variables declared inside functions
- expressions such as function calls
- blocks of code

The initial values of variables declared at the container level
(outside any function) are automatically evaluated at compile-time.

Code executed at compile-time has several limitations.

- It cannot have side-effects such as
  performing I/O operations or sending network requests.
- It cannot perform more that a fixed number of branching operations
  such as loop iterations or recursive calls.
  This limit can be set by calling `@setEvalBranchQuota(quota)`.
  The limit defaults to 1000 and cannot be set lower than this.

Compile-time calls to the `assert` function that fail
result in compile errors.

The `comptime` keyword has many uses,
one of which is implementing generic types.
For example, the following code defines the function `makeNode`
that takes a `comptime` parameter and creates a `struct` type
for a tree node whose value has a specific type.

```zig
const std = @import("std");
const print = std.debug.print;
const stdout = std.io.getStdOut();
const sow = stdout.writer();

fn makeNode(comptime T: type) type {
    return struct {
        const Self = @This();

        // left and right are optional pointers to
        // another instance of this struct type.
        left: ?*Self,
        right: ?*Self,
        value: T,

        fn init(value: T) Self {
            return Self{
                .left = null,
                .right = null,
                .value = value,
            };
        }

        pub fn depthFirstPrint(self: *Self, indent: u8) void {
            // Ignoring errors for simplicity.
            sow.writeByteNTimes(' ', indent * 2) catch {};

            // The value of specifier will be known at compile-time.
            const specifier = if (T == []const u8) "s" else "";
            // The format argument to the print function
            // must be known at compile-time.
            const format = "- {" ++ specifier ++ "}\n";
            // Ignoring errors for simplicity.
            sow.print(format, .{self.value}) catch {};

            // Recursively print the left and right nodes.
            if (self.left) |left| left.depthFirstPrint(indent + 1);
            if (self.right) |right| right.depthFirstPrint(indent + 1);
        }
    };
}

fn treeOfIntegers() void {
    const Node = makeNode(u8);
    var node1 = Node.init(1);
    var node2 = Node.init(2);
    node1.left = &node2;
    var node3 = Node.init(3);
    node1.right = &node3;
    var node4 = Node.init(4);
    node2.left = &node4;
    node1.depthFirstPrint(0);
}

fn treeOfStrings() void {
    const Node = makeNode([]const u8);
    var node1 = Node.init("one");
    var node2 = Node.init("two");
    node1.left = &node2;
    var node3 = Node.init("three");
    node1.right = &node3;
    var node4 = Node.init("four");
    node2.left = &node4;
    node1.depthFirstPrint(0);
}

const Dog = struct {
    name: []const u8,
    breed: []const u8,
    age: u8,

    const Self = @This();
    pub fn format(
        value: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        return writer.print(
            "{s} is a {d} year old {s}.",
            .{ value.name, value.age, value.breed },
        );
    }
};

fn treeOfDogs() void {
    const Node = makeNode(Dog);
    var node1 = Node.init(Dog{
        .name = "Maisey",
        .breed = "Treeing Walker Coonhound",
        .age = 3,
    });
    var node2 = Node.init(Dog{
        .name = "Ramsay",
        .breed = "Native American Indian Dog",
        .age = 3,
    });
    node1.left = &node2;
    var node3 = Node.init(Dog{
        .name = "Oscar",
        .breed = "German Short-haired Pointer",
        .age = 3,
    });
    node1.right = &node3;
    var node4 = Node.init(Dog{
        .name = "Comet",
        .breed = "Whippet",
        .age = 3,
    });
    node2.left = &node4;
    node1.depthFirstPrint(0);
}

pub fn main() !void {
    treeOfIntegers();
    // Output is
    // - 1
    //   - 2
    //     - 4
    //   - 3

    treeOfStrings();
    // Output is
    // - one
    //   - two
    //     - four
    //   - three

    treeOfDogs();
    // Output is
    // - Maisey is a 3 year old Treeing Walker Coonhound.
    //   - Ramsay is a 3 year old Native American Indian Dog.
    //     - Comet is a 3 year old Whippet.
    //   - Oscar is a 3 year old German Short-haired Pointer.
}
```

The following code computes the value of a container-level variable,
`total`, at compile-time.

```zig
const std = @import("std");
const print = std.debug.print;

// []u32 is a slice which is a pointer type already.
fn sum(numbers: []const u32) u32 {
    // When this function is called at the container level, the
    // next line gives the error "comptime call of extern function".
    // print("in sum\n", .{});
    var _sum: u32 = 0;
    for (numbers) |number| {
        _sum += number;
    }
    return _sum;
}

const scores = [_]u32{ 10, 20, 30, 40, 50 };

// This passes a pointer to the scores array to the
// `sum` function which is executed at compile-time.
const total = sum(&scores);

pub fn main() !void {
    // Since total is computed at compile-time, the generated
    // binary doesn't compute it and only has to print 150.
    print("total = {d}\n", .{total});
}
```

The {% aTargetBlank "https://godbolt.org/", "Compiler Explorer" %} website
provides an online code editor and compiler for many languages.
The screenshot below demonstrates the effect of evaluating code at compile-time
on the generated assembly language instructions.
Note that the generated code for the `demo` function just returns 150
and doesn't need to compute it because that is already done at compile-time.

<img alt="Zig in Compiler Explorer" style="width: 100%"
  src="/blog/assets/zig-godbolt.png?v={{pkg.version}}"
  title="Zig in Compiler Explorer">

The builtin types `comptime_int` and `comptime_float` represent
integer and floating point values that are known at compile-time
and whose size is not specified.
Variables of these types must be either `const` or `comptime`.
For example:

```zig
const std = @import("std");
const print = std.debug.print;

const my_int = 19;
const my_float = 3.14;

pub fn main() !void {
    print("my_int type is {}\n", .{@TypeOf(my_int)}); // comptime_int
    print("my_float type is {}\n", .{@TypeOf(my_float)}); // comptime_float
}
```

## inline Keyword

The `inline` keyword can be applied to functions, for loops, and while loops.

This screenshot shows the assembly code generated for a non-inline function.

<img alt="Zig non-inline function" style="width: 100%"
  src="/blog/assets/zig-non-inline-function.png?v={{pkg.version}}"
  title="Zig non-inline function">

This screenshot shows the assembly code generated for the same function
when changed to be `inline`.

<img alt="Zig inline function" style="width: 100%"
  src="/blog/assets/zig-inline-function.png?v={{pkg.version}}"
  title="Zig inline function">

## Reflection

The following builtin functions support reflection:

- `@This()` when inside a `struct` definition, returns its type.
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

All memory allocation in Zig is done through allocators.
There are many provided allocators that each
use a different memory management strategy.
New allocators can be defined to implement custom memory management strategies.

For guidelines on selecting an allocator, see {% aTargetBlank
"https://ziglang.org/documentation/master/#Choosing-an-Allocator",
"Choosing an Allocator" %}.

TODO: Show an example of creating and using an allocator.
TODO: You have already done this somewhere in this file.

The `defer` keyword is followed by a statement or block of code
to be executed when the containing block terminates.
This is often used to free memory that was allocated by an allocator.
Keeping code that allocates and frees memory together
is less error-prone than allocating memory,
writing a bunch of code that uses it, and
having to remember to free it after all that code.

- std.testing.allocator

  This can only be used inside a `test` block.
  It detects memory leaks.

- std.testing.FailingAllocator

  This fails after a given number of allocations.
  It is useful for testing how a program handles out of memory conditions.
  It must be passed another allocator such as `std.testing.allocator`.
  For example:

  ```zig
  const testing = std.testing;
  var allocator = testing.FailingAllocator.init(testing.allocator, 5);
  ```

- `std.heap.page_allocator`

  Your system/Target page allocator, will give you a whole OS page.

- `std.heap.ArenaAllocator`

  This uses an "arena" to handle the task of freeing the memory
  of everything allocated by it when it goes out of scope.
  This allows allocating memory for many things
  that don’t need to be individually freed.
  All that is required is to use `defer arena.deinit();` one time
  right after the `ArenaAllocator` is created.

  The following code demonstrates using this allocator.

  ```zig
  const std = @import("std");
  const base_allocator = std.testing.allocator;
  const expectEqual = std.testing.expectEqual;

  test "ArenaAllocator" {
      var arena = std.heap.ArenaAllocator.init(base_allocator);
      defer arena.deinit();
      const allocator = arena.allocator();

      var list1 = std.ArrayList([]const u8).init(allocator); // no need to deinit
      try list1.append("one");
      try list1.append("two");
      try list1.append("three");
      try expectEqual(list1.items.len, 3);

      var list2 = std.ArrayList(u8).init(allocator); // no need to deinit
      try list2.append(7);
      try list2.append(13);
      try expectEqual(list2.items.len, 2);

      // No memory is leaked even though we didn't deinit the lists.
  }
  ```

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

The Zig {% aTargetBlank "https://ziglang.org/documentation/master/std/",
"standard library" %} provides many
"commonly used algorithms, data structures, and definitions".

To use the standard library in a source file,
add the following line near the top of the file:

```zig
const std = @import("std");
```

The standard library defines many namespaces that define
types, functions, values, and error sets.
Some of these define additional child namespaces.

The top-level namespaces in the standard library include the following:

- `array_hash_map` - defines several kinds of hash maps that preserve insertion order
- `ascii` - ASCII text processing
- `atomic` - memory ordering, atomic data structures, and operations
- `base64` - Base64 encoding/decoding
- `bit_set` - bit manipulation data structures
- `builtin` - comptime-available information about the build environment, such as the target and optimization mode
- `c` - provides access to many standard C functions such as `bind`, `connect`, `fork`, `free`, `malloc`, and `send`
- `coff` - Common Object File Format (COFF)
- `compress` - compression algorithms such as zlib, zstd, and more
- `comptime_string_map` - defines compile-time known has maps with string keys
- `crypto` - cryptography
- `cstr` - defines values used when working with C strings
- `debug` - debug printing, allocation and other debug helpers
- `dwarf` - Debugging With Arbitrary Record Formats (DWARF) debugging data format
- `elf` - Executable and Linking Format (ELF)
- `enums` - enum-related metaprogramming helpers
- `event` - evented I/O data structures
- `fifo` - first in, first out data structures
- `fmt` - string formatting and parsing (e.g. parsing numbers out of strings)
- `fs` - file system-related functionality
- `hash_map` - defines several kinds of hash maps that do not preserve insertion order
- `hash` - fast hashing functions (i.e. not cryptographically secure)
- `heap` - allocator implementations
- `http` - HTTP client and server
- `io` - I/O streams, reader/writer interfaces and common helpers
- `json` - JSON parsing and serialization
- `leb` - Little Endian Base (LEB128) encoding
- `log` - standardized interface for logging
- `macho` - Mach Object (Mach-O) file format
- `math` - mathematical constants and operations
- `mem` - functions for comparing, searching, and manipulating memory
- `meta` - metaprogramming helpers
- `net` - networking
- `os` - wrappers around OS-specific APIs
- `packed_int_array` - a set of array and slice types that bit-pack integer elements
- `pdb` - Program Database (PDB) file format
- `process` - accessors for process-related info (e.g. command line arguments) and spawning of child processes
- `rand` - fast pseudo-random number generators (i.e. not cryptographically secure)
- `simd` - single Instruction Multiple Data (SIMD) helpers
- `sort` - sorting
- `start` - defines a couple of undocumented functions
- `tar` - Tape Archive (tar) archive format compression/decompression
- `testing` - testing allocator, testing assertions, and other helpers for testing code
- `time` - sleep, obtaining the current time, conversion constants, and more
- `tz` - time zones
- `unicode` - UTF-8 and UTF-16LE encoding/decoding
- `valgrind` - helpers for integrating with Valgrind
- `wasm` - constants and types representing the WebAssembly (Wasm) binary format
- `zig` - tokenizing and parsing of Zig code and other Zig-specific language tooling

Some of the most commonly used parts are described in the subsections below.

### ArrayList

The <a href="https://ziglang.org/documentation/master/std/#A;std:ArrayList"
target="_blank">ArrayList</a> data structure
is "a contiguous, growable list of items in memory."

Instances of `ArrayList` have the fields `items`, `capacity`, and `allocator`.
Instances have the methods `append`, `appendSlice`, `clone`, `deinit`,
`getLast`, `getLastOrNull`, `init`, `insert`, `insertSlice`, `orderedRemove`,
`pop`, `popOrNull`, `replaceRange`, `writer`, and many more.

The following code demonstrates common operations on HashMaps.

```zig
const std = @import("std");
const print = std.debug.print;
const allocator = std.testing.allocator;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const String = []const u8;

test "ArrayList" {
    var list = std.ArrayList(String).init(allocator);
    defer list.deinit();

    try list.append("red");
    try list.appendSlice(&[_]String{ "green", "blue" });
    try expect(list.items.len == 3);

    // Iterate over the list entries.
    print("\n", .{});
    for (list.items) |value| {
        print("{s}\n", .{value});
    }

    // There is no method to test if an ArrayList` contains a given value.
    // It's more efficient to use a `BufSet` when that is needed.

    try expectEqual(@as(?String, "blue"), list.getLastOrNull());

    try expectEqual(@as(?String, "blue"), list.popOrNull());
    try expect(list.items.len == 2);

    try list.insert(1, "pink");
    try expect(list.items.len == 3);
    // Also see the replaceRange method.

    const removed = list.orderedRemove(1);
    try expectEqual(@as(String, "pink"), removed);
    try expect(list.items.len == 2);

    try list.appendNTimes("black", 2);
    try expect(list.items.len == 4); // length was 2
    try expectEqual(@as(String, "black"), list.getLast());

    list.clearAndFree();
    try expect(list.items.len == 0);
}
```

### MultiArrayList

A {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:MultiArrayList",
"MultiArrayList" %} is similar to an `ArrayList`
in that it to stores a sequence of elements.
However, the elements must be instances of a `struct` or `union` type.

Each field is stored in a separate array which makes it easy
to obtain a slice containing all the values for a given field.
Such a slice can used to create a `Vector` which supports {% aTargetBlank
"https://en.wikipedia.org/wiki/Single_instruction,_multiple_data", "SIMD" %}
operations.

The following code demonstrates several common operations
on `MultiArrayList` instances:

```zig
const std = @import("std");
const allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;

const Range = struct {
    min: f32,
    max: f32,
    current: f32,
};

test "MultiArrayList" {
    // Unlike "ArrayList" instances, "MultiArrayList" instances
    // do not store an allocator in order to optimize memory used.
    // This is why an allocator must be passed
    // to methods like "append" and "insert".
    var list = std.MultiArrayList(Range){};
    defer list.deinit(allocator);

    // Optionally set the total capacity before appending elements
    // to avoid having to allocate memory multiple times.
    try list.ensureTotalCapacity(allocator, 10);

    const r1 = Range{ .min = 0, .max = 100, .current = 50 };
    try list.append(allocator, r1);

    try list.append(allocator, Range{ .min = 10, .max = 50, .current = 25 });

    // Insert an element at a specific index, zero in this case.
    try list.insert(allocator, 0, Range{ .min = 1000, .max = 9999, .current = 1234 });

    // Set an element at a specific index, zero in this case.
    list.set(0, Range{ .min = 1000, .max = 2000, .current = 1234 });

    try expectEqual(list.len, 3);

    // After the insert, r1 was moved to index 1.
    try expectEqual(list.get(1), r1);

    // The "items" method gets a slice of the values for a given field.
    const currents: []f32 = list.items(.current);

    const vector: @Vector(3, f32) = currents[0..3].*;
    const sum = @reduce(.Add, vector);
    try expectEqual(sum, 1309.0);
}
```

### HashMap

A hash map is a collection of key/value pairs.

<a href="https://ziglang.org/documentation/master/std/#A;std:HashMap"
target="_blank">std.HashMap</a> is a low-level implementation that
requires supplying a hashing function.
<br>

<a href="https://ziglang.org/documentation/master/std/#A;std:AutoHashMap"
target="_blank">std.AutoHashMap</a>
provides a good hashing function for most key types.
The first argument is the key type and the second is the value type.
When the key type is `[]const u8`, the following error is triggered:
"std.auto_hash.autoHash does not allow slices here ([]const u8)
because the intent is unclear. Consider using std.StringHashMap
for hashing the contents of []const u8."

The following code creates an `AutoHashMap` where
the keys are strings and the values are unsigned integers.

```zig
var map = std.AutoHashMap([]const u8, u8).init(allocator);
```

<a href="https://ziglang.org/documentation/master/std/#A;std:AutoArrayHashMap"
target="_blank">std.AutoArrayHashMap</a> is similar to `std.AutoHashMap`.
It differs in the following was described in the docs:

- "Insertion order is preserved."
- "Deletions perform a swap removal on the entries list."
- "Modifying the hash map while iterating is allowed, however,
  one must understand the well-defined behavior
  when mixing insertions and deletions with iteration.
- The `values` method "returns the backing array of values".

<a href="https://ziglang.org/documentation/master/std/#A;std:StringHashMap"
target="_blank">std.StringHashMap</a>
provides a good hashing function for string keys.
The argument is the value type.

The following code demonstrates common operations on `HashMap`s.

```zig
const std = @import("std");
const print = std.debug.print;
const allocator = std.testing.allocator;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const String = []const u8;

test "AutoArrayHashMap" {
    var map = std.AutoArrayHashMap(u8, String).init(allocator);
    defer map.deinit();

    try map.put(99, "Gretzky");
    try map.put(4, "Orr");
    try map.put(19, "Ratelle");
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.value_ptr.*, entry.key_ptr.* });
    }

    try expect(map.contains(99));

    // The `get` method returns an optional value.
    var name = map.get(99) orelse "";
    try expectEqualStrings("Gretzky", name);

    const removed = map.orderedRemove(99);
    try expect(removed);
    try expectEqual(@as(?[]const u8, null), map.get(99));
}

test "AutoHashMap" {
    var map = std.AutoHashMap(u8, String).init(allocator);
    defer map.deinit();

    try map.put(99, "Gretzky");
    try map.put(4, "Orr");
    try map.put(19, "Ratelle");
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.value_ptr.*, entry.key_ptr.* });
    }

    // Iterate over the map keys.
    var iter2 = map.keyIterator();
    while (iter2.next()) |key| {
        const number = key.*;
        if (map.get(number)) |name| {
            print("{s} number is {d}.\n", .{ name, number });
        }
    }

    try expect(map.contains(99));

    // The `get` method returns an optional value.
    var name = map.get(99) orelse "";
    try expectEqualStrings("Gretzky", name);

    const removed = map.remove(99);
    try expect(removed);
    // try expect(map.get(99) == null);
    try expectEqual(@as(?[]const u8, null), map.get(99));
}

test "ComptimeStringMap" {
    // Create an array of tuples.
    const list = .{
        .{ "Gretzky", 99 },
        .{ "Orr", 4 },
        .{ "Ratelle", 19 },
    };
    try expect(list.len == 3);

    // Create a compile-time map of string keys to u8 values.
    // Since an immutable map with a fixed size is being created,
    // there is no need to deinit it.
    const map = std.ComptimeStringMap(u8, list);

    for (map.kvs) |kv| {
        print("{s} number is {d}.\n", .{ kv.key, kv.value });
    }

    try expect(map.has("Gretzky"));
    try expect(map.has("Orr"));
    try expect(map.has("Ratelle"));

    try expectEqual(@as(u8, 99), map.get("Gretzky").?);
    try expectEqual(@as(u8, 4), map.get("Orr").?);
    try expectEqual(@as(u8, 19), map.get("Ratelle").?);
}

test "StringHashMap" {
    // The keys are strings and the values are unsigned integers.
    var map = std.StringHashMap(u8).init(allocator);
    defer map.deinit();

    try map.put("Gretzky", 99);
    try map.put("Orr", 4);
    try map.put("Ratelle", 19);
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }

    // Iterate over the map keys.
    var iter2 = map.keyIterator();
    while (iter2.next()) |key| {
        print("{s} number is {any}.\n", .{ key.*, map.get(key.*) });
    }

    try expect(map.contains("Gretzky"));

    // The `get` method returns an optional value.
    try expectEqual(@as(?u8, 99), map.get("Gretzky"));

    const removed = map.remove("Gretzky");
    try expect(removed);
    try expectEqual(@as(?u8, null), map.get("Gretzky"));
}
```

<a href="https://ziglang.org/documentation/master/std/#A;std:ComptimeStringMap"
target="_blank">std.ComptimeStringMap</a>
provides an alternative to `StringHashMap` for immutable hash maps
with string keys whose entries are fixed at compile-time.
It has a much simpler API that the hash maps described above
and does not require memory cleanup by calling a `deinit` method.

The `ComptimeStringMap` function takes two arguments.
The first is the value type and the second is an array of key/value tuples.

The following code demonstrates common operations on a `ComptimeStringMap`.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

test "ComptimeStringMap" {
    // Create an array of tuples.
    const list = .{
        .{ "Gretzky", 99 },
        .{ "Orr", 4 },
        .{ "Ratelle", 19 },
    };
    try expect(list.len == 3);

    // Create a compile-time map of string keys to u8 values.
    const map = std.ComptimeStringMap(u8, list);

    for (map.kvs) |kv| {
        print("{s} number is {d}.\n", .{ kv.key, kv.value });
    }

    try expect(map.has("Gretzky"));
    try expect(map.has("Orr"));
    try expect(map.has("Ratelle"));

    try expectEqual(@as(u8, 99), map.get("Gretzky").?);
    try expectEqual(@as(u8, 4), map.get("Orr").?);
    try expectEqual(@as(u8, 19), map.get("Ratelle").?);
}
```

### Sets

A {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:BufSet",
"BufSet" %} is a set of string values.

An {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:EnumSet",
"EnumSet" %} is a set of enum values.

A {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:DynamicBitSet",
"DynamicBitSet" %} is a set of bit values.

For sets of other kinds of values, consider using a `HashMap`
where the values have the type `void`.

The following code demonstrates common operations on both of these kinds of sets.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

test "BufSet" {
    const allocator = std.testing.allocator;
    var set = std.BufSet.init(allocator);
    defer set.deinit();

    try set.insert("Gretzky");
    try set.insert("Orr");
    try set.insert("Ratelle");
    try expect(set.count() == 3);

    // Iterate over the set keys.
    print("\n", .{});
    var iter = set.iterator();
    while (iter.next()) |key| {
        print("{s}\n", .{key.*});
    }

    try expect(set.contains("Gretzky"));

    set.remove("Gretzky");
    try expect(!set.contains("Gretzky"));
}

test "EnumSet" {
    const Color = enum { red, orange, yellow, green, blue, purple, white, black };

    // This does not use an allocator and does not have a `deinit` method.
    var set = std.EnumSet(Color).initEmpty();

    // To begin with all enum values in the set ...
    // var set = std.EnumSet(Color).initFull();

    // To begin with a subset of the enum values in the set ...
    // var set = std.EnumSet(Color).initMany(&[_]Color{ .orange, .yellow });

    // To begin with one of the enum values in the set ...
    // var set = std.EnumSet(Color).initOne(.orange);

    set.insert(.orange);
    set.insert(.yellow);
    set.insert(.black);
    try expect(set.count() == 3);

    // Iterate over the set keys.
    print("\n", .{});
    var iter = set.iterator();
    while (iter.next()) |key| {
        print("{}\n", .{key});
    }

    try expect(set.contains(.yellow));

    set.remove(.yellow);
    try expect(!set.contains(.yellow));

    // There are many more methods on `EnumSet` instances.
}
```

## Builtin Functions

Zig provides over 100 (118 as of 10/23) {% aTargetBlank
"https://ziglang.org/documentation/master/#Builtin-Functions", "builtin functions" %}.
These are known to the compiler and
do not require importing in order to use them.

The name of every builtin function begins with "@".
If `@` is followed by an uppercase letter then the function returns a type.

While the builtin functions are invoked like normal functions,
some have behavior that normal functions cannot replicate.

The official documentation at the link above does not categorize
the builtin functions.
The YouTube video {% aTargetBlank "https://www.youtube.com/watch?v=V0sthxzzN3U",
"A Look at Zig's Built-ins" %} from Loris Cro uses the categories below:

### Math Builtin Functions (25)

The math builtin functions can take advantage of CPU-specific capabilities
to achieve better performance than
functions in the standard library found in `std.math`.
However, this is partially achieved by providing less error checking.

Most of the math builtin functions can
operator on a single float or a `Vector` of floats.
When passed a `Vector` of floats, they return a new `Vector` of floats.

- `@addWithOverflow` -
- `@ceil` - returns the ceiling of a float or Vector of floats
- `@cos` - returns the cosine of a float or Vector of floats
- `@divExact` - returns the quotient of two numbers
- `@divFloor` -
- `@divTrunc` -
- `@exp` -
- `@exp2` -
- `@fabs` - returns the absolute value of the float or Vector of floats
- `@floor` - returns the floor of a float or Vector of floats
- `@log` -
- `@log10` -
- `@log2` -
- `@max` -
- `@min` -
- `@mod` -
- `@mulAdd` -
- `@mulWithOverflow` -
- `@rem` -
- `@round` -
- `@sin` - returns the sine of a float or Vector of floats
- `@sqrt` -returns the square root of a float or Vector of floats
- `@subWithOverflow` -
- `@tan` - returns the tangent of a float or Vector of floats
- `@trunc` -

### Bitwise Builtin Functions (8)

- `@bitReverse` -
- `@byteSwap` -
- `@clz` -
- `@ctz` -
- `@popCount` -
- `@shlExact` -
- `@shlWithOverflow` -
- `@shrExact` -

### Atomic and Memory Builtin Functions (12)

- `@atomicLoad` -
- `@atomicRmw` -
- `@atomicStore` -
- `@cmpxchgStrong` -
- `@cmpxchgWeak` -
- `@fence` -
- `@memcpy` -
- `@memset` -
- `@shuffle` -
- `@splat` -
- `@wasmMemoryGrow` -
- `@wasmMemorySize` -

### Cast and Conversion Builtin Functions (20)

In general, using `@as` is preferred over other casting functions.
Casts can be combined with introspection functions
to achieve better error handling.

- `@alignCast` -
- `@addrSpaceCast` -
- `@as` -
- `@bitCast` -
- `@constCast` -
- `@enumFromInt` -
- `@errorCast` -
- `@errorFromInt` -
- `@floatCast` -
- `@floatFromInt` -
- `@intCast` -
- `@intFromBool` -
- `@intFromEnum` -
- `@intFromError` -
- `@intFromFloat` -
- `@intFromPtr` -
- `@ptrCast` -
- `@ptrFromInt` -
- `@truncate` -
- `@volatileCast` -

### Programming Builtin Functions (18)

- `@cDefine` -
- `@cImport` -
- `@cInclude` -
- `@cUndef` -
- `@cVaArg`
- `@cVaCopy`
- `@cVaEnd`
- `@cVaStart`
- `@compileError` -
- `@compileLog` -
- `@embedFile` -
- `@export` -
- `@import` -
- `@setAlignStack` -
- `@setCold` -
- `@setEvalBranchQuota` -
- `@setFloatMode` -
- `@setRuntimeSafety` -

### Runtime and Async Builtin Functions (3)

- `@breakpoint` -
- `@frameAddress` -
- `@panic` -

### Introspection Builtin Functions (11)

- `@alignOf` -
- `@bitOffsetOf` -
- `@bitSizeOf` -
- `@errorName` -
- `@errorReturnTrace` -
- `@fieldParentPtr` -
- `@frameAddress` -
- `@returnAddress` -
- `@sizeOf` -
- `@src` -
- `@tagName` -

### Metaprogramming Builtin Functions (10)

- `@This` - returns the type of the containing `enum`, `struct`, or `union` 
- `@Type` - returns the type that corresponds to an instance of the `std.builtin.Type` struct
- `@TypeOf` - returns the type of a given value

- `@call` - calls a given function with arguments in a tuple

  The {% aTargetBlank "", "@call" %} function takes a {% aTargetBlank
  "https://ziglang.org/documentation/master/std/#A;std:builtin.CallModifier",
  "CallModifier" %} enum value, a function, and arguments in a tuple.
  The CallModifier enum values are `auto` (most common), `always_inline`,
  `always_tail`, `async_kw`, `compile_time`, `never_inline`, `never_tail`,
  and `no_async`.

  The following code demonstrates using `@call`.

  ```zig
  const std = @import("std");
  const expectEqual = std.testing.expectEqual;

  fn add(a: i32, b: i32) i32 {
      return a + b;
  }

  test "@call" {
      const args = .{ 2, 3 };

      const result = @call(.auto, add, args);

      try expectEqual(result, 5);
  }
  ```

- `@field` -
- `@hasDecl` -
- `@hasField` -
- `@typeInfo` - returns an instance of the `std.builtin.Type` struct that describes a given type
- `@typeName` - returns the string name of a given type
- `@unionInit` -

### Other Builtin Functions (11)

TODO: Find the proper category for these!

- `@Vector` -
- `@extern` -
- `@inComptime` -
- `@offsetOf` -
- `@prefetch` -
- `@reduce` -
- `@select` -
- `@trap` -
- `@workGroupId` -
- `@workGroupSize` -
- `@workItemId` -

### HashMap

A hash map is a collection of key/value pairs.

<a href="https://ziglang.org/documentation/master/std/#A;std:HashMap"
target="_blank">std.HashMap</a> is a low-level implementation that
requires supplying a hashing function.
<br>

<a href="https://ziglang.org/documentation/master/std/#A;std:AutoHashMap"
target="_blank">std.AutoHashMap</a>
provides a good hashing function for most key types.
The first argument is the key type and the second is the value type.
When the key type is `[]const u8`, the following error is triggered:
"std.auto_hash.autoHash does not allow slices here ([]const u8)
because the intent is unclear. Consider using std.StringHashMap
for hashing the contents of []const u8."

The following code creates an `AutoHashMap` where
the keys are strings and the values are unsigned integers.

```zig
var map = std.AutoHashMap([]const u8, u8).init(allocator);
```

<a href="https://ziglang.org/documentation/master/std/#A;std:AutoArrayHashMap"
target="_blank">std.AutoArrayHashMap</a> is similar to `std.AutoHashMap`.
It differs in the following was described in the docs:

- "Insertion order is preserved."
- "Deletions perform a swap removal on the entries list."
- "Modifying the hash map while iterating is allowed, however,
  one must understand the well-defined behavior
  when mixing insertions and deletions with iteration.
- The `values` method "returns the backing array of values".

<a href="https://ziglang.org/documentation/master/std/#A;std:StringHashMap"
target="_blank">std.StringHashMap</a>
provides a good hashing function for string keys.
The argument is the value type.

A `HashMap` can be used as a set where the values are `{}`.

The following code demonstrates common operations on `HashMap`s.

```zig
const std = @import("std");
const print = std.debug.print;
const allocator = std.testing.allocator;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const String = []const u8;

test "AutoArrayHashMap" {
    var map = std.AutoArrayHashMap(u8, String).init(allocator);
    defer map.deinit();

    try map.put(99, "Gretzky");
    try map.put(4, "Orr");
    try map.put(19, "Ratelle");
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.value_ptr.*, entry.key_ptr.* });
    }

    try expect(map.contains(99));

    // The `get` method returns an optional value.
    var name = map.get(99) orelse "";
    try expectEqualStrings("Gretzky", name);

    const removed = map.orderedRemove(99);
    try expect(removed);
    try expectEqual(@as(?[]const u8, null), map.get(99));
}

test "AutoHashMap" {
    var map = std.AutoHashMap(u8, String).init(allocator);
    defer map.deinit();

    try map.put(99, "Gretzky");
    try map.put(4, "Orr");
    try map.put(19, "Ratelle");
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.value_ptr.*, entry.key_ptr.* });
    }

    // Iterate over the map keys.
    var iter2 = map.keyIterator();
    while (iter2.next()) |key| {
        const number = key.*;
        if (map.get(number)) |name| {
            print("{s} number is {d}.\n", .{ name, number });
        }
    }

    try expect(map.contains(99));

    // The `get` method returns an optional value.
    var name = map.get(99) orelse "";
    try expectEqualStrings("Gretzky", name);

    const removed = map.remove(99);
    try expect(removed);
    // try expect(map.get(99) == null);
    try expectEqual(@as(?[]const u8, null), map.get(99));
}

test "ComptimeStringMap" {
    // Create an array of tuples.
    const list = .{
        .{ "Gretzky", 99 },
        .{ "Orr", 4 },
        .{ "Ratelle", 19 },
    };
    try expect(list.len == 3);

    // Create a compile-time map of string keys to u8 values.
    // Since an immutable map with a fixed size is being created,
    // there is no need to deinit it.
    const map = std.ComptimeStringMap(u8, list);

    for (map.kvs) |kv| {
        print("{s} number is {d}.\n", .{ kv.key, kv.value });
    }

    try expect(map.has("Gretzky"));
    try expect(map.has("Orr"));
    try expect(map.has("Ratelle"));

    try expectEqual(@as(u8, 99), map.get("Gretzky").?);
    try expectEqual(@as(u8, 4), map.get("Orr").?);
    try expectEqual(@as(u8, 19), map.get("Ratelle").?);
}

test "StringHashMap" {
    // The keys are strings and the values are unsigned integers.
    var map = std.StringHashMap(u8).init(allocator);
    defer map.deinit();

    try map.put("Gretzky", 99);
    try map.put("Orr", 4);
    try map.put("Ratelle", 19);
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }

    // Iterate over the map keys.
    var iter2 = map.keyIterator();
    while (iter2.next()) |key| {
        print("{s} number is {any}.\n", .{ key.*, map.get(key.*) });
    }

    try expect(map.contains("Gretzky"));

    // The `get` method returns an optional value.
    try expectEqual(@as(?u8, 99), map.get("Gretzky"));

    const removed = map.remove("Gretzky");
    try expect(removed);
    try expectEqual(@as(?u8, null), map.get("Gretzky"));
}
```

<a href="https://ziglang.org/documentation/master/std/#A;std:ComptimeStringMap"
target="_blank">std.ComptimeStringMap</a>
provides an alternative to `StringHashMap` for immutable hash maps
with string keys whose entries are fixed at compile-time.
It has a much simpler API that the hash maps described above
and does not require memory cleanup by calling a `deinit` method.

The `ComptimeStringMap` function takes two arguments.
The first is the value type and the second is an array of key/value tuples.

The following code demonstrates common operations on a `ComptimeStringMap`.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

test "ComptimeStringMap" {
    // Create an array of tuples.
    const list = .{
        .{ "Gretzky", 99 },
        .{ "Orr", 4 },
        .{ "Ratelle", 19 },
    };
    try expect(list.len == 3);

    // Create a compile-time map of string keys to u8 values.
    const map = std.ComptimeStringMap(u8, list);

    for (map.kvs) |kv| {
        print("{s} number is {d}.\n", .{ kv.key, kv.value });
    }

    try expect(map.has("Gretzky"));
    try expect(map.has("Orr"));
    try expect(map.has("Ratelle"));

    try expectEqual(@as(u8, 99), map.get("Gretzky").?);
    try expectEqual(@as(u8, 4), map.get("Orr").?);
    try expectEqual(@as(u8, 19), map.get("Ratelle").?);
}
```

### Sets

A {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:BufSet",
"BufSet" %} is a set of string values.

An {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:EnumSet",
"EnumSet" %} is a set of enum values.

The following code demonstrates common operations on both of these kinds of sets.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

test "BufSet" {
    const allocator = std.testing.allocator;
    var set = std.BufSet.init(allocator);
    defer set.deinit();

    try set.insert("Gretzky");
    try set.insert("Orr");
    try set.insert("Ratelle");
    try expect(set.count() == 3);

    // Iterate over the set keys.
    print("\n", .{});
    var iter = set.iterator();
    while (iter.next()) |key| {
        print("{s}\n", .{key.*});
    }

    try expect(set.contains("Gretzky"));

    set.remove("Gretzky");
    try expect(!set.contains("Gretzky"));
}

test "EnumSet" {
    const Color = enum { red, orange, yellow, green, blue, purple, white, black };

    // This does not use an allocator and does not have a `deinit` method.
    var set = std.EnumSet(Color).initEmpty();

    // To begin with all enum values in the set ...
    // var set = std.EnumSet(Color).initFull();

    // To begin with a subset of the enum values in the set ...
    // var set = std.EnumSet(Color).initMany(&[_]Color{ .orange, .yellow });

    // To begin with one of the enum values in the set ...
    // var set = std.EnumSet(Color).initOne(.orange);

    set.insert(.orange);
    set.insert(.yellow);
    set.insert(.black);
    try expect(set.count() == 3);

    // Iterate over the set keys.
    print("\n", .{});
    var iter = set.iterator();
    while (iter.next()) |key| {
        print("{}\n", .{key});
    }

    try expect(set.contains(.yellow));

    set.remove(.yellow);
    try expect(!set.contains(.yellow));

    // There are many more methods on `EnumSet` instances.
}
```

### MultiArrayList

The {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:MultiArrayList",
"MultiArrayList" %} data structure "stores a list of a struct or tagged union type".
"Instead of storing a single list of items, MultiArrayList stores
separate lists for each field of the struct or lists of tags and bare unions."

TODO: Add an example.

## Builtin Functions

Zig provides over 100 {% aTargetBlank
"https://ziglang.org/documentation/master/#Builtin-Functions", "builtin functions" %}.
These are known to the compiler and
do not require importing in order to use them.

The name of every builtin function begins with "@".
If `@` is followed by an uppercase letter then the function returns a type.

While the builtin functions are invoked like normal functions,
some have behavior that normal functions cannot replicate.

The official documentation at the link above does not categorize
the builtin functions.
The YouTube video {% aTargetBlank "https://www.youtube.com/watch?v=V0sthxzzN3U",
"A Look at Zig's Built-ins" %} from Loris Cro uses the categories below:

### Math Builtin Functions

The math builtin functions can take advantage of CPU-specific capabilities
to achieve better performance than
functions in the standard library found in `std.math`.
However, this is partially achieved by providing less error checking.

Some of these functions can operator on `Vector` types.
TODO: Which ones?

- `@abs` -
- `@addWithOverflow` -
- `@ceil` -
- `@cos` -
- `@divExact` -
- `@divFloor` -
- `@divTrunc` -
- `@exp` -
- `@exp2` -
- `@flor` -
- `@log` -
- `@log10` -
- `@log2` -
- `@max` -
- `@min` -
- `@mod` -
- `@mulAdd` -
- `@mulWithOverflow` -
- `@rem` -
- `@round` -
- `@sin` -
- `@sqrt` -
- `@subWithOverflow` -
- `@tan` -
- `@trunc` -

### Bitwise Builtin Functions

- `@bitReverse` -
- `@byteSwap` -
- `@clz` -
- `@ctz` -
- `@popCount` -
- `@shlExact` -
- `@shlWithOverflow` -
- `@shrExact` -

### Atomic and Memory Builtin Functions

- `@atomicLoad` -
- `@atomicRmw` -
- `@atomicStore` -
- `@cmpxchgStrong` -
- `@cmpxchgWeak` -
- `@fence` -
- `@memcpy` -
- `@memset` -
- `@shuffle` -
- `@splat` -
- `@wasmMemoryGrow` -
- `@wasmMemorySize` -

### Cast and Conversion Builtin Functions

In general, using `@as` is preferred over other casting functions.
Casts can be combined with introspection functions
to achieve better error handling.

- `@alignCast` -
- `@addrSpaceCast` -
- `@as` -
- `@bitCast` -
- `@boolToInt` -
- `@constCast` -
- `@enumFroInt` -
- `@errSetCast` -
- `@errorCas` -
- `@errorFromInt` -
- `@floatCast` -
- `@floatFromInt` -
- `@intCast` -
- `@intFromBool` -
- `@intFromEnum` -
- `@intFromError` -
- `@intFromFloat` -
- `@intFromPtr` -
- `@ptrCast` -
- `@ptrFromInt` -
- `@truncate` -
- `@volatileCast` -

### Programming Builtin Functions

- `@cDefine` -
- `@cImport` -
- `@cInclude` -
- `@cUndef` -
- `@cVaArg`
- `@cVaCopy`
- `@cVaEnd`
- `@cVaStart`
- `@compileError` -
- `@compileLog` -
- `@embedFile` -
- `@export` -
- `@import` -
- `@setAlignStack` -
- `@setCold` -
- `@setEvalBranchQuota` -
- `@setFloatMode` -
- `@setRuntimeSafety` -

### Runtime and Async Builtin Functions

- `@Frame` -
- `@asyncCall` -
- `@breakpoint` -
- `@frameSize` -
- `@frame` -
- `@panic` -

### Introspection Builtin Functions

- `@alignOf` -
- `@bitOffsetOf` -
- `@bitSizeOf` -
- `@byteOffsetOf` -
- `@errorName` -
- `@errorReturnTrace` -
- `@fieldParentPtr` -
- `@frameAddress` -
- `@returnAddress` -
- `@sizeOf` -
- `@src` -
- `@tagName` -

### Metaprogramming Builtin Functions

- `@TagType` -
- `@This` -
- `@TypeOf` -
- `@Type` -
- `@call` -
- `@field` -
- `@hasDecl` -
- `@hasField` -
- `@typeInfo` -
- `@typeName` -
- `@unionInit` -

### Other Builtin Functions

-

### HashMap

A hash map is a collection of key/value pairs.

<a href="https://ziglang.org/documentation/master/std/#A;std:HashMap"
target="_blank">std.HashMap</a> is a low-level implementation that
requires supplying a hashing function.
<br>

<a href="https://ziglang.org/documentation/master/std/#A;std:AutoHashMap"
target="_blank">std.AutoHashMap</a>
provides a good hashing function for most key types.
The first argument is the key type and the second is the value type.
When the key type is `[]const u8`, the following error is triggered:
"std.auto_hash.autoHash does not allow slices here ([]const u8)
because the intent is unclear. Consider using std.StringHashMap
for hashing the contents of []const u8."

The following code creates an `AutoHashMap` where
the keys are strings and the values are unsigned integers.

```zig
var map = std.AutoHashMap([]const u8, u8).init(allocator);
```

<a href="https://ziglang.org/documentation/master/std/#A;std:AutoArrayHashMap"
target="_blank">std.AutoArrayHashMap</a> is similar to `std.AutoHashMap`.
It differs in the following was described in the docs:

- "Insertion order is preserved."
- "Deletions perform a swap removal on the entries list."
- "Modifying the hash map while iterating is allowed, however,
  one must understand the well-defined behavior
  when mixing insertions and deletions with iteration.
- The `values` method "returns the backing array of values".

<a href="https://ziglang.org/documentation/master/std/#A;std:StringHashMap"
target="_blank">std.StringHashMap</a>
provides a good hashing function for string keys.
The argument is the value type.

A `HashMap` can be used as a set where the values are `{}`.

The following code demonstrates common operations on `HashMap`s.

```zig
const std = @import("std");
const print = std.debug.print;
const allocator = std.testing.allocator;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const String = []const u8;

test "AutoArrayHashMap" {
    var map = std.AutoArrayHashMap(u8, String).init(allocator);
    defer map.deinit();

    try map.put(99, "Gretzky");
    try map.put(4, "Orr");
    try map.put(19, "Ratelle");
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.value_ptr.*, entry.key_ptr.* });
    }

    try expect(map.contains(99));

    // The `get` method returns an optional value.
    var name = map.get(99) orelse "";
    try expectEqualStrings("Gretzky", name);

    const removed = map.orderedRemove(99);
    try expect(removed);
    try expectEqual(@as(?[]const u8, null), map.get(99));
}

test "AutoHashMap" {
    var map = std.AutoHashMap(u8, String).init(allocator);
    defer map.deinit();

    try map.put(99, "Gretzky");
    try map.put(4, "Orr");
    try map.put(19, "Ratelle");
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.value_ptr.*, entry.key_ptr.* });
    }

    // Iterate over the map keys.
    var iter2 = map.keyIterator();
    while (iter2.next()) |key| {
        const number = key.*;
        if (map.get(number)) |name| {
            print("{s} number is {d}.\n", .{ name, number });
        }
    }

    try expect(map.contains(99));

    // The `get` method returns an optional value.
    var name = map.get(99) orelse "";
    try expectEqualStrings("Gretzky", name);

    const removed = map.remove(99);
    try expect(removed);
    // try expect(map.get(99) == null);
    try expectEqual(@as(?[]const u8, null), map.get(99));
}

test "ComptimeStringMap" {
    // Create an array of tuples.
    const list = .{
        .{ "Gretzky", 99 },
        .{ "Orr", 4 },
        .{ "Ratelle", 19 },
    };
    try expect(list.len == 3);

    // Create a compile-time map of string keys to u8 values.
    // Since an immutable map with a fixed size is being created,
    // there is no need to deinit it.
    const map = std.ComptimeStringMap(u8, list);

    for (map.kvs) |kv| {
        print("{s} number is {d}.\n", .{ kv.key, kv.value });
    }

    try expect(map.has("Gretzky"));
    try expect(map.has("Orr"));
    try expect(map.has("Ratelle"));

    try expectEqual(@as(u8, 99), map.get("Gretzky").?);
    try expectEqual(@as(u8, 4), map.get("Orr").?);
    try expectEqual(@as(u8, 19), map.get("Ratelle").?);
}

test "StringHashMap" {
    // The keys are strings and the values are unsigned integers.
    var map = std.StringHashMap(u8).init(allocator);
    defer map.deinit();

    try map.put("Gretzky", 99);
    try map.put("Orr", 4);
    try map.put("Ratelle", 19);
    try expect(map.count() == 3);

    // Iterate over the map entries.
    print("\n", .{});
    var iter = map.iterator();
    while (iter.next()) |entry| {
        print("{s} number is {d}.\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }

    // Iterate over the map keys.
    var iter2 = map.keyIterator();
    while (iter2.next()) |key| {
        print("{s} number is {any}.\n", .{ key.*, map.get(key.*) });
    }

    try expect(map.contains("Gretzky"));

    // The `get` method returns an optional value.
    try expectEqual(@as(?u8, 99), map.get("Gretzky"));

    const removed = map.remove("Gretzky");
    try expect(removed);
    try expectEqual(@as(?u8, null), map.get("Gretzky"));
}
```

<a href="https://ziglang.org/documentation/master/std/#A;std:ComptimeStringMap"
target="_blank">std.ComptimeStringMap</a>
provides an alternative to `StringHashMap` for immutable hash maps
with string keys whose entries are fixed at compile-time.
It has a much simpler API that the hash maps described above
and does not require memory cleanup by calling a `deinit` method.

The `ComptimeStringMap` function takes two arguments.
The first is the value type and the second is an array of key/value tuples.

The following code demonstrates common operations on a `ComptimeStringMap`.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

test "ComptimeStringMap" {
    // Create an array of tuples.
    const list = .{
        .{ "Gretzky", 99 },
        .{ "Orr", 4 },
        .{ "Ratelle", 19 },
    };
    try expect(list.len == 3);

    // Create a compile-time map of string keys to u8 values.
    const map = std.ComptimeStringMap(u8, list);

    for (map.kvs) |kv| {
        print("{s} number is {d}.\n", .{ kv.key, kv.value });
    }

    try expect(map.has("Gretzky"));
    try expect(map.has("Orr"));
    try expect(map.has("Ratelle"));

    try expectEqual(@as(u8, 99), map.get("Gretzky").?);
    try expectEqual(@as(u8, 4), map.get("Orr").?);
    try expectEqual(@as(u8, 19), map.get("Ratelle").?);
}
```

### Sets

A {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:BufSet",
"BufSet" %} is a set of string values.

An {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:EnumSet",
"EnumSet" %} is a set of enum values.

The following code demonstrates common operations on both of these kinds of sets.

```zig
const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

test "BufSet" {
    const allocator = std.testing.allocator;
    var set = std.BufSet.init(allocator);
    defer set.deinit();

    try set.insert("Gretzky");
    try set.insert("Orr");
    try set.insert("Ratelle");
    try expect(set.count() == 3);

    // Iterate over the set keys.
    print("\n", .{});
    var iter = set.iterator();
    while (iter.next()) |key| {
        print("{s}\n", .{key.*});
    }

    try expect(set.contains("Gretzky"));

    set.remove("Gretzky");
    try expect(!set.contains("Gretzky"));
}

test "EnumSet" {
    const Color = enum { red, orange, yellow, green, blue, purple, white, black };

    // This does not use an allocator and does not have a `deinit` method.
    var set = std.EnumSet(Color).initEmpty();

    // To begin with all enum values in the set ...
    // var set = std.EnumSet(Color).initFull();

    // To begin with a subset of the enum values in the set ...
    // var set = std.EnumSet(Color).initMany(&[_]Color{ .orange, .yellow });

    // To begin with one of the enum values in the set ...
    // var set = std.EnumSet(Color).initOne(.orange);

    set.insert(.orange);
    set.insert(.yellow);
    set.insert(.black);
    try expect(set.count() == 3);

    // Iterate over the set keys.
    print("\n", .{});
    var iter = set.iterator();
    while (iter.next()) |key| {
        print("{}\n", .{key});
    }

    try expect(set.contains(.yellow));

    set.remove(.yellow);
    try expect(!set.contains(.yellow));

    // There are many more methods on `EnumSet` instances.
}
```

### MultiArrayList

The {% aTargetBlank "https://ziglang.org/documentation/master/std/#A;std:MultiArrayList",
"MultiArrayList" %} data structure "stores a list of a struct or tagged union type".
"Instead of storing a single list of items, MultiArrayList stores
separate lists for each field of the struct or lists of tags and bare unions."

TODO: Add an example.

## Builtin Functions

Zig provides over 100 {% aTargetBlank
"https://ziglang.org/documentation/master/#Builtin-Functions", "builtin functions" %}.
These are known to the compiler and
do not require importing in order to use them.

The name of every builtin function begins with "@".
If `@` is followed by an uppercase letter then the function returns a type.

While the builtin functions are invoked like normal functions,
some have behavior that normal functions cannot replicate.

The official documentation at the link above does not categorize
the builtin functions.
The YouTube video {% aTargetBlank "https://www.youtube.com/watch?v=V0sthxzzN3U",
"A Look at Zig's Built-ins" %} from Loris Cro uses the categories below:

### Math Builtin Functions

The math builtin functions can take advantage of CPU-specific capabilities
to achieve better performance than
functions in the standard library found in `std.math`.
However, this is partially achieved by providing less error checking.

Some of these functions can operator on `Vector` types.
TODO: Which ones?

- `@abs` -
- `@addWithOverflow` -
- `@ceil` -
- `@cos` -
- `@divExact` -
- `@divFloor` -
- `@divTrunc` -
- `@exp` -
- `@exp2` -
- `@flor` -
- `@log` -
- `@log10` -
- `@log2` -
- `@max` -
- `@min` -
- `@mod` -
- `@mulAdd` -
- `@mulWithOverflow` -
- `@rem` -
- `@round` -
- `@sin` -
- `@sqrt` -
- `@subWithOverflow` -
- `@tan` -
- `@trunc` -

### Bitwise Builtin Functions

- `@bitReverse` -
- `@byteSwap` -
- `@clz` -
- `@ctz` -
- `@popCount` -
- `@shlExact` -
- `@shlWithOverflow` -
- `@shrExact` -

### Atomic and Memory Builtin Functions

- `@atomicLoad` -
- `@atomicRmw` -
- `@atomicStore` -
- `@cmpxchgStrong` -
- `@cmpxchgWeak` -
- `@fence` -
- `@memcpy` -
- `@memset` -
- `@shuffle` -
- `@splat` -
- `@wasmMemoryGrow` -
- `@wasmMemorySize` -

### Cast and Conversion Builtin Functions

In general, using `@as` is preferred over other casting functions.
Casts can be combined with introspection functions
to achieve better error handling.

- `@alignCast` -
- `@addrSpaceCast` -
- `@as` -
- `@bitCast` -
- `@boolToInt` -
- `@constCast` -
- `@enumFroInt` -
- `@errSetCast` -
- `@errorCas` -
- `@errorFromInt` -
- `@floatCast` -
- `@floatFromInt` -
- `@intCast` -
- `@intFromBool` -
- `@intFromEnum` -
- `@intFromError` -
- `@intFromFloat` -
- `@intFromPtr` -
- `@ptrCast` -
- `@ptrFromInt` -
- `@truncate` -
- `@volatileCast` -

### Programming Builtin Functions

- `@cDefine` -
- `@cImport` -
- `@cInclude` -
- `@cUndef` -
- `@cVaArg`
- `@cVaCopy`
- `@cVaEnd`
- `@cVaStart`
- `@compileError` -
- `@compileLog` -
- `@embedFile` -
- `@export` -
- `@import` -
- `@setAlignStack` -
- `@setCold` -
- `@setEvalBranchQuota` -
- `@setFloatMode` -
- `@setRuntimeSafety` -

### Runtime and Async Builtin Functions

- `@Frame` -
- `@asyncCall` -
- `@breakpoint` -
- `@frameSize` -
- `@frame` -
- `@panic` -

### Introspection Builtin Functions

- `@alignOf` -
- `@bitOffsetOf` -
- `@bitSizeOf` -
- `@byteOffsetOf` -
- `@errorName` -
- `@errorReturnTrace` -
- `@fieldParentPtr` -
- `@frameAddress` -
- `@returnAddress` -
- `@sizeOf` -
- `@src` -
- `@tagName` -

### Metaprogramming Builtin Functions

- `@TagType` -
- `@This` -
- `@TypeOf` -
- `@Type` -
- `@call` -
- `@field` -
- `@hasDecl` -
- `@hasField` -
- `@typeInfo` -
- `@typeName` -
- `@unionInit` -

### Other Builtin Functions

-

## Unit Tests

Unit tests can be included in source files
in order to test the functions they define.

Each test is described by the `test` keyword followed by
a test description string (or a function name) and a block of code.

The block of code uses functions whose
names begin with `expect` to make assertions.
Calls to these functions must be preceded by the `try` keyword.
For information about these functions, see the {% aTargetBlank
"https://ziglang.org/documentation/master/std/#A;std:testing",
"std.testing documentation" %}.

The `expect` function takes a single argument
that must be an expression that evaluates to a `bool` value.

The `expectEquals` function takes two arguments
which are expressions representing an expected and actual value.
Using `expectedEquals` provided better failure messages than `equal`.

Other testing functions include `expectApproxEqAbs`, `expectApproxEqRel`,
`expectEqualDeep`, `expectEqualSentinel`, `expectEqualSlices`,
`expectEqualStrings`, `expectError`, `expectFmt`,
`expectStringEndsWith`, and `expectStringStartsWith`.

The functions `expectApproxEqAbs`, `expectApproxEqRel`,
`expectEqual`, and `expectEqualDeep` all have the parameters
`expected: anytype, actual: @TypeOf(expected)`.
This causes the second argument to be cast to the type of the first.
If the expected value is a literal value,
it must be cast with "@as" if it is the first argument,
but not if it is the second.
So it is typically easier to pass the actual value as the first argument
and the expected value as the second argument.

All tests in a source file are executed by running `zig test {file-name}.zig`.
To run specific tests, add the `--test-filter {text}` option
which causes it to only run tests whose description contains the given text.

To temporarily skip a test, add the line `return error.SkipZigTest`.
TODO: Will the compiler complain about unreachable code
TODO: if this is the first line in the test?

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

To test for memory leaks, use the `std.testing.allocator`
for all memory allocation.

To run tests in multiple source files,
create a new source file that imports all of them
and pass that source file to `zig test`.
For example:

```zig
test {
  _ = @import("file1.zig");
  _ = @import("file2.zig");
  _ = @import("file3.zig");
}
```

TODO: Also see std.testing.refAllDecls and std.testing.refAllDeclsRecursive.
TODO: Is there another way to do this by modifying a project `build.zig` file?

To determine whether code is running in a test, import the variable `is_test`
with `@import("builtin").is_test`.
This can be used to avoid running certain code in a test
or only run certain code in a test.
It can also be used only use `std.testing.allocator` when running in a test.

Enter `zig test --help` to see options that affect tests.

## Command-line Arguments

When running a Zig programming using `zig run`,
command-line arguments must be preceded by `--`.
For example, `zig run commmand_line_args_demo.zig -- foo bar`.
The first argument will be the path to the executable
and the remaining arguments will be the actual command-line arguments.


The following code demonstrates getting command-line arguments
and copying them into an `ArrayList` to simply using them.

```zig
const std = @import("std");
const print = std.debug.print;
const allocator = std.heap.page_allocator;

fn getCmdLineArgs(list: anytype) !void {
    var iter = std.process.args();
    while (iter.next()) |arg| {
        try list.append(arg);
    }
}

pub fn main() !void {
    var args = std.ArrayList([]const u8).init(allocator);
    defer args.deinit();
    try getCmdLineArgs(&args);

    print("arg count is {d}\n", .{args.items.len});
    print("second arg is {s}\n", .{args.items[1]});
    for (args.items) |arg| {
        print("{s}\n", .{arg});
    }
}
```

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

## Writing and Reading Files

The standard libraries `std.fs` and `std.os` define many I/O functions.
Here is an example of writing and reading a text file.

```zig
const std = @import("std");
const print = std.debug.print;

// Creates and writes a file in the current working directory.
fn writeFile() !void {
    const dir = std.fs.cwd();
    const file = try dir.createFile("data.txt", .{});
    defer file.close();

    try file.writeAll("Hello, World!");
}

// Reads a file in the current working directory.
fn readFile() !void {
    const dir = std.fs.cwd();
    const file = try dir.openFile("data.txt", .{});
    defer file.close();

    var buffer: [100]u8 = undefined;
    const length = try file.readAll(&buffer);
    print("read {} bytes\n", .{length}); // 13
    const content = buffer[0..length];
    print("{s}\n", .{content}); // Hello, World!
}

pub fn main() !void {
    try writeFile();
    try readFile();
}
```

## HTTP

TODO: See https://github.com/zigzap/zap which may be the only Zig HTTP server now.

## Compiling C and C++

Zig provides tooling to compile both C and C++ code.

Here is a C hello world program in the file `hello.c`.

```c
#include <stdio.h>

int main() {
  printf("Hello, World!\n");
  return 0;
}
```

To build this, enter `zig cc hello.c -o hello`.
To run the resulting executable, enter `./hello`.

Here is a C++ hello world program in the file `hello.cpp`.

```cpp
#include <iostream>

int main() {
  std::cout << "Hello World!" << std::endl;
  return 0;
}
```

To build this, enter `zig c++ hello.cpp -o hello`.
To run the resulting executable, enter `./hello`.

## CONTINUE CLEANUP OF EVERYTHING BELOW HERE!

Learn about async/await.

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

Learn about anonymous structs which are called "tuples".
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

when running test, use the std.testing.allocator so when tests are run, it will flag when memory is not freed.

A function return type can be a switch expression that determines the actual type returned.

function names that begin with @ built-in functions.
some built-in functions do things that user created functions cannot.
what does it mean when the name of a built-in function begins with at symbol, followed by an uppercase letter?
Runtime safety checks won’t protect you from every possible mistake, but they come close. More safety checks are planned in the future.

does the undefined behavior section of the official docs list all the current runtime safety checks?

Did you document multiline string literals?

Add the Ghosty terminal emulator to your list of Ziggy’s cases.
