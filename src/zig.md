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
It is a modern alternative to C with similar syntax
such as statements terminated with semicolons and conditions in parentheses.

Zig provides a complete, LLVM-based toolchain for creating, developing,
building, and testing apps written in Zig, C, and C++.
There are advantages to building apps with the Zig compiler
even if they have no Zig code and only use C and/or C++ code.

Zig is suitable for applications that care deeply about
performance, binary size, and/or memory usage.
Often these concerns justify the tedium and verbosity of
manual memory management that is required
due to lack of automated garbage collection.

One appeal of Zig is that it is much simpler than C++ and Rust.

Zig emphasizes:

- No hidden control flow

  Examples of hidden control flow in other languages include
  exception handling, operator overloading, and destructors.

- No hidden memory allocations

- No preprocessors or macros

  In place of these, Zig uses code that runs at compile-time,
  indicated by the `comptime` keyword.

Zig includes:

- a package manager
- a build system that is simpler that the
  combinations of build tools typically used with C and C++
- a build system API (used in `build.zig` files)
- cross-compilation support
- a test runner

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

The file `main.sig` is the starting point of the project.
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

Zig does not allow unused variables.
Editor extensions/plugins such as vscode-zig
can add lines like `_ = my_variable` for each unused variable on save
so they appear to be used.
This feature may be enabled by default, can be disabled.
In vscode-zig, the "Zls: Enable Autofix" option controls this.

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

pub fn main() void {
    const c = Color.green;
    print("c = {}\n", .{c}); // enum_demo.main.Color.green
    print("c = {}\n", .{@intFromEnum(c)}); // 8
    print("green primary? {}\n", .{c.isPrimary()}); // false
    print("green primary? {}\n", .{Color.isPrimary(c)}); // false
    print("yellow primary? {}\n", .{Color.yellow.isPrimary()}); // true
}
```

## Arrays

Array types have the syntax `[length]type`.
For example, `[5]i32` is an array of five integers.

The length can be replaced by an underscore
when it can be inferred from an initial value.
For example:

```zig
const dice_rolls = [_]u8{ 4, 2, 5, 1, 2 };
```

Arrays have a `len` field that holds its length.
The length of an array cannot be changed.
For a dynamically-sized array, consider using {% aTargetBlank
"https://ziglang.org/documentation/master/std/#A;std:ArrayList", "ArrayList" %}.

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

    // Get a slice of an array.
    // Indexes begin at zero.
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

## Strings

Strings are represented by arrays of type `[]u8`.
This treats strings like a collection of bytes rather than Unicode characters.
Literal strings are delimited by double quotes.
Zig only provides the ability to operator on strings as byte arrays.

The following string operations are supported using byte arrays.

| Operation          | Example                       |
| ------------------ | ----------------------------- |
| assign to variable | var name: []u8 = "Mark";      |
| get a byte         | const letter2 = name[1]; // a |
| modify a byte      | name[1] = 'o'; // now "Mork"  |

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

## Slices

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

When an error occurs in a function, an error `enum` value is returned.
Zig does not support throwing exceptions.

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

## comptime

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

Types in Zig, such as builtin types lik `i32` and custom `struct` types,
are first-class values.
This means they can be assigned to variables, assigned to struct fields,
passed to functions, and returned from functions.

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

## Unit Tests

Unit tests can be included in source files
in order to test the functions they define.

Each test is described by the `test` keyword followed by
a function name or a test description string and a block of code.

The block of code uses functions whose
names begin with `expect` to make assertions.
For information about these functions, see the {% aTargetBlank
"https://ziglang.org/documentation/master/std/#A;std:testing",
"std.testing documentation" %}.

The `expect` function takes a single argument
that must be an expression that evaluates to a `bool` value.
If the expression can return an error,
the `assert` must be preceded by the `try` keyword.
Otherwise it cannot be preceded by `try`.
The test will fail if an error is returned.

The `expectEquals` function takes two arguments
which are expressions representing an expected and actual value.
Using `expectedEquals` provided better failure messages than `equal`.

Other testing functions include `expectApproxEqAbs`, `expectApproxEqRel`,
`expectEqualDeep`, `expectEqualSentinel`, `expectEqualSlices`,
`expectEqualStrings`, `expectError`, `expectFmt`,
`expectStringEndsWith`, and `expectStringStartsWith`.

All tests is a source file are executed by running `zig test {file-name}.zig`.
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

## CLEANUP EVERYTHING BELOW HERE!

Can create a slice from an array, another slice, or a. multi-pointer (define).

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

Runtime safety includes array bounds checking, …

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

describe difference between defer and errdefer.

std.process.args to get command line args. Learn how to use this.

Maybe the purpose of std.heap.ArenaAllocator is that you can use it for allocating many things that don’t need to be individually freed. You just defer arena.deinit() one time right after it is created.
