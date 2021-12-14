---
eleventyNavigation:
  key: Dart
layout: topic-layout.njk
---

## Overview

<img alt="Dart logo" style="width: 40%"
    src="/blog/assets/Dart-logo.png?v={{pkg.version}}"
    title="Dart logo">

{% aTargetBlank "https://dart.dev", "Dart" %} is a
general purpose, strongly typed, programming language from Google.
It was first announced in October, 2012.

The original goal of Dart was to be an alternative to JavaScript
for running code in web browsers.
The Chrome browser planned to include a Dart VM for this purpose,
but that plan has been abandoned in favor of compiling to JavaScript.

Currently Dart is primarily used by the
{% aTargetBlank "https://flutter.dev", "Flutter" %} framework
for building mobile, web, and desktop applications.

The syntax of Dart is somewhat similar to Java.
Statements are terminated by semi-colons.
Parentheses surround conditions.
Curly braces surround blocks of code.
Indentation is typically two spaces.

Dart supports operator overloading to define the meaning
of operators when applied to instances of custom classes.

## Resources

TODO: Add more resources.

- [Dart home page](https://dart.dev)
- [Effective Dart](https://dart.dev/guides/language/effective-dart)
- [Dart Complete Course](https://youtu.be/F3JuuYuOUK4)

## Editors

Many code editors can be used for writing Dart programs.
Popular options include VS Code, Intellij IDEA, and Android Studio.
{% aTargetBlank "https://dartpad.dev", "DartPad" %}
is an online editor for experimenting with Dart features.

Recommended VS Code extensions for Dart include:

- {% aTargetBlank
  "https://marketplace.visualstudio.com/items?itemName=Dart-Code.dart-code",
  "Dart" %}

  "Provides tools for effectively editing, refactoring, running,
  and reloading Flutter mobile apps."

- {% aTargetBlank
  "https://marketplace.visualstudio.com/items?itemName=BendixMa.dart-data-class-generator",
  "Dart Data Class Generator" %}

  "Create dart data classes easily, fast and
  without writing boilerplate or running code generation."

- {% aTargetBlank
  "https://marketplace.visualstudio.com/items?itemName=jeroen-meijer.pubspec-assist",
  "Pubspec Assist" %}

  "To easily add dependencies to your Dart and Flutter project's pubspec.yaml."

## pub.dev

{% aTargetBlank "https://pub.dev", "pub.dev" %} is the
official package repository for Dart and Flutter apps.
Packages here are listed in five catagories:
"Flutter Favorites", "Most popular packages", "Top Flutter packages",
"Top Dart packages", and "Package of the Week".

There are two kinds of packages.
"Library packages" are used as dependencies of other packages.
"Application packages" are meant to be run
and can depend on library packages.

Some notable packages to consider using include:

- http

  "A composable, multi-platform, Future-based API for HTTP requests."

- json_serializable

  "Automatically generate code for converting to and from JSON
  by annotating Dart classes."

- hive

  "Lightweight and blazing fast key-value database"

- lints

  "Official Dart lint rules. Defines the 'core' and 'recommended'
  set of lints suggested by the Dart team."  
  Use of these lint rules is enabled by the following line
  found in the `analysis_options.yaml` file:

  ```yaml
  include: package:lints/recommended.yaml
  ```

  Edit the `analysis_options.yaml` file to configure the use of specific rules.
  An older set of lint rules called `pedantic` is deprecated.
  Another set of lint rules to consider using is {% aTargetBlank
  "https://pub.dev/packages/very_good_analysis", "Very Good Analysis" %}.

The official Dart package manager is `pub`.
This is similar to `npm` for Node.js.
In the same way that `npm` relies on the files
`package.json` and `package-lock.json` to manage dependencies,
`pub` relies on the files `pubspec.yaml` and `pubspec.lock` (also a YAML file).

## Creating and Running Programs

Dart source files have a `.dart` file extension.

The `main` function defines the starting point of a program.
It is passed a `List` of command-line arguments.
The type can be omitted, specified as just `List`,
or specified as `List<String>`.

```dart
main(args) {
  args.forEach((arg) => print('arg = $arg'));
}
```

To run a `.dart` file that defines a `main` function,
enter `dart {name}.dart [arguments]`.

It is not necessary to create a Dart "project"
in order to write and run Dart code, but doing so provides many benefits.
It gives a project a standard directory structure,
makes it easy to add dependencies,
enables writing tests,
makes it easy to publish packages to
{% aTargetBlank "https://pub.dev", "pub.dev" %},
and much more.

To create a new Dart project, enter `dart create {project-name}`.
Dart prefers underscores over hyphens in names.
For example:

```bash
dart create hello_world
```

To run a Dart project, cd to the project directory and enter `dart run`.
For example:

```dart
cd hello_world`
dart run # outputs "Hello world!"
```

To analyze a Dart project for syntax errors and lint rule violations,
enter `dart analyze`.
TODO: Why does this always output "No issues found!" even when
TODO: there are syntax errors and lint rule violations?

The `dart create` command creates the following files and directories:

- `.dart_tool/package_config.json`

  This file is used to resolve Dart package names to `.dart` files.

- `bin`

  This directory contains `.dart` source files
  that are meant to be executed.

- `bin/hello_world.dart`

  This Dart source file is the project starting point.

- `.gitignore`

  This file lists directories and files that
  should not be added to a Git repository.

- `lib`

  This directory is not provided, but should be created to
  hold `.dart` files that define libraries of functions and classes
  used by other `.dart` files in the project.

  Files in the `bin` directory import files from here using the syntax
  `import 'package:{project-name}/{file-name}.dart';`
  For files in subdirectories of the `lib` directory,
  add subdirectories after the project name.
  For example, `import 'package:{project-name}/{subdir-name}/{file-name}.dart';`

- `.packages`

  This file is deprecated.

- `analysis_options.yaml`

  This file configures Dart linting rules.

- `CHANGELOG.md`

  This file describes versions of the project.

- `pubspec.yaml`

  This file describes project dependencies and more.
  It is similar to `package.json` in Node.js projects.
  The properties that are optional are indicated in the list below.

  - `name`: package name (can use underscores, but not hyphens)
  - `description`: package description
  - `version`: using semver major.minor.patch convention
  - `homepage`: optional URL
  - `repository`: optional URL
  - `issue_tracker`: optional URL
  - `documentation`: optional URL

  - `publish_to`: information for publishing to pub.dev
  - `environment`: describes supported Dart versions
  - `dependencies`: list of packages needed at runtime; can omit if none
  - `dev_dependencies`: list of packages only needed for development; can omit if none

  There are three supported syntaxes for
  specifying acceptable versions of a dependency.

  1. specific semver; ex. `1.2.3`
  1. semver range; ex. `>=1.2.3 <2.0.0`
  1. caret semver; ex. `^1.2.3` (means same as above)

  The caret semver syntax is preferred.
  It has a slightly different meaning when the major version is zero.
  For example, `^0.1.2` is the same as `>=0.1.2 <0.2.0`.

  Most dependencies come from pub.dev, but it is also possible
  to install dependencies from other sources such a Git repositories
  and packages in the `packages` directory of the current project.

- `pubspec.lock`

  This file records the exact versions of each installed dependency.
  It is similar to `package-lock.json` in Node.js projects.

  When this file doesn't exist, running the `pub get` command
  installs the versions of the dependencies specified in `pubspec.yaml`
  and creates this file to record the installed versions.

  When this file exists, running the `pub get` command
  installs the versions of the dependencies specified here.

- `README.md`

  This MarkDown file describes the project.

A Dart project is actually a Dart package.
This means it is possible for it to be deployed to pub.dev
and its public API can be shared with other Dart packages.

## Dart SDK

The Dart SDK includes all the tools needed to
create, build, and run Dart applications.
It can be downloaded from
{% aTargetBlank "https://dart.dev/get-dart", "get-dart" %}
which provides instructions for installing in Windows, Linux, and macOS.

The Flutter SDK includes the Dart SDK.

After installing the Dart SDK, enter "dart" in a terminal
to see all the subcommand options.

To create a new Dart project, enter `dart create -t {type} {project-name}`
where `type` is `console-simple`, `console-full`,
`package-simple`, `server-shelf`, or `web-simple`.

The Dart SDK contains three compilers.

1. Just In Time (JIT) compiler  
   This compiles Dart code to an intermediate format
   and runs it in a virtual machine, which is ideal during development.

1. Ahead Of Time (AOT) compiler  
   This compiles and builds an executable for a Dart program,
   which is ideal for releasing a finished application.
   It performs type flow analysis and removes unused code
   in order to produce a smaller executable.
   It also performs optimizations such as inlining code
   in order to produce a faster executable.

1. JavaScript (JS) compiler  
   This compiles a Dart program to JavaScript,
   which allows it to be run in a web browser.

## Keywords

For a list of keywords in the Dart language
with links to their descriptions, see {% aTargetBlank
"https://dart.dev/guides/language/language-tour#keywords",
"Language Tour - Keywords" %}.

## Comments

Comments in Dart are written like in many other programming languages.

- Single-line comments begin with `//`.
- Multi-line comments are delimited by `/*` and `*/`.
- Documentation comments begin with `///` or are delimited by `/**` and `*/`.
  These are used to generate documentation from source files.

## Variables

There are three ways to declare variables.

1. With a type

   For example, `int score;`

1. With the `var` keyword

   The type is obtained through type inference.
   For example, these are equivalent:

   ```dart
   int n = 19;
   var n = 19;
   ```

   When a `var` declaration is not initialized,
   it is treated like `dynamic` which is described next.

1. With the `dynamic` keyword

   The type can change throughout the lifetime of the variable
   based on the value currently assigned. For example:

   ```dart
   dynamic n = 19;
   n = 3.14;
   n = "changed";
   ```

To get the runtime type of an object, access the `runtimeType` property.
This has a type of `Type` which has a `toString` method.

## Immutability with const and final

Variables whose values are known at compile-time
should be declared with the keyword `const`.
Class properties whose values are known at compile-time
should be declared with the keywords `static const`.
This prevents assigning a new value,
and also prevents modifying the value
(for example, adding elements to a `List`).

The `const` keyword can be applied to top-level variables,
local variables in functions, and class static properties.
It cannot be applied to class instance properties.

Variables and class properties whose values should be
set once at run-time and never changed
should be declared with the keyword `final`.
This prevents assigning a new value,
but does not prevent modifying the value.

The meaning of the `const` and `final` keywords depends on
whether they are applied to a variable or its initial value.
There are two aspects to consider,
whether a new value can be assigned to the variable and
whether its value can be modified (ex. adding elements to a list).

The following code demonstrates using the `const` and `final` keywords
in conjunction with declaring and creating `List` collection objects.
The same principles apply to other kinds of collections.

```dart
List<int> l1 = [1, 2];
//var l1 = <int>[1, 2]; // alternate way to write previous line
l1.add(3); // can modify current value
l1 = [3, 4]; // can assign a new value
print(l1); // [3, 4]

// const here makes the variable AND its value immutable.
// Adding const after "=" and before the value is redundant.
const List<int> l2 = [1, 2];
//l2.add(3); // throws UnsupportedError at runtime
//l2 = [3, 4]; // cannot assign a new value
print(l2); // [1, 2]

// Adding const only before the value makes the value immutable,
// but a new value can be assigned to the variable.
List<int> l3 = const [1, 2];
//l3.add(3); // throws UnsupportedError at runtime
l3 = [3, 4]; // can assign a new value
print(l3); // [3, 4]

// final here means the variable can only be assigned a value once,
// but its value can be modified.
final List<int> l4 = [1, 2];
l4.add(3); // can modify current value
//l4 = [3, 4]; // cannot assign a new value
print(l4); // [1, 2, 3]

// This is the same as the l2 declaration
// which is preferred because it is shorter.
final List<int> l5 = const [1, 2];
//l5.add(3); // throws UnsupportedError at runtime
//l5 = [3, 4]; // cannot assign a new value
print(l5); // [1, 2]
```

## Importing packages

A `.dart` file can import packages using the `import` statement.
The following imports the `dart:math` package
and places all its top-level public names (such as `pi` and `sin`)
in the current namespace:

```dart
import 'dart:math';
```

To avoid conflicting with names already in the current namespace,
use the `as` keyword.

```dart
import 'dart:math' as math;
```

Now names like `pi` can be referred to with `math.pi`.

## Types

All values in Dart are objects from some class.
This is even true for basic types like `bool`, `int`, and `double`.

All types except `Null` are subclasses of the `Object` class.

Dart supports the following built-in basic types:

- `void`: means a value is never used
- `Null`: represents not having a value
- `bool`: boolean value with literal values `true` and `false`
- `int`: 64-bit integer
- `double`: 64-bit floating point number
- `String`: sequence of UTF-16 characters delimited by single or double quotes
- `Symbol`: represents an operator or identifier with literal syntax `#name`
- `Never`: has no values

The `void` type is used at the return type
of functions that do not return anything.

The keyword `null` refers to the only instance of the `Null` type.

The `Never` type is the type of `throw` expressions,
and is the return type of functions that always throw.

Note that there is no `float` type.
The `double` type is used for all floating point numbers.

Dart supports the following built-in collection types:

- `List`: ordered collection of values (see the `List` section)
- `Set`: unordered collection of unique values (see the `Set` section)
- `Map`: unordered collection of key/value pairs (see the `Map` section)

Dart supports type inference, so types
do not need to specified if they can be inferred from values.

By default no type allows the value `null`.
To allow this prepend `?` to the type name.
For example, a variable of type `String` cannot be set to `null`,
but a variable of type `String?` can.
The compiler requires handling cases where a nullable value might be `null`.
This results in detecting errors involving `null` values
at compile-time rather than runtime.

The `is` operator tests whether a variable currently holds
a value of a given type and evaluates to a `bool`.
For example:

```dart
void evaluate(num n) {
  if (n is int) {
    // n is cast to int inside this block.
    print('$n is ${n.isOdd ? 'odd' : 'even'}.');
  }
}

void main() {
  num n = 3.1;
  evaluate(n); // outputs nothing
  n = 7;
  evaluate(n); // outputs "7 is odd."
}
```

The `is!` operator performs the opposite test.

### Enumerations

Enumerations are a special kind of class defined with the `enum` keyword.
The are often used in `switch` statements.

Each value in an `enum` is assigned an index starting from zero.
They cannot be assigned different numeric values.

Enumerations cannot be defined inside a function.

For example:

```dart
enum Color { red, green, blue }

void printColor(Color c) {
  print('$c, index=${c.index}');
}

void main() {
  printColor(Color.values[1]); // Color.blue, index=1

  for (var color in Color.values) {
    printColor(color);
  }
}
```

### bool type

Instances of the `bool` class represent a boolean value.
The only values are the literal values `true` and `false`.

The `bool` class doesn't add any interesting properties or methods,
but it defines the following operators:
`&` (and), `|` (or), and `^` (xor).

### Number types

The `int` and `double` classes are the only number types supported by Dart.
Both inherit from the `num` class and both represent 64-bit values.
Custom classes are not allowed to inherit from the `num` class.

#### num Class

Variables declared with the `num` type can be
assigned both `int` and `double` values.

The `num` class defines the following properties:

| Property     | Description                                              |
| ------------ | -------------------------------------------------------- |
| `isFinite`   | `bool` indicating whether the number is finite           |
| `isInfinite` | `bool` indicating whether the number is infinite         |
| `inNaN`      | `bool` indicating whether this is the Not-a-Number value |
| `isNegative` | `bool` indicating whether the number is negative         |
| `sign`       | -1, 0, or 1                                              |

The `num` class defines the following instance methods:

| Method                                       | Description                                                          |
| -------------------------------------------- | -------------------------------------------------------------------- |
| `abs()`                                      | returns absolute value                                               |
| `ceil()`                                     | returns least integer not smaller                                    |
| `ceilToDouble()`                             | same as `ceil`, but returns a `double`                               |
| `clamp(num lower, num upper)`                | returns n < lower ? lower : n > upper ? upper : n                    |
| `compareTo(num other)`                       | returns comparator `int` value                                       |
| `floor()`                                    | returns greatest integer not greater                                 |
| `floorToDouble()`                            | same as `floor`, but returns a `double`                              |
| `remainder()`                                | returns remainder of truncating division (modulo)                    |
| `round()`                                    | returns closest integer                                              |
| `roundToDouble()`                            | same as `round`, but returns a `double`                              |
| `toDouble()`                                 | returns number as a `double`                                         |
| `toInt()`                                    | returns number as an `int`                                           |
| `toString()`                                 | returns number as a `String`                                         |
| `toStringAsExponential(int? fractionDigits)` | returns `String` representation in exponential form                  |
| `toStringAsFixed(int fractionDigits)`        | returns `String` representation with fixed number of fraction digits |
| `toStringAsPrecision(int precision)`         | returns `String` representation with `precision` significant digits  |
| `truncate()`                                 | returns `int` result of truncating fractional digits                 |
| `truncateToDouble()`                         | same as `truncate`, but returns a `double`                           |

The `num` class defines the following static methods:

| Method                                              | Description                                          |
| --------------------------------------------------- | ---------------------------------------------------- |
| `parse(String input, [num onError(String input)?])` | returns `num` obtained by parsing a `String`         |
| `tryParse(String input)`                            | same as `parse`, but returns `null` if parsing fails |

The `parse` method throws if parsing fails or calls `onError` if supplied.

The `num` class defines the following operators:

| Category                        | Operators                                                          |
| ------------------------------- | ------------------------------------------------------------------ |
| unary                           | `-`, `++` (increment by 1), `--` (decrement by 1)                  |
| binary arithmetic               | `+`, `-`, `*`, `/`, `~/` (truncating division), `%` (modulo)       |
| binary comparison               | `<`, `<=`, `==`, `!=`, `>`, `>=`                                   |
| shorthand arithmetic assignment | `+=`, `-=`, `*=`, `/=`, `~/=` (truncating division), `%=` (modulo) |

The `++` and `--` operators can be placed on either side of a `num` value.
When on the left, a new value is computed before it is used.
When on the right, the value is used before a new value is computed.

Note that there is no `^` or `**` for exponentiation
like in many other programming languages.
Instead, import the `dart:math` package and
use the `pow(number, exponent)` function.

The fact that these operators take operands of type `num`
means that they can be applied to mixed operands
where one is an `int` and the other is a `double`.

Even though operator definitions look like methods,
they cannot be called like methods.
For example, the documentation for `+` operator in the `num` class
shows `num operator +(num other);`.

```dart
var n = 2;
n.+(3); // This does not work!
n += 3; // This works.
print(n);
```

Dividing by zero with the `/` operator results in
the `double.infinity` constant rather than throwing an exception.
When the integer division operator `~/` is used instead,
an `UnsupportedError` is thrown.
Note that `IntegerDivisionByZeroException` is deprecated.

#### int Class

The `int` class adds the following properties
to those defined in the `num` class:

| Property    | Description                              |
| ----------- | ---------------------------------------- |
| `bitLength` | minimum number of bits required to store |
| `isEven`    | `bool` indicating if even                |
| `isOdd`     | `bool` indicating if odd                 |

The `int` class adds the following methods
to those defined in the `num` class (some omitted):

| Method                     | Description                                                 |
| -------------------------- | ----------------------------------------------------------- |
| `gcd(int other)`           | returns greatest common divisor                             |
| `toRadixString(int radix)` | returns `String` representation of number with a given base |
| `toSigned(int width)`      | returns least significant bits, retaining sign bit          |
| `toUnsigned(int width)`    | returns least significant bits, not retaining sign bit      |

The `toRadixString` method can be used to convert decimal values to hexadecimal.
For example, `255.toRadixString(16)` returns the `String` `ff`.

The `int` class adds the following operators
to those defined in the `num` class:

| Category                       | Operators                                                                             |
| ------------------------------ | ------------------------------------------------------------------------------------- |
| bitwise                        | `&` (and), `\|`(or), `^`(xor), `~` (complement)                                       |
| bit shift                      | `<<` (signed shift left), `>>` (signed shift right), and `>>>` (unsigned shift right) |
| shorthand bitwise assignment   | `&=` (and), `\|=`(or),`^=` (xor)                                                      |
| shorthand bit shift assignment | `<<=`, `>>-`, and `>>>==`                                                             |

Note that there is no operator for unsigned bit shift left.

#### double Class

The `double` class adds no properties, instance methods,
static methods, or operators beyond those defined in the `num` class.

The `double` class defines the following constants
that must be prefixed with `double.`:

| Constant           | Description                                        |
| ------------------ | -------------------------------------------------- |
| `infinity`         | represents an infinite positive number             |
| `maxFinite`        | largest positive floating point number in 64 bits  |
| `minPositive`      | smallest positive floating point number in 64 bits |
| `nan`              | represents a value that is not a valid number      |
| `negativeInfinity` | represents an infinite negative number             |

#### `math` Package

The Dart `math` package does not need to be installed,
but it must be imported in order to use the items it defines.

```dart
import 'dart:math';
```

The Dart `math` package class defines the following classes:

| Class                             | Description                                                              |
| --------------------------------- | ------------------------------------------------------------------------ |
| `MutableRectangle<T extends num>` | represents a mutable rectangle that is axis-aligned (not rotated)        |
| `Point<T extends num>`            | represents an immutable 2D point                                         |
| `Random`                          | provides methods that generate random `bool`, `int`, and `double` values |
| `Rectangle<T extends num>`        | represents an immutable rectangle that is axis-aligned (not rotated)     |

The Dart `math` package class defines the following constants:

| Constant  | Description                          |
| --------- | ------------------------------------ |
| `e`       | base of natural logarithms; 2.718... |
| `ln2`     | natural log of 2; 0.693...           |
| `ln10`    | natural log of 10; 2.30...           |
| `log2e`   | base 2 log of e; 1.44...             |
| `log10e`  | base 10 log of e; 0.434...           |
| `pi`      | PI; 3.14...                          |
| `sqrt1_2` | square root of 1/2; 0.707...         |
| `sqrt2`   | square root of 2; 1.41...            |

The Dart `math` package defines the following functions:

| Function                           | Description                     |
| ---------------------------------- | ------------------------------- |
| `acos(num x)`                      | returns arc cosine of x         |
| `asin(num x)`                      | returns arc sine of x           |
| `atan(num x)`                      | returns arc tangent of x        |
| `atan2(num x, num y)`              | returns arc tangent of y/x      |
| `cos(num radians)`                 | returns cosine of radians       |
| `exp(num x)`                       | returns e to the power x        |
| `log(num x)`                       | returns log base e of x         |
| `max<T extends num>(num T, num T)` | returns larger of two numbers   |
| `min<T extends num>(num T, num T)` | returns smaller of two numbers  |
| `pow(num x, num exponent)`         | returns x to the power exponent |
| `sin(num radians)`                 | returns sine of radians         |
| `sqrt(num x)`                      | returns square root of x        |
| `tan(num radians)`                 | returns tangent of radians      |

### String type

Instances of the `String` class hold an immutable sequence of UTF-16 characters.

TODO: Why does the name of this type start uppercase,
TODO: but the `bool`, `int`, and `double` types start lowercase?

Literal single line strings are delimited by single or double quotes.
Literal multi-line strings are delimited
by a pair of three single or double quotes.

Literal strings can use interpolations.
To include the value of a variable, include `$variableName`.
To include the value of an expression, include `${expression}`.

Strings can be concatenated with the `+` operator.
The `+` operator is not needed to concatenate literal strings.
Literal strings can be written next to each other,
separated only by a space, to concatenate them.

An individual character (code unit) can be
accessed with square brackets and an index.
For example, the first character of a string `s` is obtained with `s[0]`.

There are two ways to embed newline characters in a string.
The following code demonstrates these:

```dart
  var s1 = 'first\nsecond';
  print(s1);
  var s2 = '''first
second''';
  print(s2);
```

Raw strings are strings where backslash character is not treated specially.
They are indicated by preceding the opening delimiter with "r".
For example, `r'first\nsecond'` does not treat `\n` as a newline character.
Raw strings useful when defining regular expressions.

Defining custom classes that extend the `String` class is not allowed.

The `String` class defines the following properties:

| Property     | Description                                  |
| ------------ | -------------------------------------------- |
| `codeUnits`  | a `List<int>` of the characters (code units) |
| `isEmpty`    | `bool` indicating if empty                   |
| `isNotEmpty` | `bool` indicating if not empty               |
| `length`     | number of characters (code units)            |
| `runes`      | `Iterable` over the characters (code units)  |

The `String` class defines the following instance methods:

| Method                                                                                | Description                                                                                          |
| ------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `allMatches(String s, [int start = 0])`                                               | returns `Iterable<Match>` over matching substrings                                                   |
| `codeUnitAt(int index)`                                                               | returns code unit at a given index; same as `[index]`                                                |
| `compareTo(String other)`                                                             | returns comparator value typically used for sorting                                                  |
| `contains(Pattern other, [int start = 0])`                                            | returns `bool` indicating if a pattern is contained                                                  |
| `endsWith(String other)`                                                              | returns `bool` indicating if ends with another `String`                                              |
| `indexOf(Pattern other, [int start = 0])`                                             | returns `int` index where a `Pattern` is first found                                                 |
| `lastIndexOf(Pattern other, [int start = 0])`                                         | returns `int` index where a `Pattern` is last found                                                  |
| `matchAsPrefix(String other, [int start = 0])`                                        | returns first `Match` or null of a `String`                                                          |
| `padLeft(int width, [String padding = ' '])`                                          | returns new `String` padded on left to a given width                                                 |
| `padRight(int width, [String padding = ' '])`                                         | returns new `String` padded on right to a given width                                                |
| `replaceAll(Pattern from, String replace)`                                            | returns new `String` where all `Pattern` matches are replaced by a `String`                          |
| `replaceAllMapped(Pattern from, String replace(Match match), [int start = 0])`        | returns new `String` where all `Pattern` matches are replaced by a computed value                    |
| `replaceFirst(Pattern from, String to, [int start = 0])`                              | returns new `String` where first `Pattern` match is replaced by a `String`                           |
| `replaceFirstMapped(Pattern from, String replace(Match match), [int start = 0])`      | returns new `String` where first `Pattern` match is replaced by a computed value                     |
| `replaceRange(int start, int? end, String replacement)`                               | returns new `String` where a substring range is replaced by a `String`                               |
| `split(Pattern pattern)`                                                              | returns a `List<String>` of substrings obtained by splitting on a `Pattern`                          |
| `splitMapJoin(Pattern pattern, {String onMatch(Match)?, String onNonMatch(String)?})` | returns new `String` formed by splitting on a `Pattern`, converting parts, and concatenating results |
| `startsWith(Pattern pattern, [int index = 0])`                                        | returns `bool` indicating if starts with another `String`                                            |
| `substring(int start, [int? end])`                                                    | returns new `String` that is a substring defined by indexes                                          |
| `toLowerCase()`                                                                       | returns new `String` where all characters are converted to lowercase                                 |
| `toUpperCase()`                                                                       | returns new `String` where all characters are convert to uppercase                                   |
| `trim()`                                                                              | returns new `String` formed by removing leading and trailing whitespace                              |
| `trimLeft()`                                                                          | returns new `String` formed by removing leading whitespace                                           |
| `trimRight()`                                                                         | returns new `String` formed by removing trailing whitespace                                          |

The `String` class defines the following binary operators:
`+` (concatenation), `*` (repeated n times),
`==` (same code units), `[index]` (gets code unit at index).
For example `'ho ' * 3` creates the `String` `'ho ho ho '`.

The table below summarized converting between numbers and strings.

| Conversion           | Code              |
| -------------------- | ----------------- |
| `int` to `String`    | `i.toString()`    |
| `double` to `String` | `d.toString()`    |
| `String` to `int`    | `int.parse(s)`    |
| `String` to `double` | `double.parse(s)` |

### Regular Expressions

A `Pattern` is a `RegExp` or `String` object.

Dart regular expressions use the same syntax as JavaScript regular expressions.

A `RegExp` object is created with `RegExp(r'reg-ex-here');`.
Recall that placing "r" before a literal string creates a "raw string"
that doesn't treat the `\` character specially.

The `RegExp` class defines the following properties:

| Property          | Description                                                                           |
| ----------------- | ------------------------------------------------------------------------------------- |
| `isCaseSensitive` | `bool` indicating if matches are case-sensitive (defaults to `true`)                  |
| `isDotAll`        | `bool` indicating if periods should match line terminators (defaults to `false`)      |
| `isMultiline`     | `bool` indicating if multiline matching will be performed (defaults to `false`)       |
| `isUnicode`       | `bool` indicating if whether Unicode matching will be performed (defaults to `false`) |
| `pattern`         | regular expression as a `String`                                                      |

The `RegExp` class defines the following instance methods:

| Method                                         | Description                                                                            |
| ---------------------------------------------- | -------------------------------------------------------------------------------------- |
| `allMatches(String input, [int start = 0])`    | returns `Iterable<RegExpMatch>` for iterating over matches                             |
| `firstMatch(String input)`                     | returns `RegExpMatch?` for first match found or null                                   |
| `hasMatch(String input)`                       | returns `bool` indicating if a match was found                                         |
| `matchAsPrefix(String input, [int start = 0])` | returns a `Match?` that is null unless a match is found at the start                   |
| `stringMatch(String input)`                    | returns a `String?` substring of first match in input or null (ignores capture groups) |

The following example creates a regular expression that
matches strings starting with "The " and ending with " win.".
It captures all the characters in between.

```dart
var re = RegExp(r'^The (.+) win\.$');
var s = 'The St. Louis Blues win.';
var match = re.firstMatch(s);
if (match != null) {
  print(match.group(1)); // St. Louis Blues
}
```

The optional named constructor parameter `multiLine`
has a `bool` value that indicates whether it should
match at the beginning and end of every line.
This defaults to `false`.

The optional named constructor parameter `caseSensitive`
has a `bool` value that indicates whether matching should be case-sensitive.
This defaults to `true`.

The `RegExpMatch` class extends the `Match` class.
Instances of this class describe a regular expression matching result.

The `RegExpMatch` class defines the following properties:

| Property     | Description                                        |
| ------------ | -------------------------------------------------- |
| `end`        | index where the match ends plus 1                  |
| `groupCount` | number of captured groups                          |
| `input`      | entire string from which the match was found       |
| `pattern`    | a `Pattern` object describing the matching pattern |
| `start`      | index where the match begins                       |

The `RegExpMatch` class defines the following instance methods:

| Method                           | Description                                                |
| -------------------------------- | ---------------------------------------------------------- |
| `group(int groupIndex)`          | `String` matched at a given group index                    |
| `groups(List<int> groupIndices)` | List of `String` values matched at specified group indices |

The `[index]` operator can be used in place of
the `group` method retrieve the same value.

### DateTime class

The `DateTime` class is used to create objects
that represent an instant in time.
The constructor requires and `int` value for the year
and optionally accepts `int` values for the
month, day, hour, minute, second, and milliseconds.
The month and day default to one.
The minute, second, and milliseconds default to zero.
Other ways to create `DateTime` objects include
the static `parse` method and the constructors
`DateTime.fromMillisecondsSinceEpoch`, `DateTime.now`, and `DateTime.utc`.

The following code demonstrates various ways to create `DateTime` objects.

```dart
var birthday = DateTime(1961, 4, 16); // in local time
var birthdayUtc = DateTime.utc(1961, 4, 16); // in UTC
var nowLocal = DateTime.now(); // in local time

// Defaults to local time, but can request UTC.
var epoch = DateTime.fromMillisecondsSinceEpoch(0, isUtc: true);
print(epoch); // 1970-01-01 00:00:00.000Z
```

There isn't an easy way to create `DateTime` objects
for timezones other than the local one and UTC.

The `DateTime.parse` constructor takes a string
that matches a subset of ISO 8601 of the standard.
Examples include `'1961-04-16 10:19:00'` (local time zone),
`'19610416T101900'` (same with T separating date from time),
and `'1961-04-16 10:19:00Z'` (UTC).

The `DateTime` class defines the following constants:

- `DAYS_PER_WEEK` (7)
- `MONTHS_PER_YEAR` (12)

- `JANUARY` (1)
- `FEBRUARY` (2)
- `MARCH` (2)
- `APRIL` (4)
- `MAY` (5)
- `JUNE` (6)
- `JULY` (7)
- `AUGUST` (8)
- `SEPTEMBER` (9)
- `OCTOBER` (10)
- `NOVEMBER` (11)
- `DECEMBER` (12)

- `MONDAY` (1)
- `TUESDAY` (2)
- `WEDNESDAY` (3)
- `THURSDAY` (4)
- `FRIDAY` (5)
- `SATURDAY` (6)
- `SUNDAY` (7)

The `DateTime` class defines the following properties:

| Property                 | Description                               |
| ------------------------ | ----------------------------------------- |
| `day`                    | day of month; 1 to 31                     |
| `hour`                   | 0 to 23                                   |
| `isUtc`                  | `bool`                                    |
| `millisecond`            | 0 to 999                                  |
| `millisecondsSinceEpoch` | milliseconds since 1970-01-01T00:00:00Z   |
| `minute`                 | 0 to 59                                   |
| `month`                  | 1 to 12                                   |
| `second`                 | 0 to 59                                   |
| `timeZoneName`           | timezone abbreviation such as CST         |
| `timeZoneOffset`         | difference from UTC (ex. -6:00:00.00-0.0) |
| `weekday`                | MONDAY (1) to SUNDAY (7)                  |
| `year`                   | includes all digits (4 for current year)  |

The `DateTime` class defines the following instance methods:

| Method                             | Description                                                                               |
| ---------------------------------- | ----------------------------------------------------------------------------------------- |
| `add(Duration duration)`           | returns new `DateTime` with duration added                                                |
| `compareTo(DateTime other)`        | returns comparator value, often used for sorting                                          |
| `difference(DateTime other)`       | returns `Duration` between receiver and other                                             |
| `isAfter(DateTime other)`          | returns `bool` indicating if receiver is after other                                      |
| `isAtSameMomentAs(DateTime other)` | returns `bool` indicating if receiver is at same moment as other, regardless of time zone |
| `isBefore()`                       | returns `bool` indicating if receiver is before other                                     |
| `subtract()`                       | returns new `DateTime` with duration subtracted                                           |
| `toIso8601String()`                | returns `String` in format 'yyyy-MM-ddTHH:mm:ss.sssZ, omitting Z if not UTC               |
| `toLocal()`                        | returns `DateTime` converted to local                                                     |
| `toString()`                       | returns `String` in human-readable format                                                 |
| `toUtc()`                          | returns `DateTime` converted to UTC                                                       |

### Duration class

The `Duration` class is used to create objects that represent a span to time.
The constructor takes the following named parameters
that are all optional and default to zero:
`days`, `hours`, `minutes`, `seconds`, `milliseconds`, and `microseconds`.

The `Duration` class defines the following constants:

- `HOURS_PER_DAY`
- `MICROSECONDS_PER_DAY`
- `MICROSECONDS_PER_HOUR`
- `MICROSECONDS_PER_MILLISECOND`
- `MICROSECONDS_PER_MINUTE`
- `MICROSECONDS_PER_SECOND`
- `MILLISECONDS_PER_DAY`
- `MILLISECONDS_PER_HOUR`
- `MILLISECONDS_PER_MINUTE`
- `MILLISECONDS_PER_SECOND`
- `MINUTES_PER_DAY`
- `MINUTES_PER_HOUR`
- `SECONDS_PER_DAY`
- `SECONDS_PER_HOUR`
- `SECONDS_PER_MINUTE`
- `ZERO`

The `Duration` class defines the following properties:

| Property         | Description                        |
| ---------------- | ---------------------------------- |
| `inDays`         | `int` number of whole days         |
| `inHours`        | `int` number of whole hours        |
| `inMicroseconds` | `int` number of whole microseconds |
| `inMilliseconds` | `int` number of whole milliseconds |
| `inMinutes`      | `int` number of whole minutes      |
| `inSeconds`      | `int` number of whole seconds      |
| `isNegative`     | `bool` indicating if negative      |

The `Duration` class defines the following instance methods:

| Method                      | Description                                                       |
| --------------------------- | ----------------------------------------------------------------- |
| `abs()`                     | returns new `Duration` that is the absolute value of the receiver |
| `compareTo(Duration other)` | returns comparator value, often used for sorting                  |
| `toString()`                | returns `String` representation                                   |

### Type Casts

The `as` keyword casts a value of one type to another.
For example, if a variable of type `Object` currently holds a `String` value,
it can be cast to a `String` so that
methods from that class can be called on the value.

```dart
Object obj = 'test';
print((obj as String).length);
```

This throws if the cast is not valid.

```dart
Object obj = 7;
print((obj as String).length); // throws "Script error."
```

TODO: Add more detail on this?

### Timer class

The `Timer` class executes a callback function after a given `Duration`
either once or repeatedly.
It provides capabilities similar to the JavaScript functions
`setTimeout` and `setInterval`.
The `Timer` class is defined in the `dart:async` package which must be imported.

The following example demonstrates creating `Timers`
that fire just once and repeatedly:

```dart
import 'dart:async';

void main() {
  print('starting timer');
  var timer = Timer(Duration(seconds: 2), () {
    print('finished timer');
  });
  //timer.cancel();

  var count = 0;
  Timer.periodic(Duration(seconds: 1), (timer) {
    print('isActive = ${timer.isActive}; tick = ${timer.tick}');
    count++;
    if (count >= 5) timer.cancel();
  });
}
```

### Generic Types

Generics, a.k.a parameterized types,
allow writing functions, classes, and methods
whose functionality differs based on the types of values
passed when they are called.
Using generics often reduces code duplication.

The collection classes, described next, make use of this.
For example, when creating a `List`, the type of
the elements it can hold are specific using generics.
The following are equivalent:

```dart
List<int> numbers = [1, 2, 3]; // type is specified on variable
var numbers = <int>[1, 2, 3]; // type is specified on value
var numbers = [1, 2, 3]; // type is inferred
```

To create a collection that can hold values of any type, use `dynamic`.
For example:

```dart
List<dynamic> anyList = [
  null, true, 7, 3.14, 'hello', <String>{'red', 'green', 'blue'}
];

for (var value in anyList) {
  print(value.runtimeType);
}
```

Typically parameterized types have single-letter, uppercase names like `T`.
Any number of these can be specified inside angle brackets.
They can be constrained to only types that extend a given class
using the `extends` keyword,
or left unconstrained in which case values of any type can be used.

The following example demonstrates implementing a generic function.
It takes any `Iterable` containing `num` values and returns its sum.

```dart
N sum<N extends num>(Iterable<N> numbers) {
  return numbers.reduce((acc, n) => (acc + n) as N);
}

void main() {
  print(sum([1, 2, 3])); // List; 6
  print(sum({1, 2, 3})); // Set; 6
}
```

## Collection Types

Dart provides many collection classes.
Built-in collection classes that can be used without importing
include `List`, `Set`, and `Map`.
Other collection classes are defined in the package `dart:collection`
and must be imported. These include `DoubleLinkedQueue`, `HashMap`, `HashSet`,
`LinkedHashMap`, `LinkedHashSet`, `LinkedList`, `ListQueue`, `Queue`,
and `SplayTreeMap`, `SplayTreeSet`.

### Iterable

The following collection classes all have `Iterable` as a superclass:
`DoubleLinkedQueue`, `IterableBase`, `IterableMixin`,
`LinkedList`, `List`, `ListQueue`, `Queue`, `Runes`, and `Set`.

Any `Iterable` collection can be used in a `for-in` loop
to iterate over its elements.

The `Iterable` class provides the following properties:

| Property     | Description                                         |
| ------------ | --------------------------------------------------- |
| `first`      | first element                                       |
| `hashCode`   | hash code                                           |
| `isEmpty`    | `bool` indicating if there are no elements          |
| `isNotEmpty` | `bool` indicating if there is at least one element  |
| `iterator`   | for iterating over elements                         |
| `last`       | last element                                        |
| `length`     | number of elements                                  |
| `single`     | if only one element, that element; otherwise throws |

The `Iterable` class provides the following methods (some omitted):

| Method                                                           | Description                                                                        |
| ---------------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| `any(bool test(E element))`                                      | returns a `bool` indicating if any element passes the test                         |
| `contains(Object? element)`                                      | returns a `bool` indicating if a given element is present                          |
| `elementAt(int index)`                                           | returns the element at a given index or throws `RangeError` if not found           |
| `expand<T>(Iterable<T> toElements(E element))`                   | returns an `Iterable` over the flatten elements                                    |
| `every(bool test(E element))`                                    | returns a `bool` indicating if every element passes the test                       |
| `firstWhere(bool test(E element), {E orElse()?})`                | returns the first element that passes the test or the `orElse` value               |
| `fold<T>(T initialValue, T combine(T previousValue, E element))` | reduces a collection to a single value                                             |
| `forEach(void action(E element))`                                | invokes `action` on each element                                                   |
| `join([String separator = ""])`                                  | returns a string formed by concatenating the string representation of each element |
| `lastWhere(bool test(E element), {E orElse()?})`                 | returns the last element that passes the test or the `orElse` value                |
| `map<T>(T toElement(E e))`                                       | returns a new collection of the results of calling a function on each element      |
| `reduce(E combine(E value, E element))`                          | same as `fold` but uses the first element as the initial value                     |
| `singleWhere(bool test(E element), {E orElse()?})`               | similar to `firstWhere`, but throws if more than one element passes the test       |
| `skip(int count)`                                                | returns an `Iterable` that begins after `count` elements                           |
| `skipWhile(bool test(E value))`                                  | returns an `Iterable` that begins after the initial elements that pass the test    |
| `take(int count)`                                                | returns an `Iterable` over the first `count` elements (opposite of `skip`)         |
| `takeWhile(bool test(E value))`                                  | returns an `Iterable` that ends after the initial elements that pass the test      |
| `toList(bool growable = true)`                                   | creates and returns a `List` containing the same elements                          |
| `toSet()`                                                        | creates and returns a `Set` containing the same elements                           |
| `toString()`                                                     | returns the `String` representation                                                |
| `where(bool test(E element))`                                    | returns an `Iterable` over all the elements that pass the test                     |
| `whereType<T>()`                                                 | returns a new collection of elements with a given type                             |

The differences between the `List` methods `fold` and `map` is that
`fold` takes an initial value and `map` does not.
In the first iteration of the `fold` method,
the combine function is called with `initialValue` and the first element.
In the first iteration of the `reduce` method,
the combine function is called with the first and second elements.

### Lists

Arrays in Dart are represented by `List` objects.
A literal array is written as a
comma-separated list of values surrounded by square brackets.
For example, `var numbers = [3, 7, 19];`

There are three ways to declare and initialize a variable that holds a `List`.

```dart
// type List<int> is inferred
var l1 = [1, 2, 3];

// type List<int> is specified on the value
var l1 = <int>[1, 2, 3];

// type List<int> is specified on the variable
List<int> l1 = [1, 2, 3];
```

To iterate over the elements of a `List`,
use a `for/in` loop or the `forEach` method.
For example:

```dart
const dogs = ['Maisey', 'Ramsay', 'Oscar', 'Comet'];

for (var dog in dogs) {
  print(dog);
}

// Same as above.
dogs.forEach((dog) => print(dog));

// Same as above.
dogs.forEach(print);
```

Literal lists can include logic to determine the elements,
referred to as "list comprehension".
The following examples demonstrate this:

```dart
import 'dart:math';

// Creates a List of int values from start to end - 1
// using the List.generate constructor.
Iterable<int> range(int start, int end) =>
    List.generate(end - start, (i) => start + i);

void main() {
  // Get the squares of a odd numbers in the range [0, 10).
  var squares = [for (var i = 1; i < 10; i += 2) pow(i, 2)];

  // Same using a different approach.
  // Get the squares of a odd numbers in the range [0, 10).
  squares = [for (var i in range(0, 10)) if (i % 2 == 1) pow(i, 2)];
  print(squares); // [1, 9, 25, 49, 81]

  // Find all the [x, y, z] values where
  // x^2 + y^2 = z^2 up to a maximum value of 20.
  var pythagorean = [
    for (var x in range(1, 20))
      for (var y in range(x, 20)) // start at x
        for (var z in range(y, 20)) // start at y
          if (x * x + y * y == z * z) [x, y, z]
  ];
  print(pythagorean); // [[3, 4, 5], [5, 12, 13], [6, 8, 10], [8, 15, 17], [9, 12, 15]]
}
```

The `List` class is generic and implements extends from the `Iterable` class.
It provides many constructors that support creating lists that are
empty,
filled to a given length with the same value,
filled from another `Iterable` (either modifiable or unmodifiable),
filled by a generator function,

In addition to the properties provided by the `Iterable` class,
`List` objects support the following properties:

| Property   | Description                                      |
| ---------- | ------------------------------------------------ |
| `reversed` | an `Iterable` over the elements in reverse order |

In addition to the methods provided by the `Iterable` class,
`List` objects support the following methods (some omitted):

| Method                                                       | Description                                                    |
| ------------------------------------------------------------ | -------------------------------------------------------------- |
| `add(E value)`                                               | adds an element                                                |
| `addAll(Iterable<E> iterable)`                               | adds all the elements in another collection                    |
| `clear()`                                                    | removes all the elements                                       |
| `fillRange(int start, int end, [E? fillValue])`              | sets the elements in a range to a given value or null          |
| `getRange(int start, int end)`                               | returns an `Iterable` over a range of elements                 |
| `indexOf(E element, [int start = 0])`                        | return the first index of a given element                      |
| `indexWhere(bool test(E element), [int start = 0])`          | returns the index of the first element that passes the test    |
| `insert(int index, E element)`                               | inserts an element at a given index                            |
| `insertAll(int index, Iterable<E> iterable)`                 | inserts all the elements in an `Iterable` at a given index     |
| `lastIndexOf(E element, [int? start])`                       | returns the last index of a given element                      |
| `lastIndexWhere(bool test(E element), [int? start])`         | returns the index of the last element that passes the test     |
| `remove(Object? value)`                                      | removes the first occurrence of an element                     |
| `removeAt(int index)`                                        | removes the element at a given index                           |
| `removeLast()`                                               | removes the last element; use `removeAt(0)` to remove first    |
| `removeRange(int start, int end)`                            | removes the elements in a given range                          |
| `removeWhere(bool test(E element))`                          | removes all elements that pass a test                          |
| `replaceRange(int start, int end, Iterable<E> replacements)` | replaces elements in a given range with those in an `Iterable` |
| `retainWhere(bool test(E element))`                          | removes all elements that do not pass a test                   |
| `shuffle([Random? random])`                                  | randomly reorders the elements in place                        |
| `sort([int compare(E a, E b)?])`                             | sorts the elements in place using a `Comparator` function      |
| `sublist(int start, [int? end])`                             | returns a new `List` that is a subset                          |

### Sets

A `Set` is an unordered collection of unique values.
A literal set is written as a
comma-separated list of values surrounded by curly braces.

There are three ways to declare and initialize a variable that holds a `Set`.

```dart
// type Set<String> is inferred
var colors = {'red', 'green', 'blue'};

// type Set<String> is specified on the value
var colors = <String>{'red', 'green', 'blue'};

// type Set<String> is specified on the variable
Set<String> colors = {'red', 'green', 'blue'};
```

The literal syntax `<value-type>{}` creates an empty `Set`.
The literal syntax `<key-type, value-type>{}` creates an empty `Map`.
The literal syntax `{}` creates an empty `Map` with unspecified
key and value types whose types must be inferred from the variable type.
It does not create an empty `Set`.

No properties are added beyond those provided by the `Iterable` class.

In addition to the methods provided by the `Iterable` class,
`Set` objects support the following methods (some omitted):

| Method                                  | Description                                                                  |
| --------------------------------------- | ---------------------------------------------------------------------------- |
| `add(E value)`                          | adds an element                                                              |
| `addAll(Iterable<E> iterable)`          | adds all the elements in another collection                                  |
| `clear()`                               | removes all the elements                                                     |
| `contains(Object? value)`               | returns a `bool` indicating if an element is present                         |
| `containsAll(Iterable<Object>? other)`  | returns a `bool` indicating if all the elements in an `Iterable` are present |
| `difference(Set<Object?> other)`        | returns a new `Set` containing all elements in this one not found in `other` |
| `lookup(Object? object)`                | returns `object` if found in the `Set` or `null`                             |
| `remove(Object? value)`                 | removes the first occurrence of an element                                   |
| `removeAll(Iterable<Object?> elements)` | removes all the elements in an `Iterable`                                    |
| `removeWhere(bool test(E element))`     | removes all elements that pass a test                                        |
| `retainAll(Iterable<Object?> elements)` | removes all the elements not in an `Iterable`                                |
| `retainWhere(bool test(E element))`     | removes all elements that do not pass a test                                 |
| `union(Set<E> other)`                   | returns a new `Set` containing all elements in this one and `other`          |

### Maps

A `Map` is a collection of key/value pairs.
The keys and values can have any type.
Unlike `List` and `Set`, this class is not a subclass of `Iterable`.

A literal map is written as a
comma-separated list of pairs surrounded by curly braces.
Each pair is written as a key followed by a colon and a value.
When a key is a string, it must be delimited by single or double quotes.

There are three ways to declare and initialize a variable that holds a `Map`.

```dart
// type Map<String, int> is inferred
var colorMap = {'red': 1, 'green': 2, 'blue': 3};

// type Map<String, int> is specified on the value
var colorMap = <String, int>{'red': 1, 'green': 2, 'blue': 3};

// type Map<String, int> is specified on the variable
Map<String, int> colorMap = {'red': 1, 'green': 2, 'blue': 3};
```

The `Map` class provides the following properties:

| Property     | Description                                                            |
| ------------ | ---------------------------------------------------------------------- |
| `entries`    | `Iterable` over `MapEntry` objects (have `key` and `value` properties) |
| `isEmpty`    | `bool` indicating if there are no key/value pairs                      |
| `isNotEmpty` | `bool` indicating if there is at least one key/value pair              |
| `keys`       | `Iterable` over keys                                                   |
| `length`     | number of key/value pairs                                              |
| `values`     | `Iterable` over values                                                 |

The `Map` class provides the following methods (some omitted):

| Method                                                  | Description                                                              |
| ------------------------------------------------------- | ------------------------------------------------------------------------ |
| `addAll(Map<K, V> other)`                               | adds all key/value pairs in another `Map` to this one                    |
| `addEntries(Iterable<MapEntry<K, V>> newEntries)`       | adds all `MapEntry` objects in an `Iterable`                             |
| `clear()`                                               | removes all key/value pairs                                              |
| `containsKey(Object? key)`                              | returns a `bool` indicating if a given key is present                    |
| `containsValue(Object? value)`                          | returns a `bool` indicating if a given value is present                  |
| `forEach(void action(K key, V value)`                   | executes a function on each key/value pair                               |
| `map<K2, V2>(MapEntry<K2, V2> convert(K key, V value))` | returns a new `Map` created by calling a function on each key/value pair |
| `putIfAbsent(K key, V ifAbsent()`                       | returns the value for a given key and adds a value if not present        |
| `remove(Object? key)`                                   | removes a key/value pair if present                                      |
| `removeWhere(bool test(K key, V value)`                 | removes all key/value pairs that pass a test                             |
| `update(K key, V update(V value)`                       | updates the value for a given key to the value returned by a function    |
| `updateAll(V update(K key, V value)`                    | updates all values to the value returned by a function                   |

The `MapEntry` class represents a single key/value pair from a `Map`.
To create one, call the `MapEntry` constructor passing it a key and a value.
These objects have `key` and `value` properties and a `toString` method.

## Print

The `print` function takes a single argument and writes it to stdout.

If the argument is not a `String`, it will be converted
to a `String` by calling the `toString` method of the value.
For example:

```dart
class Dog {
  String name;
  String breed;

  // Shorthand constructor - see Class section
  Dog(this.name, this.breed);

  @override
  String toString() => '$name is a $breed.';
}

void main() {
  var d = Dog('Comet', 'Whippet');
  print(d); // Comet is a Whippet.
}
```

### Spread Operators

The spread operator `...` spreads a collection inside another.

```dart
var colors = ['red', 'green', 'blue'];
var moreColors = ['black', ...colors, 'white'];
print(moreColors);

var fruits = {'b': 'banana', 'c': 'cherry'};
var moreFruits = {'a': 'apple', ...fruits, 'd': 'date'};
print(moreFruits);
```

The null-aware spread operator `...?` only spreads when the value is not null.

```dart
Map<String, String>? maybeFruits;
var evenMoreFruits = {'a': 'apple', ...fruits, ...?maybeFruits, 'd': 'date'};
print(evenMoreFruits);
```

The spread operator cannot be used to expand a `List` into function arguments.
Function.apply(add, numbers)

## Nullable Values

Dart provides special operators for dealing with nullable values.
The following example demonstrates these.

```dart
import 'dart:math';

var random = Random();
String? randomString() => random.nextBool() ? 'test' : null;

void main() {
  String? s;
  s = randomString(); // 'test' or null
  print('s = $s');

  // The ?. operator evaluates to null if the value on the left is null.
  // Otherwise it evaluates to the property or method on the right.
  print(s?.toUpperCase());

  // The ?? operator provides an alternative value to use
  // if the value on the left is null.
  print(s ?? 'missing value');

  // The ??= operator assigns a value to a variable
  // only if the variable value is currently null.
  s ??= 'supplied value';
  print(s);

  // The !. operator asserts that that vale on the left is not null.
  // The program will crash with "Script error." if it is null.
  print(s!.toUpperCase());
}
```

## Functions

Dart functions are represented by objects from the `Function` class.
They are first class which means they can be assigned variables,
passed to other functions, and returned from other functions.

Named function definitions have the following syntax:

```dart
return-type fn-name(parameter-list) {
  statements
}
```

Anonymous functions have similar syntax, but the name is omitted
and the return type is inferred.

```dart
(parameter-list {
   statements
}
```

Functions that only return the value of a single expression
can use a shorthand syntax where
`=> expression;` is short for `{ return expression; }`.

Specifying parameter types and the return type are optional.
For example, the following is a valid function definition:

```dart
add(n1, n2) => n1 + n2;
main() => print(add(2, 3)); // 5
```

The spread operator cannot be used to expand a `List` into function arguments.
The static `apply` method on the `Function` class can be used for this purpose.
For example:

```dart
var numbers = [2, 3];
print(Function.apply(add, numbers)); // 5
```

Functions can take both positional and named arguments.
All positional arguments must preceded the named ones.

Positional arguments can be required or optional.
All required positional arguments must appear before the optional ones.
Required parameters cannot be given default values.
All optional arguments must be inside square brackets.
They must either have a default value or an optional type (ending with `?`).

In the following example, `req1` and `req2` are required parameters
and `opt1` and `opt2` are optional.
If only two arguments are passed,
`opt1` is set to its default value of `0`
and `opt2` is set to `null`.

```dart
demo(int req1, int req2, [int opt1 = 0, int? opt2]) {
  ...
}
```

Named parameters are declared inside curly braces.
If they are optional, they must either have a default value
or an optional type (ending with `?`).
If they are required, the `required` keyword must appear before their type.
For example:

```dart
//int multiply(int n, {required int by}) => n * by;
int multiply(int n, {int by = 0}) => n * by;

main() {
  print(multiply(2, by: 3)); // 6
  print(multiply(4)); // 4
}
```

Anonymous function definitions are written like named function definitions,
but omit the name. For example:

```dart
var numbers = [3, 7, 9];
numbers.forEach((n) => n * 2);
```

Trailing commas are allowed after the last parameter in function definitions
and after the last argument in function calls.
This causes `dart format` to place each
parameter or argument on a separate line.

Functions that do not explicitly return a value evaluate to `null`.

## Conditional Logic

Dart supports two statements for implementing conditional logic,
`if` and `switch`.

The condition specified in an `if` statement must be
an expression that evaluates to a `bool` value.

Here are examples of `if` statements.

```dart
if (temperature < 30) print('stay inside'); // braces are optional

if (temperature > 80) {
    print('hot');
} else if (temperature < 40) {
    print('cold');
} else {
    print('comfortable');
}
```

The `switch` statement compares values of type `int` or `String`,
enumerated types, or compile-time constants.
Curly braces around the cases are required.
Here is an example.

```dart
enum Color { red, green, blue }
var color = Color.green;
switch (color) {
  case Color.red:
    print('hot');
    break;
  case Color.blue:
    print('cold');
    break;
  default:
    print('comfortable');
}
```

When an `enum` value is being evaluated, the `default` clause is required
unless there is a `case` for each possible value.

The `assert` statement asserts that a condition is true.
If it is not, an optional message is printed
and the program exits with an `AssertionError`.
These statements are only executed when enabled.
To run a Dart program with asserts enabled,
enter `dart run --enable-assert`.
For example:

```dart
assert(!worldEnding(), 'So long.');
```

## Iteration

Dart supports three statements for implementing iteration,
`for`, `while`, and `do-while`.
The condition specified in each of these must be
an expression that evaluates to a `bool` value.
The `for` and `while` loops are top-tested.
They perform a test at the beginning of each iteration
and so may not execute their bodies at all.
The `do-while` loop is bottom-tested.
It performs a test after each iteration
and so will always execute the body at least once

Here are examples of each:

```dart
for (var i = 1; i <= 5; i++) print(i); // 1 to 5; braces are optional

for (var i = 1; i <= 5; i++) {
  print(i); // 1 to 5
}

var dogs = ['Maisey', 'Ramsay', 'Oscar', 'Comet'];

for (var dog in dogs) print(dog); // each dog name; braces are optional

for (var dog in dogs) {
  print(dog); // each dog name
}

var i = 1;
while (i <= 5) {
  print(i); // 1 to 5
  i++;
}

do {
  print(i); // 6 to 1
  i--;
} while (i > 0);
```

All three kinds of loops support the `break` and `continue` statements.
The `break` statement exits the loop.
The `continue` statement skips the remainder of the current iteration
and proceeds to the beginning of the next iteration, if any.

## Exception Handling

To throw an exception, use the `throw` keyword followed by any object.
Typically the object is one that extends from `Exception` or `Error`,
but this is not required. For example, throwing a `String` is allowed.

For example:

```dart
throw FormatException('invalid phone number');
```

In general exception can be caught and handled,
but errors should result in the program terminating.

Builtin `Exception` subclasses include
`FormatException`, `IOException`, `TimeoutException`, and more.

Builtin `Error` subclasses include `ArgumentError`, `AssertionError`,
`OutOfMemoryError`, `TypeError`, `UnimplementedError`, and more.
Custom subclasses or `Exception` and `Error` can also be defined.

Custom exception classes can be created by defining and instantiating
classes that implement `Exception` or `Error`.
For example:

```dart
class RangeException implements Exception {
  String message;

  RangeException(this.message);

  @override
  String toString() => message;
}

class Range {
  num lower;
  num upper;

  Range({required this.lower, required this.upper});

  bool includes(num value) => lower <= value && value <= upper;

  void assertIncludes(num value) {
    if (value < lower) {
      throw RangeException('value $value is less than lower bound $lower');
    }
    if (value > upper) {
      throw RangeException('value $value is greater than upper bound $upper');
    }
  }
}
```

To catch an exception, use the `try`, `catch`, and `finally` keywords.
For example:

```dart
try {
  // code that can throw
} on FormatException {
  // catching a specific kind of exception,
  // but not using the exception object
} on IOException catch (e) {
  // catching a specific kind of exception,
  // and using the exception object
} catch (e) {
  // catching any kind of exception
  // and using the exception object
} finally {
  // code to run regardless of whether there was a thrown
}
```

The `Range` and `RangeException` classes defined above
can be used as follows:

```dart
var range = Range(lower: 1, upper: 10);
try {
  range.assertIncludes(3); // doesn't throw
  range.assertIncludes(0); // throws
} on RangeException catch (e) {
  print(e); // value 0 is less than lower bound 1
} catch (e) {
  print("something else went wrong");
}
```

## Access Specifiers

Dart does not support keywords to indicate access levels
of things like functions, classes, properties, and methods.
Instead add an underscore prefix to a name to indicate that it is private.
This is enforced at the library level.
Private names in a library are not accessible outside the library.
A compiler error is generated if such access is attempted.
However, private names within an application or library
are still accessible by code in the same application or library.

## Classes

Classes defined properties to hold data, constructors to create instances,
and methods to operate on the data.

Constructors are defined as methods with the same name as the class.
For example:

```dart
class Point {
  double x = 0;
  double y = 0;

  Point(double x, double y) {
    this.x = x;
    this.y = y;
  }

  // This is an alternate way to write the constructor above.
  //Point(this.x, this.y);
}
```

If a class doesn't define a constructor,
a no-arg constructor that doesn't initialize any properties is provided.
If the class has a superclass,
the default constructor calls its no-arg constructor.

Additional "named constructors" can be provided.
For example, the `Point` class above could have a
named constructor for initializing `x` and `y` to the same value.

```dart
  Point.same(double value) {
      this.x = value;
      this.y = value;
  }
```

Constructors can initialize properties before their body runs.
For example, the following is an alternate way
to write the previous named constructor.
This has no body, but a body could be added by replacing the closing semi-colon
with a block of code surrounded by curly braces.

```dart
  Point.same(double value): x = value, y = value;
```

Pulling all of this together we can write the following:

```dart
class Point {
  double x = 0;
  double y = 0;

  Point(this.x, this.y);

  Point.same(double value): x = value, y = value {
    print('I ran!');
  }

  @override
  String toString() {
    return '($x, $y}';
  }
}

void main() {
  var pt = Point(2, 3);
  print('pt = $pt'); // pt = (2.0, 3.0)
  pt = Point.same(4);
  print('pt = $pt'); // pt = (4.0, 4.0)
}
```

Here is one more example that demonstrates a constructor
that takes named parameters.

```dart
class Person {
  String name;
  int? _age;

  // The first parameter is required and the second is optional.
  Person({required this.name, int? age}) {
    _age = age;
  }
}

void main() {
  var p1 = Person(name: 'Mark', age: 60);
  print('p1.name = ${p1.name}'); // Mark
  print('p1._age = ${p1._age}'); // 60

  var p2 = Person(name: 'Tami');
  print('p2.name = ${p2.name}'); // Tami
  print('p2._age = ${p2._age}'); // null
}
```

TODO: Describe the `late` keyword.

Classes can be marked as `abstract` which prevents creating instances of them.
Abstract classes are useful for defining
functionality to be inherited by other classes.

Interface in other programming languages are collections of method signatures
that some classes implement.
Dart doesn't support defining "interfaces",
but `abstract` classes can be used for this purpose.
For example:

```dart
import 'dart:math';

abstract class Shape {
    // All classes that extend Shape must
    // implement a method with this signature.
    double getArea();
}

class Rectangle extends Shape {
    double height;
    double width;

    Rectangle({required this.width, required this.height});

    @override
    double getArea() => width * height;
}

class Circle extends Shape {
    double radius;

    Circle({required this.radius});

    @override
    double getArea() => pi * pow(radius, 2);
}

void main() {
  var r = Rectangle(width: 3, height: 4);
  var c = Circle(radius: 5);
  print(r.getArea()); // 12
  print(c.getArea()); // 78.5...
}
```

## Cascade Operator

The cascade operator (`..`) is used to
set multiple properties of an object or
call multiple methods on an object.
In the case of calling methods, it is useful when the methods
do not return the receiver, making method chaining impossible.

The following examples demonstrate the cascade operator:

```dart
var left = 20;
var top = 10;
var width = 100;
var height = 50;
var r = MutableRectangle(left, top, width, height);
print(r); // Rectangle (20, 10) 100 x 50

r
  ..left = 30
  ..top = 0
  ..width = 200
  .. height = 30;
print(r); // Rectangle (30, 0) 200 x 30

var numbers = [7, 3, 9, 2];
numbers
  ..sort() // [2, 3, 7, 9]
  ..add(10) // [2, 3, 7, 9, 10]
  ..insert(0, 1) // [1, 2, 3, 7, 9, 10]
  ..removeWhere((n) => n % 2 == 0); // [1, 3, 7, 9]
print(numbers); // [1, 3, 7, 9]
```

The null-aware cascade operator `?..`
evaluates to null if the left side is `null`,
avoiding calling a method on `null`.
It should only be used for the first cascade in a series of them.
The following example applies the null-aware cascade operator
to the result of calling the `getSquare` function which can return `null`:

```dart
import 'dart:math';

MutableRectangle? getSquare(double size) {
  if (size <= 0) return null;
  return MutableRectangle(0, 0, size, size);
}

void main() {
  var square = getSquare(5)
    ?..width *= 2 // double the width
    ..height *= 2; // double the height
  print(square); // Rectangle (0, 0) 10 x 10

  square = getSquare(0)
    ?..width *= 2 // won't run when square is null
    ..height *= 2; // won't run when square is null
  print(square); // null
}
```

## Mixins

TODO: Add this.

## Concurrency

See the `async` and `await` keywords.
Also, read about "Isolates".
TODO: Add more here.

## Tooling

### Code Formatting

Formatting of Dart code is provided by the {% aTargetBlank
"https://dart.dev/tools/dart-format", "dart format" %} command.

To get the best formatting, include a comma after every function argument,
even the last one. This places each argument on a separate line.

### Compiling

The `dart compile {format}` command compiles Dart code to other formats.
The `exe` format creates an executable program for the current platform
which can be Windows, macOS, or Linux.
The `js` format compiles Dart code to JavaScript
that includes an implementation of the Dart runtime.

### Testing

Dart tests using the `test` package.
To install this in a Dart project, enter `dart pub add test --dev`.

Here is an example of code to be tested in the file `lib/math.dart`
that is inside a project named `hello_world`.

```dart
double add(double n1, double n2) {
  return n1 + n2;
}
```

Test files should be placed in the `test` directory
and have names that end with `_test.dart`.
Here is an example of test code in the file `test/math_test.dart`.

```dart
import 'package:test/test.dart';
import 'package:demo/math.dart'; // demo is the project name

void main() {
  test('add works', () { // passing an anonymous function to test
    expect(add(2, 3), 5);
  });
}
```

The `expect` function takes an actual value and an expected value.
TODO: Are there other forms of calling this?

To run all the tests in a project, enter `dart test`.
This is a bit slow the first time it is run,
but subsequent runs are much faster.
If all of the tests pass, this will output "All tests passed!".
Otherwise it will output "Some tests failed."
along with expected and actual results.

To run tests from inside VS Code, click the beaker icon in the left nav
to display the test panel.
Then click one of the play buttons at the top (non-debug or debug).
To run a single test, hover over it in the test panel
to reveal play buttons and click one of them.

Tests can be grouped into suites sing the `group` function
which takes a name and a function that
calls the `test` and `group` functions.
For example:

```dart
import 'package:test/test.dart';
import 'dart:math';

void main() {
  group('math', () {
    group('trigonometry', () {
      test('sin works', () {
        expect(sin(pi / 2), 1);
      });
    });
  });
}
```

## Annoyances

- Dart wants many constructor calls to be preceded by the `const` keyword.
  This makes the code verbose.
