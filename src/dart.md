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
Class properties whose values are **known at compile-time**
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

In order to create instances of a class that are compile-time constants,
all fields must be final and the constructor must be marked const.
When this is done, constructor calls with same argument values
return references to the same object.
The following code demonstrates this:

```dart
class Point {
  final double x;
  final double y;

  const Point(this.x, this.y);
}

main() {
  var p1 = const Point(1, 2); // compile-time constant
  const p2 = Point(1, 2); // alternate way to create a compile-time constant
  var p3 = Point(1, 2); // not a constant
  print(identical(p1, p2)); // true - same object
  print(identical(p1, p3)); // false - not same object
}
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

These and many other classes are defined in the `dart:core` package.
Classes defined here do not need to be imported.

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

### String Class

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

An individual character (code point) can be
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

Unicode character codes can be embedded in a `String` with `\u{hex-code}`.
For example:

```dart
print('Do you prefer a \u{1F436} or \u{1F431}?'); // Do you prefer a 🐶 or 🐱?
```

Raw strings are strings where backslash character is not treated specially.
They are indicated by preceding the opening delimiter with "r".
For example, `r'first\nsecond'` does not treat `\n` as a newline character.
Raw strings useful when defining regular expressions.

Defining custom classes that extend the `String` class is not allowed.

The `String` class defines the following properties:

| Property     | Description                                   |
| ------------ | --------------------------------------------- |
| `codeUnits`  | a `List<int>` of the characters (code points) |
| `isEmpty`    | `bool` indicating if empty                    |
| `isNotEmpty` | `bool` indicating if not empty                |
| `length`     | number of characters (code points)            |
| `runes`      | `Iterable` over the characters (code points)  |

The `String` class defines the following instance methods:

| Method                                                                                | Description                                                                                          |
| ------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `allMatches(String s, [int start = 0])`                                               | returns `Iterable<Match>` over matching substrings                                                   |
| `codeUnitAt(int index)`                                                               | returns code point at a given index; same as `[index]`                                               |
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
`==` (same code points), `[index]` (gets code point at index).
For example `'ho ' * 3` creates the `String` `'ho ho ho '`.

The table below summarized converting between numbers and strings.

| Conversion           | Code              |
| -------------------- | ----------------- |
| `int` to `String`    | `i.toString()`    |
| `double` to `String` | `d.toString()`    |
| `String` to `int`    | `int.parse(s)`    |
| `String` to `double` | `double.parse(s)` |

## Type Casts

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

TODO: Add more detail on type casts?

## Generic Types

Generics, a.k.a parameterized types,
allow writing functions, classes, and methods
that accept values of multiple types.
Their functionality can differ based on the types of values provided.
Using generics often reduces code duplication
in cases where multiple blocks of code
only differ in the types on which they operate.

Generics are often used to implement new kinds of collections.
The core collection classes, described in the next section, do exactly this.
For example, when creating a `List`, the type of
the elements it can hold are specific using generics.
The following are equivalent:

```dart
List<int> numbers = [1, 2, 3]; // type is specified on the variable
var numbers = <int>[1, 2, 3]; // type is specified on the value
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
Common names for type parameters include:

- `E` for Element
- `K` for Key
- `R` for Return type
- `T` for Type
- `V` for Value

Longer, more descriptive names, like `Event` and `State` can also be used.

Any number of type parameters can be specified inside angle brackets.
They can be constrained to only types that extend a given class
using the `extends` keyword,
or left unconstrained in which case values of any type can be used.

The following example demonstrates implementing a generic function.
It takes any `Iterable` containing `num` values and returns its sum.
This means it works with both `int` and `double` values
since their superclass is `num`.

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

Dart provides many generic collection classes.
Built-in collection classes that can be used without importing
include `List`, `Set`, and `Map`.
Other collection classes are defined in the package `dart:collection`
and must be imported.
These include `DoubleLinkedQueue`, `HashMap`, `HashSet`,
`LinkedHashMap`, `LinkedHashSet`, `LinkedList`, `ListQueue`, `Queue`,
and `SplayTreeMap`, `SplayTreeSet`.

### Iterable

The `Iterable` generic class represents a collection of values
that are accessed sequentially.
The values can be lazily computed when requested
and an infinite number of values can be generated.

The following generic collection classes all have `Iterable` as a superclass:
`DoubleLinkedQueue`, `IterableBase`, `IterableMixin`,
`LinkedList`, `List`, `ListQueue`, `Queue`, `Runes`, and `Set`.

Any `Iterable` collection can be used in a `for-in` loop
to iterate over its elements.

The `Iterable` class provides the following constructors:

| Constructor                                             | Description                                     |
| ------------------------------------------------------- | ----------------------------------------------- |
| `Iterable()`                                            | used as superclass of classes that are iterable |
| `Iterable.empty()`                                      | contains no values                              |
| `Iterable.generate(int count, E generator(int index)?)` | generates values on request                     |

The `Iterable` class provides the following read-only properties:

| Property     | Description                                         |
| ------------ | --------------------------------------------------- |
| `first`      | first element                                       |
| `hashCode`   | hash code                                           |
| `isEmpty`    | `bool` indicating if there are no elements          |
| `isNotEmpty` | `bool` indicating if there is at least one element  |
| `iterator`   | `Iterator` object used to iterate over elements     |
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

The `Iterator` generic class provides
the read-only instance property `current` which holds the current element and
the instance method `moveNext()` which sets `current` to the next element
and returns a `bool` indicating if there is another element.
Instances of this class are not typically used directly.
Instead, syntax like the `for-in` loop are used to iterate over an `Iterable`.

### List Class

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

Two lists can be concatenated to create a new `List` with the `+` operator.
For example:

```dart
var list1 = [1, 2];
var list2 = [3, 4];
var list3 = list1 + list2; // [1, 2, 3, 4]
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

### Set Class

A `Set` is an unordered collection of unique values.
A literal set is written as a
comma-separated list of values surrounded by curly braces.

There are several ways to declare and initialize a variable that holds a `Set`.

```dart
// type Set<String> is inferred
var colors = {'red', 'green', 'blue'};

// type Set<String> is specified on the value
var colors = <String>{'red', 'green', 'blue'};

// type Set<String> is specified on the variable
Set<String> colors = {'red', 'green', 'blue'};

var colors = Set<String>();
colors.add('red');
colors.add('green');
colors.add('blue');

// The following creates an empty Map, not an empty Set.
// var colors = {};
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

### Map Class

A `Map` is a collection of key/value pairs.
The keys and values can have any type.
Unlike `List` and `Set`, this class is not a subclass of `Iterable`.

A literal map is written as a
comma-separated list of pairs surrounded by curly braces.
Each pair is written as a key followed by a colon and a value.
When a key is a string, it must be delimited by single or double quotes.

There are several ways to declare and initialize a variable that holds a `Map`.

```dart
// type Map<String, int> is inferred
var colorMap = {'red': 1, 'green': 2, 'blue': 3};

// type Map<String, int> is specified on the value
var colorMap = <String, int>{'red': 1, 'green': 2, 'blue': 3};

// type Map<String, int> is specified on the variable
Map<String, int> colorMap = {'red': 1, 'green': 2, 'blue': 3};

// This map has String keys and values of any type.
Map<String, dynamic> dog = {'name': 'Comet', 'age': 1, 'isFast': true};

// The following creates an empty map where
// keys and values both have the type dynamic.
var myMap = {}; // same as Map()
```

Values of `Map` keys can be retrieved with the `[]` operator.
This returns a nullable value.
For example:

```dart
var colorMap = {'red': 1, 'green': 2, 'blue': 3};
int? number = colorMap['blue'];
print(number ?? 'not found'); // 3
if (number != null) {
  print(number); // 3
}
```

The spread operator `...` can be used to spread `Map` objects into others.
For example:

```dart
var map1 = {'apple': 'red', 'banana': 'yellow'};
var map2 = {'cherry': 'red', 'grape': 'green'};
var map3 = {
  'lemon': 'yellow',
  ...map1,
  'blueberry': 'blue',
  ...map2,
  'watermelon': 'pink'
};
print(map3); // all the key/value pairs
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

  // Methods that override a superclass method should
  // be annotated with @override.  In this case we are
  // overriding the toString method in the Object class.
  @override
  String toString() => '$name is a $breed.';
}

void main() {
  var d = Dog('Comet', 'Whippet');
  print(d); // Comet is a Whippet.
}
```

## Additional Core Classes

The `dart:core` package defines all the basic types
like `bool`, `int`, `double`, and `String`.
It also defines collection types like `List`, `Set`, and `Map`.
Highlights of other classes defined in `dart:core` are described below.

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

- `daysPerWeek` (7)
- `monthsPerYear` (12)

- `january` (1)
- `february` (2)
- `march` (2)
- `april` (4)
- `may` (5)
- `june` (6)
- `july` (7)
- `august` (8)
- `september` (9)
- `october` (10)
- `november` (11)
- `december` (12)

- `monday` (1)
- `tuesday` (2)
- `wednesday` (3)
- `thursday` (4)
- `friday` (5)
- `saturday` (6)
- `sunday` (7)

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

- `hoursPerDay`
- `microsecondsPerDay`
- `microsecondsPerHour`
- `microsecondsPerMillisecond`
- `microsecondsPerMinute`
- `microsecondsPerSecond`
- `millisecondsPerDay`
- `millisecondsPerHour`
- `millisecondsPerMinute`
- `millisecondsPerSecond`
- `minutesPerDay`
- `minutesPerHour`
- `secondsPerDay`
- `secondsPerHour`
- `secondsPerMinute`
- `zero`

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

### Stopwatch Class

The `Stopwatch` class is used to measure elapsed time in a section of code.

The `Stopwatch` class defines the following properties.

| Property              | Description                                   |
| --------------------- | --------------------------------------------- |
| `elapsed`             | `elapsedTicks` converted to a `Duration`      |
| `elapsedMicroseconds` | `int` microseconds (1e-6 second)              |
| `elapsedMilliseconds` | `int` milliseconds (1e-3 second)              |
| `elapsedTicks`        | number clock ticks                            |
| `frequency`           | number of ticks in a second                   |
| `isRunning`           | `bool` indicating if the stopwatch is running |

The number of clock "ticks" in a second is equal to the `frequency` property.
A common value is 1,000,000.

The `elapsedTicks` property has the same value as `elapsedMicroseconds`
if `frequency` is 1e6.

The `Stopwatch` class defines the following instance methods:

| Method    | Description                                  |
| --------- | -------------------------------------------- |
| `reset()` | resets all the `elapsed*` properties to zero |
| `start()` | starts the stopwatch                         |
| `stop()`  | stops the stopwatch                          |

The following code demonstrates using the `Stopwatch` class:

```dart
import 'dart:math';

void main() {
  var sw = Stopwatch();
  sw.start();
  var sum = 0;
  for (var i = 0; i < 1000000000; i++) {
    sum += pow(i, 3) as int;
  }
  sw.stop();
  print(sum);
  print('elapsed = ${sw.elapsed.inMilliseconds} ms');
}
```

### StringBuffer Class

The `StringBuffer` class supports efficient, incremental building of strings.
It defines the following properties.

| Property     | Description                             |
| ------------ | --------------------------------------- |
| `isEmpty`    | `bool` indicating whether `length` == 0 |
| `isNotEmpty` | `bool` indicating whether `length` >= 0 |
| `length`     | number of code points                   |

The `StringBuffer` class defines the following instance methods:

| Method                                                | Description                                                                               |
| ----------------------------------------------------- | ----------------------------------------------------------------------------------------- |
| `clear()`                                             | clears all code points                                                                    |
| `toString()`                                          | `String` equivalent                                                                       |
| `write(Object? object)`                               | writes `String` representation of `object`                                                |
| `writeAll(Iterable objects, [String separator = ''])` | writes `String` representation of all objects in an `Iterable` with an optional separator |
| `writeCharCode(int charCode)`                         | writes a single code point                                                                |
| `writeln([Object? obj = ''])`                         | same as `write(Object? object)`, but adds newline                                         |

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

### Uri Class

The `Uri` class represents a parsed URI.
It provides many constructors for creating a `Uri` object.

The `Uri` class defines the following properties.

| Property             | Description                                                              |
| -------------------- | ------------------------------------------------------------------------ |
| `authority`          | same `String` value as `host`                                            |
| `data`               | `UriData` object describing a data URI                                   |
| `fragment`           | `String` hash portion without leading `#`                                |
| `hasAbsolutePath`    | `bool` indicating whether the URI has an absolute path                   |
| `hasAuthority`       | `bool` indicating whether the URI has an authority (or domain) part      |
| `hasEmptyPath`       | `bool` indicating whether the URI is missing a path                      |
| `hasFragment`        | `bool` indicating whether the URI has a fragment (or hash) part          |
| `hasPort`            | `bool` indicating whether the URI has a specified port                   |
| `hasQuery`           | `bool` indicating whether the URI has query parameters                   |
| `hasScheme`          | `bool` indicating whether the URI has a specified scheme such as "https" |
| `host`               | `String` domain                                                          |
| `isAbsolute`         | `bool` indicating whether the URI is absolute; false for relative        |
| `origin`             | `String` `scheme` and `host` combined                                    |
| `path`               | `String` path portion that follows `host`                                |
| `pathSegments`       | `List<String>` of `path` parts                                           |
| `port`               | `int` port that optionally follows `host`                                |
| `query`              | `String` containing all query parameters                                 |
| `queryParameters`    | `Map<String, String>` of all query parameters                            |
| `queryParametersAll` | `Map<String, List<String>>` of all query parameters including duplicates |
| `scheme`             | `String` such as "http", "https", or "data"                              |
| `userInfo`           | `String` optional username and password that can precede `host`          |

The `Uri` class defines the following instance methods:

| Method                        | Description                                                  |
| ----------------------------- | ------------------------------------------------------------ |
| `isScheme(String scheme)`     | returns `Bool` indicating whether the URI has a given scheme |
| `normalizedPath()`            | returns a new `Uri` object that is normalized                |
| `removeFragment()`            | returns a new `Uri` without the fragment (or hash) portion   |
| `replace(...)`                | returns a new `Uri` with given parts replaced                |
| `resolve(String reference)`   | returns a new `Uri` resolved relative to a URL `String`      |
| `resolveUri(Uri reference)`   | returns a new `Uri` resolved relative to another `Uri`       |
| `toFilePath({bool? windows})` | returns corresponding `String` file path                     |
| `toString()`                  | returns `String` representation                              |

The following code demonstrates parsing a URL string
using the static `parse` method and
extracting its components from fields of a `Uri` object.
There are many other static methods not described here.

```dart
var url = 'https://mvolkmann.github.io/foo/bar?color=yellow&size=10#my-hash';
var uri = Uri.parse(url);
print('scheme = ${uri.scheme}'); // https
print('authority = ${uri.authority}'); // mvolkmann.github.io
print('host = ${uri.host}'); // mvolkmann.github.io
print('port = ${uri.port}'); // 443?
print('origin = ${uri.origin}'); // https://mvolkmann.github.io
print('path = ${uri.path}'); // /foo/bar
print('pathSegments = ${uri.pathSegments}'); // ['foo', 'bar']
print('query = ${uri.query}'); // color=yellow&size=10
print('queryParametersAll = ${uri.queryParametersAll}'); // {color: [yellow], size: [10]}
print('fragment = ${uri.fragment}'); // my-hash
```

## Spread Operators

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
Use that static method `Function.apply` for this purpose.

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

All Dart functions, including anonymous ones, are closures.
This means they have access to in-scope variables declared outside the function.

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

Functions can have both positional and named parameters
and each of these can be required or optional.
The order of these in both function declarations and calls to functions
must be required positional parameters,
followed by optional positional parameters,
followed by named parameters in any order.

Required positional parameters cannot be given default values.
Optional positional parameters must appear inside square brackets
after the required positional parameters.
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
These must follow all the positional parameters, if any.
Named parameters are optional by default and must either
have a default value or a nullable type (ending with `?`).
Default values can be preceded by `=` or `:`, but `=` is preferred.
To make a named parameter required, add the `required` keyword before its type.
For example:

```dart
// In this version, the named parameter "by" is required.
//int multiply(int n, {required int by}) => n * by;

// In this version, the named parameter "by" is optional.
int multiply(int n, {int by = 0}) => n * by;

main() {
  print(multiply(2, by: 3)); // 6
  print(multiply(4)); // 4
}
```

There are three ways to call a function,
using the function invocation operator `()`,
the `call` instance method, and the static `apply` method.
The following code demonstrates these:

```dart
// This function takes two required positional parameters
// and two optional named parameters.
num sum(num n1, num n2, {num? min, num? max}) {
  var result = n1 + n2;
  if (min != null && result < min) return min;
  if (max != null && result > max) return max;
  return result;
}

void main() {
  print(sum(1, 2)); // 3
  print(sum(1, 2, min: 10)); // 10

  print(sum.call(1, 2)); // 3
  print(sum.call(1, 2, min: 10)); // 10

  // First argument to apply is a function to call.
  // Second argument to apply is an optional List of positional arguments.
  // Third argument to apply is an optional Map<Symbol, dynamic> of named arguments.
  print(Function.apply(sum, [1, 2])); // 3
  print(Function.apply(sum, [1, 2], {#min: 10})); // 10
  print(Function.apply(sum, [15, 10], {#min: 10, #max: 19})); // 19
}
```

The spread operator cannot be used to expand a `List` into function arguments.
However, the static `apply` method on the `Function` class (shown above)
can be used for this purpose.

Anonymous function definitions are written like named function definitions,
but omit the name.
Unlike in JavaScript, parentheses are required around the parameter list
even when there is only one parameter.
For example:

```dart
var numbers = [3, 7, 9];
numbers.forEach((n) => n * 2);
```

Trailing commas are allowed after the last parameter in function definitions
and after the last argument in function calls.
This causes `dart format` to place each
parameter or argument on a separate line.

Functions that do not explicitly return a value evaluate to `null`.

## Generator Functions

Generator functions generate values on demand in a lazy fashion.
The values it generates are not computed until they are requested.

Generator functions use the `yield` keyword to return a single value.
They can also use the `yield*` keyword to return all the values
from another generator function individually.
This serves as a shorter alternative to iterating over
the values from another generator and returning each one.

Synchronous generator functions include the `sync*` keyword
after the parameter list and before the body.
They always return an `Iterator` and
must generate a value as soon as requested.

The following synchronous generator function generates `int` values
in a given range which can be open-ended.

```dart
Iterable<int> range(int start, [int? end]) sync* {
  if (end == null) {
    var i = start;
    while (true) {
      yield i++;
    }
  } else {
    for (var i = start; i <= end; i++) {
      yield i;
    }
  }
}

// This demonstrates several ways to use the "range" function.
// All except the last print the numbers 1 through 5.
void main() {
  for (var i in range(1, 5)) {
    print(i);
  }

  for (var i in range(1)) {
    print(i);
    if (i == 5) break;
  }

  for (var i in range(1).take(5)) {
    print(i);
  }

  // Prints the first five positive integers that are multiples of 3
  // which are 3, 6, 9, 12, and 15.
  range(1).where((n) => n % 3 == 0).take(5).forEach(print);
}
```

The `Iterable` `generator` method provides an easy way
to generate a fixed number of values.
It is passed the number of values to generate and
a function that takes an index and returns a computed value.
For example, the `range` function above can be replaced by the following
if an end value is always supplied.

```dart
Iterable<int> range(int start, int end) {
  var count = end - start + 1;
  return Iterable<int>.generate(count, (index) => index + start);
}
```

Asynchronous generator functions are sometimes useful, but not frequently used.
They include the `async*` keyword
after the parameter list and before the body.
They always return a `Stream` and
can return a `Future` for values that will be computed later.

The following asynchronous generator function
reads text files containing player scores on separate lines.
Each file contains the scores of a single player.

```dart
import 'dart:async';
import 'dart:io';

class PlayerAverage {
  String player;
  double average;

  PlayerAverage(this.player, this.average);

  @override
  String toString() => '$player average score is $average.';
}

Stream<PlayerAverage> computeAverageScores(List<String> players) async* {
  for (var player in players) {
    var lines = await File('scores-$player.txt').readAsLines();
    var total = lines.fold(0, (int acc, String line) => acc + int.parse(line));
    var average = total / lines.length;
    print('$player average is $average.');
    yield PlayerAverage(player, average);
  }
}

void main() async {
  var players = ['Mark', 'Tami', 'Amanda', 'Jeremy'];

  PlayerAverage? winner;
  // Note the use of "async for" to iterate over
  // values in a Stream inside an async function.
  // Only use this when it is certain that the Stream will complete.
  await for (var result in computeAverageScores(players)) {
    if (winner == null || result.average > winner.average) winner = result;
  }

  if (winner != null) {
    print(
      'The winner is ${winner.player} '
      'with an average score of ${winner.average}.'
    );
  }
}
```

## Conditional Logic

Dart supports two statements for implementing conditional logic,
`if` and `switch`.
It also supports the ternary operator `? :`.

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
Here is an example of a `switch` statement.

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

The ternary operator selects a value based on boolean expressions.
For example:

```dart
print(coinSide == 'heads' ? 'You win.' : 'You lose.');

var assessment =
  temperature >= 80 ? 'hot' :
  temperature <= 32 ? 'cold' :
  'comfortable';
```

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

When a function catches an exception,
it can rethrow the exception so the caller can handle it.

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

All `.dart` source files define a "library".
Private names in a library are accessible inside the library,
but not outside it.
A compile-time error is generated if such access is attempted.

## Classes

Classes define:

- class properties (a.k.a. fields) to hold data
  not associated with a specific instance
- instance properties (a.k.a. fields) to hold data
  associated with a specific instance
- constructors to create instances
- class methods to operate on class properties
  and possibly instances passed to them
- instance methods to operate on data in a specific instance
- operators to return data computed from an instance
  and possibly one other instance (for binary operators)

### Constructors

A constructor is defined as a method with no return type
and the same name as the class.
For example:

```dart
import 'dart:math';

class Point {
  double x;
  double y0;

  /* Long way to write a constructor that is not preferred.
  Point(double x, double y) {
    this.x = x;
    this.y = y;
  }
  */

  // Another way to write the constructor is to use an initializer list.
  // Note that having a body is optional.
  //Point(double x, double y) : this.x = x, this.y = y;

  // Preferred way to write this constructor that
  // handles assigning argument values to instance properties.
  Point(this.x, this.y);

  // To make x and y be optional named parameters with default values,
  // change the previous constructor to the following:
  //Point({this.x = 0, this.y = 0});

  // To make x and y be required named parameters,
  // change the previous constructor to the following:
  //Point({required this.x, required this.y});

  double get distanceFromOrigin => sqrt(pow(x, 2) + pow(y, 2));
}

void main() {
  var p = Point(3, 4);
  print(p.distanceFromOrigin); // 5
}
```

When instance properties have default values, during instance creation
those are assigned before the constructor is called.
Those values can be used in a constructor
to compute the values of other properties.

Here's a somewhat advanced rule regarding constructors.
Properties marked as `final` that are not also marked as `late`
and have a non-nullable type must be
initialized before the constructor body is reached.
Typically this is done in the initializer list.

Static properties which exist outside of any instance
cannot be set in a constructor.

The keyword `this` is only needed to disambiguate
property and method references from local variables with the same names.
When there is no name conflict, preceding property and method names
with `this.` is not required, and is discouraged.

TODO: Describe the `late` keyword.

### Getters and Setters

When object properties are accessed, Dart actually
calls a "getter" or "setter" method.
Non-private properties (including those marked `late final` but not `final`)
are automatically given getter and setter methods
which allow them to be accessed and set from outside their class.
Getter and setter methods can be provided for private properties.
Getters can also be used to implement computed properties.
Setters can validate new values and throw when they are invalid.

The following code demonstrates writing getter and setter methods.
Typically the properties on which they operate are made private
to prevent direct access.

```dart
class Point {
  double _x = 0;
  double y = 0;

  double get x {
    print('in getter for x');
    return _x;
  }

  set x(double x) {
    print('in setter for x');
    _x = x;
  }

  Point(x, this.y) : _x = x;

  @override
  String toString() {
    return '($_x, $y)';
  }
}

void main() {
  var pt = Point(2, 3);
  pt.x = 7; // outputs "in setter for x"
  print(pt.x); // outputs "in getter for x" and 7
}
```

The following code demonstrates implementing
a read-only property and a computed property:

```dart
class Person {
  String _name;
  DateTime? birthday;

  Person({required String name, this.birthday}) : _name = name;

  // This getter computes its value.
  int get age {
    if (birthday == null) return 0;
    var now = DateTime.now();
    var years = now.year - birthday!.year;
    var month = birthday!.month;
    if (now.month < month || (now.month == month && now.day < birthday!.day)) {
      years--;
    }
    return years;
  }

  // This getter simply provides access to a private property.
  // The property is read-only because
  // it is private and no setter is defined.
  String get name => _name;

  // This setter performs validation before setting a private property.
  set name(String newName) {
    if (newName.isEmpty) throw "Person name cannot be empty";
    _name = newName;
  }

  @override
  String toString() => birthday == null ? name : '$name is $age years old.';
}

void main() {
  var p1 = Person(name: 'Mark', birthday: DateTime(1961, DateTime.april, 16));
  var p2 = Person(name: 'Tami');
  print(p1); // Mark is 60 years old.
  print(p2); // Tami

  try {
  p2.name = 'Tamara';
  print(p2); // Tamara
  p2.name = ''; // throws
  print(p2);
  } catch (e) {
    print(e); // Person name cannot be empty
  }
}
```

If a class doesn't define a constructor,
a no-arg constructor that doesn't initialize any properties is provided.
If the class has a superclass,
the default constructor calls its no-arg constructor.

### Initializer Lists

An "initializer list" is a list of property assignments
that follow the parameter list and a colon.
Both initializer lists and bodies are optional in constructors.
When both are present, the assignments in the initializer list
occur before the body is executed.
The named constructors below demonstrate
including an initializer list and omitting a body.

### Named Constructors

A class can only have one "regular constructor",
but it can have any number of "named constructors".
These begin with the the class name followed by a period and a unique name.
For example, the `Point` class above could have
a named constructor for creating an origin point
and another for initializing `x` and `y` to the same value.

```dart
  Point.origin() : x = 0, y = 0;

  Point.same(double value) : x = value, y = value;
```

The create an object from a class, call one of its constructors
in the same was as calling a function, without a `new` keyword.

Pulling all of this together we can write the following:

```dart
class Point {
  double x = 0;
  double y = 0;

  Point(this.x, this.y);
  Point.origin() : x = 0, y = 0;
  Point.same(double value): x = value, y = value;

  @override
  String toString() {
    return '($x, $y)';
  }
}

void main() {
  var pt = Point(2, 3);
  print(pt); // (2.0, 3.0)
  pt = Point.origin();
  print(pt); // (0.0, 0.0)
  pt = Point.same(4);
  print(pt); // (4.0, 4.0)
}
```

Here is one more example that demonstrates a constructor
that takes named parameters.

```dart
class Person {
  String name;
  int? age;

  // This constructor only has named parameters.
  // The first parameter is required and the second is optional.
  Person({required this.name, this.age});

  @override
  String toString() => age == null ? name : '$name is $age years old';
}

void main() {
  var p1 = Person(name: 'Mark', age: 60);
  print(p1); // Mark is 60.

  var p2 = Person(name: 'Tami');
  print(p2); // Tami
}
```

### Instance Methods

Classes can define instance methods.
These look like function definitions,
but differ in that they can use the `this` keyword.
For example, the following instance method
can be added to the `Point` class defined above:

```dart
class Point {
 ... same as before ...

  void translate(double dx, double dy) {
    // We don't need "this." before x and y here because
    // there is no name conflict with local variables.
    x += dx;
    y += dy;
  }
}

void main() {
  var pt = Point(2, 3);
  pt.translate(1, -2);
  print(pt); // (3, 1)
}
```

### Class Methods

Classes can define class methods.
These look like instance methods, but are preceded by the `static` keyword.
They cannot use the `this` keyword because
they aren't associated with a specific instance,
but they can access static properties
and the instance properties of objects passed to them.
The following example demonstrates a static method in the `Point` class
that takes a `List` of `Point` objects
and returns a `Point` in the center of them.
It also defines a static property that holds
the number of `Point` objects that have been created.

```dart
import 'dart:math';

class Point {
  static int instanceCount = 0;
  double x = 0;
  double y = 0;

  Point(this.x, this.y) {
    instanceCount++;
  }

  static Point centerOf(List<Point> points) {
    if (points.isEmpty) return Point(0, 0);

    final first = points[0];
    var minX = first.x;
    var minY = first.y;
    var maxX = first.x;
    var maxY = first.y;
    //for (var point in points) {
    for (var i = 1; i < points.length; i++) {
      var point = points[i];
      minX = min(minX, point.x);
      minY = min(minY, point.y);
      maxX = max(maxX, point.x);
      maxY = max(maxY, point.y);
    }
    return Point((maxX + minX) / 2, (maxY + minY) / 2);
  }

  @override
  String toString() {
    return '($x, $y)';
  }
}

void main() {
  var points = [Point(1, 1), Point(5, 4), Point(7, 2)];
  print(Point.instanceCount); // 3
  print(Point.centerOf(points)); // (4, 2.5)
}
```

### Operators

Operators are similar to instance methods, but include the keyword "operator".
Only operator names supported by Dart can be defined.
For example, there is no `@` operator in Dart,
so custom classes cannot define it.
The following code defines the "+" operator for the `Point` class.
This returns a new `Point` and does not modify the `Point` on the left.

```dart
class Point {
  ... same as before ...

  Point operator +(Point other) {
      return Point(x + other.x, y + other.y);
  }
}

void main() {
  var pt1 = Point(2, 3);
  var pt2 = Point(1, 2);
  var pt3 = pt1 + pt2;
  print(pt3); // (3, 5)
}
```

### Factory Constructors

A normal constructor creates an object, but doesn't explicitly return it.
A "factory constructor" can call a normal constructor with computed arguments,
modify the object it creates, and return it.
One use of a factory constructor is to implement a singleton class
where it is not possible create additional instances.

```dart
class MySingleton {
    // This is a private, named constructor.
    MySingleton._private();

    // This creates an instance of this class.
    static final _instance = MySingleton._private();

    // This factory constructor always returns the same instance.
    factory MySingleton() => _instance;
}

void main() {
  var obj1 = MySingleton();
  var obj2 = MySingleton();
  print(identical(obj1, obj2)); // true
}
```

### Inheritance

A class can inherit the properties and methods of **one** other class
using the `extend` keyword.
Classes that do not explicitly extend another class
implicitly extend the built-in `Object` class.
This means all classes extend `Object`.

The following code demonstrates creating a
subclass of the `Person` class defined above.
Note how the `super` keyword is used to call a superclass constructor
or superclass method whose name matches one in the subclass.
The superclass constructor can be called from an initializer list.

```dart
class SoftwareEngineer extends Person {
  String primaryLanguage;

  SoftwareEngineer({
    required String name,
    int? age,
    required this.primaryLanguage
  }) : super(name: name, age: age);

  // This method calls the superclass method with the same name
  // using the super keyword.
  @override
  String toString() {
    var connector = age == null ? '' : ' and';
    return '${super.toString()}$connector knows $primaryLanguage.';
  }
}

void main() {
  var p3 = SoftwareEngineer(name: 'Mark', age: 60, primaryLanguage: 'Dart');
  print(p3);

  var p4 = SoftwareEngineer(name: 'Tami', primaryLanguage: 'JavaScript');
  print(p4);
}
```

To call a named constructor of a superclass, use `super.theName(arguments)`.

### Abstract Classes (Interfaces)

Classes can be marked with the `abstract` keyword
which prevents creating instances of them.
Abstract classes are useful for defining functionality
to be implemented or inherited by other classes.
Including bodies in the methods of an abstract class is optional.
When a body is present, subclasses can use the method as is or override it.
When a body is not present, a semicolon is placed after the parameter list
and subclasses must override the method to provide a body.

A class can `implement` any number of other classes.
For example, `class A implements B, C { ... }`
means that class `A` will implement
every method described in classes `B` and `C`,
regardless of whether `B` and `C` are abstract
(because every class defines an interface)
and regardless of whether their methods include bodies.

The order which classes are listed after the `implements` keyword matters.
Method lookup occurs from right to left (last one in wins).
For example, in `class A extends B implements C, D { ... }`,
if `B`, `C`, and `D` all implement the same method,
the version in `D` will take precedence.

Interface in other programming languages are
collections of method signatures that some classes implement.
Dart doesn't support defining "interfaces",
but `abstract` classes can be used for this purpose.

A subclass of an abstract class must implement all methods
described in the abstract class that do not have bodies.
If a subclass implements all the methods,
it can use the `implements` keyword instead of the `extends` keyword
to express its relationship to the abstract class.
But if a subclass wants to inherit methods from an abstract class
that contain bodies, it must use the `extends` keyword.

When overriding a superclass method, the parameter types
can be made more restrictive using the `covariant` keyword.
The following code demonstrates using this keyword.

```dart
import 'dart:math';

abstract class Shape {
  double area();
  bool same(Shape shape);
}

class Circle implements Shape {
  double x; // center
  double y; // center
  double radius;

  Circle({required this.radius, this.x = 0, this.y = 0});

  @override double area() => pi * pow(radius, 2);

  @override bool same(covariant Circle other) =>
    x == other.x && y == other.y && radius == other.radius;
}

class Rectangle implements Shape {
  double x; // left
  double y; // bottom
  double height;
  double width;

  Rectangle({required this.height, required this.width, this.x = 0, this.y = 0});

  @override double area() => width * height;

  @override bool same(covariant Rectangle other) =>
    x == other.x && y == other.y && height == other.height && width == other.width;
}

void main() {
  var c = Circle(radius: 5);
  print(c.area()); // 78.5...

  var r = Rectangle(width: 6, height: 3, x: 2, y: 4);
  print(r.area()); // 18

  print(c.same(Circle(radius: 5))); // true
  print(c.same(Circle(radius: 3))); // false

  print(r.same(Rectangle(width: 6, height: 3, x: 2, y: 4))); // true
  print(r.same(Rectangle(width: 6, height: 3))); // false
}
```

In addition to defining methods,
abstract classes can define instance properties.
Classes that implement such an abstract class must also define
the same instance properties and annotate them with `@override`.
This is useful when an abstract class defines methods with bodies
that require certain instance properties to be present
and subclasses will inherit those method implementations.

The following example defines a generic class `Pair`.
This class implements the core abstract class `Comparable`
which describes the `compareTo` method.
Instances of the `Pair` class hold a pair of values that have the same type.
That type must also implement the `Comparable` interface.
Note how this is enforced on the generic type parameter
using the `extends` keyword.

The `List` `sort` method can sort any values that implement `Comparable`.
It can also sort values that do not implement `Comparable`
if it is passed a function for performing comparisons.
But that is not necessary for `Pair` objects.

```dart
class Pair<T extends Comparable> implements Comparable<Pair<T>> {
  T first;
  T second;

  Pair(this.first, this.second);

  @override
  int compareTo(Pair<T> other) {
    var result = first.compareTo(other.first);
    if (result == 0) result = second.compareTo(other.second);
    return result;
  }

  @override
  String toString() => '($first, $second)';
}

main() {
  var intPairs = [
    Pair(1, 2),
    Pair(4, 3),
    Pair(2, 7),
    Pair(0, 4),
    Pair(4, 3),
    Pair(3, 1)
  ];
  intPairs.sort();
  print(intPairs); // [(0, 4), (1, 2), (2, 7), (3, 1), (4, 3), (4, 3)]

  var stringPairs = [
    Pair('red', 'apple'),
    Pair('green', 'grape'),
    Pair('orange', 'peach'),
    Pair('yellow', 'banana'),
    Pair('pink', 'watermelon'),
    Pair('orange', 'orange')
  ];
  stringPairs.sort();
  print(stringPairs);
  // [(green, grape), (orange, orange), (orange, peach),
  //  (pink, watermelon), (red, apple), (yellow, banana)]
}
```

### Mixins

Mixins provide a way to share property definitions
and method implementations between classes.

There are three ways to define a mixin.

1. Use the `mixin` keyword (preferred).
1. Define a class with no constructor.
1. Define an abstract class.

Recall that classes have a constructor and can be extended.
Also recall that abstract classes can be implemented.

Regardless of how a mixin is defined, it cannot do any of the following:

- have a constructor that is used to create instances
- be used with the `extends` keyword
  to act as the superclass of another class
- be used with the `implements` keyword
  to define methods another class must implement

To mix a mixin into a class, use the `with` keyword.
A class can use any number mixins.
The class can then access any of the mixin properties and methods.
It can also override any of the mixin methods.

When calling a method on a class that extends another class
and mixes in multiple mixins, the order of the mixins matters.
The `extends` keyword must appear before the `with` keyword.
Suppose we have `class A extends B with C, D`
and `B`, `C`, and `D` all define a method with the same name.
The last one in wins, so the version in `D` is used.

The following code demonstrates defining two mixins
and mixing both into the same class:

```dart
import 'dart:math';

// Objects that are "Sized" have a height and weight.
mixin Sized {
  double height = 0; // in feet
  double weight = 0; // in pounds

  void setSize({required double height, required double weight}) {
    this.height = height;
    this.weight = weight;
  }

  double getBmi() {
    var kg = weight * 0.453592;
    var meters = height * 0.3048;
    return kg / pow(meters, 2);
  }
}

// Objects that are "Located" have x and y coordinates.
mixin Located {
  double x = 0;
  double y = 0;

  void setLocation({required double x, required double y}) {
    this.x = x;
    this.y = y;
  }

  double distanceFrom(Located other) {
    return sqrt(pow(x - other.x, 2) + pow(y - other.y, 2));
  }
}

class Animal {
  String species;

  Animal({required this.species});
}

// A class can only extend one other class,
// but it can mixin any number of mixins.
class Human extends Animal with Located, Sized {
  String name;

  Human({required this.name}) : super(species: 'homo sapiens');
}

// We can define additional classes that mixin
// one or both of the Located and Sized mixins.

main() {
  var me = Human(name: 'Mark');
  me.setSize(height: 6.17, weight: 172);
  me.setLocation(x: 0, y: 0);

  var wife = Human(name: 'Tami');
  // As an alternative to calling setSize,
  // we can set the height and weight properties directly.
  wife.height = 5.42;
  wife.weight = 120;
  // As an alternative to calling setLocation,
  // we can set the x and y properties directly.
  wife.x = 3;
  wife.y = 4;

  print('my BMI = ${me.getBmi()}'); // 22.06...
  print('wife BMI = ${wife.getBmi()}'); // 19.94...
  print('distance = ${me.distanceFrom(wife)}'); // 5
}
```

A mixin can be restricted to only be applicable
to classes that extend a given class.
For example, the first line of the `Sized` mixin above
could be changed to `mixin Sized on Animal`
to only allow it to be mixed into subclasses of `Animal`.

### Extensions

The `extension` keyword is used to
add properties and methods to an existing class,
including core Dart classes.

The following code adds a property and a method to the `String` class.

```dart
extension StringExtension on String {
  // Property that holds the first character or null.
  String? get first => isEmpty ? null : this[0];

  // Method that returns the last character or null.
  // This demonstrates adding a method, but it would be
  // better to make this a computed property like "first".
  String? last() => isEmpty ? null : this[length - 1];

  // Method that returns a new String with the characters in reverse order.
  String reverse() => String.fromCharCodes(codeUnits.reversed);
}

main() {
  var name = 'Mark';
  print(name.first); // 'M'
  print(name.last()); // 'k'
  print(name.reverse()); // 'kraM'
}
```

## Object Class

All Dart classes inherit directly or indirectly from the `Object` class.
This means all objects have the properties and methods defined by that class.

The `Object` class provides the following properties:

| Property      | Description                                     |
| ------------- | ----------------------------------------------- |
| `hashCode`    | an `int` value generated from object properties |
| `runtimeType` | a `Type` object describing the object type      |

The `Object` class provides the following methods:

| Method                                | Description                                               |
| ------------------------------------- | --------------------------------------------------------- |
| `noSuchMethod(Invocation invocation)` | called when a non-existent method is called on the object |
| `toString()`                          | returns the `String` representation of the object         |

## Invocation Class

The `Invocation` object passed to the `noSuchMethod` method
of the `Object` class provides the following properties:

| Property              | Description                                             |
| --------------------- | ------------------------------------------------------- |
| `isAccessor`          | `bool` indicating whether a getter or setter was called |
| `isGetter`            | `bool` indicating whether a getter was called           |
| `isMethod`            | `bool` indicating whether a method was called           |
| `isSetter`            | `bool` indicating whether a setter was called           |
| `memberName`          | `Symbol` name of the invoked member                     |
| `namedArguments`      | `Map` of named arguments                                |
| `positionalArguments` | `List` of positional arguments                          |
| `typeArguments`       | `List` of argument types                                |

## noSuchMethod Method

The `noSuchMethod` method defined by the `Object` class
is available in all objects.
The default implementation throws `NoSuchMethodError`.
This can be overridden to customize the handling of
references to undefined object members.

The following code demonstrates the data available inside a `noSuchMethod`

```dart
class Demo {
  @override
  noSuchMethod(Invocation invocation) {
    var name = invocation.memberName;
    if (invocation.isAccessor) {
      if (invocation.isGetter) print('$name is a getter');
      if (invocation.isSetter) print('$name is a setter');
    }
    if (invocation.isMethod) {
      print('$name is a method.');

      // Type arguments are generic parameters between angle brackets.
      var typeArgs = invocation.typeArguments;
      if (typeArgs.isNotEmpty) print('type arguments are ${typeArgs}');

      var posArgs = invocation.positionalArguments;
      if (posArgs.isNotEmpty) print('positional arguments are ${posArgs}');

      var namedArgs = invocation.namedArguments;
      if (namedArgs.isNotEmpty) print('named arguments are ${namedArgs}');
    }
  }
}

main() {
  dynamic demo = Demo(); // type must be dynamic

  print(demo.one);
  // Output is:
  // Symbol("one") is a getter
  // null

  demo.two = 'test';
  // Output is:
  // Symbol("two=") is a setter

  demo.three('a1', 'a2', a3: true, a4: 4);
  // Output is:
  // Symbol("three") is a method.
  // positional arguments are [a1, a2]
  // named arguments are {Symbol("a3"): true, Symbol("a4"): 4}

  demo.four<bool, int>(1, 2, 3);
  // Output is:
  // Symbol("four") is a method.
  // type arguments are [bool, int]
  // positional arguments are [1, 2, 3]
}
```

One use of `noSuchMethod` is implementing domain specific languages (DSL).
The following code demonstrates a basic XML builder
that can be used to generate HTML.

```dart
// Creates an indented String from lines of XML.
String formatLines(List lines) {
  var level = 0;
  var s = '';
  for (var l in lines) {
    var line = l as String;
    var isEndTag = line.startsWith('</');
    if (isEndTag) level--;
    s += ('  ' * level) + line + '\n';
    var isStartTag = !isEndTag &&
        line.startsWith('<') &&
        line.endsWith('>') &&
        !line.endsWith('/>');
    if (isStartTag) level++;
  }
  return s;
}

// Extracts the name of a Symbol.
// symbol.toString() returns 'Symbol("some-name")'.
String symbolName(Symbol symbol) {
  var name = symbol.toString().substring(8);
  return name.substring(0, name.lastIndexOf('"'));
}

class XMLBuilder {
  // Returns a List<String> representing lines of XML.
  @override
  dynamic noSuchMethod(Invocation invocation) {
    // The member name is the name of an element to create.
    var name = symbolName(invocation.memberName);

    // Only method invocations are supported.
    if (!invocation.isMethod) throw 'unsupported accessor $name';

    var tag = '<$name';
    var lines = [];

    // Positional arguments represent child elements and text content.
    for (var arg in invocation.positionalArguments) {
      if (arg is List) {
        lines.addAll(arg);
      } else {
        lines.add(arg);
      }
    }

    // Named arguments represent attributes of the current element.
    invocation.namedArguments.forEach((key, value) {
      tag += ' ${symbolName(key)}="$value"';
    });

    if (invocation.positionalArguments.isEmpty) { // no children
      lines.add(tag + ' />'); // closes element in shorthand way
    } else {
      // Children have already been added to lines.
      lines.insert(0, tag + '>'); // adds start tag before children
      lines.add('</$name>'); // adds end tag after children
    }

    return lines;
  }
}

main() {
  // This must be declared dynamic in order for noSuchMethod to work.
  dynamic b = XMLBuilder();

  // Dart requires named arguments to follow positional arguments.
  // This uses named arguments to describe XML attributes
  // and positional arguments to represent child elements and text content.
  // So unfortunately XML attributes must be described
  // after child elements and text content.
  var lines = b.html(
    b.head(
      b.title("My Title"),
      b.link(href: 'my-styles.css'),
    ),
    b.body(
      b.h1('My Header'),
      b.p('Hello, World!', style: 'color: red'),
    ),
  );
  print(formatLines(lines));
}
```

The code above outputs the following:

{% raw %}

```html
<html>
  <head>
    <title>My Title</title>
    <link href="my-styles.css" />
  </head>
  <body>
    <h1>My Header</h1>
    <p style="color: red">Hello, World!</p>
  </body>
</html>
```

{% endraw %}

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

## Libraries

Every `.dart` file defines a "library".
Typically these are placed in the `lib` directory of a project.
They can be placed at the top of the `lib` directory or
in subdirectories nested at any depth below the `lib` directory.

All names defined in a library, including private ones,
are accessible throughout the library.

### import Statement

Libraries are imported into other `.dart` files with an `import` statement.
For example, suppose a project in a directory named `my_project`
defines a library in the file `lib/math/geometry.dart`.
This library can be imported by the file `bin/my_project.dart`
with the following:

```dart
import 'package:my_project/math/geometry.dart';
```

This makes all the non-private names defined in `geometry.dart`
available in `my_project.dart`.

To avoid name conflicts with names defined in the importing file
with those in the imported file,
the import can be changed to the following:

```dart
import 'package:my_project/math/geometry.dart' as geometry;
```

With this change the names defined in `geometry.dart`
must be referred to with a `geometry.` prefix.

The following code is example content from `lib/math/geometry.dart`:

```dart
import 'dart:math';

class Circle {
  double radius;

  Circle({required this.radius});

  double get area => pi * pow(radius, 2);
}
```

The following code is example content of `bin/my_project.dart`:

```dart
import 'package:my_project/math/geometry.dart' as geometry;

void main() {
  var c = geometry.Circle(radius: 5);
  print(c.area); // 78.5...
}
```

The Dart analyzer will issue a warning if an `import` statement
uses a relative file path (starting with `./` or `../`) to navigate
from files outside the `lib` directory to files inside it.
Instead `import` paths for files under the `lib` directory
should begin with `package:{project_name}/` as shown above.

Relative file paths in `import` statements
from files inside the `lib` directory
to files outside it are not supported.
But relative file paths can be used in `import` statements between
files that are both under the `lib` directory or both outside it.

### Libraries Defined By Multiple Files

Often a library is defined by a single source file,
but libraries can be defined by multiple files.
There are two ways to do this.
One approach involves the `export` statement.
The other involves the `part` and `part of` statements.

#### export Statement

For example, a `math` library can be divided into the files
`math.dart`, `algebra.dart`, `geometry.dart`, and `trigonometry.dart`.
The `math.dart` file can contain the following `export` statements:

```dart
export 'algebra.dart';
export 'geometry.dart';
export 'trigonometry.dart';
```

Importing `math.dart` provides access to the names it defines
and also the names defined the files that it exports.
The exported files can also be imported individually in source files
that do not need access to names defined in all the exported files.
But the names in the exported files cannot be used
inside the file that exports them.

#### part and part of Statements

Using the same example, the `math.dart` file can contain
the following `part` statements instead of `export` statements:

```dart
part 'algebra.dart';
part 'geometry.dart';
part 'trigonometry.dart';
```

The `algebra.dart`, `geometry.dart`, and `trigonometry.dart` files must
contain the following `part of` statement referring back to `math.dart`:

```dart
part of 'math.dart';
```

All `import` statements needed by these four `.dart` files must
appear in file containing the `part` statements (`main.dart` in this case)
and they must appear before the `part` statements.
Files containing `part of` statements cannot contain `import` statements.

As with using `export` statements, importing `math.dart` provides access to
the names it defines and also the names defined in its `part` files.
Files containing `part of` statements cannot be imported.
Files containing `part` statements can use
the names defined in the files they reference.

## File I/O

The `dart:io` library supports all kinds of input and output,
including reading and writing files.

The following code demonstrates reading a file into a `List` of lines.
The entire file is read into memory, so this isn't applicable for large files.

```dart
import 'dart:async';
import 'dart:io';

void main() async {
  var file = File('BeverlyHillbillies.txt');
  var lines = await file.readAsLines();
  for (var line in lines) {
    print(line);
  }
}
```

The `openRead` method can be used to read a file
that is too large to fit in memory.
The following code demonstrates this:

```dart
import 'dart:convert'; // for LineSplitter
import 'dart:io'; // for File

void main() {
  var stream = File('BeverlyHillbillies.txt')
    .openRead()
    .transform(utf8.decoder)
    .transform(LineSplitter());
  stream.listen((line) {
    print(line);
  });
}
```

The `openWrite method can be used to write to a file.

```dart
import 'dart:io'; // for File

void main() {
  // By default this overwrites the file if it exists.
  var sink = File('my-file.txt').openWrite();
  sink.writeln('1st line');
  sink.writeln('2nd line');
  sink.close();
}
```

## Asynchronous Programming

The `dart:async` library provides many classes
that support asynchronous programming.
These include `Future`, `Stream`, `StreamSubscription`, and `Timer`.

In addition, the `dart:isolate` library provides classes
that support running code in a new thread.
These include `Isolate`, `ReceivePort`, and `SendPort`.

Each of these classes are described in the following sections.

### Futures

The `dart:async` library defines the `Future` class.
A `Future` represents the result of code that will run in the future
inside the current thread.
It is similar to a `Promise` in JavaScript.

Execution of these is managed by the event loop described in the next section.
Asynchronous tasks are placed on either
the event queue or the microtask queue.
Tasks on the microtask queue have a higher priority.

The `Future` class defines many named constructors.

To execute code after a given `Duration`,
create a `Future` with the `Future.delayed` constructor.
For example:

```dart
main() {
  print('first');
  Future.delayed(const Duration(seconds: 1), () {
    print('third');
  });
  print('second');
}
```

Functions can return a `Future`.
Calling code can wait from the `Future` to succeed or fail.
There are two approaches that can be used.
One approach is to use the `then` and `catchError` methods.
The other approach is to use the `async` and `await` keywords
which requires importing the `dart:async` library.

The following code demonstrates using `then` and `catchError`.

```dart
Future<int> getFutureScore(int player) {
  return Future<int>.delayed(const Duration(seconds: 1), () {
    if (player == 1) return 7;
    throw 'unknown player $player';
  });
}

void main() {
  getFutureScore(1) // throws if argument isn't 1
    // The function passed to "then" is
    // referred to as a "completion handler".
    .then((int score) {
      print('score = $score');
    })
    .catchError((error) {
      print('error = $error');
    });
}
```

The following code demonstrates using the `async` and `await` keywords.
These work similarly to the same keywords in JavaScript.
The `await` keyword can only be used inside functions marked `async`.
Such functions always return a `Future` even if one isn't explicitly created.

```dart
import 'dart:async';

Future<int> getFutureScore(int player) {
  return Future<int>.delayed(const Duration(seconds: 1), () {
    if (player == 1) return 7;
    throw 'unknown player $player';
  });
}

// If the delay in the previous function isn't needed, it can be
// rewritten as follows.  Note the addition of the async keyword.
/*
Future<int> getFutureScore(int player) async {
  if (player == 1) return 7;
  throw 'unknown player $player';
}
*/

// The placement of the "async" keyword differs from JavaScript.
// Instead of at the beginning of a function definition,
// it belongs after the parameter list and before the body.
void main() async {
  try {
    int score = await getFutureScore(1); // throws if argument isn't 1
    print('score = $score');
  } catch (e) {
    print('error = $e');
  }
}
```

The following table summarizes the `Future` class constructors.
Each of these place a `Future` on either the event or microtask queue
that cannot be evaluated until after the current function completes.

| Constructor        | Queue     | Description                                                                     |
| ------------------ | --------- | ------------------------------------------------------------------------------- |
| `Future`           | event     | creates a `Future` that can run as soon as the current function completes       |
| `Future.delayed`   | event     | similar to 1st constructor, but must wait for at least the specified `Duration` |
| `Future.error`     | microtask | creates a `Future` that fails with a given error                                |
| `Future.microtask` | microtask | same as 1st constructor, but uses the microtask queue                           |
| `Future.sync`      | microtask | runs function immediately and places result (success or failure) in a `Future`  |
| `Future.value`     | microtask | creates a `Future` that succeeds with a given value                             |

`Future.value` is similar to JavaScript `Promise.resolve`.

`Future.error` is similar to JavaScript `Promise.reject`.

TODO: Does the dart:async library provide an equivalent of JS Promise.all
TODO: for an Iterable of Futures?

The following code demonstrates the use of each of these constructors
and explains the order in which the `Future` objects will be evaluated.
When each `Future` either succeeds or fails, its value is printed.
Each line is followed by a comment that describes the output so far (O)
and the contents of the event (E) and microtask (M) queues.

```dart
void main() {
  // O: empty, E: empty, M: empty

  print(1);
  // O: 1, E: empty, M: empty

  Future(() => 7).then(print);
  // O: 1, E: 7, M: empty

  Future.delayed(const Duration(seconds: 1), () => 8).then(print);
  // O: 1, E: D8 7, M: empty; D8 here means the value 8 is delayed.

  Future.error(3).catchError(print); // treating 3 as an error value
  // O: 1, E: D8 7, M: 3

  Future.microtask(() => 4).then(print);
  // O: 1, E: D8 7, M: 4 3
  // The previous line could be replaced by
  // scheduleMicrotask(() => print(4));
  // which requires importing "dart:async".

  Future.sync(() => 5).then(print);
  // O: 1, E: D8 7, M: 5 4 3

  Future.value(6).then(print);
  // O: 1, E: D8 7, M: 6 5 4 3

  print(2);
  // O: 1 2, E: D8 7, M: 6 5 4 3

  // Now all the Futures in the microtask queue can be processed.
  // O: 1 2 3, E: D8 7, M: 6 5 4
  // O: 1 2 3 4, E: D8 7, M: 6 5
  // O: 1 2 3 4 5, E: D8 7, M: 6
  // O: 1 2 3 4 5 6, E: D8 7, M: empty

  // Now all the Futures in the event queue can be processed.
  // O: 1 2 3 4 5 6 7, E: D8, M: empty
  // O: 1 2 3 4 5 6 7 8, E: empty, M: empty
}
```

Flutter provides the `FutureBuilder` class which
builds widgets from a single value returned by a `Future`.

### Event Loop

The Dart event loop is responsible for executing function calls
placed on the event and microtask queues.
Asynchronous functions such as those passed to a constructor
of the `Future` class are placed at the end of one of these queues.
When the current function finishes executing,
the event loop selects the next function to execute.
If the microtask queue is not empty,
the function at its beginning is selected (FIFO order).
Otherwise the function at the beginning
of the event queue is selected (also FIFO order).
When a function starts executing,
all other functions must wait until it completes.
When the function completes, if there are no more
functions in the queues then the program exits.

### Streams

The `dart:async` library defines the `Stream` class.
A Dart `Stream` is like a list of `Future` objects.
It delivers zero or more values and errors over time.

The table below distinguishes four kinds of values in Dart:

|                 | Synchronous   | Asynchronous |
| --------------- | ------------- | ------------ |
| single value    | `T`           | `Future<T>`  |
| multiple values | `Iterator<T>` | `Stream<T>`  |

A `Stream` can specify functions to call when:

- data is ready (first positional argument to `listen`)
- an error occurs (`onError` named argument to `listen`)
- the `Stream` completes (`onDone` named argument to `listen`)

Some provided library functions,
such as the `openRead` method of the `File` class,
return a `Stream` object.

There are two kinds of streams, single subscription and broadcast.
Single subscription streams can only have one listener
and an error occurs if an attempt is made to add more.
Broadcast streams can have any number of listeners.
A broadcast stream can be created from a single subscription stream
by calling its `asBroadcastStream` method.

If a `Stream` generates data and there are no listeners,
the data is lost. It is not cached for delivering later.

The `Stream` class support the following constructors for creating an instance:

| Constructor                                         | Description                                               |
| --------------------------------------------------- | --------------------------------------------------------- |
| `Stream()`                                          | probably not used directly                                |
| `Stream.empty()`                                    | creates empty broadcast stream                            |
| `Stream.error(Object error)`                        | creates stream that emits a single error                  |
| `Stream.eventTransformed(Stream source, sinkFn)`    | creates stream of transformed events from another stream  |
| `Stream.fromFuture(Future<T> future)`               | creates stream of single value from a `Future`            |
| `Stream.fromFutures(Iterable<Future<T>> futures)`   | creates stream of values from `Iterable` of `Future`s     |
| `Stream.fromIterable(Iterable<T> elements)`         | creates stream of `Iterable` elements                     |
| `Stream.multi(onListenFn)`                          | advanced; see docs                                        |
| `Stream.periodic(Duration period, [computationFn])` | creates stream that emits events at a given time interval |
| `Stream.value()`                                    | creates stream that emits a single value                  |

The `Stream` class has the following instance properties,
all of which are read-only:

| Property      | Description                                                      |
| ------------- | ---------------------------------------------------------------- |
| `first`       | first element                                                    |
| `isBroadcast` | `bool` indicating whether this is a broadcast stream             |
| `isEmpty`     | `Future<bool>` indicating whether there are zero elements        |
| `last`        | last element                                                     |
| `length`      | `Future<int>` number of elements                                 |
| `single`      | `Future<T>` single value; error if zero or more than one element |

The `Stream` class support the following instance methods:

| Method                                               | Description                                                                                                                |
| ---------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `any(testFn)`                                        | returns `Future<bool>` indicating if any element passes a test                                                             |
| `asBroadcastStream()`                                | returns a broadcast stream that emits the same elements                                                                    |
| `asyncExpand(convertFn)`                             | returns stream whose elements come from streams returned by calling `convertFn` on each element                            |
| `asyncMap(convertFn)`                                | returns stream whose elements come from calling `convertFn` on each element                                                |
| `cast<R>()`                                          | returns stream where each element is cast to type `R`                                                                      |
| `contains(Object? value)`                            | returns `Future<bool>` indicating if any element is == to `value`                                                          |
| `distinct([bool equals(T previous, T next)])`        | returns stream of elements not equal to previous using `==` or provided `equals` function                                  |
| `drain()`                                            | discards remaining elements in stream                                                                                      |
| `elementAt(int index)`                               | returns `Future<T>` value at given index                                                                                   |
| `every(testFn)`                                      | returns `Future<bool>` indicating if every element passes a test                                                           |
| `expand(convertFn)`                                  | returns stream of values in `Iterable`s returned by calling `convertFn` on each element                                    |
| `firstWhere(testFn)`                                 | returns `Future<T>` first element that passes a test                                                                       |
| `fold<S>(S initialValue, combineFn)`                 | returns `Future<S>` obtained by combining all elements (like `Iterable` `fold`)                                            |
| `forEach(actionFn)`                                  | returns `Future` with no value after calling `actionFn` on each element                                                    |
| `handleError(onErrorFn, testFn)`                     | returns stream that can ignore or transform errors that pass a test                                                        |
| `join([String separator = ""])`                      | returns `Future<String>` formed by concatenating all the lements with an optional separator                                |
| `lastWhere(testFn)`                                  | returns `Future<T>` last element that passes a test                                                                        |
| `listen(onDataFn, {onError, onDone, cancelOnError})` | adds a listener and returns `StreamSubscription<T>`; `cancelOnError` defaults to `true`                                    |
| `map(convertFn)`                                     | returns stream formed by calling `convertFn` on each element (like `Iterable` `map`)                                       |
| `pipe(streamConsumer)`                               | pipes all elements into a `StreamConsumer` and returns `Future` result value                                               |
| `reduce(combineFn)`                                  | returns `Future` obtained by combining all elements (like `Iterable` `reduce`)                                             |
| `singleWhere(testFn)`                                | returns stream that only contains the first element that passes a test                                                     |
| `skip(int count)`                                    | returns stream that begins after first `count` elements                                                                    |
| `skipWhile(testFn)`                                  | returns stream that begins at first element that does not pass a test                                                      |
| `take(int count)`                                    | returns stream containing the first `count` elements                                                                       |
| `takeWhile(testFn)`                                  | returns stream containing all initial elements that pass a test                                                            |
| `timeout(Duration timeLimit, onTimeoutFn)`           | returns stream that emits same elements; if `timeLimit` passes after last emitted element, `onTimeoutFn` can generate more |
| `toList()`                                           | returns `Future<List<T>>` containing all the elements                                                                      |
| `toSet()`                                            | returns `Future<Set<T>>` containing all the elements                                                                       |
| `transform(streamTransformer)`                       | returns stream created by a `StreamTransformer` which transforms the entire stream, not necessarily one element at a time  |
| `where(testFn)`                                      | returns stream containing only elements that pass a test                                                                   |

The `StreamSubscription` object returned by the `Stream` `listen` method
has the instance property `isPaused` and the instance methods
`cancel`, `onData`, `onDone`, `onError`, `pause`, and `resume`.

The following code demonstrates creating and processing a stream:

```dart
import 'dart:async';

void main() {
  // "late" is needed here so it can be used inside the callback below.
  late StreamSubscription sub;

  // The value of "tick" starts at zero and increments by one.
  var stream = Stream.periodic(Duration(seconds: 1), (int tick) {
    if (tick == 3) throw 'rejecting $tick';
    if (tick == 6) sub.cancel();
    return tick * 100;
  });

  // Wrap the periodic stream in one that handles errors.
  stream = stream.handleError((error) {
    print('got error "$error"');
  });

  sub = stream.listen(
    (element) { print(element); },
    onError: (error) { print('got error $error'); }
  );
}
```

The output from the code above is:

```text
0
100
200
got error bad thing happened
400
500
```

Another way to build a `Stream` is to create a `StreamController`
which holds a `Stream` in one of its properties.

The `StreamController` class support the following constructors:

| Constructor                    | Description                      |
| ------------------------------ | -------------------------------- |
| `StreamController()`           | for a single subscription stream |
| `StreamController.broadcast()` | for a broadcast stream           |

The `StreamController` class has the following instance properties:

| Property      | Description                                                        |
| ------------- | ------------------------------------------------------------------ |
| `done`        | read-only `bool` that indicates no more elements will be sent      |
| `hasListener` | read-only `bool` that indicates whether there is a listener        |
| `isClosed`    | read-only `bool` that indicates no more elements can be added      |
| `isPaused`    | read-only `bool` that indicates elements cannot currently be added |
| `onCancel`    | function called when the stream is canceled                        |
| `onListen`    | function called when a listener is registered                      |
| `onPause`     | function called when the stream is paused                          |
| `onResume`    | function called when the stream is resumed                         |
| `sink`        | a `StreamSink` that has `add`, `addError`, and `close` methods     |
| `stream`      | the `Stream` being controlled                                      |

The `StreamController` class support the following instance methods:

| Method                   | Description                                    |
| ------------------------ | ---------------------------------------------- |
| `add(T element)`         | adds an element to the stream being controlled |
| `addError(Object error)` | adds an error to the stream being controlled   |
| `close()`                | closes the stream being controlled             |

The following code demonstrates another way to implement the previous example.
The only differences in the output are that it
doesn't begin with zero and it outputs "done" at the end.

```dart
import 'dart:async';

void main() {
  var controller = StreamController<int>();

  Timer.periodic(Duration(seconds: 1), (Timer t) {
    var value = t.tick;
    switch (value) {
      case 3:
        controller.sink.addError('rejecting $value');
        break;
      case 6:
        t.cancel();
        controller.close();
        break;
      default:
        controller.sink.add(value * 100);
    }
  });

  controller.stream.listen( // returns a StreamSubscription
    (element) {
      print(element);
    },
    onDone: () {
      print('done');
    },
    onError: (error) {
      print('got error "$error"');
    },
  );
}
```

The call to the `listen` method above can be replaced by
the following `for` loop which exits when the stream is closed.

```dart
  // "handleError" returns a new Stream where
  // errors are handled by a supplied function.
  var stream = controller.stream.handleError(
    (error) { print('got error "$error"'); }
  );
  await for (final value in stream) {
    print(value);
  }
```

The `for` loop above can be replaced by
a call to the `Stream` `forEach` method.

```dart
  await stream.forEach(print);
```

Flutter provides the `StreamBuilder` class
which builds widgets from data in a `Stream`.

For more advanced stream processing, see the pub.dev packages
{% aTargetBlank "https://pub.dev/packages/async", "async" %} and
{% aTargetBlank "https://pub.dev/packages/async", "rxdart" %}.
These provides classes that can cache elements, memoized elements,
merge streams, and more.

### Isolates

All Dart code runs in an "isolate" which is described by
the `Isolate` class defined by the `dart:isolate` library.
The `main` function of a Dart program and everything it invokes
runs in the main isolate which is provided by Dart.

Additional isolates can be created to run code in new threads.
This is useful for computationally intensive tasks.

Each isolate is executed in a single thread
and has its own memory and event loop.

The `dart:isolate` library cannot be used in Dart applications
that are compiled to JavaScript.
This means it cannot be used inside DartPad.

Isolates can only communicate by sending messages.
Each isolate can create multiple `ReceivePort` objects.
Each `ReceivePort` object has a corresponding `SendPort` object
that can be accessed through the `sendPort` property of the `ReceivePort`.
To send a message, call the `send` method on a `SendPort` object.
To receive these messages, call the `listen` method
on the corresponding `ReceivePort` object.

Each new isolate is given a function to execute.
An isolate is terminated and removed when this function exits
or when another isolate calls its `kill` method.
An isolate stops running temporarily
when another isolate calls its `pause` method.

The `Isolate` class support the following class methods:

| Method              | Description                                                    |
| ------------------- | -------------------------------------------------------------- |
| `exit()`            | terminates the current isolate                                 |
| `spawn()`           | creates a new isolate that runs a given function               |
| `spawnUri(Uri uri)` | creates a new isolate that runs code from a library at the URI |

`Isolate` objects support the following instance methods:

| Method                                | Description                                                                    |
| ------------------------------------- | ------------------------------------------------------------------------------ |
| `addErrorListener(SendPort port)`     | requests for uncaught errors to be sent to `port`                              |
| `addOnExitListener(SendPort port)`    | requests for a message to be sent to `port` when the isolate terminates        |
| `kill()`                              | requests the isolate to terminate                                              |
| `pause()`                             | requests the isolate to pause execution until `resume` is called               |
| `ping(SendPort port)`                 | requests the isolate to send a message to `port` to verify it is running       |
| `removeErrorListener(SendPort port)`  | stops listening for uncaught error messages                                    |
| `removeOnExitListener(SendPort port)` | stops listening for an exit message                                            |
| `resume()`                            | resumes execution after a call to `pause`                                      |
| `setErrorsFatal(bool fatal)`          | sets whether uncaught errors should terminate the isolate (defaults to `true`) |

The following code demonstrates creating a new `Isolate` to
call a REST service and compute a value based on what it returns.
This avoids blocking the event loop of the main `Isolate` which
allows a Flutter UI to remain responsive while waiting for data to return.

```dart
import 'dart:convert'; // for jsonDecode
import 'dart:isolate';
import 'package:http/http.dart' as http;

// This Dart function doesn't know anything about the Isolate
// in which it runs.  In order for it to communicate back to
// the Isolate that spawned it, it is passed a SendPort.
void getAverageSalary(SendPort sendPort) async {
    // This is a free, public REST service that returns
    // an array of objects that describe employees.
    // Each contains an "employee_salary" property.
  var restUrl = 'http://dummy.restapiexample.com/api/v1/employees';

  // http.get returns a Future, but it runs in the current thread.
  // Calling this in a new Isolate allows it to run in another thread
  // and avoid blocking the event loop of the main Isolate.
  var response = await http.get(Uri.parse(restUrl));

  var status = response.statusCode;
  if (status == 200) {
    try {
      var employees = jsonDecode(response.body)['data'];
      var total = employees.fold(0, (acc, e) {
        var salary = e['employee_salary'] as int;
        return acc + salary;
      });
      sendPort.send(total / employees.length);
    } catch (e) {
      throw e;
    }
  } else {
    // When this REST service returns a 429 status, the body is HTML.
    // There's no easy way to extract a message from it,
    // but the title element contains "Too Many Requests".
    throw status == 429 ? 'too many requests' : 'bad status $status';
  }
}

void main() async {
  // This receives a successful result from the Isolate spawned below.
  var successPort = ReceivePort();

  // Isolate.spawn creates and runs a new Isolate
  // 1st argument is a function to run in the new Isolate.
  // 2nd argument is an argument to pass to the specified function.
  var myIsolate = await Isolate.spawn(getAverageSalary, successPort.sendPort);
  // There many methods that can be called on myIsolate,
  // but only addErrorListener is used here.

  // This receives an error from the Isolate spawned above.
  var errorPort = ReceivePort();
  myIsolate.addErrorListener(errorPort.sendPort);
  errorPort.listen((error) {
    var message = error[0];
    print('got error: $message');

    successPort.close();
    errorPort.close();
  });

  // Some Isolate examples show this approach of
  // waiting for the first value to arrive on the successPort stream.
  // But we can't use this approach because if an error is received above,
  // we need to close successPort in order to exit the program.
  // Doing that would cause an error in this commented out line.
  //var averageSalary = await successPort.first;

  successPort.listen((averageSalary) {
    print('average salary is ${averageSalary.toStringAsFixed(2)}');
    successPort.close();
    errorPort.close();
  });
}
```

Flutter simplifies running code in a new thread
by providing the `compute` function.
This takes a function and data to be passed to it.
It creates a new `Isolate`, runs the function in it,
and returns a `Future` that provides the result when it succeeds.
Use the `await` keyword inside an `async` function
or the `then` method to get the result.

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

Dart unit tests can be implementing using the `test` package.
To install this in a Dart project, enter `dart pub add test --dev`.

Test source files should be placed in the `test` directory.
The directory structure under `test` should
mirror the directory structure under `lib`.
There should be one test source file corresponding to
each `.dart` file under the `lib` directory.

Each test source file name should match the name of the file
for which it provides tests, but must end with `_test.dart`.
And each must import the file that it tests.

Here is an example of code to be tested in the file `lib/math.dart`
that is inside a project named `demo`.

```dart
double add(double n1, double n2) {
  return n1 + n2;
}
```

Here is an example of test code in the file `test/math_test.dart`.

```dart
import 'package:demo/math.dart'; // demo is the project name
import 'package:test/test.dart';

void main() {
  test('add works', () { // passing an anonymous function to test
    expect(add(0, 0), 0);
    expect(add(1, 0), 1);
    expect(add(0, 1), 1);
    expect(add(2, 3), 5);
  });
}
```

The `expect` function takes an actual value and an expected value.
It also accepts the optional named parameters `reason` and `skip`.
The `reason` parameter is a `String` to be displayed
when the actual and expected values do not match.

The expected value can be a {% aTargetBlank
"https://pub.dev/documentation/matcher/latest/matcher/matcher-library.html",
"matcher" %}.
This supports more complex validation such as the following:

```dart
expect(getFullName(user), allOf([
  startsWith('R'),
  contains('Mark'),
  endsWith('Volkmann')
]));
```

If the actual and expected values in a call to `expect` do not match,
it throws a `TestFailure` exception.
This prevents other calls to `expect` in the same `test` function
from being evaluated.

To test that an expression throws a specific kind of exception,
use the `throwsA` function. For example:

```dart
void main() {
  int intDivide(int n1, int n2) => n1 ~/ n2;

  test('integer division works', () {
    expect(intDivide(7, 2), 3);
    expect(
      () => intDivide(1, 0),
      throwsA(isA<IntegerDivisionByZeroException>()),
    );
  });
}
```

To temporarily skip evaluating an `expect`,
set the `skip` parameter to `true` or any `String`.
When `skip` is set to `true`, `expect` will output
"Skip expect: ({reason-or-expected-value})".
When `skip` is set to a `String`, `expect` will output
"Skip expect: {skip-value}".
Note that the arguments to `expect` are evaluated
regardless of the value of `skip`.

TODO: Can you skip a `test` or `group`?

Suppose we want to test the function `processOrder`
on orders that contain multiple items, one item, and zero items,
and the function returns a `bool`
indicating whether the order was successfully processed.
Here are examples of `expect` calls with a `skip` value.

```dart
expect(processOrder(multipleOrder), true, skip: true);
// outputs "Skip expect: (true)" which isn't descriptive

expect(processOrder(singleOrder), true, reason: 'single order', skip: true);
// outputs "Skip expect: single order"

expect(processOrder(emptyOrder), true, skip: 'not implemented yet');
// outputs "Skip expect: not implemented yet"
```

The `test` function and `group` function (described below)
also support a `skip` named parameter
for skipping execution of a test or entire group of tests.
It works the same way as does in the `expect` function.
Unfortunately, because named parameters must follow positional ones,
the `skip` parameter must appear after the body of the `test` or `group`
which makes it difficult to spot when reading code.

To run all the tests in a project, enter `dart test`.
This is a bit slow the first time it is run,
but subsequent runs are much faster.
If all of the tests pass, this will output "All tests passed!".
Otherwise it will output the expected and actual results
of the failed tests, followed by "Some tests failed."

To run tests from inside VS Code, click the beaker icon in the left nav
to display the test panel.
Then click one of the play buttons at the top (non-debug or debug).
To run a single test, hover over it in the test panel
to reveal play buttons and click one of them.

Tests can be grouped into suites using the `group` function
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
