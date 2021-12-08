---
eleventyNavigation:
  key: Dart
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://dart.dev", "Dart" %}
is a general purpose programming language from Google.
It was first announced in October, 2012.

The original purpose of Dart was to be an alternative to JavaScript
for running code in web browsers.
The Chrome browser planned to include a Dart VM for this purpose,
but that plan has been abandoned in favor of compiling to JavaScript.

Currently Dart is primarily used by the
{% aTargetBlank "https://flutter.dev", "Flutter" %} framework
for building mobile, web, and desktop applications.

The syntax of Dart is somewhat similar to Java.

## Resources

- [title](https://some.url)

## Creating and Running Programs

Dart source files have a `.dart` file extension.
One `.dart` file can import another using the `import` command.
The `main` function defines the starting point of a program.
To run a `.dart` file that defines a `main` function, enter `dart {name}.dart`.

## Compiling

The `dart compile {format}` command compiles Dart code to other formats.
The `exe` format creates an executable program for the current platform
which can be Windows, macOS, or Linux.
The `js` format compiles Dart code to JavaScript
that includes an implementation of the Dart runtime.

## Types

Dart supports type inference, so types
do not need to specified if they can be inferred from values.

## Access Specifiers

Dart does not support keywords to indicate access levels
of things like classes, functions, and variables.
Instead it relies on the convention of prefixing names with an underscore
to indicate that they should be treated as private.

## Constructors

Constructors are methods with the same name as the class.

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
a no-arg constructor that doesn't initialize any fields is provided.
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

Constructors can initialize fields before their body runs.
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

## Arrays

Arrays in Dart are `List` objects.
A literal array is written as a
comma-separated list of values surrounded by square brackets.
For example, `var numbers = [3, 7, 19];`

## Sets

A set is an unordered collection of unique values.
A literal set is written as a
comma-separated list of values surrounded by curly braces.
For example, `var numbers = {3, 7, 19};`

## Maps

A map is a collection of key/value pairs.
A literal map is written as a
comma-separated list of pairs surrounded by curly braces.
Each pair is written as a key followed by a colon and a value.
When a key is a string, it must be delimited by single or double quotes.

## Objects

## Constants

Variables whose values are known at compile-time
should be declared with the keyword `const`.
Class fields whose values are known at compile-time
should be declared with the keywords `static const`.
When the type is a object or array, the contents also cannot be changed.

Variables and class fields whose values are
set once at run-time and then never changed
should be declared with the keyword `final`.
When the type is a object or array, the contents can be changed.

## Null Safety

By default variables cannot have the value `null`.
To enable this their type must be prepended with a question mark.
For example, a variable of type `String?` can be set to `null`,
but a variable of type `String` cannot.

## Code Formatting

Formatting of Dart code is provided by {% aTargetBlank
"https://dart.dev/tools/dart-format", "dart format" %}.
To get the best formatting, include a comma after every function parameter,
even the last one.

## Annoyances

- Dart wants many constructor calls to be preceded by the `const` keyword.
  This makes the code verbose.
