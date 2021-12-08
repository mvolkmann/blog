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
Statements are terminated by semi-colons.
Indentation is typically two spaces.

## Resources

- [title](https://some.url)

## Editors

Many code editors can be used for writing Dart programs.
Popular options include VS Code, Intellij IDEA, and Android Studio.
{% aTargetBlank "https://dartpad.dev", "DartPad" %}
is an online editor for experimenting with Dart features.

## Creating and Running Programs

Dart source files have a `.dart` file extension.
A `.dart` file can import others using the `import` statement.

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

## Types

Dart supports the following built-in basic types:

- `void`: means a value is never used
- `null`:
- `bool`: boolean value with literal values `true` and `false`
- `int`: 64-bit integer
- `double`: 64-bit floating point number
- `String`: sequence of UTF-16 characters delimited by single or double quotes

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
For example, a variable of type `String?` can be set to `null`,
but a variable of type `String` cannot.

## Variables

Variables are declared with a type or
the `var` keyword to obtain the type through type inference.
For example, these are equivalent:

```dart
int n = 19;
var n = 19;
```

## Print

The `print` function takes a single `String` and writes it to stdout.
Literal `String` values are delimited by single or double quotes.
To include the value of a variable in a `String`, use `$variableName`.
To include the value of an expression in a `String`, use `${expression}`.

## Functions

Dart functions are represented by objects with the type `Function`.
They are first class which means they can be assigned variables,
passed to other functions, and returned from other functions.

Functions have the following syntax:

````dart
return-type fn-name(parameter-list) {
  statements
}
```

Functions that only return the value of a single expression
can use a shorthand syntax where
`=> expression;` is short for `{ return expression; }`.

Specifying parameter types and the return type are optional.
For example, the following is a valid function definition:

```dart
sum(n1, n2) => n1 + n2;
main() => print(sum(2, 3)); // 5
```

Functions can take both position and named arguments.
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
//int product(int n, {required int by}) => n * by;
int product(int n, {int by = 0}) => n * by;

main() {
  print(product(2, by: 3)); // 6
  print(product(4)); // 4
}
```

Trailing commas are allowed after the last parameter in function definitions
and after the last argument in function calls.
This causes `dart format` to place each
parameter or argument on a separate line.

Functions that do not explicitly return a value evaluate to `null`.

## Classes

## Method Cascades

Method cascades provide a way to invoke several methods on the same object.

## Mixins

## Concurrency with Isolates

## Access Specifiers

Dart does not support keywords to indicate access levels
of things like functions, classes, fields, and methods.
Instead add an underscore prefix to a name to indicate that it is private.
This is enforced at the library level.
Private names in a library are not accessible outside the library.
A compiler error is generated if such access is attempted.
However, private names within an application or library
are still accessible by code in the same application or library.

## Classes

Classes defined fields to hold data, constructors to create instances,
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
````

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

## Lists

Arrays in Dart are `List` objects.
A literal array is written as a
comma-separated list of values surrounded by square brackets.
For example, `var numbers = [3, 7, 19];`

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

## Sets

A set is an unordered collection of unique values.
A literal set is written as a
comma-separated list of values surrounded by curly braces.
For example, `var numbers = {3, 7, 19};`

## Maps

A map is a collection of key/value pairs.
The keys and values can have any type.
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

The `dart test` command runs tests.

## Annoyances

- Dart wants many constructor calls to be preceded by the `const` keyword.
  This makes the code verbose.
