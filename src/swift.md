---
eleventyNavigation:
  key: Swift
layout: topic-layout.njk
---

## Overview

THIS IS A WORK IN PROGRESS!

{% aTargetBlank "https://swift.org", "Swift" %}
is an open source programming language created by Apple.
Key facts about Swift include the following:

- introduced by Apple at the 2014 WWDC conference
- became open source under an Apache license in December 2015
- goals are to be safe, fast, and expressive
- strongly typed with type inference
- supports both object-oriented and functional programming
- built on {% aTargetBlank "https://llvm.org", "LLVM" %}
  (Low Level Virtual Machine)
- interoperates with {% aTargetBlank
  "https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/",
  "Objective-C" %} code
- can be used to build applications for macOS, iOS, and Watch OS
- can be used to build command-line applications
- can be used to build server-side applications
- a compiled language, but there is also an interpreter
- supports closures, tuples, and generics
- includes a {% aTargetBlank
  "https://developer.apple.com/documentation/swift/swift_standard_library",
  "Standard Library" %}

## Resources

- main site - {% aTargetBlank "https://swift.org", "https://swift.org" %}
- {% aTargetBlank "https://docs.swift.org/swift-book/",
  "The Swift Programming Language Book" %}
- {% aTargetBlank "https://cs193p.sites.stanford.edu",
  "Stanford CS193p - Developing Apps for iOS" %} course by Paul Hegarty - free
- {% aTargetBlank "https://www.hackingwithswift.com/100", "100 Days of Swift" %}
  by Paul Hudson - free
- {% aTargetBlank "https://www.udemy.com/course/ios-13-app-development-bootcamp/",
  "iOS & Swift - The Complete iOS App Development Bootcamp" %}
  Udemy course by Angela Yu - $14.99
- {% aTargetBlank "https://www.apple.com/swift/playgrounds/",
  "Swift Playgrounds" %} iOS app - free

## Installing

Swift can be installed on macOS, Windows 10, Amazon Linux 2, CentOS, and Ubuntu.

To install Swift on macOS, install Xcode from the macOS App Store.
For other operating systems, download it from
{% aTargetBlank "https://swift.org/download/", "Download Swift" %}.

## Using Xcode

{% aTargetBlank "https://developer.apple.com/xcode/", "Xcode" %}
is an IDE from Apple for creating apps for
iPhone, iPad, Mac, Apple Watch, and Apple TV.

To experiment with Swift:

- select File ... New ... Playground...
- select a template such as "Blank" and press "Next"
- enter a name for the playground
- select the directory where it will be saved
- enter code in the provided text editor
- to run all of the code, click the square at the top of the console area
- to run only the code up to and including a specific line,
  hover over the line and click the play button that appears
- `print` output appears in the console area at the bottom

To developing an app:

- select one of the following:
  - "Create a new Xcode project"
  - "Clone an existing project"
  - "Open a project or file"

## Using the Interpreter

To start the interpreter as a REPL (Read Eval Print Loop), enter `swift`.
Then enter Swift statements to be evaluated.
For example, enter `print(1 + 2)`.

To run the interpreter on lines of code in a file,
enter `swift < file-path`.
For example, create the file `greet.swift` containing `print("Hello, World!")`
and enter `swift < greet.swift` to run it.

Interpreter commands begin with a colon.
The most commonly used commands are described in the table below.

| Command | Description                      |
| ------- | -------------------------------- |
| `:exit` | exits the interpreter            |
| `:help` | prints help on all REPL commands |

## Comments

Single-line comments begin with `//`.
Multi-line comments are delimited by `/*` and `*/`.
Multi-line comments can be nested which makes it easy to
comment out blocks of code that contain multi-line comments.

## Operators

| Category       | Operators                                            |
| -------------- | ---------------------------------------------------- |
| equal          | `==` (equal), `!=` (not equal)                       |
| identical      | `===` (identical), `!==` (not identical)             |
| relational     | `<`, `<=`, `>=`, `>`                                 |
| pattern match  | `~=`                                                 |
| logical        | `&&` (and), `\|\|`, (or), `!` (not)                  |
| ternary        | condition `?` true-value `:` false-value             |
| mathematical   | `+`, `-`, `*`, `/`, `%` (mod)                        |
| assignment     | `=`, `+=`, `-=`, `*=`, `/=`                          |
| concatenation  | `+`                                                  |
| unary sign     | `+` (positive), `-` (negative)                       |
| closed range   | `a...b` (inclusive upper), `a..<b` (exclusive upper) |
| open range     | `a...`, `...b`, `..<b`                               |
| nil-coalescing | a `??` b (value is a if not nil and b otherwise)     |
| bit shift      | `<<`, `>>`, `&<<`, `&>>`                             |
| bitwise        | `&` (and), `\|` (or), `^` (xor)                      |
| types          | `is` (type check), `as`, `as?`, `as!` (type cast)    |

The nil coalescing operator in `a ?? b` is shorthand for `a != nil ? a! : b`.

Swift supports optional chaining so chains of references to optional values
do not have to check for nil values.
See the example in the Structs section.

## Protocols

A protocol is like an interface in other programming languages.
It describes a set of method signatures and properties.
Properties can be constant (`let`) or variable (`var`).

Examples of built-in protocols include `Collection`, `Comparable`,
`Equatable`, `Hashable`, `Identifiable`, `Numeric`, and `Sequence`.

## Functions

Functions are defined using the `func` keyword,
followed by the function name, the parameter list in parentheses,
and an optional return type preceded by `->`.
The parentheses are required even for functions that have no parameters.

When the return type of a function cannot be inferred
and no return type is specified, it defaults to `Void`
which is an empty tuple (`()`).

To call a function, specify the function name
followed by arguments in parentheses.
The parentheses are required even when not passing any arguments.

```swift
func greet() {
    print("Hello, World!")
}
greet() // outputs Hello, World!
```

Parameters must have a name and type.
They can also have an "argument label"
that is the name callers must use when providing a value.
The argument label defaults to the parameter name.
Typically argument labels are omitted
and callers use the parameter names.

When an argument label is underscore,
the function must be called with only a value for that parameter.
Otherwise calls must include the argument label.
A function can have a mixture of parameters with argument labels
and parameters with none (indicated by `_`).

```swift
func add(_ n1: Int, _ n2: Int) -> Int {
    return n1 + n2
}
print(add(2, 3)) // 5
```

If the body of the function is a single expression,
the `return` keyword` is not required to return its value.
The previous function can rewritten as follows:

```swift
func add(_ n1: Int, _ n2: Int) -> Int {
    n1 + n2
}
print(add(2, 3)) // 5
```

If the return type can be inferred, the return type can be omitted.
The previous function can rewritten as follows:

```swift
func add(_ n1: Int, _ n2: Int) {
    n1 + n2
}
print(add(2, 3)) // 5
```

A parameter can specify a default value by including an equal sign (`=`)
followed by a value after the type.
Doing so makes the corresponding argument optional.
All parameters with default values must
follow those that do not have a default value.

Calls to a function must provided all the required arguments.
Each argument is specified by the argument label, a colon, and a value.
These must appear in the same orders as the corresponding parameters.
The rationale for this is that it can make some calls more expressive,
almost like sentences.
It is useful to think of the argument labels
as being part of the function name.

Anonymous functions (a.k.a closures) can be used as
the values of variables and arguments.
They are written with the following syntax:
`{ (parameter-list) -> return-type in statements }`.
The return type can be omitted when it can be inferred.

The following code defines and calls several anonymous functions.

```swift
// This function has no parameters.
let printTime = {
    let date = Date() // now
    let dateFormatter = DateFormatter()
    dateFormatter.dateFormat = "M/d/yyyy"
    print(dateFormatter.string(from: date))
}
printTime()

// This function specifies all the types.
let product = {(a: Double, b: Double) -> Double in a * b}
print(product(2, 3))

// This function omits the return type because it can be inferred.
let product2 = {(a: Double, b: Double) in a * b}
print(product2(2, 3))
```

If the parameter types of an anonymous function can be inferred from usage,
the parameter list can be omitted and the parameter values can be
referred to by index using the names `$0`, `$1`, and so on.

```swift
let numbers = [1, 3, 7]
let doubled = numbers.map({$0 * 2}) // [2, 6, 14]
```

If the last parameters to a function or method are functions,
calls can be written using "trailing closures".
Typically this is only done for the last argument.
For example, these are equivalent:

```swift
let prices = [1.23, 2.34, 3.45]
let total = prices.reduce(0, {(result, price) in result + price})
let total = prices.reduce(0) {
    (result, price) in result + price
}
let total = prices.reduce(0, {$0 + $1})
```

A "variadic" parameter accepts multiple values of the same type.
The parameter value will be a constant array of values.

```swift
func displaySum(label: String, numbers: Int...) {
    let sum = numbers.sum();
    print("\(label) = \(sum)")
}
```

Function names can be overloaded based on their parameter types.

To return multiple values, return a tuple
by returning a list of values inside parentheses.

```swift
func getAvgSum(_ numbers: [Float]) -> (Float, Float) {
    let sum = numbers.reduce(0, {$0 + $1})
    let avg = sum / Float(numbers.count)
    return (avg, sum)
}

print(getAvgSum([1, 3, 8])) // (4.0, 12.0)

// This defines a "tuple" type and assigns a name to it.
typealias RGB = (red: Int, green: Int, blue: Int)

let rgbDict: [String : RGB] = [
    "red": (red: 255, green: 0, blue: 0),
    "green": (red: 0, green: 255, blue: 0),
    "blue": (red: 0, green: 0, blue: 255)
]

func getRgb(_ color: String) -> RGB? {
    return rgbDict[color]
}

print(getRgb("red")) // (red: 255, green: 0, blue: 0)
print(getRgb("green")) // (red: 0, green: 255, blue: 0)
print(getRgb("pink")) // nill
```

By default, parameters cannot be modified in function bodies.
Adding the keyword `inout` before their type changes this.
Modifying the values of these parameters changes
the value of the corresponding argument in the caller.
This only works if the value passed in is a variable (`var`)
rather than a constant (`let`).
This seems like a feature to avoid!

The type of a function is describe by its parameter types and return type.
For example, the type of the `displaySum` function above is
`(String, Int) -> Void`.
Function types can be used for variable, parameter, and return types.
This means that compatible functions can be assigned variables,
passed to functions, and returned from functions.

Functions can defined in the bodies of other functions
to scope their usage.
Otherwise they are global and can be called from anywhere.
Nested functions can be returned by their enclosing function
to allow them to be called from outside.

## Type Aliases

A type alias assigns an alternate name to another type.

One uses is to provide additional documentation for primitive types.
For example, the following documents the expected format of a `String`.

```swift
typealias MMDDYYYY = String
var date: MMDDYYYY = "04161961"
```

The type of an anonymous function can be assigned to a `typealias`
to avoid repeating it and the possibility for typos.
This is also useful for declaring the types of functions
that are passed as arguments to other functions.
For example, the product function above can be written as follows:

```swift
typealias doublePairToDouble = (Double, Double) -> Double
let product: doublePairToDouble = {$0 * $1}
print(product(2, 3))
```

## Built-in Primitive Types

| Type          | Description                                                  |
| ------------- | ------------------------------------------------------------ |
| `Bool`        | boolean; literal values are `true` and `false`               |
| `UInt`        | unsigned integer; same number of bits as platform (32 or 64) |
| `Int`         | signed integer; same number of bits as platform (32 or 64)   |
| `Float`       | 32-bit floating point number                                 |
| `Double`      | 64-bit floating point number                                 |
| `Character`   | single character                                             |
| `String`      | text                                                         |
| `Range`       | interval from inclusive lower bound to exclusive upper bound |
| `ClosedRange` | interval from inclusive lower bound to inclusive upper bound |

## Numbers

The `Int`, `Float`, and `Double` types have some common properties and methods.
But some are not shared by all of these types.

Global numeric constants include:

| Name     | Description             |
| -------- | ----------------------- |
| `M_E`    | Double value of e       |
| `M_PI`   | Double value of pi      |
| `M_PI_2` | Double value of half pi |

Static number properties include the following:

| Property                  | Description                               |
| ------------------------- | ----------------------------------------- |
| `greatestFiniteMagnitude` | maximum value of `Float` or `Double` type |
| `leastNonzeroMagnitude`   | minimum value of `Float` or `Double` type |
| `max`                     | maximum value of `Int` type               |
| `min`                     | minimum value of `Int` type               |
| `pi`                      | value of `Float` or `Double` Pi           |

Static number methods include, but are not limited to the following:

| Method                              | Description                                       |
| ----------------------------------- | ------------------------------------------------- |
| `random(in: Range) -> number`       | returns a random number in the given range        |
| `maximum(number, number) -> Double` | returns the larger of two floating point numbers  |
| `minimum(number, number) -> Double` | returns the smaller of two floating point numbers |

```swift
// Generate a random integer from 0 to 9.
// This can also be used on the Float and Double types.
let r = Int.random(in: 0...10)
```

Number methods include, but are not limited to the following:

| Method                          | Description                                                 |
| ------------------------------- | ----------------------------------------------------------- |
| `abs(number) -> number          | returns absolute value                                      |
| `isMultiple(of: Int) -> Bool    | determines if the Int receiver is a multiple of another Int |
| `remainder(dividingBy: Double)` | returns remainder                                           |
| `round()`                       | mutates value to rounded value                              |
| `rounded() -> Double`           | returns rounded value                                       |
| `signum() -> Int`               | returns -1, 0, or 1                                         |
| `squareRoot()`                  | returns square root                                         |

Many more math functions are defined in the "Foundation" framework.
This "provides a base layer of functionality for apps and frameworks,
including data storage and persistence, text processing,
date and time calculations, sorting and filtering, and networking.
Many math functions can only be used if the Foundation framework is imported.

```swift
import Foundation
print(sin(45.0)) // 0.8509...
```

Numeric foundation functions include:

| Function              | Description                               |
| --------------------- | ----------------------------------------- |
| `abs(number)`         | returns absolute value                    |
| `ceil(number)`        | returns ceiling value                     |
| `floor(number)`       | returns floor value                       |
| `round(number)`       | returns rounded value                     |
| `sign(number)`        | returns -1, 0, or 1                       |
| `trunc(number)`       | returns truncated value                   |
| `pow(base, exponent)` | returns a number raised to a power        |
| `sqrt(number)`        | returns the square root of a number       |
| `exp(exponent)`       | returns e raised to a power               |
| `log(number)`         | returns the natural log of a number       |
| `log10(number)`       | returns the log base 10 value of a number |
| `sin(number)`         | returns sine of angle in radians          |
| `cos(number)`         | returns cosine of angle in radians        |
| `tan(number)`         | returns tangent of angle in radians       |
| `asin(number)`        | returns arc sine in radians               |
| `acos(number)`        | returns arc cosine in radians             |
| `atan(number)`        | returns arc tangent in radians            |

### Characters and Strings

Literal `Character` and single-line `String` values
are both delimited by double-quotes.
Multi-line `String` values are delimited by triple double-quotes.

Strings are value types.
This means that assigning a `String` variable to another
makes a copy rather than assigning a reference to the same memory.

To insert expressions in string values use the string interpolation syntax.

```swift
let item = "milk"
let price = 2.59
let taxRate = 0.8
print("item \(item) costs \(price * (1 + taxRate))")
```

A new string can be created by concatenating existing strings
using the `+` operator.

A string can be appended to an existing variable string
using the `+=` operator.

To iterate over the characters in a `String`,
use a `for-in` loop or the `forEach` method.

```swift
let name = "Mark"

for c in name {
  print(c)
}

name.forEach({(c: Character) -> Void in print(c)})
name.forEach({(c) in print(c)})
```

Indexes into strings have the type `String.Index` rather than `Int`.
This makes many string operations more verbose that in other languages
because obtaining a `String.Index` value requires a method call.

For example, the following gets the 2nd and 3rd characters of a string.

```swift
let name = "Mark"
let start = name.index(name.startIndex, offsetBy: 1)
let end = name.index(start, offsetBy: 1)
print(name[start...end]) // "ar"
```

`String` properties include the following:

| Property  | Description                                     |
| --------- | ----------------------------------------------- |
| `count`   | number of current characters                    |
| `first`   | first character                                 |
| `isEmpty` | `Bool` value indicating whether `count` is zero |
| `last`    | last character                                  |

`String` methods include, but are not limited to the following:

| Method                                       | Description                                                   |
| -------------------------------------------- | ------------------------------------------------------------- |
| `append(Character)`                          | appends a given `Character` to the receiver                   |
| `append(String)`                             | appends a given `String` to the receiver                      |
| `contains(Character) -> Bool`                | determines if receiver contains a given character             |
| `dropFirst(Int) -> Substring`                | returns substring not including first n characters            |
| `dropLast(Int) -> Substring`                 | returns substring not including last n characters             |
| `firstIndex(of: Character or String) -> Int` | returns index of first occurrence of a character or substring |
| `hasPrefix(String) -> Bool`                  | determines if receiver begins with a substring                |
| `hasSuffix(String) -> Bool`                  | determines if receiver ends with a substring                  |
| `insert(Character, at: index)`               | inserts a given `Character` in the receiver                   |
| `lowercased() -> String`                     | returns lowercase version                                     |
| `popLast() -> Character?`                    | removes and returns last character                            |
| `prefix(Int) -> Substring`                   | returns first n characters                                    |
| `remove(at: index)`                          | removes and returns the character at a given index            |
| `removeAll()`                                | removes all characters                                        |
| `removeFirst([n])`                           | removes first n characters, defaulting to 1                   |
| `removeLast([n])`                            | removes last n characters, defaulting to 1                    |
| `removeSubrange(Range)`                      | removes characters in `Range`                                 |
| `replaceSubrange(Range, with: String)`       | replaces characters in a given range                          |
| `split(separator: Character) -> [Substring]` | returns `Array` of substrings delimited by a given character  |
| `sorted() -> [Character]`                    | returns `Array` of characters in sorted order                 |
| `suffix(Int) -> Substring`                   | returns last n characters                                     |
| `uppercased() -> String`                     | returns uppercase version                                     |

Getting the character at a given index requires using the `index` method
which makes if quite verbose.

```swift
let name = "Mark"
print(name[name.index(name.startIndex, offsetBy: 2)]) // "r"
```

Fortunately we can override the subscript operator to make this easier.

```swift
extension String : BidirectionalCollection {
    subscript(i: Index) -> Character { return characters[i] }
}

print(name[2]) // "r"
```

### Ranges

A literal `Range` including the numbers 2, 3, and 4
can be defined with `2..<5` or `2...4`.
This can be assigned to a variable.

```swift
let r = 2...4
```

To determine if a number is in a range, pass it to the `contains` method.

```swift
print(r.contains(3)) // true
print(r.contains(5)) // false
```

To iterate over the values in a range, use a `for-in` loop.

```swift
for n in r {
  print(n)
}
```

## Enumerations

Enumerations are declared with the `enum` keyword.
By convention their names begin with an uppercase letter.
They have a name and a list of possible cases.
Each `case` has a name and an optional value
that can be a string, character, or number.

When values are provided, their type must be specified after the `enum` name.
These values are accessed with the `rawValue` property.
Why doesn't an enum name evaluated to its value like in other languages?

If a type is provided after the `enum` name,
any cases without specified values are given default values.
For example, if the type is `Int`,
incrementing values starting with zero are assigned.
And when the type is `String`,
values matching the case name are assigned.

If no type is provided, the cases are not assigned default values.
This differs from many other programming languages.

Like structs and classes, enumerations can define initializers and methods.
This seems like a misuse of enumerations.

```swift
enum Color {
    // Multiple cases can be specified on the same line.
    case red, green, blue
}

enum ColorHex: String {
    case red = "ff0000"
    case green = "00ff00"
    case blue = "0000ff"
}
```

When the type of a value can be inferred to be a specific `enum` type,
a value can be specified with only a period followed by a `case` name
as a shorthand.

```swift
var c1 = Color.red
print(c1.rawValue) // ff0000
var c2: Color = .red // using shorthand
print(c2.rawValue) // ff0000

switch c1 {
    case .red:
        print("hot") // this prints
    case .green:
        print("warm")
    case .blue:
        print("color")
}
```

If an `enum` has the type `CaseIterable` then
its cases will be held in the `allCases` property.
This can be used to iterate over the cases.
If a value type is also specified, it must appear before `CaseIterable`.

```swift
enum Color: String, CaseIterable {
    case red = "ff0000"
    case green = "00ff00"
    case blue = "0000ff"
}

print(Color.allCases.count) // 3
for color in Color.allCases {
    print(color) // red, green, and blue
}
```

Here is an example of adding a method to an `enum`.

```swift
enum Color: String, CaseIterable {
    case red = "ff0000"
    case green = "00ff00"
    case blue = "0000ff"

    func getGreenHex() -> Substring {
        let hex = self.rawValue
        let start = hex.index(hex.startIndex, offsetBy: 2)
        let end = hex.index(start, offsetBy: 1)
        return hex[start...end]
    }
}

print(Color.red.getGreenHex()) // 00
print(Color.green.getGreenHex()) // ff
```

## Built-in Collection Types

| Type         | Description                                                                                       |
| ------------ | ------------------------------------------------------------------------------------------------- |
| `Array`      | indexed collection of values with the same type                                                   |
| `Dictionary` | collection of key/value pairs where all keys have the same type and all values have the same type |
| `Set`        | unordered collection of values with the same type and no duplicates                               |
| tuple        | fixed-length, ordered collection of values that have type that can differ                         |

When a variable is set to a collection,
the elements in the collection can be modified.
However, when a constant is initialized to a collection,
the elements in the collection cannot be modified.

The class hierarchy of the built-in collections,
including the protocols they implement, is:

- `Sequence` protocol
  - `Collection` protocol
    - `Dictionary` struct
    - `Range` struct
    - `Set` struct
    - `Slice` struct
    - `MutableCollection` protocol
      - `Array` struct

The `Sequence` protocol defines operations for collections that
"provide sequential, iterated access to their elements".
Iterating can be destructive to the elements.

The `Collection` protocol defines operations for collections that
"can be traversed multiple times, nondestructively,
and accessed by an indexed subscript".

The `MutableCollection` protocol defines operations that
change the values of elements.
Methods include `reverse`, `shuffle`, and `sort`.

From "The Swift Programming Language":

> Collections defined by the standard library
> like arrays, dictionaries, and strings
> use an optimization to reduce the performance cost of copying.
> Instead of making a copy immediately, these collections share the memory
> where the elements are stored between the original instance and any copies.
> If one of the copies of the collection is modified,
> the elements are copied just before the modification.
> The behavior you see in your code is always
> as if a copy took place immediately.

### Arrays

Arrays can be created by listing elements in square brackets,
separated by commas.

```swift
var scores = [2, 5] // type is inferred to be [Int]

var numbers: [Int] = [] // can't infer type, so must specify

var numbers: Array<Int> = [] // same as previous line

var zeros = Array(repeating: 0, count: 5) // array containing 5 zeros
```

To append new elements to an array:

```swift
scores += [10, 3] // now [2, 5, 10, 3]
scores.append(9) // can only pass one value; now [2, 5, 10, 3, 9]
```

To change values at specific indexes:

```swift
scores[0] = 4 // now [4, 5, 10, 3, 9]
scores[2...3] = [1, 2] // inclusive range; now [4 5, 1, 2, 9]
```

To iterate over the elements in an `Array`, use a `for`/`in` loop.

```swift
for score in scores {
    print(score)
}
```

To also access the index of each value, use the `enumerated` method.

```swift
for (index, score) in scores.enumerated() {
    print("score \(index + 1): \(score)")
}
```

TODO: Does Swift support any form of destructuring? Probably not.

`Array` properties include the following:

| Property   | Description                                                       |
| ---------- | ----------------------------------------------------------------- |
| `capacity` | number of elements that can be held without allocating more space |
| `count`    | number of current elements                                        |
| `first`    | first element                                                     |
| `isEmpty`  | `Bool` value indicating whether `count` is zero                   |
| `last`     | last element                                                      |

`Array` methods include, but are not limited to the following:

| Method                                                    | Description                                                                                |
| --------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| `allSatisfy((Element) -> Bool) -> Bool`                   | determines if every element satisfies predicate; like JS `every`                           |
| `append(Element)`                                         | adds new element at end                                                                    |
| `append(contentsOf: S)`                                   | adds elements in `S` at end                                                                |
| `compactMap<T>((Element) -> T) -> [T]`                    | returns Array of non-nil values returned by function                                       |
| `contains(Element) -> Bool`                               | determines if an element is a member                                                       |
| `contains(where: (Element) -> Bool) -> Bool`              | determines if some element satisfies predicate; like JS `some`                             |
| `drop(while: (Element) -> Bool) -> ArraySlice<Element>`   | returns subsequence of elements after those at beginning that match predicate              |
| `dropFirst(Int) -> ArraySlice<Element>`                   | returns subsequence of elements after first n                                              |
| `dropLast(Int) -> ArraySlice<Element>`                    | returns subsequence of elements before last n                                              |
| `enumerated(t) -> EnumeratedSequence`                     | returns subsequence of (n, x) pairs where n is an index and x is the element at that index |
| `first(where: (Element) -> Bool) -> Element?`             | returns first element that satisfies predicate                                             |
| `firstIndex(of: Element) -> Int?`                         | returns index of first element matching given element                                      |
| `firstIndex(where: (Element) -> Bool) -> Int?`            | returns index of first element that satisfies predicate                                    |
| `flatMap<T>((Element) -> T) -> [T]`                       | like `map`, but concatenates results                                                       |
| `forEach((Element) -> Void)`                              | passes each element to given function                                                      |
| `last(where: (Element) -> Bool) -> Element?`              | returns last element that matches a predicate                                              |
| `lastIndex(of: Element) -> Int?`                          | returns index of last element matching element                                             |
| `lastIndex(where: (Element) -> Bool) -> Int?`             | returns index of last element that matches a predicate                                     |
| `insert(Element, at: Int)`                                | inserts new element at given index                                                         |
| `joined(String) -> String`                                | returns concatenation of elements with separator between each                              |
| `lazy`                                                    | returns sequence that can be used lazily by another function                               |
| `map<T>((Element) -> T) -> [T]`                           | returns Array of results of calling a function on each element                             |
| `max() -> Element?`                                       | returns maximum element                                                                    |
| `max((Element, Element) -> Bool) -> Element?`             | returns maximum element as determined by a comparator                                      |
| `min() -> Element?`                                       | returns minimum element                                                                    |
| `min((Element, Element) -> Bool) -> Element?`             | returns minimum element as determined by a comparator                                      |
| `partition(by: (Element) -> Bool) -> Int`                 | reorders elements using a predicate so all false are before all true                       |
| `popLast() -> Element?`                                   | removes last element and returns it; returns nil if empty                                  |
| `prefix(Int) -> ArraySlice<Element>`                      | returns subsequence of first n elements                                                    |
| `prefix(while: (Element) -> Bool) -> ArraySlice<Element>` | returns subsequence of first elements that match a predicate                               |
| `randomElement()`                                         | a random element                                                                           |
| `reduce(Result, (Result, Element) -> Result) -> Result`   | takes an initial value and a function; returns result of combining elements                |
| `remove(at: Int) -> Element`                              | removes element at given index and returns it                                              |
| `removeFirst() -> Element`                                | removes first element and returns it                                                       |
| `removeFirst(Int)`                                        | removes given number of elements from beginning                                            |
| `removeLast() -> Element`                                 | removes last element and returns it; crashes if empty                                      |
| `removeLast(Int)`                                         | removes given number of elements from end                                                  |
| `removeSubrange(Range<Int>)`                              | removes elements in range                                                                  |
| `removeAll(keepingCapacity: Bool)`                        | removes all elements, optionally keeping the capacity                                      |
| `removeAll(where: (Element) -> Bool)`                     | removes all elements that match a predicate                                                |
| `replaceSubrange(Range<Int>, with: C)`                    | replaces elements in the range with elements in `C`                                        |
| `reserveCapacity(Int)`                                    | reserves enough space for at least the given number of elements                            |
| `reverse()`                                               | reverses the elements in place                                                             |
| `reversed() -> ReversedCollection`                        | returns view of elements in reverse order                                                  |
| `shuffle()`                                               | shuffles elements in place                                                                 |
| `shuffled()`                                              | returns new array of elements in shuffled order                                            |
| `sort()`                                                  | sorts elements in place                                                                    |
| `sort(by: (Element, Element) -> Bool)`                    | sorts elements in place using a given comparator                                           |
| `sorted() -> [Element]`                                   | returns new array of elements in sorted order                                              |
| `sorted(by: (Element, Element) -> Bool) -> [Element]`     | returns new array of elements in sorted using a given comparator                           |
| `subscript(Int)`                                          | element at given index                                                                     |
| `subscript(Range<Int>)`                                   | `ArraySlice` of elements in given range                                                    |
| `suffix(Int) -> ArraySlice<Element>`                      | returns subsequence of last n elements                                                     |
| `suffix(while: (Element) -> Bool) -> ArraySlice<Element>` | returns subsequence of last elements that match a predicate                                |
| `swapAt(Int, Int)`                                        | swaps elements at the given indexes                                                        |

### Sets

Sets can be created by passing elements to the `Set` function.

```swift
// The type of the following is inferred to be Set<String>.
var cards: Set = ["5C", "KH"] // 5 of clubs and King of hearts

var cards = Set<String>() // starts empty; can't infer type so must specify
```

To add an element, use the `insert` method:

```swift
cards.insert("AD") // Ace of diamonds
```

To iterate over the elements in a `Set`, use a `for`/`in` loop.

```swift
for card in cards { // elements are unordered
    print(card)
}
```

To iterate over the elements in a `Set` in sorted order,
add use of the `sorted` method.

```swift
for card in cards.sorted() {
    print(card)
}
```

The `==` operator can be used to determine if two sets contain the same elements.

`Set` properties include the following:

| Property   | Description                                                       |
| ---------- | ----------------------------------------------------------------- |
| `capacity` | number of elements that can be held without allocating more space |
| `count`    | number of current elements                                        |
| `isEmpty`  | `Bool` value indicating whether `count` is zero                   |

`Set` methods include, but are not limited to the following:

| Method                                       | Description                                                            |
| -------------------------------------------- | ---------------------------------------------------------------------- |
| `allSatisfy((Element) -> Bool) -> Bool`      | determines if every element satisfies predicate; like JS `every`       |
| `contains(Element) -> Bool`                  | determines if an element is a member                                   |
| `contains(where: (Element) -> Bool) -> Bool` | determines if some element satisfies predicate; like JS `some`         |
| `insert(element)`                            | adds an element                                                        |
| `intersection(otherSet) -> Set`              | returns new `Set` that is intersection of receiver with another        |
| `isDisjoint(with: otherSet) -> Bool`         | true if receiver has no elements in common with `otherSet`             |
| `isStrictSuperset(of: seq) -> Bool`          | like `isSuperset`, but `false` when equal                              |
| `isSubset(of: seq) -> Bool`                  | determines if receiver is a subset of a sequence                       |
| `isSuperset(of: seq) -> Bool`                | determines if receiver is a superset of a sequence                     |
| `remove(element)`                            | deletes an element                                                     |
| `subtracting(otherSet) -> Set`               | returns new `Set` containing elements in receiver not also in another  |
| `symmetricDifference(otherSet) -> Set`       | returns new `Set` containing elements in either `Set`, but not in both |
| `union(otherSet) -> Set`                     | returns new `Set` that is union of receiver with another               |

### Dictionaries

Dictionaries are hash tables that hold key/value pairs.
They can be created by listing key/value pairs in square brackets,
separated by commas.
Keys and values are separated by colons.
While typically the keys are strings,
they can be any type that implements the `Hashable` protocol.

```swift
// When the type can't be inferred, it must be specified.
// The syntax for an empty Dictionary includes a colon.
var pairs: [Int : String] = [:] // uses short form of the type

var pairs: Dictionary<Int, String> = [:] // uses long form of the type

// The type of fruitColors is inferred to be [String : String].
var fruitColors = ["apple": "red", "banana": "yellow", "orange": "orange"];
```

To get the value of a given key:

```swift
let color = fruitColors["banana"]; // "yellow"
```

To delete a key pair:

```swift
fruitColors["banana"] = nil
fruitColors.removeValue(forKey: "banana")
```

To iterate over the keys and values in a `Dictionary`, use a `for`/`in` loop.

```swift
for (name, color) in fruitColors {
    print("\(name) is \(color)")
}
```

To iterate over the keys in a `Dictionary`, use a `for`/`in` loop.

```swift
for name in fruitColors.keys { // can add .sorted()
    print("name is \(name)")
}
```

To iterate over the values in a `Dictionary`, use a `for`/`in` loop.

```swift
for color in fruitColors.values { // can add .sorted()
    print("color is \(color)")
}
```

`Dictionary` properties include the following:

| Property   | Description                                                    |
| ---------- | -------------------------------------------------------------- |
| `capacity` | number of pairs that can be held without allocating more space |
| `count`    | number of current elements                                     |
| `isEmpty`  | `Bool` value indicating whether `count` is zero                |
| `keys`     | `Keys` `Collection` of keys                                    |
| `values`   | `Values` `MutableCollection` of values                         |

`Dictionary` methods include, but are not limited to the following:

| Method                                                | Description                                                                     |
| ----------------------------------------------------- | ------------------------------------------------------------------------------- |
| `allSatisfy((Element) -> Bool) -> Bool`               | determines if every element satisfies a predicate; like JS `every`              |
| `contains(where: (Element) -> Bool) -> Bool`          | determines if some element satisfies a predicate; like JS `some`                |
| `filter((Element) -> Bool) -> Bool) -> Dictionary`    | returns new `Dictionary` containing pairs that satisfy predicate                |
| `forEach((Element)) -> Void)`                         | alternate way to iterate over pairs; elements have `key` and `value` properties |
| `map<T>((Element) -> T) -> [T]`                       | returns `Array` of values computed from elements                                |
| `mapValues((Value) -> newValue) -> Dictionary`        | returns new `Dictionary` with same keys but transformed values                  |
| `removeAll()`                                         | removes all pairs                                                               |
| `removeValue(forKey: key) -> prevValue`               | removes pair with given key and returns previous value or `nil`                 |
| `sorted(by: (Element, Element) -> Bool) => [Element]` | returns `Array` of elements sorted using a comparator function                  |
| `updateValue(newValue, forKey: key) -> prevValue`     | sets value of given key and returns previous value or `nil`                     |

Here are examples of using some of these methods.

```swift
let scoreDict = ["Mark": 19, "Tami": 20, "Amanda": 18, "Jeremy": 17]
let allOdd = scoreDict.allSatisfy({$0.value % 2 == 1}) // false
let someOdd = scoreDict.contains(where: {$0.value % 2 == 1}) // true
let odds = scoreDict.filter({$0.value % 2 == 1}) // includes Mark & Jeremy elements
let upperNames = scoreDict.map({$0.key.uppercased()}) // array of uppercase names
let doubledScoresDict = scoreDict.mapValues({$0 * 2}) // scoreDict w/ doubled scores
//scoreDict.removeValue(forKey: "Mark")
let sorted = scoreDict.sorted(by: {$0.value > $1.value}); // descending
```

### Tuples

To define a tuple type, provide a list of elements types in parentheses.
These can be named or unnamed.

```swift
typealias MyUnnamedTuple = (Bool, Int, String)
let t1: MyUnnamedTuple = (true, 19, "Mark")
print(t1.1) // 19

typealias MyNamedTuple = (happy: Bool, score: Int, name: String)
let t2: MyNamedTuple = (happy: true, score: 19, name: "Mark")
print(t2.score) // 19
```

## Variables

Variables are declared with the
`let` (immutable) and `var` (mutable) keywords.
Variable names can be followed by a colon and a type.
They can also be followed by `=` and an initial value.
The type can be omitted if the desired type
can be inferred from the initial value.

```swift
let name1: String = "Mark" // type and initial value

let name2 = "Mark" // only value; String type is inferred

var score1: Int // only type; can't use until initialized
score1 = 19 // initializes

var score2 = 19 // only value; Int type is inferred
```

## Optionals

Variables must be assigned a value before they are accessed
unless they have an optional type indicated by a `?` after the type name.
This allows the value to be `nil`.

```swift
var message: String? // optional type

// Tests for a value AND unwraps into another variable if not nil.
// It then executes a block of code depending on whether the value was nil.
if let msg = message {
    print(msg) // doesn't print
} else {
    print("no message") // does print
}

// Print the message if it is set, otherwise do nothing.
func printMessage(_ message: String?) {
    guard let msg = message else { return }
    print(msg)
}

message = "Hello, World!"
// The ! operator does a "force unwrap" of an optional.
// If the value is nil, the program crashes.
// But here we test for nil before using the force unwrap.
print(message == nil ? "no message" : message!) // "Hello, World!"
printMessage(message) // "Hello, World!"

message = nil
print(message == nil ? "no message" : message!) // "no message"
printMessage(message) // no output
```

## Control Structures

Swift does not require parentheses around conditions in control structures.
It does require the code to be executed to be
in a block surrounded by curly braces.

### Conditional Logic

Conditional logic is implemented with the `if` statement
that can optionally include `else if` and `else` parts.
Curly braces are required around all code blocks,
even if they only contain a single statement.

```swift
if score1 == 21, score2 <= 19 { // same as score1 == 21 && score2 <= 19
  print("Player 1 has won by at least 2.")
} else if score1 > score2 {
  print("Player 1 is leading.")
} else {
  print("Player 1 is not leading.")
}
```

To compare an expression against multiple values, use a `switch` statement.
The expression can have any kind of value
including enums and collections such as tuples.
Unlike in many other languages, after executing the statements in the
first matching `case` execution does not fall through to the next `case`.
No `break` statement is needed to prevent this.
If falling through is desired, add a `fallthrough` statement
at the end of a `case` block.

```swift
switch computeScore(player1) {
    case 21:
        print("Winner!");
    case 18...20:
        print("Very good hand.")
    default:
        print("Will likely lose.")
}
```

If the `default` case is omitted, there must be a `case`
that matches every possible value of the expression
(i.e. it must be exhaustive).
If the value of the `switch` expression is an `enum`,
there must be a `case` that matches each value of the `enum`.

### Iteration

To iterate over all the elements of a sequence (such as an array),
use a for-in loop.

```swift
let names = ["Maisey", "Ramsay", "Oscar", "Comet"]
for name in names { // variable name does not need to be declared
    print(name)
}
```

To iterate while a condition is true, use a `while` loop which is top-tested.

```swift
while condition {
    statements
}
```

For a bottom-tested loop that always runs at least once, use a `repeat` loop.

```swift
repeat {
    statements
} while condition
```

## Structs

Structs define named groups of properties and methods.
By convention their names begin with an uppercase letter.
They can optionally conform to protocols which are
similar to interfaces in other languages.

Properties are declared with the `let` (immutable) and
`var` (mutable) keywords, just like variables.
Mutable properties can only be modified if the instance is also mutable
(assigned to a `var`).

Immutable properties must be given a value when an instance is initialized
and cannot be changed after that.

Here is an example of a simple struct definition.

```swift
struct Dog {
    var breed: String
    var name: String
    var age: Int
}
```

In structs that do not define an initializer (`init` method),
a default memberwise initializer is provided.
This has argument labels that match the property names
and are in the order in which the properties are defined.
No default memberwise initializer is provided for classes.

An instance of a struct or class is created by
calling the struct name as a function,
passing arguments required by an initializer.
Note that unlike in many other languages,
use a `new` keyword is not required.

```swift
// Create an instance using the provided memberwise initializer.
var dog = Dog(breed: "Whippet", name: "Comet", age: 1)
print("\(dog.name) is a \(dog.breed)") // Comet is a Whippet
```

If all of the properties are optional or have a default value,
a default initializer that takes no arguments is provided.

Structs are value types. This means that assigning one to a variable
creates a copy rather than assigning a reference to the same instance.

````swift
var dog2 = dog // dog2 is a copy of dog.
dog.age = 2 // This change doesn't affect dog2.
print(dog.age, dog2.age) // 2 1
```

A computed property is defined with a `get` function
that computes the value each time it is referenced.
It can optionally define a `set` function
whose purpose is the change the values of properties used to
compute the value so the result will be a given value.
Typically it doesn't make sense to define
the `set` function for a computed property.

A lazy property is similar to a computed property,
but its value is only computed the first time it is accessed.
This is ideal for expensive computations that may not be accessed.
It must be declared with `var` and
can be mutated after its initial value is computed.
Lazy properties are not thread safe and will be computed again in each thread.

Methods are defined with the `func` keyword.
They can use the `self` keyword to refer the instance on which they are invoked.

```swift
import Foundation // needed to use functions like sin, cos, and atan
// Importing UIKit imports Foundation for you.

struct Dog {
    var breed: String
    var name: String
    var age = 0 // has a default value; type is inferred to be Int
}

//var dog = Dog(breed: "Whippet", name: "Comet") // uses default value for age
var dog = Dog(breed: "Whippet", name: "Comet", age: 1)
print("\(dog.name) is a \(dog.age) year old \(dog.breed)")

var dog2 = dog
dog.age = 2
print(dog.age, dog2.age) // 2 1

struct Point {
    // This is a "type property".
    // Other languages refer to this as a "class property".
    // This is used to keep track of the largest y value
    // ever assigned to any instances of a Point struct.
    static var maxY = -Double.greatestFiniteMagnitude

    // This is a basic property.
    var x: Double

    // This property has a property observer.
    var y: Double {
        willSet {
            print("y is about to change to \(newValue)")
        }
        didSet {
            Point.maxY = Double.maximum(Point.maxY, y)
            print("y changed from \(oldValue) to \(y)")
        }
    }

    // A "memberwise initializer" is automatically supplied.
    // Writing this yourself enables doing more
    // than just assigning property values.
    init(x: Double, y: Double) {
        // self. is only needed here because there is a
        // parameter name with the same name as the property being set.
        self.x = x
        self.y = y
        Point.maxY = Double.maximum(Point.maxY, y)
    }

    // This is a basic computed property.
    // Its value is computed by a function every time it is referenced.
    var distanceFromOrigin: Double {
        (x*x + y*y).squareRoot()
    }

    // This is a computed property that uses the full syntax
    // that supports defining "get" and "set" functions.
    var distanceFromOrigin2: Double {
        get {
            //return (x*x + y*y).squareRoot()
            // If the body is a single expression, return can be omitted.
            (x*x + y*y).squareRoot()
        }
        // This moves the point along its current angle from the origin
        // to the new distance.
        /*
        set(distance) {
            let angle = atan(y / x)
            x = distance * cos(angle)
            y = distance * sin(angle)
        }
        */
        // If no name is specified for the new value, it defaults to newValue.
        set {
            let angle = atan(y / x)
            x = newValue * cos(angle)
            y = newValue * sin(angle)
        }
    }

    // This is a lazy property.
    // Its value is computed by a function only the first time it is referenced.
    lazy var initialDistance: Double = {
        print("computing lazy property")
        return (x*x + y*y).squareRoot()
    }() // note that the function is being called

    // This is an instance method.
    func log() {
        print("(\(x), \(y))")
    }

    // This is a mutating instance method.
    // In a class, var properties can be modified in instance methods.
    // In a struct they cannot unless the method is declared to be "mutating".
    // The underscores mean that this method can be called
    // with raw values, not including argument labels.
    mutating func translate(_ dx: Double, _ dy: Double) {
        x += dx
        y += dy
    }
}

var pt = Point(x: 3, y: 4) // creates an instance
print(pt.distanceFromOrigin) // invokes computed property get function; 5
print("initial distance =", pt.initialDistance) // 5
pt.distanceFromOrigin = 10 // invokes computed property set function
pt.log() // invokes instance method; (6, 8)
print(pt.distanceFromOrigin2) // invokes computed property; 10
print("initial distance =", pt.initialDistance) // 5

pt.y = 2 // invokes a property observer

pt.log() // (6, 2)
pt.translate(-4, 3)
pt.log() // (2.0, 5.0)
pt.translate(1, 2)
print("initial distance =", pt.initialDistance) // still 5
print(Point.maxY) // 8
let pt2 = Point(x: 0, y: 9) // creates a second instance
print(Point.maxY) // 9
```

Swift supports optional chaining so chains of references to optional values
do not have to check for nil values.
The result is either an `Optional` value or `nil`.

```swift
struct Address {
    var street: String
    var city: String
    var state: String
    var zip: String
}

struct Person {
    var name: String
    var address: Address?
}

var a = Address(street: "123 Some Lane", city: "Somewhere", state: "MO", zip: "12345")
var p: Person? = Person(name: "Mark", address: a)

print(p?.address?.zip as Any) // Optional(12345)

if let zip = p?.address?.zip {
    print(zip) // 12345
} else {
    print("no zip")
}

p!.address = nil
print(p?.address?.zip as Any) // nil

p = nil
print(p?.address?.zip as Any) // nil
print(p?.address?.zip ?? "no zip") // alternate way to handle optional; "no zip"
```

## Classes

Classes are similar to structs in many ways.
By convention their names begin with an uppercase letter.

Classes can do the following things that structs cannot.

- refer to instances by reference rather than making a copy
- inherit properties and methods from another
- use type casting to determine if an object is an instance at runtime
- define a "deinitializer" to perform cleanup when an instance is destroyed

Classes are reference types. This means that assigning one to a variable
assigns a reference to the same instance rather than making a copy.

To define a class, use the `class` keyword.
A class can have:

- one or more initializers (named `init`)
- class-level properties declared with `static`
- class-level methods declared with `static`
- instance-level properties
- instance-level methods
- a superclass

Why don't classes get a default memberwise initializer like structs?

Let's re-implement the "Point" struct as a class named "Point2".

```swift
class Point2 {
    static var maxY = -Double.greatestFiniteMagnitude
    var x: Double
    var y: Double {
        willSet {
            print("y is about to change to \(newValue)")
        }
        didSet {
            Point2.maxY = Double.maximum(Point2.maxY, y)
            print("y changed from \(oldValue) to \(y)")
        }
    }

    init(x: Double, y: Double) {
        self.x = x
        self.y = y
        Point2.maxY = Double.maximum(Point2.maxY, y)
    }

    var distanceFromOrigin: Double {
        (x*x + y*y).squareRoot()
    }

    var distanceFromOrigin2: Double {
        get {
            (x*x + y*y).squareRoot()
        }
        set {
            let angle = atan(y / x)
            x = newValue * cos(angle)
            y = newValue * sin(angle)
        }
    }

    lazy var initialDistance: Double = {
        (x*x + y*y).squareRoot()
    }() // note that the function is being called

    func log() {
        print("(\(x), \(y))")
    }

    // Note that the "mutating" keyword was removed
    // because that is only used on struct methods.
    func translate(_ dx: Double, _ dy: Double) {
        x += dx
        y += dy
    }
}
```

To implement a class (subclass) that inherits
the properties and methods of another class (superclass),
add a colon after the subclass name followed by the superclass name.

```swift
class Person {
    var name: String

    init(name: String) {
        self.name = name
    }

    func log() {
        print("\(name) is a person.")
    }
}

class Programmer : Person {
    var languages: [String]

    init(name: String, languages: [String]) {
        // Must initialize all properties of this class
        // before calling super.init.
        self.languages = languages

        // The "super" keyword is used to call methods in the superclass.
        // Subclasses must have an "init" method that
        // calls an "init" method of the superclass.
        super.init(name: name)
    }

    // The "override" keyword is required to
    // override methods in the superclass.
    override func log() {
        let langs = languages.joined(separator: " & ")
        print("\(name) is a programmer that knows \(langs).")
    }
}

var mark = Programmer(name: "Mark", languages: ["JavaScript", "Swift"])
mark.log() // Mark is a programmer that knows JavaScript & Swift.
```

## Initializers

Structs and classes can define `init` methods that initialize
ALL of their properties that do not have default values.
These are referred to as "designated initializers".
Designated initializers for classes
must also initialize all inherited properties.

As seen earlier, structs provided a default initializer.
Structs are not required to explicitly defining more.
Classes do not provide a default initializer
and at least one must be explicitly defined.

There can be more than one `init` method as long as each has a
different set of argument labels and initializes all of the properties.
Classes typically only define one designated initializer.

If a class inherits from another,
its designated initializers must call one in its immediate superclass.

Just like with functions and other kinds of methods,
using `_` for an argument label allows its value to be passed without a label.

"Convenience initializers" are `init` methods that invoke another
designated or convenience initializer in the same struct or class.
These must be labeled with the `convenience` keyword.
Why doesn't Swift allow any `init` method to do this?
Congratulations Swift for having what may be
the longest keyword in any programming language!

"Failable initializers" are `init?` methods that can return `nil`
to prevent an instance from being created
if the supplied arguments are deemed invalid.
An optional value is returned when an instance is created
using a failable initializer.
As with any optional value, callers must
test and unwrap the value in order to use it.

Deinitializers are methods named `deinit`,
If a struct or class defines this method,
it will be called when any instance is destroyed.
It is used to perform cleanup.

I have two issues with the method name `deinit`.
First, "deinitialize" is not a word.
Second, the method doesn't necessarily only
clean up after actions taken in an initializer.
It can perform cleanup of actions taken by any method.
A better name would have been "cleanup" or "onDestroy".

## Optional Properties

Properties of structs and classes whose values are allowed to be `nil`
should have a `?` at end of their type.
Just like with optional variables, they must be unwrapped to access their value.

## Access Control

Swift supports many keywords for controlling access
to values like functions, structs, classes, and
and the properties and methods of structs and classes.
These keywords appear at the beginning of declarations for these kinds of values.

The access control keywords include:

- `open`: access from anywhere; only for classes and class members
- `public`: same as `open` except cannot be subclasses or overridden
- `internal`: access from any source in in the same module (default level)
- `fileprivate`: access from code in the same source file
- `private`: access within enclosing declaration (such as a struct or class)

## Tools

TODO: Is there an equivalent of ESLint for Swift?

To format Swift code in Xcode, select the lines to be formatted (cmd-a for all)
and press ctrl-i.

Some options for Swift code formatting are
{% aTargetBlank "https://github.com/nicklockwood/SwiftFormat", "SwiftFormat" %}
and {% aTargetBlank "https://github.com/apple/swift-format", "swift-format" %}.

To install SwiftFormat, enter `brew install swiftformat`.
To format a `.swift` file, enter `swiftformat file-name.swift`.
To format all the `.swift` files in the current directory,
enter `swiftformat *.swift`.

### Xcode

Xcode 13 adds support for Vim key bindings, but it is very basic.
It does not support repeating commands with the period key,
defining macros, and other more advanced Vim features.
To enable this, select Editor ... Vim Mode.

## Annoyances

I will add to this list as I learn more.

The things that annoy me most about Swift are:

- String interpolation syntax

  They should have copied the `${expression}` syntax already in use by other
  languages instead of using the `\(expression)` syntax that is unique to Swift.

- Defaulting to named parameters

  Arguments to most functions and methods are specified with
  just a value (positionally) rather than using argument labels.
  But that is only allowed for argument labels
  that are specified to be an underscore.
  The common case should be the default.

- Trailing closures

  The syntax for trailing closures is surprising and unique to Swift.
  It is even more surprising if there is more than one.

- Going too far

  Swift has many good features that it stretches too far.
  There are too many examples to list them all, but here are some:

  - enumerations can have initializers and methods
  - enumerations support "recursive enumerations"
````
