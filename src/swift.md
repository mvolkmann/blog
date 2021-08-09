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

Xcode is an IDE from Apple for creating apps for
iPhone, iPad, Mac, Apple Watch, and Apple TV.

To experiment with Swift:

- select File ... New Playground...
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

| Command | Description           |
| ------- | --------------------- |
| `:exit` | exits the interpreter |
| `:help` | prints                |

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
| `Range`       | interval from lower bound to upper bound (exclusive)         |
| `ClosedRange` | interval from lower bound to upper bound (inclusive)         |

### Characters and Strings

Literal `Character` and single-line `String` values
are both delimited by double-quotes.
Multi-line `String` values are delimited by triple double-quotes.

### Ranges

A literal `Range` including the numbers 2, 3, and 4
can be defined with `2..<5` or `2...4`.
This can be assigned to a variable. For example:

```swift
let r = 2...4
```

To determine if a number is in a range, pass it to the `contains` method.
For example:

```swift
print(r.contains(3)) // true
print(r.contains(5)) // false
```

To iterate over the values in a range, use a `for-in` loop. For example:

```swift
for n in r {
  print(n)
}
```

## Built-in Collection Types

| Type         | Description                                                               |
| ------------ | ------------------------------------------------------------------------- |
| `Array`      | indexed collection of values with the same type                           |
| `Dictionary` | collection of key/value pairs                                             |
| `Set`        | unordered collection of values with the same type and no duplicates       |
| tuple        | fixed-length, ordered collection of values that have type that can differ |

### Arrays

Array properties include the following:

| Property   | Description                                                       |
| ---------- | ----------------------------------------------------------------- |
| `capacity` | number of elements that can be held without allocating more space |
| `count`    | number of elements                                                |
| `first`    | first element                                                     |
| `isEmpty`  | `Bool` value indicating whether `count` is zero                   |
| `last`     | last element                                                      |

Array methods include, but are not limited to the following:

| Method                                                    | Description                                                                                  |
| --------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| `allSatisfy((Element) -> Bool) -> Bool`                   | determines if all elements match a predicate                                                 |
| `append(Element)`                                         | adds a new element at the end                                                                |
| `append(contentsOf: S)`                                   | adds the elements in `S` at the end                                                          |
| `compactMap<T>((Element) -> T) -> [T]`                    | returns an array of the non-nil values returned by the function                              |
| `contains(Element) -> Bool`                               | determines if an element is a member                                                         |
| `contains(where: (Element) -> Bool) -> Bool`              | determines if any element matches a predicate                                                |
| `drop(while: (Element) -> Bool) -> ArraySlice<Element>`   | returns a subsequence of the elements after those at the beginning that match a predicate    |
| `dropFirst(Int) -> ArraySlice<Element>`                   | returns a subsequence of the elements after the first n                                      |
| `dropLast(Int) -> ArraySlice<Element>`                    | returns a subsequence of the elements before the last n                                      |
| `enumerated(t) -> EnumeratedSequence`                     | returns a subsequence of (n, x) pairs where n is an index and x is the element at that index |
| `first(where: (Element) -> Bool) -> Element?`             | returns the first element that matches a predicate                                           |
| `firstIndex(of: Element) -> Int?`                         | returns the index of the first element matching element                                      |
| `firstIndex(where: (Element) -> Bool) -> Int?`            | returns the index of the first element that matches a predicate                              |
| `flatMap<T>((Element) -> T) -> [T]`                       | like `map`, but concatenates the results                                                     |
| `forEach((Element) -> Void)`                              | passes each element to a given function                                                      |
| `last(where: (Element) -> Bool) -> Element?`              | returns the last element that matches a predicate                                            |
| `lastIndex(of: Element) -> Int?`                          | returns the index of the last element matching element                                       |
| `lastIndex(where: (Element) -> Bool) -> Int?`             | returns the index of the last element that matches a predicate                               |
| `insert(Element, at: Int)`                                | inserts a new element at the given index                                                     |
| `joined(String) -> String`                                | returns the contenation of the elements with a separator between each                        |
| `lazy`                                                    | returns a sequence that can be used lazily by another function                               |
| `map<T>((Element) -> T) -> [T]`                           | returns an Array of the results of calling a function on each element                        |
| `max() -> Element?`                                       | returns the maximum element                                                                  |
| `max((Element, Element) -> Bool) -> Element?`             | returns the maximum element as determined by a comparator                                    |
| `min() -> Element?`                                       | returns the minimum element                                                                  |
| `min((Element, Element) -> Bool) -> Element?`             | returns the minimum element as determined by a comparator                                    |
| `partition(by: (Element) -> Bool) -> Int`                 | reorders the elements using a predicate so all false are before all true                     |
| `popLast() -> Element?`                                   | removes the last element and returns it                                                      |
| `prefix(Int) -> ArraySlice<Element>`                      | returns a subsequence of the first n elements                                                |
| `prefix(while: (Element) -> Bool) -> ArraySlice<Element>` | returns a subsequence of the first elements that match a predicate                           |
| `randomElement()`                                         | a random element                                                                             |
| `reduce(Result, (Result, Element) -> Result) -> Result`   | takes an initial value and a function; returns result of combining elements                  |
| `remove(at: Int) -> Element`                              | removes the element at a given index and returns it                                          |
| `removeFirst() -> Element`                                | removes the first element and returns it                                                     |
| `removeFirst(Int)`                                        | removes a given number of elements from the beginning                                        |
| `removeLast() -> Element`                                 | removes the last element and returns it                                                      |
| `removeLast(Int)`                                         | removes a given number of elements from the end                                              |
| `removeSubrange(Range<Int>)`                              | removes the elements in the range                                                            |
| `removeAll(keepingCapacity: Bool)`                        | removes all elements, optionally keeping the capacity                                        |
| `removeAll(where: (Element) -> Bool)`                     | removes all elements that match a predicate                                                  |
| `replaceSubrange(Range<Int>, with: C)`                    | replaces elements in the range with elements in `C`                                          |
| `reserveCapacity(Int)`                                    | reserves enough space for at least the given number of elements                              |
| `reverse()`                                               | reverses the elements in place                                                               |
| `reversed() -> ReversedCollection`                        | returns a view of the elements in reverse order                                              |
| `shuffle()`                                               | shuffles the elements in place                                                               |
| `shuffled()`                                              | returns a new array of the elements in shuffled order                                        |
| `sort()`                                                  | sorts the elements in place                                                                  |
| `sort(by: (Element, Element) -> Bool)`                    | sorts the elements in place using a given comparator                                         |
| `sorted() -> [Element]`                                   | returns a new array of the elements in sorted order                                          |
| `sorted(by: (Element, Element) -> Bool) -> [Element]`     | returns a new array of the elements in sorted using a given comparator                       |
| `subscript(Int)`                                          | element at a given index                                                                     |
| `subscript(Range<Int>)`                                   | `ArraySlice` of the elements in the given range                                              |
| `suffix(Int) -> ArraySlice<Element>`                      | returns a subsequence of the last n elements                                                 |
| `suffix(while: (Element) -> Bool) -> ArraySlice<Element>` | returns a subsequence of the last elements that match a predicate                            |
| `swapAt(Int, Int)`                                        | swaps the elements at the given indexes                                                      |

Do `popLast` and `removeLast` have the same functionality?

### Dictionaries

### Sets

### Tuples

To define a tuple type, provide a list of elements types in parentheses.
For example, `(Bool, Int, String)`.

## Variables

Variables are declared with the
`let` (immutable) and `var` (mutable) keywords.
Variable names can be followed by a colon and a type.
They can also be followed by `=` and an initial value.
The type can be omitted if the desired type
can be inferred from the initial value.

For example:

```swift
let name1: String = "Mark" // type and initial value

let name2 = "Mark" // only value; String type is inferred

var score1: Int // only type; can't use until initialized
score1 = 19 // initializes

var score2 = 19 // only value; Int type is inferred
```

## Control Structures

Swift does not require parentheses around conditions in control structures.
It does require the code to be executed to be
in a block surrounded by curly braces.

### Conditional Logic

Conditional logic is implemented with the `if` statement.
For example:

```swift
if score1 == 21, score2 <= 19 { // same as score1 == 21 && score2 <= 19
  print('Player 1 has won by at least 2.`)
}
```

### Iteration

To iterate over all the elements of an array, use a for-in loop.
For example:

```swift
let names = ["Maisey", "Ramsay", "Oscar", "Comet"]
for name in names {
    print(name)
}
```

## Functions

Functions are defined using the `func` keyword,
followed by the function name, the parameter list in parentheses,
and an optional return type preceded by `->`.
The parentheses are required even for functions that have no parameters.
For example:

```swift
func add(n1: Int, n2: Int) -> Int {
    return n1 * n2
}
print(add(2, 3)) // 5
```

If the body of the function is a single expression,
the `return` keyword` is not required to return its value.
The previous function can rewritten as follows:

```swift
func add(n1: Int, n2: Int) -> Int {
    n1 * n2
}
print(add(2, 3)) // 5
```

If the return type can be inferred, the return type can be omitted.
The previous function can rewritten as follows:

```swift
func add(n1: Int, n2: Int) {
    n1 * n2
}
print(add(2, 3)) // 5
```

When the return type of a function cannot be inferred
and no return type is specified, it defaults to `Void`
which is an empty tuple (`()`).

Parameters must have a name and type.
They can also have an "argument label"
that is the name callers must use when providing a value.
The argument label defaults to the parameter name.
Typically argument labels are omitted
and callers use the parameter names.

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

When an argument label of `_` is specified,
the function must be called with only a value for that parameter.
Otherwise calls must include the argument label.
A function can have a mixture of parameters with argument labels
and parameters with none (indicated by `_`).

To call a function, specify the function name
followed by arguments in parentheses.
The parentheses are required even when not passing any arguments.

For example:

```swift
func greet() {
    print('Hello, World!')
}
greet() // outputs Hello, World!
```

Anonymous functions can be used at the values of variables and arguments.
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

// This function has parameters with unspecified types.
// It does not compile because the * operator cannot be applied to all types.
//let product3 = {a, b in a * b}
//print(product3(2, 3))
// This is a very contrived example since
// "print(2 * 3)" would do the same thing.
print({$0 * $1}(2, 3))
```

A "variadic" parameter accepts multiple values of the same type.
The parameter value will be a constant array of values.
For example:

```swift
func displaySum(label: String, numbers: Int...) {
    let sum = numbers.sum();
    print("\(label) = \(sum)")
}
```

Function names can be overloaded based on their parameter types.

To return multiple values, return a tuple
by returning a list of values inside parentheses.
For example:

```swift
TODO: Write a function that takes a color and returns a tuple
TODO: containing the amounts of red, green, and blue in the color.
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

## Protocols

A protocol is like an interface in other programming languages.
It describes a set of method signatures.
TODO: Can it also describe constants and fields?

- `Collection`
- `CustomDebugStringConvertible`
- `CustomReflectable`
- `Equatable`

## Tools

Is there an equivalent of ESLint for Swift?

Some options for Swift code formatting are
{% aTargetBlank "https://github.com/nicklockwood/SwiftFormat", "SwiftFormat" %}
and {% aTargetBlank "https://github.com/apple/swift-format", "swift-format" %}.

To install SwiftFormat, enter `brew install swiftformat`.
To format a `.swift` file, enter `swiftformat file-name.swift`.
To format all the `.swift` files in the current directory,
enter `swiftformat *.swift`.

### Xcode

Xcode 13 (still in beta as of 8/8/21) adds support for Vim key bindings.
To enable Vim key bindings, select
Preferences ... Text Editing ... Editing ... Enable Vim key bindings.
