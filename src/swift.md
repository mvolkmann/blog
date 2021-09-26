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

{% aTargetBlank "https://developer.apple.com/xcode/", "Xcode" %}
is an IDE from Apple for creating apps for
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

| Command | Description                      |
| ------- | -------------------------------- |
| `:exit` | exits the interpreter            |
| `:help` | prints help on all REPL commands |

## Comments

Single-line comments begin with `//`.
Multi-line comments are delimited by `/*` and `*/`.
Multi-line comments can be nested which makes it easy to
comment out blocks of code that contain multi-line comments.

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

```swift
func greet() {
    print('Hello, World!')
}
greet() // outputs Hello, World!
```

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

### Trailing Closures

TODO: Add this section.
TODO: See https://docs.swift.org/swift-book/LanguageGuide/Closures.html

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

TODO: List some properties and methods of number types.

### Characters and Strings

Literal `Character` and single-line `String` values
are both delimited by double-quotes.
Multi-line `String` values are delimited by triple double-quotes.

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

## Control Structures

Swift does not require parentheses around conditions in control structures.
It does require the code to be executed to be
in a block surrounded by curly braces.

### Conditional Logic

Conditional logic is implemented with the `if` statement.

```swift
if score1 == 21, score2 <= 19 { // same as score1 == 21 && score2 <= 19
  print('Player 1 has won by at least 2.`)
}
```

### Iteration

To iterate over all the elements of an array, use a for-in loop.

```swift
let names = ["Maisey", "Ramsay", "Oscar", "Comet"]
for name in names {
    print(name)
}
```

## Structs

TODO: Add this section.

## Classes

TODO: Add this section.

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

Xcode 13 adds support for Vim key bindings.
To enable this, select Editor ... Vim Mode.

```

```
