---
eleventyNavigation:
  key: Why Swift?
  parent: Swift
  order: 1
layout: topic-layout.njk
---

<img alt="Swift logo" style="width: 30%"
  src="/blog/assets/swift-logo.png?v={{pkg.version}}"
  title="Swift logo">

# What makes Swift a great programming language?

This article highlights many features of the {% aTargetBlank
"https://developer.apple.com/swift/", "Swift" %} programming language
that make it an attractive alternative to other programming languages.
For a more complete introduction to Swift, see {% aTargetBlank
"https://docs.swift.org/swift-book/documentation/the-swift-programming-language/guidedtour/",
"A Swift Tour" %}.

My goal is to provide clear, concise descriptions of a large number
of fundamental Swift concepts along with short, illustrative code snippets.
If you are familiar with at least one programming language
that supports object-oriented concepts, I'm confident this will be
enough to get you started reading and writing Swift code.

Swift is supported in macOS, Linux, and Windows.
For details, see {% aTargetBlank
"https://www.swift.org/platform-support/", "Platform Support" %}.

Today Swift is primarily used for implementing applications that
run on iPhones, iPads, Apple Watches, and Mac computers.
However, it can also be used to implement server-side code,
such as REST services. One way to do this is to utilize the
{% aTargetBlank "https://vapor.codes", "Vapor" %} framework.

It is also possible to develop Android apps using Swift
with the Swift Compiler for Android Development Environment
({% aTargetBlank "https://www.scade.io/10-scade2beta/", "SCADE" %}).

There are many open source examples of apps developed using Swift and the
{% aTargetBlank "https://developer.apple.com/xcode/swiftui/", "SwiftUI" %}
framework. For a basic example,
see my Running Calculator app for watchOS in {% aTargetBlank
"https://github.com/mvolkmann/swiftui-running-calculator/blob/main/RunningCalculator%20WatchKit%20Extension/RunningCalculatorApp.swift",
"GitHub" %}.

## Functions

Functions are defined with the `func` keyword followed by
a name, parameter list, optional return type, and body.

In functions with a return type where the body contains a single expression,
the `return` keyword is implied to precede it.

Functions that do not specify a return type cannot return a value.

Each parameter is described by an optional argument label,
a parameter name, a type, and an optional default value.
Argument labels are used in calls to functions
to specify the values being passed.
Parameter names are used to access the values inside the function body.

The argument label for each parameter has three possibilities:

- omitted, defaulting to the same as the parameter name
- a name other than an underscore
- an underscore, which makes it positional

To demonstrate these options, here are four ways to define a
simple function that returns the result of multiplying two numbers.

```swift
// No parameters have argument labels.
func multiplyV1(first: Double, second: Double) -> Double {
    first * second
}
print(multiplyV1(first: 2, second: 3)) // 6

// All parameters have argument labels.
func multiplyV2(number first: Double, by second: Double) -> Double {
    first * second
}
print(multiplyV2(number: 2, by: 3)) // 6

// The first parameter is positional and the second has an argument label.
// Positional parameters can appear before and/or after named parameters.
func multiplyV3(_ first: Double, by second: Double) -> Double {
    first * second
}
print(multiplyV3(2, by: 3)) // 6

// All parameters are positional.
func multiplyV4(_ first: Double, _ second: Double) -> Double {
    first * second
}
print(multiplyV4(2, 3)) // 6
```

The purpose of argument labels is to
make calls to functions read in a more English-like manner.
Typically argument labels are prepositions such as "at", "between", "by",
"for", "from", "in", "inside", "of", "on", "to", or "with".

The arguments in a function call must appear
in the same order in which they are defined
in order to support being read by developers in the expected way.
For example, later we will see the definition of a `Person` class
that defines a `marry` method.
Here's an example of a call to this method:

```swift
personA.marry(spouse: personB, on: date)
```

Reading "marry spouse on" feels correct,
but reading "marry on spouse" would not.

Positional parameters are used far less frequently
than named parameters in typical Swift code.

Parameters can be given default values.
This allows them to be omitted in calls.
However, specifying a default value for a parameter
does not remove the need to specify its type.

For example:

```swift
func greet(salutation: String = "Hello", name: String = "World") {
    print(salutation, name)
}
greet() // Hello World
greet(name: "Mark") // Hello Mark
greet(salutation: "Hola") // Hola World
greet(salutation: "Hola", name: "Mark") // Hola Mark
```

## Control Structures

Many programming languages have a syntax that is
modeled after the C programming language where
conditions are surrounded by parentheses and
statements are terminated by semicolons.
Swift does not follow either of those conventions.
It trades requiring parentheses around conditions
for requiring braces around code blocks.

In the examples below, note that:

- The `let` keyword declares a constant (cannot change).
- The `var` keyword declares a variable (can change).
- Literal strings must be surrounded by double quotes.
- A range of numbers is defined using the `...` and `..<` operators.
  If a number appears on both sides of the operator, it is a closed range.
  If a number appears on only one side, it is an open range.

### if statement

```swift
let score = Int.random(in: 0...30)

// Can write on a single line.
if score == 0 { print("Start the game!") }

// Can spread over multiple lines.
if score == 21 {
    print("You win!")
} else if score > 21 {
    print("You lose.")
} else {
    print("Still playing ...")
}
```

### switch statement

Switch statements must be exhaustive which means
they must either include a `case` that matches every possible value
or they must include a `default` case.

```swift
switch score {
case 21: // matches a single value
    print("You win!")
    // Control does not flow into the next case,
    // so a break statement is not necessary.
case 22...: // matches an open-ended range
    print("You lose.")
case let s where s > 21: // alternative using a where clause
    print("You lose.")
default: // matches all other values
    print("Still playing.")
}
```

### for loop

```swift
// Create a deck of playing cards.
let ranks = [
    "2", "3", "4", "5", "6", "7", "8", "9",
    "10", "J", "Q", "K", "A"
]
let suits = ["♥️", "♦️", "♣️", "♠️"]
var deck: [String] = [] // Array of String values
for rank in ranks {
    for suit in suits {
        // The syntax \(expression) inside a literal String
        // is used for String interpolation.
        // Many other languages use ${expression} for this.
        // It's unclear why Swift chose a different syntax.
        deck.append("\(rank)\(suit)")
    }
}

// Deal a hand of cards.
deck.shuffle()
var hand: [String] = []
for _ in 1...5 {
    hand.append(deck.removeFirst())
}
print(hand) // ex. ["3♦️", "3♣️", "7♥️", "8♥️", "K♦️"]
```

### while loop

```swift
var temperature = 0
func forecast() {
    print("temperature is", temperature)
    // The loop below stops when this generates a high temperature.
    temperature = Int.random(in: 0...100)
}

// top-tested loop
while temperature < 80 {
    forecast()
}
```

### repeat-while loop

```swift
temperature = 0

// bottom-tested loop
repeat {
    forecast()
} while temperature < 80
```

## Type Inference

There are many cases when it is unnecessary to specify types
because Swift infers them.

```swift
let score: Int = 19
let score = 19 // same

let distance: Double = 1.23
let distance = 1.23 // same

let name: String = "Mark"
let name = "Mark" // same

// SwiftUI is a framework for creating user interfaces.
// It defines a Text struct and a Color enum.
// Structs and enums are described later.
// foregroundColor is a method that takes a Color which is
// a struct with static properties for many common colors.
Text("Hello, World!").foregroundColor(Color.red)
Text("Hello, World!").foregroundColor(.red) // same

// multilineTextAlignment is a method that takes a TextAlignment
// which is an enum with cases for each supported option.
Text(title).multilineTextAlignment(TextAlignment.center)
Text(title).multilineTextAlignment(.center) // same
```

A closure is a kind of anonymous function that is described later.
Parameter types typically are not needed in closures
because they can be inferred based on the types the caller passes.

## Collections

The most commonly used builtin collection types are
`Array`, `Dictionary`, `Set`, and tuples.
Each of these specifies the types of the items it can contain,
and each has a literal syntax.

### Arrays

Arrays store an ordered collection of values
that have the same type or a common supertype.

Here is an example of defining and using an `Array`.

```swift
// The type of this array is inferred to be [String],
// which can also be written as Array<String>.
var dogNames = ["Maisey", "Ramsay", "Oscar"]

dogNames.append("Comet")
dogNames[2] = "Oscar Wilde"
print(dogNames[3]) // Comet

// A for loop can iterate over the items in an Array.
for name in dogNames {
    print(name)
}
```

### Dictionaries

Dictionaries store an unordered collection of key/value pairs
where the keys must be unique.

Here is an example of defining and using a `Dictionary`.

```swift
// The type of this Dictionary is inferred to be [String:String]
// which can also be written as Dictionary<String, String>.
// The first type is that of the keys and
// the second type is that of the values.
var languageCreators = [
    "Java": "James Gosling",
    "JavaScript": "Brendan Eich",
    "Python": "Guido van Rossum",
    "Rust": "Graydon Hoare"
]
languageCreators["Swift"] = "Chris Lattner"

// Getting the value for a key in a Dictionary returns an "optional"
// because it is possible that the key does not exist.
// Optionals are described in the next section.
// The nil-coalescing operator (??) is used here to handle a missing key.
print("Swift was created by \(languageCreators["Swift"] ?? "unknown").")

// A for loop can iterate over tuples of key/value pairs in a Dictionary.
for (language, creator) in languageCreators {
    print("\(language) was created by \(creator).")
}
```

### Sets

Sets store an unordered collection of unique values.

Here is an example of defining and using a `Set`.

```swift
var uniqueNumbers: Set<Int> = []

// Fill the set with up to 5 random integers from 1 to 10.
for _ in 1...5 {
    uniqueNumbers.insert(Int.random(in: 1...10))
}

print(uniqueNumbers) // can contain fewer than 5 values
print(uniqueNumbers.contains(7)) // true or false
```

### Tuples

A tuple is an ordered collection of a fixed size
whose items can have differing types.

Here is an example of defining a function that returns a tuple,
calling the function, and using the return value.

```swift
// Gets the minimum, maximum, and average of the numbers in an Array,
// returned as a tuple of optional Double values
// which will be nil if the Array is empty.
func statistics(_ values: [Double]) -> (Double?, Double?, Double?) {
    let sum = values.reduce(0) { $0 + $1 }
    // Dividing an Int by an Int truncates the result.
    // To get a Double result, at least one of the operands must be a Double.
    let average = values.isEmpty ? nil : sum / Double(values.count)
    return (values.min(), values.max(), average)
}

let numbers = [3.0, 7.0, 1.0, 4.0]
let results = statistics(numbers)
// Note the use of the tuple indices 0, 1, and 2 to get values within the tuple.
if let minimum = results.0 { print("minimum is \(minimum)") } // 1.0
if let maximum = results.1 { print("maximum is \(maximum)") } // 7.0
if let average = results.2 { print("average is \(average)") } // 3.75

// Type aliases give a name to type and
// are useful to avoid repeating long type descriptions.
typealias StatisticsTuple = (min: Double?, max: Double?, avg: Double?)

// This function is similar to the previous one but returns a "named tuple".
// An alternative to consider is using a struct.
func statistics2(_ values: [Double]) -> StatisticsTuple {
    let sum = values.reduce(0) { $0 + $1 }
    let average = values.isEmpty ? nil : sum / Double(values.count)
    return (min: values.min(), max: values.max(), avg: average)
}
let results2 = statistics2(numbers)
if let minimum = results2.min { print("minimum is \(minimum)") } // 1.0
if let maximum = results2.max { print("maximum is \(maximum)") } // 7.0
if let average = results2.avg { print("average is \(average)") } // 3.75
```

## Optionals

The value `nil` represents the absence of a value.
By default, variables cannot be set to `nil`.
They must always have a value of the declared type, even initially.
To allow a variable to be set to `nil`,
its type must be followed by a question mark, which makes it optional.

```swift
var name: String? // initial value is nil
...
name = "Mark"
```

The Swift compiler generates an error if code attempts to
access a variable of an optional type without checking for `nil`.
There are several ways, demonstrated below,
to test whether a variable holds the value `nil`
and access its value when it is not `nil`.

```swift
if name != nil {
    // The ! operator performs a "force unwrap"
    // to get the wrapped value of an optional.
    print("name is \(name!)") // name is Mark
}

// An "if let" statement assigns the wrapped value
// of an optional on the right side of the equal sign
// to a local variable on the left side
// ONLY IF its value is not nil.
// Often the local variable has the same name as the
// optional variable and shadows within the block.
// In Swift 5.7+, this can be shorted to "if let name {".
if let name = name {
    print("name is \(name)") // name is Mark
}

// The nil-coalescing operator ?? requires an optional on the left side
// and a non-optional on the right side.
// If the optional is not nil, the result is the wrapped value.
// Otherwise the result is the right side value.
print("name is \(name ?? "unknown")") // name is Mark

// The optional chaining operator ?. is applied to an optional.
// It evaluates to nil if the value of the optional is nil.
// Otherwise it evaluates a member of the wrapped value
// which can be a property or a method.
print("name length is \(name?.count ?? 0)") // 4
print("upper name is \(name?.uppercased() ?? "")") // upper name is MARK
```

## Properties

Swift provides three ways to define custom types: struct, class, and enum.
Each of these is described in the next section.
For now all you need to know is that
each of these can define properties and methods.

Properties are either "stored" or "computed".
Every instance of a type stores its own values
of all stored properties defined by the type.
Computed properties are computed based on the values of other properties
every time they are accessed.
They are not stored in instances of the type.

Computed properties must be declared with
the `var` keyword rather than `let`.
A type must be specified (cannot be inferred)
and is followed by a code block.

Computed properties always define a `get` function
that computes the value.
They can optionally define a `set` function
whose purpose is to change the values of properties used to
compute the value so the result will be a given value.
If there is no `set` function, a surrounding `get` block is not needed.
This is the case for most computed properties.

Here are examples of structs that define computed properties.

```swift
struct Race {
    let kilometers: Double // stored property

    // computed property with no set function
    var miles: Double {
        /* long version
        get {
            kilometers * 0.621
        }
        */
        kilometers * 0.621 // short version
    }
}

// Unlike in many languages, the "new" keyword
// is not needed to create an instance of a type.
let race = Race(kilometers: 5)
print(race.miles) // 3.105

struct Counter {
    private var n = 1

    // computed property with no set function
    var doubled: Int { n * 2 }

    // computed property with both get and set functions
    var tripled: Int {
        get {
            n * 3
        }
        set {
            // Swift provides the variable "newValue".
            n = newValue / 3 // truncates
        }
    }
}

var counter = Counter() // n is initially set to 1
print(counter.tripled) // returns 1 * 3 = 3, but doesn't change n

counter.tripled = 9 // changes n to 3
print(counter.doubled) // 3 * 2 = 6; doesn't change n
print(counter.tripled) // 3 * 3 = 9; doesn't change n
```

## Custom Types

As stated above, the `struct`, `class`, and `enum` keywords
provide three ways to define custom types.
These differ from each other in several ways, but they
all support defining a type that has properties and methods.
Methods are defined in the same way as functions
but appear inside a `struct`, `class`, or `enum`.

Before describing structs, classes, and enums,
it is useful to understand the concept of "protocols" which are
a kind of contract to which a custom type can conform.

### Protocols

Protocols are similar to interfaces in some other programming languages.
They define a set of computed properties and methods
that other types (structs, enums, and classes) must implement.

A type definition indicates that it conforms to one or more protocols
by following the type name with a colon and
a comma-separated list of protocol names.
For example: `struct Dog: CustomStringConvertible {`.

There are many protocols defined by Swift and custom protocols can be defined.
Commonly used protocols defined by Swift include:

- `CustomStringConvertible`: must define the computed property `description`
- `Identifiable`: must define the computed property `id`
- `Equatable`: must define the `==` operator for comparing instances
- `Comparable`: must define the `==` and `<` operators for comparing instances
- `Hashable`: must define the `hash` method

The Swift compiler can synthesize the implementations of several protocols.
This means that for types that contain only properties with basic types,
stating that a type conforms to the protocol is all that is required.

Instances of types that conform to the `CustomStringConvertible` protocol
can be automatically converted to a `String` representation
when printed or used in a `String` interpolation.

### Structs

Structs and enums are "value types".
When an instance is assigned to a variable or passed to a function,
a shallow copy is created. Technically a copy is not made until
there is an attempt to modify it using a "copy on write" strategy.

Structs are used far more frequently than classes in typical Swift code.
Nearly all builtin types are structs, including `Bool`, `Int`, `Double`,
`Character`, `String`, `Array`, `Set`, `Dictionary`, `Range`, and `Date`.

Here is an example of defining a struct, creating an instance, and using it.

```swift
struct Dog: CustomStringConvertible {
    let name: String
    let breed: String

    // This is a computed property required by
    // the CustomStringConvertible protocol.
    var description: String {
        "\(name) is a \(breed)."
    }
}

// By default, structs are given a "memberwise-initializer"
// that takes one argument for each stored property
// in the order in which they are defined.
// Structs are not required to define additional initializers,
// but they can in order to define additional ways to create instances.
let dog = Dog(name: "Comet", breed: "Whippet")

print(dog) // Comet is a Whippet.
```

### Classes

Classes are "reference types".
Multiple variables can refer to the same instance, and
passing an instance to a function passes a reference rather than a copy.

Here is an example of defining a class, creating an instance, and using it.

```swift
import Foundation // need to use the Date struct

class Person {
    // Properties

    let name: String
    var spouse: Person? // optional
    var weddingDate: Date? // optional

    // Initializers (can have more than one)

    // Classes are not given a "memberwise-initializer",
    // so they are required to define at least one initializer.
    init(name: String) {
        self.name = name
    }

    // Methods

    // "spouse" and "on" are argument labels, used by callers.
    // "person" and "date" are parameter names used inside the function.
    func marry(spouse person: Person, on date: Date) {
        self.spouse = person
        self.weddingDate = date
    }

    func report() {
        // This assigns the wrapped value of the optional property "spouse"
        // to the shadowing local variable "spouse"
        // only if its value is not nil.
        if let spouse = spouse {
            print("\(name) is married to \(spouse.name).")
        } else {
            print("\(name) is single.")
        }
    }
}

var personA = Person(name: "Mark")
var personB = Person(name: "Tami")
let date = Date() // now
personA.marry(spouse: personB, on: date)
personA.report() // Mark is married to Tami.
```

### Enums

As stated earlier, enums are value types.
They define a fixed set of values that instances can have,
referred to as "cases".

Many programming languages support enums, but Swift takes the concept farther.
In Swift, each case can have different associated data.
Also, computed properties and methods can be defined,
just like in structs and classes.

Here is an example of defining enums and creating instances.

```swift
// The cases of this enum do not have associated data.
enum Language {
    // When the cases have no associated data,
    // they can be defined with a single case statement.
    case english, french, german, spanish

    // Method
    func greet() -> String {
        switch self {
        case .english:
            return "Hello"
        case .french:
            return "Bonjour"
        case .german:
            return "Hallo"
        case .spanish:
            return "Hola"
        }
    }
}

let lang = Language.french
print(lang.greet()) // Bonjour

// The cases of this enum have associated data.
enum Shape {
    case circle(radius: Double)
    case square(side: Double)
    case rectangle(width: Double, height: Double)

    // Computed property
    var area: Double {
        switch self {
        case let .circle(radius: r):
            return Double.pi * r * r
        case let .square(side: s):
            return s * s
        case let .rectangle(width: w, height: h):
            return w * h
        }
    }
}

var shape = Shape.circle(radius: 5)
print(shape.area) // 78.5398
shape = Shape.square(side: 2)
print(shape.area) // 4.0
shape = Shape.rectangle(width: 3, height: 4)
print(shape.area) // 12.0
```

## Access Control

Swift supports many keywords for controlling access to values like
functions, types, and the properties and methods of types.
These keywords appear at the beginning of declarations.

The access control keywords include:

- `open`: access from anywhere; only used for classes and class members
- `public`: same as `open` except cannot be used in subclasses or overridden
- `internal`: access from any source in the same module (default level)
- `fileprivate`: access only from code in the same source file
- `private`: access only within enclosing declaration (such as a struct or class)

The most commonly used access control keyword is `private`.
The second most commonly used is `internal`, which is the default.

Specifying `private(set)` on a property means that
the property can be accessed as if it is `public`,
but it can only be modified as if it is `private`.

## Imports

To use values such as structs that are
defined by a framework (such as Foundation, SwiftUI, or UIKit),
it is necessary to import the framework.
For example, `import Foundation`.

It is not necessary or even supported to list specific values to be imported.
Importing a framework makes all of its `open` and `public` values available.

It is also not necessary or supported to import files or values
defined in the current project.
All files in a project have access to all non-private values
defined in any file within the project.
This is somewhat surprising for developers
coming from other programming languages.

## Closures

The syntax for anonymous functions is
an open brace, a comma-separated list of parameters,
the keyword "in", the function body, and a close brace.
Swift anonymous functions capture variables in their environment,
making them available inside their function body,
which makes them "closures".
Typically closures are passed as arguments to other functions.

Here are examples of passing a closure to the `reduce` method
of the builtin `Array` struct.

```swift
let numbers = [1, 2, 3, 4]
// The first argument to reduce is an initial value.
// The second argument is a closure that takes the
// accumulated value (so far) and the next value from the array.
let sum = numbers.reduce(0, { acc, n in acc + n })
print("sum =", sum) // 10
```

When the last argument to a function is a closure,
it can be written as a "trailing closure".
These have the same syntax as any closure
but immediately follow the function call.

```swift
let sum = numbers.reduce(0) { acc, n in acc + n }
```

Trailing closures are especially useful for improving readability
when they contain multiple statements.

```swift
let sum = numbers.reduce(0) { acc, n in
    // We could do more here.
    return acc + n
}
```

It is not necessary to specify argument names for a closure.
The positional names $0, $1, and so on can be used instead.

```swift
// Here $0 represents the accumulator and $1 represents the next array item.
let sum = numbers.reduce(0) { $0 + $1 }
```

## Guards

Guards provide a clear way to check for conditions that are necessary
in order to continue execution of a function.
They have an associated block of code
that is required to exit the current scope.
Guards are typically used at or near the beginning of functions.

```swift
// Returns the number of items in a array that contain a given String.
func countOccurrences(in items: [String], of target: String?) -> Int {
    // If the items Array is empty, there can be no occurrences.
    guard items.count > 0 else { return 0 }

    // If the target String is nil, there is nothing to find.
    guard let target = target else { return 0 }

    return items.reduce(0) { acc, item in
        // The optional target was unwrapped above.
        acc + (item.contains(target) ? 1 : 0)
    }
}

var dogNames = ["Maisey", "Ramsay", "Oscar", "Comet"]
print(countOccurrences(in: [], of: "a")) // 0
print(countOccurrences(in: dogNames, of: nil)) // 0
print(countOccurrences(in: dogNames, of: "a")) // 3
```

## Extensions

Extensions add methods and computed properties to existing types,
even builtin types.

Here is an example of an extension that adds computed properties
to the `Date` type and accesses them on a `Date` instance.

```swift
extension Date {
    var dayOfWeek: String {
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "EEEE" // gets name of day
        return dateFormatter.string(from: self)
    }

    var ymd: String {
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "yyyy-MM-dd"
        return dateFormatter.string(from: self)
    }
}

let now = Date()
print(now.dayOfWeek) // Friday
print(now.ymd) // 2022-07-08
```

## Property Observers

Property observers watch for changes to a
specific stored property of a type instance
and perform actions before and/or after changes occur.

They are defined by optional `willSet` and `didSet` functions.
It is not necessary to define both when only one is needed.

The `willSet` function cannot prevent the change from happening, but
the `didSet` function can revert to the old value if the new value is invalid.

```swift
struct Person {
    let name: String
    var age: Int {
        willSet {
            // Swift provides the variable "newValue" to
            // access the new value that is not yet assigned.
            print("about to change from \(age) to \(newValue)")
        }
        didSet {
            // Swift provides the variable "oldValue" to
            // access the previously assigned value.
            if age >= 0 {
                print("changed from \(oldValue) to \(age)")
            } else {
                age = oldValue // reverts to previous value
                print("reverted to \(oldValue)")
            }
        }
    }
}

var me = Person(name: "Mark", age: 61)
me.age = 19 // about to change from 61 to 19; changed from 61 to 19
print("age is \(me.age)") // age is 19
me.age = -5 // about to change from 19 to -5; reverted to 19
print("age is \(me.age)") // age is 19
```

## Conclusion

I hope this article has piqued your interest in the Swift programming language.
Topics that deserve further study include:

- generics: pretty much the same as in other languages
- `async` and `await` keywords
- tasks
- actors
- structured concurrency
- SwiftUI
- Xcode IDE
