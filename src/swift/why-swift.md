---
eleventyNavigation:
  key: Why Swift?
  parent: Swift
  order: 1
layout: topic-layout.njk
---

# What makes Swift a great programming language?

This article highlights many features of the Swift programming language
that make it an attractive alternative to other programming languages.
It is not a full tutorial on the language.

Of course there is no consensus on the features
that are desirable in a programming language,
so the features presented here are driven by my own preferences.

Today Swift is primarily used for implementing applications that run on
iPhones, iPads, Apple Watches, and Mac computers.
However, it can also be used to implement server-side code
such as REST services. One way to do this is to utilize the
{% aTargetBlank "https://vapor.codes", "Vapor" %} framework.

## Less Noise

Many programming languages have a syntax that is
modelled after the C programming language where
conditions are surrounded by parentheses and
statements are terminated by semicolons.
Swift does not follow either of those conventions.
It trades requiring parentheses around conditions
for mandating braces around code blocks.

In the examples below, note that
the `let` keyword declares a constant (cannot change) and
the `var` keyword declares a variable (can change).

### if statement

```swift
let score = Int.random(in: 0...30)
print("score =", score)

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

```swift
switch score {
case 21:
    print("You win!")
case 22...: // open-ended range
    print("You lose.")
case let s where s > 21: // alternative using a where clause
    print("You lose.")
default:
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
var deck: [String] = []
for rank in ranks {
    for suit in suits {
        deck.append("\(rank)\(suit)")
    }
}

// Deal a hand of cards.
deck.shuffle()
var hand: [String] = []
for _ in 1...5 {
    hand.append(deck.removeFirst())
}
print(hand) // ["3♦️", "3♣️", "7♥️", "8♥️", "K♦️"]
```

### while loop

```swift
var temperature = 0
func doSomething() {
    print("temperature is", temperature)
    // The loop below stops when this generates a high temperature.
    temperature = Int.random(in: 0...100)
}

// top-tested loop
while temperature < 80 {
    doSomething()
}
```

### repeat-while loop

```swift
temperature = 0

// bottom-tested loop
repeat {
    doSomething()
} while temperature < 80
```

## Type Inference

Swift provides great type inference.

```swift
let score: Int = 19
let score = 19 // same

let distance: Double = 1.23
let distance = 1.23 // same

let name: String = "Mark"
let name = "Mark" // same

// SwiftUI is a library for creating user interfaces.
// It defines a Text struct and a Color enum.
Text("Hello, World!").foregroundColor(Color.red)
// This line is the same because the type Color is inferred.
Text("Hello, World!").foregroundColor(.red)
```

Closures (a kind of anonymous function) are described later.
Parameter types typically are not needed in closures
because they can be inferred based on the caller.

## Optionals

The Swift value `nil` represents the absence of a value.
By default, variables cannot be set to `nil`.
They must always have a value of the declared type, even initially.
To allow a variable to be set to `nil`,
its type must be followed by a question mark which makes it "optional".

```swift
var name: String? // initial value is nil
...
name = "Mark"
```

There are several ways, demonstrated below,
to test whether a variable holds the value `nil`
and access its value when it is not `nil`.
The Swift compiler generates an error message if code attempts
to access a variable of an optional type without checking for `nil`.

```swift
if name != nil {
    print("name is \(name!)") // ! performs a "force unwrap"
}

// An "if let" statement assigns the unwrapped value of the
// optional variable on the right side of the equal sign
// to a local variable on the left side
// ONLY IF its value is not nil.
// Often the local variable has the same name as the optional variable
// and shadows within the block.
// In Swift 5.7, this can be written as "if let name {".
if let name = name {
    print("name is \(name)")
}

// This uses the nil coalescing operator ?? to choose
// either the wrapped value or the value on the right side.
print("name is \(name ?? "unknown")")

// This uses optional chaining (?.) to
// access a property on an optional variable.
// Optional chaining can also be used to
// call a method on an optional variable.
print("name length is \(name?.count ?? 0)")
```

## Custom Types

The `struct`, `class`, and `enum` keywords
provide three ways to define custom types.
These differ from each other in a few ways, but they
all support defining a type that has properties and methods.

Structs and enums are "value types".
When an instance is assigned to a variable or passed to a function,
a copy is created. Technically a copy is not made until
there is an attempt to modify it (copy on write).

`CustomStringConvertible` is a "protocol" defined by Swift.
A protocol is like an "interface" in other programming languages.
Following a `struct`, `class`, or `enum` name with a colon and a protocol name
states that the type will conform to the protocol.
That means it will implement all the
computed properties and methods required by the protocol.

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

let dog = Dog(name: "Comet", breed: "Whippet")
print(dog) // Comet is a Whippet.
```

Classes are "reference types".
Multiple variables can refer to the same instance and
passing an instance to a function passes a reference rather than a copy.

Here is an example of defining a class, creating an instance, and using it.

```swift
import Foundation // for the Date struct

class Person {
    // Properties

    let name: String
    var spouse: Person? // optional
    var weddingDate: Date? // optional

    // Initializers (can have more than one)

    init(name: String) {
        self.name = name
    }

    // Methods

    // "spouse" and "on" are argument labels, used by callers.
    // "person" and "date" are parameter names, used inside the function.
    // If the argument label "spouse" was omitted, it would default to "person".
    func marry(spouse person: Person, on date: Date) {
        self.spouse = person
        self.weddingDate = date
    }

    func report() {
        // This assigns the unwrapped value of the optional property "spouse"
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

Functions and methods typically have parameters.
Each parameter can be given an "argument label" whose purpose
is to make calls to them read in a more English-like manner.
When a parameter has no argument label, it defaults to the parameter name.

The arguments in a function or method call must appear
in the same order in which they are defined
in order to support being read by a developer in the expected way.
In the example above, reading "marry spouse on" sounds correct,
but reading "marry on spouse" does not.

Enums define a fixed set of values that instances can have,
referred to as "cases".
Many programming languages support enums, but Swift takes the concept farther
by allowing each case to have different associated data
and allowing definition of computed properties and methods.

Here is an example of defining enums and creating instances.

```swift
enum Language {
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

## Positional Parameters

Positional parameters are used far less frequently
than named parameters is typical Swift code.
However, there are valid reasons to use them.

To make a function or method parameter positional rather than named,
specify an underscore for its argument label.
For example:

```swift
func multiply(_ n1: Double, by n2: Double) -> Double {
    // When a function only contains a single expression,
    // the "return" keyword is inferred.
    n1 * n2
}

print(multiply(2, by: 3)) // 6.0
```

## Closure Syntax

The syntax for anonymous functions in Swift is
an open brace, a comma-separated list of parameters,
the keyword "in", the function body, and a close brace.
Anonymous functions capture variables in their environment
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

```swift
let sum = numbers.reduce(0) { acc, n in acc + n }
```

Trailing closures are especially useful for readability
when they contain multiple statements.

```swift
let sum = numbers.reduce(0) { acc, n in
    // We could do more here.
    return acc + n
}
```

It is not necessary to specify argument names for a closure.
They can instead use the positional names $0, $1, and so on.

```swift
let sum = numbers.reduce(0) { $0 + $1 }
```

## Computed Properties

Properties of a struct, class, or enum can be computed
based on the values of other properties.

```swift
struct Race {
    let kilometers: Double

    var miles: Double {
        kilometers * 0.621
    }
}

let race = Race(kilometers: 5)
print(race.miles) // 3.105
```

## Guards

Guards provide a clear way to check for conditions that are necessary
in order to continue execution of a function.
They have an associated block of code that is require to exit the current scope.
Guards are typically used at or near the beginning of functions.

```swift
func countOccurrences(in items: [String], of target: String?) -> Int {
    guard items.count > 0 else { return 0 }
    guard let target = target else { return 0 }

    return items.reduce(0) { acc, item in
        acc + (item.contains(target) ? 1 : 0)
    }
}

var names = ["Maisey", "Ramsay", "Oscar", "Comet"]
print(countOccurrences(in: names, of: "a")) // 3
print(countOccurrences(in: [], of: "a")) // 0
print(countOccurrences(in: names, of: nil)) // 0
```

## Extensions

Extensions can add methods and computed properties to existing types,
even builtin types.
For example, the following code demonstrates adding computed properties
to the `Date` type and accessing them on a `Date` object.

```swift
extension Date {
    var dayOfWeek: String {
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "EEEE"
        return dateFormatter.string(from: self)
    }

    // Returns a String representation of the Date in "yyyy-mm-dd" format
    // with no time display.
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

Property observers watch for changes to a specific property of an object.

```swift
struct Person {
    let name: String
    var age: Int {
        willSet {
            print("about to change from \(age) to \(newValue)")
            //TODO: Can you prevent the change?
        }
        didSet {
            print("changed from \(oldValue) to \(age)")
        }
    }
}

var me = Person(name: "Mark", age: 61)
me.age = 19
```

## Protocols

Protocols are similar to interfaces in some other programming languages.
They define a set of computed properties and methods
that other types (structs, enums, and classes) must implement.

There are many protocols defined by Swift and custom protocols can be defined.
Commonly used protocols defined by Swift include:

- `Equatable` - must define the `==` operator for comparing instances
- `Comparable` - must define the `==` and `<` operators for comparing instances
- `Hashable` - must define the `hash` method
- `Identifiable` - must define the computed property `id`
- `CustomStringConvertible` - must define the computed property `description`

The Swift compiler can synthesize the implementations of several protocols.
This means that for types that only contain properties with basic types,
stating that a type conforms to the protocol is all that is required.

TODO: Include an example of defining and using a custom protocol.
TODO: Include an example of using an extension to define default implementations of protocol methods.
