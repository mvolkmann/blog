---
eleventyNavigation:
  key: Swift Language
  parent: Swift
  order: 2
layout: topic-layout.njk
---

<img alt="Swift logo" style="width: 30%"
  src="/blog/assets/swift-logo.png?v={{pkg.version}}"
  title="Swift logo">

## Overview

{% aTargetBlank "https://swift.org", "Swift" %}
is an open source programming language created by Apple.
Key facts about Swift include the following:

- introduced by Apple at the 2014 WWDC conference
- became open source under an Apache license in December 2015
- goals are to be safe, fast, and expressive
- a compiled language, but there is also an interpreter
- strongly typed with type inference
- supports both object-oriented and functional programming
- a big language with a large number of features
  and a corresponding learning curve
- can be used to build applications for macOS, iOS, and Watch OS
- can be used to build command-line and server-side applications
- has six major types:
  structs, classes, enums, protocols, functions, and generics
- supports tuples
- standout features include computed properties, closure syntax,
  trailing closures, key paths, and
  a succinct way to refer to enum cases when their type can be inferred
- built on {% aTargetBlank "https://llvm.org", "LLVM" %}
  (Low Level Virtual Machine)
- interoperates with {% aTargetBlank
  "https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/",
  "Objective-C" %} code
- includes a {% aTargetBlank
  "https://developer.apple.com/documentation/swift/swift_standard_library",
  "Standard Library" %}
- compiler is slow, not swift
- language makes some efforts to be concise,
  but also uses many long keywords and method names

## Resources

- Swift home page - {% aTargetBlank "https://swift.org", "https://swift.org" %}

- {% aTargetBlank "https://docs.swift.org/swift-book/",
  "The Swift Programming Language Book" %}

- {% aTargetBlank "https://www.apple.com/swift/playgrounds/",
  "Swift Playgrounds" %} iOS app - free

- {% aTargetBlank "https://www.youtube.com/c/PaulHudson", "Hacking with Swift" %}
  video series on Swift and SwiftUI by Paul Hudson (@twostraws) - free

- {% aTargetBlank "https://www.hackingwithswift.com/100", "100 Days of Swift" %}
  by Paul Hudson (@twostraws) - free

- {% aTargetBlank "https://www.youtube.com/channel/UCp25X4LzOLaksp5qY0YMUzg",
  "Swiftful Thinking" %} YouTube videos by Nick Sarno - free

- {% aTargetBlank "https://www.swiftbysundell.com", "Swift by Sundell" %}
  by John Sundell

- {% aTargetBlank "https://www.youtube.com/c/StewartLynch", "CreaTECH Solutions" %}
  YouTube videos by StewartLynch- free

- {% aTargetBlank "https://seanallen.co", "Sean Allen" %}
  YouTube videos and paid courses

- {% aTargetBlank "https://codewithchris.com/swift-tutorial/", "Code With Chris" %}
  by Chris Ching

- {% aTargetBlank "https://cs193p.sites.stanford.edu",
  "Stanford CS193p - Developing Apps for iOS" %} course by Paul Hegarty - free

- {% aTargetBlank "https://www.udemy.com/course/ios-13-app-development-bootcamp/",
  "iOS & Swift - The Complete iOS App Development Bootcamp" %}
  Udemy course by Angela Yu - $14.99

## Installing

Swift can be installed on macOS, Windows 10, Amazon Linux 2, CentOS, and Ubuntu.

To install Swift on macOS, install Xcode from the macOS App Store.
For other operating systems, download it from
{% aTargetBlank "https://swift.org/download/", "Download Swift" %}.

## Editing

Most Swift developers use Xcode as their editor/IDE.
However, there are good reasons to use VS Code in addition to Xcode.

VS Code as the following advantages over Xcode:

- There is a built-way to format code every time changes to a file are saved.
  To format code in Xcode, select the lines to be formatted
  and press ctrl-shift-i.
  Xcode can be configured to run
  {% aTargetBlank "/blog/topics/#/blog/swift/SwiftFormat/", "SwiftFormat" %} on save,
  but this requires a number of undocumented steps.
  SwiftFormat is a separate tool that is similar to a combination of
  the JavaScript tools ESLint (with autofix) and Prettier.
- Git integration is better. You can see at a glance
  which files have been added or modified and
  see side-by-side diffs without initiating a commit.
- Snippet support is much better.
  A snippet can refer to built-in variables
  for inserting things like the current file name
  and placeholders can be repeated so
  the same entered text can be inserted in multiple places.
- Files in the Navigator pane are always listed in alphabetical order.
- The Vim support in VS Code (provided by the Vim extension)
  is far better than the Vim support in Xcode
  (enabled by checking "Vim mode" on the Editor menu).
  In VS Code, the dot command (for repeating the last command)
  and macros (for recording and repeating a sequence of operations)
  are supported.

However, Xcode also has the following advantages over VS Code:

- Syntax error messages are displayed inline with the code where they appear.
- iOS projects can be run in the Simulator and on devices
  directly from Xcode.

I recommend simultaneously opening a project in both VS Code and Xcode
and switching between them as needed to get the benefits of both.
When a change is made to a file that is open in both,
the other will automatically update to show the change.

### Getting Started with VS Code

Install the Swift and SwiftFormat extensions.

In "User Settings" under Text Editor ... Formatting, enable "Format on Save".

Create the file `.swiftformat` in the top project directory
with contents like the following:

```text
--maxwidth 80
--ranges no-space
--swiftversion 5.6
```

For more options supported by SwiftFormat, see {% aTargetBlank
"https://github.com/nicklockwood/SwiftFormat/blob/master/Rules.md", "Rules" %}.

## Using the Interpreter

To start the interpreter as a REPL (Read Eval Print Loop), enter `swift`.
Then enter Swift statements to be evaluated.
For example, enter `print(1 + 2)`.

To run the interpreter on code in a file, enter `swift {file-path}`.
For example, create the file `greet.swift` containing `print("Hello, World!")`
and enter `swift greet.swift` to run it.

Interpreter commands begin with a colon.
The most commonly used commands are described in the table below.

| Command | Description                      |
| ------- | -------------------------------- |
| `:exit` | exits the interpreter            |
| `:help` | prints help on all REPL commands |

## Debugging Output

The global functions {% aTargetBlank
"https://developer.apple.com/documentation/swift/print(_:separator:terminator:)",
"print" %} and {% aTargetBlank
"https://developer.apple.com/documentation/swift/dump(_:name:indent:maxdepth:maxitems:)",
"dump" %} functions are useful for debugging Swift code.

The `print` function takes any number of arguments and prints them to stdout.

The literal expressions `#file`, `#fileID`, `#function`, `#line`, and `#column`
can be used to provide context. For example:

```swift
print("\(#fileID) \(#function) score =", score)
```

The `dump` function takes one value to print and
prints each property of the object on a separate line.
To takes the optional arguments `indent`, `maxDepth`, and `maxItems`
to customized the output.

The following code prints a subset of the data in an array.

```swift
struct Dog {
    let name: String
    let breed: String
    let owner: Person
}

struct Person {
    let name: String
}

let tami = Person(name: "Tami")
let amanda = Person(name: "Amanda")
let dogs = [
    Dog(name: "Oscar", breed: "GSP", owner: amanda),
    Dog(name: "Comet", breed: "Whippet", owner: tami)
]
dump(dogs)
```

The output from the `dump` function is:

```text
▿ 2 elements
  ▿ __lldb_expr_136.Dog
    - name: "Oscar"
    - breed: "GSP"
    ▿ owner: __lldb_expr_136.Person
      - name: "Amanda"
  ▿ __lldb_expr_136.Dog
    - name: "Comet"
    - breed: "Whippet"
    ▿ owner: __lldb_expr_136.Person
      - name: "Tami"
```

To print the unique id of an object:

```swift
print("id = \(ObjectIdentifier(myObject))")
```

## Packages

The {% aTargetBlank "https://www.swift.org/package-manager/",
"Swift Package Manager" %} is a
"tool for managing the distribution of Swift code".
It is similar to npm for JavaScript.
Installing the Swift compiler also installs the Swift package manager.

To install a package in Xcode:

- select File ... Add Packages...
- Enter the GitHub URL of a package in the search input in the upper-right.
- Select the package
- Click the "Add Package" button.

## Libraries

Xcode is aware of many standard libraries such as SwiftUI and UIKit.
To use the public types, functions, and values that a library defines,
import them. For example, `import SwiftUI`.

Packages that are added to a project as described in the previous section
must also be imported.

It is possible for two libraries/packages to define the same names.
One approach to resolve such conflicts is to prefix the names
with the name of the library/package.
For example, `SwiftUI.List`.
Another approach is to define type aliases and use those names.
For example, `typealias SUIList = SwiftUI.List`.

## Comments

Single-line comments begin with `//`.
Multi-line comments are delimited by `/*` and `*/`.
Multi-line comments can be nested which makes it easy to
comment out blocks of code that contain multi-line comments.
Documentation comments begin with `///` and appear before type definitions.
This documentation appears when a name is option-clicked in Xcode.

Xcode can toggle whether lines are commented.
To comment/uncomment the current line or selected lines, press cmd-/.

Comments whose purpose is to remind you to make a change later
can be written in three ways:

- `// TODO: some text`

  This approach is not flagged when the project is built.

- `#warning("some text")`

  This approach results in a warning message when the project is built
  which makes it less likely to be overlooked than a TODO comment.
  The project will still build successfully.

- `#error("some text")`

  This approach results in an error message when the project is built
  which makes it impossible to forget because the project will not build.

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
| nil-coalescing | a `??` b (value is `a` if not nil and `b` otherwise) |
| bit shift      | `<<`, `>>`, `&<<`, `&>>`                             |
| bitwise        | `&` (and), `\|` (or), `^` (xor)                      |
| types          | `is` (type check), `as`, `as?`, `as!` (type cast)    |

The binary mathematical operators require
the variables on each side to have the same type.
For example, a `Float` cannot be divided by an `Int`.
When the types differ, one side must be cast to the type of the other side.

```swift
let a = 8.0
let b = 3
print(8.0 / 3) // both sides are literals so allowed; 2.666...
print(a / 3) // one side is literal so allowed; same result
print(a / Double(b)) // variables on both sides so cast required; same result
```

When an `Int` is divided by an `Int` using the `/` operator,
the result is truncated to an `Int`, not rounded.

The nil coalescing operator in `a ?? b` is shorthand for `a != nil ? a! : b`.
This is often used to provide a default value to use when `a` is `nil`.

Swift supports optional chaining with the `?.` operator
so chains of references to optional values (see the "Optionals" section later)
do not have to check for nil values.
See the example in the Structs section.

## Functions

Functions are defined using the `func` keyword,
followed by the function name, the parameter list in parentheses,
and an optional return type preceded by `->`.
The parentheses are required even for functions that have no parameters.

When the return type of a function cannot be inferred
and no return type is specified, it defaults to the `Void` type
The `Void` type has a single value which is an empty tuple (`()`).

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

```swift
func order(item: Item, quantity: Int) -> Int {
    // Place order and get order number.
    return orderNumber
}
let orderNumber = order(item: Item("chicken fried rice"), quantity: 2)
```

All function parameters are treated as `let` constants.
This prevents function code from assigning new values to them.
This also applies to the properties of objects passed to functions.

Swift does not support a shorthand syntax for calling functions
when in-scope variables have the same names are argument labels.

```swift
let item = Item("chicken fried rice")
let quantity = 2
//let orderNumber = order(item, quantity) // doesn't work
let orderNumber = order(item: item, quantity: quantity)
```

When an argument label is underscore, it is a positional argument.
The function must be called with only a value and no name for that parameter.
Otherwise calls must include the argument label.
A function can have a mixture of parameters with argument labels
and positional parameters.

Typically positional arguments are only used for the first argument
and only when the function name implies the meaning of the first argument.
The following code demonstrates using multiple positional arguments.

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

Calls to a function must provide all the required arguments.
Each argument is specified by the argument label, a colon, and a value.
These must appear in the same orders as the corresponding parameters.
The rationale for this is that it can make some calls more expressive,
almost like sentences.
It is useful to think of the argument labels
as being part of the function name.

Here is an example of a function where it is useful
for argument labels to differ from parameters names.
It uses the types `Ingredient` and `Food` that are not defined here.

```swift
func getDailyPercentage(of ingredient: Ingredient, in food: Food) -> Float {
    // Perhaps look up the value in a Dictionary and return it.
}

let sugar = Ingredient(name: "sugar")
let frostedFlakes = Food(name: "Frosted Flakes")
let dailySugarPct = getDailyPercentage(of: sugar, in: frostedFlakes)
```

Code hints provided by Xcode help developers know
the argument labels and types that must be provided.

A "variadic" parameter accepts multiple values of the same type,
indicated by following the type with three periods.
The parameter value will be a constant array of values.

```swift
func displaySum(label: String, numbers: Int...) {
    let sum = numbers.sum();
    print("\(label) = \(sum)")
}
```

Function names can be overloaded based on their parameter types.

To return multiple values, return a tuple
which is written as a list of values inside parentheses.

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

// The question mark at the end of the return type
// indicates that the function might return nil.
func getRgb(_ color: String) -> RGB? {
    return rgbDict[color]
}

print(getRgb("red")) // (red: 255, green: 0, blue: 0)
print(getRgb("green")) // (red: 0, green: 255, blue: 0)
print(getRgb("pink")) // nil
```

By default, parameters cannot be modified in function bodies.
Adding the keyword `inout` before a parameter type changes
the parameter to be passed by reference instead of by value.
Callers must precede corresponding argument values with an ampersand (`&`).
Modifying the values of these parameters changes
the value of the corresponding argument in the caller.
This only works if the value passed in is a variable (`var`)
rather than a constant (`let`).
The use of `inout` parameters is not allowed in concurrently executing code.

The type of a function is described by its parameter types and return type.
For example, the type of the `displaySum` function above is
`(String, Int) -> Void`.
The type of a function that takes no arguments and returns nothing
is `() -> Void`.

Function types can be used for variable, parameter, and return types.
This means that compatible functions can be assigned to variables,
passed to functions, and returned from functions.

Argument labels are not used when calling a function stored in a variable
because each function that can be assigned to the variable
could specify different argument labels. For example:

```swift
typealias BinaryFn = (Double, Double) -> Double

func add(n1: Double, n2: Double) -> Double { n1 + n2 }
let subtract: BinaryFn = { n1, n2 in n1 - n2 }
let multiply: BinaryFn = { $0 * $1 }

var operation: BinaryFn = add
print(operation(2, 3)) // 5.0
operation = subtract
print(operation(2, 3)) // -1.0
operation = multiply
print(operation(2, 3)) // 6.0
```

Functions can defined in the bodies of other functions
to scope their usage.
Otherwise they are global and can be called from anywhere.
Nested functions can be returned by their enclosing function
to allow them to be called from outside.

Swift does not support function currying.
This creates a new version of an existing function that
takes one fewer argument and uses a fixed value for the omitted argument.
JavaScript functions support this using the `bind` method.

## Closures

Anonymous functions (a.k.a closures) can be used as
the values of variables and arguments.
They are written with the following syntax:
`{ parameter-list -> return-type in statements }`.
The return type can be omitted when it can be inferred.
Note the use of the `in` keyword to mark
the end of the parameter list and the beginning of the statements.

The following code defines and calls several closures.

```swift
// This function has no parameters.
let printTime = {
    let date = Date() // now
    let dateFormatter = DateFormatter()
    dateFormatter.dateFormat = "M/d/yyyy"
    print(dateFormatter.string(from: date))
}
printTime() // 3/8/2022

// This function specifies all of its parameter types and the return type.
let product = {(a: Double, b: Double) -> Double in a * b}
print(product(2, 3)) // 6.0

// This function omits the return type because it can be inferred.
let product2 = {(a: Double, b: Double) in a * b}
print(product2(2, 3)) // 6.0
```

If the parameter types of a closure can be inferred from usage,
the parameter list can be omitted and the parameter values can be
referred to by index using the names `$0`, `$1`, and so on.

```swift
let numbers = [1, 3, 7]
let doubled = numbers.map({$0 * 2}) // [2, 6, 14]
```

If parameters to a closure are not used,
their names can be replaced by underscores.

If a closure takes no arguments and just returns the value of an expression,
it can be written as just the expression inside curly braces.
For example, the closure `{ 7 }` takes no arguments and always returns 7.

If the last parameters to a function or method are functions,
they can be passed using "trailing closures".
Typically this is only done for the last argument.
For example, these are equivalent:

```swift
let prices = [1.23, 2.34, 3.45]
let total = prices.reduce(0, { result, price in result + price })
let total = prices.reduce(0) { result, price in result + price }
let total = prices.reduce(0, {$0 + $1})
```

Closures can be passed as arguments to other functions.
If the receiving function has asynchronous behavior
that invokes the closure after the function returns,
the closure parameter must be declared to be `@escaping`.
TODO: Why is this required?
TODO: Are non-escaping closures handled in an optimized way
that discards their context after the function returns?

When an escaping closure references `self`,
typically the closure parameter list should be preceded by `[weak self]`.
This enables the object that uses the escaping closure can be garbage collected
when there are no longer references to it.
The type of `self` will be optional inside the closure, so all references
to it will have to account for the possibility of it being `nil`.

## Stopping Execution

Swift provides three global functions that can stop program execution.

The {% aTargetBlank
"https://developer.apple.com/documentation/swift/assert(_:_:file:line:)",
"assert" %} function conditionally prints a message that
includes the current file and line number and then stops the program.
These calls are ignored in release builds.

The {% aTargetBlank
"https://developer.apple.com/documentation/swift/precondition(_:_:file:line:)",
"precondition" %} function is the same as the `assert` function,
but it is also honored in release builds.

The {% aTargetBlank
"https://developer.apple.com/documentation/swift/fatalerror(_:file:line:)",
"fatalError" %} function unconditionally prints a message that
includes the current file and line number and then stops the program,
even in release builds.

## Error Handling

Errors in that occur in Swift code are described by
objects that conform to the `Error` protocol.
A protocol is like an interface in other programming languages.
These are described in detail later in the [Protocols](#protocols) section.

The `Error` protocol is merely a marker protocol,
not requiring any properties or methods.
There are a small number of provided types that conform to this protocol,
summarized below.
Perhaps only `DecodingError` and `EncodingError` are suitable
for throwing from your own functions.

- {% aTargetBlank "https://developer.apple.com/documentation/swift/cancellationerror", "CancellationError" %} is thrown when a `Task` is cancelled.
- {% aTargetBlank "https://developer.apple.com/documentation/swift/decodingerror", "DecodingError" %} is thrown when a value cannot be decoded.
- {% aTargetBlank "https://developer.apple.com/documentation/distributed/distributedactorcodingerror", "DistributedActorCodingError" %} is thrown when a distributed actor cannot encode or decode a value.
- {% aTargetBlank "https://developer.apple.com/documentation/swift/encodingerror", "EncodingError" %} is thrown when a value cannot be encoded
- {% aTargetBlank "https://developer.apple.com/documentation/distributed/executedistributedtargeterror", "ExecuteDistributedGTargetError" %} is thrown by the `executeDistributedTargetError` method
- {% aTargetBlank "https://developer.apple.com/documentation/distributed/localtestingdistributedactorsystemerror", "LocalTestingDistributedActorSystemError" %} is not described in the Apple documentation.
- {% aTargetBlank "https://developer.apple.com/documentation/swift/never", "Never" %} is the return type of functions that should never return. It's not clear why this conforms to the `Error` protocol.

Typically custom error types are defined in used.
Often these are define as an `enum` that conforms to the `Error` protocol.
Each `enum` `case` represents a variation of the error
and specific cases are thrown.

An alternative is to allow any `String` to be thrown by defining
an `extension` that makes the `String` type conform to the
`LocalizedError` protocol which inherits from the `Error` protocol
as follows:

```swift
extension String: LocalizedError {
    public var errorDescription: String? { return self }
}
```

To throw an error, use the `throw` keyword followed by an
instance of any type that conforms to the `Error` protocol.

To handle errors, use one of the following approaches:

- allow errors to propagate to the caller

  This is done by adding the `throws` keyword
  after the parameter list and before the return type.
  It is not possible to indicate the kinds of errors that can be thrown.
  For example:

  ```swift
  func divide(numerator: Double, denominator: Double) throws -> Double {
      ...
  }
  ```

- use a `do`/`catch` statement

  The `do` statement supports having any number of associated `catch` blocks
  that each handle different kinds of errors.

  Each statement in the `do` block that can throw an error
  must be preceded by the `try` keyword.
  This is different from most programming languages where
  it is assumed that any statement in a `try` block can thrown.

  Each `catch` can be followed by a pattern, or list of patterns,
  that identify the kinds of errors it handles.
  To access data associated with errors caught by these cases,
  use the `let` syntax shown in the example code below.

  A `catch` with no pattern can appear at the end
  and is used to handle all remaining kinds of errors.
  Inside this block the variable `error` is set to the value that was thrown.

  If none of the catches handle the error type that has occurred,
  the error is propagated to the caller.

  If an error propagates to the top of the call chain and is not handled,
  the program will terminate with a fatal error.

- treat the error as an optional value

  When setting a variable to the result of an expression that can throw,
  one option is to precede the expression with `try?`.
  If the expression throws, the variable is set to `nil`
  and error is considered handled.
  Such code does not need to be in a `do` block.

- assert that the error should never occur

  If an expression or statement technically can throw,
  but should never throw given the way it is being used,
  it can be preceded with `try!`.
  This performs a force unwrap of the result and
  frees the code from needing the handle errors.
  If the code actually does throw,
  the program will terminate with a fatal error.
  For this reason is not frequently used.

We have seen three variations of trying something that can throw
which are `try`, `try?`, and `try!`.
It is important to understand the differences between these.

A `defer` block contains code to execute
before its containing block or function exits,
regardless of whether an error was thrown.
This is similar to a "finally" block in other languages.
The code in a `defer` block typically performs cleanup activities
such as closing files or database connections.

The following code demonstrates using the `do`/`catch` syntax:

```swift
enum IntError: Error {
    case negative(_ n: Int)
    case other
    case zero
    case tooHigh(_ n: Int, max: Int)
}

struct PositiveInteger {
    var n: Int

    init(_ n: Int, max: Int) throws {
        if (n < 0) { throw IntError.negative(n) }
        if (n == 0) { throw IntError.zero }
        if (n > max) { throw IntError.tooHigh(n, max: max) }
        // This is used to test a catch with no pattern.
        if (n == 7) { throw IntError.other }
        self.n = n
    }
}

do {
    // Try each of the variations below by uncommented it
    // and commenting all the others.

    let pi = try PositiveInteger(3, max: 10) // doesn't throw
    print("pi =", pi)

    //let pi = try PositiveInteger(0, max: 10) // throws zero
    //print("pi =", pi)

    //let pi = try PositiveInteger(-2, max: 10) // throws negative
    //print("pi =", pi)

    //let pi = try PositiveInteger(11, max: 10) // throws tooHigh
    //print("pi =", pi)

    //let pi = try PositiveInteger(7, max: 10) // throws other
    //print("pi =", pi)
} catch IntError.negative(let n) {
    print("negative numbers like \(n) are not allowed")
} catch IntError.zero {
    print("zero is not allowed")
} catch IntError.tooHigh(let n, let max) {
    print("value \(n) exceeds the maximum of \(max)")
} catch {
    print("error value is", error)
}
```

## Type Aliases

A type alias assigns an alternate name to another type.
By convention these names are CamelCase, beginning with an uppercase letter.

One use is to provide additional documentation for primitive types.
For example, the following documents the expected format of a `String`.

```swift
typealias MMDDYYYY = String
var date: MMDDYYYY = "04161961"
```

Another use of type aliases is to give a shorter name to a long name
or complicated type definition that is used multiple times.
For example, `typealias ToString = CustomStringConvertible`.

The type of a closure can be assigned to a `typealias`.
This is useful for declaring the types of functions
that are passed as arguments to other functions.
For example, the product function above can be written as follows:

```swift
typealias DoublePairToDouble = (Double, Double) -> Double
let product: DoublePairToDouble = {$0 * $1}
print(product(2, 3))
```

Swift does not support type unions like in TypeScript.
For example, it is not possible to define a type that can be
an `Int` or a `Double` using `typealias Number = Int | Double`.

A `typealias` can be used to give a name to a combination of protocols.
For example, the following type alias is defined by Swift.

```swift
typealias Codable = Decodble & Encodable
```

Instances of types that conform to this protocol
can be serialized and deserialized to and from formats like JSON.

```swift
struct Sport: Codable {
    let name: String
    let playerCount: Int
}

let hockey = Sport(name: "Hockey", playerCount: 6)

let encoder = JSONEncoder()
let decoder = JSONDecoder()
do {
    let data = try encoder.encode(hockey)
    let json = String(data: data, encoding: .utf8)!
    print(json) // {"name":"Hockey","playerCount":6}

    // The first argument to decode specifies the type to be created.
    let newSport = try decoder.decode(Sport.self, from: data)
    print(newSport) // Sport(name: "Hockey", playerCount: 6)
} catch {
    print("fail")
}
```

## Primitive Types

In Swift, values of these types are objects with properties and methods.

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

All of these types are defined as structs and are therefore immutable.

Variables of these types cannot be set to `nil`
unless a `?` follows the type to indicate that it is optional.
See the "Optionals" section later.

The `type` function returns the type of its argument as a `String`.

```swift
var c: Character = "x"
print(type(of: c)) // "Character"
```

### Bool Type

The `Bool` type has two possible values, `true` and `false`.

`Bool` instance properties include the following:

| Property      | Description                |
| ------------- | -------------------------- |
| `description` | `String` "true" or "false" |

`Bool` type methods include, but are not limited to the following:

| Method     | Description                        |
| ---------- | ---------------------------------- |
| `random()` | randomly returns `true` or `false` |

`Bool` instance methods include, but are not limited to the following:

| Method     | Description                                  |
| ---------- | -------------------------------------------- |
| `toggle()` | toggles the value between `true` and `false` |

Note that `myBool.toggle()` is equivalent to `myBool = !myBool`.

### Numeric Types

The type hierarchy of Swift numeric types is:

- Numeric
  - BinaryInteger
    - UnsignedInteger
      - UInt\*
    - SignedInteger
      - Int\*
    - FixedWidthInteger
      - Int* and UInt*
  - SignedNumeric
    - SignedInteger (see above)
    - FloatingPoint
      - BinaryFloatingPoint
        - CGFloat, Float\*, and Double

The `Int`, `Float`, and `Double` types have some common properties and methods.
But some are not shared by all of these types.

Global numeric constants include:

| Name     | Description               |
| -------- | ------------------------- |
| `M_E`    | `Double` value of e       |
| `M_PI`   | `Double` value of pi      |
| `M_PI_2` | `Double` value of half pi |

Number type properties include the following:

| Property                  | Description                               |
| ------------------------- | ----------------------------------------- |
| `greatestFiniteMagnitude` | maximum value of `Float` or `Double` type |
| `leastNonzeroMagnitude`   | minimum value of `Float` or `Double` type |
| `max`                     | maximum value of `Int` type               |
| `min`                     | minimum value of `Int` type               |
| `pi`                      | value of `Float` or `Double` Pi           |

Number type methods include, but are not limited to the following:

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

Number instance methods include, but are not limited to the following:

| Method                          | Description                                                     |
| ------------------------------- | --------------------------------------------------------------- |
| `isMultiple(of: Int) -> Bool`   | determines if the `Int` receiver is a multiple of another `Int` |
| `remainder(dividingBy: Double)` | returns remainder                                               |
| `round()`                       | mutates value to rounded value                                  |
| `rounded() -> Double`           | returns rounded value                                           |
| `signum() -> Int`               | returns -1, 0, or 1                                             |
| `squareRoot()`                  | returns square root                                             |

Many more math functions are defined in the `Foundation` framework.
This "provides a base layer of functionality for apps and frameworks,
including data storage and persistence, text processing,
date and time calculations, sorting and filtering, and networking."
Many math functions can only be used if the Foundation framework is imported.

```swift
import Foundation
print(abs(3.7)) // gives absolute value which is 3.
print(sin(45.0)) // 0.8509...
```

Numeric `Foundation` functions include:

| Function              | Description                               |
| --------------------- | ----------------------------------------- |
| `abs(number)`         | returns absolute value                    |
| `ceil(number)`        | returns ceiling value                     |
| `floor(number)`       | returns floor value                       |
| `rounded(number)`     | returns rounded value                     |
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

To create an `Int` by rounding a `Double`, use `Int(myDouble.rounded())`.

To format a `Double` value as a `String` with a limited number of decimal places
use the `String` initializer that takes a format string and the value.
For example:

```swift
let angle = 57.2169308
let formatted = String(format: "Angle: %.2f", angle) // 57.22
```

### Measurement Type

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/measurement",
"Measurement" %} struct from the Foundation library
is used to specify a quantity (`Double`) with a unit of measure.
Values of this type can be passed to the `Text` initializer
along with a `format` argument of type {% aTargetBlank
"https://developer.apple.com/documentation/foundation/measurement/formatstyle",
"Measurement.FormatStyle" %}.

The supported quantity types include `UnitAcceleration`, `UnitAngle`,
`UnitArea`, `UnitDispersion`, `UnitDuration`, `UnitElectricalCharge`,
`UnitElectricalCurrent`, `UnitEnergy`, `UnitFrequency`, `UnitFuelEfficiency`,
`UnitIlluminance`, `UnitLength`, `UnitMass`, `UnitPoint`, `UnitPower`,
`UnitPressure`, `UnitSpeed`, `UnitTemperature`, and `UnitVolume`.
Each of these define a set of class properties that specify the supported units.
For example, `UnitLength` defines the metric system class properties
`millimeters`, `centimeters`, `meters`, and `kilometers`,
the imperial system class properties `inches`, `feet`, `yards`, and `miles`,
and many more.

A `Measurement` has a `value` and a `unit`.

Measurements support many operators including `+`, `-`, `*`, `/`.
These operators return a new `Measurement` instance.
Only one operand can be a `Measurement`. The other must be a number.

Measurements support comparison operators like ==, <, and >.

Measurements can be converted to compatible units.

For example:

```swift
let metricLength = Measurement(value: 10, unit: UnitLength.centimeters)
let imperialWidth = Measurement(value: 7, unit: UnitLength.inches)
let metricWidth = imperialWidth.converted(to: metricLength.unit)
let metricArea = Measurement(
    value: metricLength.value * metricWidth.value,
    unit: UnitArea.squareCentimeters
) // 177.8 square centimeters
```

The SwiftUI `Text` view supports displaying `Measurement` instances
including their value and unit.

### String and Character Type

The {% aTargetBlank "https://developer.apple.com/documentation/swift/string",
"String" %} type is a `struct` that represents a sequence of Unicode characters.
Literal {% aTargetBlank
"https://developer.apple.com/documentation/swift/character", "Character" %}
and single-line `String` values are both delimited by double-quotes.

The type {% aTargetBlank
"https://developer.apple.com/documentation/swift/staticstring",
"StaticString" %} represents text that is known at compile-time
which means it is not formed using string interpolation.

Multi-line `String` values are delimited by triple double-quotes
that are on their own lines.
The lines of text between the delimiters should not be indented
because indentation whitespace is retained.
Newline characters other than the first and last are retained
unless each line but the last ends with a backslash.
For example:

```swift
let someParagraph = """
The newline at the end of this line is retained.
But the newline here is not retained \
because the line ends with a backslash.
"""
```

Strings are value types.
This means that assigning a `String` variable to another
makes a copy rather than assigning a reference to the same memory.

To insert expressions in string values use the string interpolation syntax.

```swift
let item = "milk"
let price = 2.59
let taxRate = 0.8
print("item \(item) costs \(price * (1 + taxRate))") // single string
print("item", item, "costs", price * (1 + taxRate)) // alternative
```

A new string can be created by concatenating existing strings
using the `+` operator.

A string can be appended to an existing string variable
using the `+=` operator.

To iterate over the characters in a `String`,
use a `for-in` loop or the `forEach` method.

```swift
let name = "Mark"

for c in name {
  print(c)
}

name.forEach({(c: Character) -> Void in print(c)})
name.forEach({c in print(c)}) // same as previous line
```

`String` instance properties include the following:

| Property  | Description                                     |
| --------- | ----------------------------------------------- |
| `count`   | number of current characters                    |
| `first`   | first character                                 |
| `isEmpty` | `Bool` value indicating whether `count` is zero |
| `last`    | last character                                  |

`String` instance methods include, but are not limited to the following:

| Method                                       | Description                                                    |
| -------------------------------------------- | -------------------------------------------------------------- |
| `append(Character)`                          | appends a given `Character` to the receiver                    |
| `append(String)`                             | appends a given `String` to the receiver                       |
| `contains(Character or String) -> Bool`      | determines if receiver contains a given character or substring |
| `dropFirst(Int) -> Substring`                | returns substring not including first n characters             |
| `dropLast(Int) -> Substring`                 | returns substring not including last n characters              |
| `firstIndex(of: Character or String) -> Int` | returns index of first occurrence of a character or substring  |
| `hasPrefix(String) -> Bool`                  | determines if receiver begins with a substring                 |
| `hasSuffix(String) -> Bool`                  | determines if receiver ends with a substring                   |
| `insert(Character, at: index)`               | inserts a given `Character` in the receiver                    |
| `lowercased() -> String`                     | returns lowercase version                                      |
| `popLast() -> Character?`                    | removes and returns last character                             |
| `prefix(Int) -> Substring`                   | returns first n characters                                     |
| `remove(at: index)`                          | removes and returns the character at a given index             |
| `removeAll()`                                | removes all characters                                         |
| `removeFirst([n])`                           | removes first n characters, defaulting to 1                    |
| `removeLast([n])`                            | removes last n characters, defaulting to 1                     |
| `removeSubrange(Range)`                      | removes characters in `Range`                                  |
| `replaceSubrange(Range, with: String)`       | replaces characters in a given range                           |
| `split(separator: Character) -> [Substring]` | returns `Array` of substrings delimited by a given character   |
| `sorted() -> [Character]`                    | returns `Array` of characters in sorted order                  |
| `suffix(Int) -> Substring`                   | returns last n characters                                      |
| `uppercased() -> String`                     | returns uppercase version                                      |

Indexes into strings have the type `String.Index` rather than `Int`.
This makes many string operations more verbose than in other languages
because obtaining a `String.Index` value requires a method call.

For example, the following gets the 2nd and 3rd characters of a string.

```swift
let name = "Mark"
let start = name.index(name.startIndex, offsetBy: 1)
let end = name.index(start, offsetBy: 1)
print(name[start...end]) // "ar"
```

Getting the character at a given index requires using the `index` method
which makes if quite verbose.

```swift
let name = "Mark"
print(name[name.index(name.startIndex, offsetBy: 2)]) // "r"
```

Fortunately we can override the subscript operator in an `extension`
to make this easier.

```swift
extension String: BidirectionalCollection {
    subscript(i: Index) -> Character { return characters[i] }
}

print(name[2]) // "r"
```

Comparing strings in a case-insensitive way is quite verbose.

```swift
if s1.name?.caseInsensitiveCompare(s2) == .orderedSame {
    print("s1 and s2 are the same when not considering case.")
}
```

Strings that contain numbers can be converted to numbers using casting
which returns an optional.
The following code demonstrates three approaches.

```swift
let i = "3"
let f = "3.14"
let d = "3.14159"

// "Int" returns an "optional".
// "if let" unwraps it if the conversion is successful.
// See the "Optionals" section for details.
if let number = Int(i) {
    print(number * 2) // 6
}

// Using nil-coalescing
print((Float(f) ?? 0) + 2) // 5.14

let number = Double(d)
if number != nil {
    // Using force unwrap
    print(number! + 2) // 5.14159
}
```

To create a `String` from a number with a specific format,
use the `NumberFormatter` class. For example,
this creates a `String` where that contains grouping commas:

```swift
let value = 1234567
let formatter = NumberFormatter()
formatter.numberStyle = .decimal
let number = NSNumber(value: value)
let formatted = formatter.string(from: number) ?? "" // 1,234,567
```

The `formatted` method can be applied to many kinds of values
to produce a locale-specific, formatted string.
See the example code at {% aTargetBlank
"https://github.com/mvolkmann/FormattingDemo/tree/main", "FormattingDemo" %}.

### Character Type

The `Character` type is a `struct` that represents a single Unicode character.
Literal `Character` values are delimited by double-quotes.

`Character` instance properties include the following:

| Property           | Description                                                          |
| ------------------ | -------------------------------------------------------------------- |
| `asciiValue`       | `UInt8` ASCII value, if it is an ASCII character                     |
| `description`      | textual representation                                               |
| `hashValue`        | `Int` hash value                                                     |
| `hexDigitValue`    | `Int` hex value                                                      |
| `isASCII`          | `Bool` value                                                         |
| `isCased`          | `Bool` value indicating if it is changed by case conversion          |
| `isCurrencySymbol` | `Bool` value                                                         |
| `isHexDigit`       | `Bool` value                                                         |
| `isLetter`         | `Bool` value                                                         |
| `isLowercase`      | `Bool` value                                                         |
| `isMathSymbol`     | `Bool` value                                                         |
| `isNewline`        | `Bool` value                                                         |
| `isNumber`         | `Bool` value; true for Unicode characters that represent a fraction  |
| `isPunctuation`    | `Bool` value                                                         |
| `isSymbol`         | `Bool` value                                                         |
| `isUppercase`      | `Bool` value                                                         |
| `isWhitespace`     | `Bool` value                                                         |
| `isWholeNumber`    | `Bool` value; false for Unicode characters that represent a fraction |
| `utf16`            | UTF-16 encoding value                                                |
| `utf8`             | UTF-8 encoding value                                                 |
| `wholeNumberValue` | `Int` value                                                          |

`Character` instance methods include, but are not limited to the following:

| Method         | Description                                   |
| -------------- | --------------------------------------------- |
| `lowercased()` | returns `Character` that is lowercase version |
| `uppercased()` | returns `Character` that is uppercase version |

## Dates

Swift provides the `Date` and `Calendar` structs for operating on dates.

The `formatted` method can return part of a `Date` as a `String`.
For example:

```swift
let now = Date() // run on September 3, 2022 at 2:16:41 PM
print(now.formatted(.dateTime.day())) // 3
print(now.formatted(.dateTime.dayOfYear())) // 246
print(now.formatted(.dateTime.era())) // AD
print(now.formatted(.dateTime.hour())) // 2 PM
print(now.formatted(.dateTime.minute())) // 16
print(now.formatted(.dateTime.month(.wide))) // September
print(now.formatted(.dateTime.quarter())) // Q3
print(now.formatted(.dateTime.second())) // 41
print(now.formatted(.dateTime.timeZone())) // CDT
print(now.formatted(.dateTime.week())) // 36
print(now.formatted(.dateTime.weekday(.wide))) // Saturday
print(now.formatted(.dateTime.year())) // 2022
```

For more date formatting options, see {% aTargetBlank
"https://developer.apple.com/documentation/foundation/dateformatter",
"DateFormatter" %}.

For details on date format strings, see {% aTargetBlank
"https://www.unicode.org/reports/tr35/tr35-31/tr35-dates.html#Date_Format_Patterns",
"Date Format Patterns" %}.

https://nsdateformatter.com

Date/time strings in ISO 8601 format have content like "2022-08-30T15:39:19Z".
To convert one of these strings to a Swift `Date`:

```swift
let formatter = ISO8601DateFormatter()
let date = formatter.date(from: dateString) ?? .now
```

To get the number of days between two dates:

```swift
let days = Calendar.current.dateComponents(
    [.day], from: date1, to: date2
).day ?? 0
```

## Ranges

There are many uses of ranges including:

- iterating over a range of values
- checking whether a value is in a range
- extracting a subset of data from a collection
- modifying a subset of a collection

Swift supports four kinds of ranges.

- `Range`: `start..<end`

  This is half-open, meaning that the end index is not included.
  `CountableRange` is a typealias of this type.

- `ClosedRange`: `start...end`
  This is a closed, meaning that the end index is included.
  `CountableClosedRange` is a typealias of this type.

- `PartialRangeFrom`: `start...`

  This is one-sided,
  meaning that the end index is not specified.

- `PartialRangeUpTo`: `..<end`
  This is one-sided at the end,
  meaning that the start index is not specified.

A range can be assigned to a variable.

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
They have a name and a list of possible cases.
Each `case` has a name and an optional value that can be
any type, though `Int`, and `String` are common types.
By convention both `enum` names and `case` names use camel-case,
but `enum` names begin uppercase and `case` names begin lowercase.
These names should be singular rather than plural.

Enums are value types like structs.
When an enum value is assigned to a variable or passed to a function,
it is copied rather than using a reference.

When `case` values are provided,
their type must be specified after the `enum` name.
These values are accessed with the `rawValue` property.
TODO: Why doesn't an enum name evaluate to its value like in other languages
so `rawValue` would not be needed?
TODO: Why isn't this property just named `value`?

If a type is provided after the `enum` name,
any cases without specified values are given default values.
For example, if the type is `Int`,
incrementing values starting with zero are assigned.
When the type is `String`, values matching the case name are assigned.

If no type is provided, the cases are not assigned default values.
This differs from many other programming languages.

```swift
enum Color {
    // Multiple cases can be specified on the same line.
    case red, green, blue
}

// The cases here have specified values.
enum ColorHex: String {
    case red = "ff0000"
    case green = "00ff00"
    case blue = "0000ff"
}
```

When the type of a value can be inferred to be a specific `enum` type,
a value can be specified with only a period followed by a `case` name
as a shorthand.
This is a distinguishing feature of Swift and is used frequently.

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

An enumeration `case` value can have associated data specified as a list.
Each piece of associated data:

- must specify its type
- can have a name for documentation, but is identified by position when used
- can have a default value which makes it optional
  to specify a value when creating an instance

Typically a `switch` statement is used to evaluate `enum` instances
and take different actions based on the `case`.
These must be exhaustive, meaning that either there is a `case`
for every possible value or the `default` case must be specified.

For example:

```swift
enum Activity {
    case swim(googles: Bool, inPool: Bool = true)
    case bike(Bike) // this associated data item has no name
    case run(shoes: Shoe)
    case sleep // no associated data
}

// CustomStringConvertible is a protocol implemented by types that can
// be converted to a string using the computed property "description".
struct Bike: CustomStringConvertible {
    var brand: String
    var model: String
    var description: String { "\(brand) \(model)" }
}

struct Shoe: CustomStringConvertible {
    var brand: String
    var model: String
    var description: String { "\(brand) \(model)" }
}

func printActivity(_ activity: Activity) {
    switch activity {
    // Variable names are not required to match associated data names.
    // These become local variables that are scoped to the case.
    // _ can be used for a name if it isn't used.
    case .swim(let g, let inP):
        print("swimming \(inP ? "in" : "out of") a pool \(g ? "with" : "without") goggles")
    case .bike(let bike):
        print("riding a \(bike)") // uses description
    case .run(let shoes):
        print("running in \(shoes) shoes") // uses description
    case .sleep:
        print("sleeping")
    }
}

var activity: Activity = .swim(googles: true)
printActivity(activity) // swimming in a pool with googles
activity = .bike(Bike(brand: "Cannondale", model: "Topstone"))
printActivity(activity) // riding a Cannondale Topstone
activity = .run(shoes: Shoe(brand: "Hoka", model: "Clifton"))
printActivity(activity) // running in Hoka Clifton shoes
activity = .sleep
printActivity(activity) // sleeping
```

If an `enum` conforms to the `CaseIterable` protocol then
its cases will be held in the `allCases` type property.
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

Like structs and classes (described later), enumerations can define
initializers, computed properties, and methods.
Enumerations cannot have regular (non-computed) properties.
The following `enum` defines a computed property and a method.

```swift
enum Color: String, CaseIterable {
    case red = "ff0000"
    case green = "00ff00"
    case blue = "0000ff"

    // Computed property
    var redHex: Substring {
        let hex = self.rawValue
        let start = hex.startIndex
        let end = hex.index(start, offsetBy: 1)
        return hex[start...end]
    }

    // Method
    func greenHex() -> Substring {
        let hex = self.rawValue
        let start = hex.index(hex.startIndex, offsetBy: 2)
        let end = hex.index(start, offsetBy: 1)
        return hex[start...end]
    }


    func temperature() {
        // Methods can test for the case by switching on "self".
        switch self {
        case .red: print("hot")
        case .green: print("comfortable")
        case .blue: print("cold")
        }
    }
}

print(Color.red.redHex) // ff
print(Color.green.redHex) // 00
print(Color.red.greenHex()) // 00
print(Color.green.greenHex()) // ff
let color: Color = .red
print(color.temperature()) // hot
```

## Generics

Swift supports generic types.
These can be used in many settings including
struct, class, and function definitions.

The following example defines a `Stack`
where all the elements have the same type.
The element type must be specified when a `Stack` instance is created
using a "type parameter".

Any number of type parameters can be specified
inside angle brackets, separated by commas.
By convention, type parameter names are CamelCase,
beginning with an uppercase letter.
Type parameters can be used anywhere a type can appear inside the definition
including properties, variables, parameters, and return types.

```swift
struct Stack<Element> {
    var elements: [Element] = []

    mutating func push(_ item: Element) {
        elements.append(item)
    }

    mutating func pop() -> Element {
        return elements.removeLast()
    }
}

var s = Stack<String>()
s.push("Moe")
s.push("Larry")
s.push("Curly")
//s.push(7) // Cannot convert value of type 'Int' to expected argument type 'String'
print(s.pop()) // Curly
print(s.pop()) // Larry
print(s.pop()) // Moe
```

The following code demonstrates implementing and using a generic linked list:

```swift
class Node<T> {
    var value: T
    var next: Node<T>? // cannot use Self

    init(value: T, next: Node<T>? = nil) {
        self.value = value
        self.next = next
    }

    func log() {
        print("\(value)")
        if let next = next {
            next.log()
        }
    }
}

typealias INode = Node<Int>
var node1 = INode(value: 1)
var node2 = INode(value: 2)
node1.next = node2
var node3 = INode(value: 3)
node2.next = node3
// Alternate version of the previous 5 lines.
// var node1 = INode(value: 1, next: INode(value: 2, next: INode(value: 3)))
node1.log()

typealias SNode = Node<String>
var fruitList = SNode(
    value: "apple",
    next: SNode(value: "banana", next: SNode(value: "cherry"))
)
fruitList.log()
```

The type of a type parameter can be constrained to only types
that conform to given protocols using the `where` keyword.
For example:

```swift
// The Equatable protocol requires a type
// to implement the == and != operators.
// The Identifiable protocol requires a type
// to have an "id" property that uniquely identifies an instance.
struct Stack<Element> where Element: Equatable & Identifiable {
    ...
}
```

There are two ways to define functions that take generic arguments.
These are equivalent, but the second is easier to read.

```swift
func update<T: CloudKitable>(item: T) async throws { ... }

func update(item: some CloudKitable) async throws { ... }
```

## Collection Types

Swift provides several generic collection types.

| Type         | Description                                                                                       |
| ------------ | ------------------------------------------------------------------------------------------------- |
| `Array`      | indexed collection of values with the same type                                                   |
| `Dictionary` | collection of key/value pairs where all keys have the same type and all values have the same type |
| `Set`        | unordered collection of values with the same type and no duplicates                               |
| tuple        | fixed-length, ordered collection of values whose types can differ                                 |

All of these types are defined as structs.

When a variable is set to a collection instance,
the elements in the collection can be modified.
However, when a constant is initialized to a collection instance,
the elements in the collection cannot be modified.

The following is a partial class hierarchy of the collection types
provided by Swift, including the protocols to which they conform:

- `Sequence` protocol
  - `Collection` protocol
    - `Dictionary` struct
    - `Range` struct
    - `Set` struct
    - `Slice` struct
    - `BidirectionalCollection` protocol
      - `RandomAccessCollection` protocol
        - `Array` struct
        - `Data` struct - byte buffer
        - `range` struct
      - `Range` struct
      - `String`
    - `MutableCollection` protocol
      - `Array` struct
      - `Data` struct - byte buffer
      - `Dictionary.Values` struct

The `Sequence` protocol defines operations for collections that
"provide sequential, iterated access to their elements".
Iterating can be destructive to the elements.

The `Collection` protocol defines operations for collections that
"can be traversed multiple times, nondestructively,
and accessed by an indexed subscript".

The `MutableCollection` protocol defines operations that
change the values of elements.
Methods include `reverse`, `shuffle`, and `sort`.

When an instance is assigned to another variable or passed to a function,
a copy-on-write version is created that shares the data with the original.
Operations that attempt to modify an instance have two possible behaviors.
If there is only one reference to the object, it is modified in place.
Otherwise the data is copied before being modified.

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

var numbers: [Int] = [] // can't infer type, so must specify; uses short form
var numbers: Array<Int> = [] // same as previous line; uses long form
var numbers = [Int]() // same as previous lines

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
scores[2...3] = [1, 2] // inclusive range; now [4, 5, 1, 2, 9]
```

To iterate over the elements in an `Array`, use a `for`/`in` loop.

```swift
for score in scores {
    print(score)
}
```

To iterate over the indices in an `Array`, use a `for`/`in` loop.

```swift
for index in 0..<scores.count {
    print(score)
}

// This is equivalent and reads better.
for index in scores.indices {
    print(score)
}
```

To access both the index and value of each element,
use the `enumerated` method.

```swift
for (index, score) in scores.enumerated() {
    print("score \(index + 1): \(score)")
}
```

`Array` instance properties include the following:

| Property   | Description                                                       |
| ---------- | ----------------------------------------------------------------- |
| `capacity` | number of elements that can be held without allocating more space |
| `count`    | number of current elements                                        |
| `first`    | optional first element; `nil` when empty                          |
| `isEmpty`  | `Bool` value indicating whether `count` is zero                   |
| `last`     | optional last element; `nil` when empty                           |

`Array` instance methods include, but are not limited to the following:

| Method                                                    | Description                                                                                |
| --------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| `allSatisfy((Element) -> Bool) -> Bool`                   | determines if every element satisfies predicate; like JS `every`                           |
| `append(Element)`                                         | adds new element at end                                                                    |
| `append(contentsOf: S)`                                   | adds elements in `S` at end                                                                |
| `compactMap<T>((Element) -> T) -> [T]`                    | returns `Array` of non-nil values returned by function                                     |
| `contains(Element) -> Bool`                               | determines if an element is a member                                                       |
| `contains(where: (Element) -> Bool) -> Bool`              | determines if some element satisfies predicate; like JS `some`                             |
| `drop(while: (Element) -> Bool) -> ArraySlice<Element>`   | returns subsequence of elements after those at beginning that match predicate              |
| `dropFirst(Int) -> ArraySlice<Element>`                   | returns subsequence of elements after first n                                              |
| `dropLast(Int) -> ArraySlice<Element>`                    | returns subsequence of elements before last n                                              |
| `enumerated(t) -> EnumeratedSequence`                     | returns subsequence of (n, x) pairs where n is an index and x is the element at that index |
| `filter((Element) -> Bool) -> [Element]`                  | returns a new array containing a subset of the elements that match predicate               |
| `first(where: (Element) -> Bool) -> Element?`             | returns first element that satisfies predicate                                             |
| `firstIndex(of: Element) -> Int?`                         | returns index of first element matching given element                                      |
| `firstIndex(where: (Element) -> Bool) -> Int?`            | returns index of first element that satisfies predicate                                    |
| `flatMap<T>((Element) -> T) -> [T]`                       | like `map`, but concatenates results that are arrays                                       |
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
| `partition(by: (Element) -> Bool) -> Int`                 | reorders elements using a predicate so all `false` are before all `true`                   |
| `popLast() -> Element?`                                   | removes last element and returns it; returns `nil` if empty                                |
| `prefix(Int) -> ArraySlice<Element>`                      | returns subsequence of first n elements                                                    |
| `prefix(while: (Element) -> Bool) -> ArraySlice<Element>` | returns subsequence of first elements that match a predicate                               |
| `randomElement()`                                         | returns a random element                                                                   |
| `reduce(Result, (Result, Element) -> Result) -> Result`   | takes an initial value and a function; returns result of combining elements                |
| `remove(at: Int) -> Element`                              | removes element at given index and returns it                                              |
| `removeAll(keepingCapacity: Bool)`                        | removes all elements, optionally retaining the capacity                                    |
| `removeAll(where: (Element) -> Bool)`                     | removes all elements that match a predicate                                                |
| `removeFirst() -> Element`                                | removes first element and returns it                                                       |
| `removeFirst(Int)`                                        | removes given number of elements from beginning                                            |
| `removeLast() -> Element`                                 | removes last element and returns it; crashes if empty                                      |
| `removeLast(Int)`                                         | removes given number of elements from end                                                  |
| `removeSubrange(Range<Int>)`                              | removes elements in range                                                                  |
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
| `subscript(Int)`                                          | returns element at given index                                                             |
| `subscript(Range<Int>)`                                   | returns `ArraySlice` of elements in given range                                            |
| `suffix(Int) -> ArraySlice<Element>`                      | returns subsequence of last n elements                                                     |
| `suffix(while: (Element) -> Bool) -> ArraySlice<Element>` | returns subsequence of last elements that match a predicate                                |
| `swapAt(Int, Int)`                                        | swaps elements at the given indexes                                                        |

Here are examples of using some of these methods.

```swift
let data = [1, 4, 7, 10, 16]

let doubled = data.map { $0 * 2 } // uses a trailing closure
print(doubled) // [2, 8, 14, 20, 32]

let allData = [1, nil, 7, nil, nil, 10]
let goodData = allData.compactMap { $0 }
print(goodData) // [1, 7, 10]

let evens = data.filter { $0 % 2 == 0}
print(evens) // [4, 10, 16]

// This shows three ways to use the Array reduce method to sum numbers.

// Passing a closure as the last argument.
let sum = data.reduce(0, {acc: Int, n: Int in acc + n}) // 38

// Using a trailing closure.
let sum = data.reduce(0) {acc: Int, n: Int in acc + n} // 38

// Passing a binary operator.
// This works because binary operators are
// implemented as functions that take two arguments
let sum = data.reduce(0, +) // 38
print(sum)

let prices = [1.23, 5.79, 3.48]
let quantities = [5, 2, 4, 7, 1] // 7 and 1 are ignored
// The zip function only takes two sequences
// and only uses elements up to the shortest count.
// It returns a Sequence, not an Array.
let zipped = Array(zip(prices, quantities))
print(zipped) // [(1.23, 5), (5.79, 2), (3.48, 4)]
print(zipped[2].0) // 3.48
```

### Sets

Sets are created by assigning an `Array` literal
or by passing elements to the `Set` initializer.

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

`Set` instance properties include the following:

| Property   | Description                                                       |
| ---------- | ----------------------------------------------------------------- |
| `capacity` | number of elements that can be held without allocating more space |
| `count`    | number of current elements                                        |
| `isEmpty`  | `Bool` value indicating whether `count` is zero                   |

`Set` instance methods include, but are not limited to the following:

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
they can be any type that conforms to the `Hashable` protocol.
For details on the `Hashable` protocol, see the
{% aTargetBlank "https://developer.apple.com/documentation/swift/hashable",
"Apple Developer Documentation" %}.

```swift
// When the type can't be inferred, it must be specified.
// The syntax for an empty Dictionary includes a colon.
var pairs: [Int : String] = [:] // uses short form of the type
var pairs: Dictionary<Int, String> = [:] // same as previous line; uses long form
var pairs = [Int : String]() // same as previous lines

// The type of fruitColors is inferred to be [String : String].
var fruitColors = ["apple": "red", "banana": "yellow", "orange": "orange"];
```

To set the value of a given key, use the subscript operator `[]`
and an assignment.

```swift
fruitColors["strawberry"] = "red"
```

To get the value of a given key, use the subscript operator `[]`
which returns an `Optional`:

```swift
let color = fruitColors["banana"]; // Optional("yellow")
```

To delete a key pair:

```swift
fruitColors.removeValue(forKey: "banana") // returns value
fruitColors["banana"] = nil // same, but shorter and doesn't return value
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

The `default` argument provides default values for missing keys.

```swift
// This holds the sum of the scores for each person.
var scoreMap: [String: Int] = [:]

func addScore(_ score: Int, to name: String) {
    scoreMap[name, default: 0] += score
}

addScore(85, to: "Mark")
addScore(90, to: "Tami")
addScore(93, to: "Mark")
print(scoreMap) // ["Mark": 178, "Tami": 90]

// This holds an Array of all the scores for each person.
var scoresMap: [String: [Int]] = [:]

func collectScore(_ score: Int, to name: String) {
    scoresMap[name, default: []].append(score)
}

collectScore(85, to: "Mark")
collectScore(90, to: "Tami")
collectScore(93, to: "Mark")
print(scoresMap) // ["Mark": [85, 93], "Tami": [90]]
```

`Dictionary` instance properties include the following:

| Property   | Description                                                    |
| ---------- | -------------------------------------------------------------- |
| `capacity` | number of pairs that can be held without allocating more space |
| `count`    | number of current elements                                     |
| `isEmpty`  | `Bool` value indicating whether `count` is zero                |
| `keys`     | `Keys` `Collection` of keys                                    |
| `values`   | `Values` `MutableCollection` of values                         |

`Dictionary` instance methods include, but are not limited to the following:

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
| `updateValue(newValue, forKey: key) -> prevValue`     | sets value of given key and returns previous value or `nil` if not present      |

Here are examples of using some of these methods.

```swift
let scoreDict = ["Mark": 19, "Tami": 20, "Amanda": 18, "Jeremy": 17]
let allOdd = scoreDict.allSatisfy({$0.value % 2 == 1}) // false
let someOdd = scoreDict.contains(where: {$0.value % 2 == 1}) // true
let odds = scoreDict.filter({$0.value % 2 == 1}) // includes Mark & Jeremy elements
let upperNames = scoreDict.map({$0.key.uppercased()}) // array of uppercase names
let doubledScoresDict = scoreDict.mapValues({$0 * 2}) // scoreDict w/ doubled scores
scoreDict.removeValue(forKey: "Mark")
let sorted = scoreDict.sorted(by: {$0.value > $1.value}); // descending
```

### Tuples

To define a tuple type, provide a list of elements types in parentheses.
These can be named or unnamed.
Access elements by zero-based index or by name.

```swift
typealias MyUnnamedTuple = (Bool, Int, String)
let t1: MyUnnamedTuple = (true, 19, "Mark")
print(t1.1) // accessed by zero-based index; 19

typealias MyNamedTuple = (happy: Bool, score: Int, name: String)
let t2: MyNamedTuple = (happy: true, score: 19, name: "Mark")
print(t2.score) // accessed by name; 19
```

The elements of a tuple can be assigned to variables using destructuring,
All of the elements must be assigned, not just a subset.

```swift
let myTuple = (true, "test", 1, 2.3)
let (b, s, i, d) = myTuple
```

Swift only supports destructuring of tuples, not arrays or objects.

## Variables

Mutable variables are declared with the `var` keyword.
Immutable variables are declared with the `let` keyword.
This is an unfortunate choice for JavaScript developers where
`let` is used for mutable variables and
`const` is used for immutable variables.

Variable names can be followed by a colon and a type.
They can also be followed by `=` and an initial value.
The type can be omitted if the desired type
can be inferred from the initial value.

```swift
let name1: String = "Mark" // type and initial value

let name2 = "Mark" // only value; String type is inferred

var score1: Int // only type; can't use until initialized
score1 = 19 // initializes

var score2 = 19 // only value; inferred type is Int

var score3 = 1.23 // inferred type is Double, not Float
```

Multiple variables can be declared on the same line.

```swift
let a = 1, b = 2.3, c = true, d = "test"

var e: Int?, f: Double?, g: Bool?, h: String?
e = 1
f = 2.3
g = true
h = "test"
```

Swift is able to optimized storage of `let` variables more than `var` variables.
Because the value of a `let` variable never changes, its size is known.
This allows it to be allocated on the stack rather than the heap.
Data on the stack can be accessed more efficiently.
Values of `let` variables can also be inlined in the generated code.

## Type Checking and Casting

Basic type names can be used as functions
to cast another primitive type to a given type.
The `Bool` and `Characters` types can be cast to and from a `String`,
but no other types.

The following code demonstrates the supported primitive type casts.

```swift
let b = true
let i = 3
let f = 3.14
let d = 3.14159
let c: Character = "4"
var s = "7"

print(String(b)) // "true"

print(Float(i)) // 3.0
print(Double(i)) // 3.0
print(String(i)) // "3"

print(Int(f)) // 3
print(Double(f)) // 3.14
print(String(f)) // "3.14"

print(Int(d)) // 3
print(Float(d)) // 3.14159
print(String(d)) // "3.14159"

print(String(c)) // "4"

let bStr = "true"
print(Character(s)) // "7"; Fatal Error if String contains more than one character
// The remaining examples use nil-coalescing
// because the typecasts return nil if they fail.
print(Bool(bStr) ?? false) // true
print(Int(s) ?? 0) // 7
print(Float(s) ?? 0) // 7.0
print(Double(s) ?? 0) // 7.0
```

The `is` operator is used to check the type of an expression
and returns a `Bool`.

The `as?` operator is used to downcast a
value of a superclass type to a value of a subclass type.
Since this can fail, it is typically used in an `if let` statement.
The code example below uses the `is` and `as?` operators
in the `evaluate` function.

```swift
class Animal: CustomStringConvertible {
    var name: String

    init(name: String) {
        self.name = name
    }

    // This is a computed property that is required
    // by the CustomStringConvertible protocol.
    // It is used when a variable of this type
    // is printed or converted to a string.
    public var description: String { return name }
}

class Cat: Animal {
    var declawed: Bool

    init(name: String, declawed: Bool) {
        self.declawed = declawed
        // All properties in this class must be initialized
        // before the superclass init method is called.
        super.init(name: name)
    }
}

class Dog: Animal {
    var hasTail: Bool

    init(name: String, hasTail: Bool) {
        self.hasTail = hasTail
        super.init(name: name)
    }
}

let a = Animal(name: "Mystery")
let c = Cat(name: "Whiskers", declawed: true)
let d = Dog(name: "Comet", hasTail: true)

func evaluate(_ animal: Animal) {
    if animal is Cat {
        print("\(animal) is a cat. ")
    } else if animal is Dog {
        print("\(animal) is a dog.")
    } else {
        print("\(animal) is an unknown kind of animal.")
    }

    // The as? operator attempts to perform downcasting to a subclass type.
    // This is necessary to access properties
    // or call methods defined in a subclass.
    if let cat = animal as? Cat {
        print("has claws? \(!cat.declawed)")
    }
    if let dog = animal as? Dog {
        print("has tail? \(dog.hasTail)")
    }
}

evaluate(a) // Mystery is an unknown kind of animal.
evaluate(c) // Whiskers is a cat.\nhas claws? false
evaluate(d) // Comet is a dog.\nhas tail? true
```

## Optionals

Variables must be assigned a value before they are accessed
unless they have an `Optional` type.
This is a generic `enum` with the following definition:

```swift
enum Optional<T> {
    case none
    case some(T) // associated data of type T
}
```

Adding `?` after a type name is syntactic sugar
for creating an `Optional` value.
It allows the value to be `nil`.

The following are equivalent:

| Full Syntax                                  | Syntactic Sugar                       |
| -------------------------------------------- | ------------------------------------- |
| `var name: Optional<String> = .none`         | `var name: String?` (defaults to nil) |
| `var name: Optional<String> = .some("Mark")` | `var name: String? = "Mark`           |

There are several ways to extract the value from
a variable or property with an optional type.

- if let

  `if let unwrapped = someOptional { ... }`

  If the value is not `nil`, it is assigned to the variable `value`
  (which is scoped to the block that follows) and the block is executed.
  If the value is `nil`, the block is not executed.
  An `else` block can be included to specify
  code to run when the value is `nil`.

  It is common to unwrap an optional held in a variable (`someOptional` above)
  into a variable with the same name (`unwrapped` above).
  The one on the left shadows the one on the right inside the block.
  For example, `if let result = result { ... }`

- optional chaining

  `let value = myOptionalObject?.someProperty;`

  This uses the optional chaining operator `?.`.
  If `myOptionalObject` is `nil`, `value` will be set to `nil`.
  Otherwise it will be set to the value of `someProperty` in the object.
  This operator can also precede method calls
  to avoid calling them if the receiver is `nil`.

- nil-coalescing

  `let value = myDictionary[someKey] ?? defaultValue`

  This uses the nil-coalescing operator `??`
  to get either the unwrapped value or a default value.
  The example above gets a value from `myDictionary` if it exist
  or uses a default value.

- guard

  `guard let value = myOptional else { ... }`

  This uses a "guard" to
  assign the value of an optional to a variable if it is not `nil`
  or run the code in the `else` block if it is `nil`.
  In a sense this it the opposite of the `if let` syntax.
  It is typically used near the beginning of function bodies
  to check the value of an argument and exit if it is unacceptable.
  For this reason, the `else` block usually contains a `return` statement.

- force unwrap

  `if myOptional != nil { let value = myOptional!; ... }`

  This uses the `!` operator to "force unwrap" an optional.
  If the optional set to `nil`, the program will crash with a fatal error.
  Use this option only when the value should never be `nil`.

Here are more examples of working with optionals.

```swift
var message: String? // optional type

// Test for a value AND unwrap into another variable if not nil.
// Executes a block of code depending on whether the value is nil.
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
// This uses the "force unwrap" operator.
print(message == nil ? "no message" : message!) // "Hello, World!"
printMessage(message) // "Hello, World!"

message = nil
print(message == nil ? "no message" : message!) // "no message"
printMessage(message) // no output
```

## Control Structures

Swift does not require parentheses around conditions in control structures.
It does require the code to be executed to be in a block
surrounded by curly braces, even if they only contain a single statement.

### Conditional Logic

Conditional logic is implemented with the `if` statement
that can optionally include `else if` and `else` parts.

```swift
if score1 == 21 && score2 <= 19 {
  print("Player 1 has won by at least 2.")
} else if score1 > score2 {
  print("Player 1 is leading.")
} else {
  print("Player 1 is not leading.")
}
```

Conditions being tested must evaluate to a `Bool`.
Other types are not automatically coerced to a `Bool` value.
For example, an `Int` with a value of 0 is not treated as `false`.

Conditions can be specified by a comma-separated list of expressions
which must all evaluate to `true`.
This is useful in `if let` and `guard let` statements.
The advantage using commas has over using the `&&` operator
is that `let` variables can be accessed in subsequent conditions.

In the following example, assume that the `firstPlayer` method
returns an optional `Player` object with a `score` property.

```swift
struct Player {
  var name: String
  var score: Int
}

struct Game {
    var players: [Player] = []
}

let game = Game()

// This gives the error "Cannot find 'player' in scope".
// because the player variable IS NOT available after &&.
if let player = game.players.first && player.score == 0 {
    print("first player has no score")
}

// This works because the player variable IS available after the comma.
if let player = game.players.first, player.score == 0 {
    print("first player has no score")
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
that matches every possible value of the expression,
i.e. it must be exhaustive.
When the `switch` expression is a `String`,
it's not possible to have a `case` for every possible value
and the `default` case is required.
If the value of the `switch` expression is an `enum`,
there must either be a `case` that matches each value of the `enum`
or the `default` case must be included.

### Iteration

To iterate over a range of integers, use a `for`-`in` loop with a range operator.
See the "Ranges" section for more detail on these.

```swift
for frame in 1...10 {
    print(frame) // prints integers from 1 to 10 inclusive
}
```

If the current iteration value is not needed,
use an underscore for the variable name.

```swift
let times = 3
for _ in 0..<times {
    print("Hello") // prints 3 times
}
```

To iterate over elements in a sequence (such as an array),
use a `for`-`in` loop.

```swift
let names = ["Maisey", "Ramsay", "Oscar", "Comet"]
for name in names { // variable name does not need to be declared
    print(name)
}
```

To iterate over elements in a sequence and also get index values,
use the `enumerated` method.

```swift
for (index, name) in names.enumerated() {
    print("\(index + 1): \(name)")
}
```

A `where` clause can be added to restrict the items processed.

```swift
enum Size { case small, medium, large }

struct Dog {
    let name: String
    let breed: String
    let size: Size
}

let dogs = [
    Dog(name: "Maisey", breed: "Treeing Walker Coonhound", size: .medium),
    Dog(name: "Ramsay", breed: "Native American Indian Dog", size: .large),
    Dog(name: "Oscar", breed: "German Shorthaired Pointer", size: .medium),
    Dog(name: "Comet", breed: "Whippet", size: .small),
]

for dog in dogs where dog.size == .medium {
    print(dog.name) // Maisey and Oscar
}
```

To iterate while a condition is true, use a `while` loop which is top-tested.

```swift
while condition {
    statements
}
```

For a bottom-tested loop that always runs at least once,
use a `repeat`-`while` loop.

```swift
repeat {
    statements
} while condition
```

## Structs

Structs define named groups of properties and methods.
By convention their names begin with an uppercase letter.
Typically these names are singular rather than plural.

Structs cannot inherit from another struct or class,
but they can conform to any number of protocols which
are similar to interfaces in other languages.

Properties are declared with the `let` (immutable) and
`var` (mutable) keywords, just like variables.
Mutable properties can only be modified if the instance is also mutable
(assigned to a `var`).

Changing a mutable struct property creates a new instance of the struct
rather than modifying the single property of the existing instance.
For example:

```swift
struct Person {
    var name: String // using `var` makes this a mutable property
}

// We must use `var` here instead of `let`
// to enable changing mutable properties.
var p = Person(name: "Mark")

// This changes `p` to refer to a
// new instance of the `Person` struct.
p.name = "Tami"
```

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
and passing arguments required by an initializer.
Note that unlike in many other languages,
use a `new` keyword is not required.

```swift
// Create an instance using the provided memberwise initializer.
var dog = Dog(breed: "Whippet", name: "Comet", age: 1)
print("\(dog.name) is a \(dog.breed)") // Comet is a Whippet
```

Properties of structs and classes whose values are allowed to be `nil`
are considered to be `Optional`.
The shorthand syntax for indicating this is to add a `?` after their type.
Just like with optional variables, they must be unwrapped to access their value.

If all of the properties are optional or have a default value,
a default initializer that takes no arguments is provided.

Structs are value types. This means that assigning one to a variable
creates a copy rather than assigning a reference to the same instance.
This uses copy-on-write so that an actual copy
is not made until it is modified.

```swift
var dog2 = dog // dog2 is a copy of dog.
dog.age = 2 // This change doesn't affect dog2.
print(dog.age, dog2.age) // 2 1
```

### Properties

Properties of a struct, class, or enum can be "stored" or "computed".
Computed properties are like methods that take no arguments
and compute their value based on the values of other properties.

Computed properties must be declared with `var` rather than `let`.
A type must be specified and is followed by
a code block with no equal sign preceding it.

Computed properties always define a `get` function
that computes the value every time it is referenced.
They can optionally define a `set` function
whose purpose is the change the values of properties used to
compute their value so the result will be a given value.
If there is no `set` function then a surrounding `get` block is not needed.
Often there is no need for a `set` function.

```swift
struct Counter {
    private var n = 1

    // Computed property with no set function.
    var doubled: Int { n * 2 }

    // Computed property with both get and set functions.
    var tripled: Int {
        get {
            n * 3
        }
        set {
            n = newValue / 3 // truncates
        }
    }
}

var counter = Counter() // n is initially set to 1
print(counter.tripled) // returns 1 * 3 = 3, but doesn't change n

counter.tripled = 9 // changes n to 3
print(counter.doubled) // 3 * 2 = 6
print(counter.tripled) // 3 * 3 = 9
```

A lazy property is similar to a computed property,
but its value is only computed the first time it is accessed.
This is ideal for expensive computations that may not be accessed.
It must be declared with `var` and
can be mutated after its initial value is computed.
Lazy properties are not thread safe and will be computed again in each thread.

### Property Observers

Property observers are methods on a property that are invoked
before (`willSet`) and after (`didSet`) the value changes.

The `willSet` method is passed the new value.
If no parameter name is supplied, the new value will
be available in the supplied variable `newValue`.
The value cannot be changed in this method.

The `didSet` method is passed the old value.
If no parameter name is supplied, the old value will
be available in the supplied variable `oldValue`.
The value can be changed in this method.

This can be useful in "view model" classes used in a SwiftUI app.
For example:

```swift
@MainActor
class Game: ObservableObject {
    var score = 0 {
        willSet {
            print("willSet: old value = \(score)")
            print("willSet: new value = \(newValue)")
        }
        didSet {
            print("didSet: old value = \(oldValue)")
            print("didSet: new value = \(score)")
            if score < 5 {
                // Notify subscribers of the change.
                // This is an explicit alternative to adding the
                // @Published property wrapper to the score property.
                objectWillChange.send()
            } else {
                print("didSet: reset to old value")
                score = oldValue
            }
        }
    }
}
```

### Methods

Methods are defined with the `func` keyword.
They can use the `self` keyword to refer the instance on which they are invoked.
Definitions of `struct` methods that modify properties of the receiver must
begin with the `mutating` keyword to explicitly indicate that they do this.

```swift
import Foundation // needed to use functions like sin, cos, and atan
// Importing UIKit imports Foundation for you.

struct Dog {
    var breed: String
    var name: String
    var age = 0 // has default value; type inferred to be Int
}

//var dog = Dog(breed: "Whippet", name: "Comet") // uses default value for age
var dog = Dog(breed: "Whippet", name: "Comet", age: 1)
print("\(dog.name) is a \(dog.age) year old \(dog.breed)")

var dog2 = dog // creates a copy
dog.age = 2
print(dog.age, dog2.age) // 2 1

/// This represents a 2D Cartesian point.
struct Point {
    // This is a "type property".
    // Other languages refer to this as a "class property".
    // This is used to keep track of the largest y value
    // ever assigned to any instance of a Point struct.
    static var maxY = -Double.greatestFiniteMagnitude

    // This is a basic property.
    var x: Double

    // This property has a property observer.
    var y: Double {
        willSet { // runs before changes; can access newValue
            print("y is about to change to \(newValue)")
        }
        didSet { // runs after changes; can access oldValue
            Point.maxY = Double.maximum(Point.maxY, y)
            print("y changed from \(oldValue) to \(y)")
        }
    }

    // A "memberwise initializer" is automatically supplied.
    // Writing this yourself enables doing more
    // than just assign property values.
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

    // This is a lazy property.  Its value is computed by
    // a function only the first time it is referenced.
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
print("initial distance =", pt.initialDistance) // uses previous value; 5

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
The following code demonstrates this.

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
Typically these names are singular rather than plural.

Classes do the following things that structs cannot.

- refer to instances by reference rather than making a copy
- inherit properties and methods from one other class
- use type casting to determine if an object is an instance at runtime
- define a "deinitializer" to perform cleanup when an instance is destroyed
- use automatic reference counting (ARC) for garbage collection

Classes are reference types. This means that assigning one to a variable
assigns a reference to the same instance rather than making a copy.
Use a class instead of struct when it is important to
share data by holding multiple references to the same object.

To define a class, use the `class` keyword.
A class can have:

- one or more initializers (named `init`)
  that play the role of constructors in other languages
- type (or class-level) properties declared with `class` instead of `static`
- type (or class-level) methods declared with `class` instead of `static`
- instance-level properties
- instance-level methods
- a superclass

The default value of a property cannot depend on the value of another property.

Classes, unlike structs, are not provided with a default memberwise initializer.
They are provided with an initializer that takes no arguments
only if all of their properties have a default value,
It's not clear why this difference exists.

Definitions of `class` methods that modify properties of the receiver
cannot begin with `mutating` keyword.
That is only applied to `struct` and `extension` methods.

Typically structs are used instead of classes
when the need for inheritance is not anticipated.

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

Instance properties and instance methods that
refer to type properties and methods must prefix them
with the class name followed by a period.
However, initializers and type methods do not require prefixing
to refer to type properties and methods.

To implement a subclass that inherits
the properties and methods of a superclass,
add a colon after the subclass name followed by the superclass name.

Methods in a subclass that override methods in the superclass
must begin with the `override` keyword.

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

class Programmer: Person {
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

    override func log() {
        let langs = languages.joined(separator: " & ")
        print("\(name) is a programmer that knows \(langs).")
    }
}

var mark = Programmer(name: "Mark", languages: ["JavaScript", "Swift"])
mark.log() // Mark is a programmer that knows JavaScript & Swift.
```

Marking classes as `final` prevents defining subclasses
and can improve build times.

Definitions of both classes and structs can be nested inside each other
to limit their visibility and/or scope their names.

## Initializers

Structs and classes can define `init` methods
that are called when instances are created.
They do not begin with the `func` keyword.
Those that initialize ALL of non-optional properties (no default value)
are referred to as "designated initializers".
Designated initializers for classes
must also initialize all inherited properties,
typically by calling a superclass `init` method.

As seen earlier, structs provided a default initializer.
Structs are not required to explicitly defining more.
Classes do not provide a default initializer
and at least one must be explicitly defined.

There can be more than one `init` method as long as each has a
different set of argument labels and initializes all of the properties.
Classes typically only define one designated initializer.

If a class inherits from another,
its designated initializers must call one in its immediate superclass.
If an initializer has the same signature as one in the superclass,
it must indicate that it is overriding the one in the superclass
by adding the `override` keyword before `init`.

Just like with functions and other kinds of methods,
`init` methods can use `_` for argument labels
to allow values to be passed without a label.

"Convenience initializers" are `init` methods that invoke another
designated or convenience initializer in the same struct or class.
These must be labeled with the `convenience` keyword.
TODO: Why doesn't Swift allow any `init` method to do this?
Congratulations Swift for having what may be
the longest keyword in any programming language!

"Failable initializers" are methods named `init?` that can return `nil`
to prevent an instance from being created
if the supplied arguments are deemed invalid.
An optional value is returned when an instance is created
using a failable initializer.
As with any optional value, callers must
test and unwrap the value in order to use it.

Deinitializers are methods named `deinit` that are used to perform cleanup.
They do not begin with the `func` keyword, have no parameters,
and cannot be followed by a pair of parentheses.
If a struct or class defines this method,
it will be called when any instance is destroyed.

I have two issues with the method name `deinit`.
First, "deinitialize" is not a word.
Second, the method doesn't necessarily only
clean up after actions taken in an initializer.
It can perform cleanup of actions taken by any method.
A better name would have been "cleanup" or "onDestroy".

## Extensions

Extensions add functionality to existing types,
including structs, classes, enums, and protocols.
This can even be done to types defined by the standard library.

The functionality that can be added includes:

- computed type and instance properties
- initializers
- type and instance methods
- conformance to a protocol

Extensions cannot add regular properties (a.k.a stored properties).

Definitions of `extension` methods that modify properties of the receiver must
begin with the `mutating` keyword to explicitly indicate that they do this.

Strings in Swift are notoriously difficult to work with
because indexes to characters within them must be specified
using the type `String.Index` instead of `Int`.
String operations that require finding a character at a given index
are O(n) due to the fact that Unicode characters can vary in length.
These operations require traversing all the characters from the beginning,
and is the reason Swift doesn't support them out of the box.
However, these operations are commonly needed and
I think these operations should be supported despite being O(n).

It is common to use an extension to the `String` class
to add support for `Int` indexes to the subscript operator.
The following is an implementation of this idea.

When overriding the subscript operator,
five argument types should be supported.

- single index: `start`
- `Range`: `start..<end`
- `ClosedRange`: `start...end`
- `PartialRangeFrom`: `start...`
- `PartialRangeUpTo`: `..<end`

```swift
public extension String {
    // Handles negative indexes by counting from end of string.
    func getOffset(_ i: Int) -> Int {
        let count = self.count
        var offset = i >= 0 ? i : i + count
        offset = offset < 0 ? 0 : offset > count ? count : offset
        return offset
    }

    // single index
    subscript (_ i: Int) -> String {
        get {
            let offset = getOffset(i)
            if offset >= self.count { return "" }
            let idx = index(self.startIndex, offsetBy: offset)
            return String(self[idx])
        }
        set {
            let offset = getOffset(i)
            let idx = index(self.startIndex, offsetBy: offset)
            replaceSubrange(idx...idx, with: newValue)
        }
    }

    // start..<end
    subscript (_ r: Range<Int>) -> String {
        get {
            let startOffset = getOffset(r.lowerBound)
            let endOffset = getOffset(r.upperBound)
            let si = index(self.startIndex, offsetBy: startOffset)
            let ei = index(si, offsetBy: endOffset - startOffset)
            return String(self[si..<ei])
        }
        set {
            let startOffset = getOffset(r.lowerBound)
            let endOffset = getOffset(r.upperBound)
            let si = index(self.startIndex, offsetBy: startOffset)
            let ei = index(si, offsetBy: endOffset - startOffset)
            replaceSubrange(si..<ei, with: newValue)
        }
    }

    // start...end
    subscript (_ r: ClosedRange<Int>) -> String {
        get {
            let startOffset = getOffset(r.lowerBound)
            var endOffset = getOffset(r.upperBound)
            if endOffset >= self.count { endOffset -= 1 }
            let si = index(self.startIndex, offsetBy: startOffset)
            let ei = index(si, offsetBy: endOffset - startOffset)
            return String(self[si...ei])
        }
        set {
            let startOffset = getOffset(r.lowerBound)
            var endOffset = getOffset(r.upperBound)
            if endOffset >= self.count { endOffset -= 1 }
            let si = index(self.startIndex, offsetBy: startOffset)
            let ei = index(si, offsetBy: endOffset - startOffset)
            replaceSubrange(si...ei, with: newValue)
        }
    }

    // start...
    subscript (_ r: PartialRangeFrom<Int>) -> String {
        get {
            let startOffset = getOffset(r.lowerBound)
            let idx = index(self.startIndex, offsetBy: startOffset)
            return String(self[idx...])
        }
        set {
            let startOffset = getOffset(r.lowerBound)
            let idx = index(self.startIndex, offsetBy: startOffset)
            replaceSubrange(idx..., with: newValue)
        }
    }

    // ..<end
    subscript (_ r: PartialRangeUpTo<Int>) -> String {
        get {
            var endOffset = getOffset(r.upperBound)
            if endOffset >= self.count { endOffset -= 1 }
            let idx = index(self.startIndex, offsetBy: endOffset)
            return String(self[...idx])
        }
        set {
            var endOffset = getOffset(r.upperBound)
            if endOffset >= self.count { endOffset -= 1 }
            let idx = index(self.startIndex, offsetBy: endOffset)
            replaceSubrange(...idx, with: newValue)
        }
    }

    // using two Int arguments instead of a range
    func substring(_ start: Int, _ end: Int) -> String {
        return self[start...end]
    }
}
```

The following code demonstrates using the methods
added by the extension above.
It uses the built-in {% aTargetBlank
"https://developer.apple.com/documentation/swift/1541112-assert",
"assert" %} function.

```swift
var text = "Mark"

// Positive indexes
assert(text.substring(1, 2) == "ar")
assert(text[1] == "a")
assert(text[1...2] == "ar")
assert(text[1..<3] == "ar")
assert(text[1...] == "ark")
assert(text[..<2] == "Mar")

// Negative indexes
// These must be wrapped in parentheses or
// separated from the range operator by a space.
assert(text[-2] == "r")
assert(text[-3 ... -2] == "ar")
assert(text[-3 ..< -1] == "ar")
assert(text[(-3)...] == "ark") // parens work, but a space doesn't
assert(text[..<(-2)] == "Mar") // parens work, but a space doesn't

// Handling indexes that are out of range
assert(text.substring(1, 7) == "ark")
assert(text[7] == "")
assert(text[1...7] == "ark")
assert(text[1..<7] == "ark")
assert(text[7...] == "")
assert(text[..<7] == "Mark")

// Setting substrings
text[1] = "o"
assert(text == "Mork")
text[1..<3] = "as"
assert(text == "Mask")
text[1...2] = "il"
assert(text == "Milk")
text[1...] = "eet"
assert(text == "Meet")
text[..<2] = "Goa"
assert(text == "Goat")
```

## Protocols

A protocol is like an interface in other programming languages.
It can describe type properties, instance properties,
type method signatures, and instance method signatures.
Properties can be read-only (`let`) or read/write (`var`).

Protocol names have commonly used suffixes.
Protocols that enable a set of actions end in "able" (ex. `Comparable`)
Protocols that convert one data type to another end in "Convertible"
(ex. `CustomStringConvertible`).
Protocols that give responsibility to another object end in "Delegate"
(ex. `UIApplicationDelegate`).

Other types (structs, classes, and enums)
can declare that they conform to protocols by following their name
with a colon and a comma-separated list of protocol names.
The same syntax can be used by a protocol
to inherit the requirements of other protocols.

A protocol defines a type that can be used in many places
where concrete types can appear in order to state that any value
whose concrete type conforms to the protocol is acceptable.
This includes constant (`let`) types, variable (`var`) types,
object property types, collection element types,
function parameter types, and function return types,

Unlike in method implementations, methods described in protocols
cannot specify default parameter values.

Examples of commonly used built-in protocols include `Animatable`,
`Collection`, `Comparable`, `CustomStringConvertible`, `Equatable`, `Hashable`,
`Identifiable`, `Numeric`, `ObservedObject`, `Sequence`, and `View`.
Other built-in protocols that may be encountered include
`App`, `Scene`, `Shape`, and `ViewModifier`.

Swift can provide a "synthesized implementation" of the
`Comparable`, `Equatable`, and `Hashable` built-in protocols
for many custom types.
All that is required is to state that a type conforms to the protocol
and only define properties with types that also conform to the protocol.

Protocols can use the type name `Self` to refer to
the actual type that conforms to the protocol.
Swift considers protocols that use `Self` to have a "Self requirement".
Such protocols are incomplete and cannot be used as a type unless
they are preceded by the `some` or `any` keyword (described later).

In a `struct`, `class`, or `enum` definition,
`Self` can also be used to refer to static properties and methods.

Xcode can add stubs for a protocol to a type that claims to conform to it.
To trigger this, click the red circle to the left of the error message
"Type does not conform to protocol" and then click the "Fix" button.

The following code demonstrates defining and using a custom protocol.

```swift
protocol Shape {
    var area: Double { get } // a computed property

    mutating func scale(_ factor: Double)
}

// Structs can conform to protocols,
// but cannot inherit from other structs or classes.
struct Triangle: Shape {
    var base: Double
    var height: Double

    init(base: Double, height: Double) {
        self.base = base
        self.height = height
    }

    var area: Double { base * height * 0.5 }

    mutating func scale(_ factor: Double) {
        base *= factor
        height *= factor
    }
}

// Classes can inherit from another class and
// they can conform to any number of protocols.
class Rectangle: Shape {
    var height: Double
    var width: Double

    init(width: Double, height: Double) {
        self.width = width
        self.height = height
    }

    var area: Double { width * height }

    func scale(_ factor: Double) {
        height *= factor
        width *= factor
    }
}

// Any object that conforms to the Shape protocol
// can be passed to this function.
// Calling getArea on a Shape demonstrates polymorphism because
// what the call does is determined by the receiver type.
func logShape(_ shape: Shape) {
    print("area = \(shape.area)")
}

var t = Triangle(base: 3, height: 4)
let r = Rectangle(width: 4, height: 5)
logShape(t) // area = 6.0
logShape(r) // area = 20.0

let shapes: [Shape] = [t, r]
let totalArea = shapes.reduce(0) { acc, shape in acc + shape.area }
print("\(totalArea)") // 26.0

t.scale(1.5)
print("scaled area = \(t.area)")
```

The following contrived example demonstrates many of the features of protocols.

```swift
protocol Demoable {
    static var typeSetOptional: Int { get } // read-only
    static var typeSetRequired: Int { get set } // read/write

    static func typeMethod(a1 p1: Int, a2 p2: Int) -> Int

    var instanceSetOptional: Int { get } // read-only
    var instanceSetRequired: Int { get set } // read/write

    init(a1 p1: Int, a2 p2: Int)

    func instanceMethod(a1 p1: Int, a2 p2: Int) -> Int
    mutating func instanceMutatingMethod(a1 p1: Int, a2 p2: Int) -> Int
}

struct Demo: Demoable {
    // This property can be a constant since "set" is optional.
    static let typeSetOptional: Int = 1

    // This property cannot be a constant since "set" is required.
    static var typeSetRequired: Int = 2

    static func typeMethod(a1 p1: Int, a2 p2: Int) -> Int {
        return p1 + p2
    }

    // This property can be a constant since "set" is optional.
    let instanceSetOptional: Int = 3

    // This property cannot be a constant since "set" is required.
    var instanceSetRequired: Int

    init(a1 p1: Int, a2 p2: Int) {
        // Don't need to initialize instanceSetOptional
        // because it is a constant.
        instanceSetRequired = p1 + p2
    }

    // This method cannot mutate properties of this object.
    func instanceMethod(a1 p1: Int, a2 p2: Int) -> Int {
        // The next line is not allowed because
        // this is not a "mutating" method.
        //instanceSetRequired = p1 + p2
        return instanceSetOptional + instanceSetRequired + p1 + p2
    }

    // This method can mutate properties of this object.
    mutating func instanceMutatingMethod(a1 p1: Int, a2 p2: Int) -> Int {
        instanceSetRequired = instanceSetOptional + p1 + p2
        return instanceSetRequired
    }
}

var demo = Demo(a1: 7, a2: 8)
print(Demo.typeSetOptional) // 1
print(Demo.typeSetRequired) // 2
print(demo.instanceSetOptional) // 3
print(demo.instanceSetRequired) // 7 + 8 = 15
print(demo.instanceMethod(a1: 1, a2: 2)) // 3 + 15 + 1 + 2 = 21
print(demo.instanceMutatingMethod(a1: 1, a2: 2)) // 3 + 1 + 2 = 6
print(demo.instanceSetRequired) // 6
```

In summary, when a type conforms to a protocol it can mean two things:

1. The type may be required to implement some things.
2. The type may be given implementations of some things through extensions
   (described below).

### Protocol Extensions

Default method implementations cannot be defined in a protocol, but
they can be defined in an `extension` of the protocol.
When this is done for a given method, types that
conform to the protocol are not required to implement the method.
However, they can implement the method to
override the implementation defined in the extension.

In the `Sequence` protocol that is conformed to by concrete types like
`Array`, `Dictionary`, `Range`, and `String`,
a protocol extension is used to define methods like `filter`.

We can define a default implementation of the
`Demoable` protocol `instanceMethod` method (see code above)
so types that conform to the protocol are not required to implement it.

```swift
extension Demoable {
    // This property now has a default implementation
    // that is a computed property.
    var instanceSetOptional: Int { instanceSetRequired * 2 }

    // This instance method now has a default implementation.
    func instanceMethod(a1 p1: Int, a2 p2: Int) -> Int {
        return instanceSetOptional + instanceSetRequired + p1 + p2
    }
}
```

### where Clauses

The type of a function parameter can be specified to be anything
that conforms to a list of protocols using the `where` keyword
with the `&` operator between each protocol name.
For example, see the `doThis` function below.

```swift
protocol HasSize {
    var size: Int { get }
}

struct Drink: CustomStringConvertible, HasSize {
    var description: String { "Drink has size \(size)"}
    var size: Int;
}

// T can be any type that conforms to the
// CustomStringConvertible and HasSize protocols.
func doThis<T>(a p: T) where T: CustomStringConvertible & HasSize {
    print(p)
}
doThis(a: Drink(size: 1)) // Drink has size 1

// Alternate way to implement the code above is to use a typealias.
typealias MyType = CustomStringConvertible & HasSize
func doThat(a p: MyType) {
    print(p)
}
doThat(a: Drink(size: 2)) // Drink has size 2
```

### Protocol Associated Types

Many programming languages support "abstract types" where some of
their methods are only described by a signature with no implementation.
Abstract types cannot be instantiated.
Other types inherit from abstract types and these
these define implementations for the abstract methods.

A Swift `struct` or `class` cannot omit the implementation of its
computed properties and methods, and so cannot be abstract.
However, protocols can play the role of abstract types.

Swift protocols do not support generic parameters using the
angle bracket syntax that is supported for structs and classes.
Instead, "associated types" take the place of generics in protocols.
These are defined in the beginning of a protocol body
using the `associatedtype` keyword followed by a type parameter name.
These type parameter names can be used within the protocol body
to describe computed property types, method parameter types,
and method return types.
In summary, a protocol is generic if it has any associated types.

Using the `associatedtype` keyword in protocols instead of
the generic syntax used by structs and classes
was a syntax choice made by the Swift team.
They could have chosen to use the same generic syntax.

Associated types are often used in protocols that describe kinds of collections.
The Swift Standard Library collection hierarchy is described in the
[Collection Types](#collection-types)
section above.
It defines many collections types such as `Array`, `Dictionary`, and `Set`.
Each of these conform to the `Collection` protocol
which has the following associated types:

- `Element`: type of the elements within the collection
- `Index`: type that describes the position of an element
- `Indices`: type used to find an element
- `Iterator`: type used to iterate over the elements
- `SubSequence`: type that represents a subsequence of elements

In the `Array` struct, `Element` is a generic parameter type
and the remaining four associated types are defined as follows:

```swift
public typealias Index = Int
public typealias Indices = Range<Int>
public typealias Iterator = IndexingIterator<[Element]>
public typealias SubSequence = ArraySlice<Element>
```

The following code demonstrates defining a protocol
for tree nodes that can hold values of any type.

```swift
protocol TreeNode {
    associatedtype Item

    var value: Item { get set }
    var left: Self? { get set }
    var right: Self? { get set }

    func printDepthFirst(level: Int)
}

extension TreeNode {
    func printDepthFirst(level: Int = 0) {
        let indent = String(repeating: " ", count: level * 2)
        print("\(indent)\(value)")
        if let left { left.printDepthFirst(level: level + 1) }
        if let right { right.printDepthFirst(level: level + 1) }
    }
}

// Must be "final" in order to use own type for properties.
final class ITreeNode: TreeNode {
    // The type of "Item" in TreeNode is determine
    // using type inference here.
    var value: Int
    var left: ITreeNode?
    var right: ITreeNode?

    init(
        _ value: Item,
        left: ITreeNode? = nil,
        right: ITreeNode? = nil
    ) {
        self.value = value
        self.left = left
        self.right = right
    }
}

var node1 = ITreeNode(1)
var node2 = ITreeNode(2, left: ITreeNode(3), right: ITreeNode(4))
var node3 = ITreeNode(5, left: nil, right: ITreeNode(6))
node1.left = node2
node1.right = node3
node1.printDepthFirst()
// 1
//   2
//     3
//     4
//   5
//     6
```

## KeyPaths

KeyPaths provide a path to a particular property in objects.
These can be used directly to retrieve the value of a property
from a specific object or they can be passed to a function.
For example, KeyPaths can be passed to the `Array` `map` and `filter` methods
in place of a closure.

The following code demonstrates several uses of KeyPaths.

```swift
struct Address {
    let street: String
    let city: String
    let state: String
    let zip: String
}

struct Person {
    let name: String
    let address: Address
}

let p1 = Person(
    name: "Mark Volkmann",
    address: Address(
        street: "123 Some Street",
        city: "Somewhere",
        state: "MO",
        zip: "12345"
    )
)

let cityPath = \Person.address.city
let city = p1[keyPath: cityPath] // "Somewhere"

let p2 = Person(
    name: "Big Bird",
    address: Address(
        street: "123 Sesame Street",
        city: "Manhattan",
        state: "NY",
        zip: "10001"
    )
)

let people = [p1, p2]
let names = people.map(\.name) // ["Mark Volkmann", "Big Bird"]
let zips = people.map(\.address.zip) // ["12345", "10001"]

// This enables KeyPath equality tests.
func == <T, V: Equatable>(lhs: KeyPath<T, V>, rhs: V) -> (T) -> Bool {
    return { $0[keyPath: lhs] == rhs }
}

// This uses a KeyPath equality test.
let nyPeople = people.filter(\.address.state == "NY")
// This gives an Array that only contains p2 ("Big Bird").
```

## Reflection

Swift provides a way to get the type of a value at runtime
using the global `type` function.

For example:

```swift
let value = 19
let myType = type(of: value) // Int.Type
let myTypeName = String(describing: myType) // "Int"

struct Person {
    let name: String
}

let p = Person(name: "Mark")
let pType = type(of: p) // Person.Type
let pTypeName = String(describing: pType) // "Person"
```

Swift also provides the {% aTargetBlank
"https://developer.apple.com/documentation/swift/mirror", "Mirror" %} API
for discovering the stored properties of an object at runtime.
It does not support discovering the
computed properties and methods of an object.

```swift
print(String(reflecting: p))
// __lldb_expr_17.Person(name: "Mark", hungry: true)

// This iterates over the stored properties of the object,
// not computed properties or methods.
// Stored properties discovered through reflection cannot be modified.
let mirror = Mirror(reflecting: p)
for child in mirror.children {
    print("name:", child.label, "value:", child.value)
}
// name: Optional("name") value: Mark
// name: Optional("hungry") value: true
// We don't get output for "upName" because it is a computed property.
```

## Opaque and Existential Types

Opaque types are defined by applying the `some` keyword to a protocol name.
Existential types are defined by applying the `any` keyword to a protocol name.
These keywords must only precede protocol names
that have one or more associated types.
Swift considers protocols that have associated types
to have "associated type requirements".
Such protocols are incomplete and cannot be used as a type unless
they are preceded by the `some` or `any` keyword.

|                        | `some`                                           | `any`                                                       |
| ---------------------- | ------------------------------------------------ | ----------------------------------------------------------- |
| variable type          | not useful; must initialize and can never change | useful because value can be changed to any conforming value |
| stored property type   | not useful; cannot infer type                    | useful because any conforming value can be used             |
| computed property type | every access returns the same concrete type      | every access can return a different concrete type           |
| parameter type         | can pass any conforming type                     | can also pass any conforming type                           |
| function return type   | every call returns the same concrete type        | every call can return a different concrete type             |

Regardless of whether `some` of `any` is applied to a function return type,
callers are not aware of the specific concrete type that is returned.

It seems the `some` keyword is only useful for
computed property types and function return types
where they choose the concrete type of a given protocol
that they wish to return.
In all other cases where protocol types are used,
the `any` keyword should be used.

In many apps the `any` keyword is not needed and the `some` keyword
is only used in context of SwiftUI custom views because:

- Many apps do not require writing and using custom protocols.
- Many custom protocols do not have associated types.

### Opaque Types (some keyword)

The `some` keyword precedes the name of a protocol
and is used to define an "opaque type"
whose actual type will be known at compile-time.
The syntax `some MyProtocol` means
"some specific type that conforms to `MyProtocol`".
It is most useful in function return types.

In SwiftUI every struct that conforms to the `View` protocol
must define a computed property named `body` whose type is `some View`.
This means that the value returned will be
of some specific type that conforms to the `View` protocol.
It must be possible to determine at compile-time
the actual type that will be returned.
The code cannot, for example, use conditional logic to
sometimes return a `Button` and other times return a `Picker`.
All code paths in the function must return the same concrete type.

Here is the definition of the `View` protocol.

```swift
public protocol View {
    associatedtype Body: View
    var body: Body { get }
}
```

Generic types and opaque types are opposites.
In a function that returns a generic type, the caller chooses the concrete type.
When an opaque type is returned, the function chooses the concrete type.

Opaque types are typically used for
function return types and computed property types.
This avoids exposing the concrete type to callers which.
avoids exposing unnecessary details.
It allows the function or computed property to be modified in the future
to return a different concrete type without requiring
a change to the function signature or caller code.

For example:

```swift
// This function returns an opaque type.
// Callers know that some specific kind of `View` will be returned,
// but not which one.
func getView() -> some View {
    Text("I choose Text!")
}
```

Callers of functions that return an opaque type should
only depend on the returned value conforming to a protocol.
However, they can conditionally cast the returned value
to a specific concrete type using the following syntax:

```swift
if let specificValue = returnedValue as? MyConcreteType {
    ...
}
```

Below are three versions of the same function,
two of which compile and one that does not.

```swift
// This compiles because the function
// does return an instance of the `Text` struct.
private func getText(_ text: String) -> Text {
    Text(text)
}

// This does not compile because the `border` view modifier
// does not return an instance of the `Text` struct.
private func getText(_ text: String) -> Text {
    Text(text).border(.red)
}

// This compiles because the `border` view modifier
// does return some kind of `View`.
private func getText(_ text: String) -> some View {
    Text(text).border(.red)
}
```

Using opaque types (`some`) for variable types is not useful
because it requires the variable to be
initialized to a value with a specific concrete type
and does not allow a new value to be assigned later.
Using existential types (`any`) for variable types
does not have these restrictions.
The following example demonstrates this using
the `Loggable` protocol that was defined earlier.

```swift
var obj1: some Loggable = MyStruct(value: 1)

// The next line gives the error
// "Cannot assign value of type 'MyStruct' to type 'some Loggable'".
// obj1 = MyStruct(value: 2)

// The next line gives the error
// "Cannot assign value of type 'MyClass' to type 'some Loggable'".
// obj1 = MyClass(value: 3)

var obj2: any Loggable = MyStruct(value: 1)
obj2.log() // MyStruct 1
obj2 = MyStruct(value: 2) // works!
obj2.log() // MyStruct 2
obj2 = MyClass(value: 3) // works!
obj2.log() // MyClass 3
```

### Existential Types (any keyword)

The `any` keyword is placed before a protocol name.
In Swift 6+ this will be required when protocols are used for
the type of a variable, property, or function parameter.
In Swift 5.7 adding the `any` keyword before an protocol name
is optional, but omitting it generates a warning.

The following example demonstrates a function that accepts
a parameter of any concrete type that conforms to a given protocol.

```swift
// The Int and String types both conform to the CustomStringConvertible
// protocol which requires having a `description` property.
private func stringLength(_ value: any CustomStringConvertible) -> Int {
    return value.description.count
}

print("\(stringLength("test"))") // 4
print("\(stringLength(123))") // 3
```

When the `any` keyword is applied to a function return type,
it signals opting-in to runtime determination of the concrete type
which generates more code and is slower than compile-time determination.
For example:

```swift
// When a function return type uses the `any` keyword,
// the return value of each call can have a different type.
// This prevents knowing the concrete type at compile-time
// and is slower.
// The Int and String types both conform to the Comparable protocol
// which requires implementing the operators < and == as static methods.
private func greatOne(_ condition: Bool) -> any Comparable {
    if condition {
        return 99 // Int
    } else {
        return "Gretzky" // String
    }
}

// When the return type uses the `some` keyword,
// all return values must have the same type.
// This allows knowing the concrete type at compile-time
// and is faster.
private func greatTwo(_ condition: Bool) -> some Comparable {
    if condition {
        return "99" // String
    } else {
        return "Gretzky" // String
    }
}

print("\(greatOne(true))") // 99
print("\(greatOne(false))") // Gretzky
print("\(greatTwo(true))") // 99
print("\(greatTwo(false))") // Gretzky
```

Two values of an existential type cannot be compared using the `==` operator.
For example, the following will not compile:

```swift
let same = greatOne(true) == greatOne(false)
```

The following function definitions are equivalent:

```swift
// In this form the concrete type of `param` is determined at compile-time.
// This results in more efficient code because it can use "static dispatch"
// to call methods on `param` inside the function.
func demo<T: MyProtocol>(param: T) { ... }

// This form has the same characteristics as the previous form.
func demo<T>(param: T) where T: MyProtocol { ... }

// In this form the concrete type of `param` must be determined at run-time.
// This results in less efficient code because
// the call cannot be inlined and it must use "dynamic dispatch"
// to call methods on `param` inside the function.
// The `any` keyword signals opting into worse performance.
func demo(param: any MyProtocol) { ... }
```

The following code demonstrates these three forms:

```swift
protocol Loggable {
    // To constrain the type to one that conforms to a list of protocols,
    // add a colon and a comma-separated list of protocols after the type name.
    associatedtype Value

    // Protocols can define computed properties, but not stored properties,
    // so we must use `var` instead of `let` on the next line.
    var value: Value { get }

    func log()
}

struct MyStruct: Loggable {
    let value: Int

    func log() {
        print("MyStruct \(value)")
    }
}

class MyClass: Loggable {
    let value: Int

    init(value: Int) {
        self.value = value
    }

    func log() {
        print("MyClass \(value)")
    }
}

func logIt1<T: Loggable>(_ value: T) {
    value.log()
}

func logIt2<T>(_ value: T) where T: Loggable {
    value.log()
}

// The keyword `any` is required here because
// the `Loggable` protocol has associated types.
func logIt3(_ value: any Loggable) {
    value.log()
}

// Using "some" in place of "any" for a parameter type
// seems to make no difference.
func logIt4(_ value: some Loggable) {
    value.log()
}

logIt1(MyStruct(value: 1)) // MyStruct 1
logIt2(MyStruct(value: 2)) // MyStruct 2
logIt3(MyStruct(value: 3)) // MyStruct 3
logIt4(MyStruct(value: 4)) // MyStruct 4

logIt1(MyClass(value: 1)) // MyClass 1
logIt2(MyClass(value: 2)) // MyClass 2
logIt3(MyClass(value: 3)) // MyClass 3
logIt4(MyClass(value: 4)) // MyClass 4
```

There are cases where a generic return type is preferred
over an existential return type. For example:

```swift
// This does not compile because we cannot compare
// any Comparable to any other Comparable.
// They must have the same concrete type.
/*
func biggest(
    _ first: any Comparable,
    _ second: any Comparable
) -> any Comparable {
    return first >= second ? first : second
}
*/

// Using a generic type enables enforcing that
// `first` and `second` have the same concrete type.
// This function takes and returns a generic type.
func biggest<T: Comparable>(_ first: T, _ second: T) -> T {
    return first >= second ? first : second
}
print(biggest(3, 4)) // T type will be Int; prints 4
print(biggest("foo", "bar")) // T type will be String; prints foo
```

## Implementing Operators

Built-in operators can be implemented for custom types.
For example, the following code defines a way to add two colors.

```swift
struct Color {
    var red = 0
    var green = 0
    var blue = 0

    static func +(c1: Color, c2: Color) -> Color {
        let r = (c1.red + c2.red) % 256
        let g = (c1.green + c2.green) % 256
        let b = (c1.blue + c2.blue) % 256
        return Color(red: r, green: g, blue: b)
    }
}

let c1 = Color(red: 255, green: 0, blue: 0)
let c2 = Color(red: 0, green: 0, blue: 255)
let c3 = c1 + c2
print(c3)
```

A similar approach can be used to implement an operator
in an existing type using an `extension`.

## Access Control

Swift supports many keywords for controlling access to values like
functions, types, and the properties and methods of types.
These keywords appear at the beginning of declarations.

The access control keywords include:

- `open`: access from anywhere; only used for classes and class members
- `public`: same as `open` except cannot be used in subclasses or overridden
- `internal`: access from any source in the same module (default level)
- `fileprivate`: access only from code in the same source file
- `private`: access only within enclosing type
  (`enum`, `struct`, `class`, or `actor`)

The most commonly used access control keyword is `private`.
The second most commonly used is `internal`, which is the default.

Specifying `private(set)` on a property means that
the property can be accessed as if it were `public`,
but can only be modified as if it were `private`.

## Concurrency

See my newer blog page
{% aTargetBlank "/blog/topics/#/blog/swift/Concurrency", "Concurrency" %}.

See {% aTargetBlank
"https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html",
"Swift Concurrency" %}.

Swift 5.5 added support for the `async` and `await` keywords.
This was announced at the WWDC 2021 event.
It offers an alternative to
passing escaping closures (a.k.a. completion handlers)
or using the Combine framework.
Using async/await allows the compiler to provide more and better error messages
than with other approaches.

Functions that use the `await` keyword must include the `async` keyword
after their parameter list.

For example:

```swift
func getPets() async -> Pets? {
  do {
    let dogs = await getDogs()
    let cats = await getCats()
    return Pets(dogs: dogs, cats: cats)
  } catch {
    print("error: \(error)")
    return nil
  }
}
```

Async functions must be called from an asynchronous context.
This can be another `async` function.
Another option is to embed the calls in a `Task`.
For example:

```swift
Task {
    do {
        let photo = try await getPhoto("some-name")
        // Do something with photo.
    } catch {
        print("error getting photo: \(error.localizedDescription)")
    }
}
```

The code in a `Task` runs asynchronously.
For example, when the following function is called
the output will be 1, 3, 2.

```
func taskDemo() {
    print("1")
    Task {
        print("2")
    }
    print("3")
}
```

To bridge between functions that take callback functions,
referred to as "completion handlers", use the {% aTargetBlank
"https://developer.apple.com/documentation/swift/withcheckedcontinuation(function:_:)",
"withCheckedContinuation" %} or the {% aTargetBlank
"https://developer.apple.com/documentation/swift/withcheckedthrowingcontinuation(function:_:)",
"withCheckedThrowingContinuation" %} function.
See this post from {% aTargetBlank
"https://www.hackingwithswift.com/quick-start/concurrency/how-to-use-continuations-to-convert-completion-handlers-into-async-functions",
"Hacking with Swift" %}.

Suppose `getData` is a function that takes a completion handler.
To call this function inside an `async` function
so callers can use the `await` keyword:

```swift
func getDataAsync() async -> SomeData {
  return try await withCheckedThrowingContinuation { continuation in
    getData(completion: { result, error in
      if let error {
        continuation.resume(throwing: error)
      } else {
        continuation.resume(returning: result)
      }
    })
  }
}
```

## File I/O

One way to write and read files is to use the {% aTargetBlank
"https://developer.apple.com/documentation/foundation/filemanager",
"FileManager" %} class.
For example:

```swift
import Foundation

let dirUrl = FileManager.default.homeDirectoryForCurrentUser
// file:///Users/mark/

// For iOS, use this instead:
// let dirUrl = FileManager.default.urls(
//     for: .documentDirectory, in: .userDomainMask).first!

var filePath = dirUrl.appendingPathComponent("demo.txt")
// file:///Users/mark/demo.txt

// Write to the file.
let text = "Hello, World!"
do {
    // Setting atomically to true means that it will
    // write to an auxillary file first and then
    // rename that file to the target file to guarantee that the file
    // won't be partially written if the app crashes while writing.
    try text.write(to: filePath, atomically: true, encoding: .utf8)
    print("wrote file")
} catch {
    print(error.localizedDescription)
}

// Read from the file.
do {
    let data = try Data(contentsOf: filePath)
    if let text = String(data: data, encoding: .utf8) {
        print(text) // Hello, World!
    } else {
        print("failed to read from file");
    }
} catch {
    print(error.localizedDescription)
}
```

Another way to read files is to get them from the `Bundle`.
For example:

```swift
let filePath = Bundle.module.path(
    forResource: "accounting",
    ofType: "csv"
)
if let filePath {
    let contents = try String(contentsOfFile: filePath)
    // Use contents here.
}
```

## JSON

Serializing Swift types to JSON and deserializing JSON back to Swift types
is supported by the `JSONEncoder` and `JSONDecoder` classes.
These tasks are a bit more difficult in Swift than in JavaScript.

Swift types must conform to the `Encodable` and `Decodable` protocols
in order be converted to and from JSON.
`Codable` is a typealias to `Decodable & Encodable`,
so custom types typically just state that they conform to it.

```swift
import Foundation

let encoder = JSONEncoder()
encoder.outputFormatting = .prettyPrinted // optional setting
let decoder = JSONDecoder()

let arr = [1, 3, 7]
// Encode the array as JSON.
var encoded = try encoder.encode(arr)
// This String initializer returns an optional.
if let json = String(data: encoded, encoding: .utf8) {
    print("array JSON =", json)
} else {
    print("failed to encode JSON")
}

struct Demo: Codable {
    var b: Bool
    var i: Int
    var f: Float
    var d: Double
    var s: String
    // The Character type does not conform to Encodable or Decodable
    // so it cannot easily be converted to and from JSON.
}

let demo = Demo(b: true, i: 3, f: 3.14, d: 3.14159, s: "test")
// Encode the object as JSON.
encoded = try encoder.encode(demo)
if let json = String(data: encoded, encoding: .utf8) {
    print("object JSON =", json)

    // Prepare to decode the JSON.
    // The data method returns an optional.
    if let data = json.data(using: .utf8) {
        // Decode the JSON into a Demo objet.
        let demo2: Demo? = try? decoder.decode(Demo.self, from: data)
        if (demo2 != nil) { print("Demo object =", demo2!) }
    }
} else {
    print("failed to encode JSON")
}
```

To read JSON from a file in a project into a variable whose type
is described by `SomeType` (typically a struct),
use the `Bundle` class as follows:

```swift
// VERIFY THIS!
let json = Bundle.main.decode(SomeType.self, from: "some-file.json")
```

To convert JSON data that uses snake-case in keys to camel-case names,

```swift
decoder.keyDecodingStrategy = .convertFromSnakeCase
```

To convert ISO 8601 date strings to Swift dates:

```swift
decoder.dateDecodingStrategy = .iso8601
```

If any properties of a type to be decoded have a data type that varies,
decode must be performed manually.
For example, the {% aTargetBlank "https://randomuser.me/api/", "RandomUser" %}
API endpoint sometimes returns a `postcode` property as a `String`
and sometimes as an `Int`.
Here are all the required struct definitions
followed by an API call and decoding the result.
In particular, see the `Location` struct.
Properties that are not needed can be omitted from these structs
even though they may be present in the JSON.

```swift
struct Address: Decodable {
    let title: String
    let first: String
    let last: String
}

struct Coordinates: Decodable {
    let latitude: String
    let longitude: String
}

struct DateOfBirth: Decodable {
    let date: String
    let age: Int
}

struct ID: Decodable {
    let name: String
    let value: String
}

struct Info: Decodable {
    let seed: String
    let results: Int
    let page: Int
    let version: String
}

struct Location: Decodable {
    let street: Street
    let city: String
    let state: String
    let country: String
    let postcode: String
    let coordinates: Coordinates
    let timezone: Timezone

    // We need to decode this struct manually because
    // postcode can be a String or Int.
    // This requires defining the following enum.
    private enum CodingKeys: String, CodingKey {
        case street, city, state, country, postcode, coordinates, timezone
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        street = try container.decode(Street.self, forKey: .street)
        city = try container.decode(String.self, forKey: .city)
        state = try container.decode(String.self, forKey: .state)
        country = try container.decode(String.self, forKey: .country)
        do {
            // First try decoding postcode as a String.
            postcode = try container.decode(String.self, forKey: .postcode)
        } catch {
            // If it wasn't a String, try decoding postcode as an Int.
            postcode = try String(container.decode(Int.self, forKey: .postcode))
        }
        coordinates = try container.decode(
            Coordinates.self,
            forKey: .coordinates
        )
        timezone = try container.decode(Timezone.self, forKey: .timezone)
    }
}

struct Login: Decodable {
    let uuid: String
    let username: String
    let password: String
    let salt: String
    let md5: String
    let sha1: String
    let sha256: String
}

struct Name: Decodable {
    let title: String
    let first: String
    let last: String
}

struct Picture: Decodable {
    let large: String
    let medium: String
    let thumbnail: String
}

struct Registered: Decodable {
    let date: String
    let age: Int
}

struct Street: Decodable {
    let number: Int
    let name: String
}

struct Timezone: Decodable {
    let offset: String
    let description: String
}

struct User: Decodable {
    let gender: String
    let name: Name
    let location: Location
    let email: String
    let login: Login
    let dob: DateOfBirth
    let registered: Registered
    let phone: String
    let cell: String
    let id: ID
    let picture: Picture
    let nat: String
}

struct Users: Decodable {
    let results: [User]
    let info: Info
}

    let usersURL = URL(string: "https://randomuser.me/api/")!

    private func fetchUsers() async throws {
        let (data, response) =
            try await URLSession.shared.data(from: usersURL)
        guard let res = response as? HTTPURLResponse else {
            throw MyError.badResponseType
        }
        guard res.statusCode == 200 else {
            throw MyError.badStatus(status: res.statusCode)
        }
        users = try JSONDecoder().decode(Users.self, from: data)
    }
```

One option for an app to persist data is to write it to a local JSON file.
This may be appropriate for relatively small amounts of data
where rewriting all the data after every change has acceptable performance.
When this is not the case, consider using Core Data, CloudKit, or Realm.

For an example of an app that writes JSON data to a file
and restores the data from that file every time the app is launched,
see {% aTargetBlank
"https://github.com/mvolkmann/JSONFileIODemo/tree/main/JSONFileIODemo",
"JSONFileIODemo" %}.
This is inspired by the Stewart Lynch video {% aTargetBlank
"https://www.youtube.com/watch?v=8D8pb3ycdqw&list=RDCMUCOWdR4sFkmolWkU2fg669Gg&start_radio=1",
"Persisting to JSON or MarkDown in SwiftUI" %}.

## CSV

Comma Separated Value (CSV) files are text files where each line contains values separated by commas.
This is a common format for exporting spreadsheets.
While there are Swift packages for parsing CSV data such as {% aTargetBlank
"https://github.com/swiftcsv/SwiftCSV", "SwiftCSV" %},
writing the code yourself (minus certain features and error handling)
is relatively easy.

The following code provides functions for reading CSV data:

```swift
import Foundation

extension String: LocalizedError {
    // Allows String values to be thrown.
    public var errorDescription: String? { self }
}

/// Parses a CSV string into an Array of Arrays of Strings.
/// - Parameter csv: a String of CSV data containing newline characters and commas
/// - Returns: an Array of Arrays of Strings
private func parseCSV(csv: String) -> [[String]] {
    csv.split(separator: "\n").map { line in
        line.split(separator: ",").map { String($0) }
    }
}

/// Parses a CSV file into an Array of Arrays of Strings.
/// - Parameter fileName: a file name that can be found in the app bundle such as "data.csv"
/// - Throws: if the provided file name does not contain a single period or the file cannot be found
/// - Returns: an Array of Arrays of Strings
public func parseCSV(fileName: String) throws -> [[String]] {
    let parts = fileName.split(separator: ".")
    guard parts.count == 2 else { throw "invalid fileName" }
    // When in a Swift package instead of an app,
    // use `Bundle.module` instead of `Bundle.main`.
    let filePath = Bundle.main.path(
        forResource: String(parts[0]),
        ofType: String(parts[1])
    )
    guard let filePath else { throw "file not found" }
    let contents = try String(contentsOfFile: filePath)
    return parseCSV(csv: contents)
}
```

To use this:

```swift
let csv = try parseCSV(fileName: "data.csv")
```

## Threads

Swift applications can run tasks (blocks of code) in multiple threads.
While it is possible explicitly create new threads, typically we
place work in various queues and let the operating system
pull work from the queues and assign it to threads.

Queues can be serial or concurrent.
Serial queues execute one assigned block of code at a time.
Concurrent queues can execute multiple assigned blocks of code at a time.

Swift provides several queues and new ones can be created.
TODO: What are the names of the provided queues?

All code that updates UI state and renders the UI should run on the main thread.

To view the thread usage of an app running in Xcode,
click the spray can icon at the top of the Navigator.
This displays the "Debug Navigator".
Click the "CPU" tab to see the CPU usage on each of the active threads.

To run code on a new thread:

```swift
// qos stands for Quality Of Service.
DispatchQueue.global(qos: .qosName).async { some-code }
// where qosName is one of
// background - for maintenance or cleanup tasks
// default - default qos used when none is specified
// unspecified - for no specific purpose
// userInitiated - for tasks that prevent active use of app
// userInteractive - for animations, event handling, and UI updates
// utility - for tasks the user does not actively track
```

When code running off of the main thread needs to update UI state,
it should register a function to run on the main thread as follows:

```swift
await MainActor.run { some-code }
```

The code above must be run in an asynchronous context
such as an `async` function or a `Task`.

To run code on the main thread from a synchronous context:

```swift
Dispatch.main.async { some-code }
```

`Thread.isMainThread` is a `Bool` property that indicates whether
the code from which it is referenced is running on the main thread.

`Thread.current` holds a reference to the current thread object.
These have `name` (ex. "main") and `number` (ex. 1) properties.

To run code on the main thread after some number of seconds (ex. 2):

```swift
DispatchQueue.main.asyncAfter(deadline: .now() + 2) { some-code }
```

To run code on a given thread after some number of seconds:

```swift
DispatchQueue.global(qos: .qosName).asyncAfter(deadline: .now() + 2) {
    some-code
}
```

TODO: Add more detail?

## AsyncStream

{% aTargetBlank "https://developer.apple.com/documentation/swift/asyncstream",
"AsyncStream" %} is a provided struct that supports asynchronous sequences
that are generated by a closure that produces new elements.

TODO: Add lots of examples!

## AsyncAlgorithms

{% aTargetBlank "https://github.com/apple/swift-async-algorithms",
"AsyncAlgorithms" %} is a Swift package from Apple that implements
many algorithms for working with asynchronous sequences.

TODO: Add lots of examples!

See the SwiftLeeds talk {% aTargetBlank
"https://www.youtube.com/watch?v=ObYLV30-dwg",
"SwiftUI, Async/Await, AsyncAlgorithms: How does it all fit?" %}.

## Actors

An {% aTargetBlank "https://developer.apple.com/documentation/swift/actor",
"Actor" %} is a data type that is used to isolate parallel tasks.
Actors are defined similarly to classes,
but use the `actor` keyword instead of the `class` keyword.

TODO: Add much more detail about these!

{% aTargetBlank "", "MainActor" %} is a provided global actor
that executes the associated code in the main queue.

To cause a function or method to run in the main queue,
annotated it with `@MainActor`.

To cause all code in a type such as a `class` or `struct`
annotated it with `@MainActor`.

To run a block of code in the main queue,
place it in a closure that is passed to `MainActor.run`.
For example:

```swift
await MainActor.run {
    // code goes here
}
```

For more on `MainActor` see the {% aTargetBlank
"https://www.hackingwithswift.com/quick-start/concurrency/how-to-use-mainactor-to-run-code-on-the-main-queue",
"Hacking With Swift" %} post.

## HTTP

Sending HTTP requests and processing HTTP responses in Swift is fairly tedious.
Using the new `async` and `await` keywords
and creating some utility functions makes this easier.

The following example demonstrates sending
HTTP DELETE, GET, POST, and PUT requests.

Utility functions for sending HTTP requests below
are defined in the file `HttpUtil.swift`.

```swift
import Foundation

enum HttpError: Error {
    case badStatus(status: Int)
    case badUrl
    case jsonEncode
}

extension HttpError: LocalizedError {
    public var errorDescription: String? {
        switch self {
        case .badStatus(let status):
            return "bad status \(status)"
        case .badUrl:
            return "bad URL"
        case .jsonEncode:
            return "JSON encoding failed"
        }
    }
}

struct HttpUtil {

    static func delete(from url: String, id: Int) async throws {
        guard let url = URL(string: "\(url)/\(id)") else {
            throw HttpError.badUrl
        }

        var request = URLRequest(url: url)
        request.httpMethod = "DELETE"
        let (_, res) = try await URLSession.shared.data(for: request)

        if let res = res as? HTTPURLResponse, res.statusCode != 200 {
            throw HttpError.badStatus(status: res.statusCode)
        }
    }

    static func get<T>(
        from url: String,
        type: T.Type
    ) async throws -> T where T: Decodable {
        guard let url = URL(string: url) else {
            throw HttpError.badUrl
        }

        let (data, res) = try await URLSession.shared.data(from: url)
        if let res = res as? HTTPURLResponse, res.statusCode != 200 {
            throw HttpError.badStatus(status: res.statusCode)
        }
        return try JSONDecoder().decode(type, from: data)
    }

    static func post<T, U>(
        to url: String,
        with data: T,
        type: U.Type
    ) async throws -> U where T: Encodable, U: Decodable {
        return try await httpWithBody(to: url, method: "POST", with: data, type: type)
    }

    static func put<T, U>(
        to url: String,
        with data: T,
        type: U.Type
    ) async throws -> U where T: Encodable, U: Decodable {
        return try await httpWithBody(to: url, method: "PUT", with: data, type: type)
    }

    private static func httpWithBody<T, U>(
        to url: String,
        method: String,
        with data: T,
        type: U.Type
    ) async throws -> U where T: Encodable, U: Decodable {
        guard let url = URL(string: url) else {
            throw HttpError.badUrl
        }

        guard let json = try? JSONEncoder().encode(data) else {
            throw HttpError.jsonEncode
        }

        var request = URLRequest(url: url)
        request.httpMethod = method
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")

        let (data, res) = try await URLSession.shared.upload(for: request, from: json)

        if let res = res as? HTTPURLResponse, res.statusCode != 200 {
            throw HttpError.badStatus(status: res.statusCode)
        }

        return try JSONDecoder().decode(type, from: data)
    }
}
```

A SwiftUI `ViewModel` that uses the utility functions above
is defined in `ViewModel.swift` which follows.
In a real app, most of the code in the `init` method below
would be placed in various views.
One of the views could create an instance of `ViewModel`
with `@StateObject var vm = ViewModel()`
and `vm` could be passed to the other views that need it.

```swift
import Foundation

struct Dog: Codable, CustomStringConvertible {
    let id: Int
    var name: String
    var breed: String
    var description: String { "\(name) is a \(breed)" }
}

// This only differs from Dog in that it doesn't have an id property.
struct NewDog: Codable, CustomStringConvertible {
    var name: String
    var breed: String
    var description: String { "\(name) is a \(breed)" }
}

@MainActor
class ViewModel: ObservableObject {
    @Published var dogs: [Dog] = []

    private static let url = "http://localhost:8001/dog"

    func deleteDog(id: Int) async throws {
        return try await HttpUtil.delete(from: ViewModel.url, id: id)
    }

    func getDog(id: Int) async throws -> Dog {
        let url = "\(ViewModel.url)/\(id)"
        return try await HttpUtil.get(from: url, type: Dog.self)
    }

    func getDogs() async throws -> [Dog] {
        return try await HttpUtil.get(from: ViewModel.url, type: [Dog].self)
    }

    func postDog(_ dog: NewDog) async throws -> Dog {
        return try await HttpUtil.post(
            to: ViewModel.url,
            with: dog,
            type: Dog.self
        )
    }

    func putDog(_ dog: Dog) async throws -> Dog {
        let url = "\(ViewModel.url)/\(dog.id)"
        return try await HttpUtil.put(to: url, with: dog, type: Dog.self)
    }

    init() {
        // Task creates an asynchronous context that can use the await keyword.
        Task(priority: .medium) {
            do {
                // Create a new dog.
                let newDog = try await postDog(
                    NewDog(name: "Clarice", breed: "Whippet")
                )
                print("created dog with id \(newDog.id)")

                // Get the dog with id 1.
                var dog = try await getDog(id: 1)
                print("first dog =", dog)

                // Update an existing dog.
                dog.name = "Moo"
                dog.breed = "Cow"
                _ = try await putDog(dog)

                // Delete an existing dog.
                try await deleteDog(id: 2)

                // Get all the dogs.
                let fetchedDogs = try await getDogs()
                for dog in fetchedDogs {
                    print("\(dog.name) is a \(dog.breed)")
                }

                // Update the published property dogs in the main thread.
                // See the description of `[weak self]` earlier.
                DispatchQueue.main.async { [weak self] in
                    self?.dogs = fetchedDogs
                }
            } catch {
                print("error =", error.localizedDescription)
            }
        }
    }
}
```

It is also possible to implement HTTP servers in Swift.
The most popular package for doing this is
{% aTargetBlank "https://github.com/vapor/vapor", "Vapor" %}.
To install Vapor, first install
{% aTargetBlank "https://brew.sh", "Homebrew" %}
and then enter `brew install vapor`.

To create, build, and run a new Vapor project:

- Enter `vapor new {project-name} -n`.
- Enter `open Package.swift` to open the project in Xcode
  and install the dependencies.
  This takes a couple of minutes to complete.
- Press the play button in the upper-right to build and start the server.
  The first build takes a couple of minutes to complete.

The server listens on port 8080 by default.
To change this, add the following in the `configure` function
inside `Sources/App/Controllers/configure.swift`:

```swift
app.http.server.configuration.port = {new-port-number}
```

The routes are defined in `Sources/App/Controllers/routes.swift`.
The only provided routes are `GET /`which returns "It works!"
and `GET /hello` which returns "Hello, world!".

After modifying the routes, stop the server
by clicking the stop button (square) in the upper-right of Xcode
and restart the server by clicking the play button.

The following example demonstrates using Vapor to implement
a set of REST services for performing CRUD operations on a collection of dogs.
To start the server from a terminal, enter `vapor run`.
This takes a couple of minutes the first time the server is started.

```swift
import Vapor

// Must conform to the Content protocol
// in order to return from a route.
struct Dog: Content {
    let id: Int
    var name: String
    var breed: String
}

struct NewDog: Content {
    let name: String
    let breed: String
}

var dogMap: [Int: Dog] = [:]

var lastId = 0

func addDog(name: String, breed: String) -> Dog {
    lastId += 1
    let dog = Dog(id: lastId, name: name, breed: breed)
    dogMap[lastId] = dog
    return dog
}

func setup() {
    _ = addDog(name: "Maisey", breed: "Treeing Walker Coonhound")
    _ = addDog(name: "Ramsay", breed: "Native American Indian Dog")
    _ = addDog(name: "Oscar", breed: "German Shorthaired Pointer")
    _ = addDog(name: "Comet", breed: "Whippet")
}

func routes(_ app: Application) throws {
    setup()

    // Could refactor to use a controller.

    app.get { _ in "Hello, World!" }

    // The Content-Type header will automatically be set to "application/json".
    app.get("dog") { _ -> [Dog] in
        Array(dogMap.values)
    }

    app.get("dog", ":id") { req -> Dog in
        guard let id = req.parameters.get("id", as: Int.self) else {
            throw Abort(.badRequest, reason: "missing id param")
        }

        if let dog = dogMap[id] {
            return dog
        } else {
            throw Abort(.notFound)
        }
    }

    app.post("dog") { req -> EventLoopFuture<Response> in
        guard let byteBuffer = req.body.data else {
            throw Abort(.badRequest, reason: "invalid or missing body")
        }

        do {
            let newDog = try req.content.decode(NewDog.self)
            let dog = addDog(name: newDog.name, breed: newDog.breed)

            var headers = HTTPHeaders()
            headers.add(name: .location, value: "\(req.url)/\(dog.id)")
            return dog.encodeResponse(
                status: .created, headers: headers, for: req)
        } catch {
            throw Abort(.badRequest, reason: error.localizedDescription)
        }
    }

    app.put("dog", ":id") { req -> Dog in
        guard let id = req.parameters.get("id", as: Int.self) else {
            throw Abort(.badRequest, reason: "missing id param")
        }

        // This creates a copy of the struct.
        guard var dog = dogMap[id] else {
            throw Abort(.notFound, reason: "no dog with id \(id) found")
        }

        guard let byteBuffer = req.body.data else {
            throw Abort(.badRequest, reason: "invalid or missing body")
        }

        do {
            let newDog = try req.content.decode(NewDog.self)

            // Update the copied struct.
            dog.name = newDog.name
            dog.breed = newDog.breed

            // Update the value in dogMap.
            dogMap[id] = dog
            return dog
        } catch {
            throw Abort(.badRequest, reason: "failed to decode body to Dog")
        }
    }

    app.delete("dog", ":id") { req -> HTTPStatus in
        guard let id = req.parameters.get("id", as: Int.self) else {
            throw Abort(.badRequest, reason: "missing id param")
        }

        let dog = dogMap.removeValue(forKey: id)
        return dog == nil ? .notFound : .ok
    }
}
```

## Shell Commands

TODO: Is it possible to execute shell commands from Swift?
See the `Process` class.

## Compiler Directives

The `#if` compiler directive conditionally includes a block of code.
A common use is to include code that is specific to
iOS, macOS, tvOS, or watchOS.

```swift
#if os(macOS)
  // macOS code
#elseif os(iOS)
  // iOS code
#else
  // Other code
#endif
```

To include code only when the app is compiled in debug mode,
surround it with `#if DEBUG ... #endif`.

To include code only when the app is not compiled in debug mode,
surround it with `#if RELEASE ... #endif`.

To include code only when the app is compiled with a specific version of Swift,
surround it with `#if swift(<version) ... #endif`
where `version` is replaced by a number like 5.

To include code only when the app is running in the Simulator,
surround it with `#if targetEnvironment(simulator) ... #endif`.

## Logging

To log the time required to execute a section of code:

```swift
let startTime = CFAbsoluteTimeGetCurrent()
... code to measure goes here ...
let seconds = CFAbsoluteTimeGetCurrent() - startTime
print("seconds to execute =", seconds)
```

Apple provides the `Logger` struct for writing log messages
that appear in the Xcode console and in the Console app.

Here is a custom struct named `Log` that wraps the functionality of `Logger`.
It provides better output, capturing the location of log messages
including their file name, function name, and line number.
Consider copying this code into each project that needs logging.

```swift
import Foundation // for Bundle
import os

// This is necessary to allow OSLogType values to be Dictionary keys.
extension OSLogType: Hashable {}

struct Log {
    // MARK: - Constants

    private static let typeToEmoji: [OSLogType: String] = [
        .debug: "🪲",
        .error: "❌",
        .fault: "☠️",
        .info: "🔎"
    ]

    private static let typeToName: [OSLogType: String] = [
        .debug: "debug",
        .error: "error",
        .fault: "fault",
        .info: "info"
    ]

    private static let logger = Logger(
        subsystem: Bundle.main.bundleIdentifier!,
        category: ""
    )

    // MARK: - Methods

    private static func buildMessage(
        _ type: OSLogType,
        _ message: String,
        _ file: String,
        _ function: String,
        _ line: Int
    ) -> String {
        let fileName = file.components(separatedBy: "/").last ?? "unknown"
        let emoji = typeToEmoji[type] ?? ""
        let name = typeToName[type] ?? ""
        return """
        \(fileName) \(function) line \(line)
        \(emoji) \(name): \(message)
        """
    }

    static func debug(
        _ message: String,
        file: String = #file,
        function: String = #function,
        line: Int = #line
    ) {
        let message = buildMessage(.debug, message, file, function, line)
        log(message: message, type: .debug)
    }

    static func error(
        _ err: Error,
        file: String = #file,
        function: String = #function,
        line: Int = #line
    ) {
        let message = err.localizedDescription
        error(message, file: file, function: function, line: line)
    }

    static func error(
        _ message: String,
        file: String = #file,
        function: String = #function,
        line: Int = #line
    ) {
        let message = buildMessage(.error, message, file, function, line)
        log(message: message, type: .error)
    }

    static func fault(
        _ message: String,
        file: String = #file,
        function: String = #function,
        line: Int = #line
    ) {
        let message = buildMessage(.fault, message, file, function, line)
        log(message: message, type: .fault)
    }

    static func info(
        _ message: String,
        file: String = #file,
        function: String = #function,
        line: Int = #line
    ) {
        let message = buildMessage(.info, message, file, function, line)
        log(message: message, type: .info)
    }

    /*
     This sets "privacy" to "public" to prevent values
     in string interpolations from being redacted.
     From https://developer.apple.com/documentation/os/logger
     "When you include an interpolated string or custom object in your message,
     the system redacts the value of that string or object by default.
     This behavior prevents the system from leaking potentially user-sensitive
     information in the log files, such as the user’s account information.
     If the data doesn’t contain sensitive information, change the
     privacy option of that value when logging the information."
     */
    private static func log(message: String, type: OSLogType) {
        switch type {
        case .debug:
            // The argument in each of the logger calls below
            // MUST be a string interpolation!
            logger.debug("\(message, privacy: .public)")
        case .error:
            logger.error("\(message, privacy: .public)")
        case .fault:
            logger.fault("\(message, privacy: .public)")
        case .info:
            logger.info("\(message, privacy: .public)")
        default:
            logger.log("\(message, privacy: .public)")
        }
    }
}
```

## Tools

### Xcode

See [Xcode](/blog/swift/Xcode/).

### VS Code

For tips on writing Swift code in VS Code, see
{% aTargetBlank "https://nshipster.com/vscode/",
"Swift Development with Visual Studio Code" %}.

The setup steps are:

- install Xcode
- install VS Code
- `git clone https://github.com/apple/sourcekit-lsp.git`
- `cd sourcekit-lsp/Editors/vscode/`
- `npm install`
- `npm run dev-package`
- `code --install-extension out/sourcekit-lsp-vscode-dev.vsix`

### Fastlane

{% aTargetBlank "https://fastlane.tools", "Fastlane" %} is
"the easiest way to build and release mobile apps."

Fastlane provides the ability to:

- distribute beta builds to testing services like TextFlight
- publish to app stores by pushing a button
- automatically code sign iOS apps

TODO: Add more detail. Does this only help after the first manual deployment?

## Apple Developer Program

In order to submit apps to the App Store it is necessary
to join the Apple Developer Program.
This has an annual fee of $99 USD.

Benefits include:

- access to AppStore Connect

  This is used to summit and manage apps on the App Store.

- access to TestFlight

  This enables up to 10,000 users to download and test
  beta versions of your apps.
  In addition, up to 100 users can install your apps
  directly on their devices for testing
  without going through app review or the App Store
  (referred to as "Ad Hoc").

- access to Provisioning Portal

  This is an online tool that takes developers through the steps
  to test apps on iOS devices and prepare them for the App Store.

- access to beta OS releases for all platforms

  This includes iOS, iPadOS, macOS, watchOS, and tvOS.

- access to App Analytics

- support from Apple engineers

  Submit a Technical Support Incident (TSI) to request help.

## Annoyances

In a sense if you want to do web development,
your opinions about JavaScript don't matter.
Web development is primary done with JavaScript (or TypeScript)
and you have to accept that.

In a similar way, if you want to do native iOS or Mac development,
your opinions about Swift don't matter.
Most developers prefer it over Objective-C and there are no other choices.

However, it's still interesting to discuss
the features of Swift that are annoying, at least in my opinion.

- String interpolation syntax

  Swift should have copied the `${expression}` syntax already in use by other
  languages instead of using the `\(expression)` syntax that is unique to Swift.

- Trailing closures

  The syntax for trailing closures is unique to Swift.
  It's nice when used with methods like `Array` `map`.
  However, the syntax looks strange when more than one
  is used in the same function/method call.
  Using more than one shouldn't be supported.

- Too many features in enumerations

  It's not clear that enumerations t need to support initializers,
  computed properties, methods, and recursive enumerations.

- Some names are way too long

  Examples include:

  - `convenience` keyword on `init` methods

    Why is this even necessary?

  - `localizedDescription`

    This is an `Error` property that could have been named `message`.
    It can be given this name with

    ```swift
    extension Error {
        var message: String { localizedDescription }
    }
    ```

  - `CustomStringConvertible`

    This is a protocol that could have been named `ToString`.
    It can be given this name with
    `typealias ToString = CustomStringConvertible`.

- Lack of wait cursors

  When creating a new project, there is a long pause after each step
  and there is no indication that work is happening in the background.
  This is surprising coming from Apple which
  has a reputation for building great UIs.

- Code not re-evaluated

  Sometimes after fixing an error reported by the compiler,
  Xcode doesn't re-evaluate the code and the error remains.
  Resuming Preview triggers it to compile the code again.

- Flaky builds

  Sometimes a project will fail to build with no errors identified in the code.
  Pressing the "Diagnostics" button displays error messages
  that are typically not helpful.
  Sometimes pressing the "Try Again" button results in a clean build.
  Other times it is necessary to close the project, reopen it,
  and build again to resolve the mystery issues.

- Too many warnings about things developers cannot fix

  Common warnings include
  "[LayoutConstraints] Unable to simultaneously satisfy constraints"
  and "Will attempt to recover by breaking constraint",
  followed by long stack traces.
  These make it difficult to find your own `print` output.

- If a view attempts to render more than 10 sub-views,
  an error message is output that doesn't explain that limit
  ... or worse, building the app just never finishes.

## Questions

- Why is a default memberwise initializer is provided for structs,
  but not for classes?

- Why do `struct` methods that modify properties
  have to be marked with the `mutating` keyword,
  but `class` methods that modify properties
  are not required to do do this, and in fact cannot do it?

- The compiler knows whether a struct method modifies properties.
  It reports an error if a non-modifying method is marked as `mutating`.
  It also reports an error if a modifying method is not marked as `mutating`.
  So why are developers required to apply the `mutating` keyword?

- Why do closure parameters that outlive a function call
  need to be annotated with `@escaping`?
  Can't the compiler determine how to manage them?

- Since all initializers are required to initialize all properties,
  either directly or through a call to `super.init`,
  why is the `convenience` keyword needed?

- Why are subclass initializers required to initialize their own properties
  before calling `super.init`?

- Why doesn't Swift assume that any statement in a `do` block can throw?
  It seems tedious to require the `try` keyword
  in front of every expression that can throw.

- Why doesn't the `Character` type conform to the
  `Decodable` and `Encodable` protocols like the other primitive types?
- Why is the type `Int` abbreviated from Integer and
  `Bool` is abbreviated from Boolean,
  but the `Character` type is not abbreviated to `Char`?

- How is a new module defined?
  Can it be implemented by multiple source files?
  Why don't source files indicate what they use from other modules
  in import statements?
  Is the only way to determine where something being used comes from
  to command-click and select "Jump to Definition..."?

- I wish Swift functions supported a shorthand syntax for arguments
  when there is a variable with the same name.
  For example, instead of `area(width: width, height: height)`
  we could write `area(width:, height:)`.

- Why do protocols use a different syntax for type parameters
  that functions, structs, and classes?
  They use the `associatedtype` keyword instead of
  the generic syntax of placing type parameters in angle brackets.

- Can the `set` part of a computed property reject a potential change?

- How can I make all new Xcode projects default to iOS 15?
  Currently they default to iOS 14.

- Adding an editor pane in Xcode displays the currently selected file,
  but it also remains displayed in the original editor pane.
  I want it to only be in the new editor pane.
  It would be even better if a file could be dragged to the right edge
  to create a new editor pane like in VS Code.

- I wish the syntax for the type of computed properties and
  the return type of functions was the same rather than
  using a colon for one and an arrow for the other.
  The difference makes it a bit tedious to refactor a
  computed property into a function or vice versa.
  Maybe Xcode could add refactoring options to do this.
