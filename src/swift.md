---
eleventyNavigation:
  key: Swift
layout: topic-layout.njk
---

## Overview

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

## Installing

Swift can be installed on macOS, Windows 10, Amazon Linux 2, CentOS, and Ubuntu.

To install Swift on macOS, install Xcode from the macOS App Store.
For other operating systems, download it from
{% aTargetBlank "https://swift.org/download/", "Download Swift" %}.

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

| Type        | Description                                                  |
| ----------- | ------------------------------------------------------------ |
| `Bool`      | boolean; literal values are `true` and `false`               |
| `Character` | single character                                             |
| `Double`    | 64-bit floating point number                                 |
| `Float`     | 32-bit floating point number                                 |
| `Int`       | signed integer; same number of bits as platform (32 or 64)   |
| `String`    | text                                                         |
| `UInt`      | unsigned integer; same number of bits as platform (32 or 64) |

Literal `Character` and single-line `String` values
are both delimited by double-quotes.
Multi-line `String` values are delimited by triple double-quotes.

## Built-in Collection Types

| Type         | Description                                                               |
| ------------ | ------------------------------------------------------------------------- |
| `Array`      | indexed collection of values with the same type                           |
| `Dictionary` | collection of key/value pairs                                             |
| `Set`        | unordered collection of values with the same type and no duplicates       |
| tuple        | fixed-length, ordered collection of values that have type that can differ |

To define a tuple type, provide a list of elements types in parentheses.
For example, `(Bool, Int, String)`.

## Protocols

A protocol is like an interface in other programming languages.
It describes a set of method signatures.
TODO: Can it also describe constants and fields?

- `Collection`
- `CustomDebugStringConvertible`
- `CustomReflectable`
- `Equatable`

## Resources

- main site - {% aTargetBlank "https://swift.org", "https://swift.org" %}
