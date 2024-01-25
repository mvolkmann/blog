---
eleventyNavigation:
  key: Project Structure
  parent: Swift
layout: topic-layout.njk
---

## Overview

This post documents my recommended structure and practices for SwiftUI projects.

## Groups

Most files in a project should be placed in a group.
Group names typically begin with an uppercase letter and do not contain spaces.

Recommended group names include:

- Config

  This group includes files like `Assets.xcassets` and `.plist` files

- Extensions

  This group contains files that define extensions to built-in structs
  like `Date`, `String`, and `View`.
  These tend to be copied from project to project as they evolve.
  The names of files in this group should end with "Extension".

- Models

  This group contains files that define model structs, classes, and enums.
  Instances of these hold all the data used by the app.
  The names of files in this group do not need to end with "Model".

- Screens

  This group contains files that implement entire screens of the app
  as opposed to sections of a screen.
  Screens typically use views defined in the Views group.
  The names of files in this group should end with "Screen".

- Services

  This group contains files that abstract away the details of
  network communication and interactions of packages like
  Core Data, CloudKit, and HealthKit.
  The names of files in this group should end with "Service".

- Views

  This group contains files that define views used by
  screens and other views.
  The names of files in this group do not need to end with "View".

- ViewModels

  This group contains files that define classes that
  inherit from `ObservableObject`, define `@Published` properties,
  and define methods that operate on the data they encapsulate.
  Views can use the properties and methods in these classes.
  The names of files in this group should end with "ViewModel".

## Group and file order

Files not in any group, such as the main `*App.swift`
belong above all the groups.
The groups follow these files and should be in alphabetical order.
The files within each group should be in alphabetical order.

## View Models

In strict implementations of the MVVM pattern
views get all their state data from view models
and delegate all their non-view logic to view models.

In less strict implementations of the MVVM pattern,
state data and logic that is specific to a single view
is implemented in the view using `@State` properties.
Only state data and logic that is used by multiple views
is implemented in view model classes.

## Services

Services are `.swift` files that abstract away the details of
network communication and interactions with packages like
Core Data, CloudKit, and HealthKit.
View and view model source files should prefer using methods of services
instead of directly coding such interactions.

## Constants

The Swift naming convention for constants is the same as for variables.
They begin with a lowercase letter and use camel-case.
This differs from most other programming languages that use all uppercase.

There are several ways to define constants in Swift.

One approach is to define static properties.

```swift
private static let defaultName = "Anonymous"
private static let defaultSize = 6.2
```

Another approach is to wrap them in a case-less `enum`.
Using an `enum` instead of a `struct` or `class` prevents
creating instances of the wrapping construct.

```swift
enum MyConstants {
    static let static let defaultName = "Anonymous"
    static let static let defaultSize = 6.2
}
```

Yet another approach is to use a normal `enum`.
This has the advantage that `switch` statements
can enforce exhaustive handling of the cases.
However, it requires all cases to have the same type.

```swift
enum MyConstants {
    case defaultSize = 6.2
    case smallSize = 3.1
    case largeSize = 13.1
}
```

## Order Within Files

A recommended order for groups of items in a source file is:

1. initializers
1. constants
1. state properties (properties with property wrappers like
   `@Environment`, `@EnvironmentObject`, `@Published`, and `@State`)
1. non-state properties (both normal and computed)
1. methods

## Pragma Marks

Files over a certain size in number of lines
should include pragma marks to separate sections.
This adds labels in the Xcode final breadcrumb dropdown
which makes it easier to select and jump to a specific item.

Each pragma mark is a single-line comment following the pattern:

```swift
// MARK: - section-name
```

Including the hyphen adds a divider line in the breadcrumb dropdown.

## Indentation and wrapping style

Use 4-space indentation.

Most lines that are longer than 80 characters should be wrapped to be no longer
than that in order to allow comfortably viewing two or three files side-by-side.

`Array` and `Dictionary` literals that do not fit on a single line
should place each item on a separate line,
indented by four additional spaces. For example:

```swift
// SwiftFormat "--wrapcollections before-first" does this.
let colorNames = [
    "Red",
    "Green",
    "Blue"
]

// SwiftFormat "--wrapcollections before-first" does this.
let colorNameToHex = [
    "Red": "ff0000",
    "Green": "00ff00",
    "Blue": "0000ff"
]
```

Function signatures and calls that do not fit on a single line
should place each parameter/argument on a separate line,
indented by four additional spaces. For example:

```swift
// SwiftFormat "--wrapparameters before-first" does this.
func myVeryLongFunctionName(
    veryLongLabelNumber1: Type1,
    veryLongLabelNumber2: Type2,
    veryLongLabelNumber3: Type3
) -> ReturnType {
    ... code goes here ...
}

// SwiftFormat "--wraparguments before-first" does this.
myVeryLongFunctionName(
    veryLongLabelNumber1: veryLongValueNumber1,
    veryLongLabelNumber2: veryLongValueNumber2,
    veryLongLabelNumber3: veryLongValueNumber3
)
```

## Use SwiftLint

See <a href="./SwiftLint/">SwiftLint</a>.
Avoid enforcing code rules whose violations cannot be detected by this tool.

## Use SwiftFormat

See <a href="./SwiftFormat/">SwiftFormat</a>.
Avoid requiring formatting that cannot be automated using this tool.

## Access Specifiers

Apply the most restrictive access specifiers possible
to avoid unintentionally access items.
For example, most properties and many methods
in structs and classes should be `private`.

## Protocols

There are two ways for a type to implement a protocol,
directly or indirectly using an extension.

In the direct approach, all protocols implemented by a type
are specified at the beginning of the type definition.
Methods required by each protocol are defined inside
the definition of the type.

Here is an example of the direct approach.
While the example defines an `enum`, the same approach
can be used with `struct` and `class` definitions.

```swift
enum HTTPError: Error, LocalizedError {
    case badStatus(status: Int)
    case badUrl
    case jsonEncode

    public var message: String? {
        switch self {
        case let .badStatus(status):
            return "bad status \(status)"
        case .badUrl:
            return "bad URL"
        case .jsonEncode:
            return "JSON encoding failed"
        }
    }
}
```

In the indirect approach, each protocol implemented by a type
is associated with it using a separate extension.
This has the benefit of making it clear which methods
are associated with each protocol being implemented.

Here is an example of the direct approach:

```swift
enum HTTPError: Error {
    case badStatus(status: Int)
    case badUrl
    case jsonEncode
}

extension HTTPError: LocalizedError {
    public var message: String? {
        switch self {
        case let .badStatus(status):
            return "bad status \(status)"
        case .badUrl:
            return "bad URL"
        case .jsonEncode:
            return "JSON encoding failed"
        }
    }
}
```

Most Swift developers seem to prefer the indirect approach.

## Preference for async/await over completion handlers (callbacks)

Many older APIs provide asynchronous functions that take
completion handler functions, otherwise known as callbacks.
Callers pass in a function that is invoked
when the asynchronous processing completes.

Newer APIs utilize the Swift `async` and `await` keywords.

Calls to functions that take a completion handler can be wrapped in
an `async` function so they can be used in a more modern way.
This pattern is illustrated by the following contrived example.
both `myAsyncFunction` and `oldStyleFunction` could take arguments.

```swift
func myAsyncFunction() async throws -> SomeReturnType {
    // The "return" here is only necessary if some code precedes it.
    return try await withCheckedThrowingContinuation { continuation in
        // The trailing closure here is the completion handler.
        oldStyleFunction() { result, error in
            if let error = error {
                continuation.resume(throwing: error)
            } else if let result = result {
                continuation.resume(returning: result)
            } else {
                continuation.resume(throwing: "no result found")
            }
        }
    }
}
```

## Emojis in Debug Messages

In order to make debugging messages stand out in the console,
begin them with an emoji such as ✅ or ❌.

To add an emoji in Xcode, press cmd-ctrl-space.
