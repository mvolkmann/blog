---
eleventyNavigation:
  key: Project Structure
  parent: Swift
layout: topic-layout.njk
---

## Overview

This post documents my recommended structure for SwiftUI projects.

## Groups

Most files in a project should be placed in a group.
Group names typically begin with an uppercase letter and do not contain spaces.

Recommended group names include:

- Config

  This group includes files like `Assets.xcassets` and `.plist` files

- Extensions

  This group contains files that define extensions to builtin structs
  like `Date`, `String`, and `View`.
  These tend to be copied from project to project as they evolve.

- Models

  This group contains files that define model structs, classes, and enums.
  These hold all the data used by the app.

- Screens

  This group contains files that implement entire screens of the app
  as opposed to sections of a screen.
  Screens typically use views defined in the Views group.

- Services

  This group contains files that abstract away the details of
  network communication and interactions of packages like
  Core Data, CloudKit, and HealthKit.

- Views

  This group contains files that define views used by
  screens and other views.

- ViewModels

  This group contains files that define classes that
  inherit from `ObservableObject`, define `@Published` properties,
  and define methods that operate on the data they encapsulate.
  Views can use the properties and methods in these classes.

## Group and file order

Files not in any group, such as the main `*App.swift`
belong above all the groups.
The groups follow these files and should be in alphabetical order.
The files within each group should be in alphabetical order.

## File name suffixes within groups

The names of files in the "Extensions" group should end with "Extension".
The names of files in the "Screens" group should end with "Screen".
The names of files in the "Services" group should end with "Service".
The names of files in the "ViewModels" group should end with "ViewModel".
The names of files in all other groups
do not need to end with a particular suffix.

## When to use View Models

In strict implementations of the MVVM pattern,
views get all their state data from view models
and delegate all their non-view logic to view models.

In less strict implementations of the MVVM pattern,
state data and logic that is specific to a single view
is implemented in the view using `@State` properties.

## When to use Services

Services are `.swift` files that abstract away the details of
network communication and interactions of packages like
Core Data, CloudKit, and HealthKit.
Prefer using methods of services instead of directly coding
such interactions in view models or views.

## Defining Constants

The Swift naming convention for constants is the same as for variables.
They begin with a lowercase letter and use camel-case,
unlike many other programming languages that use all uppercase
There are several ways to define constants in Swift.

```swift
private static let myConstant1 = "some value"
private static let myConstant2 = "other value"

enum MyConstants {
    static let static let myConstant1 = "some value"
    static let static let myConstant2 = "other value"
}

enum MyConstants {
    case myConstant1 = "some value"
    case myConstant2 = "other value"
}
```

kk## Order of sections within files and use of pragma marks

## Indentation and wrapping style

Use 4-space indentation.

Most lines that are longer than 80 characters should be wrapped to be no longer
than that in order to allow comfortably viewing two or three files side-by-side.

Function signatures and calls that do not fit on a single line (80 characters)
should place each parameter/argument on a separate line indented
by four additional spaces. For example:

```swift
func myVeryLongFunctionName(
    veryLongLabelNumber1: Type1,
    veryLongLabelNumber2: Type2,
    veryLongLabelNumber3: Type3
) -> ReturnType {
    ... code goes here ...
}

myVeryLongFunctionName(
    veryLongLabelNumber1: veryLongValueNumber1,
    veryLongLabelNumber2: veryLongValueNumber2,
    veryLongLabelNumber3: veryLongValueNumber3
)
```

## Use SwiftLint

See <a href="./SwiftLint/">SwiftLint</a>.

## Use SwiftFormat

See <a href="./SwiftFormat/">SwiftFormat</a>.

## Preference for async/await over completion handlers (callbacks)

Many older APIs provide asynchronous functions that take
completion handler functions, otherwise known as callbacks.
Callers pass in a function that is invoked
when the asynchronous processing completes.

Newer APIs utilize the Swift `async` and `await` keywords.

Calls to functions that take a completion handler can be wrapped in
an `async` function so they can be used in a more modern way.
For example:

```swift
TODO: ADD THIS!
```
