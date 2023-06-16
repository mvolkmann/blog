---
eleventyNavigation:
  key: Macros
  parent: Swift
layout: topic-layout.njk
---

## Overview

Swift macros "allow you to generate repetitive code at compile-time".

Swift macros are:

- type-checked by the compiler before they are applied,
  unlike C macros which are applied before compile-time
- additive, never modifying or deleting existing code
- implemented in Swift
- defined in compiler plugins

For more detail, watch the WWDC 2023 videos:

- {% aTargetBlank "https://developer.apple.com/wwdc23/10166",
  "Write Swift macros" %}
- {% aTargetBlank "https://developer.apple.com/wwdc23/10167",
  "Expand on Swift macros" %}.

## Provided Macros

The following macros are provided by Swift and SwiftUI:

- {% aTargetBlank
  "https://developer.apple.com/documentation/swift/optionset-swift.macro",
  "@OptionSet" %} creates an option set from a `struct`
  that contains a nested `Options` `enum`.
- {% aTargetBlank
  "https://developer.apple.com/documentation/widgetkit/preview-macros",
  "#Preview" %} generates code to
  display a preview of a SwiftUI widget in Xcode.

- TODO: Find more of these.

## Third Party Macros

The following macros are not provided by Apple:

- {% aTargetBlank "https://github.com/ailtonvivaz/swift-request",
  "SwiftRequest" %} provides macros for sending all kinds of HTTP requests.

## Defining Macros

The steps to define a new macro are:

1. Open Xcode.
1. Select New ... File ... Package
1. Select the "Swift Macro" template.
1. Click the "Next" button.
1. Enter a package name.
1. Select the directory where it will be saved.
1. Click the "Create" button.

The generated file structure will contain the following:

- `{package-name}` directory

  - `Package.swift`

    This describes the platforms on which the macro can run,
    its dependencies, and the targets it can create
    (definining library, client example, and unit tests).

  - `Sources` directory

    - `{package-name}` directory

      - `{package-name}.swift`

        This file contains the declaration of the `stringify` macro
        which serves as an example macro
        This defines the macro signature.
        Add new macro declarations in this file and
        optionally delete the definition for the `stringify` macro.

        ```swift
        @freestanding(expression)
        public macro stringify<T>(_ value: T) -> (T, String) =
            #externalMacro(
                // This refers to a Swift file below.
                module: "MyFirstMacroMacros",
                // This refers to the name of an ExpressionMacro subtype
                // define in the Swift file.
                type: "StringifyMacro"
            )
        ```

    - `{package-name}Client`

      - `main.swift`

        This file contains an example of using the `stringify` macro.
        To see the definition (not implementation) of a given macro invocation,
        right-click it and select "Jump to Definition".
        To run this, ???

    - `{package-name}Macros`

      - `{package-name}Macro.swift`

        This file contains the implementation of the `stringify` macro.
        Add new macro implementations in this file and
        optionally delete the implementation for the `stringify` macro.

        The following code is the stringify macro implementation
        with comments added:

        ```swift
        public struct StringifyMacro: ExpressionMacro {
            // This method is required by the ExpressionMacro protocol.
            public static func expansion(
                // This argument is the AST of the passed expression.
                of node: some FreestandingMacroExpansionSyntax,
                // This argument provides access to surrounding data.
                in context: some MacroExpansionContext
            ) -> ExprSyntax {
                // This evaluates the expression passed in
                // which was defined to be an Int.
                guard let argument = node.argumentList.first?.expression else {
                    fatalError("compiler bug: the macro does not have any arguments")
                }

                // The string expression returned here
                // is automatically parsed into an AST.
                return "(\(argument), \(literal: argument.description))"
            }
        }
        ```

  - `Tests` directory

    - `{package-name}Tests` directory

      - `{package-name}Tests.swift`

        This file contains unit tests for the macros.
        Each test function calls `assertMacroExpansion`,
        passing it a string that invokes a macro, the expected result, and
        a `macros` argument whose value is a `Dictionary`
        where the keys are macro names and
        the values are the corresponding macro definitions.

        To run all the tests, select Product ... Test or press cmd-u.

Macro definitions are similar to function definitions.
They begin with the `macro` keyword and are followed by
a name, parameter list, and return type.

## Macro Types

- `@freestanding(expression)` - creates code that returns a value
- `@freestanding(declaration)` - creates one or more declarations
- `@attached(peer)` - adds peer declarations next to an existing declaration
- `@attached(accessor)` - adds accessors to a property of a type
- `@attached(memberAttribute)` - adds attributes to declarations in a type
- `@attached(member)` - adds declarations inside a type on which it is applied
- `@attached(conformance)` - adds protocol conformances to a type

Freestanding macros are invoked with a `#`.
Attached macros are invoked with a `@`.

## Freestanding Macros

Freestanding macro definitions begin with `@freestanding(expression)`
and can be applied to any expression.

The source code of the expression is made available to the macro
in an abstract syntax tree (AST).

The macro can transform the AST to provide the result AST
which is used to generate source code that is then passed to the compiler.

## Attached Macros

Attached macros augument Swift declarations for entities they decorate
such as variables, functions, enums, structs, and classes.

## Generics in Macros

Macros can be declared to use generic types
in order to make them usable in more scenarios.

## Debugging Macros

To view the AST made available to a custom macro,
set a breakpoint in the macro implementation
and run a test that uses the macro.
In the debugger, use the `po` command to
pretty-print a variable that holds the AST.

## Using Custom Macros

To use custom macros in a Swift project:

- Open the project in Xcode.
- Right-click the top entry in the Project Navigator.
- Select "Add Package Dependencies...".
- In the dialog that appears, click the "Add Local..." button.
- Check the checkboxes for each of the packages
  that define the macros to be used.
- Click the "Add Package" button.

In each file that wishes to use the macros:

- Add `import {package-name}`
- Add macro invocations in the appropriate places in the source code.

To see learn about a given macro, option-click an invocation
to open a dialog containing the documentation.

To see the code generated by a given macro, right-click its invocation
and select "Expand Macro".

## Invoking Macros

A macro is invoked by preceding its name with a pound sign (`#`).

## Unit Tests

Macro unit tests typically call the `assertMacroExpansion` function.
That function takes a `diagnostics` argument whose value is
an array of diagnostic messages that should be emitted
when the macro is invoked incorrectly, such as when
an attached macro is applied to the wrong kind of declaration.

Macros typically throw specific cases of a custom enum
that conforms to the `CustomStringConvertible` and `Error` protocols.
For example:

```swift
enum MyCustomError: CustomStringConvertible, Error {
    case badThing1
    case badThing2

    var description: String {
        switch self {
            case .badThing1: return "first bad thing happened"
            case .badThing2: return "second bad thing happened"
        }
    }
}
```

When non-test code uses a macro in a way that throws an error,
the error message will appear in Xcode next to the offending line
just like other compiler generated errors.

## Examples

- {% aTargetBlank "https://github.com/mvolkmann/TypeInitMacro", "TypeInitMacro" %}

  This macro adds a member-wise initializer to a `class` or `struct`.
