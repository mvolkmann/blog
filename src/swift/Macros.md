---
eleventyNavigation:
  key: Macros
  parent: Swift
layout: topic-layout.njk
---

## Overview

Swift macros "allow you to generate repetitive code at compile-time."

Unlike C macros which are applied before compile-time,
Swift macros are type-checked by the compiler before they are applied.

Macros are implemented in Swift and defined in compiler plugins.

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

  - `Sources` directory

    - `{package-name}` directory

      - `{package-name}.swift`

        This file begins with the definition of the `stringify` macro
        which serves as an example.
        Add new macro definitions in this file and
        optionally delete the `stringify` macro.

    - `{package-name}Client`

      - `main.swift`

        This file contains an example of using the `stringify` macro.

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

## Invoking Macros

A macro is invoked by preceding its name with a pound sign (`#`).
