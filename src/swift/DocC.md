---
eleventyNavigation:
  key: DocC
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/docc", "DocC" %}
is a documentation compiler.
It reads a variant of MarkDown text from project source files
and converts it into a format that can be displayed nicely inside Xcode.
The documentation also be converted to an HTML-based website.

## Documentation Comments

Documentation comments are single-line comments that begin with `///`
or multi-line comments that begin with `/**` and end with `*/`.
Add these before each type, property, method, and function definition.

Documentation is only generated for `public` items.
For non-public item documentation, use double rather than triple slash comments.

In documentation comments that include multiple paragraphs,
separate the paragraphs with a line that only contains `///`.

When documenting properties, describe their meaning,
valid values, and default values if any.

When documenting functions and methods, describe what they do,
any side effects, whether they perform any asynchronous actions,
and scenarios where calling them is expensive.
The first paragraph is the primary description.
All subsequent paragraphs appear in the "Discussion" section.

There are two options for documenting function and method parameters,
one way to document return values, and
one way to document errors that can be thrown.
Each of these are demonstrated in the examples below.

```swift
    /// Computes a monthly loan payment.
    ///
    /// This assumes the interest should be compounded monthly.
    ///
    /// - Parameters:
    ///   - principal: total loan amount
    ///   - years: number of years in the loan
    ///   - interestRate: interest rate compounded monthly
    /// - Returns: the monthly payment
    /// - Throws: `Finance.negative` if any argument is negative
    func monthlyPayment1(
        principal _: Int,
        years _: Int,
        interestRate _: Double
    ) -> Double {
        return 1000
    }

    /// Computes a monthly loan payment.
    ///
    /// This assumes the interest should be compounded monthly.
    ///
    /// - Parameter principal: total loan amount
    /// - Parameter years: number of years in the loan
    /// - Parameter interestRate: interest rate compounded monthly
    /// - Returns: the monthly payment
    /// - Throws: `Finance.negative` if any argument is negative
    func monthlyPayment2(
        principal _: Int,
        years _: Int,
        interestRate _: Double
    ) -> Double {
        return 1000
    }
```

## Links

To include links to related source files, ...

## Content From Other Files

To include documentation found in other files within the project, ...

## Quick Help

Option-click a name in a source file to see its documentation
in "Quick Help" which appears in a popup window.

## Generating Documentation

To generate DocC documentation for a project opened in Xcode,
select Product ... Build Documentation or press cmd-ctrl-shift-d.

After generating the documentation, a documentation window is opened.
This window has a left nav that contains lists of names.
Clicking a name displays its documentation in the main area.

The left nav begins with a section titled "Workspace Documentation".
This is where the names defined in the project appear.
For each target in the project, the "Workspace Documentation" section
lists all the classes and structs defined in the target.

Additional sections in the left nav include:

- "App Frameworks"

  This section includes "Foundation", "Swift", "SwiftUI", "UIKit", "WatchKit",
  and many more.

- "App Services"

  This section includes "CloudKit", "Core Data", "Core Location", "HealthKit",
  "MapKit", "StoreKit", "Watch Connectivity", "WeatherKit", "WidgetKit",
  and many more.

- "Developer Tools"

  This section includes "DocC", "Swift Playgrounds", "Xcode", "XCTest",
  and many more.

- "Graphics and Games"

  This section includes "ARKit", "Core Animation", "Core Graphics",
  "Core Image", "PDFKit", and many more.

- "Media"

  This section includes "AVKit", "Core Audio", "Core Media", "Core Video",
  "iTunes Library", "Media Player", "PhotoKit", and many more.

- "System"

  This section includes "Compression", "CryptoTokenKit", "Dispatch",
  "Local Authentication", "MetricKit", "Network", "os", "Security",
  "SensorKit", "System", "Uniform Type Identifiers", "XPC",
  and many more.

- "Web"

  This section includes "Link Presentation", "Safari app extensions",
  and more.

```

```
