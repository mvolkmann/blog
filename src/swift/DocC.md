---
eleventyNavigation:
  key: DocC
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/docc", "DocC" %}
stands for "Documentation Compiler".
It generates nicely formatted documentation for
the public items in Swift packages.
It is not intended to generate documentation for applications
or for the internal (non-public) features of a package.

DocC reads a variant of Markdown text from project source files
and converts it into a format that can be displayed nicely inside Xcode.
The documentation also be converted to an HTML-based website.

## Documentation Comments

Documentation comments are single-line comments that begin with `///`
or multi-line comments that begin with `/**` and end with `*/`.
Add these before each type, property, method, and function definition.
This can be done manually or by command clicking the first line of
a definition, selecting "Add Documentation", and supplying more detail.

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
Also describe their parameters, the return type, and
errors they may throw as shown in the example below.

If code examples are provided, enclose them in backtick fences (\`\`\`)
or indent the code by four spaces.

Additional named sections that can be supplied include
`Precondition`, `Postcondition`, `SeeAlso`, `Since`, `ToDo`, and `Warning`.

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
        principal: Int,
        years: Int,
        interestRate: Double
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
        principal: Int,
        years: Int,
        interestRate: Double
    ) -> Double {
        return 1000
    }
```

## Symbol Links

To include links to related items, use "symbol links".
These are strings in double backticks where the text is
the name of a type, a method signature, or a function signature.
For example:

```swift
/// Also see ``SomeStruct``.

/// Also see ``SomeStruct/someProperty``.

/// Also see ``SomeStruct/someMethod(param1:param2:)``.
```

Xcode suggests possible completions while typing symbol links,
making their creation less error prone.

To navigate to the source of a symbol link, command-click it.

Symbol links can be absolute or relative to the current source file.
For example, if the symbol link to the `someMethod` appears inside
the `SomeStruct.swift` source file then `SomeStruct/` can be omitted.

## Web Links

To include links to web pages, use Markdown syntax.
For example, `[Swift](https://www.swift.org)`.
Markdown syntax can also be used to include
images, bulleted lists, numbered lists, and tables.

## Quick Help

Option-click a name in a source file to see its documentation
in "Quick Help" which appears in a documentation window.
If the messsage "no documentation available" appears,
select Product ... Build Documentation.

## Generating Documentation

There are three ways to generate DocC documentation for an Xcode project.

1. In Xcode select Product ... Build Documentation or press cmd-ctrl-shift-d.
1. In Xcode "Build Settings", set "Build Documentation During Build" to Yes
   to automatically build documentation every time the project is compiled.
1. Run the command "xcodebuild docbuild" from a terminal or in a CI pipeline.

After generating the documentation, a documentation window is opened.

## Documentation Window

Documentation windows have a left nav that contains lists of names.
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

## Documentation Catalogs

A documentation catalog file improves the initial page
displayed in the documentation window.
TODO: Does this only work for frameworks and packages, not apps?

To add a documentation catalog to a project:

- Right-click a package group in the Navigator.
- Select "Add File...".
- Choose "Documentation Catalog".
- Click the Next button.
- Enter a name. The default name "Documentation.docc is fine.
- Click the Create button.

This adds a directory with the specified name
that includes the file `Documentation.md`.
Edit this file to describe the project
and provide an overview of the files inside it.

## Exporting

To export package documentation from Xcode, open a documentation window,
right-click the package, and select "Export...".
This can be done in the package project
or from an application that uses the package.
In the dialog that appears, optionally change the name of the file to be
created (the default is fine), select the directory where it will be stored,
and click the "Export" button.

The exported file will have an extension of `.doccarchive`.
It can be shared with other developers using email or other means.
Double-clicking the file opens the documentation in Xcode.

For more information about exporting documentation, see {% aTargetBlank
"https://developer.apple.com/documentation/xcode/distributing-documentation-to-external-developers",
"Distributing Documentation to External Developers" %}.

Documentation archive files includes a single-page web app
for rendering the documentation on the web.
The steps required to deploy this vary based on the web server being used.
See the section titled "Host a Documentation Archive on Your Website"
at the link above.

## Other Options

Options not covered here include adding
articles, tutorials, and extension files.

## Resources

For more detail on symbol links, see {% aTargetBlank
"https://developer.apple.com/documentation/xcode/formatting-your-documentation-content",
"Formatting Your Documentation Content" %}.
Also see the article {% aTargetBlank
"https://www.raywenderlich.com/34919511-docc-tutorial-for-swift-getting-started",
"DocC Tutorial for Swift : Getting Started" %}.
