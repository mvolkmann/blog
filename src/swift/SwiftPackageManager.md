---
eleventyNavigation:
  key: Swift Package Manager
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.swift.org/package-manager/",
"Swift Package Manager" %} (SPM) provides a standard way to
define and use software packages in Swift applications.
SPM is preferred over other packaging options such as
{% aTargetBlank "https://cocoapods.org", "CocoaPods" %} and
{% aTargetBlank "https://github.com/Carthage/Carthage", "Carthage" %}
because it is provided by Apple and is integrated into Xcode.

Packages can be local to a specific project or remote.

Packages can depend of other packages.
Dependencies on local packages are expressed using
relative (preferred) or absolute file paths.
Dependencies on remote packages are expressed using Git URLs.

Packages can contain unit and integration tests.

Packages typically reside in their own Git repository.
These repositories can be public or private.
For details on using private Git repositories see {% aTargetBlank
"https://developer.apple.com/documentation/xcode/building-swift-packages-or-apps-that-use-them-in-continuous-integration-workflows",
"Building Swift Packages or Apps that Use Them in Continuous Integration Workflows" %}

For example packages, see
{% aTargetBlank "https://github.com/mvolkmann/RMVGeometry", "RMVGeometry" %} and
{% aTargetBlank "https://github.com/mvolkmann/RMVSwiftUIViews",
"RMVSwiftUIViews" %}.

## Benefits

There are several benefits to factoring out code from a large application
into packages that each reside in their own source control repository.

- It enables sharing code with other applications and packages.
  Each of these can select a different version to use.
  They do not all need to upgrade to a new version at the same time.
- It enables testing and improving the code for each package in isolation.
- It decreases application build times since the code that
  has been moved to packages does not need to be compiled.
- It allows developers to focus on code with a specific purpose.
- It reduces the possibilities for source control merge conflicts.

## Terminology

- Module

  The term "module" is somewhat of an umbrella term.
  Libraries, frameworks, and Swift packages are all kinds of modules.
  A new project begins as a single module.

- Swift Package

  A "Swift package" is a collection of libraries.
  Many packages define a single library.
  A package can be added as a dependency of an app or another package.
  A package can be local to the project that uses it or
  be in a remote location such as a GitHub repository
  which allows it to be used in multiple projects.

- Library

  A "library" is a collection of targets
  that define types that have a common goal.
  Some libraries contain a single target.
  Examples include network utilities, database access,
  and a specific category of business logic.
  A library can be static or dynamic.

  A static library is compiled into each app that uses it
  which increase the executable size of each app.

  A dynamic library lives outside the apps the use it
  amd is linked into each app at runtime,
  not impacted their executable size.
  When a dynamic library is updated, all the apps that use
  can automatically use the newest version.

  On Apple platforms, only libraries supplied by Apple can be dynamic.
  All libraries you create will be static.

- Target

  A "target" can be an executable or a set of closely related types.
  Swift code imports non-executable targets by name.

- Framework

  A "framework" is folder that holds code, resources (such as images),
  static libraries, and references to dynamic libraries.
  Examples of Apple-provided frameworks include AppKit (for macOS apps),
  CloudKit, Core Data, Core Location, Dispatch, Foundation, HealthKit,
  HomeKit, JavaScriptCore, MapKit, MusicKit, PDFKit, SceneKit, SpriteKit,
  StoreKit, Swift Charts, SwiftUI, System, UIKit, WatchKit, WeatherKit,
  WebKit, WidgetKit, and XCTest.

It is common for the names of packages, libraries, and targets
to have UpperCamelCase names.
It is common for git repositories that hold the code for a package
to have a kebab-case name.

The package names used by an app must be unique within that app.
One way to avoid name conflicts it to add a two or three letter prefix
to package names that identifiers the company or developer that created it.

## Dependencies

Every Swift app and package can have dependencies on other Swift packages.
Swift Package Manager manages these dependencies.
It can install directly specified dependencies
and the dependencies of those recursively.

Swift Package Manager honors many kinds of version constraints
in order to select the Git commit of a package that:

- is tagged with the highest semantic version that has a given major version
  (ex. `from: "2.0.0"`)
- is tagged with the highest semantic version
  that has a given major and minor version
  (ex. `from: "2.3.0"`)
- is the latest on a given branch (ex. `.branch("branch-name")`)
- is tagged with a given semantic version (ex. `.exact("1.2.3")`; rarely used)
- is tagged with the highest semantic version in a range of semantic versions
  (ex. `"2.1.4"..."3.5.1"`; rarely used)
- has a given commit identifier
  (ex. `.revision("9c0445321380e97fb8f3d3f000c22a495f13b4d6")`; rarely used)

## Creating a Package

To create a new package from the command line:

- cd to the directory that will hold the new package.
- Create the package directory by entering `mkdir {package-name}`.
- Enter `swift package init`.

This creates the directory structure and files described in the
[Package Contents](#package-contents) section.

To create a new package in Xcode:

- Select File ... New ... Package...
- In the dialog that appears, enter a name for the package.
- Select the directory where it will be stored.
- Click the Create button.

To create a local package inside an existing project
for ease of testing from it:

- Follow the steps above, but in the dialog select the project directory
  and select the project in the "Add to" dropdown.
  This creates a new scheme for the library inside the package
  that is used to build and test the library.
- Add the package as a dependency of the project.
  - Select the top entry in the Project Navigator.
  - Select the main target.
  - Select the "General" tab.
  - Scroll down to the "Frameworks, Libraries, and Embedded Content" section.
  - Click the "+" button.
  - Locate a package to be added and select one of its libraries.
    Often a package only contains a single library.
  - Click the "Add" button.
    This provides access to all the targets defined in the library.
  - Import targets from the library in each app source file that will use them.

For all types, properties, methods, and functions to be exposed by the package,
change any existing access specifiers to `public`
or add `public` if there is no access specifier.
This includes the parameter and return types of functions.
It also includes the `body` computed property in
structs that conform to the `View` protocol.

For structs that are taking advantage of
the default memberwise initializer (have no `init` method),
add a `public` `init` method that takes all the `struct` properties as arguments
and explicitly initializes all the properties.

If the package includes `.xcassets` files and
uses image sets in them in SwiftUI `Image` views,
add `bundle: .module` to those `Image` initializer calls.

Add DocC documentation to each type and method.
To have Xcode supply as much of this as it can,
select the first line of a type, method, or function
and select Editor ... Structure ... Add Documentation
or press cmd-option-/.
After adding this documentation, rebuild the project
and option-click on the name of a documented item
to see the documentation in a popup.

## Package Contents

The initial package contents are:

- README.md

  This is a Markdown file that describes the package.
  It can include the goals of the package, license information,
  supported operating systems, supported Swift versions,
  installation steps, usage information, and contact information.

- Package.swift

  This is the package manifest file.
  See the [Manifest File](#manifest-file) section below for details.

- Sources

  - {package-name}

    This directory holds all the package source files
    and resources such as `.xcassets` files
    for the main target.
    If the package defines multiple targets,
    there will be a separate directory under `Sources for each.

    - {package-name}.swift

      This is the starting point of the code.

- Tests

  - {package-name}Tests

    This directory holds unit test and integration test source files
    for the main target that use the XCTest framework.
    If the package defines multiple targets,
    there will be a separate directory under `Tests` for each.

    - {package-name}Tests.swift

      This is an example unit test file.

Items such as classes, structs, enums, properties, methods, and functions
that should be accessible outside of the package must be marked as `public`.
Default memberwise initializers in structs are not `public`,
so it is necessary to add `public` initializers.

## Manifest File

The file `Package.swift` is the package manifest.
It contains Swift code the creates a `Package` object.
The `Package` initializer is passed the following arguments:

- `name` is a `String` that specifies the package name.
- `defaultLocalization` is a `String` that specifies the default language.
- `platforms` is an optional array of {% aTargetBlank
  "https://developer.apple.com/documentation/packagedescription/supportedplatform",
  "SupportedPlatform" %} instances that describe the
  operating systems and versions where the package can run.
  Versions can be specified using predefined constants or strings.
  For example, `[.iOS(.v14), .watchOS("7.0.0")]`.
  Other platforms include `.macOS`, `.tvOS`, and `.linux`.

  Packages can be used on all platforms even if they are not listed here.
  When a platform is omitted from this array,
  the first version of the platform that supports Swift
  is assumed to be the minimum supported platform version.

- `products` is an array of {% aTargetBlank
  "https://developer.apple.com/documentation/packagedescription/product",
  "Product" %} objects that describe the
  executables, libraries, and plugins that the package produces.
- `dependencies` is an array of {% aTargetBlank
  "https://developer.apple.com/documentation/packagedescription/package/dependency",
  "Package.Dependency" %} objects that describe the packages,
  including version constraints, on which this package depends.
- `targets` is an array of {% aTargetBlank
  "https://developer.apple.com/documentation/packagedescription/target",
  "Target" %} objects that describe the targets this package produces.

This file takes the place of having a `.xcodeproj` file
as is present in applications.
It is similar to a `package.json` file in a Node.js project.

Here is an example manifest for a package that defines two targets.

```swift
import PackageDescription

// This specifies the version of Swift Tools the `Package` object assumes and
// by association the minimum version of Swift needed to build this package.
// Even though it is a comment, Swift Package Manager uses it.
// swift-tools-version: 5.7

let package = Package(
    name: "RMVGeometry",
    defaultLocalization: "en",
    platforms: [
        .macOS(.v11), .iOS(.v14), .watchOS(.v7), .tvOS(.v14)
    ],
    products: [
        // Products define the executables and libraries a package produces,
        // and make them visible to other packages.
        .library(
            name: "RMVGeometry",
            targets: ["RMVCircle", "RMVGeometry"]
        ),
    ],
    dependencies: [
        // Dependencies describe other packages on which this package depends.
        // For packages in a remote repository ...
        // .package(url: some-url, from: "some-version"),
        // For packages in the local file system ...
        // .package(url: some-file-path, from: "some-version"),
        // some-file-path can be absolute or relative.
        // Relative paths are preferred so they can
        // remain valid in a CI/CD environment.
    ],
    targets: [
        // A target can define a module or a test suite.
        // Targets can depend on other targets in this package,
        // and on products in packages this package depends on.
        .target(name: "RMVCircle"),
        .testTarget(name: "RMVCircleTests"),
        .target(name: "RMVGeometry", dependencies: []),
        .testTarget(name: "RMVGeometryTests", dependencies: ["RMVGeometry"])
    ]
)
```

## Building a Package

Building a package begins by downloading all of its remote dependencies
recursively.

To build a package from the command-line, enter `swift build`.

To build a package inside Xcode,
select Product ... Build or press cmd-b.

## Unit Tests

Xcode does not provide a way to run code in a package project,
so it is important to include unit tests.
This enables verifying the functionality of a package
before deploying the initial version or changes to it.

To run all of the tests in a package from the command-line,
enter `swift test`.

To run all of the tests in a package inside Xcode,
select Product ... Test or press cmd-u.

## Documentation

Package documentation is provided by the README .md file and by
{% aTargetBlank "/blog/topics/#/blog/swift/DocC/", "DocC" %}
comments in the code.

## Available Features

To state that items in a library should only be available
in applications that require at least a certain version of iOS,
add an `@available(iOS {version}, *)` attribute before the item definition
where `{version}` is an iOS version like `16.0`.
This is alway needed when an item uses SwiftUI.

## Using GitHub

Swift packages are frequently stored in a GitHub repository.
Each package must be in its own repository.
To create a GitHub repository for a new package inside Xcode:

- Quit Xcode.
- If the package directory is inside a project directory that
  has a git repository, move the package directory to a new location.
- In the Finder, locate the `Package.swift` file of the package.
- Double-click this file in the Finder to open an Xcode window
  that only shows the files in the package.
- Select the "README.md" file and add details.
- Select Source Control ... New Git Repository...
- Click the "Create" button.
- If your GitHub account has not already been associated with Xcode:

  - Select Xcode ... Settings...
  - Select the "Accounts" tab.
  - Click the "+" button in the lower-left.
  - In the dialog that appears, select "GitHub".
  - Click the "Continue" button.
  - Enter a GitHub account name.
  - Paste in a personal access token for the account.
  - Click the "Sign In" button.

- Open the Source Control Navigator which is the second Navigator tab.
- Click the "Repositories" tab.
- Right-click the top entry and select "New {package-name} Remote...".
- In the dialog that appears, verify the account, enter a description, and
  choose "Public" or "Private".
- Click the "Create" button.
- Select the Source Control Navigator (2nd tab).
- Select the "Repositories" tab.

Package versions are specified with Git tags that are semantic version numbers.
To add a tag to the current commit of a Swift package:

- Open the Source Control Navigator which is the second Navigator tab.
- Click the "Repositories" tab.
- Expand the "Tags" section to see the existing tags.
- Right-click the top entry and select "Tag {branch-name}..."
  where branch-name is typically "main".
- In the dialog that appears, enter a semantic version number
  that is higher than the previous tag (ex. 1.2.3).
- Optionally enter a description in the Message input.
- Click the Create button.
- Select Source Control ... Push...
- In the dialog that appears,
  check the "Include Tags" checkbox and click the "Push" button.

For each new version of the package to be published,
commit the changes, tag the commit, and push it as described above.

To view the GitHub repository in your default web browser:

- Open the Source Control Navigator which is the second Navigator tab.
- Click the "Repositories" tab.
- Right-click the top entry and select "View on GitHub...".

## Finding Packages

There is no official package index similar to {% aTargetBlank
"https://www.npmjs.com", "npmjs" %} for Node.js packages.
{% aTargetBlank "https://swiftpackageindex.com", "Swift Package Index" %}
is an unofficial index where you can search for open source packages.
As of January 7, 2023 it provides information on 5,306 packages.

## Using Packages

To use a package in an application or other package:

- Open the application or package that will use the package in Xcode.

- Select File ... Add Packages... .

  Alternatively this longer set of steps can be used:

  - Select the top entry in the Project Navigator.
  - Select the project.
  - Click the "Package Dependencies" tab.
  - Click the "+" button at the bottom.

- Paste the Git URL into the search input.
  This renders the Markdown found in the `README.md` file of the package.
  TODO: Why doesn't this work for many packages including `RMVGeometry` package?

- In the "Dependency Rule" dropdown, select one of the following options
  where the repository tags are semantic version numbers:

  - Version: Up to Next Major -
    latest tag with a specified major version (most common)
  - Version: Up to Next Minor -
    latest tag with a specified major.minor version
  - Version: Range - latest tag in a specified range
  - Version: Exact - specific tag
  - Branch: latest commit on a specific branch
  - Commit: specific commit

- Enter a semantic version number.
- Click the "Add Package" button.
- In the confirmation dialog that appears,
  click a second "Add Package" button.
- In source files that will use the package,
  import the desired package targets.
- Add code that uses the public types, functions, and values
  defined in the imported package targets.

To update the version of all package dependencies,
select File ... Packages ... Update to Latest Package Versions.

To update the version of a specific package dependency:

- Open the Project Navigator.
- Under "Package Dependencies", find the package and right-click it.
- Select "Update Package".

To see source code for a package dependency:

- Open the Project Navigator.
- Under "Package Dependencies", find the package and expand it.
- Expand the "Sources" directory.
- Click a source file to view it.

In source files that wish to use a package,
import one or more targets defined by the package.
Often a package defines a single target
whose name is the same as that of the package.
But the names do not always match,
especially when a package defines multiple targets.

## Updating Packages

To update the version of a single package dependency to the latest version
that is compatible with the dependency rule for specified for the package,
right-click the dependency near the bottom of the Project Navigator
and select "Update Package".

To update the version of all of the application dependencies,
select File ... Packages ... Update to Latest Package Versions.

Application projects contain the file
`{project-name}.xcodeproj/project.xcworkspace/xcshareddata/swiftpm/Package.resolved`.
This file records the exact version of each package dependency being used.
It is similar to a `package-lock.json` file in a Node.js project.

The `Package.resolved` file can be checked into version control to enable
all developers on a team to use the same versions of all the dependencies.
This file is updated when a new package dependency is added
and when an existing package dependency is updated.

Other options in the File ... Packages menu include:

- Reset Package Caches

  This deletes all local package data and downloads all the
  dependencies again from their Git repositories.
  Sometimes this can be used to resolve odd build errors.

- Resolve Package Versions

  This updates the version of each dependency being used
  to be the version specified in the `Package.resolved` file.
  It is useful when another team member has updated that file.

## Adding Assets to a Package

To add assets such as color sets and image sets to a package:

- For a target the will use assets, right-click
  the target folder under the `Sources` folder.
- Select "New Folder".
- Name the new folder `Resources`.
- Right-click the new `Resources` folder.
- Select "New File...".
- Select "Asset Catalog".
- Click the "Next" button.
- The file `Media.xcassets` will be added.
- Add image and color sets in the same way as
  adding them to `Assets.xcassets` in an application.
- Edit the manifest file `Package.swift`.
- Add an argument like the following to the `.target` call for the target:

  ```swift
  resources: [.process("{TargetName}/Resources/Media.xcassets")]
  ```

  TODO: Describe how `.copy` differs from `.process`.

- Build the package and verify that there are no errors.
- Commit and push the changes to Git.
- Tag the commit and push the new tag.

Here is an example of a package method that returns a SwiftUI `Image`:

```swift
    public func comet(size: Double) -> some View {
        // This looks for the image inside the .xcassets file in this package
        // instead of looking in the .xcassets file of the using application.
        Image(uiImage: UIImage(named: "Comet", in: .module, with: nil)!)
            .resizable()
            .scaledToFit()
            .frame(width: size, height: size)
    }
```

## Implementing Unit Tests

Unit tests for a package reside in the `Tests/{package-name}Tests` directory
TODO: Add more to this section.

The `@testable` attribute can be applied to an `import` statement.
It raises the access level of the types exposed by the imported target.
For example, imported types that have an access level of `internal`
are treated as though they have an access level of `public`.
This enables writing tests for them.

For example, the file `IconTests.swift` below
comes from the package GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/RMVSwiftUIViews",
"RMVSwiftUIViews" %}.

```swift
@testable import RMVSwiftUIViews
import XCTest

final class IconTests: XCTestCase {
    func testEnumCases() throws {
        XCTAssertEqual(Icon.baseball.rawValue, "baseball")
        XCTAssertEqual(Icon.bell.rawValue, "sf-bell")
        XCTAssertEqual(Icon.thumbsUp.rawValue, "sf-hand.thumbsup")
    }
}
```

The file `IconToggleTests.swift` below
comes from the same package GitHub repository.
It demonstrates creating an instance of a custom `View` in a test,
including passing it a `Binding`.

```swift
@testable import RMVSwiftUIViews
import SwiftUI
import XCTest

final class RMVSwiftUIViewsTests: XCTestCase {
    func testDefaultParameters() throws {
        var bindingValue = false
        let binding = Binding(
            get: { bindingValue },
            set: { bindingValue = $0 }
        )
        let view = IconToggle(icon: .football, isOn: binding)
        XCTAssertEqual(view.color, Color.black)
        XCTAssertEqual(view.size, 20)
    }

    func testSuppliedParameters() throws {
        var bindingValue = false
        let binding = Binding(
            get: { bindingValue },
            set: { bindingValue = $0 }
        )
        let color: Color = .red
        let size = 50.0
        let view = IconToggle(
            icon: .football,
            color: color,
            size: size,
            isOn: binding
        )
        XCTAssertEqual(view.color, color)
        XCTAssertEqual(view.size, size)
    }
}
```
