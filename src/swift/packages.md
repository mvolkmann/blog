---
eleventyNavigation:
  key: Packages
  parent: Swift
layout: topic-layout.njk
---

## Overview

Packages are code libraries that can be used by applications and other packages.

Packages typically reside in their own Git repository.

Packages can be added as a dependency in multiple applications
using their GitHub URL.

## Creating a Package

To create a new package in Xcode:

- Select File ... New ... Package... .
- Enter a name for the package.
- Select the directory where it will be stored.
- Click the Create button.

## Package Contents

The initial package contents are:

- README.md

  Enter a description of the package and usage instructions here.

- Package.swift

  This defines the package name, products, dependencies, and targets.
  It is referred to as the "manifest file" and takes the place of
  having a `.xcodeproj` file as is present in applications.

- Sources

  - {PackageName}

    - {PackageName}.swift

      This is the starting point of the code.

- Tests

  - {PackageName}Tests

    - {PackageName}Tests.swift

      This contains unit test code that uses XCTest.

Items that should be accessible must be marked as `public`.
Note that default memberwise initializers in structs
are not `public`, so it is necessary to add `public` initializers.

## Manifest File

The file `Package.swift` is the package manifest.
It lists all the source files to be included
and the packages on which this package depends.

Here is an example manifest for a package that defines two targets.

```swift
import PackageDescription

let package = Package(
    name: "RMVGeometry",
    products: [
        // Products define the executables and libraries a package produces,
        // and make them visible to other packages.
        .library(
            name: "RMVGeometry",
            targets: ["RMVCircle", "RMVGeometry"]
        ),
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
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

## Using GitHub

Swift packages are frequently stored in a GitHub repository.
Package versions are specified with Git tags that are semantic version numbers.
To add a tag to the current commit of a Swift package:

- Open the Source Control Navigator which is the second Navigator tab.
- Click the "Repositories" tab.
- Expand the "Tabs" section to see the existing tags.
- Right-click the top entry and select "Tag {branch-name}...".
- In the dialog that appears, enter a semantic version number
  that is higher than the previous tag (ex. 1.2.3).
- Optionally enter a description in the Message input.
- Click the Create button.

The new tag only exists in the local repository.
To push it to the remote repository,
open a terminal window and enter `git push origin --tags`.

## Using a Package

To use a package from an application or other package:

- Open the application or package that will use the package in Xcode.
- Select the top entry in the Navigator.
- Select the project.
- Click the "Package Dependencies" tab.
- Click the "+" button at the bottom.
- Paste the Git URL into the search input.
- In the "Dependency Rule" dropdown, select one of the options.
- Enter a semantic version number.
- Click the "Add Package" button.
- In the confirmation dialog that appears,
  click a second "Add Package" button.
- In source files that will use the package,
  import the desired package targets.
- Add code that uses the public types, functions, and values
  defined in the imported package targets.

## Updating a Package

To use a new version of a package:

- Remove the dependency on the previous version.
- Add a dependency on the new version.
  It is not necessary to paste the package Git URL again.
  Since the package is already know to this project,
  enter the package name in the search input.

TODO: Is this really the best way?
