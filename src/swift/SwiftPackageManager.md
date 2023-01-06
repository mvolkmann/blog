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

Packages can contain unit tests and can depend of other packages.
Dependencies on local packages are expressed using
relative (preferred) or absolute file paths.
Dependencies on remote packages are expressed using Git URLs.

Packages typically reside in their own Git repository.
These repositories can be public or private.
For details on using private Git repositories see {% aTargetBlank
"https://developer.apple.com/documentation/xcode/building-swift-packages-or-apps-that-use-them-in-continuous-integration-workflows",
"Building Swift Packages or Apps that Use Them in Continuous Integration Workflows" %}

For a simple package example, see {% aTargetBlank
"https://github.com/mvolkmann/RMVGeometry", "RMVGeometry" %}.

## Benefits

There are several benefits to factoring out code from a large application
into packages that each reside in their own source control repository.

- It enables sharing code with other applications and packages.
- It decreases application build times since the code that
  has been moved to packages does not need to be compiled.
- It allows developers to focus on code with a specific purpose.
- It reduces the possibilities for source control merge conflicts.

## Creating a Package

To create a new package in Xcode:

- Select File ... New ... Package... .
- Enter a name for the package.
- Select the directory where it will be stored.
- Click the Create button.

To create a local package inside an existing project
for ease of testing from it:

- Follow the steps above, but select the project directory
  and select the project in the "Add to" dropdown.
- Add the package as a dependency of the project.
  - Select the top entry in the Project Navigator.
  - Select the main target.
  - Select the "General" tab.
  - Scroll down to the "Frameworks, Libraries, and Embedded Content" section.
  - Click the "+" button.
  - Under "Workspace", select the local package name.
  - Click the "Add" button.
  - Import the package in each app source file that uses it.

For all types, properties, methods, and functions to be exposed by the package,
change any existing access specifiers to `public`
or add `public` if there is no access specifier.
This includes the `body` computed property in
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

  Enter a description of the package and usage instructions here.
  This can also include license information,
  supported operating systems, supported Swift versions,
  contact information, and tutorials.

- Package.swift

  This is the package manifest file.
  See the [Manifest File](#manifest-file) section below for details.

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
It includes:

- the package name
- the platforms where it runs (defaults to all?)
  (ex. `[.iOS(.v13)]`)
- the products it defines (executables and libraries)
- its dependencies
- the targets it creates

This file takes the place of having a `.xcodeproj` file
as is present in applications.
It is similar to a `package.json` file in a Node.js project.

Here is an example manifest for a package that defines two targets.

```swift
import PackageDescription

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

## Unit Tests

Xcode does not provide a way to run code in a package project,
so it is important to include unit tests.
This enables verifying the functionality of a package
before deploying the initial version or changes to it.

To run all of the tests in a package, select Product ... Test or press cmd-u.

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
- Select the Source Control Navigator (2nd tag).
- Select the "Repositories" tab.
- Right-click "Remotes" and select "New {package-name} Remote...".
- In the dialog that appears, verify the account, enter a description, and
  choose "Public" or "Private".
- Click the "Create" button.

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
It seems odd that Xcode doesn't provide a way to do this!

## Using Packages

To use a package from an application or other package:

- Open the application or package that will use the package in Xcode.

- Select File ... Add Packages... .

  Alternatively this longer set of steps can be used:

  - Select the top entry in the Navigator.
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

## Updating Packages

To update the version of a single package dependency to the latest version
that is compatible with the dependency rule for specified for the package,
right-click the dependency near the bottom of the Navigator
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
