---
eleventyNavigation:
  key: Packages
  parent: Swift
layout: topic-layout.njk
---

## Overview

Packages are code libraries that can be used by applications and other packages.
Packages can exist in the same Git repository as a single app that uses it.
More commonly packages reside in their own Git repository
and are added as a dependency in multiple applications using their GitHub URL.

## Creating

To create a new package in Xcode:

- Select File ... New ... Package... .
- Enter a name for the package.
- Select the directory where it will be stored.
- Click the Create button.

## Package Contents

The initial package contents are:

- README.md

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

      This contains unit test code using XCTest.

Items that should be accessible must be marked as `public`.
Note that default memberwise initializers in structs
are not `public`, so it is necessary to add `public` initializers.

## Using

To use a package from an application or other package:

- Open the using application or package in Xcode.
- Select File ... Add Packages... .
- Click the "Add Local..." button at the bottom of the dialog.
- Select the package directory.
- Click the "Add Package" button.
- TODO: Why doesn't it appear in the list of packages now?

OLD?

- Select the topmost entry in the Navigator.
- Select the project in the left nav.
- Select the "Package Dependencies" tab.
- Click the "+" button to open a dialog for adding the package.
- Click the "Add Local..." button at the bottom of the dialog.
- Select the package directory.
- Click the "Add Package" button.
- TODO: Why doesn't it appear in the list of packages now?
