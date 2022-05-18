---
eleventyNavigation:
  key: XCTest
  parent: Swift
  order: 3
layout: topic-layout.njk
---

## Overview

XCTest is a unit testing framework for SwiftUI.
"XC" is an abbreviation for "Xcode".

## Configuration

1. Select the topmost entry in the Navigator.
1. Click the "+" button at the bottom of the left nav to create a new target.
1. Select "Unit Testing Bundle".
1. Click the "Next" button.
1. Click the "Finish" button.

   Note that a new folder appears in the Navigator whose name is
   the project name followed by "Tests".
   This contains a `.swift` file with the same name.

1. Edit the provided `.swift` file.
1. To make all the source files in the project available to the test,
   add the line `@testable import {project-name}` before the class definition.
1. Add test methods whose names begin with "test".
1. To make an assertion, call one of the `XCTAssert{kind}` functions.

   Assertion kind values include `Nil`, `NotNil`, `True`, `False`,
   `Equal`, `NotEqual`, `Identical`,
   `LessThan`, `LessThanOrEqual`, `GreaterThan`, `GreaterThanOrEqual`,
   `NoThrow`, and `ThrowsError`.
   Each of these has a version that takes and does not take an error message.

1. To run a single test method, click the diamond where the
   line number of the first line of the method (starts with `func`)
   would normally appear.
1. To run all the test methods in the file, click the diamond where the
   line number of the first line of the class would normally appear.
1. To run all the tests in Navigator folder, click the diamond
   to the right of the folder name.
1. To run all the tests, select Product ... Test or press cmd-u.

## Test Results

To see a report on test results:

1. Click the "Show Report Navigator" button,
   which is the last button at the top of the Navigator,
   and then click a test run with a specific time.
1. In the editor panel where test results are displayed,
   click a category of test results to view
   which include "All", "Passed", "Failed", "Skipped",
   "Expected Failures", and "Mixed".

## Code Coverage

To enable collecting code coverage data:

1. Click the target dropdown at the top of Xcode.
1. Select "Edit Scheme...".
1. Select "Test" in the left nav.
1. Click the "Options" tab.
1. Check the "Gather coverage for" checkbox.
1. Select "some targets" in the dropdown to the right of this checkbox.
1. Click the "+" button to select a target.
1. Select the target of the main app.
1. Click the "Close" button.
1. Run all the tests again.
1. View the test results as described above.
1. Click "Coverage" under the new test run.
1. Expand the test folder displayed to see the coverage percentage
   of each source file.

## View Testing

To test SwiftUI Views, install the {% aTargetBlank
"https://github.com/nalexn/ViewInspector", "ViewInspector" %} framework.
This works in conjunction with XCTest.

1. Select the topmost entry in the Navigator.
1. Select the project.
1. Click the "Package Dependencies" tab.
1. Click the "+" button to add a package.
1. In the search input, enter "ViewInspector".
1. Select "ViewInspector".
1. Click the "Add Package" button.
1. Click the next "Add Package" button.
1. This should add an `inspect` method to each view,
   but it isn't currently working!
