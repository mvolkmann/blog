---
eleventyNavigation:
  key: XCTest
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/xctest", "XCTest" %}
is a unit testing framework for SwiftUI.
"XC" is an abbreviation for "Xcode".

## Unit Test Configuration

1. Select the topmost entry in the Navigator.
1. Select the main target.
1. Click the "+" button at the bottom of the left nav to create a new target.
1. Select "Unit Testing Bundle".
1. Click the "Next" button.
1. Click the "Finish" button.

   This creates a new folder that appears in the Navigator
   whose name is the project name followed by "Tests".
   The new folder contains a `.swift` file with the same name
   containing starter test code.

1. Edit the provided `.swift` files in the new target.

## UI Test Configuration

1. Select the topmost entry in the Navigator.
1. Click the "+" button at the bottom of the left nav to create another target.
1. Select "UI Testing Bundle".
1. Click the "Next" button.
1. Click the "Finish" button.

   This creates a new folder that appears in the Navigator
   whose name is the project name followed by "UITests".
   The new folder contains a `.swift` file with the same name
   containing starter test code.

1. Edit the provided `.swift` files in the new target.

## Implementing Tests

For each set of related test methods:

1. Create a test file.

   - Create a file in the {project-name}Tests directory
     whose name ends with `Tests.swift`.
   - Make the class inherit from {% aTargetBlank
     "https://developer.apple.com/documentation/xctest/xctestcase",
     "XCTestCase" %}.
   - Add the line `@testable import {project-name}` before the class definition.
   - Replace hyphens in the project name, if any, with underscores.

1. Define setup steps.

   For setup that cannot throw, override the `setUp` method.
   For setup that can throw, override the `setUpWithError` method.

1. Define tear down steps.

   For tear down that cannot throw, override the `tearDown` method.
   For tear down that can throw, override the `tearDownWithError` method.

1. Add test methods whose names begin with "test".

   A new instance of the `XCTestCase` subtype is created
   before running each of these methods,
   so it is not possible for them to share state.
   This is good because it prevents a test method from being effected
   by previous runs of other test methods.

   A good pattern to follow in test methods is "given, when, then".
   The "given" part establish the conditions to be tested.
   The "when" part takes some action to be tested.
   The "then" part makes assertions that should not be true.

1. Make assertions by calling the `XCTAssert{kind}` functions.

   Assertion kind values include `Nil`, `NotNil`, `True`, `False`,
   `Equal`, `NotEqual`, `Identical`,
   `LessThan`, `LessThanOrEqual`, `GreaterThan`, `GreaterThanOrEqual`,
   `NoThrow`, and `ThrowsError`.
   Each of these has a version that takes and does not take an error message.

   For documentation on the provided assertion functions, see {% aTargetBlank
   "https://developer.apple.com/documentation/xctest", "Test Assertions" %}
   (scroll down a bit).

   Here is an example of an assertion that verifies that a function
   throws a specific error:

   ```swift
   XCTAssertThrowsError(
      try getData(date: Date()),
      "failed to get data"
   ) { error in
      let myError = error as? MyErrorType
      XCTAssertEqual(myError, MyErrorType.invalidDate)
   }
   ```

   An alternative to using `XCTAssertThrowsError` is ot use `do` and `catch`
   along with {% aTargetBlank
   "https://developer.apple.com/documentation/xctest/xctfail", "XCTFail" %}
   as follows:

   ```swift
   do {
       try someAsyncFunction(someArguments)
   } catch {
       XCTFail("some message")
   }
   ```

## Running Tests in Xcode

1. To run a single test method, click the diamond where the
   line number of the first line of the method would normally appear.
   If the diamonds don't appear, click a different source file
   and they click back on the test file.
1. To run all the test methods in the file, click the diamond where the
   line number of the first line of the class would normally appear.
1. To run all the tests in Navigator folder,
   click the diamond to the right of the folder name.
1. To run all the tests, select Product ... Test or press cmd-u.

In UI tests, the on-screen keyboard doesn't always appear.
If it doesn't, tests that require typing in text fields will fail.
To fix this, go the Simulator app, select I/O ... Keyboard,
and unselect "Connect Hardware Keyboard".

## Viewing Test Results

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

## UI View Testing

In a test method, to wait for a specific view to be created:

```swift
let viewExists = someView.waitForExistence(timeout: seconds)
XCTAssertTrue(viewExists)
```

To generate test code by recording user interactions:

1. Click inside a test function where code should be inserted.
1. Click the red "record" circle at the bottom of the editor pane
   of the test source file.
1. Wait for the app to begin running in the Simulator.
1. Interact with the UI to navigation to the point within the app
   where assertions should be made. This includes tapping text fields
   to move focus into them, typing text, tapping buttons,
   and selecting items from pickers.
1. Optionally manually improve the generated test code.
1. Add assertions about what should be in the UI.

## Add Utility Methods

Add utility methods in an extension of the `XCTestCase` class
to simplify writing tests. For example:

```swift
import XCTest

extension XCTestCase {
    static var app = XCUIApplication()

    func buttonExists(_ label: String) throws {
        XCTAssertTrue(XCTestCase.app.buttons[label].exists)
    }

    func enterSecureText(label: String, text: String) {
        XCTestCase.app.secureTextFields[label].tap()
        for char in text {
            XCTestCase.app.keys[String(char)].tap()
        }

        /* Tests fail with this approach.
        let field = XCTestCase.app.secureTextFields[label]
        field.tap()
        field.typeText(text)
        */
    }

    func enterText(label: String, text: String) {
        XCTestCase.app.textFields[label].tap()
        for char in text {
            let key = XCTestCase.app.keys[String(char)]
            key.tap()
        }

        /* Tests fail with this approach.
        let field = XCTestCase.app.textFields[label]
        field.tap()
        field.typeText(text)
        */
    }

    func tapButton(label: String) {
        XCTestCase.app.buttons[label].tap()
    }

    func textExists(_ text: String) throws {
        XCTAssertTrue(XCTestCase.app.staticTexts[text].exists)
    }
}
```

In the `setUpWithError` method of test files, add the following:

```swift
XCTestCase.app.launch()
```

## Run from Command Line

See {% aTargetBlank
"https://www.appsdeveloperblog.com/run-xcode-unit-tests-from-the-command-line/",
"Run Xcode Unit Tests From The Command Line" %}.

To enable this, enter
`sudo xcode-select -s /Applications/Xcode.app/Contents/Developer`

Start the Simulator app. Will the tests fail without this?
To toggle between light and dark mode, select Features ... Toggle Appearance.

`cd` to a directory containing a `.xcodeproj` file.

To get lists of targets, build configurations, and schemes
defined in an Xcode project, enter `xcodebuild -list`

To get a list of available devices supported by the simulator,
enter `xcrun simctl list devices`

To build and run all the XCTests,
enter an `xcodebuild` command similar to the the following:

```bash
xcodebuild \
  -scheme MyApp \
  -destination 'name=iPhone 13'
  -enableCodeCoverage YES \
  clean test
```

Add `-quiet` to reduce the output and only show errors.

If

## GitHub Actions

See {% aTargetBlank "https://vmois.dev/xcode-github-actions/",
"How to use GitHub Actions for testing Xcode project" %}.
