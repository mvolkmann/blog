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

1. Select the topmost entry in the Project Navigator.
1. Select the main target.
1. Click the "+" button at the bottom of the left nav to create a new target.
1. Enter "test" to filter the set of templates displayed.
1. Select "Unit Testing Bundle".
1. Click the "Next" button.
1. Click the "Finish" button.

   This creates a new folder that appears in the Project Navigator
   whose name is the project name followed by "Tests".
   The new folder contains a `.swift` file with the same name
   containing starter test code.

1. Edit the provided `.swift` files in the new target.

## UI Test Configuration

1. Select the topmost entry in the Project Navigator.
1. Click the "+" button at the bottom of the left nav to create another target.
1. Enter "test" to filter the set of templates displayed.
1. Select "UI Testing Bundle".
1. Click the "Next" button.
1. Click the "Finish" button.

   This creates a new folder that appears in the Project Navigator
   whose name is the project name followed by "UITests".
   The new folder contains a `.swift` file with the same name
   containing starter test code.

1. Edit the provided `.swift` files in the new target.

## Implementing Tests

For each set of related test methods:

1. Create a test file or use the provided example test file.

   - Create a file in the appropriate directory
     with a name that ends in `Tests.swift`.

     1. Select File ... New ... File... or press cmd-n.
     1. For non-UI tests, select "Unit Test Case Class"
        or for UI tests, select "UI Test Case Class".
     1. Enter a name that ends in "Tests".
     1. Click the "Next" button.
     1. Select the appropriate test directory.
     1. Click the "Create" button.

        The generated class will inherit from {% aTargetBlank
        "https://developer.apple.com/documentation/xctest/xctestcase",
        "XCTestCase" %}.

     1. Add the line `@testable import {project-name}` before the class definition.

        Replace hyphens in the project name, if any, with underscores.

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

## Example Unit Test

The following code defines a `struct` with a simple `static` function:

```swift
import Foundation

struct Math {
    static func add(n1: Double, n2: Double) -> Double {
        n1 + n2
    }
}
```

The following code defines unit tests for the `struct` defined above:

```swift
import XCTest

@testable import XCTestDemo
final class MathTests: XCTestCase {
    override func setUpWithError() throws {
        // Put setup code here. This method is called before
        // the invocation of each test method in the class.
    }

    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after
        // the invocation of each test method in the class.
    }

    func testAdd() throws {
        let actual = Math.add(n1: 1, n2: 2)
        let expected = 3.0
        XCTAssertEqual(actual, expected)
    }
}
```

## Finding and Testing Views

UI test cases create an instance of {% aTargetBlank
"https://developer.apple.com/documentation/xctest/xcuiapplication",
"XCUIApplication" %} and typically store it in a variable named `app`.

A call to `app.launch()` is required to launch the app.

`XCUIApplication` inherits from {% aTargetBlank
"https://developer.apple.com/documentation/xctest/xcuielement",
"XCUIElement" %}.
This enables finding all the `View` elements within the application.

The following table highlights properties in the `XCUIElement` class.
Some of these are only available when running on a specific platform.
For example, window elements are available in macOS, but not in iOS.

Some of these properties are defined by the following protocols
to which `XCUIElement` conforms: {% aTargetBlank
"https://developer.apple.com/documentation/xctest/xcuielementattributes",
"XCUIElementAttributes" %} and {% aTargetBlank
"https://developer.apple.com/documentation/xctest/xcuielementtypequeryprovider",
"XCUIElementTypeQueryProvider" %}.

| Property                               | Description                                                                                                                              |
| -------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `alerts: XCUIElementQuery`             | query that matches alert elements                                                                                                        |
| `buttons: XCUIElementQuery`            | query that matches button elements                                                                                                       |
| `checkBoxes: XCUIElementQuery`         | query that matches checkbox elements                                                                                                     |
| `comboBoxes: XCUIElementQuery`         | query that matches combo box elements                                                                                                    |
| `datePickers: XCUIElementQuery`        | query that matches date picker elements                                                                                                  |
| `debugDescription: String`             | debugging information                                                                                                                    |
| `dialogs: XCUIElementQuery`            | query that matches dialog elements                                                                                                       |
| `elementType: XCUIElement.ElementType` | enum value from {% aTargetBlank "https://developer.apple.com/documentation/xctest/xcuielement/elementtype", "XCUIElement.ElementType" %} |
| `exists: Bool`                         | indicates if the element exists on the screen                                                                                            |
| `firstMatch: XCUIElement`              | first element that matches                                                                                                               |
| `hasFocus: Bool`                       | indicates whether the element has focus                                                                                                  |
| `identifier: String`                   | associated with the element using the `.accessibilityIdentifier("id")` view modifier                                                     |
| `images: XCUIElementQuery`             | query that matches image elements                                                                                                        |
| `isEnabled: Bool`                      | indicates whether the element is enabled                                                                                                 |
| `isHittable: Bool`                     | indicates if a hit point can be determined (visible?)                                                                                    |
| `isSelected: Bool`                     | indicates whether the element is selected                                                                                                |
| `label: String`                        | label value of the element                                                                                                               |
| `links: XCUIElementQuery`              | query that matches link elements                                                                                                         |
| `maps: XCUIElementQuery`               | query that matches map elements                                                                                                          |
| `menuBarItems: XCUIElementQuery`       | query that matches menu bar item elements                                                                                                |
| `menuBars: XCUIElementQuery`           | query that matches menu bar elements                                                                                                     |
| `menuItems: XCUIElementQuery`          | query that matches menu item elements                                                                                                    |
| `menus: XCUIElementQuery`              | query that matches menu elements                                                                                                         |
| `navigationBars: XCUIElementQuery`     | query that matches navigation bar elements                                                                                               |
| `pickers: XCUIElementQuery`            | query that matches picker elements                                                                                                       |
| `placeholderValue: String?`            | placeholder value of the element (ex. in a `TextField`)                                                                                  |
| `progressIndicators: XCUIElementQuery` | query that matches progress indicator elements                                                                                           |
| `radioButtons: XCUIElementQuery`       | query that matches radio button elements                                                                                                 |
| `radioGroups: XCUIElementQuery`        | query that matches radio group elements                                                                                                  |
| `scrollBars: XCUIElementQuery`         | query that matches scroll bar elements                                                                                                   |
| `scrollViews: XCUIElementQuery`        | query that matches scroll view elements                                                                                                  |
| `secureTextFields: XCUIElementQuery`   | query that matches secure text field elements                                                                                            |
| `sheets: XCUIElementQuery`             | query that matches sheet elements                                                                                                        |
| `sliders: XCUIElementQuery`            | query that matches slider elements                                                                                                       |
| `staticTexts: XCUIElementQuery`        | query that matches label elements                                                                                                        |
| `switches: XCUIElementQuery`           | query that matches switch elements                                                                                                       |
| `textFields: XCUIElementQuery`         | query that matches text field elements                                                                                                   |
| `textViews: XCUIElementQuery`          | query that matches text elements                                                                                                         |
| `title: String`                        | title value of the element                                                                                                               |
| `toggles: XCUIElementQuery`            | query that matches toggle elements                                                                                                       |
| `toolbarButtons: XCUIElementQuery`     | query that matches toolbar button elements                                                                                               |
| `toolbars: XCUIElementQuery`           | query that matches toolbar elements                                                                                                      |
| `value: Any?`                          | raw value of the element                                                                                                                 |
| `webViews: XCUIElementQuery`           | query that matches web view elements                                                                                                     |
| `windows: XCUIElementQuery`            | query that matches window elements                                                                                                       |

The following table highlights methods in the `XCUIElement` class.

| Method                                      | Description                                                                                      |
| ------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| `children(matching) -> XCUIElementQuery`    | gets all the child elements of a given type                                                      |
| `click()`                                   | clicks the element in a macOS app                                                                |
| `descendants(matching) -> XCUIElementQuery` | gets all the descendant elements of a given type                                                 |
| `doubleClick()`                             | double-clicks the element in a macOS app                                                         |
| `doubleTap()`                               | double-taps the element in an iOS app                                                            |
| `hover()`                                   | moves the pointer over the element in an iPad or macOS app                                       |
| `pinch(withScale, velocity)`                | pinches with two touches                                                                         |
| `press(forDuration)`                        | long-presses an element in an iOS app                                                            |
| `rightClick()`                              | right-clicks the element in a macOS app                                                          |
| `rotate(angle, withVelocity)`               | rotates with two touches                                                                         |
| `scroll(byDeltaX, deltaY)`                  | scrolls the view in an iPad or macOS app                                                         |
| `swipeDown()`                               | swipes down                                                                                      |
| `swipeLeft()`                               | swipes left                                                                                      |
| `swipeRight()`                              | swipes right                                                                                     |
| `swipeUp()`                                 | swipes up                                                                                        |
| `tap()`                                     | tap the element in an iOS app                                                                    |
| `typeText(String)`                          | types text into an element that accepts input like a `TextField`, `SecureField`, or `TextEditor` |
| `waitForExistence(timeout) -> Bool`         | waits as long as the timeout and returns a `Bool` indicating if the element exists               |

The following table highlights properties in the {% aTargetBlank
"https://developer.apple.com/documentation/xctest/xcuielementquery",
"XCUIElementQuery" %} class.

| Property                                 | Description                                                |
| ---------------------------------------- | ---------------------------------------------------------- |
| `allElementsBoundByIndex: [XCUIElement]` | array of matching elements                                 |
| `count: Int`                             | number of matching elements                                |
| `element: XCUIElement`                   | the single matching element; test fails if not exactly one |
| `firstMatch: XCUIElement`                | first matching element                                     |

The following table highlights methods in the {% aTargetBlank
"https://developer.apple.com/documentation/xctest/xcuielementquery",
"XCUIElementQuery" %} class.

| Method                                 | Description                                                                              |
| -------------------------------------- | ---------------------------------------------------------------------------------------- |
| `element(boundBy: Int) -> XCUIElement` | matching element at a given index                                                        |
| `subscript(String) -> XCUIElement`     | returns the descendant element with a given accessibility identifier using `[id]` syntax |

## UI Test Utility Methods

The following utility methods defined in an extension of the `XCTestCase` class
simplify writing tests.

```swift
import XCTest

extension XCTestCase {
    static var app = XCUIApplication()

    // Verifies that a `Button` with a given label exists.
    func buttonExists(_ label: String) throws {
        XCTAssertTrue(Self.app.buttons[label].exists)
    }

    // Enters text in a `SecureField` with a given label.
    func enterSecureText(label: String, text: String) {
        Self.app.secureTextFields[label].tap()
        for char in text {
            Self.app.keys[String(char)].tap()
        }

        /* Tests fail with this approach.
         let field = Self.app.secureTextFields[label]
         field.tap()
         field.typeText(text)
         */
    }

    // Enters text in a `TextField` with a given label.
    func enterText(label: String, text: String) {
        Self.app.textFields[label].tap()
        for char in text {
            let key = Self.app.keys[String(char)]
            key.tap()
        }

        /* Tests fail with this approach.
         let field = Self.app.textFields[label]
         field.tap()
         field.typeText(text)
         */
    }

    // Taps a `Button` with a given label.
    func tapButton(label: String) {
        Self.app.buttons[label].tap()
    }

    // Searches for text anywhere on the screen.
    func textExists(_ text: String) throws {
        XCTAssertTrue(Self.app.staticTexts[text].exists)
    }

    // Searches for text in a view with a specific `accessibilityIdentifier`.
    func textExists(identifier: String, text: String) throws {
        let actual = Self.app.staticTexts[identifier].label
        XCTAssertEqual(text, actual)
    }
}
```

## Example UI Test

The following code defines a custom `View`:

```swift
import SwiftUI

struct Counter: View {
    @State private var count = 0

    var body: some View {
        HStack {
            Button("+") { count += 1 }
            Text("\(count)")
                .accessibilityIdentifier("count")
            Button("-") { count += 1 }
        }
    }
}
```

The following code defines UI tests for the `View` defined above:

```swift
import XCTest

final class CounterTests: XCTestCase {
    override func setUpWithError() throws {
        // Put setup code here. This method is called before
        // the invocation of each test method in the class.

        // In UI tests it is usually best to
        // stop immediately when a failure occurs.
        continueAfterFailure = false

        // In UI tests it’s important to set the initial state, such as
        // interface orientation, that is required for before your tests run.
    }

    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after
        // the invocation of each test method in the class.
    }

    func testIncrement() throws {
        // UI tests must launch the application that they test.
        let app = XCUIApplication()
        app.launch()

        tapButton(label: "+")
        tapButton(label: "+")
        try textExists(identifier: "count", text: "2")
    }
}
```

In the `setUpWithError` method of test files, add the following:

```swift
XCTestCase.app.launch()
```

## Running Tests in Xcode

1. To run a single test method, click the diamond where the
   line number of the first line of the method would normally appear.
   If the diamonds don't appear, click a different source file
   and they click back on the test file.
1. To run all the test methods in the file, click the diamond where the
   line number of the first line of the class would normally appear.
1. To run all the tests in Project Navigator folder,
   click the diamond to the right of the folder name.
1. To run all the tests, select Product ... Test or press cmd-u.

In UI tests, the on-screen keyboard doesn't always appear.
If it doesn't, tests that require typing in text fields will fail.
To fix this, go the Simulator app, select I/O ... Keyboard,
and unselect "Connect Hardware Keyboard".

## Viewing Test Results

To see a report on test results:

1. Click the "Show Report Navigator" button,
   which is the last button at the top of the Project Navigator,
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
