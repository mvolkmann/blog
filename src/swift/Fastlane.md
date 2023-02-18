---
eleventyNavigation:
  key: Fastlane
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://fastlane.tools/", "Fastlane" %}
is an open source platform that automates many tasks
related to Android and iOS mobile app deployment.
This includes running tests, generating screenshots,
deploying iOS apps to TestFlight,
deploying iOS apps to the App Store, and more.
Fastlane actions can be run from the command line or by CI/CD servers.

Fastlane is primarily implemented in Ruby.

Fastlane workflows can be customized with actions and plugins.

The file `Fastfile` defines "lanes" which each automate a specific task
such as running tests, generating screenshots, deploying to TestFlight,
or deploying to the App Store.

This page focuses on usage for iOS apps.

## Installing

Option #1 - using homebrew

Enter `brew install fastlane`

Option #2 - manual

1. Verify that Ruby 2.5 or newer is installed by entering `ruby --version`.
1. Install Bundler by entering `sudo gem install bundler`.
1. Create the file `Gemfile` in the root directory of a project
   containing the following:

   ```ruby
   source "https://rubygems.org"
   gem "fastlane"
   ```

1. Enter `sudo gem update --system 3.2.3`
1. Enter `sudo bundle update`
1. Enter `git add Gemfile Gemfile.lock`
1. Enter `git commit`

Additional steps:

1. Enter `sudo gem pristine ffi --version 1.12.2`
   This fails!

## Configuring

1. Open a Terminal window and cd to the root project directory.
1. Enter `fastlane init`.
1. Select one of the following options:
   - Automate screenshots
   - Automate beta distribution to TestFlight
   - Automate App Store distribution
   - Manual setup
1. Answer many more questions including your Apple ID and password
   and whether fastlane should upload screenshots to AppStoreConnect.
1. This results in a new directory named `fastlane`.
   When the option "Automate screenshots" is selected,
   this directory will contain the files
   `Appfile`, `Fastfile`, `Snapfile`, and `SnapshotHelper.swift`.
   Add the `fastlane` directory to the Xcode project and to the git repository.
1. Edit the `fastlane/Snapfile` file.
1. Uncomment lines so it indicates the devices and languages
   to use for creating screenshots. For example:

   ```ruby
   devices([
     "iPhone 8 Plus",
     "iPhone 13 Pro Max",
     "iPad Pro (12.9-inch) (2nd generation)",
     "iPad Pro (12.9-inch) (6th generation)"
   ])

   languages([
     "en-US", # English USA
     "fr-FR", # French France
     "es-ES" # Spanish Spain
   ])
   ```

1. Uncomment the line that calls the `scheme` function
   and change it to `scheme("ScreenshotTests")`.

1. Uncomment the line `output_directory("./screenshots")`
   and change the path to `./fastlane/screenshots`.

1. Uncomment the line `clear_previous_screenshots(true)`.
   This deletes all the `.png` files in the `fastlane/screenshots` directory.
   Maybe this isn't always desirable!

1. Uncomment the line `override_status_bar(true)`.

1. Add the line `headless(false)`.
   Tests that need to wait for elements to appear seem to fail without this.

1. Edit the file `fastlane/Fastfile`.

1. Change the the contents to the following:

   ```ruby
   default_platform(:ios)

   platform :ios do
     desc "Generate localized screenshots"
     lane :screenshots do
       capture_screenshots(scheme: "ScreenshotTests") # main change
       # The "update" step is optional.
       # upload_to_app_store(skip_binary_upload: true, skip_metadata: true)
     end
   end
   ```

1. Follow the steps in the instructions that are printed which guide you to:

   - Open the project in Xcode.
   - Create a new UI Test target named "ScreenshotTests" that is specifically
     for creating screenshots as described in my XCTest blog page.
     This should be separate from the target that runs the real UI tests.
   - Delete the file `ScreenshotTests/ScreenshotTestsLaunchTests.swift`.
     This isn't needed for capturing screenshots.
   - Move the `fastlane/SnapshotHelper.swift` into the new target directory.
   - Edit the file `ScreenshotTests/ScreenshotTests.swift`.
   - In the `setupWithError` method, add the following:
     ```swift
     let app = XCUIApplication()
     setupSnapshot(app)
     app.launch()
     ```
   - Rename the test method `testExample` to `testScreenshots`.
   - Replace the code in this method with code that
     visits each screen in the app.
   - After the code that visiting each screen,
     call `snapshot("{screenshot-file-name}")`.
     The actual file name will begin with the device name (ex. "iPhone 14-")
     and end with ".png".
     This works in simulators, but does nothing when running on a real device.
   - Delete the method definition for `testLaunchPerformance`.

1. Click the scheme dropdown at the top and select "New Scheme...".
1. Enter "ScreenshotTests" for the name
1. Click the "OK" button.
1. Click the scheme dropdown at the top again and select "Edit Scheme...".
1. In the dialog that appears, verify that
   the "Shared" checkbox at the bottom is checked.
1. Select "Test" in the left nav.
1. Click "+" at the bottom and add the "ScreenshotTests" target.
1. In the dialog that appears, select the "ScreenshotTests" target
   and click the "Add" button.

1. Click the "Close" button.

For more information, see {% aTargetBlank
"https://docs.fastlane.tools/getting-started/ios/screenshots/",
"fastlane screenshots" %}.

## Generating Screenshots

1. Verify that all the Simulators to be used are in the expected
   light/dark mode. Many seem to default to dark mode.
   - Open Xcode.
   - Select Xcode ... Open Developer Tool ... Simulator.
   - For each device
     - In the Simulator app, select
       File ... Open Simulator ... iOS {version} ... {device-type}.
     - In the device simulator
       - Open the Settings app.
       - Select "Developer".
       - Toggle "Dark Appearance" to the desired setting.
1. From the `fastlane` subdirectory enter `bundle exec fastlane screenshots`.
   This runs faster than just entering `fastlane screenshots`.
   This generates a lot of output and takes several minutes to complete.
1. The produced screenshot `.png` files will be
   in `fastlane/screenshots` directory.
1. An HTML file that displays all the screenshots
   will open in your default web browser.
   For me this page does not display any of the screenshots
   even though they are created in the `fastline/screenshots` directory.
   To skip this, add the following in `fastlane/Snapfile`:

   ```ruby
   skip_open_summary(true)
   ```

## Running Tests

Tests run much faster in Xcode than they do from fastlane.

To run both unit tests and UI tests from fastlane:

1. Modify the file `fastlane/Fastfile` to contain the following:

   ```ruby
   platform :ios do
     desc "Run tests"
     lane :tests do
       run_tests(scheme: "{main-scheme-name}")
     end
   end
   ```

1. From the `fastlane` subdirectory enter `bundle exec fastlane tests`.

TODO: Can you skip editing `Fastfile` and run the tests with `fastlane scan`?
