---
eleventyNavigation:
  key: Fastlane
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://fastlane.tools/", "Fastlane" %}
is an open source platform managed by Google that automates
many tasks related to Android and iOS mobile app deployment.
These include running tests, generating screenshots,
deploying iOS apps to TestFlight,
deploying iOS apps to the App Store, and more.

Some tasks require interacting with the {% aTargetBlank
"https://developer.apple.com/account", "Apple Developer Portal" %} and
{% aTargetBlank "https://appstoreconnect.apple.com/", "App Store Connect" %}.
Fastlane provides a way to do this from the command line.

The file `Fastfile` defines "lanes" which are sequences of actions
that automate a specific task such as running tests, generating screenshots,
deploying to TestFlight, or deploying to the App Store.
These lanes can be run from the command line or by CI/CD servers.

Fastlane is primarily implemented in Ruby.

This page focuses on usage for iOS apps.

## Resources

- {% aTargetBlank "https://fastlane.tools", "Fastline home page" %}
- {% aTargetBlank
  "https://www.runway.team/blog/how-to-build-the-perfect-fastlane-pipeline-for-ios",
  "How to build the perfect fastlane pipeline for iOS" %}

## High-level Steps

- Enroll in the Apple Developer Program (currently $99/year USD).

- Register the app to be managed on the Apple Developer Portal
  and App Store Connect.

  - The fastlane action
    {% aTargetBlank "https://docs.fastlane.tools/actions/produce/", "produce" %}
    is an alias for `create_app_online`.
    It creates a new iOS app on both
    the Apple Developer Portal and App Store Connect.

- Register the certificates and devices of each developer with the project.

  - The fastlane action
    {% aTargetBlank "https://docs.fastlane.tools/actions/cert/", "cert" %}
    is an alias for `get_certificates`.
    It determines if a new signing certificate is needed. If so it:
    - creates a new private key
    - creates a new signing request
    - generates, downloads, and installs the certificate
    - imports all the generated files into the Keychain app
  - The fastlane action
    {% aTargetBlank "https://docs.fastlane.tools/actions/sigh/", "sigh" %}
    is an alias for `get_provisioning_profile`.
    It can create, renew, download, and repair provisioning profiles.
  - The fastlane action
    {% aTargetBlank "https://docs.fastlane.tools/actions/match/", "match" %}
    is an alias for `sync_code_signing`.
    It creates all required certificates and provisioning profiles
    and stores them in a private git repository (or another supported location)
    so a team of developers can share them.
    The `match` action combines the functionality of `cert` and `sigh`.
    It is recommended to use `match` in place of `cert` and `sigh`
    unless the fine-grained control those provide is needed.
    Perhaps `match` should only be used
    when a project has more than one developer.

- Register each of the capabilities (ex. CloudKit or MapKit) used by the project
  with the Developer Portal and update the app entitlements.
  This is done manually in Xcode.
- Test the app.

  - Create distribution profiles for testing beta versions of the app
    in a service like TestFlight.
  - Create and code sign an IPA file.
  - Upload the IPA file to a test service like TestFlight.
  - Register beta testers in services like TestFlight.

  - To build the app, see the fastlane tool
    {% aTargetBlank "https://docs.fastlane.tools/actions/gym/", "gym" %}
    which is an alias for `build_app`.
    It builds and packages an app, creating a signed `.ipa` or `.app` file.

  - To run automated tests, see the fastlane tool
    {% aTargetBlank "https://docs.fastlane.tools/actions/scan/", "scan" %}
    which is an alias for `run_tests`.
    It runs all the automated tests in a project
    in a Simulator or on a connected device.

  - To deploy the app to TestFlight, see the fastlane tool
    {% aTargetBlank "https://docs.fastlane.tools/actions/pilot/", "pilot" %}
    which is an alias for `upload_to_testflight`.
    It can upload a build to TestFlight,
    add or remove testers,
    get information about testers and devices,
    and import or export data describing all the testers.

- Create screenshots for each screen in the app,
  repeating for each supported device size and supported language.

  - To capture localized screenshots, see the fastlane tool
    {% aTargetBlank "https://docs.fastlane.tools/actions/snapshot/", "snapshot" %}
    which is an alias for `capture_ios_screenshots`.
    It automates generating screenshots for each screen
    navigated to in a UI Test.
    It repeats this for each supported device size and language.

  - To copy screenshots to the App Store, see the fastlane tool
    {% aTargetBlank "https://docs.fastlane.tools/actions/deliver/", "deliver" %}
    is an alias for `upload_to_app_store`.
    It uploads screenshots, metadata, and binaries to App Store Connect.
    It can also submit an app for review.

  - To add device frames around screenshots, see the fastlane tool "frameit"
    {% aTargetBlank "https://docs.fastlane.tools/actions/frameit/", "frameit" %}
    is an alias for `frame_screenshots`.
    It adds a device frame around screenshots.

- Submit the tested app to the App Store.
  The {% aTargetBlank "https://docs.fastlane.tools/actions/deliver/", "deliver" %}
  action described above can do this.

Also consider using these actions:

- The {% aTargetBlank "https://docs.fastlane.tools/actions/slather/", "slather" %}
  action generates a code coverage report.
- The {% aTargetBlank "https://docs.fastlane.tools/actions/swiftlint/", "swiftlint" %}
  action performs code validation using SwiftLint.

## Provisioning Profiles and Code Signing

For details on "code signing" and "provisioning profiles",
see the Apple Tech Note {% aTargetBlank
"https://developer.apple.com/documentation/technotes/tn3125-inside-code-signing-provisioning-profiles",
"TN3125: Inside Code Signing: Provisioning Profiles" %}.
This document says the following:

> Apple platforms, except macOS, won't run arbitrary third-party code.
> All execution of third-party code must be authorized by Apple.
> This authorization comes in the form of a provisioning profile,
> which ties together five criteria:
>
> - Who is allowed to sign code? (which developers)
> - What apps are they allowed to sign? (typically a single app id,
>   but can use a wildcard to match multiple apps)
> - Where can those apps run? (platforms such as iOS)
> - When can those apps run? (expiration date
>   after which the profile becomes invalid)
> - How can those apps be entitled? (what apps are entitled to do)
>
> The exact format of provisioning profiles isn't documented
> and could change at any time.

Provisioning profiles include signing certificates, device identifiers,
and a bundle ID. They are cryptographically signed.

TODO: Does each developer have their own signing certificate?
TODO: Can a developer have multiple signing certificates?
TODO: Are these used to sign a provisioning profile so it is known who created the provisioning profile?

## Installing

### Ruby

There are often issues with using the version of Ruby that comes with macOS.
To avoid these issues, install a new version of Ruby using Homebrew:

1. Enter `brew install ruby`.
1. Modify your shell configuration file (such as `.zshrc`)
   to add `/opt/homebrew/opt/ruby/bin` to
   the beginning of the `PATH` environment variable value.
1. Start a new shell session.
1. Verify by entering `which ruby`.
1. Enter `gem install bundler`.

### Fastlane

1. Install fastlane by entering `brew install fastlane`.
1. If git is not already installed, enter `brew install git`.
1. If the Xcode Command Line Tools are not already installed,
   enter `xcode-select -install`.

## Configuring

1. Add the following environment variables in your shell configuration file
   such as `.zshrc`:

   ```bash
   export LANG=en_US.UTF-8
   export LC_ALL=en_US.UTF-8
   ```

1. Open a Terminal window and cd to a root project directory.
1. Enter `fastlane init`.
1. Select one of the following options:
   - Automate screenshots
   - Automate beta distribution to TestFlight
   - Automate App Store distribution
   - Manual setup (for implementing multiple lanes)
1. Answer many more questions including your Apple ID and password.
1. This results in a new directory named `fastlane`.
   When the option "Automate screenshots" is selected,
   this directory will contain the files
   `Appfile`, `Fastfile`, `Snapfile`, and `SnapshotHelper.swift`.
1. Add the `fastlane` directory to the Xcode project and to the git repository.

### Appfile

The file `Appfile` contains the application bundle identifier and your Apple ID.

1. Edit the file `fastlane/Appfile`.
1. Uncomment the line containing `app_identifier(`.
1. Uncomment the line containing `apple_id(`.

### Authentication

Many actions require authenticating against your App Store Connect account.
To configure this:

1. Browse {% aTargetBlank "https://appstoreconnect.apple.com",
   "App Store Connect" %}.
1. Login
1. Click the "Users and Access" button.
1. Select the "Keys" tab.
1. Click the "Request Access" button.
1. Check the checkbox to agree to terms.
1. Click the "Submit" button.
1. Click the "Generate API Key" button.
1. Enter a key name such as "fastlane key".
1. Select an access role such as "App Manager".
1. Click the "Generate" button.
1. Click "Download API Key".
1. Click the "Download" button.
1. Move the downloaded `.p8` file to the project `fastlane` directory.

To get base64 encoded key content enter `cat {key-name}.p8 | base64`.

In the file `Fastfile` add the following
before any action that requires authentication:

```ruby
app_store_connect_api_key(
  key_id: "{key-id}", # from downloaded key file name
  issuer_id: "{issuer-id}", # How can this be determined?
  key_content: "{base64-encoded-key}", # from base64 command above
  is_key_content_base64: true,
  in_house: false # indicates whether the team is Enterprise
)
```

Alternatively, set the following environment variables
and just use `app_store_connect_api_key()`:

- key_id: APP_STORE_CONNECT_API_KEY_KEY_ID
- issuer_id: APP_STORE_CONNECT_API_KEY_ISSUER_ID
- key_content: APP_STORE_CONNECT_API_KEY_KEY
- in_house: APP_STORE_CONNECT_API_KEY_IN_HOUSE

Calling `app_store_connect_api_key()` with or without arguments
sets a shared variable that can be used to get the API key in any lane.
For example:

```ruby
api_key = lane_context[SharedValues::APP_STORE_CONNECT_API_KEY]
# Pass api_key to any action.
```

### Appfile

This is a Ruby source file that defines values used in `Fastfile`.
It can contain code like the following:

```ruby
app_identifier "{app-bundle-identifier}"
apple_id "{my-apple-id}"

itc_team_id "{app-store-connect-team-id}" # 9-digit number
team_id "{developer-portal-team-id}" # 10 characters
```

For more information about the Appfile, see the fastline docs on
{% aTargetBlank "https://docs.fastlane.tools/advanced/#appfile", "Appfile" %}.

### Fastfile

By default `Fastfile` contains code written in the Ruby programming language.
There is a option to use code written in the Swift programming language,
but that executes more slowly because it still interacts with Ruby.

1. Edit the file `fastlane/Fastfile`.

1. Change the the contents to the following:

   ```ruby
   default_platform(:ios)

   platform :ios do
     lane :certs do
       cert(development: true)
       sigh(development: true)
     end

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

1. Click the "Close" button.

A lane can be specific to a given platform (ex. ios or mac)
or it can be platform independent.

To list the lanes implemented for a given project, enter `fastlane lanes`.

The `fastlane` command can be passed the name of an action or a lane to run.
To execute a lane, enter `bundle exec fastlane {platform} {action-or-lane}`.
For example, `bundle exec fastlane ios screenshots`.

### Snapfile

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
     # These are five most used languages in the world
     # in order from most to least used
     # including the region in which they are most used.
     "en-US", # English - USA
     "zh-CN", # Chinese Simplified - China
     "hi-IN", # Hindi - India
     "es-ES", # Spanish - Spain
     "fr-FR" # French - France
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

### Configuring Screenshot Generation

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
   This generates a lot of output and takes several minutes to complete.
1. The produced screenshot `.png` files will be
   in `fastlane/screenshots` directory.
1. An HTML file that displays all the screenshots
   will open in your default web browser.
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

## Code Signing

These steps will generate and install all required
keys, certificates, and provisioning profiles.

1. From the project root directory, enter `fastlane match init`
1. For the storage mode, select "1. git".
1. Paste the URL of the project GitHub repository.
1. Enter `fastlane match development`
1. Enter a passphrase that you must remember.
1. Enter the password for accessing your login keychain.
1. Enter `fastlane match appstore`
1. Browse {% aTargetBlank "https://developer.apple.com/account", "developer.apple.com" %}.
1. Click "Account" and login.
1. Under "Certificates, Identifiers & Profiles" click "Profiles".
1. Verify that the list contains a profile whose name begins with
   "match Development" and another that begins with "match "AppStore".
1. In order to allow fastlane to manage code signing
   you must disable automatic code signing in the Xcode project.
   1. Open the project in Xcode.
   1. Select the top entry in the Project Navigator.
   1. Select the main target.
   1. Select the "Signing & Capabilities" tab.
   1. Uncheck the checkbox for "Automatically manage signing".
   1. Change the value in the "Provisioning Profile" dropdown
      to the one that begins with "match AppStore".
1. Edit `fastlane/Fastfile`.
1. Add the following lane definition to synchronize certificates:

   ```ruby
   desc "Sync certificates"
   lane :sync_certificates do
     # read-only disables overriding existing certificates.
     match({readonly: true, type: "appstore"})
   end
   ```

## Building

These steps will enable Fastline to build the app.

1. From the project root directory, enter `fastlane gym init`
   to create the file `fastlane/Gymfile`.
1. Edit the file `fastlane/Gymfile`.
1. Replace the contents of the file with the following:

   ```ruby
   scheme("{main-scheme-name}")

   # Provide provisioning profiles to use.
   export_options({
     method: "app-store",
     provisioningProfiles: {
       "{bundle-identifier}" => "match AppStore {bundle-identifier}",
     }
   })

   # Specify the path to store .ipa file.
   output_directory("./fastlane/builds")

   include_bitcode(false)
   include_symbols(false)
   ```

1. Open the project in Xcode.
1. Select the top entry in the Project Navigator.
1. Select the main target.
1. Select the "Build Settings" tab.
1. Scroll down to the "Versioning" section.
1. Verify that "Versioning System" is set to "Apple Generic" (default).
1. Edit `fastlane/Fastfile`.
1. Add the following lane definition to synchronize certificates:

   ```ruby
   desc "Create ipa"
   lane :build do
     sync_profiles
     increment_build_number
     gym # creates a signed file
   end
   ```

1. From the project root directory, enter `bundle exec fastlane build`.

## Deploying to TestFlight

TODO: Finish this.

## Deploying to the App Store

TODO: Finish this.
