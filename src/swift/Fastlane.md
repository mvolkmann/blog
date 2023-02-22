---
eleventyNavigation:
  key: Fastlane
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://fastlane.tools/", "Fastlane" %}
is an open source platform managed by Google that automates
many tasks related to iOS and Android mobile app deployment.
These include running tests, generating screenshots,
deploying iOS apps to TestFlight,
deploying iOS apps to the App Store, and more.

Management of the Fastlane project may be moving from Google to the
{% aTargetBlank "https://mobilenativefoundation.org",
"Mobile Native Foundation" %} soon.
This is seen as a positive move since Google has not
devoted time to Fastlane development lately.

This page focuses on usage for iOS apps.

Some tasks require interacting with the {% aTargetBlank
"https://developer.apple.com/", "Apple Developer Portal" %} and
{% aTargetBlank "https://appstoreconnect.apple.com/", "App Store Connect" %}.
Fastlane provides a way to do this from the command line.

Fastlane is primarily implemented in Ruby.

Deploying apps to TestFlight and the App Store requires enrolling
in the Apple Developer Program which is currently $99/year USD.

Many of the sections below describe how to
accomplish a specific task using a "lane".
The final section pulls it all together into a complete `Fastfile`.

## Resources

- {% aTargetBlank "https://fastlane.tools", "Fastlane home page" %}
- {% aTargetBlank
  "https://www.runway.team/blog/how-to-build-the-perfect-fastlane-pipeline-for-ios",
  "How to build the perfect fastlane pipeline for iOS" %}

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

## Installing

### Ruby

Ruby must be installed in order to use Fastlane.
There are often issues with using the version of Ruby that comes with macOS.
To avoid these issues, install a new version of Ruby using Homebrew.

1. Enter `brew install ruby`.
1. Modify your shell configuration file.

   Add `/opt/homebrew/opt/ruby/bin` to the beginning of
   the `PATH` environment variable value.
   When using zsh, the file to edit is `.zshrc` and
   the line to add is:

   ```bash
   path=("/opt/homebrew/opt/ruby/bin" $path)
   ```

1. Start a new shell session.
1. Enter `which ruby` and verify that it outputs
   `/opt/homebrew/opt/ruby/bin/ruby`.
1. Enter `gem install bundler`.

### Fastlane

1. Install fastlane by entering `brew install fastlane`.
1. If git is not already installed, enter `brew install git`.
1. If the Xcode Command Line Tools are not already installed:
   1. Enter `xcode-select --install`.
   1. In the dialog that appears, click the "Install" button.
   1. Agree to the terms.
   1. Wait about 10 minutes for the download and install to complete.

## Configuring

1. Add the following environment variables in your shell configuration file
   such as `.zshrc`:

   ```bash
   export LANG=en_US.UTF-8
   export LC_ALL=en_US.UTF-8
   ```

1. Open a Terminal window and cd to the project root directory.
1. Enter `fastlane init`.
1. This will pause several times after outputting tips.
   After each tip, press the return key to continue.
1. Select one of the following options:
   - Automate screenshots (recommended)
   - Automate beta distribution to TestFlight
   - Automate App Store distribution
   - Manual setup (for implementing multiple lanes)
1. Answer many more questions including your Apple ID and password.
   - For the UI testing scheme, choose the default scheme.
   - For "Enable automatic upload", choose "n".
1. This results in a new directory named `fastlane`.
   When the option "Manual" is selected, this directory will
   contain the files `Appfile` and `Fastfile`.
   When the option "Automate screenshots" is selected, this directory will
   also contain the files `Snapfile`, and `SnapshotHelper.swift`.
1. Add the `fastlane` directory and the files `Gemfile` and `Gemfile.lock`
   to the Xcode project.

   1. Select File ... Add Files to "{project-name}"...
   1. Select the `fastlane` directory.
   1. Click the "Add" button.

1. Add the `fastlane` directory and the files `Gemfile` and `Gemfile.lock`
   to the git repository.

### Appfile

This is a Ruby source file found in the `fastlane` directory
that defines values used in the `Fastfile` file.
It should contain the following:

```ruby
app_identifier "{app-bundle-identifier}"
apple_id "{my-apple-id}"

# The steps below describe how to obtain this value.
team_id "{developer-portal-team-id}" # 10 characters

# The steps below describe how to obtain this value.
# Omit this line until the value is known.
itc_team_id "{app-store-connect-team-id}" # 9-digit number
```

To get the Developer Portal Team ID:

1. Browse {% aTargetBlank "https://developer.apple.com/",
   "developer.apple.com" %}.
1. Click "Account" and sign in.
1. Scroll down to the "Membership Details" section.
1. Copy the "Team ID" value.
1. Paste it as the value for `team_id` in `fastlane/Appfile`.

To get the AppStoreConnect Team ID:

1. cd to the project root directory.
1. Enter `fastfile produce`.
1. You will be prompted to select a team.
   Each team name will be followed by the ITC team id in parentheses.
   Copy the desired ITC team id value.
1. Paste it as the value for `itc_team_id` in `fastlane/Appfile`.

For more information about the file `Appfile`, see the fastlane docs on
{% aTargetBlank "https://docs.fastlane.tools/advanced/#appfile", "Appfile" %}.

### Snapfile

This file is used to determine the variations of screenshots
(device types and languages) that should be created.
It is only needed if screenshots will be generated.
It is a Ruby source file found in the `fastlane` directory.

1. If the file `fastlane/Snapfile` does not exist,
   cd to the project root directory and
   enter `fastlane snapshot init` to create it

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
   Perhaps this isn't always desirable.

1. Uncomment the line `override_status_bar(true)` to set the status bar to
   Tuesday January 9th at 9:41AM with full battery and reception.

1. Add the line `headless(false)`.
   Tests that need to wait for elements to appear seem to fail without this.

## Registering an App

To register the app to be managed on the Apple Developer Portal
and App Store Connect, see [TestFlight](/blog/swift/TestFlight).

The fastlane action {% aTargetBlank
"https://docs.fastlane.tools/actions/produce/", "produce" %}
which is an alias for `create_app_online`
can be used to automate this process.
Since new apps are not created frequently,
it may be better to do this manually.

## Lanes

The file `Fastfile` defines "lanes" which are sequences of actions
that automate a specific task.
Lanes can be run from the command line or by CI/CD servers.
A lane can be specific to a given platform (ex. ios or mac)
or it can be platform independent.

The `fastlane` command can be passed the name of an action or a lane to run.
The syntax is `fastlane {platform} {action-or-lane}`.
For example, `fastlane ios screenshots`.

If a default platform is specified in `Fastfile`
using `default_platform :ios` then lanes for that platform
can be executed without providing the platform.
For example if `ios` is the default platform then the previous command
can be executed with `fastlane screenshots`.

To list the lanes implemented for a given project, enter `fastlane lanes`.

To list the lanes in a table and optionally select one to execute,
enter `fastlane`.
To execute one of the lanes, enter its number.
To exit without executing a lane, enter 0 or press ctrl-c.

## .gitignore

The following Fastlane-related files should be listed in the `.gitignore` file,
either because they contain sensitive information or
because they contain data that changes frequently
and just doesn't need to be persisted.

```text
fastlane/AuthKey_*.p8
fastlane/builds
fastlane/Preview.html
fastlane/report.xml
fastlane/screenshots/**/*.png
fastlane/test_output
fastlane/.env.default
*.cer
*.mobileprovision
```

## Running Tests

For information on creating unit and UI tests for a project,
see [XCTest](/blog/swift/XCTest).

To run all the automated unit and UI tests in the project,
use the fastlane tool {% aTargetBlank
"https://docs.fastlane.tools/actions/scan/", "scan" %}
which is an alias for `run_tests`.
The tests run in a Simulator or on a connected device.
They run much faster in Xcode than they do from fastlane.

Add the following lane in `fastlane/Fastfile`:

```ruby
platform :ios do
  desc "Run tests"
  lane :tests do
    run_tests(scheme: "{scheme-name}")
  end
end
```

From the project root directory enter `fastlane tests`.

## Creating a Signing Certificate

To create a signing certificate, use the fastlane action {% aTargetBlank
"https://docs.fastlane.tools/actions/cert/", "cert" %}
which is an alias for `get_certificates`.
This determines if a new signing certificate is needed. If so it:

- creates a new private key
- creates a new signing request
- generates, downloads, and installs the certificate
- imports all the generated files into the Keychain app

The types of certificates can be created include
`development`, `adhoc`, and `appstore`.

A `.cer` file file is created in the project root directory.
This file should be excluded from Git by adding it in `.gitignore`.

This can be combined with the next step.

To see all your certificates, browse {% aTargetBlank
"https://developer.apple.com", "developer.apple.com" %},
click "Account", sign in, and click "Certificates" under the
"Certificates, Identifiers & Profiles" section.

## Creating a Provisioning Profile

To create a provisioning profile, use the fastlane action {% aTargetBlank
"https://docs.fastlane.tools/actions/sigh/", "sigh" %}
which is an alias for `get_provisioning_profile`.
This can create, renew, download, and repair provisioning profiles.

Add the following lane in `fastlane/Fastfile`:

```ruby
desc "Creates a signing certificate and provisioning profile"
lane :certs do
  get_certificates(development: true)
  get_provisioning_profile(development: true)
end
```

From the project root directory enter `fastlane certs`.

A `.mobileprovision` file is created in the project root directory.
This file should be excluded from Git by adding it in `.gitignore`.

## Team Development

To configure Fastlane for use by a development team,
use the fastlane action {% aTargetBlank
"https://docs.fastlane.tools/actions/match/", "match" %}
which is an alias for `sync_code_signing`.
This combines the functionality of `cert` and `sigh`.
In addition, it stores the certificates and provisioning profiles
in a separate private git repository (or another supported location)
so a team of developers can share them.

It is recommended to use `match` in place of `cert` and `sigh`
when an app is being developed by more than one person.

This is the most complex fastlane action
and I have not used it yet.

## Building an App Archive

To build an app archive file (.ipa),
use the fastlane tool {% aTargetBlank
"https://docs.fastlane.tools/actions/gym/", "gym" %}
which is an alias for `build_app`.
This builds and packages an app, creating a signed `.ipa` or `.app` file.

1. Verify project build settings.

   1. Open the project in Xcode.
   1. Select the top entry in the Project Navigator.
   1. Select the main target.
   1. Select the "Build Settings" tab.
   1. Scroll down to the "Versioning" section.
   1. Set "Versioning System" to "Apple Generic".

1. Verify that the scheme to be used is "shared".

   1. Open the project in Xcode.
   1. Select the main scheme from the dropdown
      to the left of the device dropdown at the top.
   1. Click the scheme dropdown again.
   1. Select "Edit Scheme...".
   1. In the dialog that appears,
      verify that "Shared" checkbox at the bottom is checked.
      It should be checked by default.

1. From the project root directory, enter `fastlane gym init`
   to create the file `fastlane/Gymfile`.

1. Edit the file `fastlane/Gymfile` and replace the contents with the following:

   ```ruby
   scheme("{scheme-name}")
   export_options({method: "app-store"})
   output_directory("./fastlane/builds")
   ```

1. Add the following lane in `fastlane/Fastfile`:

   ```ruby
   lane :build do
     build_app
   end
   ```

1. Update the project version with the following steps:

   1. In Xcode, select the top entry in the Project Navigator.
   1. Select the main target.
   1. Select the General tab.
   1. In the "Identity" section, update the "Version" and "Build" numbers.

      The version number should be a semantic version like 1.2.3.
      The build number should be a sequential integer number like 7.

1. From the project root directory, enter `fastlane build`.

   This creates the files `{scheme-name}.app.dSYM.zip` and `{scheme-name}.ipa`
   in the `fastlane/builds` directory.

1. If you get an error message that says
   "Provisioning profile ... doesn't include signing certificate",
   launch the "Keychain Access" app, delete all certificates
   with the name that appears in the error message,
   and enter `fastlane build` again.

## Registering Beta Testers

To register beta testers in TestFlight:

1. Browse {% aTargetBlank "https://appstoreconnect.apple.com",
   "App Store Connect" %}.
1. Click the "My Apps" button.
1. Click the button for the app to be tested.
1. Click the "TestFlight" tab.
1. Create a group of testers by clicking the "+" button
   after either "Internal Testers" or "External Testers".
1. Enter a group name.
1. For each tester to be added, click the "+" after "Testers"
   and enter their email address and name.

## Deploying to TestFlight

To deploy the app to TestFlight,
use the fastlane tool {% aTargetBlank
"https://docs.fastlane.tools/actions/pilot/", "pilot" %}
which is an alias for `upload_to_testflight`.
This can upload a build to TestFlight, add or remove testers,
get information about testers and devices,
and import or export data describing all the testers.

1. Create an app-specific password.

   1. Browse {% aTargetBlank "https://appleid.apple.com/account/manage",
      "appleid.apple.com" %}.
   1. Click the "Sign In" button and sign in.
   1. Click "App-Specific Passwords".
   1. If there are no existing app-specific passwords,
      click the "Generate an app-specific password" button.
   1. If there are existing app-specific passwords,
      click the "+" after "Passwords".
   1. Enter the app name.
   1. Click the "Create" button.
   1. Confirm your Apple ID password.
   1. Copy the generated password so it can be passed in the file described next.

1. Create the file `fastlane/.env.default` with the following contents:

   ```bash
   FASTLANE_USER={apple-id}
   FASTLANE_PASSWORD={apple-password}
   FASTLANE_APPLE_APPLICATION_SPECIFIC_PASSWORD={app-specific-password}
   ```

1. Add the following lane in `fastlane/Fastfile`:

   ```ruby
   lane :beta do
   # I prefer to update the Version and Build numbers manually in Xcode.
   # increment_build_number
   # increment_version_number(bump_type: "patch")
   upload_to_testflight(
      ipa: './fastlane/builds/{ipa-name}.ipa', # in fastlane/builds
      # I prefer to submit manually on the App Store Connect web page
      # so I can enter a description of what changed in this version.
      skip_submission: true
   )
   end
   ```

1. From the project root directory, enter `fastlane beta`.
   This waits for processing to complete which takes around four minutes.

1. If the error message "The bundle version must be higher than
   the previously uploaded version" appears:

   1. See the steps in the "Building an App Archive" section
      to update the "Version" and "Build" numbers.
   1. Enter `fastlane build` to build a new a app archive.
   1. Enter `fastlane beta` again.

## Creating Screenshots

To create localized screenshots for each screen in the app,
use the fastlane tool {% aTargetBlank
"https://docs.fastlane.tools/actions/snapshot/", "snapshot" %}
which is an alias for `capture_screenshots`
which is an alias for `capture_ios_screenshots`.
This automates generating screenshots for each screen navigated to in a UI Test.
It repeats this for each supported device size and language.

The following steps assume that "Automate screenshots" was selected
when the `fastlane init` command was run.

1. Verify that `fastlane/Snapfile` was created
   as described in the "Configuring" section above.

1. Create a new target.

   1. Select the topmost entry in the Project Navigator.
   1. Click the "+" button at the bottom of the left nav to create another target
      that is separate from the main unit and UI test targets.
   1. Enter "test" to filter the set of templates displayed.
   1. Select "UI Testing Bundle".
   1. Click the "Next" button.
   1. Change the "Product Name" to "ScreenshotTests".
   1. Click the "Finish" button.

      This creates a new folder that appears in the Project Navigator
      whose name is "ScreenshotTests".
      The new folder contains a `.swift` file with the same name
      containing starter test code.

1. Delete the file `ScreenshotTests/ScreenshotTestsLaunchTests.swift`.
   This isn't needed for capturing screenshots.

1. Create a new scheme.

   1. Click the scheme dropdown at the top and select "New Scheme...".
   1. Enter "ScreenshotTests" for the name.
   1. Click the "OK" button.
   1. Click the scheme dropdown at the top again and select "Edit Scheme...".
   1. In the dialog that appears, verify that
      the "Shared" checkbox at the bottom is checked.
   1. Select "Test" in the left nav.
   1. Click "+" at the bottom.
   1. In the dialog that appears, select the "ScreenshotTests" target
      and click the "Add" button.
   1. Click the "Close" button.

1. Implement the UI test.

   1. Move the `fastlane/SnapshotHelper.swift`
      into the new `ScreenshotTests` directory.
   1. Edit the file `ScreenshotTests/ScreenshotTests.swift`.
   1. In the `setupWithError` method, add the following:

      ```swift
      let app = XCUIApplication()
      setupSnapshot(app)
      app.launch()
      ```

   1. Delete the method definition for `testLaunchPerformance`.
   1. Rename the test method `testExample` to `testScreenshots`.
   1. Replace the code in this method with code that
      visits each screen in the app.
   1. After the code that visiting each screen,
      call `snapshot("{sequence-number}-{screen-name}")`.

      The sequence numbers keep the screenshots in the intended order.
      The actual file name will begin with the device name (ex. "iPhone 14-")
      and end with ".png".

      Screenshots will only be captured when running in a simulator.
      The `snapshot` function does nothing when running on a real device.

1. Add the following lane in `fastlane/Fastfile`:

   ```ruby
   desc "Generates localized screenshots"
   lane :screenshots do
     capture_screenshots(scheme: "{screenshot-scheme-name}")
   end
   ```

1. Verify that all the Simulators to be used are in the expected
   light/dark mode. Many seem to default to dark mode.

   1. Open Xcode.
   1. Select Xcode ... Open Developer Tool ... Simulator.
   1. For each device
      - In the Simulator app, select
        File ... Open Simulator ... iOS {version} ... {device-type}.
      - In the device simulator
        - Open the Settings app.
        - Select "Developer".
        - Toggle "Dark Appearance" to the desired setting.

1. From the project root directory, enter `fastlane screenshots`.
   This generates `.png` files in the `fastlane/screenshots` directory.
   Warning messages that say "deviceType from ...
   was NULL when -platform called" can be ignored.

1. An HTML file that displays all the screenshots
   will open in your default web browser.
   To skip this, add the following to the `screenshots` lane definition:

   ```ruby
   skip_open_summary(true)
   ```

For more information, see {% aTargetBlank
"https://docs.fastlane.tools/getting-started/ios/screenshots/",
"fastlane screenshots" %}.

## Adding Device Frames to Screenshots

To add device frames around screenshots,
use the fastlane tool "frameit" {% aTargetBlank
"https://docs.fastlane.tools/actions/frameit/", "frameit" %}
which is an alias for `frame_screenshots`.

Before running this, enter `brew install imagemagick`.

This action creates new `.png` files below the `fastlane/screenshots` directory
that have `_framed` appended to their names.
The framed screenshots are beautiful, but they are all larger than
the originals and are incompatible with the sizes the App Store accepts.
See this {% aTargetBlank
"https://github.com/fastlane/fastlane/issues/21067", "issue" %}.

Add the following lane in `fastlane/Fastfile`:

```ruby
desc "Creates new screenshots from existing ones that have device frames"
lane :frames do
  frame_screenshots
end
```

From the project root directory enter `fastlane frames`.

## Uploading Screenshots

To upload the app to the App Store,
use the fastlane tool {% aTargetBlank
"https://docs.fastlane.tools/actions/deliver/", "deliver" %}
which is an alias for `upload_to_app_store`.
This can upload screenshots, metadata, and binaries to App Store Connect.
It can also update the app version number and submit the app for review.

Add the following lane in `fastlane/Fastfile`:

```ruby
  desc "Uploads localized screenshots to App Store"
  # I needed to rename the "hi-IN" directory to "hi"
  # and the "zh-CN" directory to "zh-Hans".
  lane :upload_screenshots do
    # Only uploading screenshots.
    upload_to_app_store(
      skip_app_version_update: true,
      skip_binary_upload: true,
      skip_metadata: true
    )
  end
```

From the project root directory enter `fastlane upload_screenshots`.

## Deploying to the App Store

The same fastlane action used to upload screenshots, `deliver`,
is used to upload an app to the App Store.

I prefer to submit manually on the App Store Connect web page
so I can enter a description of what changed in the new version.

The lane definition below has not worked for me yet,
but it provides a starting point.

1. Add the following lane in `fastlane/Fastfile`:

   ```ruby
   lane :prod do
     upload_to_app_store(
       ipa: './fastlane/builds/{ipa-name}.ipa', # in fastlane/builds
       skip_app_version_update: true,
       skip_metadata: true,
       skip_screenshots: true
     )
   end
   ```

1. From the project root directory, enter `fastlane prod`.

## Other Actions

All the actions supported by fastlane are listed at {% aTargetBlank
"https://docs.fastlane.tools/actions/", "fastlane actions" %}.
There are many more than were described above!

In addition to the actions already described, consider using these:

- The {% aTargetBlank "https://docs.fastlane.tools/actions/get_version_number/",
  "get_version_number" %} and {% aTargetBlank
  "https://docs.fastlane.tools/actions/get_build_number/",
  "get_build_number" %} actions return values.
  They can be combined in a lane like the following:

  ```ruby
  desc "Prints the version and build number"
  lane :version do
    version = get_version_number
    build = get_build_number
    puts "version #{version}, build #{build}"
  end
  ```

- The {% aTargetBlank "https://docs.fastlane.tools/actions/notification/",
  "notification" %} displays a macOS notification.
  The first time this is used, the System Settings app will open.
  To enable "terminal-notifier" to display notifications,
  toggle "Allow Notifications" to on and select "Alerts".

  I tried the following, but haven't gotten it to work yet.

  ```ruby
  notification(
    title: "Morning Greeting",
    subtitle: "Daily Advice",
    message: "Good morning Mark! Learn something today."
  )
  ```

- The {% aTargetBlank "https://docs.fastlane.tools/actions/puts/", "puts" %}
  prints given text to stdout.
  `echo` is an alias for this.

- The {% aTargetBlank "https://docs.fastlane.tools/actions/say/", "say" %}
  action speaks given text.
  It is useful for announcing when a lane completes.

- The {% aTargetBlank "https://docs.fastlane.tools/actions/sh/", "sh" %}
  to execute a shell command.

- The {% aTargetBlank "https://docs.fastlane.tools/actions/slather/", "slather" %}
  action generates a code coverage report.

- The {% aTargetBlank "https://docs.fastlane.tools/actions/swiftlint/", "swiftlint" %}
  action performs code validation using SwiftLint.

## Ruby vs. Swift

By default `Fastfile` contains code written in the Ruby programming language.
There is a option to use code written in the Swift programming language,
but that executes more slowly because it still interacts with Ruby.
See {% aTargetBlank
"https://docs.fastlane.tools/getting-started/ios/fastlane-swift/",
"Getting Started with Fastlane.swift" %}.

### Authentication

The information in this section may only be needed
when using the `match` action.

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

## Complete Fastfile

This is the `Fastfile` for my WeatherKitDemo a project.

```ruby
# Uncomment the line if you want fastlane to automatically update itself
# update_fastlane

default_platform :ios

platform :ios do

  desc "Runs all unit and UI tests"
  # This works despite warnings that say
  # "deviceType from ... was NULL when -platform called".
  lane :tests do
    run_tests(
      devices: ["iPhone 8 Plus", "iPhone 13 Pro Max"],
      scheme: "WeatherKitDemo"
    )
  end

  desc "Creates a signing certificate and provisioning profile"
  lane :certs do
    get_certificates(development: true)
    get_provisioning_profile(development: true)
  end

  desc "Builds the app and produces symbol and ipa files."
  lane :build do
    build_app
  end

  desc "Uploads the app to TestFlight"
  lane :beta do
    # I prefer to update the Version and Build numbers manually in Xcode.
    # increment_build_number
    # increment_version_number(bump_type: "patch")
    upload_to_testflight(
      ipa: './fastlane/builds/WeatherKitDemo.ipa',
      # I prefer to submit manually on the App Store Connect web page
      # so I can enter a description of what changed in this version.
      skip_submission: true
    )
  end

  desc "Generates localized screenshots"
  lane :screenshots do
    capture_screenshots(scheme: "ScreenshotTests")
  end

  desc "Creates new screenshots from existing ones that have device frames"
  lane :frames do
    frame_screenshots
  end

  desc "Uploads localized screenshots to App Store"
  # I needed to rename the "hi-IN" directory to "hi"
  # and the "zh-CN" directory to "zh-Hans".
  lane :upload_screenshots do
    # Only uploading screenshots.
    upload_to_app_store(
      skip_app_version_update: true,
      skip_binary_upload: true,
      skip_metadata: true
    )
  end

  # Uploads the app to the App Store
  lane :prod do
    # I prefer to submit manually on the App Store Connect web page
    # so I can enter a description of what changed in this version.
    # This has not yet worked for me!
    upload_to_app_store(
      ipa: './fastlane/builds/WeatherKitDemo.ipa', # in fastlane/builds
      skip_metadata: true,
      skip_screenshots: true
      # submit_for_review: true # defaults to false
    )
  end

end
```
