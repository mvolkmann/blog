---
eleventyNavigation:
  key: Fastlane
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://fastlane.tools/", "Fastlane" %} is a
command-line tool that simplifies Android and iOS mobile app deployment.
It can run tests, generate screenshots, deploy iOS apps to TestFlight,
deploy iOS apps to the App Store, and more.

Fastlane is primarily implemented in Ruby.

Fastlane workflows can be customized with actions and plugins.

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

Additonal steps:

1. Enter `sudo gem pristine ffi --version 1.12.2`
   This fails!

## Configuring

1. Enter `fastlane init`.
1. Select one of the following options:
   - Automate screenshots
   - Automate beta distribution to TestFlight
   - Automate App Store distribution
   - Manual setup
1. Follow the steps in the instructions that are printed.
1. Answer many more questions including your Apple ID and password
   and whether fastlane should upload screenshots to AppStoreConnect.

As of February 2023 the instructions are:

```text
Open your Xcode project and make sure to do the following:
1) Add a new UI Test target to your project.
   (See my XCTest blog page.)
2) Add the ./fastlane/SnapshotHelper.swift to your UI Test target
   You can move the file anywhere you want.
   (Move it to the `{project-name}UITests` directory.)
3) Call `setupSnapshot(app)` when launching your app

  let app = XCUIApplication()
  setupSnapshot(app)
  app.launch()

4) Add `snapshot("0Launch")` to wherever you want to trigger screenshots
   (It seems this should be `try Self.app.snapshot()` now.)
5) Add a new Xcode scheme for the newly created UITest target.
   (Why is this needed?)
6) Add a Check to enable the `Shared` box of the newly created scheme.
   (It is checked by default.)

More information: https://docs.fastlane.tools/getting-started/ios/screenshots/
If you want more details on how to setup automatic screenshots, check out
https://docs.fastlane.tools/getting-started/ios/screenshots/#setting-up-snapshot
```

This results in a new directory named `fastlane`.
When the option "Automate screenshots" is selected,
this directory will contain the files
`Appfile`, `Fastfile`, `Snapfile`, and `SnapshotHelper.swift`.
Add this directory to the Xcode project and to the git repository.

Edit the `fastlane/Snapfile` file and uncomment lines so it indicates
the devices and languages to use for creating screenshots.

## Running Tests

To run both unit tests and UI tests from fastlane:

1. Modify the file `fastlane/Fastfile` to contain the following:

   ```ruby
   platform :ios do
     desc "Run tests"
     lane :tests do
       run_tests(scheme: "{scheme-name}")
     end
   end
   ```

1. From the `fastlane` subdirectory enter `bundle exec fastlane tests`.

## Generating Screenshots

From the `fastlane` subdirectory enter `fastlane screenshots`.
This generates a lot of output and takes about n minutes to complete.
I see many red messages that says "Caught error... 66"!
Screenshot .png files are added to the `fastlane/screenshots` directory.
