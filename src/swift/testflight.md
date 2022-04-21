---
eleventyNavigation:
  key: TestFlight
  parent: Swift
  order: 4
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/testflight/", "TestFlight" %}
enables deploying test versions of an iOS app
that does not require a full app review.
New versions can be deployed without a followup review.
Each version remains available for download for 90 days.

## Steps

### Part 1 - Creating an Identifier

1. Browse {% aTargetBlank "https://developer.apple.com",
   "developer.apple.com" %} and sign in.
1. In the top nav bar, click "Account".
1. Click the large "Certificates, Identifiers & Profiles" button.
1. In the left nav, click "Identifiers".
1. To create a new identifier, click the "+" after "Identifiers".
1. Select "App IDs".
1. Click the "Continue" button.
1. Click the "App" button.
1. Click the "Continue" button.
1. Enter an app description.
1. Open the app in Xcode.
1. Select the top entry in the Navigator.
1. Select the first entry under TARGETS.
1. Click the General tab.
1. Copy the app bundle identifier from Xcode
   and paste it in the "Bundle ID" field.
1. Check the desired "Capabilities" such as "HealthKit".
1. Click the "Continue" button.
1. Click the "Register" button.

### Part 2 - Creating an App

1. Browse {% aTargetBlank "https://appstoreconnect.apple.com",
   "appstoreconnect.apple.com" %}.
1. Click the large "My Apps" button.
1. Click the "+" button after "Apps".
1. Select "New App".
1. Check the platforms the app supports such as "iOS".
1. Enter a name for the app that is unique across all apps in the app store.
1. Select a language such as "English (U.S.)".
1. Select the bundle ID for the app that was created in part 1.
1. Enter a SKU with no spaces that is unique among your apps.
1. Choose between limited (restricted set of test users)
   and full access (any user can access).
   "Limited Access" is disabled. Why?
1. Click the "Create" button.
1. If a spinner is displayed for a long time, refresh the browser.
1. The "App Store" tab is selected by default and all the fields
   on this page must be completed to deploy to the app store,
   but to just deploy to TestFlight, click the "TestFlight" tab.

### Part 3 - Creating a Build

- Open the app in Xcode.

1. Select the top entry in the Navigator.
1. Select the first entry under TARGETS.
1. Click the General tab.
1. Set "Version" to "0.1.0" for the initial build
   which is a semantic version number.
   Modify this each time new bug fixes or features are being deployed.
1. Set "Build" to "1". Increment this for each
   subsequent build with the same value for "Version".
1. Verify that the file "Assets.xcassets" defines the image set "AppIcons"
   that contains icons for both iPhone and iPad.
1. In the device list, choose "Any iOS Device".
1. Select Product ... Archive.
   This can take a few minutes to complete.
1. Click the "Distribute App" button.
1. Select the "App Store Connect" radio button.
1. Click the "Next" button.
1. Select the "Upload" radio button.
1. Click the "Next" button.
1. In the dialog that appears, uncheck "Manage Version and Build Number".
1. Click the "Next" button.
1. Select the "Automatically manage signing" radio button.
1. Click the "Next" button.
1. If an "Apple Distribution certificate" has not yet been created:
   - Check "Generate an Apple Distribution certificate".
   - Press the "Next" button.
   - Press the "Export Signing Certificate..." button
     and save it somewhere on your hard drive.
   - Click the "Next" button.
1. Click the "Upload" button.
1. When processing completes, if you see the message "Missing Compliance",
   click the "Manage" link after the message and answer some encryption questions.
   If sending HTTP requests using HTTPS, you will have to submit a form
   to the U.S. government which is explained in the encryption questions.

### Part 4 - Distribute App

1. Return to the App Store Connect web page and the TestFlight tab.
1. Click the "Create Group" link to add testers.
1. Enter a group name.
1. Check the "Enable automatic distribution" checkbox.
1. Click the "Create" button.
1. Click "Test Information" under "General Information" in the left nav.
1. Complete the ENTIRE form.
1. If the app does not require sign in, check and uncheck that checkbox.
   Otherwise the form will not be considered complete.
1. Click the "Save" button in the upper-right.

### This part did not work! Skip to the next section.

1. Click the "+" after "Testers".
1. Click the "Users and Access" link which opens a new browser tab.
1. Click "Testers" under "Sandbox" in the left nav.
1. For each tester to be added to the group:
   - Click the "+" after "Sandbox Testers".
   - Enter first name, last name, and email.
   - Enter a password and confirm value ("testthis").
   - Enter a secret question and answer ("question" and "answer").
   - Select the birthday and country of the test user.
   - Check the checkboxes for the appropriate roles.
   - Select apps to expose from the Apps dropdown.
   - Click the "Invite" button.

### Part 5 - Add External Testing Users

1. Click the "+" after "External Testing" in the left nav.
1. Enter a group name.
1. Click the "Create" button.
1. Under "External Testing" in the left nav, click the new group name.
1. For each external testing user:

   - Click the "+" after "Tester".
   - Select "Add New Testers".
   - Enter their email, first name, and last name.
   - Click the "Add" button.

1. Click the "+" after "Build" and select a build to be tested.
1. Does something else need to be clicked to save all of this?
1. The status of the selected build should change to "Waiting for Review".
1. Wait for an approval email from Apple,
   after which the testers should receive an email containing
   a link that will allow them to download the app from TestFlight.
1. If a build of the app is rejected, it will display a status of "Rejected"
   on the "App Store Connect" web page.
   To see the reasons for the rejection:

   - Click the "App Store" link in the top nav.
   - Click "App Review" in the left nav.
   - Click the "Resolve" link on the far right.

### Part 6 - Instructions for test users

1. You receive an email with the subject
   "You've been invited to the App Store Connect".
1. View this email on your mobile device4.
1. Click the "Accept Invitation" link in the email.
1. A new page opens in mobile Safari.
1. Enter your Apple ID and password to login.
