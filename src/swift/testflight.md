---
eleventyNavigation:
  key: TestFlight
  parent: Swift
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
   on this page must be completed to deploy to the app store.
   But to just deploy to TestFlight, click the "TestFlight" tab.

### Part 3 - Creating a Build

1. Open the app in Xcode.
1. Select the top entry in the Navigator.
1. Select the first entry under TARGETS.
1. Click the General tab.
1. Set "Version" to "0.1.0" for the initial build
   which is a semantic version number.
   Modify this each time new bug fixes or features are being deployed.
   (It seems I had to set all the "MARKETING_VERSION" values to "1"
   it order to successfully upload a build.)
1. Set "Build" to "1". Increment this for each
   subsequent build with the same value for "Version".
1. Repeat the previous two steps for each target using matching values.
1. Verify that the file "Assets.xcassets" defines the image set "AppIcon"
   that contains icons for both iPhone and iPad (if building for both).
1. If a paired watchOS app is included, the watch app will have its own
   "Assets.xcassets" file that must contain "AppIcon" for the watch.
1. In the device list at the top, choose "Any iOS Device".
1. Select Product ... Archive.
   This can take a few minutes to complete on a fast machine.
1. In the dialog that appears, click the "Distribute App" button.
1. Select the "App Store Connect" radio button.
1. Click the "Next" button.
1. Select the "Upload" radio button.
1. Click the "Next" button.
1. In the dialog that appears, uncheck "Manage Version and Build Number".
   (If you get the error "Metadata/Info.plist Mismatch" when
   uploading the archive, try again with this checkbox checked.)
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
   This can take a few minutes to complete on a fast machine.
1. When processing completes, if you see the message "Missing Compliance",
   click the "Manage" link after the message and answer some encryption questions.
   If sending HTTP requests using HTTPS, you will have to submit a form
   to the U.S. government which is explained in the encryption questions.
1. If you see a dialog with the message "App {app-name} successfully uploaded.",
   click the "Done" button.

### Part 4 - Distributing an App

1. Return to the App Store Connect web page and the "TestFlight" tab.
1. In the Build section, select an uploaded built to use.
   If it says "Processing", wait for that to change.
1. In the "App Previews and Screenshots" section,
   add at least one screenshot for each supported device size.
   These must have specific pixel dimensions.
   To capture them, run the app in a simulator once
   for a device that has the desired size.
   In the Simulator app, select File ... Save Screen
   which saves a screenshot in the Desktop directory.
   On the App Store Connect page,
   click "Prepare for Submission" in the left nav.
   Drag the screenshot files into the area under "Version Information".
1. Click the "Create Group" link in the light blue rectangle to add testers.
1. Enter a group name.
1. Check the "Enable automatic distribution" checkbox.
1. Click the "Create" button.
1. Click "Test Information" under "General Information" in the left nav.
1. Complete the ENTIRE form.
1. If the app does not require sign in, check and uncheck that checkbox.
   Otherwise the form will not be considered complete.
1. Click the "Save" button in the upper-right.

### Part 5 - App Configuration

1. Return to the App Store Connect web page and the "App Store" tab.
1. If the page for the app is not already displayed,
   click the large button containing the app name.
1. Visit each section linked in the left nav and supply any missing information.
   There are many things that must be specified, especially under "App Privacy"!
   For each section, click the "Save" button
   after entering all the required data.
1. Click "Prepare for Submission" in the left nav.
1. Click the "Add for Review" button in the upper-right.
1. If you see "Unable to Add for Review",
   supply all the missing information that is listed.

### Part 6 - Adding External Testing Users

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

### Part 7 - Submit for Review

1. Return to the App Store Connect web page and the "TestFlight" tab.
1. In the table, click the build number link
   for the version that was just uploaded.
1. Address an issues, if any, identified in the "STATUS" column.
1. Wait for the status to change to "Ready to Submit".
1. If there are no entries under "Group" (for testers), add at least one.
1. Click the "Save" button in the upper-right.
1. Click the "Submit for Review" button? VERIFY THIS AGAIN!

### Part 8 - Resolve App Review Issues

1. Return to the App Store Connect web page and the "App Store" tab.
1. Click the large button for the app.
1. In the left nav, click "App Review".
1. If the "REVIEW STATUS" column shows "Rejected",
   click the "Resolve" link at the end of the row.
1. Read the messages explaining why the app was rejected.
1. Address all of the issues and verify that they are no longer present
   when running the app locally.
1. Resubmit the app for a new review.

### Part 9 - Instructions for Test Users

1. You receive an email with the subject
   "You've been invited to the App Store Connect".
1. View this email on your mobile device4.
1. Click the "Accept Invitation" link in the email.
1. A new page opens in mobile Safari.
1. Enter your Apple ID and password to login.
