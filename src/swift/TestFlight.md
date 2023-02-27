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

After all issues identified by testers have been resolved,
you are ready to submit the app for a full review
before it is added to the App Store.
TODO: Add documentation on the steps to submit to the App Store.

It is possible to get an app featured by Apple on the App Store.
See {% aTargetBlank "https://developer.apple.com/app-store/getting-featured/",
"Getting Featured on the App Store" %}.
In order to be featured you must tell Apple about your app
by completing the form at {% aTargetBlank
"https://developer.apple.com/contact/app-store/promote/",
"Tell us about your app or game" %}.

## Steps

### Part 1 - Creating an App Identifier

1. Browse {% aTargetBlank "https://developer.apple.com",
   "developer.apple.com" %} and sign in.
1. In the top nav bar, click "Account" and sign in.
1. In the "Certificates, Identifiers & Profiles" section, click "Identifiers".
1. To create a new identifier, click the "+" after "Identifiers".
1. Select "App IDs" radio button.
1. Click the "Continue" button.
1. Click the "App" button.
1. Click the "Continue" button.
1. Enter an app name in the "Description" input.
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
1. Select "New App" from the dropdown.
1. In the dialog that appears:
   1. Select the supported platforms (ex. iOS).
   1. Enter the app name which must be unique across all apps in the App Store.
   1. Select the primary language such as "English (U.S.)".
   1. Select the bundle ID for the app that was created in part 1.
   1. Enter a SKU with no spaces that is unique among your apps.
   1. Choose between limited (restricted set of test users)
      and full access (any user can access; preferred).
   1. Click the "Create" button.
1. If a spinner is displayed for a long time, refresh the browser.
1. The "App Store" tab is selected by default and all the fields
   on this page must be completed to deploy to the app store.
   But to just deploy to TestFlight, click the "TestFlight" tab.

### Part 3 - Creating a Build

1. Open the app in Xcode.
1. Select the top entry in the Project Navigator.
1. Select the first entry under TARGETS.
1. Click the General tab.
1. Set "Version" to "0.1.0" for the initial build
   which is a semantic version number.
   Typically this is modified each time new features or bug fixes
   are being deployed but changing this is not required.
   An advantage of only incrementing the build number
   is that a new review of the app will not be triggered.
   (It seems I had to set all the "MARKETING_VERSION" values to "1"
   it order to successfully upload a build.)
1. Set "Build" to "1". Increment this for each subsequent build.
   It does not need to match the value of "Version".
1. After "App Icon", enter "AppIcon" which
   should be the asset name of the application icon.
1. After "App Icon Source", check the "Include all app icon assets" checkbox.
1. Repeat the previous two steps for each target using matching values.
1. Verify that the file "Assets.xcassets" defines the image set "AppIcon"
   that contains icons for both iPhone and iPad (if building for both).
1. If a paired watchOS app is included, the watch app will have its own
   "Assets.xcassets" file that must contain "AppIcon" for the watch.
1. In the device list at the top, choose "Any iOS Device".
1. Select Product ... Archive.
   This can take a few minutes to complete on a fast machine.
1. In the dialog that appears, click the "Distribute App" button.
   This dialog may be on a different screen and behind other windows.
1. Select the "App Store Connect" radio button.
1. Click the "Next" button.
1. Select the "Upload" radio button.
1. Click the "Next" button.
1. In the dialog that appears, the "Manage Version and Build Number" checkbox
   can be unchecked.
   When this is checked and you have not changed the build number
   since the last archive, it automatically increments the build number.
   But is does not also increment the version number, so it isn't useful.
   If you already modified the version number and build number,
   leaving this checked has no effect.
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
1. You will be asked if the app uses encryption,
   such as sending HTTP requests using HTTPS.
   For apps that do not use encryption, the question can be suppressed
   by adding the Boolean key "App Uses Non-Exempt Encryption"
   in the target Info tab with a value of "NO".
   For apps that do use encryption,
   you will have to submit a form to the U.S. government
   which is explained in the encryption questions.
   After answering the questions displayed in a dialog,
   click the "Start Internal Testing" button.
   The status should change to "Ready to Submit"

#### Encryption

By default every time an app is submitted
there will be a prompt related to use of encryption.
A dialog will appear with the title "Export Compliance Information"
and the question "Does your app use encryption?".
To avoid the need to answer this question
every time a new version of the app is released:

- Select the top entry in the Navigator.
- Select the main target.
- Click the "Info" tab.
- Click the "+" button in any row.
- Add the key "App Uses Non-Exempt Encryption" with the value "YES" or "NO".

### Part 4 - Distributing an App

1. Return to the {% aTargetBlank "https://appstoreconnect.apple.com/",
   "App Store Connect" %} web page.
1. Login with your Apple ID.
1. Click the large "My Apps" button.
1. Click the app to be distributed.
1. Click the "TestFlight" tab.
1. If the build to be distributed has a STATUS of "Processing",
   refresh the page periodically until that changes.
1. If the build to be distributed has a STATUS of "Missing Compliance",
   click the "Manage" link and answer the questions asked.
   The status should change to "Ready to Submit".
   To avoid being prompted about this in the future,
   add the Info key "App Uses Non-Exempt Encryption" with a value of "NO".
1. Click the build number to be distributed (ex. 4).

1. If this is the first time distributing this app ...

   1. In the "App Previews and Screenshots" section,
      add at least three screenshots for each supported device size.

      These must have specific pixel dimensions.
      A simulator that provides screenshots for a 6.5" iPhone is "iPhone 11 Pro Max".
      A simulator that provides screenshots for a 5.5" iPhone is "iPhone 8 Plus".
      A simulator that provides screenshots for a 12.9" iPad is "iPad Pro 6th generation".

      To capture screenshots, run the app in the Simulator
      once for a device that has the desired size.
      Click the camera button in the upper-right or press cmd-s.
      This displays a screenshot thumbnail in the lower-right.
      Right-click the thumbnail and select "Save to Desktop".

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

1. Click the circled "+" after "Group".
1. In the dialog that appears, check at least one group name.
1. Click the "Next" button.
1. Enter a description of what testers should test.
1. Click the "Submit for Review" button.
1. Click the back link near the top of the page that says "iOS Builds".
1. Verify that the status changed to "Waiting for Review".
   Testers will receive an email about the new version in a day or two.
   The status of this build will change to "Testing".

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

Test users must install the TestFlight app on their devices
in order to download test builds of the app and provide feedback.

### Part 7 - Submit for Review

1. Return to the App Store Connect web page and the "TestFlight" tab.
1. In the table, click the build number link
   for the version that was just uploaded.
1. Address issues, if any, identified in the "STATUS" column.
1. Wait for the status to change to "Ready to Submit".
1. Click the "+" to the right of "Groups".
1. If there are no tester groups, add at least one.
1. Select at least one group to test the new app build.
1. Click the "Save" button in the upper-right? VERIFY THIS AGAIN!
1. Click the "Submit for Review" button? VERIFY THIS AGAIN!
1. Return to the previous page that lists versions of the app and note
   that the submitted version now has a status of "Waiting for Review".
1. It can take a couple of days for Apple to review the new build.

### Part 8 - Resolve App Review Issues

1. Return to the App Store Connect web page and the "App Store" tab.
1. Click the large button for the app.
   It seems that having the app in TestFlight
   is not enough to cause its icon to be displayed here.
   The app may need to be submitted to the App Store
   in order to display its icon on this web page.
1. In the left nav, click "App Review".
1. If the "REVIEW STATUS" column shows "Rejected",
   click the "Resolve" link at the end of the row.
1. Read the messages explaining why the app was rejected.
1. Address all of the issues and verify that they are no longer present
   when running the app locally.
1. In the left nav, select the first entry under "iOS App".
1. In the main area, scroll down to the "Build" section.
1. Click the "-" button to disassociate the previous build.
1. Click the "+" button after the "Build" section heading.
1. Select a new build.
1. Click the "Save" button in the upper-right.
1. Click the "Add for Review" button in the upper-right.
1. A "Confirm Submission" page will be displayed.
1. Click the "Submit to App Review" button in the upper-right.
1. The app status will change to "Waiting for Review".
1. Wait for feedback from Apple. This could take a couple of days.

### Part 9 - Instructions for Test Users

1. You receive an email with the subject
   "You've been invited to the App Store Connect".
1. View this email on your mobile device4.
1. Click the "Accept Invitation" link in the email.
1. A new page opens in mobile Safari.
1. Enter your Apple ID and password to login.

### Part 10 - Sales

To see statistics on app sales, browse
{% aTargetBlank "https://appstoreconnect.apple.com", "App Store Connect" %}
and click "Sales and Trends".

## Feedback

To see feedback from TestFlight users inside Xcode:

- Select Window ... Organizer.
- Click "Feedback".
