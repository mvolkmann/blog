---
eleventyNavigation:
  key: WeatherKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

See the WWDC 2022 video {% aTargetBlank
"https://developer.apple.com/videos/play/wwdc2022/10003/", "Meet WeatherKit" %}.

## Setup

1. Browse {% aTargetBlank "https://developer.apple.com",
   "developer.apple.com" %}.
1. Click "Account" and sign in.
1. Click "Certificates, Identifiers & Profiles".
1. In the left nav, click "Identifiers".
1. Click the "+" after the heading "Identifiers".
1. Select the "App IDs" radio button.
1. Click the "Continue" button.
1. Select "App".
1. Click the "Continue" button.
1. Enter an app description.
1. Paste the app bundle ID.
1. Under "Capabilities", check the checkbox for WeatherKit.
1. Click the "Continue" button.
1. Click the "App Services" tab.
1. Check the checkbox for WeatherKit.
1. Click the "Continue" button.
1. Click the "Register" button.

1. Is it also necessary to create a provisioning profile here?

   1. In the left nav, click "Profiles".
   1. Click the "+" after the heading "Profiles".
   1. Select the "iOS App Development" radio button.
   1. Click the "Continue" button.
   1. Select an app ID from the dropdown.
   1. Click the "Continue" button.
   1. Select an existing certificate.
   1. Click the "Continue" button.
   1. Select the checkboxes for the devices to be included.
   1. Click the "Continue" button.
   1. Enter a profile name (could use the app name).
   1. Click the "Generate" button.
   1. Click the "Download" button.
   1. In the Finder, open the "Downloads" directory.
   1. Double-click the downloaded `.mobileprovision` file
      to install the profile.

1. In Xcode, click the top entry in the Navigator.
1. For each target that will use WeatherKit.

   1. Select the target.
   1. Click the "Signing & Capabilities" tab.
   1. Click the "+" in the upper-left.
   1. Verify that the correct Team is selected which is listed in
      the upper-right corner of the developer.apple.com web page.
   1. Find WeatherKit and double-click it.

Wait around 30 minutes for the WeatherKit service to be enabled for your app.

I got the errors "Mescal Failed",
"Error Domain=WeatherDaemon.WDSJWTAuthenticatorService.Errors", and
"Encountered an error when fetching weather data subset"
when I ran the app.

See this {% aTargetBlank "https://developer.apple.com/forums/thread/710839",
"forum post" %}.
