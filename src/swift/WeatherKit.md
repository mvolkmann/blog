---
eleventyNavigation:
  key: WeatherKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

WeatherKit provides a variety of weather data for a given location
including condition, temperature, humidity, precipitation, wind,
and weather alerts.
Usage is free for up to 500,000 calls per month
and has a monthly cost beyond that.

WeatherKit requires iOS 16+.

See the WWDC 2022 video {% aTargetBlank
"https://developer.apple.com/videos/play/wwdc2022/10003/", "Meet WeatherKit" %}.

## Setup

1. Browse {% aTargetBlank "https://developer.apple.com",
   "developer.apple.com" %}.
1. Click "Account" and sign in.
1. Under "Certificates, Identifiers & Profiles", click "Identifiers".
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

1. In Xcode, click the top entry in the Navigator.
1. For each target that will use WeatherKit.

   1. Select the target.
   1. Click the "Signing & Capabilities" tab.
   1. Click the "+" in the upper-left.
   1. Verify that the correct Team is selected which is listed in
      the upper-right corner of the developer.apple.com web page.
   1. Find WeatherKit and double-click it.

It may be necessary to wait around 30 minutes
for the WeatherKit service to be enabled for your app.

## Sample Code

See the demo project {% aTargetBlank
"https://github.com/mvolkmann/WeatherKitDemo", "WeatherKitDemo" %} in GitHub.
Here are screenshots from this app in light and dark mode.

<img alt="WeatherKit Demo light" style="width: 40%"
  src="/blog/assets/WeatherKitDemo-light.png?v={{pkg.version}}"
  title="WeatherKit Demo light">
<img alt="WeatherKit Demo dark" style="width: 40%"
  src="/blog/assets/WeatherKitDemo-dark.png?v={{pkg.version}}"
  title="WeatherKit Demo dark">

## Limitations

Weather data cannot be retrieved in a preview or in the Simulator.
A real device must be used.
