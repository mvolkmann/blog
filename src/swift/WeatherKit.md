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
1. Under "Capabilities", check the checkbox for "WeatherKit".
1. Click the "Continue" button.
1. In the left nav, click "Services".
1. Under "WeatherKit", click "View".
1. Click the "Continue" button.
1. Click the "Register" button.
1. In Xcode, click the top entry in the Navigator.
1. Select the target that will use WeatherKit.
1. Click the "Signing & Capabilities" tab.
1. Click the "+" in the upper-left.
1. Find WeatherKit and double-click it.
