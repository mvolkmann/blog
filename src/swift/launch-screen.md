---
eleventyNavigation:
  key: Launch Screens
  parent: Swift
layout: topic-layout.njk
---

## Overview

Apps can specify a launch screen to display while the app is starting.
It is recommended to use an image and no text because
localization is not available for launch screens.
The image is often the app icon or a representation of the initial screen.

Typically the launch screen is only displayed for one second or less.

## Steps to Add

To add a launch screen to an iOS app:

1. Add an image set for the launch screen image in `Assets.xcassets`.
1. In the Inspector the image set, change the value
   for "Scales" from "Individual Scales" to "Single Scale".
1. Select the top entry in the Navigator.
1. Select the app target.
1. Select the "Info" tab.
1. Expand the "Launch Screen" key whose value is a Dictionary.
1. Click the "+" button next to "UIImageScreen" to add a new key.
1. Enter "Image Name" for the key.
1. Enter the name of the image set that was added in `Assets.xcassets`.
1. Optionally add the key "Image respects safe area insets"
   with the value "YES".

## Background Color

If no background color is specified for the launch screen,
it will be white in light mode and black in dark mode.

To specify a background color:

1. Add a color set in `Assets.xcassets`.
   This can specify different colors for light (Any) and dark modes.
2. Add a "Background color" key in the "Launch Screen" dictionary
   whose value is the color set name.
   The image will not automatically scale to fit the screen,
   so choose an image that is already the appropriate size.

## Simulator Details

When testing the launch screen in the Simulator,
if it fails to show the latest code changes,
select Device ... Restart to clear its cache
and run the app again.
