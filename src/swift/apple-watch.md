---
eleventyNavigation:
  key: Apple Watch (watchOS)
  parent: Swift
  order: 4
layout: topic-layout.njk
---

## Overview

This is a collection of tips on developing watchOS apps
using SwiftUI and Xcode.

## Simulator vs. Devices

Running on the Simulator is quite fast and is preferred for most testing.

To deploy a watchOS app to an Apple Watch:

- Attach the iPhone with which it is paired to the Mac using a USB cable.
- Select the iPhone from the device dropdown at the top of Xcode.
- Press the triangle run button.
- It may be necessary to unlock the iPhone.
- If a message says "The OS version installed ... does not support
  WatchKit App products", try ejecting the phone from the Finder
  and plugging it into a different USB port.
- When it finally works, it can take several minutes
  before the app appears on the watch.

## App Icons

To add app icons to a watchOS app:

- Open the file `Assets.xcassets`.
- Click the "+" button in the lower-left.
- Select watchOS ... watchOS App Icon.
- Drag icon images for all the sizes onto the placeholder squares.

## Launch Screen

watchOS apps do not currently support launch screens.
When an app launches, it's icon is displayed inside an
indeterminate, circular status bar. See
<https://stackoverflow.com/questions/69113082/how-can-we-add-a-launch-screen-to-a-watch-app>.

## Launching from a Complication

To create a watch face complication that launches a custom app when tapped:

1. Open the "Watch" app on the iPhone that is paired with the watch.
1. Tap the "My Watch" tab on the bottom navigation.
1. Select a watch face that supports complications.
1. Scroll down to the "Complications" section.
1. Tap a complication.
1. Scroll to the custom app and tap it.

The app icon should appear as the complication icon,
but all I see so far is two dashes in its place.
Perhaps this happens because the app was not downloaded from the store
or perhaps it is a watchOS bug.

## Display Name

To change the display name of a watchOS app:

1. Select the top entry in the Navigator.
1. Under TARGETS, select the target whose name ends in " App".
1. Change the value of "Display Name".
1. Deploy the app again.
