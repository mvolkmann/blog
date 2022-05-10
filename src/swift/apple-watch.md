---
eleventyNavigation:
  key: Apple Watch
  parent: Swift
  order: 4
layout: topic-layout.njk
---

## Overview

This is a collection of tips on developing Apple Watch apps
using SwiftUI and Xcode.

## Simulator vs. Devices

Running on the Simulator is quite fast.

To deploy an app to an Apple Watch:

- Attach the iPhone with which it is paired to the Mac using a USB cable.
- Select the iPhone from the device dropdown at the top of Xcode.
- Press the triangle run button.
- It may be necessary to unlock the iPhone.
- If a message says "The OS version installed ... does not support
  WatchKit App products", try ejecting the phone from the Finder
  and plugging it into a different USB port.
- When it finally works, it can take several minutes
  before the app appears on the watch.
  For this reason, perform most debugging in the Simulator.

## App Icons

To add app icons to an Apple Watch app:

- Open the file `Assets.xcassets`.
- Click the "+" button in the lower-left.
- Select watchOS ... watchOS App Icon.
- Drag icon images for all the sizes onto the placeholder squares.

# Launch Screen

watchOS apps do not currently support launch screens.
When an app launches, it's icon is displayed inside an
indeterminate, circular status bar. See
<https://stackoverflow.com/questions/69113082/how-can-we-add-a-launch-screen-to-a-watch-app>.
