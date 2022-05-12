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

Apple guidelines on watchOS development can be found {% aTargetBlank
"https://developer.apple.com/documentation/watchosapps/", "here" %}.
This begins with
"Apple Watch provides easy access to vital information on someone’s wrist.
The watchOS experience focuses on quick actions that
achieve useful tasks through brief, punctuated interactions."

watchOS supports a subset of SwiftUI.
I have not been able to find documentation that identifies that subset.
If an app attempts to use a SwiftUI feature in a watchOS app
that is not supported, a compile-time error will be displayed.

watchOS apps can be independent of any iOS app
or they can have a companion iOS app.
When there is a companion app, they can share data. HOW?

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

## iOS App with Watch App

To create an app that runs on both an iPhone and Apple Watch (companion apps):

1. In Xcode, select File ... New ... Project...
1. Select the "watchOS" tab.
1. Select "iOS App with Watch App".
1. Click the "Next" button.
1. Enter a project name.
1. Click the "Create" button.

The iOS app is defined by files in the "{project-name}" directory.
The watchOS app is defined by files in the
"{project-name} WatchKit Extension" directory.

To share data between these apps, use the WatchConnectivity framework.
See https://www.youtube.com/watch?v=i3_6m0a5ovw.

1. Select the top item in the Navigator.
1. Under "TARGETS", select the first target.
1. Select the "General" tab.
1. In the "Frameworks, Libraries, and Embedded Content" section,
   click the "+" button, select "WatchConnectivity.framework",
   and click the "Add" button.

To make some of the iOS source files also available to watchOS:

1. Click the top item in the Navigator.
1. Under "TARGETS", select the target
   whose name ends with " WatchKit Extension".
1. Click the "Build Phases" tab.
1. Open the "Compile Sources" section.
1. Click the "+" at the bottom of the section.
1. Select the files Data.swift, ConnectionProvider.swift, and ViewModel.swift.
1. Click the "Add" button.

To create a pair of simulators (one iPhone and one Apple Watch)
that can be launched together:

1. Click the device dropdown at the top of Xcode.
1. Select "Add Additional Simulators..." which opens a dialog.
1. Click the "Simulators" tab in the left nav.
1. Click the "+" at the bottom of the left nav.
1. Enter a name like "iPhone 13 + Watch 7".
1. Select an iPhone model from the "Device Type" dropdown.
1. Check the "Paired Apple Watch" checkbox.
1. Click the "Next" button.
1. Select a Watch model from the "Device Type" dropdown.
1. Click the "Create" button.
1. Close the dialog.
1. Select the newly created simulator pair name from the device dropdown.
1. Run the app to launch both simulators.

To run both the iPhone and Apple Watch apps in the Simulator:

1. Select the Simulator created above from the device dropdown at the top.
1. Press the play button (triangle) to run the iOS app in the Simulator.
1. Click the dropdown at the top that is displaying the current app name.
1. Select the entry that begins with the app name and ends with " WatchKit App".
1. From the device dropdown at the top, select an Apple Watch model.
1. Press the play button (triangle) again
   to run the watchOS app in the Simulator.
1. The debug console at the bottom will show the output from one of the apps.
   To see output from the other app, select it from the app name dropdown
   at the top of the debug console.
