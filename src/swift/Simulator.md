---
eleventyNavigation:
  key: Simulator
  parent: Swift
layout: topic-layout.njk
---

## Overview

When developing iOS, watchOS, and macOS apps, it is useful to
launch the app in the Simulator app directly from Xcode.
Preview and the Simulator can simulate many different devices.

To run the app in the Simulator, click the triangle at the top
or press cmd-r while focus is in Xcode (not in the Simulator).
This builds the app, launches the Simulator (if not already running),
loads the app in the Simulator, and starts it.

Unlike in Previews, the app is not automatically updated
when code changes are saved.
Click the triangle or press cmd-r again
to repeat the entire build/load/start process.

If there are errors or warnings, the number of each will be displayed
on the right side of the code editor header.
Click either number to display the messages in the "Issue Navigator".

## Zooming

For UI views that support pinch-to-zoom such as maps,
that can be performed in the Simulator by holding down the option key
and using two fingers on a trackpad.

## Comparison to Preview

Preview is more limited in functionality than the Simulator.
TODO: List the differences between the Simulator app and the Preview pane.

## Device Selection

The default device type used by the Simulator can be changed
by selecting Product ... Destination ... Choose Destination...
and selecting a device type.
This must be done in each Xcode project.

There are known rendering issues with the "iPhone 12 mini" simulator,
so choose a different device for now.

## Keyboard Shortcuts

The Simulator supports many useful keyboard shortcuts.

| Action                                        | Menu Item                                         | Keyboard Shortcut |
| --------------------------------------------- | ------------------------------------------------- | ----------------- |
| change orientation                            | Device ... Rotate {Left \| Right}                 | cmd-◀ and cmd-▶   |
| toggle use of computer keyboard to enter text | IO ... Keyboard ... Connect Hardware ... Keyboard | cmd-shift-k       |
| switch between light and dark mode            | Features ... Toggle Appearance                    | cmd-shift-a       |
| copy screen to clipboard                      | Edit ... Copy Screen                              | cmd-ctrl-c        |
| save screen shot as .png file                 | File ... Save Screen                              | cmd-s             |
| save session video as .mp4 file               | File ... Record Screen                            | cmd-r             |
| simulate use of Face ID                       | Features ... Face ID ... option                   | rarely used       |
| show home screen                              | Device ... Home                                   | cmd-shift-h       |
| show app switcher (to switch or quit apps)    | Device ... App Switcher                           | cmd-ctrl-shift-h  |

## Screenshots

To capture a screenshot, click the camera icon in the upper-right.

Screenshot file names begin with "Simulator Screen Shot" followed by
the device name, date, and time.
They are saved in the Desktop directory.
These screenshots are useful for uploading to
`https://appstoreconnect.apple.com/` when submitting an app to the App Store.

## Videos

To begin recording a video of a Simulator session,
select File ... Record Screen, press cmd-r, or
hold the option key and click the start icon
that replaces the camera icon in the upper-right.
Audio is not captured.
When finished recording, click the stop button in the upper-right
that replaced the start button.

A video thumbnail will appear at the bottom right edge of the simulator.
Right-click this and select how the video should be saved.
The options include "Save to Desktop", "Save to Documents",
"Save as Animated GIF" (to Desktop), "Show in Finder", "Delete",
and "Close" (still saves to Desktop).
If none of these options are selected,
the video is automatically saved to the Desktop.
Animated GIFs are ideal for including in project `README.md` files.

Screen recording file names begins with "Simulator Screen Recording" followed
by the device name, date, and time will be saved in the selected location.
Double-click the file to play it in the "QuickTime Player" app.
These are useful for demos and to share with designers and clients
to discuss app features.

## Face ID

Face ID options include Enrolled (approves use of Face ID),
Matching Face (simulates a match),
and Non-Matching Face (simulates failure to match).

## iCloud

To sign in to your iCloud account in the Simulator:

- Open the Settings app.
- Click "Sign in to your iPhone".
- Enter your Apple ID.
- enter your password.
- Click "Don't Merge" to avoid merging data
  from the Simulator to your iCloud account.

## Erasing Data and Settings

To erase all data and settings,
select Device ... Erase All Content and Settings...
This can be useful to run an app without having any data cached
from previous runs, such as authentication details.

## More

There are many more features in the Simulator menus.
