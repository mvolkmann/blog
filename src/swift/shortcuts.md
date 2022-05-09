---
eleventyNavigation:
  key: Shortcuts
  parent: Swift
layout: topic-layout.njk
---

## Overview

Shortcuts enabling defining app-specific actions that can be
invoked from the Shortcuts app or with Siri voice commands.
For details, watch the {% aTargetBlank
"https://developer.apple.com/videos/play/wwdc2018/211/",
"Introduction to Siri Shortcuts" %} video from WWDC 2018.

## Two APIs

There are two APIs for implementing shortcuts.

`NSUserActivity` is for shortcuts that open something in the app or
show items that are indexed in Spotlight or are offered to Handoff.

Intents for more specific app actions.
They can be defined to run even when the app is in the background.
They can include parameters to customize their behavior.
There are predefined intents and custom intents can be defined.

## Steps

1. Select File ... New ... File...
1. In the "Resource" section, choose "SiriKit Intent Definition File".
1. Click the Next button.
1. Click the Create button which saves the file as
   "Intents.intentdefinition" by default.
1. With this new file selected, click the "+" button
   in the lower-left of the editor panel.
1. Select "New Intent".
1. Change the name of the intent from "Intent" to something more specific
   (ex. SetBackgroundGreen)
1. Choose the category that most closely matches the intent
   (ex. Set).
1. Enter a description and title for the intent.
1. Add parameters required (ex. color).
1. Add suggestions of phrases to invoke the intent
   that include parameters names (ex. set background to color).
   Parameter names are selected from dropdowns while typing the suggestion.
   Users can choose other phrases to invoke your shortcuts
   (in the Shortcuts app?)
1. If the app must be in the foreground in order to execute the intent,
   uncheck "Supports background execution".
1. Multiple suggestions for the same intent can be defined.
1. This generates an Intent class that inherits from INIntent
   (ex. SetBackgroundIntent) and
   an IntentHandling protocol that inherits from NSObjectProtocol
   (ex. SetBackgroundIntentHandling).
1. In the code to be executed for each intent, donate the
   corresponding shortcuts so Siri will associate them.
   This helps Siri learn when a shortcut should be suggested
   based on usage frequency.
1. Handle each intent in the `AppDelegate` class.

To handle shortcuts in the background, create an app extension (16:50 in video).

1. Select File ... New ... Target...
1. In the "Application Extension" section, select "Intents Extension".
1. Click the Next button.
1. Enter a name (ex. MyIntents).
1. Click the Finish button?
1. Click the Activate button.
1. In the Navigator, select `Intents.indentdefinition`.
1. Click the first tab in the Inspector panel on the right.
1. Under "Target Membership", check the checkboxes for the intents extension
   (ex. `MyIntents` and `MyIntentsUI`).
1. In the dropdown after each, select "No Generated Classes".
1. In the Navigator, select the top entry.
1. Click the "Signing & Capabilities" tab.
1. For each new target:

   - Select the target in the left nav.
   - Add the "App Groups" capability to the target.
   - Check the checkbox for the default app group
     to make all the targets belong to the same app group.

1. The first new target contains a file named `IntentHandler.swift`
   that defines the `IntentHandler` class which inherits from `INExtension`
   and contains a `handler` method, a `confirm` method, and several `handle` methods.
1. There is a LARGE amount of code in this generated file
   and it seems that most of it should be replaced by custom logic.
1. The `confirm` method validates the intent parameters and
   returns an error if any are invalid.
1. The `handle` method performs the shortcut.
1. Always implement `continueUserActivity`.
   What does this mean? (18:14 in video).

To expose Siri Shortcuts to the Apple Watch face,
provide `INRelevantShortcut` objects. (18:30 in video)

To add shortcuts to Siri:

1. Open the Settings app.
1. Select "Siri & Search".
1. Donated shortcuts should appear here BUT THEY DO NOT!
1. Select a shortcut.
1. Tap the record button to record the phrase to use to invoke it.
   (Is the application-suggested phrase used if this is skipped?)

## Debugging

To aid in debugging, open the Settings app on the iPhone used for testing,
tap "Developer", and enable "Display Recent Shortcuts"
and "Display Donations on Lock Screen".
