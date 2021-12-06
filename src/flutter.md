---
eleventyNavigation:
  key: Flutter
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://flutter.dev", "Flutter" %}
enables writing mobile, web, desktop, and embedded apps
using the {% aTargetBlank "https://dart.dev", "Dart programming language" %}.

## Resources

- [Lab: Write your first Flutter app](https://flutter.dev/docs/get-started/codelab)
- [Cookbook: Useful Flutter samples](https://flutter.dev/docs/cookbook)
- [online documentation](https://flutter.dev/docs)

## Setup

- Install the Flutter SDK.

  - Download and unzip it.
  - Add the path to the `flutter/bin` directory
    to the `PATH` environment variable.

- To enable testing on iOS:

  - Install Xcode.

- To enable testing on Android:

  - Install Android Studio, even when using a different editor such as VS Code.
  - Install the Android SDK using Homebrew.
    - `brew tap homebrew/cask`
    - `brew install --cask android-sdk`

- To start the iOS Simulator:

  Enter `open -a Simulator`.
  This works in Bash and Fish, but not in Nushell.

- To start an Android emulator:

  - Launch the Android Studio app.
  - Select Tools ... AVD Manager
  - To create a new virtual device:
    - Click the "+ Create Virtual Device" button at the bottom.
    - Select a device such as "Pixel 5" and click "Next".
    - Select an Android version such as "R" and click "Next".
    - Click "Finish".
  - To start an emulator, click a play button (green triangle)
    in the "Actions" column of one of the devices.

- Verify the installation by entering `flutter doctor`.

  - I couldn't get this to find Android Studio!

- To create an app:

  Enter `flutter create app_name`.
  Hyphens are not allowed in app names, but underscores are.

- `cd app_name`

- To run an app:

  Enter `flutter run`.
  If the iOS Simulator is running, the app will run there.
  If an Android emulator is running, the app will run there.
  Otherwise it will run in the Chrome web browser.

- To deploy an app to a device:
  - Install CocoaPods by entering `sudo gem install cocoapods`.
  - Follow the Xcode signing flow to provision your project.
  - Enter `flutter run`.

## Hot Reloading

Flutter has great hot reloading!
This requires using a compatible editor which includes
Android Studio, IntelliJ, VS Code, and emacs.
After every code change is saved Flutter
updates the running app without losing state.

When using VS Code, install the Flutter extension.
Run the app from VS Code by selecting
Run ... Start Debugging or Run ... Run Without Debugging.

If the app is run from a terminal with `flutter run`
then hot reloading will only occur if focus is moved to the terminal
and the "r" key is pressed.

## Widgets

### Appbar

This is a Material Design application bar that appears at the top of an app.
It can contain other widgets such as `TabBar` and `FlexibleSpaceBar`.

### Column

This renders a vertical list of child widgets.

### Container

This applies positioning and sizing to other widgets.

### ElevatedButton

This is a Material Design button that elevates when pressed.
Should it be the opposite?

### FlutterLogo

This renders the Flutter logo.

### Icon

This renders a Material Design icon.

### Image

This renders an image.

### Placeholder

This renders a rectangle that represents where
other widgets will be added in the future.

### Row

This renders a horizontal list of child widgets.

### Scaffold

This provides the structure of a Material Design layout.
It can show drawers, snack bars, and bottom sheets.

### Text

This renders a run of text with a single style.

## Cupertino Widgets

These widgets use iOS styling rather than Material Design.

### CupertinoActionSheet

This renders a modal that slides up from the bottom
to allow selection from a set of options.
It is sometimes used for confirmation dialogs.

### CupertinoActivityIndicator

This renders an iOS-style spinner.

### CupertinoAlertDialog

This renders an iOS-style alert dialog with
a title, message, and set of buttons.

### CupertinoButton

This renders an iOS-style button that can be tapped to execute associated code.

### CupertinoContextMenu

This renders an iOS-style modal containing a set of tappable options
when a specific widget is long-pressed.

### CupertinoDatePicker

This renders an iOS-style wheel picker for entering a date and time.

### CupertinoDialogAction

This renders a button with no background color or border.
It is typically usd in `CupertinoAlertDialog`.

### CupertinoFullscreenDialogTransition

This is an iOS-style transition that is used to render fullscreen dialogs.

### CupertinoNavigationBar

This renders an iOS-style top navigation bar.
It is typically used with `CupertinoPageScaffold`.

### CupertinoPageScaffold

This renders a common iOS-style page layout.

### CupertinoPageTransition

This provides an iOS-style page transition animation.

### CupertinoPicker

This renders an iOS-style wheel picker.

### CupertinoPopupSurface

This renders a rounded rectangle for an alert dialog or action sheet.

### CupertinoScrollbar

This renders an iOS-style scrollbar.

### CupertinoSearchTextField

This renders an iOS-style search input.

### CupertinoSegmentedControl

This renders an iOS-style segmented control
which is a horizontal list of mutually-exclusive buttons.

### CupertinoSlider

This renders a slider for selecting a value from a range.

### CupertinoSlidingSegmentedControl

This renders an iOS-style segmented control
which is a horizontal list of buttons.

### CupertinoSliverNavigationBar

This renders an iOS-style navigation bar with a large title.

### CupertinoSwitch

This renders an iOS-style switch (like the SwiftUI `Toggle` view)

### CupertinoTabBar

This is an iOS-style bottom tab bar
that is typically used with `CupertinoTabScaffold`.

### CupertinoTabScaffold

This positions a tab bar below the display of select tab content.

### CupertinoTabView

This supports "parallel navigation"? between tabs
and is typically used with `CupertinoTabScaffold`.

### CupertinoTextField

This renders an iOS-style input text field.

### CupertinoTimerPicker

This renders an iOS-style wheel picker
for entering hours, minutes, and seconds.

## Layouts
