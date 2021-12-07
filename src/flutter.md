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

## Creating and Running an App

To create an app, enter `flutter create app_name`.
Hyphens are not allowed in app names, but underscores are.

To run an app, cd to the app directory and enter `flutter run`.
If the iOS Simulator is running, the app will run there.
If an Android emulator is running, the app will run there.
Otherwise it will run in the Chrome web browser.

The default app renders a header that displays an app title,
some text, a number, and a button in the lower-right
that increments the number.

The starting point of the app is the file `lib/main.dart`.
This defines the `main` function that all Dart apps must have.

To deploy an app to an iOS device:

- Install CocoaPods by entering `sudo gem install cocoapods`.
- Follow the Xcode signing flow to provision your project.
- Enter `flutter run`.
- TODO: TEST ALL OF THIS!

## Linting

Flutter apps provide default code linting.
The rules can be configured in the provided file `analysis_options.yaml`.
To disable the rule that complains about using the `print` function,
uncomment the line that contains `avoid_print: false`.
To enable the rule that complains about literal strings
using double quotes instead of single,
uncomment the line that contains `prefer_single_quotes: true`.

## Code Formatting

Formatting of Dart code is provided by {% aTargetBlank
"https://dart.dev/tools/dart-format", "dart format" %}.
To get the best formatting, include a comma after every function parameter,
even the last one.

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

## Material vs. Cupertino

Material Design can be used on any platform, not just Android.
When a Flutter app uses this, it will have Material look and feel
on all devices, including iOS.

Use the Cupertino library to have an iOS look and feel.
This also works on all platforms.
However, due to licensing restrictions it won't
have the correct fonts on Android.

## Basic Flutter App Structure

Here is an example of the structure of a basic Flutter app
that uses Material Design.

```dart
import 'package:flutter/material.dart';

// This defines the starting point of all Dart apps.
void main() {
  // When creating instances of immutable classes,
  // constructor calls should be preceded by "const".
  // This enables sharing references to instances create with
  // the same arguments which optimizes memory.
  // These instances are created at compile-time.
  // All fields of immutable classes must be
  // declared with the "final" keyword.
  runApp(const MyApp());
}

// This class is also used by tests.
// See the supplied test/widget_test.dart file.
class MyApp extends StatelessWidget {
  // All widget constructors must take an optional parameter of type "Key".
  // This uniquely identifies a widget instance and is
  // important when widgets will be added, removed, or reordered.
  // Often no key value is provided.
  // One way to specify a key value is `key: UniqueKey()`.
  // Widget constructors can also take additional parameters.
  const MyApp({Key? key}) : super(key: key);

  // Widget build methods describe what a widget renders.
  // In stateless widgets it is only called once (true?).
  // In stateful widgets it is called initially
  // and again every time the state changes.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      // This is a one-line description used by devices
      // to identify the app for users.
      // On Android titles appear above task manager app snapshots
      // displayed when users press the "recent apps" button.
      // On iOS this value is not used.
      title: 'My Title',
      theme: ThemeData(primarySwatch: Colors.amber),
      home: const MyPage(),
    );
  }
}

class MyPage extends StatelessWidget {
  const MyPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('My App'),
      ),
      body: const Text('Hello, World!'),
    );
  }
}
```

Here is an example of the structure of a basic Flutter app
that uses Cupertino theming.

```dart
import 'package:flutter/cupertino.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    print('MyApp.build entered');
    return const CupertinoApp(
      theme:
        CupertinoThemeData(barBackgroundColor: CupertinoColors.activeBlue),
      home: MyPage(),
    );
  }
}

class MyPage extends StatelessWidget {
  const MyPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return const CupertinoPageScaffold(
      navigationBar: CupertinoNavigationBar(
        middle: Text('My App'),
      ),
      child: Center(child: Text('Hello, World!')),
    );
  }
}
```

## Fonts

To use custom fonts:

- Download font files such as `.ttf` files.
- Create a directory named `fonts` in the Flutter project.
- Copy the font files into this directory.
- Modify the `pubspec.yaml` file and add the following
  which assumes the font to be added is named "Corinthia":

  ```yaml
  fonts:
    - family: Corinthia
      fonts:
        - asset: fonts/Corinthia-Bold.ttf
        - asset: fonts/Corinthia-Regular.ttf
  ```

To change the font family, size, and weight used to render text,
specify the `style` parameter. For example:

```dart
Text('Hello, World!',
  style: TextStyle(
    backgroundColor: Colors.yellow,
    //backgroundColor: CupertinoColors.systemYellow,
    color: Colors.red,
    //color: CupertinoColors.systemRed,
    fontFamily: 'Corinthia',
    fontSize: 64,
    fontWeight: FontWeight.bold,
  ),
)
```

## Styling

One way to add styling to a widget is to wrap it in a `Container` widget.
The following example adds padding, a background color, and a border
to a `Text` widget. The equivalent CSS property is shown in comments.

```dart
Container(
  child: const Text('Hello, World!'),
  decoration: BoxDecoration(
    border: Border.all(color: Colors.blue, width: 5),
    // CSS: border 5px solid blue;
    borderRadius: const BorderRadius.all(Radius.circular(10)),
    // CSS: border-radius: 10px;
    color: Colors.yellow,
    // CSS: background-color: yellow;
    ),
  padding: const EdgeInsets.all(20),
  // CSS: padding: 20px;
),
```

A `Container` widget can be given a specific size by adding
`width` and/or `height` parameters whose values are number.

## Basic Widgets

All the predefined widgets are documented in the {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets", "Widget Catalog" %}.

### DefaultTextStyle

This is the style applied to text that doesn't have an explicit style.

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

### RichText

This renders runs of text that each used different styles.
It uses `TextSpan` objects.

### Text

This renders a run of text with a single style.

## Material Widgets

These widgets use Material Design styling.
They are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/material",
"Cupertino (iOS-style) widgets" %}.

### Appbar

This is a Material Design application bar that appears at the top of an app.
It can contain other widgets such as `TabBar` and `FlexibleSpaceBar`.

### BottomNavigationBar

This renders a bottom navigation bar containing buttons
that can be tapped to switch between top-level views.

### Drawer

This renders a panel that slides in from the left side
and contains navigation links.

### MaterialApp

This wraps a set of widgets that commonly appear in Material Design apps.

### Scaffold

This provides the structure of a Material Design layout.
It can show drawers, snack bars, and bottom sheets.

### SliverAppBar

This renders a top app bar and contains a `CustomScrollView`.

### TabBar

This renders a horizontal row of tabs.

### TabBarView

This renders a page that corresponds to a tab in a `TabBar`.

### TabController

This handles rendering a `TabBarView`
when its corresponding `TabBar` tab is tapped.

### TabPageSelector

This renders a "circular indicators"? for each tab in `TabBarView`.

### WidgetsApp

This wraps a set of widgets that typically appear in an app.

## Cupertino Widgets

These widgets use iOS styling rather than Material Design.
They are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/cupertino",
"Cupertino (iOS-style) widgets" %}.

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

## Layout Widgets

The layout widgets are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/layout",
"Cupertino (iOS-style) widgets" %}.

### Single-Child Layout Widgets

#### Align

#### AspectRatio

#### Baseline

#### Center

#### ConstrainedBox

#### Container

This applies positioning and sizing to other widgets.

#### CustomSingleChildLayout

#### Expanded

#### FittedBox

#### FractionallySizedBox

#### IntrinsicHeight

#### IntrinsicWidth

#### LimitedBox

#### Offstage

#### OverflowBox

#### Padding

#### SizedBox

#### SizedOverflowBox

#### Transform

### Multiple-Child Layout Widgets

#### Column

This renders a vertical list of child widgets.
It is similar to a SwiftUI `VStack`.

#### CustomMultiChildLayout

#### Flow

#### GridView

#### IndexedStack

#### LayoutBuilder

#### ListBody

#### ListView

#### Row

This renders a horizontal list of child widgets.
It is similar to a SwiftUI `HStack`.

#### Stack

This renders widgets on top of each other.
It is similar to a SwiftUI `ZStack`.

#### Table

#### Wrap

### Sliver Widgets

Are these iOS-style layout widgets?

### Icons

Icons are provided by the CupertinoIcons package
which is documented at {% aTargetBlank
"https://pub.dev/packages/cupertino_icons", "cupertino_icons" %}.

## Annoyances

- The VS Code Flutter extension displays a comment after the closing paren
  of all widgets. It isn't really in the code, but adds visual clutter.
  I haven't found a way to disable it yet.

- Dart wants many constructor calls to be preceded by the `const` keyword.
  This makes the code verbose.
