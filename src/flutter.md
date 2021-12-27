---
eleventyNavigation:
  key: Flutter
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://flutter.dev", "Flutter" %}
enables writing mobile, web, desktop, and embedded apps
using the {% aTargetBlank "https://dart.dev", "Dart programming language" %}.
For details on the Dart programming language,
see this <a href="../dart/">page</a>.

Everything rendered by a Flutter app is rendered by a "widget".
There are many provided widgets and custom ones can be defined.

The Flutter framework is implemented in Dart,
except for the {% aTargetBlank "https://skia.org", "Skia" %}
2D graphics library which is implemented in C++.

{% aTargetBlank "https://flutterflow.io", "FlutterFlow" %} is a
relatively new, low-code tool for building Flutter applications.

## Resources

- {% aTargetBlank "https://flutter.dev/", "Flutter home page" %}
- {% aTargetBlank "https://flutter.dev/docs", "Flutter official documentation" %}
- {% aTargetBlank "https://flutter.dev/docs/get-started/codelab",
  "Write your first Flutter app" %}
- {% aTargetBlank "https://flutter.dev/docs/cookbook",
  "Cookbook: Useful Flutter samples" %}

## Comparison to React Native

Of the mobile frameworks that target both Android and iOS,
React Native and Flutter are the most popular.
The table below compares these two options.

| Topic                       | React Native          | Flutter |
| --------------------------- | --------------------- | ------- |
| developed by                | Facebook              | Google  |
| programming language        | JavaScript/TypeScript | Dart    |
| resulting app sizes         | large                 | medium  |
| hot reloading time          | slow                  | fast    |
| runtime performance         | somewhat slow         | faster  |
| GitHub stars as of 12/23/21 | 100K                  | 134K    |
| version as of 12/23/21      | 0.66.4                | 2.8.1   |

React Native is an attractive choice for development teams that
already have experience in web technologies in general and React in particular.
It is possible to share some code between React and React Native applications.

Flutter targets more than just Android and iOS.
It also targets web apps and desktop apps running on Windows, macOS, and Linux.

Because everything rendered by Flutter is just drawn on a canvas,
it can take advantage of device GPUs.
This is one reason it has better performance than React Native.

Flutter uses {% aTargetBlank "https://skia.org", "Skia" %}
to draw everything it renders.
"Skia is an open source 2D graphics library which provides common APIs
that work across a variety of hardware and software platforms.
It serves as the graphics engine for Google Chrome and Chrome OS,
Android, Flutter, Mozilla Firefox and Firefox OS, and many other products."
{% aTargetBlank "https://developer.android.com/jetpack/compose",
"Jetpack Compose" %}, a Kotlin toolkit for building native Android applications,
also uses Skia.

Companies using React Native include
Airbnb, Baidui, Bloomberg, Discord, Facebook, Instagram, Oculus, Pinterest,
Salesforce, Shopify, Skype, Tesla, Uber Eats, Walmart, and Wix.

Companies using Flutter include
Alibaba, Amazon, Betterment, BMW, Etsy, Google Ads, Google Pay,
Hamilton Musical, iRobot, New York Times, Phillips Hue, Realtor.com,
Supabase, Tonal, and Toyota.

## Terminology

The software used to test an app on a specific kind of device
without using a real device goes by different names.
For Android devices this is referred to as an "emulator".
For iOS devices this is referred to as a "simulator".
This document refers to both as a simulator.

## Setup

- Install the Flutter SDK.

  - Download and unzip it.
  - Add the path to the `flutter/bin` directory
    to the `PATH` environment variable.
  - Later, to upgrade a project to the latest version, cd to the project
    and enter the following commands:
    - `flutter upgrade`
    - `flutter clean`
    - `flutter pub get`
    - `flutter pub upgrade`

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

  - If you get the error "cmdline-tools component is missing",
    enter `sdkmanager --install "cmdline-tools" and then "latest".
  - If this gives the error
    "java.lang.NoClassDefFoundError: javax/xml/bind/annotation/XmlSchema",
    start "Android Studio", select Tools ... SDK Manager, select
    Appearance & Behavior ... System Settings ... Android SDK ... SDK Tools,
    check "Android SDK Command-line tools (latest)",
    and click the Apply button.

## Tools

Flutter provides a command-line tool
for deploying apps to the Google Play store.
TODO: What is this?
React Native requires a third-party tool for deploying apps
that is more complex.

## Creating and Running an App

To create an app, enter `flutter create app_name`.
Hyphens are not allowed in app names, but underscores are.

To run an app, cd to the app directory and enter `flutter run`.
If the iOS Simulator is running, the app will run there.
If an Android emulator is running, the app will run there.
Otherwise it will run in the Chrome web browser.
This will take about 1.5 minutes to start the first time it is run
and about 10 seconds after that.
But typically the app will remain running and
changes will be deployed with hot reload nearly instantly.

The default app renders a header that displays an app title,
some text, a number, and a button in the lower-right
that increments the number.

The starting point of the app is the file `lib/main.dart`.
This defines the `main` function that all Dart apps must have.

To avoid committing generated files to a Git repository,
add `/android` and `/ios` to the provided `.gitignore` file.

## Linting

Flutter apps provide default code linting.
The rules can be configured in the provided file `analysis_options.yaml`.
To disable the rule that complains about using the `print` function,
uncomment the line that contains `avoid_print: false`.
To enable the rule that complains about literal strings
using double quotes instead of single,
uncomment the line that contains `prefer_single_quotes: true`.

When creating instances of immutable classes, constructor calls that are
only passed static arguments should be preceded by the `const` keyword.
This enables sharing references to instances create with
the same arguments which optimizes memory.
These instances are created at compile-time.
As nice as this sounds, the linter will complain constantly
about missing or incorrectly applied `const` keywords.
To avoid these warnings, add the following rule configuration.

```yaml
prefer_const_constructors: false
```

## Code Formatting

Formatting of Dart code is provided by {% aTargetBlank
"https://dart.dev/tools/dart-format", "dart format" %}.
To get the best formatting, include a comma after every function parameter,
even the last one.

## Hot Reloading

Flutter has great hot reloading!

If the app is run from a terminal with `flutter run`
then hot reloading will only occur if focus is moved to the terminal
and the "r" key is pressed.

If the app is run from a compatible editor, the app will
automatically update after saving code changes without losing state.
Compatible editors include Android Studio, IntelliJ, VS Code, and emacs.

To run a Flutter app from VS Code, select
Run ... Start Debugging (F5) or Run ... Run Without Debugging (ctrl-F5).
The simulated or real device to use can be selected
from a menu in the VS Code footer.

## Debugging

When a Flutter app is run from a terminal,
pressing "p" displays blue outlines of all widgets.
This is useful to understand the position and size of each widget.
Press "p" again to toggle this off.

## Running on Devices

To run a Flutter app on a connected iPhone:

- From a terminal, enter `sudo gem install cocoapods`.
- Attach the phone to the computer with a USB cable.
- Unlock the phone.
- From a terminal running bash, cd to the top project directory.
- Enter `open ios/Runner.xcworkspace` to launch Xcode.
- Select "iPhone" from the device drop-down in the header.
- Click "Runner" at the top of the Navigator.
- Click the "Signing & Capabilities" tab.
- Select your team from the Team drop-down.
- Enter a unique "Bundle Identifier", perhaps containing
  your email address and the project name.
- option #1 - In Xcode, press the run button or cmd-r.
- option #2 - From a terminal, enter `flutter run` and
  select `iPhone` from the list of device options.
  TODO: Neither of these options worked!

To run a Flutter app on an iPhone wirelessly:

- The iPhone must be on the same WiFi network as the computer.
- In Xcode, select Window ... Devices and Simulators.
- In the dialog that is opened, select "iPhone" in the left nav.
- Check the "Connect vis network" checkbox.
- In the Finder, eject the iPhone.
- Disconnect the iPhone from the computer.
- Run the Flutter app using one of the options above.

## Material vs. Cupertino

Material Design can be used on any platform, not just Android.
When a Flutter app uses this, it will have Material look and feel
on all devices, including iOS.

Use the Cupertino library to have an iOS look and feel.
This also works on all platforms.
However, due to licensing restrictions it won't
have the correct fonts on Android.

## VS Code

Install the Flutter extension.
This provides many things including:

- great auto-complete support

- snippets

  - `stless` adds template code for creating a stateless widget
  - `stful` adds template code for creating a stateful widget

  The code added by these requires importing a library that defines
  the `StatelessWidget` and `StatefulWidget` classes.
  For example:

  ```dart
  import 'package:flutter/material.dart';
  ```

- phantom comments after the last lines of multi-line widget constructor calls
  to make it easy to spot where they end

- Flutter-specific entries in the status bar that include:

  - Flutter SDK version (ex Flutter 2.8.1)
  - device name being simulated (ex. iPhone 13 (ios simulator))
    that can be clicked to select a different device

- Flutter-specific command palette commands that include:

  - Change SDK
  - Clean Project
  - Focus on Outline View
  - Get Packages
  - Hot Reload

  - Inspect Widget

    After running this command, click a widget in the simulator.
    The widget will be highlighted in the simulator,
    its name will be displayed in a tooltip,
    and the corresponding code will be highlighted in VS Code.
    To turn this off, run the "Flutter: Cancel Widget Inspection" command.

  - Launch Emulator
  - List Outdated Packages
  - New Project

  - Open DevTools

    This opens a drop-down for selecting the DevTools page to open.
    The options are "Widget Inspector", "CPU Profiler", "Memory",
    "Performance", "Network", and "Logging".
    There is also an option to "Open DevTools in Web Browser"
    inside of inside VS Code.

  - Open DevTools Performance Page

  - Open DevTools Widget Inspector

    This is an incredibly useful debugging tool!
    It displays the widget tree on the left.
    Selecting a widget displays details about it on the right.
    The "Layout Explorer" tab displays layout details for the widget.
    The "Details Tree" tab displays the properties of the widget.

  - Open Observatory Timeline
  - Override Platform
  - Run Flutter Doctor
  - Run Flutter Upgrade
  - Save Screenshot

  - Select Device

    This displays a list of device name that can be simulated.
    Select one to change the device type that will be used.
    This can start the iOS Simulator and Android Emulator,
    so it is not necessary to manually do this from outside of VS Code.
    But it is still necessary to select a specific device type
    and run the app.

  - Toggle Baseline Painting
  - Toggle Brightness

    This toggles between light mode and dark mode.
    When using `MaterialApp` light mode is defined by the `theme` property,
    dark mode is defined by the `darkTheme` property.
    A good default for `darkTheme` is `ThemeData.dark()`.
    For example:

    ```dart
    theme: ThemeData(primarySwatch: Colors.blue),
    darkTheme: ThemeData.dark(),
    // This selects light/dark mode based on user preference in their OS.
    themeMode: ThemeMode.system,
    ```

  - Toggle Debug Painting

    This displays a light blue outline around each widget
    to aid in understanding the current widget layout.

  - Toggle Debug-Mode Banner

    To hides/shows the red debug banner in the upper-right corner.

  - Toggle Performance Overlay

  - Toggle Repaint Rainbow

    This draws a thick, colored outline on all widgets
    that were repainted due to the last user interaction.
    The colors rotate to indicate that a previously repainted widget
    was repainted yet again.

  - Toggle Slow Animations

    This slows down animations to enable visual inspection.

  - Upgrade Packages
  - Upgrade Packages (--major-versions)

## Widget Types

Widgets that accept other widgets as arguments typically have
a parameter named `child` for one or `children` for multiple.

Widgets with a `children` parameter that layout those widgets
in a specific way can be referred to as "layout widgets".
Commonly used layout widgets include
`Center`, `Column`, `Container`, `Expanded`, `Flow`, `GridView`,
`ListView`, `Padding`, `Row`, `SizedBox`, `Stack`, `Table`, and `Wrap`.

Some widgets take a single widget as a parameter and apply styling.
Examples include `Container`, `MediaQuery`, `Padding`, `Theme`.
These can be referred to as "styling widgets".

## Stateless vs. Stateful Widgets

Every widget is either stateless or stateful.

Stateless widgets are defined by a class that
extends `StatelessWidget`, defines a constructor,
and overrides the `build` method.
Stateless widgets render one time based parameters passed to them.

Stateful widgets are defined by a pair of classes.
The first extends `StatefulWidget`, defines a constructor,
and overrides the `createState` method.
The second extends `State`, defines state fields,
and overrides the `build` method.
Storing the state in a separate class
allows the widget class to remain immutable.
Stateful widgets render initially and again each time their state changes.

It is common for `build` methods return a nested set of widgets
rather than just a single widget.

Flutter-compatible editors provide snippets for defining new widgets
that dramatically reduce the amount of code that must be typed.
In VS Code, type "s" and select
"Flutter stateful widget" or "Flutter stateful widget".

Stateless widgets begin with the following code
which defines a single class:

```dart
class SomeName extends StatelessWidget {
  // Note the odd Dart syntax for constructors.
  const SomeName({ Key? key }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Container(

    );
  }
}
```

Stateful widgets begin with the following code
which defines a pair of related classes:

```dart
class SomeName extends StatefulWidget {
  const SomeName ({ Key? key }) : super(key: key);

  @override
  _SomeName State createState() => _SomeName State();
}

class _SomeName State extends State<SomeName > {
  @override
  Widget build(BuildContext context) {
    return Container(

    );
  }
}
```

Here is an example of a stateless widget that renders a greeting
for a given name.

<img alt="Flutter Greet" style="width: 60%"
    src="/blog/assets/Flutter-Greet.png?v={{pkg.version}}"
    title="Flutter Greet">

```dart
class Greet extends StatelessWidget {
  final String name;

  const Greet({Key? key, required this.name}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Container(
      child: Text(
        'Hello, $name!',
        style: const TextStyle(
          color: Colors.red,
          fontWeight: FontWeight.bold,
        ),
      ),
      decoration: BoxDecoration(
        border: Border.all(color: Colors.orange, width: 3),
        color: Colors.yellow,
      ),
      padding: const EdgeInsets.all(10),
    );
  }
}
```

Here is an example of a stateful widget that maintains a count
and provides buttons for incrementing and decrementing the count.
The `setState` method defined by the `State` class
is similar to the `setState` function in React.

<img alt="Flutter Counter" style="width: 60%"
    src="/blog/assets/Flutter-Counter.png?v={{pkg.version}}"
    title="Flutter Counter">

```dart
class Counter extends StatefulWidget {
  const Counter({Key? key}) : super(key: key);

  @override
  _CounterState createState() => _CounterState();
}

class _CounterState extends State<Counter> {
  static const textStyle = TextStyle(fontSize: 36);

  // Declare state fields here.
  int count = 0;

  // Define this optional method to fetch initial state values.
  @override
  void initState() {
      // Fetch initial state values.
      super.initState();
  }

  // Define this optional method to perform
  // state cleanup when the widget is disposed.
  @override
  void dispose() {
      // Cleanup after state data here.
      super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Row(children: [
      TextButton(
        child: const Text('-', style: textStyle),
        // The button is disabled when onPressed is null.
        // State must modified inside functions that are passed to setState.
        onPressed: count <= 0 ? null : () => setState(() => count -= 1),
      ),
      Text('$count', style: textStyle),
      TextButton(
        child: const Text('+', style: textStyle),
        onPressed: () => setState(() => count += 1),
      ),
      ElevatedButton(
        child: const Text('Reset'),
        onPressed: () => setState(() => count = 0),
      ),
    ]);
  }
}
```

A widget `build` method can return only `Container()`
in order to render nothing.

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
  margin: const EdgeInsets.all(20),
  // CSS: margin: 20px;
  padding: const EdgeInsets.all(10),
  // CSS: padding: 10px;
),
```

A `Container` widget can be given a specific size by adding
`width` and/or `height` parameters whose values are number.

## Icons

The `Icon` widget renders an icon from a large provided set of icons.
For example, the following renders a music note icon.

```dart
Icon(Icons.audiotrack, color: Colors.red, size: 30)
```

## Images

The `Image` widget renders an image from a URL.
For example, the following renders a photo of an owl.

```dart
Image.network(
  'https://flutter.github.io/assets-for-api-docs/assets/widgets/owl.jpg'
)
```

To render an image from a local file,

- Create an `images` directory.
- Copy an image to that directory (ex. `comet.jpg`).
- Edit `pubspec.yaml` and add the following:

  ```yaml
  assets:
    - images/comet.jpg
  ```

- TODO: Use what to render it?

To render an image from an `AssetBundle` ...

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
The "main" axis is vertical and the "cross" axis is horizontal.

This constructor for widget takes the following named parameters,
all of which are optional.

| Parameter            | Type                      |
| -------------------- | ------------------------- |
| `children`           | `List<Widget>`            |
| `crossAxisAlignment` | `CrossAxisAlignment` enum |
| `mainAxisAlignment`  | `MainAxisAlignment` enum  |
| `mainAxisSize`       | `MainAxisSize` enum       |
| `textBaseline?`      | `TextBaseline` enum       |
| `textDirection?`     | `TextDirection` enum      |
| `verticalDirection`  | `VerticalDirection` enum  |

Typically only `children`, `crossAxisAlignment`,
`mainAxisAlignment`, and `mainAxisSize` are specified.

Values of the `CrossAxisAlignment` enum include
`start`, `center` (default), `end`, `stretch`, and `baseline` (aligns text baselines).

Values of the `MainAxisAlignment` enum include
`start` (default), `center`, `end`, `spaceAround`, `spaceBetween`, and `spaceEvenly`.

Values of the `MainSize` enum include
`max` (uses all available space; default) and
`min` (uses on what is needed to fit children).

Values of the `TextBaseline` enum include `alphabetic` and `ideographic`.

Values of the `TextDirection` enum include
`ltr` (left to right) and `rtl` (right to left).

Values of the `VerticalDirection` enum include `down` (default) and `up`.

#### CustomMultiChildLayout

#### Flow

#### GridView

#### IndexedStack

#### LayoutBuilder

#### ListBody

#### ListView

This displays a scrollable list of widgets.
The list is vertical by default, but can be changed to horizontal.
Here's an example of creating a scrollable list of colors.

```dart
class ColorList extends StatelessWidget {
  const ColorList({Key? key}) : super(key: key);

  final List<String> names = const [
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'purple',
    'white',
    'gray',
    'black',
    'brown'
  ];

  @override
  Widget build(BuildContext context) {
    return Container(
      decoration: BoxDecoration(border: Border.all(color: Colors.black)),
      margin: EdgeInsets.symmetric(horizontal: 10),
      padding: EdgeInsets.all(10),
      child: SizedBox(
        height: 100,
        child: ListView.builder(
          itemCount: names.length,
          // This function is called once for each index value in itemCount
          // to programmatically create a child widgets.
          itemBuilder: (BuildContext context, int index) {
            return SizedBox(
              height: 20,
              child: Text(names[index]),
            );
          },
        ),
      ),
    );
  }
}
```

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

## Persisting State

There are many approaches to persisting app data
so it is not lost when an app is closed.

- built-in `SharedPreference` class
- {% aTargetBlank "https://bloclibrary.dev/", "bloc" %} library
- {% aTargetBlank "https://pub.dev/packages/provider", "provider" %} library
  (similar to the Context API in React)
- {% aTargetBlank "https://pub.dev/documentation/flutter_cubit/latest/",
  "cubit" %} library
- {% aTargetBlank "https://docs.flutter.dev/cookbook/persistence/sqlite",
  "SQLite" %} database on the device

## Tests

TODO: How can widget tests be implemented?

TODO: How can end-to-end tests be implemented?

## Packages

The official package registry for Flutter is
{% aTargetBlank "https://pub.dev", "pub.dev" %}.
Search for packages or see categorized lists of
"Flutter Favorites", "Most popular packages", "Top Flutter packages",
"Top Dart packages", and "Package of the Week".

To install a package in a Flutter project,
enter `flutter pub add {package-name}`.
This downloads the code to the `~/.pub-cache/hosted` directory
which contains subdirectories like `pub.dartlang.org`.
This allows the downloaded code to be shared
between all of your Flutter projects.
It also updates the dependency list in the `pubspec.yaml` file,
which is the Flutter equivalent of a Node.js `package.json` file.

To upgrade the versions of all installed packages,
enter `flutter pub upgrade`.
To upgrade the version of a specific installed packages,
enter `flutter pub upgrade {package-name}`.

To remove an installed package, enter `flutter pub remove {package-name}`.

Dependencies can also be specified by manually editing the `pubspec.yaml` file.
When this is done, enter `flutter pub get` to download the new dependencies.

In each of the commands in this section,
the `flutter` command can be replaced by the `dart` command.

## Annoyances

- The VS Code Flutter extension displays a comment after the closing paren
  of all widgets. It isn't really in the code, but adds visual clutter.
  I haven't found a way to disable it yet.
