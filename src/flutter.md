---
eleventyNavigation:
  key: Flutter
layout: topic-layout.njk
---

## Overview

<img alt="Flutter logo" style="width: 40%"
    src="/blog/assets/flutter-logo.svg?v={{pkg.version}}"
    title="Flutter logo">

{% aTargetBlank "https://flutter.dev", "Flutter" %}, originally named "Sky",
enables writing mobile, web, desktop, and embedded apps
It uses the {% aTargetBlank "https://dart.dev", "Dart programming language" %}.
For details on the Dart programming language,
see this <a href="../dart/">blog page</a>.

Everything rendered by a Flutter app is rendered by a "widget".
There are many provided widgets and custom ones can be defined.

The Flutter framework is implemented in Dart,
except for the {% aTargetBlank "https://skia.org", "Skia" %}
2D graphics library which is implemented in C++.
Flutter is not the only user of Skia. From {% aTargetBlank
"https://www.chromium.org/developers/design-documents/graphics-and-skia",
"Graphics and Skia" %}, "Chrome uses Skia
for nearly all graphics operations, including text rendering."
From {% aTargetBlank "https://en.wikipedia.org/wiki/Skia_Graphics_Engine",
"Wikipedia" %}, "the library (Skia) is used as of 2021 in
Google Chrome, Chrome OS, Chromium OS, Mozilla Firefox,
Mozilla Thunderbird, Android, Firefox OS, LibreOffice, Flutter and Avalonia."

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

## Setup (macOS-specific)

- Install the Flutter SDK.

  - Update to the latest version of macOS.
  - Download an OS-specific version of Flutter from
    {% aTargetBlank "https://docs.flutter.dev/get-started/install", "here" %}.
  - Unzip the downloaded file.
  - Move the "flutter" directory your preferred directory.
  - Add the path to the `flutter/bin` directory
    to the `PATH` environment variable.
  - Later, to upgrade a project to the latest version, cd to the project
    and enter the following commands:
    - `flutter upgrade`
    - `flutter clean`
    - `flutter pub get`
    - `flutter pub upgrade`

- Install CocoaPods by entering `sudo gem install cocoapods`.

- To enable testing on iOS:

  - Install Xcode.

- To enable testing on Android:

  - Warning: This seems complicated and error prone!
  - Install {% aTargetBlank "https://developer.android.com/studio",
    "Android Studio" %}, even when using a different editor such as VS Code.
  - Select Android Studio ... Preferences ... Appearance & Behavior ...
    System Settings ... Android SDK ... SDK Tools.
  - Check "Android SDK Command-line Tools (latest).
  - Press "OK", "OK", and "Finish".
  - Enter `flutter doctor --android-licenses`

  - Install the Android SDK using Homebrew.

    - `brew tap homebrew/cask`
    - `brew install --cask android-sdk`

  - Enter `sdkmanager --install "cmdline-tools;latest"`

- Verify the installation by entering `flutter doctor`.

  - Fix any issues identified using the commands recommended in the output.
  - Run `flutter doctor` again to verify that all the issues have been resolved.
  - If you get the error "cmdline-tools component is missing",
    enter `sdkmanager --install "cmdline-tools" and then "latest".
  - If this gives the error
    "java.lang.NoClassDefFoundError: javax/xml/bind/annotation/XmlSchema",
    start "Android Studio", select Tools ... SDK Manager, select
    Appearance & Behavior ... System Settings ... Android SDK ... SDK Tools,
    check "Android SDK Command-line tools (latest)",
    and click the Apply button.
    Then from a terminal enter `flutter doctor --android-licenses`.
    There is no need to run the `sdkmanager` command again.

- To start the iOS Simulator:

  Enter `open -a simulator`.
  This works in Bash, zsh, and Fish, but not in Nushell.

- To start the iOS Simulator from VS Code:

  - Install the Flutter extension.
  - Select "Start iOS Simulator" from the device menu in the lower-right.

- To start an Android emulator from outside VS Code:

  - Launch the Android Studio app.
  - Select Tools ... AVD Manager or More Actions ... AVD Manager
  - To create a new virtual device:
    - Click the "+ Create Virtual Device" button at the bottom.
    - Select a device such as "Pixel 5" and click "Next".
    - Select an Android version such as "R" and click "Next".
    - Click "Finish".
  - To start an emulator, click a play button (green triangle)
    in the "Actions" column of one of the devices.

- To start an Android emulator from VS Code:

  - Install AVDs in Android Studio.
  - Restart VS Code.
  - Install the "Flutter" extension.
  - Select "Flutter: Launch Emulator" from the Command Palette.
  - Select a device type.
  - If the device name in the lower-right reads "Chrome (web javascript):",
    click it and then click "Enable android for this project".
    Click the device name again and click "Enable iOS for this project".

## Creating and Running an App

To create an app, enter `flutter create app_name`.
Hyphens are not allowed in app names, but underscores are.
This generates a basic counter app with a single page.
To generate a starter app with navigation to multiple pages
and a settings pages for selecting light/dark mode,
enter `flutter create --template=skeleton app_name`.

To run an app, cd to the app directory and enter `flutter run`.
If the iOS Simulator is running, the app will run there.
If an Android emulator is running, the app will run there.
Otherwise it will run in the Chrome web browser.
This will take about 1.5 minutes to start the first time it is run
and about 10 seconds after that.
But typically the app will remain running and
changes will be deployed with hot reload nearly instantly.

In the next section we will see how to
run a Flutter app from inside VS Code.

The default app renders a header that displays an app title,
some text, a number, and a button in the lower-right
that increments the number.

The starting point of the app is the file `lib/main.dart`.
This defines the `main` function that all Dart apps must have.

See the recommended linting configuration in the next section.

To avoid committing generated files to a Git repository,
edit `.gitignore` file,
replace the lines at the bottom that
begin with `/android` with just `/android`,
and add the line `/ios`.gitignore` file.

There are many things that can be deleted
from the file `lib/main.dart` including:

- all the comments
- the `_counter` property
- the `_incrementCounter` method
- the `floatingActionButton` argument to the `Scaffold` widget

Other changes that should be made include:

- Change the `title` argument to the `MaterialApp` widget.
- Change the `title` argument to the `MyHomePage` widget.
- Replace the widgets in the `children` `List` passed to the `Column` widget.

## Running From VS Code

To run a Flutter app from VS Code:

- Select a simulated or real device to use
  from a menu on the right side of the VS Code footer.
- Select a `.dart` file such as `lib/main.dart`.
- Select Run ... Start Debugging (F5) or
  Run ... Run Without Debugging (fn-ctrl-F5).

The first time this is run for a given application
it takes around a minute to start.

Sometimes it is necessary to change `build.gradle` files
used for Android builds.

The Kotlin version can be changed by modifying the following
in the file `android/build.gradle`:

```text
buildscript {
    ext.kotlin_version = '1.6.0'
    ...
}
```

The Flutter SDK version can be changed by modifing the following
in the file `android/app/build.gradle`:

```text
android {
   ...
    defaultConfig {
        ...
        minSdkVersion 21
        ...
    }
    ...
}
```

Running a Flutter app from VS Code opens the following floating toolbar:

<img alt="VS Code Flutter Toolbar" style="width: 80%"
    src="/blog/assets/flutter-vs-code-toolbar.png?v={{pkg.version}}"
    title="VS Code Flutter Toolbar">

The orange lightning bolt does a hot reload of the app.
The green circular arrow restarts the app.
The red square stops the app.
The blue magnifier glass opens the Widget Inspector
which gives details about widget nesting, layout, and sizes.

## Android Default Package

By default a new project uses the Java package `com.example`.
To change this, follow these steps:

- Create the new package directory structure below the
  `android/app/src/main/kotlin directory.
- Under this directory, move the file `com/example/MainActivity.kt`
  into the new package directory and modify its
  `package` statement to refer to the new package.
- From the top project directory, enter `flutter clean` and `flutter run`
  to rebuild the project with the new package name.

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
To avoid these and other warnings,
the following rule configuration is recommended.

```yaml
linter:
  rules:
    avoid_print: false
    prefer_const_constructors: false
    prefer_const_constructors_in_immutables: false
    prefer_const_literals_to_create_immutables: false
    prefer_single_quotes: true
```

## Code Formatting

Formatting of Dart code is provided by {% aTargetBlank
"https://dart.dev/tools/dart-format", "dart format" %}.
To get the best formatting, include a comma after every function parameter,
even the last one.

## Hot Reloading

Flutter has great hot reloading!
This loads code changes and preserves state.

If the app is run from a terminal with `flutter run`
then hot reloading will only occur if focus is moved to the terminal
and the "r" key is pressed.
To perform a "hot restart" which loads code changes,
but resets state to initial values, press "shift-r".

If the app is run from a compatible editor, the app will
automatically update after saving code changes without losing state.
Compatible editors include Android Studio, IntelliJ, VS Code, and emacs.

## Debugging

When a Flutter app is run from a terminal,
pressing "p" displays blue outlines of all widgets.
This is useful to understand the position and size of each widget.
Press "p" again to toggle this off.

## Running on Devices

To run a Flutter app on a connected iPhone:

- Attach the phone to the computer with a USB cable.
- Unlock the phone and "trust" it.
- From a terminal running bash, cd to the top project directory.
- Enter `open ios/Runner.xcworkspace` to launch Xcode.

- In Xcode

  - Select PROJECT ... Runner ... Inf and
    set "iOS Deployment Target" to the target iOS version.
  - Select "iPhone" from the device drop-down in the header.
  - Click "Runner" at the top of the Navigator.
  - Click the "Signing & Capabilities" tab.
  - Select your team from the Team drop-down.
    This can be your Apple ID.
    After entering this the first time, enter your Apple ID password.
  - Enter a unique "Bundle Identifier", perhaps containing
    your email address, a hyphen, and the project name.
    Underscores are automatically changed to hyphens.
  - option #1
    - In Xcode, press the run button (triangle) or cmd-r.
    - There will be several dialog prompts for your Mac password.
  - option #2
    - From a terminal, enter `flutter run`.
    - Select `iPhone` from the list of device options.
    - This gives the error "Unable to find a destination
      matching the provided destination specifier".

- In VS Code
  - Select "iPhone (ios)" from the device drop-down in the footer.
  - Select Run ... Run Without Debugging (ctrl-fn-F5).
  - At "Select Environment" prompt, select "Dart & Flutter".
  - This gives the error "Unable to find a destination
    matching the provided destination specifier".

Only Xcode option #1 has worked for me.
All the options take a couple of minutes to complete
the first time they are run for an app.

After the phone is ejected, the Flutter app will stop working on the device
unless it is built in "release" mode.
To do this in Xcode:

- Select Product ... Scheme ... Edit Scheme... which opens a dialog.
- Click "Run" in the left nav (selected by default).
- Select the "Info" tab (selected by default).
- Change "Build Configuration" to "Release".
- Click the "Close" button.
- Attach an iPhone to the computer with a USB cable.
- Select "iPhone" from the device drop-down in the header.
- Click the triangle run button.
- Wait for the project to build and copy to the iPhone.

To run a Flutter app on an iPhone wirelessly:

- The iPhone must be on the same WiFi network as the computer.
- In Xcode, select Window ... Devices and Simulators.
- In the dialog that is opened, select "iPhone" in the left nav.
  (I couldn't get my device to appear!)
- Check the "Connect via network" checkbox.
- In the Finder, eject the iPhone.
- Disconnect the iPhone from the computer.
- Run the Flutter app using one of the options above.

## VS Code

VS Code has great support for Dart and Flutter,
expecially when the extensions described below are installed.

### Dart Extension

Install the {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=Dart-Code.dart-code",
"Dart extension" %}.
This extension provides many things including:

- syntax highlighting
- code completion
- snippets
- refactoring and code fixes (see lightbulb icon)
- code formatting
- support for `editor.formatOnSave` and `editor.formatOnType`
- automatic hot reloads for Flutter
- switching between devices and simulators for Flutter
- automatic package installation and upgrades when `pubspec.yaml` is modified
- "Dart: Sort Members" command to sort methods (not properties) in a class
- "Organize Imports" command to order import statements

Consider adding the following in `settings.json`:

```json
  "[dart]": {
    "editor.codeActionsOnSave": {
      "source.fixAll": true
    },
    "editor.selectionHighlight": false,
    "editor.suggest.snippetsPreventQuickSuggestions": false,
    "editor.suggestSelection": "first",
    "editor.tabCompletion": "onlySnippets",
    "editor.wordBasedSuggestions": false
  },
```

### Flutter Extension

Install the {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=Dart-Code.flutter",
"Flutter extension" %}.
This uses the {% aTargetBlank
"https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server",
"Dart Analysis Server" %}, as does Android Studio,
to power many IDE operations.
This extension provides many things including:

- great auto-complete support

  Press ctrl-space when inside a function or method argument list
  to see a list of supported named arguments.

- snippets

  - `stless` adds template code for creating a stateless widget
  - `stful` adds template code for creating a stateful widget

  The code added by these requires importing a package that defines
  the `StatelessWidget` and `StatefulWidget` classes.
  These classes are described later.
  For example:

  ```dart
  import 'package:flutter/material.dart';
  ```

- phantom comments

  These appear after the last lines of multi-line widget constructor calls
  to make it easy to spot where they end.
  If you feel that these add too much visual clutter,
  disable them by adding the following line in `settings.json`:

  ```json
    "dart.closingLabels": false,
  ```

- Flutter-specific context menus

  Place the cursor on a widget and press cmd-period or click the lightbulb icon
  to open a context menu with the following options:

  - Remove this widget (keeps `child` and `children` widgets)
  - Move widget down
  - Move widget up
  - Wrap with Builder
  - Wrap with Center
  - Wrap with Column
  - Wrap with Container
  - Wrap with Padding
  - Wrap with Row
  - Wrap with SizedBox
  - Wrap with StreamBuilder
  - Wrap with widget... (prompts for name)
  - Extract Method (replaces selected code with a call )
  - Extract Local Variable
  - Extract Widget
  - Convert to StatefulWidget (only when on line declaring a StatelessWidget)

  There is no option to convert a `StatefulWidget` to a `StatelessWidget`
  because that would require deleting code that the developer should approve.

  The "Extract" commands are great for breaking up deeply nested widget trees!

  The "Extract Method" command creates a new method containing
  the selected code and replaces it with a call to the method.
  The convention for extracted method names is to begin with "\_build".

  The "Extract Local Variable" command creates a local variable
  that is initialized to the selected widget
  and replaces it with a reference to the variable.

  The "Extract Widget" command creates a new `Widget` subclass
  whose `build` method includes the selected widget (only one)
  and replaces it with an instance of the new widget.
  The new class definition can be moved to a new source file
  so it can be reused from multiple source files.

  Select multiple widgets and press cmd-period or click the lightbulb icon
  to open a context menu with the following options:

  - Wrap with Column
  - Wrap with Row
  - Extract Local Variable

- Flutter-specific entries in the status bar that include:

  - Flutter SDK version (ex Flutter 2.8.1)
  - device name being simulated (ex. iPhone 13 (ios simulator))
    that can be clicked to select a different device

- Flutter-specific command palette commands that include:

  - Change SDK
  - Clean Project
  - Focus on Outline View
  - Fix All

    To configure VS Code to "Fix All" automatically on save,
    all the following to `settings.json`:

    ```json
    "editor.codeActionsOnSave": {
      "source.fixAll": true
    },
    ```

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

- Installs and updates dependencies

  This happens automatically when changes to `pubspec.yaml` are saved.
  However, it will not recognize new classes and functions until
  "Developer: Reload Window" is selected from the Command Palette.

  Alternatively, a package can be installed and added in `pubspec.yaml`
  by entering `flutter pub add {package-name}` in a terminal.

To view the implementation code for Flutter classes, cmd-click a class name.
For example, try this on `MaterialApp`.
From this file, continue using cmd-click on other class names to see their code.

### Awesome Flutter Snippets Extension

It is recommended to install the {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=Nash.awesome-flutter-snippets",
"Awesome Flutter Snippets" %} extension.
This provides snippets for
"a collection of commonly used Flutter classes and methods."
Highlights are described below:

| Snippet                 | Description                              |
| ----------------------- | ---------------------------------------- |
| `build`                 | `build` method                           |
| `cupeapp`               | Cupertino app                            |
| `dis`                   | `dispose` method                         |
| `f-test`                | `test` function                          |
| `importC`               | import Cupertino package                 |
| `importFT`              | import Flutter Test package              |
| `importM`               | import Material package                  |
| `initS`                 | `initState` method                       |
| `listViewB`             | `ListView.builder` constructor call      |
| `listViewS`             | `ListView.separated` constructor call    |
| `mateapp`               | Material app                             |
| `statefulW`             | stateful widget classes                  |
| `statelessW`            | stateless widget class                   |
| `toStr`                 | `toString` method                        |
| `tweenAnimationBuilder` | `TweenAnimationBuilder` constructor call |
| `widgetTest`            | `testWidgets` function                   |

### Todo Tree Extension

It is recommended to install the {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=Gruntfuggly.todo-tree",
"Todo Tree" %} extension.
This extension adds a tree icon to the activity bar.
When clicked, it displays a directory tree in the navigator area
that shows files and lines within them that have comments that contain
"TODO", "FIXME" (or alternatives "FIXIT" and "FIX"), or "BUG".
Clicking an identified line opens the file and positions the cursor on the line.

## Packages

The official package registry for Flutter is
{% aTargetBlank "https://pub.dev", "pub.dev" %}.
It hosts a large number of packages and plugins.
The distinction between these is that
plugins provide integration with platform features such as cameras.
Sometimes packages are also referred to as libraries.

To find packages, search for them by name or browse the categorized lists of
"Flutter Favorites", "Most popular packages", "Top Flutter packages",
"Top Dart packages", and "Package of the Week".

Some popular packages from pub.dev are described
on my {% aTargetBlank "/blog/topics/#/blog/dart", "Dart" %} page.

To install a package in a Flutter project,
enter `flutter pub add {package-name}`.
This downloads the code to the `~/.pub-cache/hosted` directory
which contains subdirectories like `pub.dartlang.org`.
This allows the downloaded code to be shared
It also updates the dependency list in the `pubspec.yaml` file,
which is the Flutter equivalent of a Node.js `package.json` file.

To upgrade the versions of all installed packages,
enter `flutter pub upgrade`.
To upgrade the version of a specific installed packages,
enter `flutter pub upgrade {package-name}`.

To remove an installed package, enter `flutter pub remove {package-name}`.

Dependencies can also be specified by manually editing the `pubspec.yaml` file.
IDEs generally recognize the changes and
automatically install the new dependencies for you.
When using an editor that doesn't do this,
enter `flutter pub get` to download and install them.

In each of the commands in this section,
the `flutter` command can be replaced by the `dart` command.

## Basic Flutter App Structure

Here is an example of the structure of a basic Flutter app
that uses Material Design.

```dart
// This import is required to use the provided Material Design widgets.
import 'package:flutter/material.dart';

// This defines the starting point of all Dart apps.
// The object passed to the runApp function must be an instance
// of a class that extends StatelessWidget or StatefulWidget.
// When creating instances of immutable classes,
// constructor calls should be preceded by "const".
// This enables sharing references to instances create with
// the same arguments which optimizes memory.
// These instances are created at compile-time.
// All fields of immutable classes must be
// declared with the "final" keyword.
void main() => runApp(const MyApp());

// There is no need to create the MyApp class below.
// Instead, the MaterialApp widget it creates
// can be passed directly to the runApp function.

// This class is also used by tests.
// See the supplied test/widget_test.dart file.
class MyApp extends StatelessWidget {
  // All widget constructors take an optional parameter of type "Key".
  // Usually this argument is not needed.  Keys are discussed later.
  // Widget constructors can also have additional parameters.
  const MyApp({Key? key}) : super(key: key);

  // Widget build methods describe what a widget renders.
  // In stateless widgets it is only called once (true?).
  // In stateful widgets it is called initially
  // and again every time the state changes.
  // Typically the topmost widget build method returns
  // an instance of MaterialApp which follows Material Design.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      // This is a one-line description used by devices
      // to identify the app for users.
      // On Android titles appear above task manager app snapshots
      // displayed when users press the "recent apps" button.
      // On iOS this value is not used.
      title: 'My Title',
      // primarySwatch is a MaterialColor object that specifies
      // many shades of colors to be used throughout the app.
      theme: ThemeData(primarySwatch: Colors.blue),
      home: const MyPage(), // starting page
    );
  }
}

class MyPage extends StatelessWidget {
  const MyPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    // The Scaffold widget provides a basic UI structure
    // that is common to many mobile apps.
    return Scaffold(
      // The AppBar constructor accepts many more arguments
      // that are described later.
      appBar: AppBar(
        title: const Text('My App'),
      ),
      body: const Text('Hello, World!'),
    );
  }
}
```

The styling defined by the `ThemeData` object
that is passed to the `MaterialApp` constructor
can affect all other widgets in the app.
The ThemeData constructor takes a large number of arguments.
For details, see {% aTargetBlank
"https://api.flutter.dev/flutter/material/ThemeData/ThemeData.html",
"ThemeData constructor" %}.

To avoid drawing in unsafe areas such as those that contain notches,
wrap the `Scaffold` widget in a `SafeArea` widget.
However, this looks worse because the areas to the
left and right of the notch will just be black and empty.

Here is an example of the structure of a basic Flutter app
that uses Cupertino theming.

```dart
import 'package:flutter/cupertino.dart';

void main() => runApp(const MyApp());

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    print('MyApp.build entered');
    return const CupertinoApp(
      // This is needed to enable using some
      // Material widgets inside a Cupertion app.
      // Also wrap those widgets in a Material widget.
      localizationsDelegates: const <LocalizationsDelegate<dynamic>>[
        DefaultMaterialLocalizations.delegate,
        DefaultWidgetsLocalizations.delegate,
      ],
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

## Platform Detection

An app can detect the current platform and use that information
to render different widgets or implement different logic.

To get the platform, get a `ThemeData` object from `Theme.of(context)`.
That has a `platform` property which holds a `TargetPlatform` enum value.
Possible values include `android`, `fuchsia`,
`iOS`, `Linux`, `macOS`, and `windows`.
{% aTargetBlank "https://fuchsia.dev", "Fuchsia" %} is an
experimental operating system being developed by Google.

For example:

```dart
var platform = Theme.of(context).platform;
if (platform == TargetPlatform.iOS) {
  print('detected iOS');
}
```

Dart applications can get the platform using
`import 'dart.io' show Platform;`,
but this cannot be used in Flutter apps.

## Widgets

All the predefined widgets are documented in the {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets", "Widget Catalog" %}.

Flutter provides two sets of themed widgets,
one for Material Design and
one for iOS theming (referred to as "Cupertino").
The Cupertino widgets are described later.

Material widgets can be used on any platform, not just Android.
Some Material widgets are platform-ware meaning that
they are styled different on iOS versus Android.
However, some have Material look and feel on all devices, including iOS.

Material widgets are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/material",
"Material Component widgets" %}.

To use Material widgets in a Dart source file, add the following import:

```dart
import 'package:flutter/material.dart';
```

Cupertino widgets can also be used on all platforms.
However, due to licensing restrictions it won't
use the correct fonts on Android.

To use Cupertino widgets in a Dart source file, add the following import:

```dart
import 'package:flutter/cupertino.dart';
```

All widgets have a `build` method that is passed a {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/BuildContext-class.html",
"BuildContext" %} object.
This provides "a handle to the location of a widget in the widget tree."
It can be used to traverse up and down the widget tree.
Supposedly one use is for finding ancestor styles
so they can be applied to the current widget,
but it's not apparent how this can be done.

To run code after the `build` method of a widget has been called the first time,
pass a function to `WidgetsBinding.instance?.addPostFrameCallback`.
Often this is done in the `initState` method
of the `State` class for a `StatefulWidget`.

### Keys

All widget constructors must take an optional parameter
named "key" that has the type "Key".
Typically it is only necessary to specify keys when there is
a possiblity that multiple sibling widget instances of the same type
will be added, removed, or reordered in the widget tree.
This can only occur in a `StatefulWidget`.

Flutter keys have several uses:

- preserve state when widgets are added, removed, or reordered
- preserve scroll location

Flutter provides two kinds of keys, `LocalKey` and `GlobalKey`.
Local keys are only guaranteed to be unique within their parent widget.
Global keys are guaranteed to be unique across the entire app.
They are useful when the parent of widgets might change,
ensuring that their key will remain unique amoung new sibling widgets.
Global keys are also useful to render the same widget
on multiple pages of an app.

The `LocalKey` and `GlobalKey` classes serve as a
superclass to the key classes described in the following table:

| Key Superclass | Key Class          | Description                                                                      |
| -------------- | ------------------ | -------------------------------------------------------------------------------- |
| `LocalKey`     | `UniqueKey`        | automatically creates a unique key with no constructor argument                  |
| `LocalKey`     | `ValueKey`         | uniqueness is based on a value passed to the constructor                         |
| `LocalKey`     | `ObjectKey`        | uniqueness is based on the identity of an object passed to the constructor       |
| `GlobalKey`    | `LabeledGlobalKey` | constructor takes a `String` useful for debugging, but not used for key identity |
| `GlobalKey`    | `GlobalObjectKey`  | uniqueness is based on the identity of an object passed to the constructor       |

Use "value" keys when widgets can be uniquely identified by a single value.
For example, widgets that render information about people
might be uniquely identified by their mobile phone number.

Use "object" keys when widgets can be only be uniquely identified
by a the collection of values in objects.
For example, widgets that render information about dogs
might only be uniquely identified by the combination
of their name, breed, and owner.

Use `UniqueKey` to guarantee that all widgets are distinct
from all others regardless of their associate data
or position within the widget tree.

### Child Widgets

Widgets that accept other widgets as arguments typically have
a parameter named `child` with the type `Widget` for one
or `children` with a type of `List<Widget>` for multiple.
One possible reason that the type of `children` is not `Iterable<Widget>`
is that it would enable lazy instantiation which might
make it difficult to maintain 60 FPS refresh rates.

The `List` value of a `children` argument can use the `if` and `for` keywords.
The `if` keyword is used to conditionally add a child widget.
The `for` keyword is used to add multiple child widgets,
one for each element in an `Iterable`.
For example:

```dart
Column(
  children: [
    Text('Always present'),

    // The if condition must be followed by a single widget.
    // Curly braces are not allowed.
    if (score > 90) Text('Good job!'),

    // The for loop must be followed by a single widget.
    // Curly braces are not allowed.
    for (var student in students) Text(student.name),
  ],
)
```

Some widgets take a single widget in a `child` parameter and apply styling.
The Flutter documentation refers to these as "single-child layout widgets".
Examples include `Center`, `Container`, `Expanded`,
`Flexible`, `Padding`, and `SizedBox`.

Other widgets take multiple widgets in a `children` parameter
and lay them out in a specific way.
The Flutter documentation refers to these as "multi-child layout widgets".
Examples include `Column`, `GridView`, `ListView`,
`Row`, `Stack`, `Table`, and `Wrap`.

The `List.generate` static method can be used to
generate a `List` of widgets based on an index.
For example:

```dart
Column(children: List.generate(3, (index) => Text('${index + 1}'))),
```

### Stateless vs. Stateful Widgets

Every widget is either stateless or stateful.

Flutter-compatible editors provide snippets for defining new widgets
that dramatically reduce the amount of code that must be typed.
In a VS Code editor, enter "st" and select
"Flutter stateful widget" or "Flutter stateful widget".

In both cases the `build` method returns a single widget,
but that widget typically has additional child widgets.
This can return an empty `Container()` to render nothing.

#### Stateless Widgets

Stateless widgets are defined by a class that
extends `StatelessWidget`, defines a constructor,
and overrides the `build` method.
All properties in a `StatelessWidget` must be declared `final`
which makes them immutable.
Stateless widgets render one time based parameters passed to them.

New stateless widgets begin with the following code
which defines a single class:

```dart
class SomeName extends StatelessWidget {
  const SomeName({Key? key}) : super(key: key);

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
    src="/blog/assets/flutter-greet.png?v={{pkg.version}}"
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

#### Stateful Widgets

Stateful widgets render initially and again each time their state changes.

Stateful widgets are defined by {% aTargetBlank
"https://flutteragency.com/stateful-widgets-defined-as-two-classes/",
"two classes" %}.

The first class extends `StatefulWidget`,
defines final properties, defines a constructor, and
overrides the `createState` method to create an instance of the second class.
This class is immutable because all Flutter widgets must be immutable.
This helps Flutter performance when rebuilding the UI.

The second class extends `State`, defines state properties,
and overrides the `build` method.
It can be defined in the same source file as the first.
Storing the state in a separate class
allows the widget class to remain immutable.

By convention the state classes are private,
indicated by having a name that begins with an underscore.
When a class is private, there isn't a good reason
the makes its properties private,
so the property names do not need to begin with an underscore.

State instances hold a reference to their associated `StatefulWidget` instance
in the property `widget`.
This allows them to access properties in the widget instance.

There is an {% aTargetBlank "https://github.com/dart-lang/language/issues/329",
"on-going discussion" %} about whether it would be possible
to define stateful widgets in another way that
doesn't require defining two classes.

New stateful widgets begin with the following code
which defines a pair of related classes:

```dart
class SomeName extends StatefulWidget {
  const SomeName ({Key? key}) : super(key: key);

  @override
  _SomeNameState createState() => _SomeNameState();
}

class _SomeNameState extends State<SomeName> {
  @override
  Widget build(BuildContext context) {
    return Container(
      ...
    );
  }
}
```

Here is an example of a stateful widget that maintains a count
and provides buttons for incrementing and decrementing a count.
The `setState` method defined by the `State` class
is similar to the `setState` function in React.
This is described in more detail later in the "Managing State" section.

<img alt="Flutter Counter" style="width: 60%"
    src="/blog/assets/Flutter-Counter.png?v={{pkg.version}}"
    title="Flutter Counter">

```dart
class Counter extends StatefulWidget {
  final int initialValue;

  const Counter({Key? key, this.initialValue = 0}) : super(key: key);

  @override
  _CounterState createState() => _CounterState();
}

class _CounterState extends State<Counter> {
  var count = 0;

  static const textStyle = TextStyle(fontSize: 36);

  @override
  void initState() {
    super.initState(); // call at beginning of initState
    // Note the use of "widget." to refer to
    // properties in the associated stateful widget.
    count = widget.initialValue;
  }

  @override
  Widget build(BuildContext context) {
    return Row(children: [
      TextButton(
        child: const Text('-', style: textStyle),
        // The button is disabled when onPressed is null.
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

Lifecycle methods inherited from the `State` class that can be overridden,
listed in the order in which they are called,
are described in the following table:

| Method                  | Description                                                                                                      |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------- |
| `initState`             | performs one-time initialization of non-final properties that depends on the context or the widget               |
| `didChangeDependencies` | performs initialization involving inherited widgets which are widgets that "propagate information down the tree" |
| `build`                 | builds the UI for this widget; only method that MUST be overridden                                               |
| `didUpdateWidget`       | called after every call to `build`; rarely overridden                                                            |
| `reassemble`            | called in development when a hot reload occurs                                                                   |
| `deactivate`            | called when a widget subtree containing this `State` object is removed; rarely overridden                        |
| `dispose`               | called after `deactivate` unless the widget subtree is reinserted                                                |

The `initState` method should begin with `super.initState();`.

The `dispose` method should end with `super.dispose();`.
It is frequently used to call the `dispose` method on controller objects
such as `TextEditingController`.

### Material Structure Widgets

Every Flutter app needs to begin by rendering an "app" widget.
Two common choices are `MaterialApp` and `CupertinoApp`.
The app widget provides a place to store app-wide information such as
behavior flags, theming data, route data, the home widget, and more.
The app widget doesn't render anything and instead
delegates that resposibility to its home widget
which typically renders a scaffold.

A scaffold provides UI structure by defining a place to render
widgets like the following:

- app bar: typically contains a title and buttons
- bottom navigation bar: contains buttons for navigating between pages
- bottom sheet: slides up from the bottom; similar to a dialog
- drawer: typically slides in from the left
- floating action button: for primary action
- body: the main widget

It is common for Flutter apps to have the following top-level structure:

- `MaterialApp`
  - `Scaffold`
    - `AppBar`
      - toolbar row (logical grouping)
        - `leading` `Widget`
        - `title` `Widget`
        - `actions` `List<Widget>`
      - `flexible` `Widget`
      - `bottom` `Widget` (often contains a `TabBar`)
      - `BottomAppBar` (alternative to `TabBar` inside `AppBar`)
    - `drawer` `Widget` (typically is a `SizedBox` containing a `Drawer`)

The most commonly used application structure widgets are described below:

| Widget                                                                                                              | Description                                                                                             |
| ------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/AppBar-class.html", "AppBar" %}                           | appears at top of app; can contain "leading", "title", "actions" and a `TabBar`                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/BottomAppBar-class.html", "BottomAppBar" %}               | appears at bottom of app; contains buttons used to switch between top-level views                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/BottomNavigationBar-class.html", "BottomNavigationBar" %} | row of tappable icons and labels typically used to navigate to a page; styled for Material 2            |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Drawer-class.html", "Drawer" %}                           | panel that slides in from left (by default) and can contain navigation links                            |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/MaterialApp-class.html", "MaterialApp" %}                 | topmost widget; "wraps a number of widgets that are commonly required for material design applications" |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/NavigationBar-class.html", "NavigationBar" %}             | similar to `BottomNavigationBar`, but styled for Material 3; still renders at bottom                    |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Scaffold-class.html", "Scaffold" %}                       | provides app structure; can show `Drawer`, `SnackBar`, and bottom sheets (like modals)                  |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/TabBar-class.html", "TabBar" %}                           | horizontal row of tabs                                                                                  |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/TabBarView-class.html", "TabBarView" %}                   | a page that corresponds to a `TabBar` tab                                                               |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/TabPageSelector-class.html", "TabPageSelector" %}         | renders dots that indicate current carousel item; cannot click to switch                                |

See examples of using `ButtomNavigationBar` and `NavigationBar`
in the "Navigation" section below.

#### MaterialApp Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/MaterialApp-class.html",
"MaterialApp" %} widget is topmost widget
of applications that use Material Design.

The `MaterialApp` constructor takes the following named parameters and more:

| Parameter Name               | Description                                                                                                                                    |
| ---------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `darkTheme`                  | `ThemeData` to use when running in dark mode                                                                                                   |
| `debugShowCheckedModeBanner` | `bool` that determines if a diagonal DEBUG banner should be displayed in the upper-right corner when running in debug mode (`true` by default) |
| `home`                       | `Widget` to render for the default route ('/')                                                                                                 |
| `initialRoute`               | `String` name of first route to render when using `Navigator`                                                                                  |
| `routes`                     | `Map` of route names to `WidgetBuilder` instances                                                                                              |
| `theme`                      | `ThemeData` to use when running in light mode                                                                                                  |
| `title`                      | single-line `String` that describes the app; only rendered in Android task manager                                                             |

The most common properties to set in {% aTargetBlank
"https://api.flutter.dev/flutter/material/ThemeData-class.html", "ThemeData" %}
property are `primarySwatch` and `textTheme`.
`primarySwatch` is a `MaterialColor` object that specifies
many shades of colors to be used throughout the app.

To set the default color of icons, set the `iconTheme` property
of the `ThemeData` object used for the `theme` property
to an object like `IconThemeData(color: Colors.orange)`.
This does not affect the color of `IconButton` widgets.

For example:

```dart
theme: ThemeData(
  primarySwatch: Colors.blue,
  textTheme: TextTheme(
    // Material default text style
    bodyText2: TextStyle(color: Colors.purple),
  ),
),
```

#### Scaffold Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/Scaffold-class.html", "Scaffold" %}
widget "implements the basic material design layout structure."
The `MaterialApp` constructor `home` argument is typically set to a
custom widget whose `build` method returns an instance of the `Scaffold` class.
It is common for each "page" of an app to have its own `Scaffold`
so it can customize the `AppBar`.

The `Scaffold` constructor takes the following named parameters and more:

| Parameter Name         | Description                                                                                                          |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------- |
| `appBar`               | `AppBar` to display at the top                                                                                       |
| `backgroundColor`      | `Color` for background                                                                                               |
| `body`                 | `Widget` that provides the primary content                                                                           |
| `bottomNavigationBar`  | `Widget` to display at the bottom; typically an instance of `BottomNavigationBar`                                    |
| `drawer`               | `Widget` to slide in (usually from left) when hamburger icon is tapped; typically a `SizedBox` containing a `Drawer` |
| `floatingActionButton` | `Widget` (usually a `FloatingActionButton` to display over other content in lower-right                              |

When the `drawer` argument is provided, a hamburger menu icon is provided
on the left side of the `AppBar`.

#### AppBar Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/AppBar-class.html", "AppBar" %}
widget holds several other widgets referred to as
`leading`, `title`, `actions`, `flexibleSpace`, and `bottom`.
See the diagram in the
{% aTargetBlank "https://api.flutter.dev/flutter/material/AppBar-class.html",
"official documntation" %}.
The first row is referred to as the "toolbar" in documentation
and includes the `leading`, `title`, and `actions` widgets.

The `AppBar` constructor takes the following named parameters and more:

| Parameter Name     | Description                                                  |
| ------------------ | ------------------------------------------------------------ |
| `actions`          | `List<Widget>` displayed on right side of top row            |
| `backgroundColor`  | `Color` of background                                        |
| `bottom`           | `PreferredSizeWidget` displayed on bottom row (ex. `TabBar`) |
| `centerTitle`      | `bool` defaults to `false`, but typically want `true`        |
| `elevation`        | `double` defaults 4; set to 0 remove remove bottom shadow    |
| `flexibleSpace`    | `Widget` stacked behind the toolbar and the bottom widget    |
| `foregroundColor`  | `Color` of foreground                                        |
| `leading`          | `Widget` displayed on left side of top row                   |
| `leadingWidth`     | `double` width of `leading` `Widget`                         |
| `title`            | `Widget` displayed in center of top row                      |
| `titleSpacing`     | `double` space on left and right side of `title` `Widget`    |
| `titleTextStyle`   | `TextStyle` of `title` `Widget`                              |
| `toolbarHeight`    | `double` height of the toolbar (first row)                   |
| `toolbarTextStyle` | `TextStyle` of the toolbar widgets                           |

The `bottom` parameter is typically used for page navigation buttons
to be displayed near the top of the UI
as an alternative to the `Scaffold` `botttomNavigationBar`.

See the sample app in the GitHub repo {% aTargetBlank
"https://github.com/mvolkmann/flutter_appbar", "flutter_appbar" %}.

### Cupertino Structure Widgets

The {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/cupertino-library.html",
"cupertino library" %} defines widgets that implement iOS theming.

The {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoApp-class.html",
"CupertinoApp" %} widget is topmost widget in Cupertino apps.

The `CupertinoApp` constructor takes the following named parameters and more:

| Parameter Name | Description                                                                        |
| -------------- | ---------------------------------------------------------------------------------- |
| `color`        | primary `Color`                                                                    |
| `home`         | `Widget` to render for the default route ('/')                                     |
| `initialRoute` | `String` name of first route to render when using `Navigator`                      |
| `routes`       | `Map` of route names to `WidgetBuilder` instances                                  |
| `theme`        | `CupertinoThemeData` for global styling                                            |
| `title`        | single-line `String` that describes the app; only rendered in Android task manager |

The `home` `Widget` is typically either a {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoPageScaffold-class.html",
"CupertinoPageScaffold" %} or a {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoTabScaffold-class.html",
"CupertinoTabScaffold" %}.
It is common to use a `CupertionPageScaffold` inside a `CupertinoTabScaffold`.

The `CupertinoPageScaffold` constructor takes the following named parameters and more:

| Parameter Name    | Description                                                                          |
| ----------------- | ------------------------------------------------------------------------------------ |
| `backgroundColor` | `Color` for background                                                               |
| `child`           | `Widget` that provides the primary content                                           |
| `navigationBar`   | `Widget` to display at the bottom; typically an instance of `CupertinoNavigationBar` |

The `CupertinoTabScaffold` constructor takes the following named parameters and more:

| Parameter Name    | Description                                                                                                |
| ----------------- | ---------------------------------------------------------------------------------------------------------- |
| `backgroundColor` | `Color` for background                                                                                     |
| `tabBar`          | `CupertinoTabBar` to display at the bottom                                                                 |
| `tabBuilder`      | `IndexedWidgetBuilder` function that returns tab content for a given index; typically a `CupertinoTabView` |

Pages rendered for a specific tab can render `CupertinoPageScaffold` widgets
to enable navigating through a different stack of pages for each tab.

Note that neither Cupertino scaffold widget supports a `FloatingActionButton`.
To render one in a Cupertino app,
set the `CupertinoPageScaffold` `child` argument to a `Scaffold` widget
and set the `Scaffold` `floatingActionButton` argument.
For an example, see this {% aTargetBlank
"https://github.com/mvolkmann/flutter_floatingactionbutton", "GitHub repo" %}.

### Layout Widgets

The layout widgets are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/layout",
"Layout widgets" %}.

Some layout widgets accept a single `child`
and others accept multiple `children`.

When the children of a layout widget do not all fit within it,
diagonally yellow and black warning bars are displayed.
The example below shows what happens when a `Row` containing
six colored squares is rendered and
they do not all fit within the screen width.

<img alt="widget overflow" style="width: 60%"
    src="/blog/assets/flutter-widget-overflow.png?v={{pkg.version}}"
    title="widget overflow">

One way to fix widget overflow is to nest children in a {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/scrolling",
"widget that supports scrolling" %}.
These included `GridView`, `ListView`, `PageView`,
`ReorderableListView`, `SingleChildScrollView`, and more.

#### Single-child Layout Widgets

The most commonly used widgets for laying out a single child widget
are described below:

| Widget                                                                                                                 | Description                                                                                                                   |
| ---------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Align-class.html", "Align" %}                                 | specifies where its child should be positioned within its parent (ex. `Alignment.bottomRight`)                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedContainer-class.html", "AnimatedContainer" %}         | like `Container`, but "gradually changes its values over a period of time"; provides implicit animations                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AspectRatio-class.html", "AspectRatio" %}                     | sizes its child to a specific aspect ratio                                                                                    |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Center-class.html", "Center" %}                               | centers its child horizontally and vertically in the available space                                                          |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/ConstrainedBox-class.html", "ConstrainedBox" %}               | "imposes additional constraints on its child" which include min and max width and height                                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Container-class.html", "Container" %}                         | surrounds its child with optional `padding`, `decoration` (ex. `BoxDecoration` with optional border and shadow), and `margin` |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Expanded-class.html", "Expanded" %}                           | "expands a child of a Row, Column, or Flex so that the child fills the available space"                                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/FittedBox-class.html", "FittedBox" %}                         | "scales and positions its child within itself" including clipping                                                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Flexible-class.html", "Flexible" %}                           | "controls how a child of a Row, Column, or Flex flexes"; similar to CSS flex layout                                           |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/FractionallySizedBox-class.html", "FractionallySizedBox" %}   | "sizes its child to a fraction of the total available space"; can specify `widthFactor`, `heightFactor`, and `alignment`      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/LayoutBuilder-class.html", "LayoutBuilder" %}                 | provides min/max width/height constraints that can be used to decide how/what a child component should render                 |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/LimitedBox-class.html", "LimitedBox" %}                       | "box that limits its size only when unconstrained"; useful for wrapping unconstrained children of a `ListView`                |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/ListTile-class.html", "ListTile" %}                           | "single fixed-height row that typically contains some text as well as a leading or trailing icon"                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Padding-class.html", "Padding" %}                             | "insets its child by the given padding"                                                                                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/SingleChildScrollView-class.html", "SingleChildScrollView" %} | "box in which a single widget can be scrolled"                                                                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/SizedBox-class.html", "SizedBox" %}                           | "box with a specified size" for taking up space; "if given a child, forces it to have a specific width and/or height"         |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Transform-class.html", "Transform" %}                         | transforms its child by translating, rotating, and scaling it                                                                 |

Details for some of the single-child layout widgets are provided below.

##### Center Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Center-class.html", "Center" %}
widget centers its child in
the available space of the parent widget by default.
If the `heightFactor` argument is specified,
it will center the child in a height that is
the child height multiplied by the `heightFactor`.
The `widthFactor` argument works similarly,
but affects the width in which the child is centered.

##### Container Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Container-class.html", "Container" %}
widget is the most customizable single-child layout widget.
By default its size is the size of its only child.

The most frequently used arguments are described below:

| Argument      | Description                                                                                                |
| ------------- | ---------------------------------------------------------------------------------------------------------- |
| `alignment`   | `Alignment` that specifies the alignment of the `child` inside                                             |
| `color`       | `Color` of the background (conflicts with `decoration`)                                                    |
| `constraints` | `BoxConstraints` that constrains the minimum and maximum width and height                                  |
| `decoration`  | `BoxDecoration` that can specify a `color`, `border`, `borderRadius`, `boxShadow`, `gradient`, and `shape` |
| `height`      | `double` height of container (conflicts with `constraints`)                                                |
| `margin`      | `EdgeInsets` that specifies the margin to apply outside                                                    |
| `padding`     | `EdgeInsets` that specifies the padding to apply inside                                                    |
| `width`       | `double` width of container (conflicts with `constraints`)                                                 |

To consume all the available width in the parent,
set `width` to `double.infinity`.
Likewise, to consume all the available height in the parent,
set `height` to `double.infinity`.

The `decoration` argument takes a `BoxDecoration` object
that has a `shape` property.
The only supported values are `circle` and `rectangle`.

##### Expanded and Flexible Widgets

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Expanded-class.html", "Expanded" %}
widget extends from the {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Flexible-class.html", "Flexible" %}
widget.
Both take a `flex` argument that defaults to `1`
and works similarly to the CSS `flex` property.
The percentage of the available space given to
an `Expanded` or `Flexible` widget is the
percentage that their `flex` value represents out of
the total of the `flex` values in the same parent widget.

The `Expanded` and `Flexible` widgets can only be children
of a widget that extends from the `Flex` widget.
Currently the only provided widgets that do that are `Row` and `Column`.

One use of the `Expanded` widget is to push the widgets
that follow it inside a `Row` or `Column` to the end.
Another use is to scale an `Image` widget to fit in the available space.

The `Flexible` widget behaves nearly identically to the `Expanded` widget.
But a `Flexible` widget will take on
the size of its child if the child has a fixed size.
Otherwise it will choose its size just like `Expanded`.

##### ListTile Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/ListTile-class.html",
"ListTile" %} widget renders a fixed-height row that contains text
and optional leading and trailing widgets that are typically icons.
By default the text is left justified in the space that remains
after the leading and trailing widgets take their space.
It is frequently used inside a `List` widget.

The most commonly used constructor arguments are summaried below:

| Argument            | Description                                                                           |
| ------------------- | ------------------------------------------------------------------------------------- |
| `enabled`           | `bool` indicating whether it can be selected                                          |
| `contentPadding`    | `EdgeInsets` that defaults to `EdgeInsets.symmetric(horizontal: 16)`                  |
| `iconColor`         | `Color` of icons                                                                      |
| `isThreeLine`       | `bool` indicating whether extra vertical space should be reserved for the `subtitle`  |
| `leading`           | `Widget` placed at the beginning; typically an icon                                   |
| `onTap`             | `GestureTapCallback` function to call when tapped                                     |
| `selected`          | `bool` indicating whether it is selected                                              |
| `selectedColor`     | `Color` of background when selected                                                   |
| `selectedTileColor` | `Color` of background when selected                                                   |
| `subtitle`          | `Widget` placed in the center, below the `title`                                      |
| `textColor`         | `Color` of text                                                                       |
| `tileColor`         | `Color` of background when not selected                                               |
| `title`             | `Widget` placed in the center                                                         |
| `trailing`          | `Widget` placed at the end; typically an icon                                         |
| `visualDensity`     | `VisualDensity` constant that specifies "how compact the listt tile's layout will be" |

If the `subtitle` argument is set to a `Text` widget,
it will wrap to any number of lines and respects newline characters.
This allows each `ListTile` instance to have a different height.

If the `isThreeLine` argument is set to `true`,
space for a minimum of two lines of subtitle will be reserved
regardless of whether the subtitle needs two lines.

If the `enabled` argument is set to `false`,
the color of all the widgets inside is changed to light gray
and taps and long presses are ignored.

##### Padding Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Padding-class.html", "Padding" %}
widget adds padding to its `child` widget.
The constructor `child` argument specifies the `Widget` to be padded.

The constructor `padding` argument takes an `EdgeInsets` object
that specifies the amount of padding to add to each side.
There are several `EdgeInsets` named constructors summarized below.
All their arguments are `double` values that default to `0` when not specified.

| Constructor                                                   | Description                                 |
| ------------------------------------------------------------- | ------------------------------------------- |
| `EdgeInsets.all(value)`                                       | same on all four sides                      |
| `EdgeInsets.symmetric({horizontal = 0, vertical = 0})`        | same for left/right and same for top/bottom |
| `EdgeInsets.only({left = 0, right = 0, top = 0, bottom = 0})` | can differ on all four sides                |

Unlike the `Container` widget, the `Padding` widget
does not support specifying a background color.

##### SingleChildScrollView Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/SingleChildScrollView-class.html",
"SingleChildScrollView" %} widget allows a single child widget to be scrolled.
It is most commonly wrapped around a `Row` or `Column` widget.
When there is a need to scroll a large number of widgets vertically,
using `ListView` is more efficient because it only renders the visible widgets.

The following code demonstrates wrapping `SingleChildScrollView`
around both a `Row` and a `Column`.
It also demonstrates the alternative of using `ListView`.

<img alt="Flutter SingleChildScrollView" style="width: 40%"
    src="/blog/assets/flutter-singlechildscrollview.png?v={{pkg.version}}"
    title="Flutter SingleChildScrollView">

```dart
import 'package:flutter/material.dart';

extension WidgetExtension on Widget {
  /// Wraps a widget in a Container that has a border.
  Widget border({Color color = Colors.red}) {
    return Container(
      child: this,
      decoration: BoxDecoration(border: Border.all(color: color)),
    );
  }

  /// Wraps a widget in a Padding with given padding on all sides.
  Widget padding(double size) {
    return Padding(child: this, padding: EdgeInsets.all(size));
  }
}

const itemCount = 20;

void main() => runApp(
      MaterialApp(
        title: 'Flutter Layout',
        theme: ThemeData(
          primarySwatch: Colors.blue,
          textTheme: TextTheme(
            // Material default text style
            bodyText2: TextStyle(color: Colors.purple),
          ),
        ),
        home: const Home(),
      ),
    );

class Home extends StatefulWidget {
  const Home({Key? key}) : super(key: key);

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('My Demo'),
      ),
      body: Column(
        children: [
          ScrollableRow().border().padding(10),
          SizedBox(
            height: 200,
            width: double.infinity,
            child: ScrollableColumn(),
          ).border().padding(10),
          SizedBox(
            height: 200,
            child: MyListView(),
          ).border().padding(10),
        ],
      ),
    );
  }
}

class ScrollableRow extends StatelessWidget {
  const ScrollableRow({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      scrollDirection: Axis.horizontal,
      child: Row(
        children: <Widget>[
          for (var i = 1; i <= itemCount; i++) Text('Text #$i').padding(5),
        ],
      ),
    );
  }
}

class ScrollableColumn extends StatelessWidget {
  const ScrollableColumn({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: <Widget>[
          for (var i = 1; i <= itemCount; i++) Text('Text #$i').padding(5),
        ],
      ),
    );
  }
}

class MyListView extends StatelessWidget {
  const MyListView({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return ListView.builder(
      itemBuilder: (BuildContext context, int index) {
        return Text('Text #${index + 1}').padding(5);
      },
      itemCount: itemCount,
    );
  }
}
```

##### Transform Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Transform-class.html", "Transform" %}
widget applies transformations to its `child` widget.

The following code translates its `child` widget:

```dart
var dx = 100.0; // x values increase going right
var dy = 20.0; // y values increases going down
return Transform.translate(child: button, offset: Offset(dx, dy));
```

The following code rotates its child widget by 45 degrees counterclockwise:

```dart
Transform.rotate(angle: -pi / 4.0, child: someWidget);
```

The following code uses a transformation matrix
to translate, rotate, and scale a given widget:
It requires adding the "vector_math" package to `pubspec.yaml`.

```dart
import 'package:vector_math/vector_math_64.dart' as vm;

...

Widget transform({
  required Widget child,
  double angle = 0.0, // in radians
  double dx = 0.0,
  double dy = 0.0,
  double scale = 1.0,
}) {
  var translationVector = vm.Vector3(dx, dy, 0);
  var axis = vm.Vector3(0, 0, 1); // z axis
  var rotation = vm.Quaternion.axisAngle(axis, angle);
  // Use scale for both x and y, but no change in z.
  var scaleVector = vm.Vector3(scale, scale, 1);
  var matrix = Matrix4.compose(translationVector, rotation, scaleVector);
  return Transform(transform: matrix, child: child);
}

...

// In some widget list ...
transform(child: someWidget, dx: 100.0, dy: 20.0, angle: -pi / 4, scale: 0.7);
```

#### Multi-child Layout Widgets

The most commonly used widgets for laying out multiple child widgets
are described below:

| Widget                                                                                               | Description                                                                            |
| ---------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Column-class.html", "Column" %}             | arranges widgets vertically (like `HStack` in SwiftUI)                                 |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/GridView-class.html", "GridView" %}         | "scrollable, 2D array of widgets"                                                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/IndexedStack-class.html", "IndexedStack" %} | "shows a single child from a list", "the one with the given index"                     |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/ListView-class.html", "ListView" %}         | arranges widgets vertically like `Column`, but scrolls when needed                     |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Row-class.html", "Row" %}                   | arranges widgets horizontally (like `HStack` in SwiftUI)                               |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Stack-class.html", "Stack" %}               | stacks widgets on top of each other (like `ZStack` in SwiftUI)                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Table-class.html", "Table" %}               | similar to `GridView`, but is more configurable and doesn't scroll                     |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Wrap-class.html", "Wrap" %}                 | arranges widgets horizontally or vertically and wraps to a new row or column as needed |

Details for some of the multi-child layout widgets are provided below.

##### Column Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Column-class.html", "Column" %}
widget renders a vertical list of child widgets.
It is similar to a SwiftUI `VStack`.
The child widgets are specified in a `List` that is the value
of the `children` argument constructor argument.

The "main" axis is vertical and the "cross" axis is horizontal.

The constructor for this widget takes the following named parameters,
all of which are optional.

| Parameter            | Type                                                        |
| -------------------- | ----------------------------------------------------------- |
| `children`           | `List<Widget>` of child widgets                             |
| `crossAxisAlignment` | `CrossAxisAlignment` enum; defaults to `center`             |
| `mainAxisAlignment`  | `MainAxisAlignment` enum; defaults to `start`               |
| `mainAxisSize`       | `MainAxisSize` enum; defaults to `max`                      |
| `textDirection?`     | `TextDirection` enum; defaults to `ltr` for "left to right" |
| `verticalDirection`  | `VerticalDirection` enum; defaults to `down`                |

Typically only `children`, `crossAxisAlignment`,
`mainAxisAlignment`, and `mainAxisSize` are specified.

Values of the `CrossAxisAlignment` enum include
`start`, `center` (default), `end`,
`stretch`, and `baseline` (aligns text baselines).
These have a similar effect to the CSS `align-items` property.
The `stretch` value causes it to take all available space
in the cross-axis direction.

Values of the `MainAxisAlignment` enum include
`start` (default), `center`, `end`,
`spaceAround`, `spaceBetween`, and `spaceEvenly`.
These have a similar effect to the CSS `justify-content` property.

Values of the `MainAxisSize` enum include
`max` (uses all available height in parent widget; default) and
`min` (uses only what is needed to fit children).

The `textDirection` argument specifies the
order in which to layout children horizontally.
Values of the `TextDirection` enum include
`ltr` (left to right) and `rtl` (right to left).

The `verticalDirection` argument specifies the
order in which to layout children vertically.
Values of the `VerticalDirection` enum include `down` (default) and `up`.
When the value is `down`, child widgets are placed from top to bottom.
When the value is `up`, child widgets are placed from bottom to top.

There is no parameter that controls the space between the children.
This seems like a huge oversight,
especially since the `Wrap` widget takes a `spacing` argument!
One way to add space between them is to insert `SizedBox` widgets
that specify a `height` argument.
To make this easier, consider adding an extension to the `List<Widget>` type
as follows in a file named `widget_extension.dart`.

```dart
import 'package:flutter/material.dart';

extension WidgetExtension<Widget> on List<Widget> {
  /// Adds a SizedBox between all Widgets in the List.
  List<Widget> spacing(double size) {
    for (int i = length - 1; i > 0; i--) {
      insert(i, SizedBox(width: size, height: size) as Widget);
    }
    return this;
  }
}
```

To use this, call the `spacing` method on the value passed
in the `children` argument. For example, `[...].spacing(20)`.
The same approach can be used with the
`children` parameter of the `Row` widget.
Thanks to Pat Niemeyer for this suggestion!

##### ListView Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/ListView-class.html", "ListView" %}
widget displays a scrollable list of widgets
that are often `ListTile` widgets.
The list is vertical by default, but can be changed to horizontal
by setting the `scrollDirection` named argument to `Axis.horizontal`.

Scrolling widgets like `ListView` must
be inside a widget that has a constrained size.
This means a `ListView` cannot be a direct child
of a widget like `Row` or `Column`.
But it can be a child of a `SizedBox` widget or
an `Expanded` widget that is a child of a widget with a constrained size.

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
        height: 300,
        child: ListView.separated(
          itemCount: names.length,
          // This function is called once for each index value in itemCount
          // to programmatically create a child widgets.
          itemBuilder: (BuildContext context, int index) {
            return ListTile(
              contentPadding: EdgeInsets.all(0), // removes horizontal padding
              title: Text(names[index]),
              visualDensity: VisualDensity.compact,
            );
          },
          separatorBuilder: (_, index) => Divider(
            color: Colors.black45,
            height: 1,
          ),
        ),
      ),
    );
  }
}
```

To provide a visual indication of scroll position and
the number of items in a `ListView`, wrap it in a `Scrollbar` widget.

A `ListView` can be scrolled programmatically
by creating a `ScrollController` object and setting it as the `controller`.
For example:

<img alt="Flutter ListView with Controller" style="width: 40%"
    src="/blog/assets/flutter-listview-with-controller.png?v={{pkg.version}}"
    title="Flutter ListView with Controller">

```dart
    var controller = ScrollController();
    return Padding(
      padding: const EdgeInsets.all(10),
      child: Row(
        children: [
          SizedBox(
            height: 150,
            width: 100,
            child: Scrollbar(
              child: ListView(
                controller: controller,
                children: List.generate(
                  20,
                  (index) => Text('Item #${index + 1}'),
                ),
              ),
            ),
          ),
          ElevatedButton(
            child: Text('Top'),
            onPressed: () {
              controller.animateTo(
                0,
                duration: Duration(seconds: 1),
                curve: Curves.easeInOut,
              );
            },
          ),
        ],
      ),
    );
```

Flutter can optimize the rendering of scrollable lists
that contain many children with the same type using "deferred rendering".
Rather than creating all the child widgets
that will appear in the list up front,
it only creates the number that can be visible at the same time.
It reuses those widgets when the list is scrolled, by just changing their data.
To modify the previous example to take advantage of this optimization,
replace the lines that create the `ListView` with the following:

```dart
child: ListView.builder(
  controller: controller,
  itemBuilder: (context, index) => Text('Item #${index + 1}'),
  itemCount: 20, // omit for an infinite list
  itemExtent: 20, // height of each item
),
```

The way scrolling feels is defined by the `ScrollingPhysics` class.
This provides platform-specific scrolling
so it feels different between Android and IOS.
If desired this can be overridden.

##### Row Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Row-class.html", "Row" %}
widget renders a horizontal list of child widgets.
It is similar to a SwiftUI `HStack`.
The "main" axis is horizontal and the "cross" axis is vertical.
By default it takes all the available width of its parent because
its `mainAxisSize` argument defaults to `MainAxisSize.max`.

This constructor for widget takes the
same named parameters as the `Column` widget.

There is no parameter that controls the space between the children.
This seems like a huge oversight,
especially since the `Wrap` widget takes a `spacing` argument!
One way to add space between them is to insert `SizedBox` widgets
that specify a `width` argument.

##### Stack Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Stack-class.html", "Stack" %}
widget renders child widgets on top of each other
the first child on the bottom and the last child on top.
It is similar to a SwiftUI `ZStack`.

The width and height of a `Stack` matches that of its parent
if the parent has a fixed size.
Otherwise the width is the width of its widest child
and the height is the height of its tallest child.

The constructor for this widget takes the following named parameters,
all of which are optional.

| Parameter        | Type                                                                                             |
| ---------------- | ------------------------------------------------------------------------------------------------ |
| `alignment`      | `AlignmentDirectional` class constant; defaults to `topStart`                                    |
| `children`       | `List<Widget>` of child widgets                                                                  |
| `clipBehavior`   | `Clip` enum; specifies whether positioned children partially outside the `Stack` will be clipped |
| `fit?`           | `StackFit` enum; specifies how to size non-positioned children                                   |
| `textDirection?` | `TextDirection` enum; defaults to `ltr` for "left to right"                                      |

Values of the `AlignmentDirection` enum include  
`topStart`, `topCenter`, `topEnd`,  
`bottomStart`, `bottomCenter`, `bottomEnd`,  
`centerStart`, `center`, and `centerEnd`.

To position a child widget at a certain location inside a `Stack`,
wrap it in a `Position` widget whose constructor takes a `child` argument.
To set the horizontal position, specify either the `left` or `right` argument.
To set the vertical position, specify either the `top` or `bottom` argument.
Each specifies the distance from the given side.
To constrain the size the child, specify the `width` and `height` arguments.

##### Wrap Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Wrap-class.html", "Wrap" %}
widget renders widgets in rows or columns.
It is similar to the `Row` and `Column` widgets,
but differs in that it wraps children to multiple rows or columns
when they do not all fit in a single row or column.

The constructor for this widget takes the following named parameters,
all of which are optional.

| Parameter            | Type                                                                             |
| -------------------- | -------------------------------------------------------------------------------- |
| `alignment`          | main axis alignment within runs; `WrapAlignment` enum; defaults to `start`       |
| `children`           | `List<Widget>` of child widgets                                                  |
| `crossAxisAlignment` | cross axis alignment within runs; `WrapCrossAlignment` enum; defaults to `start` |
| `direction`          | `Axis` enum; defaults to `horizontal`                                            |
| `runAlignment`       | cross axis alignment of entire runs, `WrapAlignment` enum; defaults to `start`   |
| `runSpacing`         | space between runs; `double`; defaults to 0.0                                    |
| `spacing`            | space between children in a run; `double`; defaults to 0.0                       |
| `textDirection?`     | `TextDirection` enum; defaults to `ltr` for "left to right"                      |
| `verticalDirection`  | `VerticalDirection` enum; defaults to `down`                                     |

The `children` are divided into "runs" that fit in a single row or column.
The `alignment` and `spacing` parameters apply to the children of each "run".
The `runAlignment` and `runSpacing` parameters apply to entire runs.

Values of the `Axis` enum include `horizontal` (default) and `vertical`.

Values of the `WrapAlignment` enum include
`start` (default), `center`, `end`,
`spaceAround`, `spaceBetween`, and `spaceEvenly`.
These have a similar effect to the CSS `justify-content` property.

Values of the `WrapCrossAlignment` enum include
`start` (default), `center`, and `end`,
These have a similar effect to the CSS `align-items` property.

Values of the `TextDirection` and `VerticalDirection` enums were
described with the `Column` widget above.

### Spacer Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Spacer-class.html", "Spacer" %}
widget "creates an adjustable, empty spacer that can be used to
tune the spacing between widgets in a `Flex` container, like `Row` or `Column`.
It takes up all available space.

When there is more than one `Spacer` widget in a `Row` or `Column`,
the available space is divided between them.
By default they are all given the same amount of space,
but this can be changed by specifying an `int` `flex` argument.
This works similarly to CSS flexbox layout.

An alternative to using `Spacer` widgets is to specify
the `mainAxisAlignment` argument in a `Row` or `Column`
which accepts values like `spaceAround`, `spaceBetween`, and `spaceEvenly`.

### Display Widgets

The most commonly used widgets for displaying content are described below:

| Widget                                                                                                                          | Description                                                                                                         |
| ------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Banner-class.html", "Banner" %}                                        | "displays a diagonal message above the corner of another widget"; can specify the corner                            |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/CircleAvatar-class.html", "CircleAvatar" %}                           | "circle that represents a user"                                                                                     |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/CircularProgressIndicator-class.html", "CircularProgressIndicator" %} | rotating icon used to indicate that background activity is occurring, such as waiting for an API call to complete   |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Divider-class.html", "Divider" %}                                     | "thin horizontal line, with padding on either side"                                                                 |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/ErrorWidget-class.html", "ErrorWidget" %}                              | "renders an exception's message"; useful in an error dialog                                                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/ExpandIcon-class.html", "ExpandIcon" %}                               | "rotating expand/collapse button"; "rotates 180 degrees when pressed, then reverts the animation on a second press" |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/ExpansionPanelList-class.html", "ExpansionPanelList" %}               | list of `ExpansionPanel` widgets that can be expanded and collapsed by clicking                                     |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/FlutterLogo-class.html", "FlutterLogo" %}                             | renders the Flutter logo; can specify the size                                                                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Icon-class.html", "Icon" %}                                           | renders an icon; typically passed an `IconData` object from a constant in the `Icons` class                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Image-class.html", "Image" %}                                         | renders an image from a source such as a URL, local file, or `AssetBundle`                                          |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/LinearProgressIndicator-class.html", "LinearProgressIndicator" %}     | line used to indicate that background activity is occurring, such as waiting for an API call to complete            |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Placeholder-class.html", "Placeholder" %}                             | renders a rectangle containing diagonal lines that represents where future widgets will be placed                   |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/RichText-class.html", "RichText" %}                                   | renders runs of text that each use different styles; uses `TextSpan` objects                                        |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/SnackBar-class.html", "SnackBar" %}                                   | renders "a lightweight message with an optional action which briefly displays at the bottom of the screen"          |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Text-class.html", "Text" %}                                           | renders a run of text with a single style                                                                           |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Tooltip-class.html", "Tooltip" %}                                     | used to explain the functionality of a button or other tappable UI widget; press and hold to display                |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/VerticalDivider-class.html", "VerticalDivider" %}                     | "thin vertical line, with padding on either side"                                                                   |

The primary widgets for rendering text are `Text` and `RichText`.
Both automatically wrap their text if needed by default,
but this can be changed by setting their `overflow` argument to
a value from the `TextOverflow` enum.
These include `clip`, `ellipsis`, `fade`, and `visible`.

There are many more display widgets provided in pub.dev.
For example, the {% aTargetBlank "https://pub.dev/packages/badges",
"badges package" %} provides the `Badge` widgets.
For a short introduction, see this {% aTargetBlank
"https://www.youtube.com/watch?v=_CIHLJHVoN8", "YouTube video" %}.

Details for some of the display widgets are provided below.

#### CircleAvatar Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/CircleAvatar-class.html", "CircleAvatar" %}
widget displays an image representing a user in a circle.
The image can be specified using the `backgroundImage` or `child` arguments.
The following code demonstrates each option:

<img alt="Flutter CircleAvatar" style="width: 40%"
    src="/blog/assets/flutter-circleavatar.png?v={{pkg.version}}"
    title="Flutter CircleAvatar">

```dart
// This doesn't scale the image correctly,
// but will if wrapped in a Column widget.  Why?
CircleAvatar(
  backgroundImage: NetworkImage(
    'https://avatars.githubusercontent.com/u/79312?v=4',
    scale: 0.5, // does not scale the image!
  ),
  radius: avatarSize / 2,
),

// This scales the image correctly.
CircleAvatar(
  child: ClipOval(
    child: Image.network(
      'https://avatars.githubusercontent.com/u/79312?v=4',
      height: avatarSize,
      width: avatarSize,
    ),
  ),
  radius: avatarSize / 2,
),
```

#### CircularProgressIndicator and LinearProgressIndicator

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/CircularProgressIndicator-class.html",
"CircularProgressIndicator" %} and {% aTargetBlank
"https://api.flutter.dev/flutter/material/LinearProgressIndicator-class.html",
"LinearProgressIndicator" %} widgets
are both used to indicate that some activity,
such as an API call, is occuring in the background.
And both have determinant and indeterminant forms.
They use colors from `ThemeData.accentColor` by default,
but the colors can be customized.
For determinant progress indicators, set the `value` argument
to a state variable that is updated to a value between 0 and 1
when progress occurs.
The `CircularProgressIndicator` constructor takes a `strokeWidth` argument.

#### ExpandIcon and AnimatedContainer Widgets

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/ExpandIcon-class.html",
"ExpandIcon" %} widget renders an icon button that toggles between
a down and up pointing angle bracket when tapped by rotating it.
The `isExpanded` argument dictates the direction and
the `onPressed` argument can change the value inside a `StatefulWidget`.
This can be used in combination with an {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/AnimatedContainer-class.html",
"AnimatedContainer" %} to show and hide content.

The following code demonstrates hiding and showing a `Text` widget
by toggling its `height` between `0` and `20`:

```dart
ExpandIcon(
    isExpanded: isExpanded,
    onPressed: (_) {
      setState(() => isExpanded = !isExpanded);
    }),
AnimatedContainer(
  child: Text('I am showing!'),
  duration: Duration(milliseconds: 500),
  curve: Curves.easeInOut,
  height: isExpanded ? 20 : 0,
),
```

#### ExpansionPanelList Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/ExpansionPanelList-class.html",
"ExpansionPanelList" %} widget renders a vertical list
of `ExpansionPanel` widgets that can be expanded and collapsed by clicking.

For a short introduction, see this {% aTargetBlank
"https://www.youtube.com/watch?v=2aJZzRMziJc", "YouTube video" %}.

For example code, see this {% aTargetBlank
"https://github.com/mvolkmann/flutter_expansionpanel/blob/main/lib/main.dart",
"GitHub repo" %}.

#### Icon Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/Icon-class.html", "Icon" %}
widget renders an icon.
The icon to render is specified by the first positional argument
whose value must be an `IconData` object.
Typically these come from constants in the `Icons` class.
Other arguments commonly used inlude `color` and `size`.
The following example displays a large, red fire extinguisher icon.

<img alt="Flutter Icon" style="width: 10%"
  src="/blog/assets/flutter-icon-fire-extinguisher.png?v={{pkg.version}}"
  title="Flutter Icon">

```dart
Icon(Icons.fire_extinguisher, size: 100, color: Colors.red)
```

#### Image Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/Image-class.html", "Image" %}
widget renders an image.
Supported image formats include
BMP, GIF (including animated), JPEG, PNG, WBMP, and WebP.

The `width` and `height` arguments specify its size.

The `fit` argument specifies how the image
should be sized within the given space.
Options come from the `BoxFit` enum and include
`contain` (default; centers and scales, leaving blank space),
`cover` (centers and clips), and `fill` (changes aspect ratio).

The `semanticLabel` argument provides a text description
that is displayed by assistive technologies,
similar to the HTML `image` `alt` attribute.

The `Image` class supports many named constructors.

To render an image stored in the `assets` directory of a application:

1. Create an `assets` directory with an `images` directory inside it.
1. Add image files in this directory.
1. Modify `pubspec.yaml` so the following exists:
   ```yaml
   flutter:
     assets:
       - assets/images/
   ```
1. Use the `Image.asset` constructor. For example:
   ```dart
   Image.asset('assets/images/comet.jpg')
   ```

To render an image from a URL, use `Image.network(someUrl)`.
To provide an indication of the loading progress,
use a `LinearProgressIndicator`.
Images are cached, so the progress indicator will likely
only be displayed the first time the app requests the image.

For example:

```dart
Image.network(
  'https://avatars.githubusercontent.com/u/79312?v=4',
  width: 200,
  height: 200,
  fit: BoxFit.cover,
  loadingBuilder: (context, child, progress) {
    if (progress == null) return child;
    var bytes = progress.cumulativeBytesLoaded;
    var total = progress.expectedTotalBytes ?? 1;
    var percent = bytes / total;
    return LinearProgressIndicator(value: percent);
  },
)
```

#### SnackBar Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/SnackBar-class.html", "SnackBar" %}
widget renders "a lightweight message with an optional action
which briefly displays at the bottom of the screen."

The following code displays a `Snackbar` when a button is pressed.
This is typically done inside a `build` method
so a `BuildContext` object is available.

```dart
ElevatedButton(
  child: Text('Snack'),
  onPressed: () {
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(
        SnackBar(
          content: Text('Time for a snack!'),
          duration: Duration(seconds: 3), // defaults to 4 seconds
        ),
      );
  },
),
```

#### Text Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/Text-class.html", "Text" %}
widget renders text with a single style
including color, font, font size, and weight.

Highlights of the arguments that can be passed to the `Text` constructor
are described below:

| Argument    | Description                                                                           |
| ----------- | ------------------------------------------------------------------------------------- |
| `maxLines`  | `int` limit on number of lines to display; defaults to no limit                       |
| `overflow`  | `TextOverflow` enum with values `clip`, `ellipsis`, `fade`, and `visible`             |
| `style`     | `TextStyle` object or a property from `Theme.of(context).textTheme` (ex. `headline6`) |
| `textAlign` | `TextAlign` enum value                                                                |

The `TextAlign` enum that has the values
`left`, `center`, `right`, `justify`, `start`, and `end`.
When `textDirection` is `TextDirection.ltr`,
`start` has the same meaning as `left` and
`end` has the same meaning as `right`.

The `overflow` property only takes effect when `maxLines` is specified
and that number of lines is exceeded.

Highlights of the arguments that can be passed to the `TextStyle` constructor
are described below:

| Argument          | Description                                                                 |
| ----------------- | --------------------------------------------------------------------------- |
| `backgroundColor` | `Color` of background                                                       |
| `color`           | `Color` of foreground                                                       |
| `decoration`      | `TextDecoration` constant `none`, `underline`, `lineThrough`, or `overline` |
| `fontFamily`      | `String` name of a font family                                              |
| `fontSize`        | `double` font size                                                          |
| `fontStyle`       | `FontStyle` enum value `normal` or `italic`                                 |
| `fontWeight`      | `FontWeight` constant `normal`, `bold`, or `wn00` where n is 1-9            |
| `shadows`         | `List<Shadow>` describing one or more shadows to be applied                 |

Lines created by the `decoration` argument can be styled with the
`decorationColor` (takes a `Color`),
`decorationStyle` (takes a `TextDecorationStyle` enum value), and
`decorationThickness` (takes a `double`) arguments.
The values of the `TextDecorationStyle` enum include
`dashed`, `dotted`, `double`, `solid`, and `wavy`.

The `Shadow` constructor takes the arguments `color` (a `Color`),
`offset` (an `Offset` object with `dx` and `dy` properties),
and `blurRadius` (a `double`).

#### RichText Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/RichText-class.html", "RichText" %}
widget renders runs of text that can each have a different style.
It takes a `text` argument whose value is a `TextSpan` widget
with a `children` argument that is typically a `List` of `TextSpan` widgets.
The top `TextSpan` widget can specify the
default `style` for the child `TextSpan` widgets.
Typically the text color must be specified because it defaults to white.
The child `TextSpan` widgets can override that style.

Highlights of the arguments that can be passed to the `RichText` constructor
are described below:

| Argument       | Description                                                               |
| -------------- | ------------------------------------------------------------------------- |
| `maxLines`     | `int` limit on number of lines to display; defaults to no limit           |
| `text`         | `InlineSpan` object (see commonly used subclasses below)                  |
| `textAlign`    | `TextAlign` enum value                                                    |
| `textOverflow` | `TextOverflow` enum with values `clip`, `ellipsis`, `fade`, and `visible` |

Highlights of the arguments that can be passed to the `TextSpan` constructor
are described below:

| Argument     | Description                                                          |
| ------------ | -------------------------------------------------------------------- |
| `text`       | `String` text                                                        |
| `children`   | `List<InlineSpan>` of additional objects that implement `InlineSpan` |
| `recognizer` | `GestureRecognizer` object such as a `TapGestureRecognizer`          |
| `style`      | `TextStyle` object (described earlier)                               |

If the `style` argument is omitted or doesn't specify a color,
the text is invisible.
It's possible that the color defaults to the background color,
but I haven't found any documentation to confirm this.

Classes that extend `InlineSpan` include `TextSpan` and `WidgetSpan`.
`TextSpan` objects represent a tree of text and
`WidgetSpan` objects represent a tree of widgets.
This means it is possible for a `RichText` widget to render
runs of both text and other widgets.
The `WidgetSpan` constructor takes
a `child` argument that specifies a widget to render and
an `alignment` argument that specifies how to align the widget
vertically with respect to text specified in `TextSpan` objects.

Providing a `List` of text spans enables specifying different styling for each.
The styling of each text span defaults to the styling of its parent.

The optional `recognizer` argument is typically set to
a `TapGestureRecognizer` which extends `GestureRecognizer`.
Its constructor takes an `onTap` argument
which is a function to call when a tap is detected.

#### Tooltip Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/Tooltip-class.html", "Tooltip" %}
widget briefly describe the functionality of
a button or other tappable UI widget.
To see a tooltip, users must press and hold on a widget that has a tooltip.

Highlights of the arguments that can be passed to the `Tooltip` constructor
are described below:

| Argument       | Description                                                                                                      |
| -------------- | ---------------------------------------------------------------------------------------------------------------- |
| `child`        | `Widget` to which the `Tooltip` belongs                                                                          |
| `message`      | `String` text to display                                                                                         |
| `preferBelow`  | `bool` indicating whether the tooltip should be displayed below rather than above the widget; defaults to `true` |
| `richMessage`  | `InlineSpan` rich text to display instead of plain text                                                          |
| `showDuration` | `Duration` that the tooltip should be displayed after long press is released; defaults to 1.5 seconds            |
| `triggerMode`  | `TooltipTriggerMode` enum value of `longPress` (default), `tap`, or `manual`                                     |

The following code creates a button that has a tooltip:

```dart
Tooltip(
  child: ElevatedButton(
    child: Text('Press Me'),
    onPressed: () => print('got ElevatedButton press'),
  ),
  message: 'I am a tooltip!',
)
```

The `FloatingActionButton`, `IconButton`, and `PopupMenuButton` widgets
all takes a `tooltip` argument, which simplifies adding a tooltip.

The `triggerMode` can only be set to `tap` for widgets
that do not have an `onPressed` argument value.

### Input Widgets

| Widget                                                                                                                | Description                                                                                                                        |
| --------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Autocomplete-class.html", "Autocomplete" %}                 | text input for selecting from a list of options where only options that match the entered text are displayed                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/BackButton-class.html", "BackButton" %}                     | "<" button that defaults to calling `Navigator.maybePop` when pressed                                                              |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/CloseButton-class.html", "CloseButton" %}                   | "X" button that defaults to calling `Navigator.maybePop` when pressed                                                              |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/CalendarDatePicker-class.html", "CalendarDatePicker" %}     | inline calendar date picker                                                                                                        |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Checkbox-class.html", "Checkbox" %}                         | checkbox for toggling a `bool` value                                                                                               |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/DropdownButton-class.html", "DropdownButton" %}             | dropdown menu with `DropdownMenuItem` children similar to the HTML `select` element with `option` children                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/EditableText-class.html", "EditableText" %}                  | from the `TextField` docs, "EditableText ... is the raw text editing control at the heart of a TextField ... rarely used directly" |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/ElevatedButton-class.html", "ElevatedButton" %}             | button containing any widget with a background color whose elevation increases when pressed                                        |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/FloatingActionButton-class.html", "FloatingActionButton" %} | circular (typically) button "that hovers over other content to promote a primary action"                                           |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Form-class.html", "Form" %}                                  | container for grouping form fields; see `autovalidateMode` property and `createState` method                                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/IconButton-class.html", "IconButton" %}                     | button containing an `Icon`                                                                                                        |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/OutlinedButton-class.html", "OutlinedButton" %}             | a `TextButton` with an outlined border                                                                                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/PopupMenuButton-class.html", "PopupMenuButton" %}           | similar to `DropdownButton`, but displays an ellipsis instead an upside down caret                                                 |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Radio-class.html", "Radio" %}                               | radio button for selecting between mutually exclusive options                                                                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/RangeSlider-class.html", "RangeSlider" %}                   | slider for selecting the minimum and maximum values from a range of `double` values                                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/ReorderableList-class.html", "ReorderableList" %}            | scrollable container where child widgets can be dragged to reorder them                                                            |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Slider-class.html", "Slider" %}                             | slider for selecting a double value from a given range of `double` values                                                          |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Stepper-class.html", "Stepper" %}                           | "displays progress through a sequence of steps" like in a wizard UI; not frequently used                                           |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Switch-class.html", "Switch" %}                             | toggle switch for selecting a `bool` value                                                                                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/TextButton-class.html", "TextButton" %}                     | button containing any widget, not just `Text`, with no visible border                                                              |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/TextField-class.html", "TextField" %}                       | basic text input                                                                                                                   |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/TextFormField-class.html", "TextFormField" %}               | like `TextField`, but supports validation                                                                                          |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/ToggleButtons-class.html", "ToggleButtons" %}               | set of toggle buttons, typically used to choose between exclusive options                                                          |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/YearPicker-class.html", "YearPicker" %}                     | scrollable list of years to pick from (I can't get this to work!)                                                                  |

Many of these widgets render buttons, including
`DropDownButton`, `ElevatedButton`, `FloatingActionButton`,
`IconButton`, and `TextButton`.
Flutter 2.0 renamed three button widgets.

```text
FlatButton    -> TextButton
RaisedButton  -> ElevatedButton
OutlineButton -> OutlinedButton
```

Basic usage of all of these widgets is demonstrated in the Flutter project at
{% aTargetBlank "https://github.com/mvolkmann/flutter_input",
"flutter_input" %}.

#### ElevatedButton Widget

The most commonly used button widget is {% aTargetBlank
"https://api.flutter.dev/flutter/material/ElevatedButton-class.html",
"ElevatedButton" %}.

The most commonly used constructor arguments are summaried below:

| Argument    | Description                                                        |
| ----------- | ------------------------------------------------------------------ |
| `child`     | a `Widget` to render inside the button; typically `Text` or `Icon` |
| `key`       | a `Key` often used for finding the widget in a test                |
| `onPressed` | function to call when the button is tapped                         |
| `style`     | a `ButtonStyle` specifying styling details                         |

The `style` argument can specify background color, foreground color,
font size, shadow, and more.
The easiest way to specify the `style` value is
with the `ElevatedButton.styleFrom` function.

The following code demonstrates creating an `ElevatedButton`:

```dart
ElevatedButton(
  child: const Text('Press Me'),
  onPressed: () => print('got press'),
  style: ElevatedButton.styleFrom(
    padding: EdgeInsets.all(10),
    primary: Colors.red, // background color
    onPrimary: Colors.yellow, // foreground color
    textStyle: TextStyle(fontSize: 30),
  ),
)
```

#### FloatingActionButton Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/FloatingActionButton-class.html",
"FloatingActionButton" %} widget "is a circular icon button
that hovers over content to promote a primary action".
Typically these are used as the value of the
`Scaffold` `floatingActionButton` argument.

The most commonly used constructor arguments are summaried below:

| Argument          | Description                                                                    |
| ----------------- | ------------------------------------------------------------------------------ |
| `backgroundColor` | `Color` of button background                                                   |
| `child`           | `Widget` to render inside button                                               |
| `foregroundColor` | `Color` of button foreground                                                   |
| `mini`            | `bool` indicating if a smaller version should be rendered; defaults to `false` |
| `onPressed`       | `VoidCallback` function to call when pressed                                   |

To customize the location of the button, specify the
`Scaffold` `floatingActionButtonLocation` argument
which takes constant defined in the {% aTargetBlank
"https://api.flutter.dev/flutter/material/FloatingActionButtonLocation-class.html",
"FloatingActionButtonLocation" %} class.
Most of the constants place the button near the bottom of the screen,
including the default of `endFloat` which places it in the lower-right corner.
Constants with names names that end in "Top"
place the button near the top of the screen.
Constants with names that begin with "mini"
do not reduce the size of the button.
Instead the correctly position the button
when its `mini` argument is set to `true`.

The named constructor `FloatingActionButton.large` is similar to
the normal constructor, but creates a ridulously large button.

The named constructor `FloatingActionButton.extended`
removes the `child` argument, and adds the `icon` and `label` arguments.
Rather than creating a circle button, tt creates an oval button
in order to accommodate an icon on the left and a label on the right.

Typically there is one `FloatingActionButton` per `Scaffold`.
To have more than one, wrap them in a `Row` or `Column`.
The following code creates two `FloatingActionButton` widgets
and places them in the lower-left and lower-right corners of the screen.

```dart
// Scaffold arguments
floatingActionButton: Padding(
  padding: const EdgeInsets.symmetric(horizontal: 30),
  child: Row(
    mainAxisAlignment: MainAxisAlignment.spaceBetween,
    children: [
      FloatingActionButton(
        child: Text('FAB1'),
        heroTag: 'fab1',
        onPressed: () { ... },
      ),
      FloatingActionButton(
        child: Text('FAB2'),
        heroTag: 'fab2',
        onPressed: () { ... },
      ),
    ],
  ),
),
floatingActionButtonLocation: FloatingActionButtonLocation.centerDocked,
```

When a page contains more than one `FloatingActionButton`,
each must be given a unique `heroTag` `String` value.

It is also possible to use `FloatingActionButton` widgets
outside of the `Scaffold` `floatingActionButton` argument
like any other widget.

#### TextField and TextFormField Widgets

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/TextField-class.html",
"TextField" %} and {% aTargetBlank
"https://api.flutter.dev/flutter/material/TextFormField-class.html",
"TextFormField" %} widget constructors take many optional arguments.
The highlights are described in the following table:

| Argument          | Description                                                           |
| ----------------- | --------------------------------------------------------------------- |
| `autoCorrect`     | `bool` indicating of the value should be auto-corrected               |
| `controller`      | `TextEditingController` (described later)                             |
| `decoration`      | `InputDecoration` that specifies styling details                      |
| `inputFormatters` | `List<TextInputFormatter>?` input validation                          |
| `keyboardType`    | `TextInputType` that requests a certain kind of on-screen keyboard    |
| `maxLength`       | `int` maximum length defaulting to no limit                           |
| `maxLines`        | `int` defaulting to `1`                                               |
| `obscureText`     | `bool` indicating of the value should be obscured (ex. for passwords) |
| `onChanged`       | function called with new value when the user changes it               |
| `readOnly`        | `bool` indicating if the value cannot currently be modified           |
| `style`           | `TextStyle` of text being edited                                      |

Specify the `maxLength` argument causes the number of characters entered
and the maximum length to be displayed below the input, right justified.
For example, "4/10" means 4 characters have been entered out of a maximum of 10.

The `TextFormField` constructor takes the following additional optional arguments.

| Argument       | Description                                                        |
| -------------- | ------------------------------------------------------------------ |
| `initialValue` | `String` initial value                                             |
| `validator`    | function called when value changes (see "Form Validation" section) |

The `InputDecoration` class specifies styling to be applied
described by optional arguments passed to its constructor.
The highlights are described in the following table:

| Argument     | Description                                                  |
| ------------ | ------------------------------------------------------------ |
| `border`     | `InputBorder` describing a kind of border to be added        |
| `errorText`  | `String` displayed below the input, typically in red         |
| `helperText` | `String` displayed below the input when there is `errorText` |
| `hintText`   | `String` displayed inside the input when no value is entered |
| `icon`       | `Widget` displayed before the input                          |
| `label`      | `Widget`                                                     |
| `labelText`  | `String` that indicates the meaning of the input             |
| `prefix`     | `Widget` displayed inside the input before entered text      |
| `prefixIcon` | `Widget` displayed inside the input before entered text      |
| `prefixText` | `String` displayed inside the input before entered text      |
| `suffix`     | `Widget` displayed inside the input after entered text       |
| `suffixIcon` | `Widget` displayed inside the input after entered text       |
| `suffixText` | `String` displayed inside the input after entered text       |

When `errorText` is set, the border and label (if any) become red.
Set this to `null` to indicate that the value is valid
and no error message should be displayed.

It is not valid to specify both `prefix` and `prefixText`.
Likewise, it is not valid to specify both `suffix` and `suffixText`.

The `TextInputType` class defines static properties
for various kinds of on-screen keyboards.
These properties include `datetime`, `emailAddress`, `multiline`, `name`,
`none` (no on-screen keyboard), `number`, `phone`, `streetAddress`,
`text`, `url`, and `visiblePassword`.
For example, to get an on-screen keyboard that is
optimized for entering numbers,
specify `keyboardType: TextInputType.number`.

The following code demonstrates creating a `TextFormField`
for entering a dollar amount.
It uses `prefix` and `suffix` widgets.

<img alt="Flutter TextFormField with prefix and suffix" style="width: 60%"
    src="/blog/assets/flutter-textformfield-prefix-suffix.png?v={{pkg.version}}"
    title="Flutter TextFormField with prefix and suffix">

```dart
    TextFormField(
      decoration: InputDecoration(
        border: OutlineInputBorder(),
        contentPadding: EdgeInsets.zero,
        helperText: 'Enter the price in whole U.S. dollars.',
        hintText: 'price in USD',
        labelText: 'Price',
        prefix: Container(
          child: MyText('\$', color: Colors.white),
          color: Colors.green,
          margin: EdgeInsets.only(right: 10),
          padding: EdgeInsets.all(10),
        ),
        suffix: Container(
          child: MyText('.00', color: Colors.white),
          color: Colors.green,
          margin: EdgeInsets.only(left: 10),
          padding: EdgeInsets.all(10),
        ),
      ),
      initialValue: '0',
      inputFormatters: <TextInputFormatter>[
        FilteringTextInputFormatter.digitsOnly,
      ],
      // This is not enough to restrict the value entered.
      // inputFormatters must also be specified.
      keyboardType: TextInputType.number,
      maxLength: 7,
    );
```

The `icon`, `prefixIcon`, and `suffixIcon` arguments
can be set to any widget, not just an `Icon`.
For example, they can be be `IconButton` widgets
in order to execute code when they are tapped.
A ternary operator can be used to display different icons
based on the current state.

The following code demonstrates creating a custom widget
for entering passwords that allows the user to
toggle between obscuring and showing the value.
The `suffixIcon` is an `IconButton` that toggles the value of `obscureText`.

```dart
import 'package:flutter/material.dart';

typedef ValidatorFn = String? Function(String?)?;

class MyPasswordField extends StatefulWidget {
  final String initialValue;
  final String labelText;
  final void Function(String) onChanged;
  // Optional function for validating the entered password.
  final ValidatorFn validator;

  const MyPasswordField({
    Key? key,
    this.initialValue = '',
    this.labelText = '',
    this.validator,
    required this.onChanged,
  }) : super(key: key);

  @override
  _MyPasswordFieldState createState() => _MyPasswordFieldState();
}

class _MyPasswordFieldState extends State<MyPasswordField> {
  var obscure = true;

  @override
  Widget build(BuildContext context) {
    var validator = widget.validator;
    return TextFormField(
      decoration: InputDecoration(
        border: OutlineInputBorder(),
        labelText: widget.labelText,
        suffixIcon: IconButton(
          // The icon will either be an eye to show the text
          // or an eye with a slash through it to obscure the text.
          icon: Icon(obscure ? Icons.visibility : Icons.visibility_off),
          onPressed: () {
            setState(() => obscure = !obscure);
          },
        ),
      ),
      initialValue: widget.initialValue,
      obscureText: obscure,
      onChanged: (String value) => widget.onChanged(value),
      validator: validator,
    );
  }
}
```

Instances of the {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/TextEditingController-class.html",
"TextEditingController" %} class can be passed to the
`TextField` and `TextFormField` constructors in their `controller` argument.
These objects specify the initial value,
hold the current value (in their `text` property),
can return the currently selected text, can clear the value, and
support adding listeners that are notified when the value changes.
Note that `TextField` and `TextFormField` widgets
are not required to have a controller.
They can instead obtain new values using an `onChanged` callback.

On the surface it seems the main reason to prefer
using `TextFormField` over `TextField` is to
place it inside a `Form` and have validation.
But another consideration is the ability to specify an initial value.
The only way to do this with at `TextField` is to use a `TextEditingController`
which can also be done with a `TextFormField`.

The following code demonstrates using a `TextField` with an initial value
that is specified using a `TextEditingController`.
When the user types a new value, the `Text` below it is updated.
The `TextField` contains an `IconButton` that when clicked clears the value.
This code and the related code that follows can be found in {% aTargetBlank
"https://github.com/mvolkmann/flutter_testfield_vs_textformfield", "GitHub" %}.

<img alt="Flutter TextField" style="width: 60%"
    src="/blog/assets/flutter-textfield.png?v={{pkg.version}}"
    title="Flutter TextField">

```dart
import 'package:flutter/material.dart';

class Greet1 extends StatefulWidget {
  const Greet1({Key? key}) : super(key: key);

  @override
  _Greet1State createState() => _Greet1State();
}

class _Greet1State extends State<Greet1> {
  final tec = TextEditingController(text: 'Mark'); // initial value

  @override
  void initState() {
    super.initState();
    // This triggers a rebuild of the widget
    // so the suffixIcon can be reevaluated.
    tec.addListener(() => setState(() {}));
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        TextField(
          controller: tec,
          decoration: InputDecoration(
            border: OutlineInputBorder(),
            labelText: 'Name',
            suffixIcon: tec.text.isEmpty
                ? Container(width: 0)
                : IconButton(
                    icon: Icon(Icons.close, size: 18),
                    onPressed: () => setState(() => tec.text = ''),
                  ),
          ),
        ),
        // This is needed to listen for changes in the TextEditingController.
        ValueListenableBuilder<TextEditingValue>(
          valueListenable: tec,
          builder: (context, value, child) {
            return Text(tec.text.isEmpty ? '' : 'Hello, ${tec.text}!');
          },
        ),
      ],
    );
  }
}
```

The following code demonstrates the same functionality as above,
but using a `TextFormField`.
Since this code is less complex, it can be preferrable
to use `TextFormField` even when validation is not needed.
However, if there is a need to update the `initialValue`,
this will not work because the initial value cannot be changed.
Unlike in the previous example, when the `IconButton` is tapped,
the value in the `TextFormField` is not cleared.
This can only be done when using a `TextEditingController`.

```dart
import 'package:flutter/material.dart';

class Greet2 extends StatefulWidget {
  const Greet2({Key? key}) : super(key: key);

  @override
  _Greet2State createState() => _Greet2State();
}

class _Greet2State extends State<Greet2> {
  var name = 'Mark'; // initial value

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        TextFormField(
          decoration: InputDecoration(
            border: OutlineInputBorder(),
            labelText: 'Name',
            suffixIcon: name.isEmpty
                ? Container(width: 0)
                : IconButton(
                    icon: Icon(Icons.close, size: 18),
                    // This doesn't clear the text displayed!
                    onPressed: () => setState(() => name = ''),
                  ),
          ),
          initialValue: name,
          onChanged: (String value) {
            // This doesn't clear the text displayed!
            setState(() => name = value);
          },
        ),
        Text(name.isEmpty ? '' : 'Hello, $name!'),
      ],
    );
  }
}
```

The bottom line is that text input in Flutter is a bit complicated.
Like many things in Flutter, a good approach
is to wrap complexity in custom widgets and
use them everywhere instead of directly using provided widgets.
This also provides an opportunity to specify application-specific styling.
The following code demonstrates a custom widget that does this.

```dart
import 'package:flutter/material.dart';

class EasyTextField extends StatefulWidget {
  final String label;
  final ValueChanged<String>? onChanged;
  final String value;

  const EasyTextField({
    Key? key,
    required this.label,
    required this.onChanged,
    this.value = '',
  }) : super(key: key);

  @override
  _EasyTextFieldState createState() => _EasyTextFieldState();
}

class _EasyTextFieldState extends State<EasyTextField> {
  final tec = TextEditingController();

  @override
  void initState() {
    tec.text = widget.value;
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return TextField(
      controller: tec,
      decoration: InputDecoration(
        border: OutlineInputBorder(),
        labelText: widget.label,
        suffixIcon: tec.text.isEmpty
            ? Container(width: 0)
            : IconButton(
                icon: Icon(Icons.close, size: 18),
                onPressed: () {
                  setState(() => tec.text = '');
                  // This causes a new value for
                  // the "value" parameter to be passed in
                  // which allows the suffixIcon to be reevaluated.
                  // Unlike in Greet1 there is no need to
                  // add a listener to the TextEditingController.
                  widget.onChanged!('');
                },
              ),
      ),
      onChanged: widget.onChanged,
    );
  }
}
```

Here is an example of using the custom widget defined above.
Note how much simpler this component is than
the `Greet1` and `Greet2` components.

```dart
import 'package:flutter/material.dart';
import 'easy_text_field.dart';

class Greet3 extends StatefulWidget {
  const Greet3({Key? key}) : super(key: key);

  @override
  _Greet3State createState() => _Greet3State();
}

class _Greet3State extends State<Greet3> {
  var name = 'Mark';

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        EasyTextField(
          label: 'Name',
          value: name,
          onChanged: (String value) {
            setState(() => name = value);
          },
        ),
        Text(name.isEmpty ? '' : 'Hello, $name!'),
      ],
    );
  }
}
```

In mobile apps an on-screen keyboard appears when focus is a text field.
In the iOS simulator, the on-screen keyboard will not appear
unless it is toggled on by pressing cmd-k.

When focus leaves a text field and is not in a new text field,
the keyboard should be hidden.
This occurs by default in Android, but not in iOS.
To make this happen in iOS, define to following widget
and wrap the top widget (typically `MaterialApp`) in it.

```dart
import 'package:flutter/material.dart';

class DismissKeyboard extends StatelessWidget {
  final Widget child;

  const DismissKeyboard({
    Key? key,
    required this.child,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      // If a TextField has focus, causing the on-screen keyboard to appear,
      // this code will detect taps outside of it and hide the keyboard.
      // However, if what is tapped is a button, this code will not be invoked.
      // It seems that buttons swallow this event.
      onTap: () {
        FocusScopeNode currentFocus = FocusScope.of(context);
        if (!currentFocus.hasPrimaryFocus &&
            currentFocus.focusedChild != null) {
          FocusManager.instance.primaryFocus?.unfocus();
        }
      },
      child: child,
    );
  }
}
```

Another option is to hide the on-screen keyboard if the user scrolls the page.
For each custom widget class that defines a page in the app
that supports scrolling using a `ListView` widget, do the following:

1. Declare in property that holds an instance of `ScrollController`.

   ```dart
   var scrollController = ScrollController();
   ```

1. Override the `initState` method.

   ```dart
   @override
   void initState() {
     super.initState();
     scrollController.addListener(() {
       // Remove focus from the TextField that
       // triggered the on-screen keyboard to open.
       FocusScope.of(context).requestFocus(FocusNode());
     });
   }
   ```

1. Override the `dispose` method.

   ```dart
   @override
   void dispose() {
     scrollController.dispose();
     super.dispose();
   }
   ```

1. Pass the `ScrollController` instance to the `ListView`.

   ```dart
   ListView(
     controller: scrollController,
     children: <Widget>[
       ...
     ],
   )
   ```

### Dialog Widgets

Flutter provides many widgets that render modal dialogs.
Each of these are described in the following table:

| Widget                                                                                                                  | Description                                                                                                                                 |
| ----------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/AboutDialog-class.html", "AboutDialog" %}                     | contains the "application's icon, name, version number, and copyright, plus a button to show licenses for software used by the application" |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/AlertDialog-class.html", "AlertDialog" %}                     | creates a dialog containing a message and some buttons                                                                                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoAlertDialog-class.html", "CupertinoAlertDialog" %}  | iOS-themed version of `AlertDialog`                                                                                                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/DatePickerDialog-class.html", "DatePickerDialog" %}           | contains help text and a `CalendarDatePicker`                                                                                               |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/DateRangePickerDialog-class.html", "DateRangePickerDialog" %} | contains help text and a `CalendarDatePicker` that supports selecting both a start and end date                                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/Dialog-class.html", "Dialog" %}                               | used by other dialogs, but not by extending; not typically used directly                                                                    |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/SimpleDialog-class.html", "SimpleDialog" %}                   | contains a title and a list of clickable `SimpleDialogOption` options                                                                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/TimePickerDialog-class.html", "TimePickerDialog" %}           | contains a "lollipop" UI for selecting an hour and minute in a day with toggle buttons for AM and PM                                        |

Flutter also provides a set of functions that show a dialog.
All of these return a `Future` that completes
when the user does something to dismiss the dialog.
These functions are described in the following table:

| Function                                                                                                       | Description                                                                                |
| -------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/showDialog.html", "showDialog" %}                    | displays any dialog returned by the function specified in its `builder` argument           |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/showCupertinoDialog.html", "showCupertinoDialog" %} | like `showDialog`, but themed for iOS                                                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/showGeneralDialog.html", "showGeneralDialog" %}       | like `showDialog` but supports customizing the transition used to display the dialog       |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/showDatePicker.html", "showDatePicker" %}            | provides an easier way to display a `DatePickerDialog` than using the above functions      |
| {% aTargetBlank "https://api.flutter.dev/flutter/material/showDateRangePicker.html", "showDateRangePicker" %}  | provides an easier way to display a `DateRangePickerDialog` than using the above functions |

By default, dialogs created with the `showDialog` function
are dismissed if a user taps outside them. To prevent this,
pass the `barrierDismissible` argument with a value of `false`.

Dialogs created with the `showCupertinoDialog` and `showGeneralDialog` functions
have the opposite default.
To cause them to be dismissed if a user taps outside them,
pass the `barrierDismissible` argument with a value of `true`.

Dialogs created with the `showDatePicker` and `showDatePickerRange` functions
are always dismissed if a user taps outside them.
They do not take a `barrierDismissible` argument,
so this behavior cannot be changed.

The `showDatePickerRange` displays a full-screen modal dialog
while the other `show` functions
display modal dialogs that do not cover the full screen.

The following code simplifies the use of `AlertDialog`
and provides the functions `alert` and `confirm`
which are similar to their HTML counterparts.

```dart
import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';

/// Displays an AlertDialog with an OK button
/// which just closes the dialog.  No value is returned.
Future<void> alert({
  required BuildContext context,
  required String title,
  required String message,
  bool cupertino = false,
}) async {
  await showDialog<String>(
    context: context,
    // The builder function can return any kind of dialog.
    builder: (_) => MyAlertDialog(
      cupertino: cupertino,
      title: title,
      message: message,
      options: ['OK'],
    ),
  );
}

/// Displays an AlertDialog with No and Yes buttons
/// and returns a Future that succeeds with a String
/// that is the text on the pressed button.
Future<String?> confirm({
  required BuildContext context,
  required String title,
  required String message,
  bool cupertino = false,
}) async {
  return await showDialog<String>(
    context: context,
    builder: (_) => MyAlertDialog(
      title: title,
      message: message,
      options: ['No', 'Yes'],
    ),
  );
}

class MyAlertDialog extends StatelessWidget {
  final bool cupertino;
  final String title;
  final String message;
  final List<String> options;

  MyAlertDialog({
    Key? key,
    this.cupertino = false,
    required this.title,
    required this.message,
    required this.options,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    // Dialogs are treated like routes, so to close a dialog and
    // return the previous route, call Navigator.pop(context).
    var buttons = options
        .map(
          (option) => TextButton(
            onPressed: () => Navigator.pop(context, option),
            child: Text(option),
          ),
        )
        .toList();
    return cupertino
        ? CupertinoAlertDialog(
            title: Text(title),
            content: Text(message), // can be any widget
            actions: buttons, // typically a List of TextButton widgets
          )
        : AlertDialog(
            title: Text(title),
            content: Text(message), // can be any widget
            actions: buttons, // typically a List of TextButton widgets
          );
  }
}
```

The following code demonstrates using each of the supported dialog types.

<figure style="width: 30%">
  <img alt="Flutter Dialogs"
    src="/blog/assets/flutter-dialogs.png?v={{pkg.version}}"
    title="Flutter Dialogs">
  <figcaption>Flutter Dialogs</figcaption>
</figure>
<figure style="width: 30%">
  <img alt="Flutter About Dialog"
    src="/blog/assets/flutter-dialog-about.png?v={{pkg.version}}"
    title="Flutter About Dialog">
  <figcaption>Flutter About Dialog</figcaption>
</figure>
<figure style="width: 30%">
  <img alt="Flutter About Dialog Licenses"
    src="/blog/assets/flutter-dialog-about-licenses.png?v={{pkg.version}}"
    title="Flutter About Dialog Licenses">
  <figcaption>Flutter About Dialog Licenses</figcaption>
</figure>

<figure style="width: 30%">
  <img alt="Flutter Material Alert Dialog"
    src="/blog/assets/flutter-dialog-alert.png?v={{pkg.version}}"
    title="Flutter Material Alert Dialog">
  <figcaption>Flutter Material Alert Dialog</figcaption>
</figure>
<figure style="width: 30%">
  <img alt="Flutter Cupertino Alert Dialog"
    src="/blog/assets/flutter-dialog-alert-cupertino.png?v={{pkg.version}}"
    title="Flutter Cupertino Alert Dialog">
  <figcaption>Flutter Cupertino Alert Dialog</figcaption>
</figure>
<figure style="width: 30%">
  <img alt="Flutter Confirm Dialog"
    src="/blog/assets/flutter-dialog-confirm.png?v={{pkg.version}}"
    title="Flutter Confirm Dialog">
  <figcaption>Flutter Confirm Dialog</figcaption>
</figure>

<figure style="width: 30%">
  <img alt="Flutter Date Picker Dialog"
    src="/blog/assets/flutter-dialog-date-picker.png?v={{pkg.version}}"
    title="Flutter Date Picker Dialog">
  <figcaption>Flutter Date Picker Dialog</figcaption>
</figure>
<figure style="width: 30%">
  <img alt="Flutter Date Range Dialog"
    src="/blog/assets/flutter-dialog-date-range.png?v={{pkg.version}}"
    title="Flutter Date Range Dialog">
  <figcaption>Flutter Date Range Dialog</figcaption>
</figure>
<figure style="width: 30%">
  <img alt="Flutter Time Picker Dialog"
    src="/blog/assets/flutter-dialog-time-picker.png?v={{pkg.version}}"
    title="Flutter Time Picker Dialog">
  <figcaption>Flutter Time Picker Dialog</figcaption>
</figure>

```dart
import 'package:flutter/material.dart';
import 'my_alert_dialog.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Dialog Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: const MyHomePage(),
    );
  }
}

class MyHomePage extends StatelessWidget {
  const MyHomePage({Key? key}) : super(key: key);

  void _alert({
    required BuildContext context,
    bool cupertino = false,
    required String message,
  }) {
    alert(
      context: context,
      cupertino: cupertino,
      title: 'Alert',
      message: message,
    );
  }

  void _confirm({
    required BuildContext context,
    required String question,
  }) async {
    var answer = await confirm(
      context: context,
      title: 'Confirm',
      message: question,
    );
    print('answer = $answer');
  }

  void _pickDate({required BuildContext context}) async {
    var dateTime = DateTime.now();
    var newDateTime = await showDatePicker(
      context: context,
      helpText: 'Select a date.',
      initialDate: dateTime,
      firstDate: DateTime(1970),
      lastDate: DateTime(2030, 12, 31),
    );
    print('newDateTime = $newDateTime');
  }

  void _pickDateRange({required BuildContext context}) async {
    var dateRange = DateTimeRange(
      start: DateTime(2022, 4, 16),
      end: DateTime(2022, 5, 3),
    );
    // Click the start date, then click the end date.
    // Click again to start over, selecting a new start date.
    // The days in between will be shaded
    // to indicate that they are in the range.
    var newDateRange = await showDateRangePicker(
      context: context,
      helpText: 'Select start and end dates.',
      initialDateRange: dateRange,
      firstDate: DateTime(1970),
      lastDate: DateTime(2030, 12, 31),
    );
    print('newDateRange = $newDateRange');
  }

  void _pickTime(BuildContext context) async {
    var time = TimeOfDay(hour: 10, minute: 19);
    var newTime = await showDialog(
      context: context,
      builder: (_) => TimePickerDialog(
        helpText: 'Select a time.',
        initialTime: time,
      ),
    );
    print('newTime = $newTime');
  }

  void _showAbout(BuildContext context) {
    showDialog(
        context: context,
        builder: (_) => AboutDialog(
              applicationIcon: Icon(Icons.ac_unit_outlined),
              applicationName: 'Dialog Demos',
              applicationVersion: '1.0.0',
              applicationLegalese: 'All rights reserved.',
            ));
  }

  @override
  Widget build(BuildContext context) {
    print(context);
    return Scaffold(
      appBar: AppBar(
        title: Text('Dialog Demo'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            ElevatedButton(
              child: Text('About...'),
              onPressed: () => _showAbout(context),
            ),
            ElevatedButton(
              child: Text('Show Material Alert Dialog'),
              onPressed: () => _alert(
                context: context,
                message: 'Something interesting happened.',
              ),
            ),
            ElevatedButton(
              child: Text('Show Cupertino Alert Dialog'),
              onPressed: () => _alert(
                context: context,
                cupertino: true,
                message: 'Something interesting happened.',
              ),
            ),
            ElevatedButton(
              child: Text('Show Confirm Dialog'),
              onPressed: () => _confirm(
                context: context,
                question: 'Are you sure?',
              ),
            ),
            ElevatedButton(
              child: Text('Show DatePickerDialog'),
              onPressed: () {
                _pickDate(context: context);
              },
            ),
            ElevatedButton(
              child: Text('Select Date Range'),
              onPressed: () => _pickDateRange(context: context),
            ),
            ElevatedButton(
              child: Text('Select Time'),
              onPressed: () => _pickTime(context),
            )
          ],
        ),
      ),
    );
  }
}
```

### Bottom Sheets

Bottom sheets are a less intrusive alternative to dialogs.
They slide up from the bottom and can be dragged back down by the user
or closed programmatically.
They can be modal or non-modal.

Bottom sheets attach themselves to the nearest `Scaffold` widget.

To display a bottom sheet, call the function
`showBottomSheet` (for non-modal) or `showModalBottomSheet` (for modal).
These functions take many arguments, but the most commonly used are:

- `context`: any `BuildContext` that is inside the `Scaffold`
- `builder`: function that takes a `BuildContext`
  and returns a widget to render
- `backgroundColor`: a `Color` object

It is recommended to return a `SafeArea` widget from the `builder` function
that wraps the widget to be rendered.
This avoid having the bottom sheet overlap unsafe areas such as a status bar.

Non-modal bottom sheets do not automatically span the width of the device.
To achieve this, wrap the `SafeArea` child in a `SizedBox` and
pass it the `width` argument with a value of `double.infinity`.

If the `SafeArea` child is a `Column` widget,
it will not automatically have its height adjusted to match its content.
To do this, wrap the `Column` widget in a `SizedBox` and pass it the
`mainAxisSize` argument with a value of `MainAxisSize.min`.

While the user can close the bottom sheet by dragging it down,
it may be desirable to include a close button
that calls `Navigator.pop(context)` to close it.
Alternatively, capture the controller returned by
the `showBottomSheet` or `showModalBottomSheet` function
and call the `close` method on that controller object.

The following helper function addresses all the concerns described above.

```dart
import 'package:flutter/material.dart';

void openBottomSheet({
  Color backgroundColor = Colors.blue,
  required BuildContext context,
  required Widget widget,
  bool modal = false,
  bool includeCloseButton = false,
}) {
  var fn = modal ? showModalBottomSheet : showBottomSheet;
  fn<void>(
    backgroundColor: backgroundColor,
    context: context,
    builder: (context) => SafeArea(
      child: SizedBox(
        child: Column(
          children: [
            widget,
            if (includeCloseButton)
              ElevatedButton(
                child: Text('Close'),
                onPressed: () => Navigator.pop(context),
              )
          ],
          mainAxisSize: MainAxisSize.min,
        ),
        width: double.infinity,
      ),
    ),
  );
}
```

Here is an example of using the helper function above
to display a non-modal and modal bottom sheet:

```dart
import 'package:flutter/material.dart';
import 'bottom_sheet.dart';

void main() => runApp(const MyApp());

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter BottomSheet Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: MyHomePage(title: 'BottomSheet Demo'),
    );
  }
}

class MyHomePage extends StatelessWidget {
  final sheetWidget = Text(
    'I am in a BottomSheet.',
    style: TextStyle(color: Colors.white, fontSize: 24),
  );

  final String title;

  MyHomePage({Key? key, required this.title}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
        // Using Builder just to get a Context inside Scaffold
        // so the showBottomSheet function can find the Scaffold.
        child: Builder(
          builder: (context) {
            var nonModalButton = ElevatedButton(
              child: Text('Show Non-Modal BottomSheet'),
              onPressed: () => openBottomSheet( // function defined above
                backgroundColor: Colors.green,
                context: context,
                includeCloseButton: true,
                modal: false,
                widget: sheetWidget,
              ),
            );

            var modalButton = ElevatedButton(
              child: Text('Show Modal BottomSheet'),
              onPressed: () => openBottomSheet(
                context: context,
                modal: true,
                widget: sheetWidget,
              ),
            );

            return Column(
              mainAxisAlignment: MainAxisAlignment.center,
              children: <Widget>[nonModalButton, modalButton],
            );
          },
        ),
      ),
    );
  }
}
```

### SnackBars

SnackBars are similar to bottom sheets.
Both slide in from the bottom.
But SnackBars are intended for short messages that only appear briefly.

The `SnackBar` constructor takes many arguments, but the most commonly used are:

- `action`: a `SnackBarAction` object that acts like a `TextButton`
- `backgroundColor`: a `Color` object
- `behavior`: a `SnackBarBehavior` enum with values
  `fixed` (default; displays above widgets fixed at the bottom of the screen
  like `BottomNavigationBar` and `FloatingActionButton`) and
  `floating` (displays on top of widgets fixed at the bottom of the screen)
- `content`: any widget to display
- `dismissDirection`: `DismissDirection` enum that defaults to `down`
- `duration`: `Duration` object that defaults to 4 seconds
- `elevation`: `double` z-index; defaults to 6;
  set to `0` for a flat look that has no shadow
- `margin`: `EdgeInsetsGeometry` object applied outside `SnackBar`
  when `width` is not specified
- `padding`: `EdgeInsetsGeometry` object applied inside `SnackBar`
- `width`: `double` width of `SnackBar`
  that defaults to the device width minus the `margin`

A failed assertion is triggered if both `margin` and `width` are specified.

The `SnackBarAction` constructor takes the following arguments:

- `disabledTextColor`: `Color` used for text after the `SnackBar` is dismissed
- `label`: `String` to display; does not wrap so keep this short
- `textColor`: `Color` used for text before the `SnackBar` is dismissed;
  defaults to the `ThemeData` `primarySwatch` color
- `onPressed`: function to call when the label is tapped

Here is an example of a `SnackBar` that displays an error message.
Some of the `SnackBar` arguments are commented out
just to show valid values for them.

<img alt="Flutter SnackBar" style="width: 40%"
    src="/blog/assets/flutter-snackbar.png?v={{pkg.version}}"
    title="Flutter SnackBar">

```dart
class MyHomePage extends StatelessWidget {
  final String title;

  final snackBar = SnackBar(
    action: SnackBarAction(
      label: 'Panic',
      onPressed: () => print('got press'),
      textColor: Colors.white,
    ),
    backgroundColor: Colors.red.shade900,
    //behavior: SnackBarBehavior.floating,
    content: Row(
      children: [
        Icon(Icons.error_outline, color: Colors.white, size: 32),
        SizedBox(width: 16),
        // Using Expanded allows the text to wrap.
        Expanded(
          child: Text(
            'This is a long sentence that will require wrapping.',
            style: TextStyle(fontSize: 18),
          ),
        ),
      ],
    ),
    //dismissDirection: DismissDirection.up,
    duration: Duration(seconds: 10),
    //elevation: 0,
    //margin: EdgeInsets.all(10),
    //padding: EdgeInsets.all(10),
    //shape: StadiumBorder(),
    //width: 200,
  );

  MyHomePage({Key? key, required this.title}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(title)),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            ElevatedButton(
              child: Text('Show SnackBar #1'),
              onPressed: () {
                ScaffoldMessenger.of(context)
                  // If another SnackBar is currently displayed, remove it.
                  // There are two methods that do this.
                  //..hideCurrentSnackBar() // runs exit animation
                  ..removeCurrentSnackBar() // does not run exit animation
                  // Now show the new SnackBar.
                  ..showSnackBar(snackBar);
              },
            ),
            ElevatedButton(
              child: Text('Show SnackBar #2'),
              onPressed: () {
                ScaffoldMessenger.of(context)
                  ..hideCurrentSnackBar()
                  ..showSnackBar(snackBar);
              },
            )
          ],
        ),
      ),
    );
  }
}
```

### Cupertino Widgets

These widgets use iOS styling rather than Material Design.
They are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/cupertino",
"Cupertino (iOS-style) widgets" %}.

To use Cupertino widgets in a Dart source file, add the following import:

```dart
import 'package:flutter/cupertino.dart';
```

The {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoColors-class.html",
"CupertinoColors" %} class defines constants for colors
that are commonly used in iOS apps.
The type of these constants is `CupertinoDynamicColor` which extends `Color`,
so they can be used anywhere a `Color` value is expected.
Examples include `activeBlue`, `destructiveRed`, `inactiveGray`,
`label`, `lightBackgroundGray`, `link`, `placeholderText`,
`separator`, `systemBackground`, and `systemBlue`.

| Widget                                                                                                                                               | Description                                                                                                        |
| ---------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoActionSheet-class.html", "CupertinoActionSheet" %}                               | modal that slides up from bottom to allow selection from a set of options; sometimes used for confirmation dialogs |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoActivityIndicator-class.html", "CupertinoActivityIndicator" %}                   | iOS-style spinner                                                                                                  |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoAlertDialog-class.html", "CupertinoAlertDialog" %}                               | iOS-style alert dialog with a title, message, and set of buttons                                                   |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoButton-class.html", "CupertinoButton" %}                                         | iOS-style button that can be tapped to execute associated code                                                     |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoContextMenu-class.html", "CupertinoContextMenu" %}                               | iOS-style modal containing a set of tappable options rendered when a specific widget is long-pressed               |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoDatePicker-class.html", "CupertinoDatePicker" %}                                 | iOS-style wheel picker for entering a date and time                                                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoDialogAction-class.html", "CupertinoDialogAction" %}                             | button with no background color or border, typically used in `CupertinoAlertDialog`                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoFullscreenDialogTransition-class.html", "CupertinoFullScreenDialogTransition" %} | iOS-style transition used to render fullscreen dialogs                                                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoNavigationBar-class.html", "CupertinoNavigationBar" %}                           | iOS-style top navigation bar typically used with `CupertinoPageScaffold`                                           |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoPageScaffold-class.html", "CupertinoPageScaffold" %}                             | common iOS-style page layout                                                                                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoPageTransition-class.html", "CupertinoPageTransition" %}                         | iOS-style page transition animation                                                                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoPicker-class.html", "CupertinoPicker" %}                                         | iOS-style wheel picker                                                                                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoPopupSurface-class.html", "CupertinoPopupSurface" %}                             | rounded rectangle for an alert dialog or action sheet                                                              |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoScrollbar-class.html", "CupertinoScrollbar" %}                                   | iOS-style scrollbar                                                                                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoSearchTextField-class.html", "CupertinoSearchTextField" %}                       | iOS-style search input                                                                                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoSegmentedControl-class.html", "CupertinoSegmentedControl" %}                     | iOS-style segmented control which is a horizontal list of mutually-exclusive buttons                               |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoSlider-class.html", "CupertinoSlider" %}                                         | iOS-style slider for selecting a value from a range                                                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoSlidingSegmentedControl-class.html", "CupertinoSlidingSegmentedControl" %}       | iOS-style segmented control which is a horizontal list of buttons                                                  |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoSliverNavigationBar-class.html", "CupertinoSliverNavigationBar" %}               | iOS-style navigation bar with a large title                                                                        |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoSwitch-class.html", "CupertinoSwitch" %}                                         | iOS-style switch (like the SwiftUI `Toggle` view)                                                                  |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoTabBar-class.html", "CupertinoTabBar" %}                                         | iOS-style bottom tab bar that is typically used with `CupertinoTabScaffold`                                        |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoTabScaffold-class.html", "CupertinoTabScaffold" %}                               | positions a tab bar below the display of select tab content                                                        |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoTabView-class.html", "CupertinoTabView" %}                                       | supports "parallel navigation"? between tabs; typically used with `CupertinoTabScaffold`                           |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoTextField-class.html", "CupertinoTextField" %}                                   | iOS-style input text field                                                                                         |
| {% aTargetBlank "https://api.flutter.dev/flutter/cupertino/CupertinoTimerPicker-class.html", "CupertinoTimerPicker" %}                               | iOS-style wheel picker for entering hours, minutes, and seconds that represent a countdown timer                   |

#### CupertinoDatePicker Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoDatePicker-class.html",
"CupertinoDatePicker" %} widget is an alternative
to the Material UI {% aTargetBlank
"https://api.flutter.dev/flutter/material/CalendarDatePicker-class.html",
"CalendarDatePicker" %} widget.
It allows the user to select a date, time, or both
by spinning wheel-like displays.

The following code demonstrates this:

```dart
Container(
  child: CupertinoDatePicker(
    initialDateTime: DateTime.now(),
    minimumYear: 1990,
    maximumYear: 2030,

    // Will get "Failed assert" after changing this.
    // Restart the app to resolve.
    //mode: CupertinoDatePickerMode.dateAndTime, // default
    mode: CupertinoDatePickerMode.date,
    //mode: CupertinoDatePickerMode.time,

    onDateTimeChanged: (DateTime value) {
      print('You selected $value');
    },
  ),
  height: 200, // height of wheel; affects # of visible options
);
```

Currently thewre are no enum values in `CupertinoDatePickerMode`
to request getting only a month and year or only a month and day.
See this {% aTargetBlank
"https://github.com/flutter/flutter/issues/93508", "issue" %}.

#### CupertinoPicker Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoPicker-class.html",
"CupertinoPicker" %} widget is an alternative
to the Material UI {% aTargetBlank
"https://api.flutter.dev/flutter/material/DropdownButton-class.html",
"DropdownButton" %} widget.
It allows the user to select an option from a wheel-like display.

The following code allows the user to select from a `List` of words.

```dart
Container(
  child: CupertinoPicker.builder(
    childCount: words.length,
    itemBuilder: (context, index) => Text(words[index]),
    itemExtent: 30, // height of each item
    onSelectedItemChanged: (int index) {
      // This fires while dragging the wheel,
      // not just when it is released.
      print('You selected ${words[index]}');
    },
  ),
  // This makes the vertical space that is occupied clear.
  decoration: BoxDecoration(
    border: Border.all(color: Colors.red),
  ),
  height: 150, // height of wheel; affects # of visible items
);
```

## Swipe to Delete

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/Dismissible-class.html",
"Dismissible" %} widget allows a widget to be dragged out of view.
The `onDismissed` argument can be set to a function that usually
updates the state so the widget will not be rendered again.

Typically widgets that can be dismissed are
displayed in a `List` containing `ListTile` widgets.

Using the `Dismissble` widget is not a good option for
implementing a list with "swipe to delete" behavior.
While a background containing a "Delete" button
can be exposed when the user drags a tile to the left,
it is hidden when they release the tile
which gives them no opportunity to tap the button.
A better option is to use the {% aTargetBlank
"https://pub.dev/packages/flutter_swipe_action_cell",
"flutter_swipe_action_cell" %} package.
For an example, see this {% aTargetBlank
"https://github.com/mvolkmann/flutter_dismissible/blob/main/lib/main.dart",
"GitHub repo" %}.

## Pull to Refresh

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/RefreshIndicator-class.html",
"RefreshIndicator" %} widget wraps a vertical scroll view
and provides the "pull to refresh" capability.

For a short introduction, see this {% aTargetBlank
"https://www.youtube.com/watch?v=ORApMlzwMdM", "YouTube video" %}.

The following code demonstrates using the RefreshIndicator widget.
Dragging the list down when the top item is displayed
cause three more items to be added to the beginning
until there are no more items to load.

<img alt="Flutter RefreshIndicator" style="width: 60%"
    src="/blog/assets/flutter-refreshindicator.png?v={{pkg.version}}"
    title="Flutter RefreshIndicator">

```dart
import 'package:flutter/material.dart';
import './extensions/widget_extensions.dart';

const title = 'My App';

void main() => runApp(
      MaterialApp(
        title: title,
        theme: ThemeData(primarySwatch: Colors.blue),
        home: Home(),
      ),
    );

class Home extends StatefulWidget {
  Home({Key? key}) : super(key: key);

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  final itemsToLoad = [
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'purple',
    'white',
    'gray',
    'black',
    'brown',
    'aqua',
    'teal',
    'amber',
    'turquoise',
    'peach',
  ];
  final itemsToShow = <String>[];
  final keyRefresh = GlobalKey<RefreshIndicatorState>();

  @override
  void initState() {
    super.initState();

    // Wait for the build method to complete and then
    // trigger a call to the onRefresh function of the RefreshIndicator
    // which causes the indicator to display.
    WidgetsBinding.instance
        ?.addPostFrameCallback((_) => keyRefresh.currentState?.show());
  }

  void addItem() {
    if (itemsToLoad.isNotEmpty) {
      itemsToShow.insert(0, itemsToLoad.removeAt(0));
    }
  }

  Future<void> loadItems() async {
    await Future.delayed(Duration(seconds: 1)); // simulates API call
    setState(() {
      // Items are added to the beginning of the list.
      // Another approach is to completely replace the list with new data.
      addItem();
      addItem();
      addItem();
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(title)),
      body: Column(
        children: [
          Text('Pull to refresh.'),
          // This can only be used with a vertical scroll view
          RefreshIndicator(
            key: keyRefresh,
            // This function updates the scrollable contents,
            // in this case the items in a ListView.
            // The refresh indicator disappears when
            // the promise returned by this function completes.
            onRefresh: loadItems,
            child: Scrollbar(
              child: ListView.builder(
                physics: const AlwaysScrollableScrollPhysics(),
                itemCount: itemsToShow.length,
                itemBuilder: (BuildContext context, int index) {
                  return Card(
                    child: ListTile(title: Text(itemsToShow[index])),
                  );
                },
              ),
            ),
          ).expanded,
        ],
      ),
    );
  }
}
```

## Model-View-Controller

One recommended pattern for organizing a Flutter project
is to follow the Model-View-Controller (MVC) pattern.
For example, a project can have subdirectories under the `lib` directory
named `models`, `controllers`, and `views`.

The `models` directory holds Dart source files that define classes
that each describe a particular kind of data use in the application.
Theses classes focus on data representation and not business rules.
In an application that supports scheduling meetings,
the model classes could include `User` and `Meeting`.
To make it easy for other classes to import all of the model classes
consider creating the file `data_layer.dart` that contains
one `export` statement for each model source file.

The `controllers` directory holds Dart source files
that define classes the business rules of the application.
These classes define methods for performing
actions supported by the application.
For example, methods could include `createUser`, `updateUser`, `deleteUser`,
`createMeeting`, `updateMeeting`, `deleteMeeting`, and `sendMeetingInvites`.

The `views` directory holds Dart source files that define widgets
that render the user interface of the application.
For example, widgets could include `User`, `Meeting`, and `Calendar`.

Some applications choose to create classes that
combine the roles of models and controllers.

## Responsive UIs

Flutter can build UIs that respond to the current device size.
The `MediaQuery` class provides a way to obtain a `MediaQueryData` object
that holds information about many aspects of the display
including size and user preferences.
Typically this is used inside the `build` method of a widget
which is passed a `BuildContext` object.

```dart
MediaQueryData data = MediaQuery.of(context);
```

Highlights of the properties `MediaQueryData` objects are described below:

| Property            | Description                                                                                     |
| ------------------- | ----------------------------------------------------------------------------------------------- |
| `disableAnimations` | `bool` indicating whether animations should be reduced                                          |
| `orientation`       | `Orientation` enum with the values `portrait` and `landscape`                                   |
| `size`              | `Size` object with many properties listed below                                                 |
| `viewInsets`        | `EdgeInsets` object describing completely obscured parts of screen (ex. on-screen keyboard)     |
| `viewPadding`       | `EdgeInsets` object describing partially obscured parts of screen (ex. notches and status bars) |

The `orientation` property can be used to implement different widget layouts
based on whether the device is in portrait or layout mode.

The `padding` property is an `EdgeInsets` object
that describes partially obscured parts of screen
which are not also currently completely obscured.
This is complex and likely not useful.
For example, a bottom area that is always partially obscured
might be temporarily completely obscured by an on-screen keyboard.
Suppose `viewInsets.bottom` has a value of `340`
representing the height of the on-screen keyboard
and `viewPadding.bottom` has a value of `30`
representing the height of a handle.
In this case `padding.bottom` would have a value of `0`.

`EdgeInsets` objects have the properties
`left`, `right`, `top`, `bottom`, and more.

`Size` objects have the properties
`height`, `width`, `aspectRatio`, `longestSize`, and `shortestSize`.
This information can be used to size widgets so they occupy
a specific percentage of the screen width and/or height.

The `viewInsets` and `viewPadding` properties can be
used to avoid rendering widgets in unsafe areas of the screen
such as notches and status bars.
However, an easier way to accomplish this is to
wrap the `Scaffold` widget in a `SafeArea` widget.

## Form Validation

The {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Form-class.html",
"Form" %} widget supports form validation based on the form fields inside it.
It takes a `children` argument that is a `List` of widgets.

Widget classes that extend {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/FormField-class.html",
"`FormField`" %} support a `validator` argument.
This can be set to a function that is passed the current value.
Often these functions use the `RegExp` class and its `hasMatch` method
to compare `String` values to regular expressions.
The function should return a `String` error message when the value is invalid,
and `null` when the value is valid.
Error messages can be multiline strings created using triple quotes.

The only provided widgets that extend `FormField` are
`TextFormField`, `DropdownButtonFormField`, and `CupertinoTextFormFieldRow`.
Additional widget classes can be defined to
add validation to other input widgets.
To learn how to do this, watch {% aTargetBlank
"https://www.youtube.com/watch?v=1vHf5kQ0E2I", "this video" %}
starting at 9:21.

`Form` is a stateful widget and the class that holds its state is `FormState`.
Each `Form` instance should have unique `key` argument
that is used to access the state of the `Form`
and determine whether it is valid.

To create a key for a `Form` define a `final` property
in the class that renders the `Form`.
For example:

```dart
  final _formKey = GlobalKey<FormState>();
```

Invalid fields are changed to have `errorColor` accents (typically red)
and an error message is displayed below them.

By default the form fields are not automatically validated.
This means that invalid values can be entered
and no error messages will be displayed.
To validate a `Form`, call `formKey.currentState!.validate()`
which returns a `bool` indicating if the form is valid.

To perform validation automatically, set the `Form`
`autovalidateMode` argument to a value from the `AutovalidateMode` enum.
There are three values to choose from.
`disabled` never automatically performs validation and is the default.
`always` performs every time the form state changes
which could occur programmatically.
`onUserInteraction` performs validation every time a user changes a form input.

To disable a button in a `Form` when the form is invalid,
set its `onPressed` argument to `null`.

The pub.dev package {% aTargetBlank
"https://pub.dev/packages/form_field_validator", "form_field_validator" %}
provides a set of classes that simplify form validation.
This may be overkill for most forms.

The following code demonstrates implementing a login form
with validation:

<img alt="Flutter Form Validation initial" style="width: 40%"
    src="/blog/assets/flutter-form-validation-1.png?v={{pkg.version}}"
    title="Flutter Form Validation initial">
<img alt="Flutter Form Validation with data" style="width: 40%"
    src="/blog/assets/flutter-form-validation-2.png?v={{pkg.version}}"
    title="Flutter Form Validation with data">

This code uses a custom widget that wraps the use of `TextFormField`
to simply it's usage a bit.
Here is the code from `my_text_field.dart`:

```dart
import 'package:flutter/material.dart';
import 'widget_extensions.dart';

typedef ValidatorFn = String? Function(String?)?;

class MyTextField extends StatefulWidget {
  final String hintText;
  final String initialValue;
  final String labelText;
  final bool obscureText;
  final void Function(String)? onChanged;
  final ValidatorFn validator;

  const MyTextField({
    Key? key,
    this.hintText = '',
    this.initialValue = '',
    this.labelText = '',
    this.obscureText = false,
    this.validator,
    this.onChanged,
  }) : super(key: key);

  @override
  _MyTextFieldState createState() => _MyTextFieldState();
}

class _MyTextFieldState extends State<MyTextField> {
  @override
  Widget build(BuildContext context) {
    var validator = widget.validator;
    return TextFormField(
      decoration: InputDecoration(
        border: OutlineInputBorder(),
        // Note how "widget" is used to access
        // properties in the StatefulWidget above.
        hintText: widget.hintText,
        labelText: widget.labelText,
      ),
      initialValue: widget.initialValue,
      obscureText: widget.obscureText,
      onChanged: widget.onChanged,
      validator: validator,
    ).pad(10);
  }
}
```

Here is the code from `main.dart`
that uses the `MyTextField` widget defined above:

```dart
import 'package:flutter/material.dart';
import 'my_text_field.dart';

void main() => runApp(const MyApp());

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(primarySwatch: Colors.blue),
      home: MyHomePage(),
    );
  }
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({Key? key}) : super(key: key);

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  final _formKey = GlobalKey<FormState>();

  var userName = '';
  var password = '';
  String? message;

  void login() {
    var valid = _formKey.currentState!.validate();
    var message =
        valid ? 'Processing login ...' : 'One or more fields are invalid.';
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(content: Text(message)),
    );
  }

  String? validatePassword(value) {
    if (value == null || value.isEmpty) {
      return 'A password is required.';
    }
    if (value.length < 8) {
      return 'Password must contain at least eight characters.';
    }
    if (!RegExp(r'[A-Z]').hasMatch(value)) {
      return 'Must contain at least one uppercase letter.';
    }
    if (!RegExp(r'[a-z]').hasMatch(value)) {
      return 'Must contain at least one lowercase letter.';
    }
    if (!RegExp(r'[0-9]').hasMatch(value)) {
      return 'Must contain at least one digit.';
    }
    // I can't get a single quote to work below.
    if (!RegExp(r'[~@#$%^&*()\-_=+\[\]{\]}\\\|;:",<.>/?]').hasMatch(value)) {
      return 'Must contain at least one special character.';
    }
    return null;
  }

  String? validateUserName(value) {
    if (value == null || value.isEmpty) {
      return 'User name is required.';
    }
    if (value.length < 4) {
      return 'User name must be at least four characters.';
    }
    return null;
  }

  @override
  Widget build(BuildContext context) {
    Color errorColor = Theme.of(context).errorColor;
    Color primaryColor = Theme.of(context).primaryColor;

    bool valid = _formKey.currentState?.validate() ?? false;

    // Here is an example of cross-field validation.
    if (valid && userName == password) {
      message = 'User name and password cannot be the same.';
      valid = false;
    } else {
      message = null;
    }

    return Scaffold(
      appBar: AppBar(
        title: Text('Flutter Form Validation'),
      ),
      body: Form(
        autovalidateMode: AutovalidateMode.onUserInteraction,
        key: _formKey,
        child: Column(
          children: <Widget>[
            Text(
              'Login',
              style: TextStyle(
                color: primaryColor,
                fontSize: 24,
                fontWeight: FontWeight.bold,
              ),
            ),
            MyTextField(
              labelText: 'User Name',
              initialValue: userName,
              onChanged: (value) => setState(() {
                userName = value;
              }),
              validator: validateUserName,
            ),
            MyTextField(
              labelText: 'Password',
              initialValue: password,
              obscureText: true,
              onChanged: (value) => setState(() {
                password = value;
              }),
              validator: validatePassword,
            ),
            if (message != null)
              Text(message!, style: TextStyle(color: errorColor)),
            ElevatedButton(
              child: const Text('Login'),
              onPressed: valid ? login : null,
            ),
          ],
        ),
      ),
    );
  }
}
```

Some examples of using the `Form` widget show
calling `_formKey.currentState!.save` when the user taps a button
to indicate that they are ready to do something with the form data.
This calls the `onSaved` method on each of the form fields.
I prefer saving new values of form fields when the user changes them
using the callback specified in their `onChanged` arguments
as shown above.

## State Management

For state that is only used by a single stateful widget instance,
use the `setState` function from inside that widget.

For state that must be shared across multiple widget instances,
it is recommended to choose a state management package
provided by the community.
Popular packages include:

- {% aTargetBlank "https://bloclibrary.dev/", "bloc" %}
- {% aTargetBlank "https://pub.dev/documentation/flutter_cubit/latest/",
  "cubit" %}
- {% aTargetBlank "https://pub.dev/packages/get_it", "get_it" %}
- {% aTargetBlank "https://pub.dev/packages/get", "GetX" %}
- {% aTargetBlank "https://pub.dev/packages/provider", "provider" %} package
  (similar to the Context API in React)
- {% aTargetBlank "https://riverpod.dev", "Riverpod" %}

These reduce the need for stateful widgets.

### setState Function

The `setState` function marks the current widget as "dirty" which
causes its `build` method to be called again in order to rebuild its UI.

Typically changes to state properties are made
in a callback function that is passed to `setState`.
Avoid time consuming code inside these callback functions.
This code should be limited to assignments to state properties
and not computing new values.
For example:

```dart
setState(() {
  _counter++;
});
```

Changes to state properties can also be made before the call to `setState`
and it can be passed a callback function that does nothing.
For example:

```dart
_counter++;
setState(() {});
```

### Callback Functions

One way for a child widget to pass data to its parent widget
is to pass a function from the parent to the child
and have the child call the function with data to be shared.

The following code demonstrates this approach.
The `Home` widget renders a `Numbers` widget and passes it a callback function.
The `Numbers` widget renders two `TextField` widgets
where the user can enter numbers.
When either `TextField` value is changed,
it computes their sum and passes it to the parent widget, `Home`,
using the callback function.

```dart
class Home extends StatefulWidget {
  const Home({Key? key}) : super(key: key);

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  var sum = 0;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Callback Demo'),
      ),
      body: Center(
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: <Widget>[
            Numbers(callback: (value) {
              setState(() => sum = value);
            }),
            Text('The sum is $sum.'),
          ],
        ),
      ),
    );
  }
}

typedef NumbersCallback = void Function(int value);

class Numbers extends StatefulWidget {
  final NumbersCallback callback;

  Numbers({required this.callback, Key? key}) : super(key: key);

  @override
  State<Numbers> createState() => _NumbersState();
}

class _NumbersState extends State<Numbers> {
  final controller1 = TextEditingController();
  final controller2 = TextEditingController();

  @override
  initState() {
    super.initState();
    // When the text in either TextField is changed,
    // compute a new sum and send to the parent widget
    // using the callback function.
    controller1.addListener(compute);
    controller2.addListener(compute);
  }

  @override
  Widget build(BuildContext context) {
    return Row(
      children: [
        _buildTextField(controller: controller1),
        Icon(Icons.add),
        _buildTextField(controller: controller2),
      ],
    );
  }

  Widget _buildTextField({required TextEditingController controller}) {
    return SizedBox(
      child: TextField(
        controller: controller,
        decoration: InputDecoration(
          border: OutlineInputBorder(),
        ),
        keyboardType: TextInputType.number,
      ),
      width: 70,
    );
  }

  void compute() {
    var number1 = controllerToInt(controller1);
    var number2 = controllerToInt(controller2);
    widget.callback(number1 + number2);
  }

  int controllerToInt(TextEditingController controller) {
    try {
      // int.parse throws if the text cannot be converted to an int.
      return int.parse(controller.value.text);
    } catch (e) {
      return 0;
    }
  }
}
```

### InheritedWidget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/InheritedWidget-class.html",
"InheritedWidget" %} class is a base class for
classes that wish to share data with descendant widgets.
They take a child widget to wrap around.
Instances are immutable, but they can
hold objects whose properties can be mutated.

`InheritedWidget` does not provide a method to
notify descendants of property changes.
To achieve that, wrap it in a `StatefulWidget` or use `ChangeNotifier`.
All of that gets fairly verbose.
This is why using other state management solutions
such as provider (discussed next) are more popular.

Here is a basic example of using `InheritedWidget` just to pass data down.
It does not provide a way to modify the data.

```dart
import 'package:flutter/material.dart';

void main() => runApp(const MyApp());

class AppState {
  var count = 19;
  var name = 'Mark';
}

class InheritedState extends InheritedWidget {
  final state = AppState();

  InheritedState({
    required Widget child,
    Key? key,
  }) : super(key: key, child: child);

  // This is a convenience method to make it easier
  // for descendant widgets to get an instance.
  static InheritedState of(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<InheritedState>()
          as InheritedState;

  @override
  bool updateShouldNotify(InheritedState oldWidget) {
    return oldWidget.state != state;
  }
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'InheritedWidget Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      // This wraps HomePage in InheritedState so
      // any descendant widgets can get the state.
      home: InheritedState(child: HomePage()),
    );
  }
}

class HomePage extends StatelessWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('InheritedWidget Demo'),
      ),
      body: Center(
        child: ChildWidget(),
      ),
    );
  }
}

class ChildWidget extends StatelessWidget {
  const ChildWidget({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final state = InheritedState.of(context).state;
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      children: <Widget>[
        Text('count = ${state.count}'),
        Text('name = ${state.name}'),
      ],
    );
  }
}
```

### provider Package

The {% aTargetBlank "https://pub.dev/packages/provider", "provider" %} package
is is the state management approach recommended by the Flutter team.
It is "A wrapper around `InheritedWidget`
to make them easier to use and more reusable."
It vastly reduces the amount of boilerplate code that must be written
and provides many features not present in `InheritedWidget`.

The {% aTargetBlank
"https://docs.flutter.dev/development/data-and-backend/state-mgmt/simple",
"official documentation" %} says
"If you are new to Flutter and you don’t have a strong reason
to choose another approach (Redux, Rx, hooks, etc.),
this is probably the approach you should start with."
In June 2019 Chris Sells, the Flutter Project Manager, said
"Provider is the recommended way to do State Management for apps of all sizes."

The provider package was created by Remi Rousselet.
It is a wrapper around the {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/InheritedWidget-class.html",
"InheritedWidget" %} which is
a relatively complicated way to share mutable state.
For a short introduction, see this {% aTargetBlank
"https://www.youtube.com/watch?v=utrvu-eow6U", "YouTube video" %}.

The steps to use the provider package are:

1. Add dependency to `pubspec.yaml`.

   ```yaml
   dependencies:
     provider: ^6.0.1
   ```

2. Install by entering `flutter pub get` unless your IDE does this for you.

3. Create {% aTargetBlank
   "https://api.flutter.dev/flutter/foundation/ChangeNotifier-class.html",
   "ChangeNotifier" %} classes that hold state,
   define methods for modifying it, and define getters and setters.
   For example, the following can be defined
   in the file `lib/count_state.dart`:

   ```dart
   import 'package:flutter/foundation.dart'; // defines ChangeNotifier

   class CountState extends ChangeNotifier {
     // Can declare any number of state variables here.
     var _count = 0;

     int get count => _count;

     void decrement() {
       _count--;
       notifyListeners();
     }

     void increment() {
       _count++;
       notifyListeners();
     }

     void reset() {
       _count = 0;
       notifyListeners();
     }
   }
   ```

4. Create an instance of each `ChangeNotifier` class
   in the lowest part of the widget tree where they are needed.
   For example, the following can appear in the file `lib/main.dart`:

   ```dart
   import 'package:flutter/material.dart';
   import 'package:provider/provider.dart';
   import 'count_state.dart';
   import 'counter.dart';
   import 'report.dart';

   class MyApp extends StatelessWidget {
     const MyApp({Key? key}) : super(key: key);

     @override
     Widget build(BuildContext context) {
       return ChangeNotifierProvider<CountState>(
         create: (context) => CountState(),
         child: MaterialApp(
           home: HomePage(title: 'My App'),
           ..
         ),
       );
     }
     ...
   }

   // The build method of the HomePage widget can include:
   children: <Widget>[
     Counter(),
     Report(),
   ],
   ```

5. Use the state in descendant widgets of the one
   where its `ChangeNotifier` class was created.
   For example, here is `lib/counter.dart`:

   ```dart
   import 'package:flutter/material.dart';
   import 'package:provider/provider.dart';
   import 'circle_button.dart';
   import 'count_state.dart';
   import 'widget_extensions.dart';

   class Counter extends StatelessWidget {
     Counter({Key? key}) : super(key: key);

     @override
     Widget build(BuildContext context) {
       var countState = Provider.of<CountState>(context);
       var children = <Widget>[
         CircleButton(
           color: Colors.red,
           onPressed: () {
             countState.decrement();
           },
           size: 50,
           text: '-'),
         Text(countState.count.toString(), style: TextStyle(fontSize: 50)),
         CircleButton(
           onPressed: () {
             countState.increment();
           },
           size: 50,
           text: '+'),
       ].hSpacing(10);

       return Row(
         children: children,
         mainAxisAlignment: MainAxisAlignment.center,
       );
     }
   }
   ```

There are two approaches for access provider state.
The first is to use `Provider.of`.
The second is to use `Consumer`.
`Consumer` is a widget, so it can be only be used in a widget tree.
It is good for updating a specific widget when state changes,
but it makes the code more deeply nested.
`Provider.of` is a constructor,
so it can be used anywhere in a Dart function
and used to provide state access to multiple widgets.

Both approaches are demonstrated in `lib/report.dart` below:

```dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'count_state.dart';
import 'widget_extensions.dart';

class Report extends StatelessWidget {
  const Report({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    var countState = Provider.of<CountState>(context);
    return Row(
      children: [
        Text('Report: ${countState.count}'),
        ElevatedButton(
          child: Text('Reset'),
          onPressed: () {
          countState.reset();
        }),
      ].hSpacing(10),
      mainAxisAlignment: MainAxisAlignment.center,
    );

    /*
    return Consumer<CountState>(builder: (context, data, child) {
      return Row(
        children: [ Text('Report: ${data.count}'),
          ElevatedButton(
            child: Text('Reset'),
            onPressed: () {
              data.reset();
            }),
        ].hSpacing(10),
        mainAxisAlignment: MainAxisAlignment.center,
      );
    });
    */
  }
}
```

The following Flutter app provides a more
full-featured demonstration of using provider.
The same app will be shown later using other state management packages.
It is composed of two `ChangeNotifier` subclasses for managing state
and three custom widgets that update and display the state.
The user can enter a player name and a series of scores.
The total and average of the scores is calculated and displayed.
Code for this app is in
{% aTargetBlank "https://github.com/mvolkmann/flutter_provider", "GitHub" %}.

<img alt="provider Demo" style="width: 40%"
    src="/blog/assets/flutter-provider-demo.png?v={{pkg.version}}"
    title="provider Demo">

Here are examples of `ChangeNotifier` subclasses:

```dart
// player_state.dart
import 'package:flutter/foundation.dart'; // defines ChangeNotifier

class PlayerState extends ChangeNotifier {
  var _name = 'Mark';

  String get name => _name;

  set name(String value) {
    _name = value;
    notifyListeners();
  }
}
```

```dart
// game_state.dart
import 'package:flutter/foundation.dart'; // defines ChangeNotifier

int sum(List<int> numbers) =>
    numbers.isEmpty ? 0 : numbers.reduce((acc, score) => acc + score);

class GameState extends ChangeNotifier {
  final _scores = <int>[];

  void addScore(int score) {
    _scores.add(score);
    notifyListeners();
  }

  void clearScores() {
    _scores.clear();
    notifyListeners();
  }

  double get average => _scores.isEmpty ? 0 : sum(_scores) / _scores.length;

  int get count => _scores.length;

  int get total => sum(_scores);
}
```

Here is the app entry point that registers the `ChangeNotifier` subclasses:

```dart
// main.dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'game_state.dart';
import 'player_entry.dart';
import 'player_state.dart';
import 'score_entry.dart';
import 'score_report.dart';

void main() => runApp(
      MultiProvider(
        providers: [
          ChangeNotifierProvider(create: (context) => GameState()),
          ChangeNotifierProvider(create: (context) => PlayerState()),
        ],
        child: MyApp(),
      ),
    );

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'provider Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: Home(),
    );
  }
}

class Home extends StatelessWidget {
  const Home({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final gameState = Provider.of<GameState>(context);

    return Scaffold(
      appBar: AppBar(
        title: Text('provider Demo'),
      ),
      body: Center(
        child: Padding(
          padding: EdgeInsets.all(10),
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: <Widget>[
              PlayerEntry(),
              SizedBox(height: 10),
              ScoreEntry(),
              SizedBox(height: 10),
              Text('Number of scores = ${gameState.count}'),
              ScoreReport(),
            ],
          ),
        ),
      ),
    );
  }
}
```

Here are widgets that use the `ChangeNotifier` subclasses:

```dart
// player_entry.dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'player_state.dart';

class PlayerEntry extends StatelessWidget {
  const PlayerEntry({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final playerState = Provider.of<PlayerState>(context, listen: false);

    return Row(
      children: [
        Expanded(
          child: TextFormField(
            decoration: InputDecoration(
              border: OutlineInputBorder(),
              hintText: 'player name',
              labelText: 'Player',
            ),
            initialValue: playerState.name,
            onChanged: (String name) {
              playerState.name = name;
            },
          ),
        ),
      ],
    );
  }
}
```

```dart
// score_entry.dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'game_state.dart';

// This needs to be stateful in order
// to hold a TextEditingController instance.
class ScoreEntry extends StatefulWidget {
  const ScoreEntry({Key? key}) : super(key: key);

  @override
  _ScoreEntryState createState() => _ScoreEntryState();
}

class _ScoreEntryState extends State<ScoreEntry> {
  final tec = TextEditingController();

  @override
  Widget build(BuildContext context) {
    final gameState = Provider.of<GameState>(context, listen: false);

    return Column(
      children: [
        TextField(
          controller: tec,
          decoration: InputDecoration(
              border: OutlineInputBorder(),
              hintText: 'score',
              labelText: 'Score'),
          keyboardType: TextInputType.number,
        ),
        Row(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            // This is necessary so onPressed can be updated
            // when the text in the TextEditingController changes.
            ValueListenableBuilder<TextEditingValue>(
              valueListenable: tec,
              builder: (context, value, child) {
                return ElevatedButton(
                  child: Text('Save'),
                  onPressed: value.text.isEmpty ? null : () => save(gameState),
                );
              },
            ),
            SizedBox(width: 10),
            ElevatedButton(
              child: Text('Reset'),
              onPressed: () => gameState.clearScores(),
            ),
          ],
        ),
      ],
    );
  }

  void save(gameState) {
    var score = int.parse(tec.text);
    if (score != 0) gameState.addScore(score);
    tec.clear();
  }
}
```

```dart
// score_report.dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'game_state.dart';
import 'player_state.dart';

class ScoreReport extends StatelessWidget {
  ScoreReport({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final gameState = Provider.of<GameState>(context);
    final playerState = Provider.of<PlayerState>(context);

    return Column(children: [
      // Widgets that need to update when controller data changes
      // need to be wrapped in Obx.
      Text('Player is ${playerState.name}'),
      Text('Total is ${gameState.total}'),
      Text('Average is ${gameState.average.toStringAsFixed(2)}'),
    ]);
  }
}
```

### GetX Framework

{% aTargetBlank "https://pub.dev/packages/get", "GetX" %}
is a Flutter framework that provides a dependency manager,
state manager, navigation manager, and utility functions.
It can be found in pub.dev under the name "get".

GetX has a reputation for being easier to use
than other Flutter state management packages.
One critisism is that it is a very large package
that provides much more than state management.
It can bloat the size of applications that only use it for state management.
Another issue is that the official documentation is
difficult to navigate and doesn't provide many usage examples.

#### Dependency Management

GetX dependency management provides a way to register an object
in an ancestor widget and get a reference to it in decendant widgets.
This removes the need to explicitly pass objects
down through the widget hierarchy.
It provides a kind of dependency injection.

To register an object, call `Get.put`. For example:

```dart
// There is no need to capture a reference in a variable
// if the instance is not used in this file.
Get.put(PlayerController());

// Capture a reference if needed in this file.
final GameController gamectrl = Get.put(GameController());
```

To get a reference to a registered object, call `Get.find`.
For example:

```dart
final playerCtrl = Get.find<PlayerController>();
final gameCtrl = Get.find<GameController>();
```

This assumes that only one instance of each class is needed.
To register and find multiple instances of the same class,
include the `tag` argument in calls to `Get.put` and `Get.find`.
This must be a `String` that uniquely identifies an instance.
Consider using `static` properties to hold tag names and
place them in source files that can be imported everywhere they are needed.

An alternative to `Get.put` is to use `Get.lazyPut`.
This takes a function that creates and returns the object to be registered.
The function is not called until the
first call to `Get.find` that requests the object.
This avoids creating and registering objects
that are never used in a given user session.
For example:

```dart
get.lazyPut(() => PlayerController());
```

The examples above show registering and finding controller objects,
but any kind of object can be used.

#### State Management

GetX supports several approaches to managing state in a Flutter app.
This section only covers the approach that is seen as the easiest.
The steps require are:

1. Create controller classes that hold and update state
   that must be shared across multiple widgets.
   These must extend `GetxController`.
1. Define state properties in the controller classes.
   Typically these are private.
1. Define methods (getters, setters, and others) to provide access to state.
   Call `update()` at the end of any method that modifies state
   in order to notify listeners.
1. Add `.obs` after state property values to make them observable.
   This returns a value of a type whose name begins with `Rx`
   such as `RxString`.
1. Register the controller classes using `Get.put`
   near the top of the widget tree.
1. Get references to controller classes using `Get.find`
   in any widgets that need to get or set state.
1. Wrap widgets that need to be updated when state changes
   with the `Obx` widget.
   This runs a function passed to it every time that state it uses changes
   and the function should return a new widget instance
   created with the new state.

The following Flutter app demonstrates each of the steps above.
It is composed of two controllers for managing state
and three custom widgets that update and display the state.
The user can enter a player name and a series of scores.
The total and average of the scores is calculated and displayed.
Code for this app is in
{% aTargetBlank "https://github.com/mvolkmann/flutter_getx", "GitHub" %}.

<img alt="GetX Demo" style="width: 40%"
    src="/blog/assets/flutter-getx-demo.png?v={{pkg.version}}"
    title="GetX Demo">

Here are examples of controller classes:

```dart
// player_controller.dart
import 'package:get/get.dart';

class PlayerController extends GetxController {
  final _name = 'Mark'.obs; // observable String

  // _name is an RxString.
  // We need to add .value to get its value.
  String get name => _name.value;

  set name(String value) {
    _name.value = value;
    update();
  }
}
```

```dart
// game_controller.dart
import 'package:get/get.dart';

int sum(List<int> numbers) =>
    numbers.isEmpty ? 0 : numbers.reduce((acc, score) => acc + score);

class GameController extends GetxController {
  final _scores = <int>[].obs; // observable List of int values

  void addScore(int score) {
    _scores.add(score);
    update(); // notifies listeners
  }

  void clearScores() {
    _scores.clear();
    update();
  }

  double get average => _scores.isEmpty ? 0 : sum(_scores) / _scores.length;

  int get count => _scores.length;

  int get total => sum(_scores);
}
```

Here is the app entry point that registers the controllers:

```dart
// main.dart
import 'package:flutter/material.dart';
import 'package:get/get.dart';
import 'game_controller.dart';
import 'player_controller.dart';
import 'player_entry.dart';
import 'score_entry.dart';
import 'score_report.dart';

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    // Change MaterialApp to GetMaterialApp here to use features of GetX
    // not related to state management such as navigation.
    return MaterialApp(
      title: 'GetX Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: Home(),
    );
  }
}

class Home extends StatelessWidget {
  Home({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    // Register controllers with GetX so other widgets can find them.
    final GameController gameCtrl = Get.put(GameController());
    Get.put(PlayerController());

    return Scaffold(
      appBar: AppBar(
        title: Text('GetX Demo'),
      ),
      body: Center(
        child: Padding(
          padding: EdgeInsets.all(10),
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: <Widget>[
              PlayerEntry(),
              SizedBox(height: 10),
              ScoreEntry(),
              SizedBox(height: 10),
              Obx(() => Text('Number of scores = ${gameCtrl.count}')),
              ScoreReport(),
            ],
          ),
        ),
      ),
    );
  }
}
```

Here are widgets that use the controllers:

```dart
// player_entry.dart
import 'package:flutter/material.dart';
import 'package:get/get.dart';
import 'player_controller.dart';

class PlayerEntry extends StatelessWidget {
  PlayerEntry({Key? key}) : super(key: key);

  final playerCtrl = Get.find<PlayerController>();

  @override
  Widget build(BuildContext context) {
    return Row(
      children: [
        Expanded(
          child: TextFormField(
            decoration: InputDecoration(
              border: OutlineInputBorder(),
              hintText: 'player name',
              labelText: 'Player',
            ),
            initialValue: playerCtrl.name,
            onChanged: (String name) {
              playerCtrl.name = name;
            },
          ),
        ),
      ],
    );
  }
}
```

```dart
// score_entry.dart
import 'package:flutter/material.dart';
import 'package:get/get.dart';
import 'game_controller.dart';

// This needs to be stateful in order
// to hold a TextEditingController instance.
class ScoreEntry extends StatefulWidget {
  const ScoreEntry({Key? key}) : super(key: key);

  @override
  _ScoreEntryState createState() => _ScoreEntryState();
}

class _ScoreEntryState extends State<ScoreEntry> {
  final gameCtrl = Get.find<GameController>();

  final tec = TextEditingController();

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        TextField(
          controller: tec,
          decoration: InputDecoration(
              border: OutlineInputBorder(),
              hintText: 'score',
              labelText: 'Score'),
          keyboardType: TextInputType.number,
        ),
        Row(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            // This is necessary so onPressed can be updated
            // when the text in the TextEditingController changes.
            ValueListenableBuilder<TextEditingValue>(
              valueListenable: tec,
              builder: (context, value, child) {
                return ElevatedButton(
                  child: Text('Save'),
                  onPressed: value.text.isEmpty ? null : save,
                );
              },
            ),
            SizedBox(width: 10),
            ElevatedButton(
              child: Text('Reset'),
              onPressed: () => gameCtrl.clearScores(),
            ),
          ],
        ),
      ],
    );
  }

  void save() {
    var score = int.parse(tec.text);
    if (score != 0) gameCtrl.addScore(score);
    tec.clear();
  }
}
```

```dart
// score_report.dart
import 'package:flutter/material.dart';
import 'package:get/get.dart';
import 'game_controller.dart';
import 'player_controller.dart';

class ScoreReport extends StatelessWidget {
  final gameCtrl = Get.find<GameController>();
  final playerCtrl = Get.find<PlayerController>();

  ScoreReport({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Column(children: [
      // Widgets that need to update when controller data changes
      // need to be wrapped in Obx.
      Obx(() => Text('Player is ${playerCtrl.name}')),
      Obx(() => Text('Total is ${gameCtrl.total}')),
      Obx(() => Text('Average is ${gameCtrl.average.toStringAsFixed(2)}')),
    ]);
  }
}
```

#### Route Management

To register page routes, specify the `getPages` argument
in the `GetMaterialApp` constructor call.
The value should be a `List` of `GetPage` calls.
Each of these is passsed `name` and `page` arguments.
The `name` argument value is a `String`.
The `page` argument value is a function that returns a page widget.

GetX provides many functions that navigate to a new page.
The most useful of these functions are summarized below:

| Function   | Description                                                         | Documentation                                                                                                                         |
| ---------- | ------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `Get.back` | goes back to previous route, popping current route off of the stack | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_navigation_src_extension_navigation/GetNavigation/back.html", "back" %} |
| `Get.to`   | goes to a new route, pushing it onto the stack                      | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_navigation_src_extension_navigation/GetNavigation/to.html", "to" %}     |
| `Get.off`  | pops current route and pushes new route                             | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_navigation_src_extension_navigation/GetNavigation/to.html", "to" %}     |

#### Utilities

GetX provides many utility functions.
In order to use these, the topmost widget must be
changed from `MaterialApp` to `GetMaterialApp`.
The most useful of these functions are summarized below:

| Function                 | Description                                                                      | Documentation                                                                                                                                              |
| ------------------------ | -------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Get.bottomSheet`        | displays a bottom sheet                                                          | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_navigation_src_extension_navigation/ExtensionBottomSheet/bottomSheet.html", "bottomSheet" %} |
| `Get.changeTheme`        | changes to a given theme such as `ThemeData.dark()`                              | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_navigation_src_extension_navigation/GetNavigation/changeTheme.html", "changeTheme" %}        |
| `Get.defaultDialog`      | displays a dialog                                                                | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_navigation_src_extension_navigation/ExtensionDialog/defaultDialog.html", "defaultDialog" %}  |
| `Get.snackbar`           | displays a snackbar                                                              | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_navigation_src_extension_navigation/ExtensionSnackbar/snackbar.html", "snackbar" %}          |
| `GetUtils.camelCase`     | returns camelCase version of a `String`                                          | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_utils_src_get_utils_get_utils/GetUtils/camelCase.html", "camelCase" %}                       |
| `GetUtils.capitalize`    | returns version of a `String` where the first letter of each word is capitalized | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_utils_src_get_utils_get_utils/GetUtils/capitalize.html", "capitalize" %}                     |
| `GetUtils.isEmail`       | returns a `bool` indicaing if a `String` is a valid email address                | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_utils_src_get_utils_get_utils/GetUtils/isEmail.html", "isEmail" %}                           |
| `GetUtils.isPhoneNumber` | returns a `bool` indicaing if a `String` is a valid phone number                 | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_utils_src_get_utils_get_utils/GetUtils/isPhoneNumber.html", "isPhoneNumber" %}               |
| `GetUtils.isSSN`         | returns a `bool` indicaing if a `String` is a valid social security number       | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_utils_src_get_utils_get_utils/GetUtils/isSSN.html", "isSSN" %}                               |
| `GetUtils.isURL`         | returns a `bool` indicaing if a `String` is a valid URL                          | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_utils_src_get_utils_get_utils/GetUtils/isURL.html", "isURL" %}                               |
| `GetUtils.snakeCase`     | returns snake_case version of a `String`                                         | {% aTargetBlank "https://pub.dev/documentation/get/latest/get_utils_src_get_utils_get_utils/GetUtils/snakeCase.html", "snakeCase" %}                       |

GetX provides language translation using the `.tr` getter functino
and the `Get.changeLocale` function.`

### Riverpod Package

{% aTargetBlank "https://riverpod.dev", "Riverpod" %}
was created by Remi Rousselet, who is the same developer that created
{% aTargetBlank "https://pub.dev/packages/provider", "provider" %}.
The name comes from rearranging the letters in "provider".
It addresses several issues with provider including:

1. provider depends on Flutter and
   cannot be used in non-Flutter Dart appliations.
   For example, getting state references requires access to a
   Flutter `BuildContext` which limits where they can be obtained.

1. provider only supports keeping state in a single instance
   of each state class.

1. provider does not support asynchronous state.

1. provider does not force code to handle all possible states
   with compile-time checks.
   For example, a state that returns a `Future` can be
   pending, complete with a value, or complete with an error.

There are three packages for using Riverpod.

1. {% aTargetBlank
   "https://github.com/rrousselGit/river_pod/tree/master/packages/riverpod",
   "riverpod" %} for non-Flutter Dart applications.
1. {% aTargetBlank "https://pub.dev/packages/flutter_riverpod",
   "flutter_riverpod" %} for Flutter applications.
1. {% aTargetBlank "https://pub.dev/packages/hooks_riverpod",
   "hooks_riverpod" %} for Flutter applications
   that also use {% aTargetBlank
   "https://github.com/rrousselGit/flutter_hooks", "flutter_hooks" %}.

Only use of flutter_riverpod v2 is described here.

Riverpod providers are objects that encapsulate state,
provide methods for modifying the state,
and support listening for changes.
They are typically declared as global constants.

In order for a Flutter application to use Riverpod,
it must wrap the topmost application widget in a `ProviderScope`.
For example:

```dart
void main() => runApp(ProviderScope(child: MyApp()));
```

All the kinds of providers supported by Riverpod are summarized below.

| Provider                 | Description                                                                              |
| ------------------------ | ---------------------------------------------------------------------------------------- |
| `Provider`               | provides read-only data synchronously; can modify its own value over time                |
| `StateProvider`          | provides writable data synchronously with `state` getter and setter methods              |
| `FutureProvider`         | provides read-only data asynchronously                                                   |
| `StreamProvider`         | provides a stream of read-only data, updated as new data becomes available               |
| `ChangeNotifierProvider` | state is mutable; must call `notifyListeners` after changing                             |
| `StateNotifierProvider`  | state is immutable but can be replaced; replacing state automatically notifies listeners |

The first four options are best suited when
it is acceptable to work with state objects as a whole.
`StateProvider` is the only one of thees options
that supports updating state and that is done by
replacing entire objects rather than updating individual properties.

`ChangeNotifierProvider` is a good choice when there is a need
to update individual properties of state objects
rather than replacing entire objects.

`StateNotifierProvider` is a good alternative to `ChangeNotifierProvider`
when there is a desire for state objects to be immutable.
It also has the advantage that change notification is automatic.

The following app demonstrates basic usage of each provider type.
Code for this app is in
{% aTargetBlank "https://github.com/mvolkmann/flutter_riverpod_demo", "GitHub" %}.

<img alt="Riverpod Demo" style="width: 40%"
    src="/blog/assets/flutter-riverpod-demo.png?v={{pkg.version}}"
    title="Riverpod Demo">

```dart
// main.dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'change_notifier_page.dart';
import 'computed_provider_page.dart';
import 'future_provider_page.dart';
import 'provider_page.dart';
import 'state_notifier_page.dart';
import 'state_provider_page.dart';
import 'stream_provider_page.dart';

void main() => runApp(ProviderScope(child: MyApp()));

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Riverpod Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: Home(),
      routes: {
        ProviderPage.route: (_) => ProviderPage(),
        StateProviderPage.route: (_) => StateProviderPage(),
        FutureProviderPage.route: (_) => FutureProviderPage(),
        StreamProviderPage.route: (_) => StreamProviderPage(),
        ComputedProviderPage.route: (_) => ComputedProviderPage(),
        ChangeNotifierPage.route: (_) => ChangeNotifierPage(),
        StateNotifierPage.route: (_) => StateNotifierPage(),
      },
    );
  }
}

class Home extends StatelessWidget {
  const Home({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Riverpod Demo'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            ElevatedButton(
              child: Text('Provider'),
              onPressed: () {
                Navigator.pushNamed(context, ProviderPage.route);
              },
            ),
            ElevatedButton(
              child: Text('StateProvider'),
              onPressed: () {
                Navigator.pushNamed(context, StateProviderPage.route);
              },
            ),
            ElevatedButton(
              child: Text('FutureProvider'),
              onPressed: () {
                Navigator.pushNamed(context, FutureProviderPage.route);
              },
            ),
            ElevatedButton(
              child: Text('StreamProvider'),
              onPressed: () {
                Navigator.pushNamed(context, StreamProviderPage.route);
              },
            ),
            ElevatedButton(
              child: Text('ComputedProvider'),
              onPressed: () {
                Navigator.pushNamed(context, ComputedProviderPage.route);
              },
            ),
            ElevatedButton(
              child: Text('ChangeNotifier'),
              onPressed: () {
                Navigator.pushNamed(context, ChangeNotifierPage.route);
              },
            ),
            ElevatedButton(
              child: Text('StateNotifier'),
              onPressed: () {
                Navigator.pushNamed(context, StateNotifierPage.route);
              },
            ),
          ],
        ),
      ),
    );
  }
}
```

```dart
// provider_page.dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

// This provide a single, immutable value.
// The ref parameter can be used to access other providers.
// This could be used to compute the value of this provider.
final greetingProvider = Provider((ref) => 'Hello, World!');

// Extending ConsumerWidget instead of StatelessWidget
// causes a WidgetRef argument to be passed to the build method.
// ConsumerWidget extends ConsumerStatefulWidget
// which extends StatefulWidget.
class ProviderPage extends ConsumerWidget {
  static const route = '/provider';

  ProviderPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    var watch = ref.watch; // used later

    // This is one way to access the state of a provider
    // that makes it available throughout this widget.
    // When the value changes,
    // this entire widget will be rebuilt.
    final greeting = ref.watch(greetingProvider);

    return Scaffold(
      appBar: AppBar(
        title: Text('Provider Demo'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            Text(greeting),
            Text(watch(greetingProvider)), // same as previous line
            // This is another way to access the state of a provider
            // that limits the scope to a single child widget.
            // When the value changes, only this child widget will be rebuilt.
            Consumer(builder: (context, ref, child) {
              final greeting = ref.watch(greetingProvider);
              return Text(greeting);
            }),
          ],
        ),
      ),
    );
  }
}
```

```dart
// state_provider_page.dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

// Adding ".autoDispose" causes the provider to lose its state
// there are no longer any widgets listening to it.
// The value will reset to zero every time we return to this page.
final counterStateProvider = StateProvider.autoDispose<int>((ref) => 0);

class StateProviderPage extends ConsumerWidget {
  static const route = '/state-provider';

  StateProviderPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final counter = ref.watch(counterStateProvider);

    return Scaffold(
      appBar: AppBar(
        title: Text('StateProvider Demo'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            Text('$counter'),
            FloatingActionButton(
              child: Icon(Icons.add),
              onPressed: () => incrementCounter(ref),
            ),
          ],
        ),
      ),
    );
  }

  void incrementCounter(ref) {
    // This is overly complex!
    ref.read(counterStateProvider.state).state += 1;
  }
}
```

```dart
// future_provider_page.dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

// FutureProvider automatically caches results.
// Adding ".autoDispose" causes it to lose the cached value
// when there are no longer any widgets listening to it.
// Every time we return to this page, it will be recomputed.
final futureProvider = FutureProvider.autoDispose<int>((ref) async {
  // Simulate time to make a API request.
  await Future.delayed(Duration(seconds: 2));
  return 19;
});

class FutureProviderPage extends ConsumerWidget {
  static const route = '/future-provider';

  FutureProviderPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final score = ref.watch(futureProvider);

    return Scaffold(
      appBar: AppBar(
        title: Text('FutureProvider Demo'),
      ),
      body: Center(
        child: score.when(
          loading: () => CircularProgressIndicator(),
          data: (value) => Text('$value'),
          error: (e, stack) => Text('Error: $e'),
        ),
      ),
    );
  }
}
```

```dart
// stream_provider_pagedart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

// StreamProvider automatically continues using the same stream.
// Adding ".autoDispose" causes it to dispose of the strean
// when there are no longer any widgets listening to it.
// Every time we return to this page, it will
// create a new stream starting from the beginning.
final streamProvider = StreamProvider.autoDispose<int>(
  (ref) => Stream.periodic(Duration(seconds: 1), (index) => index + 1),
);

class StreamProviderPage extends ConsumerWidget {
  static const route = '/stream-provider';

  StreamProviderPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final stream = ref.watch(streamProvider);

    return Scaffold(
      appBar: AppBar(
        title: Text('StreamProvider Demo'),
      ),
      body: Center(
        child: stream.when(
          loading: () => CircularProgressIndicator(),
          data: (value) => Text('$value'),
          error: (e, stack) => Text('Error: $e'),
        ),
        //TODO: Why can't this be used instead of the above code?
        //child: buildStream(stream),
      ),
    );
  }

  Widget buildStream(stream) {
    return stream.when(
      loading: () => CircularProgressIndicator(),
      data: (value) => Text('$value'),
      error: (e, stack) => Text('Error: $e'),
    );
  }
}
```

```dart
// computed_provider_page.dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

final priceStateProvider = StateProvider<double>((ref) => 100);
final taxStateProvider = StateProvider<double>((ref) => 0.075);
final totalStateProvider = StateProvider<double>((ref) {
  final price = ref.watch(priceStateProvider);
  final tax = ref.watch(taxStateProvider);
  return price * (1 + tax);
});

class ComputedProviderPage extends ConsumerWidget {
  static const route = '/scoped-provider';

  ComputedProviderPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final total = ref.watch(totalStateProvider);

    return Scaffold(
      appBar: AppBar(
        title: Text('ComputedProvider Demo'),
      ),
      body: Center(
        child: Padding(
          padding: EdgeInsets.all(10),
          child: Column(
            children: [
              numberField('Price', priceStateProvider, ref),
              SizedBox(height: 10),
              numberField('Tax', taxStateProvider, ref),
              SizedBox(height: 10),
              Text('Total: \$${total.toStringAsFixed(2)}'),
            ],
          ),
        ),
      ),
    );
  }

  TextFormField numberField(
    String label,
    StateProvider provider,
    WidgetRef ref,
  ) {
    final value = ref.watch(provider);
    return TextFormField(
      decoration: InputDecoration(
        border: OutlineInputBorder(),
        labelText: label,
      ),
      initialValue: value.toString(),
      keyboardType: TextInputType.number,
      onChanged: (String value) => setValue(provider, ref, value),
    );
  }

  void setValue(StateProvider provider, ref, value) {
    ref.read(provider.state).state = double.parse(value);
  }
}
```

```dart
// change_notifier_page.dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

class DogChangeNotifier extends ChangeNotifier {
  String _breed = 'Whippet';
  String _name = 'Comet';

  void update({String? breed, String? name}) {
    if (breed != null) _breed = breed;
    if (name != null) _name = name;
    notifyListeners();
  }

  @override
  void dispose() {
    print('DogNotifier dispose called');
    super.dispose();
  }

  @override
  String toString() => '$_name is a $_breed.';
}

// Adding ".autoDispose" causes the provider to lose its state
// there are no longer any widgets listening to it.
// It also enables its "dispose" method to be called.
final dogChangeNotifierProvider =
    ChangeNotifierProvider.autoDispose<DogChangeNotifier>(
  (ref) => DogChangeNotifier(),
);

class ChangeNotifierPage extends ConsumerWidget {
  static const route = '/change-notifier';

  ChangeNotifierPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final dog = ref.watch(dogChangeNotifierProvider);

    return Scaffold(
      appBar: AppBar(
        title: Text('Provider Demo'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            Text(dog.toString()),
            ElevatedButton(
              child: Text('Change Dog'),
              onPressed: () {
                dog.update(breed: 'GSP', name: 'Oscar');
              },
            ),
          ],
        ),
      ),
    );
  }
}
```

```dart
// state_notifier_page.dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

// This is an immutable class.
class Dog {
  final String breed;
  final String name;

  const Dog({
    this.breed = 'Whippet',
    this.name = 'Comet',
  });

  Dog copy({String? breed, String? name}) => Dog(
        breed: breed ?? this.breed,
        name: name ?? this.name,
      );

  @override
  String toString() => '$name is a $breed.';
}

class DogStateNotifier extends StateNotifier<Dog> {
  DogStateNotifier() : super(Dog());

  void setBreed(String breed) {
    state = state.copy(breed: breed);
  }

  void setName(String name) {
    state = state.copy(name: name);
  }

  @override
  void dispose() {
    print('DogStateNotifier dispose called');
    super.dispose();
  }
}

// Adding ".autoDispose" causes the provider to lose its state
// there are no longer any widgets listening to it.
// It also enables its "dispose" method to be called.
final dogNotifierProvider =
    StateNotifierProvider.autoDispose<DogStateNotifier, Dog>(
  (ref) => DogStateNotifier(),
);

class StateNotifierPage extends ConsumerWidget {
  static const route = '/state-notifier';

  StateNotifierPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final dog = ref.watch(dogNotifierProvider);
    final notifier = ref.read(dogNotifierProvider.notifier);

    return Scaffold(
      appBar: AppBar(
        title: Text('Provider Demo'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            Text(dog.toString()),
            ElevatedButton(
              child: Text('State Dog'),
              onPressed: () {
                notifier.setBreed('GSP');
                notifier.setName('Oscar');
              },
            ),
          ],
        ),
      ),
    );
  }
}
```

## Asynchronous Data

In some cases it is desireable to load data in an asynchronous way
before rendering a given widget.
One way to do this is to use the {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/FutureBuilder-class.html",
"FutureBuilder" %} widget.
This is typically used in the `build` method of a `StatefulWidget`.

The `future` argument should be set to a `Future` object.
This is typically is returned by function that obtains data asynchronously,
perhaps by sending an HTTP request or querying a database.

The `builder` argument should be set to a function
that takes a `BuildContext` and an `AsyncSnapshot`.
The function returns a `Widget` to render that is selected
based on the value of `snapshot.connectionState`.
Possible values include `none`, `waiting`, `active`, and `done`.

If an error occurs in the `Future`,
the `snapshot.hasError` property is set to `true` and
the `snapshot.error` property is set to an object describing the error.

`FutureBuilder` calls `setState` when the `Future` completes
to trigger rebuilding the widget.
This means that the `Future` can set state properties
without the need to call `setState`.

The following code demonstrates basic usage of `FutureBuilder`:

```dart
Container(
  child: FutureBuilder(
    // Passing context can be useful when
    // using a state management approach like provider.
    future: _loadData(context),
    builder: (context, snapshot) {
      if (snapshot.hasError) {
        return Text('Error fetching data: ${snapshot.error}');
      }
      if (snapshot.connectionState != ConnectionState.done) {
        return CircularProgressIndicator();
      }
      return Text(_loadedData);
    },
  ),
)
```

## Persisting State

There are many approaches to persisting app data
so it is not lost when an app is closed.
Two popular options are:

- {% aTargetBlank "https://pub.dev/packages/shared_preferences",
  "shared_preferences" %} package
- {% aTargetBlank "https://docs.flutter.dev/cookbook/persistence/sqlite",
  "SQLite" %} database on the device

### shared_preferences Package

The pub.dev package {% aTargetBlank
"https://pub.dev/packages/shared_preferences", "shared_preferences" %}
"wraps platform-specific persistent storage for simple data."
It is used to hold key/value pairs where the keys are strings and the
values have the types `bool`, `int`, `double`, `String`, or `List<String>`.

The following code demonstrates writing functions
that set, get, and remove values of specific keys.

```dart
import 'package:shared_preferences/shared_preferences.dart';

void setDemo() async {
  final prefs = await SharedPreferences.getInstance();
  await prefs.setBool('happy', true);
  await prefs.setInt('counter', 19);
  await prefs.setDouble('distance', 3.14);
  await prefs.setString('message', 'Hello');
  await prefs.setStringList('message', ['red', 'orange', 'yellow']);
}

void getDemo() {
  final prefs = await SharedPreferences.getInstance();
  final happy = prefs.getBool('happy');
  final counter = prefs.getInt('counter');
  final distance = prefs.getDouble('distance');
  final message = prefs.getString('message');
  final colors = prefs.getStringList('colors');
}

void removeDemo() {
  final prefs = await SharedPreferences.getInstance();
  final success = await prefs.remove('counter');
  if (success) print('The counter value was removed.');
}
```

### SQLite

The SQLite database is a popular choice for persisting data on mobile devices.
The pub.dev package
{% aTargetBlank "https://pub.dev/packages/sqflite", "sqflite" %}
is the most popular way to access a SQLite database in a Flutter application.

For a short introduction, see this {% aTargetBlank
"https://www.youtube.com/watch?v=HefHf5B1YM0", "YouTube video" %}.

The steps to use sqflite are:

1. Add the `path` and `sqflite` dependencies in `pubspec.yaml`.

1. Define model classes like the following
   corresponding to each database table:

```dart
class Dog {
  final int id;
  final int age;
  final String breed;
  final String name;

  Dog({required this.age, required this.breed, required this.name});

  // Dart doesn't support introspection,
  // so this conversion cannot be automated.
  Map<String, dynamic> toMap() {
    return {'id': id, 'age': age, 'breed': breed, 'name': name};
  }

  // For debugging
  @override
  String toString() {
    return 'Dog{id: $id, name: $name, breed: $breed, age: $age}';
  }
}
```

1. Get a connection to the database.

   ```dart
   WidgetsFlutterBinding.ensureInitialized();
   final database = await openDatabase(
     join(await getDatabasesPath(), 'doggie_database.db'),
     onConfigure: (db) async {
       // Foreign keys must be enabled in order to use them
       // and support cascading deletes.
       await db.execute('pragma foreign_keys = ON');
     },
     // The version can be used to perform database upgrades and downgrades.
     version: 1,
     ...
   );
   ```

1. Create a table corresponding to each model class
   by inserting code like the following where `...` appears above.

   ```dart
     onCreate: (db, version) {
       return db.execute(
        'create table if not exists dogs(' +
        'id integer primary key autoincrement, age integer, breed text, name text)',
       );
     },
     // The version provides a path to perform database upgrades and downgrades.
     version: 1,
   ```

1. Write a function that inserts a record.

   ```dart
   Future<void> insertDog(Dog dog) {
     return database.insert(
       'dogs',
       dog.toMap(),
       conflictAlgorithm: ConflictAlgorithm.replace,
     );
   }

   var comet = Dog(name: 'Comet', breed: 'Whippet', age: 1);
   var id = await insertDog(comet);
   ```

1. Write a function that retrieves records.

   ```dart
   Future<List<Dog>> getDogs() async {
     final List<Map<String, dynamic>> maps = await database.query('dogs');
     return List.generate(maps.length, (index) {
       var map = maps[index];
       return Dog(
         id: map['id'],
         age: map['age'],
         breed: map['breed'],
         name: map['name'],
       );
     });
   }

   var dogs = await getDogs();
   ```

1. Write a function that updates a record.

   ```dart
   Future<void> updateDog(Dog dog) {
     return database.update(
       'dogs',
       dog.toMap(),
       where: 'id = ?',
       // This prevents SQL injection.
       whereArgs: [dog.id],
     );
   }

   comet.age += 1;
   await updateDog(comet);
   ```

1. Write a function that deletes a record.

   ```dart
   Future<void> deleteDog(int id) {
     return database.delete(
       'dogs',
       where: 'id = ?',
       whereArgs: [id],
     );
   }

   await deleteDog(comet.id);
   ```

For a working example, see this {% aTargetBlank
"https://github.com/mvolkmann/flutter_sqlite", "GitHub repo" %}.

## Navigation

There are many ways to implement page navigation in Flutter.
Four that are described here involve using the `Navigator` class
and the widgets `BottomNavigationBar`, `Drawer`, and `PageView`.
Each approach is described below.

### Navigator Class

The `Navigator` class supports programmatic navigation
between pages of an app, also referred to as "routes".
It provides platform-aware transitions
whose animations differ between Android and iOS.
Android page transitions slide up from the bottom.
iOS page transitions slide in from the left.

Visited routes are maintained on a stack.
Pushing a route onto the stack navigates to it.
Popping a route from the stack returns to the previous route.

Each page is responsible for configuring its layout
which often includes a `Scaffold` and `AppBar`.
The `AppBar` provides a `<` back button for returning to the previous route.
For consistency it is useful to define a class that provides this structure
and then define page widgets that use this class.
For example:

```dart
import 'package:flutter/material.dart';

class MyPage extends StatelessWidget {
  final Widget child;
  final String title;

  const MyPage({
    Key? key,
    required this.title,
    required this.child,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: child,
    );
  }
}
```

Here is an example of a page class that uses `MyPage`:

```dart
import 'package:flutter/material.dart';
import 'my_page.dart';

class HelpPage extends StatelessWidget {
  static const route = '/help'; // used when registering this route

  const HelpPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MyPage(
      title: 'Help',
      child: Column(
        children: <Widget>[
          Text('Help is coming soon!')
        ],
      )
    );
  }
}
```

To navigate to a new page, push it onto the navigation stack.
One way to do this is to specify the widget responsible for
rendering the new page with the {% aTargetBlank
"https://api.flutter.dev/flutter/material/MaterialPageRoute-class.html",
"MaterialPageRoute" %} or {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoPageRoute-class.html",
"CupertinoPageRoute" %} class.
These differ in the transition animation they provide.

For example:

```dart
Navigator.push(
  context,
  MaterialPageRoute(builder: (context) => SomeWidget()),
);
```

To programmatically return to the previous page call `Navigator.pop(context)`.

Another approach is for the app class, typically defined in `lib/main.dart`,
to register named routes with the `routes` argument to the `MaterialApp` widget.
Assigning names to routes makes it easier to
refer to them from multiple widgets.
It is recommended to define route names as static constants in each page class,
(as shown above) except for the page class of the home route.
For example:

```dart
import 'about_page.dart';
import 'help_page.dart';
import 'home_page.dart';
...
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Navigation',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: HomePage(), // the '/' route
      routes: {
        AboutPage.route: (_) => AboutPage(),
        HelpPage.route: (_) => HelpPage(),
      },
    );
  }
```

Call the `Navigator.pushNamed` method to
push a named route on the stack and navigate to it.
Call the `Navigator.pushReplacementNamed` method to
replace top route on stack with a named route and navigate to it.
For example:

```dart
ElevatedButton(
  child: Text('About'),
  onPressed: () {
    Navigator.pushNamed(context, '/about');
  },
)
```

To pass arguments to a named route, add the `arguments` named argument in the
call to `Navigator.pushNamed` with a value that can be any kind of object.

To retrieve the arguments value inside the page widget,
add the following line in the `build` method:

```dart
final arguments = ModalRoute.of(context)!.settings.arguments;
```

Routes are not required to be named.
Call the `Navigator.push` method to
push an unnamed route on the stack and navigate to it.
Call the `Navigator.pushReplacement` method to
replace top route on stack with an unnamed route and navigate to it.
For example:

```dart
ElevatedButton(
  child: Text('Go to SomePage'),
  onPressed: () => Navigator.push(
    context,
    MaterialPageRoute<void>(
      builder: (BuildContext context) {
        return SomePage();
      },
    ),
  ),
)
```

`MaterialPageRoute` creates a platform-aware transition between routes.
This means that the animation that occurs during a page transition
differs between Android and iOS.

Using a `builder` above allows Flutter to
delay creating the route widget until it is needed.

Using named routes instead of unnamed routes has two downsides.
First, the constructor of the page widgets cannot take custom arguments.
Second, the mechanism described above for passing arguments to a named route
does not allow type checking to be performed at compile-time.

### BottomNavigationBar Widget

This approach to page navigation displays
a row of buttons at the bottom of page,
one for each top-level page in the app.
Clicking a button causes the corresponding page to be rendered.
It doesn't use the `Navigator` class and routes are not pushed onto a stack.

The `BottomNavigationBar` widget renders the row of buttons.
It works best when there are three to five pages.
This widget doesn't implement page navigation.
That part is left up to you, but is not difficult.

The steps to use `BottomNavigationBar` are:

- Create a stateful widget.
- In the `build` method return a `Scaffold` widget
  with the following named arguments:
  - `appBar` set to an `AppBar` instance that specifies
    the title to be displayed for the current page
  - `body` set to the widget to display for the current page
  - `bottomNavigationBar` set to a `BottomNavigationBar` instance
    created with the following named arguments:
    - `currentIndex` set to the index of the currently selected page
    - `onTap` set to a function that takes a selected index
      and saves it in the state of the stateful widget being defined
    - `selectedItemColor` set to the color to use
      for the icon and label of the selected page
    - `items` set to a `List<BottomNavigationBarItem>`
      where each element describes the `icon` and `label`
      to display in the bottom bar for one of the pages

That's a lot of details to get right!
Fortunately the code for doing all of this is typically the same in every app.
The helper class below handles all of this.
This code can be copied and used without modification in any Flutter app.

```dart
import 'package:flutter/material.dart';

class NavOption {
  final IconData icon;
  final String label;
  final Widget widget;

  const NavOption({
    required this.icon,
    required this.label,
    required this.widget,
  });
}

class BottomNavigation extends StatefulWidget {
  final List<NavOption> options;

  const BottomNavigation({
    Key? key,
    required this.options,
  }) : super(key: key);

  @override
  State<BottomNavigation> createState() => _BottomNavigationState();
}

class _BottomNavigationState extends State<BottomNavigation> {
  int _pageIndex = 0;

  @override
  Widget build(BuildContext context) {
    var items = widget.options
        .map(
          // To use NavigationBar instead,
          // change BottomNavigationBarItem to NavigationDestination.
          (option) => BottomNavigationBarItem(
            icon: Icon(option.icon),
            label: option.label,
          ),
        )
        .toList();

    return Scaffold(
      appBar: AppBar(
        title: Text(widget.options[_pageIndex].label),
      ),
      body: Center(child: widget.options[_pageIndex].widget),
      // Can change BottomNavigationBar to NavigationBar.
      bottomNavigationBar: BottomNavigationBar(
        // To use NavigationBar instead,
        // change currentIndex to NavigationDestination.
        currentIndex: _pageIndex,
        // To use NavigationBar instead,
        // change onTap to onDestinationSelected.
        onTap: (int index) {
          setState(() => _pageIndex = index);
        },
        // To use NavigationBar instead, remove this argument.
        // The selected item will automatically
        // be given a unique background color.
        selectedItemColor: Colors.green,
        // To use NavigationBar instead,
        // change items to destinations.
        items: items,
      ),
    );
  }
}
```

Here is an example of using the helper class above
to add a `BottomNavigationBar` to an app:

<img alt="Flutter BottomNavigationBar" style="width: 40%"
    src="/blog/assets/flutter-bottomnavigationbar.png?v={{pkg.version}}"
    title="Flutter BottomNavigationBar">

```dart
import 'package:flutter/material.dart';
import 'about_page.dart';
import 'bottom_navigation.dart';
import 'home_page.dart';
import 'settings_page.dart';

void main() => runApp(const MyApp());

class MyApp extends StatelessWidget {
  static const options = <NavOption>[
    NavOption(icon: Icons.info, label: 'About', widget: AboutPage()),
    NavOption(icon: Icons.home, label: 'Home', widget: HomePage()),
    NavOption(icon: Icons.settings, label: 'Settings', widget: SettingsPage()),
  ];

  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'BottomNavigationBar Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: BottomNavigation(options: options),
    );
  }
}
```

### Drawer Widget

This approach to page navigation uses a hamburger menu
that slides in from the left to display a list of page links.
Clicking a page link causes the corresponding page to be rendered.
It doesn't use the `Navigator` class and routes are not pushed onto a stack.

The `Drawer` widget doesn't implement page navigation.
That part is left up to you, but is not difficult.

The steps to use `Drawer` are:

- Create a stateful widget.
- In the `build` method return a `Scaffold` widget
  with the following named arguments:
  - `appBar` set to an `AppBar` instance that specifies
    the title to be displayed for the current page
  - `body` set to the widget to display for the current page
  - `drawer` set to a `Drawer` instance
    created with the following named arguments:
    - `color` set to the drawer background color
    - `drawer` set to a `Drawer` widget
      with a `child` argument set to a `Container` widget
      with a `child` argument set to a `ListView` widget
      with a `children` argument set to a `List`
    - The `List` should contain a `DrawerHeader` widget
      and one `ListTile` widget for each page link.
    - The `DrawerHeader` widget has a `child` argument
      that can be set to any widget.
    - The `ListTile` widget has the arguments
      `leading` (typically set to an icon),
      `title` (typically set to a `Text` widget), and
      `onTap` (set to a function to call when the `ListTile` is tapped).
    - The `onTap` function should set state that affects
      the `appBar` `title` value and
      the `Scaffold` body where the selected page is rendered.

That's a lot of details to get right!
Fortunately the code for doing all of this is typically the same in every app.
The helper class below handles all of this.
This code can be copied and used without modification in any Flutter app.

```dart
import 'package:flutter/material.dart';
import 'widget_extensions.dart';

class DrawerItem {
  final String title;
  final IconData icon;
  final Widget widget;

  DrawerItem({
    required this.title,
    required this.icon,
    required this.widget,
  });
}

class DrawerScaffold extends StatefulWidget {
  final List<DrawerItem> drawerItems;

  final Color? bgColor;
  final Color? fgColor;
  final double width;

  DrawerScaffold({
    Key? key,
    required this.drawerItems,
    this.bgColor,
    this.fgColor,
    this.width = 140,
  }) : super(key: key);

  @override
  State<DrawerScaffold> createState() => _DrawerScaffoldState();
}

class _DrawerScaffoldState extends State<DrawerScaffold> {
  var pageIndex = 0;

  @override
  Widget build(BuildContext context) {
    var drawerBgColor = widget.bgColor ?? Colors.blue.shade700;
    var drawerFgColor = widget.fgColor ?? Colors.white;

    var listTiles = [];
    for (var i = 0; i < widget.drawerItems.length; i++) {
      var pageItem = widget.drawerItems[i];
      listTiles.add(ListTile(
        leading: Icon(pageItem.icon, color: drawerFgColor),
        minLeadingWidth: 10, // decreases space between leading and title
        title: Text(
          pageItem.title,
          style: TextStyle(color: drawerFgColor),
        ),
        onTap: () {
          setState(() => pageIndex = i);
          Navigator.pop(context);
        },
      ));
    }

    return Scaffold(
      // Scaffolds with an AppBar automatically
      // get a hamburger menu on the left side.
      appBar: AppBar(
        title: Text(widget.drawerItems[pageIndex].title),
      ),
      body: Center(child: widget.drawerItems[pageIndex].widget),
      drawer: SizedBox(
        width: widget.width,
        child: Drawer(
          child: Container(
            color: drawerBgColor,
            child: ListView(
              padding: EdgeInsets.zero, // removes default padding
              children: [
                DrawerHeader(
                  child: IconButton(
                    color: drawerFgColor,
                    icon: Icon(Icons.menu),
                    onPressed: () => Navigator.pop(context),
                  ).align(),
                  padding: EdgeInsets.zero, // removes default padding
                  // This size lines up the two hamburger icons perfectly.
                ).size(width: 0, height: 110),
                ...listTiles,
              ],
            ),
          ),
        ),
      ),
    );
  }
}
```

Here is an example of using the helper class above
to add a `Drawer` to an app:

<img alt="Flutter Drawer" style="width: 40%"
    src="/blog/assets/flutter-drawer.png?v={{pkg.version}}"
    title="Flutter Drawer">

```dart
import 'package:flutter/material.dart';
import 'about_page.dart';
import 'drawer_scaffold.dart';
import 'home_page.dart';
import 'settings_page.dart';

void main() => runApp(const MyApp());

class MyApp extends StatelessWidget {
  static final drawerItems = <DrawerItem>[
    DrawerItem(title: 'About', icon: Icons.info, widget: AboutPage()),
    DrawerItem(title: 'Home', icon: Icons.home, widget: HomePage()),
    DrawerItem(title: 'Settings', icon: Icons.settings, widget: SettingsPage()),
  ];

  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Drawer Demo',
      theme: ThemeData(primarySwatch: Colors.blue),
      home: DrawerScaffold(drawerItems: drawerItems),
    );
  }
}
```

### PageView Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/PageView-class.html",
"PageView" %} widget provides a carousel-like scrollable list of pages.
Users can swipe through viewing the pages one at a time
or the current page can be changed programmatically.

Pages scroll horizontally by default, but can be changed to scroll vertically.

Page transitions are animated by default and the animation can be customized.

The following code demonstrates using `PageView`:

<img alt="Flutter PageView" style="width: 40%"
    src="/blog/assets/flutter-pageview.png?v={{pkg.version}}"
    title="Flutter PageView">

```dart
import 'package:flutter/material.dart';

void main() => runApp(const MyApp());

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'PageView Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: const Home(),
    );
  }
}

class Home extends StatefulWidget {
  const Home({Key? key}) : super(key: key);

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  final PageController _controller = PageController();
  var _pageIndex = 0;
  final _pages = <Widget>[Page1(), Page2(), Page3()];

  IconButton _buildButton(bool forward) {
    var hide = forward ? _pageIndex >= _pages.length - 1 : _pageIndex == 0;
    var icon = forward ? Icons.arrow_forward_ios : Icons.arrow_back_ios;
    var method = forward ? _controller.nextPage : _controller.previousPage;
    return IconButton(
      icon: Icon(icon),
      key: Key(forward ? 'forwardBtn' : 'backBtn'),
      onPressed: hide
          ? null
          : () {
              method(
                curve: Curves.easeInOut,
                duration: Duration(seconds: 1),
              );
              setState(() => _pageIndex += forward ? 1 : -1);
            },
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('PageView Demo'),
        leading: _buildButton(false),
        actions: [_buildButton(true)],
      ),
      body: SafeArea(
        child: Column(
          children: [
            Expanded(
              child: PageView(
                children: _pages,
                controller: _controller,
                onPageChanged: (index) {
                  setState(() => _pageIndex = index);
                },
                //scrollDirection: Axis.vertical,
              ),
            ),
            // See notes on using TabPageSelector instead below.
            SizedBox(
              child: Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  for (var index = 0; index < _pages.length; index++)
                    IconButton(
                      icon: Icon(
                        Icons.circle,
                        color:
                            index == _pageIndex ? Colors.black : Colors.black26,
                        size: 16,
                      ),
                      key: Key('dot${index + 1}'),
                      onPressed: () {
                        _controller.animateToPage(
                          index,
                          duration: Duration(seconds: 1),
                          curve: Curves.easeInOut,
                        );
                        setState(() => _pageIndex = index);
                      },
                    ),
                ],
              ),
              height: 30,
            ),
          ],
        ),
      ),
    );
  }
}

class Page1 extends StatefulWidget {
  const Page1({Key? key}) : super(key: key);

  @override
  State<Page1> createState() => _Page1State();
}

class _Page1State extends State<Page1> {
  var name = '';

  @override
  Widget build(BuildContext context) {
    return Container(
      child: Padding(
        padding: EdgeInsets.all(10),
        child: Column(children: [
          Text('This is page #1.'),
          TextFormField(
            decoration: InputDecoration(
              border: OutlineInputBorder(),
              filled: true,
              fillColor: Colors.white,
              labelText: 'Name',
            ),
            initialValue: name,
            key: Key('nameField'),
            onChanged: (String value) {
              setState(() => name = value);
            },
          ),
          Text(name.isEmpty ? '' : 'Hello, $name!'),
          ElevatedButton(
            child: Text('SHOUT'),
            key: Key('shoutBtn'),
            onPressed: () {
              setState(() => name = name.toUpperCase());
            },
          ),
        ]),
      ),
      color: Colors.pink[100],
      key: Key('page1'),
    );
  }
}

class Page2 extends StatelessWidget {
  const Page2({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Container(
      child: Center(
        child: Text('This is page #2.'),
      ),
      color: Colors.yellow[100],
      key: Key('page2'),
    );
  }
}

class Page3 extends StatelessWidget {
  const Page3({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Container(
      child: Center(
        child: Text('This is page #3.'),
      ),
      color: Colors.blue[100],
      key: Key('page3'),
    );
  }
}
```

The `TabPageSelector` widget can be used to render the dots
instead creating our own with the `IconButton` widget.
But the dots it creates cannot be tapped to change the page.
If all that is needed is to indicate the current page,
the following version of the `_HomeState` class above achieves this.

```dart
// The mixin is required in order to set the TabController vsync argument.
class _HomeState extends State<Home> with SingleTickerProviderStateMixin {
  final PageController _controller = PageController();
  final _pages = <Widget>[Page1(), Page2(), Page3()];
  late TabController _tabController;

  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: _pages.length, vsync: this);
  }

  IconButton _buildButton(bool forward) {
    var index = _tabController.index;
    var hide = forward ? index >= _pages.length - 1 : index == 0;
    var icon = forward ? Icons.arrow_forward_ios : Icons.arrow_back_ios;
    var method = forward ? _controller.nextPage : _controller.previousPage;
    return IconButton(
      icon: Icon(icon),
      onPressed: hide
          ? null
          : () {
              method(
                curve: Curves.easeInOut,
                duration: Duration(seconds: 1),
              );
              setPageIndex(_tabController.index + (forward ? 1 : -1));
            },
    );
  }

  void setPageIndex(int pageIndex) {
    setState(() {
      _tabController.index = pageIndex;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('PageView Demo'),
        leading: _buildButton(false),
        actions: [_buildButton(true)],
      ),
      body: SafeArea(
        child: Column(
          children: [
            Expanded(
              child: PageView(
                children: _pages,
                controller: _controller,
                onPageChanged: setPageIndex,
                //scrollDirection: Axis.vertical,
              ),
            ),
            TabPageSelector(
              color: Colors.pink, // optional
              controller: _tabController,
              indicatorSize: 20, // optional
              selectedColor: Colors.purple, // optional
            ),
          ],
        ),
      ),
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

Another way to get fonts is use the {% aTargetBlank
"https://pub.dev/packages/google_fonts", "google_fonts" %} package.
This provides over 1000 fonts.
Instead of downloading each of the fonts, they are
fetched via HTTP at runtime and cached in the app's file system.

The steps to use this are:

1. Add `google_fonts` as a dependency in `pubspec.yaml`.
1. Add the following import in all `.dart` files
   that wish to use the fonts.

   ```dart
   import 'package:google_fonts/google_fonts.dart';
   ```

1. Use the `GoogleFonts` class to select fonts.

   ```dart
   Text(
     'This is Google Fonts',
     style: GoogleFonts.getFont(
       'Dancing Script',
       textStyle: TextStyle(
         color: Colors.purple,
         fontSize: 30,
         fontStyle: FontStyle.italic,
         fontWeight: FontWeight.bold,
       ),
     ),
   ),
   Text(
    'This is Google Fonts',
    // alternative to getFont using method names that are font names
    style: GoogleFonts.dancingScript(),
   ),
   ```

Both the `getFont` method and specific font methods accept a
`textStyle` argument for specifying their color, font size, and more.

If matching font files are found in the
assets directory specified in `pubspec.yaml`,
those will be used in instead of downloading new font files.

Create the directory `asssets/fonts` and add the following in `pubspec.yaml`:

```yaml
assets:
  - assets/fonts/
```

To download font files ahead of time, browse {% aTargetBlank
"https:/fonts.google.com", "fonts.google.com" %}.
For example, download the "Dancing Script" font
and move all the `.ttf` files in the downloaded `static` directory
to a project `assets/fonts` directory.

In order for runtime font fetching to work in macOS,
a `.entitlements` file much contain the following:

```xml
<key>com.apple.security.network.client</key>
<true/>
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

## Colors

The `Colors` class defines a large number of colors as static constants.
For example, `Colors.pink.shade700` is a specific shade of pink.

To see all the colors defined by the `Colors` class,
see the documentation for the {% aTargetBlank
"https://api.flutter.dev/flutter/material/Colors-class.html", "Colors class" %}.
Note that `grey` is one of the defined colors, but `gray` is not.

To get a color from the current theme, call the `Theme.of(context)` method.
For example, to get the error color, use `Theme.of(context).errorColor`.

## Themes

Flutter uses theme objects to describe styling that should be applied
to various kinds of widgets throughout an app.
This begins with the `ThemeData` object that is
specified in the `MaterialApp` `theme` (used for light mode)
and `darkTheme` arguments.
If no theme is specified, Flutter provides a default theme.

Most Material and Cupertino widgets are theme-aware.
This means their styling is affected by the theme specified in the app widget.

For example, it is possible to specify the background color
to be used for all `ElevatedButtonWidgets` in an app as follows:

```dart
    return MaterialApp(
      ...
      theme: ThemeData(
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ButtonStyle(
            backgroundColor: MaterialStateProperty.all<Color>(
              Colors.red,
            ),
          ),
        ),
        primarySwatch: Colors.blue,
      ),
    );
```

The `ButtonStyle` `background` argument
must be set to a `MaterialStateProperty` object.
From the documentation for the {% aTargetBlank
"https://api.flutter.dev/flutter/material/ButtonStyle-class.html",
"ButtonStyle" %} class,
"Many of the `ButtonStyle` properties are `MaterialStateProperty` objects
which resolve to different values depending on the button's state.
For example the `Color` properties are defined with
`MaterialStateProperty<Color>` and can resolve to different colors
depending on if the button is pressed, hovered, focused, disabled, etc."

To use a theme property for a different purpose,
obtain the value using `Theme.of(context).propertyName`.
This uses the `BuildContext` object passed to `Theme.of`
to traverse up the widget tree to find a `Theme` widget.
It then returns its `ThemeData` object.
The traversal only happens once per widget instance
and a reference to the `ThemeData` object is then stored in the widget.

The following code finds the theme `disabledColor` value
and uses it in a `Text` widget:

```dart
// This defaults to light gray.
// "context" here is a BuildContext object
var disabledColor = Theme.of(context).disabledColor;
...
Text('some dimmed text', style: TextStyle(color: disabledColor)),
```

## Icons

The `Icon` widget renders an icon from a large provided set of icons.
For example, the following renders a music note icon.

```dart
Icon(Icons.audiotrack, color: Colors.red, size: 30)
```

To see all the icons defined by the `Icons` class,
see the documentation for the {% aTargetBlank
"https://api.flutter.dev/flutter/material/Icons-class.html", "Icons class" %}.

Cupertino icons are provided by the "Cupertino Icons" package
which is documented at {% aTargetBlank
"https://pub.dev/packages/cupertino_icons", "cupertino_icons" %}.

## Images

The `Image` widget renders an image from a URL or local file.
For example, the following renders a photo of an owl.

```dart
Image.network(
  'https://flutter.github.io/assets-for-api-docs/assets/widgets/owl.jpg'
)
```

To render an image from a local file,

- Create an `assets` directory at the top of the project directory.
  This can have subdirectories for different kinds of assets
  such as audio, fonts, icons, images, and video.
- Create an `images` directory inside the `assets` directory.
- Copy image files into the `assets/images` directory.
- Edit `pubspec.yaml` and add the following:

  ```yaml
  assets:
    - assets/
  ```

- Create an `Image` widget to render the image.
  For example, `Image.asset('assets/images/comet.jpg')`

To clip an image to an ellipse, use the `ClipOval` widget.
Clearly the person that named this widget doesn't know geometry.
The shape will be a circle if the image width and height are equal.
For example:

```dart
ClipOval(child: Image.asset('assets/images/comet.jpg'))
```

To blend a color with an image, specify the color with the `color` argument
and add the `colorBlendMode` argument to specify how it should be blended.
The `colorBlendMode` value should be one of the constants defined in the
{% aTargetBlank "https://api.flutter.dev/flutter/dart-ui/BlendMode.html",
"BlendMode" %} class.

The `Image` widget does not support SVG files.
To render SVGs, add a dependency on the pub.dev package flutter_svg.
To use this, create an `assets` directory and
register it in `pubspec.yaml` as shown above.
Then write code like the following:

```dart
import 'package:flutter_svg/flutter_svg.dart';
...
SvgPicture.asset('assets/images/some_filename.svg'),
```

## Zooming and Panning

Zooming and panning is especially useful with images,
but can also be applied to other widgets.

The built-in way to implement this is to use `InteractiveViewer`.
This is suitable for most use cases.

The pub.dev package {% aTargetBlank "https://pub.dev/packages/photo_view",
"photo_view" %} provides additional features such as
rotating and photo galleries.

To zoom in and out in the iOS Simulator using a trackpad,
hold down the option key and use two fingers on the trackpad.
To pan, just drag.

To zoom in and out in an Android emulator on macOS using a trackpad,
hold down the command key and press down and drag two fingers on the trackpad.
To pan, just drag

The following code demonstates applying `InteractiveViewer` to an image:

```dart
InteractiveViewer(child: Image.asset('assets/images/some-photo.jpg'))
```

The following code demonstates using the `photo_view` package:

```dart
import 'package:photo_view/photo_view.dart';
...
PhotoView(imageProvider: AssetImage('assets/images/some-photo.jpg'))
```

## Drawing

To draw on a widget, use the `CustomPaint` widget.
This is similar to using the HTML `canvas` and `svg` elements.

There are two steps required.
First, define a class that extends the `CustomPainter` class.
This includes a `paint` method that takes a `Canvas` and draws on it.

```dart
class _MyPainter extends CustomPainter {
  _MyPainter() : super();

  @override
  void paint(Canvas canvas, Size size) {
    final path = Path();
    const inset = 10.0;
    const minX = inset;
    final maxX = size.width - inset;
    const minY = inset;
    final maxY = size.height - inset;
    // Create a List of points for a triangle.
    var points = <Offset>[
        Offset((minX + maxX) / 2, minY),
        Offset(maxX, maxY),
        Offset(minX, maxY),
    ];
    path.addPolygon(points, true); // true to close
    path.close();

    final paint = Paint()
      ..color = Colors.red
      ..style = PaintingStyle.fill // or .stroke
      ..strokeWidth = 5;
    // Convert the path to pixels.
    canvas.drawPath(path, paint);
  }

  // Returning false optimizes for drawings
  // that do not change based on state.
  @override
  bool shouldRepaint(CustomPainter oldDelegate) => false;
}
```

The `Path` class supports a large number of methods
for describing what should be drawn. These include:
`addArc`, `addOval`, `addPath`, `addPolygon`, `addRect`, `addRRect`, `arcTo`,
`arcToPoint`, `conicTo`, `cubicTo`, `lineTo`, `moveTo`, `quadraticBezierTo`,
`relativeArcToPoint`, `relativeConicTo`, `relativeLineTo`, `relativeMoveTo`,
and `relativeQuadraticBezierTo`.
Also see the static Path method combine.

Second, render the drawing described by the class above
on another widget using the `CustomPaint` widget.
Often the targeted widget is an empty `SizedBox`,
but it can be any widget and the drawing
appears on top of what that widget renders.

```dart
return CustomPaint(
  painter: _MyPainter(),
  child: SizedBox(width: 200, height: 100),
);
```

## Charts

The pub.dev package {% aTargetBlank "https://pub.dev/packages/fl_chart",
"fl_chart" %} draws line, bar, pie, scatter, and radar charts.
The charts are quite fancy and even support animations.

The following code demonstrates creating a simple bar chart.
Other kinds are charts are created in a similar way.

<img alt="fl_chart bar chart" style="width: 50%"
    src="/blog/assets/flutter-fl_chart.png?v={{pkg.version}}"
    title="fl_chart bar chart">

```dart
import 'package:collection/collection.dart'; // for mapIndexed method
import 'package:fl_chart/fl_chart.dart';
import 'package:flutter/material.dart';

import './extensions/widget_extensions.dart';

const title = 'My App';

void main() => runApp(
      MaterialApp(
        title: title,
        theme: ThemeData(
          primarySwatch: Colors.blue,
        ),
        home: const Home(),
      ),
    );

class Home extends StatefulWidget {
  const Home({Key? key}) : super(key: key);

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  final xTitles = ['Mark', 'Tami', 'Amanda', 'Jeremy'];
  final yValues = <double>[6, 7, 10, 8];

  @override
  Widget build(BuildContext context) {
    final barGroups = yValues
        .mapIndexed(
          (index, y) => getBar(x: index, y: y),
        )
        .toList();
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Column(
        children: [
          BarChart(
            BarChartData(
              axisTitleData: FlAxisTitleData(
                bottomTitle: getAxisTitle('Person', 30),
                leftTitle: getAxisTitle('Score'),
              ),
              borderData: FlBorderData(
                border: const Border(
                  top: BorderSide.none,
                  right: BorderSide.none,
                  left: BorderSide(width: 1), // y-axis
                  bottom: BorderSide(width: 1), // x-axis
                ),
              ),
              barGroups: barGroups,
              // This hides the faint, dashed grid lines.
              gridData: FlGridData(show: false),
              titlesData: FlTitlesData(
                bottomTitles: SideTitles(
                  showTitles: true,
                  getTitles: (index) => xTitles[index.toInt()],
                  rotateAngle: -45,
                ),
                leftTitles: SideTitles(
                  showTitles: true,
                  // Only show titles with no decimal places.
                  getTitles: (value) =>
                      value % 1 == 0 ? '${value.toInt()}' : '',
                ),
                topTitles: SideTitles(showTitles: false),
                rightTitles: SideTitles(showTitles: false),
              ),
            ),
          ).padding(20).expanded,
        ],
      ),
    );
  }

  BarChartGroupData getBar({required int x, required double y}) {
    const width = 30.0;
    return BarChartGroupData(x: x, barRods: [
      BarChartRodData(
        //borderRadius: BorderRadius.zero,
        borderRadius: BorderRadius.vertical(
          top: Radius.circular(width / 2),
        ),
        colors: [Colors.blue],
        width: width,
        y: y,
      ),
    ]);
  }

  AxisTitle getAxisTitle(String title, [double margin = 0.0]) {
    return AxisTitle(
      margin: margin,
      showTitle: true,
      textStyle: TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
      titleText: title,
    );
  }
}
```

## Animation

Flutter animations provide a way to rebuild parts of the widget tree
on every frame with a goal of rendering at least 60 frames per second (fps).
Typically Flutter widget building is fast enough to support this.

For official documentation on Flutter animations, see {% aTargetBlank
"https://docs.flutter.dev/development/ui/animations",
"Introduction to animations" %}.
This page contains six videos on the topic
and a flow chart for deciding which kind of animation to use.

Flutter supports two kinds of animations, implicit and explict.
Implicit animations are generally easier to use, but provide less control.
Explicit animations are generally more difficult to use,
but provide more control.

Use an implicit animation unless any of the following are true,
in which case an explicit animation must be used.

- The animation should repeat while some condition is true,
  including just being on a certain screen.
- The animation is discontinuous, such as a circle whose
  radius increases and then jumps back to the beginning radius
  rather than reversing direction.

There are implicit animation versions of many non-animated widgets.
These are listed in the table below.

| Property to Animate     | Implicit Animation                                                                                                                                                                                                                                          | Explicit Animation                                                                                                                                                                                                                                                                                                                                                         |
| ----------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `alignment`             | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedAlign-class.html", "AnimatedAlign" %}                                                                                                                                                      | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AlignTransition-class.html", "AlignTransition" %}                                                                                                                                                                                                                                                                 |
| `decoration`            |                                                                                                                                                                                                                                                             | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/DecoratedBoxTransition-class.html", "DecoratedBoxTransition" %}                                                                                                                                                                                                                                                   |
| `List` items            | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedList-class.html", "AnimatedList" %} and {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedListState-class.html", "AnimatedListState" %}                                     |                                                                                                                                                                                                                                                                                                                                                                            |
| `Listenable` value      | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedWidget-class.html", "AnimatedWidget" %}                                                                                                                                                    |                                                                                                                                                                                                                                                                                                                                                                            |
| multiple properties     | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedContainer-class.html", "AnimatedContainer" %}                                                                                                                                              | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/TweenAnimationBuilder-class.html", "TweenAnimationBuilder" %}                                                                                                                                                                                                                                                     |
| `offset`                | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedSlide-class.html", "AnimatedSlide" %}                                                                                                                                                      |                                                                                                                                                                                                                                                                                                                                                                            |
| `opacity`               | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedOpacity-class.html", "AnimatedOpacity" %}                                                                                                                                                  | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/FadeTransition-class.html", "FadeTransition" %}                                                                                                                                                                                                                                                                   |
| `padding`               | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedPadding-class.html", "AnimatedPadding" %}                                                                                                                                                  |                                                                                                                                                                                                                                                                                                                                                                            |
| `position`              | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedPositioned-class.html", "AnimatedPositioned" %} and {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedPositionedDirectional-class.html", "AnimatedPositionedDirectional" %} | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/PositionedTransition-class.html", "PositionedTransition" %}, {% aTargetBlank "https://api.flutter.dev/flutter/widgets/RelativePositionedTransition-class.html", "RelativePositionedTransition" %}, and {% aTargetBlank "https://api.flutter.dev/flutter/widgets/SlideTransition-class.html", "SlideTransition" %} |
| `rotation`              | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedRotation-class.html", "AnimatedRotation" %}                                                                                                                                                | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/RotationTransition-class.html", "RotationTransition" %}                                                                                                                                                                                                                                                           |
| `scale`                 | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedScale-class.html", "AnimatedScale" %}                                                                                                                                                      | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/ScaleTransition-class.html", "ScaleTransition" %}                                                                                                                                                                                                                                                                 |
| `shadow`                | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedPhysicalModel-class.html", "AnimatedPhysicalModel" %}                                                                                                                                      |                                                                                                                                                                                                                                                                                                                                                                            |
| `size`                  | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedSize-class.html", "AnimatedSize" %}                                                                                                                                                        | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/SizeTransition-class.html", "SizeTransition" %}                                                                                                                                                                                                                                                                   |
| text style              | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedDefaultTextStyle-class.html", "AnimatedDefaultTextStyle" %}                                                                                                                                | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/DefaultTextStyleTransition-class.html", "DefaultTextStyleTransition" %}                                                                                                                                                                                                                                           |
| widget pair showing one | {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedSwitcher-class.html", "AnimatedSwitcher" %} and {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedCrossFade-class.html", "AnimatedCrossFade" %}                             |                                                                                                                                                                                                                                                                                                                                                                            |

In order of difficulty from easiest to most difficult,
the options for implementing animattions are

1. Built-in implicit animations: `Animated{property}`
1. Custom implicit animations": `TweenAnimationBuilder`
1. Built-in explicit animations: `{property}Transition`
1. Custom explicit animations: `AnimatedWidget`
1. Custom drawing animations: `CustomPainter`

### Implicit Animation

Built-in implicit animations have names of the form `Animated{property}`.
They animate changes to a specific property.
They are used inside a `StatefulWidget`
(or use `StreamBuilder` or `FutureBuilder`)
to allow a state variable that provides a property value to be changed.

Built-in implicit animations all extend `ImplicitlyAnimatedWidget`.

Most implicit animations constructors take the arguments `duration` and `curve`.
The `duration` argument takes a `Duration` object that specifies
the amount of time over which the animation should take place.
The `curve` argument takes a `Curve` object
that specifies an easing curve to use.`
This controls the rate of change in an animation over its duration.

See the {% aTargetBlank
"https://api.flutter.dev/flutter/animation/Curves-class.html", "Curves" %}
class for a graphical represenation of each of the built-in easing curves.
In the page linked above, each curve has a play button.
Clicking it animates movement along the curve and demonstrates
how it would affect translation, rotation, scale, and opacity.
Some easing curves bounce, meaning that the direction of the animation
reverses at some point and then goes forward again, possibly multiple times.

The `AnimatedModalBarrier` widget is an animated version
of the `ModalBarrier` widget.
Both prevent interaction with widgets that are behind it.
They are typically used to implement modal dialogs.

The `AnimatedPhysicalModel` widget is an animated version
of the `PhysicalModel` widget.
Both add a shadow to a shape based on `elevation` and other arguments.

The `AnimatedContainer` widget provides
implicit animation of multiple properties.

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/AnimatedBuilder/AnimatedBuilder.html",
"AnimationBuilder" %} and {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/TweenAnimationBuilder-class.html",
"TweenAnimationBuilder" %} classes
are used to create custom implicit animations.
For an example, see the "Animation From Scratch" section below.

### Explicit Animation

Built-in explicit animations have names of the form “{property}Transition”.
For custom "standalone" explicit animations,
consider creating a class that extends `AnimatedWidget`.
Otherwise create custom explicit animations using `AnimatedBuilder`.
If these approaches have performance issues, consider using `CustomPainter`.

Built-in explicit animations all extend `AnimatedWidget`.
This automatically rebuilds when a given `Listenable` changes.

Explict animations require the use of an {% aTargetBlank
"https://api.flutter.dev/flutter/animation/AnimationController-class.html",
"AnimationController" %} that is maintained inside a `StatefulWidget`.
This produces values that are typically in the range 0 to 1.
They also require a {% aTargetBlank
"https://api.flutter.dev/flutter/scheduler/TickerProvider-class.html",
"TickerProvider" %} to provide a `Ticker`.
A {% aTargetBlank "https://api.flutter.dev/flutter/scheduler/Ticker-class.html",
"Ticker" %} calls a callback function
once for each animation frame that is rendered.
Often the `Ticker` is provided by the mixin {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/SingleTickerProviderStateMixin-mixin.html",
"SingleTickerProviderStateMixin" %}.

An `AnimationController` typically obtains
60 values per second from the `Ticker` and
passes each value to all registered listener functions.
It can:

- change the lower and upper bounds to something other than 0 and 1
- set an animation to a specific value (starts at lower bound)
- play an animation forward (current to upper) or backward (current to lower)

To produce values in another range or with a type other than `double`,
use the `Tween` class or its subclasses such as `ColorTween`.
To use a sequence of values from multiple `Tween` objects,
use the `TweenSequence` class.

Create an instance of `AnimationController` in the `initState` method
and dispose of it in the `dispose` method.
This avoids creating a new instance of the controller
each time the `build` method is invoked.

The `AnimationController` constructor takes the arguments
`duration` (represents seconds per repeat) and
`vsync` (typically set to `this`).

The most commonly used `AnimationController` methods are described below:

| Method                                       | Description                                                  |
| -------------------------------------------- | ------------------------------------------------------------ |
| `addListener(VoidCallback)`                  | listens for value changes; sometimes used to call `setState` |
| `addStatusListener(AnimationStatusCallback)` | listens for animation status changes                         |
| `animateTo(double target)`                   | animates value from current to target                        |
| `dispose()`                                  | frees resources                                              |
| `forward()`                                  | animates value from `lowerBound` to `upperBound`             |
| `repeat()`                                   | same as `forward`, but repeats when end is reached           |
| `reset()`                                    | stops animation and sets value to `lowerBound`               |
| `reverse()`                                  | animates value from `upperBound` to `lowerBound`             |
| `stop()`                                     | stops animation                                              |

The listener function passed to `addStatusListener`
is passed an {% aTargetBlank
"https://api.flutter.dev/flutter/animation/AnimationStatus.html",
"AnimationStatus" %} constant.
When the `forward` method is called,
the status begins as `forward` and ends as `completed` (at end).
When the `reverse` method is called,
the status begins as `reverse` and ends as `dismissed` (at beginning).

Classes that use an `AnimationController` typically
have a constructor argument of type `Animation` (like `FadeTransition`)
or have an `animate` method (like `Tween`).

Multiple animations can be controlled by the same controller instance.

Explicit animations can be composed
to perform multiple, simultaneous animations.

`AnimatedWidget` and `AnimatedBuilder` are used together
to create a custom explicit animation.
`AnimatedBuilder` takes a `child` argument which is used to
avoid recreating the widget that is being animated on each animation change.
This is a performance optimization.
Note that this is not necessary if the widget being animated
has a `const` constructor because that allows
widget instances to be created at compile time.

### AnimatedSwitcher Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/AnimatedSwitcher-class.html",
"AnimatedSwitcher" %} widget renders a given widget
and then animates displaying a different widget
when the value of its `child` argument changes.
By default it uses a `FadeTransition`.
This can be changed by specifying the `transitionBuilder` argument.
It is important for each of the widgets to have a different `key` value.

The following code demonstrates using `AnimatedSwitcher`
to cycle through a set of images that are stored
in the `assets/images` directory of the application.

```dart
import 'package:flutter/material.dart';

class ImageCycle extends StatefulWidget {
  final List<String> fileNames;

  const ImageCycle({required this.fileNames, Key? key}) : super(key: key);

  @override
  _ImageCycleState createState() => _ImageCycleState();
}

class _ImageCycleState extends State<ImageCycle> {
  int index = 0;
  late List<Widget> images;

  @override
  void initState() {
    const imageSize = 200.0;
    images = widget.fileNames
        .map(
          (fileName) => SizedBox(
            child: Image.asset('assets/images/$fileName'),
            height: imageSize,
            key: ValueKey(fileName), // must have this!
            width: imageSize,
          ),
        )
        .toList();
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Column(children: [
      AnimatedSwitcher(
        child: images[index],
        duration: Duration(seconds: 1),
        /*
        transitionBuilder: (child, animation) =>
            //FadeTransition(child: child, opacity: animation), // default
            ScaleTransition(child: child, scale: animation),
        */
      ),
      ElevatedButton(
        child: Text('Next'),
        onPressed: () {
          setState(() {
            index = (index + 1) % widget.fileNames.length;
          });
        },
      )
    ]);
  }
}
```

### Hero Widget

The {% aTargetBlank
"https://docs.flutter.dev/development/ui/animations/hero-animations",
"Hero" %} widget animates a widget between two routes or pages.
For example, suppose an app has a page that renders a `ListView`.
This contains a `ListTile` widget for each item.
Each of these displays a thumbnail image and a description.

<img alt="Flutter Hero List View" style="width: 50%"
    src="/blog/assets/flutter-hero-1.png?v={{pkg.version}}"
    title="Flutter Hero List View">

Tapping a `ListTile` navigates to the detail page that displays
a larger version of the image and detailed information about the item.

<img alt="Flutter Hero Detail View" style="width: 50%"
    src="/blog/assets/flutter-hero-2.png?v={{pkg.version}}"
    title="Flutter Hero Detail View">

The image on each page can be wrapped in a `Hero` widget
which takes `tag` and `child` arguments.
The `tag` values can be any kind of object that uniquely identifies the image
such as a `String` or `ObjectKey`.
The `child` values can be `Image.asset` widgets
or any widget type that the two pages have in common.
The only requirement is for the `tag` value to be the
same in both pages for corresponding `child` values.

Here is the `Hero` widget from the list view page
that is inside a `ListTile` widget:

```dart
Hero(
  tag: ObjectKey(dog);
  child: SizedBox(
    child: Image.asset('assets/images/${dog.photoFileName}'),
    height: thumbnailSize,
    width: thumbnailSize,
  ),
)
```

The `Hero` widget in the detail page is the same as above,
but uses a larger values for `height` and `width`.

When the user navigates from one page to the other,
the `Hero` `child` in the current page will
animate to the `Hero` `child` in the new page.
In our example this works when navigating
from the list view to the detail view,
and also when navigating back to the list view.

The `Hero` widget provides a nice effect that
keeps the attention of the user focused on the `Hero` content.

### Custom Implicit Animation

As an example of a custom explicit animation,
we can define a `FadeIn` widget that takes three arguments.
The `child` argument takes the `Widget` to fade into view.
The optional `duration` argument takes the `Duration` over which to fade in
and defaults to one second.
The optional `onComplete` argument takes a `VoidCallback`
to call when the animation completes.

The `FadeIn` widget can be used as follows:

```dart
FadeIn(
  child: Text(
    'This text will fade in.',
    style: TextStyle(fontSize: 30),
  ),
  duration: Duration(seconds: 2),
),
```

This can be implemented by a `StatefulWidget`
that renders an `AnimatedOpacity` widget as follows:

```dart
class FadeIn extends StatelessWidget {
  static const defaultDuration = Duration(seconds: 1);

  final Widget child;
  final Duration duration;
  final VoidCallback? onComplete;

  FadeIn({
    required this.child,
    this.duration = defaultDuration,
    this.onComplete,
    Key? key,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return TweenAnimationBuilder(
      child: child,
      //curve: Curves.easeInOut,
      duration: duration,
      // This tweens between two double values.
      // It is also possible to tween between
      // non-numeric values such as colors.
      tween: Tween(begin: 0.0, end: 1.0),
      builder: (_, double value, Widget? child) =>
          Opacity(child: child, opacity: value),
      onEnd: onComplete,
    );
  }
}
```

### Custom Explicit Animation

As an example of a custom explicit animation,
we can define the same thing as in the previous section.
This time we will use the mixin `SingleTickerProviderStateMixin`,
a `FadeTransition` explicit animation,
an `AnimationController`, and the `Tween` class.
Clearly this approach requires more code,
but it is interesting to see how lower-level classes
can be used to achieve the same result.

The implementation of `FadeIn` follows:

```dart
import 'package:flutter/material.dart';

class FadeIn extends StatefulWidget {
  static const defaultDuration = Duration(seconds: 1);

  final Widget child;
  final Duration duration;
  final VoidCallback? onComplete;

  FadeIn({
    required this.child,
    this.duration = defaultDuration,
    this.onComplete,
    Key? key,
  }) : super(key: key);

  @override
  State<FadeIn> createState() => _FadeInState();
}

class _FadeInState extends State<FadeIn> with SingleTickerProviderStateMixin {
  late Animation<Color?> _colorAnimation;
  late Animation<double> _opacityAnimation;
  late AnimationController _controller;

  @override
  void initState() {
    _controller = AnimationController(
      duration: widget.duration,
      vsync: this, // a TickerProvider
    );

    // To see all the values the controller takes on ...
    //_controller.addListener(() => print(_controller.value));

    _opacityAnimation = Tween(begin: 0.0, end: 1.0).animate(_controller);

    _colorAnimation =
      ColorTween(begin: Colors.blue, end: Colors.red).animate(_controller);

    TickerFuture future = _controller.forward(); // start the animation
    if (widget.onComplete != null) future.whenComplete(widget.onComplete!);
  }

  @override
  dispose() {
    _controller.dispose();
    // Animations do not have a dispose method.
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    /*
    // To only animate opacity ...
    return FadeTransition(
      opacity: _opacityAnimation,
      child: widget.child,
    );
    */

    // To animate both opacity and color ...
    return AnimatedBuilder(
      animation: _controller,
      builder: (BuildContext context, _) {
        var color = _colorAnimation.value;
        var opacity = _opacityAnimation.value;
        return DefaultTextStyle(
          child: widget.child,
          style: TextStyle(color: color!.withOpacity(opacity)),
        );
      },
    );
  }
}
```

### Animation From Scratch

The code in this section demonstrates how
most Flutter animations work under the covers.

First, let's see how we can implement animatating a number from 0 to 100
over a duration of 5 seconds in the easiest way possible.

```dart
import 'package:flutter/material.dart';

class HighLevelAnimation extends StatelessWidget {
  const HighLevelAnimation({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return TweenAnimationBuilder(
      tween: IntTween(begin: 0, end: 100),
      duration: const Duration(seconds: 5),
      builder: (BuildContext context, int number, Widget? child) {
        return Text('$number');
      },
    );
  }
}
```

Now let's see how this can be implemented using lower level Flutter classes.
Typically it isn't necessary to work at this level,
but it is interesting to get a taste of how
higher level animations are actually implemented.

Objects from the `Ticker` class take a function
and invoke it once per animation frame
after their `start` method is called.
Their `dispose` method should be called when they are no longer needed.

To simplify managing a `Ticker`, apply the `SingleTTickerProviderStateMixin`
mixin to the `State` of a `StatefulWidget`.

An `AnimationController` object can get a `Ticker` instance from such a class
by setting its `vsync` constructor argument to `this`.
Call the `addListener` method of the controller
to register a funtion to be called once per animation frame.
This can update the state of the widget
which can change what the widget renders.
Call the `forward` method of the controller to start the animation.

```dart
import 'package:flutter/material.dart';

class LowLevelAnimation extends StatefulWidget {
  const LowLevelAnimation({Key? key}) : super(key: key);

  @override
  _LowLevelAnimationState createState() => _LowLevelAnimationState();
}

class _LowLevelAnimationState extends State<LowLevelAnimation>
    with SingleTickerProviderStateMixin<LowLevelAnimation> {
  late AnimationController _controller;
  int number = 0;

  @override
  void initState() {
    super.initState();
    _controller = AnimationController(
      duration: Duration(seconds: 5),
      vsync: this,
    );
    _controller.addListener(_update);
    _controller.forward(); // starts animation
  }

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  void _update() {
    setState(() {
      // _controller.value is a number between 0 and 1.
      number = (_controller.value * 100).round();
    });
  }

  @override
  Widget build(BuildContext context) {
    return Text('$number');
  }
}
```

### Animated Drawings

For animations of drawings rather than widgets,
consider using the packages
{% aTargetBlank "https://pub.dev/packages/rive", "rive" %} and
{% aTargetBlank "https://pub.dev/packages/lottie", "lottie" %} in pub.dev.

### More on Animation

For more on animation, see this {% aTargetBlank
"https://github.com/mvolkmann/flutter_animation", "GitHub repo" %}
which contains a Flutter project that uses several animations.
These include:

- `Hero` for animating thumbnail images on a list page
  to larger images on detail pages.
- `SingleTickerProviderStateMixin`, `AnimationController`, `TweenSequence`,
  and `AnimatedBuilder` to animate the size of an icon up and back down.
- `SingleTickerProviderStateMixin`, `AnimationController`, `Tween`, and
  `ColorTween` to animate the opacity and color of a `Text` widget.
- `AnimatedSwitcher` and `ScaleTransition`
  to cycle through a set of `Image` widgets.
- `AnimatedList`, `SlideTransition`,
  `WidgetsBinding.instance?.addPostFrameCallback`, and `Future.delayed`
  to slide `ListTile` widgets in from the right in staggered fashion.

## Browsing URLs

The pub.dev package {% aTargetBlank "https://pub.dev/packages/url_launcher",
"url_launcher" %} supports launching another app to process a given URL.
This includes browsing a web page (`https://somesite.com`),
dialing the phone (`tel://+1 123 456 7890`),
sending an email (`mailto:somebody@somewhere.com`), and
sending a text message (`sms:+1 123 456 7890`).

For a short introduction, see this {% aTargetBlank
"https://www.youtube.com/watch?v=qYxRYB1oszw", "YouTube video" %}.

The ability to launch other apps for browsing URLs
must be specifically configured for Android and iOS.
See the {% aTargetBlank "https://pub.dev/packages/url_launcher",
"url_launcher" %} web page for details.
After modifying the configuration files, restart the app
in order for the changes to take effect.

The following code opens the default browser to browse the Flutter web site
when an `IconButton` is pressed:

```dart
import 'package:url_launcher/url_launcher.dart';
...
final url = 'https://flutter.dev';
...
IconButton(
  icon: Icon(Icons.open_in_browser),
  onPressed: () async {
    if (await canLaunch(url)) launch(url);
  },
),
```

## Audio

The {% aTargetBlank "https://pub.dev/packages/audioplayers", "audioplayers" %}
plugin is a popular option for playing audio files in Flutter applications.
It is fairly low-level and has some quirks.
For a working example that provides a simplified layer over this plugin
and a widget that provides a play/pause button, a stop button,
and a progress bar, see this {% aTargetBlank
"https://github.com/mvolkmann/flutter_audio", "GitHub repo" %}.

## Camera and Photo Plugins

There are several pub.dev plugins that support device camera access.

Android emulators provide a simulated camera
where the user can pan around in a room that contains a television,
book shelves, a cat, a dog, and a kitchen.

The iOS Simulator does not support a camera,
so apps that require camera access must be run on iOS devices.

### camera Plugin

The {% aTargetBlank "https://pub.dev/packages/camera", "camera" %} plugin
in pub.dev provides access to device cameras in Android, iOS, and the web.
It does not provide access to device photo libraries.
See this excellent {% aTargetBlank
"https://docs.flutter.dev/cookbook/plugins/picture-using-camera",
"cookbook page" %} for step-by-step instructions on using the "camera" plugin,
including example code.

While the "camera" plugin is popular, it is not nearly as easy to use or
as full-featured as the {% aTargetBlank "https://pub.dev/packages/image_picker",
"image_picker" %} plugin which is described in the next section.

To use the "camara" plugin:

1. Add the following dependencies in `pubspec.yaml`:`

   `camera`, `path`, and `path_provider`

1. For Android, edit `android/app/build.gradle` and
   change the value of `android / defaultConfig / minSdkVersion`
   from `flutter.minSdkVersion` to `23`.

1. For iOS, add the following lines in `ios/Runner/Info.plist`
   inside the `<dict>` element:

   ```xml
   <key>NSCameraUsageDescription</key>
   <string>Explanation on why the camera access is needed.</string>
   ```

1. Get access to a camera with the following code.
   There can be multiple cameras such as front and rear facing.

   ```dart
   WidgetsFlutterBinding.ensureInitialized();
   final cameras = await availableCameras();
   final firstCamera = cameras.first;
   ```

1. Create and initialize a `CameraController`.
1. Use a `CameraPreview` to display what the camera is seeing.
1. Use `CameraController` to capture a photo.
1. Use an `Image` widget to display the photo.

For an example Flutter app that uses the "camera" plugin,
see the "main" branch of this {% aTargetBlank
"https://github.com/mvolkmann/flutter_camera", "GitHub repo" %}.

### image_picker Plugin

The {% aTargetBlank "https://pub.dev/packages/image_picker",
"image_picker" %} plugin in pub.dev provides
access to device cameras and photo libraries in Android and iOS.

To use this plugin:

1. For iOS, add the following lines in `ios/Runner/Info.plist`
   inside the `<dict>` element with app-specific descriptions:

   ```xml
    <key>NSCameraUsageDescription</key>
    <string>Camera access is needed to demonstrate the "image_picker" plugin.</string>
    <key>NSMicrophoneUsageDescription</key>
    <string>Microphone access is needed to demonstrate the "image_picker" plugin.</string>
    <key>NSPhotoLibraryUsageDescription</key>
    <string>Photo library access is needed to demonstrate the "image_picker" plugin.</string>
   ```

1. Add the following imports:

   ```dart
   import 'dart:io' show File;
   import 'package:image_picker/image_picker.dart';
   ```

1. Create an `ImagePicker` instance:

   ```dart
   final _picker = ImagePicker();
   ```

1. To take a photo with the camera:

   ```dart
   XFile? image = await _picker.pickImage(source: ImageSource.camera);
   ```

1. To select an image from the photo library:

   ```dart
   XFile? image = await _picker.pickImage(source: ImageSource.gallery);
   ```

1. Save the selected `XFile` value in the state of a `StatefulWidget`.

1. The `XFile` object has `name` and `path` properties.
   To render an image using the selected `XFile` value:

   ```dart
   Image(image: FileImage(File(_selectedXFile!.path))),
   ```

1. Restart the app to install the `pickImage` plugin.
   Unless this is done, a MissingPluginException will occur.

For an example Flutter app that uses the "image_picker" plugin,
see the "image_picker" branch of this {% aTargetBlank
"https://github.com/mvolkmann/flutter_camera/tree/image_picker",
"GitHub repo" %}.

## GeoLocation

The {% aTargetBlank "https://pub.dev/packages/geolocator", "geolocator" %}
package is a popular option for getting
the current latitude and longitude of a device.
For a basic example app that uses this package, see this {% aTargetBlank
"https://github.com/mvolkmann/flutter_geolocation", "GitHub repo" %}.
The README contains instructions on configuring Android and iOS
to request permission for obtaining geolocation data.

By default the iOS Simulator always returns a location in San Francisco.
This can be changed to use a different mock location,
but it cannot detect the location of the computer on which it is running.

## Google Maps

To display a map of a given location with Google Maps:

1. Install the {% aTargetBlank "https://pub.dev/packages/google_maps_flutter",
   "google_maps_flutter" %} plugin from pub.dev.

1. Get an API key.

   - Browse the {% aTargetBlank "https://mapsplatform.google.com",
     "Google Maps Platform" %}.
   - Click the "Get Started" button.
   - Click the project dropdown in the header.
   - In the dialog that opens, click "NEW PROJECT".
   - Enter a name for the project.
   - Click the "CREATE" button.
   - Click "SELECT PROJECT".
   - In the left nav, click "Credentials".
   - Click "+ CREATE CREDENIALS".
   - Click "API key".
   - Copy the API key that is displayed.
   - Click the "CLOSE" button.

1. Enable APIs.

   - In the left nav, click "APIs".
   - Click "Maps SDK for Android".
   - Click the "ENABLE" button.
   - Under "Additional APIs, click "Maps SDK for iOS".
   - Click the "ENABLE" button.
   - Optionally click and enable "Directions API" under "Additional APIs".

1. Edit `android/app/src/main/AndroidManifest.xml` and add the following
   after the `meta-data` element for `flutterEmbedding`:

   ```xml
   <meta-data
       android:name="com.google.android.geo.API_KEY"
       android:value="your-api-key" />
   ```

1. Add this file in `.gitignore` so the API key is no exposed
   in the Git repository.
   s
1. Edit `ios/Runner/AppDelegate.swift`.

   - Add the following import after the existing imports:

     ```swift
     import GoogleMaps
     ```

   - Add the following line as the first line in the `application` function:

     ```swift
     GMSServices.provideAPIKey("your-api-key");
     ```

1. Add this file in `.gitignore` so the API key is no exposed
   in the Git repository.

1. Display a map.

   - Add the following imports:

   ```dart
   import 'dart:async' show Completer;
   import 'package:flutter/foundation.dart' show Factory;
   import 'package:flutter/gestures.dart';
   import 'package:google_maps_flutter/google_maps_flutter.dart';
   ```

   - Render a map with the following code inside a `StatefulWidget`.
     Zoom controls are only supported in Android. See {% aTargetBlank
     "https://github.com/flutter/plugins/pull/831#discussion_r400472577",
     "this discussion" %}.
     The code below adds its own zoom in and zoom out buttons
     for a consistent UI in Android and iOS.
     It wraps the `GoogleMap` widget in a `Stack`
     and positioning buttons over the map.

   ```dart
   final controllerCompleter = Completer<GoogleMapController>();

   // Call this to render the map
   Widget buildMap() {
     // position is a Position object from geolocator plugin.
     final latLng = LatLng(position!.latitude, position!.longitude);
     final cameraPosition = CameraPosition(target: latLng, zoom: zoom);
     final marker = Marker(markerId: MarkerId('my-location'), position: latLng);

     // This allows the GoogleMap widget to process gestures for
     // panning and zooming the map even if it is inside a ListView
     // which would otherwise capture all of those gestures.
     final gestureRecognizers = {
       Factory<OneSequenceGestureRecognizer>(() => EagerGestureRecognizer()),
     };

     return SizedBox(
       child: Stack(
         children: [
           GoogleMap(
             gestureRecognizers: gestureRecognizers,
             initialCameraPosition: cameraPosition,
             onMapCreated: (GoogleMapController controller) {
               controllerCompleter.complete(controller);
             },
             mapType: MapType.normal, // or .hybrid or .satellite
             markers: {marker},
             myLocationButtonEnabled: false, // hides provided lower-right button
           ),
           Positioned(
             child: FloatingActionButton.small(
               child: Icon(Icons.add),
               onPressed: () => changeCamera(latLng, ++zoom),
             ),
             bottom: 45,
             right: 0,
           ),
           Positioned(
             child: FloatingActionButton.small(
               child: Icon(Icons.remove),
               onPressed: () => changeCamera(latLng, --zoom),
             ),
             bottom: 0,
             right: 0,
           ),
         ],
       ),
       height: 200,
       width: double.infinity,
     );
   }

   void changeCamera(LatLng latLng, double zoom) async {
     final controller = await controllerCompleter.future;
     controller.moveCamera(CameraUpdate.newCameraPosition(
     CameraPosition(target: latLng, zoom: zoom)));
   }
   ```

1. If the app is already running, stop it and restart it.

## App Icons

The steps to customize the launcher icon used by a Flutter app are:

1. Enter `flutter pub add flutter_launcher_icons`
1. Edit `pubspec.yaml` and add the following at the end:

   ```yaml
   flutter_icons:
     android: 'launcher_icon'
     ios: true
     image_path: 'assets/images/icon.png'
     remove_alpha_ios: true
   ```

1. Create the directory `assets/images` in the project.
1. Copy a square PNG file into this file and name it `icon.png`.
1. Edit `android/app/build.gradle` and change
   `minSdkVersion flutter.minSdkVersion` to `minSdkVersion 21`
   (or at least version 16)
1. Enter `flutter pub run flutter_launcher_icons:main`

## Splash Screens

The pub.dev package {% aTargetBlank "https://pub.dev/packages/splashscreen",
"splashscreen" %} displays a splash screen for a given number of seconds
when an app starts and they displays the main screen of the app.

One issue with this package is that the latest version in pub.dev (1.3.5)
does not support null safety.
But there is a version in the GitHub repository that does.
To use that version, add the following dependency in `pubspec.yaml`:

```yaml
splashscreen:
  git:
    url: https://github.com/DPLYR-dev/SplashScreenFlutterPackage.git
    ref: master
```

To add a splash screen, change the `build` method of the home screen
to something like the following:

```dart
  Widget build(BuildContext context) {
    return SplashScreen(
      backgroundColor: Colors.yellow,

      image: Image.asset('assets/images/hobbit-book.jpg'),
      // This is the height of the image.
      photoSize: 230.0,

      // This is rendered under the image.
      title: Text(
        'Splashscreen Demo',
        style: TextStyle(
          fontSize: 24,
          fontWeight: FontWeight.bold,
        ),
      ),

      // This causes a loading spinner to be rendered
      // until the splash screen goes away.
      // It is rendered under the title.
      useLoader: true,
      loaderColor: Colors.red,

      // This is rendered under the loading spinner.
      loadingText: Text('The fun will start soon.'),
      loadingTextPadding: EdgeInsets.all(20),

      // Not sure what this affects.
      styleTextUnderTheLoader: TextStyle(),

      // The splash screen will be displayed for this duration.
      seconds: 10,

      // The widget returned by this function will be
      // rendered when the splash screen goes away.
      navigateAfterSeconds: _afterSplash(),
    );
  }
```

## Onboarding Screens

The pub.dev package {% aTargetBlank
"https://pub.dev/packages/introduction_screen", "introduction_screen" %}
simplifies the implemenation of app onboarding pages.
These are typically used to provide an overview of the app functionality.
Users can view each onboarding page in order,
jump to them in any order, or skip to the last onboarding page.
The last onboading page contains a button that can be tapped
to navigate to the first page of the actual app.

This package does not provide a way to remember that a user has
already seen the onboarding pages in order to
avoid displaying them in subsequent runs of the app.
A recommended way to implement this is to use the
{% aTargetBlank "https://pub.dev/packages/shared_preferences",
"shared_preferences" %} package which can save the fact that
a user has already seen the onboarding pages on the device.

The following code demonstrates implementing onboarding screens:

```dart
import 'package:flutter/material.dart';
import 'package:flutter/services.dart' show SystemChrome, SystemUiOverlayStyle;
import 'package:introduction_screen/introduction_screen.dart';

const title = 'My App';

void main() {
  // Make the status bar background transparent so the background color
  // of the onboarding pages will also appear in the status bar
  // without being shaded.
  SystemChrome.setSystemUIOverlayStyle(
    // We are using dark instead of light so the text
    // on the status bar will be gray instead of white.
    SystemUiOverlayStyle.dark.copyWith(statusBarColor: Colors.transparent),
  );

  runApp(
    MaterialApp(
      debugShowCheckedModeBanner: false,
      home: Onboarding(),
      theme: ThemeData(primarySwatch: Colors.blue),
      title: title,
    ),
  );
}

class PageData {
  final String body;
  final String imagePath;
  final String title;

  const PageData({
    required this.body,
    required this.imagePath,
    required this.title,
  });
}

class Onboarding extends StatelessWidget {
  Onboarding({Key? key}) : super(key: key);

  final pages = [
    PageData(
      title: 'Maisey',
      body: 'This is a Treeing Walker Coonhound.',
      imagePath: 'assets/images/treeing-walker-coonhound.jpg',
    ),
    PageData(
      title: 'Ramsay',
      body: 'This is a Native American Indian Dog.',
      imagePath: 'assets/images/native-american-indian-dog.jpg',
    ),
    PageData(
      title: 'Oscar',
      body: 'This is a German Shorthaired Pointer.',
      imagePath: 'assets/images/german-shorthaired-pointer.jpg',
    ),
    PageData(
      title: 'Comet',
      body: 'This is a Wippet.',
      imagePath: 'assets/images/whippet.jpg',
    ),
  ];

  void endOnboarding(context) {
    // We are using pushReplacement instead of push so
    // there is no back button for returning to Onboarding.
    Navigator.of(context).pushReplacement(
      MaterialPageRoute(builder: (_) => Home()),
    );
  }

  @override
  Widget build(BuildContext context) {
    return IntroductionScreen(
      // Tapping this exits onboarding.
      done: Text('Done'),
      globalBackgroundColor: Colors.yellow.shade100,
      // This prevents the images from overlapping the safe area.
      isTopSafeArea: true,
      // This icon appears on the last onboarding page.
      // Tapping it ends onboarding.
      next: Icon(Icons.navigate_next),
      onDone: () => endOnboarding(context),
      pages: pages
          .map(
            (page) => PageViewModel(
              title: page.title,
              body: page.body,
              image: Image.asset(page.imagePath, height: 300),
            ),
          )
          .toList(),
      // The skip button appears on all onboarding pages except the last.
      // Tapping it advances to the last onboarding page.
      showSkipButton: true,
      skip: Text('Skip'),
    );
  }
}

class Home extends StatefulWidget {
  const Home({Key? key}) : super(key: key);

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            const Text('Add content here.'),
          ],
        ),
      ),
    );
  }
}
```

## Logging

It can be helpful to output messsges to the terminal when debugging an app.
This can be done using the Dart `print` function.

For even better logging output, consider using the pub.dev package
{% aTargetBlank "https://pub.dev/packages/logger", "logger" %}.

The following code demonstrates using this.
When the buttons are pressed in order, the colored output
shown in the screenshot is produced in the terminal.

<img alt="Flutter loggerUI" style="width: 30%"
    src="/blog/assets/flutter-logger-ui.png?v={{pkg.version}}"
    title="Flutter logger UI">
<img alt="Flutter logger output" style="width: 65%"
    src="/blog/assets/flutter-logger-output.png?v={{pkg.version}}"
    title="Flutter logger output">

```dart
import 'package:flutter/material.dart';
import 'package:logger/logger.dart';

const title = 'My App';

void main() => runApp(
      MaterialApp(
        title: title,
        theme: ThemeData(
          primarySwatch: Colors.blue,
        ),
        home: const Home(),
      ),
    );

class Home extends StatefulWidget {
  const Home({Key? key}) : super(key: key);

  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  final logger = Logger();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            ElevatedButton(
              child: Text('Verbose'),
              onPressed: () => logger.v('This is a verbose message.'),
            ),
            ElevatedButton(
              child: Text('Debug'),
              onPressed: () => logger.d('This is a debug message.'),
            ),
            ElevatedButton(
              child: Text('Info'),
              onPressed: () => logger.i('This is an info message.'),
            ),
            ElevatedButton(
              child: Text('Warning'),
              onPressed: () => logger.w('This is a warning message.'),
            ),
            ElevatedButton(
              child: Text('Error'),
              onPressed: () => logger.e('This is an error message.'),
            ),
            ElevatedButton(
              child: Text('WTF'),
              onPressed: () => logger.wtf('This is a wtf message.'),
            ),
          ],
        ),
      ),
    );
  }
}
```

## Tests

Three primary kinds of tests can be written for Flutter applications.
These include unit tests, widget tests, and integration tests.
Unit and widget tests always run headless, not requiring a simulator or device.
Integration tests always run in a simulator, web browser or device.
Unit and widget tests run fairly quickly.
Integration tests take a long time to build and start,
often around two minutes.

An additional kind of test is supported by the {% aTargetBlank
"https://pub.dev/packages/golden_toolkit", "golden_toolkit" %} package.
It is used to implement tests that capture an image of a widget
and compare it to a previously captured image.
These are regression tests that test fail if the images differ.
For those familiar with the {% aTargetBlank "https://jestjs.io", "Jest" %}
testing framework for web applications,
golden_toolkit is similar to Jest snapshot tests.

In general each test is composed of three sections: arrange, act, and assert.
The arrange section creates the environment needed by the test.
For example, in a widget test this
renders the widget with certain characteristics.
The act section performs actions on the environment.
For example, in a widget test this might include
entering text and clicking buttons.
The assert part makes assertions about the expected state of the environment.
For example, in a widget test this might assert that specific text
is present on the screen or that a button becomes enabled.

Flutter projects created with the `flutter create` command
already have a dev dependency on the `flutter_test` package
which is built on the `test` package.
Do not add a dependency on {% aTargetBlank
"https://pub.dev/packages/test", "test" %} in`pubspec.yaml`.
This will likely cause a "version solving failed" error
when the tests are run.

The {% aTargetBlank
"https://api.flutter.dev/flutter/flutter_test/flutter_test-library.html",
"flutter_test" %} package defines many
classes, constants, and functions used to implement tests.

Commonly used test constants are described below.
All of these are `Matcher` objects that match certain values.
These are used as the second positional argument
in calls to the `expect` function.

| Constants         | Matches                                                |
| ----------------- | ------------------------------------------------------ |
| `findsNothing`    | widget tree that contains no matching widgets          |
| `findsOneWidget`  | widget tree that contains a single matching widget     |
| `findsWidget`     | widget tree that contains at least one matching widget |
| `isFalse`         | everything but `true`                                  |
| `isNan`           | number value `NaN`                                     |
| `isNegative`      | any negative number                                    |
| `isNonNegative`   | zero or any positive number                            |
| `isNonPositive`   | zero or any negative number                            |
| `isNonZero`       | any number except zero                                 |
| `isNotNan`        | any number except `NaN`                                |
| `isNotNull`       | any value except `null`                                |
| `isNull`          | only `null`                                            |
| `isPositive`      | any positive number                                    |
| `isTrue`          | only `true`                                            |
| `isZero`          | only zero                                              |
| `returnsNormally` | function call that does not throw an exception         |
| `throwsException` | function call that throws an exception                 |

Commonly used test functions are described below:

| Function               | Description                                                                                                                                                 |
| ---------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `addTearDown`          | registers a function to call after the current test completes                                                                                               |
| `allOf`                | returns `Matcher` that matches if all of the argument matchers (limit of 7) match                                                                           |
| `anyElement`           | returns `Matcher` that matches if any element in an `Iterable` matches a value or `Matcher`                                                                 |
| `anyOf`                | returns `Matcher` that matches if any of the argument matchers (limit of 7) match                                                                           |
| `closeTo`              | returns `Matcher` that matches if a number is within a delta of a given value                                                                               |
| `contains`             | returns `Matcher` that matches if a value "contains" an expected value; specialized for `String`, `Map`, and `Iterable` values                              |
| `containsPair`         | returns `Matcher` that matches a `Map` containing a given key/value pair                                                                                    |
| `containsValue`        | returns `Matcher` that matches a `Map` containing a given value (ignores keys)                                                                              |
| `endsWith`             | returns `Matcher` that matches a `String` that ends with a given substring                                                                                  |
| `equals`               | returns `Matcher` that matches any object that is "structurally equal" to a given value                                                                     |
| `equalsIgnoringCase`   | returns `Matcher` that matches a `String` that equal to a given `String` ignoring case substring                                                            |
| `everyElement`         | returns `Matcher` that matches any `Iterable` where all the elements match a given value or `Matcher`                                                       |
| `expect`               | asserts that a value matches another value or `Matcher`; very commonly used                                                                                 |
| `expectLater`          | asserts that a `Future` completes with a value that matches another value or `Matcher` (such as `emitsInOrder`)                                             |
| `fail`                 | throws `TestFailure` with a given `String` message                                                                                                          |
| `findsNWidgets`        | returns `Matcher` that matches a `Finder` that finds a given number of matching widgets; alternative to `findsNothing`, `findsOneWidget`, and `findsWidget` |
| `greaterThan`          | returns `Matcher` that matches any value greater than a given value                                                                                         |
| `greaterThanOrEqualTo` | returns `Matcher` that matches any value greater than or equal to a given value                                                                             |
| `group`                | creates a group (or suite) of tests                                                                                                                         |
| `hasLength`            | returns `Matcher` that matches any object with a `length` property whose value is a given value                                                             |
| `inClosedOpenRange`    | returns `Matcher` that matches any number in the range [low, high); closed includes and open excludes                                                       |
| `inExclusiveRange`     | returns `Matcher` that matches any number in the range (low, high)                                                                                          |
| `inInclusiveRange`     | returns `Matcher` that matches any number in the range [low, high]                                                                                          |
| `inOpenClosedRange`    | returns `Matcher` that matches any number in the range (low, high]                                                                                          |
| `isA`                  | returns `Matcher` that matches any value that is or extends/implements a given type                                                                         |
| `isIn`                 | returns `Matcher` that matches if an expected value value "contains" a given value; opposite of `contains` values                                           |
| `isInstanceOf`         | returns `Matcher` that matches any value that is exactly a given type                                                                                       |
| `isNot`                | returns `Matcher` that gives the opposite result of a given `Matcher`                                                                                       |
| `lessThan`             | returns `Matcher` that matches any value less than a given value                                                                                            |
| `lessThanOrEqualTo`    | returns `Matcher` that matches any value less than or equal to a given value                                                                                |
| `matches`              | returns `Matcher` that matches a `String` that matches a given `Pattern` (regular expression)                                                               |
| `moreOrLessEquals`     | returns `Matcher` that matches a `double` that is within a given epsilon of another                                                                         |
| `same`                 | returns `Matcher` that matches an object that is the same object as another reference                                                                       |
| `setUp`                | registers a function to call before each test                                                                                                               |
| `setUpAll`             | registers a function to call once before the first test                                                                                                     |
| `startsWith`           | returns `Matcher` that matches a `String` that starts with a given substring                                                                                |
| `tearDown`             | registers a function to call after each test                                                                                                                |
| `tearDownAll`          | registers a function to call once after the last test                                                                                                       |
| `test`                 | defines a single unit test                                                                                                                                  |
| `testWidgets`          | defines a single widget test                                                                                                                                |
| `throwsA`              | returns `Matcher` that matches a function, `Future`, or function that returns a `Future` that matches a given `Matcher`                                     |
| `within`               | returns `Matcher` that matches a number that is within a "distance" of another, computed by a `DistanceFunction`                                            |

### Unit Tests

Flutter unit tests are for testing logic, not widgets.
Each unit test is intended to test a single function, method, or class.

To implement a unit test for classes and functions defined in
the file `lib/sample.dart`, create the file `test/sample_test.dart`.

In VS Code, right-click a source file in the `lib` directory
to be tested in the Navigator and select "Go to Tests".
If a corresponding test file exists in the `test` directory,
it will be opened. If not, VS Code will offer to create it.

The provided test code contains a single call to `testWidgets`,
passing it a function that takes a {% aTargetBlank
"https://api.flutter.dev/flutter/flutter_test/WidgetTester-class.html",
"WidgetTester" %} object.
For non-widget tests, change this to a `test` function
that is passed a function that takes no arguments.

Each test file defines a `main` function.
This should contain calls to the global `test` function.
The `test` function takes positional arguments for a description (`String`)
and a no-arg function that contains calls to the global `expect` function.

The `test` function also takes the named argument `skip`
(`bool` or `String` describing why) and several others.
Unfortunatly since Dart requires named arguments
to follow all positional arguments, the `skip` argument
must follow the no-arg function that defines the test.
This makes it difficult to spot the skipped tests.

The `expect` function takes positional arguments for an
actual value and a matcher (expected value or `Matcher` object).
It also takes the named arguments `reason` (`String`)
and `skip` (`bool` or `String` describing why).

To create test suites that group tests, call the global `group` function,
passing it positional arguments for a description (`String`) and
a no-arg function that contains several calls to the `test` function.
It also takes the named argument `skip` (`bool` or `String` describing why).

To run all the test files from a terminal, enter `flutter test`.
Currently there is no option to run tests in a "watch" mode
to automatically rerun them when code changes are saved.
To run only tests in specific files, specify a file path after the command.
For example, `flutter test test/home_test.dart`.

When viewing a test file in VS Code,
there will be "Run" and "Debug" links above the `main` function
and each call to the `group` and `test` functions.
Click those links to run or debug only the corresponding code.

If a test passes, VS Code will display a green, circled checkbox
in the gutter to the left of the `test` function call.
If a test fails, VS Code will display a red, circled "X" in the same location
along with the expected and actual values of the failed `expect` call.
Each `group` call will have a similar gutter icon
indicating whether all of its tests passed or any failed.

After attempting to fix the failed tests, rerun them
and repeat until all the tests have a green checkmark.
Tests can be rerun by clicking the green and red gutter icons
or by clicking the "Run" and "Debug" links.
It can take a few seconds before it is apparent
that tests have started running again, but
a beaker icon appears in the Activity Bar (left side column of icons).
with a blue clock indicator.

The following code provides a basic example of a unit test source file:

```dart
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_pageview/math.dart';

void main() {
  group('math', () {
    test('addition', () {
      expect(add(0, 0), 0);
      expect(add(0, 1), 1);
      expect(add(1, 1), 2);
    });

    test('multiplication', () {
      expect(multiply(0, 0), 0);
      expect(multiply(0, 1), 0);
      expect(multiply(1, 1), 1);
    });
  });
}
```

### Widget Tests

Flutter widget tests are for testing individual widgets.
Each test should focus on a single widget.

New Flutter projects ship with a file named `widget_test.dart`.
Use this is as an example when writing widget tests.

In contains a `main` function that makes
a single call to the `testWidgets` function.
This is passed a description `String` and
a function that is passed a {% aTargetBlank
"https://api.flutter.dev/flutter/flutter_test/WidgetTester-class.html",
"WidgetTester" %} object.
The `WidgetTester` object is key to interacting with widgets.
This includes tapping buttons, entering text, and dragging widgets.

Modify the provided function passed to `testWidgets`
and add more similar calls to define additional tests.

The function passed to `testWidget` should do four basic things:

1. Render an app widget.
1. Find widgets within the app to be used by the test using the `find` object.
1. Interact with the widgets that were found.
1. Make assertions about the state of the UI after the interactions.

The `find` object support several methods for finding widgets in different ways.
The most commonly used methods are described below:

| `find` Method | Description                                                               |
| ------------- | ------------------------------------------------------------------------- |
| `byIcon`      | pass an `IconData` object such as those in constants of the `Icons` class |
| `byKey`       | pass a `ValueKey` object that created with a key string                   |
| `byType`      | pass a widget `Type`; returns a `Finder` object                           |
| `text`        | pass a `String` to find a widget containing it                            |

Key strings assigned to widgets should be unique throughout the app.

To get individual widgets from a `Finder` object,
use the properties `first`, and `last` and the method `at(int index)`.
Interestingly there is no way to determine
the number of matches that were found.

Suppose an app renders the following button:

```dart
ElevatedButton(
  child: Text('Press Me'),
  key: Key('myButton'),
  onPress: () { print('got press')},
);
```

A test can find this button using any of the following:

```dart
var myButton = find.byKey(ValueKey('myButton'));
var myButton = find.text('Press Me');
var myButton = find.byType(ElevatedButton).first;
```

Once the button is found, it can be tapped with the following:

```dart
await tester.tap(myButton);
```

If `find.byType` finds a single match,
it can be acted on without using the `first` property.
For example:

```dart
var buttons = find.byType(ElevatedButton);
// These are equivalent when only one match is found.
await tester.tap(buttons.first);
await tester.tap(buttons);
// But if more than one match is found,
// the previous line results in the following error:
// The finder "2 widgets with type ElevatedButton" ...
// (used in a call to "tap()") ambiguously found multiple
// matching widgets.  The "tap()" method needs a single target.
```

If a `TextField` or `TextFormField` widget is found,
text can be entered into it with the following:

```dart
await tester.enterText(myTextField, 'some text');
```

After interacting with widgets, call the `WidgetTester` {% aTargetBlank
"https://api.flutter.dev/flutter/flutter_test/WidgetTester/pump.html",
"pump" %} or {% aTargetBlank
"https://api.flutter.dev/flutter/flutter_test/WidgetTester/pumpAndSettle.html",
"pumpAndSettle" %} method to rebuild the UI.
Both methods return a `Future` to `await` before making assertions.
By default the `pumpAndSettle` method repeatedly calls `pump`
every 100ms until there are no longer any frames scheduled.
It is the safest way to ensure that the UI is in the intended state
before calling the `expect` function.

```dart
await tester.pumpAndSettle();
```

Objects from the `WidgetTester` class have a `testDescription` property
that holds the `String` description of the currently running test.
It may be useful to include this in error messages.

The most commonly used `WidgetTester` methods are described below:

| Method                      | Description                                                                                                     |
| --------------------------- | --------------------------------------------------------------------------------------------------------------- |
| `drag(...)`                 | drags a given widget by a specified offset                                                                      |
| `enterText(Finder, String)` | moves focus to a text input widget and replaces its contents                                                    |
| `fling(...)`                | executes a fling gesture from the center of a widget over some offset                                           |
| `flingFrom(...)`            | executes a fling gesture from a starting location over some offset                                              |
| `getRect(Finder)`           | gets the `Rect` of a widget                                                                                     |
| `getSize(Finder)`           | gets the `Size` of a widget                                                                                     |
| `longPressj(...)`           | executes a long press at the center of a widget                                                                 |
| `longPressAt(...)`          | executes a long press at a given location                                                                       |
| `pageBack()`                | dismisses the current page                                                                                      |
| `printToConsole(String)`    | prints a `String` to the console; can also use the Dart `print` function                                        |
| `pump`                      | triggers a new "frame" (rebuilding the UI) after an optional duration                                           |
| `pumpAndSettle`             | "repeatedly calls pump with the given duration until there are no longer any frames scheduled"; frequently used |
| `pumpWidget`                | "renders the UI from the given widget"                                                                          |
| `showKeyboard`              | moves focus to a given text input widget, triggering display of an on-screen keyboard                           |

The following code provides widget tests for the `PageView` demo app
presented earlier.
While `MyApp` can be viewed as a "single widget",
widget tests typically test widgets lower in the widget tree.

```dart
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:flutter_pageview/main.dart';

void main() {
  testWidgets('Page1', (WidgetTester tester) async {
    // Build the app and trigger the first frame.
    // When testing a widget that does not return a `MaterialApp` widget,
    // tests generally need to begin with the following:
    await tester.pumpWidget(
      MaterialApp(
        home: Scaffold(
          body: MyWidget(), // widget being tested
        ),
      ),
    );

    // Find widgets.
    var shoutBtn = find.byKey(ValueKey('shoutBtn'));
    var nameField = find.byKey(ValueKey('nameField'));

    // Verify the contents of the first page.
    expect(find.text('This is page #1.'), findsOneWidget);

    var name = 'Mark';
    await tester.enterText(nameField, name);
    await tester.pumpAndSettle();
    expect(find.text('Hello, $name!'), findsOneWidget);

    await tester.tap(shoutBtn);
    await tester.pumpAndSettle();
    expect(find.text('Hello, ${name.toUpperCase()}!'), findsOneWidget);
  });

  testWidgets('forward and back buttons', (WidgetTester tester) async {
    Future<void> changePage(button, expectedText) async {
      await tester.tap(button);
      await tester.pumpAndSettle();
      expect(find.text(expectedText), findsOneWidget);
    }

    bool buttonEnabled(button) =>
        tester.widget<IconButton>(button).onPressed != null;

    // Build the app and trigger the first frame.
    // Note that MyApp creates a `MaterialApp` that creates a `Scaffold`.
    await tester.pumpWidget(MyApp());

    var backBtn = find.byKey(ValueKey('backBtn'));
    var forwardBtn = find.byKey(ValueKey('forwardBtn'));

    expect(find.text('This is page #1.'), findsOneWidget);

    // Verify that we can change pages with the forward and back buttons.
    expect(buttonEnabled(backBtn), isFalse);
    expect(buttonEnabled(forwardBtn), isTrue);
    await changePage(forwardBtn, 'This is page #2.');
    expect(buttonEnabled(backBtn), isTrue);
    await changePage(forwardBtn, 'This is page #3.');
    expect(buttonEnabled(forwardBtn), isFalse);
    await changePage(backBtn, 'This is page #2.');
    expect(buttonEnabled(forwardBtn), isTrue);
    await changePage(backBtn, 'This is page #1.');
    expect(buttonEnabled(backBtn), isFalse);
  });

  testWidgets('swipe left and right', (WidgetTester tester) async {
    Future<void> swipe(page, swipeLeft, expectedText) async {
      double deviceWidth = 600; // MediaQuery.of(context).size.width;
      var offset = Offset(deviceWidth * (swipeLeft ? 1 : -1), 0);
      var speed = 300.0; // pixels per second
      await tester.fling(page, offset, speed);
      await tester.pumpAndSettle();

      expect(find.text(expectedText), findsOneWidget);
    }

    await tester.pumpWidget(MyApp());

    var page1 = find.byKey(ValueKey('page1'));
    var page2 = find.byKey(ValueKey('page2'));
    var page3 = find.byKey(ValueKey('page3'));

    await swipe(page1, false, 'This is page #2.'); // goes forward
    await swipe(page2, false, 'This is page #3.'); // goes forward
    await swipe(page3, false, 'This is page #3.'); // stays on same page
    await swipe(page3, true, 'This is page #2.'); // goes backward
    await swipe(page2, true, 'This is page #1.'); // goes backward
    await swipe(page1, true, 'This is page #1.'); // stays on same page
  });

  testWidgets('tap dots', (WidgetTester tester) async {
    Future<void> tapDot(int number) async {
      var dot = find.byKey(ValueKey('dot$number'));
      await tester.tap(dot);
      await tester.pumpAndSettle();
      expect(find.text('This is page #$number.'), findsOneWidget);
    }

    await tester.pumpWidget(MyApp());

    for (var number = 1; number <= 3; number++) {
      await tapDot(number);
    }
  });
}
```

### Integration Tests

Flutter integration tests are for testing an app as a whole.

The code written for integration tests is
nearly identical to the code written for widget tests.
Both use the `find` object, a `WidgetTester` object
for interacting with widgets, and the `expect` function.

The way that integration tests render the app to be tested is a bit different.
Rather than calling `await tester.pumpWidget(SomeWidget());`, integration tests
call `IntegrationTestWidgetsFlutterBinding.ensureInitialized();`
outside the test functions and call the following inside each test:

```dart
app.main();
await tester.pumpAndSettle();
```

The steps to setup and create an integration test are:

1. Edit `pubspec.yaml` and add the following under `dev_dependencies`:

   ```yaml
   integration_test:
     sdk: flutter
   ```

1. Create a top-level directory named "integration_test".

1. Create the file `app_test.dart` in this directory.

1. Add code in `app_test.dart` similar to the following:

   ```dart
   import 'package:flutter_test/flutter_test.dart';
   import 'package:integration_test/integration_test.dart';

   import 'package:introduction/main.dart' as app;

   void main() {
     IntegrationTestWidgetsFlutterBinding.ensureInitialized();

     group('end-to-end test', () {
       testWidgets('tap floating action button and verify counter',
           (WidgetTester tester) async {
         app.main();
         await tester.pumpAndSettle();

         // Verify counter starts at 0.
         expect(find.text('0'), findsOneWidget);

         // Find and tap the floating action button.
         final Finder fab = find.byTooltip('Increment');
         await tester.tap(fab);

         // Rebuild the UI, triggering a new frame.
         await tester.pumpAndSettle();

         // Verify counter increments by 1.
         expect(find.text('1'), findsOneWidget);
       });
     });
   }
   ```

1. Run the test by entering `flutter test integration_test`.
   This runs all of the test source files in the `integration_test` directory.
   You will prompted to select where the test should be run.
   For example, if an iPhone is attached to the computer with a USB cable
   and the iOS simulator is running, you could see the following:

   ```text
   [1]: iPhone (00008020-001A7D141A9A002E)
   [2]: iPhone 13 (46325045-B5FE-4313-BBD5-10A7D1178B50)
   [3]: Chrome (chrome)
   ```

   Press 1, 2, or 3 to select one.

   This only runs the first test file it finds in the `integration_test` directory.
   To run others one at a time, add their name after the directory name.
   For example, enter `flutter test integration_test/second_text.dart`.
   To run all the test files, enter `flutter test integration_test/*.dart`.

   Alternatively to run the test on a specific device,
   enter `flutter devices` to see a list of available devices.
   Copy the id of a device where the test should be run and enter
   `flutter test integration_test/app_test.dart -d {device-id}`.

Even a basic integration test will take 1-2 minutes to build and start.

The following code provides integration testing for the `PageView` demo app
presented earlier.

```dart
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:integration_test/integration_test.dart';

import 'package:flutter_pageview/main.dart' as app;

void main() {
  IntegrationTestWidgetsFlutterBinding.ensureInitialized();

  testWidgets('Page1', (WidgetTester tester) async {
    app.main();
    await tester.pumpAndSettle();

    // Find widgets.
    var shoutBtn = find.byKey(ValueKey('shoutBtn'));
    var nameField = find.byKey(ValueKey('nameField'));

    // Verify the contents of the first page.
    expect(find.text('This is page #1.'), findsOneWidget);

    var name = 'Mark';
    await tester.enterText(nameField, name);
    await tester.pumpAndSettle();
    expect(find.text('Hello, $name!'), findsOneWidget);

    await tester.tap(shoutBtn);
    await tester.pumpAndSettle();
    expect(find.text('Hello, ${name.toUpperCase()}!'), findsOneWidget);
  });

  testWidgets('forward and back buttons', (WidgetTester tester) async {
    Future<void> changePage(button, text) async {
      await tester.tap(button);
      await tester.pumpAndSettle();
      expect(find.text(text), findsOneWidget);
    }

    app.main();
    await tester.pumpAndSettle();

    var backBtn = find.byKey(ValueKey('backBtn'));
    var forwardBtn = find.byKey(ValueKey('forwardBtn'));

    expect(find.text('This is page #1.'), findsOneWidget);
    await changePage(forwardBtn, 'This is page #2.');
    await changePage(forwardBtn, 'This is page #3.');
    await changePage(backBtn, 'This is page #2.');
    await changePage(backBtn, 'This is page #1.');
  });

  testWidgets('swipe left and right', (WidgetTester tester) async {
    var deviceWidth = 0.0;

    Future<void> swipe(page, swipeLeft, expectedText) async {
      var offset = Offset(deviceWidth * (swipeLeft ? 1 : -1), 0);
      var speed = 300.0; // pixels per second
      await tester.fling(page, offset, speed);
      await tester.pumpAndSettle();

      expect(find.text(expectedText), findsOneWidget);
    }

    await tester.pumpWidget(MyApp());

    // Get the device width.
    final BuildContext context = tester.element(find.byType(Container));
    deviceWidth = MediaQuery.of(context).size.width;

    var page1 = find.byKey(ValueKey('page1'));
    var page2 = find.byKey(ValueKey('page2'));
    var page3 = find.byKey(ValueKey('page3'));

    await swipe(page1, false, 'This is page #2.'); // goes forward
    await swipe(page2, false, 'This is page #3.'); // goes forward
    await swipe(page3, false, 'This is page #3.'); // stays on same page
    await swipe(page3, true, 'This is page #2.'); // goes backward
    await swipe(page2, true, 'This is page #1.'); // goes backward
    await swipe(page1, true, 'This is page #1.'); // stays on same page
  });

  testWidgets('tap dots', (WidgetTester tester) async {
    Future<void> tapDot(int number) async {
      var dot = find.byKey(ValueKey('dot$number'));
      await tester.tap(dot);
      await tester.pumpAndSettle();
      expect(find.text('This is page #$number.'), findsOneWidget);
    }

    app.main();
    await tester.pumpAndSettle();

    for (var number = 1; number <= 3; number++) {
      await tapDot(number);
    }
  });
}
```

### Mocking

Mocking in tests supplies canned results from functions
that are not meant to be tested.
For example, a function that makes an API call
can be mocked to return the same data every time.
This allows tests to focus on specific functionality being tested.
It also allows tests to run without requiring an internet connection.

The {% aTargetBlank "", "http" %} package includes
the ability to mock responses to HTTP requests.

The steps to use this are:

1. Add `http` to the list of `dependencies` in `pubspec.yaml`.

1. Add the following imports in a test file:

   ```dart
   import 'package:flutter_test/flutter_test.dart'; // defines test and expect
   import 'package:http/http.dart'; // defines Response
   import 'package:http/testing.dart'; // defines MockClient
   ```

1. Create a `MockClient` that specifies HTTP responses for given HTTP requests.
   For example:

   ```dart
   final client = MockClient((request) async {
     if (request.url.toString() != Employees.url) return Response('', 404);

     var mockJson =
         '{"data": [{"employee_salary": 100}, {"employee_salary": 200}]}';
     const headers = {'content-type': 'application/json'};
     return Response(mockJson, 200, headers: headers);
   });
   ```

1. Tell the code being tested to use `MockClient` in place of
   the normal `Client` instance provided by the `http` package.

   There are multiple ways to do this.
   One way is to define a static property in the class being tested
   like the following which can be directly set from a test.
   Then use this inside the class being tested for sending all HTTP requests.

   ```dart
   static var client = Client();
   ```

Mock operations other than HTTP requests
is simplified by using a mocking package.
The preferred Flutter mocking package is
{% aTargetBlank "https://pub.dev/packages/mockito", "mockito" %}.
This generates code for mock implementations of classes
and provides methods that are useful in writing tests.

The steps to use mockito are:

1. Add `mockito` and `build_runner` to
   the list of `dev_dependencies` in `pubspec.yaml`.

1. In the main `.dart` file of the app, add the following imports:

   ```dart
   import 'package:mockito/annotations.dart';
   import 'package:mockito/mockito.dart';
   // One of these for each class that will be mocked.
   import 'some-class.mocks.dart';
   ```

1. Add the following annotation before the `main` function:

   ```dart
   // List each class to be mocked.
   @GenerateMocks([SomeClass1, SomeClass2, ...])
   void main() {
   ```

1. Generate `.dart` files for the classes to be mocked
   by entering `dart run build_runner build`.
   These classes have the same functionality as the classes they mock,
   but gain new methods used for verifying and stubbing calls.

1. Call the following methods in tests to
   verify calls to methods of stubbed objects:

   - `verify(call)`

     This causes a test to fail if the call has not occurred.
     `call` here can have the syntax
     `mockedObject.someMethod(someArguments)` or `mockedObject.someGetter`.
     This can be followed by `.called(n)` to
     verify that the call occurred `n` times.
     `n` can be a number of a matcher like `greaterThan(n)`.

   - `verifyInOrder([call1, call2, ...])`

     This causes a test to fail if the calls
     were not all made in the order specified.

   - `verifyNever(call)`

     This causes a test to fail if the call has occurred.

1. Call the following methods in tests to change what is
   returned or thrown from methods of stubbed objects:

   - `when(call).thenReturn(value)`

     This changes a specific call to always return a fixed value.
     To return a `Future` or `Stream`, use `thenAnswer` instead
     which is described below.

   - `when(call).thenThrows(SomeException(someArguments))`

     This changes a specific call to always throw a fixed exception.

   - `when(call).thenAnswer((_) { code to calculate response })`

     This changes a specific call to calculate a different value to return.

   If a call matches multiple stubs, the one defined last is used.

Mockito provides "argument matchers" that can be used in
calls passed to `when` in place of each argument value.
These include `any`, `anyNamed`, `argThat`, and `captureThat`.

- `any`: matches any positional argument value
- `anyNamed`: matches any named argument value with a given name
- `argThat`: matches any positional or named argument that matches a given `Matcher`
- `captureThat`: matches the same as `argThat`, but also captures the value
  so it can be used in a test assertion

For more details, see the {% aTargetBlank
"https://pub.dev/packages/mockito", "offical mockito documentation" %}.

Another mocking approach to consider is implementing a service registry class.
This class becomes the authority on
deciding which service instances should be used.
It can be configured to use mock service instances
instead of real ones when tests are being run.
A downside of this approach is that all code that uses services
must be modified to get instances from the service registry class.

### Test Coverage

The `flutter test` command can generate test coverage data.
To measure and report on test coverage:

1. Run tests with the `--coverage` flag.
   This creates the file `coverage/lcov.info`.

   ```bash
   flutter test --coverage test/*.dart
   ```

1. Generate an HTML report using the `genhtml` command.

   To install this on macOS using Homebrew,
   enter `brew install lcov`.

   ```bash
   genhtml -o coverage coverage/lcov.info
   ```

1. View the report.

   ```bash
   open coverage/index.html
   ```

The following screenshot shows a sample test coverage report.
Click the links for each source file to see
detail on the lines covered by a test.

<img alt="Test Coverage" style="width: 80%"
    src="/blog/assets/flutter-lcov.png?v={{pkg.version}}"
    title="Test Coverage">

## Deploying to App Stores

Popular services that simpilify deploying Flutter apps
to the Google Play Store and the iOS App Store include
{% aTargetBlank "https://docs.fastlane.tools", "fastlane" %},
{% aTargetBlank "https://blog.codemagic.io/getting-started-with-codemagic/",
"Codemagic" %}, {% aTargetBlank
"https://devcenter.bitrise.io/en/getting-started/getting-started-with-flutter-apps.html",
"Bitrise" %}, and {% aTargetBlank
"https://appcircle.io/blog/guide-to-automated-mobile-ci-cd-for-flutter-projects-with-appcircle/",
"Appcircle" %}.

TODO: Add more detail on at least one of these options.

The pub.dev package {% aTargetBlank "https://pub.dev/packages/rename",
"rename" %} "helps you to change your flutter project's
AppName and BundleId for different platforms."

The Google app review process typically takes less than two days to complete.

### Renaming

Before deploying a new app it may be desirable to rename it.
The easiest way to do this is to use the pub.dev package
{% aTargetBlank "https://pub.dev/packages/rename", "rename" %}.
This updates `appName` and `bundleId`.

The steps to use this are:

1. Install the `rename` command by entering
   `flutter pub global activate rename`.
1. Change the `appName` by entering
   `flutter pub global run rename --appname {new-app-name}`.
1. Change the `bundleId` by entering
   `flutter pub global run rename --bundleId {new-bundle_id}`.
   The bundle id must include at least one period.
   It must be unique across all apps in the world.
   One way to achieve this it to begin with your reverse internet domain
   such as `com.objectcomputing.`.
   Another way is to begin with your email adddress
   such as `r.mark.volkmann.gmail.`.

To target a specific platform rather than all,
add `-t {platform}` at the end of each `rename` command.
Examples include `-t android` and `-t ios`.

### Google Play

A Google Developer account is required
to deploy apps to the Google Play store.
If you do not already have a Google account, create that first
by browsing {% aTargetBlank "https://accounts.google.com/",
"accounts.google.com" %}.

To create a developer account, browse the {% aTargetBlank
"https://play.google.com/console/", "Google Play console" %}
and click one of the "Get Started" links,
either for "Yourself" or "An organization or business".
Enter all the required data and click the "Create account and pay" button.
Enter credit card information and complete the payment process
to pay a one-time 25 USD fee.

You will receive an email requesting identity verification.
Click the "Verify" button in the email.
Enter all the requested information, including
uploading a photo of an official ID such as a driver's licences.
It can take a few days to receive notification
that your identity has been verified.

Android apps can be bundled in either an
Android Package (`.apk`) or Android App Bundle (`.aab`) file.
Bundle files are smaller and are preferred.
To generate an `.apk` file, enter `flutter build apk`.
To generate an `.aab` file, enter `flutter build appbundle`.
These commands takes several minutes to complete.

### First-time Deployment

To prepare for uploading an app to the Google Play store:

- Create an upload keystore by entering the command below
  (specific to macOS or Linux) and answering questions.
  Enter a password when requested and remember it.
  For apps created by an individual as opposed to a company, consider using
  an email address for both "organizational unit" and "organization".

  ```bash
  keytool -genkey -v -keystore ~/upload-keystore.jks -keyalg RSA -keysize 2048 -validity 10000 -alias upload --storetype JKS
  ```

  This creates the file `upload-keystore.jks` in your home directory.
  Move the file into the project `android/app` directory.
  Verify that `.gitignore` contains `**.*.jks`
  so no keystore files are committed to the repository.

- In the `android` directory at the top of the project directory,
  create the file `key.properties` containing the following:

  ```text
  storePassword={keystore-password}
  keyPassword={keystore-password}
  keyAlias=upload
  storeFile=../app/upload-keystore.jks
  ```

- Add `android/key.properties` to `.gitignore`.

- Edit the `android/app/build.gradle` file.

  - Add the following before the line that begins with `android {`:

    ```text
    def keystoreProperties = new Properties()
    def keystorePropertiesFile = rootProject.file('key.properties')
    if (keystorePropertiesFile.exists()) {
      keystoreProperties.load(new FileInputStream(keystorePropertiesFile))
    }
    ```

  - Replace the `buildTypes` block with the following:

    ```text
    signingConfigs {
      release {
        keyAlias keystoreProperties['keyAlias']
        keyPassword keystoreProperties['keyPassword']
        storeFile keystoreProperties['storeFile'] ? file(keystoreProperties['storeFile']) : null
        storePassword keystoreProperties['storePassword']
      }
    }
    buildTypes {
      release {
        signingConfig signingConfigs.release
      }
    }
    ```

- Edit the `android/app/src/main/AndroidManifest.xml` file.

  - Change the `application` element `android:label` attribute value
    to the actual name of the application.

  - Add the following line after the other `uses-permission` elements
    if the app needs internet access, such as accessing Google Maps:

    ```text
    <uses-permission android:name="android.permission.INTERNET" />
    ```

- Edit the `android/app/build.gradle` file.

  - Correct the value of `applicationId` under `android` ... `defaultConfig`
  - Verify the values of `compileSdkVersion`, `minSdkVersion`.

- Edit the value of `version` in the `pubspec.yaml` file.
  This is a value like `1.0.1+2`.
  It is used to automatically update `android/local.properties`.
  The part before the `+` is a semantic version
  that is used for the value of `flutter.versionName`
  and the part after the plus is an incrementing integer
  that is used for the value of `flutter.versionCode`.

- Edit the `android/local.properties` file
  and verify the value of `flutter.buildMode`.
  It can be set to `release`, `profile`, or `debug`.
  Do not modify `flutter.versionName` or `flutter.versionCode`.
  Those are automatically updated using the value of `version` in `pubspec.yaml`.

- Update the version number displayed in the app if that is being done.
  This might appear on an "About" page.

- Build an app bundle.

  - In a terminal, cd to the top app directory.
  - Enter `flutter clean` to delete any previous build artifacts.
  - Enter `flutter build appbundle` to build a new app bundle.
  - This creates the file `build/app/outputs/bundle/release/app-release.aab`.

- Browse the {% aTargetBlank "https://play.google.com/console/",
  "Goole Play Console" %}.
- Select your developer account.

- Click the "Create app" button.
- Enter an app name as it should appear in the store.
- Choose a language such as "English (United States) - en-US".
- Specify whether it is a game or app.
- Specify whether it is free or paid.
  Once an app is published as free, it cannot be changed to paid.
- Check the "Developer Program Policies" checkbox.
- Check the "Play App Signing" checkbox.
- Check the "US export laws" checkbox.
- Click the "Create app" button in the lower-right corner.

At this point you can select the new app or an existing app.
There are two ways to use the Google Play Console.

The first way to use the Google Play Console
is to select "Dashboard" in the left nav and then
work through all the sections it contains in order.
This is the preferred approach, especially for developers
that are new to use the Google Play Console.

To see the tasks to be completed in each section,
click ther "View tasks" dropdown.
Clicking on a task to be completed will open a new page
for answering questions.
Typically after answering all of the questions for a tasks
you will need to click "Save" (near the bottom)
and then "<- Dashbard" (near the top) to return to the Dashboard
and see the next task to be completed.s
If there is no "<- Dashboard" link, click "Dashboard" in the left nav.

Once the app is deployed, the sections presented in the Dashboard will change.
For example, once an app is deployed to production,
the dashboard will no longer offer to
help with creating a test version of the app.

To share the app with testers:

- In the left nav, click "Testing".
- To only allow a limited group of internal testers (up to 100)
  to test the app, click "Internal Testing".
- To only allow invited testers (unlimited)
  to test the app, click "Closed Testing".
- To allow anyone to test the app, click "Open Testing".

- If using "Open Testing"

  - Click the "New release" button.
  - Upload a new app bundle file.
  - Browse {% aTargetBlank "https://play.google.com/store?hl=en_US&gl=US",
    "play.google.com" %}.
  - Search for the app name.
  - Email the app URL to testers and ask them to install and test it.

The second way to use the Google Play Console is to
click left-nav items in the order you would like to complete them.
This requires more knowledge of the available sections.

In order to collect money for paid apps and apps that have in-app purchases,
a payment profile must be created.

- Browse the {% aTargetBlank "https://play.google.com/console/",
  "Google Play Console" %} and elect your developer account.
- In the left nav., click "Payments profile".
- Click the "Create payments profile".
- Click your name.
- If you are operating as an individual as opposed to a business,
  uncheck the checkbox for "Use legal business info name, contact, address".
- Enter your mailing address.
- Enter a business name which can be your email address.
- Optionally enter your website URL.
- For "What do you sell" dropdown, select "Computer Software".
- For "Customer support email", enter your email address.
- For "Credit card statement name", enter a credit card name such as "Visa".
- Click the "Submit" button in the lower-right corner.
- On the "Payment profile" page, click "Manage account group".
- Click the "Create account group" button.
- Verify the "Account group name", modifying it if necessary.
- Click the "Create account group" button in the lower-right corner.
- Click "Start".
- For "Developer accounts owned by your legal entity", select "No".
- For "Developer accounts with similar brand features", select "No".
- Click the "Save" button in the lower-right corner.
- Click "Review and enroll".
- Click the "Confirm and view terms" button in the lower-right corner.
- Click the "Accept and enroll" button in the lower-right corner.

### Deploying an Update

- Edit the value of `version` in the `pubspec.yaml` file.
  This is a value like `1.0.1+2`.
  It is used to automatically update `android/local.properties`.
  The part before the `+` is a semantic version
  that is used for the value of `flutter.versionName`
  and the part after the plus is an incrementing integer
  that is used for the value of `flutter.versionCode`.
- Edit the `android/local.properties` file
  and verify the value of `flutter.buildMode`.
  It can be set to `release`, `profile`, or `debug`.
  Do not modify `flutter.versionName` or `flutter.versionCode`.
  Those are automatically updated using the value of `version` in `pubspec.yaml`.
- If the app displays its version number, perhaps on an "About" page,
  update it to match.
- Create a new app bundle by entering `flutter build appbundle`.
  This creates the file `build/app/outputs/bundle/release/app-release.aab`.
- Browse the {% aTargetBlank "https://play.google.com/console/",
  "Goole Play Console" %}.
- Select your developer account.
- Click the row of the app being updated.
- In the left-nav, click "Testing", then "Open Testing".
- Click the "New release" button.
- Click "Upload" and upload the new `.aab` file.
- Browse {% aTargetBlank "https://play.google.com/store?hl=en_US&gl=US",
  "play.google.com" %}.
- Search for the app name.
- Email the app URL to testers and ask them to install and test it.

### iOS App Store

Note that the value of `PRODUCT_BUNDLE_IDENTIFIER` cannot contain underscores.
This is specified in the file `ios/Runner.xcodeproj/project.pbxproj`.

TODO: Finish this section.

## In-App Purchases

One approach to enabling in-app purchases in a Flutter app
is to use the pub.dev package {% aTargetBlank
"https://pub.dev/packages/in_app_purchase", "in_app_purchase" %}.
Click the link after "codelab" for a detailed guide.
This requires having a backend such as Firebase.
It is a good option for apps that are using Firebase for other purposes

One way to enable in-app purchases that does not require configuring a backend
is {% aTargetBlank "https://docs.revenuecat.com/docs/flutter", "RevenueCat" %}.
This is a commercial service, but it is free for a single developer
and up to 10,000 USD in monthly tracked revenue (MTR).

### RevenueCat

RevenueCat requires store setup of in-app purchases and subscriptions.

For the Google Play Store:

1. Browse the {% aTargetBlank "https://play.google.com/console/",
   "Google Play Console" %} and elect your developer account.
1. In the left nav., click "All apps".
1. Click the row describing the app to which in-app payments will be added.
1. In the left-nav, scroll down to the "Monetize" section,
   click "Products", then click "In-app products".
1. Click the "Create product" button.
1. Enter a "Product ID", "Name", "Description", and "Default Price".
   The product ID should be unique across all of your apps,
   so it is recommended for it to begin with the app name.
   It can contain underscores and periods, but not hyphens.
   The product ID must be used as the product identifier in RevenueCat.
1. Click the "Save" button in the lower-right corner
1. Click the "Activate" button in the lower-right corner

For the iOS App Store:

TODO: Add these steps.

Describing subscriptions is similar to describing one-time in-app payments.

After configuring in-app purchases in the stores,
configure RevenueCat by following these steps:

1. Browse {% aTargetBlank "https://www.revenuecat.com", "revenuecat.com" %}.
1. Click the "Get Started ->" button.
1. If you do not already have an account,
   enter account information and click the "Sign Up" button.
1. Click "Setup your first project".
1. Enter a name for the project and click the "Create project" button.
1. Click a platform button such as "App Store" or "Play Store".
1. Enter the "App name"
1. Enter the "Google Play package" which is the bundle ID.
1. Create a "Service Account credentials JSON" file.
   This is very complicated.
   The steps are described {% aTargetBlank
   "https://docs.revenuecat.com/docs/creating-play-service-credentials",
   "here" %}.
   After all of this you will see the message in the Google Cloud Console page:
   "It can take up to 36 hours for your Play Service Credentials
   to work properly with the Google Play Developer API.
   You may see "Invalid Play Store credentials" errors and be
   unable to make purchases with RevenueCat until this happens."
1. Back on the RevenueCat "Get Started" page,
   click, "2. Configure entitlements" and click an app name.
   This page supports creating products, entitlements, and offerings.
   From the docs,
   "An entitlement represents a level of access, features,
   or content that a user is "entitled" to."
   "Attach products to entitlements. These let RevenueCat know which
   entitlements to unlock for users after they purchase a specific product.
   "Offerings are the selection of products that are offered to a user."
1. Click "Products" in the left nav and create one.
   Use the "Product ID" entered in the Google Play Console
   for the RevenueCat Product Identifier.
1. Click "Entitlements" in the left nav and create one.
   Attach a product to each entitlement.
1. Click "Offerings" in the left nav and create one.
   Create a package for each offering
   and attach products to each package.
   For example, a product can include a package for Andriod and
   a package for iOS so the user can use the feature on both platforms.
1. Go to the RevenueCat dashboard.
1. Click the "Projects" dropdown at the top
   and select the previously created project.
1. In the left nav, click "API Keys".
1. In the "Public app-specific API keys" section, click "Show key".
1. Click the copy button after the key.
1. Create the file `lib/secrets.dart` and add the following line:

   ```dart
   const revenueCatApiKey = 'the-copied-key';
   ```

1. Add `lib/secrets.dart` to `.gitignore`.

1. Add the following line in `android/app/src/main/AndroidManifest.xml`
   after the other `uses-permission` elements:

   ```xml
   <uses-permission android:name="android.vending.BILLING" />
   ```

1. Build a new app bundle by entering `flutter build appbundle`.
1. Deploy the new app bundle to the stores.
   The steps to do this are provided in the previous section.

In the Flutter app:

1. Add the `purchases_flutter` package in `pubspec.yaml`.
1. In `ios/Podfile`, uncomment the following line:

   ```text
   # platform :ios, '9.0'
   ```

1. Add a button in the app to show purchase options.
   The following code demonstrates prompting the user
   to purchase a single offer.
   Call the `offerPurchase` function to prompt the user
   to make a purchase using a dialog box.
   The functions `alert` and `confirm` are defined in
   `lib/util.dart` within the `flutter_gift_track` project.

   ```dart
    import 'dart:io' show Platform;
    import 'package:collection/collection.dart';
    import 'package:flutter/material.dart' show BuildContext;
    import 'package:flutter/services.dart' show PlatformException;
    import 'package:google_api_availability/google_api_availability.dart';
    import 'package:provider/provider.dart';
    import 'package:purchases_flutter/purchases_flutter.dart';

    import './app_state.dart';
    import './secrets.dart';
    import './util.dart' show alert, confirm;

    bool canPurchase = false;

    Future<List<Offering>> _fetchOffers() async {
      try {
        await _init();
        if (!canPurchase) return [];
        final offerings = await Purchases.getOfferings();
        final current = offerings.current;
        return current == null ? [] : [current];
        //} on PlatformException catch (e) {
      } catch (e) {
        print('error getting RevenueCat offerings: $e');
        return [];
      }
    }

    Future<void> _init() async {
      final availability = await GoogleApiAvailability.instance
          .checkGooglePlayServicesAvailability(true);
      canPurchase = availability == GooglePlayServicesAvailability.success;
      if (!canPurchase) return;

      // This causes debugging log messages to be output.
      //await Purchases.setDebugLogsEnabled(true);

      if (Platform.isAndroid) {
        await Purchases.setup(revenueCatApiKey);
      } else if (Platform.isIOS) {
        //await Purchases.setup('public_ios_sdk_key');
        throw 'iOS is not supported yet.';
      }
    }

    Future<bool> offerPurchase(BuildContext context) async {
      try {
        final offerings = await _fetchOffers();
        final offer = offerings.firstOrNull;
        final package = offer?.availablePackages.firstOrNull;
        final product = package?.product;
        //final entitlement = ?;

        var purchase = false;
        if (product == null) {
          await alert(context, 'No in-app purchase offerings were found.');
        } else {
          final question = product.description +
              ' Pay ${product.priceString} ${product.currencyCode} for this?';
          purchase = await confirm(context, question);
          if (purchase) {
            //TODO: This is not working in the Android emulator!
            final purchaserInfo = await Purchases.purchasePackage(package!);
            final entitlement = purchaserInfo.entitlements.all[product.identifier];
            if (entitlement != null && entitlement.isActive) {
              final appState = Provider.of<AppState>(context, listen: false);
              appState.paid = purchase;
            }
          }
        }
        return purchase;
      } on PlatformException catch (e) {
        var errorCode = PurchasesErrorHelper.getErrorCode(e);
        if (errorCode != PurchasesErrorCode.purchaseCancelledError) {
          //TODO: Find a better way to display errors.
          print('util.dart offerPurchase: errorCode = $errorCode');
        }
        return false;
      } catch (e) {
        //TODO: Find a better way to display errors.
        print('util.dart offerPurchase: e = $e');
        return false;
      }
    }
   ```

When testing in an Android Emulator, choose a device type
where "Play Store" is enabled.
"Pixel 4 API 30" enables this, but "Pixel 5 API 30" does not.

After the app launches on the emulator,
launch the "Google Play" app and sign in.

## Advice

- Flutter prefers project and file names that separate words
  with underscores instead of hyphens, so use those.
  `
- Create lots of custom widgets that hide the complexity of provided widgets
  and provide application-specific styling.

- Create extentions to provided Flutter classes that simplify there use.
  Examples can be found in {% aTargetBlank
  "https://github.com/mvolkmann/flutter_input/blob/main/lib/widget_extensions.dart",
  "GitHub" %}.

- Add a comma after every function argument value
  so Dart code formatting works better.

## Annoyances

- Dart needs to support type inference of enum values the way Swift does.
  For example, instead of `color: Colors.red,` I want to use `color: .red,`.

- Flutter needs to find a way to define stateful widgets without defining two classes.
  See this {% aTargetBlank "https://github.com/dart-lang/language/issues/329",
  "on-going discussion" %}.

- The VS Code Flutter extension displays a comment after the closing paren
  of all widgets. It isn't really in the code, but adds visual clutter.
  This can be disabled. See "Phantom Comments" in the "VS Code" section.

- The `Row`, `Column`, and `Flex` widgets need to take a `spacing` parameter
  like the `Wrap` widget does so it isn't necessary to do tedious things
  like adding a `SizedBox` between each child to leave space between them.
  My `widget_extensions.dart` file adds a `gap` method to `Row` and `Column`.
  It also adds many methods to `Widget` and `List<Widget>`.

- It is often convenient to use the `Iterable` methods `map` and `filter`
  to create a new `Iterable`.
  But many Flutter properties and methods arguments
  require a `List` instead of an `Iterable`.
  This requires calling `.toList()` on the `Iterable` to create a `List`.
  I wish Flutter had more support for `Iterable` so this wouldn't be necessary.

- The default linting rule settings related to use of the `const` keyword
  are incredibly annoying!
  I wish Dart could figure out what should be `const`
  on its own so we wouldn't need to litter our code with that keyword.

- The need to pass `context` and `key` in so many places is annoying.

- Sometimes after a hot reload the simulator will
  display an error message in yellow text on a red background.
  There may not really be an error in the code.
  Restarting the app can make the problem go away.

- The error screen shown in the simulator when there is a runtime error
  displays the message in yellow on a red background.
  This combination is somewhat hard on the eyes.

- The error screen often does not provide
  the filename and line number of the error.
  It is displayed, in the terminal where the app is running,
  but it is buried in a long stack trace
  which makes it difficult to find.
