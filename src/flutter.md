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

To run a Flutter app from VS Code, select a simulated or real device to use
from a menu on the right side of the VS Code footer.
The select Run ... Start Debugging (F5)
or Run ... Run Without Debugging (ctrl-fn-F5).
The first time this is run for a given application
it takes around a minute to start.

## Debugging

When a Flutter app is run from a terminal,
pressing "p" displays blue outlines of all widgets.
This is useful to understand the position and size of each widget.
Press "p" again to toggle this off.

## Running on Devices

To run a Flutter app on a connected iPhone:

- Attach the phone to the computer with a USB cable.
- Unlock the phone and "trust it.
- From a terminal running bash, cd to the top project directory.
- Enter `open ios/Runner.xcworkspace` to launch Xcode.
- Click "Runner" at the top of the Navigator.

- In Xcode

  - Select "iPhone" from the device drop-down in the header.
  - Click "Runner" at the top of the Navigator.
  - Click the "Signing & Capabilities" tab.
  - Select your team from the Team drop-down.
    This can be your Apple ID.
    After entering this the first time, enter your Apple ID password.
  - Enter a unique "Bundle Identifier", perhaps containing
    your email address, a hyphen, and the project name.
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

To run a Flutter app on an iPhone wirelessly:

- The iPhone must be on the same WiFi network as the computer.
- In Xcode, select Window ... Devices and Simulators.
- In the dialog that is opened, select "iPhone" in the left nav.
- Check the "Connect vis network" checkbox.
- In the Finder, eject the iPhone.
- Disconnect the iPhone from the computer.
- Run the Flutter app using one of the options above.

## VS Code

Install the Flutter extension.
This uses the {% aTargetBlank
"https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server",
"Dart Analysis Server" %}, as does Android Studio,
to power many IDE operations.

This extension provides many things including:

- great auto-complete support

- snippets

  - `stless` adds template code for creating a stateless widget
  - `stful` adds template code for creating a stateful widget

  The code added by these requires importing a library that defines
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

All widget constructors must take an optional parameter
named "key" that has the type "Key".
This uniquely identifies a widget instance and is
important when widgets will be added, removed, or reordered.
Often no key is necessary.
One way to get a key value with the constructor call `UniqueKey()`.

### Child Widgets

Widgets that accept other widgets as arguments typically have
a parameter named `child` with the type `Widget` for one
or `children` with a type of `List<Widget>` for multiple.
TODO: Why isn't the type of `children` `Iterable<Widget>`?

Some widgets take a single widget in a `child` parameter and apply styling.
The Flutter docs refer to these as "single-child layout widgets".
Examples include `Center`, `Container`, `Expanded`,
`Flexible`, `Padding`, and `SizedBox`.

Other widgets take multiple widgets in a `children` parameter
and lay them out in a specific way.
The Flutter docs refer to these as "multi-child layout widgets".
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
  const SomeName({ Key? key }) : super(key: key);

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

Stateful widgets are defined by a pair of classes.
The first class extends `StatefulWidget`,
defines final properties, defines a constructor, and
overrides the `createState` method to create an instance of the second class.
The second class can be private and
be defined in the same source file as the first.
It extends `State`, defines state properties,
and overrides the `build` method.
Storing the state in a separate class
allows the widget class to remain immutable.
Stateful widgets render initially and again each time their state changes.

New stateful widgets begin with the following code
which defines a pair of related classes:

```dart
class SomeName extends StatefulWidget {
  const SomeName ({ Key? key }) : super(key: key);

  @override
  _SomeNameState createState() => _SomeNameState();
}

class _SomeNameState extends State<SomeName> {
  @override
  Widget build(BuildContext context) {
    return Container(

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
  var _count = 0;

  static const textStyle = TextStyle(fontSize: 36);

  @override
  void initState() {
    // Note the use of "widget." to refer to
    // properties in the associated stateful widget.
    _count = widget.initialValue;
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Row(children: [
      TextButton(
        child: const Text('-', style: textStyle),
        // The button is disabled when onPressed is null.
        onPressed: _count <= 0 ? null : () => setState(() => _count -= 1),
      ),
      Text('$_count', style: textStyle),
      TextButton(
        child: const Text('+', style: textStyle),
        onPressed: () => setState(() => _count += 1),
      ),
      ElevatedButton(
        child: const Text('Reset'),
        onPressed: () => setState(() => _count = 0),
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

### Material Structure Widgets

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

| Parameter Name | Description                                                                        |
| -------------- | ---------------------------------------------------------------------------------- |
| `darkTheme`    | `ThemeData` to use when running in dark mode                                       |
| `home`         | `Widget` to render for the default route ('/')                                     |
| `initialRoute` | `String` name of first route to render when using `Navigator`                      |
| `routes`       | `Map` of route names to `WidgetBuilder` instances                                  |
| `theme`        | `ThemeData` to use when running in light mode                                      |
| `title`        | single-line `String` that describes the app; only rendered in Android task manager |

#### Scaffold Widget

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/Scaffold-class.html", "Scaffold" %}
widget "implements the basic material design layout structure."
The `MaterialApp` constructor `home` argument is typically set to a
custom widget whose `build` method returns an instance of the `Scaffold` class.

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
"official docs" %}.
The first row is referred to as the "toolbar" in documentation
and includes the `leading`, `title`, and `actions` widgets.

The `AppBar` constructor takes the following named parameters and more:

| Parameter Name     | Description                                                  |
| ------------------ | ------------------------------------------------------------ |
| `actions`          | `List<Widget>` displayed on right side of top row            |
| `backgroundColor`  | `Color` of background                                        |
| `bottom`           | `PreferredSizeWidget` displayed on bottom row (ex. `TabBar`) |
| `centerTitle`      | `bool` defaults to `false`, but typically want `true`        |
| `flexibleSpace`    | `Widget` stacked behind the toolbar and the bottom widget    |
| `foregroundColor`  | `Color` of foreground                                        |
| `leading`          | `Widget` displayed on left side of top row                   |
| `leadingWidth`     | `double` width of `leading` `Widget`                         |
| `title`            | `Widget` displayed in center of top row                      |
| `titleSpacing`     | `double` space on left and right side of `title` `Widget`    |
| `titleTextStyle`   | `TextStyle` of `title` `Widget`                              |
| `toolbarHeight`    | `double` height of the toolbar (first row)                   |
| `toolbarTextStyle` | `TextStyle` of the toolbar widgets                           |

See the sample app in the GitHub repo {% aTargetBlank
"https://github.com/mvolkmann/flutter_appbar", "flutter_appbar" %}.

### Layout Widgets

The layout widgets are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/layout",
"Layout widgets" %}.

Some layout widgets accept a single `child`
and others accept multiple `children`.

#### Single-child Layout Widgets

The most commonly used widgets for laying out a single child widget
are described below:

| Widget                                                                                                               | Description                                                                                                                   |
| -------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Align-class.html", "Align" %}                               | specifies where its child should be positioned within its parent (ex. `Alignment.bottomRight`)                                |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AnimatedContainer-class.html", "AnimatedContainer" %}       | like `Container`, but "gradually changes its values over a period of time"; provides implicit animations                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/AspectRatio-class.html", "AspectRatio" %}                   | sizes its child to a specific aspect ratio                                                                                    |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Center-class.html", "Center" %}                             | centers its child horizontally and vertically in the available space                                                          |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/ConstrainedBox-class.html", "ConstrainedBox" %}             | "imposes additional constraints on its child" which include min and max width and height                                      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Container-class.html", "Container" %}                       | surrounds its child with optional `padding`, `decoration` (ex. `BoxDecoration` with optional border and shadow), and `margin` |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Expanded-class.html", "Expanded" %}                         | "expands a child of a Row, Column, or Flex so that the child fills the available space"                                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/FittedBox-class.html", "FittedBox" %}                       | "scales and positions its child within itself" including clipping                                                             |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Flexible-class.html", "Flexible" %}                         | "controls how a child of a Row, Column, or Flex flexes"; similar to CSS flex layout                                           |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/FractionallySizedBox-class.html", "FractionallySizedBox" %} | "sizes its child to a fraction of the total available space"; can specify `widthFactor`, `heightFactor`, and `alignment`      |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/LayoutBuilder-class.html", "LayoutBuilder" %}               | provides min/max width/height constraints that can be used to decide how/what a child component should render                 |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/LimitedBox-class.html", "LimitedBox" %}                     | "box that limits its size only when unconstrained"; useful for wrapping unconstrained children of a `ListView`                |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Padding-class.html", "Padding" %}                           | "insets its child by the given padding"                                                                                       |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/SizedBox-class.html", "SizedBox" %}                         | "box with a specified size" for taking up space; "if given a child, forces it to have a specific width and/or height"         |
| {% aTargetBlank "https://api.flutter.dev/flutter/widgets/Transform-class.html", "Transform" %}                       | transforms its child by translating, rotating, and scaling it                                                                 |

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
The most frequently used arguments are described below:

| Argument      | Description                                                                                                |
| ------------- | ---------------------------------------------------------------------------------------------------------- |
| `alignment`   | `Alignment` that specifies the alignment of the `child` inside                                             |
| `color`       | `Color` of the background (conflicts with `decoration`)                                                    |
| `constraints` | `BoxConstraints` that constrains the minimum and maximum width and height                                  |
| `decoration`  | `BoxDecoration` that can specify a `color`, `border`, `borderRadius`, `boxShadow`, `gradient`, and `shape` |
| `height`      | `double` height of container (conflicts with `constraints`)                                                |
| `padding`     | `EdgeInsets` that specifies the padding to apply inside                                                    |
| `width`       | `double` width of container (conflicts with `constraints`)                                                 |

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

The `Flexible` widget behaves nearly identically to the `Expanded` widget.
But a `Flexible` widget will take on
the size of its child if the child has a fixed size.
Otherwise it will choose its size just like `Expanded`.

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
It requires adding the "vector_math" library to `pubspec.yaml`.

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
widget displays a scrollable list of widgets.
The list is vertical by default, but can be changed to horizontal.

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
<img>

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
All of these `returns a `Future` that completes
when the user does something to dismiss the dialog.
These functions are described in the following table:

| Function              | Description                                                                                |
| --------------------- | ------------------------------------------------------------------------------------------ |
| `showDialog`          | displays any dialog returned by the function specified in its `builder` argument           |
| `showCupertinoDialog` | like `showDialog`, but themed for iOS                                                      |
| `showGeneralDialog`   | like `showDialog` but supports customizing the transition used to display the dialog       |
| `showDatePicker`      | provides an easier way to display a `DatePickerDialog` than using the above functions      |
| `showDateRangePicker` | provides an easier way to display a `DateRangePickerDialog` than using the above functions |

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
    // return the previous route, call Navigator.of(context).pop().
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

### Material Input Widgets

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
An earlier widget named `FlatButton` has been deprecated.

Basic usage of all of these widgets is demonstrated in the Flutter project at
{% aTargetBlank "https://github.com/mvolkmann/flutter_input",
"flutter_input" %}.

#### TextField and TextFormField

The {% aTargetBlank
"https://api.flutter.dev/flutter/material/TextField-class.html",
"TextField" %} and {% aTargetBlank
"https://api.flutter.dev/flutter/material/TextFormField-class.html",
"TextFormField" %} widget constructors take many optional arguments.
The highlights are described in the following table:

| Argument       | Description                                                           |
| -------------- | --------------------------------------------------------------------- |
| `autoCorrect`  | `bool` indicating of the value should be auto-corrected               |
| `controller`   | `TextEditingController` (described below)                             |
| `decoration`   | `InputDecoration` that specifies styling details                      |
| `keyboardType` | `TextInputType` that requests a certain kind of on-screen keyboard    |
| `maxLength`    | `int` maximum length defaulting to no limit                           |
| `maxLines`     | `int` defaulting to `1`                                               |
| `obscureText`  | `bool` indicating of the value should be obscured (ex. for passwords) |
| `onChanged`    | function called with new value when the user changes it               |
| `readOnly`     | `bool` indicating if the value cannot currently be modified           |
| `style`        | `TextStyle`                                                           |

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

Instances of the `TextEditingController` class can be passed to the
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

### Other Material Classes

| Class              | Description                                               |
| ------------------ | --------------------------------------------------------- |
| `DefaultTextStyle` | style applied to text that doesn't have an explicit style |

TODO: Add more here?

### Cupertino Widgets

These widgets use iOS styling rather than Material Design.
They are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/cupertino",
"Cupertino (iOS-style) widgets" %}.

To use Cupertino widgets in a Dart source file, add the following import:

```dart
import 'package:flutter/cupertino.dart';
```

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

#### CupertinoDatePicker

The {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoDatePicker-class.html",
"CupertinoDatePicker" %} widget is an alternative
to the Material UI {% aTargetBlank
"https://api.flutter.dev/flutter/material/CalendarDatePicker-class.html",
"CalendarDatePicker" %} widget.
It allow the user to select a date, time, or both.

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

#### CupertinoPicker

The {% aTargetBlank
"https://api.flutter.dev/flutter/cupertino/CupertinoPicker-class.html",
"CupertinoPicker" %} widget is an alternative
to the Material UI {% aTargetBlank
"https://api.flutter.dev/flutter/material/DropdownButton-class.html",
"DropdownButton" %} widget.
It allow the user to select an option from a wheel-like display.

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
  // This makes it clear the vertical space that is occupied.
  decoration: BoxDecoration(
    border: Border.all(color: Colors.red),
  ),
  height: 150, // height of wheel; affects # of visible items
);
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

The `Form` widget supports form validation based on the form fields inside it.
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
it is recommended to choose a state management library
provided by the community.
Three popular packages are provider, GetX, and Riverpod.
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

### provider Library

The {% aTargetBlank "https://pub.dev/packages/provider", "provider" %} library
is is the state management approach recommended by the Flutter team.
The {% aTargetBlank
"https://docs.flutter.dev/development/data-and-backend/state-mgmt/simple",
"official docs" %} say
"If you are new to Flutter and you don’t have a strong reason
to choose another approach (Redux, Rx, hooks, etc.),
this is probably the approach you should start with."
In June 2019 Chris Sells, the Flutter Project Manager, said
"Provider is the recommended way to do State Management for apps of all sizes."

The provider library was created by Remi Rousselet.
It is a wrapper around the {% aTargetBlank
"https://api.flutter.dev/flutter/widgets/InheritedWidget-class.html",
"InheritedWidget" %} which is
a relatively complicated way to share mutable state.
`InheriteWidget` is explained well in this {% aTargetBlank
"https://www.youtube.com/watch?v=utrvu-eow6U", "YouTube video" %}.

The steps to use the provider library are:

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

   return MaterialApp(
     ...
     home: ChangeNotifierProvider(
       create: (context) => CountState(),
       child: HomePage(title: 'My App'),
     ),
   );

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
The same app will be shown later using other state management libraries.
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
than other Flutter state management libraries.
One critisism is that it is a very large library
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

### Riverpod Library

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

There are three libraries for using Riverpod.

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
        //TODO: What can't this be used instead of the above code?
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

The app class, typically defined in `lib/main.dart`, can register named routes
with the `routes` argument to the `MaterialApp` widget.
Assigning names to routes makes it easier to
refer to them from multiple widgets.
It is recommended to define route names as static constants in each page class,
(as shown above) except the page class for the home route.
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

To programmatically return to the previous page call `Navigator.pop(context)`.

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
  State<BottomNavigation> createState() => BottomNavigationState();
}

//TODO: Why can't this class be made private with a leading underscore?
class BottomNavigationState extends State<BottomNavigation> {
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

## Bottom Sheets

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

## SnackBars

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

Another way to get fonts is use the "google_fonts" package.
This provides over 1000 fonts.
Add this as a dependency in `pubspec.yaml`.
Instead of downloading each of the fonts, they are
fetched via HTTP at runtime and cached in the app's file system.
For instructions on using this, see {% aTargetBlank
"https://pub.dev/documentation/google_fonts/latest/",
"google_fonts documentation" %}.
TODO: I could not get this to work!

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

## Tests

Three kinds of tests can be written for Flutter applications.
These include unit tests, widget tests, and integration tests.

Flutter projects created with the `flutter create` command
already have a dev dependency on the `flutter_test` package
which is built on the `test` package.
Do not add a dependency on {% aTargetBlank
"https://pub.dev/packages/test", "test" %} in`pubspec.yaml`.
This will likely cause a "version solving failed" error
when the tests are run.

The {% aTargetBlank
"https://api.flutter.dev/flutter/flutter_test/flutter_test-library.html",
"flutter_test" %} library defines many
classes, constants, and functions used to implement tests.

Commonly used test constants are described below:

TODO: Finish this table.

| Constants         | Description                                                        |
| ----------------- | ------------------------------------------------------------------ |
| `findsNothing`    | asserts that the widget tree contains no matching widgets          |
| `findsOneWidget`  | asserts that the widget tree contains a single matching widget     |
| `findsWidget`     | asserts that the widget tree contains at least one matching widget |
| `isFalse`         |                                                                    |
| `isNan`           |                                                                    |
| `isNegative`      |                                                                    |
| `isNonNegative`   |                                                                    |
| `isNonPositive`   |                                                                    |
| `isNonZero`       |                                                                    |
| `isNotEmpty`      |                                                                    |
| `isNotNan`        |                                                                    |
| `isNotNull`       |                                                                    |
| `isNull`          |                                                                    |
| `isPositive`      |                                                                    |
| `isTrue`          |                                                                    |
| `isZero`          |                                                                    |
| `returnsNormally` |                                                                    |
| `throwsException` |                                                                    |

Commonly used test functions are described below:

TODO: Finish this table.

| Function               | Description                                                                                 |
| ---------------------- | ------------------------------------------------------------------------------------------- |
| `addTearDown`          | registers a function to call after the current test completes                               |
| `allOf`                | returns a matcher that matches if all the argument matchers (limit of 7) match              |
| `anyElement`           | returns a matcher that matches if any element in an `Iterable` matches a value or `Matcher` |
| `anyOf`                |                                                                                             |
| `closeTo`              |                                                                                             |
| `compareLists`         |                                                                                             |
| `contains`             |                                                                                             |
| `containsAll`          |                                                                                             |
| `containsAllInORder`   |                                                                                             |
| `containsPair`         |                                                                                             |
| `endsWith`             |                                                                                             |
| `equals`               |                                                                                             |
| `equalsIgnoringCase`   |                                                                                             |
| `everyElement`         |                                                                                             |
| `expect`               | THE MOST COMMONLY USED FUNCTION.                                                            |
| `expectAsync{n}`       |                                                                                             |
| `fail`                 |                                                                                             |
| `findsNWidgets`        |                                                                                             |
| `greaterThan`          |                                                                                             |
| `greaterThanOrEqualTo` |                                                                                             |
| `group`                | defines a group (or suite) of tests                                                         |
| `hasLength`            |                                                                                             |
| `inClosedOpenRange`    |                                                                                             |
| `inExclusiveRange`     |                                                                                             |
| `inInclusiveRange`     |                                                                                             |
| `inOpenClosedRange`    |                                                                                             |
| `isA`                  |                                                                                             |
| `isIn`                 |                                                                                             |
| `isInstanceOf`         |                                                                                             |
| `isNot`                |                                                                                             |
| `isSameColorAs`        |                                                                                             |
| `lessThan`             |                                                                                             |
| `lessThanOrEqualTo`    |                                                                                             |
| `markTestSkipped`      |                                                                                             |
| `matches`              |                                                                                             |
| `moreOrLessEquals`     |                                                                                             |
| `same`                 |                                                                                             |
| `setUp`                | registers a function to call before each test                                               |
| `setUpAll`             | registers a function to call once before the first test                                     |
| `startsWith`           |                                                                                             |
| `tearDown`             | registers a function to call after each test                                                |
| `tearDownAll`          | registers a function to call once after the last test                                       |
| `test`                 | defines a single unit test                                                                  |
| `testWidgets`          | defines a single widget test                                                                |
| `within`               |                                                                                             |

### Unit Tests

Flutter unit tests are for testing logic, not widgets.

To implement a unit test for classes and functions defined in
the file `lib/sample.dart`, create the file `test/sample_test.dart`.

In VS Code, right-click a source file in the `lib` directory
to be tested in the Navigator and select "Go to Tests".
If a corresponding test file exists in the `test` directory,
it will be opened. If not, VS Code will offer to create it.

The provided test code contains a single call to `testWidgets`,
passing it a function that takes a `WidgetTester` object.
For non-widget tests, change this to a `test` function
that is passed a function that takes no arguments.

Each test file defines a `main` function.
This should contain calls to the global `test` function.
The `test` function takes a `String` description and a
no-arg function that contains calls to the global `assert` function.
The `expect` function takes an actual value and an expected value.

To create test suites that group tests, call the global `group` function,
passing it a `String` description and
a no-arg function that contains several calls to the `test` function.

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

New Flutter projects ship with a file named `widget_test.dart`.
Use this is as an example when writing widget tests.

In contains a `main` function that makes
a single call to the `testWidgets` function.
This is passed a description `String` and
a function that is passed a `WidgetTester` object.
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
| `byType`      | pass a widget `Type`; only works when a single widget matches             |
| `text`        | pass a `String` to find a widget containing it                            |

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
var myButton = find.byType(ElevatedButton);
```

Once the button is found, it can be tapped with the following:

```dart
await tester.tap(myButton);
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

The most commonly used `WidgetTester` properties are described below:

TODO: Finish this table.

| Property          | Description                               |
| ----------------- | ----------------------------------------- |
| `testDescription` | `String` name of the test that is running |
| ``                |                                           |
| ``                |                                           |
| ``                |                                           |
| ``                |                                           |

The most commonly used `WidgetTester` methods are described below:

TODO: Finish this table.

| Method                      | Description                                                              |
| --------------------------- | ------------------------------------------------------------------------ |
| `drag(...)`                 | drags a given widget by a specified offset                               |
| `enterText(Finder, String)` | moves focus to a text input widget and replaces its contents             |
| `fling(...)`                | executes a fling gesture from the center of a widget over some offset    |
| `flingFrom(...)`            | executes a fling gesture from a starting location over some offset       |
| `getRect(Finder)`           | gets the `Rect` of a widget                                              |
| `getSize(Finder)`           | gets the `Size` of a widget                                              |
| `longPressj(...)`           | executes a long press at the center of a widget                          |
| `longPressAt(...)`          | executes a long press at a given location                                |
| `pageBack()`                | dismisses the current page                                               |
| `printToConsole(String)`    | prints a `String` to the console; can also use the Dart `print` function |
| `pump`                      | triggers a new "frame" (rebuilding the UI) after an optional duration`   |
| `pumpAndSettle`             |                                                                          |
| `pumpBenchmark`             |                                                                          |
| `pumpFrames`                |                                                                          |
| `pumpWidget`                |                                                                          |
| `restartAndRestore`         |                                                                          |
| `restoreFrom`               |                                                                          |
| `runAsync`                  |                                                                          |
| `sendEventToBinding`        |                                                                          |
| `showKeyboard`              |                                                                          |
| `takeException`             |                                                                          |
| ``                          |                                                                          |
| ``                          |                                                                          |
| ``                          |                                                                          |
| ``                          |                                                                          |
| ``                          |                                                                          |

The following code provides widget tests for the `PageView` demo app
presented earlier.

```dart
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:flutter_pageview/main.dart';

void main() {
  testWidgets('Page1', (WidgetTester tester) async {
    // Build the app and trigger the first frame.
    await tester.pumpWidget(MyApp());

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

    await tester.pumpWidget(MyApp());

    var backBtn = find.byKey(ValueKey('backBtn'));
    var forwardBtn = find.byKey(ValueKey('forwardBtn'));

    // Verify that we can change pages with the forward and back buttons.
    expect(find.text('This is page #1.'), findsOneWidget);
    await changePage(forwardBtn, 'This is page #2.');
    await changePage(forwardBtn, 'This is page #3.');
    await changePage(backBtn, 'This is page #2.');
    await changePage(backBtn, 'This is page #1.');
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

1. Run the test by entering `flutter test integration_test/app_test.dart`.
   You will prompted to select where the test should be run.
   For example, if an iPhone is attached to the computer with a USB cable
   and the iOS simulator is running, you could see the following:

   ```text
   [1]: iPhone (00008020-001A7D141A9A002E)
   [2]: iPhone 13 (46325045-B5FE-4313-BBD5-10A7D1178B50)
   [3]: Chrome (chrome)
   ```

   Press 1, 2, or 3 to select one.

   Alternatively to run the test on a device,
   enter `flutter devices` to see a list of available devices.
   Copy the id of a device where the test should be run and enter
   `flutter test integration_test/app_test.dart -d {device-id}`.

Even a basic integration test will take 1-2 minutes to start.

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
IDEs generally recognize the changes and
automatically install the new dependencies for you.
When using an editor that doesn't do this,
enter `flutter pub get` to download and install them.

In each of the commands in this section,
the `flutter` command can be replaced by the `dart` command.

## Advice

- Flutter prefers project and file names that separate words
  with underscores instead of hyphens, so use those.

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

- The VS Code Flutter extension displays a comment after the closing paren
  of all widgets. It isn't really in the code, but adds visual clutter.
  I haven't found a way to disable it yet.

- The `Row`, `Column`, and `Flex` widgets need to take a `spacing` parameter
  like the `Wrap` widget does so it isn't necessary to do tedious things
  like adding a `SizedBox` between each child to leave space between them.

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
