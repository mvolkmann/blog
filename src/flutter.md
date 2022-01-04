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

- To start an Android emulator from VS Code:

  - Select "Flutter: Launch Emulator" from the Command Palette.
  - Select a device type.

- To start an Android emulator from outside VS Code:

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
  However, it will not recognized new classes and functions until
  "Developer: Reload Window" is selected from the Command Palette.

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

All widgets have a `build` method that is passed a `BuildContext` object.
This can be used to traverse up and down the widget tree.
One use is for finding ancestor styles
so they can be applied to the current widget.

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

| Widget              | Description                                                                       |
| ------------------- | --------------------------------------------------------------------------------- |
| `AppBar`            | appears at top of app; contains other widgets such as `TabBar`                    |
| `BottomAppBar`      | appears at bottom of app; contains buttons used to switch between top-level views |
| `Drawer`            | panel that slides in from left (by default) and can contain navigation links      |
| `MaterialApp`       | top of app; wraps many other widgets                                              |
| `NavigationBar`     |                                                                                   |
| `NavigationToolbar` |                                                                                   |
| `Router`            |                                                                                   |
| `Scaffold`          | provides app structure; can show `Drawer`, `Snackbar`, and bottom sheets          |
| `TabBar`            | horizontal row of tabs                                                            |
| `TabBarView`        | page that corresponds to a `TabBar` tab                                           |
| `TabPageSelector`   | renders dots that indicate current carousel item; click to switch                 |

#### MaterialApp Widget

The `MaterialApp` widget is topmost widget
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

The `Scaffold` widget "implements the basic material design layout structure."
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

The `AppBar` widget holds several other widgets referred to as
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

### Material Layout Widgets

The layout widgets are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/layout",
"Layout widgets" %}.

Some layout widgets accept a single `child`
and others accept multiple `children`.

#### Single-child Layout Widgets

| Widget                    | Description                                                                                                                   |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `Align`                   |                                                                                                                               |
| `AspectRatio`             | sizes its child to a specific aspect ratio                                                                                    |
| `Baseline`                |                                                                                                                               |
| `Center`                  | centers its child horizontally and vertically in the available space                                                          |
| `ConstrainedBox`          |                                                                                                                               |
| `Container`               | surrounds its child with optional `padding`, `decoration` (ex. `BoxDecoration` with optional border and shadow), and `margin` |
| `CustomSingleChildLayout` |                                                                                                                               |
| `Expanded`                | "expands a child of a Row, Column, or Flex so that the child fills the available space"                                       |
| `FittedBox`               |                                                                                                                               |
| `Flexible`                | "controls how a child of a Row, Column, or Flex flexes"; similar to CSS flex layout                                           |
| `FractionallySizedBox`    |                                                                                                                               |
| `IntrinsicHeight`         |                                                                                                                               |
| `IntrinsicWidth`          |                                                                                                                               |
| `LimitedBox`              |                                                                                                                               |
| `Offstage`                |                                                                                                                               |
| `OverflowBox`             |                                                                                                                               |
| `Padding`                 | "insets its child by the given padding"                                                                                       |
| `SizedBox`                | "box with a specified size" for taking up space; "if given a child, forces it to have a specific width and/or height"         |
| `SizedOverflowBox`        |                                                                                                                               |
| `Transform`               | transforms its child by translating, rotating, and scaling it                                                                 |

The `Expanded` class extends from the `Flexible` class.
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

The `Transform` widget applies transformations to its `child` widget.

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

The `Container` widget can add many kinds of "decorations" including
colors, gradients, borders, shadows, images,
and shapes (only rectangles and circles).

#### Multi-child Layout Widgets

TODO: Finish added descriptions in this table.

| Widget                   | Description                                                                            |
| ------------------------ | -------------------------------------------------------------------------------------- |
| `Column`                 | arranges widgets vertically (like `HStack` in SwiftUI)                                 |
| `CustomMultiChildLayout` |                                                                                        |
| `Flow`                   | advanced; "optimized for repositioning children using transformation matrices"         |
| `GridView`               | "scrollable, 2D array of widgets"                                                      |
| `IndexedStack`           |                                                                                        |
| `LayoutBuilder`          |                                                                                        |
| `ListBody`               |                                                                                        |
| `ListView`               | arranges widgets vertically like `Column`, but scrolls when needed                     |
| `Row`                    | arranges widgets horizontally (like `HStack` in SwiftUI)                               |
| `Stack`                  | stacks widgets on top of each other (like `ZStack` in SwiftUI)                         |
| `Table`                  | "uses the table layout algorithm for its children"                                     |
| `Wrap`                   | arranges widgets horizontally or vertically and wraps to a new row or column as needed |

##### Column

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
`start`, `center` (default), `end`,
`stretch`, and `baseline` (aligns text baselines).

Values of the `MainAxisAlignment` enum include
`start` (default), `center`, `end`,
`spaceAround`, `spaceBetween`, and `spaceEvenly`.

Values of the `MainSize` enum include
`max` (uses all available space; default) and
`min` (uses on what is needed to fit children).

Values of the `TextBaseline` enum include `alphabetic` and `ideographic`.

Values of the `TextDirection` enum include
`ltr` (left to right) and `rtl` (right to left).

Values of the `VerticalDirection` enum include `down` (default) and `up`.

There is no parameter that controls the space between the children.
One way to add space between them is to insert `SizedBox` widgets.
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

##### ListView

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

##### Row

This renders a horizontal list of child widgets.
It is similar to a SwiftUI `HStack`.
The "main" axis is horizontal and the "cross" axis is vertical.

This constructor for widget takes the
same named parameters as the `Column` widget.

##### Wrap

This renders widgets in rows or columns.
It is similar to the `Row` and `Column` widgets,
but differs in that it wraps children to multiple rows or columns
when they do not all fit in a single row or column.

This constructor for widget takes the following named parameters,
all of which are optional.

| Parameter            | Type                      |
| -------------------- | ------------------------- |
| `alignment`          | `WrapAlignment` enum      |
| `children`           | `List<Widget>`            |
| `clipBehavior`       | `Clip` enum               |
| `crossAxisAlignment` | `WrapCrossAlignment` enum |
| `direction`          | `Axis`                    |
| `runAlignment`       | `WrapAlignment` enum      |
| `runSpacing`         | `double`; defaults to 0.0 |
| `spacing`            | `double`; defaults to 0.0 |
| `textDirection`      | `TextDirection`           |
| `verticalDirection`  | `VerticalDirection` enum  |

The `children` are divided into "runs" that fit in a single row or column.
The `runAlignment` and `runSpacing` parameters apply to entire runs.
The `alignment` and `spacing` parameters apply to the children of each "run".

Values of the `Axis` enum include `horizontal` (default) and `vertical`.

Values of the `Clip` enum include
`antiAlias`, `antiAliasWithSaveLayer`, `hardEdge`, and `none` (default).

Values of the `TextDirection` and `VerticalDirection` enums were
described with the `Column` widget above.

Values of the `WrapAlignment` enum include
`start` (default), `center`, `end`,
`spaceAround`, `spaceBetween`, and `spaceEvenly`.

Values of the `WrapCrossAlignment` enum include

### Material Display Widgets

| Widget              | Description                                                                         |
| ------------------- | ----------------------------------------------------------------------------------- |
| `Banner`            |                                                                                     |
| `Canvas`            |                                                                                     |
| `CircleAvatar`      |                                                                                     |
| `Divider`           | horizontal, thin line                                                               |
| `ErrorWidget`       |                                                                                     |
| `ExpandIcon`        |                                                                                     |
| `FileImage`         |                                                                                     |
| `FlutterLogo`       | renders the Flutter logo                                                            |
| `Icon`              | renders an icon                                                                     |
| `Image`             | renders an image                                                                    |
| `ImageIcon`         |                                                                                     |
| `NetworkImage`      |                                                                                     |
| `Placeholder`       | renders a rectangle that represents where other widgets will be added in the future |
| `ProgressIndicator` |                                                                                     |
| `RichText`          | renders runs of text that each use different styles; uses `TextSpan` objects        |
| `Snackbar`          |                                                                                     |
| `Text`              | renders a run of text with a single style                                           |
| `Tooltip`           |                                                                                     |
| `VerticalDivider`   | vertical, thin line                                                                 |

The primary widgets for rendering text are `Text` and `RichText`.
Both automatically wrap their text if needed by default,
but this can be changed by setting their `overflow` argument to
a value from the `TextOverflow` enum.
These include `clip`, `ellipsis`, `fade`, and `visible`.

`Text` renders text with a single style
including color, font, font size, and weight.
It takes a `style` argument whose value can be a `TextStyle` object
or a property from the object obtain from `Theme.of(context).textTheme`
(ex. `headline6`).

`RichText` renders runs of text that can each have a different style.
It takes a `text` argument whose value is a `TextSpan` widget
with a `children` argument that is typically a `List` of `TextSpan` widgets.
The top `TextSpan` widget can specify the
default `style` for the child `TextSpan` widgets.
The child `TextSpan` widgets can override that style.

### Material Dialog Widgets

| Widget                  | Description |
| ----------------------- | ----------- |
| `AboutDialog`           |             |
| `AlertDialog`           |             |
| `DatePickerDialog`      |             |
| `DateRangePickerDialog` |             |
| `Dialog`                |             |
| `SimpleDialog`          |             |
| `TimePickerDialog`      |             |

### Material Input Widgets

| Widget                 | Description                                                                                                                        |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `Autocomplete`         | text input for selecting from a list of options where only options that match the entered text are displayed                       |
| `BackButton`           | "<" button that defaults to calling `Navigator.maybePop` when pressed                                                              |
| `CloseButton`          | "X" button that defaults to calling `Navigator.maybePop` when pressed                                                              |
| `CalendarDatePicker`   | inline calendar date picker                                                                                                        |
| `Checkbox`             | checkbox for toggling a `bool` value                                                                                               |
| `DropdownButton`       | dropdown menu with `DropdownMenuItem` children similar to the HTML `select` element with `option` children                         |
| `EditableText`         | from the `TextField` docs, "EditableText ... is the raw text editing control at the heart of a TextField ... rarely used directly" |
| `ElevatedButton`       | button containing any widget with a background color whose elevation increases when pressed                                        |
| `FloatingActionButton` | circular (typically) button "that hovers over other content to promote a primary action"                                           |
| `Form`                 | container for grouping form fields; see `autovalidateMode` property and `createState` method                                       |
| `IconButton`           | button containing an `Icon`                                                                                                        |
| `OutlinedButton`       | a `TextButton` with an outlined border                                                                                             |
| `PopupMenuButton`      | similar to `DropdownButton`, but displays an ellipsis instead an upside down caret                                                 |
| `Radio`                | radio button for selecting between mutually exclusive options                                                                      |
| `RangeSlider`          | slider for selecting the minimum and maximum values from a range of `double` values                                                |
| `ReorderableList`      | scrollable container where child widgets can be dragged to reorder them                                                            |
| `Slider`               | slider for selecting a double value from a given range of `double` values                                                          |
| `Stepper`              | "displays progress through a sequence of steps" like in a wizard UI; not frequently used                                           |
| `Switch`               | toggle switch for selecting a `bool` value                                                                                         |
| `TextButton`           | button containing any widget, not just `Text`, with no visible border                                                              |
| `TextField`            | basic text input                                                                                                                   |
| `TextFormField`        | like `TextField`, but supports validation                                                                                          |
| `ToggleButtons`        | set of toggle buttons, typically used to choose between exclusive options                                                          |
| `YearPicker`           | scrollable list of years to pick from (I can't get this to work!)                                                                  |

Many of these widgets render buttons, including
`DropDownButton`, `ElevatedButton`, `FloatingActionButton`,
`IconButton`, and `TextButton`.

Basic usage of all of these widgets is demonstrated in the Flutter project at
{% aTargetBlank "https://github.com/mvolkmann/flutter_input",
"flutter_input" %}.

### Other Material Classes

| Class              | Description                                               |
| ------------------ | --------------------------------------------------------- |
| `DefaultTextStyle` | style applied to text that doesn't have an explicit style |
| ``                 | `                                                         |

### Cupertino Widgets

These widgets use iOS styling rather than Material Design.
They are documented at {% aTargetBlank
"https://docs.flutter.dev/development/ui/widgets/cupertino",
"Cupertino (iOS-style) widgets" %}.

To use Cupertino widgets in a Dart source file, add the following import:

```dart
import 'package:flutter/cupertino.dart';
```

| Widget                                | Description                                                                                                        |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `CupertinoActionSheet`                | modal that slides up from bottom to allow selection from a set of options; sometimes used for confirmation dialogs |
| `CupertinoActivityIndicator`          | iOS-style spinner                                                                                                  |
| `CupertinoAlertDialog`                | iOS-style alert dialog with a title, message, and set of buttons                                                   |
| `CupertinoButton`                     | iOS-style button that can be tapped to execute associated code                                                     |
| `CupertinoContextMenu`                | iOS-style modal containing a set of tappable options rendered when a specific widget is long-pressed               |
| `CupertinoDatePicker`                 | iOS-style wheel picker for entering a date and time                                                                |
| `CupertinoDialogAction`               | button with no background color or border, typically used in `CupertinoAlertDialog`                                |
| `CupertinoFullScreenDialogTransition` | iOS-style transition used to render fullscreen dialogs                                                             |
| `CupertinoNavigationBar`              | iOS-style top navigation bar typically used with `CupertinoPageScaffold`                                           |
| `CupertinoPageScaffold`               | common iOS-style page layout                                                                                       |
| `CupertinoPageTransition`             | iOS-style page transition animation                                                                                |
| `CupertinoPicker`                     | iOS-style wheel picker                                                                                             |
| `CupertinoPopupSurface`               | rounded rectangle for an alert dialog or action sheet                                                              |
| `CupertinoScrollbar`                  | iOS-style scrollbar                                                                                                |
| `CupertinoSearchTextField`            | iOS-style search input                                                                                             |
| `CupertinoSegmentedControl`           | iOS-style segmented control which is a horizontal list of mutually-exclusive buttons                               |
| `CupertinoSlider`                     | iOS-style slider for selecting a value from a range                                                                |
| `CupertinoSlidingSegmentedControl`    | iOS-style segmented control which is a horizontal list of buttons                                                  |
| `CupertinoSliverNavigationBar`        | iOS-style navigation bar with a large title                                                                        |
| `CupertinoSwitch`                     | iOS-style switch (like the SwiftUI `Toggle` view)                                                                  |
| `CupertinoTabBar`                     | iOS-style bottom tab bar that is typically used with `CupertinoTabScaffold`                                        |
| `CupertinoTabScaffold`                | positions a tab bar below the display of select tab content                                                        |
| `CupertinoTabView`                    | supports "parallel navigation"? between tabs; typically used with `CupertinoTabScaffold`                           |
| `CupertinoTextField`                  | iOS-style input text field                                                                                         |
| `CupertinoTimerPicker`                | iOS-style wheel picker for entering hours, minutes, and seconds                                                    |

## Managing State

For state that is only used by a single stateful widget instance,
call the `setState` function from inside that widget.

For state that must be shared across multiple widget instances,
a recommended approach is to use the provider library.
In June, 2019 Chris Sells, the Flutter Project Manager, said
"Provider is the recommended way to do State Management for apps of all sizes."

There are many other state management options provided by the community.
Two popular packages are RiverPod and GetX.

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

The steps to use the provider library are:

1. Add dependency to `pubspec.yaml`.

   ```yaml
   dependencies:
     provider: ^6.0.1
   ```

2. Install by entering `flutter pub get`.

3. Create `ChangeNotifier` classes that hold state,
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

### GetX Library

TODO: Add information about this.

### RiverPod Library

TODO: Add information about this.

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

There are three primary ways to implement page navigation in Flutter.
The first uses the `Navigator` class.
The second uses the `BottomNavigationBar` widget.
And the third uses the `Drawer` widget.
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
      bottomNavigationBar: BottomNavigationBar(
        currentIndex: _pageIndex,
        onTap: (int index) {
          setState(() {
            _pageIndex = index;
          });
        },
        selectedItemColor: Colors.green,
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

## Bottom Sheets

Bottom sheets are a less intrusive alternative to dialogs.
They slide up from the bottom and can be dragged back down by the user
or closed programmatically.
They can be modal or non-modal.

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
              onPressed: () => openBottomSheet(
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

- The `Row`, `Column`, and `Flex` widgets need to take a `spacing` parameter
  like the `Wrap` widget does so it isn't necessary to do tedious things
  like adding a `SizedBox` between each child to leave space between them.

- The default linting rule settings related to use of the `const` keyword
  are incredibly annoying!
  I wish Dart could figure out what should be `const`
  on its own so we don't need to litter our code with that keyword.

- Needing to pass a `Key` and `BuildContext` to so many things is annoying.

- When running a Flutter app from VS Code,
  it seems that the output from `print` function calls goes nowhere.
  If the same app is run from a terminal, `print` output appears there.

- Sometimes after a hot reload the simulator will
  display an error message in yellow text on a red background.
  There may not really be an error in the code.
  Restarting the app can make the problem go away.

- The need to pass `context` and `key` in so many places is annoying.

- The error screen shown in the simulator when there is a runtime error
  displays the message in yellow on a red background.
  This combination is somewhat hard on the eyes.

- The error screen often does not provide
  the filename and line number of the error.
  It is displayed, in the terminal where the app is running,
  but it is buried in a long stack trace
  which makes it difficult to find.
