---
eleventyNavigation:
  key: SwiftUI
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/xcode/swiftui/", "SwiftUI" %}
is a Swift library for building macOS, iOS, and Apple Watch apps.

## Resources

## Getting Started

- open Xcode
- select "Create a new Xcode project"
- select an Application type such as "App" and click "Next"
- enter a Product Name
- select a Team such as "your name (Personal Team)"
- enter an Organization Identifier such as your email address
- select the Interface such as "SwiftUI"
- select the Language such as "Swift"
- check "Use Core Data" to persist data in a local database
- check "Include Tests" to enable writing unit tests
- click "Next"
- select the directory where the project will be stored and click "Create"
- press the black, right-pointing triangle near the top
  to build and run the starting version of the app
  in the simulator
- to change the device being simulated,
  click the device drop-down after the app name near the top
  and select the device type (such as "iPhone 12 mini")

The main Xcode window is divided into three main areas.
The left side is the Navigator.
The right side is the Inspector.
The center is the main editing area containing two panes.
The left pane is the code editor and the right pane is the Preview.

The Preview shows the UI outside of a simulator.
If it isn't running, press "Resume" to start it.
It automatically updates when code changes are saved.

To run the app in the Simulator, click the black triangle at the top.
That builds the app, launches the Simulator (if not already running),
loads the app in the Simulator, and starts it.
The app is not automatically updates when code changes are saved.
The triangle must be clicked again to repeat the whole build/load/start process.

Q: How can you get the simulator to automatically update
when code changes are saved?

## Icons

{% aTargetBlank "https://developer.apple.com/sf-symbols/", "SF Symbols" %}
is a library of over 3000 icons provided by Apple.
To use it, browse the website linked above and click the "Download" link.
This downloads a `.dmg` file.
Double-click this and double-click the `.pkg` file icon to install
the library and the "SF Symbols" app.
To discover icon names, launch the "SF Symbols" app,
browse the collection of icons, and click them.

To use an icon in a SwiftUI app, add an `Image` view. For example:

```swift
Image(systemName: "cloud.snow")
    .font(.system(size: 64))
```

## Views

SwiftUI Views are used for layout and components.

Layout/container views include:

- HStack
- VStack
- ZStack
- LazyHStack
- LazyVStack

- LazyHGrid
- LazyVGrid
- GridItem

- Form
- Group
- GroupBox
- ControlGroup

- ScrollView
- ScrollViewReader
- ScrollViewProxy

- List
- Section
- ForEach
- DynamicViewContent
- Table

- NavigationView
- NavigationLink
- OutlineGroup
- DisclosureGroup
- TabView
- HSplitView
- VSplitView
- TimelineView

- Spacer
- Divider

Component views include:

- Text
- TextField
- SecureField
- TextEditor

- Image
- AsyncImage

- Button
- EditButton
- PasteButton

- Link
- Menu

- Toggle
- Slider
- Stepper
- Picker
- DatePicker
- ColorPicker

- Label
- ProgressView
- Gauge

- EmptyView
- EquatableView
- AnyView
- TupleView

TODO: Are `Color` and `LinearGradient` views?

View modifiers are methods that can be called on a view to create
a new view that is like the receiver, but modified in a specific way.
For example, the `foregroundColor` method is a view modifier
that can be used as follows.

```swift
Text("Hello, World!").foregroundColor(.red)
```

## Utility Functions

The {% aTargetBlank
"https://developer.apple.com/documentation/swift/1539127-dump", "dump" %}
function takes a value and prints it for debugging purposes.
The value can be non-primitive such as an object or array.
Optional arguments that follow the value include
`name`, `indent`, `maxDepth`, and `maxItems`.

## Key Paths

A {% aTargetBlank "https://www.youtube.com/watch?v=YY7SlOklZzk",
"key path" %} refers to a property in a struct or class rather than
the value of a property in a particular instance.

A key path can be created with a backslash followed by
a type name, a period, and a property name.
For example:

```swift
struct Person {
  var name: String
}

let namePath = \Person.name
```

A key path can be used to get the value of a property
from an instance of a struct or class. For example:

```swift
let person = Person(name: "Mark")
let name = person[keyPath: namePath] // "Mark"
```

TODO: What kind of key path does `\self` create?

## Contexts

A SwiftUI context is ...
Most apps only use a single context.

## Environments

A SwiftUI environment specifies key/value pairs that affect
all sub-views of the view on which they are specified.
For example, if a font is specified on a `VStack` then it
becomes the default found for all views nested inside it at any depth.

To dump the contents on the current environment of a view,
chain the following onto the view:

```swift
.transformEnvironment(\.self) { dump($0) }
```

TODO: https://www.youtube.com/watch?v=SUiITSkAqAo&t=548s

## Network Requests

TODO: https://www.youtube.com/watch?v=2NowSN4qJUY&t=729s

## Combine

TODO: https://www.youtube.com/watch?v=bRpFHqv0tRQ&t=701s

## Core Data

TODO: https://www.youtube.com/watch?v=091Mdv_Rjb4

Core Data is an object/graph persistence framework.
It supports many features including
data validation, undo/redo, and lazy loading.

The easiest way to setup use of Core Data is to
check the "Use Core Data" checkbox on the options panel
when creating a new project.

To define the entities that will be persisted,
click the project-name.xcdatamodeld file in the Navigator pane.
This will display an entity editor.

To add entities, click the "Add Entity" button at the bottom.
This will create an empty entity named "Entity".
Double-click the name to change it.
These become class names, so begin with an uppercase letter.

To add attributes to an entity,
click the "+" at the bottom of the attribute list.
For each attribute, enter a name and select a type.

To create an initial set of objects,
edit the `preview` property defined in `Persistence.swift`.
For example:

```swift

``

If you see errors that say "cannot find type 'SomeEntityName' in scope",
restart Xcode.
```
