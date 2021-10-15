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

Q: How can you get the simulator to automatically update
when code changes are saved?

## Icons

{% aTargetBlank "https://developer.apple.com/sf-symbols/", "SF Symbols" %}
is a library of icons provided by Apple.
To use it, browse the website linked above and click the "Download" link.
This downloads a .dmg file.
Double-click this and double-click the .pkg file icon to install
the library and the "SF Symbols" app that is used to browse the icons.

To use an icon in a SwiftUI app,
add an `Image` view. For example:

```swift
Image(systemName: "cloud.snow")
    .font(.system(size: 64))
```

## Views

SwiftUI Views are used for layout and components.

Layout views include:

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
- Foreach
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
