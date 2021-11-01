---
eleventyNavigation:
  key: SwiftUI
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/xcode/swiftui/", "SwiftUI" %}
is a Swift library for building macOS, iOS, and Apple Watch apps.
It is an alternative to its predecessor UIKit.

By comparison SwiftUI ...

- is currently less capable than UIKit
- is declarative in nature than imperative
- emphasizes use of structs over classes (UIKit uses classes)
- requires far less code to do the same things

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

Press the black, right-pointing triangle near the top
to build and run the starting version of the app
in the simulator.

To change the device being simulated,
click the device drop-down after the app name near the top
and select the device type (such as "iPhone 12 mini").

To change the target version of iOS,
click the app name at the top of the Navigator
click the "General" tab, and
select an iOS version from the dropdown in the "Deployment info" section.

A new "App" project begins with the following files.

- `{app-name}.swift`

  This defines the main struct which implements the `App` protocol.
  It has computed property named `body`
  whose value implements the `Scene` protocol.
  The actual value is an instance of `WindowGroup`.
  This renders an instance of `ContentView` which is defined in the next file.

- `ContentView.swift`

  This defines the `ContentView` struct which implements the `View` protocol
  and is the top of the user interface.
  It also defines the `ContentView_Previews` struct
  which describes the Previews that should display in the Canvas area.
  This can be one or more views that are each displayed in a separate Preview.

- `Assets.xcassets`

  This associates names with assets such as images, audio files, and video files.

- `{app-name}.xcodeproj`

  This file is not visible in the Navigator,
  but can be viewed and edited by clicking the top entry in the navigator.
  The editor for this data has the following tabs:
  General, Signing & Capabilities, Resource Tags, Info,
  Build Settings, Build Phases, and Build Rules.
  The General tab configures the app display name, version of iOS,
  targets (iPhone, iPad, or Mac), status bar style, and more.

To add a particular kind of provided UI View in a `.swift` file,
manually enter code or click where it should go in the code
and click the "+" in the upper-right.
Clicking the "+" displays a dialog with a list provided View types.
Selecting one displays related documentation and code examples.
Double-click a View type to insert code for it.

### Canvas / Preview

The Canvas area displays Previews of the UI running outside of a simulator.
To hide/show the Canvas area, select Editor ... Canvas.
If Preview isn't running inside the Canvas area,
press the "Resume" button at the top to start it.

By default the Canvas area is only displayed
when the file `ContentView.swift` is selected.
To keep it displayed even when a different file is selected,
select `ContentView.swift` and
click the pin icon in the lower-left of the Canvas.

When code changes are saved and there are no errors,
the Preview is automatically updated.
When there are errors, the Preview pauses and must be manually restarted
by click the "Resume" button or pressing cmd-option-p.
TODO: Are there other things that cause the Preview to pause?

If the Canvas area displays the message "Failed to build" in the upper-left,
click the "Diagnostics" button in the upper-right to see error messages.

To zoom in and out on the Preview area,
click the magnifier glass icons in the lower right (minus and plus)
or select a zoom level from the percent dropdown.

By default the Preview is not in "Live Preview" mode.
Key things to know about not being in this mode include:

- Clicking a View in the Preview selects it rather than triggering tap events.
  The corresponding code is highlighted and
  the Inspector changes to show the properties of the selected View.
- Double-clicking a View in the Preview is similar,
  but moves focus to the code so it can be edited.
- Clicking a View in the code selects the corresponding View in the Preview
  and also populates the Inspector.
- Changes made in the Inspector update the Preview and the code,
  but they do not take effect until
  focus is moved out of the modified Navigator field.

To switch to "Live Preview" mode so tap events are honored,
click the button with a triangle inside a circle.
This also enables scrolling by dragging
which is not possible in the default mode.
And one more thing clicking this does is to
trigger the Preview to resume if it is paused
and rebuild the code.

To rotate the display to landscape mode,
click the button above the display that contains
a square with a curved arrow on its upper-left corner.
This only works when not in "Live Preview" mode.
The same button appears in the Simulator.

It is possible for the Preview area to show more than one preview.
This is controlled by the `ContentView_Previews` struct
defined in `ContentView.swift`.
For example, the following renders the app in light mode, the app in dark mode,
and a single component to test in isolation from the rest of the app.

```swift
struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView().preferredColorScheme(.light)
        ContentView().preferredColorScheme(.dark)
        CircleButton(color: .blue) { } // action that does nothing
    }
}
```

Note that if any of the previews are in "Live Preview" mode,
only that preview will be displayed.
Exit out of that mode to get the other previews to display again.

### Inspector

The Inspector shows commonly used modifiers of the selected view.
To see additional modifiers,
click "Add Modifier" at the button of the Inspector.
Optionally type part of a modifier name to filter the list.
Click a modifier to add it to the Inspector.
To stop displaying a modifier in the Inspector,
hover over it and click the "Delete" button that appears.

### Simulator

To run the app in the Simulator, click the black triangle at the top.
That builds the app, launches the Simulator (if not already running),
loads the app in the Simulator, and starts it.
The app is not automatically updates when code changes are saved.
The triangle must be clicked again to repeat the whole build/load/start process.

## Apple Human Interface Guidelines (HIG)

Apple documents guidelines for building user interfaces at {% aTargetBlank
"https://developer.apple.com/design/human-interface-guidelines/",
"Human Interface Guidelines" %}.
This includes recommendations for macOS, iOS, watchOS, and tvOS
user interfaces.

The guidelines for iOS are divided into several categories,
three of which are Bars, Views, and Controls.
The following summarizes what is found in those sections
and how the described components map to SwiftUI views.

### Bars

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/navigation-bars/",
    "Navigation Bars" %}

  - appears at top of screen below status bar
  - enables navigation through hierarchical screens
  - provides leading back button
  - can have trailing buttons like Edit and Done
  - can have a tint color
  - can have an inline or large title
  - can use a Segmented Control in place of title
  - SwiftUI creates this with `NavigationView`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/search-bars/",
    "Search Bars" %}

  - an text input for entering search text
  - has magnifier glass icon
  - can display in a Navigation Bar
  - can include clear and confirm buttons
  - SwiftUI creates this with `Text(...).searchable(text: ...)`
    (required to be inside a `NavigationView`?)

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/sidebars/",
    "Side Bars" %}

  - leading navigation to top-level pages that is collapsable
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/status-bars/",
    "Status Bars" %}

  - appears at top of screen above Navigation Bar
  - the system provided Status Bar displays the time on the left and indicators
    for cell strength, WiFi strength, and batter remaining on the right
  - can style to light or dark mode and customize colors
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/tab-bars/",
    "Tab Bars" %}

  - used to navigate to top-level app sections (groups of related pages)
  - appears at bottom of screen
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/toolbars/",
    "Toolbars" %}

  - contains buttons that perform page-specific actions
  - appears at bottom of screen
  - SwiftUI creates this with TODO

### Views

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/action-sheets/",
    "Action Sheets" %}

  - alert that presents two or more choices related to the current context
  - on small screens, slides in from bottom
  - on large screens, appears as a Popover
  - can use to request confirmation before a destructive operation

  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/activity-views/",
    "Activity Views" %}

  - set of activity buttons applicable in current context such as Copy, Add, or Find
  - appears as a Sheet or Popover
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/alerts/",
    "Alerts" %}

  - modal dialog with a title, optional message,
    one or more buttons, and optional input text fields
  - avoid having more than two buttons
  - minimize usage to important situations
  - SwiftUI creates this with the `alert` method
    that can be called on any kind of view.
    See the "Alerts" section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/collections/",
    "Collections" %}

  - manage an ordered set of content,
    like photos presented as a grid of thumbnails
  - tap an item to select
  - touch and hold an item to edit
  - swipe to scroll
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/image-views/",
    "Image Views" %}

  - displays a single image or animated sequence
  - can fill entire display
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/pages/",
    "Pages" %}

  - implements linear navigation between a set of related pages
    using either scrolling or page curl effects
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/popovers/",
    "Popovers" %}

  - modal or non-modal dialog displayed in response to
    tapping a control or tapping in an area
  - typically renders a "tail" pointing to what triggered it
  - can contain many kinds of elements including
    Navigation Bars, Toolbars, Tab Bars, and more
  - avoid using on iPhones
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/scroll-views/",
    "Scroll Views" %}

  - scrolls content larger than visible area
  - displays transient scrolling indicators
  - can operation in paging mode
  - can support zooming
  - SwiftUI creates this with ScrollView

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/sheets/",
    "Sheets" %}

  - a card that partially covers primary content
  - used to perform a task related to current context
  - top corners are rounded and can customize radius
  - two available heights, large (default) and medium
  - modal by default
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/split-views/",
    "Split Views" %}

  - presents hierarchical data with two or three columns:
    primary, supplementary, and content
  - selections in primary cause changes in supplementary
  - selections in supplementary cause changes in content
  - highlight current selections
  - used in Mail app where primary is a list of mailboxes,
    supplementary is a list of messages in the selected mailbox,
    and content is the content of the selected email
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/tables/",
    "Tables" %}

  - a scrolling, single-column or rows that are each divided into sections
  - can use for navigation in a Split View
  - three styles: plain, grouped, and inset grouped
  - not a data grid like in other UI frameworks
  - TODO: Does iOS provide a data grid component?
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/text-views/",
    "Text Views" %}

  - multi-line styled text
  - optional scrolling
  - can control alignment, font, and color
  - can be editable and if so can specify a keyboard type;
    (see the {% aTargetBlank
    "https://developer.apple.com/documentation/uikit/uikeyboardtype",
    "UIKeyboardType enum" %})
  - SwiftUI creates this with TODO

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/web-views/",
    "Web Views" %}

  - renders embedded HTML or HTML from a web site
  - can enable forward and backward navigation
  - SwiftUI creates this with TODO

### Controls

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/buttons/",
    "Buttons" %}

  - several predefined button styles are supported including
    - toggle: different from `Toggle` view
    - pop-up: displays a menu of mutually exclusive options;
      after choosing an option the button content can change to indicate the selection
    - pull-down: displays a menu of items that relate to the button purpose;
      button content never changes
  - predefined sizes are small, medium, and large
  - predefined styles include plain, gray, tinted,
    and filled (used for most likely action)
  - can include an SF Symbol icon
  - can have a role of normal, primary, cancel, or destructive
  - can be a close button (x in circle) that closes the parent view
  - can be an info button (i in circle) that displays
    information about the parent view
  - SwiftUI creates this with `Button`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/color-wells/",
    "Color Wells" %}

  - displays a currently selected color in a circle
  - when tapped, displays the system color picker to change the selected color
  - SwiftUI creates this with `ColorPicker`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/context-menus/",
    "Context Menus" %}

  - displays a menu of context-sensitive items when the menu label is tapped
  - can contain sub-menus
  - can include separators to group options
  - SwiftUI creates this with `Menu`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/edit-menus/",
    "Edit Menus" %}

  - a provided horizontal menu that is displayed when a long press
    occurs in a text field, text view, web view, or image view
  - by default contains buttons for Cut, Copy, Paste, Select, Select All, Delete, Replace..., Loop Up, and Share...
  - can disable any of the default buttons to remove them
  - SwiftUI provides this automatically when a long press occurs
    in a `TextField` or `TextEditor`.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/labels/",
    "Labels" %}

  - plain or styled text
  - SwiftUI creates this with `Label` or `Text`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/page-controls/",
    "Page Controls" %}

  - a row of indicator images that represent pages in a list
  - can handle any number of pages
  - image for current page is highlighted
  - can customize images
  - TODO: SwiftUI may not support this yet.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/pickers/",
    "Pickers" %}

  - displays distinct values in a scrollable list and allows users to select one
  - SwiftUI creates this with `Picker` and `DatePicker`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/progress-indicators/",
    "Progress Indicators" %}

  - informs users that an activity is running in the background
  - can indicate how much longer it will run using a progress bar
  - "activity indicators" are for indeterminate activities
  - SwiftUI creates this with `ProgressView`.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/refresh-content-controls/",
    "Refresh Content Controls" %}

  - becomes visible when a user pulls down on a view to request a content reload
  - usually used in a table view
  - SwiftUI creates this with the `refreshable` view modifier
    which is often used on `List` views.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/segmented-controls/",
    "Segmented Controls" %}

  - set of toggle buttons
  - can use in place of web UI radio buttons
  - can use to select between different kinds of views
  - SwiftUI creates this with `Picker(...).pickerStyle(.segmented)`
    GRONK

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/sliders/",
    "Sliders" %}

  - a horizontal track with a thumb that
    slides between minimum and maximum values
  - can display icons on leading and trailing ends
  - SwiftUI creates this with `Slider`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/steppers/",
    "Steppers" %}

  - pair of minus and plus buttons that decrement and increment a value
  - can replace "-" and "+" with images
  - doesn't display current value
  - SwiftUI creates this with `Stepper`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/switches/",
    "Switches" %}

  - a toggle between mutually-exclusive states
  - can set tint to match app theme
  - usually used in table rows with a label on the leading side
  - SwiftUI creates this with `Toggle`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/text-fields/",
    "Text Fields" %}

  - single line text input with fixed height
  - usually has rounded corners
  - brings up on-screen keyboard when tapped
  - can contain placeholder text which is preferred over preceding with a label
  - optionally includes a clear button (x in circle)
  - can mask inputs like passwords
  - can include images on leading and/or trailing sides
  - can specify a keyboard type defined by the {% aTargetBlank
    "https://developer.apple.com/documentation/uikit/uikeyboardtype",
    "UIKeyboardType enum" %})
  - SwiftUI creates this with `TextField`, `SecureField`,
    and `TextEditor` (multi-line)

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
Image(systemName: "heart").font(.largeTitle)) // one way to set font size
Image(systemName: "cloud.snow").font(.system(size: 64)) // another way
```

## Core Graphics (CG)

Several types used in views have names that begin with "CG"
which stands for "Core Graphics". These include:

- `CGAffineTransform`

  This holds a transformation matrix used in 2D transformations.

- `CGColor`

  TODO: Add information about this and compare to `Color` and `UIColor`.

- `CGFloat`

  This holds a floating point value whose size depends on the CPU architecture.
  It can be 32 or 64 bits.

- `CGPoint`

  This holds `x` and `y` properties that have `CGFloat` values.

- `CGRect`

  This holds an `origin` property that is a `CGPoint`
  and a `size` property that is a `CGSize`.
  It also has the following computed properties with `CGFloat` values:
  `height`, `width`, `minX`, `maxX`, `minY`, `maxY`, `midX`, and `midY`.

- `CGSize`

  This holds `width` and `height` properties that have `CGFloat` values.

- `CGVector`

  This holds `dx` and `dy` properties that have `CGFloat` values.

## Views

Every visible part of an app created with SwiftUI is a view.
Views can render other views.

Each view is a struct that implement the `View` protocol.
This requires having a computed property named `body` with the type `Some View`.
Most properties declared in a `View` struct should be `private`.
Exceptions include the `body` property and
any properties that are passed in when instances are created.

Views are used for components and layout.
Views that layout other views are often referred to as "combiner views".

Functions that create views often take a `ViewBuilder` their last argument.
These are typically written as trailing closures.

Views can be given an explicit identifier with the `.id` method
that is passed an identifier.
This is useful in methods that take a view identifier
like the `ScrollViewReader` method `scrollTo`.

To cause a view to take the full width of the screen,
apply the `frame` view modifier.
For example, `myView.frame(maxWidth: .infinity)`.
In can also be useful to specify the height.
For example, `myView.frame(maxWidth: .infinity, height: 100)`.

Many views utilize the app accent color, also referred to as "global tint".
To set this, select the `Assets.xcassets` file in the Navigator,
select "AccentColor", click the "Universal" swatch,
and click the "Color" dropdown in the Inspector.
Resume Preview to see the change.
This does not affect `Toggle` views which required using the view modifier
`.toggleStyle(SwitchToggleStyle(tint: someColor))`.

### ViewBuilders

Combiner views can be passed a special kind of closure
as their last argument that is called a `ViewBuilder`.
This uses list-oriented syntax to describe a list of
one to ten other views that are combined into a single view.
Note that a `ForEach` view counts as a single view.

`ViewBuilder` blocks are parsed differently by the compiler
than ordinary closures.
In addition to containing views,
they can also contain local variable declarations and
conditional logic with `if`, `if let`, and `switch` statements.
However, no other Swift syntax is allowed inside them.

For iteration in a `ViewBuilder`, use a `ForEach` `View`.
There is no corresponding view for conditional logic,
so `if` and `switch` statements are used instead.

`@ViewBuilder` is a custom parameter attribute used to indicate
that a parameter accepts function that returns a list of views.
The list of views can match one of the following types:

- `EmptyView`: zero views
- `TupleView`: two or more views
- `Content?`: generated by an `if` statement to render one view or none
- `_ConditionalContent`: generated by an `if/else` or `switch` statement
  to render one of a set of possible views
  (The underscore indicates that this is a not a public API.)

A `ViewBuilder` is a kind of result builder.
For more information on these, see the {% aTargetBlank
"https://github.com/apple/swift-evolution/blob/main/proposals/0289-result-builders.md",
"Result builders proposal" %}.

### Combiner Views

Combiner views combine other views.
They act as a container of other views
and layout those views in a specific way.

To hide a view but still take up the space that would be occupied
if the view was visible, consider setting its opacity to zero.
This is done with the `opacity` view modifier.
For example, `someView.opacity(0)`.

Here are the combiner views that are provided by SwiftUI.

- `HStack`

  This lays out child views horizontally.

  The child views are centered vertically by default.
  To change this, add the `alignment` attribute which can be set to
  `.top`, `.center`, `.bottom`, `.firstTextBaseline`, or `.lastTextBaseline`.

  A default amount of space is added between each child
  which seems to be 8 pixels (can't find this documented).
  To change the space between child views, add the `spacing` attribute.

  The following example shows the effect of
  setting `alignment` to `.lastTextBaseLine`.

  <img alt="SwiftUI HStack" style="width: 40%"
    src="/blog/assets/SwiftUI-HStack.png?v={{pkg.version}}"
    title="SwiftUI HStack">

  ```swift
  HStack(alignment: .lastTextBaseline, spacing: 0) {
      Rectangle()
          .fill(.red)
          .frame(width: 100, height: 100)
          .border(.black)
      Rectangle()
          .fill(.green)
          .frame(width: 50, height: 50)
          .border(.black)
      Text("Line 1\nLine 2").padding(20).border(.black)
  }.border(.blue)
  ```

- `VStack`

  This lays out child views vertically.

  The child views are centered horizontally by default.
  To change this, add the `alignment` attribute which can be set to
  `.leading`, `.center`, or `.trailing`.

- `ZStack`

  This stacks views from bottom to top.
  It is ideal for adding a background to a set of views.

  Here are three approaches to rendering text with a colored background,
  one of which uses a `ZStack`.

  ```swift
  struct ContentView: View {
      let bgColor: Color = .yellow
      let text = "Test"

      var body: some View {
          VStack {
              let rect = Rectangle()
                  .fill(bgColor)
                  .frame(width: 50, height: 40)
              // Semicolons must separate multiple statements
              // on the same line.
              ZStack { rect; Text(text) }
              Text(text)
                  .padding(10)
                  .background(Rectangle().foregroundColor(bgColor))
              rect.overlay(Text(text))
          }
      }
  }
  ```

- `LazyHStack`

  This is similar to `HStack`, but only
  builds and renders child views when they are visible.
  It is commonly used inside a `ScrollView` with `axes` set to `.horizontal`.

  ```swift
  ScrollView(.horizontal) {
      LazyHStack {
          ForEach(1..<100) {
              Text(String($0))
          }
      }
  }
  ```

- `LazyVStack`

  This is similar to `VStack`, but only
  builds and renders child views when they are visible.
  It is commonly used inside a `ScrollView` with `axes` set to `.vertical`,
  which is the default.

  ```swift
  ScrollView {
      LazyVStack {
          ForEach(1..<101) {
              Text(String($0))
          }
      }
  }
  ```

- `LazyHGrid`

  This specifies a number of rows and adds columns as necessary.
  The grids are described by an array of {% aTargetBlank
  "https://developer.apple.com/documentation/swiftui/griditem", "GridItem" %}
  objects that each specify their size, spacing, and alignment.
  For example, a `GridItem` can adapt to the width of its content,
  but also have a minimum size of 25 by specifying
  `GridItem(.adaptive(minimum: 25))`.

  See the example in `LazyVGrid` below.

- `LazyVGrid`

  This is similar to `LazyHGrid`, but
  specifies a number of columns and adds rows as necessary.

  The following example demonstrates both `LazyHGrid` and `LazyVGrid`.

  ```swift
  struct ContentView: View {
      private static let count = 3
      private let isVertical = false // Why can't this be static?

      // Describe the characteristics of each grid.
      private static let gridItem = GridItem(
          // This specifies the grid height in LazyHGrid
          // or the grid width in LazyVGrid.
          .fixed(40),
          // This specifies the vertical spacing in LazyHGrid
          // or horizontal spacing in LazyVGrid.
          spacing: 10,
          alignment: .center
      )

      var gridItems: [GridItem] = Array(repeating: gridItem, count: count)

      var body: some View {
          if isVertical {
              ScrollView {
                  LazyVGrid(columns: gridItems) {
                      ForEach(1..<101) {
                          Text(String($0)).padding(5).border(.blue)
                      }
                   }.border(.red)
              }
          } else {
              ScrollView(.horizontal) {
                  LazyHGrid(rows: gridItems) {
                      ForEach(1..<101) {
                          Text(String($0)).padding(5).border(.blue)
                      }
                  }.border(.red)
              }
          }
      }
  }
  ```

- `Form`

  This is a container of data entry views.

  The following example demonstrates many common views used in forms.

  <img alt="SwiftUI Form" style="width: 40%"
    src="/blog/assets/SwiftUI-Form.png?v={{pkg.version}}"
    title="SwiftUI Form">

  ```swift
  enum ShirtSize: String, CaseIterable {
    case small
    case medium
    case large
    case extraLarge
  }

  struct ContentView: View {
      private static let blogUrl = "https://mvolkmann.github.io/blog"

      // Typically form data would be tied to ViewModel properties
      // rather than using @State.
      @State private var bedTime: Date = Date()
      @State private var birthday: Date = Date()
      @State private var favoriteColor: Color = .yellow
      @State private var dogCount = 0
      @State private var hungry = false
      @State private var name = ""
      @State private var motto = "This is my motto."
      @State private var rating = 0.0
      @State private var shirtSize: ShirtSize = .large

      var isEditing = false

      var body: some View {
          NavigationView { // Pickers will be disabled without this!
              Form {
                  Section(header: Text("Profile")) {
                      TextField("Name", text: $name)
                      DatePicker(
                          "Birthday",
                          selection: $birthday,
                          displayedComponents: .date
                      )
                      Toggle("Hungry?", isOn: $hungry)
                          .toggleStyle(SwitchToggleStyle(tint: .red))
                  }
                  Section(header: Text("Preferences")) {
                      // Links work in Simulator, but not in Preview.
                      Link(
                          "Blog",
                          destination: URL(string: ContentView.blogUrl)!
                      )
                      VStack {
                          Text("Motto")
                          // It seems TextEditor lineLimit is
                          // only enforced on initial render.
                          // It doesn't prevent more lines from being
                          // displayed if the user types more text.
                          TextEditor(text: $motto).lineLimit(2)
                      }
                      ColorPicker(
                          "Favorite Color",
                          selection: $favoriteColor
                      )
                      DatePicker(
                          "Bed Time",
                          selection: $bedTime,
                          displayedComponents: .hourAndMinute
                      )
                      Picker("Shirt Size", selection: $shirtSize) {
                          ForEach(ShirtSize.allCases, id: \.self) { size in
                              Text("\(size.rawValue)").tag(size)
                          }
                      }
                      HStack {
                          Text("Rating")
                          //TODO: Why does value have to be Float
                          //TODO: instead of Int?
                          Slider(value: $rating, in: 0...10, step: 1)
                          Text("\(Int(rating))")
                      }
                      HStack {
                          Stepper(
                              "# of Dogs",
                              value: $dogCount, in: 0...10
                          )
                          Text(String(dogCount))
                      }
                  }
              }
              .navigationTitle("Profile")
              // accentColor changes the color of many elements including
              // the text cursor color, focus color, and Slider bar color.
              // It does not affect the focus background color of Toggle
              // views.  Use the "toggleStyle" view modifier for that.
              .accentColor(.red)
              .toolbar {
                  ToolbarItemGroup(placement: .navigationBarTrailing) {
                      Button("Save", action: saveUser)
                  }
              }
          }
      }
  }
  ```

  Common UI components that are not built into SwiftUI include:

  - checkbox: alternative is Toggle
  - image picker: must build or using a library
  - multiple choice: alternative is List inside NavigationView with EditButton
  - radio buttons: alternative is Picker (supported in macOS with
    Picker and .pickerStyle(RadioGroupPickerStyle())
  - toggle buttons: alternative is Picker

- `Group`
- `GroupBox`
- `ControlGroup`

- `ScrollView`

This creates a scrollable view that is vertical by default,
but can be changed to horizontal.
It occupies all the space offered to it.
Scrolling reveals additional child views when all of them do not fit.
See examples of using this in the
descriptions of `LazyHStack` and `LazyVStack`.

- `ScrollViewReader`
- `ScrollViewProxy`

- `List`

This displays a list of views in a single column.
It also acts like `ForEach` for iterating over array elements.

The following example demonstrates using a `List` inside a `NavigationView`
to enable selecting ids of the objects represented by the rows.
To select rows, tap "Edit" in the upper-right corner.

This also demonstrates implementing "pull to refresh"
using the `refreshable` view modifier.

```swift
struct ContentView: View {
    struct Dog: CustomStringConvertible, Identifiable, Hashable {
        let id = UUID()
        let name: String
        let breed: String
        var description: String { "\(name) - \(breed)" }
    }

    @State private var dogs = [
        Dog(name: "Maisey", breed: "Treeing Walker Coonhound"),
        Dog(name: "Ramsay", breed: "Native American Indian Dog"),
        Dog(name: "Oscar", breed: "German Shorthaired Pointer"),
        Dog(name: "Comet", breed: "Whippet")
    ]

    @State private var selectedIds = Set<UUID>()

    func loadMore() async {
        // This calls a REST service that returns nothing after 2 seconds.
        let url = URL(string: "https://httpbin.org/delay/2")!
        let request = URLRequest(url: url)
        // Not using return value.
        let _ = try! await URLSession.shared.data(for: request)

        dogs.append(Dog(name: "Clarice", breed: "Whippet"))
        dogs.append(Dog(name: "Vixen", breed: "Whippet"))
    }

    var body: some View {
        VStack {
            NavigationView {
                List(dogs, selection: $selectedIds) { dog in
                    let desc = String(describing: dog)
                    if selectedIds.contains(where: {$0 == dog.id}) {
                        Text(desc).bold().foregroundColor(.green)
                    } else {
                        Text(desc)
                    }
                }
                .navigationTitle("Dogs")
                // The EditButton in the toolbar toggles
                // the edit mode of the NavigationView.
                .toolbar { EditButton() }
            }.refreshable { await loadMore() }
            Text("\(selectedIds.count) selections")
        }
    }
}
```

- `Section`
- `ForEach`

  This view iterates of the elements of an array
  and renders the view specified in its `ViewBuilder`.

  The elements in the array must either conform to the `Identifiable` protocol
  (which requires them to have an `id` property)
  OR the `id:` argument must be set.
  The value of `id:` is a key path that specifies
  how to find a unique value in the element.
  For example, the `String` type does not implement `Identifiable`.
  To iterate over an array of `String` values:

  ```swift
  // \.self is a key path that refers to the entire object.
  ForEach(stringArray, id: \.self) { ... }
  ```

- `DynamicViewContent`
- `Table`

- `NavigationView`
- `NavigationLink`
- `OutlineGroup`

  This displays a tree of data with disclosure angle brackets.
  See my SwiftUI-OutlineGroup project and the questions in it.

- `DisclosureGroup`
- `TabView`
- `HSplitView`
- `VSplitView`
- `TimelineView`

### Component Views

- `Text`

  This view renders text.
  If the text is too long to fit on a single line,
  it is automatically wrapped to additional lines.
  To prevent this, apply the `lineLimit` view modifier
  and pass the number of lines that can be used (perhaps 1).
  If the text doesn't fit in the allowed number of lines,
  it will be elided and an ellipsis will appear at the end.

  To set the foreground color, apply the `foregroundColor` view modifier.
  To set the font size, apply the `font` view modifier.

  ```swift
  Text("Hello World").foregroundColor(.red).font(.system(size: 24))
  ```

- `TextField`
- `SecureField`
- `TextEditor`

- `Image`

  This renders an image.
  Many image formats are supported including PNG, JPEG, and HEIC.
  Click `Assets.xcassets` in the Navigator to
  associate a name with each image to be used.
  Click the "+" in the lower-left to add an entry.
  Give the entry a name and drag images into the 1x, 2x, and 3x boxes.
  Pass the name to the `Image` view as an unlabelled argument.
  For example, `Image("Comet")`.

  Icons from SF Symbols can be used by specifying
  their name as the `systemName` argument.
  For example, `Image(systemName: "cloud.snow")`.

- `AsyncImage`

- `Button`

  The content of a `Button` can be specified in two ways,
  passing a `String` as the first argument or using the `label` argument
  which can be specified with a trailing closure.

  `Button` specify a function to call when pressed using the `action` argument
  which can also be written as a trailing closure.

  By default buttons have no background color and the text is the accent color.
  When their `role` attribute is set to `.destructive`, the text is red.
  When their `role` attribute is set to `.cancel`, `.none`, or not specified,
  there is no visible change.
  When the `buttonStyle` view modifier is passed `.bordered`,
  the background is gray.
  When the `buttonStyle` view modifier is passed `.borderProminent`,
  the background is the accent color and the text is white.

  ```swift
  // Button containing text and action specified with a trailing closure.
  Button("My Label", role: .destructive) {
      // code to run when button is pressed
  }.buttonStyle(.borderProminent)

  // Button with an "action" argument whose value
  // can be a closure or a function reference
  // and a "contents" argument whose value is a ViewBuilder
  // that can be written as a trailing closure.
  Button(action: {
      // code to run when button is pressed
  }) {
      HStack {
          Text("Heart")
          Image(systemName: "heart")
      }
  }
  ```

- `Color`

  This creates a rectangle view with a specific background color
  that grows to fill all the space offered to it.
  For example, `Color.red` and `Color.clear` (transparent) are views.

- `EditButton`
- `PasteButton`

- `Link`

  This creates a hyperlink like an HTML `a` element.
  Clicking it opens the associated URL in Safari.
  This works in the Simulator, but Preview is not able to open Safari.

- `Menu`

  This renders a label containing the menu title.
  When clicked, a menu appears below the label
  containing a vertical stack of buttons and sub-menus.
  Include `Divider` views to separate groups of menu items.

  The order of the buttons depends on the position of the menu.
  If it is near the bottom of the display,
  the button order is reversed in order to make all the buttons visible
  and keep the first button close to the label.

  ```swift
  Menu("My Menu") {
      Button("Option 1", action: {})
      Button("Option 2", action: {})
      Menu("Option 3") {
          Button("Option 3.1", action: {})
          Button("Option 3.2", action: {})
      }
  }
  ```

  The following example uses a `Menu` to
  select the color uses to fill a `Rectangle`.

  ```swift
  @State var color = Color.red

  var body: some View {
      VStack {
          Menu("Color") {
              Button("Red") { color = .red }
              Button("Green") { color = .green }
              Button("Blue") { color = .blue }
          }
          Rectangle().fill(color).frame(width: 50, height: 50)
      }
  }
  ```

- `Toggle`
- `Slider`
- `Stepper`

  This displays "-" and "+" buttons that can be
  clicked to decrement and increment a value.

- `Picker`

  This allows selecting an option from a list.
  It displays the currently selected value.
  Tapping it popups up a list of all the options
  from which a new value can be selected.

  ```swift
  @State var shirtSize: ShirtSize = .sm

  enum ShirtSize: String, CaseIterable {
      case sm = "Small"
      case md = "Medium"
      case lg = "Large"
      case xl = "Extra Large"
  }

  var body: some View {
      Picker("Shirt Size", selection: $shirtSize) {
          ForEach(ShirtSize.allCases, id: \.self) { size in
              Text("\(size.rawValue)").tag(size)
          }
      }
  }
  ```

- `DatePicker`

  This allows selecting a date, time, or both.

  ```swift
  DatePicker(
      "Birthday",
      selection: $birthday, // a binding
      displayedComponents: [.date, .hourAndMinute] // array or single value
  )
  ```

- `ColorPicker`

  This renders a color well for displaying a currently selected color
  and changing the color using the system color picker.

  ```swift
  ColorPicker(
      "Favorite Color",
      selection: $favoriteColor // a binding
  )
  ```

- `Label`

  This is a combination of an icon and text.
  The icon appears before the text.

  ```swift
  Label("Rain", systemImage: "cloud.rain")
  ```

- `ProgressView`

  This displays a progress indicator.
  The indeterminate style uses the standard Apple spinner.
  The determinate style uses a thin progress bar.

  ```swift
  struct ContentView: View {
      @State var progress = 0.0

      func startTimer() {
          // Run every 3 hundredths of a second.
          // When "repeats" is false, this is like setTimeout in JavaScript.
          // When "repeats" is true, this is like setInterval in JavaScript.
          Timer.scheduledTimer(withTimeInterval: 0.03, repeats: true) { timer in
              progress += 0.01
              if (progress >= 1) {
                  timer.invalidate()
                  progress = 0
              }
          }
      }

      var body: some View {
          VStack {
              if progress > 0 {
                  ProgressView() // indeterminate
                  ProgressView(value: progress).padding() // determinate
              }
          }
          .onAppear { startTimer() }
      }
  }
  ```

- `Gauge`

  This shows a current value in relation to minimum and maximum values.
  One example is a car fuel gauge.
  This is currently only supported in watchOS.

- `EmptyView`
- `EquatableView`
- `AnyView`
- `TupleView`

Here is an example of using combiner and component views.
Note how views can be defined in computed properties
that are later referenced to render them.
The type of these computed properties can be a specific combiner view type
or the generic `some View`.
Custom views can also be defined in a new struct that inherits from `View`
Instances of these structs are created to render them.

```swift
// This defines a custom View that is used below.
struct MyRow: View {
    var body: some View {
        HStack {
            Text("Six")
            Text("Seven")
        }
    }
}

struct ContentView: View {
    // Assigning a View to a computed property
    var row = HStack {
        Text("Four")
        Text("Five")
    }

    var body: some View {
        VStack {
            Text("One")
            HStack {
                Text("Two")
                Text("Three")
            }
            // Referring to a variable to get a View
            // and chaining View modifiers
            row.padding().border(.red)
            MyRow() // Creating a custom View instance
        }
    }
}
```

Note how `ContentView` uses the view `MyRow`.
It is preferred to create small views like this and compose them
rather than creating views whose code is long and deeply nested.

### Drawing Views

The `Shape` protocol inherits from the `View` protocol
and there are many provided views that inherit from `Shape`.
Examples include `Circle` and `Rectangle`.

By default, all views that inherit from `Shape` are
filled with the foreground color of their parent view.
This can be changed using the `fill` view modifier.
It takes an object of a type that implements the `ShapeStyle` protocol.
Examples include `Color`, `AngularGradient`, `LinearGradient`,
`RadialGradient`, and `ImagePaint`.

An outline can be added to any `Shape` with the `.stroke` view modifier.

Many of these views support both the `border` and `strokeBorder` view modifiers.
The difference between these becomes apparent
when the border width is greater than one.
`border` is drawn so it is centered on the edge of the shape
with have inside and half outside.
`strokeBorder` is drawn so none of the border is outside of the shape.

The following example draws several shapes.

<img alt="SwiftUI Shapes" style="width: 40%"
  src="/blog/assets/SwiftUI-Shapes.png?v={{pkg.version}}"
  title="SwiftUI Shapes">

```swift
struct ContentView: View {
    let linearGradient = LinearGradient(
        gradient: Gradient(colors: [.red, .yellow]),
        startPoint: .leading, // other values are .top and .bottom
        endPoint: .trailing
    )
    let angularGradient = AngularGradient(
        // It's usually best to return to the starting color.
        gradient: Gradient(colors: [.yellow, .blue, .yellow]),
        center: .center,
        startAngle: .degrees(90),
        endAngle: .degrees(90 + 360)
    )
    let radialGradient = RadialGradient(
        gradient: Gradient(colors: [.red, .yellow]),
        center: .center,
        startRadius: 0,
        endRadius: 20
    )

    func radialGradient(over size: CGSize) -> RadialGradient {
        let diameter = min(size.width, size.height)
        let radius = diameter / 2;
        return RadialGradient(
            // Colors go from inside to outside.
            gradient: Gradient(colors: [.white, .yellow, .red]),
            center: .center,
            startRadius: 0,
            endRadius: radius
        )
    }

    var body: some View {
        VStack {
            ZStack {
                Capsule().fill(.red)
                Text("Capsule with solid color")
            }
            ZStack {
                GeometryReader { geometry in
                    Circle().fill(radialGradient(over: geometry.size))
                }
                Text("Circle\nwith\nRadialGradient")
                    .multilineTextAlignment(.center)
            }
            ZStack {
                Ellipse().fill(angularGradient)
                Text("Ellipse with AngularGradient")
            }
            ZStack {
                Rectangle().fill(linearGradient)
                Text("Rectangle with LinearGradient")
            }
            ZStack {
                RoundedRectangle(cornerRadius: 10)
                    .fill(ImagePaint(image: Image("Comet"), scale: 0.34))
                Text("RoundedRectangle with ImagePaint")
                .font(.largeTitle)
                .foregroundColor(.purple)
            }
        }.padding()
    }
}
```

The `Path` view supports many drawing commands.
For example:

<img alt="SwiftUI Path" style="width: 40%"
  src="/blog/assets/SwiftUI-Path.png?v={{pkg.version}}"
  title="SwiftUI Path">

```swift
struct ContentView: View {
    let style = StrokeStyle(lineWidth: 20, lineCap: .round, lineJoin: .round)

    var body: some View {
        Path { path in
            path.move(to: CGPoint(x: 0, y: 0))
            path.addLine(to: CGPoint(x: 0, y: 100))
            path.addLine(to: CGPoint(x: 100, y: 100))
            path.addLine(to: CGPoint(x: 100, y: 0))
            //path.closeSubpath() // clsoes the path above

            // Other path methods include:
            // addArc, addCurve, addEllipse, addLines, addPath,
            // addQuadCurve, addRect, addRects, addRelativeArc,
            // and addRoundedRect
        }
        .stroke(Color.red, style: style)
        .background(Rectangle().fill(.yellow))
        .frame(width: 100, height: 100)
    }
}
```

- `Capsule`: draws an oval
- `Circle`: draws a circle
- `Ellipse`: draws an ellipse

- `Path`

  Like `Color`, `Path` also creates a view.
  The following example creates a path that is both filled and stroked.

  ```swift
  // Define the path as a computed property so it can be
  // used once for filling and once for stroking.
  var path: Path {
      Path { path in
          path.move(to: CGPoint(x: halfWidth, y: halfWidth))
          path.addLine(to: CGPoint(x: 100, y: 100))
          path.addLine(to: CGPoint(x: 200, y: halfWidth))
          path.closeSubpath()
      }
  }

  // Inside some combiner view ...
  ZStack {
      path.fill(.yellow)
      path.stroke(
          .red,
          style: StrokeStyle(
              lineWidth: lineWidth,
              lineCap: .round,
              lineJoin: .round
          )
      )
  }
  ```

- `Rectangle`: draws a rectangle
- `RoundedRectangle`: draws a rectangle with rounded corners

- `ContainerRelativeShape`
- `OffsetShape`
- `RotatedShape`
- `ScaledShape`
- `TransformedShape`

- `AnyShapeShape`

- `AnimatablePair`
- `Animation`
- `AnyTransition`
- `EmptyAnimatableData`

- `Anchor`
- `Angle`

  This is a struct that does not implement the `View` protocol.
  Instances can be created using an initializer
  that takes either a `degrees` or a `radians` argument.
  The value can be obtained via either `degrees` or `radians` properties
  and conversions are performed automatically.

  ```swift
  let angle = Angle(radians: Double.pi)
  print(angle.degrees) // 180.0
  ```

- `ProjectionTransform`
- `UnitPoint`

### Other Views

- `GeometryReader`

  This is a view that takes all the space offered to it, wraps other views,
  and provides its size which can be used in calculations.
  The size is passed to a trailing closure and has the type `GeometryProxy`
  which has a `size` property whose type is `CGSize`.

  The following example gets the size of a `VStack` and displays it inside.

  <img alt="SwiftUI GeometryReader" style="width: 40%"
    src="/blog/assets/SwiftUI-GeometryReader.png?v={{pkg.version}}"
    title="SwiftUI GeometryReader">

  ```swift
  VStack(spacing: 0) {
      // The 3 children below are given equal heights.
      Rectangle().fill(.red)
      GeometryReader { geometry in
          VStack() {
              Text("width = \(geometry.size.width)")
              Text("height = \(geometry.size.height)")
          }
          // This expands the size of the VStack to fill the
          // GeometryReader so the contents are centered.
          .frame(width: geometry.size.width,
              height: geometry.size.height)
           }
      Rectangle().fill(.blue)
  }
  ```

- `Spacer`

  Each of these take an equal amount of the unused space
  inside the parent combiner view.
  It accepts an optional `minLength` attribute which defaults to zero.

  Using `Spacer` can be compared to web applications that use
  the CSS properties `display: flex;` and `justify-content`.

  | CSS justify-content value | Spacer placement              |
  | ------------------------- | ----------------------------- |
  | flex-start                | at end of list of views       |
  | flex-end                  | at beginning of list of views |
  | space-between             | one between each child view   |

- `Divider`

  This draws a light gray 1-pixel wide line across the container.
  The line is vertical in an `HStack` and horizontal in a `VStack`.

  The line can be customized in several ways.

  - To add space around the line, use the `padding` view modifier.

    ```swift
    Divider().padding(20)
    ```

  - To change the color of the line, use the `background` view modifier.

    ```swift
    Divider().background(.red)
    ```

  - To draw a thicker line, use the `frame` modifier.
    This doesn't actually make the `Divider` thicker,
    it just makes the area allocated wider and fills it with a color.

    ```swift
    Divider().background(.blue).frame(height: 20).background(.blue)
    ```

  - To avoid drawing the line all the way across the container,
    use the `frame` view modifier and specify the `maxWidth` attribute.

    ```swift
    Divider().frame(maxWidth: 200)
    ```

TODO: Are `Color` and `LinearGradient` views?

## View Modifiers

View modifiers are methods that can be called on a view to create
a new view that is like the receiver, but modified in a specific way.
Calls to view modifiers can be chained since each returns a new view.

The following example uses the `foregroundColor`, `padding`, and `stroke`
view modifiers.

```swift
Text("Hello, World!").foregroundColor(.red)
RoundedRectangle(cornerRadius: 20).stroke(lineWidth: 3).padding(.all)
```

Some view modifiers can be applied to any view.
Others are specific to certain kinds of views.
For example, the `stroke` view modifier used above only applies to shapes.

The official documentation for the supplied view modifiers can be found at
{% aTargetBlank
"https://developer.apple.com/documentation/swiftui/slider-view-modifiers",
"View Modifiers" %}.

Commonly used view modifiers include:

- `background(alignment, content)`
- `border(ShapeStyle, width: CGFloat = 1)`
- `cornerRadius(CGFloat, antialiased: Bool)`
- `edgesIgnoringSafeArea(Edge.Set)`
- `font(Font?)`
- `foregroundColor(Color?)`
- `foregroundStyle(ShapeStyle)`
- `frame(width: CGFloat?, height: CGFloat?, Alignment)`
- `border(ShapeStyle, width: CGFloat = 1)`
- `lineLimit(Int?)`
- `multilineTextAlignment(TextAlignment)`
- `offset(x: CGFloat, y: CGFloat)`
- `opacity(Double)`
- `overlay(ShapeStyle)`
- `padding(CGFloat)`
- `position(x: CGFloat, y: CGFloat)`
- `rotationEffect(Angle, anchor: UnitPoint)`
- `scaledToFill()`
- `scaledToFit()`
- `scaleEffect(x: CGFloat, y: CGFloat, anchor: UnitPoint)`
- `shadow(color: Color, radius: CGFloat, x: CGFloat, y: CGFloat)`
- `textCase(Text.Case?)`
- `tint(Color?)`
- `transformEffect(CGAffineTransform)`
- `transition(AnyTransition)`
- `truncationMode(Text.TruncationMode)`
- `zIndex(Double)`

The following example adds a shadow a `Text` view.

```swift
Text("After")
    .padding()
    .background(.yellow)
    .shadow(color: .gray, radius: 3, x: 3, y: 3)
```

The event handling methods like `onTapGesture` area also view modifiers.

Several view modifiers take a `ShapeStyle` object.
Types that conform to the `ShapeStyle` protocol include
`AngularGradient`, `Color`, `ForegroundStyle`, `ImagePaint`,
`LinearGradient`, and `RadialGradient`.

Many view modifiers are defined in extensions to the `View` protocol.
This makes them to any kind of views.

When view modifiers are added to combiner views,
they are passed down to all descendant views.
In the following example, all the `Text` views are red
because the `VStack` that contains them has
a view modifier that sets the foreground color.

```swift
VStack {
    Text("Alpha")
    HStack {
        Text("Beta")
        Text("Gamma")
    }
}.foregroundColor(.red)
```

In a way, view modifiers are like Svelte components that contain slots.
They take a view to be "modified" and return a new view
that typically contains the view passed to them.

Custom view modifiers can be created by defining
a struct that implements the `ViewModifier` protocol.
This requires implementing `body` method that takes
content which is a `View` to be modified,
and returns a new `View`.
The code in the `body` method is similar to that in any custom view.

The following code defines a custom `ViewModifier`
that allows the view on which it is called to be collapsed.
It wraps that view in a `VStack` containing two `HStack`s.
The second `HStack` includes a `Button` containing a chevron icon.
Clicking the `Button` toggles whether the first `HStack` is rendered.
It also rotates the chevron icon using animation
which is covered later in the "Animation" section.

<img alt="SwiftUI ViewModifier" style="width: 50%"
  src="/blog/assets/SwiftUI-ViewModifier.png?v={{pkg.version}}"
  title="SwiftUI ViewModifier">

```swift
import SwiftUI

struct Collapsable: ViewModifier {
    private static let diameter = CGFloat(120)
    private static var radius: CGFloat { diameter / 2 }

    var bgColor: Color = .gray
    var duration: Double = 0.5 // in seconds

    @State var showContent = true

    var halfCircle: some View {
        Circle()
            .trim(from: 0, to: 0.5)
            .fill(bgColor)
            .frame(
                width: Collapsable.diameter,
                height: Collapsable.radius
            )
            .offset(x: 0, y: -16)
    }

    private func toggle() {
        withAnimation(.easeInOut(duration: duration)) {
            showContent.toggle()
        }
    }

    func body(content: Content) -> some View {
        VStack {
            if showContent {
                HStack {
                    Spacer()
                    content
                    Spacer()
                }
                .background(bgColor)

                //TODO: Can you scale the height of the HStack
                //TOOO: instead of using the default fade transition?
                //.transition(.scale)
                //.scaleEffect(showContent ? 1 : 0)
                //.animation(.easeInOut(duration: 1))
            }
            HStack {
                Spacer()
                ZStack {
                    Image(systemName: "chevron.down")
                        .resizable()
                        .frame(
                            width: Collapsable.radius / 3,
                            height: Collapsable.radius / 4
                        )
                        .onTapGesture { toggle() }
                        .rotationEffect( // Angle type is inferred
                            .degrees(showContent ? 180 : 0)
                        )
                        .offset(x: 0, y: -2)
                        .background(halfCircle)
                }
                Spacer()
            }
        }
    }
}

extension View {
    func collapsable(
        bgColor: Color = .black,
        duration: Double = 0.5) -> some View {
        modifier(Collapsable(bgColor: bgColor, duration: duration))
    }
}
```

The following code demonstrates using the custom `ViewModifier` defined above.

```swift
VStack {
    Text("First line of content")
    Text("Second line of content")
}
.padding()

// This way of applying a view modifier doesn't use the View extension.
//.modifier(Collapsable(bgColor: ContentView.bgColor))

// This way uses the View extension and is preferred.
.collapsable(bgColor: ContentView.bgColor)
```

## View State

All views are immutable structs.
Typically they get data from a model.
They can also have associated mutable data
by applying the `@State` property wrapper to a property.

Properties declared with `@State` usually include the `private`
access control keyword because the data is only used by that view.
Such data is held outside of the struct and the struct holds a pointer to it.
Changes to these properties cause the view body to be rebuilt.

Any state held in a view using the `@State` property modifier
should be transient state such as data related to styling.
It is recommended to use `@State` sparingly
and prefer holding data in model objects instead.

The following example holds the status of a stoplight
in the "status" state property.
Note the use of `$` before the name to get a two-way binding with a `TextField`.

```swift
import SwiftUI

struct CircleButton: View {
    var color: Color
    var selected: Bool = false
    var action: () -> Void // a function

    var body: some View {
        Button(action: action, label: {
            ZStack {
                Circle().fill(color)
                // Conditional logic can be implemented with an "if" statement,
                // but iteration cannot be implemented with a "for-in" loop.
                // A "ForEach" View must be used instead.
                //TODO: Why the difference?
                if selected {
                    Circle().strokeBorder(Color.black, lineWidth: 5)
                }
            }
        })
    }
}

struct Light: Identifiable {
    let id: String
    let color: Color
}

struct MyTextField: View {
    var label: String
    var text: Binding<String>

    var body: some View {
        TextField(label, text: text)
            .autocapitalization(.none)
            .padding()
            .textFieldStyle(.roundedBorder)
    }
}

// This file must define a struct named "ContentView".
struct ContentView: View {
    @State var status = "stop"

    let lights: [Light] = [
        Light(id: "stop", color: .red),
        Light(id: "yield", color: .yellow),
        Light(id: "go", color: .green)
    ]

    var body: some View {
        VStack {
            // When iterating over elements that do not conform to
            // the `Identifiable` protocol, add the "id:" argument
            // whose value is a key path that specifies
            // how to find something unique in the element.
            ForEach(lights) { light in
                CircleButton(
                    color: light.color,
                    selected: status == light.id
                ) {
                    status = light.id
                }
            }
            // $ in front of status is needed for a two-way binding.
            // $ in front of status is needed for a two-way binding.
            MyTextField(label: "status", text: $status)
        }
    }
}
```

## Layout

Combiner views offer space to their child views.
The child views can choose their size within the space offered to them.
Combiner views then position the contained views knowing their sizes.
Combiner views can also then choose their own size
that perhaps differs from they offered to their child views.

Some views are "inflexible" and want to be a specific size.
Examples including `Text` and `Image` views.
Other views are "flexible" and can adapt to the space offered to them.
Examples include `Circle` and `RoundedRectangle`.

Combiner views give space to inflexible child views first and
then divide the remaining space between the flexible child views.
The priority with which combiner views give space to child views
can be altered by applying the `layoutPriority` view modifier to child views.
It is passed a float value that defaults to zero.

When a combiner view contains at least one flexible view,
it is also considered to be flexible.

Unsafe areas, such as the area at the top of iPhones that have a camera bump,
are removed from offered space by default.
Sometimes it is useful to draw in those areas.
One example, show below, is displaying a background image.

<img alt="SwiftUI Unsafe Areas" style="width: 40%"
  src="/blog/assets/SwiftUI-UnsafeAreas.png?v={{pkg.version}}"
  title="SwiftUI Unsafe Areas">

```swift
VStack {
    Spacer()
    Text("Comet the Whippet")
        .foregroundColor(.white)
        .font(.system(size: 36))
    Spacer()
}
    .background(Image("Comet").resizable().scaledToFill())
    .edgesIgnoringSafeArea(.all)
```

## Event Handling

Views support many methods whose names begin with "on"
to register a function to be called when a given event occurs.
These include:

- `onTapGesture`
- `onLongPressGesture`

- `onDrag`
- `onDrop`

- `onAppear`

  This takes a function to execute every time
  the view on which it is applied is rendered.

- `onDisappear`

- `onCommand`
- `onCopyCommand`
- `onCutCommand`
- `onDeleteCommand`
- `onExitCommand`
- `onMoveCommand`
- `onPasteCommand`
- `onPlayPauseCommand`

- `onChange`
- `onContinueUserActivity`
- `onHover`
- `onOpenURL`
- `onReceive`
- `onSubmit`

## Animation

Many view properties can be animated.
These include color, opacity, rotation, and scale.
Note that font sizes cannot be animated,
but views containing text can be scaled.

SwiftUI supports three ways of implementing animations.

- explicit: wraps code that changes model or `@State` data
  with a call to `withAnimation`
- implicit: uses the `animation` view modifier
- transition: triggers by inserting or removing a view

Explicit animations are the most commonly used
because they are triggered by model/state changes
which are typically made in response to user interactions.
These can cause multiple views to animate concurrently.
For example, the action of a `Button` can wrap calls to
ViewModel intent functions in a closure passed to `withAnimation`.
Most code that handles user events does this.

Key points to remember when implementing animations:

1. Only changes to view modifier arguments and shapes are animated.
2. Only views that are already on the screen
   are affected by explicit and implicit animations.
3. The `animation` view modifier applies to
   all view modifiers chained before it, but not to those chained after it.
4. Explicit animations do not override or prevent implicit animations.
   Both can be applied concurrently.
5. Animations are automatically and smoothly interrupted by new animations.

One way to achieve point #2 is to leave views on the screen permanently,
but conditionally hide them by setting their opacity to zero.
For example, `myView.opacity(show ? 1 : 0)`.

Animations can specify a duration (in seconds), delay, easing function,
and number of times to repeat.
Duration is the total time over which the animation takes place.
Delay is the amount of time the animation waits to begin after being triggered.
An easing function controls the speed at which an animation is applied
over its duration.

Provided easing functions include
`linear`, `easeIn`, `easeOut`, `easeInOut` (default), and `spring`.
These are static functions on the `Animation` struct.
All but `spring` take a single, optional argument
which is the `duration` in seconds.
The `spring` function takes three optional arguments named
`response`, `dampingFunction`, and `blendDuration`.
For details, see {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation/spring(response:dampingfraction:blendduration:)",
"spring" %}.

Custom easing functions can be defined with the `timingCurve` function.

The following example provides form elements
for experimenting with different kinds of animations.

<img alt="SwiftUI Animation" style="width: 40%"
  src="/blog/assets/SwiftUI-Animation.png?v={{pkg.version}}"
  title="SwiftUI Animation">

```swift
enum EasingType: String, CaseIterable {
    case forever, linear, easeIn, easeOut, easeInOut, spring
}

struct ContentView: View {
    @State private var borderColor: Color = .red
    @State private var color = false
    @State private var easingType: EasingType = .linear
    @State private var on = false
    @State private var opacity = false
    @State private var rotate = false
    @State private var scale = false

    private var easingFunction: Animation {
        switch easingType {
        case .linear: return Animation.linear(duration: 1)
        case .forever: return Animation
            .linear(duration: duration)
            .repeatForever(autoreverses: false)
        case .easeIn: return Animation.easeIn(duration: 2)
        case .easeOut: return Animation.easeOut(duration: 2)
        case .easeInOut: return Animation.easeInOut(duration: 2)
        case .spring: return Animation
            .spring(dampingFraction: 0.5)
            .speed(0.3)
        }
    }

    var body: some View {
        VStack {
            VStack {
                Text("First Line")
                Text("Second Line")
                Text("🎉").font(.largeTitle)
            }
            .padding()
            .border(borderColor, width: 10)
            .opacity(!opacity || on ? 1 : 0)
            .scaleEffect(!scale || on ? 1 : 0)
            .rotationEffect(.degrees(!rotate || on ? 0 : 360))
            .animation(easingFunction) // implicit animation

            NavigationView { // Picker will be disabled without this.
                Form {
                    Toggle("Animate Color?", isOn: $color)
                    Toggle("Animate Opacity?", isOn: $opacity)
                    Toggle("Animate Rotation?", isOn: $rotate)
                    Toggle("Animate Scale?", isOn: $scale)
                    Picker("Easing Function", selection: $easingType) {
                        ForEach(EasingType.allCases, id: \.self) { easingType in
                            Text("\(easingType.rawValue)").tag(easingType)
                        }
                    }
                    Button("Toggle") {
                        if color {
                            borderColor =
                                borderColor == .red ? .blue : .red
                        }
                        on.toggle()
                    }
                }
            }
        }
    }
}
```

TODO: Add example of using an explicit animation with `withAnimation`.

TODO: Add an example of using transitions.

Transitions are specified when a view is defined,
but they are only applied when the view is inserted or removed.
This is implemented by using an `if` or `switch` statement inside a parent view.

By default an `opacity` transition (fade) is used.
This can be changed by applying the `transition` view modifier
which is passed the kind of transition to perform.
Transitions are defined as methods on the `AnyTransition` struct.

Transitions can be applied to any kind of view including combiner views.

Typically the provided `opacity`, `scale`, and `slide` transitions are used.
The `identity` transition is used to specify that no transition should occur.
Custom transitions can also be implemented.

By default the transition is reversed when the view is removed.
For example, `opacity` changes from 0 to 1 when the view is inserted,
and from 1 to 0 when it is removed.
This can be changed using the `asymmetric` transition function
which allows specifying one transition for insertion
and a different one for removal.

Most insertion transitions do not currently work in Preview,
but they do work in the Simulator.
However, the Simulator often displays odd rendering artifacts.

The following example demonstrates sliding a view
in from the top when it is inserted and
out to the right when it is removed.

```swift
struct ContentView: View {
    @State private var include = false

    let easeFn = Animation.easeInOut(duration: 1)

    var body: some View {
        VStack {
            VStack {
                if include {
                    Text("Conditionally Included")
                        .frame(maxWidth: .infinity) // use full screen width

                        // This slides in from left and out to right.
                        //.transition(.slide)

                        // The animation call here should apply the
                        // easing function when one is not specified
                        // in the call to withAnimation, but it doesn't work.
                        //.transition(.slide.animation(easeFn))

                        .transition(.asymmetric(
                            insertion: .move(edge: .top),
                            removal: .move(edge: .trailing)
                        ))

                        // This changes scale from 0 to 1 for insertion
                        // and 1 to 0 for removal.
                        //.transition(.scale)
                }
            }
            .frame(maxWidth: .infinity, minHeight: 50) // use full width
            .border(.red)

            NavigationView {
                Form {
                    // If a Toggle is used instead of a Button
                    // to toggle the value of "include",
                    // there is no opportunity to use "withAnimation".
                    //Toggle("Include Optional Text?", isOn: $include)

                    Button("Toggle Optional Text") {
                        //withAnimation {
                        withAnimation(easeFn) {
                            include.toggle()
                        }
                    }
                }
            }

        }
    }
}
```

The `matchedGeometryEffect` view modifier is used
to smoothly move views between combiner views.
For example, this can be used to move `Text` views that describe food items
between lists of foods that available and those that have been selected.
Each food item must have a unique id.
When rendering the food items, `matchedGeometryEffect` is used
to associate the `Text` view with a particular id in a given namespace.
The following example demonstrates this.

TODO: Why do the food names eventually disappear after being moved
TODO: in both Preview and the Simulator?

```swift
struct Food: Identifiable {
    var name: String
    var selected: Bool = false
    var id: String { name }
}

struct ContentView: View {
    @Namespace private var foodNS

    @State var foods: [Food] = [
        Food(name: "Hamburger"),
        Food(name: "Fries"),
        Food(name: "Shake"),
    ]

    func foodList(selected: Bool) -> some View {
        List(foods) { food in
            if food.selected == selected {
                Text(food.name)
                    .matchedGeometryEffect(id: food.id, in: foodNS)
                    .onTapGesture {
                        withAnimation { toggle(food: food) }
                    }
            }
        }
    }

    func toggle(food: Food) {
        let index = foods.firstIndex(where: { $0.id == food.id })!
        foods[index].selected.toggle()
    }

    var body: some View {
        HStack {
            VStack {
                Text("Available")
                foodList(selected: false)
            }
            VStack {
                Text("Selected")
                foodList(selected: true)
            }
        }
    }
}
```

## MVVM

SwiftUI encourages use of the Model-View-ViewModel (MVVM) paradigm
which separates application code into three groups.
This differs from UIKit which encourages use of Model-View-Controller (MVC).

Models ...

- holds data and application logic
- have no knowledge of view code the uses the data

Views ...

- decide what to render
- should be mostly stateless
- are declarative rather than imperative because they describe
  what to render based on the current data, not when to render it
- subscribe to changes in ViewModels
  using the `@ObservedObject` property wrapper
- react to changes published by ViewModels by rebuilding their bodies
- can be associated with any number of ViewModels
- have a `body` var that rebuilds the view
  any time data in ViewModels they use changes
- can call the `onReceive` method to register a function to be called
  when new data is received (couldn't get this to work)

Any state held in a view using the `@State` property modifier
should be transient state such as data related to styling.

ViewModels ...

- are classes (not structs) that implement the `ObservableObject` protocol
- mark the properties whose values they publish
  with the `@Published` property wrapper which is
  a shorthand for explicitly calling `objectWillChange.send()`
- create and hold model instances, typically in `private` properties
- are passed to views
- have no knowledge of the views that use them
- are subscribed to by views to obtain model data
- have methods (referred to as "intents")
  that are called by by views to update model data
- react to model changes by optionally transforming model data
  and publishing changes

For example, a ViewModel could get the result of a SQL query from the Model
and turn it into an array of objects that it publishes to Views.

Read-only data flows from the Model, through the ViewModel, and into the View.
Views call ViewModel functions referred to as "intents"
to notify it about user interactions.
ViewModel methods can trigger Model updates.

Here is a basic example of using MVVM.
It's not exactly MVVM because it combines the Model and ViewModel
into a single class.
This is perhaps acceptable in cases where Model data is
presented as-is to the View without requiring any data transformation.

Note the use of the `@Published` and `@ObservedObject` property wrappers.

<img alt="SwiftUI MVVM demo" style="width: 40%"
  src="/blog/assets/SwiftUI-MVVM.png?v={{pkg.version}}"
  title="SwiftUI MVVM demo">

```swift
// DemoApp.swift
import SwiftUI

@main
struct SwiftUI_MVVMApp: App {
    var model = Model() // defined in Model.swift

    var body: some Scene {
        WindowGroup {
            // This is how the View knows about the model.
            ContentView(model: model)
        }
    }
}
```

```swift
// Model.swift
import Foundation

struct Dog: CustomStringConvertible, Identifiable {
    private static var lastId = 0;

    // This is a computed property required by the CustomStringConvertible protocol.
    var description: String { "Dog: \(name) \(selected)" }

    var breed: String
    var id: Int // required by the Identifiable protocol
    var name: String
    var selected = false

    init(name: String, breed: String) {
        Dog.lastId += 1
        id = Dog.lastId
        self.name = name
        self.breed = breed
    }
}

// This must be a class instead of a struct
// in order to conform to the ObservableObject protocol.
// Things that do this gain an objectWillChange method that publishes changes.
// This can be called directly before changes are made.
// Alternatively, if the properties that can change are annotated with
// the @Published property wrapper, changes will be published automatically.
class Model: ObservableObject {
    // The @Published property wrapper causes changes in properties
    // that are structs, not classes, to be published.
    @Published var dogs: [Dog] = []

    //private let logger = Logger()

    init() {
        // Start with an initial set of dogs.
        dogs.append(Dog(name: "Maisey", breed: "Treeing Walker Coonhound"))
        dogs.append(Dog(name: "Ramsay", breed: "Native American Indian Dog"))
        dogs.append(Dog(name: "Oscar", breed: "German Shorthaired Pointer"))
        dogs.append(Dog(name: "Comet", breed: "Whippet"))
    }

    /// An "intent" that toggles whether a given dog is selected.
    func toggle(_ dog: Dog) {
        // This is not needed if properties have the @Published annotation.
        //objectWillChange.send() // notifies subscribers

        // We can't use the "first" method to find the matching Dog
        // because that would return a copy of the Dog struct.
        // We need to be able to modify the struct in the array, not a copy.
        let index = dogs.firstIndex(where: { $0.id == dog.id })
        if let index = index {
            dogs[index].selected.toggle()
            let msg = "Model.select: selectedDog = \(String(describing: dogs[index]))"
            print(msg)
        }
    }
}
```

```swift
// ContentView.swift
import SwiftUI

// This is a custom view that renders a dog description.
// If the dog is selected, the text is made bold.
struct DogView: View {
    var dog: Dog

    var body: some View {
        let text = Text("\(dog.name) is a \(dog.breed)")
        if dog.selected {
            text.bold()
        } else {
            text
        }
    }
}

struct ContentView: View {
    // Adding the @ObservedObject property wrapper subscribes to changes.
    // Only view bodies that are affected by observed changes are rebuilt,
    // so it is efficient.
    // Selecting a dog mutates this model
    // which causes this view body to be rebuilt.
    @ObservedObject var model: Model

    var body: some View {
        VStack {
            Text("Dogs (\(model.dogs.count))").font(.title).padding()
            VStack(alignment: .leading) {
                ForEach(model.dogs) { dog in
                    DogView(dog: dog).onTapGesture {
                        model.toggle(dog)
                    }
                }
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView(model: Model())
    }
}
```

TODO: Add information about the @EnvironmentObject property wrapper
TODO: which is used to share data between views.
TODO: See https://www.hackingwithswift.com/quick-start/swiftui/how-to-use-environmentobject-to-share-data-between-views

## Alerts

Alerts are simple modal dialogs that contain
a title, an optional message, and buttons.
They are defined using the `alert` method
that can be chained onto any view.
They are displayed when a given binding is set to true.

The buttons to display are described in the `actions` argument.
Like any button, these can have associated actions.

When there are more than two buttons,
they are stacked vertically.
Otherwise they are placed on one row.

If no buttons are provided, a default "OK" button
with a role of `.cancel` is provided.

```swift
struct ContentView: View {
    @State var dogCount = 0
    @State var pressed = false
    @State var tooManyDogs = false

    var body: some View {
        VStack {
            Button("Press Me") { pressed = true }
            HStack {
                Stepper(
                    "# of Dogs: \(dogCount)",
                    value: $dogCount, in: 0...10
                ) { v in
                    tooManyDogs = dogCount > 2
                }
            }.padding()
        }
        .alert(
            "My Title",
            isPresented: $pressed,
            actions: {}, // no custom buttons
            message: {
                Text("My Message")
            }
        )
        .alert(
            "Dog Alert",
            isPresented: $tooManyDogs,
            actions: {
                Button("OK") {
                    print("pressed OK")
                }
                Button("Keep") {
                    print("pressed Keep")
                }
            },
            message: {
                // Can only have a single Text view here,
                // not any kind of View!
                Text("You have too many dogs!")
            }
        )
    }
}
```

## Modal Dialogs

Basic modal dialogs can be created using the
`alert` and `confirmationDialog` view modifiers

Custom modal dialogs are implemented by displaying a "sheet".
The sheet slides in from the bottom by default.

The following example defines the custom view `MyModal`
which is displayed in the sheet.
Custom views, like any struct, can have properties
that are passed in when instances are created.
The `ContentView` struct declares the boolean property `showModal`
and passes it to the `MyModal` struct as a binding.
This allows the action of the "Close" button in `MyModal`
to set it to false which hides the sheet.

```swift
import SwiftUI

struct MyModal: View {
    //TODO: Add more detail on using the @Binding property wrapper.
    @Binding var show: Bool
    var message: String

    var body: some View {
        VStack {
            Text("My Modal").font(.title)
            Spacer()
            Text(message).padding(20)
            Spacer()
            Button("Close") { show = false }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(.black.opacity(0.2))
    }
}

struct ContentView: View {
    @State private var count = 0
    @State private var message = "Opened once"
    @State private var showModal = false

    var body: some View {
        VStack {
            Text("Main View")
            Button(action: {
                count += 1
                message = "Opened \(count) times"
                showModal = true
            }) {
                Text("Show Modal")
                .padding()
                .background(.yellow)
                .cornerRadius(10)
            }
            .sheet(isPresented: $showModal) {
                // By default the sheet slides in from the bottom.
                MyModal(show: $showModal, message: message)
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
```

## Navigation

The `NavigationView` view marks the spot where
the views associated with `NavigationLinks` will be rendered.
The example below renders four pages.
Page 1 contains links to pages 2 and 3.
Page 3 contains a link to page 4.

```swift
import SwiftUI

struct Page1: View {
    var body: some View {
        VStack {
            Text("This is page one.")
                .navigationTitle("Page 1")
                .navigationBarTitleDisplayMode(.inline)
                // .large leaves a large amount of space above
            Spacer()
            HStack {
                NavigationLink(destination: Page2()) {
                    Text("Page 2")
                }
                NavigationLink(destination: Page3()) {
                    Text("Page 3")
                }
            }
        }
    }
}

struct Page2: View {
    var body: some View {
        Text("This is page two.").navigationTitle("Page 2")
    }
}

struct Page3: View {
    var body: some View {
        VStack {
            Text("This is page three.").navigationTitle("Page 3")
            Spacer()
            NavigationLink(destination: Page4()) {
                Text("Page 4")
            }
        }
    }
}

struct Page4: View {
    var body: some View {
        Text("This is page four.").navigationTitle("Page 4")
    }
}

struct ContentView: View {
    var body: some View {
        NavigationView {
            Page1()
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
```

If the view inside a `NavigationLink` doesn't have a `navigationTitle`,
the link to get back to it will just display "Back".

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

All objects have the property `self` that refers to the whole object.
To write a key path that refers to the whole object
rather than a specific property inside it, use `\.self`.

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

## UserDefaults

The `UserDefaults` class is "an interface to the user's defaults database,
where you store key-value pairs persistently across launches of your app."
The code below demonstrates using this to persist data about dogs as JSON.
TODO: Can you store an array objects directory without encoding as JSON?

```swift
struct Dog: Codable, CustomStringConvertible, Identifiable {
    var name: String
    var breed: String
    var description: String { "\(name) is a \(breed)"}
    var id: String { name }

    // Can also use this to specify different names for the JSON keys.
    enum CodingKeys: CodingKey {
        case name
        case breed
    }
}

func deleteData(for key: String) {
    UserDefaults.standard.removeObject(forKey: key)
}

func getData<T>(for key: String, defaultingTo defaultValue: T) -> T where T: Decodable {
    if let data = UserDefaults.standard.data(forKey: key) {
        print("init: data =", data)
        if let decoded = try? JSONDecoder().decode(T.self, from: data) {
            return decoded
        }
    }
    return defaultValue
}

func setData<T>(for key: String, to value: T) where T: Encodable {
    if let encoded = try? JSONEncoder().encode(value) {
        if let json = String(data: encoded, encoding: .utf8) {
            print("setData: json =", json)
        }
        UserDefaults.standard.set(encoded, forKey: key)
    }
}

struct ContentView: View {
    private static let KEY = "dogs"

    @State private var breed = ""
    @State private var name = ""
    @State private var dogs: [Dog] // can't initialize this to an empty array!

    init() {
        dogs = getData(for: ContentView.KEY, defaultingTo: [])
        print("init: dogs =", dogs)
    }

    func deleteAll() {
        deleteData(for: ContentView.KEY)
        dogs = []
    }

    func deleteDogs(at offsets: IndexSet) {
        dogs.remove(atOffsets: offsets)
        save()
    }

    func save() {
        setData(for: ContentView.KEY, to: dogs)
    }

    var body: some View {
        NavigationView {
            VStack {
                Section(header: Text("New Dog")) {
                    Form {
                        TextField("Name", text: $name)
                        TextField("Breed", text: $breed)
                        Button("Add") {
                            dogs.append(Dog(name: name, breed: breed))
                            save()
                            name = ""
                            breed = ""
                        }
                    }
                }
                Section(header: Text("Current Dogs")) {
                    List {
                        ForEach(dogs) { dog in
                            Text(String(describing: dog))
                        }
                        .onDelete(perform: deleteDogs)
                    }
                    Button("Delete All", action: deleteAll)
                }
                Spacer()
            }.navigationBarTitle("Dog Collection", displayMode: .inline)
        }
    }
}
```

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
While it is possible to add the use of Core Data to an existing app,
the setup steps are quite involved.

The generated files setup use of Core Data.
The file `{app-name}App.swift` registers an "environment"
with the main `ContentView` that uses
a `PersistenceController` instance defined in `Persistence.swift`
and made available through a static `shared` property.
The main view defined in `ContentView.swift` has access to this
and uses the `@FetchRequest` property wrapper
to get `Item` objects from the persistent store.
The `Item` type is defined in `{app-name}.xcdatamodeld`
and has a single attribute `timestamp`.

The generated app is fully functional.
It displays a list of items that each have a timestamp.
Tap an item to get to a page that displays detail about just that item.
To delete an item, swipe it left and click the "Delete" button that appears.
To add an item with the current timestamp, click the "+" button at the top.
To delete items without swiping them, click the "Edit" button at the top.
This displays a red circle containing a minus sign to left of each item.
Click those to delete items.
When finished, click the "Done" button at the top
that replaced the "Edit" button.
From here you can edit the code to provide your own CRUD functionality.

To define additional entity types that can be persisted,
and possibly delete the provided `Item` type,
click the `{app-name}.xcdatamodeld` file in the Navigator pane.
This will display an entity editor.

To add entities, click the "Add Entity" button at the bottom.
This will create an empty entity named "Entity".
Double-click the name to change it.
These become the names of generated class definitions,
so they should begin with an uppercase letter.
The generated classes inherit from `NSManagedObject`.

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
