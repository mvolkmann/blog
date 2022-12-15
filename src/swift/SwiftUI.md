---
eleventyNavigation:
  key: SwiftUI
  parent: Swift
  order: 3
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/xcode/swiftui/", "SwiftUI" %}
is a Swift library for building macOS, iOS, and Apple Watch apps.
It is an alternative to its predecessor
{% aTargetBlank "https://developer.apple.com/documentation/uikit", "UIKit" %}.

By comparison SwiftUI ...

- is declarative in nature than imperative
- emphasizes use of structs over classes (UIKit uses classes)
- requires far less code to do the same things
- has somewhat better performance
- only runs on iOS 13 and above
- is currently missing some features of UIKit
  (ex. SwiftUI `Text` is less powerful than UIKit `UITextView`.)
- doesn't use Storyboard to build views
- doesn't use `AppDelegate` or `SceneDelegate`
  (read about these {% aTargetBlank
  "https://learnappmaking.com/scene-delegate-app-delegate-xcode-11-ios-13/",
  "here" %})

Much of the same code can be used to build apps across all Apple platforms.
However, there are platform differences that require different code.

Many SwiftUI views are built on UIKit components,
but knowing their relationships is not necessary.

It is possible to use UIKit components in a SwiftUI app
by wrapping them in a struct that conforms to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/uiviewrepresentable",
"UIViewRepresentable" %} protocol.
However, there is enough functionality in SwiftUI
that this is typically not necessary.

## Getting Started

- open Xcode which includes Swift and SwiftUI
- select "Create a new Xcode project"
- select an Application type such as "App" and click "Next"
- enter a Product Name
- select a Team such as "your name (Personal Team)"
- enter an Organization Identifier such as your email address
- select the Interface such as "SwiftUI"
- select the Language such as "Swift"
- check "Use Core Data" to enable persisting data in a local database
- check "Include Tests" to get a jump start on enable writing unit tests
- click "Next"
- select the directory where the project will be stored and click "Create"

The new project will have the following structure and files:

A new "App" project begins with a group that has the same name as the project.
The group contains the following files:

- `{ProjectName}App.swift`

  This defines the main struct which implements the `App` protocol.
  It is preceded by the `@main` attribute
  which marks the entry point of the app.
  It has computed property named `body`
  whose value implements the `Scene` protocol.
  The actual value is an instance of `WindowGroup`.
  This renders an instance of `ContentView` which is defined in the next file.
  Apps for macOS and iPadOS can specify more than one window
  in the `WindowGroup`, but iOS and watchOS apps cannot.
  iOS will render all the windows as if they were in a `VStack`.

- `ContentView.swift`

  This defines the `ContentView` struct which implements the `View` protocol
  and is the topmost view.
  It also defines the `ContentView_Previews` struct
  which describes the previews that should display in the Canvas area.
  This can be one or more views that are each displayed in a separate Preview.

- `Assets.xcassets`

  This associates names with assets such as colors, images, and data files.
  It is not used for audio and video files.

- `Preview Content` group

  This contains the file `Preview Assets.xcassets`
  which holds assets that are only used to previews of views.

- `{ProjectName}.xcodeproj`

  This file is not visible in the Navigator,
  but can be viewed and edited by clicking the top entry in the navigator.
  The editor for this data contains the following tabs:
  General, Signing & Capabilities, Resource Tags, Info,
  Build Settings, Build Phases, and Build Rules.
  The General tab configures the app display name, version of iOS,
  targets (iPhone, iPad, or Mac), status bar style, and more.

To add a particular kind of provided UI View in a `.swift` file,
manually enter code or click where it should go in the code
and click the "+" in the upper-right of the Code Editor.
Clicking the "+" displays the Object Library dialog
that contains a list provided view types.
Selecting one displays related documentation and code examples.
Double-click a view type to insert code for it at the cursor
or drag a view type to the desired code location.

To change the device being simulated,
click the device drop-down after the app name near the top
and select the device type (such as "iPhone 13").

Press the black, right-pointing triangle near the top
to build and run the initial version of the app in the simulator.

To change the target version of iOS,
click the app name at the top of the Navigator,
click the "General" tab, and
select an iOS version from the dropdown in the "Deployment info" section.

To change the name displayed below the app icon,
click the app name at the top of the Navigator,
select the app target, and
modify the value of "Display Name".

### Canvas / Preview

The Canvas area displays Previews of the UI running outside of a simulator.
To hide/show the Canvas area, select Editor ... Canvas
or press cmd-option-return.
If Preview isn't running inside the Canvas area,
press the "Resume" button at the top to start it
or press cmd-option-p.

By default a preview of the view is only displayed
when the file that defines it is selected in the Navigator.
To keep the preview of a specific view in the Canvas area
even when a different file is selected,
select its source file in the Navigator and
click the pin icon in the lower-left of the Canvas.

When code changes are saved and there are no errors,
the Preview is automatically updated.
When there are errors, the Preview pauses and must be manually restarted
by click the "Resume" button or pressing cmd-option-p.

If the Canvas area displays the message "Failed to build" in the upper-left,
click the "Diagnostics" button in the upper-right to see error messages.

Typically views are added to the `body` of the currently selected source file
by typing code into the editor pane.
Another way to do this is to:

- Open the Preview.
- Open the Library (press cmd-shift-l).
- Select the "Show the Views Library" tab.
- Drag a view over the Preview without dropping it.
- Note the popup below the Preview that indicates how it will be added.
- Drop the view onto the Preview.
- Configure the view in the Inspector on the right.

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
  This is a great way to learn about supported view modifiers and their syntax.

To switch to "Live Preview" mode so tap events are honored,
click the button with a triangle inside a circle.
This also enables scrolling by dragging
which is not possible in the default mode.
Clicking this also triggers the Preview to resume if it is paused
and rebuild the code.

Output from `print` calls does not appear in the Debug area
when interacting with a preview.

To rotate the display to landscape mode,
click the button above the display that contains
a square with a curved arrow on its upper-left corner.
This only works when not in "Live Preview" mode.
The same button appears in the Simulator.

It is possible for the Preview area to show more than one preview.
This is controlled by the `ContentView_Previews` struct
defined in `ContentView.swift`.
For example, the following renders
`ContentView` in portrait orientation and light mode,
`ContentView` in landscape orientation and dark mode.

```swift
struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
            .previewInterfaceOrientation(.portrait)
            .preferredColorScheme(.light)
        ContentView()
            .previewInterfaceOrientation(.landscapeLeft)
            .preferredColorScheme(.dark)
    }
}
```

Note that if any of the previews are in "Live Preview" mode,
only that preview will be displayed.
Exit out of that mode to get the other previews to display again.

To reduce the size of previews to only show their content
and not the surrounding device chrome,
call `.previewLayout(.sizeThatFits)`.

### Inspector

The Inspector shows commonly used modifiers of the selected view.
To see additional modifiers,
click "Add Modifier" at the button of the Inspector.
Optionally type part of a modifier name to filter the list.
Click a modifier to add it to the Inspector.
To stop displaying a modifier in the Inspector,
hover over it and click the "Delete" button that appears.

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

## SF Symbols

SF Symbols is a macOS app from Apple that provides over 4,000 icons.
These can be rendered in custom app using the `Image` view
with the `systemName` argument.

Some symbols support multiple symbol rendering modes
that enable using different colors for parts of the icon.
To see the available rendering modes for a given icon,
select the icon and click the paint brush tab in the Inspector on the right.
Each icon has a preferred rendering mode,
but a different rendering mode can be selected by applying the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/image/renderingmode(_:)",
"renderingMode" %} view modifier.

To specify a symbol rendering mode, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/symbolrenderingmode(_:)",
"symbolRenderingMode" %}. The supported symbol rendering modes include:

- `.monochrome` - single color; seems the same as hierarchical
- `.hierarchical` - single color with multiple opacity levels
- `.palette` - two or three colors
- `.multicolor` - seems the same as hierarchical

When the symbol rendering mode is not specified,
the preferred rendering mode of the icon of is used.

Icons that support variable colors are grouped in the "Variable" category.
These display additional parts of the icon as a percentage value increases.
To see this in action:

- Select one of the icons in the "Variable category.
- Select the paint brush tab in the Inspector on the right.
- Select any rendering mode.
- Activate variable color mode by clicking the
  button to the left of the percentage slider.
- Drag the slider to change the percentage value.
- Note that all the displayed icons update to show the effect.

Here's an example of using such an icon:

```swift
@State private var percent = 0.0
...
Image(systemName: "cellularbars", variableValue: percent)
Button("Decrease") {
    if percent > 0 { percent -= 0.1 }
}
Button("Increase") {
    if percent < 1 { percent += 0.1 }
}
```

See the {% aTargetBlank
"https://github.com/mvolkmann/SFSymbolsDemo/tree/main", "SFSymbolsDemo" %}
app in GitHub.

To ignore the colors in a multicolor SF Symbol icon
and just use it as a template for applying a single specified color,
apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/image/renderingmode(_:)",
"renderingMode" %} view modifier with the argument value `.template`.
For example:

```swift
Image(systemName: "doc.fill.badge.plus")
    .renderingMode(.template)
    .resizable()
    .foregroundColor(.pink)
    .scaledToFit()
    .frame(width: 100, height: 100)
```

## Views

Views in SwiftUI are structs that conform to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view", "View" %} protocol.
The only requirement this imposes is that conforming types
must define a `body` computed property whose type is `some View`.
The `some` keyword describes an "opaque type".
In this case it means that `body` an return
an instance of any type that conforms to the `View` protocol.

A `body` definition can contain any number of top-level views.
These are automatically wrapped in a {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/tupleview", "TupleView" %}
which becomes the single view that is returned.
By default this positions its child views vertically like a `VStack`.

The following sections describe the views defined by SwiftUI.

### Container Views

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/action-sheets/",
    "Action Sheet" %}

  - an alert that presents two or more choices related to the current context
  - on small screens, slides in from bottom
  - on large screens, appears as a Popover
  - can use to request confirmation before a destructive operation
  - SwiftUI creates this with `ActionSheet`, but that is deprecated.
    TODO: What takes its place? Perhaps it must be created using `Sheet`.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/activity-views/",
    "Activity View" %}

  - a set of activity buttons applicable in current context
    such as Copy, Add, or Find
  - also referred to as a "share sheet"
  - appears as a Sheet or Popover
  - SwiftUI creates this with what?
    TODO: Is this supported in SwiftUI? Maybe UIKit is required.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/alerts/",
    "Alert" %}

  - a modal dialog with a title, optional message,
    one or more buttons, and optional input text fields
  - avoid having more than two buttons
  - minimize usage to important situations
  - SwiftUI creates this with the `alert` view modifier
    that can be called on any kind of view.
    See the "Alerts" section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/collections/",
    "Collection" %}

  - manages an ordered set of content,
    like photos presented as a grid of thumbnails
  - tap an item to select
  - touch and hold an item to edit
  - swipe to scroll
  - SwiftUI creates this with what?
    TODO: Is this supported in SwiftUI? Maybe UIKit is required.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/image-views/",
    "Image View" %}

  - displays a single image or an animated sequence
  - can fill the entire display
  - SwiftUI creates this with `Image`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/pages/",
    "Page" %}

  - implements linear navigation between a set of related pages
    using either scrolling or page curl effects
  - SwiftUI provides this in `TabView` when it has a
    `tabViewStyle` view modifier with a value of `.page`.
    See an example in the `TabView` section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/popovers/",
    "Popover" %}

  - a modal or non-modal dialog displayed in response to
    tapping a control or tapping in an area
  - typically renders a "tail" pointing to what triggered it
  - can contain many kinds of elements including
    Navigation Bars, Toolbars, Tab Bars, and more
  - avoid using on iPhones
  - SwiftUI creates this with the `popover` view modifier

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/scroll-views/",
    "Scroll View" %}

  - scrolls content larger than the visible area
  - displays transient scrolling indicators
  - can operate in paging mode
  - can support zooming
  - SwiftUI creates this with `ScrollView`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/sheets/",
    "Sheet" %}

  - a card that partially covers the primary content
  - used to perform a task related to current context
  - top corners are rounded and can customize the radius
  - there are two available heights, large (default) and medium
  - modal by default
  - SwiftUI creates this with the `sheet` view modifier.
    See an example in the "Modal Dialogs" section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/split-views/",
    "Split View" %}

  - presents hierarchical data with two or three columns:
    primary, supplementary, and content
  - selections in primary cause changes in supplementary
  - selections in supplementary cause changes in content
  - highlights current selections
  - used in Mail app where primary is a list of mailboxes,
    supplementary is a list of messages in the selected mailbox,
    and content is the content of the selected email
  - SwiftUI doesn't directly support this, but
    similar functionality can be implemented using `NavigationView`.
    This displays side-by-side views on iPads in landscape mode.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/tables/",
    "Table" %}

  - a scrolling, single-column of rows that are each divided into sections
  - can use for navigation in a Split View
  - three styles: plain, grouped, and inset grouped
  - not a data grid like in other UI frameworks
  - SwiftUI uses the `List` view in place of the UIKit `UITableView` view.
    `List` is easier to use.
    It doesn't require specifying the number of rows,
    and cells don't need to be configured.
    Instead a `List` is composed of rows that each
    know how to arrange and display their data.
    See the "List" section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/text-views/",
    "Text View" %}

  - multi-line styled text
  - optional scrolling
  - can control alignment, font, and color
  - can be editable and if so can specify a keyboard type;
    see the {% aTargetBlank
    "https://developer.apple.com/documentation/uikit/uikeyboardtype",
    "UIKeyboardType enum" %}
  - SwiftUI creates this with a combination of `Text` and `AttributedString`.
    See the `AttributedString` section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/views/web-views/",
    "Web View" %}

  - renders embedded HTML or HTML from a web site
  - can enable forward and backward navigation
  - SwiftUI does not include support for web views.
    `WKWebView` can be used, but setting it up in SwiftUI app is not trivial.

### Controls

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/buttons/",
    "Button" %}

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
  - can have a `role` of `normal`, `primary`, `cancel`, or `destructive`
  - can be a close button (x in a circle) that closes the parent view
  - can be an info button (i in a circle) that displays
    information about the parent view
  - SwiftUI creates this with `Button`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/color-wells/",
    "Color Well" %}

  - displays a currently selected color in a circle
  - when tapped, displays the system color picker to change the selected color
  - SwiftUI creates this with `ColorPicker`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/context-menus/",
    "Context Menu" %}

  - displays a menu of context-sensitive items when the menu label is tapped
  - can contain sub-menus
  - can include separators to group options
  - SwiftUI creates this with `Menu`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/edit-menus/",
    "Edit Menu" %}

  - a provided horizontal menu that is displayed when a long press
    occurs in a text field, text view, web view, or image view
  - by default contains buttons for Cut, Copy, Paste, Select, Select All, Delete, Replace..., Look Up, and Share...
  - can disable any of the default buttons to remove them
  - SwiftUI provides this automatically when a long press occurs
    in a `TextField` or `TextEditor`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/labels/",
    "Label" %}

  - renders an icon and/or text
  - takes a String for the text and an `systemImage` string
    that is the name of an icon in SF Symbols
  - by default displays both
  - to render only the text, add the modifier `.labelStyle(TitleOnlyLabelStyle)`
  - to render only the icon, add the modifier `.labelStyle(IconOnlyLabelStyle)`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/page-controls/",
    "Page Control" %}

  - a row of indicator images that represent pages in a list
  - can handle any number of pages
  - image for current page is highlighted
  - can customize images
  - SwiftUI provides this in `TabView` when it has a
    `tabViewStyle` view modifier with a value of `.page`.
    See an example in the `TabView` section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/pickers/",
    "Picker" %}

  - displays distinct values in a scrollable list and allows users to select one
  - SwiftUI creates this with `Picker` and `DatePicker`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/progress-indicators/",
    "Progress Indicator" %}

  - informs users that an activity is running in the background
  - can indicate how much longer it will run using a progress bar
  - "activity indicators" are for indeterminate activities
  - SwiftUI creates this with `ProgressView`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/refresh-content-controls/",
    "Refresh Content Control" %}

  - becomes visible when a user pulls down on a view to request a content reload
  - usually used in a table view
  - SwiftUI creates this with the `refreshable` view modifier
    which is often used on `List` views

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/segmented-controls/",
    "Segmented Control" %}

  - a set of toggle buttons
  - can use in place of web UI radio buttons
  - can use to select between different kinds of views
  - SwiftUI creates this with `Picker(...).pickerStyle(.segmented)`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/sliders/",
    "Slider" %}

  - a horizontal track with a thumb that
    slides between minimum and maximum values
  - can display text and/or icons on leading and trailing ends
  - SwiftUI creates this with `Slider`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/steppers/",
    "Stepper" %}

  - pair of minus and plus buttons that decrement and increment a value
  - can replace "-" and "+" with images
  - doesn't display current value
  - SwiftUI creates this with `Stepper`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/switches/",
    "Switch" %}

  - a toggle between mutually-exclusive states
  - can set tint to match app theme
  - usually used in table rows with a label on the leading side
  - SwiftUI creates this with `Toggle`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/controls/text-fields/",
    "Text Field" %}

  - single-line text input with fixed height
  - usually has rounded corners
  - brings up on-screen keyboard when tapped
  - can contain placeholder text which is preferred over preceding with a label
  - optionally includes a clear button (x in a circle)
  - can mask inputs like passwords
  - can include images on leading and/or trailing sides
  - can specify a keyboard type defined by the {% aTargetBlank
    "https://developer.apple.com/documentation/uikit/uikeyboardtype",
    "UIKeyboardType enum" %}
  - SwiftUI creates this with `TextField`, `SecureField`,
    and `TextEditor` (multi-line)

## Icons

{% aTargetBlank "https://developer.apple.com/sf-symbols/", "SF Symbols" %}
is a library of over 4000 icons provided by Apple.
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

The size, weight, and color of an SF Symbols icon
can be specified using the same view modifiers
as are used to style `Text` views.

Some SF Symbols do not have a 1-1 aspect ratio,
so using the `frame` modifier with the same values for `width` and `height`
can skew them.

Some SF Symbols support multiple colors and optional parts.
For example:

```swift

```

To use FontAwesome icons,
select File ... Add Packages...
and enter the URL https://github.com/onmyway133/FontAwesomeSwiftUI
in the search input in the upper-right.
Select FontAwesomeSwiftUI and press the "Add Package" button.

To render a FontAwesome icon, call `FontAwesome.register()`
and pass icons to the `Text` view.
For example, `Text(AwesomeIcon.aws.rawValue)`.

### Bars

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/navigation-bars/",
    "Navigation Bar" %}

  - appears at top of screen below status bar
  - enables navigation through hierarchical screens
  - provides a leading back button
  - can have trailing buttons like "Edit" and "Done"
  - can have a tint color
  - can have an inline or large title
  - can use a "Segmented Control" in place of title
  - SwiftUI creates this with `NavigationView`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/search-bars/",
    "Search Bar" %}

  - a text input for entering search text
  - has magnifier glass icon
  - can display in a Navigation Bar
  - can include clear and confirm buttons
  - SwiftUI creates this with the `searchable` view modifier
    can be applied to a view that is inside a `NavigationView`.
    See an example in the "Search" section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/sidebars/",
    "Sidebar" %}

  - provides app-level navigation to top-level collections of content
  - for example, the Mail app displays a list of mailboxes in a sidebar
  - selecting an item in the sidebar changes what is displayed
    in the pane that follows
  - SwiftUI creates this by applying the `.listStyle(.sidebar)` view modifier
    to a `List` view. See an example in the "Sidebar" section.

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/status-bars/",
    "Status Bar" %}

  - appears at top of screen above Navigation Bar
  - the system provided Status Bar displays the time on the left and indicators
    for cell strength, WiFi strength, and battery remaining on the right
  - can style to light or dark mode and customize colors
  - should not replace with a custom status bar
  - can temporarily hide it, but should never permanently hide it
  - to hide the system status bar, apply the `.statusBar(hidden: true)`
    view modifier to the top `NavigationView`
  - SwiftUI needs nothing to support this because the system provides it

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/tab-bars/",
    "Tab Bar" %}

  - used to navigate to top-level app sections (groups of related pages)
  - appears at bottom of screen
  - SwiftUI creates this with `TabView`

- {% aTargetBlank
    "https://developer.apple.com/design/human-interface-guidelines/ios/bars/toolbars/",
    "Toolbar" %}

  - contains buttons that perform page-specific actions
  - appears at bottom of screen
  - SwiftUI creates this with the `toolbar` view modifier
    and `ToolbarItemGroup` or `ToolbarItem` views.
    See an example in the "Toolbars" section.

## Core Graphics (CG)

Several types used in views have names that begin with "CG"
which stands for "Core Graphics". These include:

- `CGAffineTransform`

  This holds a transformation matrix used in 2D transformations.

- `CGColor`

  `CGColor` is a type used by Core Graphics to represent colors.
  It is typically only used to apply color operations in
  Core Graphics contexts like CGImage and CGLayer.

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

Each view is a struct that implements the `View` protocol.
This requires having a computed property named `body` with the type `Some View`.

Most properties declared in a `View` struct should be `private`.
Exceptions include the `body` property and
any properties that are passed in when instances are created.
The `body` value is a `ViewBuilder` that
can contain up to 10 child view instances.
When this limit is exceeded, the rather unhelpful error
"Extra argument in call" will be reported.
The `Group` view can be used to work around this limitation
since each `Group` only counts as one view instance.

Views are used for both components and layout.
Views that layout other views are often referred to as
"container" or "combiner" views.

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
For example, `myView.frame(maxWidth: .infinity, maxHeight: 100)`.

Many views utilize the app accent color, also referred to as "global tint".
To set this, select the `Assets.xcassets` file in the Navigator,
select "AccentColor", click the "Universal" swatch,
and click the "Color" dropdown in the Inspector.
Resume Preview to see the change.
This does not affect `Toggle` views which required using the view modifier
`.toggleStyle(SwitchToggleStyle(tint: someColor))`.

### Frames

To specify the width, height, and alignment of a view, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/frame(width:height:alignment:)",
"frame" %} view modifier.
The frame of a view is only considered for determining layout.
To clip a view so any content outside its frame is hidden,
apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/clipped(antialiased:)",
"clipped" %} view modifier.

### Constants

Custom views often make use of constants.
One recommended way to define constants that are used by multiple source files
is to place them in the file `Constants.swift`.
The contents of this file could be similar to the following:

```swift
import SwiftUI

struct Constants {
    static let someValue = 19

    struct Colors {
        // This uses a custom Color initializer takes a hex parameter.
        // It is defined in a Color extension in the Colors section below.
        static let background = Color(hex: 0xf1faff)
        static let disabled = Color.gray
        static let primary = Color(hex: 0x006197)
        static let secondary = Color(hex: 0x3eb3e5)
    }
}
```

Note how a nested struct is used to group related constants.
An example reference to one of these is `Constants.Colors.primary`.

### Executing Code When View Appears/Disappears

There are three ways to specify code to run
when a view is created or first appears.

1. Place code in the view initializer (`init`).
1. For synchronous code, apply the `onAppear` view modifier
   to the outermost view returned by the `body` function,
   passing it a closure.
1. For asynchronous code, apply the `task` view modifier
   to the outermost view returned by the `body` function,
   passing it a closure.

The `task` view modifier takes a closure that is run in an asynchronous content
and the code inside it can use the `await` keyword.
If the user navigates away from the view before the code is run,
certain asynchronous tasks such as network requests are automatically cancelled.

To rerun the code in a `task` closure every time the value of
an observable property such as a `@State` variable changes,
include the `id` argument.

By default the task is run at the highest priority.
To give it a lower priority, include the `priority` argument.

The `onAppear` view modifier also takes a closure,
but its code can only use the `await` keyword if it is wrapped in a `Task`.

To run code when a view disappears, apply the `onDisappear` view modifier,
passing it a closure.

To run code the app launches, add an `init` method to the main struct
that inherits from `App` and place the code there.
This is an ideal place to set default `UsersDefaults` data
if it has not yet been set.

### Extracting Views

When the `body` of a view is longer than what can be displayed on the screen
it is a good idea to extract some of the content.
There are three options for doing this.

1. Move some of the content to a computed property.
2. Move some of the content to a method.
3. Move some of the content to a new struct that
   inherits from `View` and has its own `body` property.

Creating a computed property is useful for cases where the extracted code
is only useful in the current view and no arguments need to be provided.
Creating a method is similar, but allows arguments to be passed.
Creating a new struct is useful when the new view might be used by other views.
In this case, consider moving it to its own source file.

Command-click a view to get a context menu that contains the options
"Extract to Variable", "Extract to Method", and "Extract Subview".

### @State Property Wrapper

All views are immutable structs.
Typically they get data from a model object.
They can also have associated mutable data
by applying the `@State` property wrapper to a property.
This essentially creates a constant pointer inside a view struct
to non-constant data held outside the view struct.
Changes to these properties cause the view body to be rebuilt.

Properties declared with `@State` usually include the `private`
access control keyword because the data is only used by that view.

Updates to `@State` properties should occur in the main queue.
To do this from asynchronous code, wrap the update as follows:

```swift
DispatchQueue.main.async {
    myState = newValue
}
```

To initialize a state property based on
data passed to an initializer (`init` method),
prefix the state name with an underscore
and set it to a `State` object.

The following example demonstrates using component state
to implement a counter view:

```swift
import SwiftUI

struct Counter: View {
    @State private var count = 0

    init() {} // starts with default count value

    init(start: Int) {
        _count = State(initialValue: start)
    }

    // a computed property
    var body: some View {
        // HStack is a container view that arranges it children horizontally.
        HStack {
            Button("-") { count -= 1 }
            Text("\(count)")
            Button("+") { count += 1 }
        }.font(.system(size: 24))
    }
}

struct ContentView: View {
    var body: some View {
        // VStack is a container view that arranges it children vertically.
        VStack {
            Counter()
            Counter(start: 7)
        }
    }
}
```

The following example holds the status of a stoplight
in a state property named "status".
Note the use of `$` before the name to
get a two-way binding with a `TextField`.

<img alt="SwiftUI Stoplight" style="width: 50%"
  src="/blog/assets/SwiftUI-Stoplight.png?v={{pkg.version}}"
  title="SwiftUI Stoplight">

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
    @State private var status = "stop"

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
            MyTextField(label: "status", text: $status)
        }
    }
}
```

## ViewBuilders

A {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/viewbuilder",
"ViewBuilder" %} is a parameter type that is function
that takes no arguments and returns a `View`.
It uses list-oriented syntax to describe a list of
one to ten other views that are combined into a single view.
Note that a `ForEach` view counts as a single view.

A `ViewBuilder` is a kind of result builder.
For more information on these, see the {% aTargetBlank
"https://github.com/apple/swift-evolution/blob/main/proposals/0289-result-builders.md",
"Result builders proposal" %}.

The last argument of container view initializers has the type `ViewBuilder`.
This allows a list of child views to be supplied using a trailing closure.

`ViewBuilder` blocks are parsed differently by the compiler
than ordinary closures.
In addition to containing views,
they can also contain local variable declarations and
conditional logic with `if`, `if let`, and `switch` statements.
However, no other Swift syntax is allowed inside them.

For iteration in a `ViewBuilder`, use a `ForEach` `View`.
There is no corresponding view for conditional logic,
so `if` and `switch` statements are used instead.

Functions that have a return type of `some View`
must always return the same kind of view.
When conditional logic is used inside a function
to decide which kind of `View` to return,
there are a few ways to satisfy this requirement, shown below.
It is usually best to avoid writing functions
that can can return multiple kinds of views.

```swift
import SwiftUI

struct ContentView: View {
    /*
     // This doesn't compile because it can return two kinds of Views.
     private func doesNotCompile(_ condition: Bool) -> some View {
         if condition {
             Text("Hello, World!")
         } else {
             Color.red.frame(width: 100, height: 100)
         }
     }
     */

    private func useGroup(_ condition: Bool) -> some View {
        Group {
            if condition {
                Text("Hello, World!")
            } else {
                Color.red.frame(width: 100, height: 100)
            }
        }
    }

    private func useAnyView(_ condition: Bool) -> some View {
        if condition {
            return AnyView(Text("Hello, World!"))
        } else {
            return AnyView(Color.red.frame(width: 100, height: 100))
        }
    }

    @ViewBuilder
    private func useViewBuilder(_ condition: Bool) -> some View {
        if condition {
            Text("Hello, World!")
        } else {
            Color.red.frame(width: 100, height: 100)
        }
    }

    var body: some View {
        VStack {
            useGroup(true)
            useGroup(false)
            useAnyView(true)
            useAnyView(false)
            useViewBuilder(true)
            useViewBuilder(false)
        }
    }
}
```

`@ViewBuilder` is a custom parameter attribute used to indicate
that a parameter accepts `ViewBuilder` closure that returns a list of views.
This is similar to the concept of "slots" in some web frameworks
which allow chunks of HTML to be passed to and rendered by a component.

The following example implements a custom view
that takes two `@ViewBuilder` parameters.
These represent views for the front and back of a card.
Tapping the card causes it to flip to the other side with a 3D animation.

```swift
import SwiftUI

// Since each ViewBuilder parameter can result in a different kind of View,
// we need two generic parameters to represent their types.
struct Card<V1, V2>: View where V1: View, V2: View {
    @ViewBuilder let front: V1
    @ViewBuilder let back: V2

    @State private var backDegrees = 90.0 // goes from 90 to 0
    @State private var frontDegrees = 0.0 // goes from 0 to -90
    @State private var isFlipped = false

    typealias Axis = (x: CGFloat, y: CGFloat, z: CGFloat)

    let axis: Axis = (x: 0, y: 1, z: 0)
    let cardHeight = 300.0
    let cardWidth = 200.0
    let cornerRadius = 20.0
    let duration: CGFloat = 0.3

    var body: some View {
        ZStack {
            front
                .frame(width: cardWidth, height: cardHeight)
                .clipShape(RoundedRectangle(cornerRadius: cornerRadius))
                .rotation3DEffect(
                    Angle(degrees: frontDegrees),
                    axis: axis
                )
            back
                .frame(width: cardWidth, height: cardHeight)
                .clipShape(RoundedRectangle(cornerRadius: cornerRadius))
                .rotation3DEffect(
                    Angle(degrees: backDegrees),
                    axis: axis
                )
        }
        .onTapGesture {
            flip()
        }
    }

    private func flip() {
        isFlipped.toggle()
        if isFlipped {
            withAnimation(.linear(duration: duration)) {
                frontDegrees = -90
            }
            withAnimation(.linear(duration: duration).delay(duration)) {
                backDegrees = 0
            }
        } else {
            withAnimation(.linear(duration: duration)) {
                backDegrees = 90
            }
            withAnimation(.linear(duration: duration).delay(duration)) {
                frontDegrees = 0
            }
        }
    }
}
```

The `Card` view above can be used as follows:

```swift
Card(
    front: {
        ZStack {
            Color.red
            Text("Front").font(.largeTitle)
        }
    },
    back: {
        ZStack {
            Color.blue
            Text("Back").font(.largeTitle)
        }
    }
)
```

## View Modifiers

View modifiers are methods that can be called on a view.
They don't modify the view. They create a new view
that is like the receiver, but modified in a specific way.
Calls to view modifiers can be chained since each returns a new view.

The following example uses the `foregroundColor`, `padding`, and `stroke`
view modifiers.

```swift
Text("Hello, World!").foregroundColor(.red)
RoundedRectangle(cornerRadius: 20).stroke(lineWidth: 3).padding(.all)
```

Some view modifiers can be applied to any view.
Others are specific to certain kinds of views.
For example, the `stroke` view modifier can only be applied
to views that implement the `Shape` protocol.

The official documentation for the supplied view modifiers can be found at
{% aTargetBlank
"https://developer.apple.com/documentation/swiftui/slider-view-modifiers",
"View Modifiers" %}.

Commonly used view modifiers include:

- `background(alignment, content)`
- `border(ShapeStyle, width: CGFloat = 1)`
- `cornerRadius(CGFloat, antialiased: Bool)`
- `disabled(Bool)` disables any form input such as a `Button`
- `edgesIgnoringSafeArea(Edge.Set)`
- `font(Font?)`
- `foregroundColor(Color?)`
- `foregroundStyle(ShapeStyle)`
- `frame(width: CGFloat?, height: CGFloat?, alignment: Alignment)`
- `frame(maxWidth: CGFloat?, maxHeight: CGFloat?, alignment: Alignment)`
- `border(ShapeStyle, width: CGFloat = 1)`
- `lineLimit(Int?)`
- `multilineTextAlignment(TextAlignment)`
- `offset(x: CGFloat, y: CGFloat)`
- `opacity(Double)`
- `overlay(ShapeStyle)`
- `padding(CGFloat)`

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/background(alignment:content:)",
"background" %} view modifier `content` argument is a `ViewBuilder` function
that can return any kind of `View` including a `Color`, `Shape`, or `Image`.

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/background(alignment:content:)",
"background" %} view modifier name doesn't end in `Color`
like {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/list/foregroundcolor(_:)",
"foregroundColor" %} because it can specify background content that is
any `View` and is not restricted to being only a color.

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/text/multilinetextalignment(_:)",
"multilineTextAlignment" %} view modifier specifies how text that
wraps across multiple lines should be horizontally aligned.
It can be passed `.leading` (default), `.center`, or `.trailing`.

The following view modifiers change the styling
of specific kinds of predefined views.

- `buttonStyle(ButtonStyle)`
- `controlGroupStyle(ControlGroupStyle)`
- `datePickerStyle(DatePickerStyle)`
- `gaugeStyle(GaugeStyle)`
- `indexViewStyle(IndexViewStyle)`
- `labelStyle(LabelStyle)`
- `menuStyle(MenuStyle)`
- `navigationViewStyle(NavigationViewStyle)`
- `pickerStyle(PickerStyle)`
- `progressViewStyle(ProgressViewStyle)`
- `presentedWindowStyle(WindowStyle)`
- `presentedWindowToolbarStyle(WindowToolbarStyle)`
- `tableStyle(TableStyle)`
- `tabViewStyle(TabViewStyle)`
- `textFieldStyle(TextFieldStyle)`
- `toggleStyle(ToggleStyle)`

  This can be passed a side which can be a single value or an array
  of `.all` (default),
  `.leading`, `.trailing`, `.horizontal` (same as `.leading` and `.trailing`),
  `.top`, `.bottom`, or `.vertical` (same as `.top` and `.bottom`).
  It can also be passed a `CGFloat` value for the length.
  The length defaults to `nil` and means to use the system default of 20.

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

The following example adds a shadow to a `Text` view:

<img alt="SwiftUI Shadow" style="width: 60%"
  src="/blog/assets/SwiftUI-Shadow.png?v={{pkg.version}}"
  title="SwiftUI Shadow">

```swift
Text("Shadow Demo")
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
This makes them applicable to any kind of view.

When view modifiers are added to container views,
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

<img alt="SwiftUI ViewModifier" style="width: 70%"
  src="/blog/assets/SwiftUI-ViewModifier.png?v={{pkg.version}}"
  title="SwiftUI ViewModifier">

```swift
import SwiftUI

struct Collapsible: ViewModifier {
    private static let diameter = CGFloat(120)
    private static var radius: CGFloat { diameter / 2 }

    var bgColor: Color = .gray
    var duration: Double = 0.5 // in seconds

    @State private var showContent = true

    var halfCircle: some View {
        Circle()
            .trim(from: 0, to: 0.5)
            .fill(bgColor)
            .frame(
                width: Collapsible.diameter,
                height: Collapsible.radius
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
                //TODO: instead of using the default fade transition?
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
                            width: Collapsible.radius / 3,
                            height: Collapsible.radius / 4
                        )
                        .onTapGesture { toggle() }
                        .rotationEffect( // Angle type is inferred
                            .degrees(showContent ? 180 : 0)
                        )
                        .offset(x: 0, y: -2)
                        // Use a view as a background.
                        .background(halfCircle)
                }
                Spacer()
            }
        }
    }
}

extension View {
    func collapsible(
        bgColor: Color = .black,
        duration: Double = 0.5) -> some View {
        modifier(Collapsible(bgColor: bgColor, duration: duration))
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
//.modifier(Collapsible(bgColor: ContentView.bgColor))

// This way uses the View extension and is preferred.
.collapsible(bgColor: ContentView.bgColor)
```

## Property Wrappers

Swift property wrappers attach logic to the properties
of classes, structs, and enums.

SwiftUI supports the following property wrappers:

### @Binding

This property wrapper is applied to a property of a child view
that is passed a binding (`$` syntax) from a parent view.
It connects a child property to a parent property.
This allows the child view to get and set a property that
is owned by a parent view and si passed in as an argument.

```swift
struct ChildView: View {
    @Binding var n: Int

    var body: some View {
        VStack {
            Text("ChildView: n = \(n)")
            Button("Increment") {
                n += 1
            }
            .buttonStyle(.bordered)
        }
    }
}

struct ParentView: View {
    @State var number = 0

    var body: some View {
        VStack {
            Text("ParentView: number = \(number)")
            ChildView(n: $number) // $ is required to pass a binding
        }
    }
}
```

To pass a binding to a constant value, use `.constant(value)`.

### @Environment

This is used to access environment values.
See the "Environment" section.

### @EnvironmentObject

The `@EnvironmentObject` property wrapper allows multiple views
to share access to an `ObservableObject`.

The following code demonstrates this.
It works in the Simulator, but not in Preview.

```swift
// Environment objects must be defined by classes
// that conform to the ObservableObject protocol
// and they should publish at least one property.
class SharedData: ObservableObject {
    @Published var name = "Mark"
    @Published var score = 0
}

struct ChildView: View {
    // This value will be received from the environment.
    @EnvironmentObject var sharedData: SharedData

    var body: some View {
        VStack {
            Text("ChildView: name = \(sharedData.name)")
            Text("ChildView: score = \(sharedData.score)")
            Button("Increment Score") {
                // This change will be published to all views
                // that use the SharedData score property.
                sharedData.score += 1
            }
            .buttonStyle(.bordered)
            GrandchildView()
        }
        .environmentObject(sharedData)
    }
}

struct GrandchildView: View {
    // This value will be received from the environment.
    @EnvironmentObject var sharedData: SharedData

    var body: some View {
        VStack {
            Text("GrandchildView: name = \(sharedData.name)")
            Text("GrandchildView: score = \(sharedData.score)")
        }
    }
}

struct ContentView: View {
    @StateObject var sharedData = SharedData()

    var body: some View {
        VStack {
            Text("ContentView: name = \(sharedData.name)")
            Text("ContentView: score = \(sharedData.score)")
            ChildView()
        }
        // All views inside this VStack can access the sharedData object.
        // The environmentObject view modifier can be called
        // multiple times, but only once for each type because
        // the type is how the values are distinguished.
        .environmentObject(sharedData)
    }
}
```

### @State

This property wrapper was described earlier
near the end of the section on "Views".
It enables view structs to maintain state.
This is intended for storing basic values with types like
`Bool`, `Int`, `Double`, and `String`.

When the value of a normal struct property
(declared without a property wrapper)
is modified, a new instance of the struct is created.
This happens because structs are value types.

We don't want a new instance to be created for structs that represent views.
Applying the `@State` property wrapper to a struct property
prevents this because SwiftUI manages the value outside of the struct.
When the value of this kind of property is changed,
the view `body` is recomputed.

This is somewhat like the `useState` hook in React.

An `@State` property can also store more complex types.
However if it is used to store a class instance (a reference type)
and a property of the class is modified,
the associated view `body` will not be recomputed.
A new class instance must be created to trigger an update.

Note that using a `struct` instead of a `class`
in the scenario described above does work.
The reason is that changing the value of a struct property
creates a new instance of the struct.
However, this may not be desirable because
it copies every property of the struct.

### @StateObject

The `@StateObject` property wrapper is used to
create an instance of an `ObservableObject`
which is an object that publishes changes to its properties
that are annotated with the `@Published` property wrapper.
When the values of these properties change,
the associated view `body` will be recomputed.
The following example demonstrates this:

```swift
class MyState: ObservableObject {
    @Published var score: Int = 0
    // Could declare additional published properties here.
}

struct ContentView: View {
    @StateObject var myState = MyState()

    var body: some View {
        VStack {
            Button("Increment") {
                myState.score += 1
            }
            .buttonStyle(.bordered)
            Text("score = \(myState.score)")
        }
    }
}
```

To initialize a state variable in an `init` method,
write code like the following:

```swift
    init() {
        // Note the underscore prefix on the variable name.
        _myState = StateObject(wrappedValue: someValue)
    }
```

### @ObservedObject

The `@ObservedObject` property wrapper marks a property
that receives an instance of an `ObservableObject` subclass
that is passed in from a parent view.
This subscribes to changes published by an observable object
and recomputes the associated view `body` when it changes.
The following example demonstrates this:

```swift
class MyState: ObservableObject {
    @Published var score: Int = 0
    // Could declare additional published properties here.
}

struct ChildView: View {
    @ObservedObject var share: MyState

    var body: some View {
        VStack {
            Button("ChildView: Increment") {
                // This also updates myState in ContentView.
                share.score += 1
            }
            .buttonStyle(.bordered)
            Text("ChildView: score = \(share.score)")
        }
    }
}

struct ContentView: View {
    @StateObject var myState = MyState()

    var body: some View {
        VStack {
            Button("ContentView: Increment") {
                // This also updates share in ChildView.
                myState.score += 1
            }
            .buttonStyle(.bordered)
            Text("ContentView: score = \(myState.score)")

            // Passing the @StateObject variable.
            ChildView(share: myState)
        }
    }
}
```

## Colors

Colors are defined by the `Color` struct.
It provides many static properties for predefined colors
and many initializers for specifying custom colors.

A "color literal" displays a color swatch in source code.
To create one, enter `Color(#colorLiteral())`.
The part passed to `Color` will be replaced by a color swatch.
Double-click the swatch to select a different color.
Click the "Other..." button to open the system color picker
which provides many more ways to select a color.

It is recommended to use the predefined system colors
because they are dynamic, meaning that the actual color used
automatically changes when switching between light and dark mode.
To use these in code, enter `Color(UIColor.system)`
and select a system color from the code completion popup.

Typically `Color.primary` is used for text because it
automatically switches between black for light mode and white for dark mode.
This is the default `foregroundColor` of `Text` views.

SwiftUI uses the following color terminology:

- Accent Color

  This is the primary theme color of controls in an app
  such as `Button` and `TextField`.
  It is typically be specified by setting `AccentColor` in `Assets.xcassets`.
  This is preferred over calling the `accentColor` view modifier in code.
  Different colors can be specified for light and dark mode.
  Controls use the accent color for their default tint color.

  Accent color is not automatically applied to
  text-based views like `Text` and `Label`.
  To apply the accent color to text-based views,
  call `.foregroundColor(.accentColor)` on them.

- Tint

  This overrides the default accent color of controls.
  It is not applied to text-based views.
  It is set by calling the `tint` view modifier on a specific view.
  User preferences cannot override this.

- Foreground Color

  This takes precedence over both the tint and accent color of a view.
  It is typically only applied to text-based views.
  To set it, call the `foregroundColor` view modifier on a specific view.

There are several ways to switch between light and dark mode in the simulator.

1. Press cmd-shift-a (easiest).
2. Open the Settings app, select Developer,
   and change the "Dark Appearance" toggle.
3. Apply the following view modifier to the top-most view:
   `.preferredColorScheme(.dark)` or `.light`
4. Apply the following view modifier to the top-most view:
   `.environment(\.colorScheme, .light)` or `.dark`

To define a custom named color pair, one for light mode and one for dark:

- In the Navigator, select the `Assets.xcassets` file.
- Right-click in the list of assets and select "New Color Set".
- Change the default name "Color" to the name that will be specified in code.
- Click the "Any Appearance" swatch (used for light mode).
- Click the "Attributes" tab in the Inspector (4th tab with sliders icon).
- In the Inspector, select a color.
- Click the "Dark" swatch.
- In the Inspector, select a color.
  Consider selecting the same color used for "Any Appearance".
  Then click the "Show Color Panel" button,
  change the slider type drop-down to "HSB Sliders",
  and reduce the value for Brightness (perhaps to 50%).

To use a custom color in code, enter `Color("some-name")`.
To avoid typos when using these named colors, consider defining a
`Color` extension that defines computed properties like the following:

```swift
extension Color {
    static var danger: Color { Color("Danger") }
    static var success: Color { Color("Success") }

    // This custom initializer enables creating colors based on hex codes.
    init(hex: UInt, alpha: Double = 1) {
        self.init(
            .sRGB,
            red: Double((hex >> 16) & 0xFF) / 255,
            green: Double((hex >> 8) & 0xFF) / 255,
            blue: Double(hex & 0xFF) / 255,
            opacity: alpha
        )
    }

    // This uses the custom initializer above.
    static let background = Color(hex: 0xf1faff)
}
```

UIKit uses `UIColor` instead of `Color`.
To create a `UIColor` from a `Color`,
pass the `Color` to a `UIColor` initializer.
For example, `let redUIColor = UIColor(Color.red)`.

## Mapping from UIKit to SwiftUI

The name of the SwiftUI view that corresponds to a UIKit class
is often the same without the "UI" prefix.
The table below lists the corresponding names.

| UIKit Class               | SwiftUI View                 |
| ------------------------- | ---------------------------- |
| `NSAttributedString`      | `Text`                       |
| `UIActivityIndicatorView` | `ProgressView` without value |
| `UIAlertController`       | `Alert` or `ActionSheet`     |
| `UIButton`                | `Button`                     |
| `UICollectionView`        | `LazyHGrid` or `LazyVGrid`   |
| `UIDatePicker`            | `DatePicker`                 |
| `UIImageView`             | `Image`                      |
| `UILabel`                 | `Text`                       |
| `UINavigationController`  | `NavigationView`             |
| `UIProgressView`          | `ProgressView` with value    |
| `UISegmentedControl`      | `Picker`                     |
| `UISlider`                | `Slider`                     |
| `UIStackView`             | `HStack` or `VStack`         |
| `UIStepper`               | `Stepper`                    |
| `UISwitch`                | `Toggle`                     |
| `UITableView`             | `List`                       |
| `UITextField`             | `TextField` or `SecureField` |
| `UITextView`              | `TextEditor`                 |

## Container Views

Container views act as a container of other views
and layout those views in a specific way.

To hide a view but still take up the space that would be occupied
if the view was visible, consider setting its opacity to zero.
This is done with the `opacity` view modifier.
For example, `someView.opacity(0)`.

To automate wrapping views in a container view,
select them and command-click to get a context menu that contains the options
"Embed in HStack", "Embed in VStack", "Embed in ZStack", "Embed in List",
and "Embed..." (for embedding in an arbitrary container).

Here are the container views that are provided by SwiftUI.

### HStack

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

### VStack

This lays out child views vertically.

The child views are centered horizontally by default.
To change this, add the `alignment` attribute which can be set to
`.leading`, `.center`, or `.trailing`.

### ZStack

{% aTargetBlank "https://developer.apple.com/documentation/swiftui/zstack",
"ZStack" %} is a container view that, by default,
stacks views from bottom to top.
It is ideal for adding a background to a set of views.

Here are three approaches to render text with a colored background,
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

            // Approach #1: Use a ZStack.
            // Semicolons must separate multiple statements
            // on the same line.
            ZStack { rect; Text(text) }

            // Approach #2: Use the background view modifier.
            Text(text)
                .padding(10)
                // Use a view as a background.
                .background(Rectangle().foregroundColor(bgColor))

            // Approach #3: Use the overlay view modifier.
            rect.overlay(Text(text))
        }
    }
}
```

By default, all child views of a `ZStack` have a z-index of zero
and they are stacked in the order in which they are specified.
To change the display order without changing the order in which
the child views are specified, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/zindex(_:)",
"zIndex" %} view modifier to child views
that should have a z-index other than zero.
Pass the desired z-index, which can be positive or negative,
to this view modifier.

For example:

<img alt="SwiftUI ZStack zIndex" style="width: 40%"
      src="/blog/assets/SwiftUI-ZStack-zIndex.png?v={{pkg.version}}"
      title="SwiftUI ZStack zIndex">

```swift
let size = 100.0
ZStack(alignment: .topLeading) {
    Color.red
        .frame(width: size, height: size)
    Color.green
        .frame(width: size, height: size)
        .padding(20.0)
        .zIndex(1)
    Color.blue
        .frame(width: size, height: size)
        .padding(40.0)
```

### LazyHStack

This is similar to `HStack`, but only
builds and renders child views when they are visible.
The rendered child views are retained in memory
after scrolling out of view so rendering them again later is efficient.
`LazyHStack` is commonly used inside a `ScrollView`
with `axes` set to `.horizontal`.

```swift
ScrollView(.horizontal) {
    LazyHStack {
        ForEach(1..<100) {
            Text(String($0))
        }
    }
}
```

### LazyVStack

This is similar to `VStack`, but only
builds and renders child views when they are visible.
`LazyVStack` is commonly used inside a `ScrollView`
with `axes` set to `.vertical`,
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

### LazyHGrid

This specifies a number of rows and adds columns as necessary.
The grids are described by an array of {% aTargetBlank
  "https://developer.apple.com/documentation/swiftui/griditem", "GridItem" %}
objects that each specify their size, spacing, and alignment.
For example, a `GridItem` can adapt to the width of its content,
but also have a minimum size of 25 by specifying
`GridItem(.adaptive(minimum: 25))`.

See the example in `LazyVGrid` below.

### LazyVGrid

This is similar to `LazyHGrid`, but
specifies a number of columns and adds rows as necessary.

The following example demonstrates both `LazyHGrid` and `LazyVGrid`.
It spreads a list of numbers over either rows or columns.

<img alt="SwiftUI LazyHGrid and LazyVGrid" style="width: 40%"
      src="/blog/assets/SwiftUI-Lazy-Grids.png?v={{pkg.version}}"
      title="SwiftUI LazyHGrid and LazyVGrid">

```swift
struct ContentView: View {
  private static let count = 4
  @State private var isVertical = false // Why can't this be static?

  // Describe the characteristics of each grid.
  private static let gridItem = GridItem(
      // This specifies the grid width in LazyVGrid.
      // or the grid height in LazyHGrid.
      .fixed(40),

      // This specifies that the width of each column
      // should be based on the total size available
      // AND the width of the views in each column.
      // Minimum and maximum widths can be specified.
      // .flexible()

      // This is similar to `flexible`, but allows
      // multiple views to be placed in the same column.
      // .adaptive(minimum: 100)

      // This specifies the vertical spacing in LazyHGrid
      // or horizontal spacing in LazyVGrid.
      spacing: 10,
      alignment: .trailing
  )

  var gridItems: [GridItem] = Array(repeating: gridItem, count: count)

  var body: some View {
      VStack {
          if isVertical {
              ScrollView {
                  LazyVGrid(columns: gridItems) {
                      ForEach(1..<101) {
                          Text(String($0)).padding(5).border(.blue, width: 3)
                      }
                  }
                  .padding()
                  .border(.red, width: 5)
              }
          } else {
              ScrollView(.horizontal) {
                  LazyHGrid(rows: gridItems) {
                      ForEach(1..<101) {
                          Text(String($0)).padding(5).border(.blue, width: 3)
                      }
                  }
                  .padding()
                  .border(.red, width: 3)
              }
          }
          Button("Toggle Direction") {
              isVertical.toggle()
          }.buttonStyle(.bordered)
      }
  }
}
```

The following example demonstrates `LazyVGrid`
where the views created are intended for specific columns.

<img alt="SwiftUI LazyVGrid" style="width: 40%"
      src="/blog/assets/SwiftUI-LazyVGrid.png?v={{pkg.version}}"
      title="SwiftUI LazyVGrid">

```swift
func intPow(_ base: Int, _ exponent: Int) -> Int {
    Int(pow(Float(base), Float(exponent)))
}

struct ContentView: View {
    var body: some View {
        let columns: [GridItem] =
            Array(
                repeating: GridItem(.flexible(), alignment: .trailing),
                count: 3
            )

        ScrollView {
            LazyVGrid(columns: columns) {
                Text("Number").font(.title)
                Text("Squared").font(.title)
                Text("Cubed").font(.title)
                ForEach(1 ... 50, id: \.self) { number in
                    Text("\(number)")
                    Text("\(intPow(number, 2))")
                    Text("\(intPow(number, 3))")
                }
            }
            .padding(.horizontal)
        }
    }
}
```

### Form

This is a container of data input views.
Embedding data input views in a `Form`
instead of another container such as `VStack`
can change the way they look and feel.
One example of a data input that changes is `Picker`.

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
        NavigationView { // deprecated in iOS 16
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
                    // The saveUser function is not shown here.
                    Button("Save", action: saveUser)
                }
            }
        }
    }
}
```

To run code when the user completes entering text in any `TextField`,
`SecureField`, or `TextEditor` within the `Form`,
apply the `onSubmit` view modifier passing it a closure.
This can also be applied to an individual input
as described in the [TextField](#textfield) section.

Common UI components that are not built into SwiftUI include:

- checkbox: alternative is Toggle
- image picker: must build or use a library
- multiple choice: alternative is `List`
  inside `NavigationView` with an `EditButton`
- radio buttons: alternative is `Picker`, supported in macOS with
  `Picker` and `.pickerStyle(RadioGroupPickerStyle())`
- toggle buttons: alternative is `Picker`

### Section

`Section` views break a view into sections that are optionally labelled.
They are also optionally collapsible.

`Section` views are only useful inside collection views like
`Form`, `List`, and `Picker`.
See the examples in the `Form` section above and in the `List` section below.

`Section` titles are made all uppercase by default.
To prevent this, apply the `textCase` view modifier passing it `nil`.

### Group

This collects all its child views into a single view
without changing their layout.
View modifiers applied to the `Group` are applied to each of the children.

```swift
Group {
    Text("One")
    Text("Two")
}.foregroundColor(.blue)
```

### GroupBox

This creates a logical grouping of other views
with an optional `Label` at the top.

<img alt="SwiftUI GroupBox" style="width: 40%"
    src="/blog/assets/SwiftUI-GroupBox.png?v={{pkg.version}}"
    title="SwiftUI GroupBox">

```swift
struct ContentView: View {
    static let text = "Known as the Great One, Wayne Gretzky holds more NHL records than any other player in history."

    @State private var like = true

    var body: some View {
        GroupBox(
            label: Label(
                "Wayne Gretzky",
                systemImage: "sportscourt"
            ).font(.title)
        ) {
            ScrollView {
                Text(ContentView.text)
            }.frame(maxWidth: .infinity, maxHeight: 130)
            Toggle("Like", isOn: $like)
        }.padding()
    }
}
```

### ControlGroup

This groups related controls.
It is typically used with `Button` views.
There are two built-in styles, `automatic` (default) and `navigation`.

<img alt="SwiftUI ControlGroup" style="width: 40%"
    src="/blog/assets/SwiftUI-ControlGroup.png?v={{pkg.version}}"
    title="SwiftUI ControlGroup">

```swift
ControlGroup {
    Button("Cancel") {
        print("canceling")
    }
    Button("Save") {
        print("saving")
    }
}
ControlGroup {
    Button("Cancel") {
        print("canceling")
    }
    Button("Save") {
        print("saving")
    }
}.controlGroupStyle(.navigation)
```

### ScrollView

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/scrollview",
"ScrollView" %} view creates a scrollable view where
scrolling reveals additional child views when they do not all fit.

Scrolling is vertical by default, but can be changed to horizontal
by passing `.horizontal` to the initializer.
To enable scrolling in both directions,
pass the array `[.horizontal, .vertical]`.

A `ScrollView` occupies all the space offered to it.

To hide the scroll bars (indicators),
pass the `showIndicators` argument with a value of `false` to the initializer.

Another way to gain the ability to scroll is to use a `List` view.

See examples of using `ScrollView` in the
descriptions of `LazyHStack` and `LazyVStack`.

### ScrollViewReader

To scroll programmatically, use {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/scrollviewreader",
"ScrollViewReader" %} and the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/scrollviewproxy/scrollto(_:anchor:)",
"scrollTo" %} method as shown below.
Note the use of the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/withanimation(_:_:)",
"withAnimation" %} function to add animation to the programmatic scrolling.

<img alt="SwiftUI ScrollViewReader" style="width: 40%"
    src="/blog/assets/SwiftUI-ScrollViewReader.png?v={{pkg.version}}"
    title="SwiftUI ScrollViewReader">

```swift
struct ContentView: View {
    @Namespace var topId
    @Namespace var bottomId

    var body: some View {
        ScrollView {
            ScrollViewReader { proxy in
                Button("Scroll to Bottom") {
                    withAnimation {
                        proxy.scrollTo(bottomId)
                    }
                }
                .id(topId)

                VStack(spacing: 0) {
                    ForEach(1 ..< 101) { i in
                        Text(String(i))
                    }
                }

                Button("Scroll to Top") {
                    withAnimation {
                        proxy.scrollTo(topId)
                    }
                }
                .id(bottomId)
            }
        }
    }
}
```

### ScrollViewProxy

An instance of this type is passed to the trailing closure
of `ScrollViewReader`. See the example above.

### List

A `List` view displays a list of other views in a single, scrollable column.
It is not necessary to wrap a `List` in a `ScrollView`
to gain the ability to scroll.

A `List` can act like `ForEach` for iterating over array elements.
It is not necessary to place a `ForEach` inside a `List`
in order to iterate over the elements of a sequence.

If a `List` contains more items that can be rendered at once,
it automatically provides scrolling.
There is no need to wrap it in a `ScrollView`.

The contents of a `List` can be any views.
These can be grouped using `Section` views.

<img alt="SwiftUI List with Sections" style="width: 40%"
  src="/blog/assets/SwiftUI-List-Sections.png?v={{pkg.version}}"
  title="SwiftUI List with Sections">

```swift
List {
    Section("Breakfast") {
        Text("pancakes")
        Text("bacon")
        Text("orange juice")
    }
    Section("Lunch") {
        Text("sandwich")
        Text("chips")
        Text("lemonade")
    }
    Section("Dinner") {
        Text("spaghetti")
        Text("bread")
        Text("water")
    }
}
//.listStyle(.grouped) // for edge-to-edge display
```

To change the style of the `List`, apply the `listStyle` view modifier
which takes the enum values `grouped`, `inset`, `insetGrouped`, `plain`,
and `sidebar`.

To remove horizontal padding that is added to list elements by default,
apply the `.listRowInsets(EdgeInsets())` view modifier
to each child of the `List`.

The following example demonstrates using a `List` inside a `NavigationView`
to enable selecting ids of the objects represented by the rows.
To select rows, tap "Edit" in the upper-right corner.

This also demonstrates implementing "pull to refresh"
using the `refreshable` view modifier.
Dragging the list down displays an activity indicator
and then adds new dogs to the list.

<figure>
  <img alt="SwiftUI List with Selection Before" style="width: 40%"
    src="/blog/assets/SwiftUI-List-Selection1.png?v={{pkg.version}}"
    title="SwiftUI List with Selection Before">
  <figcaption>Before item selection</figcaption>
</figure>
<figure>
  <img alt="SwiftUI List with Selection During" style="width: 40%"
    src="/blog/assets/SwiftUI-List-Selection2.png?v={{pkg.version}}"
    title="SwiftUI List with Selection During">
  <figcaption>During item selection</figcaption>
</figure>
<figure>
  <img alt="SwiftUI List with Selection After" style="width: 40%"
    src="/blog/assets/SwiftUI-List-Selection3.png?v={{pkg.version}}"
    title="SwiftUI List with Selection After">
  <figcaption>After selecting items</figcaption>
</figure>

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

    //@State private var selection: UUID? // single selection
    @State private var selectedIds = Set<UUID>() // multiple selection

    func loadMore() async {
        // This calls a REST service that returns nothing after 2 seconds.
        let url = URL(string: "https://httpbin.org/delay/2")!
        let request = URLRequest(url: url)
        // Not using the return value for this example.
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

The following example is similar to the previous one,
but allows rows to be deleted and moved.
It seems that it isn't possible for a `List` to support
row selection and also support deleting and moving rows.
See this {% aTargetBlank "https://developer.apple.com/forums/thread/693743",
"forum thread" %}.

<img alt="SwiftUI List with Delete and Move" style="width: 40%"
  src="/blog/assets/SwiftUI-List-Delete-Move.png?v={{pkg.version}}"
  title="SwiftUI List with Delete and Move">

```swift
struct Dog {
    var name: String
    var breed: String
}

// This makes it easier to render text that has a specified width.
struct SizedText: View {
    private var text: String
    private var width: Double

    init(_ text: String, width: Double) {
        self.text = text
        self.width = width
    }

    var body: some View {
        Text(text).frame(width: width, alignment: .leading)
    }
}

struct DogRow: View {
    var dog: Dog

    var body: some View {
        HStack {
            SizedText(dog.name, width: 100)
            SizedText(dog.breed, width: 200)
        }
    }
}

struct ContentView: View {
    @State private var dogs = [
        Dog(name: "Maisey", breed: "Treeing Walker Coonhound"),
        Dog(name: "Ramsay", breed: "Native American Indian Dog"),
        Dog(name: "Oscar", breed: "German Shorthaired Pointer"),
        Dog(name: "Comet", breed: "Whippet")
    ]

    var body: some View {
        NavigationView {
            VStack {
                List {
                    // A nested ForEach is required in order to
                    // enable swipe to delete/move.
                    // The onDelete and onMove methods exists on ForEach,
                    // but not on List because a List can include static rows.
                    ForEach(dogs, id: \.name) { dog in
                        DogRow(dog: dog)
                    }
                    .onDelete(perform: deleteDog)
                    .onMove(perform: moveDog)
                }
                .listStyle(.grouped)
                .toolbar {
                    EditButton()
                }
            }
        }
    }

    private func deleteDog(at offsets: IndexSet) {
        dogs.remove(atOffsets: offsets)
    }

    private func moveDog(source: IndexSet, destination: Int) {
        dogs.move(fromOffsets: source, toOffset: destination)
    }
}
```

### ForEach

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/foreach",
"ForEach" %} struct iterates of the elements of a `RandomAccessCollection`
(includes `Array` and `Range` types)
and renders the view specified by a provided `ViewBuilder`.

The elements in the `RandomAccessCollection` must either conform to
the `Identifiable` protocol (which requires them to have an `id` property)
OR the `id:` argument must be supplied.
If the elements are not `Identifiable`
and no `id:` argument is supplied, the view
may not update property when the collection changes.

If the elements are not `Identifiable` and you wish to use
the elements values as their id, specify `id: \.self`.

Only constant ranges are allowed (ex. `0..<5`, but not `begin..<end`)
unless the `id` argument key path `\.self` is specified.

The value of the `id` argument is a key path that specifies
how to find a unique value in the element.
For example, the `String` type does not implement `Identifiable`.
To iterate over an array of `String` values:

```swift
// \.self is a key path that refers to the entire object.
ForEach(stringArray, id: \.self) { ... }
```

### Table

This is only available in macOS 12 and above.
The following example demonstrates using a `Table`
that supports row selection and column sorting.

```swift
struct Dog: Identifiable {
  let name: String
  let breed: String
  let color: String
  let id = UUID()
}

private var dogs = [ // initially sorted on name
    Dog(name: "Comet", breed: "Whippet", color: "black brindle"),
    Dog(name: "Maisey", breed: "Treeing Walker Coonhound", color: "black"),
    Dog(name: "Oscar", breed: "German Shorthaired Pointer", color: "white"),
    Dog(name: "Ramsay", breed: "Native American Indian Dog", color: "gray")
]

struct ContentView: View {
    @State private var selectedDogs = Set<Dog.ID>()
    @State private var sortOrder = [KeyPathComparator(\Dog.name)] // initial

    var body: some View {
        Table(dogs, selection: $selectedDogs, sortOrder: $sortOrder) {
            TableColumn("Name", value: \.name)
            TableColumn("Breed", value: \.breed)
            TableColumn("Color", value: \.color)
        }
        // Why do these view modifiers have no effect?
        //.tableStyle(InsetTableStyle.inset(alternatesRowBackgrounds: true))
        //.tableStyle(BorderedTableStyle.bordered(alternatesRowBackgrounds: true))

        // Why does the first click on a table heading do nothing?
        .onChange(of: sortOrder) { dogs.sort(using: $0) }
        Text("\(selectedDogs.count) dogs selected")
    }
}
```

### NavigationView

This marks an area where a stack of views will be rendered one at a time.
It contains `NavigationLink` views that are similar to HTML anchor elements.
Tapping them causes the associated view
to be rendered inside the `NavigationView`.
See the "Navigation" section later.

This is deprecated in iOS 16. See the new approach at
{% aTargetBlank "/blog/topics/#/blog/swift/Navigation/", "Navigation" %}.

### NavigationLink

These are used inside a `NavigationView`.
See the "Navigation" section later.

This is deprecated in iOS 16. See the new approach at
{% aTargetBlank "/blog/topics/#/blog/swift/Navigation/", "Navigation" %}.

### OutlineGroup

This displays a tree of data with disclosure angle brackets.
See my {% aTargetBlank
"https://github.com/mvolkmann/SwiftUI-OutlineGroup/blob/main/SwiftUI-OutlineGroup/ContentView.swift",
"SwiftUI-OutlineGroup" %} project and the questions in it.

### DisclosureGroup

This hides and shows its contents based on whether it is in an expanded state.
By default it is not expanded.
It can be expanded by tapping or by associating
a `Bool` binding that is programmatically set to `true`.

<img alt="SwiftUI DisclosureGroup" style="width: 40%"
  src="/blog/assets/SwiftUI-DisclosureGroup.png?v={{pkg.version}}"
  title="SwiftUI DisclosureGroup">

```swift
@State private var cyclist = false
@State private var firstName = ""
@State private var lastName = ""
@State private var personalExpanded = true
@State private var runner = false

var body: some View {
    Form {
        DisclosureGroup("Personal", isExpanded: $personalExpanded) {
            TextField("First Name", text: $firstName)
            TextField("Last Name", text: $lastName)
        }

        DisclosureGroup("Preferences") {
            Toggle("Runner", isOn: $runner)
            Toggle("Cyclist", isOn: $cyclist)
        }
    }
}
```

### TabView

This creates a row of buttons at the bottom of the display
that can be tapped to navigate to associated views.
If there are more than five buttons, the first four will render followed by
along with a "More" button.
The "More" button can be tapped to go a view containing
a tappable list of the remaining options.

From the docs, "Tab views only support tab items of type Text, Image,
or an image followed by text. Passing any other type of view
results in a visible but empty tab item."
A good size for these images is 32x32.

To use `TabView` in conjunction with `NavigationView`,
wrap each page in its own `NavigationView`
rather than placing the `TabView` inside a `NavigationView`.
Having a `NavigationView` for each page
allows use of the `toolbar` view modifier
to add a toolbar at the top of each page.

<img alt="SwiftUI TabView" style="width: 40%"
    src="/blog/assets/SwiftUI-TabView.png?v={{pkg.version}}"
    title="SwiftUI TabView">

```swift
struct Transportation: View {
    var kind: String

    var body: some View {
        Text("Information about \(kind) transportation goes here.")
            .navigationBarTitle("\(kind) Transportation")
    }
}

struct ContentView: View {
    var body: some View {
        TabView {
            Transportation(kind: "Car").tabItem {
                Label("Car", systemImage: "car")
            }
            Transportation(kind: "Bus").tabItem {
                Label("Bus", systemImage: "bus")
            }
            Transportation(kind: "Train").tabItem {
                Label("Train", systemImage: "tram")
            }
            Transportation(kind: "Airplane").tabItem {
                Label("Airplane", systemImage: "airplane")
            }
        }
        .onAppear() {
            UITabBar.appearance().backgroundColor = .systemGray5
        }
        // Change color of Image and Text views which defaults to blue.
        .accentColor(.purple)
    }
}
```

By default the first tab is initially displayed.
To start on another tab, add a state variable
that holds the tag of the desired initial tab.

```swift
@State private var selection = 3
...
TabView(selection: $selection) {
    ...
    SomeView().tabItem {
        ...
    }
    .tag(3)
}
```

Here's another example that displays a set of pages
the user can swipe through.
Page controls with a dot representing each page are displayed at the bottom.

<img alt="SwiftUI TabView Pages" style="width: 40%"
    src="/blog/assets/SwiftUI-TabView-Pages.png?v={{pkg.version}}"
    title="SwiftUI TabView Pages">

```swift
struct Page: View {
    var title: String
    var description: String
    var imageName: String

    var body: some View {
       VStack {
            Text(title).font(.headline)
            Image(systemName: imageName)
                .resizable()
                .scaledToFit() // or .scaledToFill()
                .frame(width: 100, height: 100)
                .foregroundColor(.red)
            Text(description)
        }
    }
}

struct ContentView: View {
    var body: some View {
        TabView {
            Page(
                title: "Planes",
                description: "Fly like the wind!",
                imageName: "airplane"
            )
            Page(
                title: "Trains",
                description: "Travel the tracks!",
                imageName: "tram"
            )
            Page(
                title: "Automobiles",
                description: "Drive the open road!",
                imageName: "car"
            )
        }
        .tabViewStyle(.page(indexDisplayMode: .always))
        .indexViewStyle(.page(backgroundDisplayMode: .always))
    }
}
```

Specific tab bar destinations can hide the tab bar by applying
the view modifier `.toolbar(.hidden, for: .tabBar)` view modifier
to the destination view.

### HSplitView

This is a layout container that organizes its children horizontally
and allows users to resize the children by dragging dividers between them.
It is only supported in macOS.

### VSplitView

This is a layout container that organizes its children vertically
and allows users to resize the children by dragging dividers between them.
It is only supported in macOS.

### TimelineView

This is a container that re-renders its children at scheduled times.
The following example renders the date and time every second.
For example, "Nov 8, 2021 at 5:19:47".

```swift
private var dateFormatter: DateFormatter {
    let df = DateFormatter()
    df.dateFormat = "MMM d, YYYY 'at' h:mm:ss a"
    return df
}

private let schedule = PeriodicTimelineSchedule(from: Date(), by: 1)

var body: some View {
    VStack {
        TimelineView(schedule) { context in
            Text(dateFormatter.string(from: context.date))
        }
    }
}
```

## Component Views

### Button

The content of a `Button` can be specified in two ways,
passing a `String` as the first argument or using the `label` argument
which can be specified with a trailing closure.

A `Button` specifies a function to call when pressed
using the `action` argument
which can also be written as a trailing closure.
The `action` argument is ignored for `Button` views that are inside a `List`.
In that case use the `onTapGesture` modifier instead.

By default buttons have no border, no background color,
and the text is the accent color.

When the `buttonStyle` view modifier is passed `.bordered`,
the background is gray.
When the `buttonStyle` view modifier is passed `.borderedProminent`,
the background is the accent color and the text is white.
In both cases no actual border is drawn.
They just add a background color inside a rounded rectangle.
To change the background color, apply the `tint` view modifier.

To change the border shape, apply the `buttonBorderShape` view modifier
passing it `.capsule`, `.roundedRectangle`,
or `.roundedRectangle(radius: cornerRadius)`.
This only takes effect when the `buttonStyle` view modifier is also applied.

To change the text color,
apply the `tint` or `foregroundColor` view modifier passing it a `Color`.

When their `role` attribute is set to `.destructive`, the text is red.
When their `role` attribute is set to `.cancel`, `.none`, or not specified,
there is no visible change.

To change the background color,
apply the `tint` view modifier passing it a `Color`.

To change the size, apply the `controlSize` modifier,
passing it `.small` or `.large` (default is `.regular`).

To disable a `Button`,
apply the `disabled` view modifier passing it a `Bool`.

```swift
// Button containing text and action specified with a trailing closure.
Button("My Label", role: .destructive) {
    // code to run when button is pressed
}.buttonStyle(.borderedProminent)

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

When there are multiple buttons on the same row of a `Form` or `List`,
tapping any of them runs all of their actions
instead of just the action of the tapped `Button`.
One fix for this is to add `.buttonStyle(.borderless)` to each of the buttons.
This is demonstrated in the simple app below.

```swift
struct ContentView: View {
    @State var number = 0

    var body: some View {
        // VStack { // no issues
        // List { // has bug
        Form { // has bug
            HStack {
                Button("One") { number = 1 }
                Button("Two") { number = 2 }
                Button("Three") { number = 3 }
            }
            .buttonStyle(.borderless) // fixes the bug

            Text("You tapped button #\(number)").padding()
        }
    }
}
```

### Color

This creates a rectangular view with a specific background color
that grows to fill all the space offered to it.
For example, `Color.red` and `Color.clear` (transparent) are views.

A `UIColor` can be converted to a `Color`.
For example, `UIColor.blue` can be converted with `Color(.systemBlue)`.

### Image

This renders an image.
Many image formats are supported including PNG, JPEG, and HEIC.
To add images, click `Assets.xcassets` in the Navigator.
Click the "+" in the lower-left to add an entry.
Give the entry a name and drag images into the 1x, 2x, and 3x boxes.
Pass the name to the `Image` view as an unlabelled argument.
For example, `Image("Comet")`.

Icons from SF Symbols can be used by specifying
their name as the `systemName` argument.
For example, `Image(systemName: "cloud.snow")`.

`Image` views are not resizable by default.
To make them resizable, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/image/resizable(capinsets:resizingmode:)",
"resizable" %} view modifier.
To tile the image across the space given to it,
apply the view modifier `.resizable(resizingMode: .tile)`.

To maintain the original aspect ratio, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/aspectratio(_:contentmode:)-771ow",
"aspectRatio" %} view modifier.
This takes a `contentMode` argument which can
have the values `.fill` and `.fit`.
These are equivalent: `.aspectRatio(contentMode: .fill)` and `.scaledToFill()`.
And these are equivalent: `.aspectRatio(contentMode: .fit)` and `.scaledToFit()`.

To change an `Image` to have a size different from its default,
apply the `frame` view modifier.

A new size can cause the image to skew.
To clip an image to a given shape, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/clipshape(_:style:)",
"clipShape" %} view modifier.

The order in which these view modifiers are applied is important.
Here is an example of correct usage.

```swift
Image("some-name")
    .resizable()
    .frame(width: 300, height: 300)
    .clipShape(Circle())
    .overlay(Circle().stroke(Color.red, lineWidth: 10))
```

### AsyncImage

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/asyncimage",
"AsyncImage" %} view asynchronously loads and displays an image.
A specified placeholder image is displayed
while the specified image is being downloaded.
It works in the Simulator, but not in Preview.

The following example renders the Swift logo:

<img alt="SwiftUI AsyncImage" style="width: 40%"
    src="/blog/assets/SwiftUI-AsyncImage.png?v={{pkg.version}}"
    title="SwiftUI AsyncImage">

```swift
struct ContentView: View {
    private let imageUrl =
        "https://developer.apple.com/swift/images/swift-og.png"
    private let size = 100.0

    var body: some View {
        VStack {
            AsyncImage(
                url: URL(string: imageUrl),
                content: { image in
                    image
                        .resizable()
                        .frame(width: size, height: size)
                },
                placeholder: { ProgressView() } // spinner
            )
            .frame(width: 100, height: 100)
        }
    }
}
```

`AsyncImage` does not cache downloaded images,
so they can be downloaded multiple times.
To cache the images, see this {% aTargetBlank
"https://stackoverflow.com/questions/69214543/how-can-i-add-caching-to-asyncimage",
"Stack Overflow post" %}.

### Text

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

The `Text` view can only be passed a `String`.
Other types such as `Int` and `Double` must be converted to `String`.
Consider extending `Text` to add initializers for other types.
For example:

```swift
import SwiftUI

extension Text {
    init(_ number: Double) {
        self.init(String(number))
    }

    init(_ number: Int) {
        self.init(String(number))
    }
}
```

There are many view modifiers that can be applied to `Text` views.

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/environmentvalues/linelimit",
"lineLimit" %} view modifier limits the number of lines
on which the text can be wrapped, specified by the argument value.
If more lines are needed, the text is elided
(truncated with an ellipsis at the end).
To change where the ellipsis appears, apply the `truncationMode` view modifier
with the value `.head`, `.middle` or `.tail` (default).
The ellipsis always appears in the last line of multi-line text.

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/text/kerning(_:)",
"kerning" %} and {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/lazyvstack/tracking(_:)",
"tracking" %} view modifiers both add space between letters.
The difference is that the former even adds space between ligatures
whereas the later does not.

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/text/minimumscalefactor(_:)",
"minimumScaleFactor" %} view modifier causes the font size
to be scaled down in order to get the text to fit within its frame.
Its argument is a percent value between 0 and 1.
For example, `Text(myTitle).font(.title).minimumScaleFactor(0.75)`
will use the `title` font size if the text will fit,
but will scale as low as 75% of that font size.
It uses the largest size that will fit that is between
100% and 75% of the requested font size.
If the text doesn't fit at 75% of the requested font size,
it will be elided (truncated with ... at the end).

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/text/textcase(_:)",
"textCase" %} view modifier transforms the case of the text.
The supported argument values are:

- `.lowercase`: changes all characters to lowercase
- `.uppercase`: changes all characters to uppercase
- `none`: makes no changes

The `Text` view automatically recognizes and honors Markdown syntax.
For example:

```swift
Text("plain *italic* **bold** ~strike~ `code`")
Text("[link](https://apple.com)")
```

By default users cannot select the text rendered by `Text` views.
To enable this, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/textselection(_:)",
"textSelection" %} view modifier with the argument value `.enabled`.
To copy the text, press until a popup containing
"Copy" and "Share" buttons appears.
The entire text is copied, not individual characters or words.

Applying the `textSelection` view modifier to a `List` view
makes each `Text` view inside it selectable.

To render text with multiple styles and no space between them,
use the "+" operator between `Text` views. For example:

```swift
Text("Red").foregroundColor(.red) +
Text("Green").foregroundColor(.green) +
Text("Blue").foregroundColor(.blue)
```

The `Text` initializer can be passed a `Date`
if a `style` argument is also passed.
For example:

```swift
Text(Date(), style: .date) // December 3, 2022
Text(Date(), style: .time) // 11:20 AM

// Each of these update automatically every second.
Text(Date(), style: .offset) // +41 seconds
Text(Date(), style: .relative) // 41 sec
Text(Date(), style: .timer) // 0.41
```

The `Text` initializer can be passed a type other than `String`
if a `format` argument with a compatible value is also passed.
For example:

```swift
// These format currency values, but do not perform currency conversion.
let price = 123.456
Text(price, format: .currency(code: "USD")) // $123.46
Text(price, format: .currency(code: "GBP")) // ￡123.46

// These format the current date.
Text(Date(), format: .dateTime) // 12/3/2022, 11:17 AM
Text(Date(), format: .iso8601) // 2022-12-03T17:17:56Z

// These format an array of strings by placing a comma between each
// and including the word "and" or "or" before the last value.
let stooges = ["Moe", "Larry", "Curly"]
Text(stooges, format: .list(type: .and)) // Moe, Larry, and Curly
Text(stooges, format: .list(type: .or)) // Moe, Larry, or Curly
```

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/measurement",
"Measurement" %} struct from the Foundation library
is used to specify a quantity (`Double`) with a unit of measure.
Values of this type can be passed to the `Text` initializer
along with a `format` argument of type {% aTargetBlank
"https://developer.apple.com/documentation/foundation/measurement/formatstyle",
"Measurement.FormatStyle" %}.

The supported quantity types include `UnitAcceleration`, `UnitAngle`,
`UnitArea`, `UnitDispersion`, `UnitDuration`, `UnitElectricalCharge`,
`UnitElectricalCurrent`, `UnitEnergy`, `UnitFrequency`, `UnitFuelEfficiency`,
`UnitIlluminance`, `UnitLength`, `UnitMass`, `UnitPoint`, `UnitPower`,
`UnitPressure`, `UnitSpeed`, `UnitTemperature`, and `UnitVolume`.
Each of these define a set of class properties that specify the supported units.
For example, `UnitLength` defines the metric system class properties
`millimeters`, `centimeters`, `meters`, and `kilometers`,
the imperial system class properties `inches`, `feet`, `yards`, and `miles`,
and many more.

For example:

```swift
// A `Measurement` has a `value` and a `unit`.
let metricLength = Measurement(value: 10, unit: UnitLength.centimeters)
Text("metric length value is \(metricLength.value)") // 10.000000
Text("metric length unit is \(metricLength.unit.symbol)") // cm

let imperialWidth = Measurement(value: 7, unit: UnitLength.inches)

// The output shown in the comments below assumes
// the app is run in the United States where
// the Imperial system is used rather the Metric system.
Text(metricLength, format: .measurement(width: .wide)) // 3.9 inches
Text(metricLength, format: .measurement(width: .abbreviated)) // 3.9 in
Text(metricLength, format: .measurement(width: .narrow)) // 3.9"
Text(imperialWidth, format: .measurement(width: .wide)) // 7 inches

// Measurements support many operators including +, -, *, /.
// These operators return a new `Measurement` instance.
// Only one operand can be a `Measurement`.
// The other must be a number.

// Measurements support comparison operators like ==, <, and >.

// Measurements can be converted to compatible units.
let metricWidth = imperialWidth.converted(to: metricLength.unit)
Text("\(metricWidth.value)") // 17.78 cm

let metricArea = Measurement(
    value: metricLength.value * metricWidth.value,
    unit: UnitArea.squareCentimeters
) // 177.8 square centimeters
Text(metricArea, format: .measurement(width: .wide)) // 28 square inches
```

Also see the "AttributedString" section for applying
different formatting to substrings of a string.

### TextField

A {% aTargetBlank "https://developer.apple.com/documentation/swiftui/textfield",
"TextField" %} provides single-line text entry.
It also works with non-String types using a `FormatStyle` object
to convert between `String` values and the type.
It takes label text, a binding to a variable, and an optional prompt.
In macOS apps, the label precedes the input area
and the prompt is used as placeholder text.
In iOS apps, no text precedes the input area
and the prompt or label is used as placeholder text.

The style can be set with the `textFieldStyle` view modifier.
The options are `plain` (default) and `roundedBorder`.
When the `plain` style is used, it's not obvious that the value can be edited
unless it is wrapped in a `Form`.

Auto-capitalization of words is provided by default.
To disable this, pass `.none` to the `autocapitalization` view modifier.

Auto-correction is provided by default.
To disable this, pass `true` to the `disableAutocorrection` view modifier.

To allow entering multiple lines of text,
set the `axis` argument to `.vertical`.

```swift
// No prompt
TextField("First Name", text: $firstName)
    .padding()
    .textFieldStyle(.roundedBorder)

// Prompt specified
TextField(text: $lastName, prompt: Text("Required")) {
    Text("Last Name")
}
.textFieldStyle(.roundedBorder)
.padding()
```

`TextField` supports a `format` argument that can be set to `.number`.
However, this doesn't prevent users from entering non-numeric characters.
It only causes it to attempt to convert what is entered to a number
after the user taps the "return" key.
A better way to only accept numeric characters is to
use the `numberPad` keyboard as shown below.

```swift
// Using a non-String value, Int in this case.
// This doesn't provide arrows to increment and decrement the value.
TextField("Score", value: $score, format: .number)
    // Use .decimalPad to allow floating point numbers,
    // but this allows entering more than one decimal point.
    .keyboardType(.numberPad)
    .padding()
    .textFieldStyle(.roundedBorder)
```

To use good foreground and background colors in both light and dark mode
use the following:

```swift
TextField("my placeholder", text: $locationVM.searchQuery)
    .padding(10)
    .background(
        RoundedRectangle(cornerRadius: 10).fill(.background)
    )
    .foregroundColor(.primary)
```

By default the on-screen keyboard that appears
when a `TextField`, `SecureField`, or `TextEditor` has focus
contains a "submit" button labelled "return".
This can be changed to eight other values by applying the
`submitLabel` view modifier and passing it a {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/submitlabel",
"SubmitLabel" %} static property. These values include `.continue`,
`.done`, `.go`, `.join`, `.next`, `.return`, `.search`, and `.send`.

To run code when the user completes entering text in a `TextField`,
`SecureField`, or `TextEditor` by pressing the "submit" button,
apply the `onSubmit` view modifier passing it a closure.
For example:

```swift
struct ContentView: View {
    @State private var name = ""
    @State private var textColor: Color = .red

    var body: some View {
        VStack {
            TextField("Name", text: $name)
                .textFieldStyle(.roundedBorder)
                .onSubmit {
                    textColor = .green
                }
            if !name.isEmpty {
                Text("Hello, \(name)!")
                    .foregroundColor(textColor)
            }
        }
        .padding()
    }
}
```

The `onSubmit` view modifier can be attached to any view.
The closure passed to it will run when any `TextField`,
`SecureField`, or `TextEditor` nested inside it is submitted.
This makes the `Form` view and good candidate.

### SecureField

This is like `TextField`, but obscures the characters that are typed.
It is typically used for sensitive data like
passwords and social security numbers.

```swift
SecureField("Password", text: $password)
    .autocapitalization(.none)
    .disableAutocorrection(true)
```

### TextEditor

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/texteditor", "TextEditor" %}
view supports multi-line text entry.
The number of lines can be limited,
but it seems the limit is only enforced on initial render.
It doesn't prevent more lines from being
displayed if the user types more text.
Use the `frame` view modifier to set the size.

```swift
let lines = 3
TextEditor(text: $reasonForVisit)
    .lineLimit(lines)
    .frame(maxWidth: .infinity, maxHeight: CGFloat(lines * 24))
    .overlay(
        RoundedRectangle(cornerRadius: 5)
            .stroke(Color(UIColor.lightGray))
    )
```

### EditButton

This toggles the edit mode of a `List`.
It is typically added to a `List` using the `toolbar` view modifier.
See the example in the "Lists" section.

### PasteButton

This is only available in macOS 10.15 and above.
It renders a button for pasting data from the system clipboard.

### Link

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/link",
"Link" %} view creates a hyperlink like an HTML `a` element.
It takes link text and a `destination` argument
whose value is a {% aTargetBlank
"https://developer.apple.com/documentation/foundation/url", "URL" %} struct.
Tapping a `Link` opens the associated URL in Safari.
Links work in the Simulator but Preview is not able to open Safari.

```swift
Link(
    "link text",
    destination: URL(string: "https://some-domain/some-path"
)
```

To display a view instead of plain link text,
add the `label` argument as follows:

```swift
Link(
    destination: URL(string: "https://some-domain/some-path"),
    label: SomeView
)
```

Apply the `font` view modifier to change the font.

Apply the `tint` view modifier to change the color.

When using `Link` views it may be necessary to
add the key "Supports Document Browser" with the Boolean value "YES"
in the target Info tab to remove a build warning.

To open a `URL` programmatically, get the `openURL` function
from the environment and pass it a `URL` object.
For example:

```swift
struct SomeView: View {
    @Environment(\.openURL) var openURL

    var body: some View {
        Button("My Blog") {
            openURL(URL(string: "https://mvolkmann.github.io/blog/")!)
        }
    }
}
```

### Menu

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
select the color used to fill a `Rectangle`.

```swift
@State private var color = Color.red

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

### Toggle

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/toggle",
"Toggle" %} view enables toggling between on and off states.
By default it renders as a switch with a circular thumb.

The default switch background color is
the `AccentColor` set in `Assets.xcassets`.
To change this apply the `tint` view modifier.

A `Toggle` can also render as a button whose text is the accent color
and whose background color indicates whether it is off or on.
When it is off, the background color is clear.
When it is on, the background color is a lighter shade of the accent color.

The `Toggle` initializer takes a `String` to render
(either before the switch or inside the button)
and a binding to a `Bool` value.

```swift
// A
Toggle("Hungry", isOn: $hungry).tint(.red)

// The default `toggleStyle` is `.switch`.
Toggle("Hungry", isOn: $hungry).toggleStyle(.button)
```

<img alt="SwiftUI Toggle" style="width: 60%"
    src="/blog/assets/SwiftUI-Toggle.png?v={{pkg.version}}"
    title="SwiftUI Toggle">

### Slider

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/slider",
"Slider" %} view renders a horizontal track with a thumb
that slides between minimum and maximum values
specified by the `in` argument whose value is a `ClosedRange`.

The `value` argument value is a binding that holds the current value
as a `Float`, not an `Int`.

An optional `step` argument indicates the steps in which the value changes.

Text and/or icons can be displayed at the leading and trailing ends.

<img alt="SwiftUI Slider" style="width: 60%"
    src="/blog/assets/SwiftUI-Slider.png?v={{pkg.version}}"
    title="SwiftUI Slider">

```swift
// No text or icons at ends.
Slider(value: $rating, in: 0...10, step: 1)

// Has text at ends and displays current value below.
VStack {
    Slider(
        value: $rating,
        in: 0...10,
        step: 1,
        label: { Text("Rating") }, // not rendered
        // The next two attributes must be closures that return the
        // same kind of view.  Image and Text are common choices.
        minimumValueLabel: { Image(systemName: "hand.thumbsdown") },
        maximumValueLabel: { Image(systemName: "hand.thumbsup") }
    ).padding()
    Text("rating = \(Int(rating))")
}
```

### Stepper

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/stepper",
"Stepper" %} view displays "-" and "+" buttons that can be
tapped to decrement and increment a value.
It takes a label to display before the buttons,
a binding for the current value,
an optional range the value must remain inside, and
a closure to execute every time the `Stepper` changes the value.

<img alt="SwiftUI Stepper" style="width: 60%"
    src="/blog/assets/SwiftUI-Stepper.png?v={{pkg.version}}"
    title="SwiftUI Stepper">

```swift
VStack {
    Stepper(
        "# of Dogs: \(dogCount)",
        value: $dogCount,
        in: 0...10
    ) { v in
        tooManyDogs = dogCount > 2
    }

    Text("Too Many Dogs? \(String(tooManyDogs))")
}.padding()
```

The optional `onIncrement` and `onDecrement` arguments take a closure
that runs when the plus and minus buttons are tapped.
They are typically used to modify the value
in a way other than adding or subtracting one.

### Picker

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/picker",
"Picker" %} view supports selecting an option from a list.
A `Picker` takes the text to display as a prompt and
a `selection` argument which is a binding that
holds something to identify the selected value.
The options are specified with child `Text` views.

The label text passed to the `Picker` initializer
is only displayed if the `Picker` is inside a `Form` or `List`.

The `pickerStyle` view modifier can be applied
to change the way the options are rendered.
The specified prompt is only rendered by some styles.
The values that can be passed to this include:

- `.automatic` (default)

  This selects a style based on the context in which the `Picker` is used.
  It is typically either `.menu` or `.wheel`.
  When the `Picker` is inside a `Form` which is inside a `NavigationView`
  and no `pickerStyle` is specified,
  clicking the `Picker` displays the options on a separate page.

  <figure>
    <img alt="SwiftUI Picker"
      src="/blog/assets/SwiftUI-Picker1.png?v={{pkg.version}}"
      title="SwiftUI Picker automatic before clicking">
    <figcaption>automatic picker before clicking</figcaption>
  </figure>
  <figure>
    <img alt="SwiftUI Picker"
      src="/blog/assets/SwiftUI-Picker2.png?v={{pkg.version}}"
      title="SwiftUI Picker automatic after clicking">
    <figcaption>automatic picker new page after clicking</figcaption>
  </figure>

- `.inline`

  This displays the prompt and all the options (visible simultaneously)
  in the current sheet.
  The selected option is indicated by a check mark.
  This works when the `Picker` is inside a `Form`.
  Otherwise it uses the `.wheel` style.

- `.menu`

  This does not display the prompt and
  only displays the currently selected value.
  When the current value is tapped, all the options are
  rendered in a dropdown menu inside the current sheet.
  There is no limit to the number of options that can appear in the menu
  and it will scroll vertically if they do not all fit on the screen.

  <figure>
    <img alt="SwiftUI Picker"
      src="/blog/assets/SwiftUI-Picker-menu1.png?v={{pkg.version}}"
      title="SwiftUI Picker menu before clicking">
    <figcaption>menu picker before clicking</figcaption>
  </figure>
  <figure>
    <img alt="SwiftUI Picker"
      src="/blog/assets/SwiftUI-Picker-menu2.png?v={{pkg.version}}"
      title="SwiftUI Picker menu after clicking">
    <figcaption>menu picker after clicking</figcaption>
  </figure>

- `.radioGroup` - not available in iOS

- `.segmented`

  This does not display the prompt,
  and renders the options as a "Segmented Control"
  which is a horizontal row of buttons.
  There is no limit to the number of options,
  but their text will be elided if it doesn't fit inside the buttons.

  <figure>
    <img alt="SwiftUI Picker segmented"
      src="/blog/assets/SwiftUI-Picker-segmented.png?v={{pkg.version}}"
      title="SwiftUI Picker segmented">
    <figcaption>segmented picker</figcaption>
  </figure>

- `.wheel`

  This does not display the prompt,
  and renders all the options as a scrollable wheel.
  It requires sufficient vertical space to render properly.
  A height of 300 works well.

  <figure>
    <img alt="SwiftUI Picker wheel"
      src="/blog/assets/SwiftUI-Picker-wheel.png?v={{pkg.version}}"
      title="SwiftUI Picker wheel">
    <figcaption>wheel picker</figcaption>
  </figure>

The font and background colors used by a `Picker`
can be customized in limited ways.

To set the background color, apply the `background` view modifier
to the `Picker`. For example, `.background(Color.yellow.opacity(0.3))`.

To set the font and foreground color, apply the `font` and `foregroundColor`
view modifiers to the `Text` views used to render the options.
For example, `.font(.headline).foregroundColor(.red)`.
This only applies to `automatic` and `wheel` pickers,
not `menu` or `segmented` pickers.

When the options are generated using `ForEach` iterating over an array,
the selected value is described by the `id` property values
of the array elements.
This can be changed by specifying a `tag` value for each option.
The type of the `tag` values must match the type of the `selection` argument.
If these types differ, it will not be possible to select an option.

```swift
@State private var shirtSize: ShirtSize = .sm

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
    } // .pickerStyle(.segmented)
}
```

Here's a more complex example where the `Picker` begins
with no option being selected.
Assume that `people` is an array of `Person` objects
that have an optional `name` property.

```swift
@State private var selectedPersonIndex: Int = -1 // nothing selected

private var selectedPerson: Person? {
    selectedPersonIndex == -1 ? nil : people[selectedPersonIndex]
}

...

Picker("Owner", selection: $selectedPersonIndex) {
    ForEach(people.indices) { index in
        Text(people[index].name ?? "").tag(index)
    }
}
```

### DatePicker

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/datepicker", "DatePicker" %}
view allows selecting a date, time, or both.
It takes the text to display as a prompt and
a `selection` argument which is a binding that
holds the currently selected `Date` value.

The optional `displayedComponents` argument specifies
what can be selected using a single value or an array of
{% aTargetBlank "", "DatePickerComponent" %} values
which include `.date` and `.hourAndMinute`.
Note that it is not possible to request allowing
selection of a month and day without a year.

```swift
DatePicker(
    "Birthday",
    selection: $birthday, // a binding
    displayedComponents: [.date, .hourAndMinute] // array or single value
)
```

To restrict the dates that can be selected,
add the `in` argument with a `Range` value after the `selection` argument.
To only allow dates in the past or now, use `...Date()`.
To only allow dates in the future or now, use `Date()...`.

To hide the label, apply the `labelsHidden` view modifier.

```swift
DatePicker("", selection: $birthday, displayedComponents: .date)
    .labelsHidden()
```

Apply the `datePickerStyle` view modifier to choose a style.
The options that can be passed to this include:

- `.automatic` - This is the default and is the same as `.compact` in iOS.

- `.compact` - This renders date in a button that can be tapped
  to render the `.graphical` controls (see below) for changing the date.

  <figure>
    <img alt="SwiftUI DatePicker compact style"
      src="/blog/assets/SwiftUI-DatePicker-compact1.png?v={{pkg.version}}"
      title="SwiftUI DatePicker compact style">
    <figcaption>DatePicker compact style</figcaption>
  </figure>
  <figure>
    <img alt="SwiftUI DatePicker compact style after tapping"
      src="/blog/assets/SwiftUI-DatePicker-compact2.png?v={{pkg.version}}"
      title="SwiftUI DatePicker compact style after tapping">
    <figcaption>DatePicker compact style after tapping</figcaption>
  </figure>
  <figure>
    <img alt="SwiftUI DatePicker compact style after tapping month-year"
      src="/blog/assets/SwiftUI-DatePicker-compact3.png?v={{pkg.version}}"
      title="SwiftUI DatePicker compact style after tapping month-year">
    <figcaption>DatePicker compact style after tapping </figcaption>
  </figure>

- `.field` - This not available in iOS.

- `.graphical` - The renders a calendar view that contains controls for
  picking a month and year, going to the previous or next month,
  and selecting a day of the month.

  <figure>
    <img alt="SwiftUI DatePicker graphical style"
      src="/blog/assets/SwiftUI-DatePicker-graphical.png?v={{pkg.version}}"
      title="SwiftUI DatePicker graphical style">
    <figcaption>DatePicker graphical style</figcaption>
  </figure>

- `.stepperField` - This not available in iOS.

- `.wheel` - This provides separate wheel pickers for month, day, and year.

  <figure>
    <img alt="SwiftUI DatePicker wheel style"
      src="/blog/assets/SwiftUI-DatePicker-wheel.png?v={{pkg.version}}"
      title="SwiftUI DatePicker wheel style">
    <figcaption>DatePicker wheel style</figcaption>
  </figure>

### ColorPicker

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/colorpicker",
"ColorPicker" %} view renders a color well
for displaying a currently selected color
and changing the color using the system color picker.
It takes a label and a `selection` argument
that is a binding to the currently selected `Color`.

<figure>
  <img alt="SwiftUI ColorPicker before tap"
    src="/blog/assets/SwiftUI-ColorPicker1.png?v={{pkg.version}}"
    title="SwiftUI ColorPicker before tap">
  <figcaption>ColorPicker before tap</figcaption>
</figure>
<figure>
  <img alt="SwiftUI ColorPicker after tap"
    src="/blog/assets/SwiftUI-ColorPicker2.png?v={{pkg.version}}"
    title="SwiftUI ColorPicker after tap">
  <figcaption>ColorPicker after tap</figcaption>
</figure>

```swift
ColorPicker(
    "Favorite Color",
    selection: $favoriteColor // a binding
)
```

### Label

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/label",
"Label" %} view renders an icon and text where the icon appears first.
`Label` supports many initializers but the most commonly used
take the text as a `String` followed by the icon
as either a `systemName` `String` (for an SF Symbol)
or a `image` `String` (for an image asset name).

For example:

```swift
Label("Rain", systemImage: "cloud.rain")
```

Applying the `font` view modifier scales both the text and the icon.

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/labelstyle", "labelStyle" %}
view modifier changes which parts are rendered.
The default argument value is `.titleAndIcon` which renders both parts.
To render only the text, pass `.titleOnly`.
To render only the icon, pass `.iconOnly`.

### ProgressView

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/progressview",
"ProgressView" %} displays a progress indicator.
It takes a label,
an optional `value` argument that is a binding to the current value,
and an optional `total` argument that is the maximum value (default is 0.0).
If the `value` argument is supplied then the determinate style
that displays a thin progress bar is used.
If the `value` argument is omitted then the indeterminate style
that uses the standard Apple spinner is used.

```swift
struct ContentView: View {
    @State private var progress = 0.0

    func startTimer() {
        // Run every 3 hundredths of a second.
        // When "repeats" is false, this is like setTimeout in JavaScript.
        // When "repeats" is true, this is like setInterval in JavaScript.
        Timer.scheduledTimer(withTimeInterval: 0.03, repeats: true) { timer in
            progress += 0.01
            if (progress >= 1) {
                timer.invalidate() // stops Timer
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

To change the color of a `ProgressView`
apply the `tint` view modifier passing it a `Color`.

To change the size of an indeterminate `ProgressView`
apply the `scaleEffect` modifier. For example:

```swift
ProgressView()
    .scaleEffect(x: 3, y: 3, anchor: .center)
```

### Gauge

This shows a current value in relation to minimum and maximum values.
One example is a car fuel gauge.
This is currently only supported in watchOS.

### EmptyView

This renders nothing. It is useful in cases where
a view needs to be returned, but there is nothing to display.

### EquatableView

TODO: What is this?

### AnyView

TODO: What is this?

### TupleView

TODO: What is this?

## Fonts

The `font` view modifier specifies the font, size, and weight
to be used in a view.
This is typically applied to `Text` views and
`Image` views that display an SF Symbol icon.

Here are examples of using the default system font:

```swift
Text("Hello").font(.system(size: 24, weight: .bold))
Image(systemName: "cloud.snow").font(.system(size: 64))
```

Another option is to use "Dynamic Type" font names
whose size changes based on user preferences.
The dynamic font names are listed below in order from largest to smallest size.
Each is followed by the corresponding fixed size
when the user has not changed their text size preference.

- `largeTitle` (35)
- `title` (28)
- `title2` (23)
- `title3` (20)
- `headline` (17) // same size as .body but bold
- `body` (17)
- `callout` (16)
- `subheadline` (15)
- `footnote` (13)
- `caption` (12)
- `caption2` (11)

Users can scale the dynamic fonts used in all apps from
Settings ... Display & Brightness ... Text Size.
Move the slider to one of the seven options.
The middle option (4th one) is the default.
The screenshots below show all the dynamic fonts in the largest (1st option),
default (4th option), and smallest (7th option) text sizes.

<img alt="SwiftUI large dynamic fonts" style="width: 30%"
  src="/blog/assets/SwiftUI-dynamic-fonts-large.png?v={{pkg.version}}"
  title="SwiftUI large dynamic fonts">
<img alt="SwiftUI medium dynamic fonts" style="width: 30%"
  src="/blog/assets/SwiftUI-dynamic-fonts-medium.png?v={{pkg.version}}"
  title="SwiftUI medium dynamic fonts">
<img alt="SwiftUI small dynamic fonts" style="width: 30%"
  src="/blog/assets/SwiftUI-dynamic-fonts-small.png?v={{pkg.version}}"
  title="SwiftUI small dynamic fonts">

Fonts can be scaled even larger using an accessibility option.
Select Settings ... Accessibility ... Display & Text Size ... Larger Text.
Enable "Larger Accessibility Sizes" and move the slider to one of the 12 options.
The screenshot below shows all the dynamic fonts in the largest possible
accessibility size.

<img alt="SwiftUI accessibility dynamic fonts" style="width: 30%"
  src="/blog/assets/SwiftUI-dynamic-fonts-accessibility.jpg?v={{pkg.version}}"
  title="SwiftUI accessibility dynamic fonts">

In apps that use dynamic fonts, test all screens at various font size
preferences to verify that the resulting layout is acceptable.

## foregroundStyle

They {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/foregroundstyle(_:)",
"foregroundStyle" %} view modifier is used style any kind of `View`
that supports styling with a {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/shapestyle", "ShapeStyle" %}.
This includes `Text` and views that conform to the `Shape` protocol.
For example, it can be used to add gradient color to text.

<img alt="SwiftUI foregroundStyle gradient" style="width: 30%"
  src="/blog/assets/SwiftUI-foregroundStyle-gradient.png?v={{pkg.version}}"
  title="SwiftUI foregroundStyle gradient">

```swift
Text("Hello, World!")
    .foregroundStyle(
        .linearGradient(
            colors: [.blue, .red],
            startPoint: .leading,
            endPoint: .trailing
        )
    )
```

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/hierarchicalshapestyle",
"HierarchicalShapeStyle" %} struct defines the static properties
`primary`, `secondary`, `tertiary`, and `quaternary`.
They can be used to style text and shapes.
They are defined by the OS and cannot be modified.
The only difference I see between them is that
they vary from dark gray to light gray.
They are typically passed to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/foregroundstyle(_:)",
"foregroundStyle" %} view modifier as shown below:

These are typically passed to the `foregroundStyle` view modifier
as shown below:

<img alt="SwiftUI foregroundStyle HierarchicalShapeStyle" style="width: 30%"
  src="/blog/assets/SwiftUI-foregroundStyle-HierarchicalShapeStyle.png?v={{pkg.version}}"
  title="SwiftUI foregroundStyle HierarchicalShapeStyle">

```swift
Text("Primary").foregroundStyle(.primary)
Text("Secondary").foregroundStyle(.secondary)
Text("Tertiary").foregroundStyle(.tertiary)
Text("Quaternary").foregroundStyle(.quaternary)
```

## Safe Area

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/ignoressafearea(_:edges:)",
"ignoresSafeArea" %} view modifier expands a view so it can
extend outside of the safe area which is the area of the screen
that excludes the top navigation bar and the bottom toolbar.

This is often used to provide a background color.
For example:

```swift
ZStack {
    Rectangle().fill(.blue.gradient).ignoresSafeArea()
    VStack {
        Text("Hello").font(.largeTitle)
    }
}
```

## Floating Action Button

SwiftUI does not provide a floating action button view.
This is a circular button that hovers of the content of a screen.
They typically appear in the lower-right or lower-left corner of the screen.
Floating action buttons are popular in Android applications.

The following code demonstrates one way to
implement a floating action button in SwiftUI:

```swift
import SwiftUI

struct FloatingActionButton: View {
    var label: String = "+"
    var action: () -> Void

    let diameter: CGFloat = 70
    let shadow: CGFloat = 3

    var body: some View {
        Button(
            action: action,
            label: {
                ZStack(alignment: .center) {
                    Circle()
                        .background(Color.blue)
                        .frame(width: diameter, height: diameter)
                    Text(label)
                        .font(.system(size: 60))
                        .foregroundColor(.white)
                        .padding(.bottom, 7)
                        .frame(height: diameter)
                }
            }
        )
        .cornerRadius(diameter / 2)
        .padding(.trailing, 35)
        .shadow(
            color: .black.opacity(0.3),
            radius: shadow,
            x: shadow,
            y: shadow
        )
    }
}

struct FloatingActionButton_Previews: PreviewProvider {
    static var previews: some View {
        ZStack(alignment: .bottomTrailing) {
            Color.yellow.ignoresSafeArea()
            FloatingActionButton() {
                print("got tap")
            }
        }
    }
}
```

## Combining Container and Component Views

The following code demonstrates using both container and component views.
Note how views can be defined in computed properties
that are later referenced to render them.
The type of these computed properties can be a specific container view type
or the generic type `some View`.
Custom views can also be defined in a new struct that inherits from `View`
Instances of these structs can be created to render them.

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
    // This assigns a View to a computed property.
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
            // This refers to a variable to get a View
            // and chains View modifiers onto it.
            row.padding().border(.red)
            MyRow() // This creates a custom View instance.
        }
    }
}
```

Note how `ContentView` uses the view `MyRow`.
It is preferred to create small views like this and compose them
rather than creating views whose code is long and deeply nested.

## Drawing Views

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/shape",
"Shape" %} protocol inherits from the `View` protocol
and there are many provided views that inherit from `Shape`.
Examples include {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/circle", "Circle" %} and
{% aTargetBlank "https://developer.apple.com/documentation/swiftui/rectangle",
"Rectangle" %}.

By default, all views that inherit from `Shape` are
filled with the foreground color of their parent view.
This can be changed using the `fill` view modifier.
It takes an object of a type that implements the `ShapeStyle` protocol.
Examples include `Color`, `AngularGradient`, `LinearGradient`,
`RadialGradient`, and `ImagePaint`.

An outline can be added to any `Shape` with the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/shape/stroke(linewidth:)",
"stroke" %} and {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/insettableshape/strokeborder(_:style:antialiased:)",
"strokeBorder" %} view modifiers.
The difference between these becomes apparent
when the border width is greater than one.
`stroke` is drawn so it is centered on the edge of the shape
with half inside and half outside.
`strokeBorder` is inset so none of the stroke is outside of the shape.

To fill a shape apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/shape/fill(style:)",
"fill" %} view modifier.

It is not possible to apply both the `fill` view modifier
and either the `stroke` or `strokeBorder` view modifier.
One way to render a shape is filled AND has a border
is to render it twice in a `ZStack` where
the bottom shape is filled and the top shape has a stroke.
Another way is to render the shape once with a stroke
and add a background version of the shape that is filled.
Examples of both approaches follow:

```swift
ZStack {
    Circle().fill(Color.yellow)
    Circle().stroke(.red, lineWidth: 10)
}
.frame(width: 100, height: 100)

Circle()
    .stroke(.red, lineWidth: 10)
    .background(Circle().fill(.yellow))
    .frame(width: 100, height: 100)
```

The following example code draws several shapes:

<img alt="SwiftUI Shapes" style="width: 40%"
  src="/blog/assets/SwiftUI-Shapes.png?v={{pkg.version}}"
  title="SwiftUI Shapes">

```swift
struct ContentView: View {
    let linearGradient = LinearGradient(
        // Any number of colors can be specified.
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
        }
        .padding()
    }
}
```

To render a fraction of a shape, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/shape/trim(from:to:)",
"trim" %} view modifier. This takes `from` and `to` arguments
that are both percentages between 0 and 1.
For example:

```swift
// In a Circle, 0 is at 0 degrees (right)
// and it is drawn clockwise.
// Percentage values are a percentage along the perimeter.
// 0.25 is at 270 degrees (bottom),
// 0.5 is at 180 degrees (left), and
// 0.75 is at 90 degrees (top).
Circle()
    .trim(from: from, to: to)
    .stroke(.red, lineWidth: 10)
    .frame(width: 100, height: 100)

// In a Rectangle, zero is at the upper right
// and it is drawn clockwise.
// Percentage values are a percentage along the perimeter.
Rectangle()
    .trim(from: from, to: to)
    .stroke(.blue, lineWidth: 10)
    .frame(width: 200, height: 100)
```

To modify the zero location of the trim,
rotate the shape. For example:

```swift
Circle()
    .trim(from: from, to: to)
    .stroke(.red, lineWidth: 10)
    .frame(width: 100, height: 100)
    .rotationEffect(.degrees(90))
```

Trimming filled shapes doesn't typically produce a desired result.

### Angle

This is a struct that does not implement the `View` protocol,
but is used by several drawing views.
Instances can be created using an initializer
that takes either a `degrees` or a `radians` argument.
The value can be obtained via either `degrees` or `radians` properties
and conversions are performed automatically.

```swift
let angle = Angle(radians: Double.pi)
print(angle.degrees) // 180.0
```

### Path

Like `Color`, `Path` also creates a view.
The following example draws a path for a triangle
that is both filled and stroked.

<img alt="SwiftUI Path" style="width: 40%"
  src="/blog/assets/SwiftUI-Path.png?v={{pkg.version}}"
  title="SwiftUI Path">

```swift
// Define the path as a computed property so it can be
// used once for filling and once for stroking.
var path: Path {
    Path { path in
        path.move(to: CGPoint(x: 0, y: width))
        path.addLine(to: CGPoint(x: halfWidth, y: 0))
        path.addLine(to: CGPoint(x: width, y: width))
        path.closeSubpath()
    }
}

// Inside some container view ...
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

The `Path` view supports many drawing methods including:
`addArc`, `addCurve`, `addEllipse`, `addLines`, `addPath`, `addQuadCurve`,
`addRect`, `addRects`, `addRelativeArc`, and `addRoundedRect`.

### Other Drawing Views

Many drawing views draw exactly what their name implies.
These include `Circle`, `Ellipse`, `Rectangle`,
and `RoundedRectangle` (has rounded corners).

To add a drop shadow to a shape, apply the `fill` view modifier.
For example:

```swift
Circle()
    .fill(.yellow.shadow(.drop(color: .black, radius: 10)))
    .frame(width: 100)
```

Other drawing views are less obvious from their name, including:

- `Anchor`: ?
- `AnimatablePair`: ?
- `Animation`: ?
- `AnyShapeShape`: ?
- `AnyTransition`: ?
- `Capsule`: draws an oval
- `ContainerRelativeShape`: ?
- `EmptyAnimatableData`: ?
- `OffsetShape`: ?
- `ProjectionTransform`: ?
- `RotatedShape`: ?
- `ScaledShape`: ?
- `TransformedShape`: ?
- `UnitPoint`: ?

## Other Views

### GeometryReader

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/geometryreader",
"GeometryReader" %} view takes all the space offered to it, wraps other views,
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

### Spacer

Each `Spacer` view takes an equal amount of the unused space
inside its parent container view.
It accepts an optional `minLength` attribute which defaults to zero.

Using `Spacer` can be compared to web applications that use
the CSS properties `display: flex;` and `justify-content`
with the values shown in the following table:

| CSS justify-content value | Spacer placement              |
| ------------------------- | ----------------------------- |
| flex-start                | at end of list of views       |
| flex-end                  | at beginning of list of views |
| space-between             | one between each child view   |

### Divider

This draws a light gray, 1-pixel wide line across the container.
The line is vertical in an `HStack` and horizontal in a `VStack`.

The line can be customized in several ways:

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

## AttributedString

The `AttributedString` struct supports associating specific "attributes"
with substrings in a `String` that it holds.
For example, the string "Red Green Blue" can have attributes
that specify the foreground color to use for each word.
When rendered, it can apply the attributes to affect styling.

For basic styling such as making parts of a string bold or italic,
it is not necessary to use `AttributedString` because
the `Text` view supports a subset of Markdown syntax.
However, this only works when a literal `String` is passed.
To pass a variable of type `String` that contains Markdown syntax,
use `Text(.init(myVariable))`.

There are several approaches that can be used to associate attributes
with substrings in an `AttributedString` instance.
Adding extension methods to `AttributedString` and `Text`
greatly simplifies this.

<img alt="SwiftUI AttributedString" style="width: 50%"
  src="/blog/assets/SwiftUI-AttributedString.png?v={{pkg.version}}"
  title="SwiftUI AttributedString">

```swift
extension AttributedString {
    // Style the range occupied by a given substring using a closure.
    mutating func style(
        text: String,
        style: (inout AttributedSubstring) -> Void
    ) {
        if let range = self.range(of: text) {
            style(&self[range])
        }
    }
}

extension Text {
    // Creates a Text view from a String and styles the entire value.
    init(_ string: String, style: (inout AttributedString) -> Void) {
        var attributedString = AttributedString(string)
        style(&attributedString) // style using the closure
        self.init(attributedString) // create a Text view
    }
}

struct ContentView: View {
    @Environment(\.font) var font // default font

    // Using extension to AttributedString
    var demo: AttributedString {
        var s = AttributedString("Red Green Blue")
        s.style(text: "Red") {
            $0.foregroundColor = .red
            $0.font = .body.italic() // italic version of body font
        }
        s.style(text: "Green") {
            $0.foregroundColor = .green
        }
        s.style(text: "Blue") {
            $0.foregroundColor = .purple
            // Use the italic version of either the default or body font.
            $0.font = (font ?? .body).italic()
        }
        return s
    }

    var body: some View {
        VStack {
            // Using built-in Markdown support
            Text("plain *italic* **bold** ~strike~ `code`, [link](https://apple.com)")

            // Using a computed property defined above
            Text(demo)

            // Concatenating Text views that each have their own styles.
            Text("Hello").foregroundColor(.red) +
                Text(", ") +
                Text("World").foregroundColor(.green) +
                Text("!")

            HStack {
                // Using extension to Text.
                Text("Red") {
                    $0.foregroundColor = .red
                    $0.font = Font.system(size: 24).bold().italic()
                }
                Text("Green") {
                    $0.foregroundColor = .green
                    $0.font = Font.system(size: 36, design: .monospaced)
                }
                Text("Blue") {
                    $0.foregroundColor = .blue
                    $0.underlineColor = .green // doesn't work
                }
            }

            // Using extension to Text.
            Text("Apple") {
                $0.link = URL(string: "https://apple.com")
                $0.underlineColor = .blue // doesn't work
            }
        }
    }
}
```

## Layout Details

Container views offer space to their child views.
The child views can choose their size within the space offered to them.
Container views then position the contained views knowing their sizes.
Container views can also then choose their own size
that perhaps differs from they offered to their child views.

Some views are "inflexible" and want to be a specific size.
Examples including `Text` and `Image` views.
Other views are "flexible" and can adapt to the space offered to them.
Examples include `Circle` and `RoundedRectangle`.

Container views give space to inflexible child views first and
then divide the remaining space between the flexible child views.
The priority with which container views give space to child views
can be altered by applying the `layoutPriority` view modifier to child views.
It is passed a float value that defaults to zero.

When a container view contains at least one flexible view,
it is also considered to be flexible.

Unsafe areas, such as the area at the top of iPhones that have a camera bump,
are removed from offered space by default.
Sometimes it is useful to draw in those areas.
One example, shown below, is displaying a background image.

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
    .edgesIgnoringSafeArea(.all) // allows drawing in unsafe areas
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
  Also see the `task` method.

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

  This registers a closure to be invoked
  when the value of a given state property changes.
  The closure is passed the new value.
  If the closure takes no arguments, the somewhat confusing error message
  "Type '()' cannot conform to 'Equatable'" will appear.
  To fix this, add a parameter to the closure
  which can be `_` if the value is not used.

- `onContinueUserActivity`
- `onHover`
- `onOpenURL`
- `onReceive`
- `onSubmit`

The `View` method `onTapGesture` has a needlessly long name.
To use `onTap` instead, add this extension method:

```swift
import SwiftUI

extension View {
    public func onTap(
        count: Int = 1,
        perform: @escaping () -> Void
    ) -> some View {
        onTapGesture(count: count, perform: perform)
    }
}
```

## Keyboard Shortcuts

To defined keyboard shortcuts, apply the `keyboardShortcut` view modifier.
Pass this a `String` that describes the key (ex. "x").
This assumes the command key will be held down.

Alternatively the `keyboardShort` view modifier can be passed a "semantic key".
There are only two defined semantic keys.
The value `.cancelAction` refers to the escape key.
The value `.defaultAction` refers to the return key.

For example:

```swift
struct ContentView: View {
    @State private var message = "Tap a button."

    var body: some View {
        VStack {
            Button("First") {
                message = "You selected First."
            }
            .keyboardShortcut("f") // cmd-f

            Button("Second") {
                message = "You selected Second."
            }
            .keyboardShortcut(
                .return,
                modifiers: [.command, .shift]
            ) // cmd-shift-return

            Text(message)
        }
    }
}
```

To require modifier keys other than the default command key,
add the `modifiers` argument with a value that is
an array of all the required {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/eventmodifiers",
"EventModifiers" %} values. These include `.capslock`, `.command`,
`.control`, `numericPad`, `option`, and `shift`.
For example, to require the command key and the shift key
add `modifiers: [.command, .shift]`.

On an iPad with a physical keyboard, hold down the command key
to see a keyboard shortcut overlay that lists the available keyboard shortcuts.

## Controlling Focus

The `@FocusState` property wrapper is used to
track and modify which input view currently has focus.
The following code demonstrates its use:

```swift
import SwiftUI

struct ContentView: View {
    enum Field: Hashable {
        case firstName, lastName
    }

    @FocusState private var focus: Field? // cannot initialize here

    @State private var firstName = ""
    @State private var lastName = ""
    @State private var message = ""
    @State private var showError = false
    @State private var showWelcome = false

    var body: some View {
        VStack {
            TextField("First Name", text: $firstName)
                .focused($focus, equals: .firstName)
            TextField("Last Name", text: $lastName)
                .focused($focus, equals: .lastName)
            Button("Submit", action: submit)
                .buttonStyle(.borderedProminent)
            Spacer()
        }
        .autocorrectionDisabled(true)
        .textFieldStyle(.roundedBorder)
        .padding()
        .onAppear {
            focus = .firstName // initial focus
        }
        .alert(
            "Invalid Input",
            isPresented: $showError,
            actions: {}, // no custom buttons
            message: { Text(message) }
        )
        .alert(
            "Welcome",
            isPresented: $showWelcome,
            actions: {}, // no custom buttons
            message: { Text("Hello, \(firstName) \(lastName)!") }
        )
    }

    private func submit() {
        if firstName.isEmpty {
            message = "First name is required"
            focus = .firstName
        } else if lastName.isEmpty {
            message = "Last name is required"
            focus = .lastName
        } else if lastName.count < 2 {
            message = "Last name is too short"
            focus = .lastName
        } else {
            message = ""
            focus = nil // dismisses keyboard
        }

        showWelcome = message.isEmpty
        showError = !showWelcome
    }
}
```

## Dismissing Keyboard

It's good practice to provide a way for the user to dismiss the keyboard.
One approach is to add a button at the top of the keyboard
using the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/toolbar(content:)-5w0tj",
"toolbar" %} view modifier. For example:

<img alt="SwiftUI Keyboard Dismiss" style="width: 33%"
  src="/blog/assets/SwiftUI-keyboard-dismiss.png?v={{pkg.version}}"
  title="SwiftUI Keyboard Dismiss">

```swift
import SwiftUI

extension View {
    #if os(iOS) // not supported in watchOS
        @available(iOSApplicationExtension, unavailable)
        func dismissKeyboard() {
            UIApplication.shared.sendAction(
                #selector(UIResponder.resignFirstResponder),
                to: nil,
                from: nil,
                for: nil
            )
        }
    #endif
}

struct ContentView: View {
    @State private var name = ""

    var body: some View {
        VStack {
            TextField("Name", text: $name)
                .textFieldStyle(.roundedBorder)
                .toolbar {
                    ToolbarItemGroup(placement: .keyboard) {
                        Button(action: dismissKeyboard) {
                            Image(systemName: "keyboard.chevron.compact.down")
                        }
                    }
                }
            if !name.isEmpty {
                Text("Hello, \(name)!")
            }
        }
        .padding()
    }
}
```

## Environment

SwiftUI provides many values to all views through the "environment".
Any view in the view hierarchy can access environment data
using the `@Environment` property wrapper.

Highlights of environment data include:

- `accessibilityReduceMotion: Bool`

  This captures the user preference for reducing motion.
  It can be used to determine the kinds of animations that should be used.

- `colorScheme: ColorScheme`

  This is an `enum` with the cases `light` and `dark`.

- `defaultWheelPickerItemHeight: CGFloat`

  This can be used to calculate the `frame` height needed for a wheel picker.

- `managedObjectContext: NSManagedObjectContext`

  This holds the context being used for Core Data.

- `locale: Locale`

  This holds the current locale which can be used for internationalization.

- `editMode: EditMode`

  This indicates whether the user can currently edit view content.
  It is an `enum` with the cases `active`, `inactive`,
  and `transient` (temporary edit mode).

- `font: Font?`

  This describes the default font.

It is also possible to add custom data to the environment.
Any view below the view where the environment data is added can access it.

The following example demonstrates this by adding
the custom environment value "primaryColor".
Environment values provided by Apple are accessed and modified in the same way.

```swift
import SwiftUI

// Define a custom environment key and its default value.
private struct PrimaryColorKey: EnvironmentKey {
    static let defaultValue = Color.red
}

// Add the custom environment value.
extension EnvironmentValues {
    var primaryColor: Color {
        get { self[PrimaryColorKey.self] }
        set { self[PrimaryColorKey.self] = newValue }
    }
}

struct ChildView: View {
    @Environment(\.primaryColor) var primaryColor

    var body: some View {
        VStack {
            // Renders in blue.
            Text("in ChildView").foregroundColor(primaryColor)
            GrandchildView()
        }
    }
}

struct GrandchildView: View {
    @Environment(\.primaryColor) var primaryColor

    var body: some View {
        // Renders in blue.
        Text("in GrandchildView").foregroundColor(primaryColor)
    }
}

struct ContentView: View {
    @Environment(\.primaryColor) var primaryColor

    var body: some View {
        VStack {
            // Renders in red when run in the Simulator
            // and in green when run in Preview.
            Text("in ContentView").foregroundColor(primaryColor)
            // Override the default value of primaryColor
            // for all views under ChildView.
            ChildView().environment(\.primaryColor, .blue)

        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        // Override the default value of primaryColor for entire app.
        ContentView().environment(\.primaryColor, .green)
    }
}
```

## State Detection

To detect when the app moves between the
foreground, background, and inactive states, get the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/scenephase",
"scenePhase" %} from the environment and apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/onchange(of:perform:)",
"onChange" %} view modifier to any view
to watch for changes to the `scenePhase`.

For example:

```swift
struct ContentView: View {
    @Environment(\.scenePhase) var scenePhase

    var body: some View {
        Button("Tap Me!") {
            print("got tap")
        }
        .onChange(of: scenePhase) { newPhase in
            print("phase is now \(newPhase)")
        }
    }
}
```

An app can have multiple scenes.

An app is considered active if any of its scenes are active
which means a scene is in the foreground (visible) and interactive.

An app is considered inactive when none of its scenes are active.
One or more scenes might be visible, but none are interactive.
This happens when the user swipes up from the bottom of the screen
to reveal the "App Switcher".
This also happens when in multi-tasking mode and
the app is visible but another app is active.

An app moves to the background state when another app becomes active.

## AppDelegate

SwiftUI apps, unlike UIKit apps, are not required to define
a class to be defined that conforms to the {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uiapplicationdelegate",
"UIApplicationDelegate" %} protocol.
This protocol defines many methods are automatically invoked
at key points in the application lifecycle including:

- app becomes active
- app becomes inactive
- app is about to move to the background
- app has moved to the background
- app is about to move to the foreground
- app is about to terminate
- app is about to leave the background and become active
- app receives a memory warning
- app receives a request to preserve its state
- app receives a request to restore its state
- and many more

While not required, there are situations where have such a class is useful.
To add use of an `AppDelegate` to a SwiftUI app:

1. Define a class, typically named `AppDelegate` that
   inherits from `NSObject` and conforms to `UIApplicationDelegate`.

   ```swift
   class AppDelegate: NSObject, UIApplicationDelegate {
       // Define lifecycle methods here.
   }
   ```

2. Add the following inside the main struct that inherits from `App`:

   ```swift
   @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
   ```

## Detecting Device Orientation

The device orientation can be detected by listening for
`orientationDidChangeNotification` notifications.
The "Hacking With Swift" post {% aTargetBlank
"https://www.hackingwithswift.com/quick-start/swiftui/how-to-detect-device-rotation",
"How to detect device rotation" %} describes implementing a custom view modifier
that simplifies doing this.
The following is a customized version of the code in that post.

```swift
import SwiftUI

struct OrientationViewModifier: ViewModifier {
    // This is a function that will called when the orientation changes.
    let action: (UIDeviceOrientation) -> Void

    func body(content: Content) -> some View {
        content
            // We must have "onAppear" in order to use "onReceive",
            // but it doesn't need to do anything.
            .onAppear()
            .onReceive(
                NotificationCenter.default
                    .publisher(
                        for: UIDevice
                            .orientationDidChangeNotification
                    )
            ) { _ in
                action(UIDevice.current.orientation)
            }
    }
}

// Define a custom view modifier.
extension View {
    func onRotate(
        action: @escaping (UIDeviceOrientation) -> Void
    ) -> some View {
        modifier(OrientationViewModifier(action: action))
    }
}

struct ContentView: View {
    // UIDeviceOrientation is an enum defined by UIKit.
    @State private var orientation = UIDeviceOrientation.unknown

    var description: String {
        switch orientation {
        case .faceDown, .faceUp:
            return "flat"
        case .landscapeLeft, .landscapeRight:
            return "landscape"
        case .portrait, .portraitUpsideDown:
            return "portrait"
        case .unknown:
            return "unknown"
        @unknown default:
            fatalError("unknown UIDeviceOrientation value")
        }
    }

    var body: some View {
        Text(description)
            .onRotate { orientation in
                self.orientation = orientation
            }
    }
}
```

The {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uideviceorientation",
"UIDeviceOrientation" %} enum also defines the following computed properties:

- `isFlat`: true for `.faceDown` and `.faceUp`
- `isLandscape`: true for `.landscapeLeft` and `.landscapeRight`
- `isPortrait`: true for `.portrait` and `.portraitUpsideDown`
- `isValidInterfaceOrientation`: true if one of the portrait or landscape values

## Search

SwiftUI provides a search input containing a magnifier glass icon
that is rendered by the `searchable` view modifier.
This is typically applied to a `List` view.

The following code demonstrates filtering a `List`
using the `searchable` view modifier.

<img alt="SwiftUI Search" style="width: 50%"
  src="/blog/assets/SwiftUI-Search.png?v={{pkg.version}}"
  title="SwiftUI Search">

```swift
struct ContentView: View {
    @State private var query = ""

    private let people = [
        "Mark",
        "Tami",
        "Amanda",
        "Jeremy",
        "Meghan",
        "RC"
    ]

    // Computed property based on people and query.
    private var matchingPeople: [String] {
        let lower = query.lowercased()
        return lower.isEmpty ?
            people :
            people.filter { $0.lowercased().contains(lower) }
    }

    // This is rebuilt when the value of query changes.
    var body: some View {
        NavigationView {
            List(matchingPeople, id: \.self) { person in
                Text(person)
            }
            .searchable(
                text: $query,
                // Without this the search input will be hidden
                // until the user drags down on the List.
                placement: .navigationBarDrawer(displayMode: .always),
                prompt: "Person Name"
            )
            .autocapitalization(.none)
            .navigationTitle("People")
        }
    }
}
```

## Animation

Many view properties can be animated.
These include color, opacity, rotation, and scale.
Note that font sizes cannot be animated,
but views containing text can be scaled.

SwiftUI supports three ways of implementing animations.

- explicit: wraps code that changes a model or `@State` data
  with a call to `withAnimation`
- implicit: uses the `animation` view modifier
- transition: triggers when a view is inserted or removed

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
An easing function controls the speed at which an animation advances
over its duration.

Provided easing functions include
`linear`, `easeIn`, `easeOut`, `easeInOut` (default), and `spring`.
These are static functions on the `Animation` struct.
All but `spring` take a single, optional argument
which is the `duration` in seconds.
The `spring` function takes three optional arguments named
`response`, `dampingFunction`, and `blendDuration`.
For details, see the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation/spring(response:dampingfraction:blendduration:)",
"spring method" %}.

Custom easing functions can be defined with the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation/timingcurve(_:_:_:_:duration:)",
"timingCurve" %} function.

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
            .animation(easingFunction, value: on ? 1 : 0) // implicit animation

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

Transitions can be applied to any kind of view including container views.
They are specified when a view is defined,
but they are only applied when the view is inserted or removed.
This is implemented by using an `if` or `switch` statement inside a parent view.

By default an `opacity` transition (fade) is used.
This can be changed by applying the `transition` view modifier
which is passed the kind of transition to perform.
Transitions are defined as methods on the `AnyTransition` struct.

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
to smoothly move views between container views.
For example, this can be used to move `Text` views that describe food items
between lists of foods that available and those that have been selected.
Each food item must have a unique id.
When rendering the food items, `matchedGeometryEffect` is used
to associate the `Text` view with a particular id in a given namespace.

The following code demonstrates this:

```swift
struct Food: Identifiable {
    var name: String
    var selected: Bool = false
    var id: String { name }
}

struct ContentView: View {
    @Namespace private var foodNS

    @State private var foods: [Food] = [
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

TODO: Why do the food names eventually disappear after being moved
in both Preview and the Simulator?

## Camera

To enable camera access in a SwiftUI app:

1. In the Navigator, select the root group to open the project editor.
2. In the project editor, select the target.
3. Select the "Info" tab.
4. Click the "+" after any existing key to create a new key.
5. For the key name enter "Privacy - Camera Usage Description".
6. For the key value, enter a user prompt like "Please allow camera access."

Preview and the Simulator cannot access the camera.
To test camera access, run the app on a real device.

TODO: Add detail on taking photos from an app.

## Photo Library

TODO: Add detail on accessing images in the Photos app.

## Scanning QR Codes and Barcodes

The library "CodeScan" makes this easy. The YouTube video
{% aTargetBlank "https://www.youtube.com/watch?v=j3MODOPZINs",
"Scanning QR codes with SwiftUI" %} demonstrates using this.

- Select File ... Add Packages...
- Enter {% aTargetBlank "https://github.com/twostraws/CodeScanner",
  "CodeScanner" %}" in the search input
- Click the "Add Package" button.
- Click the next "Add Package" button.

TODO: Add more detail on using this package.

## Audio

The {% aTargetBlank
"https://developer.apple.com/documentation/avfaudio/avaudioplayer",
"AVAudioPlayer" %} class in the {% aTargetBlank
"https://developer.apple.com/documentation/avkit", "AVKit" %} framework
enables playing audio in several formats such as MP3.

A good source of free audio files is
{% aTargetBlank "https://freesoundslibrary.com", "Free Sounds Library" %}.
Download audio files and copy them into an Xcode project next to `.swift` files.

To add an audio file to the project:

- Do not add the video file to `Assets.xcassets`.
- Drag an audio file into the Navigator. A dialog will appear.
- In the "Destination" section, check the "Copy items if needed" checkbox.
- In the "Add to target" section, check the app target checkbox.
- Click the "Finish" button.

The following example creates buttons
that each play a different sound when tapped.
It relies on having the files `click.mp3` and `ding.mp3`
copied into the project.

```swift
import AVKit
import SwiftUI

// This makes player a "retained" variable which is necessary
// so the player is retained while the sound is playing.
private var player: AVAudioPlayer?

struct ContentView: View {
    @State private var message: String?

    private func play(_ name: String) {
        let url = Bundle.main.url(forResource: name, withExtension: ".mp3")
        guard let url else {
            message = "\(name).mp3 not found"
            return
        }

        do {
            player = try AVAudioPlayer(contentsOf: url)
            player?.play()
            message = nil
        } catch {
            message = "error playing sound: \(error)"
        }
    }

    var body: some View {
        VStack {
            HStack {
                Button("Click") { play("click") }
                Button("Ding") { play("ding") }
            }
            .buttonStyle(.bordered)
            if let message {
                Text(message)
            }
            Spacer()
        }
    }
}
```

## Video

The {% aTargetBlank
"https://developer.apple.com/documentation/avfoundation/avplayer", "AVPlayer" %}
class from the {% aTargetBlank
"https://developer.apple.com/documentation/avkit", "AVKit" %} framework
enables playing videos in several formats such as MP4.
To render this player in SwiftUI, wrap it in a {% aTargetBlank
"https://developer.apple.com/documentation/avkit/videoplayer", "VideoPlayer" %}
view.

To add a video file to the project:

- Do not add the video file to `Assets.xcassets`.
- Drag a video file into the Navigator. A dialog will appear.
- In the "Destination" section, check the "Copy items if needed" checkbox.
- In the "Add to target" section, check the app target checkbox.
- Click the "Finish" button.

The video can be played in Xcode by selecting the newly added file in
the Navigator and clicking the play button at the bottom of the editor.

```swift
import AVKit
import SwiftUI

struct ContentView: View {
    // Use this approach to play video from the web.
    /*
     let url = URL(
         string: "https://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4"
     )
     */

    // Use this approach to play video from a file in the project.
    let url = Bundle.main.url(forResource: "BigBuckBunny", withExtension: "mp4")

    var body: some View {
        VStack {
            Text("Big Buck Bunny").font(.largeTitle)
            if let url {
                VideoPlayer(player: AVPlayer(url: url))
                    .frame(height: 300)
            } else {
                Text("video not found")
            }
        }
        .padding()
        .onAppear {
            print("url =", url)
        }
    }
}
```

## Dialing Phone

To initiate making a phone call using the Phone app,
use code like the following:

```swift
guard let url = URL(string: "tel://\(phoneNumber)") else {
    print("Phone: invalid phone number \(phoneNumber)")
    return
}
UIApplication.shared.open(url)
```

## Sending Text Messages

To initiate sending a custom text message using the Messages app,
use code like the following:

```swift
guard let url = URL(string: "sms:+1\(phoneNumber)") else {
    print("Phone: invalid phone number \(phoneNumber)")
    return
}
UIApplication.shared.open(url)
```

## MVVM

SwiftUI encourages use of the Model-View-ViewModel (MVVM) paradigm
which separates application code into three groups.
This differs from UIKit which encourages use of Model-View-Controller (MVC).

Any state held in a view using the `@State` property modifier
should be transient state such as data related to styling.

Models ...

- holds data and application logic
- have no knowledge of the view code that uses the data
- are structs that typically conform to the `Identifiable` protocol
  which requires having an `id` property (can have an `UUID` value);
  allows iterating over them in a `ForEach` loop

  For a unique `String` value, use `UUID().uuidString`.
  This is useful for inserting in a database.

ViewModels ...

- are classes (not structs) that implement the `ObservableObject` protocol
  (Making them classes allows multiple views
  to share a reference to the same object.)
- should be marked `final` to prevent subclassing and decrease build times
- mark the properties whose values they publish
  with the `@Published` property wrapper which is
  a shorthand for explicitly calling `objectWillChange.send()`
- are passed to views or created inside them and
  held in properties with the `@StateObject` property wrapper
  (which subscribes to ViewModel data)
- have no knowledge of the views that use them
- have methods referred to as "intents"
  that are called by by views to update model data
- optionally create and hold model instances,
  typically in `private` properties
- react to model changes by optionally transforming model data
  and publishing changes

Views ...

- decide what to render
- should be mostly stateless
- are declarative rather than imperative because they describe
  what to render based on the current data, not when to render it
- can be associated with any number of ViewModels
- subscribe to changes in ViewModels by declaring properties
  using the `@ObservedObject` or `@StateObject` property wrapper
- react to changes published by ViewModels by rebuilding their bodies
- have a `body` var that rebuilds the view
  any time there are changes in the data of ViewModels they use
- can call the `onReceive` method to register a function to be called
  when new data is received (I couldn't get this to work.)

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

    var breed: String
    var id: Int // required by the Identifiable protocol
    var name: String
    var selected = false

    // This is a computed property required by the CustomStringConvertible protocol.
    var description: String { "Dog: \(name) \(selected)" }

    init(name: String, breed: String) {
        Dog.lastId += 1
        id = Dog.lastId
        self.name = name
        self.breed = breed
    }
}

// This must be a class instead of a struct
// in order to conform to the ObservableObject protocol.
// Classes that do this gain an objectWillChange method
// that publishes changes.
// This can be called directly before changes are made.
// Alternatively, if the properties that can change are annotated with the
// @Published property wrapper, changes will be published automatically.
class Model: ObservableObject {
    // The @Published property wrapper causes changes in properties
    // that are structs, not classes, to be published.
    @Published var dogs: [Dog] = []

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

## Popovers

Popovers are often used for tooltip or help text.

The following example displays a popover above a `Text` view
when a `Button` is tapped.
On an iPad popovers are rendered like speech bubbles
with a tail pointing at an associated view.
On an iPhone popovers are rendered as sheets that slide in from the bottom.

<figure>
  <img alt="SwiftUI Popover before tap" style="width: 60%"
    src="/blog/assets/SwiftUI-popover1.png?v={{pkg.version}}"
    title="SwiftUI Popover before tap">
  <figcaption>popover before tapping</figcaption>
</figure>
<figure>
  <img alt="SwiftUI Popover after tap" style="width: 60%"
    src="/blog/assets/SwiftUI-popover2.png?v={{pkg.version}}"
    title="SwiftUI Popover after tap">
  <figcaption>popover after tapping</figcaption>
</figure>

```swift
func runAfter(seconds: Int, closure: @escaping () -> Void) {
    DispatchQueue.main.asyncAfter(
        deadline: DispatchTime.now() + 2,
        execute: closure
    )
}

struct ContentView: View {
    @State private var showHelp = false

    var body: some View {
        VStack(spacing: 30) {
            Text("Some complex text goes here.")
                .padding()
                .background(Color(UIColor.lightGray))
                .popover(isPresented: $showHelp) {
                    Text("This is help text.").padding()
                }

            Button("Help") {
                // Display popover for 2 seconds.
                // SwiftUI doesn't support showing
                // multiple popovers at the same time.
                // If this is attempted, none will be displayed.
                showHelp = true
                runAfter(seconds: 2) { showHelp = false }
            }
            .buttonStyle(.bordered)
        }
    }
}
```

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

<figure>
  <img alt="SwiftUI alert before taps" style="width: 60%"
    src="/blog/assets/SwiftUI-alert1.png?v={{pkg.version}}"
    title="SwiftUI alert before taps">
  <figcaption>alert demo before taps</figcaption>
</figure>
<figure>
  <img alt="SwiftUI alert after tapping Press Me" style="width: 60%"
    src="/blog/assets/SwiftUI-alert2.png?v={{pkg.version}}"
    title="SwiftUI alert after tapping Press Me">
  <figcaption>alert demo after tapping Press Me</figcaption>
</figure>
<figure>
  <img alt="SwiftUI alert after tapping + too many times" style="width: 60%"
    src="/blog/assets/SwiftUI-alert3.png?v={{pkg.version}}"
    title="SwiftUI alert after tapping + too many times">
  <figcaption>alert demo after tapping + too many times</figcaption>
</figure>

```swift
struct ContentView: View {
    @State private var dogCount = 0
    @State private var pressed = false
    @State private var tooManyDogs = false

    var body: some View {
        VStack {
            Button("Press Me") { pressed = true }
            HStack {
                Stepper(
                    "# of Dogs: \(dogCount)",
                    value: $dogCount,
                    in: 0...10
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
`alert` and `confirmationDialog` view modifiers.

Custom modal dialogs are implemented by displaying a "sheet".
The sheet slides in from the bottom by default.

The following example defines the custom view `MyModal`
which is displayed in the sheet.
Custom views, like any struct, can have properties
that are passed in when instances are created.
The `ContentView` struct declares the boolean property `showModal`
and passes it to the `MyModal` struct as a binding.
This allows the action of the "Close" button in `MyModal`
to set it to `false` which hides the sheet.

<figure>
  <img alt="SwiftUI sheet before tap" style="width: 60%"
    src="/blog/assets/SwiftUI-sheet1.png?v={{pkg.version}}"
    title="SwiftUI sheet before tap">
  <figcaption>sheet demo before tap</figcaption>
</figure>
<figure>
  <img alt="SwiftUI sheet after tap" style="width: 60%"
    src="/blog/assets/SwiftUI-sheet2.png?v={{pkg.version}}"
    title="SwiftUI sheet after tap">
  <figcaption>sheet demo after tap</figcaption>
</figure>

```swift
struct MySheetView: View {
    // Approach #1
    //@Binding var isPresented: Bool

    // Approach #2
    @Environment(\.dismiss) var dismiss

    var body: some View {
        ZStack {
            Color.yellow
            VStack {
                Text("This is my modal content.")
                Button("Close") {
                    //isPresented = false // Approach #1
                    dismiss() // Approach #2
                }.buttonStyle(.bordered)
            }
        }.edgesIgnoringSafeArea(.bottom)
    }
}

struct ContentView: View {
    @State private var isSheetPresented = false

    var body: some View {
        VStack {
            Text("This is my main view.")
            Button("Show Sheet") { isSheetPresented = true }
                .buttonStyle(.bordered)
                // This slides up from the bottom and covers ~90% of the screen.
                // To cover the entire screen, call fullScreenCover instead.
                .sheet(isPresented: $isSheetPresented) {
                    // Only need to pass this argument in approach #1.
                    //MySheetView(isPresented: $isSheetPresented)
                    MySheetView()
                }
        }
    }
}
```

To change the vertical size of a sheet,
apply the `presentationDetents` view modifier
to the top view passed to `.sheet` with an array of size specifiers
which can be `.medium` or `.large`.
The first value specifies the default size of the sheet.
The optional second value specifies the size of the sheet
if the user taps on the drag indicator or drags it up.
For example, `.presentationDetents([.medium, .large])`.

For more precise sizing, pass `.fraction` and `.height` values.

When only one size is specified, the drag indicator is not displayed.
When two sizes are specified, the drag indicator is displayed.
To change the visibility of the drag indicator,
apply the `presentationDragIndicator` view modifier
to the top view passed to `.sheet` with a `Bool` value.

To display full screen sheet, use the `.fullScreenCover` view modifier
in place of the `.sheet` view modifier.

## Toolbars

Toolbars are collections of buttons that can be
displayed at the top or bottom of the display.
The are created by applying the `toolbar` view modifier
to the top view inside a `NavigationView`.
Single buttons can be described inside a `ToolbarItem` view.
Multiple buttons can be described inside a `ToolbarItemGroup` view.

If there are only two buttons in a toolbar,
they are laid out as if there is a `Spacer` between them.
For other layouts, add your own `Spacer` views.

The placement of a toolbar is specified by the `placement` attribute
in the `ToolbarItem` and `ToolbarItemGroup` views.
Supported `placement` values include:

- `.navigationBarLeading`: top, left-justified
- `.navigationBarTrailing`: top, right-justified
- `.bottomBar`: bottom, centered

Other `placement` values seem to be unable to render multiple buttons.

<img alt="SwiftUI Toolbars" style="width: 40%"
  src="/blog/assets/SwiftUI-Toolbars.png?v={{pkg.version}}"
  title="SwiftUI Toolbars">

```swift
struct ContentView: View {
    @State private var selection = "Tap a toolbar button."

    var body: some View {
        NavigationView {
            Text(selection).padding()
                .navigationTitle("Toolbar Demo")
                .toolbar {
                    ToolbarItemGroup(placement: .navigationBarTrailing) {
                        Button(
                            action: { selection = "planes" },
                            label: { Image(systemName: "airplane") }
                        )
                        Button(
                            action: { selection = "trains" },
                            label: { Image(systemName: "tram") }
                        )
                        Button(
                            action: { selection = "automobiles" },
                            label: { Image(systemName: "car.fill") }
                        )
                    }
                    ToolbarItemGroup(placement: .bottomBar) {
                        Button(
                            action: { selection = "hearts" },
                            label: { Image(systemName: "suit.heart.fill") }
                        )
                        Button(
                            action: { selection = "diamonds" },
                            label: { Image(systemName: "suit.diamond.fill") }
                        )
                        Button(
                            action: { selection = "clubs" },
                            label: { Image(systemName: "suit.club.fill") }
                        )
                        Button(
                            action: { selection = "spades" },
                            label: { Image(systemName: "suit.spade.fill") }
                        )
                        Spacer()
                    }
                }
        }
    }
}
```

## Navigation

The approach described here is deprecated in iOS 16. See the new approach at
{% aTargetBlank "/blog/topics/#/blog/swift/Navigation/", "Navigation" %}.

The `NavigationView` view marks the spot where
the views associated with `NavigationLinks` will be rendered.
The example below renders four pages.
Page 1 contains links to pages 2 and 3.
Page 3 contains a link to page 4.

<img alt="SwiftUI NavigationLink page 1" style="width: 24%"
  src="/blog/assets/SwiftUI-navigation-page1.png?v={{pkg.version}}"
  title="SwiftUI NavigationLink page 1">
<img alt="SwiftUI NavigationLink page 2" style="width: 24%"
  src="/blog/assets/SwiftUI-navigation-page2.png?v={{pkg.version}}"
  title="SwiftUI NavigationLink page 2">
<img alt="SwiftUI NavigationLink page 3" style="width: 24%"
  src="/blog/assets/SwiftUI-navigation-page3.png?v={{pkg.version}}"
  title="SwiftUI NavigationLink page 3">
<img alt="SwiftUI NavigationLink page 4j" style="width: 24%"
  src="/blog/assets/SwiftUI-navigation-page4.png?v={{pkg.version}}"
  title="SwiftUI NavigationLink page 4">

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
```

If the view inside a `NavigationLink` doesn't have a `navigationTitle`,
the link to go back will just display "Back".

To go back to the previous view programmatically,
add the following to a view and call `dismiss` with no arguments.

```swift
@Environment(\.dismiss) var dismiss
```

Here is a more advanced example of using `NavigationView`.

<img alt="SwiftUI NavigationView" style="width: 32%"
  src="/blog/assets/SwiftUI-Navigation1.png?v={{pkg.version}}"
  title="SwiftUI NavigationView">
<img alt="SwiftUI NavigationView" style="width: 32%"
  src="/blog/assets/SwiftUI-Navigation2.png?v={{pkg.version}}"
  title="SwiftUI NavigationView">
<img alt="SwiftUI NavigationView" style="width: 32%"
  src="/blog/assets/SwiftUI-Navigation3.png?v={{pkg.version}}"
  title="SwiftUI NavigationView">

```swift
// SwiftUI-NavigationApp.swift
import SwiftUI

@main
struct SwiftUI_NavigationApp: App {
    // This registers use of AppDelegate defined in AppDelegate.swift.
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate

    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}
```

```swift
// AppDelegate.swift
import UIKit

// To use this, register it in the main .swift file.
class AppDelegate: UIResponder, UIApplicationDelegate {
    func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil
    ) -> Bool {
        // Change the appearance of the status and navigation bars.
        // This is not currently possible using only SwiftUI,
        // so we need to use the UIKit approach.
        let appearance = UINavigationBarAppearance()
        appearance.configureWithOpaqueBackground()
        appearance.backgroundColor = .systemRed // a bit more muted than .red
        appearance.largeTitleTextAttributes = [
            .foregroundColor: UIColor.white
            //.font: UIFont.monospacedSystemFont(ofSize: 36, weight: .bold)
        ]
        UINavigationBar.appearance().scrollEdgeAppearance = appearance

        return true
    }
}
```

```swift
// ContentView.swift
import SwiftUI

// This data is shared between pages as an environment object.
class SharedData: ObservableObject {
    @Published var v1 = 0
    @Published var v2 = "x"
}

// This displays data in an environment object
// and allows users to modify the data.
struct DataView: View {
    // This is needed to gain access to the environment object.
    @EnvironmentObject var data: SharedData

    var body: some View {
        VStack {
            Text("v1 = \(data.v1)")
            // Why does these buttons appear to become disabled
            // after tapping them in the Simulator?
            // This doesn't happen in Preview.
            Button("Add 1") { data.v1 += 1 }.buttonStyle(.bordered)
            Text("v2 = \(data.v2)")
            Button("Update") { data.v2 += "x" }.buttonStyle(.bordered)
        }
    }
}

struct MainPage: View {
    // This is needed to gain access to the environment object.
    @EnvironmentObject var data: SharedData

    // This can have a type other than String such as Int.
    @State private var selection: String? = nil

    // This is used to toggle between displaying page 4 and 5.
    @State private var pageToggle = false

    var body: some View {
        VStack {
            Text("This is on the main page.")

            // This creates two links to ChildPage views.
            ForEach(1..<3) { number in
                NavigationLink(destination: ChildPage(number: number)) {
                    Text("Go to child \(number) page")
                }
            }

            // Tab-based navigation
            // Also consider using the isActive argument which is an
            // @State Bool that indicates if the link should be activated.
            NavigationLink(
                destination: ChildPage(number: 4),
                tag: "four",
                selection: $selection
            ) {
                Text("Go to four")
            }
            NavigationLink(
                destination: ChildPage(number: 5),
                tag: "five",
                selection: $selection
            ) {
                Text("Go to five")
            }

            // Programmatic navigation to the links above
            Button("Mystery Page") {
                // Could make a REST call here and use the data
                // to determine the value of "selection".
                selection = pageToggle ? "five" : "four"

                // Could conditionally decide how to set "pageToggle"
                // which is used above to determine the value of "selection".
                pageToggle.toggle()

                // Return to the current page after 2 seconds.
                // This is like setTimeout in JavaScript.
                DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
                    selection = nil
                }
            }

            Text("data.v1 = \(data.v1)")

            // Can attach an environment object to any view like this.
            //NavigationLink(destination: DataView().environmentObject(data)) {

            // But can also attach an environment object
            // to the NavigationView (see below)
            // to make it available in all linked views.
            NavigationLink(destination: DataView()) {
                Text("Go to DataView")
            }
        }
    }
}

struct ChildPage: View {
    var number: Int

    var body: some View {
        VStack {
            Text("This is on the child \(number) page.")

            // This creates three links to GrandchildPage views.
            ForEach(1..<4) { number in
                NavigationLink(destination: GrandchildPage(number: number)) {
                    Text("Grandchild \(number)")
                }
            }
        }
        .navigationBarTitle("Child", displayMode: .inline)
    }
}

struct GrandchildPage: View {
    var number: Int

    var body: some View {
        Text("This is on the grandchild \(number) page.")
            .navigationBarTitle("Grandchild", displayMode: .automatic)
    }
}

struct ContentView: View {
    // This is registered as an environment object below.
    @ObservedObject var data = SharedData()

    // Going full screen requires hiding both
    // the status bar and the navigation bar.
    // See where this is used below.
    @State private var fullScreen = false

    var body: some View {
        // Usually NavigationView is only used at the top-level.
        // One exception is when using multiple tabs
        // where each has its own NavigationView.
        //
        // Currently SwiftUI only supports two customizations of the navbar.
        // It can be hidden entirely or only the back button can be hidden.
        // Other customizations require use of UIKit.
        NavigationView {
            VStack {
                Button("Toggle Full Screen") { fullScreen.toggle() }
                MainPage()
            }

            // The navigationBarTitle goes on a view inside NavigationView,
            // not on the NavigationView,
            // because the title can change for each page.
            // See other calls to this above.
            // The optional "displayMode" attribute has three options:
            // large (default), inline, and automatic.
            // Automatic uses large for the top view and inline for others.
            .navigationBarTitle("Main")

            .navigationBarHidden(fullScreen)

            // This adds buttons in the navigation bar,
            // only for the view to which this is applied, which is
            // the VStack above that is only rendered on the main page.
            // If the background color of the navigation bar is changed,
            // the color of these buttons typically should be changed also.
            .navigationBarItems(
                leading:
                    Button("Down") { data.v1 -= 1 }
                    .foregroundColor(.white),
                trailing:
                    HStack {
                        Button("Up") { data.v1 += 1 }
                        Button("Double") { data.v1 *= 2 }
                    }.foregroundColor(.white)
            )
        }
        .statusBar(hidden: fullScreen)

        // This makes an @ObservedObject available
        // to all linked views as an @EnvironmentObject.
        // All views that use it will have their body property
        // reevaluated if the value changes.
        .environmentObject(data)
    }
}
```

Most SwiftUI code works on all platforms.
However, watchOS does not support `NavigationView`.
Here is a shim that provides a shell implementation
so the same code that works in iOS can work in watchOS.

To run this app as a watchOS app in the simulator:

- select File ... New ... Target
- click the watchOS tab
- scroll down and select "Watch App for iOS App"
- click Next
- enter a product name
- click Finish, then Activate

TODO: Why does the Apple Watch Simulator always just display "Hello, World!"
and not what is in this app?

```swift
#if os(watchOS)
struct NavigationView<Content: View>: View {
    let content: () -> Content

    init(@ViewBuilder content: @escaping () -> Content) {
        self.content = content
    }

    var body: some View {
        VStack(spacing: 0) {
            content()
        }
    }
}
#endif
```

macOS does not support the `navigationBarTitle` view modifier.
Here is a shim that provides a shell implementation
so the same code that works in iOS can work in macOS.

To run this app as a macOS app in the Simulator,
create the app as either a Multiplatform App or a macOS app.

```swift
#if os(macOS)
extension View {
    func navigationBarTitle(_ title: String) -> some View {
        self
    }
}
#endif
```

## Sidebars

A sidebar provides app-level navigation to top-level collections of content.
For example, the iOS Mail app displays a list of mailboxes in a sidebar.
Selecting an item in the sidebar changes what is displayed
in the pane that follows.

SwiftUI creates a sidebar when the `.listStyle(.sidebar)` view modifier
is applied to a `List` view.

The following example uses a sidebar for selecting a sport.
When a sport is selected, the pane to its right
displays a list of teams in that sport and allows one to be selected.
When a team is selected, the pane to its right
displays a list of players on that team.

<img alt="SwiftUI Sidebar Page 1" style="width: 32%"
  src="/blog/assets/SwiftUI-sidebars1.png?v={{pkg.version}}"
  title="SwiftUI Sidebar Page 1">
<img alt="SwiftUI Sidebar Page 2" style="width: 32%"
  src="/blog/assets/SwiftUI-sidebars2.png?v={{pkg.version}}"
  title="SwiftUI Sidebar Page 2">
<img alt="SwiftUI Sidebar Page 3" style="width: 32%"
  src="/blog/assets/SwiftUI-sidebars3.png?v={{pkg.version}}"
  title="SwiftUI Sidebar Page 3">

```swift
// model.swift
import Foundation

struct Sport: Hashable {
    var name: String
    var teams: [Team] = []
}

struct Team: Hashable {
    var name: String
    var players: [Player] = []
}

struct Player: Hashable {
    var name: String

    init(_ name: String) {
        self.name = name
    }
}

class Model: ObservableObject {
    var baseball = Sport(name: "Baseball")
    var football = Sport(name: "Football")
    var hockey = Sport(name: "Hockey")

    @Published var sports: [Sport] = []

    init() {
        // Baseball data

        var team = Team(name: "Cardinals")
        team.players.append(Player("Yadier Molina"))
        team.players.append(Player("Adam Wainwright"))
        baseball.teams.append(team)

        team = Team(name: "Cubs")
        team.players.append(Player("Jason Heyward"))
        team.players.append(Player("Patrick Wisdom"))
        baseball.teams.append(team)

        sports.append(baseball)

        // Football data

        team = Team(name: "Buccaneers")
        team.players.append(Player("Tom Brady"))
        team.players.append(Player("Rob Gronkowski"))
        football.teams.append(team)

        team = Team(name: "Packers")
        team.players.append(Player("Aaron Rodgers"))
        team.players.append(Player("Davante Adams"))
        football.teams.append(team)

        sports.append(football)

        // Hockey data

        team = Team(name: "Blues")
        team.players.append(Player("Vladimir Tarsenko"))
        team.players.append(Player("Jordan Binnington"))
        hockey.teams.append(team)

        team = Team(name: "Blackhawks")
        team.players.append(Player("Marc-Andre Fleury"))
        team.players.append(Player("Jonathan Toews"))
        hockey.teams.append(team)

        sports.append(hockey)
    }
}
```

```swift
// SwiftUI-SidebarApp.swift
import SwiftUI

@main
struct SwiftUI_SidebarApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView(model: Model())
        }
    }
}
```

```swift
// ContentView.swift
import SwiftUI

struct SportsView: View {
    @ObservedObject var model: Model
    @Binding var selectedSport: Sport?
    @Binding var selectedTeam: Team?

    var body: some View {
        VStack {
            Text("Sports").font(.headline)
            List(model.sports, id: \.self) { sport in
                NavigationLink(
                    destination: TeamsView(
                        selectedSport: $selectedSport,
                        selectedTeam: $selectedTeam
                    ),
                    tag: sport,
                    selection: $selectedSport
                ) {
                    Text(sport.name)
                }
            }.listStyle(.sidebar)
        }
    }
}

struct TeamsView: View {
    @Binding var selectedSport: Sport?
    @Binding var selectedTeam: Team?

    var body: some View {
        VStack {
            Text(selectedSport?.name ?? "None").font(.headline)
            List(selectedSport?.teams ?? [], id: \.self) { team in
                NavigationLink(
                    destination: PlayersView(
                        selectedTeam: $selectedTeam
                    ),
                    tag: team,
                    selection: $selectedTeam

                ) {
                    Text(team.name)
                }
            }
        }
    }
}

struct PlayersView: View {
    @Binding var selectedTeam: Team?

    var body: some View {
        VStack {
            Text(selectedTeam?.name ?? "None").font(.headline)
            List(selectedTeam?.players ?? [], id: \.self) { player in
                Text(player.name)
            }
        }
    }
}

struct ContentView: View {
    @State private var selectedSport: Sport?
    @State private var selectedTeam: Team?

    var model: Model

    var body: some View {
        NavigationView {
            SportsView(
                model: model,
                selectedSport: $selectedSport,
                selectedTeam: $selectedTeam
            )
            Text("Select a sport")
            Text("Select a team")
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView(model: Model())
    }
}
```

## Size Classes

The environment variables `horizontalSizeClass` and `verticalSizeClass`
can be used to get a rough idea of
the amount of screen space available in those directions.
The values of these variables can change when a device
is rotated between portrait and landscape mode.
The value of these variables is either `.regular` or `.compact`.

To make the values of these variables available in a custom view:

```swift
@Environment(
    \.horizontalSizeClass
) var horizontalSizeClass: UserInterfaceSizeClass?
@Environment(
    \.verticalSizeClass
) var verticalSizeClass: UserInterfaceSizeClass?
```

To test the value of these variables, add code similar to the following:

```swift
if horizontalSizeClass == .compact {
    Text("Compact")
} else {
    Text("Regular")
}
if verticalSizeClass == .compact {
    Text("Compact")
} else {
    Text("Regular")
}
```

## ViewThatFits

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/viewthatfits",
"ViewThatFits" %} view takes any number of child views
and renders only the first one that fits in its parent frame.
This can often be used in place of evaluating size classes
to determine the best way to layout a set of views.

The example below renders ten buttons.
If there is enough room to lay them out in a single row, it does that.
Otherwise it lays them out vertically.

```swift
import SwiftUI

struct ContentView: View {
    private let labels = [
        "One", "Two", "Three", "Four", "Five",
        "Six", "Seven", "Eight", "Nine", "Ten"
    ]

    private func getContent() -> some View {
        ForEach(labels, id: \.self) {
            Button($0) { print("got tap") }
        }
    }

    var body: some View {
        ViewThatFits {
            HStack(content: getContent)
            VStack(content: getContent)
        }
        .buttonStyle(.bordered)
    }
}
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

Key paths can also refer to deeply nested properties.
For example, if the `Person` struct contained an `address` property
that referred to an `Address` struct that contained a `country` property
then the key path `\Person.address.country` would refer to it.

A key path can be used to get the value of a property
from an instance of a struct or class. For example:

```swift
let person = Person(name: "Mark")
let name = person[keyPath: namePath] // "Mark"
```

All objects have the property `self` that refers to the whole object.
To write a key path that refers to the whole object
rather than a specific property inside it, use `\.self`.

## Environments

A SwiftUI environment specifies key/value pairs that available
in (and can effect) all sub-views of the view on which they are specified.
For example, if a font is specified on a `VStack` then it
becomes the default font for all views nested inside it at any depth.

To dump the contents on the current environment of a view,
chain the following onto the view:

```swift
.transformEnvironment(\.self) { dump($0) }
```

TODO: Watch this {% aTargetBlank
"https://www.youtube.com/watch?v=SUiITSkAqAo&t=548s", "video" %}.

## Device ID

To get the unique ID of the current device:

```swift
if let uuid = UIDevice.current.identifierForVendor?.uuidString {
    // use uuid
}
```

## Network Requests

See the "HTTP" section in my blog page on Swift.

TODO: Watch this {% aTargetBlank
"https://www.youtube.com/watch?v=2NowSN4qJUY&t=729s", "video" %}.

## UserDefaults

The `UserDefaults` class is "an interface to the user's defaults database,
where you store key-value pairs persistently across launches of your app."
When an app is deleted, it's `UserDefaults` data is also deleted.

The `@AppStorage` property wrapper makes it easier to work with `UserDefaults`,
but it is limited to the following types:
`Bool`, `Int`, `Double`, `String`, `URL`, and `Data` (byte buffer).
It can't be used in the example below because
it stores an array of struct instances.
TODO: Can you store an array objects directly without encoding as JSON?

The code below demonstrates using this to persist data about dogs as JSON.

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
        if let decoded = try? JSONDecoder().decode(T.self, from: data) {
            return decoded
        }
    }
    return defaultValue
}

func setData<T>(for key: String, to value: T) where T: Encodable {
    if let encoded = try? JSONEncoder().encode(value) {
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

From the official docs, "The Combine framework provides
a declarative Swift API for processing values over time.
These values can represent many kinds of asynchronous events.
Combine declares publishers to expose values that can change over time,
and subscribers to receive those values from the publishers."

TODO: Watch this {% aTargetBlank
"https://www.youtube.com/watch?v=bRpFHqv0tRQ&t=701s" , "video" %}.

## Secrets

Some apps need to use data that should be not be visible to users
and show not be stored in version control systems.
This includes keys and passwords that are required to invoke REST services.
Here is one approach to store these secrets.

1. Create a new file, select the "Other" category,
   and select "Configuration Settings File".
1. Name the file `Secrets.xcconfig`.
1. Add secrets in this file where each line has the format
   `SOME_SECRET_KEY = SOME_SECRET_VALUE`.
1. In the Navigator, select the top project group.
1. In the project editor left nav, select the project.
1. Select the "Info" tab.
1. Under "Configurations", expand the "Debug" and "Release" sections.
1. Register the new file with the app name under each of these
   by clicking "None" and selecting "Secrets".
1. In the project editor left nav, click the target.
1. Open the "Custom iOS Target Properties" section.
1. For each secret
   - Hover over any existing key.
   - Click the "+" to add another property.
   - Enter a key, select a type, and enter a value

Access secret values in code as follows:

```swift
let secretValue = Bundle.main.object(
    forInfoDictionaryKey: "SOME_SECRET_KEY"
) as? String
```

If the secret values used by the release version of the app
need to differ from those used by the debug version,
create a different Configuration Settings File for each
(perhaps with the names `SecretsDebug.xcconfig` and `SecretsRelease.xcconfig`)
and register them appropriately on the 'Info" tab.

Add the Configuration Settings Files to `.gitignore`
to keep them out of version control.
Consider checking in a template Configuration Settings File
that doesn't contain real values
in order to document the secret keys that are required.
In addition, add the generated file `{app-name}-Info.plist` to `.gitignore`.

## Background Tasks

Apple introduced the BackgroundTasks framework in 2019 as part of iOS 13.

See {% aTargetBlank
"https://www.spaceotechnologies.com/ios-background-task-framework-app-update/",
"How To Use iOS Background Tasks Framework" %}.

To run tasks while an app is in the background:

1. Select the top project entry in the Navigator.
1. Click the top entry under TARGETS
1. Click the "Signing & Capabilities" tab.
1. Click the "+" to add a capability.
1. In the dialog that appears, double-click "Background Modes".
1. Check the checkboxes for "Background fetch" and "Background processing".
1. Click the "Info" tab.
1. Expand the "Custom iOS Target Properties" section if not already expanded.
1. Click the row of the last entry and then click the circled "+" that appears.
1. Add the key "Permitted background task scheduler identifiers"
1. Set the type to "Array".
1. For each background tasks

   1. Click the "+" on that row to add an item.
   1. The item type should be "String".
   1. Set the value to a unique string.
   1. Register a task.

1. Edit the file "AppDelegate.swift".
1. Add `import BackgroundTasks`

## Siri

Siri can be used to trigger actions within an app.
To configure this:

1. Select the project at the top of the Navigator view.
1. Select the topmost target.
1. Select the "Signing & Capabilities" tab.
1. Select to top entry under "TARGETS".
1. Click the "+" button in the upper-left.
1. Add the "Siri" capability.
1. Select the "Info" tab
1. Under "Custom iOS Target Properties", hover over the last row.
1. Click the "+" button.
1. Select the key "Privacy - Siri Usage Description"
1. Enter the reason Siri is being used.
   For example, "Siri is used to trigger app actions by voice."

1. Select the "General" tab.
1. Scroll down to the "Frameworks, Libraries, and Embedded Content" section.
1. Click the "+" button.
1. Enter "intent".
1. Select "Intents.framework".
1. Click the Add button.
1. Click the "+" button.
1. Enter "intent".
1. Select "IntentsUI.framework".
1. Click the Add button.

1. In the Navigator, select the file `Info.plist`.
1. Add the key "NSUserActivityTypes" and set the type to "Array".
1. Add elements describing the activities to be triggered by Siri.
   For example, "SetBackgroundIntent".

1. Select File ... New ... Target...
1. Under the "Application Extension" section, click "Intents UI Extension".
1. Click the Next button.
1. In the "Product Name" input, enter the current app name
   followed by "Intent" with no spaces.
1. Click the Finish button.
1. Click the Activate button.

1. In the Navigator, select the application folder
   that contains the file `ContentView.swift`.
1. Select File ... New ... File...
1. Under the "Resources" section, click "SiriKit Intent Definition File".
1. Click the Next button.
1. Verify that all the "Targets" checkboxes are checked.
1. Click the Create button.

For each intent to be added:

1. In the Navigator, select the file "Intents.intentdefinition".
1. Click the "+" in the lower-left.
1. Select "New Intent".
1. Rename the intent from it's default name of "Intent".
   The name must begin with a capital letter, for example, "SetBackground".
1. Enter a human-readable "Description".
1. Select a "Category" such as "Set".

For each parameter of an intent:

1. Click the "+" in under "Parameters".
1. Select a "Type" such as "String".
1. Under "Siri Dialog", enter a "Prompt".
   For example, "Say the name of a color".

For each shortcut phrase to be supported:

1. Scroll down to the "Shortcut app" section.
1. Select the "Input Parameter" if one is needed. For example, "color".
1. Select the "Key Parameter" if one is needed. For example, "color".
1. Enter a "Summary" such as "change background to color".
   The word "color" will be highlighted because it is the name of a parameter.

1. Scroll down to the "Suggestions" section.
1. Verify that the "Supports background execution" checkbox is checked.

1. In the intents editor left nav, select "Response".
1. In the "Response Templates" section, select "success"
   and enter text for the "Voice-Only Dialog".
1. In the "Response Templates" section, select "failure"
   and enter text for the "Voice-Only Dialog".

1. Select Product ... Build.
1. Attach an iPhone to the computer with a USB cable.
1. Select the iPhone from the device menu at the top.
1. Select Product ... Run or click the run triangle.
1. Why does it prompt "Choose an app to run"?

## StoreKit

{% aTargetBlank "https://developer.apple.com/documentation/storekit",
"StoreKit" %} is an Apple framework that supports
in-app purchases, ad network attribution, Apple Music integration,
and enabling app ratings and reviews.

To use StoreKit in an app:

1. Create a new file.
1. Scroll the "Other" category.
1. Select "StoreKit Configuration File".
1. A file name can be entered, but the default name
   of "Configuration.storekit" is fine.
1. Click the "+" in the lower-left of the editor and select a type.
   For one-time purchases, select "Add Non-Consumable In-App Purchase".
1. Enter a "Reference Name". This can be the app name.
   To find this, click the top entry in the Navigator, select the "General" tab,
   and note the value of Identity ... Display Name.
1. Enter a "Product ID". This can be the project bundle identifier.
   To find this, click the top entry in the Navigator, select the "General" tab,
   and note the value of Identity ... Bundle Identifier.
1. Enter a price.
1. Under "Localizations", double-click an option such as "English (U.S.)".
1. Enter a "Display Name".
   This can be the same as the Reference Name entered above.
1. Enter a "Description" of the purchasable item.
1. Click "+" under "Localizations" to add more supported locales.
1. Repeat steps 5-12 for each additional non-consumable that can be purchased.
1. Create a new Swift file. A good name is `StoreKit.svelte`.
1. Add the following in `StoreKitStore.svelte`:

   ```swift
    import StoreKit

    class StoreKitStore: NSObject, ObservableObject {
        // This is a Set of purchasable product ids.
        private var allProductIdentifiers =
            Set(["r.mark.volkmann.gmail.com.GiftTrack"])

        private var productsRequest: SKProductsRequest?

        private var fetchedProducts: [SKProduct] = []

        typealias CompletionHandler = ([SKProduct]) -> Void

        private var completionHandler: CompletionHandler?

        override init() {
            super.init()
            fetchProducts { products in
                print("products =", products)
            }
        }

        private func fetchProducts(
            _ completion: @escaping CompletionHandler
        ) {
            guard productsRequest == nil else { return }
            completionHandler = completion
            productsRequest =
                SKProductsRequest(productIdentifiers: allProductIdentifiers)
            productsRequest?.delegate = self
            productsRequest?.start()
        }
    }

    extension StoreKitStore: SKProductsRequestDelegate {
        func productsRequest(
            _ request: SKProductsRequest,
            didReceive response: SKProductsResponse
        ) {
            let loadedProducts = response.products
            let invalidProducts = response.invalidProductIdentifiers
            guard !loadedProducts.isEmpty else {
                print("failed to load products")
                if !invalidProducts.isEmpty {
                    print("invalid products found: \(invalidProducts)")
                }
                productsRequest = nil
                return
            }

            // Cache the fetched products.
            fetchedProducts = loadedProducts

            // Notify listeners of loaded products.
            DispatchQueue.main.async {
                self.completionHandler?(loadedProducts)
                self.completionHandler = nil
                self.productsRequest = nil
            }
        }
    }
   ```

1. In the `{app-name}.swift` file, add the following
   inside the struct that inherits from `App`:

   ```swift
   @StateObject private var store = StoreKitStore()
   ```

1. In any views that need to access the store, add the following:

   ```swift
   @EnvironmentObject private var store: StoreKitStore
   ```

1. In the Navigator, select `{app-name}.swift`.
1. In the top bar, click the project name to the left of the device drop-down
   and select "Edit Scheme..." which opens a dialog.
1. Click "Run" in the dialog left nav.
1. Click the "Options" tab.
1. Change the value of "StoreKit Configuration"
   from "None" to "Configuration.storekit".
1. Click the "Close" button.

## UIKit Integration

The UIKit framework preceded the SwiftUI framework.
While SwiftUI is the future, it is still missing some features of UIKit.

It is possible to embed the use of UIKit in a SwiftUI app.
The key to doing this is to wrap UIKit views in a `UIRepresentable` wrapper.

Here is an example of wrapping the use of `UITextField`
so it can be used in the SwiftUI app.
One feature this has that is missing from the SwiftUI `TextField` view
is the ability to include a clear button on the trailing end
that is an "X" in a circle. Tapping this clears the value.

```swift
import SwiftUI

struct MyUITextField: UIViewRepresentable {
    var placeholder = ""
    @Binding var text: String

    func makeCoordinator() -> Coordinator {
        Coordinator(text: $text)
    }

    func makeUIView(context: Context) -> UITextField {
        let textField = UITextField()

        textField.borderStyle = .roundedRect
        textField.clearButtonMode = .whileEditing
        textField.delegate = context.coordinator
        textField.placeholder = placeholder

        return textField
    }

    func updateUIView(_ uiView: UITextField, context: Context) {
        uiView.text = text
    }

    class Coordinator: NSObject, UITextFieldDelegate {
        @Binding var text: String

        init(text: Binding<String>) {
            self._text = text
        }

        func textFieldDidChangeSelection(_ textField: UITextField) {
            print("in textFieldDidChangeSelection")
            text = textField.text ?? ""
        }
    }
}
```

To use this inside a SwiftUI view, add code like the following:

```swift
@State private var firstName = "Mark"
...
MyUITextField(placeholder: "First Name", text: $firstName)
    .frame(height: 31)
```

## App Demos

One approach for demonstrating iOS apps to others
using a real device instead of the Simulator is to use QuickTime.
The steps to do this are:

1. Enable "Developer Mode" on an iPhone
   (Settings ... Privacy & Security ... Developer Mode).
1. Attach an iPhone to a computer running macOS using a USB cable.
1. Launch the QuickTime Player application.
1. Select File ... New Movie Recording.
1. Click the drop-down next to the record button and select "iPhone".
1. Connect with another user using a video chat service
   that supports screen sharing such as Google Meet.
1. Share your screen.
1. Demonstrate apps on the iPhone.
1. When finished, quit QuickTime Player without saving the recorded movie
   and eject the iPhone from Finder.

## App Clips

From the Apple {% aTargetBlank
"https://developer.apple.com/documentation/app_clips/",
"App Clips documentation" %}
"An App Clip is a lightweight version of your app that
offers users access to some of the app’s functionality."

App Clips binaries must be small.
For iOS 15 and before the limit is 10 MB.
For iOS 16 and beyond the limit is 15 MB.

To implement an app clip for an existing app,
create a new target from the "App Clip" template.
Select source files in the main app that should be shared with the
App Clip by checking the appropriate checkboxes in the Inspector.

App Clips differ from widgets in that they can be interactive
whereas widgets cannot.

## Questions/Thoughts

- How can I change the default device used by new projects?
  It defaults to "iPod touch (7th generation)".

- With over 4000 icons in SF Symbols,
  how can there be none related to animals?
  Perhaps Apple feels that emojis should be used for these.

- Adoption of Swift and SwiftUI would be much higher if
  there was a way to generate an Android app from a Swift app.

- Why do SwiftUI `ViewBuilders` support `if` and `switch` statements,
  but not `for` loops? We have to use the `ForEach` view instead.

- Some method names seem longer than they should be.
  For example, `onTapGesture` could be just `onTap`.

- Why doesn't SwiftUI provide a way to specify default styling
  that applies to all views in an app,
  similar to CSS for web applications?

- Why do `Picker` views appear to be disabled
  unless they are wrapped in a `NavigationView` and a `Form`?
