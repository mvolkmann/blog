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

### Preview

The Preview area shows the UI running outside of a simulator.
If it isn't running, press the "Resume" button at the top to start it.
The Preview automatically updates when code changes are saved.

To zoom in and out on the Preview area,
click the magnifier glass icons in the lower right (minus and plus)
or select a zoom level from the percent dropdown.

After saving code changes, if an error is detected the Preview will pause.
To restart it, click the "Resume" button at the top or press cmd-option-p.

By default the Preview is not in "Live Preview" mode.
Key things to know about not being in this mode include:

- Clicking a View in the Preview selects it rather than triggering tap events.
  The corresponding code is highlighted and
  the Navigator changes to show the properties of the selected View.
- Double-clicking a View in the Preview is similar,
  but moves focus to the code so it can be edited.
- Clicking a View in the code selects the corresponding View in the Preview
  and also populates the Navigator.
- Changes made in the Navigator update the Preview and the code,
  but they do not take effect until
  focus is moved out of the modified Navigator field.

To switch to "Live Preview" mode so tap events are honored,
click the button with a triangle inside a circle.

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
        CircleButton(color: .blue) { }
    }
}
```

Note that if any of the previews are in "Live Preview" mode,
only that preview will be displayed.
Exit out of that mode to get the other previews to display again.

### Simulator

To run the app in the Simulator, click the black triangle at the top.
That builds the app, launches the Simulator (if not already running),
loads the app in the Simulator, and starts it.
The app is not automatically updates when code changes are saved.
The triangle must be clicked again to repeat the whole build/load/start process.

## MVVM

SwiftUI uses the Model-View-ViewModel paradigm.
The Model holds data and application logic.
The View decides what to render.
The View gets data from the ViewModel.
The ViewModel binds views to a model.
It gets data from the Model and
optionally transforms it before sending it to the View.
For example, it could get the result of a SQL query from the Model
and turn it into an array of objects that it passes to the View.

TODO: Does the view send events to the ViewModel which sends them to the Model?

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

SwiftUI View subclasses are used for components and layout.
Each of these are defined as a struct.

### Combiner Views

Combiner views combine other views.
They act as a container of other views
and layout those views in a specific way.

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

### Component Views

- Text
- TextField
- SecureField
- TextEditor

- Image
- AsyncImage

- Button

  Here are two ways to create a `Button`.

  ```swift
  // Button containing text and action specified with a trailing closure.
  Button("My Label") {
      // code to run when button is pressed
  }

  // Button with an action argument that is a function and
  // contents that can be any View specified with a trailing closure.
  Button(action: {
      // code to run when button is pressed
  }) {
      Text("My Label")
  }
  ```

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

### Other Views

- Spacer
- Divider

TODO: Are `Color` and `LinearGradient` views?

### View Modifiers

View modifiers are methods that can be called on a view to create
a new view that is like the receiver, but modified in a specific way.
For example, the `foregroundColor` method is a view modifier
that can be used as follows.

```swift
Text("Hello, World!").foregroundColor(.red)
```

### View State

All views are immutable structs.
Typically they get mutable data from a model.
They can also have associated mutable data
by applying the `@State` property wrapper to a property.

Properties with `@State` are typically only used for
temporary data related to styling details.
Such data is held outside of the struct and the struct holds a pointer to it.

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
