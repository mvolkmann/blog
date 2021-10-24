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

- is currently less capable than UIKit.
- is declarative in nature than imperative.
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

## MVVM

SwiftUI encourages use of the Model-View-ViewModel (MVVM) paradigm
which separates application code into three groups.
This differs from UIKit which encourages use of Model-View-Controller (MVC).

The Model holds data and application logic.
It is independent from the view code and has no knowledge of it.

The View decides what to render and should be mostly stateless.
Any state held in the view using the `@State` property modifier
should be primarily related to styling and not application data.

Views reacts to changes published by the ViewModel.
Their `body` vars return new views
any time the ViewModel data they use changes.
Views are declarative rather than imperative
because they describe what to render based on the current data,
not when to render it.

The ViewModel binds views to a model.
It reacts to changes in the Model and
optionally transforms it before sending it to the View.
For example, it could get the result of a SQL query from the Model
and turn it into an array of objects that it passes to the View.

Read-only data flows from the Model, through the ViewModel, and into the View.

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
Image(systemName: "heart").font(.largeTitle)) // one way to set font size
Image(systemName: "cloud.snow").font(.system(size: 64)) // another way
```

## Views

Every visible part of an app created with SwiftUI is a view.
Views can render other views.

Each view is a struct that implement the `View` protocol.
This requires having a computed property named `body` with the type `Some View`.

Views are used for components and layout.
Views that layout other views are often referred to as "combiner views".

Functions that create views often take a `ViewBuilder` their last argument.
These are typically written as trailing closures.

Views can be given an explicit identifier with the `.id` method
that is passed an identifier.
This is useful in methods that take a view identifier
like the `ScrollViewReader` method `scrollTo`.

### Combiner Views

Combiner views combine other views.
They act as a container of other views
and layout those views in a specific way.

Combiner views can be passed a special kind of closure
as their last argument that is called a `ViewBuilder`.
These accept one to ten other Views.
A `ViewBuilder` is a kind of result builder.
For more information on these, see the {% aTargetBlank
"https://github.com/apple/swift-evolution/blob/main/proposals/0289-result-builders.md",
"Result builders proposal" %}.

`ViewBuilder` blocks also support variable declarations and
conditional logic with `if` and `switch` statements.
However, no other Swift syntax is allowed in them.
For iteration in a `ViewBuilder`, use a `ForEach` `View`.
There is no corresponding view for conditional logic,
so `if` and `switch` statements are used instead.

- `HStack`: lays out child views horizontally

- `VStack`: lays out child views vertically

- `ZStack`: stacks views from bottom to top

- `LazyHStack`

  This is similar to `HStack`, but only renders items when they are visible.
  It is commonly used inside a `ScrollView`.

- `LazyVStack`

  This is similar to `VStack`, but only renders items when they are visible.
  It is commonly used inside a `ScrollView`.

- `LazyHGrid`

  This specifies a number of rows and adds columns as necessary.
  The rows are described by an array of {% aTargetBlank
  "https://developer.apple.com/documentation/swiftui/griditem", "GridItem" %}
  objects that each specify their size, spacing, and alignment.
  For example, a `GridItem` can adapt to the width of its content,
  but also have a minimum size of 25 by specifying
  `GridItem(.adaptive(minimum: 25))`.

- `LazyVGrid`

  This specifies a number of columns and adds rows as necessary.
  The columns are described by an array of {% aTargetBlank
  "https://developer.apple.com/documentation/swiftui/griditem", "GridItem" %}
  objects that each specify their size, spacing, and alignment.

- `Form`
- `Group`
- `GroupBox`
- `ControlGroup`

- `ScrollView`

  This creates a scrollable view that is vertical by default,
  but can be changed to horizontal.
  The following example creates a horizontally scrollable view
  containing 20 numbered `Text` views.

  ```swift
  ScrollView(.horizontal) {
      HStack(spacing: 10) {
          ForEach(1..<21) { // 1...20 isn't allowed here
              Text("Number \($0)").padding(5).border(.red, width: 2)
          }
      }
  }
  ```

- `ScrollViewReader`
- `ScrollViewProxy`

- `List`
- `Section`
- `ForEach`

  This view iterates of the elements of an array
  and renders the view specified in its `ViewBuilder`.
  The elements in the array must either conform to the `Identifiable` protocol
  OR `id:` argument must be specified.
  The value of `id:` is a key path that specifies
  how to find a unique value in the element.

- `DynamicViewContent`
- `Table`

- `NavigationView`
- `NavigationLink`
- `OutlineGroup`
- `DisclosureGroup`
- `TabView`
- `HSplitView`
- `VSplitView`
- `TimelineView`

### Component Views

- `Text`
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

- `AsyncImage`

- `Button`

  Here are two ways to create a `Button`.

  ```swift
  // Button containing text and action specified with a trailing closure.
  Button("My Label") {
      // code to run when button is pressed
  }

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

- `EditButton`
- `PasteButton`

- `Link`
- `Menu`

- `Toggle`
- `Slider`
- `Stepper`
- `Picker`
- `DatePicker`
- `ColorPicker`

- `Label`
- `ProgressView`
- `Gauge`

- `EmptyView`
- `EquatableView`
- `AnyView`
- `TupleView`

Here is an example of using combiner and component views.
Note how these can be assigned to a variable
that is later referenced to render them.
They can also be placed in a new struct
from which instances are created to render them.

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
    // Assigning a View to a variable
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

Many of these views support both the `border` and `strokeBorder` view modifiers.
The difference between these becomes apparent
when the border width is greater than one.
`border` is drawn so it is centered on the edge of the shape
with have inside and half outside.
`strokeBorder` is drawn so none of the border is outside of the shape.

- `Capsule`
- `Circle`
- `Circle`
- `Ellipse`
- `Path`
- `Rectangle`
- `RoundedRectangle`

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
- `GeometryProxy`
- `GeometryReader`
- `ProjectionTransform`
- `UnitPoint`

### Other Views

- `Spacer`

  Each of these take an equal amount of the unused space
  inside the parent combiner view.
  This can be compared to web applications that use
  the CSS properties `display: flex;` and `justify-content`.

  | CSS justify-content value | Spacer placement              |
  | ------------------------- | ----------------------------- |
  | flex-start                | at end of list of views       |
  | flex-end                  | at beginning of list of views |
  | space-between             | one between each child view   |

- `Divider`

TODO: Are `Color` and `LinearGradient` views?

### View Modifiers

View modifiers are methods that can be called on a view to create
a new view that is like the receiver, but modified in a specific way.
For example:

```swift
// foregroundColor is a view modifier.
Text("Hello, World!").foregroundColor(.red)

// stroke and padding are view modifiers.
RoundedRectangle(cornerRadius: 20).stroke(lineWidth: 3).padding(.all)
```

Calls to view modifiers can be chained since each returns a new view.

View modifiers can be specific to certain types of views.
For a list of them, see {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/slider-view-modifiers",
"View Modifiers" %}.

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

## Modals

Modal dialogs are implemented by displaying a "sheet".
The sheet slides in from the bottom by default.

The following example defines the custom view `MyModal`
which is displayed in the sheet.
The `ContentView` struct declares the boolean property `showModal`
and passes it to the `MyModal` struct as a binding.
This allows the action of the "Close" button in `MyModal`
to set it to false which hides the sheet.

```swift
import SwiftUI

struct MyModal: View {
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
