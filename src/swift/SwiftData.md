---
eleventyNavigation:
  key: SwiftData
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "SwiftData", "https://developer.apple.com/xcode/swiftdata/" %}
was added in iOS 17.

> SwiftData makes it easy to persist data using declarative code.
> You can query and filter data using regular Swift code.
> And it’s designed to integrate seamlessly with SwiftUI.

## Projects

To create a new project that uses SwiftData:

- Select File ... New ... Project...
- In the dialog that appears, select "App".
- Click the "Next" button.
- Enter a "Product Name".
- For the Interface, select "SwiftUI".
- For the Language, select "Swift".
- For the Storage, select "SwiftData".
- Optionally check the checkbox for "Host in CloudKit"
  to enable sharing the data between devices.
  When not checked, the data will only be stored
  in the device where the app runs.
- Click the "Next" button.
- Select the directory where the project will be stored.
- Click the "Create" button.

This creates a project containing the following files:

- `Item.swift` - an example model definition
  where the only property is `timestamp`
- `ContentView.swift` - a `View` subtype that supports
  creating, retrieving, and deleting `Item` instances
- `Assets.xcassets` - an asset file that contains
  placeholders for `AccentColor` and `AppIcon`
- `{project-name}App.swift` - an `App` subtype that
  creates a model container for `Item` instances
  and displays a `ContentView`

## Models

An example model is provided in `Item.swift`.
Define additional models in new `.swift` files.
Optionally delete `Item.swift` if not needed.

To define a model, add the `@Model` macro to a `class` definition.
I suspect it needs to be a `class` instead of a `struct`
to support updating existing data.

Declarations can be added class properties to customize how they are persisted.
For example:

```swift
@Attribute(.unique) var id: int

@Relationship(.cascade) var cars: [Car] // for cascading deletes

@Transient var socialSecurityNumber // to prevent from being persisted
```

TODO: Watch the session “Model your schema with SwiftData” session.

## Containers

Create a container to manage persistence.

For example:

```swift
let container = try ModelContainer(
  for: [Person.self, Car.self],
  configurations: ModelConfiguration(url: URL("some-path") // optional
)
```

To create a container at the top of the view hierarchy:

```swift
struct MyApp: App {
  var body: some Scene {
    WindowGroup {
    ContentView()
  }
  .modelContainer(for: [Person.self, Car.self])
}
```

To access the container in a view:

```swift
import SwiftData
import SwiftUI

struct ContentView: View {
  @Environment(\.modelContent) private var context
  ...
}
```

To get the context outside the view hierarchy:

```swift
import SwiftData

let context = container.mainContext
// or
let context = ModelContext(container)
```

## Fetching Data

To fetch specific data, create and use a predicate:

```swift
let predicate = #Predicate<Person> {
  $0.state == “MO” && $0.alive
}
let people = try context.fetch(predicate)
```

To fetch sorted people:

```swift
let descriptor =
```

## @Observable and @Bindable

The `@Observable` macro provides a new way to define view models.
The `@Bindable` property wrapper provides a new way to access view models.
Both are new in iOS 17 and are defined in the SwiftData package.

```swift
import SwiftData
import SwiftUI

@Observable // defined in SwiftData
class MyViewModel {
    // No longer need @Published on each property.
    var name = ""
}

struct ContentView: View {
    @Bindable var model = MyViewModel()

    var body: some View {
        VStack {
            TextField("Name", text: $model.name)
                .textFieldStyle(.roundedBorder)
            Text("Hello, \(model.name)!")
        }
        .padding()
    }
}
```

## Example Project

See {% aTargetBlank "https://github.com/mvolkmann/SwiftDataDemo",
"SwiftDataDemo" %}.
