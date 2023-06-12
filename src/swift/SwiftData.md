---
eleventyNavigation:
  key: SwiftData
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/xcode/swiftdata/", "SwiftData" %}
was added in iOS 17. From Apple,

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

The new project will contain the following files:

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
This macro cannot be applied to `struct` definitions.
The macro causes the class to conform to the {% aTargetBlank
"https://developer.apple.com/documentation/SwiftData/PersistentModel",
"PersistentModel" %} protocol.

Declarations can be added class properties to customize how they are persisted.
For example:

```swift
// Add a uniqueness constraint to a property.
// If a persisted object exists with the same value for this attribute,
// an "upsert" is performed instead of an insert.
// No error is thrown.
@Attribute(.unique) var id: int

// Change the name of an existing property after data has been persisted.
// Existing and future persisted objects will continue using the original name,
// but code can be changed to use the new name (creationDate in this case.)
@Attribute(originalName: "creation_date") var creationDate: Date

// If an object is deleted, also delete the objects
// referrred to by this property (cascading delete).
// If a referenced object is deleted,
// persisted references to it will automatically be removed.
@Relationship(.cascade) var cars: [Car]

// It is also possible to specify the `min` and `max` occurrences
// allowed in a relationship.

// Do not persist this property.
// These properties should have a default value that is
// used when objects are fetched using SwiftData.
@Transient var socialSecurityNumber = ""
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

The `ModelContext` object has an `autosaveEnabled` property
that defaults to `true`. When this is `true`, there is no need to call
the `save` method after creating, updating, or deleting persisted objects
because it happens automatically.
An alternative approach is to set `autosaveEnabled` to `false` and
manually call the `save` method after a batch of changes are made.

Also see the {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/modelcontext/transaction(block:)",
"transaction" %} and {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/modelcontext/rollback()",
"rollback" %} methods.

## Creating Data

To create and persist an object,
create it using an initializer of a `class`
that is decorated with the {% aTargetBlank "", "Model" %} macro
and pass the object to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/modelcontext",
"ModelContext" %} `insert` method.
For example:

```swift
let todo = Todo(title: "some new title")
context.insert(todo)
```

## Retrieving Data

A view can specify the data it needs using the {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/query", "Query" %}
property wrapper.
Any time persisted data matching the query changes, the view will update.

The `Query` property wrapper supports many initializers
that accept the following:

- `filter`: a {% aTargetBlank
  "https://developer.apple.com/documentation/foundation/predicate",
  "Predicate" %} object that describes criteria for the objects to return
- `sort`: a `KeyPath` or array of {% aTargetBlank
  "https://developer.apple.com/documentation/foundation/sortdescriptor",
  "SortDescriptor" %} objects that describe
  how the returned objects should be sorted
- `order`: a {% aTargetBlank
  "https://developer.apple.com/documentation/foundation/sortorder",
  "SortOrder" %} enum value of `.forward` or `.reverse`
- `animation`: an {% aTargetBlank
  "https://developer.apple.com/documentation/SwiftUI/Animation", "Animation" %}
  to apply when the data changes

For example, the following query fetches `Todo` objects
sorted on their title, returning only those not completed:

```swift
@Query(
    filter: #Predicate<Todo> { $0.completed == false },
    sort: \Todo.title,
    order: .forward,
    animation: .spring
)
```

To fetch data outside of a `Query`, create and use
a `Predicate` and a {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/fetchdescriptor",
"FetchDescriptor" %}.
For example:

```swift
// Can't use ! operator here.
let predicate = #Predicate<Todo> { $0.completed == false }
let descriptor = FetchDescriptor(
    predicate: predicate,
    sortBy: [SortDescriptor(\Todo.created, order: .reverse)]
let uncompletedTodos = try? context.fetch(descriptor)
```

## Updating Data

To update the properties of persisted objects,
directly modify their properties.
For example:

```swift
todo.title = "some new title"
```

SwiftData will detect and persist the change
with no further action on your part

## Deleting Data

To delete persisted objects, pass them to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/modelcontext",
"ModelContext" %} `delete` method.
For example:

```swift
context.delete(todos[index])
```

The delete method can also be passed a model type and a predicate
in order to delete multiple objects in a single call.

## Migration

When the object schema being persisted needs to change
after data has already been persisted, migration is necessary.

Define a {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/versionedschema",
"VersionedSchema" %} for each version of the schema.
Each contains all the model classes it supports.

Next, define a custom `enum` that is a subtype of {% aTargetBlank
"https://developer.apple.com/documentation/swiftdata/schemamigrationplan",
"SchemaMigrationPlan" %}.
This should contain the following `static` properties:

- `schemas`: an ordered array of the `VersionSchema` objects
- `stages`: an ordered array of `MigrationStage` objects
- multiple stage objects

Stage objects can be lightweight or custom.

To create a lightweight stage, call `Migration.lightweight`,
passing it `fromVersion` and `toVersion` arguments.

To create a custom state, call `MigrationState.custom`
which also takes `fromVersion` and `toVersion` arguments.
Optionally pass a `willMigrate` function to run code that can
perform transformations before the migration takes place.
Optionally pass a `ditMigrate` function to run code
after the migration takes place.

## @Observable and @Bindable

This section is not really related to SwiftData.

The {% aTargetBlank
"https://developer.apple.com/documentation/observation/observable-swift.macro",
"Observable" %} macro provides a new way to define view models.
It is a useful alternative to the `Model` macro
for sharing data between views that is not persisted.

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/bindable", "Bindable" %}
property wrapper provides a new way to access view models.
Both are new in iOS 17.

```swift
import Observation
import SwiftUI

@Observable
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

`Observable` objects can be passed to views
that accept them as `Bindable` properties.

For example, the code above can be modified as follows:

```swift
// Add this view definition.
struct Greet: View {
    @Bindable var model = MyViewModel()

    // Note that we do not need to define an initializer
    // in order to pass in "model".

    var body: some View {
        Text("Hello, \(model.name)!")
    }
}

// Replace the following:
// Text("Hello, \(model.name)!")
// With this:
Greet(model: model)
```

## Example Project

See {% aTargetBlank "https://github.com/mvolkmann/SwiftDataDemo",
"SwiftDataDemo" %}.
