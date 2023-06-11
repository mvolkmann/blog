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

## Models

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
