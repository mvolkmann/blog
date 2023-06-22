---
eleventyNavigation:
  key: Observation
  parent: Swift
layout: topic-layout.njk
---

## Overview

Observation enables defining model objects using Swift syntax
and triggering SwiftUI updates based on changes to model objects.

## Resources

- {% aTargetBlank "https://developer.apple.com/wwdc23/10149",
  "What is Observation?" %} video from WWDC 2023

## @Binding

The `@Binding` property wrapper allows a view to mutate data
that is owned by another view, typically its parent view.

The following code implements a checkbox view:

```swift
import SwiftUI

struct Checkbox: View {
    var label: String
    @Binding var isOn: Bool

    var body: some View {
        // This approach only works in macOS.
        // Toggle(isOn: $isOn) { Text(label) }.toggleStyle(.checkbox)

        Button {
            isOn.toggle()
        } label:
            Image(systemName: isOn ? "checkmark.square" : "square")
            Text(label)
        }
        .buttonStyle(.plain)
    }
}
```

The following code demonstrates using the `Checkbox` view:

```swift
struct ContentView: View {
    @State private var isHappy = false

    var body: some View {
        VStack(alignment: .leading) {
            Checkbox(label: "Happy?", isOn: $isHappy)
            Text(isHappy ? "Good for you!" : "Maybe tomorrow.")
        }
        .padding()
    }
}
```

## @Observable

To define a data model, apply the `Observable` macro to a class definition.
For example:

```swift
@Observable class TodosModel {
    var todos: [Todo] = []
}
```

## @Bindable

The `@Bindable` property wrapper creates a two-way binding to a property
in a class to which the `@Observable` property wrapper is applied.

TODO: Add information about this.

## Example App

See {% aTargetBlank
"https://github.com/mvolkmann/ObservationDemo/blob/main/ObservationDemo/ContentView.swift",
"ObservableDemo" %}.
