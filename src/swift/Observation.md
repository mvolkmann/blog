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

## @Observable

To define a data model, apply the `Observable` macro to a class definition.
For example:

```swift
@Observable class TodosModel {
    var todos: [Todo] = []
}
```

## @Bindable

The `@Binding` property wrapper allows a view to mutate data
that is owned by another view, typically its parent view.

The `@Bindable` marks an instance of a class
to which the @Observable macro is applied.
It can be used to create a two-way binding

TODO: Add information about this.

## Example App

See {% aTargetBlank
"https://github.com/mvolkmann/ObservationDemo/blob/main/ObservationDemo/ContentView.swift",
"ObservableDemo" %}.
