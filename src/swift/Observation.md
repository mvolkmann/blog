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

The `@Bindable` property wrapper creates a two-way binding
to instance of a class to which the `@Observable` macro is applied.
This binding can be used to get and set properties in the object.

The app {% aTargetBlank
"https://github.com/mvolkmann/ObservationDemo/blob/main/ObservationDemo/ContentView.swift",
"ObservableDemo" %} demonstrates this.
It defines a `ViewModel` as follows:

```swift
import Observation

@Observable
class ViewModel {
    var todos: [Todo] = [
        Todo("Cut grass"),
        Todo("Buy Milk", done: true)
    ]

    func addTodo(_ todo: Todo) {
        todos.append(todo)
    }

    func deleteTodo(_ todo: Todo) {
        todos.removeAll { $0 == todo }
    }

    func toggleTodo(_ todo: Todo) {
        // todo.done.toggle() // This doesn't work.
        if let index = todos.firstIndex(where: { $0.id == todo.id }) {
            todos[index] = Todo(todo.description, done: !todo.done)
        }
    }
}
```

The `Todo` type is defined as follows:

```swift
import Foundation // for UUID
import Observation

// This needs to be a class instead of a struct
// in order to apply the @Observable macro.
@Observable
class Todo: Equatable, Identifiable {
    var description = ""
    var done = false
    let id: UUID = .init()

    init(_ description: String, done: Bool = false) {
        self.description = description
        self.done = done
    }

    static func == (lhs: Todo, rhs: Todo) -> Bool {
        lhs.id == rhs.id:w
    }
}
```

The main view is defined as follows:

```swift
import Observation
import SwiftUI

struct ContentView: View {
    @Environment(ViewModel.self) private var vm
    @State private var description = ""

    var body: some View {
        VStack(alignment: .leading) {
            HStack {
                TextField("todo description", text: $description)
                    .textInputAutocapitalization(.never)
                    .textFieldStyle(.roundedBorder)
                Button("Add") {
                    vm.addTodo(Todo(description))
                    description = ""
                }
                .buttonStyle(.borderedProminent)
                .disabled(description.isEmpty)
            }

            List {
                // The `sortedInsensitive` method is defined in
                // Extensions/SequenceExtension.swift.
                ForEach(vm.todos.sortedInsensitive(by: \.description)) { todo in
                    TodoRow(todo: todo)
                }
            }
            .listStyle(.plain)
        }
        .padding()
    }
}
```

The `TodoRow` view takes a binding to an instance of the `Todo` class
to which the `@Observable` macro is applied.
A binding to the `done` property of the `Todo` object is passed
to the `Checkbox` view which can modify that property.

```swift
import Observation
import SwiftUI

struct TodoRow: View {
    @Bindable var todo: Todo
    @Environment(ViewModel.self) private var vm

    var body: some View {
        HStack {
            Checkbox(label: todo.description, isOn: $todo.done)
                .strikethrough(todo.done)
            Spacer()
            Button {
                vm.deleteTodo(todo)
            } label: {
                Image(systemName: "trash")
            }
            // Without this, tapping any button triggers
            // all buttons in the same HStack!
            .buttonStyle(.borderless)
        }
    }
}
```

The `Checkbox` view is defined as follow:

```swift
import SwiftUI

struct Checkbox: View {
    var label: String
    @Binding var isOn: Bool

    var body: some View {
        Button {
            isOn.toggle( gg
        } label: {
            Image(systemName: isOn ? "checkmark.square" : "square")
            Text(label)
        }
        .buttonStyle(.plain)
    }
}
```

## Example App

See {% aTargetBlank
"https://github.com/mvolkmann/ObservationDemo/blob/main/ObservationDemo/ContentView.swift",
"ObservableDemo" %}.
