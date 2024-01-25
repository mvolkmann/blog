---
eleventyNavigation:
  key: Navigation
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS 16 introduced new ways to manage screen navigation.

See the WWDC 2022 video on the new navigation API titled
{% aTargetBlank
"https://developer.apple.com/videos/play/wwdc2022/10054/",
"The SwiftUI cookbook for navigation" %}.

Also see the excellent YouTube videos from Stewart Lynch:

- {% aTargetBlank "https://www.youtube.com/watch?v=6-OeaFfDXXw", "NavigationStack in iOS 16" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=pwP3_OX2G9A", "Back to Root and Deep Links" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=RsmMLLL8FB0", "NavigationSplitView in iOS 16" %}

## Basic Example

In this example the initial screen contains three navigation links
and a `Button`. Tapping any of these navigates
to a detail view for a selected kind of fruit.

<img alt="SwiftUI Navigation Basic" style="width: 50%"
  src="/blog/assets/SwiftUI-navigation-basic.png?v={{pkg.version}}"
  title="SwiftUI Navigation Basic">

```swift
struct ContentView: View {
    @State private var showingBanana = false
    private let fruits = ["Apple", "Banana", "Cherry"]

    var body: some View {
        NavigationStack {
            VStack {
                ForEach(fruits, id: \.self) { fruit in
                    NavigationLink(fruit, value: fruit)
                }

                Button("Go Ape!") { showingBanana.toggle() }
                    .buttonStyle(.borderedProminent)
            }
            .navigationTitle("Fruits")
            .navigationDestination(for: String.self) { item in
                switch item {
                case "Apple":
                    // During development destinations can be simple
                    // `Text` views until real views are developed.
                    AppleView()
                case "Banana":
                    BananaView()
                case "Cherry":
                    CherryView()
                default:
                    Text("unsupported fruit")
                }
            }
            .navigationDestination(isPresented: $showingBanana) {
                BananaView()
            }
        }
    }
}

struct AppleView: View {
    private let name = "Apple"

    var body: some View {
        Image(name)
            .navigationTitle(name)
            .navigationBarTitleDisplayMode(.automatic)
    }
}

struct BananaView: View {
    private let name = "Banana"

    var body: some View {
        Image(name).navigationTitle(name)
            .navigationBarTitleDisplayMode(.inline)
    }
}

struct CherryView: View {
    private let name = "Cherry"

    var body: some View {
        Image(name).navigationTitle(name)
            .navigationBarTitleDisplayMode(.large)
    }
}
```

## Example App

See {% aTargetBlank "https://github.com/mvolkmann/NavigationStackDemo",
"NavigationStackDemo" %} which demonstrates everything
shared in the Steward Lynch videos linked above.

The app has four tabs.

### Fruits tab

The first tab "Fruits" is implemented in the file `FruitListView.swift`.
It uses a `NavigationStack` to allow the user
to select a fruit from a `List` of `NavigationLink` views.

<img alt="Fruits List"
  src="/blog/assets/swiftui-navigation-01-fruits.png?v={{pkg.version}}">

Nested inside the `NavigationStack` are instances of `NavigationLink`.
This struct supports many initializers, some of which take a `value` argument.
When these links are tapped, their value is placed on the stack.

When a fruit is selected, a view that displays "You chose {fruit-emoji}."
is pushed onto the navigation stack.

<img alt="Fruits Detail"
  src="/blog/assets/swiftui-navigation-02-fruit.png?v={{pkg.version}}" />

Click "< Fruits" in the upper-left to return to the list of fruits.
The "Show Favorite" button at the bottom demonstrates a `NavigationList`
that is outside of the `List`.
The "Smile" button at the bottom demonstrates a `NavigationList`
that takes advantage of the fact that the only registered
`navigationDestination` handles any `String` value.

### Authors tab

The second tab "Authors" is implemented in the file `AuthorListView`.
It uses a `NavigationStack` to allow the user
to select an `Author` from a `List` of `NavigationLink` views.

<img alt="Authors List"
  src="/blog/assets/swiftui-navigation-03-authors.png?v={{pkg.version}}" />

When an author is selected, a view that displays specific information
about the author is pushed onto the navigation stack.

<img alt="Author Detail"
  src="/blog/assets/swiftui-navigation-04-author.png?v={{pkg.version}}" />

There are four `navigationDestination` registrations that handle the types:

- `String` for author name
- `Int` for number of books by an author
- `Color` for a color associated with an author
- `Author` for an entire `Author` object

The "Random" button selects a random author
and displays information about that author.

Unlike in the Fruits tab, a `path` is passed to `NavigationStack`.
This can be an instance of `NavigationPath` or any array of any type
whose values conform to the `Codable` and `Hashable` protocols.
Many built-in types such as `String` already conform to both of these protocols.

It seems the `NavigationStack` is only populated when
the `NavigationLink` instances supply the `value` argument.
This enables modifying the stack to navigate to another screen.
For example, the following code navigates to the "root" screen:

```swift
path.removeLast(path.count)
```

If the path passed to `NavigationStack` is an `Array` of any type
instead of a `NavigationPath` instance and it is held in state,
navigate to the "root" screen by
setting the that state variable to an empty array.

### Countries Stack tab

The third tab "Countries Stack" uses a `NavigationStack` to allow the user
to select a `Country` from a `List` of `NavigationLink` views.

<img alt="Countries List"
  src="/blog/assets/swiftui-navigation-05-stack.png?v={{pkg.version}}" />

When a country is selected, a view that displays a list of
cities in the country is pushed onto the navigation stack.

<img alt="Cities List"
  src="/blog/assets/swiftui-navigation-06-stack.png?v={{pkg.version}}" />

Each city is represented by a `NavigationLink`. Tapping one of these pushes
a view onto the stack that displays detailed information about that city.
A star icon is displayed behind the information
if the city is the capital of its country.
A bar chart showing the population of each supported city in the same country
is displayed below the city data.
The "Back to Countries" button demonstrates popping back to
the initial view in this tab.

<img alt="City Detail"
  src="/blog/assets/swiftui-navigation-07-stack.png?v={{pkg.version}}" />

### Countries Split tab

The fourth tab "Countries Split" is similar to the third tab,
but uses a `NavigationSplitView` instead of a `NavigationStack`.
A `NavigationSplitView` can have two of three sections arranged horizontally.
These are named "sidebar", "content" (optional), and "detail".
This app uses a three-column view where
the first column contains a list of countries,
the second column contains a list of cities in the selected country,
and the third column contains detail about the selected city.
Having a "Back to Countries" button doesn't apply in this scenario.

<img alt="Countries List"
  src="/blog/assets/swiftui-navigation-08-split.png?v={{pkg.version}}" />

<img alt="Cities List"
  src="/blog/assets/swiftui-navigation-09-split.png?v={{pkg.version}}" />

<img alt="City Detail"
  src="/blog/assets/swiftui-navigation-10-split.png?v={{pkg.version}}" />

## Deep Linking

This app supports navigation via a URL.

To create a URL scheme:

- Select the top item in Navigator.
- Select the main target.
- Select the Info tab.
- Expand "URL Types".
- Click the "+" button.
- For "Identifier", enter the app bundle id found in the target General tab.
- For "URL Schemas", enter a name that is unique within the app
  such as "NavStack".
- "Icon" is not required.
- "Role" can keep the default value of "Editor".

To configure use of URLs:

- In the App subclass, add the `onOpenURL` view modifier to the initial view
  (`ContentView` here).

To test use of URLS:

- Launch the Simulator. The app does not need to be running in the Simulator.
- In a terminal window, enter a command like these:

  - `xcrun simctl openurl booted NavStack://Canada`
  - `xcrun simctl openurl booted NavStack://Canada/Vancouver`

- The URLs above can also be links in an email or reminder.
  Clicking these links will launch the app if it is not already running
  and will navigate to the appropriate navigation path.
- The app will ask for permission to receive a URL
  the first time a request is received.
