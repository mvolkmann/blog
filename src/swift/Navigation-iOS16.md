---
eleventyNavigation:
  key: Navigation in iOS 16
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS 16 introduced new ways to manage screen navigation.

See the WWDC 2022 video on the new navigation API titled
{% aTargetBlank
"https://developer.apple.com/videos/play/wwdc2022/10054/",
"The SwiftUI cookbook for navigation" %}.

Also see the excellent YouTube videos from Steward Lynch:

- {% aTargetBlank "https://www.youtube.com/watch?v=6-OeaFfDXXw", "NavigationStack in iOS 16" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=pwP3_OX2G9A", "Back to Root and Deep Links" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=RsmMLLL8FB0", "NavigationSplitView in iOS 16" %}

## Example App

See {% aTargetBlank "https://github.com/mvolkmann/NavigationStackDemo",
"NavigationStackDemo" %} which demonstrates everything
shared in the Steward Lynch videos linked above.

The app has four tabs.

### Fruits tab

The first tab "Fruits" uses a `NavigationStack` to allow the user
to select a fruit from a `List` of `NavigationLink` views.

TODO: What is wrong with these img tags?
<img alt="Fruits List" href="/blog/assets/swiftui-navigation-01-fruits.png?v={{pkg.version}}" />

When a fruit is selected, a view that displays "You chose {fruit-emoji}."
is pushed onto the navigation stack.

<img alt="Fruits Detail" href="/blog/assets/swiftui-navigation-02-fruit.png?v={{pkg.version}}" />

Click "< Fruits" in the upper-left to return to the list of fruits.
The "Show Favorite" button at the bottom demonstrates a `NavigationList`
that is outside of the `List`.
The "Smile" button at the bottom demonstrates a `NavigationList`
that takes advantage of the fact that the only registered
`navigationDestination` handles any `String` value.

### Authors tab

The second tab "Authors" uses a `NavigationStack` to allow the user
to select an `Author` from a `List` of `NavigationLink` views.

<img alt="Authors List" href="/blog/assets/swiftui-navigation-03-authors.png?v={{pkg.version}}" />

When an author is selected, a view that displays specific information
about the author is pushed onto the navigation stack.

<img alt="Author Detail" href="/blog/assets/swiftui-navigation-04-author.png?v={{pkg.version}}" />

There are four `navigationDestination` registrations that handle the types:

- `String` for author name
- `Int` for number of books by an author
- `Color` for a color associated with an author
- `Author` for an entire `Author` object

The "Random" button selects a random author
and displays information about that author.

### Countries Stack tab

The third tab "Countries Stack" uses a `NavigationStack` to allow the user
to select a `Country` from a `List` of `NavigationLink` views.

<img alt="Countries List" href="/blog/assets/swiftui-navigation-05-stack.png?v={{pkg.version}}" />

When a country is selected, a view that displays a list of
cities in the country is pushed onto the navigation stack.

<img alt="Cities List" href="/blog/assets/swiftui-navigation-06-stack.png?v={{pkg.version}}" />

Each city is represented by a `NavigationLink`. Tapping one of these pushes
a view onto the stack that displays detailed information about that city.
A star icon is displayed behind the information
if the city is the capital of its country.
A bar chart showing the population of each supported city in the same country
is displayed below the city data.
The "Back to Countries" button demonstrates popping back to
the initial view in this tab.

<img alt="City Detail" href="/blog/assets/swiftui-navigation-07-stack.png?v={{pkg.version}}" />

### Countries Split tab

The fourth tab "Countries Split" is similar to the third tab,
but uses a `NavigationSplitView` instead of a `NavigationStack`.
This provides a three-column view where
the first column contains a list of countries,
the second column contains a list of cities in the selected country,
and the third column contains detail about the selected city.
Having a "Back to Countries" button doesn't apply in this scenario.

<img alt="Countries List" href="/blog/assets/swiftui-navigation-08-split.png?v={{pkg.version}}" />

<img alt="Cities List" href="/blog/assets/swiftui-navigation-09-split.png?v={{pkg.version}}" />

<img alt="City Detail" href="/blog/assets/swiftui-navigation-10-split.png?v={{pkg.version}}" />
