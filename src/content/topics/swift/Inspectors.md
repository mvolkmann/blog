---
eleventyNavigation:
  key: Inspectors
  parent: Swift
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

## Overview

iOS 17 added the [inspector](<https://developer.apple.com/documentation/swiftui/view/inspector(ispresented:content:)?v=1.1.1>) view modifier.
This toggles the display of an inspector panel in a platform-specific way.
It macOS and iPadOS, an inspector view slides in the from right
and occupies the full height of the display.
It iOS, an inspector view slides up from the bottom like a sheet
and occupies the full width of the display.

## Resources

- [Inspectors in SwiftUI: Discover the details](<https://developer.apple.com/videos/play/wwdc2023/10161/?v=1.1.1>)

## Example

<img alt="SwiftUI inspector closed" style="width: 49%"
  src="/blog/assets/SwiftUI-inspector-closed.png?v=1.1.1"
  title="SwiftUI inspector closed">
<img alt="SwiftUI inspector open" style="width: 49%"
  src="/blog/assets/SwiftUI-inspector-open.png?v=1.1.1"
  title="SwiftUI inspector closed">

```swift
struct ContentView: View {
    @State private var showingInspector = false

    var body: some View {
        NavigationStack {
            VStack {
                Text("Main Content Goes Here!")
                    .font(.largeTitle)
            }
            .inspector(isPresented: $showingInspector) {
                Inspector()
                    .ignoresSafeArea(edges: [.bottom])
                    .presentationDetents([.medium])
            }
            .padding()
            .toolbar {
                Spacer()
                Button {
                    showingInspector.toggle()
                } label: {
                    Label("Toggle Inspector", systemImage: "info.circle")
                }
            }
        }
    }
}

struct Inspector: View {
    var body: some View {
        ZStack {
            Color.yellow.opacity(0.2)
            VStack {
                Text("My Inspector")
                    .font(.largeTitle)
            }
        }
    }
}

```
