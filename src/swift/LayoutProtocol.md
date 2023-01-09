---
eleventyNavigation:
  key: Layout Protocol
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS 16 introduced the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/layout",
"Layout" %} protocol and the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/anylayout",
"AnyLayout" %} struct.

See the excellent YouTube videos from Stewart Lynch, {% aTargetBlank
"https://www.youtube.com/watch?v=WD7ebJZ7PaI",
"AnyLayout and Custom Layouts in iOS 16" %}.

## Example App

See {% aTargetBlank "https://github.com/mvolkmann/AnyLayoutDemo",
"AnyLayoutDemo" %} which demonstrates everything
shared in the Steward Lynch video linked above.

## Layout Protocol

The {% aTargetBlank "https://developer.apple.com/documentation/swiftui/layout",
"Layout" %} protocol ...

TODO: Add detail.

## AnyLayout Struct

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/anylayout", "AnyLayout" %}
struct is a type-erased instance of the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/layout", "Layout" %}
protocol.
TODO: Add detail.

## Radial Layout

Paul Hudson provided an excellent example of implementing a custom layout
in his post {% aTargetBlank
"https://www.hackingwithswift.com/quick-start/swiftui/how-to-create-a-custom-layout-using-the-layout-protocol",
"How to create a custom layout using the Layout protocol" %}
It arranges a set of circles in a circular pattern.

The following code is my modified version of his example.
It adds a random color to each of the circles and
keeps the same color for each when the number of circles is changed.

<img alt="SwiftUI Radial Layout" style="border: 1px solid gray; width: 50%"
  src="/blog/assets/SwiftUI-Radial-Layout.png?v={{pkg.version}}"
  title="Swift Radial Layout">

```swift
public extension Color {
    static func random() -> Color {
        Color(
            red: .random(in: 0 ... 1),
            green: .random(in: 0 ... 1),
            blue: .random(in: 0 ... 1),
            opacity: 1
        )
    }
}

struct RadialLayout: Layout {
    // This "assigns positions to the container’s subviews."
    func placeSubviews(
        in bounds: CGRect,
        proposal: ProposedViewSize,
        subviews: Subviews,
        cache: inout Void
    ) {
        // Get half the smallest dimension to use as the circle radius.
        let radius = min(bounds.size.width, bounds.size.height) / 2

        // Get the angle between each subview.
        let deltaAngle = Angle.degrees(360 / Double(subviews.count)).radians

        for (index, subview) in subviews.enumerated() {
            let idealSize = subview.sizeThatFits(.unspecified)

            // Get the offset from the circle center
            // that would place the subview on the circle.
            let angle = deltaAngle * Double(index) - .pi / 2
            let dx = cos(angle) * (radius - idealSize.width / 2)
            let dy = sin(angle) * (radius - idealSize.height / 2)

            // Calculate the subview position within the parent view.
            let position = CGPoint(x: bounds.midX + dx, y: bounds.midY + dy)

            // Place the subview at the calculated position.
            subview.place(at: position, anchor: .center, proposal: .unspecified)
        }
    }

    // This "reports the size of the composite layout view."
    func sizeThatFits(
        proposal: ProposedViewSize,
        subviews: Subviews,
        cache: inout Void
    ) -> CGSize {
        // "Creates a new proposal that replaces
        // unspecified dimensions in this proposal with
        // the corresponding dimension of the specified size."
        proposal.replacingUnspecifiedDimensions()
    }
}

struct ContentView: View {
    @State private var count = 16
    @State private var colors: [Color] = []

    init() {
        _colors = State(initialValue: getColors(count: count))
    }

    var body: some View {
        RadialLayout {
            ForEach(0 ..< count, id: \.self) { index in
                let color = index < colors.count ? colors[index] : .black
                Circle()
                    .foregroundColor(color)
                    .frame(width: 32, height: 32)
            }
        }
        .padding()
        .safeAreaInset(edge: .bottom) {
            // This allows the user to change the number of subviews
            // and animates the change.
            Stepper("Count: \(count)", value: $count.animation(), in: 0 ... 36)
                .onChange(of: count) { _ in
                    ensureColors()
                }
        }
    }

    private func ensureColors() {
        let missingCount = count - colors.count
        guard missingCount > 0 else { return }
        colors.append(contentsOf: getColors(count: missingCount))
    }

    private func getColors(count: Int) -> [Color] {
        var colors: [Color] = []
        for _ in 0 ..< count {
            colors.append(.random())
        }
        return colors
    }
}
```
