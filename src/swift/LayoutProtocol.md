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
"Layout" %} protocol "defines the geometry of a collection of views".
It is useful in apps that need view layouts that cannot be easily achieved
using standard container views like `HStack` and `VStack`.

SwiftUI provides three structs that conform to the `Layout` protocol.
These take `alignment` and `spacing` arguments that customize the layout.

- {% aTargetBlank "https://developer.apple.com/documentation/swiftui/hstacklayout", "HStackLayout" %}
- {% aTargetBlank "https://developer.apple.com/documentation/swiftui/vstacklayout", "VStackLayout" %}
- {% aTargetBlank "https://developer.apple.com/documentation/swiftui/zstacklayout", "ZStackLayout" %}

Custom layouts that conform to the `Layout` protocol can also be created.
The following code provides a heavily commented example
derived from the Stewart Lynch video:

```swift
import SwiftUI

/**
  This is a custom layout that is a variation on VStackLayout.
  Child views are referred to as "subviews".
  Subviews that have even indexes (0, 2, ...) are left-aligned.
  Subviews that have odd indexes (1, 3, ...) are also left-aligned,
  but are indented in by the width of the widest even subview.
 */
struct AlternateStackLayout: Layout {
    // This is used to share data between methods in the `Layout` protocol.
    struct Cache {
        let maxEvenWidth: CGFloat
        let maxOddWidth: CGFloat
        let sizes: [CGSize]
    }

    // This is called first in order to computed data
    // that can be used by the other methods.
    func makeCache(subviews: Subviews) -> Cache {
        // Get the size of each subview.
        // The type is `[CGSize]`.
        // `CGSize` has `width` and `height` properties.
        let sizes = subviews.map { $0.sizeThatFits(.unspecified) }

        // Get the maximum width of the even subviews
        // and the maximum width of the odd subviews.
        var maxEvenWidth = 0.0
        var maxOddWidth = 0.0
        var isEven = true
        for size in sizes {
            let width = size.width
            if isEven {
                if width > maxEvenWidth { maxEvenWidth = width }
            } else {
                if width > maxOddWidth { maxOddWidth = width }
            }
            isEven.toggle()
        }

        return Cache(
            maxEvenWidth: maxEvenWidth,
            maxOddWidth: maxOddWidth,
            sizes: sizes
        )
    }

    // This is called before `placeSubviews`.
    // It determines the container width and height required to
    // hold all the subviews in their computed locations.
    func sizeThatFits(
        proposal: ProposedViewSize, // seems to be the entire screen size
        subviews: Subviews,
        cache: inout Cache
    ) -> CGSize {
        subviews.isEmpty ? .zero : CGSize(
            width: cache.maxEvenWidth + cache.maxOddWidth,
            height: cache.sizes.map { $0.height }.reduce(0, +)
        )
    }

    // This is called after `sizeThatFits`.
    // It places each subview at a computed x, y location with a proposed size.
    func placeSubviews(
        in bounds: CGRect,
        proposal: ProposedViewSize, // seems to be the entire screen size
        subviews: Subviews,
        cache: inout Cache
    ) {
        // If there are no subviews then there is no work to be done.
        // guard !subviews.isEmpty else { return }

        // Determine the `x` values for even and odd subviews.
        let evenX = bounds.minX
        let oddX = bounds.minX + cache.maxEvenWidth

        // Determine the `y` value of the first subview.
        var y = bounds.minY

        var x = evenX
        for (subview, size) in zip(subviews, cache.sizes) {
            subview.place(
                at: CGPoint(x: x, y: y),
                anchor: .topLeading,
                proposal: ProposedViewSize(size)
            )

            // The next subview will use the other x value.
            x = x == evenX ? oddX : evenX

            // The y value for the next subview will greater than
            // the y value of this subview by the height of this subview.
            y += size.height
        }
    }
}
```

## AnyLayout Struct

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/anylayout", "AnyLayout" %}
struct is a type-erased instance of the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/layout", "Layout" %}
protocol.
`AnyLayout` is used "to enable dynamically changing the type of
a layout container without destroying the state of the subviews".

The following code displays a series of rectangles and
uses `AnyLayout` to allows the user to toggle between
the three provided layouts and the custom layout implemented above.
This code is also derived from the Stewart Lynch video.

```swift
import SwiftUI

struct ContentView: View {
    @State private var layoutType = LayoutType.h

    enum LayoutType: Int, CaseIterable {
        case h, v, z, alt

        var index: Int {
            Self.allCases.firstIndex(where: { $0 == self })!
        }

        var layout: any Layout {
            switch self {
            case .h: return HStackLayout(alignment: .top, spacing: 0)
            case .v: return VStackLayout(alignment: .leading, spacing: 0)
            case .z: return ZStackLayout(alignment: .topLeading)
            case .alt: return AlternateStackLayout()
            }
        }
    }

    struct Box {
        let color: Color
        let width: CGFloat
        let height: CGFloat
    }

    private let boxes = [
        Box(color: .indigo, width: 100, height: 100),
        Box(color: .teal, width: 80, height: 80),
        Box(color: .purple, width: 60, height: 60),
        Box(color: .red, width: 40, height: 40)
    ]

    var body: some View {
        NavigationStack {
            AnyLayout(layoutType.layout) {
                ForEach(boxes, id: \.color) { box in
                    box.color.frame(
                        width: box.width,
                        height: box.height
                    )
                }
            }
            .padding()
            .navigationTitle("AnyLayout Demo")
            .animation(.default, value: layoutType)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(
                        action: {
                            let cases = LayoutType.allCases
                            layoutType =
                                cases[(layoutType.index + 1) % cases.count]
                        },
                        label: {
                            Image(systemName: "circle.grid.3x3.circle.fill")
                                .imageScale(.large)
                        }
                    )
                }
            }
        }
    }
}
```

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
