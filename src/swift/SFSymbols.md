---
eleventyNavigation:
  key: SF Symols
  parent: Swift
layout: topic-layout.njk
---

## Overview

SF Symbols is a macOS app from Apple that provides over 4,000 icons.
These can be rendered in custom app using the `Image` view
with the `systemName` argument.

Some symbols support multiple symbol rendering modes
that enable using different colors for parts of the icon.
To see the available rendering modes for a given icon,
select the icon and click the paint brush tab in the Inspector on the right.
Each icon has a preferred rendering mode,
but a different rendering mode can be selected by applying the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/image/renderingmode(_:)",
"renderingMode" %} view modifier.

To specify a symbol rendering mode, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/symbolrenderingmode(_:)",
"symbolRenderingMode" %}. The supported symbol rendering modes include:

- `.monochrome` - single color; seems the same as hierarchical
- `.hierarchical` - single color with multiple opacity levels
- `.palette` - two or three colors
- `.multicolor` - seems the same as hierarchical

When the symbol rendering mode is not specified,
the preferred rendering mode of the icon of is used.

Icons that support variable colors are grouped in the "Variable" category.
These display additional parts of the icon as a percentage value increases.
To see this in action:

- Select one of the icons in the "Variable category.
- Select the paint brush tab in the Inspector on the right.
- Select any rendering mode.
- Activate variable color mode by clicking the
  button to the left of the percentage slider.
- Drag the slider to change the percentage value.
- Note that all the displayed icons update to show the effect.

Here's an example of using such an icon:

```swift
// The @State property wrapper is described later.
// It allows a view instance to maintain state inside the property wrapper.
@State private var percent = 0.0
...
Image(systemName: "cellularbars", variableValue: percent)
Button("Decrease") {
    if percent > 0 { percent -= 0.1 }
}
Button("Increase") {
    if percent < 1 { percent += 0.1 }
}
```

See the {% aTargetBlank
"https://github.com/mvolkmann/SFSymbolsDemo/tree/main", "SFSymbolsDemo" %}
app in GitHub.

To ignore the colors in a multicolor SF Symbol icon
and just use it as a template for applying a single specified color,
apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/image/renderingmode(_:)",
"renderingMode" %} view modifier with the argument value `.template`.
For example:

```swift
Image(systemName: "doc.fill.badge.plus")
    .renderingMode(.template)
    .resizable()
    .foregroundColor(.pink)
    .scaledToFit()
    .frame(width: 100, height: 100)
```
