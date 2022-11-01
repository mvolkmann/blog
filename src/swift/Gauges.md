---
eleventyNavigation:
  key: Gauges
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS 16 added the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/gauge", "Gauge" %} view
that graphically displays a value within a range.

## Example App

See the example app {% aTargetBlank
"https://github.com/mvolkmann/GaugesDemo", "GaugesDemo" %}.
It is based on the YouTube video {% aTargetBlank
"https://www.youtube.com/watch?v=k38t-tjCM7g", "Gauge View iOS 16" %}
by Stewart Lynch.

## Gauge

The `Gauge` view takes the following arguments:

- `value`: required `Double` or `Int`
- `in`: an optional `Double` or `Int` `Range` that defaults to `0...1`
- `label`: required closure that returns a view to describe the gauge
- `currentValueLabel`: optional closure that returns a view to display the current value
- `minimumValueLabel`: optional closure that returns a view to display the minimum value
- `maximumValueLabel`: optional closure that returns a view to display the maximum value

The `gaugeStyle` view modifier specifies the kind of gauge to be displayed.
Supported argument values include:

- `.linearCapacity`

  This displays a horizontal bar representing the entire range of values.
  This bar is gray by default and there does not seem to be a way to change it.
  The current value is indicated by drawing a bar in the specified tint color
  on top of the background bar starting from the left.

  A `label`, if provided, is displayed
  above the bar centered horizontally.

  The `currentValueLabel`, if provided, is displayed
  below the bar centered horizontally.

  The `minimumValueLabel` and `maximumValueLabel`, if provided,
  are displayed on the leading and trailing edge.
  If one is provided, the other must also be provided.
  They cannot be `nil` or an `EmptyView`, but they can be `Text("")`.

- `.accessoryLinear`

  This displays a thinner bar than `.linearCapacity`.

  The current value is indicated by a circle drawn on the bar that
  has the specified tint color and includes a small amount of clear padding.
  If the tint is a gradient, the circle color takes on the
  appropriate color at its position within the gradient.

  A `label` is required but is not displayed.

  The `currentValueLabel`, if provided, is displayed on the leading edge
  only if the `minimumValueLabel` and `maximumValueLabel` are not provided.
  The `minimumValueLabel` and `maximumValueLabel` values, if provided,
  appear on the leading and trailing edges.

- `.accessoryLinearCapacity`

  This is similar to `.linearCapacity`, but draws a thinner bar.

  The `label` is displayed above the bar,
  but on the leading edge instead of being centered horizontally.

  The `currentValueLabel` is displayed below the bar,
  but on the leading edge instead of being centered horizontally.

  Gradient tints are not supported in this style.

- `.accessoryCircular`

  This displays a circle that is open at the bottom where either a `label`
  or `minimumValueLabel` and `maximumValueLabel` can be displayed.

  The current value is indicated by a circle drawn on the bar that
  has the specified tint color and includes a small amount of clear padding.
  If the tint is a gradient, the circle color takes on the
  appropriate color at its position within the gradient.

  The `currentValueLabel` is displayed in the center.

  The opening at the bottom of the circle can contain
  either the `currentValueLabel` or
  the pair `minimumValueLabel` and `maximumValueLabel`.

- `.accessoryCircularCapacity`

  This displays a complete circle that can
  include `currentValueLabel` in the center.

  The `label`, `minimumValueLabel`, and `maximumValueLabel` are not used.

  The circle color uses a lower opacity version of the specified tint color.
  The current value is indicated by drawing an arc in the specified tint color
  starting at the top of the circle and going clockwise.
  There does not seem to be a way to specify separate colors
  for the background and foreground circles.

<img alt="SwiftUI Linear Gauges" style="width: 45%"
  src="/blog/assets/swiftui-gauges-linear.png?v={{pkg.version}}"
  title="SwiftUI Linear Gauges">
<img alt="SwiftUI Circular Gauges" style="width: 45%"
  src="/blog/assets/swiftui-gauges-circular.png?v={{pkg.version}}"
  title="SwiftUI Circular Gauges">
