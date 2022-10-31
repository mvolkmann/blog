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

- `value`: a `Double`
- `in`: a `Double` `Range`
- `label`: closure that returns a view to describe the gauge
- `currentValueLabel`: optional closure that returns a view to display the current value
- `minimumValueLabel`: optional closure that returns a view to display the minimum value
- `maximumValueLabel`: optional closure that returns a view to display the maximum value

The `gaugeStyle` view modifier specifies the kind of gauge to be displayed.
Supported argument values include:

- `.linearCapacity`: ?
- `.accessoryLinear`: ?
- `.accessoryLinearCapacity`: ?
- `.accessoryCircular`: ?
- `.accessoryCircularCapacity`: ?
- others?
