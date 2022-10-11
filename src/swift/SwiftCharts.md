---
eleventyNavigation:
  key: Swift Charts
  parent: Swift
layout: topic-layout.njk
---

## Overview

Apple introduced the {% aTargetBlank
"https://developer.apple.com/documentation/charts", "Swift Charts" %}
framework in iOS 16.

"Swift Charts is a powerful and concise SwiftUI framework
for transforming your data into informative visualizations.
With Swift Charts, you can build effective and customizable charts
with minimal code. This framework provides
marks, scales, axes, and legends as building blocks that you can
combine to develop a broad range of data-driven charts."

Swift Charts supports creating bar charts, line charts,
area charts, scatter plots, and heat maps.

By default charts occupy all the space made available to them
by their parent view.

Pie charts are not supported. This was an intentional omission
based on the opinion that pie charts are often not the best choice
for indicating the differences between values that are somewhat close.

Values to be plotted can be quantitative (numbers),
nominal (`String` objects or enums with `String` values),
or temporal (Date objects representing a date or time).

One axis must correspond to quantitative data
and the other axis must correspond to nominal or temporal data.
If y values are quantitative then the chart will be vertical
and the x values will be used for x-axis labels.
If x values are quantitative then the chart will be horizontal.
and the y values will be used for y-axis labels.

## Example App

## Marks

Swift Charts supports six kinds of "marks" for creating various kinds of charts.
Each of these serve as direct children of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart", "Chart" %} view.
They include:

- {% aTargetBlank "https://developer.apple.com/documentation/charts/areamark", "AreaMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/linemark", "LineMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/pointmark", "PointMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/rectanglemark", "RectangleMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/rulemark", "RuleMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/barmark", "BarMark" %}

The initializer for each kind of mark takes `x` and `y` values
that have the type {% aTargetBlank
"https://developer.apple.com/documentation/charts/plottablevalue",
"PlottableValue" %}.
Instances are created by calling the `PlottableValue` static method `value`
which takes a `String` label and
a value that can be a number, `String`, or `Date`.

It is typical to iterate over a collection of objects
that hold data to be plotted using `ForEach`
and create one or more marks from each object.
These objects must conform to the `Identifiable` protocol
which requires an `id` property.

If the only child view of the `Chart` is a `ForEach`,
the collection can be passed to the `Chart` initializer
and the `ForEach` can be removed.

## Basics

## `Chart`

The `Chart` initializer can be passed a collection over which to iterate
to get the data for each mark.

## `BarMark`

These are used to display bar charts.

Bars can be stacked by including multiple instances with the same
`x` value (for vertical bar charts) or `y` value (for horizontal bar charts).

For a stacked bar chart, create multiple `BarMark` views
with the same `x` value (for vertical) or `y` value (for horizontal).
Assign a different color to each `BarMark`
with the `foregroundStyle` view modifier.
To automatically choose a different color for each corresponding `BarMark`,
use `.foregroundStyle(by: .value("some label", data.someProperty))`.

## `LineMark`

These are used to display line charts.

## `PointMark`

These are used to display scatter plots or to add points to line charts.

## `AreaMark`

These are used to display area charts which shade the
area below what would otherwise be a line chart.

## `RectangleMark`

These are often used to display heat maps.

## `RuleMark`

These add a vertical line in vertical charts
or a horizontal line in horizontal charts.
One example use if for indicating the average value.

## Legends

By default, a legend is displayed below each chart.
To hide the legend, ...
To move the legend, ...

## Animation
