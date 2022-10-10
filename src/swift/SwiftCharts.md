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

Pie charts are not supported. This was an intentional omission
based on the opinion that pie charts are often not the best choice
for indicating the differences between values that are somewhat close.

## Example App

## Basics

## `BarMark`

These are used to display bar charts.

Bars can be stacked by including multiple instances with the same
`x` value (for vertical bar charts) or `y` value (for horizontal bar charts).

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

## Animation
