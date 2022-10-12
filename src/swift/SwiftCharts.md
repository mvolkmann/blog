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

Pie charts are not supported. This is an intentional omission
based on the opinion that pie charts are often not the best choice
for indicating the differences between values that are somewhat close.

Values to be plotted can be quantitative (`Int` or `Double`),
nominal (`String` or enums with `String` values),
or temporal (`Date` representing a date or time).

One axis must correspond to quantitative data and
the other axis must correspond to either nominal or temporal data.
If the y values are quantitative then the chart will be vertical
and the x values will be used for x-axis labels.
If the x values are quantitative then the chart will be horizontal.
and the y values will be used for y-axis labels.

By default charts occupy all the space made available to them
by their parent view.

## Resources

See the excellent YouTube videos from Stewart Lynch:

- {% aTargetBlank "https://www.youtube.com/watch?v=dIvE-E8nlsA",
  "Charts Framework 1 - BarCharts Introduction" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=lB3poCA8ZN4",
  "Charts Framework 2 - Visualizing Large Data Sets with Bar Charts" %}

## Example App

See {% aTargetBlank "https://github.com/mvolkmann/ChartsDemo", "ChartsDemo" %}
which is a SwiftUI app that demonstrates nearly everything
shared in the Steward Lynch videos linked above.

## Marks

Swift Charts supports six kinds of "marks" for creating various kinds of charts.
Each of these serve as direct children of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart", "Chart" %} view.
They include:

- {% aTargetBlank "https://developer.apple.com/documentation/charts/areamark", "AreaMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/barmark", "BarMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/linemark", "LineMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/pointmark", "PointMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/rectanglemark", "RectangleMark" %}
- {% aTargetBlank "https://developer.apple.com/documentation/charts/rulemark", "RuleMark" %}

The initializer for each mark type takes `x` and `y` arguments
that have the type {% aTargetBlank
"https://developer.apple.com/documentation/charts/plottablevalue",
"PlottableValue" %}.
Instances are created by calling the `PlottableValue` static method `value`
which takes a `String` label and a value that can be
a number (quantitative), `String` (nominal), or `Date` (temporal).

It is typical to iterate over a collection of objects
that hold data to be plotted using `ForEach`
and create one or more marks from each object.
These objects must conform to the `Identifiable` protocol
which requires an `id` property.

If the only child view of the `Chart` is a `ForEach`,
the collection can be passed to the `Chart` initializer
and the `ForEach` can be removed.

## `Chart`

The {% aTargetBlank "https://developer.apple.com/documentation/charts/chart",
"Chart" %} view contains marks that define the chart to be displayed.

To set the height of a chart and its background color, apply the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart/chartplotstyle(content:)",
"chartPlotStyle" %} view modifier to the `Chart`.
For example:

```swift
Chart {
    ...
}
.chartPlotStyle { plotArea in
    plotArea
        .frame(height: 400)
        .background(.yellow.opacity(0.2))
}
```

To customize the x-axis, apply the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart/chartxaxis(content:)",
"chartXAxis" %} view modifier.
To customize the y-axis, apply the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart/chartyaxis(content:)",
"chartYAxis" %} view modifier.
For more on these, see the "Axis Labels" section below.

To detect tap and drag gestures on the chart, apply the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart/chartoverlay(alignment:content:)",
"chartOverlay" %} view modifier.
For more on this, see the "Event Handling" section below.

## `BarMark`

Instances of the `BarMark` struct describe individual bars in a bar charts.

Negative quantitative values cause the bar to be rendered
below the typical x-axis..

Bars can be stacked by including multiple instances
with the same nominal or temporal value.

To assign a different color to each corresponding `BarMark`,
apply the `foregroundStyle` view modifier.
This can be passed a specific `Color`.
To allow Swift Charts to automatically choose a different color
for each corresponding `BarMark`, use
`.foregroundStyle(by: .value("some label", data.someProperty))`.

In bar charts that use temporal data, the unit defaults to `.hour`.
With this unit the axis that represents the temporal values
is scaled to accommodate 24 bars in each day.
To instead display bars that correspond to entire days,
pass the `unit` argument to the `BarMark` initializer with a value of `.day`.

To annotate a bar, apply the `annotation` view modifier to a `BarMark`.
By default this renders a given view above the bar.
To position the annotation on the center of the bar,
pass the `position` argument with a value of `.overlay`.
For example:

```swift
BarMark(x: value1, y: value2)
    .annotation(position: .overlay) {
        Text("\(data.quantity)")
            .bold()
            .foregroundColor(.white)
    }
}
```

## `LineMark`

These are used to display line charts.

To assign a different color to each corresponding `LineMark`,
apply the `foregroundStyle` view modifier
as described in the `BarMark` section above.

## `PointMark`

These are used to display scatter plots or to add points to line charts.

To assign a different color to each corresponding `PointMark`,
apply the `foregroundStyle` view modifier
as described in the `BarMark` section above.

## `AreaMark`

These are used to display area charts which shade the
area below what would otherwise be a line chart.

There can only be one area on the chart.
Shading below two areas with different colors
is not currently supported.

The example app draws one line for male data and one line for female data.
Toggling the "Show Area" option causes it to only shade below the male line.

## `RectangleMark`

These are often used to display heat maps.

## `RuleMark`

These add a vertical line in vertical charts
or a horizontal line in horizontal charts.
One example use if for indicating the average value.

To add an text annotation to a `RuleMark`,
apply the `annotation` view modifier.
The following example adds a red, dashed, horizontal line at
the y value that is the average of the quantitative values being plotted.
The text "Average" appears below this line beginning at its leading end.

```swift
RuleMark(y: .value("Average", average))
    .foregroundStyle(.red)
    .lineStyle(StrokeStyle(lineWidth: 1, dash: [10]))
    .annotation(position: .bottom, alignment: .leading) {
         Text("Average").font(.caption)
    }
```

## Axis Labels

To customize axis labels, apply the `chartXAxis`
and/or `chartYAxis` view modifiers to the `Chart`.
Each of these take a closure.
If the closure is empty, the axis is hidden.

To customize axis labels, call `AxisMarks` inside the previous closure.
This takes another closure that is passed a value.
Inside this closure, call `AxisValueLabel` and specify
what should be displayed for the given value.
To center labels below their bar,
pass the `centered` argument with a value of `true`.
For example:

```swift
Chart {
    ...
}
.chartXAxis {
    AxisMarks { value in
        AxisValueLabel(
            format: .dateTime.month().day(),
            centered: true
        )
    }
}
```

## Legends

By default, a legend is displayed below each chart.

To hide the legend, apply the `chartLegend` view modifier to the `Chart`,
passing a value of `.hidden`.
For example:

```swift
Chart { ... }.chartLegend(.hidden)
```

To change the legend position, apply the `chartLegend` view modifier to the `Chart`,
passing the `position` argument with a value like `.top`, `.trailing`, or `.leading`.
For example:

```swift
Chart { ... }.chartLegend(position: .top)
```

## Scrolling

To create a chart that is larger that its allocation screen space
and can be scrolled horizontally to view all the content:

1. Embed the `Chart` in a `ScrollView(.horizontal)`.
1. Apply the `frame` view modifier to the `ScrollView`
   to set its display width. For example:

   ```swift
   .frame(width: 400)
   ```

1. Apply the `chartPlotStyle` view modifier to the `Chart`
   to set the chart width and height. For example:

   ```swift
   .chartPlotStyle { plotArea in
       plotArea
           .frame(width: 1000, height: 400)
           .background(.yellow.opacity(0.2))
       }
   ```

The y-axis will only be visible when scrolled all the way to the right.
The y-axis can be moved to the leading edge of the chart,
but then it would only be visible when scrolled all the way to the left.
Swift Charts really needs an option to
make the y-axis sticky in horizontally scrolling charts and
make the x-axis sticky in vertically scrolling charts.

To scroll vertically instead of horizontally,
embed the `Chart` in a `ScrollView` without passing `.horizontal`
and set the `ScrollView` height instead of the width.

## Event Handling

To listen for tap and drag gestures on a chart,
apply the `chartOverlay` view modifier to the `Chart`.
Note that drag gestures will not be captured
if the `Chart` is inside a `ScrollView`.

For example, the following somewhat complex code displays
an annotation above a bar chart when dragging across the bars:

```swift
struct BarChartDemo: View {
    @State private var selectedData: MyDataStruct?

    private var annotation: some View {
        VStack {
            if let selectedData {
                Text(selectedData.key)
                Text("\(selectedData.quantity)")
            }
        }
        .padding(5)
        .background {
            RoundedRectangle(cornerRadius: 5, style: .continuous)
                .fill(annotationFill)
        }
        .foregroundColor(Color(.label))
    }

    private var annotationFill: some ShapeStyle {
        let fillColor: Color = colorScheme == .light ?
            .white : Color(.secondarySystemBackground)
        return fillColor.shadow(.drop(radius: 3))
    }

    var body: some View {
        Chart {
            ForEach(objects.indices, id: \.self) { index in
                let object = objects[index]
                let key = PlottableValue.value("Key Title", object.key)

                BarMark(x: key, y: .value("Quantity", object.quantity))

                if object.key == selectedData?.key {
                    RuleMark(x: key)
                        .annotation(position: annotationPosition(index)) {
                            annotation
                        }
                        // Display a red, dashed, vertical line.
                        .foregroundStyle(.red)
                        .lineStyle(StrokeStyle(dash: [10, 5]))
                  }
              }
        }
        .chartOverlay { proxy in chartOverlay(proxy: proxy) }
    }

    private func chartOverlay(proxy: ChartProxy) -> some View {
        GeometryReader { geometry in
            let areaX = geometry[proxy.plotAreaFrame].origin.x
            return Rectangle()
                .fill(.clear)
                .contentShape(Rectangle())

                // Handle tap gestures.
                .onTapGesture { value in
                    let x = value.x - areaX
                    if let category: String = proxy.value(atX: x) {
                        let data = categoryToDataMap[category]
                        print("got tap on", data)
                    }
                }

                // Handle drag gestures.
                .gesture(
                    DragGesture()
                        .onChanged { value in
                            let x = value.location.x - areaX
                            if let category: String = proxy.value(atX: x) {
                                selectedData = categoryToDataMap[category]
                            }
                        }
                        .onEnded { _ in selectedData = nil }
                )
        }
    }
}
```

## Animation
