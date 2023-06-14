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

Support for pie and donut charts was added in iOS 17.

Values to be plotted can be quantitative (`Int`, `Double`, or `Decimal`),
nominal (`String` or enum with `String` values),
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

- {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2022/10136/",
  "Hello Swift Charts" %} from Apple WWDC 2022
- {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2022/10137",
  "Swift Charts: Raise the bar" %} from Apple WWDC 2022
- {% aTargetBlank "https://www.youtube.com/watch?v=dIvE-E8nlsA",
  "Charts Framework 1 - BarCharts Introduction" %} from Stewart Lynch
- {% aTargetBlank "https://www.youtube.com/watch?v=lB3poCA8ZN4",
  "Charts Framework 2 - Visualizing Large Data Sets with Bar Charts" %}
  from Stewart Lynch
- {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2023/10037/",
  "Explore pie charts and interactivity in Swift Charts" %}
  from Apple WWDC 2023

## Example App

See {% aTargetBlank "https://github.com/mvolkmann/ChartsDemo", "ChartsDemo" %}
which is a SwiftUI app that demonstrates nearly everything
shared in the Steward Lynch videos linked above.

## Chart

The {% aTargetBlank "https://developer.apple.com/documentation/charts/chart",
"Chart" %} view contains marks that define the chart to be displayed.

Charts are composed of three kinds of areas,
axes (x and y), plot area, and legend.
Each of these can be customizes.
Axes and the legend can be hidden.

To set the height of a chart, its background color, and a border,
apply the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart/chartplotstyle(content:)",
"chartPlotStyle" %} view modifier to the `Chart`.
For example:

```swift
Chart {
    ...
}
.chartPlotStyle { plotArea in
    plotArea
        .frame(height: 400) // can also set width
        .background(.yellow.opacity(0.2))
        .border(.purple, width: 5)
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
"chartOverlay" %} view modifier or the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart/chartbackground(alignment:content:)",
"chartBackground" %} view modifier.
For more on these, see the "Event Handling" section below.

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
- {% aTargetBlank "https://developer.apple.com/documentation/charts/sectormark", "SectorMark" %}

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
If these objects do not conform to the `Identifiable` protocol,
which requires an `id` property, add the `id` argument to the `ForEach`
to specify a key path that uniquely identifiers each object.

If the only child view of the `Chart` is a `ForEach`,
the collection can be passed to the `Chart` initializer
and the `ForEach` can be removed.
The `Chart` initializer also supports the `id` argument.

### BarMark

Instances of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/barmark", "BarMark" %}
struct describe individual bars in a bar charts.

Negative quantitative values cause the bar to be rendered
below the typical x-axis..

To assign a different color to each corresponding `BarMark`,
apply the `foregroundStyle` view modifier.
This can be passed a specific `Color`.
To allow Swift Charts to automatically
choose a different color for the marks in each data series,
pass the `by` argument that identifies the data series.
For example:

```swift
BarMark(x: ageCategory, y: .value("Male", statistic.male))
    .foregroundStyle(by: .value("Gender", "Male"))
BarMark(x: ageCategory, y: .value("Female", statistic.female))
    .foregroundStyle(by: .value("Gender", "Female"))
```

Bars are automatically stacked when there are multiple instances
with the same nominal or temporal value.
To display the corresponding bars side-by-side instead of stacking them,
apply the `position` view modifier,
passing the `by` argument that identifies the data series.
For example:

```swift
BarMark(x: ageCategory, y: .value("Male", statistic.male))
    .foregroundStyle(by: .value("Gender", "Male"))
    .position(by: .value("Gender", "Male"))
BarMark(x: ageCategory, y: .value("Female", statistic.female))
    .foregroundStyle(by: .value("Gender", "Female"))
    .position(by: .value("Gender", "Female"))
```

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

### LineMark

Instances of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/linemark", "LineMark" %}
struct are used to display line charts.

To assign a different color to each corresponding `LineMark`,
apply the `foregroundStyle` view modifier
as described in the `BarMark` section above.

To display points at the ends of each line,
add a `PointMark` (described in the next section)
after each `LineMark` with the same `x` and `y` values.

An alternative to adding `PointMark` instances is to
apply the `symbol` view modifier to the `LineMark` instances.
See the examples in the `PointMark` section below.

To change the line style, apply the `lineStyle` view modifier
to each `LineMark`. For example, the following uses a dashed line:

```swift
LineMark(x: ageCategory, y: male)
    .foregroundStyle(by: .value("Male", "Male"))
    .lineStyle(StrokeStyle(lineWidth: 1, dash: [10]))
```

To smooth the lines, apply the `interpolationMethod` view modifier
to each `LineMark` with the value `.monotone`, `.cardinal`, or
`.catmullRom` (formulated by Edwin Catmull and Raphael Rom).
For example:

```swift
LineMark(x: ageCategory, y: male)
    .foregroundStyle(by: .value("Male", "Male"))
    .interpolationMethod(.catmullRom)
```

### PointMark

Instances of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/pointmark", "PointMark" %}
struct are used to display scatter plots or to add points to line charts.

To assign a different color to each corresponding `PointMark`,
apply the `foregroundStyle` view modifier
as described in the `BarMark` section above.

To display a symbol instead of a filled circle for each point,
apply the `symbol` view modifier to each `PointMark`
passing it a `by` argument whose value identifies a data series.
For example:

```swift
PointMark(x: ageCategory, y: male)
    .foregroundStyle(.blue)
    .symbol(by: .value("Gender", "Male"))
PointMark(x: category, y: female)
    .foregroundStyle(.red)
    .symbol(by: .value("Gender", "Female"))
```

The values `"Male"` and `"Female"` identify the data series
to which each point belongs.
These values could come of the data objects rather than being literal values.

### AreaMark

Instances of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/areamark", "AreaMark" %}
struct are used to display area charts which
shade the area below single values in a data series
(by specifying the `y` argument) or
between two values in a data series
(by specifying the `yStart` and `yEnd` arguments).

An example where shading a range of values is useful is when creating
a chart that shows a series of minimum, average, and maximum values.
`LineMark` instances can be used to plot the average values.
`AreaMark` instances can be used to shade
the area between the minimum and maximum values.

Areas can be stacked, but cannot overlap.

The example app draws one line for male data and one line for female data.
Toggling the "Show Area" option causes it to only shade below the male line
due to this restriction.

### RectangleMark

Instances of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/rectanglemark", "RectangleMark" %}
struct are used to display heat maps.

The following code demonstrates using this to display a heat map
that represents hourly temperature forecasts over the 5-day period.
For a full implementation that uses [WeatherKit](/blog/swift/weatherkit)
to get real temperature forecasts, see {% aTargetBlank
"https://github.com/mvolkmann/WeatherKitDemo", "WeatherKitDemo" %}.

<img alt="Swift Charts heat map" style="width: 45%"
  src="/blog/assets/swift-charts-heat-map.png?v={{pkg.version}}"
  title="Swift Charts heat map">

```swift
import Charts
import SwiftUI

extension Date {
    // Returns an abbreviated day of the week (ex. Sun).
    var dayOfWeek: String {
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "EEE"
        return dateFormatter.string(from: self)
    }

    // Returns 1 for Sunday and 7 for Saturday.
    var dayOfWeekNumber: Int {
        Calendar.current.dateComponents([.weekday], from: self).weekday!
    }

    // Returns the hour of a `Date`.
    var hour: Int {
        Calendar.current.component(.hour, from: self)
    }
}

// This holds a subset of the properties
// in the WeatherKit `HourWeather` struct.
struct HourWeather {
    let date: Date
    let temperature: Measurement<UnitTemperature>
}

struct ContentView: View {
    private static let daysOfWeek =
        ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

    private static let days = 5

    private static let gradientColors: [Color] =
        [.blue, .green, .yellow, .orange, .red]

    @State private var hourlyForecast: [HourWeather] = []

    // This is displayed on the left side of the heat map.
    private var dayLabels: some View {
        VStack(spacing: 21) {
            let startIndex = Date().dayOfWeekNumber - 1
            let range =
                startIndex ..< startIndex + Self.days
            ForEach(range, id: \.self) { index in
                dayLabel(Self.daysOfWeek[index % 7])
            }
        }
        .padding(.top, 11)
    }

    var body: some View {
        HStack(alignment: .top, spacing: 0) {
            dayLabels
            ScrollView(.horizontal) {
                heatMap()
            }
        }
        .onAppear {
            generateData()
        }
    }

    // This displays an abbreviated day of the week (ex. Sun)
    // rotated to read from the right.
    private func dayLabel(_ day: String) -> some View {
        Text(day)
            .font(.subheadline)
            .rotationEffect(Angle.degrees(-90))
            .frame(height: 55)
    }

    // We could use WeatherKit to get real temperature forecasts
    // as is done in https://github.com/mvolkmann/WeatherKitDemo.
    // To keep things simple this generates random temperatures.
    private func generateData() {
        var date = Date() // current date and time
        let calendar = Calendar.current
        date = calendar.date(bySetting: .hour, value: 0, of: date)!

        var temperature = 50.0

        var data: [HourWeather] = []
        for _ in 1 ... Self.days { // days
            for _ in 0 ... 23 { // hours
                // Change the temperature by a random amount
                // of not more than two degrees.
                temperature += Double.random(in: -2 ... 2)
                data.append(HourWeather(
                    date: date,
                    temperature: Measurement(
                        value: temperature,
                        unit: UnitTemperature.fahrenheit
                    )
                ))

                // Advance to the next hour.
                date = calendar.date(byAdding: .hour, value: 1, to: date)!
            }
        }

        hourlyForecast = data
    }

    private func heatMap() -> some View {
        Chart {
            ForEach(hourlyForecast.indices, id: \.self) { index in
                let forecast = hourlyForecast[index]
                mark(forecast: forecast)
            }
        }

        // Set the range of background colors to be used for
        // each `RectangleMark` created in the `mark` method.
        .chartForegroundStyleScale(
            range: Gradient(colors: Self.gradientColors)
        )

        // Add hour labels on the x-axis.
        .chartXAxis {
            AxisMarks(position: .bottom, values: .automatic) { axisValue in
                AxisTick()
                AxisValueLabel(centered: true) {
                    let index = axisValue.index
                    let mod = index % 12
                    let hour = mod == 0 ? 12 : mod
                    Text("\(hour)\n\(index < 12 ? "AM" : "PM")")
                        .multilineTextAlignment(.center)
                }
            }
        }

        // The y-axis labels generated by Swift Charts appear
        // at the top of each row rather that centered on the row.
        // So this hides them and `dayLabels` provides custom y-axis labels.
        .chartYAxis(.hidden)

        .frame(width: 800, height: Double(Self.days * 90))
    }

    // This creates an individual cell in the heat map.
    private func mark(forecast: HourWeather) -> some ChartContent {
        let date = forecast.date
        let fahrenheit = forecast.temperature.converted(to: .fahrenheit).value

        return Plot {
            RectangleMark(
                // Why do String values work, but Int values do not?
                x: .value("Time", "\(date.hour)"),
                y: .value("Day", date.dayOfWeek),
                width: .ratio(1),
                height: .ratio(1)
            )
            // Choose a cell color based on the temperature.
            .foregroundStyle(by: .value("Temperature", fahrenheit))
            // Display the temperature on top of the cell.
            .annotation(position: .overlay) {
                Text("\(String(format: "%.0f", fahrenheit))℉")
                    .rotationEffect(.degrees(-90))
                    .font(.body)
                    .frame(width: 55)
            }
        }
    }
}
```

### RuleMark

Instances of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/rulemark", "RuleMark" %}
struct are used to add a vertical line (when only the `x` value is set)
or a horizontal line (when only the `y` value is set).

To add an text annotation to a `RuleMark`,
apply the `annotation` view modifier.

The following example adds a red, dashed, horizontal line at the y value
that is the average of the quantitative values being plotted.
The text "Average" appears below this line beginning at its leading end.

```swift
RuleMark(y: .value("Average", average))
    .foregroundStyle(.red)
    .lineStyle(StrokeStyle(lineWidth: 1, dash: [10]))
    .annotation(position: .bottom, alignment: .leading) {
         Text("Average").font(.caption)
    }
```

The example in the "Event Handling" section below
listens for drag events on the chart and
adds a red, dashed, vertical line
through the mark being dragged over.
Information about the data point is displayed
at the top of the line, above the chart.

## SectorMark

Instances of the {% aTargetBlank
"https://developer.apple.com/documentation/charts/sectormark", "SectorMark" %}
struct are used to add slices to pie and donut charts.
This was added in iOS 17.

Each `SectorMark` specifies an `angle` argument
whose value is an instance of the `PlottableValue` struct
which has many static `value` methods.
The first argument of the `value` method can be
a `String`, `LocalizedStringKey`, or `Text`.
The second argument can be a `Double` (quantitative),
`Date` (temporal), or `String` (categorical).

The size (angle) of each `SectorMark` is computed based on
the proportion of its value to the total.
The total of the values does not need to be 100.

To add a gap between the sectors, specify an `angularInset` argument.
For example, `angularInset: 2`.

To round the corners of a sector, apply the `cornerRadius` view modifier
to a `SectorMark`. For example, `.cornerRadius(10)`.

To create a donut chart instead of a pie chart,
each `SectorMark` must also specify an `innerRadius` argument.
For example, `innerRadius: .ratio(0.618)`.

Each `SectorMark` can also specify an `outerRadius` argument,
although doing so is not typical.
Each `SectorMark` can have different values
for `innerRadius` and/or `outerRadius`, but this is also not typical.

TODO: Is there a way to place text in each `SectorMark`?

## Example Charts

This example plots hourly temperature forecasts from hard-coded data.
It could of course come from WeatherKit.

The code can be found at {% aTargetBlank
"https://github.com/mvolkmann/SwiftChartsBasic", "SwiftChartsBasic" %}.

There is a segmented `Picker` at the top that enables switching between
two chart representations. The first is the bar chart and the second
is a combination of a line chart, area chart, and point chart.

<img alt="Swift Charts bar chart" style="width: 45%"
  src="/blog/assets/swift-charts-bar.png?v={{pkg.version}}"
  title="Swift Charts bar chart">
<img alt="Swift Charts line/area/point chart" style="width: 45%"
  src="/blog/assets/swift-charts-line-area-point.png?v={{pkg.version}}"
  title="Swift Charts line/area/point chart">

The bars and points are assigned a color that selected from
a gradient based on the temperature value they represent.
This is done using the following `Array` extension:

```swift
import SwiftUI

extension Array where Element: UIColor {
    /// Gets a color that is a given percentage through an array of Colors.
    /// - Parameters:
    ///   - percentage: Double between 0.0 and 1.0
    /// - Returns: Color object
    func colorAt(percentage: Double) -> Color {
        guard percentage > 0 else { return Color(first ?? .clear) }
        guard percentage < 1 else { return Color(last ?? .clear) }

        let floatIndex = percentage * Double(count - 1)
        let leftIndex = Int(floatIndex.rounded(.down))
        let rightIndex = Int(floatIndex.rounded(.up))
        let defaultIndex = Int(floatIndex.rounded())

        let leftColor = self[leftIndex]
        let rightColor = self[rightIndex]
        let fallbackColor = self[defaultIndex]

        var (r1, g1, b1, a1): (CGFloat, CGFloat, CGFloat, CGFloat) =
            (0, 0, 0, 0)
        guard leftColor.getRed(&r1, green: &g1, blue: &b1, alpha: &a1)
        else { return Color(fallbackColor) }

        var (r2, g2, b2, a2): (CGFloat, CGFloat, CGFloat, CGFloat) =
            (0, 0, 0, 0)
        guard rightColor.getRed(&r2, green: &g2, blue: &b2, alpha: &a2)
        else { return Color(fallbackColor) }

        let subPercentage = floatIndex - Double(leftIndex)
        let uiColor = UIColor(
            red: CGFloat(r1 + (r2 - r1) * subPercentage),
            green: CGFloat(g1 + (g2 - g1) * subPercentage),
            blue: CGFloat(b1 + (b2 - b1) * subPercentage),
            alpha: CGFloat(a1 + (a2 - a1) * subPercentage)
        )
        return Color(uiColor)
    }
}
```

Here is the code the renders the charts:

```swift
import Charts
import SwiftUI

extension Date {
    // Creates a Date object for a given hour in the current day.
    static func hour(_ hour: Int) -> Date {
        var components = Calendar.current.dateComponents(
            [.year, .month, .day],
            from: Date()
        )
        components.hour = hour
        return Calendar.current.date(from: components)!
    }
}

struct Weather: Identifiable {
    let dateTime: Date
    let temperature: Double
    var id: Date { dateTime }
}

// This is the data to be plotted.
private let forecast: [Weather] = [
    .init(dateTime: Date.hour(8), temperature: 43.0),
    .init(dateTime: Date.hour(9), temperature: 48.0),
    .init(dateTime: Date.hour(10), temperature: 55.0),
    .init(dateTime: Date.hour(11), temperature: 60.0),
    .init(dateTime: Date.hour(12), temperature: 64.0),
    .init(dateTime: Date.hour(13), temperature: 67.0),
    .init(dateTime: Date.hour(14), temperature: 69.0),
    .init(dateTime: Date.hour(15), temperature: 70.0),
    .init(dateTime: Date.hour(16), temperature: 71.0),
    .init(dateTime: Date.hour(17), temperature: 71.0),
    .init(dateTime: Date.hour(18), temperature: 69.0),
    .init(dateTime: Date.hour(19), temperature: 67.0),
    .init(dateTime: Date.hour(20), temperature: 65.0),
    .init(dateTime: Date.hour(21), temperature: 63.0),
    .init(dateTime: Date.hour(22), temperature: 61.0),
    .init(dateTime: Date.hour(23), temperature: 58.0),
    .init(dateTime: Date.hour(24), temperature: 55.0)
]

struct ContentView: View {
    @State private var chartType: String = "bar"

    let areaColor = LinearGradient(
        gradient: Gradient(colors: [.yellow, .blue]),
        startPoint: .top,
        endPoint: .bottom
    )

    // This is used to select bar and point colors
    // based on the temperature they represent.
    let colors: [UIColor] = [.blue, .yellow, .red]

    var body: some View {
        VStack {
            Picker("Chart Type", selection: $chartType) {
                Text("Bar").tag("bar")
                Text("Line").tag("line")
            }
            .pickerStyle(.segmented)

            Chart(forecast) { data in
                let time = PlottableValue.value("Time", data.dateTime)
                let temp = PlottableValue.value("Temperature", data.temperature)
                let color = color(for: data.temperature)
                if chartType == "bar" {
                    // Each BarMark can be a different color.
                    BarMark(x: time, y: temp)
                        .foregroundStyle(color)
                } else {
                    // Each PointMark can be a different color,
                    // but LineMarks and AreaMarks cannot.
                    // They can however be gradient colors.
                    LineMark(x: time, y: temp)
                        .foregroundStyle(.blue)
                        .interpolationMethod(.catmullRom)
                    AreaMark(x: time, y: temp)
                        .foregroundStyle(areaColor.opacity(0.7))
                    PointMark(x: time, y: temp)
                        .foregroundStyle(color)
                }
            }
        }
        .padding()
    }

    // This returns a color to use for a given temperature.
    func color(for temperature: Double) -> Color {
        let low = 30.0
        let high = 100.0
        let percentage = temperature <= low ? 0.0 :
            temperature >= high ? 1.0 :
            (temperature - low) / (high - low)
        return colors.colorAt(percentage: percentage)
    }
}
```

## Series Colors

To set the color to be used for each data series by its name,
apply the `chartForegroundStyleScale` view modifier to the `Chart`.
For example:

```swift
Chart {
    // chart content goes here
}
.chartForegroundStyleScale([
    "Male": .blue,
    "Female": .red,
])
```

## Axis Marks

Axis marks can be added to the x-axis and y-axis.
Each is composed of three optional parts,
a grid line, a tick, and a label.

To customize axis marks, apply the `chartXAxis`
and/or `chartYAxis` view modifiers to the `Chart`.
Each of these take a closure.
Leaving the closure empty causes the axis to be hidden.
However, a more explicit way to hide an axis
is to pass `.hidden` to these view modifiers.

The default axes are the same as what is generated by writing the following:

```swift
Chart {
    // chart content goes here
}
.chartXAxis {
    AxisMarks()
}
.chartYAxis {
    AxisMarks()
}
```

To customize axis marks, add arguments to the `AxisMarks` initializers
and/or add a trailing closure that is passed a single axis value.

The `AxisMarks` argument `position` indicates where the axis should appear.
The x-axis can appear below or above the chart.
The y-axis can appear on the left or right side of the chart.

The `AxisMarks` argument `values` provides an array of desired axis labels.

Inside the closure passed to `AxisMarks` it is common to call
`AxisGridLine`, `AxisTick`, and `AxisValueLabel`.
`AxisGridLine` causes a grid line to be drawn across the plot area.
`AxisTick` causes a tick to be drawn before an axis label.
`AxisValueLabel` specifies the axis label
that should be displayed for the given value.

`AxisGridLine`, `AxisTick`, and `AxisValueLabel` can be called conditionally
based on the value passed to the `AxisMarks` trailing closure
so they only appear for some marks.

The `AxisValueLabel` argument `format` specifies
how axis labels should be formatted.
The `AxisValueLabel` argument `centered` specifies
whether axis labels should be centered relative to their mark.
`AxisValueLabel` can have a trailing closure that returns
the view that should be used to render the label.

For example:

```swift
Chart {
    // chart content goes here
}
.chartXAxis {
    AxisMarks(position: .leading) { value in
        AxisGridLine()
        AxisTick()
        AxisValueLabel(
            format: .dateTime.month().day(),
            centered: true
        )
        // Alternative that formats labels to be
        // a number of millions followed by the letter M.
        // AxisValueLabel {
        //     Text(value == 0 ? "" : "\(value / delta)M")
        // }
    }
}
```

## Legends

By default, a legend is displayed below each chart.

To hide the legend, apply the {% aTargetBlank
"https://developer.apple.com/documentation/charts/chart/chartlegend(_:)",
"chartLegend" %} view modifier to the `Chart`, passing a value of `.hidden`.
For example:

```swift
Chart {
    // chart content goes here
}
.chartLegend(.hidden)
```

To change the legend position, apply the `chartLegend` view modifier to the `Chart`,
passing the `position` argument with a value like `.top`, `.trailing`, or `.leading`.
For example:

```swift
Chart { ... }.chartLegend(position: .top)
```

## Scale

Swift Charts automatically determines the minimum and maximum
quantitative values to be plotted and
selects appropriate x-axis and y-axis scales.
The default axis scales can be overridden by applying the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/chartxscale(domain:range:type:)",
"chartXScale" %} and {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/chartyscale(domain:range:type:)",
"chartYScale" %} view modifiers to the `Chart`.
For example:

```swift
Chart {
    // chart content goes here
}
.chartYScale(domain: 19 ... 157)
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

## Background and Overlay

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/chartbackground(alignment:content:)?changes=latest_minor",
"chartBackground" %} view modifier can be applied to a `Chart`
to specify any view that should be rendered behind the chart marks.

The `chartOverlay` view modifier can be applied to a `Chart`
to specify any view that should be rendered on top of the chart marks.

For example:

```swift
Chart {
    // chart content goes here
}
.chartBackground { _ in
    Image(systemName: "flag.checkered")
        .resizable()
        .frame(width: 300, height: 300)
}
```

## Event Handling

To listen for tap and drag gestures on a chart, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/chartoverlay(alignment:content:)?changes=latest_minor",
"chartOverlay" %} view modifier to the `Chart`.
Note that drag gestures will not be captured
if the `Chart` is inside a `ScrollView`.

For example, the following somewhat complex code displays
an annotation above a bar chart when dragging across the bars:

```swift
struct BarChartDemo: View {
    @Environment(\.colorScheme) private var colorScheme
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

        // This fixes the chart jump issue during dragging.
        // See https://developer.apple.com/forums/thread/724770.
        .chartYScale(domain: minYValue...maxYValue)

        .chartOverlay { proxy in chartOverlay(proxy: proxy) }
    }

    // This chooses a position based on whether
    // the data point is near one of the chart edges.
    private func annotationPosition(_ index: Int) -> AnnotationPosition {
        let percent = Double(index) / Double(vm.statistics.count)
        return percent < 0.1 ? .topTrailing :
            percent > 0.95 ? .topLeading :
            .top
    }

    // ChartProxy is useful for supporting user interactions like tap and drag.
    private func chartOverlay(proxy: ChartProxy) -> some View {
        GeometryReader { geometry in // of the overlay view
            let origin = geometry[proxy.plotAreaFrame].origin
            Rectangle()
                .fill(.clear)
                .contentShape(Rectangle())

                // Handle tap gestures.
                .onTapGesture { value in
                    let x = value.x - origin.x

                    // The ChartProxy "position" method translates a
                    // data value to a coordinate value within the chart.
                    // The ChartProxy "value" method translates a
                    // coordinate value within the chart to a data value.
                    if let category: String = proxy.value(atX: x) {
                        let data = categoryToDataMap[category]
                        print("got tap on", data)
                    }
                }

                // Handle drag gestures.
                .gesture(
                    DragGesture()
                        .onChanged { value in
                            let x = value.location.x - origin.x
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

Charts can be animated using `withAnimation` and
changing the values to be plotted from zero to their actual value.
This can be done at a different point in time for each data point
so they each animate individually.

One way to implement this is described below.

1. Add a state variable to hold an array of `Bool` values
   that indicate the data points that should shown now.

   ```swift
   @State private var show: [Bool] = []
   ```

1. In the loop inside the `Chart` where marks are created,
   determine if the current data point should be shown now,
   base its value on that, and use that value in the mark.

   ```swift
   let shouldShow = index < show.count && show[index] == true
   let value = shouldShow ? data[index].quantity : 0
   ```

1. When the chart appears, call the `animateChart` function.

   ```swift
   Chart {
       // chart content goes here
   }
   .onAppear { animateChart() }
   ```

1. Add the following function:

   ```swift
   private func animateChart() {
       show = []
       for index in vm.statistics.indices {
           // Delay rendering each data point
           // a bit longer than the previous one.
           DispatchQueue.main.asyncAfter(
               deadline: .now() + Double(index) * 0.05
           ) {
               let spring = 0.5
               withAnimation(.interactiveSpring(
                   response: spring,
                   dampingFraction: spring,
                   blendDuration: spring
               )) {
                   show.append(true)
               }
           }
       }
   }
   ```

## Accessibility

To make charts accessible, apply the `accessibilityLabel`
and `accessibilityValue` view modifiers to each mark.
For visually impaired users, the text passed to these
will be spoken when VoiceOver is activated.
