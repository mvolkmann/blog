---
eleventyNavigation:
  key: Instruments
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/xcode/features/", "Instruments" %}
is a profiling app included with Xcode for analyzing things like:

- How many views were created?
- How often are the views redrawn?
- How long did it take create them?
- How many times was the evaluation of a view body slow?
- What properties do the views have?
- How did the view properties changed over time?
- How has the state of a view changed over time?
- How long did each function execution take to complete?
- How many core animation commits occurred?

While profiling can be performed in the Simulator,
running on a device will provide more realistic results.

## Launching

There are three ways to launch the app using Instruments:

- press cmd-i
- Select Product ... Profile
- Select Xcode ... Open Developer Tool ... Instruments.

The Instruments app will open a dialog for choosing at profiling template.
A commonly used profiling template is "SwiftUI",
but there are over 20 templates to try including "Core Data",
"Leaks", "Logging", "Network" (ex. HTTP traffic), and "Swift Concurrency".

## SwiftUI Profiling

Select the "SwiftUI" category and click the "Choose" button.
A new window will open where profiling data will be displayed.

Click the record button in the upper-left to
launch the app (in release mode) and begin analyzing its performance.
Exercise the app in the Simulator to trigger the functionality to be analyzed.

Click the pause or stop buttons in the upper-left
to stop execution so the results so far can be examined.
Select a category row in the top half to see
detail on that category displayed in the bottom half.
Categories include:

- View Body

  This category measures the number and duration of view body evaluations
  divided into provided SwiftUI views and custom views.

- View Properties

  This category shows the current and previous values of all view properties.
  Unfortunately it does not show the names of the properties.
  I wasn't able to get this category to show ANY data!

- Core Animation Commit

  This category shows the number and duration of
  Core Animation transaction commits.

- Time Profiler

  This category shows time spent in each part of the code.

The bottom half displays a table of statistics for the selected category.
The columns "Count" and "Average Duration" are particularly interesting.
For example, when the "View Body" category is selected,
we can see how many times a `Button` view was evaluated
and the average number of milliseconds or microseconds required each time.

For the "View Body" category, the "Timing Summary" in the bottom half
of the window divides the data into two categories, the app and SwiftUI.
Expand those sections to get detail on each.
The views in each section are sorted by their "Count" from largest to smallest.

## Timelines

To zoom in and out on the bars in the top half of the window,
press cmd-plus and cmd-minus.

To see data summarized over a particular time interval of the run,
identify the time interval of interest by
dragging across any bar in the top half of the window.
