---
eleventyNavigation:
  key: Widgets
  parent: Swift
layout: topic-layout.njk
---

## Setup Steps

- Create a new iOS app in Xcode.
  The project name will be the name of the widget displayed to the user,
  so choose a name that will be meaningful to users.
- Select File ... New ... Target...
- Select "Widget Extension".
- Click the "Next" button.
- In "Product Name", enter a widget name typically ending in "Widget".
- Click the "Finish" button.
- Click the "Activate" button.

This creates a new folder in the Navigator
whose name matches the widget name.
This folder will contain a `.swift` file
whose name also matches the widget name.
The file contains starting code that only renders the current time.

## Terminology

Timeline - an array of `TimelineEntry` objects that describe
when the widget display should be updated

Entry - struct that holds data used by the widget

The provided sample code defines the struct `SimpleEntry`.
Rename this to be relevant to your widget.

Reload Policy - determines when the widget requests a new timeline;
options include:

- `atEnd` to get a new timeline after the last date in the current line passes
- `after` to get a new timeline after a specified date/time
- `never` to only get a new timeline when the app makes one available

## Configuration

Widgets can have static configuration that users cannot customize
or an `IntentConfiguration` which allows users to long press the widget
to reveal customization options.

## Adding Files

When adding source files within the widget folder of the Navigator,
add them to the widget extension target.
To do this, click a file in the Navigator, open the Inspector on the right,
and click the checkbox for the target under "Target Membership".
