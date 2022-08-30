---
eleventyNavigation:
  key: Widgets
  parent: Swift
layout: topic-layout.njk
---

# Setup Steps

- Create a new iOS app in Xcode.
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

# Terminology

Timeline - an array of `TimelineEntry` objects that describe
when the widget display should be updated

Entry - data used by the widget

Reload Policy - determines when the widget requests a new timeline;
options include:

- `atEnd` to get a new timeline after the last date in the current line passes
- `after` to get a new timeline after a specified date/time
- `never` to only get a new timeline when the app makes one available

# Configuration

Widgets can have static configuration that users cannot customize
or an `IntentConfiguration` which allows users to long press the widget
to reveal customization options.
