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
