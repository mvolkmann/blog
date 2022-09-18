---
eleventyNavigation:
  key: ActivityKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

ActivityKit enables implementing lock screen widgets that update automatically,
referred to as "live activities".
These can be used to report on activities such as sports scores.

Live activities were supported in the Xcode 14 beta,
but are not supported in Xcode 14.
Support should be added later in 2022.

See the YouTube video {% aTargetBlank
"https://www.youtube.com/watch?v=oloHJn1kj3w",
"Create a Live Activity with ActivityKit & WidgetKit for iOS 16" %}.

## Setup Steps

- Create a new iOS app in Xcode.
  The project name will be the name of the widget displayed to the user,
  so choose a name that will be meaningful to users.
- Select File ... New ... Target...
- Select "Widget Extension".
- Click the "Next" button.
- In "Product Name", enter a name such as simply
  "Widget" when the app with have only one associated widget
  or "Widgets" when the app with have multiple associated widget.
- To enable users to configure the widget, check the checkbox for
  "Include Configuration Intent".
- Click the "Finish" button.
- Click the "Activate" button.

## Implementation Steps

- Edit the file that defines the widget in the widget target.
- Delete the "Provider" struct.
- Delete the "SimpleEntry" struct.
- Delete the "\*\_Previews" struct.
- At the top of the file, add "import ActivityKit".
- In the `body` of the struct that inherits from `Widget`,
  replace the `StaticConfiguration` with `ActivityConfiguration`.
- TODO: Finish this after support is added to Xcode 14.
