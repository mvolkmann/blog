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
- To enable users to configure the widget, check the checkbox for
  "Include Configuration Intent"
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

## Data

Widgets can get data from network calls.

## Widget Sizes

Four sizes can be supported.
These are represented by the

To specify the sizes a widget supports, call `.supportedFamilies(sizes)`
on the `IntentConfiguration` object returned by the `body` computed property
of the struct that inherits from `Widget`.
The value passed to this (`sizes`) is an array of `WidgetFamily` enum values
that include `.systemSmall`, `.systemMedium`, `.systemLarge`,
and `systemExtraLarge`.

When users install the widget they can select one of the supported sizes.
The selected size can be determined from the following environment value:

```swift
@Environment(\.widgetFamily) var family
```

This value can be used in the `body` computed property
of the struct that inherits from `View` to decide what to render.

## Reload Budget

In order to conserve system resources and battery,
iOS limits the number of times a widget can be reloaded each day.
A typical number of daily refreshes is between 40 and 70
which is approximately every 15 to 60 minutes.

For more detail, see {% aTargetBlank
"https://developer.apple.com/documentation/widgetkit/keeping-a-widget-up-to-date",
"Keeping a Widget Up To Date" %}.

## Widget Title

The title displayed under the widget is the display name of the associated app.
To configure this, clicking the top entry in the Navigator,
select the app target, click the "General" tab,
and enter a title in the "Display Name" input in the "Identity" section.
The widget target also has a "Display Name" value
but it's unclear how that is used.

## Updating Widgets

iOS limits the frequency of widget updates
as described in the "Reload Budget" section above.

Each widget defines a `Provider` struct that includes a `getTimeline` method.
This method creates an array of objects that inherit from `TimelineEntry`.
These provide data to the widget.
The array must contain at least one entry object.

The next step is to create a `Timeline` object that
holds the array of entry objects and specifies an update policy.
Finally, the completion handler function passed to the `getTimeline` method
must be called, passing it the `Timeline` object.

Here is an example using the `after` policy:

```swift
// A request to update the widget sooner than 15 minutes from now will
// likely not be honored and will instead occur in 15 minutes or later.
let later = now.addingTimeInterval(60) // seconds
let timeline = Timeline(entries: entries, policy: .after(later))
completion(timeline)
```

There are three update policies to choose from: `atEnd`, `after`, and `never`.
Each of these determine when the `getTimeline` method should be called again
which creates a new array of entry objects
and triggers a new round of widget updates.

The `.atEnd` policy triggers a new round of updates after
the widget is updated using the final entry in the array.

The `.after(someDate)` policy triggers a new round of updates after
a given date/time is passed.

The `.never` policy indicates that the associated application will
take responsibility for indicating when the widget will be update.
The app can do this by importing `WidgetKit`
and running code like the following:

```swift
WidgetCenter.shared.getCurrentConfigurations { result in
    guard case .success(let widgets) = result else { return }
    for widget in widgets {
        WidgetCenter.shared.reloadTimelines(ofKind: widget.kind)
    }
}
```

## "Live Activities" Widgets

iOS 16 beta 4 adds support for live activities widgets on the lock screen.
These are supported by the new ActivityKit framework.

From {% aTargetBlank "https://developer.apple.com/news/?id=hi37aek8",
"Apple News and Updates" %},
"Live Activities and ActivityKit won’t be included
in the initial public release of iOS 16.
Later this year, they'll be publicly available in an update."
