---
eleventyNavigation:
  key: MapKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/mapkit/", "MapKit" %}
enables embedding maps in apps.
Users can pan and zoom to adjust their view of the map.
The maps can contain markers that can be tappable
to invoke app-specific actions.

"LookAround" capabilities can be added to display street level views.
See https://developer.apple.com/videos/play/wwdc2022/10035/.

The {% aTargetBlank
"https://developer.apple.com/documentation/mapkit/mklocalsearch",
"MKLocalSearch" %} class can be used to search for places that match given text
and markers for each match can be added to the map.

See the demo app at {% aTargetBlank
"https://github.com/mvolkmann/swiftui-mapkit", "swiftui-mapkit" %}.

<img alt="MapKit Initial" style="width: 32%"
  src="/blog/assets/mapkit-initial.jpg?v={{pkg.version}}"
  title="MapKit Initial">
<img alt="MapKit Selected Place" style="width: 32%"
  src="/blog/assets/mapkit-selected-place.jpg?v={{pkg.version}}"
  title="MapKit Selected Place">
<img alt="MapKit Browse Place" style="width: 32%"
  src="/blog/assets/mapkit-browse-place.jpg?v={{pkg.version}}"
  title="MapKit Browse Place">

TODO: Add many more sections!

## Resources

- {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2022/10035/",
  "What's new in MapKit" %} from WWDC 2022
- {% aTargetBlank
  "https://holyswift.app/new-mapkit-configurations-with-swiftui/",
  "New MapKit Configurations with SwiftUI" %}

## Getting Started

To enable use of MapKit in a project:

- Select the top entry in the file navigator.
- Select the main target.
- Select the "Signing & Capabilities" tab.
- Click the "+ Capability" button in the upper-left.
- Enter "MapKit".
- Double-click that capability to add it to the project.
- Check the checkboxes for the desired routing modes such as
  "Bike", "Car", and "Pedestrian".

## Showing Current Location

To show the current location of the user on the map,
add the `showUserLocation` argument with a value of `true`
to the `Map` initializer call.

```swift
Map(
    ...
     showsUserLocation: true,
     ...
)
```

## Simulator

To simulate a drag event, simply click and drag.

To zoom in, double-click.
To zoom out, hold down the option key and click.
Another option is to hold down the option key to display
two circles that represent finger touch points.
Drag the circles closer together to zoom out
and farther apart to zoom in.

In the Simulator the location defaults to the location of
Apple headquarters in Cupertino, California near San Jose.
To change this, select Features ... Location ... Custom Location...
and enter latitude and longitude values.
