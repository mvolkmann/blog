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

## Resources

- {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2022/10035/",
  "What's new in MapKit" %} from WWDC 2022
- {% aTargetBlank
  "https://holyswift.app/new-mapkit-configurations-with-swiftui/",
  "New MapKit Configurations with SwiftUI" %}

## Example App

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

## Directions

Driving and walking directions are available in most cities.
But as of 2022, transit directions are only available in around 80 cities.
For a list, see {% aTargetBlank
"https://www.apple.com/ios/feature-availability/#maps-look-around",
"iOS and iPadOS 16 Feature Availability" %}.
Click the "Transit" link in the "Maps" section.
Transit directions are not currently available in London or Paris.
Actually I've never been able to get transit directions
from the `MKDirections` `calculate` method in any city!

The list also includes many cities outside the U.S.
such as Berlin, Germany, London, United Kingdom, and Venice, Italy.

The {% aTargetBlank
"https://developer.apple.com/documentation/mapkit/mklocalsearch",
"MKLocalSearch" %} class can be used to search for places that match given text
and markers for each match can be added to the map.

## Look Around

"Look Around" capabilities, introduced in iOS16,
can be added to display street level views.
See https://developer.apple.com/videos/play/wwdc2022/10035/.

Countries and cities for which Look Around support has been added
are listed at {% aTargetBlank
"https://www.apple.com/ios/feature-availability/#maps-look-around",
"iOS and iPadOS 16 Feature Availability" %}.
Click the "Look Around" link in the "Maps" section.

The list includes the following U.S. cities:

- Atlanta, Georgia
- Boston, Massachusetts
- Chicago, Illinois
- Denver, Colorado
- Detroit, Michigan
- Dublin, Ireland
- Houston, Texas
- Las Vegas, Nevada
- Los Angeles, California
- Miami, Florida
- New York, New York
- Philadelphia, Pennsylvania
- Phoenix, Arizona
- Portland, Oregon
- San Diego, California
- San Francisco, California
- Santa Cruz, California
- Seattle, Washington
- Washington, D.C.

When using the Apple Maps app,
if Look Around is available for the current map location then
a button containing a binocular icon will appear in the lower-left.
Tap the button to open a Look Around view.
This works in iOS Maps app, but doesn't not seem to work in macOS Maps app.

## Map vs MKMapView

There are two options for displaying maps.
UIKit provides {% aTargetBlank
"https://developer.apple.com/documentation/mapkit/mkmapview", "MKMapView" %}.
In iOS 14 MapKit added the SwiftUI view {% aTargetBlank
"https://developer.apple.com/documentation/mapkit/map", "Map" %}.
`Map` only supports a subset of the features available in `MKMapView`.
For this reason in SwiftUI apps it is often desirable to define a struct that
conforms to `UIViewRepresentable` and wraps access to a `MKMapView`.

`MKMapView` supports listening for tap gestures
and getting the corresponding map coordinates.
`Map` is not able to do this.

Both `Map` and `MKMapView` support including annotations
which are single point markers.
`MKMapView` also supports overlays which are graphical elements
like lines, shapes, and images displayed on top of a map.
These are often used to draw routes from one map location to another.
`Map` does not support overlays.

`MKMapView` is able to load custom map tiles for a map background
instead of relying on the default map tiles.
`Map` is not able to do this.

## Relationships

{% aTargetBlank "https://developer.apple.com/documentation/mapkitjs/place",
"MKPlacemark" %} objects provide a description of a map location.
This class inherits from the `CLPlacemark` class.

{% aTargetBlank "https://developer.apple.com/documentation/corelocation/clplacemark",
"CLPlacemark" %} objects also provide a description of a map location.
The `location` property is a `CLLocation`

{% aTargetBlank "https://developer.apple.com/documentation/corelocation/cllocation",
"CLLocation" %} holds latitude, longitude, and course information.
The `coordinate` property has the type `CLLocationCoordinate2D`.

{% aTargetBlank "https://developer.apple.com/documentation/corelocation/cllocationcoordinate2d",
"CLLocationCoordinate2D" %} holds latitude and longitude values.

MKPlacemark -> CLLocation -> CLLocationCoordinate2D

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

## More

TODO: Add many more sections!
