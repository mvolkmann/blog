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

## Showing Current Location

To show the current location of the user on the map,
set `showUserLocation` to `true`.

## Simulator

In the Simulator the location defaults to San Jose.
To change this, select Features ... Location ... Custom Location...
