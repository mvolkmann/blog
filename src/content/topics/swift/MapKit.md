---
eleventyNavigation:
  key: MapKit
  parent: Swift
layout: topic-layout.njk
---

## TODO

Add content from [MapKit Beginner](<https://www.appcoda.com/mapkit-polyline-polygon/?v=1.1.1>)

## Overview

[MapKit](<https://developer.apple.com/documentation/mapkit/?v=1.1.1>)
enables embedding maps in apps.
Users can pan and zoom to adjust their view of the map.
The maps can contain markers that can be tappable
to invoke app-specific actions.

## Resources

- [What](<https://developer.apple.com/videos/play/wwdc2022/10035/?v=1.1.1>) from WWDC 2022
- [New MapKit Configurations with SwiftUI](<https://holyswift.app/new-mapkit-configurations-with-swiftui/?v=1.1.1>)
- [Meet MapKit for SwiftUI](<https://developer.apple.com/videos/play/wwdc2023/10043/?v=1.1.1>) from WWDC 2023

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

## Example App

See the demo apps at [MapKitForSwiftUI](<https://github.com/mvolkmann/MapKitForSwiftUI?v=1.1.1>)
(iOS 17+) and [swiftui-mapkit](<https://github.com/mvolkmann/swiftui-mapkit?v=1.1.1>).

<img alt="MapKit iOS 17+" style="width: 32%"
  src="/blog/assets/mapkit-initial.jpg?v=1.1.1"
  title="MapKit Initial">
<img alt="MapKit Initial" style="width: 32%"
  src="/blog/assets/mapkit-initial.jpg?v=1.1.1"
  title="MapKit Initial">
<img alt="MapKit Selected Place" style="width: 32%"
  src="/blog/assets/mapkit-selected-place.jpg?v=1.1.1"
  title="MapKit Selected Place">
<img alt="MapKit Browse Place" style="width: 32%"
  src="/blog/assets/mapkit-browse-place.jpg?v=1.1.1"
  title="MapKit Browse Place">

## Basic Map Display

To enable use of MapKit in a SwiftUI app:

- Open the Project Navigator by selecting
  the first navigator tap or pressing cmd-1.
- Select the top entry.
- Select the main target.
- Select the "Signing & Capabilities" tab.
- Click the "+ Capability" button in the upper-left.
- Double-click "Maps".
- To access route data, check all the checkboxes for the desired route types
  such as "Bike", "Bus", "Car", "Pedestrian", and "Train".

The following class defines a view model that is
used to get the current location of the user.

```swift
import CoreLocation
import SwiftUI

class LocationManager: NSObject, ObservableObject, CLLocationManagerDelegate {
    @Published var error: Error?
    @Published var location: CLLocationCoordinate2D?
    let manager = CLLocationManager()

    override init() {
        super.init()
        manager.delegate = self
    }

    func requestLocation() {
        manager.requestLocation()
    }

    func locationManager(
        _ manager: CLLocationManager,
        didUpdateLocations locations: [CLLocation]
    ) {
        location = locations.first?.coordinate
    }

    func locationManager(
        _ manager: CLLocationManager,
        didFailWithError error: Error
    ) {
        self.error = error
    }
}
```

The [CLLocationCoordinate2D](<https://developer.apple.com/documentation/corelocation/cllocationcoordinate2d?v=1.1.1>) struct from the Core Location framework
does not conform to the `Equatable` protocol.
We need it to do that in order to listen for changes to a location
using the `onChange` view modifier.
The following extension fixes this.

```swift
extension CLLocationCoordinate2D: Equatable {
    public static func == (
        lhs: CLLocationCoordinate2D,
        rhs: CLLocationCoordinate2D
    ) -> Bool {
        lhs.latitude == rhs.latitude && lhs.longitude == rhs.longitude
    }
}
```

The following view displays a [Map](<https://developer.apple.com/documentation/mapkit/map?v=1.1.1>)
that begins by showing Apple Park.
The user can tap "Current Location" button
to obtain their latitude and longitude.
The map then updates to centered on that location.
From there the user can pan and zoom the map.
To reset the map to be centered on their current location,
the user can tap the "Return" button.

```swift
struct ContentView: View {
    private static let appleParkLatitude = 37.334_900
    private static let appleParkLongitude = -122.009_020
    private static let meters = 750.0

    @State private var region = MKCoordinateRegion(
        center: CLLocationCoordinate2D(
            latitude: Self.appleParkLatitude,
            longitude: Self.appleParkLongitude
        ),
        latitudinalMeters: Self.meters,
        longitudinalMeters: Self.meters
    )

    @StateObject var locationManager = LocationManager()

    init() {
        // This has no effect.
        MKMapView.appearance().mapType = .hybrid
    }

    func panToCurrentLocation() {
        guard let location = locationManager.location else { return }
        Task { @MainActor in
            region.center = location
        }
    }

    var body: some View {
        VStack {
            HStack {
                // To change the text displayed in a `LocationButton`
                // pass it one of the following enum values:
                // `.currentLocation` - "Current Location" (default)
                // `.sendCurrentLocation` - "Send Current Location"
                // `.sendMyCurrentLocation` - "Send My Current Location"
                // `.shareCurrentLocation` - "Share Current Location"
                // `.shareMyCurrentLocation` - "Share My Current Location"
                LocationButton {
                    locationManager.requestLocation()
                }
                .foregroundColor(.white) // defaults to black
                Spacer()
                if locationManager.location != nil {
                    Button("Return") {
                        panToCurrentLocation()
                    }
                    .buttonStyle(.bordered)
                }
            }

            // Display the current location.
            // This updates if the user pans the map.
            let center = region.center
            Text("Lat: \(center.latitude), Lng: \(center.longitude)")

            // A binding must be passed so it can be
            // modified if the user pans or zooms the map.
            Map(coordinateRegion: $region, showsUserLocation: true)

            Spacer()
        }
        .padding()
        .onChange(of: locationManager.location) { _ in
            panToCurrentLocation()
        }
    }
}
```

The `Map` view currently lacks many features found in [MKMapView](<https://developer.apple.com/documentation/mapkit/mkmapview?v=1.1.1>).
One example is the ability to display a satellite view.
Until `Map` becomes more full-featured,
it is useful to wrap `MKMapView` in `UIViewRepresentable`
to enable it to be used with SwiftUI in place of `Map`.
Code to do this can be found in my
[UIKit blog page](/blog/swift/uikit/#using-a-view-model).

## Distance vs. Angle

Some MapKit methods require passing distances in meters and others require
passing distances in latitude and longitude deltas which are angles.

A latitude value specifies the
north (positive) or south (negative) angle from the equator
which circles the Earth midway between the north and south poles.
The latitude is zero degrees at the equator,
90 degrees at the north pole, and -90 degrees at the south pole.
Positive values are sometimes followed by "N" for North.
Negative values are sometimes written without the minus sign
and are following by "S" for South.

A longitude value specifies the east (positive) or west (negative) angle
from the Prime Meridian (Greenwich) which runs from the north to south pole,
passing through the Royal Observatory in Greenwich, England (near London).
The longitude is zero degrees at the Prime Meridian,
-180 degrees moving halfway around the Earth to the west,
and 180 degrees moving halfway around the Earth to the east.
Positive values are sometimes followed by "E" for East.
Negative values are sometimes written without the minus sign
and are following by "W" for West.

We can perform conversions between latitude/longitude angles and distances
by assuming that the Earth is a perfect sphere.
The conversions are approximate because the Earth bulges at the equator
and so is not a perfect sphere.

The average radius of the Earth is 6,371 kilometers or 6,371,000 meters.
The circumference of the Earth at the equator is
approximately 40,075.017 kilometers.
The circumference of the Earth through the poles is
approximately 40,007.863 kilometers.

The distance spanned by a latitude angle of one degree is 1,852 meters
regardless of the longitude.

The distance spanned by a longitude angle varies based on the latitude.
One degree of longitude spans a distance of approximately
111.319 kilometers at the equator but spans 0 kilometers at the poles.

The formula to compute the distance of one degree of longitude
at a given latitude is the following where
d is the distance, r is the sphere radius, and l is the latitude in degrees:

```
d = cos(lπ / 180) * r / 360
```

The values for d and r will have the same unit.
For example, if r is given in meters then d will be in meters.

## Map Points

MapKit provides the [MKMapPoint](<https://developer.apple.com/documentation/mapkit/mkmappoint?v=1.1.1>) struct which represents an Earth coordinate
that has been projected onto a two-dimensional map.

To create an `MKMapPoint` for a given `CLLocationCoordinate2D` object
which holds latitude and longitude values:

```
let mapPoint = MKMapPoint(coordinate)
```

`MKMapPoint` objects have the following properties:

- `x`: a `Double` that is the x-coordinate on a 2D map
- `y`: a `Double` that is the y-coordinate on a 2D map
- `coordinate`: a `CLLocationCoordinate2D` that holds
  the Earth latitude and longitude values

To calculate the distance between two `MKMapPoint` objects:

```
let distance = mapPoint1.distance(to: mapPoint2)
```

To calculate the distance between to `CLLocationCoordinate2D` objects:

```
let distance = MKMapPoint(coordinate1).distance(to: MKMapPoint(coordinate2))
```

## Annotations

Annotations are markers that are rendered on a map.
The easiest way to add annotations is to use the provided [MKMarkerAnnotationView](<https://developer.apple.com/documentation/mapkit/mkmarkerannotationview?v=1.1.1>) which displays a pin in a bubble.

Custom annotations can be created using [MKAnnotationView](<https://developer.apple.com/documentation/mapkit/mkannotationview?v=1.1.1>).
See the Swiftful Thinking YouTube video [Custom Map Annotation Pins for SwiftUI MapKit Map](<https://www.youtube.com/watch?v=javFZbCYGfc&v=1.1.1>).

## Overlays

Overlays draw on top of a map.
A common example is drawing route lines that
indicate how to travel from one location to another.

The provided subclasses of [MKOverlayRenderer](<https://developer.apple.com/documentation/mapkit/mkoverlayrenderer?v=1.1.1>) include:

- [MKCircleRenderer](<https://developer.apple.com/documentation/mapkit/mkcirclerenderer?v=1.1.1>) fills and strokes a circle
- [MKPolygonRenderer](<https://developer.apple.com/documentation/mapkit/mkpolygonrenderer?v=1.1.1>) fills and strokes a polygon
- [MKPolylineRenderer](<https://developer.apple.com/documentation/mapkit/mkpolylinerenderer?v=1.1.1>) is like MKPolygonRender but doesn't fill
  because the shape isn't necessarily closed
- [MKOverlayPathRenderer](<https://developer.apple.com/documentation/mapkit/mkoverlaypathrenderer?v=1.1.1>) renders a shape defined by a CGPath
- [MKTileOverlayRenderer](<https://developer.apple.com/documentation/mapkit/mktileoverlayrenderer?v=1.1.1>) renders a bitmap image over a map tile
- [MKGradientPolygonRenderer](<https://developer.apple.com/documentation/mapkit/mkgradientpolygonrenderer?v=1.1.1>) is like MKPolylineRenderer but uses gradient color
- [MKMultiPolygonRenderer](<https://developer.apple.com/documentation/mapkit/mkmultipolygonrenderer?v=1.1.1>) renders multiple polygons
- [MKMultiPolylineRenderer](<https://developer.apple.com/documentation/mapkit/mkmultipolylinerenderer?v=1.1.1>) renders multiple polylines

## Directions

Driving and walking directions are available in most cities.
But as of 2022, transit directions are only available in around 80 cities.
For a list, see [iOS and iPadOS 16 Feature Availability](<https://www.apple.com/ios/feature-availability/#maps-look-around?v=1.1.1>).
Click the "Transit" link in the "Maps" section.
Transit directions are not currently available in London or Paris.
Actually I've never been able to get transit directions
from the `MKDirections` `calculate` method in any city!

The list also includes many cities outside the U.S.
such as Berlin, Germany, London, United Kingdom, and Venice, Italy.

The [MKLocalSearch](<https://developer.apple.com/documentation/mapkit/mklocalsearch?v=1.1.1>) class can be used to search for places that match given text
and markers for each match can be added to the map.

## Look Around

"Look Around" capabilities, introduced in iOS 16,
can be added to display street level views.
See https://developer.apple.com/videos/play/wwdc2022/10035/.

Countries and cities for which Look Around support has been added
are listed at [iOS and iPadOS 16 Feature Availability](<https://www.apple.com/ios/feature-availability/#maps-look-around?v=1.1.1>).
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
UIKit provides [MKMapView](<https://developer.apple.com/documentation/mapkit/mkmapview?v=1.1.1>).
In iOS 14 MapKit added the SwiftUI view [Map](<https://developer.apple.com/documentation/mapkit/map?v=1.1.1>).
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

[MKPlacemark](<https://developer.apple.com/documentation/mapkitjs/place?v=1.1.1>) objects provide a description of a map location.
This class inherits from the `CLPlacemark` class.

[CLPlacemark](<https://developer.apple.com/documentation/corelocation/clplacemark?v=1.1.1>) objects also provide a description of a map location.
The `location` property is a `CLLocation`

[CLLocation](<https://developer.apple.com/documentation/corelocation/cllocation?v=1.1.1>) holds latitude, longitude, and course information.
The `coordinate` property has the type `CLLocationCoordinate2D`.

[CLLocationCoordinate2D](<https://developer.apple.com/documentation/corelocation/cllocationcoordinate2d?v=1.1.1>) holds latitude and longitude values.

MKPlacemark -> CLLocation -> CLLocationCoordinate2D

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

## Tracking User

To track the user location on a map, set the Info properties
"Privacy - Location When In Use Usage Description" and
"Privacy - Location Always and When in Use Usage Description"
to strings that describe why permission to track location is being requested.

Create a `Map` instance with the following:

```swift
@State private var region = MKCoordinateRegion(
  center: CLLocationCoordinate2D(latitude: startLat, longitude: startLng),
  span: MKCoordinateSpan(latitudeDelta: 0.5, longitudeDelta: 0.5)
)

Map(
    coordinateRegion: $region,
    showsUserLocation: true,
    userTrackingMode: .constant(.follow)
)
.frame(width: someWidth, height: someHeight)
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

## Deploying

The `Info.plist` file in one of my apps needed the following
in order to deploy an archive without warnings:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleDocumentTypes</key>
	<array>
		<dict>
			<key>CFBundleTypeName</key>
			<string>MKDirectionsRequest</string>
			<key>LSHandlerRank</key>
			<string>None</string>
			<key>LSItemContentTypes</key>
			<array>
				<string>com.apple.maps.directionsrequest</string>
			</array>
		</dict>
	</array>
	<key>ITSAppUsesNonExemptEncryption</key>
	<false/>
	<key>MKDirectionsApplicationSupportedModes</key>
	<array>
		<string>MKDirectionsModeBike</string>
		<string>MKDirectionsModeCar</string>
		<string>MKDirectionsModePedestrian</string>
	</array>
</dict>
</plist>
```

## More

TODO: Add many more sections!
