---
eleventyNavigation:
  key: UIKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

TODO: Add detail here.
The {% aTargetBlank "https://developer.apple.com/documentation/uikit",
"UIKit" %} framework predates SwiftUI which is now the preferred framework
for building iOS, watchOS, macOS, and tvOS apps.

UIKIt has features not present in SwiftUI.
When those features are needed, UIKit views can be used inside a SwiftUI app.
This is accomplished by wrapping a UIKit view in a struct
that conforms to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/uiviewrepresentable",
"UIViewRepresentable" %} protocol.
This is explained in the
[UIViewRepresentable](#uiviewrepresentable) section below.

## UIView

All views in the UIKit framework are classes that inherit from the
{% aTargetBlank "https://developer.apple.com/documentation/uikit/uiview",
"UIView" %} class, either directly or indirectly.
For example, the {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uibutton", "UIButton" %}
class inherits from {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uicontrol", "UIControl" %}
class which inherits from `UIView` class.

UIKit views typically have properties that are set after an instance is created.
The following code creates a {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uilabel", "UILabel" %}
and sets some of its properties.

```swift
let label = UILabel()
label.font = UIFont.systemFont(ofSize: 30)
label.textColor = .red
label.text = "Hello, World!"
```

UIKit views that need to report on changes or user interactions
do so using a "delegate".
This is an object that conforms to a specific protocol
and is assigned to a `UIView` instance.

## Relationship to SwiftUI

The table below identifies the SwiftUI views
that correspond to UIKit concepts:

| UIKit                                         | SwiftUI                        |
| --------------------------------------------- | ------------------------------ |
| `NSAttributedString`                          | `Text` with `AttributedString` |
| `UIActivityIndicatorView`                     | `ProgressView without a value` |
| `UIAlertController` with style `.actionsheet` | `ActionSheet`                  |
| `UIAlertController` with style `.alert`       | `Alert`                        |
| `UIButton`                                    | `Button`                       |
| `UICollectionView`                            | `LazyVGrid and LazyHGrid`      |
| `UIDatePicker`                                | `DatePicker`                   |
| `UIImageView`                                 | `Image`                        |
| `UILabel`                                     | `Text`                         |
| `UINavigationController`                      | `NavigationView`               |
| `UIProgressView`                              | `ProgressView with a value`    |
| `UISegmentedControl`                          | `Picker`                       |
| `UISlider`                                    | `Slider`                       |
| `UIStackView` with horizontal axis            | `HStack`                       |
| `UIStackView` with vertical axis              | `VStack`                       |
| `UIStepper`                                   | `Stepper`                      |
| `UISwitch`                                    | `Toggle`                       |
| `UITableView`                                 | `List`                         |
| `UITextField`                                 | `TextField`                    |
| `UITextField` with `isSecureTextEntry` true   | `SecureField`                  |
| `UITextView`                                  | `TextEditor` for plain strings |

It is possible to use UIKit components in a SwiftUI app
by wrapping them in a struct that conforms to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/uiviewrepresentable",
"UIViewRepresentable" %} protocol.
However, there is enough functionality in SwiftUI
that this is typically not necessary.

## UIViewRepresentable

There is no need to wrap a `UILabel` in a `UIViewRepresentable`
because SwiftUI provides the `Text` view which can be used instead.
However, doing so provides a simple example of using `UIViewRepresentable`
and this is demonstrated in the following code.

```swift
import SwiftUI
import UIKit

struct MyLabel: UIViewRepresentable {
    let text: String

    // Creates a kind of UIView and configures its initial state.
    func makeUIView(context: Context) -> UILabel {
        let label = UILabel()
        label.font = UIFont.systemFont(ofSize: 30)
        label.textColor = .red
        // There is no need to set the `text` property here
        // because `updateUIView` will do it.

        // This sets the priority with which a view resists
        // being made larger than its intrinsic size.
        // The UILabel will be the minimum size required to fit the text.
        label.setContentHuggingPriority(.required, for: .horizontal)
        label.setContentHuggingPriority(.required, for: .vertical)

        return label
    }

    // Updates the state of the UIView created in `makeUIView`
    // with new data provided by a SwiftUI View.
    // This called after makeUIView and again every time a parameter changes.
    func updateUIView(_ uiView: UILabel, context: Context) {
        uiView.text = text
    }
}
```

The struct above can be used in SwiftUI as follows:

```swift
struct ContentView: View {
    @State private var labelText = "First"

    var body: some View {
        VStack {
            Button("Toggle Label Text") {
                labelText = labelText == "First" ? "Second" : "First"
            }
            MyLabel(text: labelText)
        }
    }
}
```

In example above, SwiftUI sends data to the `UIViewRepresentable`
using the `text` parameter, but no data comes back.

Let's look at an example where data also needs to come back.
There are many approaches for doing this.
One approach is to pass a binding to the `UIViewRepresentable` subtype
that it can update.
Changing the binding will cause the view that owns the binding to update.

The following code demonstrates this approach.

```swift
TODO: Add MapView example where a starting location is passed in
TODO: and the current map center is passed back when the user pans the map.
```

Another approach for updating a view based on changes in a
`UIViewRepresentable` subtype is to store the changeable data in a view model
The following code demonstrates this approach.
It renders a map using the UIKit `MKMapView` class.
Initially the map is centered on Apple Park.
Tapping the "Current Location" button uses Core Location
to get the current device location and centers the map there.
The user can drag the map to a new location.
The latitude and longitude of the map center is displayed above the map
and updates during dragging.
The user can re-center the map on the device location
by tapping the "Reset" button.

```swift
// ContentView.swift
import CoreLocation
import CoreLocationUI
import MapKit
import SwiftUI

struct ContentView: View {
    @StateObject private var locationManager = LocationManager.shared

    var body: some View {
        VStack {
            if locationManager.userLocation == nil {
                // Tap this to get the device location and pan the map to it.
                LocationButton {
                    locationManager.requestLocation()
                }
                .foregroundColor(.white) // defaults to black
            } else {
                // Tap this to reset the map to the device location.
                Button("Reset") {
                    locationManager.panToDeviceLocation()
                }
                .buttonStyle(.bordered)
            }

            // This updates if the user drags the map.
            if let c = locationManager.mapCenter {
                Text("Lat: \(c.latitude), Lng: \(c.longitude)")
            }

            MapView(initialCenter: locationManager.initialCenter)

            Spacer()
        }
        .padding()
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
```

```swift
// LocationManager.swift
import CoreLocation
import SwiftUI

private let appleParkLatitude = 37.334_900
private let appleParkLongitude = -122.009_020

class LocationManager: NSObject, ObservableObject, CLLocationManagerDelegate {
    @Published var error: Error?

    // This is the requested map center.
    @Published var initialCenter = CLLocationCoordinate2D(
        latitude: appleParkLatitude,
        longitude: appleParkLongitude
    )

    // This is the current map center which differs
    // from initialCenter if the user drags the map.
    @Published var mapCenter: CLLocationCoordinate2D?

    // This is the device location.
    // It is not updated if the device moves.
    @Published var userLocation: CLLocationCoordinate2D?

    let manager = CLLocationManager()

    static var shared = LocationManager()

    override private init() {
        super.init()
        manager.delegate = self
    }

    // This is called when the device location is determined.
    // An attempt to determine the location is triggered by
    // the `requestLocation` method below.
    func locationManager(
        _ manager: CLLocationManager,
        didUpdateLocations locations: [CLLocation]
    ) {
        userLocation = locations.first?.coordinate
        initialCenter = userLocation!
    }

    // This is called if there is an error determining the device location.
    func locationManager(
        _ manager: CLLocationManager,
        didFailWithError error: Error
    ) {
        print("LocationManager error:")
        self.error = error
    }

    // This sets the map center to the device location.
    func panToDeviceLocation() {
        guard let userLocation else { return }

        // Hack alert!
        // If the initial map center is already at the user location ...
        if initialCenter == userLocation {
            // Change it slightly to try `MapView` to re-render.
            initialCenter.longitude += 0.0000001
        } else {
            // Go to the user location.
            initialCenter = userLocation
        }
    }

    // This is called by `ContentView`.
    func requestLocation() {
        manager.requestLocation()
    }
}
```

```swift
// MapView.swift
import CoreLocation
import SwiftUI

private let appleParkLatitude = 37.334_900
private let appleParkLongitude = -122.009_020

class LocationManager: NSObject, ObservableObject, CLLocationManagerDelegate {
    @Published var error: Error?

    // This is the requested map center.
    @Published var initialCenter = CLLocationCoordinate2D(
        latitude: appleParkLatitude,
        longitude: appleParkLongitude
    )

    // This is the current map center which differs
    // from initialCenter if the user drags the map.
    @Published var mapCenter: CLLocationCoordinate2D?

    // This is the device location.
    // It is not updated if the device moves.
    @Published var userLocation: CLLocationCoordinate2D?

    let manager = CLLocationManager()

    static var shared = LocationManager()

    override private init() {
        super.init()
        manager.delegate = self
    }

    // This is called when the device location is determined.
    // An attempt to determine the location is triggered by
    // the `requestLocation` method below.
    func locationManager(
        _ manager: CLLocationManager,
        didUpdateLocations locations: [CLLocation]
    ) {
        userLocation = locations.first?.coordinate
        initialCenter = userLocation!
    }

    // This is called if there is an error determining the device location.
    func locationManager(
        _ manager: CLLocationManager,
        didFailWithError error: Error
    ) {
        print("LocationManager error:")
        self.error = error
    }

    // This sets the map center to the device location.
    func panToDeviceLocation() {
        guard let userLocation else { return }

        // Hack alert!
        // If the initial map center is already at the user location ...
        if initialCenter == userLocation {
            // Change it slightly to try `MapView` to re-render.
            initialCenter.longitude += 0.0000001
        } else {
            // Go to the user location.
            initialCenter = userLocation
        }
    }

    // This is called by `ContentView`.
    func requestLocation() {
        manager.requestLocation()
    }
}
```

```swift
// CLLocationCoordinate2DExtension.swift
import MapKit

extension CLLocationCoordinate2D: Equatable {
    // This is need in order to compare instances of this type.
    public static func == (
        lhs: CLLocationCoordinate2D,
        rhs: CLLocationCoordinate2D
    ) -> Bool {
        lhs.latitude == rhs.latitude && lhs.longitude == rhs.longitude
    }
}
```
