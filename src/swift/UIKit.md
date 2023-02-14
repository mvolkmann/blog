---
eleventyNavigation:
  key: UIKit
  parent: Swift
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

## Overview

## UIKit Integration

The {% aTargetBlank "https://developer.apple.com/documentation/uikit",
"UIKit" %} framework predates SwiftUI which is now the preferred framework
for building iOS, watchOS, macOS, and tvOS apps.

UIKIt still has features not present in SwiftUI.
When those features are needed, UIKit views can be used inside a SwiftUI app.
This is accomplished by wrapping a UIKit view in a struct
that conforms to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/uiviewrepresentable",
"UIViewRepresentable" %} protocol.
This is explained in the
[UIViewRepresentable](#uiviewrepresentable) section below.

## Type Hierarchy

The hierarchy of a subset of the types defined by UIKit is shown below:

- NSObject
  - UIColor
  - UIResponder
    - UIView
      - UIControl
        - UIButton
        - UIColorWell
        - UIDatePicker
        - UIPageControl
        - UISegmentedControl
        - UISlider
        - UIStepper
        - UISwitch
        - UITextField
      - UILabel
      - UIWindow
    - UIViewController

## UIView

All views in the UIKit framework are classes that inherit from the
{% aTargetBlank "https://developer.apple.com/documentation/uikit/uiview",
"UIView" %} class, either directly or indirectly.
For example, the {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uibutton", "UIButton" %}
class inherits from {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uicontrol", "UIControl" %}
class which inherits `UIView`.

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

UIKit views that need to report on changes to their state
or user interactions sometimes do so using a "delegate".
This is an object that conforms to a specific protocol
and is assigned to a `UIView` instance.
For example, {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uitextview", "UITextView" %}
has a `delegate` property whose type is the {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uitextviewdelegate",
"UITextViewDelegate" %} protocol.
That protocol defines several methods that are invoked when
the user interacts with the `UITextView` such as `textViewDidChange`.

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

## UIViewRepresentable

UIKit views can be used in a SwiftUI app by wrapping them
in a struct that conforms to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/uiviewrepresentable",
"UIViewRepresentable" %} protocol.
However, there is enough functionality in SwiftUI
that this is typically not necessary.

For cases when it is needed,j

### Display-only

When a SwiftUI app needs to use a UIKit view for display purposes
and no state changes or user interactions need to be captured,
the following approach can be used.

There is no need to wrap a `UILabel` in a `UIViewRepresentable`
because SwiftUI provides the `Text` view which can be used instead.
However, doing so provides a simple example of using `UIViewRepresentable`.

<img alt="UIViewRepresentable with display-only" style="width: 40%"
  src="/blog/assets/SwiftUI-UIViewRepresentable-DisplayOnly.png?v={{pkg.version}}"
  title="UIViewRepresentable with display-only">

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
    // This called after makeUIView and again every time
    // a parameter changes (ex. text).
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

### Using a Binding

There are many approaches for getting data back
from a `UIViewRepresentable` subtype.
One approach is to pass a binding to the `UIViewRepresentable` subtype
that it can update.
Changing the binding will cause the view that owns the binding to update.
The following code demonstrates this approach.

<img alt="UIViewRepresentable with Binding" style="width: 40%"
  src="/blog/assets/SwiftUI-UIViewRepresentable-Binding.png?v={{pkg.version}}"
  title="UIViewRepresentable with Binding">

```swift
// ContentView.swift
import SwiftUI

struct ContentView: View {
    @State private var isOn = false

    var body: some View {
        VStack {
            MySwitch(isOn: $isOn)
            Text(isOn ? "ON" : "OFF")
        }
        .padding()
    }
}
```

```swift
// MySwitch.swift
import SwiftUI
import UIKit

// Because SwiftUI provides the `Toggle` view, we don't need to
// to wrap a UIKit UISwitch view in a UIViewRepresentable.
// This just demonstrates how a binding can be used to
// share data from a `UIViewRepresentable` subtype
// with a SwiftUI view that uses it.
struct MySwitch: UIViewRepresentable {
    typealias UIViewType = UISwitch

    @Binding var isOn: Bool

    init(isOn: Binding<Bool>) {
        // The underscore here indicates that we are setting
        // the wrapped value of the binding property above.
        _isOn = isOn
    }

    // This method is required to conform to UIViewRepresentable,
    func makeUIView(context: Context) -> UIViewType {
        let view = UISwitch()

        // In order to listen for changes in the value of the UISwitch,
        // we need to register a listener that is an object
        // with a method that is accessible to Objective-C.
        // We can use a Coordinator object for that.
        view.addTarget(
            context.coordinator,
            action: #selector(context.coordinator.onValueChanged(_:)),
            for: .valueChanged
        )

        return view
    }

    func makeCoordinator() -> Coordinator {
        // Passing `self` to the `Coordinator`
        // allows it to access the `isOn` binding.
        Coordinator(self)
    }

    // This method is required to conform to UIViewRepresentable,
    // but it doesn't need to do anything.
    func updateUIView(_ uiView: UISwitch, context: Context) {
        // do nothing
    }

    class Coordinator: NSObject {
        private var parent: MySwitch

        init(_ parent: MySwitch) {
            self.parent = parent
        }

        @objc
        func onValueChanged(_ view: UISwitch) {
            parent.isOn = view.isOn
        }
    }
}
```

The following code provides a similar example where `UITextField`
is wrapped in a `UIViewRepresentable`.
One feature this has that is missing from the SwiftUI `TextField` view
is the ability to include a clear button on the trailing end
that is an "X" in a circle. Tapping this clears the value.
An easier way to add a clear button to `TextField` instances is
described in the [SwiftUI TextField](/blog/swift/SwiftUI/#textfield) section.

```swift
struct MyUITextField: UIViewRepresentable {
    var placeholder = ""
    @Binding var text: String

    func makeCoordinator() -> Coordinator {
        Coordinator(text: $text)
    }

    func makeUIView(context: Context) -> UITextField {
        let textField = UITextField()

        textField.borderStyle = .roundedRect
        textField.clearButtonMode = .whileEditing
        textField.delegate = context.coordinator
        textField.placeholder = placeholder

        return textField
    }

    func updateUIView(_ uiView: UITextField, context: Context) {
        uiView.text = text
    }

    class Coordinator: NSObject, UITextFieldDelegate {
        @Binding var text: String

        init(text: Binding<String>) {
            self._text = text
        }

        func textFieldDidChangeSelection(_ textField: UITextField) {
            text = textField.text ?? ""
        }
    }
}
```

To use this inside a SwiftUI view, add code like the following:

```swift
@State private var firstName = "Mark"
...
MyUITextField(placeholder: "First Name", text: $firstName)
    .frame(height: 31)
```

### Using a View Model

Another approach for getting data back from a `UIViewRepresentable` subtype
is to store the changeable data in a view model.
The following code demonstrates this approach.

This code renders a map using the UIKit `MKMapView` class.
Initially the map is centered on Apple Park in Cupertino, California.
Tapping the "Current Location" button uses Core Location
to get the current device location and centers the map there.
The user can drag the map to a new location.
The latitude and longitude of the map center is
displayed above the map and updates during dragging.
The user can center the map on the device location
by tapping the "Reset" button.

<img alt="UIViewRepresentable with ViewModel" style="width: 40%"
  src="/blog/assets/SwiftUI-UIViewRepresentable-ViewModel.png?v={{pkg.version}}"
  title="UIViewRepresentable with ViewModel">

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
import MapKit
import SwiftUI

/**
 For now we have to wrap an MKMapView in a UIViewRepresentable
 in order to use some MapKit features in SwiftUI
 such as displaying satellite images.

 I was getting the error "The following Metal object is being
 destroyed while still required to be alive by the command buffer".
 This thread provided a solution:
 https://developer.apple.com/forums/thread/699119
 I had to edit the current Xcode scheme, click "Run" in the left nav,
 and uncheck the "API Validation" checkbox
 in the Diagnostics ... Metal section.
 */
struct MapView: UIViewRepresentable {
    typealias UIViewType = MKMapView

    let initialCenter: CLLocationCoordinate2D

    // This method is required to conform to UIViewRepresentable.
    func makeUIView(context: Context) -> UIViewType {
        // Other `mapType` options are `.standard` and `.satellite`.
        MKMapView.appearance().mapType = .hybrid

        let mapView = UIViewType()
        mapView.delegate = context.coordinator

        let meters = 750.0
        mapView.region = MKCoordinateRegion(
            center: initialCenter,
            latitudinalMeters: meters,
            longitudinalMeters: meters
        )

        // Add a blue circle over the current user location.
        mapView.showsUserLocation = true

        return mapView
    }

    // This is called initially and again every time
    // ContentView passes a new value for `initialCenter`.
    @MainActor
    func updateUIView(_ mapView: UIViewType, context: Context) {
        mapView.centerCoordinate = initialCenter
    }

    func makeCoordinator() -> Coordinator {
        // Use this if `Coordinator` needs access to its parent.
        // Coordinator(self)
        Coordinator()
    }

    class Coordinator: NSObject, MKMapViewDelegate {
        // Use this if access to the parent is needed.
        /*
         private var parent: MapView

         init(_ parent: MapView) {
             self.parent = parent
         }
         */

        // This is called when the user drags the map.
        func mapViewDidChangeVisibleRegion(_ mapView: UIViewType) {
            Task { @MainActor in
                LocationManager.shared.mapCenter = mapView.centerCoordinate
            }
        }
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
