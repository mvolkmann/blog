---
eleventyNavigation:
  key: WeatherKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

See the WWDC 2022 video {% aTargetBlank
"https://developer.apple.com/videos/play/wwdc2022/10003/", "Meet WeatherKit" %}.

## Setup

1. Browse {% aTargetBlank "https://developer.apple.com",
   "developer.apple.com" %}.
1. Click "Account" and sign in.
1. Click "Certificates, Identifiers & Profiles".
1. In the left nav, click "Identifiers".
1. Click the "+" after the heading "Identifiers".
1. Select the "App IDs" radio button.
1. Click the "Continue" button.
1. Select "App".
1. Click the "Continue" button.
1. Enter an app description.
1. Paste the app bundle ID.
1. Under "Capabilities", check the checkbox for WeatherKit.
1. Click the "Continue" button.
1. Click the "App Services" tab.
1. Check the checkbox for WeatherKit.
1. Click the "Continue" button.
1. Click the "Register" button.

1. In Xcode, click the top entry in the Navigator.
1. For each target that will use WeatherKit.

   1. Select the target.
   1. Click the "Signing & Capabilities" tab.
   1. Click the "+" in the upper-left.
   1. Verify that the correct Team is selected which is listed in
      the upper-right corner of the developer.apple.com web page.
   1. Find WeatherKit and double-click it.

It may be necessary to wait around 30 minutes
for the WeatherKit service to be enabled for your app.

## Sample Code

See the demo project {% aTargetBlank
"https://github.com/mvolkmann/WeatherKitDemo", "WeatherKitDemo" %} in GitHub.

Here is some of the code from that project:

```swift
import SwiftUI
import WeatherKit

struct ContentView: View {
    @StateObject private var locationManager = LocationManager()

    let weatherService = WeatherService.shared
    @State private var weather: Weather?

    private var temperature: String {
        guard let weather else { return "" }
        let current = weather.currentWeather
        return current.wind.formatted()
    }

    private var wind: String {
        guard let weather else { return "" }
        let current = weather.currentWeather
        return current.wind.formatted()
    }

    var body: some View {
        VStack {
            Text("WeatherKitDemo").font(.headline)
            Text("Temperature: \(temperature)")
            Text("Wind: \(wind)")
        }
        .padding()
        .task(id: locationManager.currentLocation) {
            do {
                if let location = locationManager.currentLocation {
                    weather = try await weatherService.weather(for: location)
                }
            } catch {
                print("ContentView.body: error =", error)
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
```

## Limitations

Weather data cannot be retrieved in a preview or in the Simulator.
A real device must be used.
