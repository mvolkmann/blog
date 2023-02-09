---
eleventyNavigation:
  key: App Reviews
  parent: Swift
layout: topic-layout.njk
---

## Overview

Users can submit app reviews that include 1-5 starts and text.
They can do this by finding the app in the App Store
and tapping "Write a Review".
In addition, an app can ask the user to provide a review.
This post describes how to implement such requests in an app.

Regardless of the number of times the an app requests a review,
the system will only prompt the user up to three times per year.
This is not enforced when running locally during development.

If a user opens the Settings app, selects "App Store", and toggles off
"In-App Ratings & Reviews" then they will never see app review requests.

Apple provides these basic guidelines:

- Make the request at a time that does not interrupt
  a task the user is attempting to complete.
- Avoid making a request immediately after the app launches.
- Avoid making a request as a result of a user action.

Presumably this leaves making the request based on a timer.

For example, in the "Feature Weather" app I could only request a review
after the user has visited all four screens and
looked at weather in a city other than their own.
After that I could only ask for another review
after a new version of the app is installed
and all these same steps are performed again.

For testing the app review logic, you could make the triggering activity
be opening an "info" sheet a given number of times.

## Resources

- Apple Developer page {% aTargetBlank
  "https://developer.apple.com/documentation/storekit/requesting_app_store_reviews",
  "Requesting App Store reviews" %}
- Stewart Lynch YouTube video {% aTargetBlank
  "https://www.youtube.com/watch?v=fLR7E2579Dg", "App Review Request iOS 16" %}

## AppReview Type

The following code is derived from the excellent Stewart Lynch YouTube video
{% aTargetBlank "https://www.youtube.com/watch?v=fLR7E2579Dg",
"App Review Request iOS 16" %}.

```swift
import SwiftUI

struct AppReview {
    // We only want to prompt for an app review if the user
    // has visited all the screens in the app multiple times.
    // That way they should know enough about the app
    // to leave an informed review.
    @AppStorage("chartVisits") private var chartVisits = 0
    @AppStorage("currentVisits") private var currentVisits = 0
    @AppStorage("forecastVisits") private var forecastVisits = 0
    @AppStorage("heatMapVisits") private var heatMapVisits = 0
    @AppStorage("settingsVisits") private var settingsVisits = 0

    // Once we have asked the user to leave a review,
    // we only want to ask again if they have installed a new version.
    @AppStorage("lastReviewedAppVersion") var lastReviewedAppVersion = ""

    // Singleton
    static let shared = AppReview()
    private init() {}

    private let triggerTarget = 3

    private var appVersion: String {
        let infoDict = Bundle.main.infoDictionary!
        let key = "CFBundleShortVersionString"
        return infoDict[key] as? String ?? ""
    }

    private func clearVisits() {
        chartVisits = 0
        currentVisits = 0
        forecastVisits = 0
        heatMapVisits = 0
        settingsVisits = 0
    }

    var haveNewVersion: Bool {
        appVersion != lastReviewedAppVersion
    }

    // For debugging
    func printVisits() {
        print("currentVisits =", currentVisits)
        print("forecastVisits =", forecastVisits)
        print("chartVisits =", chartVisits)
        print("heatMapVisits =", heatMapVisits)
        print("settingsVisits =", settingsVisits)
    }

    func reviewURL(appId: Int) -> URL? {
        let string = "https://apps.apple.com/app/id\(appId)?action=write-review"
        return URL(string: string)
    }

    var shouldRequest: Bool {
        guard haveNewVersion else {
            clearVisits()
            return false
        }

        // printVisits()
        let target = 3
        let usedEnough =
            chartVisits >= target &&
            currentVisits >= target &&
            forecastVisits >= target &&
            heatMapVisits >= target &&
            settingsVisits >= 1
        // print("usedEnough =", usedEnough)
        if usedEnough {
            lastReviewedAppVersion = appVersion
            clearVisits()
        }
        return usedEnough
    }
}
```

## Automatic Prompt

After the user has used a sufficient number of app features,
it's a good idea to wait a few seconds and
then display a dialog that prompts for a review.
The following code does this:

```swift
private func appReview() {
    guard AppReview.shared.shouldRequest else { return }

    Task {
        // Wait 3 seconds before requesting an app review.
        try await Task.sleep(
            until: .now + .seconds(3),
            clock: .suspending
        )
        requestReview()
    }
}
```

Call the `appReview` function after every visit to an app feature
that is being tracked. For example:

```swift
@AppStorage("chartVisits") private var chartVisits = 0
@AppStorage("currentVisits") private var currentVisits = 0
@AppStorage("forecastVisits") private var forecastVisits = 0
@AppStorage("heatMapVisits") private var heatMapVisits = 0
@AppStorage("settingsVisits") private var settingsVisits = 0

.onChange(of: selectedTab) { _ in
    switch selectedTab {
    case "chart": chartVisits += 1
    case "current": currentVisits += 1
    case "forecast": forecastVisits += 1
    case "heatmap": heatMapVisits += 1
    // settingsVisits is incremented when a gear icon is tapped.
    default: break
    }

    appReview()
}
```

## Review Button

When the user is asked to leave a review they can tap "Not Now" to opt out.
Later they may want to change their review or leave a review for the first time.
It's a good idea to provide a button they can tap to do so
rather than requiring them to find the app in the App Store
and leave a review there.

The following code adds a button for leaving a review
only if the user has been asked to leave a review
based on having used a sufficient number of app features.

```swift
// This is the 10-digit app id seen in the App Store Connect URL for the app.
let appId = 1234567890

let appReview = AppReview.shared
if !appReview.haveNewVersion {
    // This only works on a real device, not in the Simulator.
    if let url = appReview.reviewURL(appId: appId) {
        Link(destination: url) {
            Text("Write a Review")
        }
        .buttonStyle(.borderedProminent)
    }
}
```
