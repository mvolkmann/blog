---
eleventyNavigation:
  key: AppInfo
  parent: Swift
layout: topic-layout.njk
---

## Overview

It is possible to determine a lot of information about an app
from its bundle and from the App Store (for released apps).

## AppInfo struct

The following code gathers information about an app
and makes it available through computed properties.

```swift
import Foundation

struct AppInfo {
    static let infoDict = Bundle.main.infoDictionary!

    var json: [String: Any] = [:]

    private init(json: [String: Any]) {
        // print("json =", json)
        self.json = json
    }

    static func create() async throws -> Self {
        let urlPrefix = "https://itunes.apple.com/lookup?bundleId="
        let identifier = infoDict["CFBundleIdentifier"] as? String ?? ""
        let url = URL(string: "\(urlPrefix)\(identifier)&country=US")
        guard let url else {
            throw "AppStoreService: bad URL \(String(describing: url))"
        }

        // Using the ephemeral configuration avoids caching.
        let session = URLSession(configuration: .ephemeral)
        let (data, _) = try await session.data(from: url)
        guard let json = try JSONSerialization.jsonObject(
            with: data,
            options: [.allowFragments]
        ) as? [String: Any] else {
            throw "AppStoreService: bad JSON"
        }

        guard let results =
            (json["results"] as? [Any])?.first as? [String: Any] else {
            throw "AppStoreService: JSON missing results"
        }

        return Self(json: results)
    }

    private func date(_ key: String) -> Date {
        json[key] as? Date ?? Date()
    }

    private func double(_ key: String) -> Double {
        json[key] as? Double ?? 1.2 // 0
    }

    private func info(_ key: String) -> String {
        Self.infoDict[key] as? String ?? ""
    }

    private func int(_ key: String) -> Int {
        json[key] as? Int ?? 0
    }

    private func string(_ key: String) -> String {
        json[key] as? String ?? ""
    }

    var appId: Int { int("trackId") }
    var appURL: String { string("trackViewUrl") }
    var author: String { string("sellerName") }
    var bundleId: String { string("bundleId") }
    var description: String { string("description") }
    var iconURL: String { string("artworkUrl100") }
    var supportURL: String { string("sellerUrl") }

    var haveLatestVersion: Bool {
        let order = storeVersion.compare(installedVersion, options: .numeric)
        return order != .orderedDescending
    }

    var installedVersion: String { info("CFBundleShortVersionString") }
    var identifier: String { info("CFBundleIdentifier") }
    var minimumOsVersion: String { string("minimumOsVersion") }
    var name: String { string("trackName") }
    // "Promotional Text" is not present in the App Store JSON.
    var price: Double { double("price") }
    var releaseDate: Date { date("currentVersionReleaseDate") }
    var releaseNotes: String { string("releaseNotes") }
    var storeVersion: String { string("version") }
}
```

## Creating Instance

The following code gets an instance of `AppInfo` in a SwiftUI view:

```swift
@State private var appInfo: AppInfo?

...

.task {
    do {
        appInfo = try await AppInfo.create()
    } catch {
        print("Error getting AppInfo:", error)
    }
}
```

## Link to Help

The following code uses `AppInfo` to add a help button to the navigation bar.
Tapping this opens the app support page in Safari.

```swift
.toolbar {
    ToolbarItem(placement: .navigationBarTrailing) {
        if let appInfo {
            Link(destination: URL(string: appInfo.supportURL)!) {
                Image(systemName: "questionmark.circle")
            }
        }
    }
}
```

## Link to App Store

The following code uses `AppInfo` to inform users when
a newer version of the app is available in App Store and
provides a link that directs them to the App Store so it can be installed.

```swift
if let appInfo,
    !appInfo.haveLatestVersion,
    let url = URL(string: appInfo.appURL) {
    Link("Update Available", destination: url)
}
```
