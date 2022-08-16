---
eleventyNavigation:
  key: Mobile App Options
layout: topic-layout.njk
---

## Overview

This post summarizes the options for building mobile applications
and lists their pros and cons.

Factors to consider include:

- Performance
- Application size
- Access to platform features
- Need for platform-specific implementations
- Deployment/update process
- Developer availability

## Native

For iOS the preferred approach is to use the SwiftUI framework
with the Swift programming language.

For Android the preferred approach is to use the Jetpack Compose framework
(released in July 2021) with the Kotlin programming language.
Jetpack Compose was created by Google and is well-supported by Intellij.
Jetpack Compose replaces the previous XML-based approach to defining views
in the same way that on iOS, SwiftUI replaces UIKit.

The table below summarizes native mobile development options.

|                            | Android                                                                                                                                                                                               | iOS                                                                                                                                                                     |
| -------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| imperative framework       | {% aTargetBlank "https://developer.android.com/codelabs/basic-android-kotlin-training-xml-layouts#0", "XML views" %}                                                                                  | {% aTargetBlank "https://developer.apple.com/documentation/uikit", "UIKit" %}                                                                                           |
| declarative framework      | {% aTargetBlank "https://developer.android.com/jetpack/compose?gclid=Cj0KCQjwgO2XBhCaARIsANrW2X26oavaAZPerKUsKB5UEmiA4Civ8OiBoZWzaOpH30MrlUFTd4jKbnQaAoAsEALw_wcB&gclsrc=aw.ds", "Jetpack Compose" %} | {% aTargetBlank "https://developer.apple.com/xcode/swiftui/", "SwiftUI" %}                                                                                              |
| older programming language | {% aTargetBlank "https://www.java.com/en/", "Java" %}                                                                                                                                                 | {% aTargetBlank "https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html", "Objective-C" %} |
| newer programming language | {% aTargetBlank "https://kotlinlang.org", "Kotlin" %}                                                                                                                                                 | {% aTargetBlank "https://developer.apple.com/swift/", "Swift" %}                                                                                                        |
| preferred IDE              | {% aTargetBlank "https://developer.android.com/studio", "Android Studio" %}                                                                                                                           | {% aTargetBlank "https://developer.apple.com/xcode/", "Xcode" %}                                                                                                        |

### Pros

- Performance

  Native apps provide the best performance.

- Application size

  Native apps have the smallest application sizes
  so they download faster and occupy less space on devices.

- Access to platform features

  Native apps provide the best access to platform features
  (for example, the ability to schedule background tasks
  or access Siri on iOS).

  When new APIs are released, they typically can only be used
  in apps that require the latest version of the operating system.
  In order to support users that do not upgrade
  their operating system frequently, apps often must
  wait a year or more to incorporate the use of new APIs.

### Cons

- Multiple implementations required

  Separated code bases are needed to support each mobile operating system.
  This typically requires employing a separate team for each platform.

- Deployment/update process

  The initial deployment of native apps and subsequent updates
  both require waiting for app store approval.
  This introduces a delay in making apps available to users.

- Developers availability

  There are many more experienced web developers than there are
  developers experienced in targeting specific mobile platforms.

## Cross-Platform

The most popular cross-platform mobile frameworks are
{% aTargetBlank "https://reactnative.dev", "React Native" %},
{% aTargetBlank "https://flutter.dev/", "Flutter" %}, and
{% aTargetBlank "https://capacitorjs.com", "Capacitor" %}.

### Pros

- Single implementation

  Cross-platform apps can target Android and iOS with the same code base.

- Access to platform features

  Cross-platform apps typically have more access to platform features
  than web apps, but less than native apps.
  Native code can be written to bridge the gap,
  making all platform features available.
  The need to write native code negates some of the
  benefit of using a cross-platform framework.
  However, many applications do not require access to platform features.

### Cons

- Performance

  Performance is typically worse than native apps, but better than web apps.

- Application size

  The size of cross-platform apps is typically
  larger than native apps and smaller than web apps.

- Deployment/update process

  The deployment process for cross platform apps is the same as for native apps.
  Waiting for app store approval is required.
  However, some cross-platform frameworks (those that are JavaScript-based)
  remove the need for updates to undergo app store approval.

- Developer availability

  The number of developers that have experience with
  a particular cross-platform framework is likely less than
  those with experience in native and web frameworks.

## Responsive Web

Any web framework can be used to build a responsive web app.

### Pros

- Single implementation

  Responsive web apps can target Android and iOS with the same code base.

- Deployment/update process

  Users do not need to install web apps.
  They only need to open a mobile web browser and
  enter the URL of the app,
  or click a link to the app in an email or text message.

  For apps that will be used repeatedly, users can
  bookmark the app in their mobile web browser or
  create a home screen icon for launching the app
  without explicitly opening their mobile web browser.

  Updates can be deployed without permission from an app store.
  All that is required is to redeploy the web app to a server.

- Developer availability

  There are a large number of developers that are experienced in using
  HTML, CSS, JavaScript, TypeScript, and specific web frameworks.

### Cons

- Performance

  Web applications have the worst performance.
  Despite this the performance is acceptable for many applications.

- Application size

  Web applications tend to have the largest application size.
  However, depending on how the app is implemented,
  it may not be necessary to download all of the code
  to mobile devices.
  Some of the code reside and run on a server.

- Access to platform features

  Basic platform features such as camera access and geolocation
  are available through web APIs, but more advanced features
  may not be available.
  However, many applications do not require access to platform features.

- Browser caching

  Browser caching can prevent users from getting app updates.
  A cache busting strategy must be employed to prevent this issue.
