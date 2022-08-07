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
- Deployment/update process
- Developer availability

## Native

For iOS the preferred approach is to use the SwiftUI framework
with the Swift programming language.

For Android the preferred approach is to use the ? framework
with the Kotlin programming language.

### Pros

- Performance

  Native apps provide the best performance.

- Application size

  Native apps have the smallest application sizes.

- Access to platform features

  Native apps provide the best access to platform features.

  When new APIs are released, they typically can only be used
  in apps that require the latest version of the operating system.
  In order to support users that do not upgrade
  their operating system frequently, apps often must
  wait a year or more to incorporate the use of new APIs.

### Cons

- Multiple implementations required

  Separated code bases are needed to support each mobile operating system.
  This typically requires employing a separate team for each operating system.

- Deployment/update process

  The initial deployment of native apps and subsequent updates
  both require waiting for app store approval.
  This introduces a delay in making apps available to users.

- Developers availability

  There are many more experienced web developers than there are
  developers experienced in targeting specific mobile operating systems.

## Cross-Platform

The most popular cross-platform mobile frameworks are
React Native and Flutter.

### Pros

- Single implementation

  Cross-platform apps can target Android and iOS with the same code base.

- Access to platform features

  Cross-platform apps typically have more access to platform features
  than web apps, but less than native apps.
  Native code can be written to bridge the gap,
  making all platform features available.
  But the need to write some native code negates
  some of the benefit of using a cross-platform framework.

### Cons

- Performance

  Performance is typically worse than native apps, but better than web apps.

- Application size

  The size of cross-platform apps is typically
  larger than native apps and smaller than web apps.

- Deployment/update process

  The deployment process for cross platform apps is the same as for native apps.
  Waiting for app store approval is required.
  However, some cross-platform frameworks (only JavaScript-based ones)
  remove the need for updates to undergo app store approval.

- Developer availability

  The number of developers that have experience with
  a particular cross-platform framework is likely less than
  those with experience in native and web frameworks.

## Responsive Web

Any web framework can be used to build a responsive web app.

### Pros

- Can target most mobile operating systems with the same code base.

- Deployment/update process

  Users do not need to install the web apps.
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

- Application size

  Web applications tend to have the largest application size.
  However, depending on how the app is implemented,
  it may not be necessary to download all of the code
  to mobile devices.
  Some of the code can only run on a server.

- Access to platform features

  Basic platform features such as camera access and geolocation
  are available through web APIs, but more advanced features
  may not be available.

- Browser caching

  Browser caching can prevent users from getting app updates.
  A cache busting strategy must be employed to prevent this issue.
