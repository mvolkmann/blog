---
eleventyNavigation:
  key: Progressive Web Apps (PWAs)
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="PWA logo" style="border: none; width: 30%"
  src="/blog/assets/pwa-logo.svg?v={{pkg.version}}"
  title="PWA logo">

## Overview

Progressive Web Apps (PWAs) enable using web technologies
to implement mobile apps.

Supported features include background sync, bluetooth, camera access,
contact access, device motion, file access, geolocation, offline mode,
push notifications, and touch gestures.

When a PWA attempts to access device features, such as contacts or the camera,
the user will be prompted to grant permission.

To open a PWA in a phone, browse its URL from the a mobile web browser.
Users can optionally choose to install the PWA.
Doing so downloads all the files required by the app.
It also adds an app icon to the home screen so in the future
the user can double-click that icon to relaunch the app.
The app will still run in the mobile web browser,
but the app can choose to hide the browser "chrome"
so it appears more like a native app.

Production PWAs must be downloaded using HTTPS in order to use service workers.
In development it is possible to configure the browser to
allow using service workers with localhost URLs that use HTTP.

## Advantages Over Native Apps

The advantages that PWAs have over native mobile apps include the ability to:

- allow users to access apps by URL
  instead of needing to download from an app store
- bypass app store review
- avoid app store cut of purchase prices (such as Apple's 30% cut)
- provide automatic app updates
- implement using widely known web technologies
- run on web, Android, and iOS with a single code base

## Service Workers

A service worker is a kind of web worker.
This means that its functionality is defined in a JavaScript source file
and it runs in a background thread.

Service workers have many use cases:

- enable offline functionality by acting as
  a proxy between a web app and the network,
  deciding how to handle each resource request
- listen for external events
- periodically fetch data
- send notifications to associated web apps using push notifications

Service workers can allow parts of a web application
to continue functioning after network connectivity is lost.
They do this by returning cached responses for requests
when requests for inaccessible resources are received.

Common tasks performed by service workers include:

- creating caches
- storing resources in caches
- intercepting network requests and deciding how to respond

There is a limit to the amount of data each application domain can cache
and a limit to the amount of data that can be cached across all domains.
The limits differ across web browsers,
but are mostly consistent for Chrome, Edge, and Firefox.
The limit for a single domain is 20% of the overall limit.
Typically the limits are based on the available space
as shown in the following table.

| Available Space | Total Cache Limit                     |
| --------------- | ------------------------------------- |
| up to 8 GB      | 50 MB                                 |
| 8 to 32 GB      | 500 MB                                |
| 32 to 128 GB    | 4% of volume size                     |
| over 128 GB     | smaller of 20 GB or 4% of volume size |

Safari is an outlier.
It limits each application to 50 MB of cache storage
regardless of the available space, and
it purges this after two weeks of not being used.
An alternative is to use IndexedDB, which has a
limit of 500 MB per application domain in Safari.

When the total limit is reached, some browsers (Chrome and Firefox)
remove the least recently used caches to make room for new caches.

Service Workers do not have access to the DOM of their associated web app,
so they cannot directly change what is rendered in the browser.
However, they can communicate with the web app via message passing.
The `Worker` `postMessage` method is used to
send a message from a web worker to the web app
or from the web app to a web worker.

## Manifest File

## Evaluating Readiness

To determine if a web app can be used as a PWA:

- Use a desktop computer or laptop to open the app in the Chrome web browser.
- Open the DevTools.
- Click on the "Lighthouse" tab.
- Click the "Analyze page load" button.
- Look for the "Progressive Web App" score.

## Resources

- {% aTargetBlank "https://web.dev/explore/progressive-web-apps", "web.dev" %} on PWAs
