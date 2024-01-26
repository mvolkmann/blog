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
The window or tab in which the associated web app is running
can be closed and associated service workers can continue executing.

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

For example, the following sequence of events can occur:

- web app posts a message to a service worker
- service worker sends a request to an API endpoint to fetch data
- service worker posts a message to the web app
  to send it some of the fetched data
- web app updates the DOM using the received data

## Caching Strategies

Services workers can implement many caching strategies,
perhaps based on the kinds of resources that will be requested.
Resources can be files with content such as
HTML, CSS, JavaScript, JSON, and image data.
Resources can also be the results of API calls.

The diagram below is helpful in describing the various caching strategies.
The descriptions below refer to the letters in this diagram.

<img alt="Service Workers" style="border: 0; width: 60%"
  src="/blog/assets/service-workers.png?v={{pkg.version}}"
  title="Service Workers">

Possible caching strategies include:

- Network Only

  This strategy is applicable when using
  cached versions of a resource is unacceptable.
  For example, a banking app might decide it is better to
  let the user know when they are offline and not show stale data.

  All resource requests are forwarded to the network,
  and only resources obtained from the network are returned.
  No caching is used.
  In the diagram above, this is represented by the path A-B-C-F.

- Cache Only

  This strategy is applicable for resources that never change
  or that change very rarely.
  Examples include a CSS file that defines site styling
  or an image file for a company logo.

  The service worker is responsible for initially populating the caches
  and thereafter only returns resources from the caches.
  Resource requests are never forwarded to the network.
  In the diagram above, this is represented by the path A-D-E-F.

- Network or Cache

  This strategy is applicable when having the latest data is preferred,
  but it is acceptable to use previously fetched data.
  For example, a site that reports basketball scores
  prefers to show the latest scores, but
  showing the last known scores is better than showing no scores.

  Resource requests are first forwarded to the network.
  If a response is obtained from the network, that is returned.
  Then the cache is updated with the response, so it can be used
  again later if the same resource is requested while offline.
  If no response is obtained from the network and
  a previously cached response is available, that is returned.
  In the diagram above, this is represented by the path A-B-C-(D-E)-F
  where steps D and E are optional.

- Cache and Update

  This strategy is applicable when fast responses are prioritized
  over having the latest data.
  It’s difficult to think of a case when this strategy might be preferred,
  but the next strategy augments this to make it more applicable.

  If the requested resource is available in a cache, that is returned.
  Then the request is forwarded to the network to obtain an up-to-date value.
  If the network returns a different value, the cache is updated
  so the next request for the same resource will receive the updated value.
  This is great for performance but
  has the downside of potentially using stale data.
  In the diagram above, this is represented by the path A-D-E-F-B-C-D.

- Cache, Update, and Refresh

  This strategy starts the same way as the "cache and update" strategy,
  but after new data is received from the network,
  the UI is triggered to refresh using the new data.

  For example, a theatre website might use this approach
  to quickly display the known shows and ticket availability from the cache.
  As soon as new data becomes available,
  it can update this information in the browser.

- Embedded Fallback

  In this strategy, the service worker provides default responses for cases
  when the resource cannot be obtained from the network or a cache.
  For example, a service worker that returns photos of specific dogs
  can return stock images that match the breeds of requested dogs
  when the requested photos are unavailable.
  This, of course, assumes that the stock images have already been cached.

  This strategy can be employed as a
  supplement to the previously described strategies
  to provide an alternative to returning a "Not Found" (404) status.

For some web applications it is possible to define
a subset of functionality that can be supported
for offline use and then only cache resources related to that.
For example, a todo app can
cache the latest todo items so they can be displayed,
but disallow adding, modifying, and deleting todo items while offline.

For some web applications it is acceptable
to accumulate transactions when offline
and execute them later when online again.
For example, suppose a time sheet web app allows users
to enter the hours they worked on various projects.
If network connectivity is lost,
the app can save the hours entered in a cache.
When connectivity is restored, it can read data from the cache,
make the appropriate API calls to save it on a server,
and delete the data from the cache.

This can be challenging to implement due to
special cases that must be considered.
For example, what should be done in the time sheet app if we are
saving hours for a project that has been deleted by someone else
after network connectivity was lost for the user that entered the hours?
Perhaps the new hours should just be ignored, or
perhaps the project should be recreated and the hours should be applied to it.
There can be many such cases to consider.

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
