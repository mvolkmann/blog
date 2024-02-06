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
to implement mobile applications.

PWAs can provide many of the features
typically associated with native applications.
These include the ability to:

- continue working while offline,
  possibly with reduced functionality
  and delayed transactions
- install a home screen icon that can
  be used to launch the application
- launch quickly without requiring download
  of files from the internet
- communicate with users via dialog boxes, even after
  they have left the application, to reengage them
  (using the Push API and the Notifications API)
- look like native apps by running
  without browser "chrome" or in full-screen mode
- support multiple operating systems with a single code base,
  reducing development costs

Supported features include background sync, bluetooth, camera access,
contact access, device motion, file access, geolocation, offline mode,
push notifications, and touch gestures.

## Advantages Over Native Apps

The advantages that PWAs have over native mobile apps include the ability to:

- implement using widely known web technologies
- bypass app store review
- allow users to access apps by URL
  rather than downloading them from an app store
- avoid app store cut of purchase prices (such as Apple's 30% cut)
- run on the web, Android, and iOS with a single code base
- provide automatic app updates

TODO: Fix the order of the remaining sections.

## Service Workers

{% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API",
"Service Workers" %} are the key to many PWA features.

A service worker is a kind of {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API",
"Web Worker" %}.
This means that its functionality is defined in a JavaScript source file
and it runs in a background thread.

Each of the windows and tabs associated with the web app
share access to the service workers of the app.
Even if the window and tabs are closed,
the service workers of the app can continue executing.
Viewing a service worker in Chrome DevTools
will list each of these windows and tabs as "Clients" below the "Status".

Service workers have many use cases:

- enable offline functionality
- listen for external events
- periodically fetch data
- send push notifications to the associated web app

A PWA can register any number of service workers,
but typically only one is used.

Service workers can allow parts of a web application
to continue functioning after network connectivity is lost.

Common tasks performed by service workers include:

- creating caches
- storing resources in caches
- intercepting network requests and deciding how to respond,
  including responding with cached values

## Registering a Service Worker

Each page of a web app that wished to utilize a service worker
must register the service worker.
The following code does this for a service worker
defined in the file `service-worker.js`.

```js
// This can be used to post messages to the service worker.
let serviceWorker;

if ('serviceWorker' in navigator) {
  try {
    const reg = await navigator.serviceWorker.register('service-worker.js');
    serviceWorker = reg.installing || reg.waiting || reg.active;
  } catch (error) {
    console.error('service worker registered failed:', error);
  }
} else {
  console.error('Your browser does not support service workers');
}
```

Services workers for each site are only loaded and registered once.
However, if their source file is modified
then they will automatically load and register again.

To see this, add a `console.log` call in the service worker JavaScript file.
Refresh the site in a browser and note the output in the DevTools Console.
Refresh the page again and the output will not appear.
Modify the service worker JavaScript file
to change what the `console.log` call outputs.
Refresh the page again and this time the output will appear.

## Manifest File

Service workers require a `manifest.json` file.
These are typically located at the top of the `public` directory
along with `index.html`.

There are many properties that can be set in the manifest file.
TODO: See the O'Reilly book "Building Progressive Web Apps", pages 176-181 for details.

The following properties are required:

- `name` and/or `short_name`
- `start_url`
- `icons`
- `display`

Other properties that can be set include:

- `description`
- `orientation`
- `theme_color`
- `background_color`
- `scope`
- `dir`
- `lang`
- `prefer_related_applications`
- `related_applications`

Once a service worker has been registered,
its manifest can be examined in Chrome devtools
by clicking the "Application" tab and clicking "Manifest" in the left nav.

## Generating Icons

The Node package {% aTargetBlank
"https://github.com/elegantapp/pwa-asset-generator", "pwa-asset-generator" %}
generates all the icons required by a PWA from a single image file.
To use this:

- Create the file `public/images/logo.png`.
- Enter `bunx pwa-asset-generator public/images/logo.png public/icons`.
- This will generate many images files in the `public/icons` directory.
- Copy the JSON array this outputs and paste it into `manifest.json`
  as the value of the `icons` property.
- Remove "public/" from the beginning of each icon `src` value.

### Storage Limits

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

## Evaluating Readiness

To determine if a web app can be used as a PWA:

- Use a desktop computer or laptop to open the app in the Chrome web browser.
- Open the DevTools.
- Click on the "Lighthouse" tab.
- Click the "Analyze page load" button.
- Click the "Progressive Web App" circle and address any issues identified.

### Message Passing

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

TODO: Implement message passing between a page and a service worker
and in the other direction. Then document the required code here.

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

  The following service worker code implements this strategy.

  ```js
  const cacheName = 'pwa-demo';

  // No fetch events are generated in the initial load of the web app.
  // A second visit (or refresh) is required to cache all the resources.
  self.addEventListener('fetch', event => {
    const {request} = event;

    const getResource = async () => {
      const {url} = request;
      let resource;

      try {
        // Get from network.
        // Note that resources coming from a local HTTP server
        // can be fetched even when offline.
        // To use cached versions of those, stop the local HTTP server.
        resource = await fetch(request);
        console.log('service worker got', url, 'from network');

        // Save in cache for when we are offline later.
        const cache = await caches.open(cacheName);
        await cache.add(url);
        console.log('service worker cached', url);
      } catch (e) {
        // Get from cache.
        resource = await caches.match(request);
        console.log('service worker got', url, 'from cache');
      }

      return resource;
    };

    event.respondWith(getResource());
  });
  ```

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

  The following service worker code implements this strategy.

  ```js
  const cacheName = 'pwa-demo-v1';

  self.addEventListener('activate', event => {
    const deleteOldCaches = async () => {
      const keyList = await caches.keys();
      return Promise.all(
        keyList.map(key => (key !== cacheName ? caches.delete(key) : null))
      );
    };
    event.waitUntil(deleteOldCaches());
  });

  // No fetch events are generated in the initial load of the web app.
  // A second visit is required to cache all the resources.
  self.addEventListener('fetch', event => {
    const {request} = event;

    const getResource = async () => {
      const {url} = request;
      let resource;

      // Get from cache.
      resource = await caches.match(request);
      if (resource) {
        console.log('service worker got', url, 'from cache');
      } else {
        try {
          // Get from network.
          resource = await fetch(request);
          console.log('service worker got', url, 'from network');

          // Save in cache for when we are offline later.
          const cache = await caches.open(cacheName);
          await cache.add(url);
          console.log('service worker cached', url);
        } catch (e) {
          console.error('service worker failed to get', url);
          resource = new Response('', {status: 404});
        }
      }

      return resource;
    };

    event.respondWith(getResource());
  });
  ```

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

  The following service worker code implements this strategy.

  ```js
  const cacheName = 'pwa-demo-v1';

  const filesToCache = [
    '/', // need in order to hit web app with domain only
    '/demo.css',
    '/demo.js',
    '/images/avatar.jpg',
    '/images/birthday-192.jpg'
  ];

  self.addEventListener('activate', event => {
    const deleteOldCaches = async () => {
      const keyList = await caches.keys();
      return Promise.all(
        keyList.map(key => (key !== cacheName ? caches.delete(key) : null))
      );
    };
    event.waitUntil(deleteOldCaches());
  });

  self.addEventListener('install', event => {
    const cacheAll = async () => {
      const cache = await caches.open(cacheName);
      await cache.addAll(filesToCache);
    };
    event.waitUntil(cacheAll());
  });

  // No fetch events are generated in the initial load of the web app.
  // A second visit is required to cache all the resources.
  self.addEventListener('fetch', event => {
    const {request} = event;

    const getResource = async () => {
      const {url} = request;
      const isAvatar = url.includes('githubusercontent.com');
      let resource;

      // Get from cache.
      resource = await caches.match(request);
      if (resource) {
        console.log('service worker got', url, 'from cache');
      } else {
        try {
          // Get from network.
          resource = await fetch(request);
          console.log('service worker got', url, 'from network');

          if (!isAvatar) {
            // Save in cache for when we are offline later.
            const cache = await caches.open(cacheName);
            await cache.add(url);
            console.log('service worker cached', url);
          }
        } catch (e) {
          if (isAvatar) {
            console.log('service worker using generic avatar');
            resource = Response.redirect('/images/avatar.jpg');
          } else {
            console.error('service worker failed to get', url);
            resource = new Response('', {status: 404});
          }
        }
      }

      return resource;
    };

    event.respondWith(getResource());
  });
  ```

## Offline Support

Web applications can determine whether they currently have network connectivity
by checking the value of `navigator.onLine`.
It's odd that the "L" is uppercase since "online" is a word!

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

## Service Worker Events

Service workers listen for events and act on them.

### install Event

The first event received is `install`.
This is a one-time event.
One way to handle this event is to do the following:

- Open a cache whose name is `cache`
  concatenated with the value of the `timestamp` variable.
  If this cache does not exist, it is created.
- Add some files to the cache that will always be served from the cache.
  They will be available without a network connection
  after the app is initially loaded from the network.

### activate Event

The second event received is `activate`.
This is also a one-time event.
One way to handle this event is to delete any old caches for this app
that were created when previous builds of the app were run.
This can be determined by checking whether their names
contain the current value of the `timestamp` variable.

### message Event

A `message` event occurs when a controlled page
calls `ServiceWorker.postMessage(message)`
to send a message to a specific service worker.
The service worker can optionally send a response back
by calling `Client.postMessage(message)`.

### fetch Event

The third event type received is `fetch`.
This can be received many times.
The caching strategy is implemented here.
One way to handle this event is to evaluates each request
using the following steps in this sequence:

- Only process `GET` requests.
- Don’t process requests asking for just part of a document,
  using an HTTP "Range" header (not commonly used).
- Only process URLs with a protocol beginning with "http".
  For example, URLs with the "data" protocol are ignored.
- Serve all static files from the cache.
- If the file is not found in the cache and
  the request has a `cache` property of `only-if-cached`,
  don't attempt to find the file using the network.
- Otherwise, attempt to satisfy the request using the network.
- If found, add the file to the cache and return its contents.
- If not found and a match for the URL is in the cache, return that content.

This caching strategy means that the results of API service calls are cached.
Later, if the same request is made again and the service is offline,
the cached value will be returned.

### sync Event

This event is used to determine when data can be synchronized
between a web page and service worker.
It relies on a `SyncManager` which is experimental.
It is supported by Chrome and Edge, but not by Safari or Firefox.

### push Event

A `push` event occurs when a push notification is received.
TODO: Try implementing push notifications.

## Running and Installing a PWA

To run a PWA, browse its URL.
This can be done by clicking a link received in an email or chat message.
It can also be done by searching for the app in web browser
or manually entering the URL in the browser location bar.

Installing a PWA is optional.
The steps to do so vary based on operating system.
In iOS, tap the share button and select "Add to Home Screen".

Installing a PWA downloads all the required files.
It also adds an app icon to the home screen so in the future
the app can be launched by tapping its icon.
The app will still run in the mobile web browser,
but the app can choose to hide the browser chrome
so it appears more like a native app.

In development, PWA can be run from URLs that being with `http://localhost`.
But in production, PWAs must be run from URLS that use HTTPS
in order to utilize service workers.

## Security

When a PWA attempts to access device features, such as contacts or the camera,
the user will be prompted to grant permission.

## Managing Service Workers

The Chrome DevTools provide ways to interact with service workers
and the caches they create.

To view service workers for the current site,
click the DevTools Application tab.
Then click Service Workers in the left nav.
The main area will display information about each of the
service workers for the site.

<img alt="Service Workers in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-service-workers.png?v={{pkg.version}}"
  title="Service Workers in Chrome DevTools">

The status of a service worker is displayed after the Status label.
For example, it may say “activated and is running.”
To stop the service worker, click the Stop link after the status.
The Stop link will change to Start, and this can be clicked to restart it.

### Updating a Service Worker

By default, changes to service worker code
are not loaded by refreshing the browser.

Unregistering a service worker allows it to
run through its lifecycle again when the page is refreshed.
This includes processing the `install` and `activate` events again.
This is useful for debugging the code that handles those events.

To unregister a service worker in Chrome, click the "Unregister" link
to the right of a service worker description
and refresh the page (twice?).

In Chrome, to enable page refreshes
to reload service workers during development,
check the "Update on reload" checkbox and refresh the page.

<img alt="Service Workers Update on reload" style="width: 100%"
  src="/blog/assets/service-workers-update-on-reload.png?v={{pkg.version}}"
  title="Service Workers Update on reload">

By default, changes to deployed service workers will not take effect for users
until they close all browser tabs that are using the previous service workers
and open new tabs.

To force existing tabs that are browsing a site
to activate service worker updates,
add the following code in the service worker.

```js
self.addEventListener('install', event => {
  // This causes a newly installed service worker to
  // progress to the activating state, regardless of
  // whether there is already an active service worker.
  self.skipWaiting();
});
```

In order for this change to take effect,
users must close existing tabs for the site and open a new one.
TODO: After this is done, do users have to refresh the page to load service worker updates?

### Viewing Source Code

To view the source code for a service worker,
click the link after the Source label.
This switches to the Sources tab and displays the code.
Typically the code will have been minified.

<img alt="Minimized service worker code in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-service-worker-code-minimized.png?v={{pkg.version}}"
  title="Minimized service worker code in Chrome DevTools">

To see a pretty-printed version of this,
click the “{}” at the bottom.

<img alt="Pretty-printed service worker code in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-service-worker-code-pretty-printed.png?v={{pkg.version}}"
  title="Pretty-printed service worker code in Chrome DevTools">

### Cached Files

The list of requested files in the Network tab has a very small gear icon
before each file that was loaded from a cache.
This is useful for determining whether specific files
were served from the network or from a cache.

<img alt="Files loaded from cache in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-network-files-from-cache.png?v={{pkg.version}}"
  title="Files loaded from cache in Chrome DevTools">

To see the space being used by the current application domain,
click the Application tab and click "Storage" in the left nav.
This displays the amount of space currently occupied by
cache storage, service workers, and IndexedDB.
It also displays a series of check boxes for categories of things
hat can be cleared including "Unregistered service workers",
"Local and session storage", "IndexedDB", "Web SQL" (deprecated),
"Cookies", and "Cache storage".

By default, all the check boxes are checked.
Click the "Clear Site Data" button above the checkboxes
to clear the data associated with every checked category.

<img alt="Clearing storage in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-clear-storage.png?v={{pkg.version}}"
  title="Clearing storage in Chrome DevTools">

A great video covering most of the topics in this section,
created by the Chrome team, can be found at {% aTargetBlank
"http://mng.bz/oPwp", "Debugging Service Workers in Chrome" %}.

A similar video for Firefox from the same team can be found at {% aTargetBlank
"http://mng.bz/nPw2", "Debugging Service Workers in Firefox" %}.

To see the files that have been cached,
click the DevTools "Application" tab.
Then click the disclosure triangle before "Cache storage" in the left nav.
This will show a list of all the current caches for the site in the left nav.
Click one of the cache names to see a list
of the files that it has cached in the main area.
Click a file to see its contents at the bottom of the main area.

<img alt="Cached files in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-cache.png?v={{pkg.version}}"
  title="Cached files in Chrome DevTools">

There are several ways to remove an individual file from a cache.

1. Right-click a file name and select delete.
1. Click a file name to select it and press the Delete key.
1. Click a file name to select it and click the “X” at the top.

To delete an entire cache, right-click a cache name and select Delete.

### Clearing Everything

The Chrome Devtools provide a way to clear many things
associated with the current application domain with a single click.
This includes service worker registrations, local and session storage,
IndexedDB databases, cookies, and cached files.
To do this, open the DevTools, click the "Application Tab",
click "Storage" near the top of the left nav,
verify that all the checkboxes at the bottom are checked,
and click the "Clear site data" button.

<img alt="Chrome Clear site data button" style="width: 100%"
  src="/blog/assets/chrome-clear-site-data.png?v={{pkg.version}}"
  title="Cached files in Chrome DevTools">

### Simulating Offline

In order to test the ability of service workers to use cached files,
it is useful to simulate being offline.
To do this, open Chrome Devtools, click the "Application" tab,
click Service Workers in the left nav, and
check the "Offline" check box at the top of the main area.
This is an alternative to going to the Network tab
and changing the "No throttling" drop-down to "Offline".
A warning icon will appear in the Network tab
to remind you that you are offline.

<img alt="Simulating being offline in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-offline.png?v={{pkg.version}}"
  title="Simulating being offline in Chrome DevTools">

### Bypassing Service Workers

To bypass the use of service workers,
causing all requests to go to the network,
check the Bypass for Network check box at the top of the main area.
This, of course, requires being online.

### Service Workers in macOS Safari

To see service workers in Safari, click the "Develop" menu,
hover over "Service Workers" to reveal a menu that lists them,
and click one of the service workers.
This opens a window with three tabs at the top ...
"Console", "Sources", and "Network".

Output from `console` methods like `console.log`
do not appear in the Web Inspector Console.
Instead they appear in the Console tab of the service worker window.

Safari does not provide an easy way to unregister service workers.
To unregister all of them for a given site, browse the site,
open the "Web Inspector", click the "Console" tab, and enter the following code.

```js
navigator.serviceWorker.getRegistrations().then(registrations => {
  for (const registration of registrations) {
    registration.unregister();
  }
});
```

To also delete a specific IndexedDB database, enter the following.

```js
indexedDB.deleteDatabase('db-name');
```

### Service Workers in iOS Safari

The following steps enable debugging PWAs running in iOS Safari.

1. Attach the device (iPhone or iPad) to a Mac using a USB cable.
1. A "Trust This Computer?" dialog will appear on the device.
1. Tap "Trust".
1. Enter the device passcode.
1. Launch Safari on the device.
1. Browse the URL of a PWA.
1. Launch Safari on the Mac.
1. Click the "Develop" menu.
1. Hover over the device name that appears.
1. Click "Use for Development..."
1. A "Trust This Computer?" dialog will appear again on the device.
1. Tap "Trust" again.
1. Enter the device passcode again.
1. Click the "Develop" menu again.
1. Hover over the device name that appears again.
1. If it says "Pairing Denied, Reconnect Device to Continue",
   disconnect the USB cable and connect it again.
1. Click the "Develop" menu again.
1. Hover over the device name that appears again.
1. Select the URL of the PWA to open a "Web Inspector" window
   that can be used to interact with the device
   and see its output in the Console tab.
1. Select ? to open a "Service Worker" window
   that can be used to interact with the Service Worker running on the device
   and see its output in this separate Console tab.

## Workbox

{% aTargetBlank "https://web.dev/learn/pwa/workbox", "Workbox" %} is a set of
open-source libraries that help with implementing PWA caching strategies
in service workers.

The easiest way to use Workbox is to open a terminal, cd to the project root,
and enter `bunx workbox-cli wizard`.
This will ask a series of questions about the PWA.

This will create the file `workbox-config.js` with content like the following:

```js
module.exports = {
  globDirectory: 'public/',
  globPatterns: ['**/*.{js,png,jpg,gif,html,json,css}'],
  swDest: 'public/sw.js',
  ignoreURLParametersMatching: [/^utm_/, /^fbclid$/]
};
```

Create the file `public/sw.js` with content like the following:

```js
import {registerRoute} from 'workbox-routing';
import {CacheFirst} from 'workbox-strategies';
import {CacheableResponsePlugin} from 'workbox-cacheable-response';

const pageStrategy = new CacheFirst({
  // Put all cached files in a cache named 'pages'
  cacheName: 'pages',
  plugins: [
    // Only requests that return with a 200 status are cached
    new CacheableResponsePlugin({
      statuses: [200]
    })
  ]
});

// Cache page navigations (HTML) with a Cache First strategy
registerRoute(({request}) => request.mode === 'navigate', pageStrategy);
```

TODO: Describe how to load this service worker from a script!

## Resources

- {% aTargetBlank "https://web.dev/explore/progressive-web-apps", "web.dev" %} on PWAs

TODO: For more content, see https://github.com/mvolkmann/pwa-demo/blob/master/pwa.md.

See demo app at https://github.com/mvolkmann/pwa-cloudflare-demo.

## Outstanding Questions

Demonstrate sending messages between the app and a service worker.

How can mobile clients get app updates without restarting the app?

Do push notifications work in iOS now?

Should you use the workbox and workbox-cli libraries
to simplify service worker code?
