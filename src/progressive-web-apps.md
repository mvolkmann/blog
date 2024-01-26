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

## Running and Installing a PWA

The steps to run a new PWA for the first time are:

1. Open a web browser.
1. Search for the application or enter its URL.
1. Click its link.
1. Optionally install the PWA which downloads all the require files.
   This also adds an app icon to the home screen so in the future
   the app can be launched by double-clicking its icon.
   The app will still run in the mobile web browser,
   but the app can choose to hide the browser chrome
   so it appears more like a native app.

Often the first three steps are replaced by clicking
a link that is found in another way such as
in an email message or a social media site.
In this case a new PWA can be launched with a single click.

Production PWAs must be downloaded using HTTPS in order to use service workers.
In development it is possible to configure the browser to
allow using service workers with localhost URLs that use HTTP.

## Security

When a PWA attempts to access device features, such as contacts or the camera,
the user will be prompted to grant permission.

## Advantages Over Native Apps

The advantages that PWAs have over native mobile apps include the ability to:

- allow users to access apps by URL
  instead of needing to download from an app store
- bypass app store review
- avoid app store cut of purchase prices (such as Apple's 30% cut)
- provide automatic app updates
- implement using widely known web technologies
- run on web, Android, and iOS with a single code base

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

## Evaluating Readiness

To determine if a web app can be used as a PWA:

- Use a desktop computer or laptop to open the app in the Chrome web browser.
- Open the DevTools.
- Click on the "Lighthouse" tab.
- Click the "Analyze page load" button.
- Look for the "Progressive Web App" score.

## Service Workers

Service workers are the key to many PWA features.
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

A PWA can register any number of service workers,
but typically only one is used.

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

The first event received is `install`.
This is a one-time event.
One way to handle this event is to do the following:

- Open a cache whose name is `cache`
  concatenated with the value of the `timestamp` variable.
  If this cache does not exist, it is created.
- Add some files to the cache that will always be served from the cache.
  They will be available without a network connection
  after the app is initially loaded from the network.

The second event received is `activate`.
This is also a one-time event.
One way to handle this event is to delete any old caches for this app
that were created when previous builds of the app were run.
This can be determined by checking whether their names
contain the current value of the `timestamp` variable.

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

## Managing Service Workers in Chrome

The Chrome DevTools provide a way to interact with service workers
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

Unregistering a service worker allows it to run through its lifecycle again
one time, when the page is refreshed.
This includes processing the `install` and `activate` events again.
This is useful for debugging the code that handles those events.
To do this, click the Unregister link to the
right of a service worker description.

To cause service workers to install and activate again
every time the page is reloaded, and without creating a new build,
check the Update on Reload check box at the top of the main area,
and refresh the page.

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

To see the files that have been cached,
click the DevTools Application tab.
Then click the disclosure triangle before Cache Storage in the left nav.
This will display a list of all the current caches for the site.
The caches that Sapper apps create by default have names that
begin with “cache” and “offline” and end with a build timestamp.

Click one of these to see a list of the files that it has cached.
Click a file to see its contents at the bottom of the main area.

<img alt="Cached files in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-cache.png?v={{pkg.version}}"
  title="Cached files in Chrome DevTools">

To remove an individual file from a cache,
select the file in the main area and
press the Delete key or click the “X” above the list of files.
To delete an entire cache, right-click a cache name and select Delete.

To simulate being offline, click Service Workers in the left nav
and check the Offline check box at the top of the main area.
This is an alternative to going to the Network tab
and changing the Online drop-down to Offline.
A warning icon will appear in the Network tab
to remind you that you are offline.
This is useful for testing the ability of service workers
to use cached files.

<img alt="Simulating being offline in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-offline.png?v={{pkg.version}}"
  title="Simulating being offline in Chrome DevTools">

To bypass the use of service workers,
causing all requests to go to the network,
check the Bypass for Network check box at the top of the main area.
This, of course, requires being online.

The list of requested files in the Network tab
has a gear icon before each file that was loaded from a cache (see figure 19.8).
This is useful for determining whether specific files
were served from the network or from a cache.

<img alt="Files loaded from cache in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-network-files-from-cache.png?v={{pkg.version}}"
  title="Files loaded from cache in Chrome DevTools">

To clear many things at once,
click the Application tab and
click Clear Storage (preceded by a trashcan icon) in the left nav.
This displays a series of check boxes in the main area
for categories of things that can be cleared.
By default, all the check boxes are checked.
This includes Unregister Service Workers and Cache Storage.
Click the Clear Site Data button to clear
the data associated with every checked category.

<img alt="Clearing storage in Chrome DevTools" style="width: 100%"
  src="/blog/assets/devtools-clear-storage.png?v={{pkg.version}}"
  title="Clearing storage in Chrome DevTools">

A great video covering most of the topics in this section,
created by the Chrome team, can be found at {% aTargetBlank
"http://mng.bz/oPwp", "Debugging Service Workers in Chrome" %}.

A similar video for Firefox from the same team can be found at {% aTargetBlank
"http://mng.bz/nPw2", "Debugging Service Workers in Firefox" %}.

## Development Tips

During development it is often necessary to force certain files to be reloaded.
Files cached by a service worker are not cleared by clearing the browser cache.

In Chrome, one approach to force files to be reloaded
on the next browser refresh is to open the devtools,
click the "Application" tab, and check the "Update on reload" checkbox.

To force reloading for a specific service worker,
select the "Application" tab, select "Service Workers",
and click the "Unregister" link for the service worker.

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

Document generating icons using https://github.com/elegantapp/pwa-asset-generator.

- Create the file `public/images/logo.png`.
- Enter `bunx pwa-asset-generator public/images/logo.png public/icons`.
- This will generate many images files in the `public/icons` directory.
- Copy the JSON array this outputs and paste it into `manifest.json`
  as the value of the `icons` property.
- Remove "public/" from the beginning of each icon `src` value.
