---
eleventyNavigation:
  key: Progressive Web Apps
layout: layout.njk
---

Progressive web apps support offline operation using a service worker.
11ty sites can also use a service worker
so their pages are accessible when offline.

One approach is to cache all asset files
(including images and stylesheets)
and cache each page only when it is visited.

We also want to start over with a new cache
every time a new version of a site is deployed.

To do this we need to write some data to a JSON file
when a site is built.
The `.eleventy.js` file is a good place to do this
because that is executed when a site is built.

Here is a snippet of relevant code from `.eleventy.js`:

```js
module.exports = eleventyConfig => {
  // Create JSON file that is read by service-worker.js.
  const serviceWorkerData = {
    assets: fs.readdirSync('_site/assets'),
    timestamp: Date.now()
  };
  fs.writeFileSync(
    '_site/service-worker-data.json',
    JSON.stringify(serviceWorkerData)
  );
```

Here is a service worker implementation that uses this JSON file:

```js
let cacheName;

async function getServiceWorkerData() {
  try {
    // .eleventy.js writes this file.
    const res = await fetch('service-worker-data.json');
    if (res.ok) return res.json();
    const text = await res.text();
    throw new Error(text);
  } catch (e) {
    console.error('error getting service-worker.json:', e);
  }
}

self.addEventListener('install', async event => {
  try {
    const data = await getServiceWorkerData();
    cacheName = 'cache-' + data.timestamp;
    const toCache = data.assets.map(file => '/blog/assets/' + file);

    // Precache asset files.
    const cache = await caches.open(cacheName);
    await cache.addAll(toCache);
    //self.skipWaiting();
  } catch (e) {
    console.error('service-worker.js install: error', e);
  }
});

self.addEventListener('activate', async event => {
  // Get all the current cache keys.
  const keys = await caches.keys();

  // Delete all old caches.
  for (const key of keys) {
    if (key !== cacheName) await caches.delete(key);
  }

  self.clients.claim();
});

self.addEventListener('fetch', async event => {
  const {request} = event;
  const url = new URL(request.url);

  // Only handle GET requests.
  if (request.method !== 'GET') return;

  // Don't handle BrowserSync requests.
  if (url.pathname.startsWith('/browser-sync/')) return;

  // Don't handle non-http requires such as data: URIs.
  if (!url.protocol.startsWith('http')) return;

  async function getResponsePromise() {
    const cache = await caches.open(cacheName);
    // Try to find a response in the cache.
    let response = await cache.match(request);
    if (!response) {
      // Get the response from the network.
      // We will get a 404 error if not found.
      response = await fetch(request);
      console.log('service-worker.js got', url.pathname, 'from network');

      // Cache the response.
      cache.put(request, response.clone());
    } else {
      console.log('service-worker.js got', url.pathname, 'from cache');
    }
    return response;
  }

  event.respondWith(getResponsePromise());
});
```

The main layout file that produces the outer HTML
should register the service worker.
Place the following inside the `head` element to do this:

```html
<script>
  // Only register the service worker when not on localhost.
  if (location.hostname !== 'localhost' && 'serviceWorker' in navigator) {
    window.addEventListener('load', () => {
      navigator.serviceWorker.register('/blog/service-worker.js');
    });
  }
</script>
```
