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
when the site is built.
The `.eleventy.js` file is a good place to do this
because that is executed when a site is built.

Here is a snippet of relevant code from `.eleventy.js`:

```js
module.exports = eleventyConfig => {
  // Create JSON file that is read by service-worker.js.
  let files = fs.readdirSync('_site/assets');
  files = files.map(file => '/blog/assets/' + file);
  files.push('/blog/'); // cache the start URL (Lighthouse wants this)
  const timestamp = Date.now();
  const serviceWorkerData = {files, timestamp};
  fs.writeFileSync(
    '_site/service-worker-data.json',
    JSON.stringify(serviceWorkerData)
  );
  console.log('wrote service-worker-data.json with timestamp', timestamp);
```

Here is a service worker implementation that uses this JSON file:

```js
let cacheName;

async function getServiceWorkerData() {
  try {
    // .eleventy.js writes this file.
    // We are avoiding reading it from the cache in order to
    // always get updated versions from the network.
    const res = await fetch('service-worker-data.json', {cache: 'no-store'});
    if (res.ok) return res.json();
    const text = await res.text();
    throw new Error(text);
  } catch (e) {
    console.error(
      'service-worker.js error fetching service-worker-data.json:',
      e
    );
    return {files: [], timestamp: 'error'};
  }
}

self.addEventListener('install', async event => {
  // There is no need to do anything here.
});

self.addEventListener('activate', async event => {
  // We are getting this data in the activate handler
  // because if we do it in the install handler
  // it sometimes isn't available yet here.
  const {files, timestamp} = await getServiceWorkerData();

  cacheName = 'cache-' + timestamp;
  console.info('service-worker.js using cache', cacheName);

  // Delete all old caches.
  const keys = await caches.keys();
  for (const key of keys) {
    if (key !== cacheName) await caches.delete(key);
  }

  // Precache asset files.
  const cache = await caches.open(cacheName);
  await cache.addAll(files);

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

    // Try to find response in the cache.
    let response = await cache.match(request);

    if (!response) {
      if (request.cache === 'only-if-cached') return;

      // Try to fetch response from the network.
      // We will get a 404 error if not found.
      response = await fetch(request, {cache: 'no-store'});
      console.info('service-worker.js got', url.pathname, 'from network');

      // Cache the response.
      cache.put(request, response.clone());
    } else {
      console.info('service-worker.js got', url.pathname, 'from', cacheName);
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
<link rel="manifest" href="/blog/assets/manifest.json" />

<script>
  // Only register the service worker when not on localhost.
  if (location.hostname !== 'localhost' && 'serviceWorker' in navigator) {
    window.addEventListener('load', () => {
      navigator.serviceWorker.register('/blog/service-worker.js');
    });
  }
</script>
```

Here is sample content for `assets/manifest.json` that should
be copied to `_site/assets` by the site build process:

```json
{
  "short_name": "blog",
  "name": "Mark Volkmann's blog",
  "description": "Mark Volkmann's blog",
  "icons": [
    {
      "src": "/blog/assets/mark-volkmann-192.png",
      "type": "image/png",
      "sizes": "192x192"
    },
    {
      "src": "/blog/assets/mark-volkmann-512.png",
      "type": "image/png",
      "sizes": "512x512"
    }
  ],
  "start_url": "/blog/",
  "background_color": "cornflowerblue",
  "display": "standalone",
  "scope": "/",
  "theme_color": "cornflowerblue"
}
```

For more information, see
[Every website deserves a service worker](https://blog.logrocket.com/every-website-deserves-a-service-worker/).
