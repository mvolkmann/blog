let cacheName;
let data;

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
    console.error('error getting service-worker.json:', e);
  }
}

self.addEventListener('install', async event => {
  // Get data written by .eleventy.js that describes
  // files to cache initially and a build timestamp.
  data = await getServiceWorkerData();
  console.log('service-worker.js install: data =', data);

  /*
  // Precache asset files.
  const cache = await caches.open(cacheName);
  await cache.addAll(data.files);
  //self.skipWaiting();
  */
});

self.addEventListener('activate', async event => {
  console.log('service-worker.js activate: data =', data);
  // Delete all old caches.
  cacheName = 'cache-' + data.timestamp;
  console.log('service-worker.js install: cacheName =', cacheName);
  const keys = await caches.keys();
  for (const key of keys) {
    if (key !== cacheName) {
      console.log('service-worker.js activate: deleting cache', key);
      await caches.delete(key);
      console.log('service-worker.js activate: deleted cache', key);
    }
  }

  // Precache asset files.
  const cache = await caches.open(cacheName);
  await cache.addAll(data.files);
  //self.skipWaiting();

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
    console.log(
      'service-worker.js fetch: getting',
      url.pathname,
      'from cache',
      cacheName
    );
    // Try to find a response in the cache.
    let response = await cache.match(request);
    if (!response) {
      if (request.cache === 'only-if-cached') return;

      // Get the response from the network.
      // We will get a 404 error if not found.
      response = await fetch(request, {cache: 'no-store'});
      console.log('service-worker.js got', url.pathname, 'from network');

      // Cache the response.
      cache.put(request, response.clone());
    } else {
      console.log(
        'service-worker.js got',
        url.pathname,
        'from cache',
        cacheName
      );
    }
    return response;
  }

  event.respondWith(getResponsePromise());
});
