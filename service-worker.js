let cachedSet = new Set();
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
    cachedSet = new Set(toCache);

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

self.addEventListener('fetch', event => {
  const {request} = event;
  const url = new URL(request.url);
  console.info('service-worker.js fetch: url =', url);

  if (request.method !== 'GET') {
    console.info(
      'service-worker.js fetch: skipped request with method',
      request.method
    );
    return;
  }

  // Don't try to handle non-http requires such as data: URIs.
  if (!url.protocol.startsWith('http')) {
    console.info(
      'service-worker.js fetch: skipped request with protocol',
      url.protocol
    );
    return;
  }

  console.log('service-worker.js fetch: calling event.waitUntil');
  event.waitUntil(async () => {
    // Try to get response from cache.
    console.log('service-worker.js fetch: checking cache');
    const cache = await caches.open(cacheName);
    console.log('service-worker.js fetch: cache =', cache);
    let response = cache.match(request);
    console.log('service-worker.js fetch: cache response =', response);
    if (!response) {
      // Try to get response from network:.
      console.log('service-worker.js fetch: checking network');
      response = fetch(request);
      console.log('service-worker.js fetch: network response =', response);

      // Cache the response.
      cache.put(request, response.clone());
      console.info('service-worker.js fetch: cached', request.url);
    }
    event.respondWith(response);
    console.log('service-worker.js fetch: called event.respondWith');
  });

  /*
  //TODO: Why check host?
  if (url.host === self.location.host && cachedSet.has(url.pathname)) {
    console.log('service-worker.js fetch: getting', url.pathname, 'from cache');
    event.respondWith(caches.match(event.request));
    return;
  }
  */

  /*
  event.respondWith(async () => {
    console.log('service-worker.js fetch: checking caches');
    const cachedResponse = await caches.match(request);
    // Return it if we found one.
    if (cachedResponse) return cachedResponse;
    // If we didn't find a match in the cache, use the network.
    console.log('service-worker.js fetch: checking network');
    const response = await fetch(request);
    cache.put(request, response.clone());
    return response;
  });
  */

  /*
    try {
      const cache = await caches.open(cacheName);
      console.log('service-worker.js fetch: trying cache');
      let response = await cache.match(request);
      console.log('service-worker.js fetch: cache response =', response);
      if (response) {
        console.log('service-worker.js fetch: got from cache');
        return response;
      }
      console.log('service-worker.js fetch: trying network');
      response = await fetch(request);
      console.log('service-worker.js fetch: network response =', response);
      if (response) {
        console.log('service-worker.js x: got from network');
        cache.put(request, response.clone());
        return response;
      }
    } catch (err) {
      console.error('service-worker.js fetch: error =', err);
    }
  });
  */
});
