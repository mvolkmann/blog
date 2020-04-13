let cachedSet;
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
    console.log('service-worker.js install: toCache =', toCache);
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
  const urlString = request.url;
  console.info('service-worker.js fetch: urlString =', urlString);

  if (request.method !== 'GET' || request.headers.has('range')) {
    console.info('service-worker.js fetch: method =', request.method);
    console.info('service-worker.js fetch: skipped request');
    return;
  }

  const url = new URL(urlString);

  // Don't try to handle non-http requires such as data: URIs.
  if (!url.protocol.startsWith('http')) {
    console.info('service-worker.js fetch: protocol =', url.protocol);
    console.log('service-worker.js fetch: skipped request');
    return;
  }

  // Serve from cache if possible.
  if (url.host === self.location.host && cachedSet.has(url.pathname)) {
    console.log('service-worker.js fetch: getting', url.pathname, 'from cache');
    event.respondWith(caches.match(event.request));
    return;
  } else {
    console.log(
      'service-worker.js fetch: NOT getting',
      url.pathname,
      'from cache'
    );
  }

  // Try the network first, falling back to cache if offline.
  event.respondWith(
    caches.open(cacheName).then(async cache => {
      try {
        const response = await fetch(request);
        cache.put(request, response.clone());
        //console.log(
        //  'service-worker.js fetch: got response from network and cached'
        //);
        return response;
      } catch (err) {
        const response = await cache.match(request);
        if (response) {
          //console.log('service-worker.js fetch: got response from cache');
          return response;
        }
        throw err;
      }
    })
  );
});
