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
