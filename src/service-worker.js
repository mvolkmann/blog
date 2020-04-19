let cacheName;

ESLINT ERROR HERE

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

self.addEventListener('install', async () => {
  // There is no need to do anything here.
});

self.addEventListener('activate', async () => {
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
