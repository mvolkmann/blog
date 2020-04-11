importScripts(
  'https://storage.googleapis.com/workbox-cdn/releases/5.1.2/workbox-sw.js'
);

if (workbox) {
  console.log(`Yay! Workbox is loaded 🎉`);
  const strategy = new workbox.strategies.NetworkFirst();
  workbox.routing.registerRoute(/\.css$/, strategy);
  workbox.routing.registerRoute(/\.html$/, strategy);
  workbox.routing.registerRoute(/\.jpg$/, strategy);
} else {
  console.log(`Boo! Workbox didn't load 😬`);
}
