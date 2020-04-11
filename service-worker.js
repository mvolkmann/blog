/**
 * Welcome to your Workbox-powered service worker!
 *
 * You'll need to register this file in your web app and you should
 * disable HTTP caching for this file too.
 * See https://goo.gl/nhQhGp
 *
 * The rest of the code is auto-generated. Please don't update this file
 * directly; instead, make changes to your Workbox build configuration
 * and re-run your build process.
 * See https://goo.gl/2aRDsh
 */

importScripts("workbox-v4.3.1/workbox-sw.js");
workbox.setConfig({modulePathPrefix: "workbox-v4.3.1"});

workbox.core.setCacheNameDetails({prefix: "eleventy-plugin-pwa"});

workbox.core.skipWaiting();

workbox.core.clientsClaim();

/**
 * The workboxSW.precacheAndRoute() method efficiently caches and responds to
 * requests for URLs in the manifest.
 * See https://goo.gl/S9QRab
 */
self.__precacheManifest = [
  {
    "url": "_eleventy_redirect/index.html",
    "revision": "ea95efa440aabf0ea14e76837c5e7e68"
  },
  {
    "url": "11ty-configuration/index.html",
    "revision": "7ffe55b08937a3efb440b38cb72dbd09"
  },
  {
    "url": "11ty-overview/index.html",
    "revision": "fc6115705391ef3f99ee7aaa3397634e"
  },
  {
    "url": "assets/favicon.ico",
    "revision": "4b10e3585f82c916af922d259afb3602"
  },
  {
    "url": "assets/favicon.png",
    "revision": "09b1bb9ed374f29226a44a494848fa06"
  },
  {
    "url": "assets/manifest.json",
    "revision": "434ec0e26a4c7173dd04f561c5208b81"
  },
  {
    "url": "assets/mark-volkmann-192.png",
    "revision": "12cdec085689716939fc1c59df020ee8"
  },
  {
    "url": "assets/mark-volkmann-512.png",
    "revision": "489dbce249ec1e92f11e28287f3af6a4"
  },
  {
    "url": "assets/service-worker-workbox.js",
    "revision": "e3cfb3a922dd398b979e248116038b92"
  },
  {
    "url": "assets/styles.css",
    "revision": "bb007b5db7e7ef26067863734df5b4f5"
  },
  {
    "url": "assets/sw.js",
    "revision": "63689603898d5065967ae88cd453fe03"
  },
  {
    "url": "assets/sw.js.map",
    "revision": "b8e0eddfdbf80956623483a1e2bbf185"
  },
  {
    "url": "assets/workbox-6cd01981.js",
    "revision": "9544c34a53b9787a751a8b8af59efb4f"
  },
  {
    "url": "assets/workbox-6cd01981.js.map",
    "revision": "998ce08f897010567a8325bbd1e8b382"
  },
  {
    "url": "collection-sorting/index.html",
    "revision": "0b9043992433731d2db7fd3569ed2801"
  },
  {
    "url": "collections/index.html",
    "revision": "6019b29247d389333099eb19dca4393b"
  },
  {
    "url": "data-cascade/index.html",
    "revision": "8281bf54b40e7693ae3d110001a3873d"
  },
  {
    "url": "debugging-tips/index.html",
    "revision": "4e6224a24c38553a732871c8228a1ade"
  },
  {
    "url": "front-matter/index.html",
    "revision": "e2b8c27b7874bbf9efb5e418fb6771df"
  },
  {
    "url": "getting-started/index.html",
    "revision": "c79e2ca453929db7eee83a36cfebf339"
  },
  {
    "url": "github-pages/index.html",
    "revision": "c08b9ebff9d423fe673a83c857253879"
  },
  {
    "url": "global-data/index.html",
    "revision": "9a9b6630bcac2113e4fddbb760b084b1"
  },
  {
    "url": "index.html",
    "revision": "ebdb26e26c823f822ea7de21c9a3a73c"
  },
  {
    "url": "internationalization/index.html",
    "revision": "4654ddb5f1c7097ba607127246ab2e94"
  },
  {
    "url": "markdown/index.html",
    "revision": "6a7f565c28121a36835f593e0520bc3a"
  },
  {
    "url": "nunjucks/index.html",
    "revision": "77c582187b5aec2b5ec4a616a385f9c5"
  },
  {
    "url": "pagination/index.html",
    "revision": "3f01794deb4d6fd3f8b60332e34ce3c5"
  },
  {
    "url": "permalinks/index.html",
    "revision": "98545872b1c3a5c85bf3973e1009e13b"
  },
  {
    "url": "rest-services/index.html",
    "revision": "39565feb08daec3a6adad3679f39ac47"
  },
  {
    "url": "shortcodes/index.html",
    "revision": "0b9608c80efe639cd4b2944abd0885c1"
  },
  {
    "url": "ssgs/index.html",
    "revision": "d5f8ec5fd96e7f47ede93de2b202cae8"
  },
  {
    "url": "styling-with-sass/index.html",
    "revision": "a3c2a78f0407515b44d90f91cf9015f0"
  },
  {
    "url": "styling/index.html",
    "revision": "da11ce3ae61579e4e45ece40503d6975"
  },
  {
    "url": "syntax-highlighting/index.html",
    "revision": "5822abbac7d83234ece28f5d89ee605a"
  },
  {
    "url": "templating-languages/index.html",
    "revision": "2ee21c2759416e0f0051a19c54e3084d"
  },
  {
    "url": "why-11ty/index.html",
    "revision": "353e9962b2f74e055d7bb4e83a216182"
  },
  {
    "url": "yaml/index.html",
    "revision": "921e92c72efc20b03164e598d41b1d93"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
