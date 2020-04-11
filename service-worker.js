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
    "url": "11ty-configuration/index.html",
    "revision": "f2bc57a2d9ae8f20cad8dc705207c0e2"
  },
  {
    "url": "11ty-overview/index.html",
    "revision": "2e3025a62add99a6840b7f3b162138de"
  },
  {
    "url": "assets/favicon.png",
    "revision": "09b1bb9ed374f29226a44a494848fa06"
  },
  {
    "url": "assets/manifest.json",
    "revision": "ed1092892abb47dde79888e3671c7d86"
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
    "url": "assets/styles.css",
    "revision": "bb007b5db7e7ef26067863734df5b4f5"
  },
  {
    "url": "collection-sorting/index.html",
    "revision": "12c373bd806af1e70e9eea6d5e7a5562"
  },
  {
    "url": "collections/index.html",
    "revision": "c71114a2d740912c9cedacc246922406"
  },
  {
    "url": "data-cascade/index.html",
    "revision": "0a7dd5171de11fb6f2487f1157ff9a3b"
  },
  {
    "url": "debugging-tips/index.html",
    "revision": "962b72e449af2d8cd532c16f2d5e59d2"
  },
  {
    "url": "front-matter/index.html",
    "revision": "b19caf1c77e5216c458890b8771caad8"
  },
  {
    "url": "getting-started/index.html",
    "revision": "25be7ed87b2b67b5af7132fde57f280a"
  },
  {
    "url": "github-pages/index.html",
    "revision": "67a635a8968392efc4b8b60c9323b74d"
  },
  {
    "url": "global-data/index.html",
    "revision": "307ea685a234cc7f3630ac4a951de6b1"
  },
  {
    "url": "index.html",
    "revision": "8b8711b494af5943fc1e2221de0581c6"
  },
  {
    "url": "internationalization/index.html",
    "revision": "258d33c6f6dcaff9f3bbc5917678bc1f"
  },
  {
    "url": "markdown/index.html",
    "revision": "15274933d0fbdb6900a5d9f6a773d44a"
  },
  {
    "url": "nunjucks/index.html",
    "revision": "576603de54264ed63dbe7a96c1f0f3a8"
  },
  {
    "url": "pagination/index.html",
    "revision": "fa74235626dcc30ecc7925206da51850"
  },
  {
    "url": "permalinks/index.html",
    "revision": "cb0900630df58e59776c9316e7f161d9"
  },
  {
    "url": "rest-services/index.html",
    "revision": "2bdedb3c565d16a5bb051aa14e721c81"
  },
  {
    "url": "shortcodes/index.html",
    "revision": "451eb6d33a60fce928f825b6f72da525"
  },
  {
    "url": "ssgs/index.html",
    "revision": "bfbb0cfe2d833d4d8c5e83e77869add1"
  },
  {
    "url": "styling-with-sass/index.html",
    "revision": "3201506c63634fb086219421431ffc4a"
  },
  {
    "url": "styling/index.html",
    "revision": "f9dca57d9152cc77f744dacaab96f19f"
  },
  {
    "url": "syntax-highlighting/index.html",
    "revision": "d8ccf9423f2bf2977f74e5d1f3c7fa93"
  },
  {
    "url": "templating-languages/index.html",
    "revision": "4f703f1c408ba0433993ce6de82d2fad"
  },
  {
    "url": "why-11ty/index.html",
    "revision": "0dcb1acff7db18397b8c2128889a689c"
  },
  {
    "url": "yaml/index.html",
    "revision": "f8feaf28d25117f5ad34ee14fa55f12d"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
