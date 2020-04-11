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
    "revision": "a3d093d7acfc3fd3010bb3f2988ab9b7"
  },
  {
    "url": "11ty-overview/index.html",
    "revision": "e3dddcb17aec8457d116ae482e662e83"
  },
  {
    "url": "assets/favicon.png",
    "revision": "09b1bb9ed374f29226a44a494848fa06"
  },
  {
    "url": "assets/manifest.json",
    "revision": "882aa37ac8ac693c733f8789c906bc4b"
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
    "revision": "3e6a607a128360ee400431ea142439bc"
  },
  {
    "url": "collections/index.html",
    "revision": "a250bbbdb706eda71b59d84db06b6ea2"
  },
  {
    "url": "data-cascade/index.html",
    "revision": "699948aa3a98c6c6771ceaa3427b394d"
  },
  {
    "url": "debugging-tips/index.html",
    "revision": "7e3e72ef5261696f4251feebd7835286"
  },
  {
    "url": "front-matter/index.html",
    "revision": "8053212159245602f34069d582ce00fc"
  },
  {
    "url": "getting-started/index.html",
    "revision": "d09b40fe77788aba5f734c0142e1a72b"
  },
  {
    "url": "github-pages/index.html",
    "revision": "7f98707c3795d08e1f8f934c2ffcfd54"
  },
  {
    "url": "global-data/index.html",
    "revision": "474576694c92e22d4537a3dc35d0a3c9"
  },
  {
    "url": "index.html",
    "revision": "e8c0d6ed2d27257fa01f76eef690a7bc"
  },
  {
    "url": "internationalization/index.html",
    "revision": "775bf471515f2ace9329d7b6238fee7c"
  },
  {
    "url": "markdown/index.html",
    "revision": "0e2ef5ed1ed246edda72ae12ffb2d9de"
  },
  {
    "url": "nunjucks/index.html",
    "revision": "7505c09781f537edd543b033a0a35de7"
  },
  {
    "url": "pagination/index.html",
    "revision": "e0dbf4c4da4e87fd8c76731edd05b7da"
  },
  {
    "url": "permalinks/index.html",
    "revision": "d70e534a262e89e38a9d1be277cf2b6b"
  },
  {
    "url": "rest-services/index.html",
    "revision": "ecdcff704f2fde4a89ce578a0526cf05"
  },
  {
    "url": "shortcodes/index.html",
    "revision": "b84afee14b07c78bda58f32e8024d43c"
  },
  {
    "url": "ssgs/index.html",
    "revision": "16274b0e587c6ce26e341fcadcc00793"
  },
  {
    "url": "styling-with-sass/index.html",
    "revision": "51728ac15523da00a9e5a3f9fcd9042d"
  },
  {
    "url": "styling/index.html",
    "revision": "5521169ec2b48ea8d2573d58e6b3c7c5"
  },
  {
    "url": "syntax-highlighting/index.html",
    "revision": "df83f503f120231befe1c04c63f5ff21"
  },
  {
    "url": "templating-languages/index.html",
    "revision": "d888a204cbf788c0db603fc7826d02bc"
  },
  {
    "url": "why-11ty/index.html",
    "revision": "92a8b4b44343f53e76b8762543845970"
  },
  {
    "url": "yaml/index.html",
    "revision": "6a749077128f173dae2406a9280b5d36"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
