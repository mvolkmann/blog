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
    "revision": "4e31c79a443fa1952de43bee8dec130f"
  },
  {
    "url": "11ty-overview/index.html",
    "revision": "da0cdedf2de7c48c2b51689ed57a1e4f"
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
    "revision": "3a3f5317a1b46c65c0d56b245140871f"
  },
  {
    "url": "collections/index.html",
    "revision": "ae1f761cf81e8a740d30e93a4aee6680"
  },
  {
    "url": "data-cascade/index.html",
    "revision": "82831c120c318fdd9a45ef997b64b6ba"
  },
  {
    "url": "debugging-tips/index.html",
    "revision": "c5a51bea2e9e390a432f8d080b89eaa4"
  },
  {
    "url": "front-matter/index.html",
    "revision": "63e0cd0d11867d41339045a0caf2ae1f"
  },
  {
    "url": "getting-started/index.html",
    "revision": "254d530a81b3a84619875478dc995545"
  },
  {
    "url": "github-pages/index.html",
    "revision": "e9165f481673b59d0ee092eec0d0135a"
  },
  {
    "url": "global-data/index.html",
    "revision": "535abf839b9a487a834d423d2e3d80e0"
  },
  {
    "url": "index.html",
    "revision": "544f16bf0dcd9bd1e3bc2d876d01a125"
  },
  {
    "url": "internationalization/index.html",
    "revision": "b145a4bd0c6964a23bf444669bbbe1bd"
  },
  {
    "url": "markdown/index.html",
    "revision": "51d64d7f3bfc7c8a864529dc97efb616"
  },
  {
    "url": "nunjucks/index.html",
    "revision": "1db469c6459dcc2df97c49f38df281cf"
  },
  {
    "url": "pagination/index.html",
    "revision": "e49cc75a45ea0f738e0a9bfcfd34c631"
  },
  {
    "url": "permalinks/index.html",
    "revision": "e814489d96720a607f2b78cf6995e4d6"
  },
  {
    "url": "rest-services/index.html",
    "revision": "3e20fa6f571fb5771094df29c94102ce"
  },
  {
    "url": "shortcodes/index.html",
    "revision": "30305bb1d2cf302ba81d6c8098523d17"
  },
  {
    "url": "ssgs/index.html",
    "revision": "dcecba3ca69060aa2fc3068483d51f6e"
  },
  {
    "url": "styling-with-sass/index.html",
    "revision": "11b808711534f2d175bb3fbe7f214aec"
  },
  {
    "url": "styling/index.html",
    "revision": "64b6b7d74e26654a109b0120c01be0aa"
  },
  {
    "url": "syntax-highlighting/index.html",
    "revision": "a22ed74c3e6fb84878d85cc45faa5e35"
  },
  {
    "url": "templating-languages/index.html",
    "revision": "00ff571f6ec93e46959cd20d33b9440a"
  },
  {
    "url": "why-11ty/index.html",
    "revision": "70517137d6bf4e497dc27362e83003a2"
  },
  {
    "url": "yaml/index.html",
    "revision": "272c9a476d664ffd048ae56fd64b0088"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
