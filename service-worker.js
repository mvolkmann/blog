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
    "revision": "914020fbc8d7bd49a9654c86e7601791"
  },
  {
    "url": "11ty-overview/index.html",
    "revision": "6cb6716ff15aede9fbf4512cf4078ba3"
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
    "url": "assets/styles.css",
    "revision": "bb007b5db7e7ef26067863734df5b4f5"
  },
  {
    "url": "collection-sorting/index.html",
    "revision": "730f0448ff9bf99e950d5e66fd46deff"
  },
  {
    "url": "collections/index.html",
    "revision": "791c492ff5c6a05d6e6f0a1b04aece82"
  },
  {
    "url": "data-cascade/index.html",
    "revision": "71ca5169f40658c33d174302109cf087"
  },
  {
    "url": "debugging-tips/index.html",
    "revision": "7589f1b01a19d6bc3040810bad83478d"
  },
  {
    "url": "front-matter/index.html",
    "revision": "925d20b8b2060f14e10a483f4200d6e6"
  },
  {
    "url": "getting-started/index.html",
    "revision": "0e06266d1deee13f860e860aacbb5e5b"
  },
  {
    "url": "github-pages/index.html",
    "revision": "eb199d7f87e4be14637358cb23b07b11"
  },
  {
    "url": "global-data/index.html",
    "revision": "4a4a71ad5991f11d15b0b08eb042f234"
  },
  {
    "url": "index.html",
    "revision": "a44d26d456d135dc1f367f1524bb49b8"
  },
  {
    "url": "internationalization/index.html",
    "revision": "6f0049371df67a4c3d2cf2e527fa91de"
  },
  {
    "url": "markdown/index.html",
    "revision": "6127a184348eb4f5def84f3d0630c1fb"
  },
  {
    "url": "nunjucks/index.html",
    "revision": "b7dfc3a527bfed7cda6c4bae3a0b605b"
  },
  {
    "url": "pagination/index.html",
    "revision": "aa12d8b7e1ea63f918142f62d6efe1ae"
  },
  {
    "url": "permalinks/index.html",
    "revision": "5c5ed68b3713bfd1dc0b6f2cc039be81"
  },
  {
    "url": "README/index.html",
    "revision": "5d49948cb669a37767446a916018728b"
  },
  {
    "url": "rest-services/index.html",
    "revision": "4268d7be107478c9dcfc0f40d6a53f10"
  },
  {
    "url": "shortcodes/index.html",
    "revision": "360e98df277fc9f037db77db793a112a"
  },
  {
    "url": "ssgs/index.html",
    "revision": "ece012db8a35aa0edea0d7b6cc92c767"
  },
  {
    "url": "styling-with-sass/index.html",
    "revision": "a9187ce8c0abec1b86ba0f87cf28ae01"
  },
  {
    "url": "styling/index.html",
    "revision": "d96579ad8740e3f94eda084cd77eb30c"
  },
  {
    "url": "syntax-highlighting/index.html",
    "revision": "84052f26b19523f52e15b73b19441a74"
  },
  {
    "url": "templating-languages/index.html",
    "revision": "5389f79fb17c3b9460ce2ae625bf3817"
  },
  {
    "url": "TODO/index.html",
    "revision": "18a66c4f8b5697844340d6af17c07182"
  },
  {
    "url": "why-11ty/index.html",
    "revision": "3f62ad722a1d3446254482b152d8b79d"
  },
  {
    "url": "yaml/index.html",
    "revision": "005533231fb8b5915310028f7aa8b826"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
