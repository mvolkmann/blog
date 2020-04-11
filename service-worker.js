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
    "revision": "e026c1e14e3bb9107c385ae6c213665e"
  },
  {
    "url": "11ty-overview/index.html",
    "revision": "c2b1ac802fafc0835edf18c75cb07f0a"
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
    "revision": "f2cda940bdfb6d2f4bc6779cd02c270d"
  },
  {
    "url": "collections/index.html",
    "revision": "2984bac3b241cb9ebda9bd87c7442373"
  },
  {
    "url": "data-cascade/index.html",
    "revision": "e918348417ad4469686be7ac24a75342"
  },
  {
    "url": "debugging-tips/index.html",
    "revision": "d4accd6c5d29ffab6621d851999ea15b"
  },
  {
    "url": "front-matter/index.html",
    "revision": "bf60621340c2a7d9188a46f6d61d794c"
  },
  {
    "url": "getting-started/index.html",
    "revision": "a2f70eba503ed6d74c5d16b13fccbcf2"
  },
  {
    "url": "github-pages/index.html",
    "revision": "230dfb04d4e8829d62ec466598a8dde4"
  },
  {
    "url": "global-data/index.html",
    "revision": "13e55ae34d6d08e999107f68505d4dfc"
  },
  {
    "url": "index.html",
    "revision": "d5468aea642205f4c04d9d94f4ee1730"
  },
  {
    "url": "internationalization/index.html",
    "revision": "f6a319a1cdf89987cd8894982119ffad"
  },
  {
    "url": "markdown/index.html",
    "revision": "e8fa43b17f87643c81fd107a4c677522"
  },
  {
    "url": "nunjucks/index.html",
    "revision": "97a1d9fbb9c9f4b1af0c540f25056959"
  },
  {
    "url": "pagination/index.html",
    "revision": "d98465902c1964e357fbf41520cd6f4e"
  },
  {
    "url": "permalinks/index.html",
    "revision": "b4396deb37cb07a2f4fc2c929b9ef906"
  },
  {
    "url": "rest-services/index.html",
    "revision": "c04ee8356bacbc047be7605eb5c064c9"
  },
  {
    "url": "shortcodes/index.html",
    "revision": "462b3cad5bd9ae5f6bcc6bfc4408d947"
  },
  {
    "url": "ssgs/index.html",
    "revision": "24b4b1e9463e690c59ba7f5391834ab3"
  },
  {
    "url": "styling-with-sass/index.html",
    "revision": "92b12a2ad1d793fe746c8ff8c61e4bbf"
  },
  {
    "url": "styling/index.html",
    "revision": "4971eadbb1de1b6c192f8bcc3c5705e8"
  },
  {
    "url": "syntax-highlighting/index.html",
    "revision": "021bbf981d4d515640459b70c2710b7e"
  },
  {
    "url": "templating-languages/index.html",
    "revision": "2117ced2e198d5a03cdb36b82a6a8615"
  },
  {
    "url": "why-11ty/index.html",
    "revision": "ceb97e8983a0b293bddf9c3137eaf8a7"
  },
  {
    "url": "yaml/index.html",
    "revision": "548cffa13414edab731212e6e647f610"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
