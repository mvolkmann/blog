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
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
