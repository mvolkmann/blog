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
    "revision": "db4214fdfd07204b565f87f0f8b5c298"
  },
  {
    "url": "11ty-overview/index.html",
    "revision": "bc97f3791ed501bba6c19a6a6af47c5c"
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
    "revision": "10fcbbe72080ec4f9157b93ddc2c3ac6"
  },
  {
    "url": "collections/index.html",
    "revision": "a545e1aa99c2740680519f7161b267bc"
  },
  {
    "url": "data-cascade/index.html",
    "revision": "ad6b1ab5a5ed5ab9ebf5707242fbb960"
  },
  {
    "url": "debugging-tips/index.html",
    "revision": "64ebff5b40cb8d686482d44890f1be41"
  },
  {
    "url": "front-matter/index.html",
    "revision": "584200b24ab1ad0d250e129be486db5f"
  },
  {
    "url": "getting-started/index.html",
    "revision": "bed4b78d2c8a5359084760edb07f22de"
  },
  {
    "url": "github-pages/index.html",
    "revision": "5a7c435b991ac120a8c393ca3679ba9b"
  },
  {
    "url": "global-data/index.html",
    "revision": "fed2483e80f9176ab871fc396664dab6"
  },
  {
    "url": "index.html",
    "revision": "0505b327d56cb53552c502ba9478b9bd"
  },
  {
    "url": "internationalization/index.html",
    "revision": "948ca54c3638737dbf79e648cb71544c"
  },
  {
    "url": "markdown/index.html",
    "revision": "66fb55efdb1bbf477f3f305079733e8d"
  },
  {
    "url": "nunjucks/index.html",
    "revision": "d495753635b14daa0bd52686a97106df"
  },
  {
    "url": "pagination/index.html",
    "revision": "b578dfcfff60ac02b8828d86ddab3f93"
  },
  {
    "url": "permalinks/index.html",
    "revision": "683af6139bd213c4620731a232909769"
  },
  {
    "url": "rest-services/index.html",
    "revision": "5bace0cf42c619eccb3b6225957ec7c7"
  },
  {
    "url": "shortcodes/index.html",
    "revision": "2414b0e79bc267bb0b56b3f448430a49"
  },
  {
    "url": "ssgs/index.html",
    "revision": "b2fe7c309f3dd5fae3b29c1646e28e6a"
  },
  {
    "url": "styling-with-sass/index.html",
    "revision": "76bee9b1213cf4446279e39d657a1b22"
  },
  {
    "url": "styling/index.html",
    "revision": "55b59644fc72c334873477d8c546115d"
  },
  {
    "url": "syntax-highlighting/index.html",
    "revision": "ecb9728b6b65de0cea7d0c17cdd9eeec"
  },
  {
    "url": "templating-languages/index.html",
    "revision": "5339adf4e1ed71bb821bde23fd84ff63"
  },
  {
    "url": "TODO/index.html",
    "revision": "18a66c4f8b5697844340d6af17c07182"
  },
  {
    "url": "why-11ty/index.html",
    "revision": "2dd5469f7489750fe226743ed3abe300"
  },
  {
    "url": "yaml/index.html",
    "revision": "a9c0119f78456433c066e96b27d2aa5d"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerRoute(/^.*\.(html|jpg|png|gif|webp|ico|svg|woff2|woff|eot|ttf|otf|ttc|json)$/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https?:\/\/fonts\.googleapis\.com\/css/, new workbox.strategies.StaleWhileRevalidate(), 'GET');
