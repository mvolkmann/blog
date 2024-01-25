---
eleventyNavigation:
  key: Hot Topics 2024
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

Options for software development are in a constant state of change.
This article summarizes the concepts, libraries, and tools
that have captured my attention recently.

Perhaps some of these will be of use to you and your organization today,
while others may be things to watch and consider adopting in the future.

Here are my top picks, presented in alphabetical order.

## Alpine

Alpine is a JavaScript framework that provides custom attributes that are
applied to HTML elements in order to add dynamic behavior to web pages.

Alpine was created by Caleb Porzio, who also created the Laravel PHP framework
<a href="https://laravel-livewire.com" target="_blank">Livewire</a>.

Alpine provides reactivity similar to larger frameworks
like React, Svelte, and Vue.
Changes to data (aka state) maintained by Alpine trigger DOM updates.

The minified Alpine library for version 3.13.3,
which is the latest as of December 2023, is only 17K.

Here's a quick example of implementing a counter.
The `x-data` attribute declares state with a JavaScript object.
The `x-on` attribute registers an event handler.
The `x-text` attribute renders the value of a property in Alpine state.

<img alt="Alpine counter" style="width: 20%"
  src="/blog/assets/alpine-counter.png?v={{pkg.version}}">

```js
<html>
  <head>
    <script
      defer
      src="https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    ></script>
  </head>
  <body>
    <div style="display: flex; gap: 1rem" x-data="{ count: 0 }">
      <button x-bind:disabled="count <= 0" x-on:click="count--">
        -
      </button>
      <div x-text="count"></div>
      <button x-on:click="count++">+</button>
    </div>
  </body>
</html>
```

Alpine pairs nicely with htmx which is discussed later.
Both of these libraries increase the expressiveness of HTML
by adding support for new HTML attributes.

For more detail, see my
<a href="/blog/topics/#/blog/alpine/" target="_blank">Alpine</a> blog page.

## Astro

<a href="https://astro.build" target="_blank">Astro</a>
is a free, open-source (MIT license) framework that is
"the web framework for content-driven websites".
It can be used to generate static sites,
build server-side rendered (SSR) sites,
and define API endpoints.

A major focus of Astro is shipping less JavaScript code to browsers
and doing more work on the server side.

Astro supports using many kinds of UI components including
Astro, Alpine, Lit, Preact, React, SolidJS, Svelte, Vue, WebComponents, and more.

Astro uses the "<a href="https://docs.astro.build/en/concepts/islands/"
target="_blank">Islands architecture</a>".
Jason Miller (creator of the Preact framework)
describes this approach as a way to
"render HTML pages on the server, and inject placeholders or slots
around highly dynamic regions that can then be
hydrated on the client into small self-contained widgets,
reusing their server-rendered initial HTML."
Each island is a bit of JavaScript-enabled interactivity
and the water around them is static HTML.

For more detail, see my
<a href="/blog/topics/#/blog/astro/" target="_blank">Astro</a> blog page.

## Bun

"<a href="https://bun.sh" target="_blank">Bun</a>
is a fast JavaScript all-in-one toolkit."
It includes a JavaScript runtime, package manager, bundler, and test runner.
All of this is free and open source under the MIT license.

Bun can be used as a drop-in replacement for npm and Node.js.
The Bun runtime supports nearly all Node.js builtin modules (around 40 of them).

The benefits of using Bun over npm and Node.js include:

- Bun provides significantly better performance.
- Bun simplifies the current state of JS/TS tooling
  (transpilers, bundlers, testing tools, and so on).
  There is less to install and configure and it is easier to use.
- Bun supports both CommonJS (`require`) and ESM (`import`)
  in the same source files.
- Bun supports TypeScript out of the box.
- Bun supports JSX/TSX files for generating HTML.
- Bun has builtin support for SQLite databases using the `bun:sqlite` module.
  It is 2 times faster than Deno and 4 times faster than Node.js
  in queries per second.
  Future support for MySQL and Postgres is planned.
- Bun has builtin support for Jest-compatible unit tests
  using the `bun:test` module.
- Bun has better support for Web APIs including
  `fetch`, `Request`, `Response`, `ReadableStream`, `WebSocket`, and more.
  There is no need to install dependencies such as `node-fetch` and `ws`.
  Bun's native implementation of these tends to be
  faster than third-party libraries used with Node.js.
- Bun provides native hot reloading using the `--hot` flag
  which updates a running server without restarting it.
  This allows preserving application state
  and connections such as HTTP and WebSockets.
  Hot reloading in Node.js currently requires either using
  an external tool like nodemon or the experimental `--watch` flag.
- Bun provides bun-specific APIs that are alternatives to many npm modules.
  These are highly optimized and perform much better than their Node.js equivalents.
- Bun has a plugin API that is similar to esbuild.
  Many esbuild plugins work in Bun without modification.

The Bun team makes the following additional performance claims:

- Bun is 4 times faster than Node.js for a "hello world" program.
- The time to build and run TS code in Bun is 4 times faster than esbuild,
  15 times faster than TSX, and 43 times faster than TSC + Node.
- `bun install` is 29 times faster than npm and 17 times faster than pnpm.
- `bun run` can be used in place of `npm run` and is 5 times faster.
- `bun test` is 8 times faster than Vitest and 13 times faster than Jest.

For more detail, see my
<a href="/blog/topics/#/blog/bun/" target="_blank">Bun</a> blog page.

## Cloudflare Workers

For more detail, see my
<a href="/blog/topics/#/blog/cloudflare-workers/" target="_blank">Cloudflare Workers</a> blog page.

## Drizzle

For more detail, see my
<a href="/blog/topics/#/blog/drizzle/" target="_blank">Drizzle</a> blog page.

## GitHub Copilot

For more detail, see my
<a href="/blog/topics/#/blog/github/copilot/" target="_blank">GitHub Copilot</a> blog page.

## Hono

For more detail, see my
<a href="/blog/topics/#/blog/hono/" target="_blank">Hono</a> blog page.

## htmx

Of all the topics in my list, this one excites me the most!

For more detail, see my
<a href="/blog/topics/#/blog/htmx/" target="_blank">htmx</a> blog page.

## Long Polling

For more detail, see my
<a href="/blog/topics/#/blog/long-polling/" target="_blank">Long Polling</a> blog page.

## Server-Sent Events (SSE)

For more detail, see my
<a href="/blog/topics/#/blog/server-sent-events/" target="_blank">Server-Sent Events</a> blog page.

## WebSockets

For more detail, see my
<a href="/blog/topics/#/blog/websockets/" target="_blank">WebSockets</a> blog page.

## Zig

For more detail, see my
<a href="/blog/topics/#/blog/zig/" target="_blank">Zig</a> blog page.

## Zod

For more detail, see my
<a href="/blog/topics/#/blog/zod/" target="_blank">Zod</a> blog page.
