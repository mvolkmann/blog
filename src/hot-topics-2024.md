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

My top picks are presented in alphabetical order below.

All of the picks that are software libraries or tools
have experienced significant increases in their number of GitHub stars,
as shown in the following chart.
GitHub stars are a measure of the number of software developers
that find a repository interesting, and perhaps already use or plan to use it.

<img alt="GitHub stars" style="border: 0; width: 100%"
  src="/blog/assets/github-stars-history.png?v={{pkg.version}}">

## Alpine

<img alt="Alpine logo" style="border: 0; width: 20%"
  src="/blog/assets/alpine-logo.png?v={{pkg.version}}">

Alpine is a JavaScript framework that provides custom attributes that are
applied to HTML elements in order to add dynamic behavior to web pages.
Alpine is notable for how easy it is to use and
how small it is compared to other web frameworks.

Alpine is in my 2024 list because it is popular to use in conjunction with htmx,
which is also in the list.

Alpine was created by Caleb Porzio, who also created the Laravel PHP framework
<a href="https://laravel-livewire.com" target="_blank">Livewire</a>.

Alpine provides reactivity similar to larger frameworks
like React, Svelte, and Vue.
Changes to data (aka state) maintained by Alpine trigger DOM updates.

The minified Alpine library for version 3.13.5,
which is the latest as of January 2024, is only 17K.

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

<img alt="Astro logo" style="border: 0; width: 20%"
  src="/blog/assets/astro-logo.svg?v={{pkg.version}}">

<a href="https://astro.build" target="_blank">Astro</a>
is a free, open-source (MIT license) framework that is
"the web framework for content-driven websites".
It can be used to generate static sites,
build server-side rendered (SSR) sites,
and define API endpoints.

Astro is in my 2024 list due to significant improvements in version 4.0
which was released on December 5, 2023.

A major focus of Astro is shipping less JavaScript code to browsers
and doing more work on the server side.

Astro supports using many kinds of UI components including
Astro, Alpine, Lit, Preact, React, SolidJS, Svelte, Vue, WebComponents,
and more.

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

<img alt="Bun logo" style="border: 0; width: 20%"
  src="/blog/assets/bun-logo.svg?v={{pkg.version}}">

"<a href="https://bun.sh" target="_blank">Bun</a>
is a fast JavaScript all-in-one toolkit."
It includes a JavaScript runtime, package manager, bundler, and test runner.
All of this is free and open source under the MIT license.

Bun is in my 2024 list because version 1.0 was released on September 8, 2023,
marking a major milestone in its stability.

Bun can be used as a drop-in replacement for npm and Node.js.
The Bun runtime supports nearly all Node.js built-in modules
(around 40 of them).

The benefits of using Bun over npm and Node.js include:

- Bun provides significantly better performance.
- Bun simplifies the current state of JS/TS tooling
  (transpilers, bundlers, testing tools, and so on).
  There is less to install and configure and it is easier to use.
- Bun supports both CommonJS (`require`) and ESM (`import`)
  in the same source files.
- Bun supports TypeScript out of the box.
- Bun supports JSX/TSX files for generating HTML.
- Bun has built-in support for SQLite databases using the `bun:sqlite` module.
  It is 2 times faster than Deno and 4 times faster than Node.js
  in queries per second.
  Future support for MySQL and Postgres is planned.
- Bun has built-in support for Jest-compatible unit tests
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
  These are highly optimized and perform much better
  than their Node.js equivalents.
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

<img alt="Cloudflare Workers logo" style="border: 0; width: 20%"
  src="/blog/assets/cloudflare-workers-logo.svg?v={{pkg.version}}">

<a href="https://workers.cloudflare.com" target="_blank">Cloudflare Workers</a>
host edge functions.

Cloudflare workers are in my 2024 list
because I just learned about them recently.
I waited far too long to investigate them.

Features provided include:

- automatic scaling

  There is no need to configure auto-scaling or load balancers.

- high performance global network

  Cloudflare workers run in a network of data centers that use V8 isolates
  that have very low latency (approximately 25ms in my testing).

- ability to write in a variety of programming languages,
  including JavaScript, TypeScript, Rust, C, and C++.

- run instantly without cold starts

- affordable

  The first 100,000 requests each day are free.
  After that the cost is $5 USD per 10 million requests.

- no servers to maintain

- provides edge storage of static assets using "Workers KV"

- can generate assets at runtime,
  including images, SVGs, PDFs, and more

For more detail, see my <a href="/blog/topics/#/blog/cloudflare-workers/"
target="_blank">Cloudflare Workers</a> blog page.

## Drizzle

<img alt="Drizzle logo" style="border: 0; width: 20%"
  src="/blog/assets/drizzle-logo.png?v={{pkg.version}}">

<a href="https://orm.drizzle.team" target="_blank">Drizzle</a> is a
TypeScript-based Object Relational Mapper (ORM) library
that is free and open-source.
It competes with other popular ORMs such as
<a href="https://www.prisma.io" target="_blank">Prisma</a>.

Drizzle is in my 2024 list because it is the first database ORM
I have seen that has a syntax resembling SQL, making it easy to learn.

Drizzle is designed "to be a thin layer on top of SQL
and introduce minimal runtime overhead".

Database table schemas are defined entirely in TypeScript.
These are used to create/migrate tables AND provide type checking in code.

The pros of Drizzle include:

- The Drizzle methods for interacting with databases are SQL-like,
  so it doesn't feel like learning a new syntax.
- Queries created with the Drizzle Query API always result in one SQL query.
  This helps with performance and minimizes round trips to the server.
- The Drizzle library is lightweight (32K minified).
- Drizzle is easy to use.
- Drizzle does not require any code generation.
- Drizzle has no dependencies.
- Drizzle supports many databases. These include
  LiteFS, MySQL, Neon, PlanetScale, PostgreSQL,
  SQLite, Supabase, Turso, Vercel Postgres, Web SQLite, and Xata.
  Notable exceptions include Microsoft SQL Server and MongoDB.
- Drizzle can generate TypeScript schema definitions
  from existing database tables.
- Drizzle supports schema migrations.
- Drizzle supports many edge platforms. These include:

  - <a href="https://bun.sh" target="_blank">Bun</a>
  - <a href="https://developers.cloudflare.com/workers/" target="_blank">Cloudflare Workers</a>
  - <a href="https://deno.com/deploy" target="_blank">Deno Deploy</a>
  - <a href="https://www.electronjs.org" target="_blank">Electron</a>
  - <a href="https://fly.io" target="_blank">Fly.io</a>
  - <a href="https://supabase.com/docs/guides/functions" target="_blank">Supabase functions</a>
  - <a href="https://vercel.com/docs/functions/serverless-functions" target="_blank">Vercel functions</a>

While Drizzle supports many kinds of databases,
switching the configuration and code that works with one type
to work with another is fairly tedious.
There are differences in the configuration and code required for each.

For more detail, see my
<a href="/blog/topics/#/blog/drizzle/" target="_blank">Drizzle</a> blog page.

## Hono

<img alt="Hono logo" style="border: 0; width: 20%"
  src="/blog/assets/hono-logo.png?v={{pkg.version}}">

<a href="https://hono.dev/" target="_blank">Hono</a>
is a JavaScript HTTP server library that runs in any JavaScript runtime.
This includes AWS Lambda, Bun, Cloudflare Pages, Cloudflare Workers,
Deno, Fastly, Lagon, Netlify, NextJS, Node.js, and Vercel.

Hono is in my 2024 list because it makes it
very easy to define endpoints that return HTML.
Htmx, which is also in my list, requires defining many of those.

"Honō" is the Japanese word for "flame" or "blaze" which explains its logo.

Hono is significantly faster than
<a href="https://expressjs.com" target="_blank">Express</a>,
which is currently the most popular HTTP library for Node.js.

<a href="https://elysiajs.com/" target="_blank">Elysia</a> is a competitor
to Hono. It has slightly better performance than Hono, but only runs in Bun.

For more detail, see my
<a href="/blog/topics/#/blog/hono/" target="_blank">Hono</a> blog page.

## htmx

<img alt="htmx logo" style="border: 0; width: 20%"
  src="/blog/assets/htmx-logo.png?v={{pkg.version}}">

Htmx is in my 2024 list because it
provides a fresh take on building web applications
that has many benefits over currently popular approaches.
Of all the topics in my list, this one excites me the most!

<a href="https://htmx.org" target="_blank">Htmx</a>
is a hypermedia-oriented, client-side JavaScript library.
Hypermedia is any data format that can describe
branching from one "media" (ex. a document) to another.
A hypermedia control is an element that describes a server interaction
such as HTML anchor (`a`) and `form` elements.

Htmx adds support for new HTML attributes that make HTML more expressive.
These attributes enable implementing "Hypermedia-Driven Applications" (HDAs).

The new HTML attributes support responding to
any kind of interaction (ex. click) with any HTML element
by sending an HTTP request using any method (GET, POST, PUT, PATCH, or DELETE).
The response must contain HTML or plain text, not JSON.
Rather than performing a complete page refresh, the returned HTML
replaces an existing DOM element or is inserted relative to one.

Htmx removes the need to serialize data to JSON on the server,
parse the JSON on the client, and build an HTML representation from the data.

The server endpoints can be implemented using
any programming language and server framework.
This is referred to as "Hypermedia On Whatever you'd Like" (HOWL).

The server typically plays two roles.
First, it serves static files such as HTML, CSS, JavaScript, and images.
Second, it responds to HTTP requests by returning HTML or text.

Many web app features typically thought to require custom JavaScript code
can instead be implemented with only htmx.
Examples include lazy loading of data, infinite scrolling,
and searching while the user types in an `input`.

Users perceive apps built with htmx to be fast because
the initial page load only requires
the htmx library (< 17K) and the initial HTML/CSS to render.
Subsequent interactions only require fetching snippets of HTML or text.
No client-side hydration of JavaScript code is needed.
Browsers are very efficient at updating the DOM from strings of HTML.

For more detail, see my
<a href="/blog/topics/#/blog/htmx/" target="_blank">htmx</a> blog page.

## Long Polling

Long polling is in my 2024 list only because I recently learned about it.
I should have learned about this sooner!

Polling is a technique that web clients can use
to get changing data from a server.
The easiest way to implementing polling is to
send requests to a server at fixed time intervals,
such as once every 30 seconds.

Polling is useful in many scenarios such as getting updates to
sports scores, stock prices, and weather forecasts.

Fixed interval polling has drawbacks.
The data presented can be out-of-date until the next request/response completes.
Also, there is more network traffic and server load than necessary
because many consecutive requests can result in the same response.

Long polling addresses the issues with fixed interval polling.
The client sends an initial request for data.
The server does not respond until there is a change in the data to be reported.
When the client receives the data, it immediately requests the next update.

For more detail, see my <a href="/blog/topics/#/blog/long-polling/"
target="_blank">Long Polling</a> blog page.

## Server-Sent Events (SSE)

<a href="https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events"
target="_blank">Server-Sent Events</a> (SSE) are used to send data using TCP
from a server to a client, but not in the other direction.

Server-Sent Events is in my 2024 list only because I recently learned about them.
I should have learned about them sooner!

For two-way communication, consider using
<a href="/blog/topics/#/blog/websockets/" target="_blank">WebSockets</a>.
WebSockets support text and binary data, whereas SSE only supports UTF-8 text.
When only text is needed, SSE is a good option because
the required code is a bit easier to write than the code for WebSockets.

SSE provides automatic reconnection.
If a SSE connection is closed, perhaps due to a network issue,
clients will automatically attempt to reconnect to the server.
WebSockets do not provide this, so code must be written
to listen for disconnects and reconnect later.
This feature alone may be a good reason to choose SSE over WebSockets.

For more detail, see my <a href="/blog/topics/#/blog/server-sent-events/"
target="_blank">Server-Sent Events</a> blog page.

## WebSockets

<img alt="WebSocket logo" style="border: 0; width: 20%"
  src="/blog/assets/websocket-logo.svg?v={{pkg.version}}">

<a href="https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_client_applications"
target="_blank">WebSockets</a> are a standardized protocol for
two-way communication between clients and servers using TCP.
They are widely supported by web browsers.

WebSockets are in my 2024 list because it is important to know
how they differ from long polling and Server-Sent Events.
There are situations where any of these three options is the best choice.

For one-way communication from servers to clients, consider using
<a href="/blog/topics/#/blog/server-sent-events/"
target="_blank">Server-Sent Events</a> (SSE).
WebSockets support text and binary data, whereas SSE only supports UTF-8 text.

WebSockets require upgrading an existing HTTP connection.
For details, see the MDN page {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/HTTP/Protocol_upgrade_mechanism",
"Protocal upgrade mechanism" %}.

Many WebSocket libraries for server-side programming languages
and environments exist.

For more detail, see my <a href="/blog/topics/#/blog/websockets/"
target="_blank">WebSockets</a> blog page.

## Zig

<img alt="Zig logo" style="border: 0; width: 20%"
  src="/blog/assets/zig-logo.svg?v={{pkg.version}}">

<a href="https://ziglang.org" target="_blank">Zig</a>
is a free, open source (MIT license),
high performance, systems programming language.
It is a modern alternative to C with similar syntax
such as statements terminated with semicolons and conditions in parentheses.

Zig is my 2024 list for two reasons.
First, it is the primary primary programming language
used to implement Bun, which is also in my list.
Second, it is great alternative to
systems programming languages like C++ and Rust.

Interest in Zig increased due to its use in the implementation of
<a href="https://bun.sh" target="_blank">Bun</a>, a JavaScript/TypeScript
run-time and toolchain.
Bun has many advantages over Node.js and Deno including much better performance.

Zig provides a complete, LLVM-based toolchain for creating, developing,
building, and testing apps written in Zig, C, and C++.
There are advantages to building apps with the Zig compiler
even if they have no Zig code and only use C and/or C++ code.

Zig is suitable for applications that care deeply about
performance, memory usage, and/or binary size.
Often these concerns justify the tedium of
manual memory management that is required
due to lack of automated garbage collection.

A major appeal of Zig is that it is simpler than C++ and Rust and safer than C.
However, Zig does not provide the same level of memory safety as Rust.

Zig emphasizes:

- No hidden control flow

  Examples of hidden control flow in other languages include
  exception handling, operator overloading, destructors, and decorators.

- No hidden memory allocations

  All memory allocation is performed by allocators that the developer selects.
  Each kind of allocator implements a different allocation strategy.
  Zig does not support closures, so allocations do not outlive their scope.

- No preprocessors or macros

  In place of these, Zig uses code that runs at compile-time,
  indicated by the `comptime` keyword.

- Having only one way to accomplish each task.

Zig includes:

- a package manager
- a build system that is simpler that the
  combinations of build tools typically used with C and C++
- a build system API (used in `build.zig` files)
- cross-compilation support
- a test runner
- ability to target all platforms supported by LLVM, including WebAssembly

Zig is not an object-oriented (OO) programming language.
There is no support for defining classes, using inheritance,
or using polymorphism.
However, Zig does support defining structs with methods
and for many applications that is close enough to OO.

Andrew Kelly began work on Zig in August, 2015 (first commit).
The first public release was in February, 2016.

Despite still being in beta, it has been adopted by many projects.
The current version of Zig is 0.11.0.
Zig is expected to reach 1.0 in 2025, after 10 years of work.
Rust took nine years to reach 1.0, so the time frames are similar.

For more detail, see my
<a href="/blog/topics/#/blog/zig/" target="_blank">Zig</a> blog page.

## Zod

<img alt="Zod logo" style="border: 0; width: 20%"
  src="/blog/assets/zod-logo.svg?v={{pkg.version}}">

<a href="https://zod.dev" target="_blank">Zod</a>
is a TypeScript library for validating JavaScript values.
It is commonly used to validate HTTP requests, including
request headers, path parameters, query parameters, and request bodies.

Zod is in my 2024 list because it provides
a great way to validate HTTP requests in endpoints implementing using Hono,
which is a great library to use in conjunction with htmx.

For example:

```ts
// Validate that a path parameter value is a positive number.
const idSchema = z.object({
  id: z.coerce.number().positive()
});
const idValidator = zValidator('param', idSchema);
app.delete('/todos/:id', idValidator, (c: Context) => { ...});

// Validate that a request body holds a form property
// named "description" whose value is a non-empty string.
const todoSchema = z
  .object({
    description: z.string().min(1)
  })
  .strict(); // no extra properties allowed
const todoValidator = zValidator('form', todoSchema);
app.post('/todos', todoValidator, async (c: Context) => { ... });
```

For more detail, see my
<a href="/blog/topics/#/blog/zod/" target="_blank">Zod</a> blog page.
