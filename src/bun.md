---
eleventyNavigation:
  key: Bun
layout: topic-layout.njk
---

These notes are currently in a very rough form!

Compare performance of bun, deno, and node on your rush-hour code. Implement your rush-hour program in Zig. Compare performance.

Is the only plan for profit to provide hosting?ar
“Oven has decided to adopt a strategy twinned with Deno Company. They will launch sales of a cloud architecture based on Bun.”

Create code examples that demonstrate the most important bun-specific APIs.

Does it share module code across projects like pnpm?

See https://bun.sh

What is it?

- a free, open source, “complete” toolkit for JavaScript and TypeScript
- created by startup company Oven.sh because that’s what buns come out of
- started in 2022 by Jarred Sumner with VC funding
- build, run, debug, test
- built on JavaScriptCore from Safari which has faster startup times than V8
- implemented in Zig and some C++; “Zig is sort of similar to writing C, but with better memory safety features in debug mode and modern features like defer (sort of similar to Go's) and arbitrary code can be executed at compile-time via comptime. It's really good for writing performant low-level software.”; “It has very few keywords so it's a lot easier to learn than, for example, C++ or Rust.”
- works with nearly all major web frameworks including Next.js, SvelteKit, Astro, Nuxt, and Fastify
- a package manager that works with npm packages
- became stable (1.0) in September 2023
- Oven will donate to the Zig Software Foundation to keep that project going

The Name

“A friend suggested the name “bun” because she has a bunny named bun. My initial reaction was "I'm not going to name it after your bunny." And then I thought about it more and it made some sense.”

Also “because it’s a bundling of the JavaScript ecosystem and a bundler”.

Why?

- simplifies current state of JS/TS tooling (transpilers, bundlers, testing tools, …); less to install and configure; easier to use
- provides significantly better performance
- supports both CommonJS (require) and ESM (import) in the same source files

Features

- The bun runtime replaces Node.js and supports nearly all Node.js builtin modules (around 40 of them; rarely used modules such as dgram, http2, inspector, and repl are not yet supported; v8 module is specific to Webkit which Bun does not use)
- Supports JSX/TSX files.
- hot reloading with “bun —hot {file} (watch mode equivalent of nodemon?)
- has a plugin API that is similar to that of esbuild, so many esbuild plugins work in bun without modification
- bun:test a Jest-compatible test runner; 8 times faster than Vitest and 13 times faster than Jest
- SQLite database built into the bun runtime; see bun:sqlite; 2 times faster than Deno and 4 times faster than Node in queries per second
- bun-specific, highly-optimized APIs that perform better than Node.js equivalents
  - Bun.file returns a File object with the same properties as DOM File objects; contents are lazily loaded; many async methods for reading content in different formats including text, json, stream, and ArrayBuffer; reads files up to 10 times faster
  - Bun.write to write many kinds of values including strings, buffers, files, and responses from HTTP requests; writes 3 times faster than Node.js
  - Bun.serve to start an HTTP server based on web APIs; handles 4 times as many requests per second as Node.js
  - Bun.env provides access to environment variables without using the dotenv package
  - Websocket support handles 5 times as many messages per second as Node.js and is easier to use
  - password hashing (uses Bcrypt and Argon)
  - many more APIs!

Performance

- 4 times faster than Node.js for a hello world program
- time to build and run TS code is 4 times faster than esbuild, 15 times faster than TSX, and 43 times faster than TSC + Node
- “bun run” can be used in place of “npm run” and is 5 times faster
- “bun install” is 29 times faster than npm and 17 times faster than pnpm; uses package.json files just like npm; uses a binary bun.lockb file instead of package-lock. json; can be used as a replacement for npm even when not using the bun runtime
- “bunx” is the equivalent of “npx”

To install locally, enter `curl -fsSL https://bun.sh/install | bash`.
Also describe how to use with Docker, Cloudflare, and others.

Concerns

- Was it a good idea to build with Zig, a new, not yet popular language?
- We’re the Node APIs implemented to have identical characteristics with no bugs?

More

- Elysia is a bun-specific server framework which is 18 times faster than Express
  - `bun create elysia project-name` generates a scaffolded app
  - `cd project-name`
  - `bun run dev` provides hot reloading
  - browse localhost:3000
  - bunx prisma init —datasource-provider mysql
- Hono is a close competitor to Elysia
