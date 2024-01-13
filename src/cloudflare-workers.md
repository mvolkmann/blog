---
eleventyNavigation:
  key: Cloudflare Workers
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="Hono logo" style="border: none; width: 30%"
  src="/blog/assets/cloudflare-workers.svg?v={{pkg.version}}"
  title="Hono logo">

## Overview

{% aTargetBlank "https://workers.cloudflare.com", "Cloudflare Workers" %}
host edge functions.

Features provided include:

- automatic scaling

  There is no need to configure auto-scaling or load balancers.

- high performance global network

  Cloudflare workers run in a network of data centers that use V8 isolates
  that have very low latency (approximately 25ms in my testing).

- write in a variety of programming languages,
  including JavaScript, TypeScript, Rust, C, and C++.

- run instantly without cold starts

- affordable

  The first 100,000 requests each day are free.
  After that the cost is $5 USD per 10 million requests.

- no servers to maintain

- provides edge storage of static assets using "Workers KV"

- can generate assets at runtime,
  including images, SVGs, PDFs, and more

## Projects

To create a new project that uses Hono,
enter `npm create cloudflare -- {app-name}`.
This will prompt for:

- In which directory do you want to create you application?
- What type of application do you want to execute?

  - "Hello World" Worker
      - Website or web app
       - Example router & proxy Worker
       - Scheduled Worker (Cron Trigger)
       - Queue consumer & producer Worker
       - ChatGPT plugin
       - OpenAPI 3.1

- Do you want to use TypeScript?
- Do you want to use git for version control?
- Do you want to deploy your application?

  This will open a browser window where you can sign up for an account
  or log in if you already have an account.
  You will also be prompted in a browser window to
  allow "wrangler" to make changes in your account.

If you chose to deploy the application,
it will be opened in a new browser window.

When it finishes, the target directory will be created
and will contain the following files:

- `node_modules` directory
- `package.json`
- `package-lock.json`
- `src/index.ts`
- `tsconfig.json`
- `wrangler.toml`

Run the project locally by entering `npm run dev`.
Then browse localhost:8787.
It provides hot reloading of the local server,
but not hot reloading of the web browser.
