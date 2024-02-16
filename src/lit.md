---
eleventyNavigation:
  key: Lit
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 50%">
  <img alt="Lit logo" style="border: 0"
    src="/blog/assets/lit-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://lit.dev", "Lit" %}
is library that simplifies developing
{% aTargetBlank "https://www.webcomponents.org/", "web components" %}.

## Installing

The easiest way to get started using Lit is to
get the library from this {% aTargetBlank
"https://cdn.jsdelivr.net/gh/lit/dist@3/core/lit-core.min.js", "CDN" %}.
This single file can be saved locally to avoid depending on the CDN.
The file is 17 KB.

Another approach is to use {% aTargetBlank "https://vitejs.dev/", "Vite" %}.
This has many advantages including:

- configuration to use TypeScript
- running in watch mode so code changes are automatically recompiled
- automatic browser updates

The following steps create a new project that uses Vite and Lit.

1. Enter `npm create vite@latest`.
1. If prompted with "OK to proceed?", press the return key.
1. After the "Project name" prompt, enter a project name.
1. For the "Select a framework" prompt, select "Lit".
1. For the "Select a variant" prompt, select "TypeScript".
1. cd to the new project directory.
1. Install dependencies by entering `npm install` or `bun install`.
1. Run the project by entering `npm run dev` or `bun dev`.
1. Type "h" and press the return key for help.
1. Type "o" and press the return key to open a browser tab for localhost:5173.
