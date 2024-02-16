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

The easiest way to get started using Lit is to get it from this {% aTargetBlank
"https://cdn.jsdelivr.net/gh/lit/dist@3/core/lit-core.min.js", "CDN" %}.
This single file can be saved locally to avoid depending on the CDN.
The file is 17 KB.
