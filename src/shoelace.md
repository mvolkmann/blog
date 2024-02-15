---
eleventyNavigation:
  key: Shoelace
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 50%">
  <img alt="Shoelace logo" style="border: 0"
    src="/blog/assets/shoelace-logo.svg?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://shoelace.style/", "Shoelace" %}
is a "forward-thinking library of web components".

## Installing

The easiest way to use Shoelace is from a CDN.
Include the following in the `head` element
of each web page that wishes to use Shoelace.
Modify the version number in each URL to use the latest version.

```html
<link
  rel="stylesheet"
  href="https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.13.1/cdn/themes/light.css"
/>
<script
  type="module"
  src="https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.13.1/cdn/shoelace-autoloader.js"
></script>
```

This loads the light theme.
To instead load the dark theme, change "light" to "dark" in the CSS URL.
