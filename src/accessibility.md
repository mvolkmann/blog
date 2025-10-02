---
eleventyNavigation:
  key: Accessibility
layout: topic-layout.njk
---

## Overview

TODO: Add something here.

## Top Mistakes

96% of all accessibility errors fall into the following six categories.

### Low Contrast Text

The contrast between text background and foreground colors
should be at least 4.5:1.

### Missing alt Text

All `img` elements should include the `alt` attribute
whose value describes the image.
The description should not include works like "image" or "picture"
because the browser knows it is one of those.
For images that are purely decorative, set `alt` to an empty string.

### Missing Labels

All form control elements such as `input`, `textarea`, and `select`
should have an associated `label` element.

### Empty Links

Anchor (`a`) elements whose content is an image rather than text
should specify a concise name using the
`aria-label` or `aria-labelledby` attribute.

### Empty Buttons

`button` elements, and elements that act as buttons,
whose content is an image rather than text
should specify a concise name using the
`aria-label` or `aria-labelledby` attribute.

### Missing Language

The `html` element should always include the `lang` attribute.
It does not default to English (`en`).
When this attribute is missing, assistive technologies can
(TODO: Describe what can go wrong.)

## Tools

The following tools are recommended for detecting accessibility issues.
Each of these can be installed as a browser extension.
These tools typically only detect up to 30% of accessibility issues,
so human testing is also needed.

### axe

### Accessibility Insights fo Web

### WAVE
