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
whose value

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
However, they typically only detect up to 30% of the issues,
so human text is also needed.
