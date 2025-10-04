---
eleventyNavigation:
  key: Accessibility
layout: topic-layout.njk
---

## Overview

Much of this content was derived from an excellent talk
"Top 6 Accessibility Mistakes and How to Fix Them"
by Tyler Hawkins at the 2025 iJS New York conference.

Every year [WebAIM](https://webaim.org/projects/million/) evaluates
the top one million web site home pages for accessibility errors
using WAVE and publishes a report.

In the 2025 evaluation, 95% of all errors found fell into six categories.

TODO: Add something here.

## Top Errors

### Low Contrast Text (79% of pages)

The contrast between text background and foreground colors
should be at least:

- 3:1 for normal text that is 18 point (24px) or larger
- 3:1 for bold text that is 14 point (18.67px) or larger
- 4.5:1 for all text smaller than the above

In Chrome, to see the contrast for a particular element in a popup,
open the DevTools, click the inspect button in the upper left,
and hover over an element.

It is particularly challenging to achieve good color contrast for
text that has a background image containing a wide variety of colors.
This is especially true for responsive sites where the text moves
to a different part of the background image for different screen sizes.

### Missing alt Text (56% of pages)

All `img` elements should include the `alt` attribute
whose value describes the image.
The description should not include works like "image" or "picture"
because the browser knows it is one of those.
For images that are purely decorative, set `alt` to an empty string.

### Missing Labels (48% of pages)

All form control elements such as `input`, `textarea`, and `select`
should have an associated `label` element.
Other elements that play the role of a form control should include
the `aria-label`, `aria-labelledby`, or `title` attribute.

### Empty Links (45% of pages)

Anchor (`a`) elements whose content is an image rather than text
should specify a concise name using the
`aria-label` or `aria-labelledby` attribute.

### Empty Buttons (30% of pages)

`button` elements, and elements that act as buttons,
whose content is an image rather than text
should specify a concise name using the
`aria-label` or `aria-labelledby` attribute.

### Missing Language (16% of pages)

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
