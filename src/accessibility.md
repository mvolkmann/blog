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
For images that are purely decorative, set `alt` to an empty string
to hide the image from assistive technologies.

Good `alt` values describe the intent of the image.
For example, suppose we have an `img` element that displays a trash can and
that is inside a `button` whose purpose is to delete something when clicked.
The `alt` value of the `img` should be "delete", not "trash can".

The description should not include words like "image" or "picture"
because the browser knows it is one of those.

The `alt` attribute cannot be added to `svg` elements.
Instead, `svg` elements should include a `title` child element.

### Missing Labels (48% of pages)

All form control elements such as `input`, `textarea`, and `select`
should have a visible label. For example:

```html
<label for="last-name-input">Last Name</label>
<input id="last-name-input" name="lastName" />
```

Typically this is achieved by including associated `label` elements.
Other elements that play the role of a form control should include
the `aria-label`, `aria-labelledby`, or `title` attribute.

The `placeholder` attribute is not a suitable alternative
to the approaches described above.
Formatting requirements should always be visible
rather than describing them in `placeholder` attribute
since that text disappears as soon as the user begins entering a value.

If the design of a page calls for using `placeholder` attributes
instead of `label` elements, do something like the following:

```html
<input aria-label="last name" name="lastName" placeholder="Last Name" />
```

### Empty Links (45% of pages)

Anchor (`a`) elements whose content is an image rather than text
should specify a concise name using the `alt` attribute.

### Empty Buttons (30% of pages)

`button` elements, and elements that act as buttons,
whose content is an image rather than text
should specify a concise name using the
`aria-label` or `aria-labelledby` attribute.

### Missing Language (16% of pages)

The `html` element should always include the `lang` attribute.
It does not default to English (`en`).
The value can specify only a language such as "fr" for French
or a language and a locale such as "fr-CA" for French Canadian.

The `lang` attribute can be added to any HTML element that can include text.
This is useful when some elements contain text written in
a different language than the one specified on the `html` element.

When this attribute is missing, several issues can occur.

1. Assistive technologies that speak text
   can choose the wrong sound library or phonetic rules,
   resulting in incorrect pronunciations.

1. Assistive technologies can choose to
   read text using the default language of the user
   despite the page text not being written in that language.

1. Assistive technologies that present Braille text
   can translate the text incorrectly

1. Language translation tools can provide incorrect translations
   if they cannot correctly determine the source language.

1. Search Engine Optimization (SEO) tools can file to correctly tag the content.
   a
1. Web browsers can apply incorrect language-specific styling,
   hyphenation, and spell checking.

## Other Issues

Verify that all form controls can be
navigated to and modified using only the keyboard.

## Tools

The tools described below are recommended for detecting accessibility issues.
Each of these can be installed as a Chrome browser extension.

These tools typically only detect up to 30% of accessibility issues,
so human testing is also needed.
For example, tools can verify that all `img` elements have an `alt` attribute,
but they cannot verify that the `alt` value is good.

### axe DevTools - Web Accessibility Testing

To install this, launch Chrome, browse the
[chrome web store](https://chromewebstore.google.com/detail/axe-devtools-web-accessib/lhdoppojpmngadmnindnejefpokejbdd?hl=en-US)
link, and click the "Add to Chrome" button.

To run this, open the DevTools and select the "axe DevTools" tab.
Then click one of the large buttons such as "Full Page Scan".

### Accessibility Insights for Web

To install this, launch Chrome, browse the
[chrome web store](https://chromewebstore.google.com/detail/accessibility-insights-fo/pbjjkligggfmakdaogkfomddhfmpjeni?hl=en)
link, and click the "Add to Chrome" button.

To run this, click the extensions button (looks like a jigsaw puzzle piece)
and select "Accessibility Insights for Web" from the drop down menu.
Then click one of the following options:

- FastPass: runs in under five minutes
- Quick Assess: can run from around 30 minutes
- Assessment: a guided process
- Ad hoc tools: visualizations that identify issues

### WAVE Evaluation Tool

To install this, launch Chrome, browse the
[chrome web store](https://chromewebstore.google.com/detail/wave-evaluation-tool/jbbplnpkjmmeebjpijfedlgcdilocofh)
link, and click the "Add to Chrome" button.

To run this, click the extensions button (looks like a jigsaw puzzle piece)
and select "WAVE Evaluation Tool" from the drop down menu.
