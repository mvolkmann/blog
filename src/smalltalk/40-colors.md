---
eleventyNavigation:
  key: Colors
  order: 40
  parent: Smalltalk
layout: topic-layout.njk
---

The `Color` class defines many methods for creating and operating on colors.
It defines the following class methods that create colors by name:

- `black`, `blue`, `brown`, `cyan`, `gray`, `green`, `magenta`,
  `orange`, `pink`, `purple`, `red`, `tan`, `white`, `yellow`
- `lightBlue`, `lightBrown`, `lightCyan`, `lightGray`, `lightGreen`, `lightMagenta`,
  `lightOrange`, `lightRed`, `lightYellow`
- `darkGray`, `veryDarkGray`, `veryLightGray`, `veryVeryDarkGray`, `veryVeryLightGray`
- `transparent`

To get a random color, send the `#random` message to the `Color` class.

To set the opacity of a color, send the `#alpha:` message to a `Color` object
with an argument that is a float value between
zero (fully transparent) and one (fully opaque).
