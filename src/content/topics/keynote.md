---
navigation:
  key: Keynote
layout: topic-layout.njk
---

## Overview

This page documents selected features of the macOS app Keynote
for creating slide presentations.
It is far from comprehensive.

## Build Effects

Build effects gradually display the contents of a slide.
For example, in a slide with bullet points,
text boxes, and shapes (such as arrows),
you may wish to display the bullets one at a time and
display the text boxes and arrows when a specific bullet is appears.
To do this:

1. For each set of text boxes and shapes that should appear together ...
   1. Select each item in the set.
   1. Click the animation button (contains two overlapping diamonds)
      in the upper-right to show animation options.
   1. Click the "Add an Effect" button.
   1. Select "Appear".

1. Select the single set of bullets.
1. Click the animation button.
1. Click the "Add an Effect" button.
1. Select "Appear".
1. Change the "Deliver" dropdown value to "By Bullet" or "By Bullet Group".
1. Click the "Build Order" button at the bottom of the panel.
1. For each non-bullet item ...
   1. Drag it to below the bullet after which it should appear.
   1. Change its "Start" dropdown value to "With Build n" or "After Build n".

1. Optionally click the "Preview" button to see how
   all the items on the slide will animate into view.

"With Build" means that multiple items begin their animation at the same time.
"After Build" means that the animation of that item
begins after the animation of the preceding item.
When the effect is "Appear", the animation is instantaneous,
so there is no visible difference between "With Build" and "After Build".
