---
eleventyNavigation:
  key: Alfred
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.alfredapp.com", "Alfred" %} is a macOS app
that provides a more powerful replacement for the built-in Spotlight app.

This is a free app, however many features are
only available if the "PowerPack" is purchased.

## Preferences

To configure Alfred, launch the "Alfred Preferences" app
by clicking the menubar icon that looks like a bowler hat
and selecting "Preferences...".

## Opening

By default, an Alfred search box is opened by pressing option-space.
To modify this, open the preferences, select "General" in the left nav,
and change the "Alfred Hotkey".

## Calculator

To perform a mathematical computation and place the result on the clipboard,
open the Alfred search box, enter "=" followed by a mathematical expression
(ex. "=sin(45)"), and press enter.
Then paste where the result is desired.

The supported functions are:

- sqrt
- abs, ceil, floor, round, trunc, rint (nearest integer)
- dtor (degrees to radians), rtod (radians to degrees)
- sin, cos, tan
- asin, acos, atan
- sinh, cosh, tanh
- asinh, acosh, atanh
- log, log2, ln, exp
- near (?)

## File Actions

To open a list of actions that can be performed on a selected file,
press the right arrow key

## Clipboard

To enable clipboard management,
open Alfred, select "Features" in the left nav,
select "Clipboard History", and check "Keep Plain Text" and "Keep Images".

Every time text or images are copied to the clipboard,
they will be saved in the Alfred clipboard history.
To view them, open the Alfred search input and type "clipboard"
or press the hotkey which defaults to cmd-option-c.
To paste a saved clipboard entry, click it or
navigate to it with the up and down arrow keys and press return.

## Snippets

To create snippets, open Alfred, select "Features" in the left nav,
and select "Snippets".

Create collections to hold groups of related snippets.
Each collection has a name and can have "affix" characters
that must be typed before snippet keyword in the collection.
A common affix is a single exclamation point.

Create snippets inside the collections.
Each snippet has a name, a keyword, and snippet (expansion text).
Check "Auto expansion allowed" to allow the snippet to be used
by simply typing the affix character(s) followed by the keyword.
For example, I created a snippet named "full name" with the keyword "rmv".
Typing "!rmv" expands to "R. Mark Volkmann".

Snippet text can include the following {% aTargetBlank
"https://www.alfredapp.com/help/workflows/advanced/placeholders/",
"dynamic placeholders" %}:

- `{clipboard}`
- `{cursor}` - moves the cursor to this spot after expanding
- `{date}`
- `{datetime}`
- `{time}`

These can include a format specifier.
For example, `{date.long}` and `{time -10m -30s:long}`.

## 1Password Integration

To enable access to 1Password from Alfred:

1. Open 1Password preferences, click the Advanced tab,
   and check "Enable integration with 3rd party apps".
1. Open Alfred, select "Features" in the left nav,
   select "1Password", and check "Enable 1Password Bookmarks".

This supports quickly logging into any site
whose URL and credentials are stored in 1Password.
For example, enter "1p bank" in the Alfred search box
to login to the Bank of America website.

TODO: Much more is coming soon!
