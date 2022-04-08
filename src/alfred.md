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
