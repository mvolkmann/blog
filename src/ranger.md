---
eleventyNavigation:
  key: Ranger
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://ranger.github.io", "Range" %}
is a Vim-inspired, terminal-based file manager.

## Installing

To install Ranger in macOS, install Homebrew and enter `brew install ranger`.
To run it, enter `ranger`.

## Help

For help, press the `?` key.
This displays a line at the bottom of the window
that describes other keys that can be pressed for specific help.

| Key | Description           |
| --- | --------------------- |
| `c` | commands              |
| `k` | key bindings          |
| `m` | man page              |
| `s` | view current settings |

Settings for ranger are defined in the file `~/.config/ranger/rc.conf`.

## Functionality

Ranger displays three columns.

The left column shows the contents of the parent directory.

The middle column shows the contents of the current directory.

The right column shows the contents of the file selected in the middle column
or a description of it.

## Navigating

To move the cursor to the next file or directory in the middle column,
press `j` or the down arrow.

To move the cursor to the previous file or directory in the middle column,
press `k` or the up arrow.

To open the file or directory under the cursor,
press `l`, the right arrow, or return.
Text files are displayed in the right column.
Non-text files are opened by their default application.
For example, in macOS image files are opened in the Preview app.

To navigate to the parent directory, press `h` or the left arrow.

To exit Ranger, press `q` or type `:q`.
