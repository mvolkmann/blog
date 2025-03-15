---
eleventyNavigation:
  key: Ghosty
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://ghostty.org", "Ghosty" %}
"is a fast, feature-rich, and cross-platform terminal emulator
that uses platform-native UI and GPU acceleration".
It is currently available for macOS and Linux.
Windows support is planned for the future.

Ghostty is primarily implemented in Zig.

All the keyboard shortcuts described below are for macOS.

## Installing

Browse {% aTargetBlank "https://ghostty.org", "ghostty.org" %}
and click the "Download" button, choose an installation method,
and follow the directions.

Once Ghostty is running, check for updates
by selecting Ghostty ... Check for updates...

## Keyboard Shortcuts

| Action                  | Keyboard Shortcut      |
| ----------------------- | ---------------------- |
| open new window         | cmd-n                  |
| open new tab            | cmd-t                  |
| create horizontal split | cmd-d                  |
| create vertical split   | cmd-shift-d            |
| increase font size      | cmd-plus               |
| decrease font size      | cmd-minus              |
| reset font size         | cmd-0 (zero)           |
| close split             | cmd-w                  |
| zoom split              | cmd-shift-return       |
| select previous split   | cmd-[                  |
| select next split       | cmd-]                  |
| select split above      | cmd-option-up arrow    |
| select split below      | cmd-option-down arrow  |
| select split left       | cmd-option-left arrow  |
| select split right      | cmd-option-right arrow |
| resize split up         | cmd-ctrl-up arrow      |
| resize split down       | cmd-ctrl-down arrow    |
| resize split left       | cmd-ctrl-left arrow    |
| resize split right      | cmd-ctrl-right arrow   |
| toggle full screen      | globe f                |

Each new tab begins with a single split.

Changing the font size only affects the current split.

Closing the last/only split in a tab closes the tab.

"Zoom split" causes the current split to use the entire window
and hides all other splits.
Doing it again returns to the previous view.

Tabs display their current working directory.
They cannot be named.

## Settings

To modify Ghostty settings, select Ghostty ... Settings... or press cmd-comma.
This will open the file
`~/Library/Application Support/com.mitchellh.ghostty/config`
in your default text editor.
See the comments in that file for instructions.

To see all the available configuration options and their default values,
enter the following in a split:

```bash
ghostty +show-config --default | sort
```

To also see documenation on the configuration options
enter the following in a split:

```bash
ghostty +show-config --default --docs
```

This outputs over 2500 lines!
Pipe the previous command to Vim to make it easier
to navigate and search the content.

After modifying the `config` file, return to Ghostty
and select Ghostty ... Reload Configuration or press cmd-shift-comma.

## Themes

To view the available color themes, enter the following in a split:

```bash
ghostty +list-themes
```

To select a color theme, edit the settings file,
add line containing `theme = {name}`, and reload the configuration.

## Font

Ghostty uses the "JetBrains Mono" font by default.
This font supports ligatures which are enabled by default.

To select a different font, edit the settings file,
add line containing `font-family = {name}`, and reload the configuration.

## Comparison to Warp

| Topic          | Ghostty                        | Warp                               |
| -------------- | ------------------------------ | ---------------------------------- |
| focus          | performance                    | user experience and AI integration |
| implemented in | Zig                            | Rust                               |
| features       | minimalistic                   | rich                               |
| configuation   | in a single configuration file | in a graphical UI                  |

Warp supports blocks which hold a command and all its output.
Users can search within a block and
can scroll to the top or bottom of the output.
