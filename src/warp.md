---
eleventyNavigation:
  key: Warp Terminal
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.warp.dev", "Warp" %} is
"the terminal for the 21st century".

Currently it only runs in macOS, but there are plans to support Linux and Windows in the future.

Warp is implemented in Rust.

## Installing

To install Warp using Homebrew, enter `brew install warp`.
This creates `Warp.app` in your `Applications` directory.

## Running

Double-click `Warp.app` in your `Applications` directory to launch it.
The first launch will prompt you to sign up.
Subsequent launches will prompt you to sign in.

## Blocks

Entering a shell command creates a "block".

There are two ways to select a previous block:

1. Scroll up until it is visible and click it.
2. Click any block and using the up and down arrow keys to navigate to it.

To perform an action on a block, right-click it or
click the vertical ellipsis in the upper-right corner of the block.

Block actions include:

- Copy Command
- Copy Output
- Copy Both
- Create Permalink...
- Copy Prompt
- Copy Working Directory
- Copy Git Branch
- Find Within Block
- Toggle Bookmark
- Split pane right
- Split pane left
- Split pane down
- Split pane up
- Toggle maximize pane
- Close pane

# Command Palette

To open the command palette, press cmd-p.

# Themes

To choose a theme

# Panes

To create a new pane, right-click and select one of the "Split pane" options
to open a new pane on the right, left, down (bottom), or up (top).

To close the currently focused pane, enter "exit" or press cmd-w.

To close any pane, right-click in it and select "Close pane".

To navigate to the next pane, press cmd-].

To navigate to the previous pane, press cmd-[.

To resize panes, drag a divider line or press cmd-ctrl-{any-arrow-key}.

# Tabs

To open a new tab, click the "+" to the right of the last open tab.

To rename a tab, right-click it, select "Rename",
enter the new name, and press return.
