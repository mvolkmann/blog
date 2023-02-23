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

## Hierarchy

Windows hold tabs.

Tabs hold panes.

Panes hold blocks.

Blocks hold a command and its output.

## Installing

To install Warp using Homebrew, enter `brew install warp`.
This creates `Warp.app` in your `Applications` directory.

## Running

Double-click `Warp.app` in your `Applications` directory to launch it.
The first launch will prompt you to sign up.
Subsequent launches will prompt you to sign in.

When quitting Warp if there are any processes running in blocks,
a dialog will appear allowing you to verify that you
really want to quit which will kill the processes.
Click the "Show running processes" button to see a list of the processes.

## Settings

To view and modify settings, select Warp ... Settings ... Settings
or press cmd-,.

The left nav of the Settings dialog contains the following sections:

- Account

  This displays the logged in username and current version of Warp.

- Appearance

  When "Sync with OS" is on, Warp switches between light and dark mode
  based on the current OS setting.
  It is off by default.

  The "Window Opacity" can be adjusted to enable
  seeing content behind Warp windows.
  It is 100% by default.

  When the Panes "Dim inactive panes" is on, all panes except the current one
  are dimmed to make it clear which pane is active.
  It is off by default.

  When the Blocks "Compact mode" is on, the vertical space
  around the horizontal lines that separate blocks is removed.
  It is off by default.

  In the Text section, the "Terminal font", "Font size", and "Line height"
  can be set.
  I am using "FiraCode Nerd Font Mono" which supports ligatures.

  In the Cursor section, the "Blinking cursor" option can be toggled.
  It is on by default.

- Features

  This section contains a large number of settings.
  The highlights are:

  - "Restore windows, tabs, and panes on startup" (on by default)

    When this is on, new Warp sessions begin with all the
    windows, tabs, and panes of the previous session.
    However, processes that were running will not be automatically restarted.

  - "Show sticky command header" (on by default)

  - "Show tooltip on click on links" (on by default)

    Hovering over a URL changes it to be blue and underlined.
    Command-clicking it opens the URL in the default web browser.
    When this option is on and a URL is clicked, a tooltip appears
    containing "Open Link" that can be clicked to open the URL.

  - "Choose an editor to open file links" ("Default App" by default)
    can be changed to open all file links in VSCode.

    One way to get file links is to run the `ls` command.
    Hovering over any file name that is output
    changes it to be blue and underlined.
    Command-clicking it opens the file in the default editor app.
    Clicking it displays a tooltip appears containing "Open File" or
    "Open Directory" that can be clicked to open the file
    or open the directory in a Finder window.

- Shared blocks
- Keyboard shortcuts
- Privacy
- About

## Blocks

Entering a shell command creates a "block".

Blocks are divided by thin horizontal lines.
By default there is a lot of vertical space on both sizes of these lines.
To remove the space, open Settings, select Appearance,
and toggle on "Compact mode".

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

## Commands

When entering a command, a suggested completion will appear in dimmed text.
To accept the suggestion, press the right arrow key.

To execute a command, press the return key.

## Command Palette

To open the command palette, press cmd-p.

## Themes

To choose a theme, open the command palette and select "Open Theme Picker"
or press cmd-ctrl-t.

## Panes

Each pane can hold any number of blocks.

To create a new pane, right-click and select one of the "Split pane" options
to open a new pane on the right, left, down (bottom), or up (top).

To close the currently focused pane, enter "exit" or press cmd-w.

To close any pane, right-click in it and select "Close pane".

To navigate to a pane in a specific direction, press cmd-option-{any-arrow-key}.

To navigate to the next pane, press cmd-].

To navigate to the previous pane, press cmd-[.

To resize panes, drag a divider line or press cmd-ctrl-{any-arrow-key}.

To clear all the blocks in the current pane,
open the command palette and select "Clear Blocks",
enter the command "clear block", or press cmd-k.

## Tabs

Each tab can contain any number of panes.

To open a new tab, click the "+" to the right of the last open tab.

To rename a tab, right-click it, select "Rename",
enter the new name, and press return.

To change the color of the bottom border and background gradient of a tab,
right-click it, and select one of the six theme colors
at the bottom of the popup.

To navigate to the tab to the right, press cmd-}.

To navigate to the tab to the left, press cmd-{.

To reorder tabs, drag them left or right.

## Windows

Each window can contain any number of tabs.

To open a new window, select File ... New Window or press cmd-n.
