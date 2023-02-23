---
eleventyNavigation:
  key: Warp Terminal
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.warp.dev", "Warp" %} is
"the terminal for the 21st century".

Currently it only runs in macOS, but there are plans to
support Linux and Windows in the future.

Warp functions more like a standard text editor than most terminal programs.
The cursor can be positioned by using the arrow keys
or clicking anywhere in a command.
Any text can be selected, copied, and pasted.

Warp currently only supports the shells Xsh, Bash, and Fish.
If your default shell is not one of these, Warp will default to Zsh.
The Warp team has stated that they would like to support Nushell in the future.

Warp is implemented in Rust.

Vim keybindings are not currently supported, but
the Warp team has stated that they want to add support.
See {% aTargetBlank "https://github.com/warpdotdev/warp/issues/159",
"issue 159" %}.

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

  - General section

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

    - "Show warning before quitting" (on by default)

      When this is on and an attempt is made to quit Warp
      while there are processes running,
      a dialog will appear allowing you to verify that you
      really want to quit which will kill the processes.
      Click the "Show running processes" button to see a list of the processes.

  - Session section

    - "Honor user's custom prompt (PS1)" (off by default)

      When this is on, custom prompts created in shell configuration files
      or by utilities like [Starship](/blog/topics/blog/starship)
      are used.
      Multi-line prompts may not work.

  - Keys section

    None of the options here are particularly interesting
    and the default settings seem fine.

  - Editor section

    - "Open completions menu as you type" (off by default)

      When this is on, a completions menu is display while typing commands.
      For example, typing "git" followed by a space displays all the possible
      git commands followed by brief descriptions.
      This can be both very helpful and very annoying.

    None of the remaining options here are particularly interesting
    and the default settings seem fine.

  - Terminal section

    None of the options here are particularly interesting
    and the default settings seem fine.

- Shared blocks

  This lists all the permalinks that have been created.
  See the description of permalinks in the "Blocks" section.

- Keyboard shortcuts

  The keyboard shortcuts for all commands can be customized here.
  Some commands do not have a default keyboard shortcut.
  One example is "Rename the Current Tab".

  To see all the current keyboard shortcuts at any time, press cmd-/.

- Privacy

  This contains options to send app analytics and crash reports
  to the Warp team to help them improve the product.
  Both options are on by default.

  Click "Close settings and view network logging" to
  add a command to the current pane that will
  display all the communications being sent to the Warp team.
  After viewing it, press ctrl-c to return to the pane.

- About

  This displays the current version of Warp.

## Blocks

Entering a shell command creates a new "block".

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

Creating a permalink provides a URL that can be shared with others
to view the command and output for a given block.
A dialog will appear.
Click the "Create and Copy Link" button.
This displays the URL and automatically copies it to the clipboard.

Permalinks remain active until they are deleted.
To see all the permalinks that have been created,
open the Settings dialog by pressing cmd-,.
Then select "Shared blocks" in the left nav.
To delete a permalink, click its vertical ellipsis and select "Unshare".
Confirm this by clicking the "Unshare" button in the dialog that appears.

## Commands

When entering a command, a suggested completion will appear in dimmed text.
To accept the suggestion, press the right arrow key.

To execute a command, press the return key.

To recall a command previously entered in the current pane,
repeatedly press the up arrow key until the command is highlighted
and press the return key to execute it.

To search command history, workflows, and AI commands, press ctrl-r.
To limit the search to one of these three categories,
click the "history", "workflows", or "AI command search" button.
Then enter text to perform a fuzzy search.
For example, entering "start" will find any command containing those letters
in that order, but not necessarily together.
This would match "fastlane screenshots".

## Command Editing

There are many keyboard shortcuts that trigger text editing commands.
The following table summarizes the most useful shortcuts.

| Command               | Keyboard Shortcut |
| --------------------- | ----------------- |
| Delete All Left       | cmd-delete        |
| Delete All Right      | cmd-fn-delete     |
| Delete Word Left      | option-delete     |
| Delete Word Right     | option-fn-delete  |
| Insert Newline        | ctrl-j            |
| Move to Start of Line | cmd-left-arrow    |
| Move to End of Line   | cmd-right-arrow   |
| Select All            | cmd-a             |
| Clear Command Editor  | ctrl-c            |
| Clear Screen          | ctrl-l            |

For more keyboard shortcuts, see the {% aTargetBlank
"https://docs.warp.dev/features/keyboard-shortcuts#input-editor",
"Keyboard Shortcuts" %}.

## AI Commands

To generate a command using an AI search, press ctrl-`.
Enter English text describing the desired command and press return.
For example, entering "delete local git branch"
suggests "git branch -d branch_name".
To accept this suggestion, click the "Input Command" button or press cmd-return.
Replace any placeholders and press return to execute the command.

Sometimes the suggested commands are correct for Linux and not macOS.
To get macOS equivalents, and "on mac" to the description.

## Workflows

Workflows are named sets of commands (typically just one)
that are parameterized.
They are similar to snippets in code editors.

To search for a workflow, press cmd-shift-r.
In the popup that appears, select a category in the left nav such as "git".
Then click one of the workflows to copy it to the current block.
Replace any placeholders and press return to execute the workflow.

To create custom workflows ... TODO: FINISH THIS!

## Command Palette

To quickly find and execute a Warp command (not a shell command),
open the command palette by pressing cmd-p,
filter the list of commands by typing part of its name,
and click a command.

## Themes

To choose a theme, open the command palette and select "Open Theme Picker"
or press cmd-ctrl-t.

Each theme is defined by a `.yaml` file.

To use or create a custom theme, see {% aTargetBlank
"https://docs.warp.dev/appearance/custom-themes", "Custom Themes" %}.

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
