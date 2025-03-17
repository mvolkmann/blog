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

Ghostty supports multiple windows, tabs within a window,
and panes within a tab (referred to as "splits").

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

Changing the font size only affects the currently focused split.
It would be nice to have a way to change the font size of all splits at once.

Closing the last/only split in a tab closes the tab.

"Zoom split" causes the currently focused split to use the entire window
and hides all other splits.
Doing it again returns to the previous view.

Tabs display their current working directory.
They cannot be named.

## Settings

To modify Ghostty settings, select Ghostty ... Settings... or press cmd-comma.
This will open a TOML file in your default text editor.
In macOS this file is
`~/Library/Application Support/com.mitchellh.ghostty/config`.
In Linux this file is `$HOME/.config/ghostty/config`.
See the comments in this file for instructions.

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

The following is an example of a Ghostty config file.

```toml
background = 282c34
foreground = ffffff

keybind = shift+ctrl+r=reload_config

keybind = shift+ctrl+h=new_split:left
keybind = shift+ctrl+j=new_split:down
keybind = shift+ctrl+k=new_split:up
keybind = shift+ctrl+l=new_split:right

# Empty values reset the configuration to default value.
font-family
```

Setting `font-size` has no effect!

## Themes

To view the available color themes, enter the following in a split:

```bash
ghostty +list-themes
```

To select a color theme, edit the settings file,
add line containing `theme = {name}`, and reload the configuration.

## Fonts

Ghostty uses the "JetBrains Mono" font by default.
This font supports ligatures which are enabled by default.

To list the available fonts, enter `ghostty +list-fonts`.

To use a different font, edit the config file,
add line containing `font-family = {name}`, and reload the config file.

## Key Bindings

To list the current key bindings, enter `ghostty +list-keybinds`.

To change the key bindings, modify and reload the config file.

For more detail, see {% aTargetBlank "https://ghostty.org/docs/config/keybind",
"Custom Keybindings" %}.

## Images

To render images in Ghostty:

- `brew install --cask kitty`
- `kitten icat {file-path}`

## Comparison to Warp

| Topic          | Ghostty                        | Warp              |
| -------------- | ------------------------------ | ----------------- |
| focus          | performance                    | user experience   |
| AI integration | none                           | present           |
| features       | minimalistic                   | rich              |
| tabs           | only show working directory    | can be named      |
| configuation   | in a single configuration file | in a graphical UI |
| implemented in | Zig                            | Rust              |

Supposedly tab naming has been implemented.
See https://github.com/ghostty-org/ghostty/pull/4217.
But when I right-click in the body of a split,
the context menu does not contain "Set Title".

Warp supports blocks which hold a command and all its output.
Users can search within a block and
can scroll to the top or bottom of the output.
An alternative in Ghostty is to redirect command output to a file,
open the file in Vim with read-only mode,
and use its commands to navigate and view the output.
For example: `ls | vim -R`.

## Building from Source

Here are the steps to build the Ghostty app from source in macOS:

- Install Zig with `brew install zig`.
- Install Xcode from the "App Store" app.
- `git clone https://github.com/ghostty-org/ghostty.git`
- Open a terminal window and cd to the cloned directory.
- `zig build -Doptimize=ReleaseFast` (runs for several minutes)
- `cd macos`
- `xcodebuild`

To use this version of the app:

- `mv build/releaseLocal/Ghostty.app /Applications`
- Open the Finder.
- Navigate to Applications.
- Double-click ghostty.app.
