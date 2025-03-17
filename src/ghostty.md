---
eleventyNavigation:
  key: Ghostty
layout: topic-layout.njk
---

<video height="200" controls>
  <source src="/blog/assets/ghostty-boo.mp4" type="video/mp4">
  Your browser does not support the video tag.
</video>

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

## Auto Updates

In macOS, Ghostty can be configured to notify you about an available update.
It can also automatically download a new version, but will not install it.

Set the configuration option `auto-update` to `off`, `check`, or `download`.

Set the configuration option `auto-update-channel`
to `stable` or `tip` (latest development version).
The default is based on whether the currently running version is from the `tip`.

## Keyboard Shortcuts

| Action                  | Keyboard Shortcut      |
| ----------------------- | ---------------------- |
| toggle full screen      | globe f                |
| increase font size      | cmd-plus               |
| decrease font size      | cmd-minus              |
| open new window         | cmd-n                  |
| goto next window        | cmd-`                  |
| open new tab            | cmd-t                  |
| goto next tab           | cmd-shift-]            |
| goto previous tab       | cmd-shift-[            |
| create horizontal split | cmd-d                  |
| create vertical split   | cmd-shift-d            |
| reset font size         | cmd-0 (zero)           |
| close split             | cmd-w                  |
| zoom split              | cmd-shift-return       |
| goto previous split     | cmd-[                  |
| goto next split         | cmd-]                  |
| goto split above        | cmd-option-up arrow    |
| goto split below        | cmd-option-down arrow  |
| goto split left         | cmd-option-left arrow  |
| goto split right        | cmd-option-right arrow |
| resize split up         | cmd-ctrl-up arrow      |
| resize split down       | cmd-ctrl-down arrow    |
| resize split left       | cmd-ctrl-left arrow    |
| resize split right      | cmd-ctrl-right arrow   |

Each new tab begins with a single split.

Changing the font size only affects the currently focused split.
It would be nice to have a way to change the font size of all splits at once.

Closing the last/only split in a tab closes the tab.

"Zoom split" causes the currently focused split to use the entire window
and hides all other splits.
Doing it again returns to the previous view.

Tabs display their current working directory.
Currently tabs cannot be given a custom title, but that feature is coming soon.
It is available now if Ghostty is built from source.

It seems there is currently no way to swap the positions of existing splits.

## Configuration

To modify Ghostty settings, select Ghostty ... Settings... or press cmd-comma.
This will open a text file (seems to use TOML syntax)
in your default text editor.
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
background-opacity = 0.9
font-size = 16
macos-titlebar-style = tabs
theme = catppuccin-mocha

# Save the windows, tabs, and splits when exiting Ghostty
# so they are automatically restored when Ghostty is launched again.
window-save-state = always # This does not save split titles!

keybind = shift+ctrl+r=reload_config

# Empty values reset the configuration to default value.
font-family
```

## Themes

Ghostty ships with support for hundreds of color themes,
including all of them from
{% aTargetBlank "https://iterm2colorschemes.com", "Iterm2-color-schemes" %}.

To view the available color themes, enter the following in a split:

```bash
ghostty +list-themes
```

To exit, press q.

To select a color theme, edit the settings file,
add line containing `theme = {name}`, and reload the configuration.

To configure Ghostty to use different themes for light and dark mode
based on the OS light/dark mode, set the theme like the following:

```text
theme = dark:catppuccin-frappe,light:catppuccin-latte
```

## Fonts

Ghostty uses the "JetBrains Mono" font by default.
This font supports ligatures which are enabled by default.

To list the available fonts, enter `ghostty +list-fonts`.

To use a different font, edit the config file,
add line containing `font-family = {name}`, and reload the config file.

## Key Bindings

To list the current key bindings, enter `ghostty +list-keybinds`.

To change the key bindings, modify and reload the config file.
Add lines with the syntax `keybind = {trigger}={action}`.
For example, `keybind = ctrl-shift-c=clear_screen`.

The trigger can be a single key, a single key with modifiers,
or a sequence of keys.

Supported keys include:

- letters: a to z
- digits: 0 to 9
- punctuation: . , ; ' ` / \ + - = [ ]
- arrows: up down right left
- enter (return), escape, space, tab,
- function keys: F1 to F25
- shift, control, alt, super
- and more keys that are less frequently used in key bindings

In macOS, "alt" means the option key and "super" means the command key.

As of Ghostty version 1.1.3, the supported actions include:

- `adjust_selection`
- `clear_screen`
- `close_all_windows`
- `close_surface`
- `close_tab`
- `close_window`
- `copy_to_clipboard`
- `copy_url_to_clipboard`
- `crash`
- `csi`
- `cursor_key`
- `decrease_font_size`
- `equalize_splits`
- `esc`
- `goto_split`
- `goto_tab`
- `ignore`
- `increase_font_size`
- `inspector`
- `jump_to_prompt`
- `last_tab`
- `move_tab`
- `new_split`
- `new_tab`
- `new_window`
- `next_tab`
- `open_config`
- `paste_from_clipboard`
- `paste_from_selection`
- `previous_tab`
- `prompt_surface_title`
- `quit`
- `reload_config`
- `reset`
- `reset_font_size`
- `reset_window_size`
- `resize_split`
- `scroll_page_down`
- `scroll_page_fractional`
- `scroll_page_lines`
- `scroll_page_up`
- `scroll_to_bottom`
- `scroll_to_top`
- `select_all`
- `text`
- `toggle_fullscreen`
- `toggle_maximize`
- `toggle_quick_terminal`
- `toggle_secure_input`
- `toggle_split_zoom`
- `toggle_tab_overview`
- `toggle_visibility`
- `toggle_window_decorations`
- `unbind`
- `write_screen_file`
- `write_scrollback_file`
- `write_selection_file`

For more detail, see {% aTargetBlank "https://ghostty.org/docs/config/keybind",
"Custom Keybindings" %}.

## More Command-Line Interface (CLI)

The following options can be specified after the `ghostty` command
to obtain specific information:

| Option           | Information                                                     |
| ---------------- | --------------------------------------------------------------- |
| +boo             | returns the Ghostty ASCII animation (ctrl-c to exit)            |
| +crash-report    | to inspect and send crash reports                               |
| +help            | outputs help on Ghostty                                         |
| +list-actions    | outputs all Ghostty actions (can be bound to keys)              |
| +list-colors     | lists all predefined color names and their RGB codes            |
| +list-fonts      | lists all available fonts                                       |
| +list-keybinds   | lists all current key bindings                                  |
| +list-themes     | lists and previews all available themes (ctrl-c to exit)        |
| +show-config     | outputs the current values of all configuration options         |
| +show-face       | shows font face used to render a given codepoint or string      |
| +validate-config | validates configuration file, outputting nothing if valid       |
| +version         | outputs Ghostty version, Zig version used to build it, and more |

For help on a given option, enter `ghostty {option} --help`.

## Images

To render images in Ghostty:

- `brew install --cask kitty`
- `kitten icat {file-path}`

## Shaders

Shaders apply a graphical effect to the terminal background.
They are typically written in the OpenGL Shading Language (GLSL).
Shaders can create dynamic, animated, or stylized effects.

There is a collection of shaders for Ghostty at
{% aTargetBlank "https://github.com/hackr-sh/ghostty-shaders/",
"ghostty-shaders" %}.
Most of these work, but a few seem to have no effect.

For example, download the file `cineShader-Lava.glsl`
and add the following configuration option:

```toml
custom-shader = {path-to-shader-files}/cineShader-Lava.glsl
```

Reload the configuration to see a lava lamp effect in the Ghostty background.
This shader makes Ghostty much slower,
but the others don't seem to hurt performance.

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
