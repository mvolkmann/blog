---
eleventyNavigation:
  key: Vifm
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://vifm.info", "Vifm" %}
is a terminal-based file manager that uses Vim keybindings
and is implemented in C.
It is similar to Ranger, but is more heavily based on Vim.

For documentation see the
{% aTargetBlank "https://vifm.info/manual.shtml", "Manual" %}.

For key bindings see the
{% aTargetBlank "https://vifm.info/cheatsheets.shtml", "Cheatsheet" %}.

For color schemes see the
{% aTargetBlank "https://vifm.info/colorschemes.shtml", "Color Schemes" %}.

## Installing

See the "Download link" at the bottom of the
{% aTargetBlank "https://vifm.info", "home page" %}
for downloading the source or Windows executables.

To install Vifm in macOS, install Homebrew and enter `brew install vifm`.

## Launching

To start Vifm, enter `vifm`.

Vifm displays two panes by default that are views of two directories.
This makes it easy to copy and move files between directories.
These directories can be specified as
command-line arguments with `vifm` is started.
If they are not specified, it starts with
both panes showing the current directory.
However, Vifm remembers the last directory visited in each pane
and restores these in subsequent sessions.
To force starting in the current directory, start with `vifm .`.

To exit Vifm, enter `:q` just like quitting Vim.

## Help

For help, enter `:help` or just `:h`.
To exit help, enter `:q`.

## Navigating

| Direction                          | Keys                        |
| ---------------------------------- | --------------------------- |
| next item in current directory     | `j` or down arrow           |
| previous item in current directory | `k` or up arrow             |
| into child directory               | `l` or right arrow or enter |
| into parent directory              | `h` or left arrow           |

The status bar at the bottom of the window
shows information about the file or directory under the cursor.
This includes the read/write/execute permissions, owner:group,
size, and last modified date and time.

To open the item the cursor,
press `l`, the right arrow, or return.
Text files are displayed in the right column.
Non-text files are opened by their default application.
For example, in macOS image files are opened in the Preview app.
TODO: Verify this.

To create a bookmark for the current directory,
enter `:bmark` followed by a letter or press ???.
To navigate to a bookmark, press ???.
To list the current bookmarks, enter `:bmarks`.
To delete a bookmark, enter ???.
It seems like "marks" and "bookmarks" may be separate concepts.

## Functionality

To open the file under the cursor, press the return key.
Text files are opened in Vim.

## Commands

Vifm commands can entered by pressing `:` which
opens a line at the bottom for typing a command just like in Vim.

Common tasks are described in the table below.
The term "item" refers to a file or directory, often the one under the cursor.

| Task                                 | Key Binding            | Notes                                    |
| ------------------------------------ | ---------------------- | ---------------------------------------- |
| directory - change                   | `:cd path`             | can use environment variables in path    |
| directory - create                   | `:mkdir name`          |                                          |
| file - create                        | `:touch name`          |                                          |
| file - edit                          | enter                  | `:q` to quit editing                     |
| file - view when not in preview mode | `e`                    | `q` or `:q` to quit viewing              |
| help - display                       | `:help`                |                                          |
| hidden items - hide                  | `zm`                   |                                          |
| hidden items - show                  | `zo`                   |                                          |
| item - copy                          | `yy`                   | for yank                                 |
| item - delete/cut                    | `dd`                   |                                          |
| item - paste                         | `p`                    | after a delete or copy                   |
| item - rename name and/or extension  | `cw`                   |                                          |
| item - rename only name              | `cW`                   |                                          |
| item - undo delete                   | `u`                    |                                          |
| mark - create                        | `m` followed by letter | remembers selected item in directory     |
| mark - delete under cursor in list   | `dd`                   |                                          |
| mark - go to                         | `'` followed by letter |                                          |
| mark - go to home directory          | `'h`                   | uses predefined mark                     |
| mark - list all                      | `:marks`               |                                          |
| move - to bottom of pane             | `G`                    |                                          |
| move - to top of pane                | `gg`                   |                                          |
| pane - move to other                 | tab                    |                                          |
| pane - show only one                 | `:only` or `ctrl-w o`  |                                          |
| pane - split horizontal              | `sp` or `ctrl-w s`     | also switches from 1 to 2 panes          |
| pane - split vertical                | `vs` or `ctrl-w v`     | also switches from 1 to 2 panes          |
| preview mode - toggle                | `w`                    | changes 2nd pane to show preview of item |
| preview pane - toggle focus          | shift-tab              | can navigate up and down in preview pane |

## Configuring

By default `vifm` is configured in the file `~/.vifm/vifmrc`.

## Color Schemes

Vifm provides predefined
{% aTargetBlank "https://vifm.info/colorschemes.shtml", "color schemes" %}.
To use these, download their `.vifm` files
into the `~/.config/vifm/colors` directory.
Then edit `~/.config/vifm/vifmrc` and change the `colorscheme` setting.
For example, `colorscheme desert`.
Finally, restart `vifm` for the change to take effect.

## Icons

To display icons before files of specific types
add lines like the following in the `vifmrc` configuration file
and restart `vifm`.

```text
set classify='  :dir:'
set classify+='🏃 :exe:'
set classify+=' :link:' " link
set classify+=' ::*.css::' " CSS
set classify+=' ::*.go::' " Go
set classify+=' ::*.html::' " HTML
set classify+=' ::*.java::' " Java
set classify+=' ::*.js::' " JavaScript
set classify+=' ::*.md::' " Markdown
set classify+=' ::*.py::' " Python
set classify+=' ::*.rb::' " Ruby
set classify+=' ::*.rs::' " Rust
set classify+=' ::*.sass::' " Sass
```

## Questions

How do you copy or delete multiple items as a group?

How do you search/grep inside vifm?
