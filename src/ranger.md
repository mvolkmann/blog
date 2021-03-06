---
eleventyNavigation:
  key: Ranger
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://ranger.github.io", "Ranger" %}
is a Vim-inspired, terminal-based file manager
that is implemented in Python.
But use of the Vim keybindings is not required, so it is
useful even if you haven't internalized those keybindings.

See the {% aTargetBlank
"https://github.com/ranger/ranger/wiki/Official-user-guide",
"Official User Guide" %} for tips on efficient usage.

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

## Configuration

Ranger user four configuration files,
all found in the `~/.config/ranger` directory.

| Configuration File | Purpose                                                    |
| ------------------ | ---------------------------------------------------------- |
| `commands.py`      | defines Python functions that modify Ranger behavior       |
| `rc.conf`          | sets options and key bindings                              |
| `rifle.conf`       | maps file extensions to applications that should open them |
| `scope.sh`         | shell script that generates file previews                  |

For many users none of these files will need to be created or modified.

## Functionality

Ranger displays three columns.

The left column shows the contents of the parent directory.

The middle column shows the contents of the current directory.

The right column shows the contents of the file selected in the middle column
or a description of it.

## Navigating

Navigation always occurs in the middle column.

| Direction                          | Keys               |
| ---------------------------------- | ------------------ |
| next item in current directory     | `j` or down arrow  |
| previous item in current directory | `k` or up arrow    |
| into child directory               | `l` or right arrow |
| edit file under cursor             | `l` or right arrow |
| into parent directory              | `h` or left arrow  |

Text files are displayed in the right column.
Non-text files are opened by their default application.
For example, in macOS image files are opened in the Preview app.

## Commands

Ranger commands can entered by pressing `:` which
opens a line at the bottom for typing a command just like in Vim.

Commonly used commands are described in the table below.

| Command                   | Key Binding    | Description                                                                                                 |
| ------------------------- | -------------- | ----------------------------------------------------------------------------------------------------------- |
| `cd dir-path`             |                | changes current directory being displayed;<br>cannot use environment variables                              |
| `copy [set\|add\|remove]` | `yy`           | copies cursor item or selected items;<br>argument defaults to `set`                                         |
| `cut [set\|add\|remove]`  | `dd`           | cuts cursor item or selected items;<br>argument defaults to `set`                                           |
| `delete`                  | `dD`           | deletes cursor item or selected items;<br>requests confirmation for non-empty directories or multiple files |
| `edit`                    | enter          | edits file under cursor using Vim                                                                           |
| `exit`                    | `q` or `:q`    | exits ranger                                                                                                |
| `grep text`               |                | searches for text in selected files and directories                                                         |
| `help`                    |                | displays ranger help                                                                                        |
| `linemode mode`           |                | TODO: What are the supported modes?                                                                         |
| `mkdir dir-name`          |                | creates a directory                                                                                         |
| `paste_ext`               |                | like `paste`, but adds a unique suffix to name if it already exists                                         |
| `paste`                   | `pp`           | pastes files or directories that were copied or cut                                                         |
| `rename new-name`         | `cw`           | renames cursor item                                                                                         |
| `touch file-name`         |                | creates a file                                                                                              |
| `trash`                   |                | moves cursor item to system trash                                                                           |
| `yank [name\|dir\|path]`  |                | copies name, directory, or path of cursor item to clipboard                                                 |
|                           | space          | toggles mark of cursor item                                                                                 |
|                           | `zh` or delete | toggles display of hidden files                                                                             |

TODO: Which commands operate on marked files?

By default the copy, cut, and rename commands
act on the file under the cursor.
If multiple files have been selected,
the copy and cut commands act on all of them.

To paste the file in a different directory,
navigate to it and press `pp`.

After copying or cutting a file, its name is dimmed.
To undo this, enter `:copy remove` or `:cut remove`.

To bookmark a directory, navigate to it and
press `m` followed by the letter to assign.
To navigate to a bookmark, press backtick and the letter.
