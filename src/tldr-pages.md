---
eleventyNavigation:
  key: tldr pages
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://tldr.sh", "tldr pages" %} "are a community effort
to simplify the beloved man pages with practical examples."

To install using npm, enter `npm install -g tldr`.

To install using Homebrew for macOS, enter `brew install tldr`.

To get help on a given topic, enter `tldr {topic}`.

To see a list of currently supported topics, enter `tldr -l | sort`.

## Command Example

For example, entering `tldr rg` gives the following help on "ripgrep":

{% raw %}

```text
rg

Ripgrep is a recursive line-oriented search tool.
Aims to be a faster alternative to `grep`.
More information: https://github.com/BurntSushi/ripgrep.

- Recursively search the current directory for a regular expression:
    rg regular_expression

- Search for regular expressions recursively in the current directory, including hidden files and files listed in `.gitignore`:
    rg --no-ignore --hidden regular_expression

- Search for a regular expression only in a subset of directories:
    rg regular_expression set_of_subdirs

- Search for a regular expression in files matching a glob (e.g. `README.*`):
    rg regular_expression --glob glob

- Search for filenames that match a regular expression:
    rg --files | rg regular_expression

- Only list matched files (useful when piping to other commands):
    rg --files-with-matches regular_expression

- Show lines that do not match the given regular expression:
    rg --invert-match regular_expression

- Search a literal string pattern:
    rg --fixed-strings -- string
```

{% endraw %}

## Subcommand Example

To get help on a subcommand, enter `tldr {command} {subcommand}`.
For example, entering `tldr git stash` outputs the following:

{% raw %}

```text
git stash

Stash local Git changes in a temporary area.
More information: https://git-scm.com/docs/git-stash.

- Stash current changes, except new (untracked) files:
    git stash push -m optional_stash_message

- Stash current changes, including new (untracked) files:
    git stash -u

- Interactively select parts of changed files for stashing:
    git stash -p

- List all stashes (shows stash name, related branch and message):
    git stash list

- Show the changes as a patch between the stash (default is `stash@{0}`) and the commit back when stash entry was first created:
    git stash show -p stash@{0}

- Apply a stash (default is the latest, named stash@{0}):
    git stash apply optional_stash_name_or_commit

- Drop or apply a stash (default is stash@{0}) and remove it from the stash list if applying doesn't cause conflicts:
    git stash pop optional_stash_name

- Drop all stashes:
    git stash clear
```

{% endraw %}
