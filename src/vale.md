---
eleventyNavigation:
  key: Vale
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://vale.sh", "Vale CLI" %}
is an open source, command-line tool that
reports on writing style issues found in text files.

## Installing

To install Vale in macOS:

1. Enter `brew install vale`.

1. Create a configuration file named `.vale.ini` in your home directory
   containing settings like the following:

   ```text
   StylesPath = vale-styles
   MinAlertLevel = suggestion
   Packages = Microsoft, proselint, write-good

   # Only process AsciiDoc, Markdown, and plain text files.
   [*.adoc|*.md|*.txt]
   BasedOnStyles = Vale, Microsoft, proselint, write-good
   Microsoft.Contractions = off
   Microsoft.Quotes = off

   [*]
   BasedOnStyles = Empty
   ```

1. Enter the command `vale sync` to download
   each of the packages listed in the config file.

1. Create the empty directory `~/vale-styles/Empty`.

1. Set the environment variable `VALE_CONFIG_PATH`
   to point to the `.vale.ini` file.
   For example, add the following in `.zshenv`:

   ```bash
   export VALE_CONFIG_PATH=$HOME/.vale.ini
   ```

## Running

1. Open a terminal window.
1. `cd` to the directory of a file to check.
1. Enter the command `vale {file-name}`.

## VS Code

Install the {% aTargetBlank
"https://github.com/chrischinchilla/vale-vscode", "Vale VSCode" %}
extension from Chris Chinchilla.
This causes VS Code to flag writing style issues.
