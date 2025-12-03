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

To install Vale in macOS, enter `brew install vale`.

Create a configuration file named `.vale.ini` in your home directory
containing settings like the following:

```text
StylesPath = vale-styles
MinAlertLevel = suggestion
Packages = Google, Microsoft, proselint, write-good

[*]
BasedOnStyles = Vale, Google, Microsoft, proselint, write-good
```

Enter the command `vale sync` to download each of the packages
listed in the config file.

Set the environment variable `VALE_CONFIG_PATH` to point to the `.vale.ini` file.
For example, add the following in `.zshenv`:

```bash
export VALE_CONFIG_PATH=$HOME/.vale.ini
```

## Running

From a terminal, `cd` to the directory of a file the be checked.
Enter the command `vale {file-name}`.

## VS Code

Install the "Vale VSCode" extension from Chris Chinchilla.
This causes VS Code to flag writing style issues.
