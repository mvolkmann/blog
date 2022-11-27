---
eleventyNavigation:
  key: SwiftFormat
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://github.com/nicklockwood/SwiftFormat", "SwiftFormat" %}
is a "command-line tool and Xcode Extension for formatting Swift code".

## Installing

One way to install SwiftFormat is to use
{% aTargetBlank "https://brew.sh", "HomeBrew" %}.
Enter `brew install swiftformat`.

For other options, see the {% aTargetBlank
"https://github.com/nicklockwood/SwiftFormat#how-do-i-install-it",
"How do I install it?" %}.

Create the file `.swift-version` in each project root directory
that contains the version of Swift being used such as "5.6".

## Running from a Terminal

To run SwiftFormat from a terminal,
cd to a project directory and enter `swiftformat --swiftversion 5.7 .`.
This will output the number of files that were modified
and the number of files that were evaluated.

DO NOT RUN THIS FROM YOUR ROOT DIRECTORY!
Doing so will reformat every `.swift` file found in and below it.

## Running from Xcode

To configure Xcode so SwiftFormat can be run on the file
currently being edited by selecting a menu option:

1. Enter `brew install --cask swiftformat-for-xcode`
1. Open Finder.
1. Open the Applications folder.
1. Double-click "SwiftFormat for Xcode.app".
1. Optionally configure the rules to be applied by clicking the
   "Rules" button at the top and checking and unchecking rules.
   Some rules offer more fine-grained configuration.
1. Open "System Preferences".
1. Click "Extensions".
1. In the left nav, select "Xcode Source Editor".
   If this is missing, see the WARNING section below.
1. Check the checkbox for "SwiftFormat".
1. Restart Xcode.

To run SwiftFormat inside Xcode on a single `.swift` file:

1. Open the file inside Xcode.
1. Select Editor ... Swift Format ... Format File to format the entire file.

To configure a keyboard shortcut for this:

1. Select Xcode ... Preferences ... Key Bindings
1. Type "SwiftFormat" in the the filter input on the right.
1. In the row that begins with "SwiftFormat - Format File",
   double-click in the "Key" column.
1. Type the desired keyboard shortcut (ex. cmd-s).
   Using cmd-s will format the file AND save it.
1. If that keyboard shortcut is already in use,
   a message at the bottom of the dialog will describe the current mapping.
   Choose another keyboard shortcut to avoid disabling the current mapping.
1. Close the Preferences dialog.

There is no built-in way to configure a keyboard shortcut
for running SwiftFormat inside Xcode and

SwiftFormat will continue working inside Xcode
after quitting the "SwiftFormat for Xcode" app.

There is currently no way to configure Xcode to run SwiftFormat
every time a file is saved.

To configure Xcode to run SwiftFormat every time the project is built, see
{% aTargetBlank "https://github.com/nicklockwood/SwiftFormat#xcode-build-phase",
"Xcode build phase" %}.

### WARNING

Periodically Xcode loses the ability to run SwiftFormat and
the Editor menu will no longer contain "SwiftFormat" at the bottom.
This happens when System Preferences ... Extensions
loses the left nav entry for "Xcode Source Editor".
Pressing cmd-s will do nothing but make a sound.
To fix this, enter the following commands from a terminal:

```bash
PATH=/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support:"$PATH"
lsregister -f /Applications/Xcode.app # or whatever executable name is used
```

This tip was found in this GitHub {% aTargetBlank
"https://github.com/nicklockwood/SwiftFormat/issues/494", "issue" %}.

Since this fix is required periodically, it's a good idea to
write a script that does this and an alias to make it easy to run.
Here is the script `fix-swift-format`:

```bash
#!/usr/bin/env bash
# This fixes the use of SwiftFormat in Xcode.
PATH=/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support:"$PATH"
lsregister -f /Applications/Xcode.app
```

Here is the alias definition from `.zshrc`:

```bash
alias fixsf="fix-swift-format"
```

Now when "format on save" stops working in Xcode
you can just enter `fixsf` from a terminal.

## Configuration

The app "SwiftFormat for Xcode" is installed
by the "brew install" command described above.
When running SwiftFormat from Xcode, use this app to configure the rules.

When running SwiftFormat from the command line,
configure the rules by creating the file `.swiftformat`
at the root of each project.
This file should contain one line for each desired command-line option.
Here are recommended rule settings:

```
--hexgrouping 2,2
--indent 4
--maxwidth 80
--unusedarguments unnamed-only
--wraparguments before-first
--wrapcollections before-first
--wrapconditions after-first
--wrapparameters before-first
--disable trailingCommas,wrapMultilineStatementBraces
```

## Rules

For documentation on supported rules, see the {% aTargetBlank
"https://github.com/nicklockwood/SwiftFormat/blob/master/Rules.md",
"Rule.md" %}.
