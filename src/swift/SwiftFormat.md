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
"https://github.com/nicklockwood/SwiftFormat", "SwiftFormat home page" %}.

Create the file `.swift-version` in each project root directory
that contains the version of Swift being used such as "5.6".

## Running from a Terminal

To run SwiftFormat from a terminal,
cd to a project directory and enter `swiftformat .`.
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
1. Check the checkbox for "SwiftFormat".
1. Restart Xcode.

To run SwiftFormat inside Xcode on a single `.swift` file:

1. Open the file inside Xcode.
1. Select Editor ... Swift Format ... Format File to format the entire file.

SwiftFormat will continue working inside Xcode
after quitting the "SwiftFormat for Xcode" app.

There is no built-in way to configure a keyboard shortcut
for running SwiftFormat inside Xcode and
there is currently no way to configure it to run every time a file is saved.

To configure Xcode to run SwiftFormat every time the project is built, see
{% aTargetBlank "https://github.com/nicklockwood/SwiftFormat#xcode-build-phase",
"Xcode build phase" %}.

## Configuration

To configure the rules used by SwiftFormat,
create the file `.swiftformat` at the root of each project.
This file should contain one line for each desired command-line option.
Here is an example:

```
--commas inline
--disable spaceInsideComments
```

## Rules

For documentation on supported rules, see the {% aTargetBlank
"https://github.com/nicklockwood/SwiftFormat/blob/master/Rules.md",
"Rule.md" %}.
