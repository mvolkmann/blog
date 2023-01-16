---
eleventyNavigation:
  key: dataTile
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://underplot.com/dataTile/", "dataTile" %} is a
developer tool that integrates with the Simulator
to display debugging data in tiles.
These are easier to see than searching for output in the debug console
produced by calls to the `print` function.

The "Mini" version is free, but provides no customizations options
and is limited to displaying three tiles.
The "Pro" version costs $7.99 per month and removes these limitations.

## Installing

dataTile can be downloaded from the {% aTargetBlank
"https://apps.apple.com/app/datatile-for-simulator/id6444231603?l=en&mt=12",
"Mac App Store" %}.
Once installed it will appear as an app in your Applications directory.
Double-click it to launch.

## Configuring

There are two ways to use dataTile in an app.

1. Add a run pre-action.

   1. Click the app name to the left of the device dropdown at the top of the Xcode window.
   1. Click "Edit Scheme..."
   1. In the dialog that appears, expand the "Run" section.
   1. Click "Pre-actions".
   1. Click "+" in the lower-left.
   1. Enter the following: `open tile://new/{project-name}`
   1. Click the "Close" button.

1. Observe an app (DID NOT WORK FOR ME!)

   1. Launch the Tile app from the Finder by navigating to
      the Applications directory and double-clicking Tile.
   1. Select File ... Add new app...
   1. Enter the name of any app that uses Apple's unified logging system,
      such as Xcode.
   1. Click the "Create" button.

## Using

In code that wishes to display debugging data in a tile:

1. Add `import OSLog`
1. Add calls to the `os_log` function that are passed
   a string in one of the following formats:

   - "Name: value"
   - "Name 'value'"
   - "Name = value"

The logged values will be displayed in a tile
and will also be written to the debug console.
