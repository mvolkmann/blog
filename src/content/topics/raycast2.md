---
navigation:
  key: Raycast 2
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="Raycast logo" style="border: 0"
    src="/blog/assets/raycast-logo.png">
</figure>

## Overview

<a href="https://www.raycast.com/new" target="_blank">Raycast v2</a> is ...

## Installing

The steps to install Raycast in macOS are:

The steps to install Raycast in Windows are:

## Configuring

The steps for first-time configuration of Raycast are:


 If you wish to use cmd-space, you will need to change the hotkey for Spotlight.
 To do this:

## Launching

To launch Raycast, press its hotkey.
A Raycast window will open on the monitor where the mouse cursor resides.

Raycast displays a single stack of windows.
To pop the current window off of the stack,
returning to the previous window, press the esc key.

## Features

### Launch an application

1. Enter part of the application name.
1. Select the application.
1. Press the return key.

### Find a file based on its name

1. Enter part of the file name.
1. Optionally open the file by selecting it and pressing the return key.

### Find a file based on its content

If not already enabled, press cmd- to open the Raycast settings,
enter "File Search", click it, scroll down, and turn on "Content Search".

1. Enter "fs" for "File Search".
1. Enter the words to find.
   It will find documents that contain any of the words in any order,
   but files that contain the words in the specified order will be listed first.
1. Optionally open the file by selecting it and pressing the return key.

If expected files do not appear in the results,
click the "Index Files" button in the lower-right and try again.

### Clipboard History

Raycast automatically saves the last ? clipboard entries.
To paste one of them, open Raycast, enter "clip" to open "Clipboard History",
select the entry to paste, and press the return key.

To instead copy the selected entry to the clipboard
so it can be pasted in the normal way later,
press cmd-k, and select "Copy to Clipboard".

Frequently used clipboard items can be
pinned to the top of the list for easier reuse.
To do this, select an entry, press cmd-k, and select "Pin Entry".
To unpin an entry, select it, press cmd-k, and select "Unpin Entry".
Alternatively, select an entry and press cmd-. to toggle whether it is pinned.

To edit a clipboard entry, select it, press cmd-k, and select "Edit".

To edit a clipboard entry, select it, press cmd-k, and select "Delete".

### Emoji search

To search for an emoji,
enter "emoji", select "Search for GIFs", and enter a topic.

To paste an emoji into the current document,
select it and press the return key.

To copy an emoji to the clipboard,
select it, press cmd-k, and select "Copy to Clipboard".

### GIF search

To search for a GIF image,
enter "gif", select "Search Emoji & Symbols", and enter a topic.

To copy the selected image to the clipboard, press the return key.

### Dictation

If dictation isn't already enabled,
press cmd-, to open the Raycast settings,
select "Dictation" in the left nav,
click the toggle in the upper-right to enable,
and add a hotkey to the "Dictate" command (such as cmd-d).

Optionally change "Language" from "Auto" to a specific language (like "English")
if you will always be speaking in that language.

To dictate text to be entered in the focused text input,
press the hotkey, speak the text, and press the hotkey again.

A period will automatically be added to the end of the text.
To prevent this, open the Raycast settings,
select "Dictation" in the left nav,
click "Personalization", click "Custom Instructions", and enter
"Transcribe speech naturally, but do not include
a period at the end of the final sentence or phrase."
It's ridiculous that this is so complicated!

To paste a past dictation, enter "Dictation History",
select one and press the return key.

### Window Management

