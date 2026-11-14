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

- Browse https://www.raycast.com/new (for v2).
- Click the "Download" button.
- Double-click the downloaded file.

The steps to install Raycast in Windows are:

- same as for macOS?

## Configuring

The steps for first-time configuration of Raycast are:

- TODO: Add these.

If you wish to use cmd-space, you will need to change the hotkey for Spotlight.
To do this, TODO.

To configure a key to be the hyper key
which can be used for global hotkeys,
open the Raycast settings, search for "Hyper Key",
and select a key in the "Hyper Key" dropdown.
I chose the right command key.

To assign a global hotkey to a Raycast command,
open Raycast, search for the command, select it,
press cmd-k, select "Configure Command...",
select "Set Hotkey...", and press the desired hotkey combination.

By default Raycast will remember the last content it was displaying
and show that again if Raycast is reopened within 90 seconds.
To adjust this, open the Raycast settings,
search for "Pop to Root Search", and select a different setting.
The "Immediately" option causes Raycast to always open at the root.

## Launching

To launch Raycast, press its hotkey.
A Raycast window will open on the monitor where the mouse cursor resides.

## Basics

To view the Raycast Manual, open the Raycast Settings and select "Manual".

Raycast displays a single stack of windows.
To pop the current window off of the stack,
returning to the previous window, press the esc key.

The initial Raycast window lists items marked as favorites first,
followed by suggestions (How are those determined?),
and then the most recently used commands.

Once you start typing, the items are filtered using fuzzy matching
based on what has been typed so far.
For example, typing "yt" will find the YouTube application
because its name contains those letters in that order.

Item names appear on the left side and their type appears on the right side.
The supported types include:

- AI Extension (all are Pro-only)
- Application
- Command (some are marked as Pro)
- Draft
- Menu Bar Command
- Quicklink
- Shortcut
- System Setting

There are three ways to select an item in the Raycast window.
You can click an item.
You can use the down and up arrow keys to navigate to an item.
Finally, you can hold down the command key to display a number
on the right side of each item and then press that number.

When an item is selected, its default action is executed
by pressing the return key, or in some cases cmd-return.
An item can be a Raycast command, application, file, or directory.
Other item-specific actions can be displayed in a menu by pressing cmd-k.
To execute an action, selected it and press the return key
or double-click it.

## Basic Features

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

I set the hotkey for this command to rightCmd-f.

### Folders/Directories

To open a directory by name in a new Finder window,
enter its name under the "Files" section,
select it, and press the return key.

## More Features

### AI

Raycast AI features such as "Quick AI" and "AI Chat"
are only available in the Pro version which currently costs $8 per month.

### Aliases

Aliases can be assigned to items to simplify selecting them.
For example, I assigned the alias "kn" to the Keynote application.
To assign an alias to an item, select the item, press cmd-k,
select "Configure {item-type}...", select "Set Alias...",
enter an alias, and press the return key.

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

### Color Picker

To copy the hex code for a color from a selected pixel on the screen,
enter "Pick Color".  The code can then be pasted anywhere.

Also see the Raycast commands "Color Wheel" and "Convert Color".

### Confetti

To sprinkle confetti all over the screen,
enter the "Confetti" command.
I set the hotkey for this command to rightCmd-c.

### Currency Conversions

To convert between currencies, enter an expression like
"1$ in euros" which evaluates to "€0.90".

Raycast periodically fetches currency exchange rates in the background
whenever you have an internet connection
and caches them locally on your machine.
Because the rates are cached, currency conversions
still work offline using the most recently synced rates.

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

### Emoji search

To search for an emoji,
enter "emoji", select "Search Emoji & Symbols", and enter a topic.
I assigned the hotkey for this command to rightCmd-e.

To paste an emoji into the current document,
select it and press the return key.

To copy an emoji to the clipboard,
select it, press cmd-k, and select "Copy to Clipboard".

### GIF search

To search for a GIF image,
enter "gif", select "Search for GIFs", and enter a topic.
I assigned the hotkey for this command to rightCmd-g.

To copy the selected image to the clipboard, press the return key.

### Internet Speed

To measure your internet connection, install the "Speedtest" extension
and run the "Test Internet Speed" command.

### Keyboard Cleaning

To lock your keyboard so it can be cleaned without activating any keys,
measure your internet connection, install the "Clean Keyboard" extension,
run the "Lock Keyboard" command, select a duration.
A countdown will be displayed, during which time you can clean the keyboard.
It will automatically unlock at the end of the selected duration.
To unlock the keyboard earlier, press ctrl-u.

### Language Translation

The "Translate" command is only available in the Pro version
which currently costs $8 per month.G

### Math

To evaluate a math expression, enter it.
For example, "sin(19deg)^2 + cos(19deg)^2" evaluates to 1.

To copy the answer so it can be pasted elsewhere, press the return key.

To compute percentage increases, enter an expression like
"943 + 3.6%" which evaluates to "976.948".

To perform a unit conversion, enter an expression like
"26.2 miles in kilometers" which evaluates to "42.1648128 km"
or "1 gal in liters" which evaluates to "3.785411784 L".

## MDN Documentation

To view specific documentation in the Microsoft Developer Network (MDN) site,
install the "Search MDN" extension, run the "Search Web Docs" command,
enter search text, select a matching topic, and press the return key.

### Processes

To kill the process listening on a given port,
run the "Kill Process Listening on" command.
Enter the port and press the return key.
(NOTE: This doesn't work if the process was started with "node -watch".)

### Quick Fix

The "Quick Fix" command corrects spelling and grammar errors in selected text.
By default, it is trigger by pressing the right shift key twice.
For example, it changes "Ths txt has meny speling erors."
to "This text has many spelling errors."

### Quicklinks

Quicklinks provide a fast way to open a file, folder, application, or URL.
They can also provide a fast way to run a Raycast command.

To create a quicklink, open Raycast, find and select the target,
press cmd-k, select "Save as QuickLink",
enter a name, optionally select an icon,
select the application in which to open it, optionally enter tags,
and press cmd-return to save it.

To create a quicklink to a URL, open Raycast,
run the "Create Quicklink" command, and enter or paste a URL.
If the URL was currently being browsed, this will be filled in automatically.
Then enter a name, optionally select an icon,
select the web browser in which to open it, optionally enter tags
and press cmd-return to save it.

To open a quicklink, enter part of its name,
select it, and press the return key.

### Reminders

To create a new reminder in the Apple Reminders app,
run the "Create Reminders" command.
This opens a dialog where the details can be entered.
Press cmd-return to create it.

### Schedule

To see scheduled Calendar app items, enter "My Schedule".
Select an event and press the return key to view it in the Calendar app.
Select an event and press cmd-k for other operations including
Create Event (to create a new event), Edit Event, and Delete Event.

### Sleep

To prevent the computer from sleeping
which allows a long running process to continue,
install the "Coffee" extension.
Then run the "Caffeinate" command.
To allow the compute to sleep again, run the "Decaffeinate" command.

### Snippets

To enable use of snippets, open "System Settings",
select "Privacy & Security", select "Input Monitoring",
click the "+", and add the Raycast app.

To create a new snippet, enter the "Create Snippet" command.
In the "Snippet" field, enter the snippet text.
In the "Name & Icon" field, enter a name for the snippet
and optionally select an icon from a long dropdown.
There are icons for many topics, including specific programming languages.
In the "Keyword" field, enter the keyword text,
typically starting with a special character
so the keyword is unlikely to be useful literally.

To expand a snippet inside another app,
type the trigger text.

To view all the defined snippets, enter the "Search Snippets" command.

I created the following snippets:

- @@ for my email address
- +1 for the thumbs up emoji
- @lambda for the lambda character
- @log for JavaScript console.log statements

  This prompts for an expression and expands to something like
  `console.log('foo =', foo);`

Snippets can contain placeholders and prompt for text to replace them.
For an example, see my @log snippet.

### System Info

The "System Info" command provides information about your computer such as
CPU usage across all cores, memory usage, memory pressure, top processes,
and disk usage.

Time Machine backups are each listed as separate disks in this display.

### System Settings

To open a specific section within the System Settings app,
enter its name, select it, and press the return key.

### Time

To find the current time in another city,
enter "time" followed by a city name.
For example, "time Honolulu".

### Trash

To empty the trash, enter the "Empty Trash" command.

### Typing Practice

To practice typing and measure speed, run the "Start Typing Practice" command.

### Window Management

TODO: Add detail here.

## Extensions

The Raycast store offers many free extensions.
To view them, run the "Store" command.

Several extensions are installed by default.

To see the extensions that are already installed,
open the Raycast settings (cmd-,).
They are listed in the left nav below "Window Management".

To install an extension from the store, select it,
press the return key to see details about it,
and press the return key again to install it.

To uninstall an extension, run the "Store" command,
change the Category dropdown in the upper-right
to only show installed extensions,
select the extension to uninstall, press cmd-k,
select "Uninstall extension", and confirm.
