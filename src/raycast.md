---
eleventyNavigation:
  key: Raycast
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="Raycast logo" style="border: 0"
    src="/blog/assets/raycast-logo.png?v={{pkg.version}}">
</figure>

## Overview

<a href="https://www.raycast.com" target="_blank">Raycast</a> is a macOS
"blazingly fast, totally extendable launcher.
It lets you complete tasks, calculate, share common links, and much more."

## Installing

The steps to install Raycast are:

1. Browse the Raycast <a href="https://www.raycast.com" target="_blank">
   home page</a>.
1. Click the "Download for Mac" button.
1. Double-click the downloaded `Raycast.dmg` file.
   An installer window will appear.
1. Drag the Raycast app icon into the Applications folder.
1. Close the installer window.
1. In the finder, eject the Raycast "drive" from the left nav.

There is also an option to install it using Homebrew.

## Configuring

The steps for first-time configuration of Raycast are:

1. Open the Finder.
1. Select "Applications" in the left nav.
1. Double-click Raycast.app. This will open a Raycast window.
1. Click the "Begin setup" button.
1. Scroll down and modify the toggle switches
   to enable/disable specific built-in extensions.
   For example, I enabled the "Contacts" extension.
1. Click the "Continue" button.
1. Choose the hotkey (keyboard shortcut) that will be used to launch Raycast.

   If you wish to use cmd-space, you will need to change the hotkey for Spotlight.
   To do this:

   1. Open "System Settings".
   1. Select Keyboard in the left nav.
   1. Click the "Keyboard Shortcuts" button.
   1. Select "Spotlight" in the left nav.
   1. In the row for "Show Spotlight search", double-click "cmd-space".
   1. Type a new hotkey such as cmd-period.
   1. Click the "Done" button.

   Click the hotkey under "Raycast Hotkey".
   Type a new hotkey such as cmd-space.
   Click the "Continue" button.

1. Optionally enter your email address and click the "Subscribe" button
   to receive periodic information about Raycast.
1. Click the "Continue" button.
1. Click the "Launch Raycast" button.
1. Double-click "Start supercharging your productivity"
   for a walkthrough of Raycast features.
1. Double-click each topic to learn more.

## Manual

See the official
<a href="https://manual.raycast.com" target="_blank">Raycast Manual</a>.

## Raycast Window

The search input at the top of the Raycast window
supports fuzzy searches.
For example, typing "vc" will match "Visual Studio Code".

Regardless of which "page" you have navigated to within the Raycast window,
you can press cmd-k to see a popup that
lists context-sensitive actions that are available.

Press esc to go back to the previous page.

When on the main page, pressing esc will close the Raycast window.

Press cmd-esc to go back to the main page.

Press cmd-w from any page to close the Raycast window.

## Settings

To open the Raycast settings dialog, open Raycast and press cmd-comma.

Click the "Extensions" tab in the settings dialog
to enable and disable extensions.

## Command Aliases and Hotkeys

To assign aliases and hotkeys to commands,
open Raycast, open the settings dialog, click the "Extension" tab,
select a command, and enter an alias and/or a hotkey for the command.

## Contacts

To search contacts, open Raycast, enter "con", select "Contacts",
and enter part of a name.
The first time you do this you will need to grant permission.

## Launching Apps

To quickly launch an app, press the hotkey for Raycast,
enter part of the app name, and press the return key.
For example, 1Password can be launched by just typing "1p".

Raycast will provide suggestions for recently or frequently used apps.
The app you want is in the list of suggestions, navigate to it
with the up and down arrow keys and press return to launch it.

## Calculator

To perform a calculation, open Raycast, type "calc",
select "Calculator History", and enter an expression.
The result is automatically copied to the clipboard.

Actually calculator expressions can be entered in the initial search input
without first selecting "Calculator History"!

For example, "sin(19)^2 + cos(19)^2" gives 1.

This can also perform unit conversions.
For example, entering 10 km gives 6.2137119224 mi,
entering 70 F gives 21.1111111111 °C, and
entering 8 in in mm gives 203.2 mm.

A history of previous calculations is maintained
so you can copy their result to the clipboard again.
You cannot recall and modify them.

Selecting "Calculator" launches the macOS Calculator app
which is probably not what you want.

## Calendar

The Calendar extension is enabled by default, but it requires permission.
Open the Raycast settings, click the "Extensions" tab,
select "Calendar", and click the "Permission Required" button.
A dialog will appear. Click the "Grant Access" button.
Another dialog will appear. Click the "Allow Full Access" button.

To see your schedule for today, open Raycast, enter "my sch",
and select "My Schedule".

## Clipboard History

To view clipboard history, open Raycast, enter "clip",
and select "Clipboard History".

Click an entry to see which application created it,
when it was created, and more.

Double-click an entry to copy it to the clipboard
AND paste it at the current cursor location.

## File Search

To search for files, open Raycast, type "sf",
select "Search Files", and enter search text.
The first time you do this, you will need to enable it
by selecting "Authorize to search user folders".

To enable search both file names and contents, open the Raycast settings,
click the "Extension" tab, select "Search Files", and
change the "Search By" option to "Name and Contents".

Move the cursor to one of the listed files, then press:

- return to open it in its default application
- cmd-o to open it in a given application (select the application)
- cmd-return to show it in the Finder
- cmd-shift-c to copy to file to another directory

So far this doesn't seem to be able to find all files by their name.
For example, it cannot file Documents/BeverlyHillbillies.txt.
