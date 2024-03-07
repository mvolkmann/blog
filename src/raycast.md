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

Press the up and down arrow keys to highlight an item
in the list of suggestions or matches.

Press cmd-k to see a list of actions
that can be performed on the highlighted item.
One action that can be performed on a command is "Add to Favorites"
which adds it to the list of favorite commands
that are always visible on the starting Raycast page.

Regardless of which Raycast "page" you have navigated to within the window
and which item is highlighted, you can press cmd-k to see a popup
that lists context-sensitive actions that are available.

For example, on the "Search Files" page when a file is highlighted
the available actions include "Open", "Show in Finder", "Quick Look",
"Open With...", "Show Info in Finder", "Enclosing Folder",
"Toggle Details", "Share...", "Move to...", "Copy to...", "Duplicate",
"Copy File", "Copy Name", "Copy Path", and "Move to Trash".

Press esc to go back to the previous page.
When on the starting page, pressing esc will close the Raycast window.

Press cmd-esc to go back to the main page.

Press cmd-w on any page to close the Raycast window.

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
For example:

- 10 km gives 6.2137119224 mi
- 8 in in mm gives 203.2 mm
- 70 F gives 21.1111111111 °C
- 1 euro in usd gives $1.09
- days until april 16 gives 41 (today)
- time in boston gives 12:30 PM

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

To see your schedule for today, open Raycast, enter "sche",
and select "My Schedule".

## Clipboard History

To view clipboard history, open Raycast, enter "clip",
and select "Clipboard History".
By default only entries from the last 7 days are saved.

Filter the list by entering search text.

Click an entry to see which application created it,
when it was created, and more.

Double-click an entry to copy it to the clipboard
AND paste it at the current cursor location.

To disable saving clipboard entries from specific apps (such as 1Password):

- Open the Raycast settings.
- Click the "Extensions" tab.
- Select "Clipboard History".
- Click the "Select More Apps" button under "Disabled Application".
- Select an application.

To delete entries from the clipboard history, press cmd-k and
select "Delete Entry", "Delete Entries..." (last 5, 15, or 30 minutes),
or "Delete All Entries".

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

## Quicklinks

Quicklinks simplify opening URLs and
supplying a query parameter for sites the perform searching.
The only quicklinks that are provided are
"Search DuckDuckGo" and "Search Google".

To add a predefined quicklink:

- Open Raycast.
- Press cmd-comma to open the settings.
- Click the "Extensions" tab.
- Select "Quicklinks".
- Click the "Find in Library" button on the right.
- Select a quicklink.
  Examples include "Search Wikipedia" and "Search YouTube".
- In the dialog that appears, click the "Create Quicklink" button.

To create a new quicklink for a website that
performs a search using a URL query parameter:

- Go to the desired site in a web browser.
- Perform a search.
- Copy the URL of the result page.
- Open Raycast.
- Type "quick".
- Double-click "Create Quicklink".
- Enter a name for the new Quicklink.
  For example, "Search YouTube".
- Paste the URL in the "Link" input.
  For example, "https://www.youtube.com/results?search_query=giraffe"
- Optionally replace the search term with "{Query}"
  for URLs that include a search query parameter.
  For example, "https://www.youtube.com/results?search_query={Query}"
- Select the web browser to be used.
- Click the "Create Quicklink" button.

To use a quicklink:

- Open Raycast.
- Enter part of the quicklink name.
- Highlight the quicklink using the up and down arrow keys.
- Enter a search term and press the return key.

To create a quicklink that opens a frequently used local file,
use the "Search Files" command to find the file in Raycast,
press cmd-k, and select "Create Quicklink".
Enter a name for the quicklink, select the application that should open it,
and click the "Create Quicklink" button.
Now to open the file you can just enter part of the assigned name
in the input on the main Raycast page.

I created a quicklink for the website [removebg](https://www.remove.bg)
which removes the background from images.
I gave it the alias "bg".

I also created a quicklink for ChatGPT.

Mozilla Developer Network (MDN) does not perform searches
using a URL query parameters, so you cannot create a quicklink for it.
However, there is an extension that can be installed from the "store"
that searches MDN called "Search MDN".
The command name this adds is "Search Web Docs".
It displays documentation in the Raycast window
and provides a "Open in Browser" button.

## Search Emojis & Symbols

This command displays a list of all emojis and symbols.
To quickly find this command, enter "emo".

Enter a search term to filter the list.

Double-click an emoji or symbol to insert it at the current cursor position.

For example, enter "kiss" to see only the related emojis and symbols.

## Snippets

Raycast snippets are text identified by keywords.
When the keywords are entered in _any_ application,
they are replaced by the associated text.

To see the existing snippets:

- Open Raycast.
- Enter "sni" and select "Search Snippets".

To define a snippet:

- Open Raycast.
- Enter "sni" and select "Create Snippet".
- Enter a descriptive name.
- Enter the text of the snippet.
- Enter the keyword that will be used to activate the snippet.

The snippet text can contain placeholders.
For details, see <a href="https://manual.raycast.com/dynamic-placeholders"
target="_blank">Dynamic Placeholders</a>.

For example, the following snippet adds an HTML anchor tag.

```html
<a href="{argument name="url"" target="_blank">{argument name="title"}</a>
```

If this is given the keyword "atb" and that keyword is entered in any application,
a dialog will appear to prompt for values for "url" and "title".
After entering the values, click the "Expand" button or press cmd-return.

## Store

The Raycast Store is the place to download more extentions.
To open it, open Raycast and enter "store".

I installed the "Search MDN" extension.

## System Commands

Raycast provides many system commands including:

- Empty Trash
- Restart
- Show Desktop
- Shut Down
- Sleep
- Toggle System Appearance (between light and dark mode)

## Window Managment

Raycast provides many commands to change the size and location of windows.
The most useful are the following:

- enter "max" for Maximize and Maximize Height|Width
- enter "hal" for Left|Right|Bottom|Top|Center Half
- enter "thi" for First|First Two|Last|Last Two|Center Third
- enter "fou" for First|Second|Third|Last Fourth
- enter "qua" for Top Left|Top Right|Bottom Left|Bottom Right Quarter
- enter "mov" for Move Left|Right|Up|Down
- enter "dis" for Previous|Next Display
- enter "cen" for Center
- enter "tog" for Toggle Fullscreen

I created these keyboard shortcuts:

| Command          | Shortcut              |
| ---------------- | --------------------- |
| left half        | cmd-left arrow        |
| right half       | cmd-right arrow       |
| bottom half      | cmd-down arrow        |
| top half         | cmd-up arrow          |
| center half      | cmd-shift-c           |
| left quarter     | cmd-shift-left arrow  |
| right quarter    | cmd-shift-right arrow |
| maximize         | cmd-shift-m           |
| next display     | cmd-shift-n           |
| previous display | cmd-shift-p           |

## Popular Extensions

- Color Picker
- GitHub
- iTranslate - language translations and speech
- Kill Process
- Music
- System Monitor
