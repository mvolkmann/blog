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

From the Raycast manual,
<a href="https://www.raycast.com/new" target="_blank">Raycast v2</a>
is a ground-up rebuild of the keyboard-driven productivity launcher
for macOS and Windows, designed to unify application launching,
file search, system utilities, and AI workflows.
Built specifically for Apple Silicon and macOS Tahoe (and requiring those),
it replaces Raycast v1 while
migrating existing extensions, snippets, and settings.
It also runs on Windows 10 and 11.

TODO: Look for more details to add in https://manual.raycast.com/new-in-v2.

## Installing

The steps to install Raycast in macOS are:

- Browse https://www.raycast.com/new (for v2).
- Click the "Download" button.
- Double-click the downloaded file.
- Double-click the Raycast app in the Applications directory to start it.

This adds a menu bar item for Raycast.
Clicking it opens a menu containing many items including
"Send Feedback...", "Manual" (opens in default web browser),
"Troubleshooting" (opens in default web browser),
"Check for updates", "Settings...", "Quit Raycast", and more.

The steps to install Raycast in Windows are:

TODO: Try this on one of Jeremy's laptops.

- same as for macOS?

## Configuring

To open the Raycast settings, launch Raycast and
click the menu button in the lower-left (or press cmd-,).

The hotkey for launching Raycast defaults to option-space.
The change this to cmd-space, replacing the use of Spotlight,
open the Raycast settings, select "General" in the left nav,
and click the "Replace Spotlight" button.

To regain the ability to launch Spotlight using a hotkey,
open "System Settings", select "Keyboard" in the left nav,
click the "Keyboard Shortcuts..." button,
select "Spotlight" in the left nav,
check the "Show Spotlight search" button,
double-click its hotkey (cmd-space),
press a new hotkey combination (such as option-space),
and click the "Done" button.

To configure a key to be the hyper key
which can be used for global hotkeys,
open the Raycast settings, search for "Hyper Key",
and select a key in the "Hyper Key" dropdown.
I chose the right command key.

To assign a global hotkey to a Raycast command,
open Raycast, search for the command, select it,
press cmd-k, select "Configure Command...",
select "Set Hotkey...", and press the desired hotkey combination.

By default Raycast opens on the screen containing the mouse.
To change this, open the Raycast settings,
search for "Show Raycast on", and select one of the following options:
"Screen containing mouse", "Screen with active window", or "Primary screen".

## Launching

To launch Raycast, press its hotkey.
A Raycast window displaying the "root search" will open,
by default on the monitor where the mouse cursor resides.
To change where it opens, open the Raycast settings,
select "Launcher" in the left nav, and
change the "Show Raycast on" dropdown to
"Screen containing mouse", "Screen with active window", or "Primary screen".

## Basics

To view the Raycast Manual, open the Raycast Settings and select "Manual".

Raycast displays a single stack of windows starting at the root search.
To pop the current window off of the stack,
returning to the previous window, press the esc key.
The root search window displays the Raycast icon before the text input.
All other windows display a left chevron before the text input.
So you have returned to the root search when you see the Raycast icon.
To return to the root search without needing to
press the escape key multiple times, press cmd-esc.

The root search window lists items marked as favorites first,
followed by suggestions (How are those determined?),
and then the most recently used commands.

By default, Raycast will remember the last content it was displaying
and show that again if Raycast is reopened within 90 seconds.
To adjust this, open the Raycast settings,
search for "Pop to Root Search", and select a different setting.
The "Immediately" option causes Raycast to always open at the root.

Once you start typing, the items are filtered using fuzzy matching
based on what has been typed so far.
For example, typing "yt" will find the YouTube application
because its name contains those letters in that order.

There are many commonly used commands whose names begin with "Search".
Type that to see them.

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

An item can be a Raycast command, application, file, or directory.
When an item is selected,
its primary action is executed by pressing the return key.
For example, the primary action for an application is to launch it.
You may prefer launching applications this way
rather than moving the cursor to the dock and clicking them.
If so, most (or even all) applications can be removed from the dock.

Many items also have a secondary action that is executed by pressing cmd-return.
For example, when a file selected,
the primary action is to open it in its default application and
the secondary action is to open it in the finder
so you can see where the file is located.

Additional item-specific actions can be selected from an "action panel"
which is displayed by pressing cmd-k.
To execute an action in the action panel,
selected it and press the return key or double-click it.

## Basic Features

### Launch an application

1. Enter part of the application name.
1. Select the application.
1. Press the return key.

### Find a file based on its name

1. Enter part of the file name.
1. Optionally open the file by selecting it and pressing the return key.

Rather than performing a true fuzzy search,
it tends to favor filename prefixes and word beginnings.

### Find a file based on its content

If not already enabled, press cmd-, to open the Raycast settings,
enter "File Search", click it, scroll down, and turn on "Content Search".

1. Run the "Search Files" command.
1. Enter text contained in the file.
1. Optionally open the file by selecting it and pressing the return key.

In macOS this depends on Spotlight's index.

If expected files do not appear in the results,
clicking the "Index Files" button in the lower-right
and trying again may help.

I assigned the alias "sf" and the hotkey rightCmd-f
to the "Search Files" command.

The free extension "Fuzzy File Search" is an alternative
that performs fuzzy matching of file names.
It has its own "Search Files" command that you may prefer.

### Folders/Directories

To open a directory by name in a new Finder window,
enter its name under the "Files" section,
select it, and press the return key.

## More Features

### AI

Raycast AI features such as "Quick AI" and "AI Chat"
are only available in the Pro version which currently costs
$10 per month or $8 per month when billed annually.

### Clipboard History

Raycast automatically saves clipboard entries.
In the free version, this is limited to entries from the past three months.
It defaults to 1 week and can be changed to
5 minutes, 1 hour, 1 day, 1 week, 1 month, or 3 months.

To paste a clipboard entry, launch Raycast,
run the "Clipboard History" command, and
double-click the entry to paste (or select it and press the return key).
Type any part of the desired clipboard text to
filter the entries to only those containing what is typed.
I assigned the hotkey cmd-shift-v to the "Clipboard History" command.

To instead copy the selected entry to the clipboard
so it can be pasted in the normal way later,
press cmd-k, and select "Copy to Clipboard".

Frequently used clipboard items can be
pinned to the top of the list for easier reuse.
To do this, select an entry, press cmd-k, and select "Pin Entry".
To unpin an entry, select it, press cmd-k, and select "Unpin Entry".
Alternatively, select an entry and press cmd-. to toggle whether it is pinned.

To edit a clipboard entry, select it, press cmd-k, and select "Edit".

To delete a clipboard entry, select it, press cmd-k, and select "Delete".

### Color Picker

To copy the hex code for a color from a selected pixel on the screen,
enter "Pick Color". The code can then be pasted anywhere.

Also see the Raycast commands "Color Wheel" and "Convert Color".

### Confetti

To sprinkle confetti all over the screen,
enter the "Confetti" command.
I set the hotkey for this command to rightCmd-c.

### Contacts

The Contacts extension integrates with the Contacts app.
To search for a contact, run the "Search Contacts" command.
I assigned the alias "con" to this command.

### Currency Conversions

To convert between currencies, enter an expression like
"1$ in euros" which evaluates to "€0.90".

Raycast periodically fetches currency exchange rates in the background
whenever you have an internet connection
and caches them locally on your machine.
Because the rates are cached, currency conversions
still work offline using the most recently synced rates.

### Dictation (requires Pro)

If dictation isn't already enabled,
press cmd-, to open the Raycast settings,
select "Dictation" in the left nav,
click the toggle in the upper-right to enable,
and add a hotkey to the "Dictate" command (such as cmd-d).

Optionally change "Language" from "Auto" to a specific language (like "English")
if you will always be speaking in that language.

To dictate text to be entered in the focused text input,
press the hotkey, speak the text, and press the hotkey again.

It seems that a period is automatically added to the end of the text.
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

### Favorites

Any item can be pinned to the top of the root search
in the "Favorites" section that only appears
when there is at least one favorited item.

To favorite an item, select it, press cmd-k, and select "Add to Favorites".

Favorites are grouped by type and can only be reordered within their type.
To move a favorite up or down, select it, press cmd-k,
and select "Move Favorite Up" or "Move Favorite Down".

To remove an item from the Favorites list,
select it, press cmd-k, and select "Remove from Favorites".

### GIF search

To search for a GIF image,
enter "gif", select "Search for GIFs", and enter a topic.
I assigned the hotkey for this command to rightCmd-g.

To copy the selected image to the clipboard, press the return key.

### Google Gemini

The "Google Gemini" extension provides commands
for interacting with "Google Gemini", including "AI Chat".

To use this, browse https://aistudio.google.com/api-keys,
sign in with your Google credentials, click "API Keys" in the left nav,
click the key for the "Default Gemini Project", and click the "Copy key" button.
Then install the "Google Gemini" extension and run its "AI Chat" command.
The first time this is run, you'll be prompted for a Gemini API Key.
Paste the key copied above and press cmd-return.
Enter a prompt and press return to evaluate it.

A log of past prompts is maintained in the left nav.
Click one to recall its response.

I assigned the alias "gemini" to the "AI Chat" command.

A downside of the "AI Chat" command is that
prompts must be entered on a single line.
Long prompts scrolls horizontally, hiding the beginning of the prompt.

Another issue is that it seems there is no way to issue followup prompts,
keeping the previous context.

### Internet Speed

To measure your internet connection speed, install the "Speedtest" extension
and run the "Test Internet Speed" command.

### Keyboard Cleaning

To lock your keyboard so it can be cleaned without activating any keys,
measure your internet connection, install the "Clean Keyboard" extension,
run the "Lock Keyboard" command, select a duration.
A countdown will be displayed, during which time you can clean the keyboard.
It will automatically unlock at the end of the selected duration.
To unlock the keyboard earlier, press ctrl-u.

### Language Translation

To translate text from one language to another,
install the "Google Translate" extension.

The "Translate Form" command presents a form with fields for input text,
source language, and target language.
The input text defaults to text selected in the active application, if any.
This is good for longer, multi-paragraph text or
manually selecting the from and to languages
without needing to create a language set.

The "Translate" command translates using auto-detect or a saved "language set".
Select a language set, then enter text to be translated.
Select a translation and press cmd-t to hear it spoken.

To create a new language set, select "Manage Language Sets..."
from the dropdown in the upper-right.
Then double-click "Add new language set...".
For example, you can create a language set where the source language in English
and the target languages are French, German, and Spanish.
With that language set selected, you can enter text in English
and see its translation in all the target languages.

The "Quick Translate" command translates
the text selected in the active application
and displays the result using the "Quick Translate Results" command.
The source language can be automatically detected
by selecting "Auto-Detect" from the dropdown in the upper-right.
Alternatively you can specify the source language
by selecting it in the dropdown.
By default, this command only translates to English.
I don't see a way to change this.
See https://github.com/raycast/extensions/issues/33547.

The "Quick Translate Results" command display the results
from the last run of the "Quick Translate" command.

All the commands whose name begins with "Instant Translate"
translate the selected text in the active application
to the "Primary Language" selected in the extension settings.
None of them open a Raycast window.

The "Instant Translate Copy" command copies the result to the clipboard.

The "Instant Translate Paste" command pastes the result over the selected text.

The "Instant Translate View" displays the result in a popup
whose display duration scales with text length.
The result is display-only and cannot be manually copied.

I created a quicklink that uses the Google Translate website.
It uses the URL
"https://translate.google.com/?sl={argument name="from"}&tl={argument name="to"}&text={argument name="input"}&op=translate"
and the name "My Google Translate", and the alias "translate".
This prompts for from and to language codes and input text.
For example, enter "en", "es", and "What time is it?"
opens a web page the displays "¿Qué hora es?".

### Light/Dark Mode

The command "Toggle System Appearance"
toggles the system between light and dark mode.
I assigned the hotkey rightCmd-t to this command.

### Math

To evaluate a math expression, enter it.
For example, "sin(19deg)^2 + cos(19deg)^2" evaluates to 1.

To copy the answer so it can be pasted elsewhere, press the return key.

To compute percentage increases, enter an expression like
"943 + 3.6%" which evaluates to "976.948".

To perform a unit conversion, enter an expression like
"26.2 miles in kilometers" which evaluates to "42.1648128 km"
or "1 gal in liters" which evaluates to "3.785411784 L".

### MDN Documentation

To view specific documentation in the MDN site
(formerly known as the Mozilla Developer Network),
install the "Search MDN" extension, run the "Search Web Docs" command,
enter search text, select a matching topic, and press the return key.

### Menu Items

The "Search Menu Bar Items" command searches for
menu items in the current application and can select them.
For example, when the Finder is the current application,
searching for "list" reveals that
the View menu contains the "as List" command.

### Processes

To kill the process listening on a given port,
run the "Kill Process Listening on" command.
Enter the port and press the return key.
(NOTE: This doesn't seem to work if
the process was started with "node --watch".)

### Quick Fix

The "Quick Fix" command corrects spelling and grammar errors in selected text.
By default, its hotkey is pressing the right shift key twice.
For example, it changes "Ths txt has meny speling erors."
to "This text has many spelling errors."

### Quicklinks

Quicklinks provide a fast way to open a file, folder, URL,
or application (with deep linking).
They can also provide a fast way to run a Raycast command
with a URL starting with raycast://extensions/.

To create a quicklink for a file or folder,
open Raycast, find and select the file or folder,
press cmd-k, select "Save as QuickLink",
enter a name, optionally select an icon,
select the application in which to open it,
optionally enter tags, and press cmd-return to save it.
For example, you can create a quicklink that
opens a frequently used spreadsheet in the Numbers app or
opens your Downloads folder in a new Finder window.

To create a quicklink to a URL,
run the "Create Quicklink" command and enter or paste a URL.
If the URL was currently being browsed, it will be filled in automatically.
There are buttons that can be clicked to
add a [dynamic placeholder](#dynamic-placeholders) in the URL
or to create a URL for a file or folder.
The icon will default to the favicon of the web page.
Then enter a name, optionally select a different icon,
select the web browser in which to open it, optionally enter tags
and press cmd-return to save it.

A quicklink URL can contain [dynamic placeholder](#dynamic-placeholders).

To open a quicklink, enter part of its name,
select it, and press the return key.
Alternatively, press cmd-k and select "Open With..."
to select the application in which to open it.

To see a list of all the quicklinks that have been created,
run the "Search Quicklinks" command.
The quicklinks are ordered by how frequently they have been used,
not alphabetically.
Select a quicklink to act on it. Then ...

- to open it with its specified application, press the return key
- to open it with a different application,
  press cmd-k, select "Open With...", and select the application
- to pin it to the top of the list, press cmd-k, and select "Pin Quicklink"
- to edit it, press cmd-k, and select "Edit Quicklink"
- to add a hotkey, press cmd-k, select "Configure QuickLink...",
  select "Set hotkey...", press the hotkey combination,
  and press the return key
- to specify an alias, press cmd-k, select "Configure QuickLink...",
  select "Set alias...", enter the alias, and press the return key
- to delete it, press cmd-k, and select "Delete Quicklink"

I created the following quicklinks, some with an alias and/or hotkey:

- Bank of America website, hotkey rightCmd-b
- my Github web page, alias "mg"
- my Google Translate, alias "translate"
- Project Gutenberg web page (for free e-books), alias "pg"
- Remove Background Image website, alias "bg"
- my retirement spreadsheet, alias "retirement"
- my RunningLog spreadsheet, alias "runlog"
- Star of Honolulu website, alias "soh"

### Reminders

To add Raycast commands for interaction with the Apple Reminders app,
install the "Apple Reminders" extension.

To create a new reminder, run the "Create Reminders" command.
This opens a dialog where the details can be entered.
Press cmd-return to create it.

I assigned the hotkey rightCmd-r to open the Apple Reminders app.

### Schedule

To see scheduled Calendar app items, run the "My Schedule" command.
This displays calendar items in groups listed in the following order:
Today, Next Week, Rest of {current month}, and each month that follows.
I gave this the alias "cal".

To view an item in the Calendar app,
double-click it (or select it press the return key).

For other operations on an item, select it and press cmd-k.
These include "Create Event", "Edit Event", and "Delete Event".

### Script Commands

Script Commands are used to run scripts written in many languages including
AppleScript (macOS-only), Bash, C#, Go, JavaScript,
PowerShell (Windows-only), Python, Ruby, Swift, and TypeScript.

Raycast extension to be published in the official store
must be implemented in JavaScript/TypeScript and can use React.

Let's walk through the steps to create a simple script command in TypeScript.

- Open Raycast.
- Run the "Create Script Command".
- Choose a template. I chose "TypeScript".
- Choose a mode.

  - Compact:
    This displays the last line of output in the status bar
    in the lower-left of the root search window.
    Press cmd-t to display options, including Copy with cmd-option-c.
  - Silent:
    Use this for scripts that are only run for side effects
    rather than displaying output.
    The last line of output is briefly flashed near the bottom of the screen.
  - Full Output:
    This expands into a scrollable terminal-like view
    where all output is displayed.
    It is ideal for scripts that produce multiple lines of output.
  - Inline:
    This mode is ideal for scripts that need to run repeated
    at a specific time interval and display a new value.
    These scripts don't require manual execution.
    They instead run automatically when Raycast is opened
    and run repeatedly on a set @raycast.refreshTime
    The value must be specified in seconds (like 30s; minimum is 10s),
    minutes (like 1m), hours (like 1h), or days (like 1d).
    The first line of their output is displayed
    after the command name in the root search (inline).
    Consider selecting "Add to Favorites" for these commands
    so they always appear near the top of the root search.
    See the example later in this section.
    Raycast only automatically refreshes the first 10 inline script commands.
    Any beyond that must be refreshed manually by selecting them
    and pressing the return key.

- Enter a title, description, and package name (optional?).
- Optionally check the "Needs confirmation" checkbox.

  If checked, when the script is triggered, a confirmation dialog will appear,
  to confirm that you really want to run it.
  The dialog will contain the script name,
  the text "Are you sure you want to run this script?",
  a "Cancel" button, and a "Run Script" button.
  This prevents accidental runs which is useful for scripts that
  perform destructive, irreversible, or high-impact actions
  like quitting all applications or restarting the computer.

- Optionally check the "With argument" checkbox.

  If checked, when the command is selected in the root search,
  a single, inline input field will appear after the command name.
  By default, it will have the placeholder text "Placeholder".
  To change this, modify the following configuration comment
  inside the script file:
  `// @raycast.argument1 { "type": "text", "placeholder": "your name" }`

  After entering an argument value, press the return key.
  The script can access this value as a command-line argument.
  In JavaScript, it can be accessed with `process.argv[2]`.

- Verify the "Author Name" and "Author URL".
- Press cmd-return.
- Verify the "Save As" file name.
- Select the directory where it will be saved.
- Click the "Save" button.

The generated script will just output "Hello World!".
Modify it to change its functionality.

To run the script:

- Run the "Search Script Commands" command.
- Select the script in the left nav.
- Press the return key to execute it.

After a script has been run for the first time,
it can be found by entering part of its name in the root search.

To assign an alias or hotkey to a script command,
enter part of its name in the root search,
select the script command, press cmd-k,
select "Configure Command...", and
click "Set Hotkey..." or "Set Alias...".

After modifying a script file, it may be necessary
to run the "Reload Script Commands" command.

Here's an example of a script that runs in inline mode
and displays the current temperature at your location.

```js
#!/usr/bin/env node

// Required parameters:
// @raycast.schemaVersion 1
// @raycast.title Current Temperature
// @raycast.mode inline

// Optional parameters:
// @raycast.icon ☀️
// @raycast.description This uses Open-Metio to get the current temperature at your location.
// @raycast.refreshTime 1m

// Documentation:
// @raycast.author mvolkmann
// @raycast.authorURL https://raycast.com/mvolkmann

// Create a free ipapi.is account at https://ipapi.is/app/signup
// and get its API key.
const apiKey = 'insert-isapi-key-here';

async function getLocalTemperature() {
  let res = await fetch(`https://api.ipapi.is/?key=${apiKey}`);
  if (!res.ok) throw new Error('Failed to resolve location');

  let json = await res.json();
  const {latitude, longitude} = json.location;

  const url = `https://api.open-meteo.com/v1/forecast?latitude=${latitude}&longitude=${longitude}&current=temperature_2m&temperature_unit=fahrenheit`;
  res = await fetch(url);
  if (!res.ok) throw new Error('Failed to fetch weather data.');

  json = await res.json();
  const temp = json.current.temperature_2m;
  const unit = json.current_units.temperature_2m;
  console.log(`${temp}${unit}`);
}

getLocalTemperature();
```

### Settings

To export all your Raycast settings into an encrypted, binary file:

- Run the "Export Settings & Data" command.
- Press the return key.
- Enter a password for encrypting the file.
- Enter it again to confirm.
- Select the directory where the file will be saved.
- Click the "Export" button.

This creates a file named
`Raycast-X-{year}-{month}-{day} {hour}.{minute}.{seconds}.rayconfig`.
The file describes data in many categories including
Clipboard History, Quicklinks, Raycast Notes, "Settings, Aliases & Hotkeys",
Snippets, Store Extensions, and Window Management Layouts.

To use those settings on another computer,

- Copy the `.rayconfig` file to the computer.
- Run the "Import Settings & Data" command.
- By default, all data categories are included.
  Uncheck the categories that should not be imported.
- Click the "Import" button.
- Select the `.rayconfig` file.
- Click the "Select File" button.

TODO: Are there any other steps?

When the "Store Extensions" checkbox is checked,
any store extensions (not local extensions) listed in the `.rayconfig` file
that aren't already installed will be installed.

### Shortcuts

Shortcuts provide an easier way to access items.
There are two kinds, aliases and hotkeys.
Aliases can be typed in the Raycast root search input
to find the associate item.
Hotkeys activate the associated item and are global,
which means they work regardless of the current application.

Aliases can be assigned to items to simplify selecting them.
When part of an alias is typed in the root search,
associated items are listed first.
For example, I assigned the alias "kn" to the Keynote application.
To assign an alias to an item, select the item, press cmd-k,
select "Configure {item-type}...", select "Set Alias...",
enter an alias, and press the return key.

The same alias can be assigned to any number of items.
Entering the alias displays all matching items.

To assign a hotkey to an item, select the item, press cmd-k,
select "Configure {item-type}...", select "Set Hotkey...",
press a key combination, and press the return key.

The same hotkey cannot be assigned to multiple items.
If this is attempted, the message "Already used by {name}" appears.
Press the return key to remove the previous assignment and keep this one.
Press the escape key to keep the previous assignment.

To see all the shortcuts that have been defined in Raycast,
run the Shortcuts command associated with "Raycast Settings",
not the Shortcuts application.
This lists items in five alphabetical groups which are
Applications, Extensions, Quicklinks, System, and System Settings.
Each section can be collapsed or expanded to show its items.

Extensions can have any number of associated commands
that each can have their own shortcuts.

The row for each item shows the item name,
its alias if any, and its hotkey if any.
The checkmark at the end of each row
allows its shortcuts to be temporarily disabled.
Most items will likely not have an alias or hotkey.

### Sleep

To cause the computer to sleep now, run the "Sleep" command.

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

A snippet can contain [dynamic placeholder](#dynamic-placeholders)
For an example, see my @log snippet.

### SVG Search

The Svgl extension provides the command "Search SVG Logos"
which finds tech-related SVG logos for things like
frameworks, cloud providers, developer tools, social platforms, and AI tools.
Enter text to find matching logos.
Press the return key to copy it to the clipboard.

### System Info

The "System Info" command provides information about your computer such as
CPU usage across all cores, memory usage, memory pressure, top processes,
and disk usage.

Time Machine backups are each listed as separate disks in this display.

### System Settings

To open a specific section within the System Settings app,
enter its name, select it, and press the return key.
Examples include Accessibility, Appearance, Audio, Battery, Desktop & Dock,
Displays, General, Keyboard, Mouse, Network, Notifications, Passwords,
Printers & Scanners, Privacy& Security, Sharing, Software Update, Sound,
Spotlight, Storage, Time Machine, Touch ID & Password, Users & Groups,
Wallet & Apple Pay, Wallpaper, and Wi-Fi.

### Time

To find the current time in another city,
enter "time" followed by a city name.
For example, "time Honolulu".

### Trash

To empty the trash, enter the "Empty Trash" command.

### Typing Practice

To practice typing and measure speed, run the "Start Typing Practice" command.

### Window Management

Window Management is a built-in extension that,
at last check in macOS, provides 70 commands for things like
resizing and repositioning the active window.

"Save Current Layout" is a Pro command that
opens a prepopulated layout editor that can save the current
multi-application arrangement and create a new command.
When that command is run, it launches any applications
associated with the saved windows that aren't already running,
opens each of the windows, sizes them, and positions them
as they were when the layout was created.
Like any other command,
an alias and a hotkey can be assigned to the new command.

I assigned the following aliases and hotkeys to Window Management commands:

- Left Half - lh, rightCmd-left arrow
- Right Half - rh, rightCmd-right arrow

- First Third - lt
- Center Third - ct
- Last Third - rt
- First Two Thirds - l2t
- Last Two Thirds - r2t

- First Fourth - lq (q for quarter), cmd-shift-left arrow
- Center Half - ch, cmd-shift-c
- Last Fourth - rq, cmd-shift-right arrow

- Maximize - max, cmd-shift-m

- Move to Next Display - nd, cmd-shift-n
- Move to Previous Display - pd

## <a name="dynamic-placeholders">Dynamic Placeholders</a>

Dynamic placeholders are special text wrapped in curly braces
that are replaced by computed content.
They can be used in snippets, quicklinks, and AI commands (pro).

The supported kinds of placeholders include:

- argument (`{argument name="..."}`)
- selected text (`{selection}`)
- clipboard text (`{clipboard}`)
- current date (`{date}`)
- current time (`{time}`)
- current date & time (`{datetime}`)
- current day of the week (`{day}`)
- snippet value (`{snippet name="..."}`) - only used in snippets;
  nests the result of one snippet in another
- generated UUID (`{uuid}`)
- result of a math expression to be entered (`{calculator}`)
- cursor position (`{cursor}`) - only used in snippets;
  marks the position where the cursor will be placed after inserting the text
- focused browser tab (rarely used)

An example of using an argument placeholder is
`https://www.google.com/search?q={argument name="topic"}`.
When the quicklink is activated, prompts for the arguments will appear.
Also see the "My Google Translate" quicklink
described at the end of the "Language Translations" section.

An argument placeholder can specify a default value.
For example, `{argument name="planet" default="Earth"}`.

An argument placeholder can specify options
so one can be selected from a dropdown.
For example, `{argument name="color" options="red,green,blue"}`.

An argument placeholder can use the same value in multiple places.
For example, `foo {argument name="text"} bar {argument name="text"} baz`.
A snippet defined in this way will prompt for "text" once and
insert the same value for both arguments because they have the same name.

Placeholders can use one or more modifiers to modify their value.
These include `uppercase`, `lowercase`, `trim`,
`percent-encode`, and `json-stringify`.

A `date` placeholder can specify a `format`
to use a date format other than the default.

A `date`, `time`, `datetime`, or `day` placeholder
can include an `offset` to use a value other than their current value.
For example, `{date offset=+1d}` for tomorrow.

A `date`, `time`, `datetime`, or `day` placeholder
can include a locale to output the value
in a locale other than the default.

For more detail on placeholders in quicklink URLs,
see https://manual.raycast.com/dynamic-placeholders.

## Extensions

The Raycast store offers many free extensions,
none of which are installed by default.
To view them, run the "Store" command.

To see the extensions that are already installed,
open the Raycast settings (cmd-,).
They are listed in the left nav below "Window Management".

To install an extension from the store, select it,
press the return key to see details about it,
and press the return key again to install it.

There are two ways to uninstall an extension.

The first way uses the "Store" command.
Change the Category dropdown in the upper-right
to only show installed extensions,
select the extension to uninstall, press cmd-k,
select "Uninstall extension", and confirm.

The second way uses the Raycast settings screen.
All the installed extensions are listed in the left nav
below "Window Management.
Select an extension, click the ellipsis in the upper-right,
and select "Uninstall Extension".
Alternatively, you can disable the extension without uninstalling it
by click the switch in the upper-right.

When updates to extensions installed from the Raycast Store are available,
they are automatically updated in the background.
You can also run the "Check for Extension Updates" command
to manually trigger a check.

- Apple Reminders: integrates with the Reminders app;
  see the "Create Reminder" command
- Clean Keyboard: locks keyboard while cleaning it;
  see the "Lock Keyboard" command
- Coffee: prevents computer from going to sleep;
  see the "Caffeinate" and "Decaffeinate" commands
- GIF Search: searches for matching GIF images;
  see the "Search for GIFs" command
- Google Translate: translates text from one language to another
- Kill Process: displays all processes sorted on CPU percentage or memory usage,
  and can kill a selected process; can filter on name
- Port Manager: can kill the process listening on a given port, and more;
  see the "Kill Process Listening on" command
- Search MDN: displays MDN documentation; see the "Search Web Docs" command
- SF Symbols Search: find and copy SF symbols
  with the "Search SF Symbols" command
- Speedtest: measures internet speed; see the "Test Internet Speed" command
- Svgl: finds tech-related SVG logos
- System Monitor: displays information related to
  CPU usage, memory usage, power usage, network usage, and temperature;
  see the "System Monitor" command

More extensions to try:

- Apple Intelligence
- Brew
- Claude
- ChatGPT
- Google Gemini
- Messages
- Music
- Ollama AI
- OpenAI GPT
- YouTube Search
- Wikipedia

## Comparing to macOS Spotlight

macOS Spotlight now covers several of Raycast’s everyday uses,
though Raycast remains more customizable.
However, it seems that in most cases
the Spotlight equivalents are considerably slower.

The Raycast features that are also available in Spotlight include:

- **app launching**:
  Find and open applications.
- **file search**:
  Find and open files and folders.
- **calculator**:
  Calculate the results of math expressions.
- **conversions**:
  Convert between units and currencies.
- **clipboard history**:
  Search and reuse previously copied text, links, images, and files.
- **actions inside apps**:
  Perform app-specific tasks such as sending messages,
  creating reminders, and starting timers directly from Spotlight.
  Third-party apps can expose actions through Apple's App Intents API.
- **running shortcuts**:
  Find and execute workflows created in Apple's Shortcuts app.
- **short command aliases**:
  Spotlight's "Quick Keys" resemble Raycast aliases.
  For example, type `sm` to run the "Send Message" command.
- **menu command search**:
  Find commands from the active app's menu bar without navigating its menus.

The Raycast features that are not available in Spotlight include:

- **Windows support**:
  Use the same tool across platforms (macOS and Windows).
- **extensive extension store**:
  Install community-built integrations and utilities
  that provide their own interfaces and commands inside Raycast.
  Spotlight supports third-party app actions,
  but doesn’t offer an equivalent extension marketplace.
- **snippets and text expansion**:
  Save reusable text, insert it through search,
  or expand abbreviations automatically,
  with dynamic placeholders for templates.
- **custom Quicklinks**:
  Create named links to websites, files, and folders,
  including parameterized searches.
  For example, a command that searches a particular website
  for whatever you type.
- **more extensive window management**:
  Resize, position, and move windows between displays
  using commands and customizable hotkeys.
  macOS has its own window tiling, but Raycast
  supplies a broader command-driven interface.
- **global hotkeys for individual commands**:
  Assign a keyboard combination that runs a Raycast command
  immediately from any app.
  Spotlight's Quick Keys must be typed after opening Spotlight.
- **richer clipboard tools**:
  Pin and edit entries, extract text from images,
  paste in different formats, and turn clips into snippets.
  Raycast also supports much longer retention.
- **custom script commands**:
  Expose your own scripts as searchable commands
  with arguments and displayed output.
  Spotlight can run automation through Shortcuts,
  but lacks Raycast’s direct script-command system.
- **integrated, multi-model AI workspace**:
  Raycast Pro (paid) offers AI chat, reusable AI commands,
  model selection, and AI-powered extensions.
  This goes beyond launching an AI app or running an AI-enabled Shortcut.
