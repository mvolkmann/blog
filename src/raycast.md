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

Raycast ...

- is a native macOS app implemented in Swift.
- is a replacement for Spotlight that competes with utilities like Alfred.
- places an emphasis on supporting keyboard navigation for all actions.
- uses a "stack of pages" metaphor
- is notable in that makes it easy to create new extensions and
  submit them to its store so other users can install and use them.

Raycast extensions can add items to the macOS menu bar.
Clicking a menu bar item can reveal a list of items.
Clicking a list item can trigger an action such as opening a URL in Safari.

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

## Help

See the official
<a href="https://manual.raycast.com" target="_blank">Raycast Manual</a>.

There are many YouTube videos about using Raycast.

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
Select a favorite and press cmd-k to get
actions for moving it up or down in the list.

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

## Dictionay

To lookup a word in the dictionary, enter "def"
and select the "Define Word" command.

## Fun

See the "Confetti" command.

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

To show upcoming meetings in the macOS menu bar,
open the Raycast settings, select the "My Schedule" command,
scroll to the bottom of its settings,
and change the value of "Show Events in Menu Bar".
It's unclear whether this works with the Bartender utility.

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

## Emojis & Symbols

The "Search Emoji & Symbols" command displays a list of them.
To quickly find this command, enter "emo".

Enter a search term to filter the list.

Double-click an emoji or symbol to insert it at the current cursor position.

For example, enter "kiss" to see only the related emojis and symbols.

To enter an emoji in any Raycast input, type a colon.
This opens an emoji picker containing recently used emojis.
Type part of the name of an emoji to see others.

## Reminders

Install the "Apple Reminders" extension from the store
to add the following commands:

- Create Reminder

  This does what the name says.

- Menu Bar Reminders

  This is supposed to display reminders in the menu bar.
  Maybe it only displays them when their due date/time arrives.
  Or maybe this feature doesn't work with the Bartender utility.

- My Reminders

  This displays your reminders.
  By default only reminders for today and those overdue are show.
  To display other reminders, change the dropdown in the upper-right
  to "Scheduled" or "All".
  You can also select a specific list to see on the reminders in that list.

  Select a reminder and press the return key to mark it as completed.
  Press cmd-k to "Mark as Complete", "Open Reminder", "Edit Reminder",
  "Set Priority...", "Set Due Date", "Delete Reminder", and more.

- Quick Add Reminder

  This adds an unscheduled reminder to the Inbox.

## Snippets

Raycast snippets are text or emojis identified by keywords.
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

To create a snippet for an emoji, use the "Search Emojis" command
to find the desired emoji, press cmd-k, select "Save as Snippet",
optionally modify the name, and optionally modify the keyword.

## Store

The Raycast Store is the place to download more extentions.
To open it, open Raycast and enter "store".

It displays three features extensions (updated every 15 days),
a list of trending extensions, and
an infinite scrolling list of all extensions.
Filter the list by entering part of a name.

Popular extensions include:

- Apple Reminders - see the "Reminders" section above
- Brew - search, install, and update Homebrew formulae

  This adds the commands "Search", "Clean Up", "Upgrade",
  "Show Installed", and "Show Outdated".

- Color Picker
- GIF Search
- GitHub
- iTranslate - language translations and speech
- Kill Process
- Music - controls the Apple Music app

  This adds the commands "Next Track", "Previous Track",
  "Toggle Play/Pause", and "Add to Library".

- Node.js Evaluate

  This provides the "Evaluate Code" command
  for evaluating a JavaScript expression.

- Port Manager

  Enter "port" and select the "Open Ports" command.
  Filter the list by entering a port number.
  To kill the process that is listening on the port,
  press cmd-k and select "Kill".
  Confirm the method to use by selecting "With SIGTERM" or "With SIGKILL".

- Search MDN
- System Monitor
- tldr - searches tldr documentation pages

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
| Left Half        | cmd-left arrow        |
| Right Half       | cmd-right arrow       |
| Bottom Half      | cmd-down arrow        |
| Top Half         | cmd-up arrow          |
| Center Half      | cmd-shift-c           |
| Left Quarter     | cmd-shift-left arrow  |
| Right Quarter    | cmd-shift-right arrow |
| Maximize         | cmd-shift-m           |
| Next Display     | cmd-shift-n           |
| Previous Display | none                  |

I didn't assign the shortcut cmd-shift-p to "previous display"
because that shortcut is used in VS Code to open the command palette.

For the "Move" commands, you can turn off the "Center Window" option
so the window retains its current position in the opposite of the
move dimension rather then being centered on the target edge.
"Center Window" is a toggle in the settings
for each of the four "Move" commands.

## Creating Custom Extensions

Raycast extensions are implemented in TypeScript with
React (using a custom renderer like React Native), Node, and Markdown.
The custom renderer uses Raycast-specific components
rather than HTML and CSS.
AppleScript is used communication with macOS apps.

To create a new extension:

1. Open Raycast
1. Open the "Create Extension" command.
1. Select a template.

   The supported template types include:

   - None
   - Detail
   - Form
   - Grid
   - List and Detail
   - Menu Bar Extras
   - Script
   - Static List
   - Typeahead Search

1. Enter an extension name.
1. Enter a description.
1. Select a category.
1. Select the location where the project will be stored.
1. Enter a name and description of a command that will be defined.
1. Optionally describe more commands.
1. Click the "Create Extension" button.
1. From a terminal window, cd to the directory of the extension.
1. Enter `npm install` and `npm run dev`.
1. Open Raycast.
1. Select your command from the "Development" category and run it.

### VS Code

See the VS Code extension "Raycast" by tonka3000.
It provides commands for common tasks, debugging, auto-completion, and snippets.

### Manifest

The `package.json` file of an extension serves as its manifest
where many aspects of an extension are specified.
This includes the extension name, title, description,
icon, author, categories, keywords, preferences, license, commands, and more.

For each command, a JSON object specifies its
name, title, subtitle, description, icon, mode,
interval, keywords, arguments, and preferences.
The `mode` of a command must be set to "view", "no-view", or "menu-bar".

If the extension exports a default function, it will be called automatically.
If the function returns a React component,
it will rendered as the root component.
The default function for commands that do not require a user interface,
such as those with `mode` set to "no-view", can by `async`.

The supplied `package.json` file for a new extension will contain the following scripts:

```json
"scripts": {
  "build": "ray build -e dist",
  "dev": "ray develop",
  "fix-lint": "ray lint --fix",
  "lint": "ray lint",
  "publish": "npx @raycast/api@latest publish"
}
```

The "fix-lint" and "lint" scripts perform some Raycast-specific validations,
and also run both ESLint and Prettier.

### Arguments

Each extension command can take up to three arguments.
The only supported types are "text", "password", and "dropdown".
The arguments for a command are described in the JSON description
of the command found in the `package.json` file for the extension.
The order in which argument inputs will appear
matches their order in the `package.json` file.

There is no support for validating argument values.
For example, it is not possible to require an argument value to be an integer.
An extension perform its own validation and display and error message
if an argument has an invalid value. For example:

```ts
function isInteger(str: string): boolean {
  return Number.isInteger(parseInt(str, 10));
}

...

  const {port} = props.arguments;
  if (!isInteger(port)) {
    showToast({
      style: Toast.Style.Failure,
      title: 'Bad Port',
      message: 'The port must be an integer.'
    });
    return;
  }
```

### Forms

Extensions can display forms that allow users to enter several values.
Each field has a label and an input area.
Each field can have custom validation logic.

### HTTP Requests

Extension commands can fetch data by sending an HTTP request.
There are several ways to do this including
the `useFetch` hook from "@raycast/utils" and
npm packages like `node-fetch` and `got`.

### UI Components

Raycast supports a limited set of UI components.
This makes it easy for users to use extensions
because there is a low learning curve for interaction patterns.

The supported UI components include:

- Action Panel

  This component displays a list of actions that can be performed.
  The list can be context-sensitive, based on the selected item.
  For example, each `List.Item` in a `List` can have its own `ActionPanel`,
  specified in its `actions` attribute.

- Actions

  These components appear as children of an `ActionPanel` component.
  There are built-in actions that perform specific actions when selected,
  including `CopyToClipboard`, `CreateSnippet`, `CreateQuicklink`,
  `Open`, `OpenInBrowser`, `OpenWith`, `Paste`, `PickDate`, `Push`,
  `ShowInFinder`, `SubmitForm`, `ToggleQuickLook`, and `Trash`.

- Colors

  This is not a component, but is a way to specify standard colors,
  dynamic colors (based on the current theme), and raw colors.

  Standard colors include `Red`, `Orange`, `Yellow`, `Green`,
  `Blue`, `Magenta`, `Purple`, `PrimaryText`, and `SecondaryText`.

  Currently only two themes are supported, light and dark.

  Raw colors can be specified with
  hex (ex `#FF0000`), short hex (ex. `#F00`),
  RGBA (ex. `rgb(255, 0, 0, 1.0)`), or
  HSL (ex. `hsla(100, 30%, 25%, 0.4)`).

- Detail

  This component renders a string of Markdown.
  It can display a panel of metadata on the right side.
  It can also have an associated `ActionPanel`.

- Form

  This component renders a form containing
  any number of inputs and a submit button.
  Each input can specify its type, validation constraints, and error display.
  The form only be submitted if there are no errors.

  The supported field types include:

  - `Form.Checkbox`

  - `Form.DatePicker`

    This component can be used to select a date or a date/time.
    It can limit the range of allowed dates.

  - `Form.Description`

    This component only displays text.

  - `Form.FilePicker`

    This component can be used to select one or multiple files or directories.
    It can be configured to allow or disallow seletion of files or directories.

  - `Form.LinkAccessory`

    This compoennt is a hyperlink.

  - `Form.TextArea`

  - `Form.TextField`

  - `Form.PasswordField`

  - `Form.Separator`

    This component just renders a horizontal line.

  - `Form.TagPicker`

    This component allows selected any number of items from a predefined list.
    Each item has a title, value, and optional icon.

- Grid

  This component is a alternative to a `List` which
  allows any number of items to appear on the same row.
  It is ideal for display a collection of images.

- Icons & Images

  The `Icon` component renders an icon from a large set of provide icons.

  `Image.Mask` changes the shape of an image
  from a rectangle to a circle or rounded rectangle.

  Some components such as `List.Item` accept an `icon` attribute whose value
  is an `Icon` instances or a generic object with specfic properties.
  To render an image, use an object with a `source` property.
  Other supported properties include `fallback`, `mask`, and `tintColor`.

- List

  This component display a vertical list of item.
  The list can be filtered by entering text in a search input
  that filters the list as the user types (typeahead).
  List items can be made selectable.
  Related items can be grouped into sections
  that have titles and optional subtitles.

  TODO: What other features are available on list items?

- Navigation

  The `useNavigation` hook provides `push` and `pop` functions
  for navigating in the stack of pages for an extension.

### Toast Messages

Toast messages appear at the bottom of the Raycast window.
Three styles are supported: `Success`, `Failure`, and `Animated`.

Animated toasts can have their `style`, `title`, and `message` properties
modified after they are created.

To display a toast message:

1. Import these:

   ```js
   import {showToast, Toast} from '@raycast/api';
   ```

1. Call the `showToast` function.

   ```js
   showToast({
     style: Toast.Style.Failure, // or Success or Animated
     title: 'My Title',
     message: 'My message.'
   });
   ```

### LocalStorage

The `LocalStorage` object manages data for the current extension.
Extensions can only access their own data.
The object has the following methods:

- `allItems` retreives all stored items.
- `clear` removes all stored items.
- `getItem` gets the item with a given key.
- `removeItem` removes an item with a given key.
- `setItem` sets an item with a given key.

### Preferences

The Preferences API is used to make an extension configurable.
They can be specific to a command defined by the extension
or be shared by all commands defined by the extension.

All preferences marked as "required" must be supplied by the user
before they can use the commands.

Preferences are defined in the `package.json` file of an extension.
Each command object in the "commands" array can specify
a `preferences` property whose value is an array of objects.

For example:

```json
"preferences": [
  {
    "name": "defaultPort",
    "description": "specifies the default port to use if none is provided",
    "type": "textfield",
    "required": true,
    "placeholder": "Default Port"
  }
]
```

For more detail, see <a href="https://developers.raycast.com/information/manifest#preference-properties" target="_blank">Preference properties</a>.

## Utilities

The following utility functions are provided in "@raycast/api".

- `clearSearchBar` clears text in the search bar.
- `closeMainWindow` closes the main Raycast window.
- `getApplications` returns an array of all applications that can open a given file.
- `getDefaultApplication` returns the default application for opening a given file.
- `getFrontmostApplication` returns the active application.
- `open` opens a given file, directory, or URL with a given application.
- `popToRoot` pops the navigation stack back to the root search.
- `showInFinder` opens the Finder app at a given directory path.
- `trash` moves a given file or directory to the trash.

The addon package `@raycast/utils` provides
more utility functions and React hooks.

The provided functions include:

- `getAccessToken` (OAuth)
- `getAvatarIcon`
- `getFavIcon`
- `getProgressIcon`
- `runAppleScript`
- `showFailureToast`
- `withAccessToken` (OAuth)

The provided React hooks include:

- `useAI`
- `useCachedPromise`
- `useCachedState`
- `useExec`
- `useFetch`
- `useForm`
- `useFrecencySorting`
- `usePromise`
- `useSQL` - queries a local database

TODO: Are any databases other than SQLite supported?

### Publishing an Extension

Here's a checklist of things to do in order to
submit an extension for review so it can be published in the store.

- Verify that all the properties and dependencies
  in `package.json` are what you want.

- Verify that `npm lint` doesn't find any issues.

- Verify that `npm build` runs successfully.

- Update the README.md file.

  It should clearly state the purpose of each command
  and the steps to use them.

- Create an icon and screenshots.

  Place these in the `assets` directory.

  See <a href="https://developers.raycast.com/basics/prepare-an-extension-for-store" target="_blank">Prepare an Extension for Store</a>
  for more detail.

- Update `CHANGELOG.md`.

- Run `npm run publish`.

  You will receive an email from raycastbot
  confirming that it is being reviewed.

- Wait for review.

  This is usually completed within five days.
