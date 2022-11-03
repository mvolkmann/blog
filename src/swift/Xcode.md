---
eleventyNavigation:
  key: Xcode
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/xcode/", "Xcode" %}
is an IDE from Apple for creating apps for
iPhone, iPad, macOS, Apple Watch, and Apple TV.

## Panels

The Xcode user interface contains many panels that are described below:

| Area      | Location     | Purpose                                       | Toggle With                                                       |
| --------- | ------------ | --------------------------------------------- | ----------------------------------------------------------------- |
| Navigator | left         | display files, search results, and more       | View ... Navigators ... {Hide\|Show Navigator} or cmd-zero        |
| Editor    | center left  | view and edit code and data in selected files | none                                                              |
| Canvas    | center right | view and interact with Previews               | Editor ... Canvas or cmd-option-return                            |
| Minimap   | right        | display zoomed out view of entire file        | Editor ... Minimap or cmd-ctrl-shift-m                            |
| Inspector | right        | view and editing details about selected item  | View ... Inspectors ... {Hide\|Show Inspector} or cmd-option-zero |
| Debug     | bottom       | display print output and more                 | View ... Debug Area ... {Hide\|Show Debug Area} or cmd-shift-y    |

## Navigator

The Navigator has nine icon buttons at the top.

- folder icon (first)

  This displays a tree view of files in the project.

  Files can be organized into groups.
  To create a new group, right-click on an
  existing group or file and select "New Group".
  This creates a group and a directory on the file system
  named "New Group" which can be renamed.
  Existing files can then be dragged into the new group.
  Another option is to select all the files to be moved to a subdirectory,
  right-click one of them, and select "New Group From Selection".

  To open a file, click its name.
  To "quickly open" a file by typing part of its name,
  select File ... Open Quickly..." or press cmd-shift-o.
  Both of these open a temporary view of the file,
  indicated by the file name in the tab being in italic.
  If there is already a temporary view tab and another file is opened, the
  temporary view content will be replaced by the new file content.
  To change the temporary view to a permanent view,
  double click its tab or edit its contents.

  To sort the files displayed in a directory shown in the Navigator,
  right-click the directory and select "Sort by Name" or "Sort by Type".

- "x" in square icon (second)

  This displays a list of files that have been modified (M)
  or added (A).

  Click a file to see its contents.

  Click the button in the upper-right containing a right and left arrow
  to see diffs.

  Click the button to the right of that one containing horizontal lines
  to display a menu of options including
  "Inline Comparison" and "Side by Side Comparison" (preferred).
  Oddly, in "Side by Side" mode the current version of the file
  is shown on the left and the previous version is shown on the right.
  See the "Source Control" section below for the steps to fix this.

  To discard all changes to a file, right-click the file name
  in the Navigator and select "Discard Changes in ...".

  Right-click a file to see a pop-up menu that includes the options
  "Commit" and "Discard Changes".
  Oddly it seems the only way to commit multiple files at once
  is to select "Commit..." from the "Source Control" menu.

- tree diagram icon (third)

  This displays an alphabetized list of files.
  Click a file to expand it to a list of the properties and methods it defines.
  These can be clicked to navigate to their definition.
  The "Hierarchical" and "Flat" options at the top of the list
  seem identical.

- magnifier glass icon (fourth)

  This is used to find and replace text in the project.

  <img alt="Xcode Find" class="keep-size"
    src="/blog/assets/xcode-find-navigator.png?v={{pkg.version}}">

  Click "Find" to optionally switch to "Replace" mode.

  <img alt="Xcode Find" class="keep-size"
    src="/blog/assets/xcode-find-navigator-dropdown1.png?v={{pkg.version}}">

  Click "Text" to optionally switch to searching for "References",
  "Definitions", "Regular Expression", or "Call Hierarchy".

  <img alt="Xcode Find" class="keep-size"
    src="/blog/assets/xcode-find-navigator-dropdown2.png?v={{pkg.version}}">

  Click "Containing" to optionally switch to
  "Matching Word", "Starting With", or "Ending With".

  <img alt="Xcode Find" class="keep-size"
    src="/blog/assets/xcode-find-navigator-dropdown3.png?v={{pkg.version}}">

  Click "In Project" to optionally limit the search
  to a specific project directory
  and select "New Scope..." for advanced options.

  <img alt="Xcode Find" class="keep-size"
    src="/blog/assets/xcode-find-navigator-dropdown4.png?v={{pkg.version}}">

  Click "Ignoring Case" to optionally switch to "Matching Case".

  <img alt="Xcode Find" class="keep-size"
    src="/blog/assets/xcode-find-navigator-dropdown5.png?v={{pkg.version}}">

- warning icon (triangle icon containing exclamation point; fifth)

  This displays "Buildtime" and "Runtime" issues.

- diamond containing minus (sixth)

  This displays a list of tests.
  TODO: Can they be run individually from here?

- spray can icon (seventh)

  This displays information about CPU, Memory, Disk, and Network usage.
  Click each of these to see detailed information.
  For example, clicking "CPU" displays information about thread usage.

- tag icon (eighth)

  This displays debugging breakpoints.
  TODO: Can they be disabled and/or deleted here?

- document icon (ninth)

  This displays historical information on recent project builds,
  including error messages.
  Click a build to see details.

## Editor

Code entered in an editor pane is checked for errors while typing.
Saving is not required.

Lines with errors are indicated with red bars in the right gutter.
By default these contains the number of errors on the line,
an error icon (circle containing an "X"), and the first error message.
Click anywhere in the red bar to fully display all the error messages.
Sometimes errors contain a "Fix" button that can be clicked
to automatically fix the error.

To hide the first error message, only displaying
the number of errors and the error icon,
select Preferences ... General ... Issues ... Show Minimized.
With this setting, click anywhere on a red bar,
to see the text for all the errors on the line.

To see basic documentation of a name in code, option-click it.
For more detail, click the "Open in Developer Documentation".
Alternatively, click the circled question mark at the top of the Inspector.
The Inspector will then display help for
anything name under the cursor in the Code Editor.

Command-click a name to display a context menu
that can include the following options:

- Jump to Definition - same as cmd-ctrl-click to bypass context menu
- Fold - collapses a code block to an ellipsis;
  double-click the ellipsis to re-open
- Show Quick Help - same as option-click to bypass context menu
- Callers...
- Edit All in Scope - with Vim keybindings must be in insert mode
- Rename
- Show SwiftUI Inspector - opens inspector in a dialog
  as an alternative to using Inspector on right side
- Extract Subview - creates a new view named "ExtractedView"
  that renders the selected view
- Embed in HStack
- Embed in VStack
- Embed in ZStack
- Embed in List
- Group - embeds in a `Group`
- Make Conditional - surrounds in an `if`/`else`
- Repeat - surrounds in a `ForEach` loop
- Embed... - embeds in a placeholder view named `Container`
  that must be renamed to a real view
- Extract to a Variable
- Extract to a Method
- Extract All Occurrences

Note that the "Extract Subview" option and the "Embed in \*" options
only appear a preview is displayed and a view name is command-clicked.

In the past there was a "Rename..." option in this menu.
It was moved to Editor ... Refactor ... Rename.

### Multiple Editor Panes

To open a file in a new split pane,
hold down the option key and click the file in the Navigator.
If the file is already open, to open another view in a new split pane
hold down the option key and click the existing editor tab.

Alternatively, click the button the upper-right
that is a rectangle containing a vertical line and a "+" to open
a split pane containing the same file in the the currently editor pane.

To open a new split below the current one instead of to the right,
open down the option key before clicking the button.

The dropdown menu to the left of this button enables
toggling the display of the Canvas area and much more.

### Matching Braces

To find the matching brace for a given brace,
hold down the command key and hover over a brace.

### Code Completion

Xcode provides great intellisense and code completion.

When accepting a function completion, only non-dimmed arguments are included.
To also include the dimmed arguments, hold down the option key
before selecting a completion.
To include a selected subset of the arguments,
type the beginning of each of their names in a single string and press return.
For example, `.frame(mina` gives `.frame(minWidth: , alignment: )`.

For function calls, press tab after entering each argument
to advance to the next argument.
If the last argument is a closure, press return to
automatically turn it into a trailing closure.

### Spell Checking

To configure Xcode to check spelling while typing, select
Edit ... Format ... Spelling and Grammar ... Check Spelling While Typing.
A faint red line will be displayed under all misspelled words.

### Code Folding

To enable code folding, select Xcode ... Preferences ... Text Editing
and check the checkbox for "Code folding ribbon".

To fold the code in any construct (function, argument list, struct, class, ...),
click the corresponding section in the code folding ribbon.

To unfold code, click the ">" in the code folding ribbon
or double-click "..." in the code.

To enable code folding,

### Multiple Cursors

To get multiple cursors so the same changes
can be made in multiple locations simultaneously:

- Move the cursor to the first location.
- If using Vim keybindings, enter insert mode.
- Hold down the ctrl and shift keys and click additional locations.
- Type the changes.

### Replacing a Rectangular Selection

- If using Vim keybindings, enter insert mode.
- Hold down the option key and
  select a rectangular region using the mouse or trackpad.
- Type the text that will replace the selected portion of each line.

### Rename All in Scope

One approach is to use refactoring as follows:

- Right-click one occurrence.
- Select Refactor ... Rename from the context menu.
- Make the change in the current instance and all instances will change.
- Click the "Rename" button in the upper-right.

Another approach is to use "Edit All in Scope" as follows:

- Place the cursor on one of the occurrences.
- Press cmd-ctrl-e.
- Make the change in the current instance and all instances will change.
- Press the return key or click outside the change.

## Console

The bottom section is not visible by default.
Drag the status row at the bottom up to expose it.

The bottom section has two subsections.
The left side of the bottom section is for the debugging.
The right side of the bottom section is for the console output
such as that from `print` function calls.

Click the trashcan icon in the lower-right to clear the output.
The output from `print` calls only appears when running in the debug mode.

Xcode used to support debug mode in Previews,
but now it is only supported in the Simulator.
This is why `print` output never appears when running in Preview.
This is really bad because the Simulator
takes much longer to start than Preview!

To make the console area appear automatically when new text is written to it,
select Xcode ... Preferences... ... Behaviors ... Generates output,
click the "Show" checkbox, and select "Variables & Console View".

## Playgrounds

Playgrounds are great for experimenting with the Swift programming language.

To create and use a Playground:

- Select File ... New ... Playground...
- Select a template such as "Blank" and press "Next".
- Enter a name for the playground.
- Select the directory where it will be saved.
- Enter code in the provided text editor.
- To run all of the code,
  click the blue triangle in the gutter after the last line,
  press cmd-shift-return, or select Editor ... Run Playground.
- To run only the code up to and including a specific line,
  hover over the line and click the play button that appears.
- Output from `print` calls appears in the console area at the bottom.
  The `print` function takes any number of arguments
  and outputs them with a space between each.
- If the console area is not visible, drag up from the bottom to expose it.

Xcode is slow at evaluating a playground.
It can take several seconds after saving a change
for it to identify syntax errors and run the code.

Playgrounds treat files under “Sources” as separate, unnamed modules.
To expose things defined in those files to the main playground code,
declare them as `public`.
They do not need to be "imported".

## Projects

To develop an app:

- Select File ... New ... Project... or press cmd-shift-n.

- Select a target platform such as "iOS".

- Select a template and press "Next".
  The templates are organized by types which include
  "Multiplatform" (for apps that run in multiple OSes),
  "iOS", "macOS", "watchOS", "tvOS", "DriverKit", and "Other".
  The templates are further divided into the categories
  "Application", "Framework & Library", and "Other".
  Usually "App" under the "Application" category is the desired template.

- Enter a product name, select a team,
  enter an "Organization Identifier" (can be your email address),
  and press "Next".

- Select the directory where the project should be saved and press "Create".

To open an existing app,
select File ... Open Recent ... {project-path} or
select File ... Open, navigate to the project directory, and click "Open".
Another way to open an existing Xcode project
is to double-click its `.xcodeproj` file.

## Building

To build a project, select Product ... Build or press cmd-b.

To hear sounds that indicate whether the build was successful or failed,
select Xcode ... Preferences ... Behaviors.
Then select "Succeeds, check "Play sound", and select a sound.
Repeat for "Fails" and select a different sound.

To see how long it takes each build to complete
after the text "Build Succeeded" in the title bar,
enter the following command in a terminal window:

```bash
defaults write com.apple.dt.Xcode ShowBuildOperationDuration -bool YES
```

## Clean Builds

Occasionally it is useful to delete all generated files and start over.
To do this, select Product ... Clean Build Folder or press cmd-shift-k.

## Running

To run an app:

- Select a simulator device or real device from the dropdown in the top center.
- Select Product ... Run or press cmd-r.

Devices must be connected to the computer via a cable
unless they have been configured to run wirelessly.
To configure a device to run apps wirelessly:

- Attach the device to the computer with a cable.
- Select Window ... Devices and Simulators.
- In the window that appears, select the device.
- Check the "Connect via network" checkbox.
- Ensure that the device is on the same Wi-Fi network as the computer.

To run an app on the device wirelessly:

- Disconnect the device from the computer.
- Select the device from the dropdown in the top center.
- Run the app.

## Previews

Previews allow testing a single view outside of the Simulator or a real device.

## Files

To open a file within a project in an editor pane,
select File ... Open Quickly... (or press cmd-shift-o),
enter part of the file name (at least three characters),
and click the file within the list that appears.
This also works with any kind of name including
types, variables, functions, structs, classes, properties, and methods.
It even works with framework types.

## Themes

To choose a different theme, select Xcode ... Preferences ... Themes.
From here you can add, delete, and select themes.
I like the "Classic (Dark)" theme.
This only affects editor windows. To change the entire Xcode UI
to use a dark theme regardless of the system setting,
select Preferences ... General and select "Dark" from the Appearance dropdown.

## Column Marker

To add a vertical line at a given column width,
select Xcode ... Preferences ... Text Editing,
check "Page guide column at:", and choose a column number.
The indentation amount and type (spaces vs. tabs)
can also be specified here.

## Code Folding

To enable code folding, select Preferences ... Text Editing ... Display ... Code Folding Ribbon.
This adds a code folding ribbon on the left side of editor panes.
Click in the ribbon to fold the code at the corresponding line.
To unfold, click again or double-click the ellipsis in the code.

## Vim Support

To enable use of many Vim key bindings, select Editor ... Vim Mode.
Dot commands and keyboard macros are not yet supported.

## Duplicating Files

To duplicate a source file, select it in the Navigator
and click File ... Duplicate.
Oddly the context sensitive menu that is displayed when
a file in the Navigator is right clicked does not include this option.

## App Name

To give the app a different name from the project:

- In the Navigator, select the root project directory.
- In the editor that appears, select the first entry under "Targets".
- Rename "Development Assets" under "Development".
- In the General tab under Identity, modify the "Display Name".
- In the Navigator, rename the root directory
  and the directory below that with the same name.
- TODO: I'm not confident all of these steps are correct or required.

## New Files

To create a new file within a project,
select File ... New ... File... or press cmd-n.
This will display a dialog that prompts for a file type
followed by another that prompts for the file name
and directory where it should be saved.

When a new `.swift` file is created,
by default it includes comment lines at the top
like these where several placeholders are replaced:

```swift
//
//  ___FILENAME___
//  ___PACKAGENAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  ___COPYRIGHT___
//
```

## Header Comments

To prevent Xcode from adding the comment lines above to new files, edit the file
`/Applications/Xcode.app/Contents/Developer/Library/Xcode/Templates/File Templates/Multiplatform/Source/Swift File.xctemplate/__FILEBASENAME__.swift`
and delete the line `//___FILEHEADER__` and the blank line that follows.

## Mapping Files to Targets

By default files are only accessible from their associated target.
If an app has multiple targets and a source file should be shared between them,
select the file in the Navigator, open the Inspector on the right,
and check the checkboxes for each target that should have access to the file.
