---
eleventyNavigation:
  key: HyperCard
layout: topic-layout.njk
---

<img alt="HyperCard logo" style="border: 0; width: 10%"
  src="/blog/assets/hypercard-logo.jpg?v={{pkg.version}}">
<br />
<img alt="HyperCard Welcome card" style="border: 0; width: 80%"
  src="/blog/assets/hypercard-welcome.jpg?v={{pkg.version}}">

## Overview

HyperCard is a Macintosh application for viewing and creating
multimedia applications built with HyperCard stacks
(a.k.a HyperCard applications) which are collections of cards.
Cards primarily contain text, graphics, and buttons.

HyperCard initially released in August, 1987.
The last version of HyperCard, released in 1988, was 2.4.1.
The oldest version of Mac OS that supports some version of HyperCard
is Mac OS 6.0.1.
The newest version of Mac OS that can run HyperCard is 9.2.2.

HyperCard was developed at Apple by Bill Atkinson and Dan Winkler,
using Apple Pascal.

The original version of the game Myst was developed in HyperCard.

The Home card contains buttons that navigate to commonly used stacks.

Changes to stacks are saved automatically.
There is no save button or menu item.

## Launching HyperCard

To launch HyperCard, double-click on the app icon
or on the icon of a HyperCard stack.

## Help

For popup help on a particular menu item or button,
select Help ... Show Balloons and hover over an item.
This is a Mac OS feature and is not specific to HyperCard.
To turn this off, select Help ... Hide Balloons.

## Basic Operations

## Stacks

To create a new stack:

- Select File ... New Stack...
- Enter a stack name.
- Select an option from the "Card size" dropdown with the following options:
  - Small: 416 x 240
  - Classic: 512 x 342
  - PowerBook: 640 x 400
  - Large: 640 x 480
  - MacPaint: 576 x 720
  - Window: 640 x 480 (same as Large)
- Optionally check "Open stack in new window".
- Click the Save button.

To make it easy to open your stacks from the Home stack:

- Press cmd-h to open the Home stack.
- Go to Card 3, 4, or 5 that all say
  "You can add your own buttons to this card."
- Add a button to one of those cards.
- Select the Button tool.
- Double-click the button open its Info dialog.
- Change the "Button Name" to the name of your stack.
- Click the "Script..." button.
- Enter the following, replacing the path with one for your stack:

  ```text
  on mouseUp
    go to stack "MacOS_HD:Documents:HyperCard Stacks:My First Stack"
  end mouseUp
  ```

- In the Home stack, optionally rename both the card named "Card 3"
  and the button that contains "Card 3" to "My Stacks".

- Change "go to" to "open" to open the stack in a new window.
- Select the Browse tool.
- From now on you can click this button to navigate to your stack.

To get information about the current stack:

- Select Objects ... Stack Info... to open a Stack Info dialog.

  This enables renaming the stack, resizing it, and editing its script.
  It also gives the number of cards in the stack
  and the number of backgrounds used.

To edit a background used by the current card:

- Select Edit ... Background or press cmd-b to toggle background mode.
  This is indicated by a hatched pattern in the menu bar.

To share a stack with others, send them the stack file.
They can open the stack by double-clicking it,
or by launching HyperCard and selecting File ... Open Stack...

## Cards

To add a card to the current stack, select Edit ... New Card or press cmd-n.

To configure the current card, select Objects ... Card Info...
This enables editing the card name, its script, and checking the checkboxes
for "Card Marked", "Don't Search Card", and "Can't Delete Card".

To delete the current card:

- Select Edit ... Delete Card.

To copy the current card and paste the copy after another card:

- Select Edit ... Copy Card.
- Navigate to the destination card.
- Select Edit ... Paste Card.

The background of new card will be the same as that of the copied card.

Sadly there is no easy way to:

- reorder the cards in a stack
- change the background used by an existing card

This seem like large oversights!

## Tools

The tools menu contains a grid of buttons
that can be clicked to select one of the 18 tools.
Drag the Tools menu off the menu bar
to create a floating palette of tool buttons.

<img alt="HyperCard Tools" style="border: 0; width: 20%"
  src="/blog/assets/hypercard-tools.png?v={{pkg.version}}">

The tools include:

- Row #1

  - **Browse**

    This enters Browse mode which enables interacting with cards as a user
    rather than as an author.

  - **Button**

    This enters Button mode which enables editing existing buttons.
    When in this mode, a thin black border is drawn around all buttons
    so they can be located even when they are transparent with no text or icon.

  - **Field**

    This enters Field mode which enables editing existing fields.
    When in this mode, a thin black border is drawn around all text fields
    so they can be located even when they are transparent with no text.

- Row #2

  - **Rectangle Selection**

    This selects a rectangular area of a drawing
    which can then be copied, cut, deleted, dragged, or transformed
    (using the Options menu items Rotate, Slant, Distort, and Perspective).

  - **Lasso Selection**

  This selects an arbitrary area of a drawing
  which can then be copied, cut, deleted, or dragged.

  - **Pencil**

    This is used for freehand drawing.
    The pencil width is a single pixel and cannot be changed.

- Row #3

  - **Brush**

    This is used for freehand painting.
    To change the shape and size of the brush,
    Select Options ... Brush Shape... before painting.

  - **Eraser**

    Drag over painted items to erase them.
    The size of the eraser cannot be changed.
    To erase large areas, use the Rectangle and Lasso selection tools.

  - **Line**

    Click at start of line and drag to end of line.
    To change the line size, Select Options ... Line Size... before drawing.

- Row #4

  - **Spray**

    This sprays paint using the currently selected pattern.

  - **Rectangle**

    This draws a rectangle, specified by clicking at the
    location of any corner and dragging to the opposite corner.
    To draw a square, hold down the shift key while dragging.

  - **Round Rectangle**

    This draws a rectangle with rounded corners, specified by clicking at the
    location of any corner and dragging to the opposite corner.

- Row #5

  - **Bucket**

    This fills an area with the currently selected pattern
    from the Patterns menu or palette
    when a pixel inside an enclosed area is clicked.

  - **Oval**

    This draws an oval which is any closed curve that
    resembles the outline of an egg or an ellipse.
    It doesn't have a strict mathematical definition.
    To draw a circle, hold down the shift key while dragging.

  - **Curve**

    This is similar to the Line tool, but will automatically
    close the shape if Options ... Draw Filled is selected.

- Row #6

  - **Text**

    This paints text that cannot be edited.
    The text style must be specified by selecting Edit ... Text Style...
    before typing the text.
    Then click where the text should go and begin typing.

    For text that may need to be edited later, use a text field instead
    and check the "Lock Text" checkbox in its Field Info dialog.

  - **Regular Polygon**

    This draws a convex polygon whose sides all have the same length.
    Select Options ... Polygon Sides...
    to select the number of sides to include.
    The options are 3, 4, 5, 6, 8, and circle.

  - **Polygon**

    This draws an arbitrary polygon.
    Click at each point in the polygon. Double-click to end.
    It does not automatically connect the last point to the first.

To undo the last drawing action, press the esc key or cmd-z.

To cause a drawn shape to be centered at the location of the initial click,
select Options ... Draw Centered.
This stays in effect until it is toggled off.

To select all or part of a drawing,
use the Rectangle or Lasso Selection Tools.
Shapes cannot be selected by clicking them.

To rotate the selected portion of a drawing,
select Options ... Rotate and drag any of the selection handles.
Other options include Slant, Distort, and Perspective.

To erase part of a drawing, do one of the following:

- Select the Erase tool and drag over the drawing.
- Select an area with the Rectangle or Lasso Selection Tools
  and press the delete key, or select Edit ... Cut Picture, or press cmd-x.

## Patterns

When the Bucket tool is selected, a Patterns menu appears.
This menu contains a grid of buttons that can clicked
to select the pattern that is used by the Bucket tool.
Drag the Patterns menu off the menu bar
to create a floating palette of pattern buttons.

<img alt="HyperCard Patterns" style="border: 0; width: 20%"
  src="/blog/assets/hypercard-patterns.png?v={{pkg.version}}">

To edit one of the provided 40 patterns,
select it and select Options ... Edit Pattern...
Then click or drag across pixels in the dialog
to toggle them between black and white.
I couldn't find a way to reset a pattern to its default state,
so be careful with these changes!

To cause drawn shapes to be filled with the currently selected pattern,
select Options ... Draw Filled.

## Buttons

There are four basic kinds of buttons:

1. Buttons that perform an action when clicked.
   These can contain text, an icon, both, or neither.
1. A check box
1. A radio button
1. Buttons that display a popup (dropdown menu) when they are clicked.

To add a button to the current card.

- Select Objects ... New Button.
  Alternatively, select the Button tool and
  cmd-drag to indicate the location and size of a new button.
  This adds a new button to the center of the current card.
- Double-click the button to open its "Button Info" dialog.
- Change "Button Name" to be a name used to refer to the button
  or the text that will appear on it.
  It's too bad buttons can't have separate values for
  the name used to refer to them and the label displayed in them.
- Choose a button style from the following options:

  - Transparent: default when created with cmd-drag
  - Opaque
  - Rectangle
  - Round Rect: default when created with "New Button" menu item
  - Shadow
  - Check Box: for a Boolean selection; click the checkbox or the name
    to toggle the value of its `hilite` property
  - Radio Button: for a set of mutually exclusive choices
  - Standard: TODO: Investigate
  - Default: TODO: Investigate
  - Oval: TODO: Investigate
  - Popup: a dropdown containing options

- Decide whether the button name should be displayed
  by checking or unchecking the "Show Name" checkbox.

  - It will be checked if the button was created with "New Button".
  - It will be unchecked if the button was created with cmd-drag.

- Optionally click the "Icon..." button to add an icon to the button.
  Icon sizes cannot be modified.
- Optionally cause the button to highlight when it is clicked
  by checking the "Auto Hilite" checkbox.
- Click the "Script..." button.
- Enter the command(s) to execute when the button is clicked.

To move a button:

- Select the Button tool.
- Drag the inside or edge of any button.

To resize a button:

- Select the Button tool.
- Drag any corner of a button.

To create a copy of a button,
option-drag it OR select Edit ... Copy Button.

To configure a button so clicking it navigates to another card or stack:

- Select the Button tool.
- Double-click a button to open its "Button Info" dialog.
- Option #1
  - Click the "Tasks..." button.
  - In the "Tasks" dialog, select a destination from the radio buttons
    that include "Back", "Home",
    "First Card", "Previous Card", "Next Card", and "Last Card".
- Option #2
  - Click the "LinkTo..." button.
  - Navigate to another card.
  - In the "Link" dialog, click the "This Card" button.

These are alternatives to writing a script.
They write the script for you, adding a `go` command
that is executed when the user clicks the button.

To edit the script for a button:

- Select the Button tool.
- Double-click a button to open its "Button Info" dialog.
- Click the "Script..." button.
- Enter commands.

To delete a button:

- Select the Button tool.
- Click a button to select it.
- Press the delete key, or select Edit ... Cut Button, or press cmd-x.

Buttons have the following properties:

- `autoHilite`: whether it highlights when clicked
- `blendLevel`: transparency level
- `enabled`: whether the button can be clicked
- `family`: for associating radio buttons
- `hilite`: whether the button is highlighted (or check box checked)
- `icon`: an icon ID
- `id`: unique identifier
- `name`: a name used to refer to the button or the text displayed on it
- `rectangle`: gives the position and size
- `sharedHilite`: whether the highlight state is shared (radio-style)
- `showName`: whether the name is shown
- `style`: one of the dropdown values
- `visible`: whether it is visible

Example button references include:

- `me` (when handler is attached to the button),
- by name: `button "{name}"`
- by id: `button id {id}`
- by number: `button {number-within-card}`.
- `the target` (when handler is attached the parent card)

If the button is on the card instead of the background,
optionally add `card` or `cd` before `button`.
If the button is on the background instead of the card,
add `background`, `bkgnd`, or `bg` before `button`.
If none of these keywords are added,
HyperTalk will assume the button is on the card.
This differs from the default for fields.

If the button is not on the current card, add `of card {card-reference}`.
If the button is not on a card in the current stack,
add `of card {card-reference} of stack "{stack-name"}`.

To make a section of an image clickable,
add a button that is transparent and has no label.
Unfortunately the clickable area must be rectangular.
Modify the button script to execute HyperTalk commands when clicked.

To detect whether special keys where held down during a click, check the
state of the special variables `commandKey`, `optionKey`, and `shiftKey`.
There is no support for detecting that the control key is down.
The following code demonstrates special key detection:

```text
on mouseUp
  if the commandKey is down then
    put "command" into whichKey
  else if the optionKey is down then
    put "option" into whichKey
  else if the shiftKey is down then
    put "shift" into whichKey
  else
    put "none" into whichKey
  end if
  answer "key =" && whichKey
end mouseUp
```

To allow a button to be dragged to a new location,
add the following handler in its script.
This isn't particularly useful, but it demonstrates changing
the location of an object based on the current mouse location.

```text
on mouseStillDown
  set the location of me to the mouseLoc
end mouseStillDown
```

### Check Boxes

To get the value of a check box,
use the command `get [the] hilite of {button-reference}`.

To put the value of a check box into the message box for testing,
use the command `put [the] hilite of {button-reference} into message [box]`.

### Radio Buttons

In order to make a set of radio buttons mutually exclusive,
the `family` property of each must be set to the same integer from 1 to 15.
To set the family number (1-15) of a radio button,
select a number from the Family dropdown or
enter the following command in the message box:

```text
set the family of {button-reference} to {family-number}
```

To get the name of the selected radio button:

```text
function selectedButtonName familyNumber
  repeat with i = 1 to the number of buttons
    if the family of button i is familyNumber then
      if the hilite of button i then
        return the short name of button i
      end if
    end if
  end repeat
  return empty
end selectedButtonName

on mouseUp
  put selectedButtonName(3) -- updates the message box
end mouseUp
```

HyperTalk can store the name or id of a button/field in a variable,
but it cannot store a button or field object in a variable.

## Popup

A button with the style "Popup" displays a dropdown list of options.
To specify the options, click the "Contents..." button
and enter each option on its own line.

To get the currently selected value of a Popup,
TODO: Finish this.

## Fields

A text field is referred to as simply a "field".

To add a field to the current card:

- Select Objects ... New Field.
  Alternatively, select the Field tool and
  cmd-drag to indicate the location and size of a new field.
  This adds a new field to the center of the current card.
- Click the browse tool (hand with pointing finger).
- Click the field.
- Enter text.

To change the font or styling of a field:

- Select the Field tool.
- Double-click a field to open its "Field Info" dialog.
  Alternatively, click a field to select it
  and select Objects ... Field Info...
- Select one of the following options from the Style dropdown:
  - Transparent - no border and background is transparent,
    showing what is behind it
  - Opaque - no border and background is opaque,
    hiding what is behind it
  - Rectangle - rectangle border with no shadow
  - Shadow - rectangle border with a shadow to lower-right
  - Scrolling - adds a vertical scrollbar;
    text wraps by default and there is never a horizontal scrollbar
- Click the "Text Style..." button.
- Select any of the style checkboxes.
- Select an alignment option (Left, Center, or Right).
- Select a font name.
- Select a font size.
- Select a line height.
- Click the OK button.

To move a field:

- Select the Field tool.
- Drag the inside or edge of any field.

To resize a field:

- Select the Field tool.
- Drag any corner of a field.

To create a copy of a field,
option-drag it OR select Edit ... Copy Field.

To delete a field:

- Click the Field tool.
- Click an object to select it.
- Press the delete key, or select Edit ... Cut Field, or press cmd-x.

Example field references include:

- `me` (when handler is attached to the button),
- by name: `field "{name}"`
- by id: `field id {id}`
- by number: `field {number-within-card}`.
- `the target` (when handler is attached the parent card)

The keyword `field` can be abbreviated to `fld`.

If the field is on the card instead of the background,
add `card` or `cd` before `button`.
If the button is on the background instead of the card,
optionally add `background`, `bkgnd`, or `bg` before `button`.
If none of these keywords are added,
HyperTalk will assume the field is on the background.
This differs from the default for buttons.

If the field is not on the current card, add `of card {card-reference}`.
If the field is not on a card in the current stack,
add `of card {card-reference} of stack "{stack-name"}`.

To get the text from a field, use the expression
`the text of card field "{field-name}"`.

To execute a HyperTalk command that was entered in a field,
run the command `do card field "{field-name}"`.

## Drawing

To draw a shape, select a tool from the Tools menu
and drag the mouse in a card.

## Sounds

To play a predefined sound, use the `beep` or `play` commands.
The `beep` command takes an optional argument
that specifies the number of times to play, defaulting to one.
The `play` command takes the following arguments:

- name of a sound, either boing or harpsichord
- optional keyword "tempo" followed by a number (seems to default to 120)
- optional set of notes to play in double quotes

For example, `play harpsichord tempo 120 "c4 e g4 e c5 q"`

To play a sound file, use the `play file` command.
TODO: What sound formats are supported?

## Backgrounds

Select Edit ... Background or select cmd-b
to toggle between background and foreground modes.

To add a new background to the current stack,
select Objects ... New Background.
This creates a new card that uses the new background,
but it will not be in background mode.
Press cmd-b to enter background mode and add content.
Then press cmd-b again to enter foreground mode and add content.
Cards created after this one will use the new background by default.

To assign a name to a background, select Options ... Bkgnd Info...
and enter/change the "Background Name".

It seems there is no way to change the background used by an existing card.
The only option seems to be copying the content from the existing card
to a new card that use the desired background.

## Card Transitions

TODO: Add detail on these.

## Images

One source of images is other stacks.

For example, the "Art Bits" stack is linked from
the first card of the Home stack.
It contains links to the cards with images in the following categories:
Beasts, Buildings, Communication and media, HyperCard miscellany,
Icon ideas, Macintosh miscellany, Nature and science, Odds and ends,
People, Small treasures, and Transportation.

Another source is the stacks linked from the "Stack Kit" card of the Home stack.
See "Readymade Buttons", "Readymade Fields", "Stack Templates",
and "Background Art".

To get an image, open a stack, find a desired image on one of its cards,
use the selection tools to select it, copy it, and
paste it onto a card in your stack.

TODO: What image formats can be pasted into cards?

## Color

Some amount of color support was added to HyperCard in version 2.3.
To use colors, install the color tools.
This requires more than the default amount of memory.
To add more memory:

- Quit HyperCard if it is running.
- Locate and select the HyperCard app in the Finder.
- Press cmd-i to open its Info dialog.
- In the Show dropdown, select Memory.
- Try changing the "Minimum Size" to 5000 K
  and the "Preferred Size" to 10000 K.
- Close the Info dialog.
- Launch HyperCard.

To enable use of colors:

- Click "Color Tools are OFF" on the Home card to toggle it ON.
  This will add a Color menu that contains the menu item "Open Coloring Tools".
- Select that menu item to open the color palette.
  This has buttons labeled Button, Field, Pict, Rect, and Paint at the top.
- Select one of the buttons to indicate
  the kind of object for which a color will be selected.
- Select a color.

TODO: Why does the Tools menu disappear when the Color palette is open?

## Keyboard Shortcuts

| Shortcut | Action                                                       |
| -------- | ------------------------------------------------------------ |
| cmd-h    | Home                                                         |
| cmd-~    | go to Back to previous card                                  |
| cmd-1    | go to First card in stack                                    |
| cmd-2    | go to Prev card in stack                                     |
| cmd-3    | go to Next card in stack                                     |
| cmd-4    | go to Last card in stack                                     |
| cmd-b    | toggle Background mode                                       |
| cmd-c    | copy                                                         |
| cmd-e    | open Scroll window                                           |
| cmd-f    | Find within current stack                                    |
| cmd-i    | open Icon editor                                             |
| cmd-m    | open Message box (where commands can be entered)             |
| cmd-o    | open a stack                                                 |
| cmd-l    | go to the next open HyperCard window                         |
| cmd-n    | New card                                                     |
| cmd-r    | open window of recently visited cards (click one to open it) |
| cmd-v    | past                                                         |
| cmd-x    | cut                                                          |
| cmd-z    | undo                                                         |

The "Open Stack" dialog contains:

- "Show Preview" checkbox that causes a thumbnail of the first card
  of the selected stack to be displayed inside the dialog.
- "Open stack in new window" which does what it says when a stack is opened.

The "Home" stack is inside the "HyperCard 2.4" directory
which I placed in the Applications directory.

## HyperTalk

HyperTalk ...

- is the scripting language used by HyperCard.
- has an English-like syntax.
- is interpreted at run-time, but cached for executing again later.
- supports message passing (events) and message handlers.
- implements a simple database.

The commands (a.k.a. statements) supported by HyperTalk
are documented at {% aTargetBlank
"https://www.hypercard.center/HyperTalkReference#commands",
"HyperTalk Reference" %}.

### Message Box

One way to test commands is to enter them in the message box.
To open the message box, select Go ... Message or press cmd-m.
Then enter commands separated by semicolons and press return to execute them.

To write a value to the message box, use the command
`put {expression} into [the] message [box]`.
The put command writes to the message box by default,
so the previous command can shorted to `put {expression}`.

### Comments

Single-line comments begin with `--` and extend to the end of the line.
Multi-line comments are not supported.

To comment a set of lines, select them and
press cmd-dash or select Script ... Comment.

To uncomment a set of lines, select them and
press cmd-equal or select Script ... Uncomment.

### Data Types

HyperTalk supports the following data types:

- booleans with the literal values `true` and `false`
- numbers with literal values that are either integers or floating point
- string with literal values delimited by double quotes
- string lists that are a single string with commas delimiting the items
- containers which are variables, buttons, and fields

To concatenate strings, or values that can be converted to strings,
use the binary operators `&` and `&&`.
The double ampersand adds a space between the values.

To extract a substring from a string list, use the `of` keyword.
For example:

```text
put "apple,banana,cherry" into fruits
put item 2 of fruits into fruit -- sets to banana
```

## Scripts

A HyperCard script is a collection of function and event handler definitions.
Often a script only contains a single handler definition.
However, to aid in finding a definition inside a long script,
the script editor includes dropdowns in the upper-right
for selecting and scrolling to a selected function or handler.

Scripts are associated with a specific object
such as a button, field, or card.
They handle messages that are triggered by many actions.

Scripts are implemented with the {% aTargetBlank
"https://en.wikipedia.org/wiki/HyperTalk", "HyperTalk" %} language
which has an English-like syntax.
The messages travel through the object hierarchy,
searching for an object that handles them.
The levels of the object hierarchy, from bottom to top are:

- buttons and fields
- cards
- backgrounds
- stacks
- Home stack
- HyperCard app

This is also the order in which scripts are most commonly defined,
with button scripts being the most common
and HyperCard app scripts being the least common.

You cannot control which scripts will exist
at the home card and HyperCard levels
when other users use your stacks,
so it is risky to depend on those.

When editing a script, press the tab key to format it
which indents the lines properly.

### Message Handlers

A single script can define any number of message handlers
that each begin with the `on` keyword.
Each message handler listens for a specific kind of event
and executes the code inside when triggered.
Unlike functions, message handlers cannot return a value.

For example, in a stack with two cards where the first card contains a button:

- Open the stack. The first card will be displayed.
- Select the Button tool.
- Double-click the button.
- Click the "Script" button.
- Enter the following:

  ```text
  on mouseUp
    go to next card
  end mouseUp
  ```

- Press cmd-s to save the changes.
- Press cmd-w to close the script window.

- Select Objects ... Card Info...
- Click the "Script" button.
- Enter the following:

  ```text
  on mouseUp
    beep
  end mouseUp
  ```

- Select Objects ... Stack Info...
- Click the "Script" button.
- Enter the following:

  ```text
  on mouseUp
    flash
  end mouseUp
  ```

- Select the Browse tool.
- Click outside the button.
- Notice that the `beep` command runs because the card script is run.
- Click the button.
- Notice that it navigates to the next card because the button script is run.
- While on the next card, click anywhere inside it
  except on a button or field.
- Notice that the window flashes because the stack script is executed.

Another interesting example:

```text
on mouseUp
  get the length of card field "user name"
  multiply it by 2
  answer it
end mouseUp
```

When changes to a script are saved,
the script is not checked for possible errors.
Any errors are only found when the script is run.

Message handlers for custom event names can be implemented in any script.
For example:

```text
on doubleBeep
  beep
  beep
end doubleBeep
```

To send an event, just use its name. For example, `doubleBeep`.
If no handler is found for the event, a dialog that says
"Can't understand {message}." will open.

Message handlers can have parameters that appear
after the event name in a comma-separated list.
For example:

```text
on add n1, n2
  answer "The sum is" && (n1 + n2) & "."
end add
```

To invoke this, use an expression like `add 2, 3`.

### Standard System Events

- `appleEvent`
- `arrowKey`: argument gives direction
- `closeStack`
- `deleteButton`
- `deleteField`
- `doMenu`
- `enterKey`
- `idle`
- `mouseDown`
- `mouseEnter`
- `mouseLeave`
- `mouseStillDown`
- `mouseUp`
- `mouseWithin`
- `newButton`
- `newField`
- `openCard`
- `openStack`
- `quit` of HyperCard
- `resume` of HyperCard
- `returnKey`
- `startup` of HyperCard
- `suspend` of HyperCard
- `tabKey`

### Constants

HyperTalk defines the following predefined constants:

- Boolean values `true` and `false`
- special characters `quote`, `return`, `space`, and `tab`
- `pi`: approximately 3.14159265359
- `empty`: empty string

- directions `up`, `down`, `left`, and `right`
- horizontal positions `left`, `center`, and `right`
- vertical positions `top`, `middle`, and `bottom`

- `it`: stores the result of some commands such as
  `ask`, `get`, and `read from file`.

- `result`: stores the result of some operations such as the following:

  - `go`: Sets result to an error message if the card/stack can't be found
  - visual effect: if the effect can't be completed
  - `open file` - sets result to an error message if the file can't be opened
  - `close file` - sets result if there's an error closing the file
  - `read from file` - sets result to "EOF" when the end of file is reached
  - `write to file` - sets result if the write operation fails
  - external commands (XCMDs) to indicate success or failure
  - external functions (XFCNs) to indicate success or failure
  - `create stack`: if an error occurs
  - `save stack`: if an error occurs
  - `create` - if object creation fails
  - `delete` - if object deletion fails
  - `answer`: sets to the button that was clicked
  - `answer file`: sets to the selected file path or empty if canceled
  - `do`: sets to any error message from the executed script
  - `value`: sets if the expression cannot be evaluated

  Typically `result` is empty if an operation succeeds
  and is set to an error message if it fails.

- `me`: refers to the current object
- `target`: refers to the object that received the message

- mouse positions `mouseH` and `mouseV`
- state of modifier keys `commandKey`, `optionKey`, and `shiftKey`

- `ticks`: number of ticks (1/60th of a second) since system startup

### Variables

Variables exist in two scopes, local to a specific handler
and global across all handlers in all stacks.

Local variables spring into existence when a value is assigned to them
and are not declared.

Global variables must be declared everywhere they are used
with the `global` keyword.
For example, `global favoriteColor, maximumTemperature, taxRate`
The values of global variables are not saved across HyperCard sessions.

To assign a value to a variable, use the `put` keyword.
For example, `put 3.14159265 into pie` (`pi` is a predefined constant)

It's a good idea to initialize all global variables used by a stack
in the `openStack` handler associated with the stack.
This avoids using values assigned by other stacks.

### go Command

The `go` command supports going to another card.
It supports many arguments described below.

- Ordinal

  - `go first` - 1st card in current stack
  - `go second` - 2nd card in current stack
  - `go third` - 3rd card in current stack
  - `go last` - last card in current stack
  - `go card {n}` - nth card in current stack

- Positional

  - `go next` - next card
  - `go prev` - previous card
  - `go this` - stay on current card

- Other ways to go to another card in the current stack

  - `go any card` - randomly selects a card?
  - `go bkgnd "{background-name}"

- Other ways to go to another card, possibly in another stack

  - `go back` - previously visited card
  - `go bkgnd "{background-name}" of stack "{stack-name}"`
  - `go card id {card-id}`
  - `go forth` - opposite of `go back` used after that command
  - `go home` - Home stack
  - `go [[to] stack] {stack-name} [in a new window]` -
    first card in a given stack (ex. "HyperCard Help")

### get Command

The `get` command sets the value of the special variable `it`
to the value of an expression.
The command `get {expression}` is equivalent to `put {expression} into it`.

The following are examples of using the `get` command:

```text
get 2 + 3
get the short date -- ex. 4/12/25; short is the default and can be omitted
get the long date -- ex. Saturday, April 12, 2025
get the value of card field "My Foreground Field"
get the value of background field "My Background Field"
get the label of button "My Button"
get the hilite of button "My Checkbox"
get item 2 of colorList -- where colorList is a comma-delimited string
get word 1 of "some long string"
get line 3 of field "My Text Area"
```

Two ways see the value of any variable, including `it`,
are to use the `put` and `answer` commands.
The `put` command writes the value to the message box.
The `answer` command opens a dialog box containing the value.
Both commands take an expression as an argument
which can be just a variable name.

Once the variable `it` is set by using the `get` command,
it can be used in subsequent expressions.
For example, `multiply it by 2`.

### put Command

The `put` command sets the value of a variable or object property.
For example:

```text
put "test" into card field "My Field Name" -- by name
put "test" into card field id 10 -- by id
put "test" into card field 2 -- by number
put the value of card field "user name" into myVariable
```

If the keyword "card" is omitted, it will only look for a background field.

### Math

HyperCard supports the arithmetic infix binary operators
`+`, `-`, `*`, `\`, and `mod`.
Normal precedence rules apply.
Parentheses can be used to change the evaluation order.

A "container" is a variable or field.

To add a number to a container that already contains a number,
use the `add` keyword. For example, `add 3 to total`.

To subtract a number from a container that already contains a number,
use the `subtract` keyword. For example, `subtract 3 from total`.

To multiply a number in a container that already contains a number,
use the `multiply` keyword. For example, `multiply total by 3`.

To divide a number in a container that already contains a number,
use the `divide` keyword. For example, `divide total by 3`.

### Conditional Logic

Conditional logic can be implemented with an if-then-else statement.
When the branches only contain single statements,
the following syntax can be used:

```text
if condition1
then statement1
else if condition2
then statement2
else statement3
```

When the branches can contain multiple statements,
the following syntax can be used:

```text
if condition1 then
  statements1
else if condition2 then
  statements2
else
  statements3
end if
```

### Iteration

To repeat a set of statements forever, or until `exit repeat` is reached,
use a `repeat` statement. For example:

```text
repeat [forever]
  ...
  if condition then exit repeat
  ...
end repeat
```

To repeat a set of statements a given number of times
use a `repeat for` statement. For example:

```text
repeat [for] 5 [times]
  ...
end repeat
```

To repeat a set of statements until a condition is true
use a `repeat until` statement. For example:

```text
repeat until cowsComeHome
  ...
end repeat
```

To repeat a set of statements while a condition is true
use a `repeat while` statement. For example:

```text
repeat while cowsAreGone
  ...
end repeat
```

To iterate over a range of integers, use a `repeat with` statement.
For example:

```text
repeat with name = start [down] to end
  ...
end repeat
```

### Mouse Events

The following mouse-related events are automatically triggered:

- `mouseDoubleUp` - double-click
- `mouseDown` - pressed but not released
- `mouseEnter` - entered
- `mouseLeave` - exited
- `mouseStillDown` - triggered continuously while over
- `mouseUp` - released
- `mouseWithin` - triggered on every move within

### Functions

Functions can be defined inside scripts.
They can have parameters.
Unlike message handlers, they can return a value.

Function definitions have the following syntax:

```text
function fnName -- no parameters
  ...
end fnName

function fnName param1, param2 -- parameters separated by commas
  ...
end fnName
```

The `return` keyword is used to return a value from a function.

Functions are called with the following syntax:

```text
fnName()
fnName(arg1, arg2)
```

The following function takes two numbers and returns their sum.
This can be defined in any script.
If it is defined in the stack script of the current stack,
it can be invoked from any card using the message box.

```text
function sum n1, n2
  return n1 + n2
end sum
```

To call this function and display its result in a dialog,
enter the following in the message box.

```text
answer sum(2, 3)
```

### Navigation

To go to another card or stack, use the following commands:

- `go first|next|prev|last`
- `go home`
- `go stack "stack-name"`

### Dialogs

To display a dialog box that displays specified text
and includes an "OK" button that can be clicked to dismiss it,
use the `answer` command. For example:

```text
answer "Good morning!"
```

To display a dialog box that asks the user a question
and includes a set of buttons they can click,
use the `answer` command.
By default, the buttons "OK" and "Cancel" are included.
The result will be in a variable named `result`.
For example:

```text
on mouseUp
  answer "Are you happy?" with Yes or No or Maybe
  put it into card field "user name" -- can change card to background
end mouseUp
```

To hide the value entered by the user, use `answer password`.

To display a dialog box that asks the user a question
and includes a field where they can type an answer,
along with OK and Cancel buttons, use the `ask` command.
The result will be in a variable named `it`.
For example the following script can be attached to a button:

```text
on mouseUp
  ask "What is your name?"
  put "Hello," && it & "!" into greeting
  put greeting into card field "user name"
end mouseUp
```

Using the `put` command to set `message` or `msg`
also opens the message box and puts it there.

### Messages

Message are sent when:

- a system event occurs such as opening a stack,
  opening a card, or clicking a button.
- a script explicitly sends an event with the `send` command
- the user sends an event from the message box

To send a message to the next level up in the object hierarchy,
use the `pass` command.
For example, `pass "messageName [parameterList]"`.

To send a message to another object, use the `send` command.
For example, `send "messageName [parameterList]" to objectReference`.

### Menu Items

To execute a menu item, use the command `doMenu`
with argument that is the menu item label.
For example, a button can create a new card with the following handler:

```text
on mouseUp
  doMenu "New Card"
end mouseUp
```

The new card will be inserted immediately after the current card.

### Long Running Tasks

For scripts that run for a bit, it is a good idea to
change the cursor to a watch until the script completes.
The supported cursor names include `arrow`, `busy`, `cross`,
`hand`, `iBeam`, `none`, `plus`, `wait`, and `watch`.

Here's an example:

```text
on mouseUp
  set cursor to watch
  wait 2 seconds
  set cursor to hand -- default
  answer "Finished!"
end mouseUp
```

### Debugging

TODO: Add content on debugging support.

TODO: What causes the Debug menu to appear? Can you trigger it?

## Search Paths

HyperCard searches specific directories to find
stacks, applications, and documents.
To view these directory lists:

- Open the home stack by pressing cmd-h.
- Click the left-pointing triangle in the lower-left two times.
- Click one of the buttons labeled Stacks, Applications, or Documents.
- Optionally edit the list of directories.

An alternative to modifying the directory lists is to
move your files into one of the directories already in the lists.

## User Levels

HyperCard supports the following five user levels:

- 5 Browsing: read-only
- 4 Typing: can enter text in fields
- 3 Painting: can use tools to draw and paint on cards
- 2 Authoring: can add and modify buttons and fields on cards
- 1 Scripting: can add and modify scripts associated with any object

To set the level for the HyperCard application as a whole:

- Open the Home stack by pressing cmd-h.
- Click the left-pointing triangle in the lower-left.
- Click one of the five levels.

Alternative, open the message box and enter
`set [the] userLevel to n` where n is a number from 1 to 5.

Each stack can specify its own user levels.
To do this:

- Set the HyperCard-wide user level to 5 (see above).
- Open the stack whose user level will be changed.
- Select Objects ... Stack Info.
- In the Stack Info dialog, click the "Script..." button.
- Enter the following which runs every time a user opens the stack:

  ```text
  on openStack
    set [the] userLevel to n -- replace n with a number from 1 to 5
  end openStack
  ```

To restore the highest user level for a stack with a restricted level,
open the message box (cmd-m) and enter `set [the] userLevel to 5`.

## Popular Stacks

- {% aTargetBlank "https://macintoshgarden.org/games/the-haunted-house",
  "The Haunted House" %} by Mark Klink

## Demos

### Experimenting With HyperTalk

<img alt="HyperCard Experimenting" style="border: 0; width: 60%"
  src="/blog/assets/hypercard-experimenting.png?v={{pkg.version}}">

Create a Field where arbitrary HyperTalk statements can be entered.
Set its Field Name to "code" and set its Style to "Scrolling".

Create a Button that executes the code using the following script:

```text
on mouseUp
  do card field "code"
end mouseUp
```

### Adding Numbers

<img alt="HyperCard Tools" style="border: 0; width: 50%"
  src="/blog/assets/hypercard-adding-numbers.png?v={{pkg.version}}">

```text
-- Script for "Add Number" button
on mouseUp
  ask "Number"
  put it & return after card field "numbers"
end mouseUp

-- Script for "Total" button
on mouseUp
  put 0 into total
  put the number of lines in card field "numbers" into lines

  repeat with i = 1 to lines
    put line i of card field "numbers" into line
    if line is a number then add line to total
  end repeat

  put return & "total =" && total after card field "numbers"
end mouseUp

-- Script for "Clear" button
on mouseUp
  put empty into card field "numbers"
end mouseUp
```

## Resources

- {% aTargetBlank "https://en.wikipedia.org/wiki/HyperCard",
  "HyperCard on Wikipedia" %}
- {% aTargetBlank "https://hypercard.org", "hypercard.org" %}
- {% aTargetBlank
  "https://www.youtube.com/playlist?list=PL1GQPxft2cQh_KsYz399Hlq7FHPp7XhBJ",
  "Eric's Edge" %} YouTube videos on HyperCard
- {% aTargetBlank "https://hcsimulator.com", "HyperCard Simulator" %} -
  web-based
- {% aTargetBlank "https://beyondloom.com/decker", "Decker" %}
  open-source, web-based HyperCard clone
- {% aTargetBlank "https://www.youtube.com/watch?v=x-FkNd5DkOQ",
  "[LIVE '21] Modifiable Software Systems: Smalltalk and HyperCard" %}
  YouTube video
- {% aTargetBlank "https://hypercard.org/HyperTalk%20Reference%202.4.pdf",
  "HyperTalk Reference" %} - a PDF containing screenshots
  of most of the pages of the HyperTalk Reference stack
  that came with HyperCard 2.4 (362 pages!)
  To see this inside HyperCard, open the message box
  and enter `go "HyperCard Help".
- {% aTargetBlank "https://www.hypercard.center/HyperTalkReference",
  "HyperTalk Reference" %} - another reference with links for each command
- {% aTargetBlank "https://archive.org/details/AppleMacintoshSystem753",
  "Macintosh System 7.5.3" %} on Internet Archive;
  click to begin; includes HyperCard
- {% aTargetBlank "https://livecode.com", "LiveCode" %} active,
  commercial alternative to HyperCard
