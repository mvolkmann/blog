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
They can also play sounds and videos.

The original application name was WildCard. It was changed
to HyperCard shortly before the initial release
because the name WildCard was already trademarked.

HyperCard initially released in August, 1987.
The last version of HyperCard, released in 1988, was 2.4.1.
The oldest version of Mac OS that supports some version of HyperCard
is Mac OS 6.0.5.
The newest version of Mac OS that can run HyperCard is 9.2.2.

HyperCard was developed at Apple by Bill Atkinson and Dan Winkler,
using Apple Pascal.

Bill Atkinson said of HyperCard that "It's programming for the rest of us."
HyperCard allows non-developers to create applications
that can easily be shared with others.

For a time, HyperCard came preinstalled on all Macintosh computers.

The original version of the game Myst was developed in HyperCard.

The first card in the default HyperCard Home stack
contains buttons that navigate to commonly used stacks.
Begin by clicking the "HyperCard Tour" stack
which provides a good overview of the application.
At any point, press cmd-h to return to the Home stack.

Changes to stacks are saved automatically.
There is no save button or menu item.

## Launching HyperCard

To launch HyperCard, double-click on the app icon
or on the icon of a HyperCard stack.
If the app icon was double-clicked, the Home stack will be opened.
The first card in the opened stack will be displayed.

## Help

For popup help on a particular menu item or button,
select Help ... Show Balloons and hover over an item.
This is a Mac OS feature and is not specific to HyperCard.
To turn this off, select Help ... Hide Balloons.

## Basic Operations

## Stacks

A stack is a collection of cards that all have the same size.
Each stack can use a different card size.

A stack can be thought of as an application that runs in HyperCard
OR as a database table with a user interface for
creating, reading, updating, and deleting records.

Each stack is stored in its own file in the Finder.
All changes made to the cards in a stack are automatically saved.
This includes modifying the content of fields, adding cards, and deleting cards.

Stacks are not meant to be concurrently modified by multiple users.

### Home Stack

HyperCard ships with several stacks including one named "Home".
When HyperCard is launched, it looks for the file `Home`
in the following locations in this order:

1. `System Folder` at the root of the drive containing the HyperCard app
1. directory containing another stack that was
   double-clicked to launch HyperCard
1. directory containing the HyperCard app (default location of `Home` file)
1. `HyperCard Stacks` directory in the director containing the HyperCard app
1. root directory of the drive containing the HyperCard app

If the Home stack is not found in one of these locations,
a dialog will appear asking you to locate it.

The default Home stack contains nine cards.

- "Welcome to HyperCard" contains buttons to open several stacks.

  To jump to this card, select Home ... Home Cards.

  The stacks include:

  - HyperCard Tour
  - HyperCard Help
  - Art Bits: a collection of clip art that can be
    copied and pasted into your stacks
  - Addresses: a sample address book
  - Graph Maker: displays bar, column, line, and pie charts
  - and more

- "Stack Kit" contains buttons to open several stacks including:

  - Readymade Buttons
  - Readymade Fields
  - Stack Templates
  - Power Tools
  - Background Art
  - HyperTalk Reference
  - Color Tools
  - HyperTalk AppleScript Reference
  - Apple Event Primer
  - AppleScript Library

- "Card 3", "Card 4", and "Card 5"

  These are excellent places to add buttons that open:

  - your stacks
  - other applications
  - documents in other applications

- "Search Paths"

  This is a set of three cards that list in order the paths that
  HyperCard will search to find stacks, applications, and documents.
  To jump to the first of these cards, select Home ... Search Paths.

- "Preferences"

  To jump to this card, select Home ... Preference.
  This enables specifying the following preferences:

  - Your name

    The string entered here is saved in the global variable `UserName`.
    It can be used in scripts like the one below that can be added to a button.

    ```text
    on mouseUp
      global UserName
      answer "Hello," && UserName & "!"
    end mouseUp
    ```

  - User level: 1 through 5

  - Blind Typing

    When no field has focus and the message box is open,
    all typed characters are added to the message box
    because it gets focus by default.
    When "blind typing" is enabled, and no field has focus,
    all typed characters are added to the message box
    even when the message box is not open.

  - Power Keys

    This enables issuing commands related to the painting tools
    by pressing the letter, digit, `[`, and `]` keys.

    Examples include:

    - 1, 2, 3, 4, 6, and 8 set the line thickness to that number of pixels
    - `a` selects all the painting on the current card (not buttons or fields)
    - `c` toggles draw centered on and off
    - `d` darkens all pixels in the selected region
    - `e` traces the edges of the current shape; can apply repeatedly
    - `g` toggles snap to grid on and off
    - `h` flips the selection horizontally
    - `i` inverts all the pixels in the selected area
    - `l` lightens all pixels in the selected region
    - `v` flips the selection vertically
    - `[` rotates the selection left 90 degrees
    - `]` rotates the selection right 90 degrees

  - Arrow Keys in Text

    By default, pressing the arrow keys navigates to other cards.
    Checking this checkbox enables using all four arrow keys
    to move the cursor inside focused fields.

### Stack Operations

There are three ways to open an existing stack.

1. Double-click its file in the Finder.
1. In the HyperCard app, select File ... Open Stack...
1. In the HyperCard app, from the Home stack,
   click a button that opens the stack.

Multiple stacks can be open at the same time,
each it its own window.

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

To open an existing stack:

- Select File ... Open Stack...
- In the dialog that appears, navigate to the stack to open.
- Optionally check the "Open stack in new window" checkbox.
- Click the "Open" button.

At least one stack window must be open.
To close another stack that is open in its own window,
click its close box in the upper-left, press cmd-w,
or select File ... Close Stack.

To enable opening your stacks from the Home stack:

- Press cmd-h to open the Home stack.
- Go to Card 3, 4, or 5 that all say
  "You can add your own buttons to this card."
- Select Home ... New Link to Stack...
- In the dialog that appears, select a stack file.
- Click the "Open" button to add a new button.
- Select the Browse tool.
- The new button can be clicked to open the stack in a new window.

To enable opening other applications from the Home Stack:

- Press cmd-h to open the Home stack.
- Go to Card 3, 4, or 5 that all say
  "You can add your own buttons to this card."
- Select Home ... New Link to Application...
- In the dialog that appears, select an application.
- Click the "Open" button to add a new button.
- Select the Browse tool.
- The new button can be clicked to launch the application.

The process is similar to create a button that opens
a given document using a given application.
Select Home ... New Link to Document...
TODO: Why doesn't this work? Maybe it just doesn't work in emulators.

The Home stack initially contains buttons labeled
"Card 3", "Card 4", and "Card 5"
that navigate to a card with the same name.
To rename these buttons and their associated card,
go to one of the cards, select Home ... Rename This Card,
enter the new name in the dialog that appears, and click the "OK" button.
For example, consider renaming "Card 3" to "My Stacks"
if that is where you place links to your stacks.

To get information about the current stack:

- Select Objects ... Stack Info... to open a Stack Info dialog.

  This enables renaming the stack, resizing it, and editing its script.
  It also gives the number of cards in the stack
  and the number of backgrounds used.

To create a copy of the current stack:

- Select File ... Save a Copy...
- In the dialog that appears, select the target directory.
- Enter a stack name under the label "Save a copy of stack as:".
- Optionally change the "File type" to "Application"
  to create an application that can be run without HyperCard.
- Click the "Save" button.

To view and optionally edit a background used by the current card:

- Select Edit ... Background or press cmd-b to toggle background mode.
  This is indicated by a hatched pattern in the menu bar.

To share a stack with others, send them the stack file.
They can open the stack by double-clicking it,
or by launching HyperCard and selecting File ... Open Stack...

### Stack Compacting

When changes are made in a stack, such as entering text in a field,
HyperCard allocates extra space
in anticipation of more space being needed later.
This makes future changes more efficient because
it isn't always necessary to allocate more space.
In large stacks that have undergone many changes,
this can result in quite a bit of allocated space that is unused ...
referred to as "free space".

To see the amount of free space in a stack,
open it and select Objects ... Stack Info... .
In the dialog that appears, the total amount of disk space
used by the stack is listed after "Size of stack" and
the amount of free space is listed after "Free in stack".

The free space can be removed, reducing the size of the stack,
by selecting File ... Compact Stack.
This also verifies the internal structure of the stack
and is able to fix some issues.

It is recommended to periodically compact all stacks,
perhaps daily for frequently modified stacks.

## Cards

A card belongs to a specific stack.
Each card has a foreground and background layer.
Both the foreground and background can contain
graphics, buttons, and fields (for text).
All transparent parts of a card foreground display
content from the background at the same location.
Typically many cards share a background.

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

These seem like large oversights!

HyperCard remembers the last 42 unique cards viewed in the current session.
To view thumbnails of the viewed cards, select Go ... Recent or press cmd-r.
This opens a modal dialog.
The thumbnail for the card currently being viewed has a black border.
To navigate to a different card and dismiss the dialog, click its thumbnail.
To remain on the current card and dismiss the dialog,
click the background of the dialog.

TODO: Discuss "marking" cards and what you can do with marked cards.

## Tools

The tools menu contains a grid of buttons
that can be clicked to select one of the 18 tools.
Drag the Tools menu off the menu bar
to create a floating palette of tool buttons.

<img alt="HyperCard Tools" style="border: 0; width: 15%"
  src="/blog/assets/hypercard-tools.png?v={{pkg.version}}">

The tools include:

- Row #1

  - **Browse**

    This enters Browse mode which enables interacting with cards as a user
    rather than as an author.

    Another way to enter Browse mode is to press cmd-tab.
    This will not work with running HyperCard in an emulator on a modern Mac,
    because cmd-tab there is used to switch applications.

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

    This is used for freehand painting using the selected pattern.
    To change the shape and size of the brush,
    Select Options ... Brush Shape... before painting.

  - **Eraser**

    Drag over painted items to erase them.
    The size of the eraser cannot be changed.
    To erase large areas, use the Rectangle and Lasso selection tools.

  - **Line**

    Click at start of line and drag to end of line.
    To change the line size, Select Options ... Line Size... before drawing.
    If the option key is held down while drawing,
    the line will use the selected pattern.

- Row #4

  - **Spray**

    This sprays paint using the selected pattern.

  - **Rectangle**

    This draws a rectangle, specified by clicking at the
    location of any corner and dragging to the opposite corner.
    To draw a square, hold down the shift key while dragging.

  - **Round Rectangle**

    This draws a rectangle with rounded corners, specified by clicking at the
    location of any corner and dragging to the opposite corner.

- Row #5

  - **Bucket**

    This fills an area with the selected pattern
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
    The text style must be specified before typing the text.
    To open the Text Style dialog, select Edit ... Text Style..., press cmd-t,
    or double-click the Text tool icon ("A") in the Tools palette.
    Then click the Text tool, click on the card or background
    where the text should go, and begin typing.
    To create multiline text, press the return key at the end of a line.

    To apply a pattern to painted text, see the "Patterns" section below.

    For text that may need to be edited later, use a text field instead
    and check the "Lock Text" checkbox in its Field Info dialog
    to make it read-only.

  - **Regular Polygon**

    This draws a convex polygon whose sides all have the same length.
    Select Options ... Polygon Sides...
    to select the number of sides to include.
    The options are 3, 4, 5, 6, 8, and circle.

  - **Polygon**

    This draws an arbitrary polygon.
    Click at each point in the polygon. Double-click to end.
    It does not automatically connect the last point to the first.

When a painting tool is selected,
the Objects, Font, and Style menus disappear,
and the Paint, Options, and Patterns menus appear.
When the browse, button, or field tools are selected,
the reverse menu changes occurs.

Like the Tools menu, it can be dragged off of the menu bar
to create a floating palette.

When a painting tool is selected, pressing the tab key
toggles the display of the Patterns palette,
even if it was not previously dragged off of the menu bar.

If Options ... Draw Filled is selected, the following tools will
fill their shape with the selected pattern:
Rectangle, Rounded Rectangle, Oval, Regular Polygon, and Polygon.

If Options ... Draw Filled is selected, the following tools will
draw their lines with the selected pattern:
Line, Rectangle, Rounded Rectangle, Oval, Regular Polygon, and Polygon.

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

<img alt="HyperCard Patterns" style="border: 0; width: 15%"
  src="/blog/assets/hypercard-patterns.png?v={{pkg.version}}">

To fill an existing, closed shape with the selected pattern,
select the Bucket tool and click the
dripping tip of the bucket icon (lower-right) inside the shape.

To cause drawn shapes to be filled with the selected pattern,
select Options ... Draw Filled before drawing the shape.

To apply a pattern to the lines in characters drawn with the Text tool,
select a pattern, select the Bucket tool, and click inside each character.
To include a black outline around each character,
select all the text, select Paint ... Trace Edges, and use
the Bucket tool to fill the resulting character outlines with a pattern.

<img alt="HyperCard Text Patterns" style="border: 0; width: 30%"
  src="/blog/assets/hypercard-text-patterns.png?v={{pkg.version}}">

The Trace Edges menu item can be applied multiple times.
The screenshot below shows the effect when it is applied to text four times.

<img alt="HyperCard Trace Edges multiple times" style="border: 0; width: 25%"
  src="/blog/assets/hypercard-trace-edges-multiple.png?v={{pkg.version}}">

Then click or drag across pixels in the dialog
to toggle them between black and white.
I couldn't find a way to reset a pattern to its default state,
so be careful with these changes!

## Buttons

There are four basic kinds of buttons:

1. Buttons that perform an action when clicked.
   The style for these can be "Transparent", "Opaque", "Rectangle",
   "Rounded Rect", "Shadow", "Oval", "Standard", or "Default".
   Any of these button styles can contain text, an icon, both, or neither.
1. "Check Box" for specifying a Boolean value.
1. "Radio Button" for selecting from a small set of options.
1. "Popup" that displays a dropdown menu of options when clicked
   for selecting from a possibly large set options.

Each of the supported button styles are shown below.

<img alt="HyperCard button types" style="border: 0; width: 50%"
  src="/blog/assets/hypercard-button-types.png?v={{pkg.version}}">

Standard and Default buttons conform to
the Macintosh interface guidelines for buttons.
There should be at most one Default button per card
and a card script must be added to
trigger the button when the return key is pressed.
For example:

```text
on returnKey
  send "mouseUp" to card button "My Default Button"
end returnKey
```

Oval buttons are transparent and do not display their border.
They only differ from Transparent buttons in
the shape of their target (clickable) area.

To find all the buttons on the current card, press and hold cmd-option.
This adds a dotted outline around all the buttons
that disappears when the keys are released.

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

  - Transparent: no border and can see through;
    default when created with cmd-drag
  - Opaque: no border and cannot see through
  - Rectangle: opaque with rectangle border
  - Round Rect: opaque with rectangle border that has rounded corners;
    default when created with "New Button" menu item
  - Shadow: like Rectangle, but adds shadows on the right and bottom sides
  - Check Box: for a Boolean selection; click the checkbox or the name
    to toggle the value of its `hilite` property
  - Radio Button: for a set of mutually exclusive choices
  - Standard: conforms to Macintosh interface guidelines for non-default buttons
  - Default: conforms to Macintosh interface guidelines for default buttons
  - Oval: like Transparent, but the target area is an oval instead of a rectangle
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
- The ability to snap-to-grid is missing and would be useful for better layout.

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

To change the text on a button in a handler:

```text
set name of {button-reference} to "new-text"
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

### Popup

A button with the style "Popup" displays a dropdown list of options.
To specify the options, click the "Contents..." button
and enter each option on its own line.

The name assigned to the button is used for
a label that appears to the left of the dropdown.
By default, the label is given a width of zero and doesn't appear.
To make it appear, enter a value for "Title Width" in the Button Info dialog.
Alternatively, drag the left edge of the button to the right
to reveal the title and set the "Title Width".

To get the selected text or line number of a Popup,
use the following expressions:

```text
the selectedText of button "My Popup Name"
the selectedLine of button "My Popup Name"
```

## Fields

A text field is referred to as simply a "field".
These hold text that can differ on each card within a stack.
For example, the background of the cards in the provided "Addresses" stack
contains "Name" and "Telephone" fields.
Different values can be entered in these fields for each card.

Fields can hold a single line of text or multiple lines.
The choice is specified in the "Field Info" dialog for each field.

To add a field to the current card:

- Select Objects ... New Field.
  Alternatively, select the Field tool and
  cmd-drag to indicate the location and size of a new field.
  This adds a new field to the center of the current card.
- With the Field tool selected:

  - Enter a Field name.
  - Select a Style from the following options:

    - Transparent: can see through to objects behind it
    - Opaque: cannot see through to objects behind it
    - Rectangle: adds a rectangular border
    - Shadow: same as Rectangle, but adds shadows
      on the right and bottom sides for a 3D effect
    - Scrolling: adds a rectangular border and
      a vertical scrollbar (even when not needed)

  - Select the desired checkbox options which include:

    - Lock Text
    - Don't Wrap
    - Auto Select
    - Multiple Lines
    - Wide Margins
    - Fixed Line Height
    - Show Lines: displays dotted lines it indicate where text can be entered
    - Auto Tab
    - Don't Search

    TODO: Describe the options above.

- Click the Browse tool (hand with pointing finger).
- Click the field.
- Enter text.

- Double-click a Text object.
- Enter

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
- The ability to snap-to-grid is missing and would be useful for better layout.

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

If the field is on the card (foreground) instead of the background,
add `card` or `cd` before `button`.
If the button is on the background instead of the card (foreground),
optionally add `background`, `bkgnd`, or `bg` before `button`.
If none of these keywords are added,
HyperTalk will assume the field is on the background.
This differs from the default for buttons.

If the field is not on the current card, add `of card {card-reference}`.
If the field is not on a card in the current stack,
add `of card {card-reference} of stack "{stack-name"}`.

To get the text from a field, use a field reference.
For example, `card field "{field-name}"`.
This explains why setting a variable to a card reference
only gets its text, not an object reference.

To execute a HyperTalk command that was entered in a field,
run the command `do card field "{field-name}"`.

To move the focus to the next field, press the tab key.

## Sounds

To play a predefined sound, use the `beep` or `play` commands.
The `beep` command takes an optional argument
that specifies the number of times to play, defaulting to one.
The `play` command takes the following arguments:

- name of a sound, either `boing` or `harpsichord`
- optional keyword `tempo` followed by a number (seems to default to 120)
- optional set of notes to play in double quotes

For example, `play harpsichord tempo 120 "c4 e g4 e c5 q"`

To play a sound file, use the `play file` command.
TODO: What sound formats are supported?

## Backgrounds

Every stack has at least one background, but it can be empty.

To switch to background mode,
which temporarily hides the foreground of the current card
and only displays its background,
select Edit ... Background or press cmd-b.
Repeat to return to foreground mode which
displays the foreground and background of the current card.

To add a new background to the current stack,
select Objects ... New Background.
This creates a new card that uses the new background,
but it will not be in background mode.
Press cmd-b to enter background mode and add content.
Then press cmd-b again to enter foreground mode and add content.
Cards created after this one will use the new background by default.

To copy a background and create a new background
that starts out with the same configuration,
TODO: ???

To assign a name to a background, select Options ... Bkgnd Info...
and enter/change the "Background Name".

In a stack that uses multiple backgrounds,
button and field names can be duplicated across the backgrounds.
This enables background scripts to use the same code
for referring to those containers.

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

## Scroll Window

The scroll window enables scrolling in a stack window.
It is useful when the card size of a stack is larger than the monitor.
This is not typically the case, so this tool is rarely used.

To toggle display of the scroll window, select Go ... Scroll or press cmd-e.

The black rectangle in the scroll window represents
the visible size of the stack with focus.

There are two ways to change the viewable size of the stack window.

1. Drag any edge or corner of the rectangle in the scroll window.
1. Press cmd-shift-e and drag the grow box that appears
   in the lower-right corner of the stack window.

To change the portion of the stack window that is visible,
drag the rectangle in scroll window.

To toggle the viewable size between full size and the last reduced size,
double-click the rectangle in the scroll window or
click the zoom box in the upper-right corner of the stack window.
The rectangle cannot be dragged if its size is the same as the scroll window.

The screenshot below shows the scroll Window
after its rectangle has been resized and dragged.
It also shows the portion of the Home stack that is visible
based on the state of the scroll window.

<img alt="HyperCard Scroll Window" style="border: 0; width: 70%"
  src="/blog/assets/hypercard-scroll-window.png?v={{pkg.version}}">

## Keyboard Shortcuts

| Shortcut    | Action                                                                         |
| ----------- | ------------------------------------------------------------------------------ |
| cmd-h       | Home                                                                           |
| cmd-~       | go to Back to the previously browsed card                                      |
| cmd-1       | go to First card in stack                                                      |
| cmd-2       | go to Prev card in stack                                                       |
| cmd-3       | go to Next card in stack                                                       |
| cmd-4       | go to Last card in stack                                                       |
| left arrow  | go to Prev card in stack                                                       |
| right arrow | go to Next card in stack                                                       |
| up arrow    | go to First card in current stack or first card in Home stack if already there |
| down arrow  | go to previously visited card in any stack?                                    |
| cmd-b       | toggle Background mode                                                         |
| cmd-c       | copy                                                                           |
| cmd-e       | open Scroll window                                                             |
| cmd-f       | Find within current stack                                                      |
| cmd-i       | open Icon editor                                                               |
| cmd-m       | open Message box (where commands can be entered)                               |
| cmd-o       | open a stack                                                                   |
| cmd-l       | go to the next open HyperCard window                                           |
| cmd-n       | New card                                                                       |
| cmd-r       | open window of recently visited cards (click one to open it)                   |
| cmd-v       | past                                                                           |
| cmd-x       | cut                                                                            |
| cmd-z       | undo                                                                           |

The "Open Stack" dialog contains:

- "Show Preview" checkbox that causes a thumbnail of the first card
  of the selected stack to be displayed inside the dialog.
- "Open stack in new window" which does what it says when a stack is opened.

The "Home" stack is inside the "HyperCard 2.4" directory
which I placed in the Applications directory.

## HyperTalk

HyperTalk ...

- is the scripting language used by HyperCard
- has an English-like syntax
- is case-insensitive except when comparing strings
- is interpreted at run-time, but cached for executing again later
- supports message passing (events) and message handlers
- implements a simple database

The commands (a.k.a. statements) supported by HyperTalk
are documented at {% aTargetBlank
"https://www.hypercard.center/HyperTalkReference#commands",
"HyperTalk Reference" %}.

### Message Box

One way to execute HyperTalk commands is to enter them in the message box.
This is a single, small window that allows entry of a single HyperTalk command.
If the text entered extends past the right edge, that portion
will not be visible and the text cannot be scrolled horizontally.
The message box can be moved, but it cannot be resized.

To toggle display of the message box, select Go ... Message or press cmd-m.
Then enter commands separated by semicolons and press return to execute them.

Typing while focus is not in the message box or in a card field
replaces the text in the message box.
See "Blind Typing" in the "Home Stack ... Preferences" section for more detail.

To change the font family and size used in the message box,
click in it to give it focus and make selections in the Font and Style menus.

If a field reference is entered in the message box,
it is replaced by the contents of the field.
For example, `card field 2`, `card field id 7`, or `card field "first name"`.

To write a value to the message box, use the command
`put {expression} into [the] message [box]`.
The put command writes to the message box by default,
so the previous command can shorted to `put {expression}`.

The following are examples of HyperTalk commands
that can be entered in the message box:

- navigation

  - `go to Art Bits` - opens the stack named "Art Bits"
  - `go art bits` - same but without optional keyword `to`
    and without capitalizing

- scripts

  - `edit script of button "My Button"`

    This opens a script editor for a given script.
    Unless a card name is specified, the script must be in the current card.

  - `searchScript "some text", "stack name"`

    This opens the script editor for the first script
    found in the stack "stack name" that contains the text "some text".
    Closing the script editor opens another for the next match found.
    After the last match is displayed,
    a dialog containing "Search script done!" is opened.

    `searchScript` is a handler defined in the Home stack.
    To see it, open the Home stack, select Objects ... Stack Info...,
    and click the "Script..." button.
    Alternatively, enter `edit script of Home` in the message box.
    Press cmd-f to open a Find dialog and enter "searchScript".
    Repeatedly press cmd-g to find the next match until the line
    `on searchScript pattern,stackName` is found.

- mathematical expressions

  The entered expression is replaced by its result.

  - `2 * 3` - gives `6`
  - `4 * (5 - 2)` - uses parentheses to control order of evaluation and gives `12`
  - `4^3` - uses exponentiation and gives `64`
  - `2 * pi * 3^2` - gives `56.548668`
  - `sin(45)` - evaluates a trigonometry function and gives `0.850904`

  The built-in mathematical functions include
  `abs`, `annuity`, `atan`, `average`, `cos`, `ln`, `ln1`, `log2`,
  `exp`, `exp1`, `exp2`, `ln`, `ln1`, `log2`, `max`, `min`, `random`,
  `round`, `sin`, `sqrt`, `sum`, `tan`, and `trunc`

- multiple commands

  While only a single command can be entered, multiple commands
  can be described in a string with a return character between each command.
  The string can be passed to the `do` command to execute each of the commands.
  For example:

  ```text
  do "beep" & return & "wait for 1 second" & return & "play boing"
  ```

### Comments

Single-line comments begin with `--` and extend to the end of the line.
Multi-line comments are not supported.

To comment a set of lines, select them and
press cmd-dash or select Script ... Comment.

To uncomment a set of lines, select them and
press cmd-equal or select Script ... Uncomment.

### Data Types

HyperTalk supports the following data types:

- booleans with the literal values `true` (or `1`) and `false` (or `0`)
- numbers with literal values that are either integers or floating point
- string with literal values delimited by double quotes
- string lists that are a single string with commas delimiting the items
- containers which are variables, buttons, and fields

To concatenate strings, or values that can be converted to strings,
use the binary operators `&` and `&&`.
The double ampersand adds a space between the values.

To extract a substring from a string list, use the keyword `of`.
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
This uses two-space indentation,
but any indentation (including none) will work.

### Message Handlers

A single script can define any number of message handlers
that each begin with the keyword `on`.
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

TODO: Add more to this list and describe each one.

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

Constants are variables whose values cannot be changed.
HyperTalk defines the following predefined constants:

- Boolean values `true` and `false`
- special characters `quote`, `return`, `space`, and `tab`
- `pi`: approximately 3.14159265359
- `empty`: empty string
- directions `up`, `down`, `left`, and `right`
- horizontal positions `left`, `center`, and `right`
- vertical positions `top`, `middle`, and `bottom`
- `the screenRect`: size of screen in a string
  with the content `left,top,right,bottom`
  (It seems that left and top are always zero.)

### Special Variables

The following variables are set by HyperCard and cannot be modified:

- `it`: stores the result of some commands such as
  `ask`, `get`, and `read from file`.
- modifier keys state `commandKey`, `optionKey`, and `shiftKey`
- `me`: refers to the current object
- `the clickLoc`: location of the last mouse click
- `the [short|long] date`: the current date in two formats (default is short)
- `the mouseLoc`: current location of the mouse pointer
- mouse positions `mouseH` and `mouseV`
- `the params`: inside a message handler this is set to a string
  that is a comma-separated list of all the parameter values

- `the result`: stores the result of some operations such as the following:

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

- `the target`: in a handler this refers to the object
  that initially received the message
- `ticks`: number of ticks (1/60th of a second) since system startup

The following variables are set by HyperCard and CAN be modified:

- `the cursor`: current cursor shape
- `the lockScreen`: a Boolean that determines whether
  screen updates will appear during script execution.
  Setting this to false can improve performance.
- `the userLevel`: current user level from 1 to 5

### Variables

Variables exist in two scopes, local to a specific handler
and global across all handlers in all stacks.

Local variables spring into existence when a value is assigned to them
and are not declared.

Global variables must be declared everywhere they are used
with the keyword `global`.
For example, `global favoriteColor, maximumTemperature, taxRate`
The values of global variables are not saved across HyperCard sessions.

To assign a value to a variable, use the `put` command.
For example, `put 3.14159265 into pie` (`pi` is a predefined constant)

To assign a value to a property of an object
(such as a stack, card, button, or field), use the `set` command.
For example,`set the visible of button "save" to false`.

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

If the keyword `card` is omitted, it will only look for a background field.

### Math

HyperCard supports the arithmetic infix binary operators
`+`, `-`, `*`, `\`, and `mod`.
Normal precedence rules apply.
Parentheses can be used to change the evaluation order.

A "container" is a variable or field.

To add a number to a container that already contains a number,
use the `add` command. For example, `add 3 to total`.

To subtract a number from a container that already contains a number,
use the `subtract` command. For example, `subtract 3 from total`.

To multiply a number in a container that already contains a number,
use the `multiply` command. For example, `multiply total by 3`.

To divide a number in a container that already contains a number,
use the `divide` command. For example, `divide total by 3`.

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

In this form, `end if` is not allowed.

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

In this form, `end if` is required.

Conditions can use the following "operators":

- `is` or `=`
- `is not` or `<>`
- `<`
- `>`
- `<=`
- `>=`

For example, `is empty` checks for an empty string.

Conditions can be combined with `and` and `or`.

Conditions can be negated with `not`.

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

The `return` command is used to return a value from a function.

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

## Speaking

To speak the words in a string, use the `speak` command.
For example, `speak "Happy Birthday!`.

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
- Go the last card by pressing cmd-4 or
  by clicking the left-pointing triangle in the lower-left.
- Select a level by clicking it or
  by dragging the right-pointing triangle up and down.

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

### Card Numbers

To display a card number on each card in a stack:

- Add the following field to each background:

  <img alt="HyperCard card number field" style="border: 0; width: 60%"
  src="/blog/assets/hypercard-card-number-field.png?v={{pkg.version}}">

- Add the following script to each background:

  ```text
  on openCard
    put the number of this card into field "cardNumber"
  end openCard
  ```

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

### Counter

Let's create a read-only field that displays an integer
and buttons that increment and decrement the value.
Start by creating the following objects:

- "+" button to increment the value
- field to display the counter value
- "-" button to decrement the counter

<img alt="HyperCard Counter" style="border: 0; width: 20%"
  src="/blog/assets/hypercard-counter.png?v={{pkg.version}}">

The following screenshot shows the field configuration:

<img alt="HyperCard Counter field" style="border: 0; width: 60%"
  src="/blog/assets/hypercard-counter-field.png?v={{pkg.version}}">

Add the following script to the card
to reset the state every time to card is opened:

```text
on openCard
  put 0 into cd fld counter
  set enabled of cd button "-" to false
end openCard
```

Add the following script to the "+" button:

```text
on mouseUp
  put cd fld "counter" + 1 into cd fld "counter"
  set enabled of cd button "-" to true
end mouseUp
```

Add the following script to the "-" button:

```text
on mouseUp
  put cd fld "counter" - 1 into counter
  put counter into cd fld "counter"
  if counter is 0 then
    set enabled of cd button "-" to false
  end if
end mouseUp
```

### Dice Button

Let's create a button whose icon is a dice face.
Each time the button is clicked, its icon changes to the next dice value.

<img alt="HyperCard Dice Button" style="border: 0; width: 6%"
  src="/blog/assets/hypercard-dice-button.png?v={{pkg.version}}">

Create the following button with its icon set to any of the dice icons:

<img alt="HyperCard Dice Button Info" style="border: 0; width: 55%"
  src="/blog/assets/hypercard-dice-button-info.png?v={{pkg.version}}">

Add the following script to the button:

```text
on mouseUp
  -- The dice icon IDs are 2101 to 2106.
  put the icon of me - 2101 into index
  put (index + 1) mod 6 into index
  set the icon of me to 2101 + index
end mouseUp
```

### Previous and Next Buttons

It is common for the background of all cards in a stack to contain
buttons that can be clicked to go the next and previous cards.
There are several provided icons that are typically used for this purpose.
The screenshot below shows a common choices
that often appear at the bottom of each card.

<img alt="HyperCard Previous and Next Buttons" style="width: 70%"
  src="/blog/assets/hypercard-prev-next-buttons.png?v={{pkg.version}}">

To implement buttons like these:

- Press cmd-b to enter background mode.
- Add the following buttons in the lower-left and lower-right corners:

  <img alt="HyperCard Previous Button" style="width: 49%"
    src="/blog/assets/hypercard-prev-button.png?v={{pkg.version}}">
  <img alt="HyperCard Next Button" style="width: 49%"
    src="/blog/assets/hypercard-next-button.png?v={{pkg.version}}">

- Create the following field between the buttons:

  <img alt="HyperCard Card Number Field" style="width: 49%"
    src="/blog/assets/hypercard-card-number-field.png?v={{pkg.version}}">

- Select Objects .. Bkgnd Info...
- Click the Script button.
- Add the following script:

  ```text
  on openCard
    put the number of this card into cardNum
    put the number of cards into cardCount
    put cardNum && "of" && cardCount into field "cardNumber"

    put cardNum &lt;&gt; 1 into showPrev
    set the visible of bg button "prevButton" to showPrev

    put cardNum &lt;&gt; cardCount into showNext
    set the visible of bg button "nextButton" to showNext
  end openCard
  ```

- Press cmd-b to exit background mode.
- Test the new buttons.
  These should appear on every card, with the exceptions that:

  - The previous button will not appear on the first card.
  - The next button will not appear on the last card.

### Adding Numbers

Let's create a field where the user can type any number of lines.
When the "Add Number" button is clicked, a dialog is displayed
which prompts the user to enter a number.
Whatever they enter, number or not, is added as a new line in the field.
The field can also be directly edited.

When the "Total" button is clicked, the total of all the numbers entered on
separate lines in the field is computed and displayed inside the field.
When the "Clear" button is clicked, the contents of the field is cleared.

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

## Books

The book "The Complete HyperCard 2.2 Handbook" volumes 1 and 2
are widely seen as the best documentation on HyperCard.
But these books to not cover improvements made in HyperCard 2.3 and 2.4.
The following subsections describe those improvements.

### HyperCard 2.3

This version of HyperCard ...

- added limited color capabilities through XCMD extensions
  and better integration with ColorTools
- enhanced support for QuickTime movies and sound resources
- improved AppleScript support, allowing HyperCard
  to be controlled by other applications
- added optimizations for faster script execution and card rendering
- added support for running on the PowerPC Macintosh models,
  though not as a native PowerPC application
- enhanced printing capabilities
- improved the script editor
- improved debugging tools
- added external commands (XCMDs) and functions (XFCNs).
- included bug fixes

### HyperCard 2.4

This version of HyperCard ...

- improved support for styled text
- improved the painting tools
- added ability to use a palette of 256 colors
- added improvements to the HyperTalk language
- added support for over 30 standard media file types, including PICT files
- added support stacks that span multiple monitors
- improved multimedia controls, making it possible to
  resize, flip, mirror, rotate, or skew QuickTime movies in real-time,
  as well as loop movies for continuous play
- added QuickTime integration which enables linking HyperCard stacks to
  QuickTime movies, QuickTime VR scenes, and the World Wide Web
- included bug fixes

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
- {% aTargetBlank "https://archive.org/details/hypercardstacks",
  "HyperCard Stacks" %}
- {% aTargetBlank "https://livecode.com", "LiveCode" %} active,
  commercial alternative to HyperCard
- {% aTargetBlank "https://www.vipercard.net", "ViperCard" %} -
  an open source recreation and re-imagination of HyperCard
