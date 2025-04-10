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

{% aTargetBlank "https://hyperscript.org", "_hyperscript" %} is
programming language that can be used in HTML files
to implement interactive features such as event handling.
It also supports asynchronous operations such as
fetching data from a server by sending an HTTP request.

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

To get information about the current stack:

- Select Objects ... Stack Info... to open a Stack Info dialog.

  This enables renaming the stack, resizing it, and editing its script.
  It also gives the number of cards in the stack
  and the number of backgrounds used.

To edit a background used by the current stack:

- Select Edit ... Background or press cmd-b to toggle background mode.
  This is indicated by a hatched pattern in the menu bar.

## Cards

To add a card to the current stack, select Edit ... New Card or press cmd-n.

To configure the current card, select Objects ... Card Info...
This enables editing the card name, its script, and checking the checkboxes
for "Card Marked", "Don't Search Card", and "Can't Delete Card".

To delete the current card:

- Select Edit ... Delete Card.

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

  - **Field**

    This enters Field mode which enables editing existing text fields.

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

To add a button to the current card.

- Select Objects ... New Button.
  This adds a new button to the center of the current card.
- Double-click the button to open its "Button Info" dialog.
- Change "Button Name".
- Click the "Script..." button.
- Enter the command(s) to execute when the button is clicked.

To add an icon to a button:

- Click the Button tool.
- Double-click a button to open its "Button Info" dialog.
- Click the "Icon..." button.
- Click the OK button.
- Optionally remove the text name on the button by unchecking "Show Name".
- Optionally cause the button to highlight when it is clicked
  by checking the "Auto Hilite" checkbox.
- Icon sizes cannot be modified.

To move a button:

- Select the Button tool.
- Drag the inside or edge of any button.

To resize a button:

- Select the Button tool.
- Drag any corner of a button.

To edit the script for a button:

- Select the Button tool.
- Double-click a button to open its "Button Info" dialog.
- Click the "Script..." button.
- Enter commands.

To delete a button:

- Click the Button tool.
- Click a button to select it.
- Press the delete key, or select Edit ... Cut Button, or press cmd-x.

## Text Fields

To add a text field to the current card:

- Select Objects ... New Field.
  This adds a new text field to the center of the current card.
- Click the browse tool (hand with pointing finger).
- Click the text field.
- Enter text.

To change the font or styling of a text field:

- Select the Field tool.
- Double-click a text field to open its "Field Info" dialog.
  Alternatively, click a text field to select it
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

To move a text field:

- Select the Field tool.
- Drag the inside or edge of any text field.

To resize a text field:

- Select the Field tool.
- Drag any corner of a text field.

To delete a text field:

- Click the Field tool.
- Click an object to select it.
- Press the delete key, or select Edit ... Cut Field, or press cmd-x.

## Drawing

To draw a shape, select one of the tools from the Tools menu.

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

TODO: How can a stack use more than one background?
TODO: How can a stack use more than one background?
TODO: How do you select the background that should be used by a particular card?

## Card Transitions

TODO: Add detail on these.

## Images

One source of images is other stacks.

For example, the "Art Bits" stack is linked from
the first card of the Home stack.
It contains links to the cards with images in the following categories:
Beasts, Buildings, Communication and media, Hypercard miscellany,
Icon ideas, Macintosh miscellany, Nature and science, Odds and ends,
People, Small treasures, and Transportation.

Another source is the stacks linked from the "Stack Kit" card of the Home stack.
See "Readymade Buttons", "Readymade Fields", "Stack Templates",
and "Background Art".

To get an image, open a stack, find a desired image on one of its cards,
use the selection tools to select it, copy it, and
paste it onto a card in your stack.

## Scripts

Scripts handle messages that are triggered by many actions.
The messages travel through the object hierarchy,
searching for an object that handles them.
The levels of the object hierarchy, from bottom to top are:

- buttons and text fields
- cards
- backgrounds
- stacks
- Home stack
- HyperCard app

This is also the order in which scripts are most commonly defined,
with button scripts being the most common
and HyperCard app scripts being the least common.

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

- Select Object ... Card Info...
- Click the "Script" button.
- Enter the following:

  ```text
  on mouseUp
    beep
  end mouseUp
  ```

- Select Object ... Stack Info...
- Click the "Script" button.
- Enter the following:

  ```text
  on mouseUp
    flash
  end mouseUp
  ```

- Click the Browse tool.
- Click outside the button.
- Notice that the `beep` command runs because the card script is run.
- Click the button.
- Notice that it navigates to the next card because the button script is run.
- While on the next card, click anywhere inside it
  except on a button or text field.
- Notice that the window flashes because the stack script is executed.

When editing a script, press the tab key to format it
which indents the lines properly.

When changes to a script are saved,
the script is not checked for possible errors.
Any errors are only found when the script is run.

## Color

To use colors in HyperCard, install the color tools.
This requires more than the default amount of memory.
To add more:

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

## HyperTalk Commands

- go first|next|prev|last
- go home
- go stack "stack-name"

## User Levels

Each stack can specify one of the following user levels:

- Browsing
- Typing
- Painting
- Authoring
- Scripting

TODO: Document these.

## Popular Stacks

- {% aTargetBlank "https://macintoshgarden.org/games/the-haunted-house",
  "The Haunted House" %} by Mark Klink

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
