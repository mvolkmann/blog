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
HyperCard stacks (a.k.a HyperCard applications) which are collections of cards.

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

## Help

For popup help on a particular menu item or button,
select Help ... Show Balloons and hover over an item.

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

To add a card to the current stack:

- Select Edit ... New Card or press cmd-n.

To delete the current card:

- Select Edit ... Delete Card.

### Buttons

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

### Text Fields

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

To draw a shape, select one of the following tools:

- Pencil
- Brush
- Eraser
- Line
- Spray
- Rectangle
- Round Rectangle
- Bucket - This fills a clicked area with the currently selected pattern
  from the Patterns menu.
- Oval - To draw a circle, hold down the shift key while dragging.
- Curve
- Text
- Regular Polygon - This draws a convex polygon
  whose sides have the same length.
  Select Options ... Polygon Sides...
  to select the number of sides it should have.
  The options are 3, 4, 5, 6, 8, and circle.
- Polygon - Click at each point in the polygon. Double-click to end.
  It does not automatically connect the last point to the first.

To select all or part of a drawing,
use the Rectangle or Lasso Selection Tools.
Shapes cannot be selected by clicking them.

To cause a drawn shape to be centered at the location of the initial click,
select Options ... Draw Centered.
This stays in effect until it is toggled off.

To cause drawn shapes to be filled with the currently selected pattern,
select Options ... Draw Filled.

To rotate the selected portion of a drawing,
select Options ... Rotate and drag any of the selection handles.
Other options include Slant, Distort, and Perspective.

To undo the last drawing action, press the esc key or cmd-z.

To erase part of a drawing, do one of the following:

- Select the Erase tool and drag over the drawing.
- Select an area with the Rectangle or Lasso Selection Tools
  and press the delete key, or select Edit ... Cut Picture, or press cmd-x.

## Sounds

To play a sound, use the `play` command
with an argument that is the name of a sound.
The builtin sounds are Boing, Chime, Click,
Clunk, Glass, Honk, Pop, and Thud.

## Keyboard Shortcuts

| Shortcut | Action                                           |
| -------- | ------------------------------------------------ |
| cmd-h    | Home                                             |
| cmd-~    | go to Back to previous card                      |
| cmd-1    | go to First card in stack                        |
| cmd-2    | go to Prev card in stack                         |
| cmd-3    | go to Next card in stack                         |
| cmd-4    | go to Last card in stack                         |
| cmd-b    | toggle Background mode                           |
| cmd-c    | copy                                             |
| cmd-e    | open Scroll window                               |
| cmd-f    | Find within current stack                        |
| cmd-i    | open Icon editor                                 |
| cmd-m    | open Message box (where commands can be entered) |
| cmd-n    | New card                                         |
| cmd-r    | Recent cards                                     |
| cmd-v    | past                                             |
| cmd-x    | cut                                              |
| cmd-z    | undo                                             |

## Tools

The Tools menu contains the following buttons that select a tool:

- Browse
- Button
- Field
- Rectangle selection
- Lasso selection
- Pencil
- Brush
- Eraser
- Line
- Spray
- Rectangle
- Rounded Rectangle
- Bucket
- Oval
- Curve
- Text
- Regular Polygon
- Polygon

To tear off the Tools menu into a floating window for easier access ...

## HyperTalk Commands

- go first|next|prev|last
- go home
- go stack "stack-name"

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
