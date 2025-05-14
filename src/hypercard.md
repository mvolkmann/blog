---
eleventyNavigation:
  key: HyperCard
layout: topic-layout.njk
---

<img alt="HyperCard logo" style="width: 10%"
  src="/blog/assets/hypercard-logo.jpg?v={{pkg.version}}">
<br />
<img alt="HyperCard Welcome card" style="width: 80%"
  src="/blog/assets/hypercard-home-stack.jpg?v={{pkg.version}}">

## Overview

HyperCard is a Macintosh application for viewing and creating
multimedia applications built with HyperCard stacks
which are collections of cards.
Actions can be scripted using the built-in HyperTalk language or AppleScript.
This includes interacting with other applications
and exchanging data with documents from other applications.

The original application name was WildCard. It was changed
to HyperCard shortly before the initial release
because the name WildCard was already trademarked.

HyperCard initially released in August, 1987.
The last version of HyperCard, released in 1988, was 2.4.1.
The oldest version of Mac OS that can run some version of HyperCard
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
Click the "HyperCard Tour" stack for a good overview of the application.
At any point, press cmd-h to return to the Home stack.

Changes to stacks are saved automatically.
There is no save button or menu item.

## Launching HyperCard

To launch HyperCard, double click on the app icon
or on the icon of a HyperCard stack.
If the app icon was double clicked, the Home stack will be opened.
The first card in the opened stack will be displayed.

## Help

For popup help on a particular menu item or button,
select Help ... Show Balloons and hover over an item.
This is a Mac OS feature and is not specific to HyperCard.
To turn this off, select Help ... Hide Balloons.

## Domains

Cards have two domains, a background and a foreground (referred to as "card").
Each domain can contain painted objects (e.g. a filled rectangle),
buttons, and fields.
Fields display text and can allow users to enter text.

Buttons and fields are referred to collectively as objects.
When an object is created, it is
automatically assigned a part number and an ID.
Both are unique within their domain which is a specific background or card.

The ID values never change and are never reused, even if an object is deleted.

The part numbers specify stacking order where
objects with higher part numbers are drawn on top of
objects with lower part numbers.
All objects in the card domain are drawn on top of
all objects in the background domain.
For more detail, see the "Layers" section below.

A background can be shared by any number of cards in its stack.
Objects in the background of the current card that are
not obscured by objects in card are visible.

## Stacks

A stack is a collection of cards that all have the same size.
Each stack can use a different card size.

A stack can be thought of as an application that runs in HyperCard
OR as a database table with a user interface for
creating, reading, updating, and deleting records.

Each stack is stored in its own file in the Finder.
All changes made to the cards in a stack are automatically saved.
This includes modifying the content of fields, adding cards, and deleting cards.

Many HyperCard windows can be open simultaneously.
These include windows for stacks, scripts, and palettes.
If multiple windows are open, select Go ... Next Window
or press cmd-l to move focus to the next one.

A stack can be homogeneous or heterogeneous.
In a homogeneous stack, all the cards have the same background.
In a heterogeneous stack, the cards use more than one background.
An example is a stack that begins with a table of contents card
that contains buttons that link to all other cards
which share a different background.
TODO: Learn how to implement this.

A table of contents card can have multiple levels.
For example, it can have a list of categories on the left
and a list of topics on the right.
Clicking a category on the left can
change the list of topics displayed on the right.
Clicking a topic can navigate to a card that provides details on the topic.
TODO: See page 74 in HyperCard Handbook. Learn how to implement this.

A stack can contain links to cards in other stacks.

Stacks are not meant to be concurrently modified by multiple users.

### Home Stack

HyperCard ships with several stacks including one named "Home".
If the Home stack is not currently open,
it can be opened by selecting Go ... Home or pressing cmd-h.

<img alt="HyperCard Home stack" style="width: 60%"
  src="/blog/assets/hypercard-home-stack.jpg?v={{pkg.version}}">

When HyperCard is launched, it looks for the file `Home`
in the following locations in this order:

1. `System Folder` at the root of the drive containing the HyperCard app
1. directory containing another stack that was
   double clicked to launch HyperCard
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

### Stack Creation

There are three ways to open an existing stack.

1. Double-click its file in the Finder.
1. In the HyperCard app, select File ... Open Stack...
1. In the HyperCard app, from the Home stack,
   click a button that opens the stack.

Multiple stacks can be open at the same time,
each it its own window.

To create a new stack from stack template:

- Open the Home stack.
- Click the "Stack Kit" button on the bottom row.
- Click the "Stack Templates" button to display a list of templates.

  <img alt="HyperCard Stack Kit card" style="width: 60%"
    src="/blog/assets/hypercard-stack-kit-card.png?v={{pkg.version}}">

- Click the name of one of the 15 stack templates.

  <img alt="HyperCard Stack Templates" style="width: 60%"
    src="/blog/assets/hypercard-stack-templates.png?v={{pkg.version}}">

- In the dialog that appears on top of the template:

  <img alt="HyperCard Todo List template" style="width: 60%"
    src="/blog/assets/hypercard-todo-list-template.png?v={{pkg.version}}">

  - Click the lightbulb button to display information about the template.
  - Click the "Create Stack" button to
    create a new stack using the selected template.
  - Click the house icon to return the Home stack.
  - Click the "Templates" button to return to the list of templates.
  - Click the left arrow to go to the previous template.
  - Click the right arrow to go to the next template.

To create a new stack from scratch:

- Select File ... New Stack...
- Enter a stack name.
- Select an option from the "Card size" dropdown with the following options
  where the sizes are in pixels:

  - Small: 416 x 240
  - Classic: 512 x 342
  - PowerBook: 640 x 400
  - Large: 640 x 480
  - MacPaint: 576 x 720
  - Window: 640 x 480 (same as Large)
  - Screen: This fills the current screen.
  - Custom: Specify a size between 64x64 and 1280x1280 in 32-pixel increments
    by dragging the lower-right corner of
    the rectangle under the "Card size" label.

  This will default to the size of the stack currently being viewed.

  To later change the card size used by a stack,
  open its Stack Info dialog and click the "Resize..." button.
  However, this can require rearranging the objects on each card,
  especially if the size is reduced.

- Optionally check "Copy current background" to copy the
  background of the currently open stack into the new stack.
- Optionally check "Open stack in new window"
  to keep the stack currently being viewed open.
- Click the Save button.

### Stack Opening

To open an existing stack:

- Select File ... Open Stack...
- In the dialog that appears, navigate to the stack to open.
- Optionally check the "Open stack in new window" checkbox.
- Click the "Open" button.

At least one stack window must be open.
To close another stack that is open in its own window,
click its close box in the upper-left, press cmd-w,
or select File ... Close Stack.

To edit the properties of the current stack, select Objects ... Stack Info...
which opens the following dialog:

<img alt="HyperCard Stack Info" style="width: 45%"
  src="/blog/assets/hypercard-stack-info.png?v={{pkg.version}}">

To enable opening your stacks from the Home stack:

1. Press cmd-h to open the Home stack.
1. Go to Card 3, 4, or 5 that all say
   "You can add your own buttons to this card."
1. Select Home ... New Link to Stack...
1. In the dialog that appears, select a stack file.
1. Click the "Open" button to add a new button.
1. Position the new button as desired.
1. Select the Browse tool.
1. The new button can be clicked to open the stack in a new window.

To enable opening other applications from the Home Stack:

1. Press cmd-h to open the Home stack.
1. Go to Card 3, 4, or 5 that all say
   "You can add your own buttons to this card."
1. Select Home ... New Link to Application...
1. In the dialog that appears, select an application.
1. Click the "Open" button to add a new button.
1. Select the Browse tool.
1. The new button can be clicked to launch the application.

The process is similar to create a button that opens
a given document using a given application.

1. Press cmd-h to open the Home stack.
1. Go to Card 3, 4, or 5 that all say
   "You can add your own buttons to this card."
1. Select Home ... New Link to Document...
1. In the dialog that appears, select a document.
1. Click the "Open" button.
1. In the dialog that appears, select an application that can open the document.
1. Click the "Open" button.

The Home stack initially contains buttons labeled
"Card 3", "Card 4", and "Card 5"
that navigate to a card with the same name.
To rename these buttons and their associated card,
go to one of the cards, select Home ... Rename This Card,
enter the new name in the dialog that appears, and click the "OK" button.
For example, consider renaming "Card 3" to "My Stacks"
if that is where you place links to your stacks.

### Stack Information

To get information about the current stack:

- Select Objects ... Stack Info... to open a Stack Info dialog.

  This enables renaming the stack, resizing it, and editing its script.
  It also gives the number of cards in the stack
  and the number of backgrounds used.

### Stack Navigation

To navigate to the next open stack window,
select Go ... Next Window or press cmd-l.

### Stack Backgrounds

To view and optionally edit a background used by the current card:

- Open the stack.
- Navigate to a card that uses the background.
- Select Edit ... Background or press cmd-b to toggle background mode.
  This is indicated by a hatched pattern in the menu bar.
  The card layer is temporarily hidden and
  only the background layer is displayed.

### Stack Sharing

To share a stack with others, send them the stack file.
They can open the stack by double clicking it,
or by launching HyperCard and selecting File ... Open Stack...

### Card Sorting

To sort all the cards currently in a stack based on
the content of a background field (appears on every card),
open the message box and enter a command like
`sort by field dogName`.
This changes the number of each card in the stack, but not their IDs.

Newly added cards are not automatically places in the current sort order
because the contents of their fields is not known when the card is created.

To automatically resort the stack each time it is opened,
select Objects ... Stack Info..., click the Script button,
and add a handler like the following:

```text
on openStack
  sort by field dogName
end openStack
```

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

### Stack Protection

To protect a stack from user actions,
open the stack and select File ... Protect Stack...
which opens the following dialog:

<img alt="HyperCard Protect Stack dialog" style="width: 50%"
  src="/blog/assets/hypercard-protect-stack.png?v={{pkg.version}}">

If the user level of the stack causes
the "Protect Stack..." menu item to be missing from "File" menu,
hold down the command key when opening the File menu to reveal it.

When the "Can't Modify Stack" checkbox is checked,
users can browse the stack, but are prevented from making any changes
including modifying the text in fields.

When the "Can't Delete Stack" checkbox is checked,
users are prevented from deleting the stack.

When the "Can't Abort" checkbox is checked,
are prevented from aborting operations by pressing cmd-period.
This can be useful in stacks that run in kiosk settings,

When the "Can't Peek" checkbox is checked,
prevents users from seeing:

- the locations of buttons by pressing cmd-option
- the locations of fields by pressing cmd-shift-option

When the "Private Access" checkbox is checked,
users must enter a password in order to access the stack.
If no password has been specified,
a dialog for entering one will be opened.

The radio buttons under "Limit user level to"
set the default user level that is active when the stack is opened.

The "Set Password..." button opens a dialog where a password can be entered.
If the "Private Access" checkbox is checked,
users must enter this password (once per HyperCard session)
in order to open the stack.
Regardless of the "Private Access" setting,
users must enter this password in order to open the "Protect Stack" dialog.

Users that have access to a stack file can find ways
in the HyperCard app to bypass its password protection.
A better way to ensure password protection is to
save the stack as an application (see the next section)
and distribute that.
Applications cannot be edited in HyperCard.

### Stack Copying

To create a copy of the current stack:

- Select File ... Save a Copy...
- In the dialog that appears, select the target directory.
- Enter a stack name under the label "Save a copy of stack as:".
- Optionally change the "File type" to "Application"
  to create an application that can be run without HyperCard.
- Click the "Save" button.

## Cards

A card belongs to a specific stack.
Each card has a foreground and background layer.
Both the foreground and background can contain
graphics, buttons, and fields (for text).
All transparent parts of a card foreground display
content from the background at the same location.
Typically many cards share a background.

### Creating Cards

To add a card to the current stack, select Edit ... New Card or press cmd-n.
The new card will:

- use the same background as the current card.
- be placed immediately after the current card.
- become the current card.
- be assigned a unique ID within the stack that never changes.
- be assigned a number that specifies its order within the stack,
  which can change if the cards are inserted and/or sorted later.

To add a new, first card in a stack:

- Press cmd-1 to go to the first card.
- Select Edit ... New Card to add a new, second card.
- Press cmd-1 to return to the first card.
- Select Edit ... Cut Card to remove the first card.
- Select Edit ... Paste Card to paste the cut card after the new first card.

All of the steps above can be performed in a script using the `doMenu` command.

### Card Information

To edit the properties of the current card, select Objects ... Card Info...
which opens the following dialog:

<img alt="HyperCard Card Info" style="width: 45%"
  src="/blog/assets/hypercard-card-info.png?v={{pkg.version}}">

Cards can be assigned names.
A HyperTalk script can go to a card with a given name
using the command `go [to] card "{card-name}".

The "Card Info" dialog contains the following:

- card name which can be edited
- card number (position) within its stack
- unique card ID within its stack
- number of fields on the card
- number of buttons on the card
- "Card Marked" checkbox to "mark" the card
- "Don't Search Card" checkbox to omit the card from searches
- "Can't Delete Card" checkbox to prevent users from deleting the card
  If an attempt is made to delete the card,
  a dialog will open that says "Can't delete protected card."
- "Script..." button to view and edit the associate script

### Card Navigation

To navigate between the cards in a stack:

| Destination             | Command      | Keyboard Shortcut |
| ----------------------- | ------------ | ----------------- |
| first card              | Go ... First | cmd-1             |
| previous card           | Go ... Prev  | cmd-2             |
| next card               | Go ... Next  | cmd-3             |
| last card               | Go ... Last  | cmd-4             |
| previously browsed card | Go ... Back  | cmd-~             |

Another way to navigate is to open the Navigator Palette (shown below)
by entering the command `nav` in the message box.
This contains buttons that map to the each of the Go menu items except "Scroll".

<img alt="HyperCard Navigator Palette" style="width: 20%"
  src="/blog/assets/hypercard-navigator-palette.png?v={{pkg.version}}">

### Saving Cards

There is no explicit save command.
Changes to the content of a field are automatically saved
when any of the following occur:

- The return key is pressed.
- The mouse is clicked outside the field.
- Focus is moved to another field.
- A menu item is selected.
- Navigation to another card occurs.
- A new card is created.

### Copying Cards

To copy the current card and paste the copy after another card:

- Select Edit ... Copy Card.
- Navigate to the destination card.
- Select Edit ... Paste Card.

The background of new card will be the same as that of the copied card,
not a copy of the background.
Modifying the background will affect all cards that use the background.

If the card is pasted into a different stack
that does not already contain the background of the card,
that background will be added to the stack.

### Moving Cards

To move the current card to a new location in the stack:

- Select Edit ... Cut Card.
- Navigate to the card after which it will be placed.
- Select Edit ... Paste Card.

The background of moved card will not change.
Modifying the background will affect all cards that use the background.

If the card is pasted into a different stack
that does not already contain the background of the card,
that background will be added to the stack.

### Deleting Cards

To delete the current card, select Edit ... Delete Card or press cmd-delete.
Be careful because no confirmation is requested!
If no other action has taken place yet, the deleted card
can be recovered by selecting Edit ... Undo or pressing cmd-z.

### Recent Cards

HyperCard remembers the last 42 unique cards viewed in the current session.
To view thumbnails of the viewed cards, select Go ... Recent or press cmd-r.
This opens a modal dialog.
The thumbnail for the card currently being viewed has a black border.
To navigate to a different card and dismiss the dialog, click its thumbnail.
To remain on the current card and dismiss the dialog,
click the background of the dialog.

### Marking Cards

Marking cards provides a way to identify a subset of
the cards in a stack that meet specific criteria.

There are many reasons to mark cards including:

- bookmark cards to be revisited later
- perform an operation on a subset of the cards, such as exporting data
- print a subset of the cards

To mark or unmark a card from the HyperCard UI:

1. Open its "Card Info" dialog.
1. Check or uncheck the "Card Marked" checkbox.

The following HyperTalk commands mark or unmark cards in scripts:

- `[un]mark {card-ref}`

- `[un]mark cards where {boolean-expression}`

  e.g. `mark cards where the length  of field dogBreed > 6`

- `[un]mark cards by finding {string} in {field-ref}`

  e.g. `mark cards by finding whip in field dogBreed`

- `[un]mark all cards`

To mark all cards that meet some criteria, use the command `mark {card-ref}`.

### Finding Cards

To search for an occurrence of text, select Go .. Find... or press cmd-f.
Then enter search text inside the provided double quotes
and press the return key to go to
the next occurrence of a field containing matching text.

Key facts about the `find` command are:

- The search is case-insensitive.
- The matching text is surrounded by a black rectangle.
- Only fields are searched, not button labels or painted text.
- If no match is found, the beep sound is played.
- The last search is remembered. If you begin a new search later,
  the previous search text will be in the quotes.
- When the search string contains spaces,
  they are treated as delimiters between search terms.
  The search is for the next card with a field containing
  a word that **begins** with each of the search terms.
  The words are not required to be in the same field.
  For example, `find "com whip"` matches a card with
  a field that contains "Comet Fireball" AND
  a field that contains "Brindle Whippet".
- Multi-term searches are AND searches, not OR searches.

To repeat the search to find the next occurrence,
possibly on the same card, press the return key.
After the last occurrence is found,
the search wraps around to the first card in the stack.

The `find words` command matches cards with fields that contain
complete words that match the search terms.
For example, `find words "comet whippet"` matches a card with
a field that contains "Comet Fireball" AND
a field that contains "Brindle Whippet".

The `find whole` command matches cards with a single field that contains
the specified words in order.
For example, `find whole "comet fireball"` matches
a card with a field that contains "Comet Fireball", but does not match
a card with a field that contains "Comet the Fireball".
This command can be entered in the message box by pressing cmd-shift-f.

The `find chars` command matches substrings.
For example, `find chars "ome pet"` matches a card with
a field that contains "Comet Fireball" AND
a field that contains "Brindle Whippet".

HyperCard does not treat the characters "?" and "*"
in search terms like wildcards.
Instead of using `find chars "wh*pet"`and hoping to match "Whippet",
use`find chars "wh pet"` which is almost the same.
It differs in that words containing "wh" and "pet"
can be found in different fields of the same card.

The `find string` command is similar to `find chars`, but it
treats the search string as a single term even if it contains spaces.
It matches cards with a single matching field.
For example, `find string "met fir"` matches a card with
a field that contains "Comet Fireball".

To treat accented characters the same as their non-accented counterparts
when finding matches, add the keyword `international` after `find`
in any of the `find` commands described above.
For example, this treats the character é the same as the character e.

### Card Issues

Sadly there is no easy way to:

- reorder the cards in a stack
- change the background used by an existing card

These seem like large oversights!

## Tools

The tools menu contains a grid of buttons
that can be clicked to select one of the 18 tools.
These are arranged in six rows and three columns.

Drag the Tools menu off the menu bar
to create a floating palette of tool buttons.
Alternatively, press option-tab to toggle display of the Tools palette.

<img alt="HyperCard Tools" style="width: 15%"
  src="/blog/assets/hypercard-tools.png?v={{pkg.version}}">

Painting can be performed in both the background and card domains.

Painting tool operations can be undone by
selecting Edit ... Undo, pressing cmd-z, or pressing the esc key.
But this must be done BEFORE performing another operation.

### Tools Palette

The first row of buttons select the current mode
which is always one of Browse, Button, or Field.
The remaining rows are painting tools that act on bitmap graphics.
They select, draw, or erase square pixels that are either white or black.

The tools include:

- Row #1

  - **Browse**

    The Browse tool enters Browse mode which enables interacting with cards as a user
    rather than as an author.

    In Mac OS 7, another way to enter Browse mode is to press cmd-tab.
    This doesn't work in Mac OS 8 and 9 because in those,
    cmd-tab switches to the next active application.
    Another approach for Mac OS 8 and 9 is
    described in "HyperTalk - Function Keys" below.

  - **Button**

    The Button tool enters Button mode which enables editing existing buttons.
    When in this mode, a thin black border is drawn around all buttons
    so they can be located even when they are transparent with no text or icon.

    In Mac OS 7, another way to enter Button mode is to press cmd-tab-tab.

  - **Field**

    The Field tool enters Field mode which enables editing existing fields.
    When in this mode, a thin black border is drawn around all text fields
    so they can be located even when they are transparent with no text.

    In Mac OS 7, another way to enter Field mode is to press cmd-tab-tab-tab.

- Row #2

  - **Selection**

    The Selection tool selects a rectangular area specified by
    dragging from any corner of the painted area to be selected
    to the opposite corner.
    The selected area will be surrounded by a rectangle of marching ants.
    The selected area can then be copied, cut, deleted, dragged, or transformed
    (using the Options menu items Rotate, Slant, Distort, and Perspective).

    If the cmd key is held down before releasing the mouse button,
    the selected area will collapse to be the
    smallest rectangle that contains all the black pixels.

    To constrain dragging of the selected area
    to be only in the horizontal or vertical direction,
    hold down the shift key while dragging it.

    To duplicate the selected area,
    hold down the option key while dragging it.

    To copy or move the selected area to another card,
    possibly in another stack:

    1. Select Edit ... Copy (cmd-c) to copy or
       Edit ... Cut Button (cmd-x) to move.
    1. Navigate to the destination card.
    1. Select Edit ... Paste Button (cmd-v).

       The pasted content will be selected and can be dragged to a new location.

    To both constrain the movement AND duplicate the selected area,
    hold down both the shift and option keys while dragging it.

    To select the entire picture layer
    of the current domain (background or card),
    double click the Selection tool in the Tools palette.
    This is useful for copying everything that was painted
    to another card and/or domain.

    To paint a trail of the selected area repeated every 1 to 8 pixels:

    - Specify the number of pixels between the copies by
      pressing option-{n} where n is a number from 1 to 8.
    - Hold down the cmd and option keys.
    - Optionally, also hold down the shift key to
      constrain the movement to be only horizontal or only vertical.
    - Drag the selected area.

    To stretch (or shrink) the selected area, hold down the cmd key
    and drag any corner or edge of the selection rectangle.

    Select Options ... Grid to constrain dragging and stretching
    to be in increments of 8 pixels.

  - **Lasso**

    The Lasso tool enables selecting
    non-rectangular portions of the painted layer.
    Drag around an arbitrary portion of the painted area.
    When the mouse button is released, the dragged path will automatically
    be closed, as if with a straight line to the starting point.
    Each contiguous set of black pixels inside the selected area
    will be surrounded by a path of marching ants.
    The selection is the combined contents of those paths,
    not the entire contents of the lassoed area.
    The selection can then be copied, cut, deleted, or dragged.

    The modifier keys that affect the Selection tool
    affect the Lasso tool in mostly the same way.

    To select all the items in the picture layer
    of the current domain (background or card),
    double click this tool.

  - **Pencil**

    The Pencil tool is used for freehand drawing
    which is accomplished by dragging.
    The pencil width is a single pixel is not affected by
    selecting a different size from Options ... Line Size...

    If the current pixel color at the start of the drag is black
    then the drawing color will be white.
    Otherwise it will be black.
    It makes no difference whether the start pixel color
    comes from the card or background domain.
    The drawing color remains the same throughout the drag.

    Hold the shift key down while dragging to
    constrain the line to be horizontal or vertical,
    depending on the initial drag direction.

    To toggle FatBits mode, double click this tool
    as an alternative to selecting Options ... FatBits.
    Another way to enter FatBits mode when the Pencil tool is selected
    is to cmd-click on the pixel to be in the center of the FatBits display.

- Row #3

  - **Brush**

    The Brush tool is used for freehand painting
    using the selected brush shape and pattern.
    To change the shape and size of the brush,
    Select Options ... Brush Shape... before painting.

    <img alt="HyperCard Brush Shape dialog" style="width: 50%"
      src="/blog/assets/hypercard-brush-shape-dialog.png?v={{pkg.version}}">

    For example, select the diagonal line brush shape
    to draw calligraphy.

    Hold the shift key down while dragging to
    constrain the painted stroke to be horizontal or vertical,
    depending on the initial drag direction.

    To use the Brush tool as an eraser,
    hold down the cmd key while dragging.
    When used in the background domain, this turns all pixels to white.
    When used in the card domain, this turns all pixels to transparent.

    To turn all erased pixels to white instead of transparent,
    hold down the cmd key while dragging.
    When used in a card domain, this provides an easy way to
    add white pixels that hide part of the background domain for that card.
    It is an alternative to using the Brush tool when the all white pattern
    or the Rectangle tool filled with the all white pattern.

    The default brush shape is a small circle.
    To open the "Brush" dialog so the brush shape can be changed
    to one of the 32 options, double click this tool
    as an alternative to selecting Options ... Brush Shape...

  - **Eraser**

    The Eraser tool is a white square whose shape and size cannot be changed.
    Drag over pixels to erase them.
    In a background domain this changes black pixels to white.
    In a card domain this changes black and white pixels to transparent,
    revealing black pixels in the background.

    To erase large areas, it is more efficient to
    select an area using the Selection or Lasso tool
    and then press the delete key.

    For fine grained erasing, use "Fat Bits" mode.

    Hold the shift key down while dragging to
    constrain the eraser movement to be horizontal or vertical,
    depending on the initial drag direction.

    To erase all painting the current domain (background or card),
    double click this tool. Undo to restore everything that was erased.
    To keep a selected area and erase all the rest,
    select the area to keep, copy it to the clipboard,
    double click this tool, and paste the copied area.

    It's too bad that the Erase shape and size
    is not determined by the selected brush shape.

  - **Line**

    The Line tool draws a straight line.
    Click the start pixel and drag to end pixel.

    To change the line width from the default of 1 pixel,
    Select Options ... Line Size... or double click this tool before drawing.
    This opens the "Line Size" dialog where
    a width of 1, 2, 3, 4, 6, or 8 pixels can be selected.

    <img alt="HyperCard Line Size dialog" style="width: 30%"
      src="/blog/assets/hypercard-line-size-dialog.png?v={{pkg.version}}">

    To constrain the angle of the line to multiples of 15 degrees,
    hold down the shift key
    before ending the drag by releasing the mouse button.
    This is most often used to create horizontal (0 or 180 degrees)
    or vertical (90 or 270 degrees) lines.

    To draw the line using the selected pattern,
    hold down the option key before dragging.
    The painted pattern is not affected by the angle of the line.

    To draw a dotted line:

    1. Double click the Line tool.
    1. Select a line width of 1 pixel.
    1. Select one of the provided patterns such as
       3, 4, 5, 11, 13, 14, 17, or 19.
    1. Hold down the shift and option keys.
    1. Drag to create a dotted line that is horizontal or vertical.

    To draw a dashed line:

    1. Double click the Line tool.
    1. Select a line width of 1 pixel.
    1. Edit one of the patterns to match the screenshot below.
    1. Select that pattern.
    1. Hold down the shift and option keys.
    1. Drag to create a dashed line that is horizontal or vertical.

    <img alt="HyperCard dashed line pattern" style="width: 30%"
      src="/blog/assets/hypercard-dashed-line-pattern.png?v={{pkg.version}}">

- Row #4

  - **Spray**

    The Spray tool sprays paint (black pixels) using
    randomly chosen pixels from the selected pattern.
    The more this is applied to a specific area,
    the more it will resemble the selected pattern.

    The speed of the drag affects the number of pixels that are painted,
    with slow drags painting more pixels.

    Hold the shift key down while dragging to
    constrain the spray movement to be horizontal or vertical,
    depending on the initial drag direction.

    To spray white pixels instead of black,
    hold down the cmd key while dragging.

  - **Rectangle**

    The Rectangle tool draws a rectangle.
    Click the location of any corner and drag to the opposite corner.

    To draw a square, hold down the shift key while dragging.

    To specify the center point of the rectangle
    and drag out its size from there,
    select Options ... Draw Centered before dragging out the rectangle.
    The center of the cross cursor will turn white to indicate
    being in this mode and aid it locating the desired center pixel.

    The "Draw Centered" mode can be used to draw concentric shapes of all types.
    Begin by drawing a dot at the target center.
    Draw each shape by starting at this center location and dragging outward.
    Finally, erase the dot at the center.

    To move the rectangle as it is being drawn, keep the mouse button down,
    hold the cmd key down, and drag to a new location.
    To continue drawing the rectangle, release the cmd key,
    but keep the mouse button down and continue dragging.

    To toggle the "Draw Filled" option, double click this tool
    as an alternative to selecting Options ... Draw Filled.

    To draw the border using the selected line thickness and pattern,
    hold down the option key before the initial corner click.
    Combine this with the "Draw Filled" option to
    use the same pattern for both the border and the fill,
    resulting in a rectangle that appears to have no border.

    To draw a dashed line border, select a line width of 1 pixel,
    edit one of the patterns to match the screenshot below,
    select that pattern, and draw the rectangle with the option key held down.

    <img alt="HyperCard dashed line pattern" style="width: 30%"
      src="/blog/assets/hypercard-dashed-line-pattern.png?v={{pkg.version}}">

  - **Round Rectangle**

    The Round Rectangle tool draws a rectangle with rounded corners.
    It is used in the same way as the Rectangle tool,
    with all the same keyboard shortcuts.

    The size of the rounded corners cannot be adjusted.
    The only option to obtain a different rounded corner size
    is to draw a normal rectangle and
    use "Fat Bits" mode to manually edit the corners.
    This is quite tedious.

- Row #5

  - **Bucket**

    The Bucket tool fills an area encompassed by black pixels
    with the selected pattern from the Patterns menu or palette.
    The cursor changes to match the Bucket tool icon.
    Position the bottom top of the dripping paint in the cursor icon
    over a pixel inside the area to be filled and click.

    When a solid black area is filled with the Bucket tool,
    all the black pixels, including the border,
    are replaced by the selected pattern.

    To toggle display of the Patterns palette, double click this tool.

  - **Oval**

    The Oval tool draws an oval which is any closed curve that
    resembles the outline of an egg or an ellipse.
    It doesn't have a strict mathematical definition.

    This tool is used in the same way as the Rectangle tool.

    To draw a circle, hold down the shift key while dragging.

    To draw a portion of an oval,
    draw a complete oval and erase the undesired part.

  - **Curve**

    The Curve tool is similar to the Pencil tool, but it:

    - uses the selected line width
    - automatically closes the shape and fills it
      if Options ... Draw Filled is selected
    - draws with the selected pattern if the option key is held down

    To toggle the "Draw Filled" option, double click this tool
    as an alternative to selecting Options ... Draw Filled.

- Row #6

  - **Text**

    The Text tool paints text that cannot be edited.
    It is often used for card titles or for
    labels that appear above or to the left of fields.

    While entering text, press the return key to create multi-line text.

    The text style must be specified before typing the text.
    To open the "Text Properties" dialog,
    select Edit ... Text Style..., press cmd-t, or
    double click this tool in the Tools palette.
    Select the font styles, alignment, font name,
    font size, and line height to be used.
    A sample of using the selected text properties
    is displayed in the lower right of the dialog.
    Click the "OK" button when satisfied.

    To paint text, click the Text tool, click on the card or background
    where the text should go, and begin typing.

    To paint multiline text, press the return key at the end of a line.

    To select the text after it has been typed, but before clicking away,
    press cmd-s. The text can then be dragged to a new location.

    As long as you have not clicked away from the text being typed,
    its text properties can be changed.
    Opening the "Text Properties" dialog again and
    changing them affects the text already typed and
    the text that will be typed after the dialog is closed.

    To apply a pattern to painted text, see the "Patterns" section below.

    Pros of painted text:

    - Users do not need to have the selected font installed
      because the text is stored in bitmap form.
    - The text is automatically read-only.
      Using a field instead requires opening its "Field Info" dialog
      and checking the "Lock Text" checkbox.

    Cons of painted text:

    - The bitmap representation requires more disk space
      than would be used by the same text in a locked field.
    - The text cannot be modified later.
      Changes require erasing the text and recreating it.
      For text that may need to be edited later, use a locked field instead.
    - All the characters in one set of painted text
      must use the same text properties.
      To use multiple text properties settings,
      create multiple sets of painted text and position them next to each other.
      The text in a field can used multiple text properties settings.

  - **Regular Polygon**

    The Regular Polygon tool draws a convex polygon
    whose sides all have the same length.
    It is used in the same way as the Rectangle tool,
    with many of the same keyboard shortcuts.

    Select Options ... Polygon Sides... or double-click this tool
    to opens a dialog with the options
    triangle (3), square (4), pentagon (5),
    hexagon (6), octagon (8), and circle (infinite).

    <img alt="HyperCard Polygon Sides dialog" style="width: 40%"
     src="/blog/assets/hypercard-polygon-sides-dialog.png?v={{pkg.version}}">

    Click the center location and draw outward
    to specify the size and rotation of the shape.

    To constrain the angle of the shape to multiples of 15 degrees,
    hold down the shift key
    before ending the drag by releasing the mouse button.

    To change the border width from the default of 1 pixel,
    Select Options ... Line Size... or double click the Line tool
    before using this tool.

    To draw a filled polygon, select Options ... Draw Filled
    before using this tool.

    To move the polygon as it is being drawn, keep the mouse button down,
    hold the cmd key down, and drag to a new location.
    To continue drawing the polygon, release the cmd key,
    but keep the mouse button down and continue dragging.

    To draw the border using the selected line thickness and pattern,
    hold down the option key before the center click.
    Combine this with the "Draw Filled" option to
    use the same pattern for both the border and the fill,
    resulting in a polygon that appears to have no border.

  - **Irregular Polygon**

    The Irregular Polygon tool draws an arbitrary polygon.
    Click at each point in the polygon.
    To close the polygon, click on the start point.
    If the "Draw Filled" option is on, the polygon can be closed by
    double-clicking the last point, even if the selected pattern is all white.

    To toggle the "Draw Filled" option, double click this tool
    as an alternative to selecting Options ... Draw Filled.

    The keyboard shortcuts described for the Regular Polygon tool
    are also used by this tool.

    To constrain the angle of each edge to multiples of 15 degrees,
    hold down the shift key before clicking the first point.

Unlike in some applications, shapes cannot be selected by clicking them.
To select all or part of a drawing,
use the Rectangle or Lasso Selection Tools.

To erase part of a drawing, do one of the following:

- Select the Erase tool and drag over the drawing.
- Select an area with the Rectangle or Lasso Selection Tools
  and press the delete key, or select Edit ... Cut Picture, or press cmd-x.

### Menu Changes

When a painting tool is selected,
the Objects, Font, and Style menus disappear,
and the Paint, Options, and Patterns menus appear.
When the Browse, Button, or Field tools are selected,
the opposite menu changes occur.

### Paint Menu

The Paint menu is only present when one of the painting tools is selected
in the Tools menu or the Tools palette.
This menu contains the following menu items:

- Select (cmd-s)

  This selects the last most recently painted item.
  To also exit background editing mode,
  hold down the shift key when clicking this.

- Select All (cmd-a)

  This selects the entire painting layer
  of the current domain (background or card).

- Fill

  This acts on the last item created.
  If the item is already filled,
  it changes the interior to be filled with the currently selected pattern.
  If the item is not filled,
  it changes its border to be painted with the currently selected pattern.

- Invert

  This inverts all pixels in the selected area,
  changing transparent and white pixels to black,
  and changing black pixels to transparent.

- Pickup

  This copies an area of the painting layer
  so it can be pasted elsewhere.
  To do this:

  1. Draw any filled shape on top of the area to be copied.
  1. Select Paint ... Fill.
  1. Drag the copy that is automatically selected
     to another location of the current card OR
     cut it, move to another card, and paste it.

- Darken

  This can be applied repeatedly to gradually and randomly change pixels
  inside the most recently painted item or the selected area to black.

- Lighten

  This can be applied repeatedly to gradually and randomly change pixels
  inside the most recently painted item or the selected area to white.

- Trace Edges

  This highlights the edges of all the selected shapes by
  inverting their border color and
  tracing new shapes around them, both outside and inside the original shapes.

  The Trace Edges menu item can be applied multiple times.
  The screenshot below shows the effect when it is applied to text four times.

  <img alt="HyperCard Trace Edges multiple times" style="width: 25%"
    src="/blog/assets/hypercard-trace-edges-multiple.png?v={{pkg.version}}">

- Rotate Left

  This rotates the most recently painted item or the selected area
  90 degrees to the left.

  In can introduce gaps in the border of some shapes
  that prevent filling with the Bucket tool.
  The gaps can be repaired in FatBits mode.

- Rotate Right:

  This rotates the most recently painted item or the selected area
  90 degrees to the right.

  In can introduce gaps in the border of some shapes
  that prevent filling with the Bucket tool.
  The gaps can be repaired in FatBits mode.

- Flip Vertical

  This flips the selected pixels over a horizontal line
  through the center of the selected area.

- Flip Horizontal

  This flips the selected pixels over a vertical line
  through the center of the selected area.

- Opaque

  This changes all transparent pixels in the selected area to be white.
  It is useful to hide background elements.

- Transparent:

  This changes all white pixels in the selected area to be transparent
  which allows background elements to show through.
  It is useful when copying a selected area of a painting layer
  because it defaults to having an opaque background.

- Keep

  Multiple changes made to a painting layer (background or card)
  are not save until one the following occurs:

  - select a non-painting tool
  - switch to the opposite domain (background or card)
  - navigate to another card
  - select Paint ... Keep

  The Keep menu item provides a way to explicitly save painting layer changes.
  The next menu item, Revert, only reverts unsaved painting layer changes.

- Revert

  This reverts (undoes) all painting layer changes since they were last saved.
  The description of the Keep menu item above
  lists the actions that cause those changes to be saved.

### Options Menu

The Options menu contains the following menu items:

- Grid

  This toggles grid mode where invisible grid lines are spaced every 8 pixels.
  When turned on, all points selected when painting
  using the following tools are snapped to a grid point:
  Line, Rectangle, Rounded Rectangle, Oval,
  Regular Polygon, and Irregular Polygon.
  This makes it easier to create items that are a specific size
  and line up multiple painted items horizontally or vertically.

- FatBits

  This toggles FatBits mode.
  When turned it, the window zooms in on the selected pixels
  or the most recently painted item.
  This makes detailed painting easier.
  The pixels can be modified using all the painting tools
  in the Tools menu and palette.
  To toggle individual pixels, select the Pencil tool and click the pixels.

  A small window that initially appears in the lower left shows
  how the area visible in the large window will appear at the normal scale.
  This window can be dragged to a different location
  or closed if not needed.

  To scroll around in the large window where editing takes place,
  hold down the option key (which turns the cursor into a hand) and drag.

  To enter FatBits mode, perform any of the following actions:

  - select Options ... FatBits
  - double click the Pencil tool
  - with the Pencil tool selected,
    cmd-click anywhere in the card window

  The last option is preferred because it is the only option
  that specifies exactly where the zoom should be centered.

  To exit FatBits mode,
  perform any of the actions above for entering FatBits mode
  OR select the Browse, Button, or Field tool.

- Power Keys

  This enables the use of single-character key shortcuts
  to activate a specific menu item or perform a common action.
  Many of the keys require an area of the painting layer to be selected
  which is what they will act upon.

  - A - Paint ... Select All
  - B - Select the all black pattern.
    (TODO: This selects a different pattern for me.)
  - C - Options ... Draw Centered (toggles)
  - D - Paint ... Darken
  - E - Paint ... Trace Edges
  - F - Paint ... Fill
  - G - Options ... Grid (toggles)
  - H - Paint ... Flip Horizontal
  - I - Paint ... Invert
  - L - Paint ... Lighten
  - M - Options ... Draw Multiple (toggles)
  - O - Paint ... Opaque
  - P - Paint ... Pickup
  - R - Paint ... Revert
  - S - Paint ... Select
  - T - Paint ... Transparent
  - V - Paint .. Flip Vertical
  - W - Select the all white pattern.
  - 1, 2, 3, 4, 6, and 8 - Set the line thickness to that number of pixels.
  - [ - Paint ... Rotate Left
  - ] - Paint ... Rotate Right

  The D and L keys are handy for repeatedly applying Darken and Lighten
  more easily than repeatedly selecting the corresponding menu items.

  Additional keyboard shortcuts enabled by the "Power Keys" mode
  that begin with cmd-shift include:

  - [ - previous font
  - ] or F - next font
  - &lt; - next smaller font size
  - &gt; - next larger font size
  - minus - decrease line height
  - plus - increase line height

- Line Size...

  This opens a dialog that allows one of six line widths to be selected.
  The default is a small, round brush.
  The choice affects all subsequent painting operations
  and remains in effect until changed.

  <img alt="HyperCard Line Size dialog" style="width: 30%"
    src="/blog/assets/hypercard-line-size-dialog.png?v={{pkg.version}}">

- Brush Shape...

  This opens a dialog that allows one of 32 brush shapes to be selected.
  The default is a single pixel.
  The choice only affects use of the brush tool
  and remains in effect until it is changed.

  <img alt="HyperCard Brush Shape dialog" style="width: 50%"
    src="/blog/assets/hypercard-brush-shape-dialog.png?v={{pkg.version}}">

- Edit Pattern...

  This opens a dialog that allows the pixel colors (white or black)
  of the currently selected 8x8 pixel pattern to be modified.
  Click or drag across pixels in the dialog to toggle them
  between black and white in the same way as using the Pencil tool.
  Other painting tools such as Line and Rectangle cannot be used here.

  <img alt="HyperCard Edit Pattern dialog" style="width: 30%"
    src="/blog/assets/hypercard-dashed-line-pattern.png?v={{pkg.version}}">

  The supplied patterns are designed for placing copies in columns and rows.
  This means that their right edges flow nicely into their left edges
  and their bottom edges flow nicely into their top edges.
  Any edits you make should do this as well.

  I couldn't find a way to reset a pattern to its default state,
  so be careful with these changes!

- Polygon Sides...

  This opens a dialog that allows selection of the number of sides
  drawn by the regular polygon tool.
  The choices are triangle (3), square (4), pentagon (5),
  hexagon (6), octagon (8), and circle (infinite).

  <img alt="HyperCard Polygon Sides dialog" style="width: 40%"
   src="/blog/assets/hypercard-polygon-sides-dialog.png?v={{pkg.version}}">

- Draw Filled:

  If this option is selected, the following tools will
  fill their shape with the selected pattern:
  Rectangle, Rounded Rectangle, Oval, Regular Polygon, and Polygon.

  If this option is selected, the following tools will
  draw their lines with the selected pattern:
  Line, Rectangle, Rounded Rectangle, Oval, Regular Polygon, and Polygon.

  A shortcut for toggling the "Draw Filled" option is to double click
  any of the shape buttons in the Tools palette that draw a closed shape.

- Draw Centered:

  This option causes a drawn shape to be
  centered at the location of the initial click.
  It stays in effect until toggled off.

- Draw Multiple

  This option causes the following painting tools to
  draw multiple times as the mouse is dragged:
  Rectangle, Rounded Rectangle, Oval, Curve, and Regular Polygon.
  Before drawing a shape with this option turned on,
  specify the number of pixels between the copies by
  pressing option-{n} where n is a number from 1 to 8.

  Interesting designs can be created by
  using this with the Regular Polygon tool.
  For example, double click that tool and select the triangle shape.
  Then click the intended center and drag away in a curved path
  to create something like the screenshot below.

  <img alt="HyperCard Regular Polygon Draw Multiple" style="width: 40%"
    src="/blog/assets/hypercard-regular-polygon-draw-multiple.png?v={{pkg.version}}">

- Rotate

  This menu item is only enabled when the Selection tool (not the Lasso tool)
  has been used to select a rectangular area.
  It temporarily changes the selection rectangle lines to solid lines
  and adds square handles to the four corners.
  The handles can be dragged to rotate all the selected pixels
  around the center point of the selection rectangle.
  To restrict the rotation angle to increments of 15 degrees,
  hold down the shift key during dragging.
  During dragging, only the solid lines and handles move.
  When the mouse button is released, the selected pixels are modified.

- Slant

  This menu item works similarly to Rotate,
  but dragging a handle turns the selected rectangle into a parallelogram.
  The result is to "slant" all the selected pixels.

- Distort

  This menu item works similarly to Rotate,
  but dragging a handle moves only that handle in any direction.
  The result is to "distort" all the selected pixels.
  This can be thought of as a combination of Slant and Perspective.

- Perspective:

  This menu item works similarly to Rotate,
  but dragging a handle moves only that handle horizontally or vertically.
  The result is to change the selected pixels
  in a way that creates an illusion of depth.

### Patterns Menu

The Patterns menu contains the Patterns palette
which contains a grid of 40 buttons that can clicked
to select the pattern that is used by the Bucket and Spray Can tools.

<img alt="HyperCard Patterns" style="width: 15%"
  src="/blog/assets/hypercard-patterns.png?v={{pkg.version}}">

Like the Tools menu, the Patterns menu can be
dragged off of the menu bar to create a floating palette.

When a painting tool is selected, press the tab key
to toggle display of the Patterns palette.
This works even if it was not previously dragged off of the menu bar.

To fill an existing, closed shape with the selected pattern,
select the Bucket tool and click the
dripping tip of the bucket icon (lower-right) inside the shape.

To cause drawn shapes to be filled with the selected pattern,
select Options ... Draw Filled before drawing the shape.

To edit the pixels in a pattern:

1. Double click the pattern to open the "Edit Pattern" dialog.
1. Click pixel in the left square to toggle them between white and black.
1. Verify that the intended, actual size pattern is visible in the right square.
1. Click the "OK" button.

TODO: How can an edited pattern be reset to its default?

The patterns are numbered from 1 to 40 starting in the upper-left
and progressing down each column.
For example, the first pattern in the second column is number 11.
To copy a pattern from one stack to another, TODO: HOW?

To apply a pattern to the lines in characters drawn with the Text tool,
select a pattern, select the Bucket tool, and click inside each character.
To include a black outline around each character,
select all the text, select Paint ... Trace Edges, and use
the Bucket tool to fill the resulting character outlines with a pattern.

<img alt="HyperCard Text Patterns" style="width: 30%"
  src="/blog/assets/hypercard-text-patterns.png?v={{pkg.version}}">

## Buttons

Buttons perform some action defined by the stack author when they are clicked.

In HyperCard, only fields received focus, not buttons like in HTML.
So the part numbers of buttons only affect stacking order,
not tab navigation.

For inspiration on buttons that can be created,
open the "Home" stack, click "Stack Kit", click "Readymade Buttons",
and visit each of the topics.
The buttons defined here can be copied into your stacks.

### Button Types

There are four basic kinds of buttons:

1. Buttons that perform an action when clicked.
   The style for these can be "Transparent", "Opaque", "Rectangle",
   "Rounded Rect", "Shadow", "Oval", "Standard", or "Default".
   Any of these button styles can contain text, an icon, both, or neither.
1. "Check Box" for specifying a Boolean value.
1. "Radio Button" for selecting from a small set of options.
1. "Popup" that displays a dropdown menu of options when clicked
   for selecting from a possibly large set options.

To implement selection from a scrolling list options,
use a field that displays a scrolling list of options.
See the demo "List Selection" below.

Each of the supported button styles are shown below.

<img alt="HyperCard button types" style="width: 50%"
  src="/blog/assets/hypercard-button-types.png?v={{pkg.version}}">

### Creating Buttons

To add a button to the current card.

1. Verify that the correct domain, card or background, is selected.

1. Select Objects ... New Button.
   Alternatively, select the Button tool and
   cmd-drag to indicate the location and size of a new button.
   This adds a new button to the center of the current card.

1. Double-click the button to open its "Button Info" dialog.

   <img alt="HyperCard Button Info" style="width: 50%"
     src="/blog/assets/hypercard-button-info.png?v={{pkg.version}}">

1. Change "Button Name" to be a name used to refer to the button
   and the text that will appear on it.
   This can be up to 30 characters and can include
   spaces, numbers, and punctuation, but cannot be all numbers.
   It's too bad buttons can't have separate values for
   the name used to refer to them and the label displayed in them.

1. Select a button style from the following options:

   - Transparent: no border and can see through

     This style is used for buttons that:

     - have an icon, but "Show Name" is unchecked
     - are positioned over a specific part of a graphic (e.g. city on a map)
     - are positioned over a specific text to act as a hyperlink

     This is the default style when a button is created with cmd-drag.

   - Opaque: no border and cannot see through

   - Rectangle: opaque with rectangle border

   - Round Rectangle: opaque with rectangle border, rounded corners,
     and shadows on right and bottom sides;

     This is the default when created with the "New Button" menu item.

   - Shadow: like Rectangle, but adds shadows on the right and bottom sides

   - Check Box: for a Boolean selection

     Check "Show Name" to display a label for the checkbox to its right.

     It doesn't make sense to uncheck the "Auto Hilite" checkbox.
     When it is checked, the state is toggled by
     clicking either the checkbox or the label.
     Doing so toggles the value of its `hilite` property.

     For more detail, see the "Check Boxes" section below.

   - Radio Button: for a set of mutually exclusive choices

     Check "Show Name" to display a label for the radio button to its right.
     The "Auto Hilite" checkbox cannot be unchecked for radio buttons.

     Select the same "Family" number (1-15) to each radio button
     on the card that is a member of the same group.

     For more detail, see the "Radio Buttons" section below.

   - Standard: conforms to Macintosh interface guidelines for non-default buttons

   - Default: conforms to Macintosh interface guidelines for default buttons

     There should be at most one Default button per card
     and a card script must be added to
     trigger the button when the return key is pressed.
     For example:

     ```text
     on returnKey
       send "mouseUp" to card button "My Default Button"
     end returnKey
     ```

   - Oval: like Transparent, but the target (clickable) area
     is an oval instead of a rectangle

   - Popup: a dropdown containing options

     For more detail, see the "Popups" section below.

1. If the selected style is "Radio Button",
   select a Family number from 1 to 15
   that is the same for all radio buttons in the group.

1. Decide whether the button name should be displayed
   by checking or unchecking the "Show Name" checkbox.

   - It will be checked if the button was created with "New Button".
   - It will be unchecked if the button was created with cmd-drag.

1. Optionally check the "Auto Hilite" checkbox to
   cause the button to highlight when it is clicked.

   Highlighting toggles the colors of the pixels for the button
   when the mouse button is down and
   restores the colors when the mouse button is released.

1. Optionally uncheck the "Enabled" checkbox to
   cause the button to be disabled by default.

   When a button is disabled, it will be grayed out
   and clicking it will not run its script.

   To enable/disable a button from a script, use the following command:

   ```text
   set the enabled of {button-ref} to {true|false}
   ```

1. Click the "Text Style..." button to open the "Text Properties" dialog.
   This allows specifying the text style, alignment, font, and size.
   Alternatively, specify each of these except alignment
   using the Font and Style menus.

1. Click the "Icon..." button to add an icon to the button or remove its icon.

   This opens a "Choose Icon" dialog.

   To add an icon:

   - Select an icon.

     Its ID, name and source file are displayed at the top of the dialog.
     For example, most of the icons come from ICON resources
     defined in the "HyperCard" app itself.
     Some icons come from ICON resources in the "Home" stack.
     Optionally click the Edit button to edit the pixels in the icon.

   - Click the OK button to use the selected icon.

   To remove the current icon, click the "None" button.

   Icons are all 32x32 pixels. Their sizes cannot be modified.

   The active area of all buttons that receives mouse clicks
   is always a rectangle.
   This is the case even if the button has the "Transparent" style
   and has a non-rectangular icon.

1. Click the "LinkTo..." button to modify the `mouseUp` handler
   so clicking the button navigates to another card or stack.

   This opens a dialog with the buttons "This Card", "This Stack", and "Cancel".

   - To cause a click on the button to navigate to
     another card in the same stack, navigate to that card
     and then click the "This Card" button.
     This adds the command `go to card id "{card-id}"`
     and returns to the card containing the button.
   - To cause a click on the button to navigate to
     the first card in another stack, navigate to that stack (any of its cards)
     and then click the "This Stack" button.
     This adds the command `go to stack "{stack-name}"`
     and returns to the card containing the button.
   - To close the dialog without making a change, click the "Cancel" button.

   The change affects the script handler for the "mouseUp" action.
   The handler is added if it doesn't exist and is modified if it already exists.
   Open the script for the button to see the change that is made.

1. Click the "Tasks..." button to modify the `mouseUp` handler
   to execute a command for one of the following seven actions:

   1. Go to Destination: Choose from 10 radio buttons.
   1. Visual Effect: Choose an effect and a speed.
      For more detail, see the "Card Transitions" section below.
   1. Launch Application: Choose an application and
      optionally a specific document to open.
   1. Link to URL: Enter a URL to be opened in the default web browser.
   1. Movie: Choose a movie file and specify details on
      how and where it should be played.
   1. Sound: Choose a sound resource already available to the stack,
      record a sound to create a new sound resource (doesn't work),
      or import a sound resource ("snd") from another file.
      Select playback quality (Good, Better, or Best).
   1. Speak Text: Enter text or use selected text. Select a voice.

1. Optionally click the "Script..." button
   to open a dialog where a script can be added or modified.

   Enter code that defines action handlers and functions.
   Actions related to buttons include `mouseDown`, `mouseEnter`,
   `mouseLeave`, `mouseUp`, `mouseWithin`.
   The most commonly implemented handler is for the `mouseUp` action.

   Buttons with the style "Check Box" and "Radio Button"
   do not typically have an associated script.
   Their state is usually queried by the script of another button.

### Icons

Icons are 32x32 arrangements of black and white pixels
that have a unique ID and an optional name.

To add an icon to a button:

1. Select the Button tool.
1. Double click a button to open its "Button Info" dialog.
1. Click the "Icon..." button.
1. Select an icon from the scrolling list.

   The icons that appear in this list come from "ICON" resources
   in the following files, searched in this order:

   - current stack
   - "Audio Help" stack
   - Home stack
   - HyperCard app
   - System (in "System Folder")

   When an icon is selected, its ID, name, and source
   are displayed at the top of this dialog.

1. Click the "OK" button.

To remove the icon from a button:

1. Select the Button tool.
1. Double click a button to open its "Button Info" dialog.
1. Click the "Icon..." button.
1. Click the "None" button.

To create or modify an icon in the current stack:

1. Open an "Icon Editor" dialog in one of these ways:

   1. Select Edit ... Icon... and select the icon to be edited.

      If the stack does not already contain any custom icons,
      an ID will be automatically assigned and no initial pixels will be black.
      If the stack does contain custom icons,
      select one as the starting point.
      Scroll through them using the scrollbar at the bottom, or use
      the keyboard shortcuts cmd-1 (first), cmd-2 (previous),
      cmd-3 (next), and cmd-4 (last).

   1. Select the Button tool, select a button that uses the icon,
      and select Edit ... Icon...

   1. Select the Button tool, double click a button that uses the icon
      to open its "Button Info" dialog, click the "Icon..." button,
      and click the "Edit..." button.

1. If the icon is not defined in the current, a dialog will appear
   asking if you want to make a copy. Click the "OK" button.
1. Optionally modify the name.
1. To create a new icon, change the ID to a unique value.
1. Modify the pixels as desired.

   - To clear all the pixels, select Icon ... Erase.
   - To modify individual pixels, toggling between white and black,
     click them or drag over them.
     Hold down the shift key while dragging to
     constrain movement to be only horizontal or vertical.
   - To drag all the pixels as a group,
     hold down the option key and drag in any direction.
     Also hold down the shift key
     to constrain dragging to horizontal or vertical.
     When the mouse is released,
     any pixels that are dragged out of view are lost.
   - To select a rectangle of pixels,
     hold down the cmd key and drag out a rectangle.
     The selection can be copied (cmd-c), cut (cmd-x), or cloned (option-drag).
   - To copy a 32x32 black and white swatch from anywhere on the screen:

     1. Select Icon ... Pickup.
     1. Drag over the area to copy, noting the updated icon pixels.
     1. Release the mouse button when satisfied.

   - To commit the current state so it cannot be undone by
     selecting Icon ... Revert, select Icon ... Keep.
   - To revert all changes made since the beginning of editing
     or since Icon ... Keep was selected, select Icon ... Revert.

1. Click the "OK" or "Cancel" button.

   If the ID matches that of an existing icon,
   a dialog will prompt whether to replace the existing icon.

To copy an icon from another stack or application (source)
and make it available in your stack (destination):

1. Install the ResEdit application if not already installed.
1. Launch ResEdit.
1. Open the source stack or application in ResEdit.
1. Double-click on "ICON" to view those resources.
1. Click a resource to select it.
1. Press cmd-c to copy it.
1. Open the destination stack in ResEdit.
1. Press cmd-v to paste the copied "ICON" resource.
1. Press cmd-s to save the changes.
1. Press cmd-q to quit ResEdit.

The copied icon will be available when the "Icon..." button
in a "Button Info" dialog is clicked.

When the "Icon Editor" dialog is open, the following new menu items appear.
Some of these menu items can be triggered by Power Keys
even if "Power Keys" is not checked in the Options menu
(which can only be done when a painting tool is selected).

- File menu

  - New Icon: This clears the pixels and changes the ID to a unique value
    in preparation for creating a new icon.
  - Close Icon Editor: This is the same as clicking the "Cancel" button.
  - Duplicate Icon: This changes the ID to a unique value
    in preparation for creating a new icon.

- Edit menu

  The icon-related commands in this menu act on the selected pixels,
  or on all the pixels if none are selected.

  - Undo (cmd-z, esc, backtick, or tilde)
  - Cut Icon (cmd-x)
  - Copy Icon (cmd-c)
  - Paste Picture (cmd-v or cmd-option-v)

    Pastes the icon ID, name, and pixels,
    but only the pixels if cmd-option-v is pressed.

  - Clear Icon (cmd-delete)

    If a rectangle of pixels have been selected,
    the menu item changes to "Clear Picture"
    and selecting it changes all the selected pixels to white.

    If there is no selection,
    this deletes the ICON resource from the current stack.
    Icons from other stacks and the HyperCard app cannot be deleted
    with this menu item.
    WARNING: This cannot be undone!
    Another way to delete ICON resources that works on any file
    is to use ResEdit.

  - New Button

    This creates a new button that uses the icon being edited
    and has "Show Name" unchecked.
    Changes to the icon must still be saved by clicking the "OK" button.

- Icon menu

  - Erase (e): changes all the pixels to white
    regardless of whether any are selected
  - Pickup (cmd-p): enters Pickup mode
  - Keep (cmd-k): saves pixel changes
  - Revert (r): reverts all pixel changes or only those since the last Keep
  - First (cmd-1 or cmd-left arrow): scrolls to the first icon
  - Prev (cmd-2 or left arrow): scrolls to the previous icon
  - Next (cmd-3 or right arrow): scrolls to the next icon
  - Last (cmd-4 or cmd-right arrow): scrolls to the last icon
  - Find (cmd-f): searches for an icon by its complete name or ID

- Special Menu

  - Flip Horizontal (h): flips the icon pixels over a vertical line
    at the center of the icon or the selection
  - Flip Vertical (v): flips the icon pixels over a horizontal line
    at the center of the icon or the selection
  - Frame (f): adds a 1-pixel rectangle around the inside edge
    of the icon or selection
  - Gray (g): changes all black pixels or only those in the selection to white
    if they are in an odd column of an odd row
    or an even column of an even row (checkerboard pattern)
  - Invert (i): toggles all pixels or only those in the selection
    between white and black
  - Mirror Horizontal (m): copies all or selected pixels
    from the left size to the right side
  - Mirror Vertical (x): copies all or the selected pixels
    from the top side to the bottom side
  - Rotate 90° ([ for clockwise or ] for counter-clockwise):
    rotates all of the selected pixels by 90°
    around the center of the icon or the selection
  - Shadow: adds black pixels below and to the right
    of all contiguous sets of black pixels
    to give the appearance of a drop shadow

For stacks that will be distributed for use by others,
add all custom icons to that stack to ensure that the users will have them.

The Apple logo icon is an icn4 and icn8 resource
found in Macintosh HD:System Folder:System.
To copy this icon:

- Select the `System` file.
- Press cmd-d to duplicate it, creating the file `System copy`.
- Launch ResEdit.
- Open the file `System copy`.
- Double click the resource type "icn8".
- Click the resource with id -16386 to select it.
- Press cmd-c to copy it.

### Deleting Icons

The HyperCard app can remove icons from buttons,
but cannot delete icons from the resources of a stack.

To delete icon resources from a stack:

- Launch ResEdit.
- Open the stack file.
- Double-click the "ICON" resource type.
- For each icon to be deleted, select it and press the delete key.
- Press cmd-s to save the changes.
- Press cmd-q to quit ResEdit.

### Finding Buttons

To find all the buttons on the current card when in Browse mode,
press and hold cmd-option.
This adds a dotted outline around all the buttons
that disappears when the keys are released.
This is particularly useful for finding transparent buttons.

Selecting the Button tool from the Tools menu or palette
adds a solid, rectangular outline around all the buttons.

### Editing Buttons

To edit the properties of a button, select the Button tool,
and double click a button.
Alternatively, select a button, and select Objects ... Button Info...
Either way, a "Button Info" dialog like the one shown above will open.
This works regardless of whether the button is in the card or background layer.
It is not necessary to switch to background mode
in order to modify a background button.

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

### Moving Buttons

To move a button within its card:

1. Select the Button tool.
1. Drag the inside or edge of any button.

To constrain the movement to be only horizontal or only vertical,
hold down the shift key while dragging.

The ability to snap-to-grid is missing and would be useful for better layout.

To move a button to another card (possibly in another stack):

1. Select Edit ... Cut Button (cmd-x).
1. Navigate to the destination card.
1. Select Edit ... Paste Button (cmd-v).

To move a button to the opposite domain (background or card),
cut it from its current domain, switch to the other domain, and paste it.

When a button is moved to another stack, it is given a new ID and part number.

### Resizing Buttons

To resize a button:

1. Select the Button tool.
1. Drag any corner of a button.
   It is easiest to grab a corner by clicking just inside it
   rather than trying to click directly on it.

To constrain the resizing to be only horizontal or only vertical,
hold down the shift key while dragging.
If the button style is "Rounded Rectangle", "Standard", or "Default",
dragging with the shift key down changes the height to the Macintosh standard.

### Copying Buttons

To create a copy of a button, option-drag it OR
select Edit ... Copy Button (cmd-c),
optionally navigate to another card (possibly in another stack),
and select Edit ... Paste Button (cmd-v).

If the shift key is held down during option-drag,
the movement of the copy is constrained to be only horizontal or only vertical.
This simplifies creating a perfectly aligned row or column of buttons.

### Deleting Buttons

To delete a button:

1. Select the Button tool.
1. Click a button to select it.
1. Press the delete key, or select Edit ... Cut Button, or press cmd-x.

### Linking Buttons

To configure a button so clicking it navigates to another card or stack:

- Option #1
  1. Click the "Tasks..." button.
  1. In the "Tasks" dialog, select a destination from the radio buttons
     that include "Back", "Home",
     "First Card", "Previous Card", "Next Card", and "Last Card".
- Option #2
  1. Click the "LinkTo..." button.
  1. Navigate to another card.
  1. In the "Link" dialog, click the "This Card" button.

These are alternatives to writing a script.
They write the script for you, adding a `go` command
that is executed when the user clicks the button.

### Button References

Example button references that can be used in scripts include:

- `me` (when handler is attached to the button),
- by name: `button "{name}"`

  The quotes are not required if the name does not
  contain spaces or other special characters.

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

### Clickable Images

To make a section of an image clickable,
add a button that is transparent and has no label.
Unfortunately the clickable area must be rectangular.
Modify the button script to execute HyperTalk commands when clicked.

### Scripting Buttons

There are three ways to view and edit the script for a button:

1. Select the Button tool, double click the button to open
   its "Button Info" dialog, and click the "Script..." button.
1. Select the Button tool, hold down the shift key, and double click the button.
1. Hold down cmd-option and click the button.

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

The "Button Info" dialog for background checkboxes contains
the checkbox "Shared Hilite" which is not present for card checkboxes.
When this is checked, the state of the checkbox is
shared across all cards that use the background.
When this is unchecked, each card that uses the background
has its own state for the checkbox.

### Radio Buttons

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

The "Button Info" dialog for background radio buttons contains
the checkbox "Shared Hilite" which is not present for card radio buttons.
When this is checked, the state of the radio button family is
shared across all cards that use the background.
When this is unchecked, each card that uses the background
has its own state for the radio button family.

### Popups

A button with the style "Popup" displays a dropdown list of options.
To specify the options, click the "Contents..." button
and enter each option on its own line.

The name assigned to the button is used for
a label that appears to the left of the dropdown.
It should end with a colon.
By default, the label is given a width of zero and doesn't appear.
To make it appear, enter a number in "Title Width" input
in the "Button Info" dialog, noting the effect in the "Preview" area.
This input only appears for Popup style buttons.
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
For example, the background of the cards in the provided "Addresses" stack,
shown below, contains "Name" and "Telephone" fields.
Typically each field is preceded by a label
that describes the data that should be entered.
Different values can be entered in background fields for each card.

<img alt="HyperCard Addresses stack" style="width: 60%"
  src="/blog/assets/hypercard-addresses-stack.png?v={{pkg.version}}">

Fields can hold a single line of text or multiple lines.
The choice is specified in the "Field Info" dialog for each field.

For inspiration on fields that can be created,
open the "Home" stack, click "Stack Kit", click "Readymade Fields",
and visit each of the topics.
The fields defined here can be copied into your stacks.

### Creating Fields

To add a field to the current card:

1. Verify that the correct domain, card or background, is selected.

1. Select Objects ... New Field.
   This adds a new field to the center of the current card
   that is sized to hold five lines of text.
   and has its style is set to Rectangle.
   Alternatively, select the Field tool and
   cmd-drag to indicate the location and size of a new field.
   This creates a field whose style is set to Transparent.
   Both approaches create a field with
   a default font of Geneva, 12 points.

1. Double-click the field to open its "Button Info" dialog.

<img alt="HyperCard Field Info" style="width: 50%"
    src="/blog/assets/hypercard-field-info.png?v={{pkg.version}}">

1. Enter a "Field Name" which can be used to
   refer to the field in scripts and reports.
   This can be up to 30 characters and can include
   spaces, numbers, and punctuation, but cannot be all numbers.
1. Select a field style from the following options:

- Transparent: no border and can see through to objects behind it
- Opaque: no border and cannot see through to objects behind it;
  useful to place on top of graphics
- Rectangle: adds a rectangular border; most common style
- Shadow: same as Rectangle, but adds shadows
  on the right and bottom sides for a 3D effect
- Scrolling: adds a rectangular border and
  a vertical scrollbar (even when not needed),
  but never a horizontal scrollbar

1. Select the desired checkbox options which include:

   - Lock Text: makes the content read-only
   - Don't Wrap: prevents words from wrapping when they go past the right edge,
     without a way to scroll the text to see that content
   - Auto Select: automatically selects an entire line in the field
     when any character in the line is clicked,
     making it a list selection component (see Demos ... List Selection below)
   - Multiple Lines:
     This option is only available when "Auto Select" is checked.
     When checked, multiple lines can be selected when the shift key is held down.
     The lines must be contiguous.
     The expression "the selectedText" only returns the first selected line.
     The expression "the selectedLines" returns a string
     that matches the pattern "line {i} to {j} of card field {n}".
   - Wide Margins: adds a top margin of about half character height
     along with left and right margins of about a character width,
     but no bottom margin
   - Fixed Line Height: keeps the same default height for all lines
     regardless of the font sizes used in the lines;
     allows text to overlap vertically which seems bad
   - Show Lines: displays dotted lines that indicate
     where each line of text can be entered
   - Auto Tab: moves focus to the next field when the return key is pressed
     and the text cursor is on the last visible line of a non-scrolling field;
     most useful in a series of single-line fields
   - Don't Search: prevents contents from being searched by `find` commands
   - Shared Text: displays read-only text in a background

     This option is only available for background fields and
     only allows its content to be edited when in background mode.
     This makes it a good alternative to painted (bitmap) text
     that is the same, read-only text which
     appears on each card that uses the background.
     When this option is checked,
     the "Don't Search" option is also required to be checked.

1. Click the "Text Style..." button to open the "Text Properties" dialog.
   This allows specifying the text style, alignment, font, and size.
   Alternatively, specify each of these except alignment
   using the Font and Style menus.

1. Optionally click the "Script..." button
   to open a dialog where a script can be added or modified.

   Enter code that defines action handlers and functions.
   Actions related to fields include
   `closeField`, `exitField`, `keyDown`, `keyUp`, `mouseDown`, `mouseEnter`,
   `mouseLeave`, `mouseUp`, `mouseWithin`, and `openField`.
   It is not common to implement handlers for any of these actions.

1. Click the Browse tool (hand with pointing finger).
1. Click the field.
1. Enter text.
1. For read-only text, open the "Field Info" dialog again
   and check the "Lock Text" checkbox.

### Finding Fields

One way to find fields on the current card when in Browse mode
is to move the cursor around the card and notice when the cursor changes
from the browser cursor (hand with pointing index finger) to an I-beam.

A better way to find fields when in Browse mode
is to press and hold cmd-shift-option.
This adds a dotted outline around all the fields
that disappears when the keys are released.
This is particularly useful for finding transparent fields.

Selecting the Field tool from the Tools menu or palette
adds a solid, rectangular outline around all the fields.

### Editing Fields

To edit the properties of a field, select the Field tool,
and double click a field.
Alternatively, select a field, and select Objects ... Field Info...
Either way, a "Field Info" dialog like the one shown above will open.
This works regardless of whether the field is in the card or background layer.
It is not necessary to switch to background mode
in order to modify a background field.

To change the default text style of a field:

- Select one of the following options from the Style dropdown:
  - Transparent - no border and background is transparent,
    showing what is behind it
  - Opaque - no border and background is opaque,
    hiding what is behind it
  - Rectangle - rectangle border with no shadow
  - Shadow - rectangle border with a shadow to lower-right
  - Scrolling - adds a vertical scrollbar;
    text wraps by default and there is never a horizontal scrollbar
- Click the "Text Style..." button to open the "Text Properties" dialog.
- Select any of the style checkboxes.
- Select an alignment option (Left, Center, or Right).
- Select a font name.

  Commonly available fonts include Chicago, Courier,
  Geneva, Helvetica, Monaco, Palatino, New York, and Times.
  If a font other than these is selected and
  users of the stack do not have the chosen font,
  a substitute font will be selected automatically
  and the cards may not render as expected.
  This is not an issue for painted text because
  the fonts used are not required by others that browse the stack.

- Select a font size.
- Select a line height which is the distance between text base lines.
  Typically this is 4 points more than the font size,
  but 2 points more is a good value for closely spaced lines.
  TODO: Changing this seems to have no effect!
- Click the OK button.

### Text Style of Selected Text

To change the text style of selected text
in a single field on the current card to something
different than the default text style for the field:

- Select the text by dragging over it or
  press cmd-shift-a to select all the text.
- Select a font name from the Font menu.
- Select a font style from the Style menu.
- Select a font size from the Style menu.

The following keyboard shortcuts modify the style of the selected field:

| Keyboard Shortcut | Style Change                          |
| ----------------- | ------------------------------------- |
| cmd-shift-p       | plain                                 |
| cmd-shift-b       | bold                                  |
| cmd-shift-i       | italic                                |
| cmd-shift-u       | underline                             |
| cmd-shift-o       | outline                               |
| cmd-shift-s       | shadow                                |
| cmd-shift-c       | condense (less space between letters) |
| cmd-shift-x       | extend (more space between letters)   |
| cmd-shift-g       | create a group from selected text     |
| cmd-shift-<       | decrease font size                    |
| cmd-shift->       | increase font size                    |
| cmd-shift-[       | next font                             |
| cmd-shift-]       | previous font                         |
| cmd-shift-d       | return to defaults                    |

The text style does not affect the ability to
search for matching text using the `find` command.

### Moving Fields

To move a field within its card:

1. Select the Field tool.
1. Drag the inside or edge of any field.

To constrain the movement to be only horizontal or only vertical,
hold down the shift key while dragging.

The ability to snap-to-grid is missing and would be useful for better layout.

To move a field, to another card (possibly in another stack):

1. Select Edit ... Cut Field (cmd-x).
1. Navigate to the destination card.
1. Select Edit ... Paste Field (cmd-v).

To move a field to the opposite domain (background or card),
cut it from its current domain, switch to the other domain, and paste it.
To retain text already entered in the field,
hold down the shift key while pasting.

When a field is moved to another stack, it is given a new ID and part number.

### Resizing Fields

To resize a field:

- Select the Field tool.
- Drag any corner of a field.
  It is easiest to grab a corner by clicking just inside it
  rather than trying to click directly on it.

To constrain the resizing to be only horizontal or only vertical,
hold down the shift key while dragging.

### Copying Fields

To create a copy of a field, option-drag it OR
select Edit ... Copy Field (cmd-c),
optionally navigate to another card (possibly in another stack),
and select Edit ... Paste Field (cmd-v).

If the shift key is held down during option-drag,
the movement of the copy is constrained to be only horizontal or only vertical.
This simplifies creating a perfectly aligned row or column of fields.

Field text that is copied retains its text style.
To paste the text without its text style,
instead using the text style of the character before where it is pasted,
hold down the shift key when selecting Edit ... Paste or press cmd-shift-v.

### Deleting Fields

To delete a field:

- Click the Field tool.
- Click an object to select it.
- Press the delete key, or select Edit ... Cut Field, or press cmd-x.

### Field Focus

To move the focus to the next field in the current card,
defined by their part numbers, press the tab key.
After the last field on the card is reached, pressing the tab key
moves the focus to the first field on the card.

### Field References

Example field references include:

- `me` (when handler is attached to the field),
- by name: `field "{name}"`

  The quotes are not required if the name does not
  contain spaces or other special characters.

- by id: `field id {id}`
- by number: `field {number-within-card}`.
- `the target` (when handler is attached the parent card)

The keyword `field` can be abbreviated to `fld`.

If the field is on the card instead of the background,
add `card` or `cd` before `field`.
If the field is on the background instead of the card,
optionally add `background`, `bkgnd`, or `bg` before `field`.
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

### Scripting Fields

There are three ways to view and edit the script for a field:

1. Select the Field tool, double click the field to open
   its "Field Info" dialog, and click the "Script..." button.
1. Select the Field tool, hold down the shift key, and double click the field.
1. Hold down cmd-option-shift and click the field.

### Field Groups

TODO: Learn about the "Group" text style which can be
added to a group of words in a locked field.
Select words and press cmd-shift-g to create a group.
Scripts can listen for clicks on a group to take action
such as treating the group as a hyperlink.
This is covered in Volume 2 of "The Complete HyperCard 2.2 Handbook".

### Reverse Lettering

To create white text on a black background:

1. Select the Rectangle tool from the Tools palette.
1. Drag out a rectangle that is at least
   as large as the anticipated size of the text.
1. Press the tab key to open the Patterns palette.
1. Select the solid black pattern.
   If none of the patterns are solid black, double click the one
   closest to solid black to open its "Edit Pattern" dialog,
   click all the white pixels to turn them black, and click the "OK" button.
1. Select the Bucket tool.
1. Click inside the rectangle to fill it with black.
1. Click the Field tool.
1. Hold down the cmd key and drag out a
   transparent field on top of the black rectangle.
1. Press cmd-t to open the "Text Properties" dialog for the field.
1. Check the "Outline" checkbox.
1. Select a font that works well with the Outline style like "Chicago".
1. Select the desired font size.
1. Check the "OK" button.
1. Select the Browse tool.
1. Enter the desired text in the field.
1. Double-click the field to open its Info dialog.
1. Check the "Lock Text" checkbox.
1. Check the "OK" button.
1. If the black rectangle is too large,
   select the excess on the right side with the Selection tool
   and press the delete key.
1. Repeat the previous step for excess on the bottom side.

## Layers

The HyperCard application uses the following layers, from bottom to top:

- Document layer - displays stack windows which display cards
- Palette layer - displays the Tools, Patterns, and Nav palettes
- Dialog layer - displays all dialogs including those for
  Stack Info, Background Info, Card Info, Button Info, Field Info,
  `answer` command, and `ask` command.
- Menu layer - displays the menu items in each of the menus
- Cursor layer - displays the mouse cursor

The elements of a card consist of
painted objects (e.g. polygon), buttons, and fields.
Each element is displayed in its own layer
within the "document layer".

To set the visibility of all painted object on the current card or background,
use the following command:

```text
set showPict of this {background|card} to {true|false}`.
```

All painted objects appear below buttons and fields.
So buttons and fields are never obscured by painted objects.

The order in which elements are created determines their initial stacking order,
with the newer elements on top of older elements.

The stacking order of buttons and fields with their domain can be changed.
To do so:

- Select the Button or Field tool.
- Selecting the button or field whose order will be changed.
- Select Objects ... Bring Closer (cmd-plus) to increase the part number.
- Select Objects ... Send Farther (cmd-minus) to decrease the part number.

To move an object to the extreme end of the stacking order,
hold down the shift key when issuing the command.

For fields, the stacking order affects the order in which
they are visited when the tab key is pressed.
If the focus is in a background field, repeatedly pressing the tab key
visits each of the remaining background fields that are
higher the stacking order before visiting each of the card fields.
If the focus is in a card field, repeatedly pressing the tab key
visits each of the remaining card fields that are
higher the stacking order before visiting each of the background fields.
It is not possible to set a stacking order that
alternates between background and foreground fields.

When the mouse is clicked, only one element receives the message ...
the element that overlaps the click location and is in the closest layer.

## Sounds

To play a predefined sound, use the `beep` or `play` commands.
The `beep` command takes an optional argument
that specifies the number of times to play, defaulting to one.
The `play` command takes the following arguments:

- name of a sound, either `boing` or `harpsichord`
- optional keyword `tempo` followed by a number (seems to default to 120)
- optional set of notes to play in double quotes

For example, `play harpsichord tempo 120 "c4 e g4 e c5 q"`

Sound resources can be copied from other files such as:

- the `System` file in the directory `System Folder`
- application files (e.g. cow in
  HyperCard 2.4:Your Tour of HyperCard:HyperCard Tour)
- a HyperCard stack (e.g. "Ghost Laugh" in
  Documents:HyperCard Stacks:Haunted House:The Haunted House 1.0.2)

To copy a sound response (type "snd") from another file:

1. Install the ResEdit application if not already installed.
1. Launch ResEdit.
1. Open the source stack or application in ResEdit.
1. Double-click on "snd" to view its sound resources.
1. Click a resource to select it.
1. Press cmd-c to copy it.
1. Open the destination stack in ResEdit.
1. Press cmd-v to paste the copied "snd" resource.
1. Press cmd-s to save the changes.
1. Press cmd-q to quit ResEdit.

The copied sound can now be played
with the command `play "{snd-resource-name}"`.
In a handler, consider using the command `play stop`
before this command to stop any sound that is already playing
before playing another.

To play a sound file, use the `play file {sound-file-path}` command.
Supposedly the AIFF sound format is the most reliable.
TODO: I could not get this to work!

To record new sounds, see the "Sounds" section in the "iMac G3" blog page.

## Importing Graphics

Graphics can be imported from files that use the MacPaint format,
such as the MacPaint application.

To create graphics in MacPaint and import them into a HyperCard stack:

1. Launch MacPaint.
1. Create graphics using its painting tools
   which are nearly identical to those in HyperCard.
1. Save the graphics in a file.
1. Launch HyperCard.
1. Open the target stack.
1. Choose the target domain (background or card).
1. Choose the specific target background or card.
1. Select any painting tool so the following menu item is available.
1. Select File ... Import Paint...
1. Select the file created earlier.
1. Select the pasted graphics and move them to the desired location.
1. Optionally edited the imported graphics.

## Exporting Graphics

The painting layer of the current card or background
can be exported to a file that uses the MacPaint format.
This includes buttons (along with their icons)
and fields (including text entered into them).

To export the current painting layer:

1. Navigate to the card or background whose graphics will be exported.
1. Select any painting tool so the following menu item is available.
1. Select File ... Export Paint...
1. Select the target directory.
1. Enter a file name.
1. Click the "Save" button.

It seems that the right edge is cut off.

## Backgrounds

Every stack has at least one background, but it can be empty.
Often backgrounds have a design that mimics something in real life
such as an address book, calendar, notebook, or paper form.
They contain buttons and fields that
appear on every card that uses the background.

To edit the properties of the background of the current card,
select Objects ... Bkgnd Info... which opens the following dialog:

<img alt="HyperCard Background Info" style="width: 45%"
  src="/blog/assets/hypercard-background-info.png?v={{pkg.version}}">

Assigning a name to a background is optional,
but they are handy when printing reports and
they enable accessing backgrounds by name in scripts.

To prevent a background from being deleted,
check the "Can't Delete Background" checkbox.
When this is checked, if a user tries to
delete the last card that uses the background,
a dialog will open that says "Can't delete last card of protected background".

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

If some cards that use a particular background
do not wish to display all the elements in the background layer,
those elements can be covered by elements in the card layer.
For example, a rectangle that is filled with white
can be used to hide painted elements, buttons, and fields.

To draw a white rectangle without a black border:

- Select Options ... Draw Filled.
- Select solid white from the Patterns menu or palette.
- Select the Rectangle tool from the Tools menu or palette.
- Hold down the option key while dragging out the rectangle.

To copy a background and create a new background
that starts out with the same configuration,
TODO: ???

To create multiple backgrounds that have some common elements,
the common elements must be copied from one background to the others.

To assign/change the name of a background,
select Options ... Bkgnd Info... and enter/change the "Background Name".

In a stack that uses multiple backgrounds,
button and field names can be duplicated across the backgrounds.
This enables background scripts to use the same code
for referring to those containers.

It seems there is no way to change the background used by an existing card.
The only option seems to be copying the content from the existing card
to a new card that use the desired background.

## Card Transitions

To add a transition effect that occurs on card navigation, add a script
like the following to each button whose handler navigates to a new card.
In the `visual` command, the keyword `effect` is optional.

```text
on mouseUp
  visual effect wipe right slowly
  go to previous card
end mouseUp

on mouseUp
  visual effect wipe left slowly
  go to next card
end mouseUp
```

There are 27 effects and five speeds.
To specify a visual effect that is trigger by a specific button:

1. Select the Button tool.
1. Double-click the button to open its "Button Info" dialog.
1. Click the "Tasks..." button.
1. Select "Visual Effect" in the left list.
1. Select an effect from the center list.
1. Select a speed from the right radio buttons.
1. Click the "Assign Tasks" button.

This adds a `visual effect` command to the `mouseUp` handler.
If one is already present, it is replaced.

Clicking the button will only exercise the visual effect
if the button has also been configured to navigate to another card.
To configure this, open the "Button Info" dialog for the button
and do one of the following:

- Click the "LinkTo..." button and follow the instructions
  for this in the "Creating Buttons" section above.
- Click the "Tasks..." button and follow the instructions
  for this in the "Creating Buttons" section above.
- Click the "Scripts..." button and manually
  add a `go` command after the `visual` command.

The "LinkTo..." and "Tasks..." options
add a `go` command to the `mouseUp` handler.
If one is already present, it is replaced.

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
double click the rectangle in the scroll window or
click the zoom box in the upper-right corner of the stack window.
The rectangle cannot be dragged if its size is the same as the scroll window.

The screenshot below shows the scroll Window
after its rectangle has been resized and dragged.
It also shows the portion of the Home stack that is visible
based on the state of the scroll window.

<img alt="HyperCard Scroll Window" style="width: 70%"
  src="/blog/assets/hypercard-scroll-window.png?v={{pkg.version}}">

## Menu Bar

To hide the menu bar that typically appears at the top of the screen,
use the command `hide menubar`.
To restore the menu bar, use the command `show menubar`.

## Keyboard Shortcuts

| Shortcut        | Action                                                                         |
| --------------- | ------------------------------------------------------------------------------ |
| cmd-h           | Home                                                                           |
| cmd-~           | go to Back to the previously browsed card                                      |
| cmd-1           | go to First card in stack                                                      |
| cmd-2           | go to Prev card in stack                                                       |
| cmd-3           | go to Next card in stack                                                       |
| cmd-4           | go to Last card in stack                                                       |
| left arrow      | go to Prev card in stack                                                       |
| right arrow     | go to Next card in stack                                                       |
| up arrow        | go to First card in current stack or first card in Home stack if already there |
| down arrow      | go to previously visited card in any stack?                                    |
| cmd-left arrow  | go to First card in stack                                                      |
| cmd-right arrow | go to Last card in stack                                                       |
| cmd-b           | toggle Background mode                                                         |
| cmd-c           | copy                                                                           |
| cmd-e           | open Scroll window                                                             |
| cmd-f           | Find text within current stack                                                 |
| cmd-i           | open Icon editor                                                               |
| cmd-m           | open Message box (where commands can be entered)                               |
| cmd-o           | open a stack                                                                   |
| cmd-l           | go to the next open HyperCard window                                           |
| cmd-n           | New card                                                                       |
| cmd-r           | open window of recently visited cards (click one to open it)                   |
| cmd-v           | past                                                                           |
| cmd-x           | cut                                                                            |
| cmd-z           | undo                                                                           |

The "Open Stack" dialog contains:

- "Show Preview" checkbox that causes a thumbnail of the first card
  of the selected stack to be displayed inside the dialog.
- "Open stack in new window" which does what it says when a stack is opened.

The "Home" stack is inside the "HyperCard 2.4" directory
which I placed in the Applications directory.

## HyperTalk

HyperTalk ...

- is the primary scripting language used by HyperCard
- can only be used with HyperCard, not as a standalone programming language
- is used to write message handlers and functions in the scripts
  associated with stacks, backgrounds, cards, buttons, and fields
- has an English-like syntax
- is case-insensitive, even when comparing strings
- is interpreted at run-time, but cached for executing again later
- supports message passing (events) and message handlers
- implements a simple database

The commands (a.k.a. statements) supported by HyperTalk
are documented at {% aTargetBlank
"https://www.hypercard.center/HyperTalkReference#commands",
"HyperTalk Reference" %}.

A good way to learn about HyperTalk is to
examine the scripts in provided stacks such as the Home stack.

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
select Script ... Comment or press cmd-dash.

To uncomment a set of lines, select them and
select Script ... Uncomment or press cmd-equal.

### Data Types

HyperTalk supports the following data types:

- booleans with the literal values `true` (or `1`) and `false` (or `0`)
- numbers with literal values that are either integers or floating point
- strings with literal values delimited by double quotes
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

#### Booleans

TODO: Add more to this section.

#### Numbers

TODO: Add more to this section.

#### Strings

| Desired Result | Expression                          |
| -------------- | ----------------------------------- |
| length         | `the length of {string-expr}`       |
| substring      | `character i to j of {string-expr}` |

TODO: Add more to this section.

### Accessing Fields

A field reference evaluates to the value of the field.
The syntax is:

```text
[background|card] field "{field-name}"
[background|card] field id {id-number}
[background|card] field {sequence-number}
```

TODO: Can you add "in stack {stack-name}" to these?

If neither `background` nor `card` is specified, it defaults to `background`.

The sequence-number option is not recommended because those can change.

Quotes around field names are optional if the field name
doesn't contain special characters such as spaces.
The keyword "background" can be abbreviated to "bkgnd" or "bg".

The `put` command can be used to change the contents of a field.
It has the syntax `put {value} into {field-ref}`.
It the value is a literal string, it must be surrounded by quotes
if it contains special characters such as spaces.

### Accessing Buttons

A button reference evaluates to a button object.
The syntax is:

```text
[background|card] button "{button-name}"
[background|card] button id {id-number}
[background|card] button {sequence-number}
```

TODO: Can you add "in stack {stack-name}" to these?

If neither `background` nor `card` is specified, it defaults to `card`.

The sequence-number option is not recommended because those can change.

It is more common to refer to buttons by their ID than by their name.
The reason is that button names are often used as their displayed labels
and a card can have multiple buttons with the same name.

To change the name of a button:

```text
set name of {button-ref} to {new-name}
```

To click a button from a script:

```text
send mouseUp to {button-ref}
```

### Scripts

A HyperCard script is a collection of message handler and function definitions.
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
at the Home stack and HyperCard app levels
when other users use your stacks,
so it is risky to depend on those.

To open a Script editor window for any kind of object,
open its "Info" window and click the "Script..." button.
The following keyboard shortcuts remove the need to open an "Info" window
and directly open the Script editor for a given object:

| Object Type | Keyboard Shortcut                                  |
| ----------- | -------------------------------------------------- |
| Button      | select Button tool and cmd-option-click the button |
| Field       | select Field tool and cmd-option-click the field   |
| Background  | cmd-option-b                                       |
| Card        | cmd-option-c                                       |
| Stack       | cmd-option-s                                       |

When editing a script, press the tab key to format it
which indents the lines properly.
This uses two-space indentation,
but any indentation (including none) will work.

### Message Handlers

A single script can define any number of message handlers
that each begin with the keyword `on`.
Each message handler listens for a specific kind of message
and executes the code inside when triggered.
Unlike functions, message handlers cannot return a value.

For example, in a stack with two cards where the first card contains a button:

1. Open the stack. The first card will be displayed.
1. Select the Button tool.
1. Double-click the button.
1. Click the "Script" button.
1. Enter the following:

   ```text
   on mouseUp
     go to next card
   end mouseUp
   ```

1. Press cmd-s to save the changes.
1. Press cmd-w to close the script window.

1. Select Objects ... Card Info...
1. Click the "Script" button.
1. Enter the following:

   ```text
   on mouseUp
     beep
   end mouseUp
   ```

1. Select Objects ... Stack Info...
1. Click the "Script" button.
1. Enter the following:

   ```text
   on mouseUp
     flash
   end mouseUp
   ```

1. Select the Browse tool.
1. Click outside the button.
1. Notice that the `beep` command runs because the card script is run.
1. Click the button.
1. Notice that it navigates to the next card because the button script is run.
1. While on the next card, click anywhere inside it
   except on a button or field.
1. Notice that the window flashes because the stack script is executed.
   The `flash` command alternates the color of every pixel on the card
   between white and black two times.

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

Message handlers for custom message names can be implemented in any script.
For example:

```text
on doubleBeep
  beep
  beep
end doubleBeep
```

To send a message, just use its name. For example, `doubleBeep`.
If no handler is found for the message, a dialog that says
"Can't understand {message}." will open.

Message handlers can have parameters that appear
after the message name in a comma-separated list.
For example:

```text
on add n1, n2
  answer "The sum is" && (n1 + n2) & "."
end add
```

To invoke this, use an expression like `add 2, 3`.

### Standard System Messages

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
  - `visual effect`: if the effect can't be completed
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

HyperTalk can store the name or id of a button/field in a variable,
but it cannot store a button or field object in a variable.

### go Command

The `go` command supports going to another card.
It can be followed by the optional preposition "to"
which is omitted in the examples below.
The `go` command supports many arguments described below.

- Ordinal

  - `go first` - 1st card in current stack
  - `go second` - 2nd card in current stack
  - `go third` - 3rd card in current stack
  - `go last` - last card in current stack
  - `go card {n}` - nth card in current stack

- Positional

  - `go next [card]` - next card
  - `go prev[ious] [card]` - previous card
  - `go this [card]` - stay on current card

- Other ways to go to another card in the current stack

  - `go any card` - randomly selects a card?
  - `go bkgnd "{background-name}"

- Other ways to go to another card, possibly in another stack

  - `go back` - previously visited card
  - `go bkgnd "{background-name}" [of stack "{stack-name}"]`
  - `go card id {card-id} [of stack "{stack-name}]`
  - `go card "{card-name}" [of stack "{stack-name}]`
  - `go forth` - opposite of `go back` used after that command
  - `go home` - Home stack
  - `go stack {stack-name} [in [a] new window]` -
    first card in a given stack (e.g. "HyperCard Help")

All the `go` commands above implement "hard links"
that do not depend on other data.

"Soft links" depend on other data.
For example, suppose we have a stack where specific dogs are
described on their own card and the card name is the dog name.
The first card in the stack can contain
a field where a dog name can be entered (named "dogName") and
a button that navigates to the matching card when clicked.

Add the following script to the stack
to define a handler for a custom action
that is triggered by the clicking the button
or by pressing the return key while in the field:

```text
on findDog name
  if there is a card name then
    go to card name
  else
    answer "No matching dog was found."
  end if
end findDog
```

Add the following script to the button:

```text
on mouseUp
  findDog card field dogName
end mouseUp
```

Add the following script to the field so
pressing the return key after ending a dog name
has the same effect as clicking the button:

```text
on returnInField
  findDog me
end keyDown
```

### get Command

The `get` command sets the value of the special variable `it`
to the value of an expression.
The command `get {expression}` is equivalent to `put {expression} into it`.

The following are examples of using the `get` command:

```text
get 2 + 3
get the short date -- e.g. 4/12/25; short is the default and can be omitted
get the long date -- e.g. Saturday, April 12, 2025
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

### Strings

Literal strings are surrounded by double quotes.

The value of `the itemDelimiter` defaults to a comma.

Indexes for characters, words and lines are all one-based.

The following table describes common operations on strings.
In the example scripts, `s`, `s1`, and `s2` are all string references.

| Operation                | Script                                         |
| ------------------------ | ---------------------------------------------- |
| concatenate              | `s1 & s2`                                      |
| concatenate with a space | `s1 && s2`                                     |
| include carriage return  | `s1 & return & s2`                             |
| length in characters     | `[the] length of s`                            |
| length in characters     | `[the] number of char[acter]s in s`            |
| length in characters     | `length(s)`                                    |
| length in words          | `[the] number of words in s`                   |
| character by index       | `char i of s`                                  |
| substring by indexes     | `char i to j of s`                             |
| substring by word        | `word i of s`                                  |
| substring by words       | `word i to j of s`                             |
| substring by line        | `line i of s`                                  |
| substring by lines       | `line i to j of s`                             |
| get the item delimiter   | `the itemDelimiter`                            |
| set item delimiter       | `set the itemDelimiter to "{char}"`            |
| substring by delimiters  | `item i of s`                                  |
| substring by delimiters  | `item i to j of s`                             |
| substring index          | `the offset of s1 in s2` - s1 is substring     |
| substring index          | `offset(s1, s2)` - s1 is substring             |
| convert to number        | `number(s)` - 0 if invalid TODO: Doesn't work! |

The following expression result is `3`:

```text
the number of lines in ("red" & return & "green" & return & "blue")
```

The following script displays the string "green":

```text
set the itemDelimiter to ";"
put "red;green;blue" into s
answer item 2 of s
```

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

The built-in mathematical functions include
`abs`, `annuity`, `atan`, `average`, `cos`, `ln`, `ln1`, `log2`,
`exp`, `exp1`, `exp2`, `ln`, `ln1`, `log2`, `max`, `min`, `random`,
`round`, `sin`, `sqrt`, `sum`, `tan`, and `trunc`

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

### Waiting

To pause for a given amount of time, use the `wait` command.
The syntax is one of the following:

- `wait [for] {n} {tick | ticks | second | seconds}`
- `wait until {boolean-expression}`
- `wait while {boolean-expression}`

A tick is 1/60th of a second.

### Mouse Messages

The following mouse-related messages are automatically triggered:

- `mouseDoubleUp` - double click
- `mouseDown` - pressed but not released
- `mouseEnter` - entered
- `mouseLeave` - exited
- `mouseStillDown` - triggered continuously while over
- `mouseUp` - released
- `mouseWithin` - triggered on every move within

### Functions

Functions can be defined inside scripts.
Like message handlers, they can have parameters.
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
Any number of these can be used inside a function,
including none in functions that only cause side effects.
Commands in a function body that follow
an unconditional `return` are never executed.

Functions are called with the following syntax.
The open and closing parentheses are required,
even if no arguments are being passed.

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

### Built-in Functions

HyperCard provides many built-in functions,
most of which are prefixed with the keyword "the".
Examples of these functions include:

TODO: Finish summarizing what each of these do.

- `the abbreviated date`: e.g. Tue, May 13, 2025

  `abbreviated` can be shortened to `abbrev` or `abbr`.

- `the clickChunk`
- `the clickH`: x coordinate of the last mouse click
- `the clickLine`
- `the clickLoc`: string containing x and y coordinates
  from upper-left of window separated by a comma of the last mouse click
- `the clickText`
- `the clickV`: y coordinate of the last mouse click
- `the commandKey`: `up` or `down`
- `the date`: e.g. 5/13/25
- `the destination`
- `the foundChunk`
- `the foundField`
- `the foundLine`
- `the foundText`
- `the length of {string}`: number of characters in string
- `the length of {field-ref}`: number of characters in field content
- `the long date`: e.g. Tuesday, May 13, 2025
- `the long time`: e.g. 8:20:52 PM
- `me`
- `the menus`: a string containing all the menu names separated by newlines;
  includes the system menus Apple, Help, and Application
- `the mouse`: `up` or `down`
- `the mouseClick`
- `the mouseH`: x coordinate of the current mouse location
- `the mouseLoc`: string containing x and y coordinates
  of the current mouse location
  from upper-left of window separated by a comma of the current mouse location
- `the mouseV`: y coordinate of the current mouse location
- `the number of [card|bkgnd] buttons`
- `the number of [card|bkgnd] fields`
- `the number of backgrounds`
- `the number of cards [of {bkgnd-expr}]`
- `the number of marked cards`: in the current stack;
  To mark a card, open its "Card Info" dialog
  and check the "Card Marked" checkbox.
- `the number of menuItems of {menu-expr}`
- `the number of menus`: includes the system menus Apple, Help, and Application
- `the number of windows`: TODO: result is higher than expected
- `the optionKey`: `up` or `down`
- `the param`
- `the paramCount`
- `the params`
- `the random`
- `the result`
- `the screenRect`
- `the sec[ond]s`: e.g. 3830012502; seconds since ?
- `the selectedButton`
- `the selectedChunk`
- `the selectedField`
- `the selectedLine[s] of [{field-ref}|{button-ref}]`
- `the selectedLoc`
- `the selectedText [of [{field-ref}|{button-ref}]`
- `the shiftKey`: `up` or `down`
- `the sound`
- `the stacks`
- `the sum`
- `the systemVersion`
- `[the] target`: the object that triggered the message such as a specific button
- `the ticks`
- `the time`: e.g. 8:20 PM
- `the tool`: e.g. "browse tool"
- `the value`
- `the version`: of HyperCard; e.g. 2.41
- `the windows`

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

### Icons

To get the id of the icon used by a button,
use the expression `the icon of {button-ref}`.

To change the icon used by as button,
use the command `set the icon of {button-ref} to {icon-id}`.

### Function Keys

The handler "on functionKey {whichKey}"
can be used to act on presses of function keys.
However, the operating system intercepts all the function keys by default
which prevents the handler from being invoked.
To fix this:

1. Open the Keyboard control panel.
1. Click the "Function Keys..." button.
1. Uncheck the "Enable Hot Function Keys" checkbox.
1. Click the "OK" button.
1. Close the control panel.

The handlers registered in the "Stack Info..." script of the Home stack
are active for all stacks.
To register function keys to choose the Browse, Button, and Field tools
in all stacks, even if the Tools palette is not open:

1. Launch HyperCard.
1. Open the Home stack.
1. Select Objects ... Stack Info...
1. Click the "Script..." button.
1. Add the following to the end of the existing script.
   I chose the function keys F9, F10, and F11, but others can be used instead.

   ```text
   on functionKey whichKey
     if whichKey is 9 then choose browse tool
     else if whichKey is 10 then choose button tool
     else if whichKey is 11 then choose field tool
     else pass functionKey
   end functionKey
   ```

### Messages

Message are sent when:

- a system event occurs such as opening a stack,
  opening a card, or clicking a button.
- a script explicitly sends a message with the `send` command
- the user sends a message from the message box

To send a message to the next level up in the object hierarchy,
use the `pass` command.
For example, `pass "messageName [parameterList]"`.

To send a message to another object, use the `send` command.
For example, `send "messageName [parameterList]" to objectReference`.

### Custom Menus

A stack can add custom menus, remove menus,
and remove specific menu items using HyperTalk.
For example, the Home stack adds the Home menu
and the Addresses stack adds the Utilities menu.

To see how this is done:

- Open the Home stack.
- Select Objects ... Stack Info...
- Click the Script... button.
- Examine the handler `createTheMenus`,
  the function `homeMenuItems`, and
  the function `homeMenuMsgs`.

TODO: Learn how to do this in one of your stacks.

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

The user level affects the menu items that are present
and the functionality available to the user.

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

### Card Numbering

To display a card number on each card in a stack:

- Add the following field to each background:

  <img alt="HyperCard card number field" style="width: 60%"
  src="/blog/assets/hypercard-card-number-field.png?v={{pkg.version}}">

- Add the following script to each background:

  ```text
  on openCard
    put the number of this card into field "cardNumber"
  end openCard
  ```

### Experimenting With HyperTalk

<img alt="HyperCard Experimenting" style="width: 60%"
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

- "-" button to decrement the counter
- field to display the counter value
- "+" button to increment the value

<img alt="HyperCard Counter" style="width: 20%"
  src="/blog/assets/hypercard-counter.png?v={{pkg.version}}">

The following screenshot shows the field configuration:

<img alt="HyperCard Counter field" style="width: 60%"
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
Each time the button is clicked, its icon changes to 10 random values,
before settling on a new value.

<img alt="HyperCard Dice Button" style="width: 6%"
  src="/blog/assets/hypercard-dice-button.png?v={{pkg.version}}">

Create the following button with its icon set to any of the dice icons:

<img alt="HyperCard Dice Button Info" style="width: 55%"
  src="/blog/assets/hypercard-dice-button-info.png?v={{pkg.version}}">

Add the following script to the button:

```text
on mouseUp
  repeat 10 times
    -- The dice icon IDs are 2101 to 2106.
    put the icon of me - 2101 into index
    put (index + random(6)) mod 6 into index
    set the icon of me to 2101 + index
    wait for 5 ticks
  end repeat
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

  An alternative approach is to:

  - Omit the script above.
  - Allow `prevButton` and `nextButton` to be visible on all cards.
  - Draw an opaque button over the `prevButton` in the card layer of the first card.
  - Draw an opaque button over the `nextButton` in the card layer of the last card.

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

<img alt="HyperCard Adding Numbers" style="width: 50%"
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

### Number Field

To create a field where only a positive integer can be entered,
add the following script to the field.

```text
function decrementField
  if me &gt; 1 then
    put me - 1 into me
    select after text of me
  else
    beep
  end if
end decrementField

function incrementField
  put me + 1 into me
  select after text of me
end incrementField

function startsWith s, prefix
  return char 1 to length of prefix of s is prefix
end startsWith

-- This assumes that the stack script contains the following line
-- to allow the use of the left and right arrow keys in fields:
-- set the textArrows to true

on keyDown which
  put char 1 of which into firstChar
  put charToNum(firstChar) into asciiCode

  if asciiCode is 8 -- delete key
  then pass keyDown
  else if asciiCode is 9 -- tab key
  then pass keyDown
  else if asciiCode is 28 -- left arrow key
  then pass keyDown
  else if asciiCode is 29 -- right arrow key
  then pass keyDown
  else if asciiCode is 30 -- up arrow key
  then incrementField
  else if asciiCode is 31 -- down arrow key
  then decrementField
  else if firstChar &lt; 0 or firstChar &gt; 9 -- not a digit key
  then beep
  else if me is empty and firstChar = 0 -- cannot start with zero
  then beep
  -- Cannot replace first character with zero.
  else if startsWith(the selectedChunk, "char 1 ") and firstChar is 0
  then beep
  else pass keyDown -- allow digit key
end keyDown
```

### List Selection

This demonstrates using a field to display a list of options.
When the user clicks an option, it is highlighted by
reversing the background and foreground colors of the selected line.
The option text is also copied to another field
to demonstrate that is was captured.

We need a way to remember the previously selected option.
When an option is selected, if it was already selected
then the option is deselected.

We can't associate a value with a container
like a stack, card, button, or field.
We can store a value in a hidden field or in a global variable.
Neither option is ideal, but using a global variable seems best.

There is a non-strict convention where global variables
begin with the letter "g" and are followed by a CamelCase name.
So we will use the global variable `gColorListIndex`.

<img alt="HyperCard List Selection" style="width: 30%"
  src="/blog/assets/hypercard-list-selection.png?v={{pkg.version}}">

Create fields like the following:

<img alt="HyperCard field #1 for list selection" style="width: 49%"
  src="/blog/assets/hypercard-field1-for-list-selection.png?v={{pkg.version}}">
<img alt="HyperCard field #2 for list selection" style="width: 49%"
  src="/blog/assets/hypercard-field2-for-list-selection.png?v={{pkg.version}}">

Note that currently "Lock Text" is not checked in the "colorList" field.
Enter the options in the "colorList" field on separate lines.
Then open the Field Info dialog for the "colorList" field and
check the "Lock Text" checkbox so users cannot modify it.

Add the following script to the "colorList" field:

```text
on mouseUp
  global gCurrentColor
  get the selectedText of me
  if it is gCurrentColor then
    put empty into gColorListIndex
    put empty into card field selectedColor
    -- select empty -- should clear the selection, but doesn't!
    select line 0 of me
  else
    put it into gCurrentColor
    put it into card field selectedColor
  end if
end mouseUp
```

Clicking a line in the first field highlights it
and sets the content of the second field to that line.
Clicking a line that is already highlighted
removes the highlighting and clears the second field.

### Detecting Duplicates in a List

Create a field "Style" set to "Scrolling"
and "Don't Wrap" checked.
Add the following script:

```text
on returnInField
  get me -- saves copy of field contents in "it"
  delete last line of it
  if last line of me is in it then
    answer "Duplicate names cannot be entered."
    select last line of me
  else
    pass returnInField -- allows newline to be added
  end if
end returnInField
```

### Disclosure Buttons

For an example of the script handlers and functions necessary
to implement a button that toggles the display of a field:

1. Open the "Home" stack.
1. Click the "Stack Kit" button.
1. Click "Readymade Buttons".
1. Click "Create pop-up fields".
1. Select the Button tool.
1. Double-click the "Things To Do" or "HyperCard Notes" button.
1. Click the "Script..." button.
1. Study the script

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
- {% aTargetBlank "https://www.youtube.com/watch?v=fFX1otbE_wU",
  "NOTACON 5: Wasn't HyperCard Cool?" %} by Drew Ivan
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
