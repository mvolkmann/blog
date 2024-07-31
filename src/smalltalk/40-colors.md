---
eleventyNavigation:
  key: Colors
  order: 40
  parent: Smalltalk
layout: topic-layout.njk
---

## Color Class

The `Color` class defines many methods for creating and operating on colors.
It defines the following class methods that create colors by name:

- `black`, `blue`, `brown`, `cyan`, `gray`, `green`, `magenta`,
  `orange`, `pink`, `purple`, `red`, `tan`, `white`, `yellow`
- `lightBlue`, `lightBrown`, `lightCyan`, `lightGray`, `lightGreen`, `lightMagenta`,
  `lightOrange`, `lightRed`, `lightYellow`
- `darkGray`, `veryDarkGray`, `veryLightGray`, `veryVeryDarkGray`, `veryVeryLightGray`
- `transparent`

To get a random color, send the `#random` message to the `Color` class.

To set the opacity of a color, send the `#alpha:` message to a `Color` object
with an argument that is a float value between
zero (fully transparent) and one (fully opaque).

## XKCD Colors

Cuis Smalltalk supports many sets of named colors.
One of them is named "XKCD", created by the author of the XKCD comics.
The color names were selected through a
<a href="https://blog.xkcd.com/2010/05/03/color-survey-results/"
target="_blank">Color Survey</a> performed in 2010.

To install this color set, enter `Feature require: 'XKCD-NamedColors'`
in a Workspace and "Do it".

## Color Names Dictionaries

There are many `Dictionary` objects where the keys are `Symbol` color names
and the values are `Color` objects.

To change the colors associated with the instance variables of a `Morph`:

- Open the halo of the `Morph`.
- Click the blue Menu handle.
- Click the red push pin to keep the menu open.
- Click one of the instance variables that hold a `Color` object
  such as `borderColor`, `color`, `fillColor`, or `lineColor`.
- Select a color palette such as
  CSS3 (Web) Colors, Crayon Colors, NBSISCC Colors, XKCD Colors,
  or ColorMix Editor (opens a Color Editor dialog).
- Drag a color swatch from the dialog
  onto the swatch for "borderColor" or "color".
- Close the color swatch dialog.
- Close the `Morph` menu.

The `Color-Extras` package adds the
class method `colorNamesDict` to the `Color` class.
This returns the color `Dictionary` that is considered to be the default.
To open a color palette that uses this `Dictionary`:

- Open the World menu.
- Select "New Morph...".
- Select "User Interface...A - Dro...ColorPalette".
- Move the mouse to where it should be dropped and click to drop it.

## ColorEditorPanel

A `ColorEditorPanel` is a subclass of `DialogPanel`
that is used to select a color from a cube of colors.
In menus this is referred to as the "ColorMix Editor".

<img alt="ColorEditorPanel"
  src="/blog/assets/cuis-color-editor-panel.png?v={{pkg.version}}"
  style="width: 70%">

The vertical slider in the middle specifies the value of
a single color aspect such as green.
The square on the left specifies the value of
the other two color aspects such as red and blue.
You can think of the vertical slider in the middle
as specifying the z-coordinate within the cube and
the square on the left as specifying
the x and y coordinates within the cube.
For more detail, see the `ColorPaneMorph` class comment.

To install this:

- Clone the `Cuis-Smalltalk-UI` GitHub repository
  into the same directory as `Cuis-Smalltalk-Dev`.
- Enter `Feature require: 'UI-Color-Panel'` into a Workspace and "Do it".

To open this, open the World menu and select
New Morph... User Interface ... A - Dro ... ColorEditPanel.

To select a color:

- Click a radio button on the right to indicate which aspect of the color
  will be controlled by the vertical slider in the middle.
  The choices are Red, Green, Blue, Hue, Saturation, and Brightness.
- Drag the slider in the middle to select a value for
  the color aspect that corresponds to the selected radio button.
- Click a point in the square on the left.
- Drag the slider in the lower-left to select the
  level of transparency in the color.
- Optionally modify the numbers in the text inputs on the right.

The view the color aspect values of a color swatch
from one of the color palettes (such as Crayon Colors):

- Open a `ColorEditorPanel`.
- Open the default color palette.

  - Open the World menu.
  - Select New Morph... User Interface ... A - Dro ... ColorPalette.
  - Move the mouse cursor to where you would like to place it.
  - Click to drop it.

- Hover over a color swatch in the color palette to see its name in a tooltip.
- Drag a color swatch from the color palette to the
  first or second swatch in the `ColorEditorPanel`.
- Optionally modify the color within the `ColorEditorPanel`.

To get additional menu options in the menu displayed by
opening a `Morph` halo and clicking the blue Menu handle,
enter `Feature require: 'UI-MetaProperties` in a Workspace and "Do it".
The menu items with purple text all have special editors
that are opened by clicking the menu item.
To change the color associated with
any purple menu item that displays a color swatch,
drag a color swatch from any color palette onto it and drop it.

For example:

- Create a `Morph` whose colors will be modified.

  For example, open the World menu, select "New Morph...",
  and select Vector Graphics ... A - Rec ... CircleShapeMorph.

- Open the halo of a `Morph`.
- Click the blue Menu button near the top-left.
- In the menu that appears, click the red pin
  so the menu remains open if you click outside it.
- Click the menu item whose color is to be changed.
- Select one of the available color palettes such as "Crayon Colors"
  or "ColorMixEditor".
- Drag one of the color swatches in the palette or ColorMixEditor to
  one of the menu items for an instance variable such as
  `borderColor`, `color`, `fillColor`, or `lineColor`.

  Tooltips for each of the three color swatches
  in the ColorMix Editor describe their purpose.
  The left swatch uses the selected color with the selected alpha value.
  The center swatch uses the selected color without the selected alpha value.
  The right swatch uses the closest web color to the selected color.

- Close the menu.
