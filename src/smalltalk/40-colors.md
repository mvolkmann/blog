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

## ColorEditorPanel

A `ColorEditorPanel` is a subclass of `DialogPanel`
that is used to select a color from a cube of colors.

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
from one of the color palettes (such as ?):

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

To enable dropping color swatches
on those for the instance varaibles of a morph,
enter `Feature require: 'UI-MetaProperties` in a Workspace and "Do it".

To use the selected color:

- Create a morph whose colors will be modified.

  For example, open the World menu, select "New Morph...",
  and select Vector Graphics ... A - Rec ... CircleShapeMorph.

- Open the halo of a morph.
- Click the blue Menu button near the top-left.
- In the menu that appears, click the red pin
  so the menu remains open if you click outside it.
- Drag one of the color swatches in the "Color Editor" dialog to
  one of the swatches in the menu for an instance variable such as
  `borderColor`, `color`, `fillColor`, or `lineColor`.

  Tooltips for each of the three color swatches describe their purpose.
  The left swatch uses the selected color with the selected alpha value.
  The center swatch uses the selected color without the selected alpha value.
  The right swatch uses the closest web color to the selected color.

- Close the menu.
