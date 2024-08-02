---
eleventyNavigation:
  key: Morphic
  order: 41
  parent: Smalltalk
layout: topic-layout.njk
---

Morphic is a GUI framework that is included into popular Smalltalk images
such as Squeak, Pharo, and Cuis.
It defines user interfaces with "morphs" which are objects that
know how to display themselves in the Smalltalk environment.
They are what other graphical systems refer to as widgets or components.

For a great introduction to Morphic, see
<a href="https://www.youtube.com/watch?v=62baNn3c56Y"
target="_blank">Holistic computing with Smalltalk and Morphic. Part 1</a>.

<a href="http://www.jvuletich.org/Morphic3/Morphic3-201006.html"
target="_blank">Morphic 3</a> was developed by Juan Vuletich
and is used in Cuis Smalltalk.
It adds display resolution independence, float coordinates,
modeling of coordinate systems as objects,
alias free rendering based on signal processing theory,
use of textures and photos,
zoomable user interfaces (not tied to pixel density), and vector graphics.

## Morphs

Any `Morph` be embedded in another.
However, some morphs (such as `WorldMorph` and `LayoutMorph`)
are more intended to have submorphs than others.

Each `Morph` holds a reference to its parent `Morph`
in its instance variable `owner`.
Each `Morph` stores its children (embedded) morphs
in its `Array` instance variable `submorphs`.

Morphic uses the term "extent" to describe the width and height of a `Morph`.
It is represented by a `Point` object with
an `x` instance variable that holds the width and
a `y` instance variable that holds the height.

The location of a `Morph` is represented by a `MorphicTranslation` object
that has the instance properties `deltaX` and `deltaY`.

Each `Morph` can store additional properties in its `properties` instance variable
which holds a reference to a `Dictionary` of key/value pairs.

## Creating and Modifying Morphs

Only a small set of morphs are provided by default.
The package UI-MetaProperties is a good source of additional morphs.
To install it:

- Clone the https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-UI repository
  into the same directory as the Cuis-Smalltalk-Dev repository.
- Open a Workspace.
- Enter `Feature require: 'UI-MetaProperties'` and "Do it".

  This installs the following other packages
  that can also be installed individually:

  - Complex
  - UI-Color-Panel
  - UI-Components
  - UI-Layout-Panel
  - UI-Packager
  - UI-Shapes

- The UI-Compoents package installed above
  installs the following other packages:

  - UI-Click-Select
  - UI-Core
  - UI-DragAndDrop
  - UI-Entry
  - UI-Panel
  - UI-Widgets

The set of "Basic" morphs will now include `BoxedMorph`, `EllipseMorph`,
`FrameMorph`, `ImageMorph`, `LabelMorph`, `LineMorph`, `PointerLineMorph`,
`Sonogram`, and `TileResizeMorph`.
TODO: Why does selecting `Sonogram` lock up the image, requiring a Force Quit?

To create a `Morph`:

- Open the World menu.
- Select "New morph...".
- In the dialog that appears, select a category of morphs
  and then a specific kind (ex. `BoxedMorph`).
- The `Morph` will appear attached to the cursor
  (uses the `Morph` method `openInHand`).
- Move the cursor to the location where the `Morph` should be placed
  and click to drop it.

To modify an existing `Morph`:

- cmd-click a `Morph` to open its halo.
- Click the orange Explore handle to open an Explorer OR
  Click the orange Debug handle and select "inspect morph" to open an Inspector.
- Send a message to `self` in the bottom pane of the Explorer or the Inspector
  to change a property of the `Morph`. For example, `self color: Color red`.

Alternatively, to modify a `Morph` from a Workspace:

- cmd-click a `Morph` to open its halo.
- Click the blue Menu handle on the top and select "copy to clipboard".
- Open a Workspace window.
- Press cmd-v to paste the `Morph` reference from the clipboard.
- Send messages to the `Morph` reference to modify it.

To change the colors used by a `Morph` from its halo:

- Ensure that the "UI-Color-Panel" package is installed
  (a dependency of the "UI-MetaProperties" package).
- cmd-click a `Morph` to open its halo.
- Click the blue menu button on the top and select "change color...".
- A window titled "Click-Select a Color" will appear attached to the cursor.
- Position the window by moving the cursor and click to drop it.
- Click a color swatch.
- A menu will appear with the options "Cancel",
  "adoptWidgetsColor:", "color:", and "borderColor:".
  Select an option to change that color property of the `Morph`.
  Some morphs don't support all the options.
  For example, "color" of a `LabelMorph` can be changed,
  but it does not have a border.
  TODO: What does "adoptWidgetsColor:" do?

To explicitly set the size of a `Morph`, send it the `morphExtent:` message
with a `Point` value that represents the new width and height.
For example, `morph morphExtent: 200@100`.

See my Morphic demos in package `Volkmann`
in the classes `VButtonDemo` and `VGreet`.
In a Workspace, enter `VButtonDemo new` or `VGreet new` and "Do it".

## Halo

A `Morph` halo is a set of circle buttons,
referred to as "handles", that surround a `Morph`.
Each handle changes the `Morph` in some way or reveals information about it.
Hover over a handle to see a tooltip that describes its purpose.

To open the halo for a `Morph`,
cmd-click it in Cuis or option-click it in Squeak.
If the item is embedded in other morphs, repeat this multiple times
until a halo appears around the desired `Morph`.

The class of the `Morph` will be displayed at the bottom of the `Morph`.

<img alt="Smalltalk halo" style="width: 50%"
  src="/blog/assets/smalltalk-halo.png?v={{pkg.version}}">

The following buttons are provided:

| Button                               | Location    | Tooltip      | Purpose                                         |
| ------------------------------------ | ----------- | ------------ | ----------------------------------------------- |
| red circle with white "x"            | upper-left  | Remove       | removes the item                                |
| blue circle with white document      | top         | Menu         | opens menu "A" (1)                              |
| black circle with house              | top         | Pick up      | drag to move the item within its parent (2)     |
| brown circle with resize icon        | top         | Move         | drag to move the item out of its parent         |
| green circle with copy icon          | upper-right | Duplicate    | drag to position a duplicate of the item        |
| orange circle with wrench            | right side  | Debug        | opens a menu of debugging options               |
| blue circle with magnifier glass     | right side  | Change Scale | drag to change scale of item                    |
| yellow circle with resize icon       | lower-right | Change Size  | drag to resize the item                         |
| light blue circle with question mark | bottom      | Help         | click and hold to display a related tooltip (3) |
| blue circle with rotate icon         | lower-left  | Rotate       | drag to rotate item (4)                         |
| dull yellow circle with odd shape    | left side   | Collapse     | click to collapse (hide) the item (5)           |
| orange circle with wrench            | left side   | Explore      | opens an "Explore" window (6)                   |

A `Morph` can also be dragged directly without
opening its halo and using the drag buttons.
This only works if the area that is dragged
does not process mouse events.
For example, you cannot drag a `Morph` that contains a button
by dragging the button.

(1) This menu contains the following options:

- send to back
- bring to front
- embed into >
- change color...
- halo actions...
- checkbox for "resist being picked up"
- checkbox for "be locked"
- copy to clipboard
- export...
- debug...

It is often useful to click the red pin in the upper-right of the menu
so it remains open if you click outside it.
This enables dragging values onto it (such as color swatches)
to change the values of instance variables.
If you forget the `Morph` to which a pinned menu belongs,
click "show target halo" to open the halo of the `Morph`.

To change the value of an instance variable in the menu, such as
`borderColor`, click it and use the provided editor to change its value.
When changing a color, select a color palette and
drag a color swatch onto the instance variable.

<img alt="Crayon Colors palette"
  src="/blog/assets/cuis-crayon-colors-palette.png?v={{pkg.version}}"
  style="width: 100%">

Color swatches can be dragged from one menu to another to copy colors.
Fonts can also be dragged from one menu to another to copy fonts.

(2) If the `Morph` is embedded in another `Morph`,
this changes the owner to world, which unembeds it.

(3) It seems most of the help tooltips default to "Help not yet supplied".
To edit the help text, click the orange circle on the right,
select "edit balloon help", and modify the help text.

(4) To change the center of rotation of a `Morph`, click the "Explore" handle,
enter the following in the bottom pane, and "Do it":

```smalltalk
self setProperty: #rotationCenter toValue: newX @ newY.
```

(5) To restore a collapsed item, click it's thumbnail in the bottom bar.

(6) "Explore" windows enable viewing data associated with an item
such as its location, extent (width and height), and color.
Send messages to `self` in the bottom pane to modify the `Morph`.
To add a method to the mo

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-Inspectors.html"
target="_blank">A brief introduction to Inspectors</a>.

## Desktop Color

One way to change the desktop color is to:

- Right-click the desktop to open its halo.
- Click the blue menu button.
- Select "debug...inspect morph" to open an Inspect window.
- Click in the bottom pane of the Inspect window.
- Enter `self color: Color red` or use some other color name.
- "Do it".

This works with all colors except `transparent` and alpha values are ignored.

## Embedding

To embed a `Morph` into another (such as a LayoutMorph) so
they are treated as a single unit and can be positioned together:

- Drag a `Morph` on top of its intended parent `Morph`.
- Open the halo of the `Morph`.
- Click the blue Menu handle.
- Hover over "embed into" to display a popup containing
  the names of every `Morph` under the one being embedded.
  Hover over each option to highlight the corresponding `Morph`,
  which helps to verify the correct selection.
- Click the name of the `Morph` that will become the parent.
  Often the intended parent `Morph` is a "LayoutMorph".

To embed a `Morph` in another using code send the message `#addMorph:`.
For example:

```smalltalk
lm := LayoutMorph new.
b1 := BoxedMorph new.
b2 := EllipseMorph new.
lm addMorph: b1.
lm addMorph: b2.
lm openInWorld.
lm morphPosition: 200 @ 200. "relative to upper-left corner of World"
```

## LayoutMorph

A `LayoutMorph` actively manages the position and size of its submorphs
based on the following instance properties:

- `direction`: `#horizontal` for a row or `#vertical` for a column

- `separation`: a `Measure`

  By default, there will be no separation between the submorphs.
  To add separation, send the `#separationX:`, `#separationY`,
  or `#separation:` (both x and y) messages.
  For example, `myLayout separation: 20`.

- `axisEdgeWeight`: a number from 0 to 1

  By default, all the submorphs will be
  pushed to the left of a row or top of a column.
  To change this, send the `#axisEdgeWeight:` message with a number from 0 to 1.
  A value zero pushes to the left/top,
  a value one pushes to the right/bottom,
  and a value of 0.5 centers.

- `layoutSpec`: an optional `LayoutSpec` object that specifies
  how to layout children on the off axis (opposite of `direction`)
  using instance variables like `offAxisEdgeWeight`

Practically any layout can be achieved by nesting instances of this class.

An instance can be created with:

- `LayoutMorph newColumn`
- `LayoutMorph newRow`
- `LayoutMorph new`

  This calls `newRow` and sets the background color to `Color red alpha: 0.2`.

For example, `myLayout := Layout newRow`.

To add a submorph to a `LayoutMorph`, send it the `#addMorph:` message.
For example, `myLayout addMorph: EllipseMorph new`
and `myLayout addMorph: BoxedMorph new`.

If the UI-Layout-Panel package is installed,
all of these values can be specified interactively.

- Open the halo for a `LayoutMorph`.
- Click the blue menu button.
- Select "edit me (a LayoutMorph)". The dialog below will appear.
- Click the red push pin to enable trying multiple changes.
- After each set of changes, click the "Update" button.
- When satisfied withthe changes, close the dialog.

<img alt="Cuis Morphic Layout dialog" style="width: 75%"
  src="/blog/assets/cuis-morphic-layout-dialog.png?v={{pkg.version}}">

## Editing Characteristics

Many chararacteristics of a `Morph` can be edited by
opening its halo and clicking its blue menu button.
To get a halo for a submorph, cmd-click repeatedly
until the halo appears around the target `Morph`.
The following menu will appear:

<img alt="Cuis halo blue menu" style="width: 60%"
  src="/blog/assets/cuis-halo-blue-menu.png?v={{pkg.version}}">

Click the push pin at the upper-right of the menu
to keep the menu open, which simplifies making multiple changes.

To change the border width, size (`morphExtent`), or position (`morphPosition`)
of a `Morph`:

- Click `borderWidth`, `morphExtent`, or `morphPosition`.
- Modify the numbers in the dialog that appears.
- Click the "Accept" to save the changes or the "Cancel" button to discard them.

To change the border color or color of a `Morph`:

- Click "borderColor" or "color".
- Select one of the following color sets
  to open a dialog containing color swatches:
  CSS3 (Web) Colors, Crayon Colors, NBSISCC Colors, XKCD Colors,
  or ColorMix Editor (opens a Color Editor dialog).
  A Color Editor dialog enables selecting a color
  with RGB, HSL, and transparency.
  For more detail on these color dictionaries, see
  <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Packages/Features/NamedColors/README.md"
  target="_blank">Cuis-NamedColors</a>.
- Drag a color swatch from the dialog
  onto the swatch for "borderColor" or "color".
- Close the color swatch dialog.
- Close the `Morph` menu.

To edit the width, height, and off-axis edge weight of a submorph

- Open the halo for a submorph.
- Click the blue menu button.
- Select "edit my LayoutSpec". The dialog below will appear.
- Click the red push pin to enable trying multiple changes.
- After each set of changes, click the "Update" button.
- When satisfied withthe changes, close the dialog.

<img alt="Cuis edit my LayoutSpec" style="width: 80%"
  src="/blog/assets/cuis-edit-my-layoutspec.png?v={{pkg.version}}">

For more detail on layouts, see
<a href="https://github.com/Cuis-Smalltalk/Learning-Cuis/blob/master/LayoutTour.md"
target="_blank">Exploring morph layouts in Cuis</a>.

## Coordinate Systems

The world coordinate system places (0, 0) in
the upper-left corner of the main window.
X values increase going right and Y values increase going down.

Each `Morph` has its own local coordinate system
with (0, 0) in the upper-left corner of the `Morph`.
This coordinate system is used for drawing the `Morph`.

Changing the translation, rotation, or scale of a `Morph`
changes its local coordinate system.

Positive rotations are clockwise and negative rotations are counter-clockwise.

## Creating a Custom Morph

Custom morphs are typically implemented as subclasses of the `PlacedMorph` class
and implement the `drawOn:` method.
They can be directly dragged to new locations.
Otherwise dragging requires opening the `Morph` halo and using the Move handle.

The following example includes the instance variable `extent`
to allow the width and height to be used to determine what to draw.

```smalltalk
PlacedMorph subclass: #CanvasDemo
    instanceVariableNames: 'extent fillColor'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

The instance method `drawOn:` is passed a `VectorCanvas` object.
For example, the following draws a green rectangle with a red border
and a red line from its upper-left to lower-right.
It has a default width of 100, height of 100, and
default location of the upper-left corner of the world.

```smalltalk
drawOn: aCanvas
    | x1 x2 y1 y2 |
    x1 := 0.
    y1 := 0.
    x2 := extent x.
    y2 := extent y.
    aCanvas
        strokeWidth: 10
        color: Color red
        fillColor: fillColor
        do: [
        aCanvas
            moveTo: x1 @ y1;
            lineTo: x2 @ y2;
            lineTo: x2 @ y1;
            lineTo: x1 @ y1;
            lineTo: x1 @ y2;
            lineTo: x2 @ y2
    ]

extent
    ^ extent

extent: aPoint
    extent := aPoint

initialize
    super initialize.
    extent := 100@100 "default size"
    "Place in upper-left corner by default."
    self location: (MorphicTranslation withTranslation: 0@0).
```

To try this:

- Open a Workspace.
- Enter `(CanvasDemo new extent: 300@400) openInHand.`
- Press cmd-d to "Do it".
- Move the cursor to where the `Morph` should be dropped.
- Click to drop it.

`VectorCanvas` is a subclass of `AbstractVectorCanvas`,
which is a subclass of `MorphicCanvas`.
The `MorphicCanvas` class defines several methods
whose names begin with `drawString:` method.

To draw text in this `Morph` that is centered,
add the following in the `drawOn:` method:

```smalltalk
font := FontFamily defaultFamilyPointSize: 24.
aCanvas
    drawString: 'Hello'
    atCenterXBaselineY: x1 + x2 / 2 @ (y1 + y2 / 2)
    font: font "pass nil to use default font"
    color: Color yellow.
```

Instances of `PlacedMorph` subclasses have a `location` instance variable.
If the `Morph` only has a location and has not be rotated or scaled
then `location` will hold a `MorphicTranslation` object
with `deltaX` and `deltaY` instance variables.
If the `Morph` has been rotated or scaled
then `location` will hold an `AffineTranslation` object
with `scale`, `degrees`, and `translation` instance variables.

By default, morphs rotate about their center. To change this,
override the `rotationCenter` method to return a different `Point`.
For example, the following causes rotation
to occur around the upper-left corner:

```smalltalk
rotationCenter
    ^ 0@0
```

The `Morph` method `openInHand` causes the `Morph` to appear
and be attached to the cursor.
Move the cursor to the location where it should be placed and click to drop it.

Alternative, send the message `#openInWorld` to cause the `Morph` to appear
and not be attached to the cursor.
If the location of the `Morph` was specified by sending the
`#location#` message to it with a `MorphicTranslation` argument
then it will be placed at that location.
Otherwise it will be placed at a random location.

Drawing-related methods like `drawOn:`
should be placed in the "drawing" method category.

## Mouse Events

To handle mouse clicks on a custom morph,
add instance methods like the following to a `Morph` subclass.
On each click, this changes the `fillColor` instance variable
to a random color and signals that the `Morph` needs to be redrawn.

```smalltalk
handlesMouseDown
    ^ true

mouseButton1Up: aMouseEvent localPosition: aPosition
    fillColor := Color random.
    self redrawNeeded.
```

An alternative to adding instance methods is to set properties as follows,
perhaps in the instance method `initialize`.

```smalltalk
self
    setProperty: #handlesMouseDown
    toValue: true.
self
    setProperty: #mouseButton1Up:localPosition:
    toValue: [:event :position |
        fillColor := Color random.
        self redrawNeeded
    ].
```

Using a receiver other that `self` for the message sends above enables
configuring event handling on a specific instance of a `Morph` subclass
rather than for all instances.

## Keyboard Events

To handle keyboard events on a custom morph,
add instance methods like the following to a `Morph` subclass.

```smalltalk
handlesKeyboard
    ^ self visible.

mouseEnter: event
    (Preferences at: #focusFollowsMouse)
        ifTrue: [ event hand newKeyboardFocus: self ].

mouseLeave: event
    (Preferences at: #focusFollowsMouse)
        ifTrue: [ event hand releaseKeyboardFocus: self ].

keyboardFocusChange: aBoolean
    ('has keyboard focus? {1}' format: {aBoolean}) print.

keyStroke: aKeyboardEvent
    | char |
    char := Character codePoint: aKeyboardEvent keyValue.
    char logAs: 'character'. "defined in Getting Started"
    aKeyboardEvent isArrowUp ifTrue: [ 'got up arrow' print ].
```

## MorphicCanvas

The `MorphicCanvas` class has many subclasses including
`BitBltBoundsFinderCanvas`, `BitBltCanvas`, `BoundsFinderCanvas`,
`HybridCanvas`, and `VectorCanvas`.

`BitBltCanvas` is a legacy class that doesn't support
vector graphics operations, anti-aliasing, or zooming.
But it provides great performance due to its simplicity.

TODO: Is it worthwhile to learn about any of these besides `VectorCanvas`?

## BoxedMorph

The `BoxedMorph` class is a subclass of the `PlacedMorph` class
that adds the instance variables `extent` (width and height),
`color`, `borderWidth`, and `borderColor`.
It is intended for morphs that are rectangular.

TODO: The class comment says "DON'T subclass from here." Why?

All the developer tool windows are subclasses of `BoxedMorph`.
For example, System Browsers are implemented by the `BrowserWindow` class
which has the following inheritance hierarchy.

- `Object`
  - `Morph`
    - `PlacedMorph`
      - `BoxedMorph`
        - `PluggableMorph`
          - `SystemWindow`
            - `CodeWindow`
              - `BrowserWindow`

## Fonts

To see a popup list of the installed fonts and fonts available to install,
enter `FontFamily promptUser` in a Workspace and "Do it".
The installed font names are displayed using their font.
To install a font, click its checkbox under "Available to install".
The list may include the following:

- Alex Brush
- Amaranth
- CMU Typewriter Text
- DejaVu Sans (default)
- JetBrains Mono NL
- Kiwi Maru Light
- Kurinto Sans
- Learning Curve
- Noto Sans EqyptHiero
- Parc Place Legacy
- Source Sans 3

The font files are located in the `Cuis-Smallltalk-Dev/TrueTypeFonts` directory.
To add a new font, create a subdirectory whose name is the font name
and place `.ttf` files for each variation of the font inside it.

To set the default font size used in the development environment:

```smalltalk
Preferences at: #defaultFontSize put: 14
```

To set the default font family used in the development environment:

```smalltalk
PreferenceSet setDefaultFont: 'Alex Brush'
```

If an invalid font name is used, the environment will lock up.
TODO: Report this!

To get a `Dictionary` of available font families:

```smalltalk
fontFamilies := FontFamily availableFamilies
```

To get the default font:

```smalltalk
font := FontFamily defaultFamilyAndPointSize
```

To get the default font with a specified point size:

```smalltalk
font := FontFamily defaultFamilyPointSize: 18
```

To get a font for a specific family and point size:

```smalltalk
font := FontFamily familyName: 'Alex Brush' pointSize: 36.
```

## Button Labels

Button labels are automatically shortened to fit within the button width
using the `String` instance method `squeezeTo:`.
It begins by removing spaces from right to left.
It then removes vowels from right to left.
Finally, it removes consonants from right to left
and adds an ellipsis at the end.

## Button Issue

The class `PluggableButtonMorph` uses the value of `Theme current buttonLabel`
as the color for the label on all instances.
But sending `#color:` to an instance changes its background color.
Depending the background color selected, this can result in poor contrast.
It also uses an embossed font for the label.

A way to fix this, suggested by Mariano Montone,
is to create a subclass of `PluggableButtonMorph` as follows:

1. Define the following new class:

   ```smalltalk
   PluggableButtonMorph subclass: #VButtonMorph
       instanceVariableNames: 'labelColor'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Volkmann'
   ```

1. Define the following instance methods in the `VButtonMorph` class:

   ```smalltalk
   labelColor
       ^labelColor ifNil: [Theme current buttonLabel]

   labelColor: anObject
       labelColor := anObject
   ```

1. Override `VButtonMorph` instance method `drawEmbossedLabelOn:`
   by copying the same method from `PluggableButtonMorph`
   and modifying two lines, the one that sets `colorForLabel`
   and the one that sets `embossed`.

   ```smalltalk
   drawEmbossedLabelOn: aCanvas
       | availableW center colorForLabel f l labelMargin targetSize w x y |
       label ifNotNil: [
           "The next line was modified."
           colorForLabel := self enableLabelColorWith: self labelColor.
           self isPressed
               ifFalse: [
                   self mouseIsOver
                       ifFalse: [ colorForLabel := colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
               ifTrue: [ colorForLabel := colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
           f := self fontToUse.
           center := extent // 2.
           labelMargin := 3.
           w := f widthOfString: label.
           availableW := extent x - labelMargin - labelMargin.
           availableW >= w
               ifTrue: [ l := label ]
               ifFalse: [
                   x := labelMargin.
                   targetSize := label size * availableW // w.
                   l := label squeezedTo: targetSize.
                   (f widthOfString: l) > availableW ifTrue: [
                       targetSize := targetSize - 1.
                       l := label squeezedTo: targetSize ]
               ].

           w := f widthOfString: l.
           x := center x - (w // 2).
           y := center y - (f lineSpacing // 2).
           aCanvas
               drawString: l
               at: x@y
               font: f
               color: colorForLabel
               "The next line was modified."
               embossed: false
       ]
   ```

## Button Demo in Cuis

Add this code in a Workspace, select it all, and "Do it".
It uses the class `VButtonMorph` defined above.

```smalltalk
label := LabelMorph new
  contents: '0';
  color: Color white.
decBtn := VButtonMorph new
  color: Color yellow;
  label: 'Decrement';
  labelColor: Color red;
  model: [ label contents: (label contents asNumber - 1) asString ];
  action: #value.
incBtn := VButtonMorph new
  color: Color yellow;
  label: 'Increment';
  labelColor: Color green;
  model: [ label contents: (label contents asNumber + 1) asString ];
  action: #value.
layout := LayoutMorph new
  addMorph: decBtn;
  addMorph: label;
  addMorph: incBtn;
  "color: Color transparent ;"
  separation: 10;
  location: (MorphicTranslation withTranslation: 70@70);
   rotateBy: 15 degreesToRadians;
  scale: 1.5;
  openInWorld.

"Add horizontal padding in buttons."
decBtn morphWidth: (incBtn morphWidth + 20).
incBtn morphWidth: (incBtn morphWidth + 20).
```

To delete this from the World,
enter layoutDelete in the Workspaces and "Do it", or
open the halo for the `LayoutMorph` and click the red button in the upper-left.

## SVG

To work with SVG images:

- Clone the following Git repositories from Cuis Smalltalk:

  - <a href="https://github.com/Cuis-Smalltalk/Numerics.git" target="_blank">Numerics</a>
  - <a href="https://github.com/Cuis-Smalltalk/SVG.git" target="_blank">SVG</a>

- Open a Workspace and install the packages `LinearAlgebra` and `SVG`.

  For each package, enter `Feature require: '{package-name}'` and "Do it".

- Enter `SVGMainMorph exampleTiger openInWorld` and "Do it".

There are many more example class methods in the `SVGMainMorph` class.
To open all the SVG examples,
enter `SVGMainMorph openAllExamples SVGMainMorph` and "Do it".
To delete all those morphs, enter
`SVGMainMorph allInstancesDo: [ :obj | obj delete ]`.

Another way to open all the SVG examples is to use reflection as follows:

```smalltalk
selectors := SVGMainMorph class allMethodsInCategory: #examples.
selectors do: [ :selector |
    (selector beginsWith: 'example') ifTrue: [
        (SVGMainMorph perform: selector) openInWorld
    ]
].
```

## Full Screen Buttons

You can add buttons to the World that simplify toggling full screen mode.
The following class defines a class method `buttons` that displays buttons
that activate and deactivate full screen mode.

<img alt="Cuis full screen buttons" class="logo" style="width: 20%"
  src="/blog/assets/cuis-full-screen-buttons.png?v={{pkg.version}}">

```smalltalk
Object subclass: #VFullScreenButtons
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'

fullScreen: aBoolean
    Display fullScreenMode: aBoolean.
    Display newDepth: Display depth

fullScreenOff
    self fullScreen: false

fullScreenOn
    self fullScreen: true

initialize
    "renders buttons for setting full screen on or off"

    super initialize.
    LayoutMorph newColumn
        addMorph: (LabelMorph contents: 'Full Screen');
        addMorph: (LayoutMorph newRow
            addMorph: (
                VButtonMorph label: 'On' block: [ self fullScreenOn ]
            );
            addMorph: (
                VButtonMorph label: 'Off' block: [ self fullScreenOff ]
            );
            color: Color transparent
        );
        location: (MorphicTranslation withTranslation: 10@10);
        openInWorld
```

## Morph Methods

The `Morph` class provides a large number of instance methods.
Highlights are provided in the following table:

| Method     | Description                                                                |
| ---------- | -------------------------------------------------------------------------- |
| `collides` | answers `Boolean` indicating if receiver contour overlaps argument contour |

TODO: Add more methods to this table.

## PopUpMenu

The class `PopUpMenu` provides an easy way to render a dialog that
displays information, asks the user for confirmation,
or ask the user to select an option.
It is similar to the JavaScript DOM functions `alert` and `confirm`.

For example:

```smalltalk
PopUpMenu inform: 'Hello, World!'.

likesIceCream := PopUpMenu confirm: 'Do you like ice cream?'.
likesIceCream print. "prints true or false"

likesIceCream := PopUpMenu
    confirm: 'Do you like ice cream?'
    trueChoice: 'Love it!'
    falseChoice: 'Not for me'.
likesIceCream print.

color := PopUpMenu withCaption: 'Choose a color.' chooseFrom: #('red' 'green' 'blue').
color print. "prints 1, 2, or 3"
```

## Event Handling

The `Morph` class provides many methods for event handling
in the "events" method category. Examples include:

- `keyDown:`
- `keyStroke:`
- `keyUp:`
- `keyboardFocusChange:`
- `keyboardShortcut:`
- `mouseButton1Down:localPosition:`
- `mouseButton1Up:localPosition:`
- `mouseButton2Down:localPosition:`
- `mouseButton2Up:localPosition:`
- `mouseButton3Down:localPosition:`
- `mouseButton3Up:localPosition:`
- `mouseEnter:`
- `mouseHover:localPosition:`
- `mouseLeave:`
- `mouseMove:localPosition:`
- `mouseScroll:localPosition:`
- `windowEvent:`

The keyboard and mouse event handling methods are only called
if the `Morph` is configured to handle them.
For example, the `keyDown:`, `keyUp:`, and `keyStroke:` methods are only called
if the `handlesKeyboard:` method is implemented to return `true`.
Also, the `mouseEnter:` and `mouseLeave:` methods are only called
if the `handlesMouseOver:` method is implemented to return `true`.
See comments in each event handling method to determine how to enable it.

TODO: For a good code example, see `VGreet` class in the `Volkmann` category.

Each of these methods is passed a `MorphicEvent` object.
To get the `Morph` object that triggered the event,
send the message `#hand` to the event object.

To move focus to a `Morph`, send it the message
`#newKeyboardFocus:` with the argument `self`.
To give up focus from a `Morph`, send it the message
`#releaseKeyboardFocus:` with the argument `self`.

The `Morph` class provides several methods for focus handling
in the "focus handling" method category. These include:

- `hasKeyboardFocus` - answers a `Boolean`
- `hasMouseFocus` - answers a `Boolean`
- `keyboardFocusChange` - sent to morphs when they gain or lose focus
- `keyboardFocused` TODO: Why does this not have any code?

Consider changing the background color of a custom `Morph`
when it gains and loses focus.

The class `EventSensor` handles keyboard and mouse events.

TODO: Add much more detail on this.

The inheritance hierarchy of classes that describe events is as follows:

- `MorphicEvent`
  - `DropEvent`
  - `DropFilesEvent`
  - `UserInputEvent`
    - `KeyboardEvent`
    - `MouseEvent`
      - `MouseButtonEvent`
      - `MouseMoveEvent`
      - `MouseScrollEvent`
  - `WindowEvent`

To add event handling to a specific `Morph` instances
instead of adding it to the definition of a `Morph` subclass:

- Open the halo of the `Morph`.
- Click the blue Menu button.
- Press cmd-c (copy to clipboard).
- Open a Workspace.
- Press cmd-v (paste) which will insert
  a `Morph` reference like `ellipseMorph2611483`.
- After the `Morph` reference, add code like the following:

  ```smalltalk
  ellipseMorph2611483 setProperty: #handlesMouseDown toValue: true.
  ellipseMorph2611483
      setProperty: #mouseButton1Up:localPosition:
      toValue: [:event :position | self inform: 'got click'].
  ```

  Be careful when using `setProperty:` that the `Symbol` argument value
  is spelled correctly!

- Select those lines of code and "Do it".
- Click the `Morph`.
- Verify that a PopUpMenu appears containing "got click".

To create and configure an `EllipseMorph` in code:

```smalltalk
em := EllipseMorph new openInWorld.
em setProperty: #handlesMouseDown toValue: true.
em
    setProperty: #mouseButton1Up:localPosition:
    toValue: [:event :position | self inform: 'got click'].
```

## Redrawing

After making code changes, if the UI does not update properly,
there are two things that can be done to update the display.

1. Open the World menu and select "Restore Display".
1. Open the World menu and select "Debug ... Start drawing all again".

TODO: What is the difference between these?

## Annoynances

Some of the methods in Morphic classes are inconsistently named.
For example, the class `TextModelMorph` defines the methods
`alwaysHideVerticalScrollbar` and `hideScrollBarsIndefinitely`.
Note how the "b" is sometimes lowercase and sometimes uppercase.

## Todo App

TODO: Implement a Todo app using Morphic and learn how to deploy it.
