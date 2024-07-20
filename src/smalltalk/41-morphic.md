---
eleventyNavigation:
  key: Morphic
  order: 41
  parent: Smalltalk
layout: topic-layout.njk
---

Morphic is a GUI framework that is included into popular Smalltalk images
such as Squeak, Pharo, and Cuis.
It defines user interfaces with "morphs" which are
what other graphical systems refer to as widgets or components.
Morphs are graphical items that can be added to
the `WorldMorph` (desktop) or a `LayoutMorph`.
TODO: Technically speaking, can any morph be embedded in any other morph?

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

Some of the methods in Morphic classes are inconsistently named.
For example, the class `TextModelMorph` defines the methods
`alwaysHideVerticalScrollbar` and `hideScrollBarsIndefinitely`.
Note how the "b" is sometimes lowercase and sometimes uppercase.

Morphic uses the term "extent" to describe the width and height of a morph.
It is represented by a `Point` object with
an `x` instance variable that holds the width and
a `y` instance variable that holds the height.

The location of a morph is represented by a `MorphicTranslation` object
that has the instance properties `deltaX` and `deltaY`.

## Creating and Modifying Morphs

To create a morph:

- Open the World menu.
- Select "New morph...".
- In the dialog that appears, select a category of morphs
  and then a specific kind (ex. `BoxedMorph`).
- The morph will appear attached to the cursor
  (uses the `Morph` method `openInHand`).
- Move the cursor to the location where the morph should be placed
  and click to drop it.

A morph can be modified even after the code that defines it has been deleted,
because its definition is part of the running image.
TODO: Is this actually true?

To modify an existing morph:

- cmd-click a morph to open its halo.
- Click the blue menu button on the top and select "copy to clipboard".
- Open a Workspace window.
- Assign the morph to a variable.
  For example, enter `morph := ` and press cmd-p to paste the reference.
- Press cmd-d (Do it).
- Send messages to the morph to modify it.
  For example, `morph borderColor: Color red`

To change the colors used by a morph from its halo:

- cmd-click a morph to open its halo.
- Click the blue menu button on the top and select "copy to clipboard".
- Select "change color..."
- A window titled "Click-Select a Color" will appear attached to the cursor.
- Position the window by moving the cursor and click to drop it.
- Click a color swatch.
- A menu will appear with the options "Cancel",
  "adoptWidgetsColor:", "color:", and "borderColor:".
  Select an option to change that color property of the morph.
  Some morphs don't support all the options.
  For example, "color" of a `LabelMorph` can be changed,
  but it does not have a border.
  TODO: What does "adoptWidgetsColor:" do?

To explicitly set the size of a morph, send it the `morphExtent:` message
with a `Point` value that represents the new width and height.
For example, `morph morphExtent: 200@100`.

Only a small set of morphs are provided by default.
A good source of additional morphs is the package "UI-Tools".

TODO: Do you need to clone the https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-UI repository for this?

To install this, open a Workspace, enter `Feature require: 'UI-Tools'`,
and press cmd-d (Do it).
TODO: The UI-Tools package is being removed.
This installs the following other packages
that can also be installed individually:

- Collections-CompactArrays
- Compression
- CSS3
- Graphics-Files-Additional
- UI-Click-Select
- UI-Color-Panel
- UI-Components
- UI-Core
- UI-DragAndDrop
- UI-Edit-Lens
- UI-Entry
- UI-Layout-Panel
- UI-MetaProperties
- UI-Palette
- UI-Panel
- UI-Shapes
- UI-Widgets

The set of "Basic" morphs will now include `BoxedMorph`, `EllipseMorph`,
`FrameMorph`, `ImageMorph`, `LabelMorph`, `LineMorph`, `PointerLineMorph`,
`Sonogram`, and `TileResizeMorph`.
TODO: Why does selecting `Sonogram` lock up the image, requiring a Force Quit?

TODO: Supposedly Cuis 7 will remove support for the UI-Tools package
and the desired subpackages will need to be installed individually.
Still true?

See my Morphic demos in package `Volkmann`
in the classes `MorphicDemos` and `MorphicGreet`.
In a Workspace, enter `MorphicDemos incDecButtons.` and "Do it".
Then enter `MorphicGreet new.` and "Do it".

## Halo

A morph halo is a set of circle buttons,
referred to as "handles", that surround a morph.
Each handle changes the morph in some way or reveals information about it.
Hover over a handle to see a tooltip that describes its purpose.

To open the halo for a morph,
cmd-click it in Cuis or option-click it in Squeak.
If the item is embedded in other morphs, repeat this multiple times
until a halo appears around the desired morph.

The class of the morph will be displayed at the bottom of the morph.

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

A morph can also be dragged directly without
opening its halo and using the drag buttons.
This only works if the area that is dragged
does not process mouse events.
For example, you cannot drag a morph that contains a button
by dragging the button.

TODO: How can you change the point about which a morph rotates?

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

(2) If the morph is embedded in another morph,
this changes the owner to world, which unembeds it.

(3) It seems most of the help tooltips default to "Help not yet supplied".
To edit the help text, click the orange circle on the right,
select "edit balloon help", and modify the help text.

(4) To change the center of rotation of a morph, click the "Explore" handle,
enter the following in the bottom pane, and "Do it":

```smalltalk
self setProperty: #rotationCenter toValue: newX @ newY.
```

(5) To restore a collapsed item, click it's thumbnail in the bottom bar.

(6) "Explore" windows enable viewing data associated with an item
such as its location, extent (width and height), and color.
Send messages to `self` in the bottom pane to modify the morph.
To add a method to the mo

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-Inspectors.html"
target="_blank">A brief introduction to Inspectors</a>.

## Desktop Color

One way to change the desktop color is to:

- Right-click the desktop to open its halo.
- Click the blue menu button.
- Select debug...inspect morph to open an Inspect window.
- Click in the bottom pane of the Inspect window.
- Enter `self color: Color red` or use some other color name.
- "Do it".

This works with all colors except `transparent` and alpha values are ignored.

## Embedding

To embed a morph into another (such as a LayoutMorph) so
they are treated as a single unit and can be positioned together:

- Drag a morph on top of its intended parent morph.
- Open the halo of the morph.
- Click the blue circle on the top.
- Select "embed into" ... {parent morph name}.
  The parent morph name is typically "LayoutMorph".

## LayoutMorph

A `LayoutMorph` arranges submorphs in a row or column.
Pratically any layout can be achieved by nesting instances of this class.

An instance can be created with:

- `LayoutMorph newColumn`
- `LayoutMorph newRow`
- `LayoutMorph new`

  This calls `newRow` and sets the background color to `Color red alpha: 0.2`.

For example, `myLayout := Layout newRow`.

To add a submorph to a `LayoutMorph`, send it the `#addMorph:` message.
For example, `myLayout addMorph: EllipseMorph new`
and `myLayout addMorph: BoxedMorph new`.

By default, there will be no separation between the submorphs.
To add separation, send the `#separationX:`, `#separationY`,
or `#separation:` (both x and y) messages.
For example, `myLayout separation: 20`.

By default, all the submorphs will be
pushed to the left of a row or top of a column.
To change this, send the `#axisEdgeWeight:` message with a number from 0 to 1.
A value zero pushes to the left/top,
a value one pushes to the right/bottom,
and a value of 0.5 centers.

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

Many chararacteristics of a morph can be edited by
opening its halo and clicking its blue menu button.
To get a halo for a submorph, cmd-click repeatedly
until the halo appears around the target morph.
The following menu will appear:

<img alt="Cuis halo blue menu" style="width: 60%"
  src="/blog/assets/cuis-halo-blue-menu.png?v={{pkg.version}}">

Click the push pin at the upper-right of the menu
to keep the menu open, which simplifies making multiple changes.

To change the border width, size (`morphExtent`), or position (`morphPosition`)
of a morph:

- Click `borderWidth`, `morphExtent`, or `morphPosition`.
- Modify the numbers in the dialog that appears.
- Click the "Accept" to save the changes or the "Cancel" button to discard them.

To change the border color or color of a morph:

- Click "borderColor" or "color".
- Select one of the following color sets
  to open a dialog containing color swatches:
  CSS3 (Web) Colors, Crayon Colors, NBSISCC Colors, XKCD COlors,
  or ColorMix Editor (opens a Color Editor dialog).
  A Color Editor dialog enables selecting a color
  with RGB, HSL, and transparency.
- Drag a color swatch from the dialog
  onto the swatch for "borderColor" or "color".
- Close the color swatch dialog.
- Close the morph options menu.

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

## Creating a Custom Morph

Custom morphs are typically implemented as subclasses of the `PlacedMorph` class
and implement the `drawOn:` method.
They can be directly dragged to new locations.
Otherwise dragging requires opening the morph halo and using the Move handle.

The following example includes the instance variable `extent`
to allow the width and height to be used to determine what to draw.

```smalltalk
PlacedMorph subclass: #CanvasDemo
    instanceVariableNames: 'extent'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

The instance method `drawOn:` is passed a `VectorCanvas` object
and uses local coordinates for drawing, not world coordinates.
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
    aCanvas strokeWidth: 10 color: Color red fillColor: Color green do: [
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
- Move the cursor to where the morph should be dropped.
- Click to drop it.

Instances of `PlacedMorph` subclasses have a `location` instance variable.
If the morph only has a location and has not be rotated or scaled
then `location` will hold a `MorphicTranslation` object
with `deltaX` and `deltaY` instance variables.
If the morph has been rotated or scaled
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

Changing the translation, rotation, or scale of a morph
changes its local coordinate system.

The `Morph` method `openInHand` causes the morph to appear
and be attached to the cursor.
Move the cursor to the location where it should be placed and click to drop it.

Alternative, send the message `#openInWorld` to cause the morph to appear
and not be attached to the cursor.
If the location of the morph was specified by sending the
`#location#` message to it with a `MorphicTranslation` argument
then it will be placed at that location.
Otherwise it will be placed at a random location.

## BoxedMorph

The `BoxedMorph` class is a subclass of the `PlacedMorph` class
that adds the instance variables `extent` (width and height),
`color`, `borderWidth`, and `borderColor`.
It is intended for morphs that are rectangular.

TODO: The class comment says "DON'T subclass from here." Why?

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

## Redrawing

After making code changes, if the UI does not update properly,
there are two things that can be done to update the display.

1. Open the World menu and select "Restore Display".
1. Open the World menu and select "Debug ... Start drawing all again".

TODO: What is the difference between these?
