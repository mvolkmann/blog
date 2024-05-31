---
eleventyNavigation:
  key: Smalltalk
layout: topic-layout.njk
---

<style>
  .logo {
    --size: 8rem;
    max-height: var(--size);
    max-width: var(--size); 
    margin-left: 2rem;
  }

  .row {
    display: flex;
    align-items: center;
    margin-bottom: 1rem;
  }
</style>

<figure style="width: 30%">
  <img alt="Smalltalk Byte magazine cover" style="border: 0"
    src="/blog/assets/smalltalk-byte-cover.jpg?v={{pkg.version}}">
</figure>

## Overview

<a href="https://en.wikipedia.org/wiki/Smalltalk" target="_blank">Smalltalk</a>
"is a purely object oriented programming language (OOP)."

## Smalltalk Pros

- It has a small, consistently applied syntax.
- It has a great development environment consisting of tools such as
  System Browser, Workspace, Transcript, Debugger, Hierarchy Browser,
  Method Finder and more
- Everything is an object.
- It provides automatic version control.
- It provides extreme polymorphism.
  Any kind of object can be passed to a method as long as it
  responds to the messages that will be sent to it.
- It has a great web app. framework (Seaside) and a great CMS framework (Pier).

## Smalltalk Cons

- It isn't as popular as many other programming languages.

  - Schools generally don't teach it.
  - Few jobs using it are available.
  - IT press doesn't talk about it.
  - It's difficult to convince others to use it.

- It doesn't minimize compile-time errors as much as
  statically typed languages such as C++, C# and Java.
  However, it does do incremental compiling when methods are saved,
  so it finds syntax errors before runtime, unlike most scripting languages.

- All the code for a project is stored in one big image file (often over 30 MB).

- The syntax is fairly different from most programming languages.

  - no dots or parentheses used in method calls
  - conditional and iterating constructs are method calls instead of keywords
  - keyword messages are a departure from positional arguments
  - method cascading (sending multiple messages to the same object) is a new concept

- Performance may be an issue.

- Classes are not in a namespace, so all class names must be unique.

  Using class name prefixes is recommended.
  This is important for using Squeak packages and Monticello.
  Squeak has a prefix registry in the wiki.

## History

- Smalltalk-71

  This was a product of research led by
  Alan Kay at Xerox Palo Alto Research Center (PARC).
  It was created by Alan Kay in just a few days.

- Smalltalk-72

  This version influenced the actor model
  that is used by some modern programming languages.
  From this point on, most of the implementation was provided by Dan Ingalis.

- Smalltalk-72

  This version added most of GUI tools present in current versions of Smalltalk.

- Smalltalk-80

  This version added support for metaclasses
  of everything, including classes, could be treated as an object.
  This was the first version of Smalltalk that was shared outside of PARC.

- ANSI Smalltalk

  This became the standard language reference for Smalltalk in 1998.

- Squeak
- VisualWorks
- ObjectWorks
- ParcPlace Systems for Unix and Sun systems
- Digitalk for Windows and OS/2 systems
- Enfin
- Cincom
- GemTalk
- Etoys for the One Laptop per Child (OLPC) project
- GNU Smalltalk
- Pharo
- Cuis

## Implementations

There are many Smalltalk implementations.
The most popular include:

- <div class="row">
    <a href="https://squeak.org" target="_blank">Squeak</a>
    <img alt="Squeak Smalltalk log" class="logo" style="border: 0"
      src="/blog/assets/squeak-smalltalk-logo.svg?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://pharo.org" target="_blank">Pharo</a>
    <img alt="Pharo Smalltalk log" class="logo" style="border: 0"
      src="/blog/assets/pharo-smalltalk-logo.png?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://cuis.st" target="_blank">Cuis</a>
    <img alt="Cuis Smalltalk log" class="logo" style="border: 0"
      src="/blog/assets/cuis-smalltalk-logo.png?v={{pkg.version}}">
  </div>

## Installing Cuis Smalltalk

Download a zip from from the
<a href="https://github.com/Cuis-Smalltalk/Cuis6-2"
target="_blank">Cuis Smalltalk Git repository</a> and unzip it.

Run the appropriate start script based on your operating system.

- for Windows, open a Command Prompt and run `RunCuisOnWindows.bat`
- for Linux, open a Terminal and run `RunCuisOnLinux.sh`
- for Mac

  1. Double-click CuisVM.app which will fail because the app is not verified.
  1. Open the Settings app.
  1. Select "Privacy & Security".
  1. Scroll down to the "Security" section.
  1. Click the "Allow" button for CuisVM.app.
  1. Double-click a Smalltalk image file such as `CuisImage/Cuis6.2.image`.
  1. The following main app window will open.

     <img alt="Cuis Smalltalk log" class="logo" style="width: 400px"
       src="/blog/assets/cuis-smalltalk-startup.png?v={{pkg.version}}">

## Data Types

Strings are delimited by single quotes, not double quotes.

Literal arrays between with `#(`, end with `)`,
and contain space-separated values.
For example, `#(True 7 'Tami' (Color red))`.

Collection methods include:

| Method          | Description                 |
| --------------- | --------------------------- |
| `collect:`      | like `map` in JavaScript    |
| `detect:`       | like `find` in JavaScript   |
| `select:`       | like `filter` in JavaScript |
| `allSatisfy:`   | like `every` in JavaScript  |
| `anySatisfy:`   | like `some` in JavaScript   |
| `inject: into:` | like `foldl` in Haskell     |

## Getting Started

The main window is a WorldMorph.

To change the font size used in all the windows:

- Click on the WorldMorph background and
  select Preferences...Size of GUI elements...
- Select a point size such as 12.
- Close the dialog by clicking its red circle containing an "x".

To select an item, click it.

To open an context-sensitive menu for an item, right-click it.
After an item is selected from this menu, it will close.
To keep it open so multiple selections can be made,
click its push pin in the upper-right corner.

To print "Hello World!":

1. Click on the WorldMorph background and select Open...Workspace.
1. Enter `Transcript show: 'Hello World!'` in the Workspace window.
1. If no Transcript window is open, open one by
   clicking on the WorldMorph background and selecting Open...Transcript.
1. Right-click inside the Workspace window and select "Do it" or press cmd-d.
1. The output will appear in the Transcript window.
1. To clear the output in the Transcript,
   right-click in it and select "Clear Transcript".

To evaluate an expression in a Workspace
and display the result after it in the workspace:

- Enter an expression in the Workspace window like `3 factorial`.
- Select it by pressing ctrl-a or dragging over it with the mouse.
- Press ctrl-p to print it.
- Remove the output from the Workspace window
  by pressing ctrl-z to undo adding it.

## Blocks

A block is closure (anonymous function) that can have parameters
and contain many statements.
The value of the block is the value of the last statement.

A block of code can be saved in a variable, passed as a parameter, and can be used multiple times.

```smalltalk
myBlock := [:a :b | a + b].
myBlock value: 2 value: 3.
```

For blocks with more than four parameters,
you must pass the values in an array using `#valueWithArguments:`.

Blocks can be passed a arguments to methods.

## System Browser

To open a System Browser window, click on the WorldMorph background,
select Open, and select Browser.

To browse a class, type its name (ex. String) in a Workspace window,
then right-click and select "Browse it" (or press cmd-b).
This opens a System Browser window with the class already selected.

Class categories appear in the first column.
For example, the `String` class is in the `Text` category.

Classes in the selected category appear in the second column.

Protocols (or method categories) of the selected class
appear in the third column.
Example category names include "accessing", "comparing", "copying",
"converting", "enumerating", and "printing".

The top, default protocol is "-- all --" which contains all the methods.
The methods in the selected protocol appear in the fourth column.

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-the-system-Browser.html"
target="_blank">A brief introduction to the System Browser</a>.

## Workspaces

TODO: Describe these.

## Transcripts

TODO: Describe these.

## Text Editors

TODO: Describe these.
How do these differ from Workspaces?

## Message Names

These windows enable searching for implementors
of methods whose name contains a given substring.
For example, enter "select:" to find all the classes
that have a method whose names end with that.
Those include `Bag`, `Collection`, `Dictionary`, `Heap`,
`OrderedCollection`, `SequenceableCollection`, and `SortedCollection`.

## Morphs

Morphs are graphical items that can be added to the WorldMorph or a LayoutMorph.
To create one:

- Click the WorldMorph background.
- Select "New morph...".
- In the dialog that appears,
  select a category of morphs and then a specific kind.

Examples include:

-

## Halo

To open the halo (set of surrounding buttons) for an item,
ctrl-shift-click it (or ctrl-right-click it).

The following buttons are provided:

| Button                               | Location    | Purpose                                         |
| ------------------------------------ | ----------- | ----------------------------------------------- |
| red circle with white "x"            | upper-left  | removes the item                                |
| blue circle with white document      | top         | opens menu "A" (1)                              |
| black circle with house              | top         | drag to move the item within its parent         |
| brown circle with resize icon        | top         | drag to move the item out of its parent         |
| green circle with copy icon          | upper-right | duplicates the item on top of itself            |
| orange circle with wrench            | right side  | opens a menu of debugging options               |
| blue circle with magnifier glass     | right side  | drag to change scale of item                    |
| yellow circle with resize icon       | lower-right | drag to resize the item                         |
| light blue circle with question mark | bottom      | click and hold to display a related tooltip (2) |
| blue circle with rotate icon         | lower-left  | drag to rotate item                             |
| dull yellow circle with odd shape    | left side   | click to collapse (hide) the item (3)           |
| orange circle with wrench            | left side   | opens an "Explore" window (4)                   |

(1) This menu contains the following:

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

(2) It seems most of the help tooltips default to "Help not yet supplied".
To edit the help text, click the orange circle on the right,
select "edit balloon help", and modify the help text.

(3) To restore a collapsed item, click it's thumbnail in the bottom bar.

(4) "Explore" windows enable viewing data associated with an item
such as its location, size (extent), and color.

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-Inspectors.html"
target="_blank">A brief introduction to Inspectors</a>.

To embed a morph into another (such as a LayoutMorph):

- drag the morph on top of its intended parent morph
- open the halo of the morph
- click the blue circle on the top
- select "embed into" ... {parent morph name}
  (typically LayoutMorph)

## Packages

To see all the installed packages, click the WorldMorph background
and select Open...Installed Packages.
This opens an "Installed Packages" window.

To create a new package:

- Click the "New" button in the center strip of buttons.
- Enter a package name.
- Select the newly created package.
- Click the "Save" button.

To determine where packages are saved:

- Open a Workspace.
- Enter `Smalltalk imagePath.`
- Press cmd-p to print the result.

For me this is
`/Users/volkmannm/Documents/dev/lang/smalltalk/Cuis6-2-main/Cuis6.2.image`.
Packages I create go in a similar path which is
`/Users/volkmannm/Documents/dev/lang/smalltalk/Cuis6-2-main-UserFiles/NewPackages/Volkmann.pck.st`

## Example Code

```smalltalk
Transcript show: 'Hello World!'

Smalltalk allClasses size.

label1 := LabelMorph new.
label1 contents: 'red label'.
label1 color: (Color red).

label2 := LabelMorph new.
label2 contents: 'yellow label'.
label2 color: (Color yellow).

layout1 := LayoutMorph new.
"The :: syntax is used to set multiple properties on an object."
layout1 beRow :: borderWidth: 5 :: borderColor: (Color white).
layout1 separation: 30.
layout1 axisEdgeWeight: 0.5. "0.0 for left, 0.5 for center, 1.0 for right"
layout1 addMorph: label1.
layout1 addMorph: label2.
layout1 openInHand.

image1 := ImageMorph new.
layout1 addMorph: image1.

label2 delete.

layout1 beColumn.

array1 := #(True 7 'Tami' (Color red)).
array1 size. "4"
"This prints each item in the array on a separate line in the Transcript window.
The convention is to refer to each item with the parameter name `each`."
array1 do: [:each | Transcript show: each :: newLine].

"Operations on integers that have a non-integer result are reported as ratios (fractions)."
#(89 97 94) mean. "result is 280/3"

"This creates a range of integers from 1 to 5 and computes their sum."
(1 to: 5) sum.

myBlock := [:a :b | a + b].
myBlock value: 2 value: 3.
```
