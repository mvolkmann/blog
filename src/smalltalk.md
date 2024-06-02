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
  <img alt="Smalltalk Byte magazine cover"
    src="/blog/assets/smalltalk-byte-cover.jpg?v={{pkg.version}}">
</figure>

## Overview

<a href="https://en.wikipedia.org/wiki/Smalltalk" target="_blank">Smalltalk</a>
"is a purely object oriented programming language (OOP)."
Messages are sent to objects which decide whether and how to act on them.

Smalltalk is a dynamically typed language.
Types of variables, method parameters,
and method return types are never specified.
Instead, duck typing is used. Any object can be used as long as it
is able to respond to all the messages that are sent to it.
This is determined at run-time.

## Resources

- <a href="https://en.wikipedia.org/wiki/Smalltalk"
  target="_blank">Smalltalk in Wikipedia</a>
- <a href="https://cuis.st" target="_blank">Cuis Smalltalk</a>
- <a href="https://pharo.org" target="_blank">Pharo Smalltalk</a>
- <a href="https://squeak.org" target="_blank">Squeak Smalltalk</a>
- <a href="https://www.fast.org.ar"
  target="_blank">Fundación Argentina de Smalltalk</a> (FAST)

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
- Pharo - forked from Squeak with goal to be more comprehensive
- Cuis - forked from Squeak with goal to remain small and easy to learn

Both Pharo and Cuis began as forks of Squeak
after maintenance of Squeak was turned over to the community
and there was a lack of concensus on its future goals.

## Implementations

There are many Smalltalk implementations.
The most popular include:

- <div class="row">
    <a href="https://squeak.org" target="_blank">Squeak</a>
    <img alt="Squeak Smalltalk log" class="logo"
      src="/blog/assets/squeak-smalltalk-logo.svg?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://pharo.org" target="_blank">Pharo</a>
    <img alt="Pharo Smalltalk log" class="logo"
      src="/blog/assets/pharo-smalltalk-logo.png?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://cuis.st" target="_blank">Cuis</a>
    <img alt="Cuis Smalltalk log" class="logo"
      src="/blog/assets/cuis-smalltalk-logo.png?v={{pkg.version}}">
  </div>

  The mascot is southern mountain cavy which is a "tailless rodents with
  short, speckled, greyish-yellow fur, fading to pale grey on the underparts."
  They look similar to a mouse, but grow to around eight inches in length.
  They are found in Argentina.
  Juan Vuletich began development of Cuis Smalltalk in Buenos Aires s, Argentina.
  The work "cuis" means "squeak" in Rioplatense Spanish.

  The GitHub repository for Cuis Smalltalk is at
  <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev"
  target="_blank">Cuis-Smalltalk-Dev</a>.
  As of May 2024, 96.8% of it was implemented in Smalltalk.

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

The app CuisVM.app implements the virtual machine used by Cuis Smalltalk.
It is taken directly from Squeak and does not differ in any way.
All the differences between Cuis and Squeak are implemented in
its base image file found in the `CuisImage` subdirectory
with a name like `Cuis6.2.image` or `Cuis7.1-6367.image`.

## Getting Started

Double-click a Smalltalk image file such as `CuisImage/Cuis6.2.image`.
The following main app window will open:

<img alt="Cuis Smalltalk log" class="logo" style="width: 400px"
  src="/blog/assets/cuis-smalltalk-startup.png?v={{pkg.version}}">

Alternatively you can double-click a VM file such as `CuisVM.app`,
but that will open a dialog that prompts for an image file.

The main window contains a `WorldMorph`.

To change the font size used in all the windows:

- Click on the `WorldMorph` background and
  select Preferences...Size of GUI elements...
- Select a point size such as 14.
- Close the dialog by clicking its red circle containing an "x".

To select an item, click it.

To open an context-sensitive menu for an item, right-click it.
After an item is selected from this menu, it will close.
To keep it open so multiple selections can be made,
click its push pin in the upper-right corner.

To print "Hello World!":

1. Click on the WorldMorph background and select Open...Workspace.
1. Enter `Transcript show: 'Hello World!'` in the Workspace window.
   Alternatively, enter `'Hello World!' print` which works because
   the class `CharacterSequence` which is the superclass of `String`
   has the method `print` which does this: `Transcript show: self; newLine`.
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

## Syntax

| Item                                          | Example                                          |
| --------------------------------------------- | ------------------------------------------------ |
| comment                                       | `"some text"`                                    |
| local variable (private scope)                | `myLocal` (camelCase)                            |
| global variable (shared scope)                | `MyGlobal` (CamelCase)                           |
| pseudo variable (cannot assign)               | `self`                                           |
| integer                                       | `123`                                            |
| float                                         | `3.14`                                           |
| exponential notation                          | `1.23e4`                                         |
| character                                     | `$a`                                             |
| string                                        | `'text'` (double ' to include)                   |
| string and array concatenation (comma)        | `'foo', 'bar', 'baz'` or `#(1 2), #(3 4)`        |
| symbol (globally unique string)               | `#name'`                                         |
| static array (elements known at compile time) | `#(1 4 8)'`                                      |
| dynamic array (elements computed at run time) | `{1. 2 * 2. 2 raisedTo: 3}`                      |
| assignment                                    | `<variable> := <expression>.`                    |
| method variable declarations                  | `\| foo bar baz \|`                              |
| block with no arguments                       | `[ <expressions> ]`                              |
| block with arguments                          | `[:a :b \| a + b]`                               |
| unary message send                            | `<object> <message>`                             |
| binary message send (operators)               | `4 * 5`                                          |
| keyword message send                          | `2 raisedTo: 4 modulo: 3`                        |
| message cascade - sends to initial receiver   | `Transcript show: 'foo'; newLine; show: 'bar'`   |
| message chaining - sends to previous result   | `2 * 3 :: squared :: negated` (-36)              |
| method return value                           | `^ <expression>`                                 |
| expression separator (period)                 | `Transcript show: 'foo'. Transcript show: 'bar'` |
| reference to current object in a method       | `self`                                           |

In static arrays the elements are separated by spaces.

In dynamic arrays the expressions are separated by periods.

TODO: What is a "compound literal"?

### Classes

Classes define sets of associated class variables, instance variables,
class methods, and instance methods.

All classes inherit from one other class,
except `Object` which is the highest superclass of all classes.

TODO: Are class names required to be unique across all packages?

Programming languages use many terms to describe data
that is encapsulated by objects created from a class.
Examples include "attribute", "property", and "field".
Smalltalk calls these "instance variables".

Instance variables can only be directly accessed by methods in the same class.
To expose them outside the class, add getter methods.
For example, if `score` is an instance variable
then the following is a getter method for it.
By convention, the name of the method is the same as
the name of the instance variable, but this is not required.

```smalltalk
score
    ^score
```

To create a new class:

- Open a System Browser.
- Select a category in the top, first pane.
- Click in the empty area of the top, second pane.
- A starting template for a new class definition
  will appear in the bottom pane.
- Change "NameOfSubclass" to the name of the new class.
- If the class needs any instance variables,
  add them to the value of the `instanceVariableNames` string
  separated by spaces.
- If the class needs any class variables,
  add them to the value of the `classVariableNames` string
  separated by spaces.
- To save the changes, right-click in the bottom pane
  and select "Accept" or press cmd-s.

### Methods

All methods are public.
By convention, methods that should only be used by
other methods in the same class are placed in the "private" message category.

To find a method:

- Click the `WorldMorph` back and select Open ... Message Names.
  This will open a "Message names" window.
- Enter part of a message and press the return key.
  This will display a list of matching message names.
- Click one of the message names to see the classes that implement it.
- Click one of the class names and click the "Browse" button
  to open a System Browser that shows the method implementation.

For example, entering "nj" will find the "inject:into:" message
that is implemented by the `Collection` class.

Squeak Smalltalk supports finding methods by part of their name
OR by providing example input and output.
The steps to use this are:

- From the "Tools" menu, select "Method Finder".
  This opens a "Selector Browser" window.
- Enter part of the method name OR
  an example input, followed by a period, and the expected output.
- Press the return key.
- A list of all matching methods will be displayed.
- Click one of the methods to open a System Browser
  that shows the method implementation.

To add a method to a class:

- Open a System Browser.
- Select the category of the class in the top, first pane.
- Select the class in the top, second pane.
- Click the message category in which the new method will be placed,
  or select "as yet unclassified" in the top, third pane.
- A starting template for a new method definition
  will appear in the bottom pane.
- Change "messageSelectorAndArgumentNames"
  to the name of the new method.
- Modify the comment describing the method.
- Update the list of temporary variable names or delete that line.
- Replace "statements" with the method implementation.
- If the method was not associated with a method category ...
  - If the top, third pane does not contain a suitable message category ...
    - Right-click in that pane and select "new category",
      or click in the top, third pane and press cmd-n.
    - Select a category to add.
    - If none of the provided categories are suitable, select "new..."
      and enter the name of a category to be added.
      For consistency, try to stick with the provided category names.
  - Click "as yet unclassified" in the top, third pane.
  - Drag the name of the new method from the top, fourth pane
    to its method category to associate it.

To sort the message category names alphabetically,
right-click in the top, third pane and select "alphabetize".

To remove a method in the System Browser,
right-click it and select "remove method"
or select it and press cmd-x.

The following method can be added to the `Collection` class
to enable computing the sum of the numbers in any collection.
This method is present by default in Squeak, but not in Cuis.

```smalltalk
sum
    "answers the sum of the numbers in the collection"
    ^self inject: 0 into: [:acc :n | acc + n]
```

Superclasses can define methods that subclasses must implement.
For example, a class named `VShape` can define the following method:

```smalltalk
area
    "answers the area of the shape"
    self subclassResponsibility
```

A class named `VCircle` can be defined as a subclass of `VShape`.
If it does not defined the `area` method
and that message is sent to an instance,
an Error dialog with the title "My subclass should have overridden #area"
will appear.

To add the missing method, click the "Create" button,
select a message category for the method,
enter its implemenation, press cmd-s to save, and
press the "Proceed" button to continue running the code
at the point of the failed message send.

### File Out and In

To save all the code for a package to a text file:

- Open a System Browser.
- Select the package in the top, first pane.
- Right-click and select "fileOut".

The file will be saved in
`{distribution-name}-UserFiles/FileOuts/{package-name}.st`.

To read all the code for a package from a text file:

- Open a File List.
- Locate and select a `.st` file created by a "fileOut".
- Right-click and select "fileIn".
- Enter your initials and then your name
  for tracking who performed the "fileIn".
- All the class categories, classes, and methods defined in the file
  will now be available in the current image.

### Messages

The only mechanism for communication between objects
is for one to send a message to another.

In documentation, message names are preceded by `#`,
but that does not appear when sending the messages.

Smalltalk supports three types of messages:

- unary

  These message names are alphanumeric and begin lowercase.
  For example, in `5 factorial`, `#factorial` is a unary message.

- binary

  These message names contain one or more of the following characters:
  `+ - * / \ ~ < > = @ % | & ? ,`

  For example, in `a * b`, `#*` is a binary message.

- keyword

  Each keywork is alphanumeric, begin lowercase, and ends in a colon.
  For example, `#at:put` is a keyword message in the
  `OrderedCollection` class which is the superclass of `Array`.
  This message is sent as follows:

  ```smalltalk
  colors := #('red' 'green' 'blue').
  colors at: 2 put: 'yellow'.
  ```

When multiple messages of these types are combined in a single expression,
the order of evaluation is:

- unary messages from left to right
- binary messages from left to right
- keyword messages from left to right

For example, in `2 raisedTo: 1 + 3 factorial`,
the order is `#factorial`, `#+`, and `#raisedTo`.

### Control Flow

Control flow is provided through message passing.

The `Boolean` class in the `Kernel:Objects` category contains the methods
`#ifTrue`, `#ifFalse`, `#ifTrue:ifFalse`, and `#ifFalse:ifTrue`.
For example:

```smalltalk
result := a < b ifTrue: 'less' ifFalse: 'more'.
```

The values for `ifTrue` and `ifFalse` can be
literal values, variables, or blocks with no parameters.

## Data Types

Strings are delimited by single quotes, not double quotes.

Literal arrays between with `#(`, end with `)`,
and contain space-separated values.
For example, `#(True 7 'Tami' (Color red))`.

Collection methods include:

| Method          | Description                  |
| --------------- | ---------------------------- |
| `collect:`      | like `map` in JavaScript     |
| `detect:`       | like `find` in JavaScript    |
| `do:`           | like `forEach` in JavaScript |
| `select:`       | like `filter` in JavaScript  |
| `allSatisfy:`   | like `every` in JavaScript   |
| `anySatisfy:`   | like `some` in JavaScript    |
| `inject: into:` | like `reduce` in JavaScript  |

For example, `#(1 2 3) inject: 0 into: [:acc :n | acc + n]` gives `6`.

### Array

TODO: Add detail.

### Bag

TODO: Add detail.

### Dictionary

This is a subclass of the `Set` class.
TODO: Add detail.

### LinkedList

TODO: Add detail.

### Set

TODO: Add detail.

## Creating Objects

New objects can be created from a class using the class method `new` or `basicNew`.
By default, both initialize all attributes of the new object to `nil`.
The difference between them is that `new` could be overridden
to do something different, whereas `basicNew` cannot be overridden.

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

## Main Windows

There are four main windows that can be opened
by clicking on the WorldMorph background,
selecting Open, and selecting a window name.

- Browser: for examining code (a.k.a System Browser)
- File List: file explorer for viewing all local files and editing text files
- Installed Packages: lists all installed packages and allows more to be installed
- Message Names: for determining which classes implement a given method
- Process Browser: displays the state of all Smalltalk processes
- SUnit Test Runner: for running unit tests
- Text Editor: for editing code
- Transcript: displays output
- Workspace: for experimenting with code

## System Browser

System Browser windows contain four rows.

- The first (top) row contains four panes for displaying and operating on
  class categories, classes, message categories, and methods.

  - Selecting a class category in the first pane
    displays the classes in that category in the second pane.
    For example, the class `String` is in the class category `Text`.
  - Selecting a class in the second pane
    displays message categories for the class in the third pane.
    Example message category names include "accessing", "comparing",
    "copying", "converting", "enumerating", and "printing".
  - Selecting a message category in the third pane
    displays methods in that category in the fourth pane.
    The top, default messate category is "-- all --",
    which contains all the methods.

  TODO: Are message categories also referred to as protocols?

- The second row displays a message describing the item selected in the top row.
- The third row contains a series of buttons that can be clicked to
  open other windows that show information related to the selected item.
  One exception is the "show..." button. Clicking this displays a popup
  containing exclusive checkboxes that can be checked
  to indicate what should be displayed in the fourth row.
  The default is "source" which is typically the desired choice.
- The fourth row displays information about the selected item
  based on the checkbox that is selected for the "show..." button.
  By default it displays Smalltalk code for the selected item
  and can be used to edit the code.

To open a System Browser window, click on the `WorldMorph` background,
select Open, and select Browser.

To search for a class by part of its name,
right-click in the class category pane and select "find class..."
or click in that pane and press cmd-f.
Then enter part of a class name and press return.
A popup list of matching classes will appear.
Click one of the names to browse that class.

To browse a class, type its name (ex. String) in a Workspace window,
then right-click and select "Browse it" (or press cmd-b).
This opens a Browser window with the class already selected.

To delete a method from a class, click the method name to select it,
right-click the method name, and
select "Remove it" or "Remove, then browse senders".
The latter option allows the senders to be modified.

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-the-system-Browser.html"
target="_blank">A brief introduction to the System Browser</a>.

## Workspaces

Workspace windows enable experimenting with code.
They are somewhat like REPLs in other programming languages.

Enter any number of expressions separated by periods.

To execute expressions, select them or
place the cursor at the end of a single expression.

To only execute them, right-click and select "Do it" or press cmd-d.
To execute them and print the result,
right-click and select "Print it" or press cmd-p.
You will do both of these often,
so remember the cmd-d and cmd-p keyboard shortcuts.

To inspect a variable, right-click it and select "Inspect it"
or select it and press cmd-i.

## Transcript

This window displays output written to it.
One way to do this is to do `Transcript show: <some-value>`,
perhaps in a Workspace window.

Another way is to use the `print` method in the `CharacterSequence` class
which is the superclasss of the `String` class.
This executes `Transcript show: self; newLine`.
For example, `'Hello World!' print`.

The `print` message can be sent to strings, symbols,
and any object that has a `printString` method.

To clear the contents of the Transscript window,
right-click in it and select "Clear Transcript".

TODO: Why does this window contain the word "Transcript" in its content?

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

## MessageNotUnderstood Windows

When a message is sent to an object that doesn't have a corresponding method,
a MessageNotUnderstood window is opened.
This displays a stack trace showing the origin of the message send
with the most recent call at the top.

One option is to implement the missing method.
To do this:

- Click the "Create" button.
- Select the class in which the method will be added.
- Select a category for the method.
- Enter an implementation for the method.
- Press cmd-s to save it.
- Optionally click the "Proceed" button to
  resume execution with calling the new method.

## Morphs

Morphs are what other graphical systems refer to as widgets.
They are graphical items that can be added to the WorldMorph or a LayoutMorph.

To create a morph:

- Click the WorldMorph background.
- Select "New morph...".
- In the dialog that appears,
  select a category of morphs and then a specific kind.

Examples of morphs include:

-

To create and add morphs with code:

## Halo

To open the halo (set of surrounding buttons) for an item,
ctrl-shift-click it (or ctrl-right-click it).

<img alt="Smalltalk halo" style="width: 50%"
  src="/blog/assets/smalltalk-halo.png?v={{pkg.version}}">

The following buttons are provided:

| Button                               | Location    | Purpose                                         |
| ------------------------------------ | ----------- | ----------------------------------------------- |
| red circle with white "x"            | upper-left  | removes the item                                |
| blue circle with white document      | top         | opens menu "A" (1)                              |
| black circle with house              | top         | drag to move the item within its parent         |
| brown circle with resize icon        | top         | drag to move the item out of its parent         |
| green circle with copy icon          | upper-right | drag to position a duplicate of the item        |
| orange circle with wrench            | right side  | opens a menu of debugging options               |
| blue circle with magnifier glass     | right side  | drag to change scale of item                    |
| yellow circle with resize icon       | lower-right | drag to resize the item                         |
| light blue circle with question mark | bottom      | click and hold to display a related tooltip (2) |
| blue circle with rotate icon         | lower-left  | drag to rotate item                             |
| dull yellow circle with odd shape    | left side   | click to collapse (hide) the item (3)           |
| orange circle with wrench            | left side   | opens an "Explore" window (4)                   |

TODO: How can you change the point about which a morph rotates?

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
- Enter a package name. For example, "Volkmann".
- Select the newly created package.
- Enter a comment describing the package.
- Click the "Save" button.

To determine where packages are saved:

- Open a Workspace.
- Enter `Smalltalk imagePath.`
- Press cmd-p to print the result.

For me this is
`/Users/volkmannm/Documents/dev/lang/smalltalk/Cuis6-2-main/Cuis6.2.image`.
Packages I create go in a similar path which is
`/Users/volkmannm/Documents/dev/lang/smalltalk/Cuis6-2-main-UserFiles/NewPackages/Volkmann.pck.st`

## Adding and Saving Code

Create a new package for your code as described above.
While still in the "Installed Packages" window,
select the package and click "browse"
to open a Browser window for the package.

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
