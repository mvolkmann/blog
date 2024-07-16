---
eleventyNavigation:
  key: Development Environment
  order: 1.3
  parent: Smalltalk
layout: topic-layout.njk
---

You will be opening and using many windows in the development environment.
To open a window, open the World menu, hover over "Open"
to display a submenu of window types, and click one of them.

To close a window, click its red circle button on the left side of its title bar.
Alternatively, move the mouse cursor over the window and press cmd-w.

The available windows, in the order listed, include:

- Text Editor: for editing text other than Smalltalk source code
- Workspace: for experimenting with code
- Browser (a.k.a System Browser): for examining and editing code
- Message Names: for finding classes that implement a given method
- Transcript: displays output
- Installed Packages: lists all installed packages and allows more to be installed
- Change Sorter: for viewing, manipulating, and saving ChangeSets
- Process Browser: displays the state of all Smalltalk processes
  and enables terminating them
- Emergency Evaluator: a limited user interface that appears when
  an error occurs that prevents opening a Debugger;
  can revert the last code modification in order to recover
- File List: file explorer for viewing all local files and editing text files
- SUnit Test Runner: for running unit tests and viewing the results

Of these, the most frequently used windows tend to be
Workspace, Transcript, and Browser.

To tile all the open windows, open the World menu
and select Windows...Tile open windows.

To refresh all the windows after code changes that affect them
(or if the display renders incorrectly for some reason),
open the World menu and select "Windows...Restore all Windows".
This does not update windows to use a newly selected theme.

## Editing Code

Many kinds of windows support entering Smalltalk code.
The syntax highlighting described in the table below is provided.

| Token Type        | Styling        |
| ----------------- | -------------- |
| class name        | black and bold |
| comment           | green          |
| instance variable | purple         |
| keyword           | red            |
| message name      | blue           |
| string            | purple         |
| symbol            | blue and bold  |

In any text editing pane, right-click and select "Help..."
to see a list of the supported key bindings.

To select arbitrary text, drag over it.  
To select a word, double-click it.  
To select an entire line, triple-click it.  
To select all the code in an editing pane, press cmd-a.

To toggle surrounding selected text with a given delimiter character,
press cmd and the starting delimiter character
which can be `'`, `"`, `(`, `[`, or `{`.
Pressing cmd-" is useful to toggle whether selected code commented/uncommented.

To change the indentation of a block of code, select all the lines and
press tab to increase indentation or shift-tab to decrease it.

To get help on a token, select it, right-click it, and select "Explain".
A popup will appear.
If the token is a literal value, it give its class name.
If the token is a class name, it will give its class category.
If the token is a message name, it will list the classes that implement it.

## Inspect Windows

Inspect windows display all the instance variables of a specific object.
To open one, select an object reference or place the cursor immediately after it
and press cmd-i (Inpect it).

<img alt="Cuis Inspect window" style="width: 40%"
  src="/blog/assets/cuis-inspect-window.png?v={{pkg.version}}">

Clicking an item in the top left pane,
displays related information in the top right pane.

- Click "self" to display the class name of the object.
- Click "all inst vars" to display a list
  of all instance variables and their values.
- Click the name of an instance to display its current value.

The bottom pane can be used to enter and execute Smalltalk expressions.
Instance variables can be directly accessed and
where `self` refers to the object being inspected.
For example, when the object is a morph:

- To get the value of the `color` instance variable,
  enter `color` or `self color` and press cmd-p (Print it).
- To set the value of the `color` instance variable,
  enter `color := Color red` or `self color: Color red`
  and press cmd-d (Do it).

Inspector windows are live, so changes made to the instance variables
are reflected.

Let's walk through an example:

1. Open the World menu and select "New Morph...".
1. Select "Basic...Boxed Morph".
   An orange rectangle will appear, attached to the mouse cursor.
1. Move to where you want to place it and click to drop it.
1. cmd-click on the morph to open its halo.
1. Click the blue button on the top row.
1. Select "debug...inspect morph" to open an Inspect window
   for the object that represents that morph.
1. Click the `color` instance variable and
   note that the value is a `Color` object.
1. Select the name "Color" and press cmb-b (Browse it)
   to open a System Browser focused on the `Color` class.
1. Click the "class" button to see a list of
   the class methods in the `Color` class.
1. Click the method category "named colors"
   and note that one of the methods is "red".
1. Back in the Inspect window, click in the bottom pane.
1. Enter `color := Color red` and "Do it".
1. Note that the value displayed in the top right pane updates,
   but the fill color of the morph does not update.
1. Enter `self color: Color red` and "Do it".
   This time the value displayed in the top right pane updates
   AND the fill color of the morph updates.
   The reason is that the `color:` method in the `BoxedMorph` class
   sends the `#redrawNeeded` message to `self`
   which triggers the morph to redraw itself with the updated color.

## Explore Windows

Explore windows display an object tree starting at a specific object.
Select an object reference or place the cursor immediately after it
and press cmd-shift-i (Explore it).
Click the disclosure triangles to drill down into instance variable values.

Use the bottom pane to enter and execute Smalltalk expressions
where `self` refers to the selected object in the top pane.

<img alt="Cuis Explore window" style="width: 40%"
  src="/blog/assets/cuis-explore-window.png?v={{pkg.version}}">

## System Browsers

A System Browser can be used to read and modify any class,
including those provided by the base image,
those provided by installed packages, and those you create.

To open a System Browser, open the World menu and select Open...Browser.
Alternatively, type a class name (ex. String) in a Workspace window
and press cmd-b (Browse it)
to open a System Browser with that class already selected.

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-the-system-Browser.html"
target="_blank">A brief introduction to the System Browser</a>.

### System Browser UI

System Browsers contain four rows.

- The first (top) row contains four panes for displaying and operating on
  lists of class categories, classes, message categories, and methods.
  Clicking an item toggles whether it is selected.

  Selecting a class category in the first pane
  displays the classes in that category in the second pane.
  For example, the class `String` is in the class category `Kernel-Text`.

  Selecting a class in the second pane
  displays message categories for the class in the third pane.
  Example message category names include "accessing", "comparing",
  "copying", "converting", "enumerating", and "printing".
  There is a separate set of method categories
  for instance methods and class methods.

  Selecting a message category (a.k.a protocol) in the third pane
  displays methods in that category in the fourth pane.
  The top message category, provided by default, is "-- all --".
  If no message category is selected, or if "-- all --" is selected,
  all the methods in all categories are listed.

  The names in the class category and method category panes
  are not sorted alphabetically by default.
  To sort class categories, hover over that pane and press cmd-shift-a (alphabetize).
  To sort method categories, hover over that pane and press cmd-a (alphabetize).
  The reason for the shortcut key difference is that in the class category pane,
  cmd-a is used for "add item...".

  To scroll any list to the first item that begins with a given letter,
  over over the lsit and type the letter.

  The global `SystemOrganization` refers to
  an instance of the `SystemOrganizer` class.
  To get an `Array` of symbols that are the names of all
  current class categories, evaluate `SystemOrganization categories`.

- The second row displays a message describing the item selected in the top row.

  | Item Type       | Description                                                         |
  | --------------- | ------------------------------------------------------------------- |
  | class category  | # of classes in the category, total # of instance and class methods |
  | class           | # of instance and class methods defined in the class                |
  | method category | # of methods                                                        |
  | method          | # of message sends, implementors, and senders                       |

- The third row contains a series of buttons that can be clicked to
  open other windows that show information related to the selected item.
  One exception is the "show..." button, described below.

  The "browse" button opens a new System Browser focused on
  the currently selected class, message category, or method.
  This enables maintaining the view in the current System Browser and
  navigating somewhere relative to that location in a new System Browser.

  The "senders" button opens a "Senders of" window that lists
  all the methods that send a message to invoke the selected method.
  Selecting one displays its implementation in the bottom pane.
  If no method is selected, a text entry will be displayed
  to prompt for a method selector.

  The "implementors" button opens an "Implementors of" window
  that lists all the classes that implement the selected method.
  Selecting one displays its implementation in the bottom pane.

  The "versions" button opens a "Recent versions" window
  that displays a list of time stamps for recent versions of the method.
  TODO: Does this only include changes made since the current image was opened?
  Clicking a time stamp displays that version of the code in the bottom pane.

  If one of the buttons "lineDiffs", "wordDiffs",
  "linePrettyDiffs", or "wordPrettyDiffs" is pressed, it will show
  differences between that version and the current version.
  The two "line diff" options show whole line differences.
  The two "word diff" options show individual word differences.
  The two "pretty" options show the code in its formatted form.
  To stop showing differences, click the selected diff button to toggle it off.

  Code added by the selected version will be in green.
  Code removed by the selected version will be in red.
  Click the "revert" button to restore that version
  as the current version of the code.
  The "compare to current" button displays the same information
  as the lineDiffs button, but in a new window.

  The "inheritance" button opens an "Inheritance of" window
  that shows the superclass methods of the same name
  that are invoked by the selected method,
  all the way up the inheritance hierarchy.

  The "hierarchy" button opens a Hierarchy Browser
  that shows the inheritance hierarchy of the current class.
  This includes all superclass and subclasses of the current class.

  The "inst vars" button displays a popup list containing
  all the instance variables in the current class.
  Clicking one of them opens an "Accesses to" window that
  lists all the methods that use the instance variable it its top pane.
  Clicking on a method displays its code in the bottom pane.

  The "class vars" button displays a popup list containing
  all the class variables in the current class.
  Clicking one of them opens a new "Users of" window that
  lists all the methods that use the class variable in its top pane.
  Clicking on a method displays its code in the bottom pane (fourth row).

  The "show..." button displays a popup list
  of ways the selected item can be displayed in the bottom pane.
  Each option is represented by a checkbox, but only one can be selected.

  The default option is "source" which merely
  displays the source code of the selected method.
  This is typically the desired choice.

  Selecting "documentation" only displays
  the signature and comment for the selected item.
  When there is no commment, it displays "Has no comment".

  Seleting "prettyPrint" displays the code for a method in a
  nicely formatted way, but doesn't actually modify the code.
  I wish it did.

  Selecting "lineDiffs", "wordDiffs", "linePrettyDiffs", or "wordPrettyDiffs"
  has the same functionality as described earlier for "Recent versions" windows.

  Selecting "decompile" displays code that is similar to the source code,
  but variable names are changed.
  Parameter variable names are replaced by `arg1`, `arg2`, and so on.
  Local variable names are replaced by `temp1`, `temp2`, and so on.
  It's not clear when this would be useful.

  Selecting "byteCodes" displays the byte codes
  generated by the method source code.
  This could be useful for evaluating the efficency of the code.

  Methods categories used by a given class are stored in
  its provided instance variable `organization`.

- The fourth row displays information about the selected item
  based on the checkbox that is selected for the "show..." button.
  By default it displays Smalltalk code for the selected item
  and can be used to edit the code.
  When there are unsaved code changes in this pane,
  a thin, red border appears around it.
  Press cmd-s (Accept) to save the changes
  and the thin, red border will disappear.

### Code Formatting

To automatically display formatted code,
enter the following in a Workspace and "Do it":

```smalltalk
Preferences at: #browseWithPrettyPrint put: true
```

This does not take effect in existing windows such as System Browsers,
but does in newly opened windows.
Pretty printing is applied when code is initially displayed,
and again every time it is saved.
However, it does not affect the actual use of whitespace, so
code written to files in a fileOut or package is not affected.
I wish it was.

An alternative to setting the `#browseWithPrettyPrint` preference is to
click the "show..." button in System Browsers and select "prettyPrint".

<img alt="Cuis System Browser" style="width: 100%"
  src="/blog/assets/cuis-system-browser-window.png?v={{pkg.version}}">

### Working with Classes

To create a new class:

- Select a class category.

  If the desired class category is already selected
  and a class in that category is selected,
  click it so it is deselected.
  The bottom pane will now contain the following message send template:

  ```smalltalk
  Object subclass: #NameOfSubclass
      instanceVariableNames: ''
      classVariableNames: ''
      poolDictionaries: ''
      category: '{SelectedClassCategory}'
  ```

- To inherit from a class other than `Object`,
  change that to another class name.
- Change "NameOfSubclass" to the name of the new class.
- Add desired instance and class variable names as space-separated strings.
- Save by pressing cmd-s (Accept).

To delete a class, select it and press cmd-x (Remove it).
A confirmation popup will appear.
If the class has subclasses, a second popup will appear
to confirm that you also wish to delete those classes.

### Working with Methods

To create a new method:

- Click the "instance" or "class" button to indicate the scope of the method.
- Select a method category.

  This can be "-- all --", "as yet unclassified", or any other method category.
  To create new category, move the mouse cursor over the method category pane,
  press cmd-n (new category...), and enter the name of the new category.

  The bottom pane will now contain the following method template:

  ```smalltalk
  messageSelectorAndArgumentNames
      "comment stating purpose of message"

      | temporary variable names |
      statements
  ```

- Modify the code template to define the new method.
- Save by pressing cmd-s (Accept).

To move a method from the class side to the instance side or vice-versa,
right-click the method name in the top fourth pane and
select "refactorings...move to instance/class methods".

To delete a method from a class, select it and press cmd-x (Remove it).
Then select "Remove it" or "Remove, then browse senders".
The latter option allows the senders to be modified.

### Refactorings

To refactor a method or code, select it, right-click,
and select an option from the "refactorings" submenu.

For methods the options include:

- rename... (cmd-shift-r)
- change keyword order...
- add parameter...
- remove parameter...
- inline method...
- move to instance/class method
- push up
- add in superclass as subclassResponsibility
- push down to subclasses
- push down to one subclass

To rename a method without opening the refactorings menu,
select the method and press cmd-shift-r.

For selected code the options include:

- Extract as Parameter...
- Extract Temporary...
- Extract Method...
- Inline Temporary...
- Inline Method...
- Temporary to Instance Variable
- Push Up Instance Variable
- Push Down Instance Variable

## Search Browser

There is no provided way to search for code that contains a given string.
However, Mariano Montone implemented a "Search Browser" that provides this.

To install it, clone the repository
<a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Tools"
target="_blank">Cuis-Smalltalk-Tools</a>,
open a Workspace, enter `Feature require: 'SearchBrowser'`, and "Do it".
This adds the World menu item "Open...Search Browser".
It also adds menu items to the menu that appears in a System Browser
when you right-click a class name in the top second pane.
The new menu items are "search in class...",
"search in class hierarchy...", and "search in class protocol...".
These all open a Search Browser with a different search scope.

Enter search text in the input to the right of
the "Search" button at the bottom.
The scope and characteristics of the search
can be specified with menus and a button at the bottom.

The "in:" menu has the options "source" (default),
"message name", and "string literal".

The "of:" menu has the options "class hierarchy" (default),
"class protocol", "class", "system category", and "every class".

The "using:" menu has the options
"substring matcher" (default) and "wild matcher".
When using "wild matcher", the `CharacterSequence` `match:` method is used.
Comments in that method provide many examples.
The `#` wildcard character matches any single character and
the `*` character matches any sequence of characters.

The "Case sensitive" button toggles whether the search will be case sensitive.

Methods whose code matches the search will appear in the top pane.
Select a method name to see its implementation in a lower pane.

## Hierarchy Browsers

When a class is selected in a System Browser,
the list of classes in the second pane only includes
those defined in the selected class category.
To also see the class hierarchy of a selected class,
open a Hierarchy Browser by pressing cmd-h (browse hierarchy).

This window is similar to a System Browser,
but it omits the class categories pane and
displays the complete class hierarchy of the selected class.

Here's an example of a Hierarchy Browser for the `Array` class:

<img alt="Cuis Hierarchy window" style="width: 40%"
  src="/blog/assets/cuis-hierarchy-window.png?v={{pkg.version}}">

## Protocol Browsers

When a class is selected in a System Browser,
only the methods defined directly in that class are displayed.
To also see methods defined in superclasses,
open a Protocol window by pressing cmd-p (browse protocol).

This window is similar to a System Browser,
but it omits the class categories pane and
displays all instance methods available on instances of the class.
Methods defined directly on the class are in bold,
and methods defined in superclasses are not.

Here's an example of a Protocol window for the `Array` class:

<img alt="Cuis Smalltalk Protocol window" style="width: 100%"
  src="/blog/assets/cuis-protocol-window.png?v={{pkg.version}}">

## Text Editor Windows

"Text Editor" windows enable editing text files.
They support changing the font size, color, and style of selected text.
They are not intended to be used to edit Smalltalk source code.

The text can be saved in external text files,
but all the formatting is discarded and only the raw text is saved.

## Message Names Windows

These windows enable searching for method implementations
whose name contains a given substring.
For example, enter "select:" to find all the classes
that have a method whose names end with that.
The results include `Bag`, `Collection`, `Dictionary`, `Heap`,
`OrderedCollection`, `SequenceableCollection`, and `SortedCollection`.
Click one the class names to see the method implementation.

<img alt="Cuis Smalltalk Protocol window" style="width: 100%"
  src="/blog/assets/cuis-protocol-window.png?v={{pkg.version}}">

## MessageNotUnderstood Errors

A `MessageNotUnderstood` error is signaled when a message is sent to an object
and no method is found in the class of the object or any of its superclasses
that answer the message.

Message sends are processed in the following way:

- If the receiver class implements a compatible method, that is called.
- Otherwise the superclasses of the receiver class are searched
  in order from nearest to `Object` for a compatible method.
- If the search makes it to the `Object` class and no compatible method is found,
  the `#doesNotUnderstand:` message with a `Message` argument
  is sent to the original receiver,.
- If the receiver class implements the `doesNotUnderstand:` method,
  that is called.
- Otherwise the superclasses of the receiver class are searched
  in order from nearest to `Object` for the `doesNotUnderstand:` method.
- If the search makes it to the `Object` class, that implements the
  `doesNotUnderstand:` method to signal a `MessageNotUnderstood` error,
  which results in a Debugger window opening.
  That window that includes a stack trace which describes
  the stack at the time the original message was sent.

The `doesNotUnderstand:` method is passed a `Message` object
which has the accessor methods `selector` (returns a `String`)
and `arguments` (returns an `Array`).
Those can be used to determine whether and how to answer the message.
If the message will not be handled,
the `#doesNotUnderstand:` message should be resent to the superclass.
For example, the following implementation of `doesNotUnderstand:`
could be implemented in a class that represents a dog.
This is a contrieved example because it would be better
to directly implement a `bark:` method.

```smalltalk
doesNotUnderstand: aMessage
    (aMessage selector = 'bark:') ifTrue: [
        | count |
        count := aMessage arguments first.
        count isNumber ifTrue: [ ('Woof! ' repeat: count) print. ^ nil ]
    ].
    super doesNotUnderstand: aMessage
```

There is no method in the the `String` class that answers the message `#repeat:`,
so one must be implemented in order for the example above to work.
One possible implementation is the following:

```smalltalk
repeat: anInteger
    | stream |
    stream := String writeStream.
    anInteger timesRepeat: [stream nextPutAll: self].
    ^stream contents
```

<img alt="Cuis MessageNotUnderstood window" style="width: 85%"
  src="/blog/assets/cuis-messagenotunderstood-window.png?v={{pkg.version}}">

## Debug Windows

To debug code, select one or more lines in a Workspace window
and press cmd-shift-d (Debug it).
A Debug window will appear.
Execution will be stopped at the beginning of the selected code,
waiting for you examine variable and decide whether/how to proceed.

To set breakpoints in methods, add `self halt` message sends
in selected locations in their code.
The `halt` method is defined in the `Object` class.
It sends the `#signal` message to the `Halt` class
which is a sublcass of `Exception` that is resumable.
Then run code the invokes the methods.
When `self halt` is evaluated, a Debugger will open.
Locate the line in the stack trace ending in ">>halt".
Click the line immediately after it to examine the
method containing `self halt` and its variables.

<img alt="Cuis Debug window" style="width: 100%"
  src="/blog/assets/cuis-debug-window.png?v={{pkg.version}}">

Click the "Into" button to begin executing the code.
The "Proceed", "Restart", "Into", "Over" buttons
function as expected if you have used other debuggers.

To run up to a specific location in the code,
click to place the cursor where execution should stop
and click the "Run to Cursor" button.

The in-scope variables are listed in the third pane of the bottom row.
Click a variable name to see its current value
in the fourth pane of the bottom row.
To change the value of a variable, edit it where displayed
and press cmd-s (Accept).

Click the "Where" button to highlight the next message to be sent in the code.

The Debug window will close when the end of the selected code is reached.

Using `self halt` in the middle of Morphic operations
seems to produce unexpected results. TODO: Why?

## Change Sorter Windows

Change Sorters summarize all the currently unsaved changes.
The first row left pane displays a list of change set names.
Selecting one displays a list of modified classes in the first row right pane.
Selecting a modified class displays a list of modified methods
in the second row.
Selecting a modified method displays its implementation
or a description of the change in the bottom row.

Right-click a change set, class, or method name to get
a context menu of operations that can be performed on it.

<img alt="Cuis Change Sorter" style="width: 100%"
  src="/blog/assets/cuis-change-sorter.png?v={{pkg.version}}">

## File List

To view local files and operate on them,
select Open ... File List from the World menu.

By default, the top directory will be one from which Cuis was started,
referred to as "Cuis top".
To instead start from the root directory of the drive,
right-click in the upper-left pane and
select "default initial directories...OS roots".
This change will not take effect until a new File List is opened.

A common operation performed in a File List window
is to locate and select a `.pck.st` file that defines a package
and click the "install package" button to install it.

## Installed Packages Window

To see all the installed packages, open the World menu
and select Open...Installed Packages.
This opens an "Installed Packages" window.

To browse everything that is defined in a package:

- Select the package.
- Click the "Browse" button.
- This opens a System Browser that is focused on the selected package package.

To create a new package:

- Click the "New" button in the center strip of buttons.
- Enter a package name. For example, "Volkmann".
- Select the newly created package.
- Enter a comment describing the package.
- Click the "Save" button.
- You will be prompted for the file path and name
  where the package will be saved.
  It's a good idea to keep the suggested file name,
  but feel free to change the directory to one outside
  the distribution directory.

If package name matches the name of a class category
that was created previously, all the classes in that category
and their methods will automatically be associated with the new package.

I created a package whose name is my last name.
I use this package to save all my experimental code
so I can easily load it into new images.
This is useful in case I accidentally modify an image in an unintended way.
I can then return to using a base image and load my package into it.

## Process Browsers

Process Browsers display a list of all the Smalltalk-related processes
that are running.
Processes come and go. By default the list updates automatically.
To toggle that, press cmd-a (turn off/on auto-update).

To terminate a process, select it and press cmd-t (terminate).
This is especially useful for terminating
processes named "WebServer's listening process".

<img alt="Cuis Process Browser" style="width: 85%"
  src="/blog/assets/cuis-process-browser-window.png?v={{pkg.version}}">

For example, the following code starts a process that
writes to the Transcript every five seconds.

```smalltalk
block := [[true] whileTrue: [
    'Hello' print.
    (Delay forSeconds: 5) wait
]].
block forkAt: Processor userBackgroundPriority named: 'hello'.
```

The name of the process will be "hello".
To stop it, right-click the process, select it and press cmd-t (terminate).

## Emergency Evaluator

The Emergency Evaluator appears when there is
an error for which a Debugger cannot be opened.
In this case the environment can become unusable.
Enter the "revert" command to revert the last code modification.
Enter the "exit" command to exit this window.

<img alt="Cuis Emergency Evaluator" style="width: 30%"
  src="/blog/assets/cuis-emergency-evaluator.png?v={{pkg.version}}">
