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

Alan Kay said "OOP to me means only messaging,
local retention and protection and hiding of state-process,
and extreme late-binding of all things."
Late binding means that messages sent to objects
are looked up for compatible methods at runtime.
However, Smalltalk editors do check for "unknown selectors"
when code is entered.

TODO: Are there any linting tools for Smalltalk?

## Resources

- <a href="https://en.wikipedia.org/wiki/Smalltalk"
  target="_blank">Smalltalk in Wikipedia</a>
- <a href="https://cuis.st" target="_blank">Cuis Smalltalk</a>
- <a href="https://pharo.org" target="_blank">Pharo Smalltalk</a>
- <a href="https://squeak.org" target="_blank">Squeak Smalltalk</a>
- <a href="https://www.fast.org.ar"
  target="_blank">Fundación Argentina de Smalltalk</a> (FAST)
- <a href="https://www.gnu.org/software/dr-geo/" target="_blank">Dr. Geo</a>

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

To install Cuis Smalltalk:

1. Browse <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev"
   target="_blank">Cuis Smalltalk Git repository</a>.
1. Click the "Zip file" link near the bottom of the page.
1. Unzip the downloaded file to obtain the directory "Cuis-Smalltalk-Dev-master".
1. Move this directory to your desired location.

To start Cuis Smalltalk, run the appropriate start script found
in the installed directory based on your operating system.

- for Windows, open a Command Prompt and run `RunCuisOnWindows.bat`
- for Linux, open a Terminal and run `RunCuisOnLinux.sh`
- for Mac

  1. Double-click CuisVM.app which will fail because the app is not verified.
  1. Open the System Settings app.
  1. Select "Privacy & Security".
  1. Scroll down to the "Security" section.
  1. Look for the message '"CuisVM.app" was blocked from use
     because it is not from an identified developer.'
  1. Click the "Open Anyway" button.
  1. Click the "Open" button in the next dialog that appears.
  1. Select the image file `Cuis6.2.image`.
  1. Click the "Open" button.
  1. You will see several dialogs that say '"CuisVM.app"
     would like to access files in your Documents folder'.
     Click the "Allow" button each time.

The file `CuisVM.app` (36 MB) implements the virtual machinex
used by Cuis Smalltalk.
It is taken directly from Squeak and does not differ in any way.
All the differences between Cuis and Squeak are implemented in
its base image file found in the `CuisImage` subdirectory
with a name like `Cuis6.2.image` or `Cuis7.1-6367.image`.
The base Cuis image file is around 19 MB.

By contrast, `PharoLauncher.app` is 198MB.

The reported name of the `CuisVM.app` app in macOS is "Squeak 5.0".
To change this:

- Open a terminal window.
- `cd` to the directory where you placed the file `CuisVM.app`.
- Enter `cd CuisVM.app/Contents`.
- Edit the file `Info.plist`.
- Find the key "CFBundleName".
- Change its value from "Squeak" to "Cuis".

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

Menus can be made to remain open so multiple selections can be made
by clicking its push pin in the upper-right corner.
If the menu is closed and re-opened, the push pin state will be reset.

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
   right-click in it and select "Clear Transcript" (no keyboard shortcut).

To evaluate an expression in a Workspace
and display the result after it in the workspace:

- Enter an expression in the Workspace window like `3 factorial`.
- Select it by pressing ctrl-a or dragging over it with the mouse.
- Press ctrl-p to print it.
- Remove the output from the Workspace window
  by pressing ctrl-z to undo adding it.

## Windows

You will be opening and using many windows.
To open one, click on the `WorldMorph` to open the World menu
and select a window type from the "Open" submenu.

The available windows include:

- Text Editor: for editing text other than Smalltalk source code
- Workspace: for experimenting with code
- Browser: for examining code (a.k.a System Browser)
- Message Names: for determining which classes implement a given method
- Transcript: displays output
- Installed Packages: lists all installed packages and allows more to be installed
- Change Sorter: TODO: Describe this.
- Process Browser: displays the state of all Smalltalk processes
- Emergency Evaluator: TODO: Describe this.
- File List: file explorer for viewing all local files and editing text files
- SUnit Test Runner: for running unit tests and viewing the results

Of these, the most frequently used windows tend to be
Workspace, Transcript, and Browser.

To tile all the open windows, open the World menu
and select Windows...Tile open windows.

TODO: Are other options in the Windows menu useful?

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
| method return value                           | `^<expression>`                                  |
| expression separator (period)                 | `Transcript show: 'foo'. Transcript show: 'bar'` |
| reference to current object in a method       | `self`                                           |

To display a left pointing arrow in place of `:=` for all assigments,
open the World menu and select Preferences...Show ST-80 Assignments.
The next time code is modified, all the `:=` messages
will be replaced by a left pointing arrow.
And typing `:=` or `_` will be replaced by a left pointing arrow.
To revert to showing `:=` messages,
open the World menu and select Preferences...Show ANSI Assignments.

The caret (^) in a return expression can be followed by a space,
but a space is not typically included.

In static arrays the elements are separated by spaces.

In dynamic arrays the expressions are separated by periods.

TODO: What is a "compound literal"?

### Classes

Classes define sets of associated class variables, instance variables,
class methods, and instance methods.

Classes are defined by sending the message
`#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:`
to a superclass which can be `Object` or any other class.
The `subclass` keyword takes a symbol.
The remaining keywords all take strings.
All the keywords must be supplied, even if their value is an empty string.
The following is an example class definition.

```smalltalk
Object subclass: #Math
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Instance variable names must begin lowercase.
Class variable names must begin uppercase.
If the category is an empty string, it will be changed to 'as yet unclassified'.

It is common to not use class variables or pool dictionaries.

Pool dictionaries enable sharing data between related classes.
They reside in the `Smalltalk` dictionary.
To create a pool dictionary: `Smalltalk at: #MyPool put: (Dictionary new)`.
Then refer to it from any number of classes with `poolDictionaries: 'MyPool'`.

All classes are global and there is no namespacing.
Class names are added to the global variable `Smalltalk`
which is a `SystemDictionary`.
This requires all class names to be unique.
Typically a common prefix is added to a set of related class names
in order to make the unique.
Lack of namespacing is seen by some as a weakness of Smalltalk.

All classes inherit from one other class,
except `Object` which is the highest superclass of all classes.

TODO: Are class names required to be unique across all packages?

Programming languages use many terms to describe data
that is encapsulated by objects created from a class.
Examples include "attribute", "property", and "field".
Smalltalk calls these "instance variables".

Instance variables can only be directly accessed by methods in the same class.
To expose them outside the class, add getter and setter (optional) methods.
For example, if `score` is an instance variable
then the following is a getter method for it.
By convention, the name of getter and setter methods is the same as
the name of the associated instance variable, but this is not required.

```smalltalk
score
    ^score

score: aNumber
    score := aNumber
```

As shown above, another convention is for variables associated with
keyword messages to indicate their expected type.

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
- To save the changes, press cmd-s (Accept).

### Objects

In Smalltalk, code and data are both represented by objects.
Code can be described by a method or block, both of which are kinds of objects.

Objects are created by sending a message to a class.
In addition, some kinds of objects can be created from a literal syntax
such as numbers, strings, and arrays.

To determine the class of an object, send it the `class` unary message.
For example, `19 class` returns `SmallInteger`.

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

See <a href="https://www.youtube.com/watch?v=cI_yBWdmoeI&list=PLu8vLCSA-4hklsvT9W6ruintbdx_K0DYW&index=11&t=28s"
target="_blank">The amazing Squeak Method Finder</a>.

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
select it and press cmd-x (remove method).

The following methods can be added to the `Integer` class.

```smalltalk
predecessor
    "answers the predecessor of this integer"
    ^self - 1

successor
    "answers the successor of this integer"
    ^self + 1
```

Superclasses can define methods that subclasses must implement.
For example, a class named `VShape` can define the following method:

```smalltalk
area
    "answers the area of the shape"
    self subclassResponsibility
```

The classes `VCircle` and `VRectangle` can be defined as subclasses of `VShape`.
If they do not define the `area` method
and that message is sent to an instance,
an Error dialog with the title "My subclass should have overridden #area"
will appear.

To add the missing method, click the "Create" button,
select a message category for the method,
enter its implemenation, press cmd-s to save, and
press the "Proceed" button to continue running the code
at the point of the failed message send.

The `VCircle` class can have the following class method for creating instances:

```smalltalk
radius: aNumber
    ^self new initializeRadius: aNumber-
```

The `VCircle` class can have the following instance methods:

```smalltalk
initializeRadius: aNumber
    radius := aNumber

area
    ^Float pi * radius * radius
```

The `VRectangle` class can have the following class method for creating instances:

```smalltalk
height: aHeight width: aWidth
    ^self new initializeHeight: aHeight width: aWidth
```

The `VRectangle` class can have the following instance methods:

```smalltalk
initializeHeight: aHeight width: aWidth
    height := aHeight.
    width := aWidth

area
    ^height * width
```

To delete a method, select it and press cmd-x (remove method).

To delete a method category and all the methods in it,
select it and press cmd-x (remove).

Both class and instance methods can call themselves recursively.

Here is an example of a class method from a class I created named `Math`
that calls itself recursively:

```smalltalk
factorial: n
    "answers the factorial of a given integer"
    ^(n = 1
        ifTrue: 1
        ifFalse: [n * (Math factorial: n - 1)])
```

Here is an example of an instance method I added to the `Integer` class
that calls itself recursively.
This method already exists in that class
and is more efficient than the version below.

```smalltalk
factorial2
    "answers the factorial of this integer"
    ^(self = 1
        ifTrue: 1
        ifFalse: [self * (self - 1) factorial2])
```

If you edit the name of a method in code editing pane of a System Browser,
it will create a copy of the method with the new name.
A method with the previous name will still exist and can be deleted.
An alternative is to right-click the method in the 4th pane
and select "refactorings...rename...".

While it is not commonly done, a method can check the types of its arguments
an alter its functionality based on those.
For example, this class method returns a number
that is double what is passed to it.
If it is given a `String` instead of a `Number`,
it converts it to a `Number` and doubles it.
If it is given some other kind of object, it just returns `0`.
TODO: How can you add error handling for strings that do not contain a number?

```smalltalk
double: obj
    "demonstrates taking different actions based on the type of an argument"

    (obj isKindOf: Number) ifTrue: [^obj * 2].
    (obj isKindOf: String) ifTrue: [^obj asNumber * 2].
    ^0
```

## Refactorings

To refactor a method or code, select it, right-click,
and select an option from the "refactorings" submenu.

For methods the options include:

- rename...
- change keyword order...
- add parameter...
- remove parameter...
- inline method...
- move to instance/class method
- push up
- add in superclass as subclassResponsibility
- push down to subclasses
- push down to one subclass

For selected code the options include:

- Extract as Parameter...
- Extract Temporary...
- Extract Method...
- Inline Temporary...
- Inline Method...
- Temporary to Instance Variable
- Push Up Instance Variable
- Push Down Instance Variable

## Keywords self and super

The `self` keyword can be used in instance methods
to refer to the current object.
It can also be used in class methods to refer to the current class.

TODO: Add more detail.

## Variables

Smalltalk supports three kinds of variables:

- Class variables are associated with a class.

  These are declard in a space-separated string that is
  the value of `classVariableNames` in a class definition.

- Instance variables are associated with a specific instance of a class.

  These are declard in a space-separated string that is
  the value of `instanceVariableNames` in a class definition.

- Temporary (or local) variables are accessible only within a method or block.

  These are declared in a space-separated string between vertical bars
  inside a method or block definition.

While Smalltalk does not support global variables,
the `Smalltalk` `SystemDictionary` object can be used for this purpose.
The following code adds the key "color" with the value "yellow"
and they retrieves the value for that key:

```smalltalk
Smalltalk at: 'color' put: 'yellow'.
color := Smalltalk at: 'color' ifAbsent: 'none'.
```

## File List

To view local files and operate on them,
select Open ... File List from the World menu.

By default, the top directory will be one from which Cuis was started,
referred to as "Cuis top".
To instead start from the root directory of the drive,
right-click in the upper-left pane and
select "default initial directories" ... "OS roots".
This change will not take effect until a new File List window is opened.

A common operation performed in a File List window
is to locate and select a `.pck.st` file that defines a package
and click the "install package" button to install it.

## File Out and File In

To save all the code for a package to a text file:

- Open a System Browser.
- Select the package in the top, first pane.
- Right-click and select "fileOut".

"fileOut" can be used to save any of these to a `.st` text file:

- a single method
- all the methods in a single method category
- a single class and all its methods
- a single package, but not methods it adds to
  classes in other packages (or class categories)

The file will be saved in
`{distribution-name}-UserFiles/FileOuts/{package-name}.st`.

TODO: Is the content of these files referred to as a change set?

To read all the code for a package from a text file:

- Open a File List.
- Locate and select a `.st` file created by a "fileOut".
- Right-click and select "fileIn".
- Enter your initials and then your name
  for tracking who performed the "fileIn".
- All the class categories, classes, and methods defined in the file
  will now be available in the current image.

## Packages

Cuis Smalltalk supports the ability to save code outside an image file
and load it into running images.
This is an alternative to Monticello which is used in Squeak and Pharo.

Packages are collections of Smalltalk code
stored in files with a `.pck.st` file.

There are three ways to install a package.

1. Drag a package file onto the `WorldMorph`
   and select "install package".
1. Open a "File List" window, locate a package file,
   select it, and click the "install package" button.
1. Open a Workspace window,
   enter the command `Feature require: '{package-name}'`,
   and press cmd-d (Do it).
   This option only works if the package is
   in the same directory as the image file that is loaded.

To see all the installed packages, click the WorldMorph background
and select Open...Installed Packages.
This opens an "Installed Packages" window.

To browse everything that is defined in a package:

- Select the package.
- Click the "Browse" button.
- This opens a "Browser for package" window.

To create a new package:

- Click the "New" button in the center strip of buttons.
- Enter a package name. For example, "Volkmann".
- Select the newly created package.
- Enter a comment describing the package.
- Click the "Save" button.

I created a package whose name is my last name.
I use this package to save all my experimental code
so I can easily load it into new images.
This is useful in case I accidentally modify an image in an unintended way.
I can then return to using a base image and load my package into it.

To determine where packages are saved:

- Open a Workspace.
- Enter `Smalltalk imagePath.`
- Press cmd-p to print the result.

Suppose the name of your Cuis directory is `Cuis-Smalltalk-Dev-master`.
For me this is
`~/Documents/dev/lang/smalltalk/{cuis-directory}/CuisImage/Cuis7.1-6367.image`.
Packages I create go in a similar path which is
`~/Documents/dev/lang/smalltalk/{cuis-directory}-UserFiles/NewPackages/Volkmann.pck.st`

To add or override methods in existing classes
and save the changes in your package:

- Add a message category to an existing class whose name is
  an asterisk followed by the new package name.
  For example, I used "\*Volkmann".
- Add new methods to the existing class in the new message category.
- Open an "Installed Packages" window and select the package.
  An asterisk before the name indicates that it has unsaved changes.
- Click the "Save" button.

To define new classes and save them in your package:

- Add a class category whose name is the same as the new package name.
- Add classes in the new class category.
- Add methods to the new classes in any method category.
- Open an "Installed Packages" window and select the new package.
- Click the "Save" button.

To verify that all this worked:

- Click the `WorldMorph` background and select "Quit without saving"
  so the changes are not saved in the current image.
- Restart Cuis Smalltalk with the same image.
- Verify that the methods and classes that were added are not present.
- Install the package.
- Verify that the methods and classes that were saved in the package
  are now present.

There is no provided way to uninstall a package.
The only way to remove it from the image is to start with a fresh image
and only install the desired packages.
TODO: What does the "delete/merge" button in the "Installed Packages" window do?
TODO: It does not uninstall the selected package or delete the file that defines it.

## Adding and Saving Code

Create a new package for your code as described above.
While still in the "Installed Packages" window,
select the package and click "browse"
to open a Browser window for the package.

### Messages

The only mechanism for communication between objects
is for one to send a message to another.

In documentation, message names are preceded by `#`,
but that does not appear when sending the messages.

Smalltalk supports three types of messages:

- unary

  These message do not take any arguments.
  Their names are alphanumeric and begin lowercase.
  For example, in `5 factorial`, `#factorial` is a unary message.

- binary

  These message take a single argument and
  have names that use a restricted set of characters that
  make them look like operators in other programming languages.
  Their names can only contain one or more of the following characters:
  `+ - * / \ ~ < > = @ % | & ? ,`

  For example, in `a * b`, `#*` is a binary message.
  This sends the message `#*` to the object `a`, passing it the argument `b`.

  The binary message `==` tests whether
  two objects are identical (same objects in memory).

  The binary message `=` tests whether two objects are equal,
  meaning one can be used in place of the other.
  Each class can define this method to decide
  how their objects should be compared.
  If they do not define `=`, an implementation
  will be found in the inheritance hierarchy.
  The `Object` class defines `=` to be the same as `==`.

- keyword

  These messages take one or more arguments
  that are each preceded by a keyword.
  Each keyword is alphanumeric, begins lowercase, and ends in a colon.
  For example, `#at:put` is a keyword message in the
  `OrderedCollection` class which is the superclass of `Array`.
  This message is sent as follows:

  ```smalltalk
  colors := #('red' 'green' 'blue').
  colors at: 2 put: 'yellow'.
  ```

  The parts of a keyword message must be specified
  in the order in which they appear.
  It's possible define additional methods that support other orders,
  but that is not typically done.

When multiple messages of these types are combined in a single expression,
the order of evaluation is:

- unary messages from left to right
- binary messages from left to right
- keyword messages from left to right

For example, in `2 raisedTo: 1 + 3 factorial`,
the order is `#factorial`, `#+`, and `#raisedTo`.

### Reserved Words

There are only six reserved words in Smalltalk which are
`true`, `false`, `nil`, `self`, `super`, and `thisContext`.

TODO: Describe `thisContext`.

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

The `Boolean` literal values are `true` and `false`.
These are instances of the classes `True` and `False`
which are subclasses of the class `Boolean`.

Characters are represented by the `Character` class.
Printable literal characters are preceded by a dollar sign.
For example, `$a`.
Non-printable characters can be obtained from
unary class methods in the `Character` class.
For example, `Character space`, `Character tab`, and `Character cr`.

Strings are represented by the `String` class.
Literal strings are delimited by single quotes, not double quotes.

Literal arrays between with `#(`, end with `)`,
and contain space-separated values.
For example, `#(true 7 'Tami' (Color red))`.

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

`Array` instances are fixed-length, ordered collections.
TODO: Add detail.

### OrderedCollection

`OrderedCollection` instances are variable-length, ordered collections.
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
and contain many expressions.
They are represented by the class `BlockClosure`.
The value of the block is the value of the last expression.
It cannot explicitly return a value with `^`.

Blocks can be saved in variables, passed as arguments to methods and blocks,
and used multiple times. For example:

```smalltalk
noArgBlock := [2 * 3].
singleArgBlock := [:a | a * 3].
multipleArgBlock := [:a :b | a * b].
```

The `value` message evaluates a block
and can be used to pass zero to four arguments.
For blocks with more than four parameters,
pass them in an array using `#valueWithArguments:`.
For example:

```smalltalk
noArgBlock value: 2 value: 3.
singleArgBlock value: 2.
multipleArgBlock value: 2 value: 3.
```

Blocks can declare and use temporary variables.
Blocks cannot explicitly return a value with the caret (^).
Their value is that if their final expression.

For example:

```smalltalk
average := [:a :b |
    | sum |
    sum := a + b.
    sum / 2.0
```

Blocks are closures, meaning that they can
access variables defined outside them. For example:

```smalltalk
n := 19.
b := [:a | a + n].
b value: 2. "result is 21"
```

To use a block as an iteration condition,
use the methods `whileTrue`, `whileFalse`, `whileNotNil`, and `whileNil`
that are defined in the `BlockClosure` class.

For example:

```smalltalk
TODO: Add a whileTrue example.
```

A block can call itself if it passes itself in as an argument.
For example:

```smalltalk
fact := [:block :n |
    n = 1
        ifTrue: 1
        ifFalse: [n * (block value: block value: n - 1)]
].

fact value: fact value: 5 "gives 120"
```

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
  - Selecting a message category (a.k.a protocol) in the third pane
    displays methods in that category in the fourth pane.
    The top, default messate category is "-- all --",
    which contains all the methods.

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
click in the class category pane and press cmd-f (find class...).
Then enter part of a class name and press return.
A popup list of matching classes will appear.
Click one of the names to browse that class.

To browse a class, type its name (ex. String) in a Workspace window
and press cmd-b (Browse it).
This opens a Browser window with the class already selected.

To delete a method from a class, select it and press cmd-x (Remove it).
Then select "Remove it" or "Remove, then browse senders".
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
To execute them for their side effects, press cmd-d (Do it).
To execute them and print the result, press cmd-p (Print it).
Output from "Print it" will be selected,
so it can be removed by pressing the delete key.
You will use "Do it" and "Print it" often, so memorize the keyboard shortcuts.

If the code goes into an infinite loop, break out of it by pressing cmd-period.

To inspect a variable, select it and press cmd-i (Inspect it).
This opens an inspect window that lists all of its instance variables
in the top left pane.
Clicking one displays its current value in its top right pane.
The bottom pane can be used to send messages to the object.
For example, when inspecting a morph, entering `self color: Color red`
and pressing cmd-d (Do it) changes the color.

To browse a class, enter its name and press cmd-b (Browse it).

## Transcript

This window displays output written to it.
One way to do this is to send `Transcript show: <object>`,
perhaps in a Workspace window.
This can output any kind of object
because every object has a string representation.

Another way is to use the `print` method in the `CharacterSequence` class
which is the superclasss of the `String` class.
This executes `Transcript show: self; newLine`.
For example, `'Hello World!' print`.

The `print` message can be sent to strings, symbols,
and any object that has a `printString` method.

To clear the contents of the Transcript window,
right-click in it and select "Clear Transcript" (no keyboard shortcut).

TODO: Why does this window contain the word "Transcript" in its content?

## Text Editors

"Text Editor" windows enable editing text files.
They support changing the font size, color, and style of selected text.

These are used to edit text other than Smalltalk source code.

The text can be saved in `.txt` files, but
all the formatting is discarded and only the raw text is saved.

In any text editng pane, including those in Browser windows,
right-click and select "Help..." to see a list of the supported key bindings.

To comment/uncomment selected lines of code, press cmd-".

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

## Unit Tests

To create unit tests for an existing class:

- Create a new class in the same class category as the class to be tested
  that is a subclass of `TestCase`.
  To test the class "Foo", a suggested class name is "FooTests".
  For example:

  ```smalltalk
  TestCase subclass: #VShapeTests
      instanceVariableNames: ''
      classVariableNames: ''
      poolDictionaries: ''
      category: 'Volkmann'
  ```

- Add a message category like "test" or "testing".
  The name doesn't really matter.
- Add test instance methods whose name begins with "test".
  Each method can contain any number of assertions.
  For example:

  ```smalltalk
  testCircleArea
      | c |
      c := VCircle radius: 3.
      self assert: c area isCloseTo: 28.2743339
  ```

The supported assertion method defined in the `TestCase` class include:

- `assert:` for Boolean values
- `assert:changes`
- `assert:changes:by`
- `assert:changes:from:to`
- `assert:description`
- `assert:description:resumable`
- `assert:doesNotChange`
- `assert:equals`
- `assert:includes`
- `assert:isCloseTo`
- `assert:isCloseTo:withinPrecision`
- `assert:isNotCloseTo`
- `assert:isNotCloseTo:withinPrecision`

To run tests, select a test class, test method category, or test method,
and press cmd-t (run tests).
Alternatively, open a "SUnit Test Runner" from the World menu,
select one or more test classes, and click the "Run" button.
After adding new test classes, click the "Refresh" button
to make the "SUnit Test Runner" window aware of them.

To install some example tests:

- Open a "File List" window from the World menu.
- Navigate to and expand `Cuis-Smalltalk-Dev-master`
  or the name of your version of Cuis.
- Navigate to and expand "Packages" and then "Features".
- Enter "test" in the filter input in the upper-left.
- Select one of more of the packages whose names begin with "Test-".
- Click the "install package" button.
- View the code for those packages in a System Browser window.

## Color

The `Color` class defines many methods for creating and operating on colors.
It defines the following class methods that create colors by name:

- `black`, `blue`, `brown`, `cyan`, `gray`, `green`, `magenta`,
  `orange`, `pink`, `purple`, `red`, `tan`, `white`, `yellow`
- `lightBlue`, `lightBrown`, `lightCyan`, `lightGray`, `lightGreen`, `lightMagenta`,
  `lightOrange`, `lightRed`, `lightYellow`
- `darkGray`, `veryDarkGray`, `veryLightGray`, `veryVeryDarkGray`, `veryVeryLightGray`

## Morphic

Morphic is a GUI framework that is built into popular Smalltalk images.
It defines user interfaces with "morphs" which are
what other graphical systems refer to as widgets.
They are graphical items that can be added to
the `WorldMorph` (desktop) or a `LayoutMorph`.

For a great introduction to Morphic, see
<a href="https://www.youtube.com/watch?v=62baNn3c56Y"
target="_blank">Holistic computing with Smalltalk and Morphic. Part 1</a>.

To modify an existing morph:

- Open the halo for the morph.
- Click the blue menu button on the top and select "copy to clipboard".
- Open a Workspace window.
- Assign the morph to a variable.
  For example, enter `morph := ` and press cmd-p to paste the reference.
- Press cmd-d (Do it).
- Send messages to the morph to modify it.
  For example, `morph borderColor: (Color pink)`

To create a morph:

- Click the WorldMorph background.
- Select "New morph...".
- In the dialog that appears,
  select a category of morphs and then a specific kind.

Only a small set of morphs is provided by default.
A good source of additional morphs is the package "UI-Shapes".
To install this:

- Open a terminal.
- `cd` to the directory that contains the base image being used.
  For me this is `Cuis-Smalltalk-Dev-master/CuisImage`.
- Run the command `git clone git@github.com:Cuis-Smalltalk/Cuis-Smalltalk-UI.git`.
- In Cuis, open a Workspace window.
- Enter `Feature require: 'UI-Shapes'` and press cmd-d (Do it).
- The set of "Basic" morphs will now include `BoxedMorph`, `EllipseMorph`,
  `FrameMorph`, `ImageMorph`, `LabelMorph`, `LineMorph`, `PointerLineMorph`,
  and `TileResizeMorph`.

To create and add morphs with code:
TODO: Add this.

## Halo

To open the halo (set of surrounding buttons) for an item, cmd-click it.
If the item is embedded in other morphs, cmd-click multiple times
until a halo appears around the desired morph.

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

## Deploying Applications

Smalltalk can be used to build command-line, desktop, and web applications.
After writing and testing the code in a standard Smalltalk image,
you will want to create a stripped down version of the image
that only contains what is necessary for running the application.

All developer tools can be removed.
For command-line and web applications, everything related to
the Morphic GUI framework can be removed.

Stripping an image is a manual process that is
described in the FAST 2024 conference video
<a href="https://www.youtube.com/watch?v=MfAclig5XyI"
target="_blank">Bootstrap: Creating Minimal Images from Scratch</a>.

Also see
<a href="https://www.youtube.com/watch?v=b3oGOMCjKU8&list=PLu8vLCSA-4hklsvT9W6ruintbdx_K0DYW&index=2&t=53s"
target="_blank">Make a standalone click-&-run Smalltalk application for macOS</a>.

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

array1 := #(true 7 'Tami' (Color red)).
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

## Unresolved Questions

- Does Smalltalk have an FFI for calling code written in other languages?
- How can you work with SQLite and Postgres databases from Smalltalk?
- Where are method categories stored?
- Build a Todo app using Morphic and practice stripping the image
  to create a version that can be distributed to users.
- Is there an easy way to create a memoized version of a method?
  Try this with the "collatz" method that you added to the Integer class.
- A Smalltalk image contains a set of class definitions, existing objects,
  and code that implements a GUI for the objects that are graphical.
  Anything else?
- Code can be saved in an image, a package, or a fileOut.
  Are those all the options?
- Are packages unique to Cuis Smalltalk?
- Is the only way to save changes to a package to open an
  "Installed Packages" window, select the package, and click the "save" button?
- Do any collection classes support structural sharing for immutability support?
