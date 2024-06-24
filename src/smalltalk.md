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

<a href="https://en.wikipedia.org/wiki/Simula" target="_blank">Simula</a>
is considered to be the first programming language
to use object-oriented programming (OOP).
But Smalltalk was the first programming language to make OOP popular.

Smalltalk is perhaps most known for its incredible development tools.
These support:

- finding code in many ways
- live code editing where changes are immediately reflected in the environment
- TODO: Add more here!

Smalltalk is a dynamically typed language.
Types of variables, method parameters,
and method return types are never specified.
Instead, duck typing is used. Any object can be used as long as it
is able to respond to all the messages that are sent to it.
This is determined at run-time.

Alan Kay, Dan Ingalls, and Adele Goldberg worked at
Xerox PARC (Palo Alto Research Center) in the 1970s.
All of them collaborated to create Smalltalk.
Alan Kay was the principal designer of Smalltalk and gave it its name.
The original goal was to use it for teaching programming.

Many other technologies were invented at PARC
including graphical user interfaces, the mouse, and virtual machines.

Alan Kay said "OOP to me means only messaging,
local retention and protection and hiding of state-process,
and extreme late-binding of all things."

Late binding means that messages sent to objects
are looked up for compatible methods at runtime.
However, Smalltalk editors do check for "unknown selectors" when
code is entered that sends a message to a literal object (not to a variable).

Smalltalk didn't gain much traction outside Xerox Parc
BYTE magizine published an issue focuses on Smalltalk in August 1981.
The cover, shown at the beginning of this article,
featured a colorful hot air balloon.

In the 1990's the popularity of Smalltalk had risen enough
that it was seen as a possible alternative to C++.
For a time, IBM promoted replacing COBAL with Smalltalk.
In 1995, Smalltalk (15%) was the second most popular OO language
after C++ (71%).

At the OOPSLA 1997 conference, Alan Kay said
"Actually, I made up the term 'object-oriented'
and I can tell you I did not have C++ in mind."
He also said "Languages like C++ and Java are OOP done wrong.
Smalltalk is OOP done right."

Today Smalltalk is still used by the financial industry,
manufacturers, utilities, transportation, and academia.

The entire syntax of Smalltalk can be demonstrated on a post card.

<img alt="Smalltalk on a post card"
  src="/blog/assets/smalltalk-on-postcard.jpg?v={{pkg.version}}">

## Resources

- <a href="https://en.wikipedia.org/wiki/Smalltalk"
  target="_blank">Smalltalk in Wikipedia</a>
- <a href="https://cuis.st" target="_blank">Cuis Smalltalk</a>
- <a href="https://github.com/Cuis-Smalltalk" target="_blank">Cuis GitHub repositories</a>
- <a href="https://pharo.org" target="_blank">Pharo Smalltalk</a>
- <a href="https://squeak.org" target="_blank">Squeak Smalltalk</a>
- <a href="https://github.com/Cuis-Smalltalk/Learning-Cuis/blob/master/Quick-UI-Tour.md"
  target="_blank">Quick-UI-Tour</a> for Cuis Smalltalk
- <a href="https://www.fast.org.ar"
  target="_blank">Fundación Argentina de Smalltalk</a> (FAST)
- <a href="https://www.gnu.org/software/dr-geo/" target="_blank">Dr. Geo</a>
- <a href="https://www.goodreads.com/shelf/show/smalltalk"
  target="_blank">Smalltalk Books</a> list on goodreads.
- <a href="https://www.youtube.com/playlist?list=PL6601A198DF14788D"
  target="_blank">Squeak from the very start</a>
  YouTube videos by Lawson English

  "A program to design and manipulate interactive geometric sketches.
  It helps kids to explore geometry."

There is a <a href="https://lists.cuis.st/mailman/listinfo/cuis-dev"
target="_blank">Cuis Smalltalk mailing list</a>,
but no Discord or Slack channel.

There is a <a href="https://discord.gg/43VEWSw2"
target="_blank">Discord channel channel for Squeak Smalltalk</a>.

## Smalltalk Pros

- It has a small, consistently applied syntax.
- It has a great development environment consisting of tools such as
  System Browser, Workspace, Transcript, Debugger, Hierarchy Browser,
  Method Finder and more.
- Everything is an object.
- All methods are invoked through message passing
  which supports extreme polymorphism.
  Any kind of object can be passed to a method as long as it
  responds to the messages that will be sent to it.
- It has a great web app. framework (Seaside) and a great CMS framework (Pier).
- It provides automatic version control.

## Smalltalk Cons

- It isn't as popular as many other programming languages.

  - Schools generally don't teach it.
  - Few jobs using it are available.
  - IT press doesn't talk about it.
  - It's difficult to convince others to use it.

- Help is limited.

  There are fewer developers using Smalltalk
  than many other programming languages.
  This means there are fewer people available
  to answer questions for new developers.

- Library documentation is lacking.

  Many Smalltalk libraries have little to no documentation and example code.
  There seems to be a feeling that since the source code is easily accessible,
  developers can just read that to determine how to use a library.
  This makes it difficult to get started using new libraries.

- Classes are global and not in namespaces, so all class names must be unique.

  Using class name prefixes is recommended.
  This is important for using Squeak packages and Monticello.
  Squeak has a prefix registry in the wiki.

- Immutability is not favored.

  While it is possible to define Smalltalk classes whose objects are immutable,
  this is not common.
  This will feel wrong to developers that prefer functional programming.

- Application deployment is tedious.

  Tools to strip a Smalltalk image of developer-only features
  in order to create an image that is suitable for deployment are lacking.
  This is a highly manual process.

- Smalltalks use of late binding for resolving message sends
  means that there are more errors that can only be detected at run-time
  than in statically typed languages such as C++, C#, and Java.
  However, Smalltalk does do incremental compiling when methods are saved,
  so it finds syntax errors before runtime, unlike most scripting languages.

- All the code for a project is stored in one big image file (often over 30 MB).
  However, it is common to store custom code and modifications to
  provided classes in a "file out" or package that can be
  shared with other developers and installed into fresh images.

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
    <a href="https://squeak.org" target="_blank">Squeak (2,832 classes)</a>
    <img alt="Squeak Smalltalk log" class="logo"
      src="/blog/assets/squeak-smalltalk-logo.svg?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://pharo.org" target="_blank">Pharo (10,405 classes)</a>
    <img alt="Pharo Smalltalk log" class="logo"
      src="/blog/assets/pharo-smalltalk-logo.png?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://cuis.st" target="_blank">Cuis (675 classes)</a>
    <img alt="Cuis Smalltalk log" class="logo"
      src="/blog/assets/cuis-smalltalk-logo.png?v={{pkg.version}}">
  </div>

  Juan Vuletich began work on Cuis Smalltalk began in 2005
  and version 1.0 was released in March 2009.
  He has been active in the Smalltalk community since 1997.

  The objectives of Cuis Smalltalk are to:

  - strive for the simplicity of Smalltalk-80
  - include a minimal number of classes required for the kernel,
    a GUI framework, and development tools
  - create a system that is good for learning and experimenting

  The number of predefined classes in each implementation above
  were obtained by printing the result of `Smalltalk allClasses size`
  with latest versions as of June 10, 2024.

Squeak, Pharo, and Cuis all use the MIT license.

Other Smalltalk implementations include

- <a href="https://amber-lang.net" target="_blank">Amber</a>

  This is a language that is "deeply inspired by Smalltalk".
  It compiles to JavaScript.

- <a href="https://github.com/dolphinsmalltalk" target="_blank">Dolphin Smalltalk</a>,
  only for Microsoft Windows
- <a href="https://www.gnu.org/software/smalltalk/" target="_blank">GNU Smalltalk</a>
- <a href="https://github.com/nikboyd/hoot-smalltalk#hoot-smalltalk" target="_blank">Hoot Smalltalk</a>

  This runs on the Java Virtual Machine and uses some Java features.

- <a href="https://www.cincomsmalltalk.com/main/" target="_blank">Cincom Smalltalk</a>
- <a href="https://www.instantiations.com/vast-platform/" target="_blank">VAST Platform</a>

  VAST stands for "VisualAge SmallTalk" and is provided by Instantiations.

- <a href="https://gemtalksystems.com/products/gs64/" target="_blank">GemStone/S</a>

Cuis supports TrueType fonts, whereas Squeak and Pharo do not.

The Cuis mascot is southern mountain cavy which is a "tailless rodents with
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

To update Cuis Smalltalk:

- One time, clone the GitHub repository at
  <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev"
  target="_blank">Cuis-Smalltalk-Dev</a>.
- Each time, do a `git pull` on the repository.
- Open the World menu
- Select Changes...Install New Updates.

Alternatively, start Cuis using a platform-specific shell script
which always updates to the latest version.
This scripts have a name that begins with `RunCuisOn`.

## Installing Squeak Smalltalk

- Browse <a href="https://squeak.org" target="_blank">Squeak home</a> page.
- Click the "Downloads" link.
- Click the "Link" button for your operating system.
- Double-click the downloaded file.

On macOS:

- Drag the application icon to the Finder "Applications" directory.
- Double-click the `Squeak*.app` file.

## World

The Smalltalk window or desktop is referred to as the "World"
and is implemented by the `WorldMorph` class.
Many features are available on the World menu
which is opened by clicking anywhere on the desktop.

One way to change the desktop color is to right-click it to open its halo,
click the blue menu button, select "debug...inspect morph",
click in the bottom pane, enter `self color: Color {some-color-name}`,
and press cmd-d (Do it).
This works with all colors except `transparent` and alpha values are ignored.

## Font Size

To adjust the font size:

- Open the World menu.
- Click "appearance...".
- Click "system fonts...".
- Click "increase font size" or "decrease font size".

## Full Screen

You will likely want to work in full screen mode.
To enable this, open the World menu and select Preferences ... Full screen on.
To disable this, select Preferences ... Full screen on.

The following class defines a class method `buttons` that displays buttons
which make it easier to toggle between full screen and windowed modes.

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

## Optional Packages

The shell script `clonePackageRepos.sh` clones many
commonly used Git repositories that define optional packages.
Cloning those enables installing the packages they define.

The repositories that this clones include:

- AMQP
- AnimatedGIF
- Cairo
- Calendars
- CodeExamples
- Cuis-Smalltalk
- Cuis-Smalltalk-Historical
- Cuis-Smalltalk-Regex
- Cuis-Smalltalk-Tools
- Cuis-Smalltalk-UI
- Cuis-Website
- EnhancedText
- Erudite
- firmata
- Games
- GeographicInformationSystems
- Learning-Cuis
- Machine-Learning
- Measures
- Morphic
- Numerics
- OSProcess
- Parsers
- StyledTextEditor
- SVG
- VMMaker

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
   For more options, see the "Transcript" section below.
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

## Other Preferences

Many supported preferences are not directly on the menu
accessed by opening a World menu and selecting Preferences.
To access more preferences, click "All preferences..."
which opens a Preferences window.
Click a preference symbol in the left pane
to display its current value in the right pane.

To change the value of a preference:

- Select it in the left pane.
- Press cmd-i (inspect) to open an Inspect window.
- In the bottom pane, enter `value := {new-value}` and "Do it".
- Close the Inspect window.
- Close the Preferences window.

NOTE: Changing `#prettyPrintRectangularBlocks` to `true`
did not cause saving chnages to method code to be formatted.

## Images

A Smalltalk image contains the definitions of system-provided classes,
your classes, and objects that have been created,
including those that represent all open windows.

An image can be used to manage collections of data,
perhaps held in `Dictionary` objects, as an alternative to using a database.

One way to start an image is to double-click its file.
Once started, to discover the directory where it is stored,
enter `DirectoryEntry smalltalkImageDirectory` in a workspace and "Print it".

To save any changes, include open windows, their position and size,
and their content (ex. Workspaces),
open the World menu and select "Save Image",
"Save Image as", or "Save Image and Quit".

To quit without saving changes,
open the World menu and select "Quit without saving".

While the classes and methods provided by a base image can be modified,
it is not a good idea to do so because
there won't be a good way to apply those changes to a fresh image.

It is better to create new subclasses of provided classes that
override methods and save the new subclasses in a new package or "file out".
Doing this enables installing the new package in a fresh image.

To see the file path for the currently running image,
select Help...About this System... or
hover over the task bar at the bottom of the World window.
This opens a Text Editor window that displays
basic information about Cuis Smalltalk.
It also opens a dialog that displays the current version of Cuis Smalltalk,
the latest update number, and the file page to the current image.

## Help

The World menu contains a Help submenu which contains the following:

- About this System ...
- Terse Guide to Cuis
- Class Comment Browser
- Code management in Cuis
- Using GitHub to host Cuis packages
- Editor keyboard shortcuts
- Useful Expressions
- VM Statistics
- Space Left

### Class Comment Browser

Selecting Help ... Class Comment Browser from the World menu the first time
will display a message explaining that it requires cloning the
<a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Tools"
target="_blank">Cuis-Smalltalk-Tools</a> repository.
After this is done, selecting this menu item again
will ask for permission to install the tool.
After this is done, selecting this menu item again
will open a Class Comment Browser.

The left pane displays an alphabetical list of class names
where each name is preceded by a disclosure triangle.
Clicking a class name displays its first comment in the right pane.
Clicking a disclosure triangle expands the class to show a
numbered list of comments found in the source code for the class.
Clicking any of those displays the comment text in the right pane.

It's probably more useful to open a System Browser,
find a class of interest, an view the comments there.

### Terse Guide

An excellent source of help is the "Terse Guide".
To open it, open the World menu and select Help...Terse Guide to Cuis.
When prompted "The Terse Guide is not loaded.
Would you like me to load it for you now?", select "Yes".
The following window will open.

<img alt="Cuis Terse Guide"
  src="/blog/assets/cuis-terse-guide.jpg?v={{pkg.version}}">

Click a topic to see example code.
Select code and "Do it" or "Print it" to experiment.
Modify the code as desired.
Changes will not be saved, so it is safe to experiment.

## Themes

Colors and other features of the Cuis Smalltalk UI
are determined by selecting a theme.
By default, only two themes are provided, "BrightColorTheme" and "DarkTheme".
To add more, open the World menu, select Preferences...Themes...,
and select "\* Load Additional Themes \*".
This adds the themes "ClassicTheme", "DarkBlueTheme", "DarkColorTheme",
"HighContractBlackTheme", "HighContrastWhiteTheme", "LightBluetheme",
"LightGrayTheme", "LightTheme", and "PersonalizedTheme".
Select one of these to switch to that theme.

See this <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/issues/283"
target="_blank">issue</a> related to PersonalizedTheme.

Open windows will not be correctly updated after selecting a new theme.
Close them and open new windows to get the intended styling.

To customize the current theme, open the World menu
and select Preferences...Theme Customizer...
Alternatively you can:

- Open a System Browser.
- Select the "Graphics - Themes" class category.
- Select the `Theme` class.
- Select the `colors` message category.
- Select one of the methods such as `buttonLabel`.
- Change the color returned from its default value to a new value.

This will affect all morphs that use that theme property.
For example, the `PluggableButtonMorph` method `drawEmbossedLabelOn`
uses `Theme current buttonLabel`.
This method could be modified to enable
specifying a different label color for specific buttons.

## Windows

You will be opening and using many windows.
To open one, open the World menu and
select a window type from the "Open" submenu.

To close one, click its red circle button in the upper left
or move the mouse cursor over the window and press cmd-w.

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

Other windows are only open for a specific object.

To tile all the open windows, open the World menu
and select Windows...Tile open windows.

To refresh all the windows after code changes that affect them
(or if the display renders incorrectly for some reason),
open the World menu and select "Windows...Restore all Windows".

TODO: Are other options in the Windows menu useful?

### Editing Code

Many kinds of windows support entering Smalltalk code.
Syntax highlighting is provided.

| Token Type        | Styling        |
| ----------------- | -------------- |
| class name        | black and bold |
| comment           | green          |
| instance variable | purple         |
| keyword           | red            |
| message name      | blue           |
| string            | purple         |
| symbol            | blue and bold  |

By default, class names are black and bold,
message names are blue, and symbols are blue and bold.

In any text editng pane, right-click and select "Help..."
to see a list of the supported key bindings.

To toggle surrounding selected text with a given delimiter character,
press cmd and the starting delimiter character
which can be `'`, `"`, `(`, `[`, or `{`.
Pressing cmd-" is useful to toggle whether selected code commented/uncommented.

To change the indentation of a block of code, select all the lines and
press tab to increase indentation or shift-tab to decrease it.

### Workspace Windows

Workspace windows enable experimenting with code.
They are somewhat like REPLs in other programming languages.

Enter any number of expressions separated by periods.

To prepare to execute lines of code, select them or
place the cursor at the end of a single-line expression.
To execute them for their side effects, press cmd-d (Do it).
To execute them and print the result of the last expression
inside the Workspace, press cmd-p (Print it).
Output from "Print it" will be selected,
so it can be removed by pressing the delete key.
You will use "Do it" and "Print it" often, so memorize their keyboard shortcuts.

If the code goes into an infinite loop, break out of it by pressing cmd-period.

To browse a class, enter its name and press cmd-b (Browse it).

<img alt="Cuis Workspace window" style="width: 80%"
  src="/blog/assets/cuis-workspace-window.png?v={{pkg.version}}">

### Transcript Windows

This is a read-only window displays output written to it.

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

An even better approach is to add the following method to the `Object class:

```smalltalk
logAs: aString
    ('{1} = {2}' format: {aString. self}) print
```

For example, this can be used to print a `Dog` object as follows:

```smalltalk
myDog logAs: 'myDog'
```

When output is sent to the `Transcript`, it appears in all `Transcript` windows.
So it doesn't make sense to open more than one.

To clear the contents of the Transcript window,
right-click in it and select "Clear Transcript" (no keyboard shortcut).
If there is more than one `Transcript` window, all of them will be cleared.

TODO: Why does this window contain the word "Transcript" in its content?

<img alt="Cuis Transcript window" style="width: 80%"
  src="/blog/assets/cuis-transcript-window.png?v={{pkg.version}}">

### Inspect Windows

This displays all the instance variables of a specific object.
Select an object reference or place the cursor immediately after it
and press cmd-i (Inpect it).

<img alt="Cuis Inspect window" style="width: 40%"
  src="/blog/assets/cuis-inspect-window.png?v={{pkg.version}}">

Click the name of an instance variable in the top left pane
to displays its current value in the top right pane.

Use the bottom pane to enter and execute Smalltalk expressions
where `self` refers to the object being inspected.
For example, when the object is a morph,
enter `self color: Color red` and press cmd-d (Do it),
or enter `self color` and press cmd-p (Print it).

Instance variables can be directly set in the bottom pane
using assignment instead of sending a message.
For example: `color := Color red`.

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
1. Enter `self color: Color red` and press cmd-d (Do it).
1. Note that the fill color of the morph changes to red.

### Explore Windows

This displays an object tree starting at a specific object.
Select an object reference or place the cursor immediately after it
and press cmd-shift-i (Explore it).
Click the disclosure triangles to drill down into instance variable values.

Use the bottom pane to enter and execute Smalltalk expressions
where `self` refers to the selected object in the top pane.

<img alt="Cuis Explore window" style="width: 40%"
  src="/blog/assets/cuis-explore-window.png?v={{pkg.version}}">

### System Browsers

System Browsers contain four rows.

- The first (top) row contains four panes for displaying and operating on
  lists of class categories, classes, message categories, and methods.
  Clicking an item toggles whether it is selected.

  Selecting a class category in the first pane
  displays the classes in that category in the second pane.
  For example, the class `String` is in the class category `Text`.

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

  The items in these panes are not sorted alphabetically by default.
  To sort them, hover over a pane and press cmd-shift-a (alphabetize)

- The second row displays a message describing the item selected in the top row.

- The third row contains a series of buttons that can be clicked to
  open other windows that show information related to the selected item.
  One exception is the "show..." button, described below.

  The "browse" button opens a new System Browser focused on
  the currently selected class, message category, or method.
  This enables maintaining the view in the current System Browser and
  navigating somewhere relative to that location in a new System Browser.

  The "senders" button ... TODO

  The "implementors" button ... TODO

  The "versions" button opens a "Recent versions" window (a.k.a Versions Browser)
  that displays a list of time stamps for recent versions of the method.
  Clicking a time stamp displays that version of the code in the bottom pane.
  If one of the buttons lineDiffs, wordDiffs,
  linePrettyDiffs, or wordPrettyDiffs is pressed, it will show
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

  The "inst vars" button displays a drop-down list containing
  all the instance variables in the current class.
  Clicking one of them opens a new "Accesses to" window that
  lists all the methods that use the instance variable it its top pane.
  Clicking on a method displays its code in the bottom pane.

  The "class vars" button displays a drop-down list containing
  all the class variables in the current class.
  Clicking one of them opens a new "Users of" window that
  lists all the methods that use the class variable in its top pane.
  Clicking on a method displays its code in the bottom pane (fourth row).

  The "show..." button displays a drop-down list
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

  Selecting "lineDiffs" displays ...

  Selecting "lineDiffs" displays ...

  Selecting "wordDiffs" displays ...

  Selecting "linePrettyDiffs" displays ...

  Selecting "wordPrettyDiffs" displays ...

  Selecting "decompile" displays code that is similar to the source code,
  but variable names are changed.
  Parameter variable names are replaced by `arg1`, `arg2`, and so on.
  Local variable names are replaced by `temp1`, `temp2`, and so on.
  It's not clear when this would be useful.

  Selecting "byteCodes" displays the byte codes
  generated by the method source code.

- The fourth row displays information about the selected item
  based on the checkbox that is selected for the "show..." button.
  By default it displays Smalltalk code for the selected item
  and can be used to edit the code.
  When there are unsaved code changes in this pane,
  a thin, red border appears around it.
  Press cmd-s (Accept) to save the changes
  and the thin, red border will disappear.

To automatically format code on save,
enter the following in a Workspace and "Do it":
`Preferences at: #browseWithPrettyPrint put: true`.

To open a System Browser, click on the `WorldMorph` background,
select Open, and select Browser.

To search for a class by part of its name,
click in the class category pane and press cmd-f (find class...).
Then enter part of a class name and press return.
A popup list of matching classes will appear.
Click one of the names to browse that class.

<img alt="Cuis System Browser" style="width: 100%"
  src="/blog/assets/cuis-system-browser-window.png?v={{pkg.version}}">

To browse a class, type its name (ex. String) in a Workspace window
and press cmd-b (Browse it).
This opens a System Browser with the class already selected.

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
      category: 'SelectedClassCategory'
  ```

- Change "NameOfSubclass" to the name of the new class.
- Add desired instance and class variable names as space-separated strings.
- Save by pressing cmd-s (Accept).

To create a new method:

- Click the "instance" or "class" button to indicate the scope of the method.
- Select a method category.

  This can be "-- all --", "as yet unclassified", or any other method category.
  To create new category, move the mouse cursor over the method category pane
  and press cmd-n (new category...).

  The bottom pane will now contain the following:

  ```smalltalk
  messageSelectorAndArgumentNames
      "comment stating purpose of message"

      | temporary variable names |
      statements
  ```

- Modify the code template to define the new method.
- Save by pressing cmd-s (Accept).

To move a method from the class side to the instance side or vice-versa,
right-click the method name in the fourth panel and
select refactorings...move to instance/class methods.

To delete a method from a class, select it and press cmd-x (Remove it).
Then select "Remove it" or "Remove, then browse senders".
The latter option allows the senders to be modified.

When the mouse cursor is over any of the four lists at the top, typing a letter
causes the list to scroll to the first item that begins with that letter.

There is no provided way to search for code that contains a given string.
Mariano Montone implemented this and shared a change set via email.
See the file `SearchSourceMenus-MarianoMontone.cs.st`.
Location this in a "File List" window and click the "install" button.
This adds the context menu item "search source...".

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-the-system-Browser.html"
target="_blank">A brief introduction to the System Browser</a>.

### Hierarchy Browsers

When a class is selected in a System Browser,
the class pane (second) only displays a list of classes
defined in the selected class category.
To also see class hierarchy of a selected class,
open a Hierarchy Browser by pressing cmd-h (browse hierarchy).

This window is similar to a System Browser,
but it omits the class categories pane and
displays the complete class hierarchy of the selected class.

Here's an example of a Hierarchy Browser for the `Array` class:

<img alt="Cuis Hierarchy window" style="width: 40%"
  src="/blog/assets/cuis-hierarchy-window.png?v={{pkg.version}}">

### Protocol Browsers

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

### Text Editor Windows

"Text Editor" windows enable editing text files.
They support changing the font size, color, and style of selected text.

These are used to edit text other than Smalltalk source code.

The text can be saved in `.txt` files, but
all the formatting is discarded and only the raw text is saved.

### Message Names Windows

These windows enable searching for implementors
of methods whose name contains a given substring.
For example, enter "select:" to find all the classes
that have a method whose names end with that.
Those include `Bag`, `Collection`, `Dictionary`, `Heap`,
`OrderedCollection`, `SequenceableCollection`, and `SortedCollection`.
Click one the class names to see the method implementation.

<img alt="Cuis Smalltalk Protocol window" style="width: 100%"
  src="/blog/assets/cuis-protocol-window.png?v={{pkg.version}}">

### MessageNotUnderstood Windows

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

<img alt="Cuis MessageNotUnderstood window" style="width: 85%"
  src="/blog/assets/cuis-messagenotunderstood-window.png?v={{pkg.version}}">

### Debug Windows

To debug code, select one or more lines in a Workspace window
and press cmd-shift-d (Debug it).
A Debug window will appear.

<img alt="Cuis Debug window" style="width: 100%"
  src="/blog/assets/cuis-debug-window.png?v={{pkg.version}}">

Click the "Into" button to begin executing the code.
The "Proceed", "Restart", "Into", "Over" buttons
function as expected if you have used other debuggers.

To run up to a specific location in the code,
click to place the cursor where execution should stop
and click the "Run to Cursor" button.

The in-scope variables are listed in the third panel of the bottom row.
Click a variable name to see its current value
in the fourth panel of the bottom row.
To change the value of a variable, edit it where displayed
and press cmd-s (Accept).

Click the "Where" button to highlight the next message to be sent in the code.

The Debug window will close when the end of the selected code is reached.

### Change Sorter Windows

TODO: Explain how to use these.

### Process Browsers

Process Browsers display a list of all the Smalltalk-related processes
that are running.
By default the list updates automatically and processes come and go.
To toggle that, press cmd-a (turn off/on auto-update).

To terminate a process, select it and press cmd-t (terminate).
This is especially useful for terminating "WebServer's listening process".

<img alt="Cuis Process Browser" style="width: 85%"
  src="/blog/assets/cuis-process-browser-window.png?v={{pkg.version}}">

For example, the following code starts a process that
writes to the Transscript every five seconds.

```smalltalk
block := [[true] whileTrue: [
    'Hello' print.
    (Delay forSeconds: 5) wait
]].
block forkAt: Processor userBackgroundPriority named: 'hello'.
```

The name of the process will be "hello".
To stop it, right-click the process, select it and press cmd-t (terminate).

### Emergency Evaluator

The Emergency Evaluator can be useful to save work
when the current session becomes somewhat unusable.

<img alt="Cuis Emergency Evaluator" style="width: 30%"
  src="/blog/assets/cuis-emergency-evaluator.png?v={{pkg.version}}">

TODO: Explain when this is useful.

## Syntax

| Item                                              | Example                                          |
| ------------------------------------------------- | ------------------------------------------------ |
| comment                                           | `"some text"`                                    |
| temporary (local) variable (private scope)        | `myTemp` (camelCase)                             |
| global variable (shared scope)                    | `MyGlobal` (CamelCase)                           |
| pseudo variable (cannot assign)                   | `self`                                           |
| integer                                           | `123`                                            |
| float                                             | `3.14`                                           |
| exponential notation                              | `1.23e4`                                         |
| character                                         | `$a`                                             |
| string                                            | `'text'` (double ' to include)                   |
| string and array concatenation (comma message)    | `'foo', 'bar', 'baz'` or `#(1 2), #(3 4)`        |
| symbol (globally unique string)                   | `#name`                                          |
| static array (elements are literal values)        | `#(1 4 8)`                                       |
| dynamic array (elements are computed at run time) | `{1. 2 * 2. 2 raisedTo: 3}`                      |
| assignment                                        | `<variable> := <expression>.`                    |
| method variable declarations                      | `\| foo bar baz \|`                              |
| block with no arguments                           | `[ <expressions> ]`                              |
| block with arguments                              | `[:a :b \| a + b]`                               |
| unary message send                                | `<object> <message>`                             |
| binary message send (look like operators)         | `4 * 5`                                          |
| keyword message send                              | `2 raisedTo: 4 modulo: 3`                        |
| message cascade - sends to initial receiver       | `Transcript show: 'foo'; newLine; show: 'bar'`   |
| message chaining - sends to previous result       | `2 * 3 :: squared :: negated` (-36)              |
| method return value                               | `^<expression>`                                  |
| expression separator (period)                     | `Transcript show: 'foo'. Transcript show: 'bar'` |
| reference to current object in a method           | `self`                                           |

To render a left pointing arrow in place of `:=` for all assigments,
open the World menu and select Preferences...Show ST-80 Assignments.
The next time code is modified, all the `:=` messages
will be rendered by a left pointing arrow.
Typing an underscore is a shorthand way to type `:=`.
Typing either will be rendered as a left pointing arrow.
This does not change the characters that are actually used.
To revert to rendering `:=` messages,
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
Typically a common prefix, perhaps 2 or 3 uppercase letters,
is added to a set of related class names in order to make the unique.
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

### Accessor Methods

Instance variables are never accessed directly
from outside of the class that defines them.
To expose their values, write getter methods.
To allow them to be modified, write setter methods.

Suppose a class Dog has the instance variable `breed`.
The following accessor methods can be implemented:

```smalltalk
breed
    ^breed

breed: aString
    breed := aString
```

Accessor methods for all instance variables in a class can be generated
by right-clicking the class name in a System Browser
and selecting "more...create inst var accessors".

### Objects

In Smalltalk, code and data are both represented by objects.
Code can be described by a method or block, both of which are kinds of objects.

Objects are created by sending a message to a class.
In addition, some kinds of objects can be created from a literal syntax
such as numbers, strings, and arrays.

Every class supports the class method `new`,
which creates and returns a new instance of the class.
If the class defines the instance method `initialize`,
the `new` method will call it.
The `initialize` method typically
initializes the instance variables of the object.

Every class also supports the class method `basicNew` which is similar to
the `new` method, but does not call the instance method `initialize`.

Let's look at an example `Rect` class
with instance variables `height` and `width`.

We can define the class method `height:width:`
that provides an alternate way to create objects as follows.
This assumes we can send the message `#setHeight:width:`
(in the `private` message category) to instances.

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth
```

We can then define the following instance methods:

```smalltalk
initialize
    height := 1.
    width := 1

setHeight: aHeight width: aWidth
    height := aHeight.
    width := aWidth

area
    ^height * width
```

We can use the `Rectangle` class as follows:

```smalltalk
r1 := Rect new.
Transcript show: r1 area. "1"
r2 := Rect height: 2 width: 3.
Transcript show: r2 area. "6"
```

To determine the class of an object, send it the `class` unary message.
For example, `19 class` returns `SmallInteger`.

Variables defined in Workspace windows hold references to their object values.
It may be necessary to close a Workspace window
in order to trigger garbage collection of those objects.

### Immutability

To enforce immutability of objects:

- Enter `Feature require: 'Immutability' and "Do it".

  This package can be found at
  <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Packages/System/Immutability.pck.st"
  target="_blank">Immutability.pck.st</a>. Among other things,
  it adds the instance method `beImmutable` to the`Object` class.

- Send the `beImmutable` message to any object.

  For example:

  ```smalltalk
  height: aHeight width: aWidth
      ^self new setHeight: aHeight width: aWidth; beImmutable; yourself
  ```

  Note the use of `yourself` to return the current object
  rather than the return value of the `beImmutable` method.

If an attempt is made to modify any property of an immutable object,
a "ModificationForbidden" window will open
containing a stack trace that indicates where the attempt was made.

### Methods

Methods are associated with a specific class.
Instance methods handle messages sent to objects instantiated from the class.
Class methods handle messages sent to the class.

In keyword methods, parameter variable names typically
indicate the expected object type and begin with "a" or "an".
For example, `aNumber`, `aString`, or `anArray`.
This works well because the keyword that
precedes the parameter variable indicates its meaning.
For example, `name: aString score: aNumber`.

When multiple parameters have the same data type, a good way to
name them is to include their meaning and type in the name.
For example, `latitude: latNumber longitude: lngNumber`.

Methods that return an object are said to "answer" a value.
For example, the instance method `asUppercase` in the `String` class
contains the comment "Answer a String made up from
the receiver whose characters are all uppercase."

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
- Update the list of temporary (local) variable names or delete that line.
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
    ^self new setRadius: aNumber
```

The `VCircle` class can have the following instance methods:

```smalltalk
setRadius: aNumber
    radius := aNumber

area
    ^Float pi * radius * radius
```

The `VRectangle` class can have the following class method for creating instances:

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth
```

The `VRectangle` class can have the following instance methods:

```smalltalk
setHeight: aHeight width: aWidth
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

### Primitive Methods

From the Blue Book ...

> All behavior in the system is invoked by messages, however,
> all messages are not responded to by executing Smalltalk-80 methods.
> There are about one hundred primitive methods that
> the Smalltalk-80 virtual machine knows how to perform.
> Examples of messages that invoke primitives are
> the `+` message to small integers,
> the `at:` message to objects with indexed instance variables,
> and the `new` and `new:` messages to classes.
> When `3` gets the message `+ 4`, it does not execute a Smalltalk-80 method.
> A primitive method returns `7` as the value of the message.
> The complete set of primitive methods is included in the
> fourth part of this book, which degcribes the virtual machine.
> Methods that are implemented as primitive methods begin with an
> expression of the form `<primitive #>` where `#` is an integer
> indicating which primitive method will be followed.
> If the primitive fails to perform correctly,
> execution continues in the Smalltalk-80 method.
> The expression `<primitive #>` is followed by
> Smalltalk-80 expressions that handle failure situations.

In Cuis Smalltalk, see the comment at the beginning of
the class method `whatIsAPrimitive` in the `Object` class.
It contains the following:

> When the Smalltalk interpreter begins to execute a method which specifies a
> primitive response, it tries to perform the primitive action and to return a
> result. If the routine in the interpreter for this primitive is successful,
> it will return a value and the expressions in the method will not be evaluated.
> If the primitive routine is not successful, the primitive 'fails', and the
> Smalltalk expressions in the method are executed instead. These
> expressions are evaluated as though the primitive routine had not been
> called.

## Refactorings

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

## Reflection

Smalltalk provides many methods for
getting information about classes and objects.
The following table lists some of them.

| Method                                                  | Answers                                                                                                      |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `Smalltalk allClasses`                                  | an `Array` of all classes defined in the current image                                                       |
| `Smalltalk allClassesImplementing: #selector`           | an `Array` of all classes that implement a given selector                                                    |
| `SomeClass allClassVarNames`                            | a `Set` of class variable names defined in this class                                                        |
| `SomeClass allSelectors`                                | an `IdentitySet` of all message selectors supported by this class, including selectors for inherited methods |
| `SomeClass allInstances`                                | an `Array` of all existing instances of this class                                                           |
| `SomeClass allInstVarNames`                             | an `Array` of instance variable names defined in this class                                                  |
| `SomeClass allInstVarNamesEverywhere`                   | an `Array` of instance variable names defined in this class and inherited classes                            |
| `SomeClass allMethodsInCategory: 'some-category'`       | an `Array` of instance methods in a given category, including those defined in this class and inherited      |
| `SomeClass class allMethodsInCategory: 'some-category'` | an `Array` of class methods in a given category, including those defined in this class and inherited         |
| `SomeClass allSubclasses`                               | an `OrderedCollection` of subclasses                                                                         |
| `SomeClass allSuperclasses`                             | an `OrderedCollection` of superclasses                                                                       |
| `CodeListPackages installedPackages`                    | an `Array` of `CodePackage` objects (appear in System Browser class category pane)                           |

TODO: Why does `allClassVarNames` return a `Set` when `allInstVarNames` returns an `Array`?
TODO: Is there a way to get all the message categories used by a class?

To run code on every instance of a given class,
send the `allInstancesDo:` message to the class.

For example, to delete all instances of a given class, run
`SomeClass allInstancesDo: [ :obj | obj delete ]`.

## Code Management

Selecting Help ... Code management in Cuis from the World menu
opens a window titled "Managing your code in Cuis".
That offers the following advice:

> Code that is not part of the Cuis Core image itself,
> like applications, frameworks and libraries, should be stored in Packages.
> New code that are meant as patches, fixes or additions;
> that could eventually become part of Cuis itself, is not part of any Package,
> and is therefore automatically stored in ChangeSets.

### File List

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

### File Out and File In

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

These files use the bang-separated "chunked format".
The following is an example for a `Dog` class.
Each chunk is delimited by exclamation marks.
A chunk can contain:

- a "From" line the gives the version of Smalltalk that create the file
  and the date and time at which the file was created
- a "classDefinition" that associates a class or metaclass with a class category
- a "subclass: message send that creates a class
- a "methodsFor" which states that the method definitions
  that follow are in a given method category
- a method definition
- a message send that creates an object that should exist in the environment
- a message send that executes code for its side effects

The following is an example of a fileOut file.

```smalltalk
'From Cuis7.1 [latest update: #6464] on 12 June 2024 at 10:47:55 am'!
!classDefinition: #Dog category: #Volkmann!
Object subclass: #Dog
    instanceVariableNames: 'breed name'
    classVariableNames: 'Count'
    poolDictionaries: ''
    category: 'Volkmann'!

!Dog methodsFor: 'initialization' stamp: 'RMV 6/12/2024 10:47:34'!
initialize
    super initialize.
    Count := Count + 1! !

!Dog methodsFor: 'initialization' stamp: 'RMV 6/11/2024 20:00:43'!
setName: aName breed: aBreed
    name := aName.
    breed := aBreed! !


!Dog methodsFor: 'accessing' stamp: 'RMV 6/11/2024 20:03:03'!
breed
    ^breed! !

!Dog methodsFor: 'accessing' stamp: 'RMV 6/11/2024 20:02:57'!
name
    ^name! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Dog class' category: #Volkmann!
Dog class
    instanceVariableNames: ''!

!Dog class methodsFor: 'accessing' stamp: 'RMV 6/12/2024 09:45:13'!
count
    ^Count! !


!Dog class methodsFor: 'initialization' stamp: 'RMV 6/12/2024 10:41:45'!
initialize
    "This must be explicitly called with Dog initialize."
    Count := 0! !

!Dog class methodsFor: 'initialization' stamp: 'RMV 6/12/2024 10:43:52'!
name: aName breed: aBreed
    ^self new setName: aName breed: aBreed! !


Dog initialize!
```

To read all the code for a package from a text file:

- Open a File List.
- Locate and select a `.st` file created by a "fileOut".
- Right-click and select "fileIn".
- Enter your initials and then your name
  for tracking who performed the "fileIn".
- All the class categories, classes, and methods defined in the file
  will now be available in the current image.

### Packages

Cuis Smalltalk supports the ability to save code outside an image file
and load it into running images.
This is an alternative to Monticello which is used in Squeak and Pharo.

Packages are collections of Smalltalk code
stored in files with a `.pck.st` file.

Package names are used as prefixes on class and method categories names.

The GitHub account "Cuis-Smalltalk" provides many package repositories,
32 as of June 2024.
Sadly the documentation included in the `README.md` files of these packages
is quite sparse.
These repositories must be cloned in order to install them.

For additional packages, search GitHub for
repositories whose names begin with "Cuis-Smalltalk-".

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

Let's learn where the `Feature require:` method searches for packages.

1. Browse the `Feature` class.
1. Click the `require` method on the class side.
1. Note that this sends the `#name:` message to the `FeatureRequirement` class
   to create an instance and then sends it the `#require` message.
1. Select the `FeatureRequirement` class.
1. Select the `require` method on the instance side.
1. Note that this sends the
   `#requireUnlessIn:main:requiringFeature:allRequiringFeatures:` message
   to `self`.
1. Select that method on the instance side.
1. Note that this sends the `#findPackageFileAsReqOf:` message to `self`.
1. Select that method on the instance side.
1. Note that this sends the `#placesToLookForPackagesDo:` message to `self`.
1. Select that method on the instance side.
1. Note the following comments in this code:

   - "Look inside my own folder"
   - "Look in codePackageFile folder"
   - "Packages that come included with Cuis"
   - "Packages created by user"
   - "Packages in other folders or repos in the project directory"

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

### Adding and Saving Code

Create a new package for your code as described above.
While still in the "Installed Packages" window,
select the package and click "browse"
to open a System Browser for the package.

## Messages

The only mechanism for communication between objects
is for one to send a message to another.
Messages are always sent to a explicit receiver,
which is `self` to send a message to the current object.
Arguments in messages are always passed by reference, not by value.

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

When entering code to send a message, completion hints are provided
if at least the first letter in the message name is typed
and the tab key is pressed.
For example, entering `7 s` and pressing the tab key
shows possible completions of `shallowCopy`, `sqrt`, and more.
Use the up and down arrow keys to select a completion
and press the return key to accept it.

To enable completions without typing any characters, enter
`Preferences at: #spaceTabOpensAutoCompletion put: true`
in a Workspace window and "Do it".
For example, with this preference set, you can enter `'test'`
followed by a space and press the tab key to get completion hints.

Matching messages found anywhere in the inheritance hierarchy appear in black.
If there are no matching messages,
it will show all known selectors that match in any class in blue.
The reason is that you can send any message to any object.
Even if the object has no matching method anywhere in its inheritance hierarchy,
it could still respond by handling it in `doesNotUnderstand`.
I'll go on record saying that I do not find this helpful.
I wish it did not show those messages.

If a message is sent to an object from a Workspace window
and no compatible method is found, the following popup will appear:

<img alt="Unknown Selector popup" style="width: 50%"
  src="/blog/assets/smalltalk-unknown-selector.png?v={{pkg.version}}">

If the selector was incorrectly typed,
an implemented selector can be selected from this popup.

If the selector is confirmed or if such a message is sent from runnning code,
the following window will appear:

<img alt="MessageNotUnderstood window"
  src="/blog/assets/smalltalk-message-not-understood.png?v={{pkg.version}}">

To implement the missing method, click the "Create" button.
A popup will appear to prompt for the class within the inheritance hierarchy
of the object where the new method should be added.
After selecting a class, a second popup will appear to prompt for
the method category to which the new method should be associated.
The method can be implemented inside the "MessageNotUnderstood" window.
Initially it will just contain `self shouldBeImplemented`.
Replace that with the real implementation, which in this case is `^self * 3`,
and press cmd-s to save the change.

### Other Ways To Send Messages

The `#perform:` message and its variations can be sent to any class or object
to send a message specified by the symbol that follows `perform:`.
This is useful in sitations where the message to send
needs to be determined at run-time.

Another alternative it to use the `MessageSend` class.
The class methods `:receiver:selector`, `:receiver:selector:argument:`, and
`:receiver:selector:arguments:` return an object that describes a message send.
The object can be passed around and the actual send can be triggered later.

For example, the following sets of expressions are equivalent:

```smalltalk
"These demostrates sending a unary message. Each gives the result 2."
4 sqrt.
4 perform: #sqrt.
(MessageSend receiver: 4 selector: #sqrt) value.

"These demonstrate sending a binary message. Each gives the result 6."
2 * 3.
2 perform: #* with: 3.
(MessageSend receiver: 2 selector: #* argument: 3) value.

"These demonstrate sending a keyword message. Each gives the result 4."
'foobarbaz' findString: 'BAR' startingAt: 1 caseSensitive: false.
'foobarbaz'
    perform: #findString:startingAt:caseSensitive:
    with: 'BAR' with: 1 with: false.
(MessageSend
    receiver: 'foobarbaz'
    selector: #findString:startingAt:caseSensitive:
    arguments: #('BAR' 1 false)
) value.
```

To provide more than three keyword arguments, use `#perform:withArguments`.

## Reserved Words

There are only six reserved words in Smalltalk which are
`true`, `false`, `nil`, `self`, `super`, and `thisContext`.

From the
<a href="https://cuis-smalltalk.github.io/TheCuisBook/Pseudo_002dvariables.html"
target="_blank">Cuis book</a>, "`thisContext` ...
represents the top frame of the run-time stack. ...
It is essential for implementing development tools like the Debugger and
it is also used to implement exception handling and continuations."

## Control Flow

Control flow is provided through message passing.

The `Boolean` class in the `Kernel:Objects` category contains the methods
`#ifTrue`, `#ifFalse`, `#ifTrue:ifFalse`, and `#ifFalse:ifTrue`.
For example:

```smalltalk
result := a < b ifTrue: ['less'] ifFalse: ['more'].
```

The values for `ifTrue` and `ifFalse` can be
literal values, variables, or blocks with no parameters.
Those messages just send the `value` message to the argument value.
Typically that is used to evaluate a no-arg block.
But the `Object` class defines the `value` method to just return `self`.

When blocks are used, the compiler is able to optimized the code by
inlining the code within the block and
avoiding the need to send the `value` message.
So it is more efficient to use blocks.

The `Object` instance method `caseOf` is similar to
the `switch` statement in other programming languages.

For example:

```smalltalk
color := 'blue'.
assessment := color caseOf: {
    ['red'] -> ['hot'].
    ['green'] -> ['warm'].
    ['blue'] -> ['cold']
}
```

## Data Types

### UndefinedObject

The reserved word `nil` refers to
an instance of the `UndefinedObject` class.
No additional instances can be created.
This is prevented by overriding the class method `new`
in the `UndefinedObject` class.

### Booleans

The reserved words `true` and `false` refer to
instances of the classes `True` and `False`
which are subclasses of the class `Boolean`.

`True` and `False` are singleton classes.
No additional instances can be created.
This is prevented by overriding the class method `new`
in the `Boolean` class.

Representing the values `true` and `false` by distinct classes
simplifies the implementation of many of their methods.
For example, here are the implementations of the
`&` and `ifTrue:` instance methods in the `True` class.

```smalltalk
& alternativeObject
    ^alternativeObject

ifTrue: alternativeBlock
    ^alternativeBlock value
```

## Numbers

The following list depicts the class hierarchy for numbers:

- `Number`
  - `Float`
    - `BoxedFloat64`
    - `SmallFloat64`
  - `Fraction`
    - `Integer`
      - `LargePositiveInteger`
        - `LargeNegativeInteger`
      - `SmallInteger`

Literal numbers without a decimal point automatically become
objects of one of the `Integer` subclasses.

Literal numbers with a decimal point automatically become
objects of one of the `Float` subclasses.

The assignment operator `:=` can be used to assign a literal number to a variable.
For example:

```smalltalk
n := 1.
n := n + 1
```

There are no shorthand assignment operators like `+=` for numbers.

### Fractions

When the message `/` is sent to an integer with an integer argument,
the result is a `Fraction` object.
Operations of fractions always return a new `Fraction` object
rather than a `Float` object in order to maintain accuracy.
For example, the following sets `result` to the `Fraction` `4/3`
rathern than the `Float` `1.333333...`.

```smalltalk
result := (1/3) * 4
```

Fraction objects have the instance variables `numerator` and `denominator`.

### Characters

Characters are represented by the `Character` class.
Printable literal characters are preceded by a dollar sign.
For example, `$a`.
Non-printable characters can be obtained from
unary class methods in the `Character` class.
For example, `Character space`, `Character tab`, and `Character cr`.

### Strings

A `String` is a mutable collection of characters.

The following list depicts the class hierarchy for character data:

- `CharacterSequence`
  - `String`
    - `Symbol`
  - `UnicodeSmtring`
    - `UnicodeSymbol`

Strings are represented by the `String` class.

Literal strings are delimited by single quotes, not double quotes.

The following table describes some of the instance methods
defined in the `String` and `CharacterSequence` classes.

| Method                             | Description                                                                                                |
| ---------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| `,`                                | returns new string that results from appending argument                                                    |
| `=`                                | compares receiver with argument                                                                            |
| `<`                                | compares receiver with argument                                                                            |
| `<=`                               | compares receiver with argument                                                                            |
| `>`                                | compares receiver with argument                                                                            |
| `>=`                               | compares receiver with argument                                                                            |
| `asLowercase`                      | returns new string that is all lowercase                                                                   |
| `asUppercase`                      | returns new string that is all uppercase                                                                   |
| `at:`                              | gets character at given index                                                                              |
| `at:put:`                          | replaces character at given index                                                                          |
| `byteSize`                         | returns size in bytes                                                                                      |
| `capitalized`                      | returns new string where first letter is changed to uppercase                                              |
| `findString:startingAt:`           | returns index where a substring begins                                                                     |
| `isEmpty`                          | returns `Boolean` indicating if receiver size is zero                                                      |
| `size`                             | returns largest legal index                                                                                |
| `substrings`                       | returns `Array` of `String` objects created by splitting receiver on whitespace                            |
| `asCamelCase`                      | returns string created by camelCasing white-space separated words (first letter lower)                     |
| `asDate`                           | returns `Date` parsed from receiver `String`                                                               |
| `asNumber`                         | returns number parsed from receiver `String`                                                               |
| `asPlural`                         | returns plural `String` of an English word                                                                 |
| `asUnicodeString`                  | returns receiver converted to a `UnicodeString`                                                            |
| `beginsWith:`                      | returns `Boolean` indicating if receiver begins with given substring                                       |
| `endsWith:`                        | returns `Boolean` indicating if receiver ends with given substring                                         |
| `findTokens:`                      | returns `Array` of `Strings` created by splitting receiver on delimiters                                   |
| `format:`                          | returns `String` created using interpolation                                                               |
| `includesSubString:`               | returns `Boolean` indicating if receiver contains substring                                                |
| `includesSubstring:caseSensitive:` | returns `Boolean` indicating if receiver contains substring                                                |
| `indexOf:`                         | returns index of a character                                                                               |
| `join:`                            | returns `String` formed by joining `Array` elements of any type with receiver delimiter                    |
| `match:`                           | returns `Boolean` indicating whether receiver matches a pattern                                            |
| `padded:to:width:`                 | returns `String` formed by padding receiver on left or right with a given `Character`                      |
| `prefixAndSuffix:`                 | returns `Array` of `String` objects formed by splitting receiver on last occurrence of a `Character`       |
| `squeezedTo:`                      | returns `String` that optimizes readability of receiver in given number of characters                      |
| `subStrings:`                      | returns `Array` of `String` objects formed by splitting receiver on delimiters                             |
| `substringsSeparatedBy:`           | returns `Array` of `String` objects formed by splitting receiver on a single delimiter `Character`         |
| `truncateWithElipsisTo:`           | returns `String` formed by truncating receiver to given length with elipsis in last 3 of length            |
| `uncapitalized`                    | returns new string where first letter is changed to lowercase                                              |
| `withBlanksCondensed`              | returns `String` created by removing leading and trailing spaces and replacing consecutive spaces with one |
| `withBlanksTrimmed`                | returns `String` created by removing leading and trailing spaces                                           |
| `withoutEnclosing:`                | returns `String` created by removing first and last characters if they match a given `Character`           |
| `withoutLeadingBlanks`             | returns `String` created by removing leading blanks                                                        |
| `withoutPrefix`                    | returns `String` created by removing given substring prefix                                                |
| `withoutSuffix`                    | returns `String` created by removing given substring suffix                                                |
| `withoutTrailingBlanks`            | returns `String` created by removing trailing blanks                                                       |

#### format:

The `format` method returns a new `String` from a template using interpolation
where input comes from an array.
For example:

```smalltalk
s := 'Player {1} is number {2}.' format: #('Gretzky' 99).
```

This sets `s` to `'Player Gretzky is number 99.'`.

The following works in Squeak, but not in Cuis, to produce the same result
using a `Dictionary` as input:

```smalltalk
'Player {p} is number {n}.' format: ({'p'->'Gretzky'. 'n'->99} as: Dictionary).
```

TODO: Verify that this works in Squeak.

The `String` `format:` message is useful for print-style debugging.
For example, the following is the equivalent
of a `console.log` call in JavaScript.

```smalltalk
('myVariable = {1}' format: {myVariable}) print
```

#### padded:to:with:

The following returns the `String` `' 19'`:

```smalltalk
19 asString padded: #left to: 5 with: Character space
```

#### prefixAndSuffix:

The following returns an `Array` containing `'/foo/bar'` and `'baz.txt')`.

```smalltalk
'/foo/bar/baz.txt' prefixAndSuffix: $/
```

### UUIDs

The package "Identities-UUID" generates UUID values.
To install it, enter `Feature require: 'Identities-UUID'` in a Workspace
and "Do it".
To generate a UUID value, use `UUID new`.

### Collections

Smalltalk supports a large number of collection classes.
Collection elements can be any kind of object, including other collections.

The following list depicts the partial class hierarchy for collections:

- `Collection`
  - `SequenceableCollection`
    - `ArrayedCollection`
      - `Array`
      - `ByteArray`
      - `ColorArray`
      - `FloatArray`
      - `IntegerArray`
        - `PointArray`
    - `Heap`
    - `Interval`
    - `LinkedList`
    - `OrderedCollection`
      - `SortedCollection`
  - `Bag`
  - `Set`
    - `Dictionary`
      - `OrderedDictionary`
- `Array2D` - a two-dimensional array
- `SharedQueue`

TODO: Add a section on each commonly used collection class.

`Collection` methods include:

| Method         | Description                  |
| -------------- | ---------------------------- |
| `collect:`     | like `map` in JavaScript     |
| `detect:`      | like `find` in JavaScript    |
| `do:`          | like `forEach` in JavaScript |
| `select:`      | like `filter` in JavaScript  |
| `allSatisfy:`  | like `every` in JavaScript   |
| `anySatisfy:`  | like `some` in JavaScript    |
| `inject:into:` | like `reduce` in JavaScript  |

For example, `#(1 2 3) inject: 0 into: [:acc :n | acc + n]` gives `6`.

### Interval

Instances of the `Interval` class represent a finite arithmetic progress
which is a sequence of numbers where
the difference between consecutive terms is constant.
An example would be the numbers 2, 4, 6, and 8.

To create an `Interval` instance, use one of the following class methods:

- `from:to:` - each value is one more than the previous
- `from:to:by:` - specifies the increment between values
- `from:to:count:` - specifies the number values to include,
  which can be `Fraction` objects
- `integersFrom:to:count:` - specifies the number values to include,
  which will be the closest `Integer` match and not `Fraction` objects

The following table describes some of the instance methods
defined in the `Interval` class.

| Method | Description                                         |
| ------ | --------------------------------------------------- |
| `do:`  | iterates over each value and passes them to a block |
| ``     |                                                     |

TODO: Describe more of these methods.

### Array

Literal arrays between with `#(`, end with `)`,
and contain space-separated values.
For example, `#(true 7 'Tami' (Color red))`.

`Array` instances are fixed-length, ordered collections.

The following table describes some of the instance methods defined in the
`Array`, `ArrayedCollection`, `SequenceableCollection`, and `Collection` classes.

TODO: Finish added descriptions of these methods.

| Method                  | Description                                                           |
| ----------------------- | --------------------------------------------------------------------- |
| `allButFirst:`          |                                                                       |
| `allButFirst`           |                                                                       |
| `allButFirstDo:`        |                                                                       |
| `allButLast:`           |                                                                       |
| `allButLast`            |                                                                       |
| `allButLastDo:`         |                                                                       |
| `allSatisfy:`           | returns `Boolean` indicating whether all elements satisfy a condition |
| `at:ifAbsent:`          |                                                                       |
| `collect:`              |                                                                       |
| `collect:thenSelect:`   |                                                                       |
| `do:`                   |                                                                       |
| `fillWith:`             |                                                                       |
| `findFirst:`            |                                                                       |
| `findFirst:startingAt:` |                                                                       |
| `findLast:`             |                                                                       |
| `first:`                |                                                                       |
| `first`                 |                                                                       |
| `firstAvailable:`       |                                                                       |
| `from:to:do`            |                                                                       |
| `head:`                 |                                                                       |
| `includes:`             |                                                                       |
| `indexOf:`              |                                                                       |
| `isEmpty`               |                                                                       |
| `keysAndValuesDo:`      |                                                                       |
| `last:`                 |                                                                       |
| `last`                  |                                                                       |
| `lastAvailable:`        |                                                                       |
| `lastIndexOf:`          |                                                                       |
| `polynomialEval:`       |                                                                       |
| `select:`               |                                                                       |
| `select:thenCollect:`   |                                                                       |
| `shuffled`              |                                                                       |
| `size`                  | returns number of elements                                            |
| `sort:`                 |                                                                       |
| `sort`                  |                                                                       |
| `sorted:`               |                                                                       |
| `tail:`                 |                                                                       |

Arrays support binary messages that operate on all the elements
and return a new array containing the results.
For example, `#(1 2 3) * 2` returns `#(2 4 6)`.

To compute the average from an array of numbers, send it in the `#mean` message.
For example, `#(1 2 3 4) mean` returns the `Fraction` `5/2`.

To create an array of numbers from a range, send the `#asArray` message to a `Range`.
For example, `(1 to: 5) asArray` returns `#(1 2 3 4 5)`.

To iterate over the elements, send the `do:` message. For example:

```smalltalk
#('foo' 'bar' 'baz') do: [ :item | item print ]
```

TODO: Add more detail.

### OrderedCollection

`OrderedCollection` instances are variable-length, ordered collections.

To create an `OrderedCollection` from an array, send the `#newFrom:` message.
For example, `fruits := OrderedCollection newFrom: #('apple' 'banana' 'cherry')`

To get the number of elements, send the `#size` message.
For example, `fruits size` returns `3`.

To get an element at a specific position send messages like
`#first`, `#last`, and `#at:` which takes a 1-based index.
For example, `fruits first` returns `'apple'`,
`fruits last` returns `'cherry'`,
and `fruits at: 2` returns `'banana'`.

To add an element to the end, send the `#add` message.
For example, `fruits add: 'date'`.

To add an element at a specific index,
send the `#add:beforeIndex:` or `#add:afterIndex` message.
For example, `fruits add: 'orange' beforeIndex: 3`.

To remove an element at a given 1-based index send the `#removeAt` message.
For example, `fruits removeAt: 3` removes `'cherry'`.

To get the index of the first occurence of a given value,
send the `#indexOf:` message.
For example, `fruits indexOf: 'banana'` returns 2.

TODO: Add more detail.

### Bag

TODO: Add detail.

### Association

An `Association` represents a key/value pair.
An instance can be created in the following ways:

```smalltalk
a := Association key: someKey value: someValue.
a := someKey -> someValue.
```

The message `->` is defined in the `Object` class
which make it easy to create an `Association` with any object as the key.

### Dictionary

This is a subclass of the `Set` class.

The keys in a `Dictionary` are often symbols,
but they can be any kind of object that supports the `=` and `hash` messages.

To create a `Dictionary`:

```smalltalk
dict := Dictionary new.
dict := Dictionary newFrom: { k1 -> v1. k2 -> v2. ... }.
dict := { k1 -> v1. k2 -> v2. ... } asDictionary.
```

To add a key/value pair:

```smalltalk
dict at: #key put: value.
dict add: key -> value.
```

To get the value for a key:

```smalltalk
value := dict at: #key
value := dict at: #key ifAbsent: defaultValue
value := dict at: #key ifAbsentPut: defaultValue
```

If a default value is not provided and the key is not found,
an Error window will open that says "key not found".

To get all the keys, values, or associations:

```smalltalk
ks := dict keys.
vs = dict values.
as = dict associations.
```

To iterate over the values:

```smalltalk
dict do: [ :value | value print ].
```

To iterate over the keys and values:

```smalltalk
dict associationsDo: [ :assoc |
    Transcript
        show: assoc key;
        show: ' ';
        show: assoc value;
        cr
].
```

TODO: Add more detail?

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

Blocks can take zero or more positional arguments,
which is something methods cannot do.
Argument names are appear at the beginning of a block
and each name is preceded by a colon.
The argument list is separated from the code by a vertical bar.

Blocks can be saved in variables, passed as arguments to methods and blocks,
and used multiple times. For example:

```smalltalk
noArgBlock := [2 * 3].
singleArgBlock := [:a | a * 3].
multipleArgBlock := [:a :b | a * b].
```

The `value` message evaluates a block and
returns the value of its final expression.
It can provide zero to four arguments.
For blocks with more than four parameters,
pass them in an array using `#valueWithArguments:`.
For example:

```smalltalk
noArgBlock value.
singleArgBlock value: 1.
multiArgBlock value: 1 value: 2.
multiArgBlock value: 1 value: 2 value: 3.
multiArgBlock value: 1 value: 2 value: 3 value: 4.
multiArgBlock valueWithArguments: #(1 2 3 4 5).
```

A block must be passed the same number of arguments as it has parameters.
If a block is passed fewer or more arguments than it accepts,
an Error window will open.

Blocks can declare and use temporary (local) variables.

If a block uses the caret symbol to return a value,
the containing method will exit and return that value.

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
Note that these are not methods on the `Boolean` class.

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

## Exception Handling

Smalltalk methods can throw exceptions.
Exceptions that are thrown by code in a block can be caught and handled.
Unhandled exceptions result in a Debug window being opened
that contains a stack trace.

Smalltalk seems to use the words "exception" and "error" interchangably.

To throw a generic exception:

```smalltalk
Error signal: 'some message'
```

Smalltalk provides my subclasses of the `Exception` class.
Examples include `ArithmeticError`, `AssertionFailure`, `Error`, `Halt`,
`MessageNotUnderstood`, `NotYetImplemented`, and `ZeroDivide`.

To define a custom exception, create a class that inherits from
the `Exception` class or one of its subclasses such as `Error.
This can include instance variables and methods
that are specific to that exception.
For example:

```smalltalk
Error subclass: #OutOfBoundsException
    instanceVariableNames: 'lowerBound upperBound'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

The following class method is used to create an instance:

```smalltalk
lower: aLower upper: anUpper
    ^self new setLower: aLower upper: anUpper
```

The following instance method is used by the class method above:

```smalltalk
setLower: aLower upper: anUpper
    super initialize.
    lowerBound := aLower.
    upperBound := anUpper
```

If the exception subclass defines class methods that create a new instance,
make sure to call `super initialize` as shown above.

To throw a custom exception send the class, or an instance of it,
the `#signal:` message.
For example, the following instance method could be defined
in a class that has a `score` instance variable:

```smalltalk
score: aNumber
    | ex |
    ex := OutOfBoundsException lower: lowerBound upper: upperBound.
    aNumber <  lowerBound ifTrue: [ ex signal: 'too low' ].
    aNumber > upperBound ifTrue: [ ex signal: 'too high' ].
    score := aNumber
```

To catch an exceptions that may be thrown by a method,
send a message that invokes the method inside a block
and set the `#on:do:` message to the block.
For example:

```smalltalk
[s := Game new score: 5] on: OutOfBoundsException do: [ :ex |
    ex messageText print.
]
```

Sending the message `#messageText` to an exception object
returns the message text of the exception.

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

<img alt="Cuis SUnit Test Runner" style="width: 90%"
  src="/blog/assets/cuis-sunit-test-runner.png?v={{pkg.version}}">

To install some example tests:

- Open a "File List" window from the World menu.
- Navigate to and expand `Cuis-Smalltalk-Dev-master`
  or the name of your version of Cuis.
- Navigate to and expand "Packages" and then "Features".
- Enter "test" in the filter input in the upper-left.
- Select one of more of the packages whose names begin with "Test-".
- Click the "install package" button.
- View the code for those packages in a System Browser.

## Color

The `Color` class defines many methods for creating and operating on colors.
It defines the following class methods that create colors by name:

- `black`, `blue`, `brown`, `cyan`, `gray`, `green`, `magenta`,
  `orange`, `pink`, `purple`, `red`, `tan`, `white`, `yellow`
- `lightBlue`, `lightBrown`, `lightCyan`, `lightGray`, `lightGreen`, `lightMagenta`,
  `lightOrange`, `lightRed`, `lightYellow`
- `darkGray`, `veryDarkGray`, `veryLightGray`, `veryVeryDarkGray`, `veryVeryLightGray`
- `transparent`

## Morphic

Morphic is a GUI framework that is built into popular Smalltalk images.
It defines user interfaces with "morphs" which are
what other graphical systems refer to as widgets.
They are graphical items that can be added to
the `WorldMorph` (desktop) or a `LayoutMorph`.

For a great introduction to Morphic, see
<a href="https://www.youtube.com/watch?v=62baNn3c56Y"
target="_blank">Holistic computing with Smalltalk and Morphic. Part 1</a>.

Some of the method names in Morphic classes are inconsistently named.
For example, the class `TextModelMorph` defines the methods
`alwaysHideVerticalScrollbar` and `hideScrollBarsIndefinitely`.
Note how the "b" is sometimes lowercase and sometimes uppercase.

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

To explicitly set the size of a morph, send it the `morphExtent:` message
with a `Point` value. For example, `myMorph morphExtent: 200@100`.

Only a small set of morphs is provided by default.
A good source of additional morphs is the package "UI-Tools".
To install this:

- Open a Workspace window.
- Enter `Feature require: 'UI-Tools'` and press cmd-d (Do it).

This installs many packages including:

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
- UI-Tools
- UI-Widgets

The set of "Basic" morphs will now include `BoxedMorph`, `EllipseMorph`,
`FrameMorph`, `ImageMorph`, `LabelMorph`, `LineMorph`, `PointerLineMorph`,
and `TileResizeMorph`.

Supposedly Cuis 7 will remove support for the UI-Tools package
and the desired subpackages will need to be installed individually.

See my Morphic demos in package `Volkmann`
which contains the classes `VButtonDemo` and `VGreet`.

### Halo

To open the halo (set of surrounding buttons) for a morph,
cmd-click it in Cuis or option-click it in Squeak.
If the item is embedded in other morphs, repeat this multiple times
until a halo appears around the desired morph.

The class of the morph will be displayed at the bottom of the morph.

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

A morph can also be dragged directly without
opening its halo and using the drag buttons.
This only works in the area that is dragged
does not process mouse events.
For example, you cannot drag a morph that contains a button
by dragging the button.

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

### LayoutMorph

A `LayoutMorph` arranges submorphs in a row or column.
Pratically any layout can be achieved by nesting instances of this class.

An instance can be created with:

- `LayoutMorph newColumn`
- `LayoutMorph newRow`
- `LayoutMorph new`

  This calls `newRow` and sets the background color to `Color red alpha: 0.2`.

For example, `myLayout := Layout newRow`.

To add a submorph to a `LayoutMorph`, send it the `addMorph:` message.
For example, `myLayout addMorph: EllipseMorph new`
and `myLayout addMorph: BoxedMorph new`.

By default, there will be no separation between the submorphs.
To add separation, send the `separation:` (both x and y),
`separationX:`, `separationY` messages.
For example, `myLayout separation: 20`.

By default, all the submorphs will be
pushed to the left of a row or top of a column.
To change this, send the `axisEdgeWeight:` message with a number from 0 to 1.
A value zero pushes to the left/top,
a value one pushes to the right/bottom,
and a value of 0.5 centers.

Many chararacteristics of a morph can be edited by
cmd-clicking it to display its halo and clicking its blue menu button.
To get a halo for a submorph, cmd-click repeatedly
until the halo appears around the target morph.
The following menu will appear:

<img alt="Cuis halo blue menu" style="width: 60%"
  src="/blog/assets/cuis-halo-blue-menu.png?v={{pkg.version}}">

To change the border width, size (`morphExtent`), or position (`morphPosition`)
of a morph:

- Click `borderWidth`, `morphExtent`, or `morphPosition`.
- Modify the numbers in the dialog that appears.
- Click the "Accept" or "Cancel" button.

To change the border color or color of a morph:

- Click the push pin in the menu window so it remains open.
- Click "borderColor" or "color".
- Select one of the color sets such as ...
- Drag a color swatch onto the swatch for "borderColor" or "color".
- Close the color set dialog.
- Close the dialog of morph options.

If the UI-Layout-Panel package is installed,
all of these values can be specified interactively by
opening the halo for a `LayoutMorph`, clicking the blue menu button,
and selecting "edit me". The following dialog will appear:

<img alt="Cuis Morphic Layout dialog" style="width: 75%"
  src="/blog/assets/cuis-morphic-layout-dialog.png?v={{pkg.version}}">

To edit the width, height, and off-axis edge weight of a submorph
select "edit my LayoutSpec".

The following dialog will appear:

<img alt="Cuis edit my LayoutSpec" style="width: 80%"
  src="/blog/assets/cuis-edit-my-layoutspec.png?v={{pkg.version}}">

### Buttons

Button labels are automatically shortened to fit within the button width
using the `String` instance method `squeezeTo:`.
It begins by removing spaces from right to left.
It then removes vowels from right to left.
Finally, it removes consonants from right to left
and adds an ellipsis at the end.

### Button Issue

The class `PluggableButtonMorph` uses the value of `Theme current buttonLabel`
as the color for the label on all instances.
But sending `#color:` to an instance changes its background color.
Depending the background color selected, this can result in poor contrast.
It also uses an embossed font for the label.

A way to fix this, suggested by Mariano Montone,
is to create a subclass of `PluggableButtonMorph` that does the following:

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

1. Override this instance method in the `VButtonMorph` class
   by copying the same method from `PluggableButtonMorph`
   and modifying two lines.

   ```smalltalk
   drawEmbossedLabelOn: aCanvas
       | availableW center colorForLabel f l labelMargin targetSize w x y |
       label ifNotNil: [
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
               embossed: false
       ]
   ```

   The modified lines are the one that sets `colorForLabel`
   and the one that sets `embossed`.

### Button Demo in Cuis

Add this code in a Workspace, select it all, and "Do it".
It uses the class `ButtonMorph` which is a custom subclass of `PluggableButtonMorph`
that is defined in the file `ButtonMorph.st` from Mariano Montone.

```smalltalk
layout delete.
label := LabelMorph new
  contents: '0';
  color: Color white.
decBtn := ButtonMorph new
  color: Color yellow;
  label: 'Decrement';
  labelColor: Color red;
  model: [ label contents: (label contents asNumber - 1) asString ];
  action: #value.
incBtn := ButtonMorph new
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

### Button Demo in Squeak

To create a button:

In Squeak, the following code creates a label and two buttons.
The label displays an integer.
Clicking the buttons increments or decrements the integer in the label.

```smalltalk
label := StringMorph new
    contents: '0';
    color: Color yellow;
    position: 120@100;
    openInWorld.
SimpleButtonMorph new
    label: 'Increment';
    position: 150@100;
    target: [ label contents: (label contents asInteger + 1) asString ];
    actionSelector: #value;
    openInWorld.
SimpleButtonMorph new
    label: 'Decrement';
    position: 0@100;
    target: [ label contents: (label contents asInteger - 1) asString ];
    actionSelector: #value;
    openInWorld.
```

### Canvas Drawing

To demo this, create a subclass of `Morph` and
define the instance method `drawOn:`.
For example:

```smalltalk
drawOn: aCanvas

    aCanvas strokeWidth: 20 color: Color red do: [
        aCanvas
            moveTo: -100 @ -100;
            lineTo: 400 @ 200
    ]
```

### SVG

Starting around 32:30 in the YouTube video
<a href="https://www.youtube.com/watch?v=_NB2_Q4bYEk"
target="_blank">FAST Talks - Vector Graphics in Cuis Smalltalk</a>,
Juan demonstrates the class `SVGElementMorph` from the package "SVG".

To work with SVG images:

- Open a terminal.

- Clone the following Git repositories from Cuis Smalltalk:

  - <a href="https://github.com/Cuis-Smalltalk/Numerics.git" target="_blank">Numerics</a>
  - <a href="https://github.com/Cuis-Smalltalk/SVG.git" target="_blank">SVG</a>

- Open a Workspace window and install the packages `LinearAlgebra` and `SVG.

  For each package, enter `Feature require: '{package-name}'` and "Do it".

- Enter `SVGMainMorph exampleTiger openInWorld` and "Do it".

There are many more example class methods in the `SVGMainMorph` class.
To open all the examples,
enter `SVGMainMorph openAllExamples SVGMainMorph` and "Do it".
To delete all those morphs, enter `SVGMainMorph allInstancesDo: [ :obj | obj delete ]`.

Another way to open all the examples using reflection is:

```smalltalk
selectors := SVGMainMorph class allMethodsInCategory: #examples.
selectors do: [ :selector |
    (selector beginsWith: 'example') ifTrue: [
        (SVGMainMorph perform: selector) openInWorld
    ]
].
```

## Overriding doesNotUnderstand

The `Object` class defines the `doesNotUnderstand` method
to open a `MessageNotUnderstood` window.
This can be overridden in specific classes to provide specialized processing
of messages that do not have corresponding methods.

Let's demonstrate this by defining an `Accessible` class that enables
sending messages set and get any instance variable
in classes that inherit from this class.

Here is the class definition:

```smalltalk
Object subclass: #Accessible
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Here is its one and only instance method:

```smalltalk
doesNotUnderstand: aMessage
    "gets or sets an instance variable"

    | argCount getters index key setters |

    argCount := aMessage numArgs.
    argCount > 1 ifTrue: [ ^super doesNotUnderstand: aMessage ].

    key := aMessage keywords first.

    getters := self class allInstVarNames.
    index := getters indexOf: key.
    index ifNotZero: [^self instVarAt: index].

    setters := getters collect: [ :name | name, ':' ].
    index := setters indexOf: key.
    index ifNotZero: [^self instVarAt: index put: aMessage arguments first ].

    ^super doesNotUnderstand: aMessage
```

Here is a subclass of `Accessible:

```smalltalk
Accessible subclass: #Person
    instanceVariableNames: 'birthdate country firstName height lastName'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Here is an example of using this class:

```smalltalk
p := Person new.
p firstName: 'Mark'.
p firstName print.
```

Getting and setting instance variables in this way is quite inefficient.
A better approach is to generate accessor methods
by right-clicking the `Person` class in a System Browser
and selecting "more...create inst var accessors"
to generate accessor methods for each instance variable.

## File I/O

In macOS, Cuis Smalltalk may prompt repeatedly for permission to access
various file system folders.
See this <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/issues/282"
target="_blank">issue</a>.

To create a file, send the `#asFileEntry` message
to a string that contains the file name.
The file is assumed to be in the '\*-UserFiles' directory.
For example:

```smalltalk
fileEntry := 'demo.txt' asFileEntry.
```

To delete a file:

```smalltalk
fileEntry delete.
```

To write to a text file, overwriting any previous contents:

```smalltalk
fileEntry forceWriteStreamDo: [ :fileStream |
    fileStream nextPutAll: 'line #1'.
    fileStream newLine.
    fileStream nextPutAll: 'line #2'
].
```

Another way to obtain a stream for writing and reading a file at a given path
is the following:

```smalltalk
stream := FileStream fileNamed: 'some-file-path'
```

To read the entire contents of a text file into a string:

```smalltalk
contents := fileEntry fileContents.
```

To write serialized objects to a file:

```smalltalk
fileEntry writeStreamDo: [ :fileStream |
    | refStream |
    refStream := ReferenceStream on: fileStream.
    refStream nextPut: true.
    refStream nextPut: 3.
    refStream nextPut: 'text'.
    refStream nextPut: #(1 2 3).
].
```

TODO: Do ReferenceStreams support circular object references?

To read serialized objects from a file:

```smalltalk
fileEntry readStreamDo: [ :fileStream |
    | object refStream |
    refStream := ReferenceStream on: fileStream.
    [ refStream atEnd ] whileFalse: [
        object := refStream next.
        object print "writes to Transcript"
    ]
]
```

TODO: Harvest more information about file system operations from this video.
<a href="https://youtu.be/stMoWMlLVzk?si=_3rmJFPkZ2g4ZIIV"
target="_blank">squeak smalltalk tutorial: file handling part 1</a>.

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

## Games

Some games implemented in Cuis Smalltalk can be found in the GitHub repository
<a href="https://github.com/Cuis-Smalltalk/Games"
target="_blank">Cuis-Smalltalk/Games</a>.

To install them:

- Clone this repository.
- Launch Cuis Smalltalk.
- Open a "File List" window.
- Locate and select the file `Morphic-Games-Solitaire.pck.st`.
- Click the "Install Package" button.
- Open a Workspace window.

To play them, open the World menu, select "New morph...Layouts",
and then select FreeCell or Klondike.
Alternatively, open a Workspace window,
enter something like `Freecell newGame`, and "Do it".

## JSON

The `JSON` package defines the classes `Json`, `JsonObject`, and `JsonError`.
It also adds the method `jsonWriteOn:` to many classes including
`Array2D`, `Association`, `CharacterSequence`, `Collection`, `Dictionary`,
`False`, `Integer`, `Number`, `Text`, `True`, and `UndefinedObject`.

To install this, enter `Feature require: 'JSON'` in a Workspace and "Do it".

Custom classes should implement the instance method `jsonWriteOn:`
to describe which of their instance variables should be included.
Here's how it could be implemented for a `Dog` class
with instance varaibles `id`, `name`, and `breed`.

```smalltalk
jsonWriteOn: aWriteStream
    {
        #id->id.
        #name->name.
        #breed->breed
    } asDictionary jsonWriteOn: aWriteStream
```

Here's how we can get a JSON string for a `Dog` object.

```smalltalk
json := Json render: dog
```

Custom classes should also implement a class method like `fromJson:`
that takes a stream of JSON data and returns a new object created from it.
Here's an example that uses the `VDog` class method `id:name:breed:`
to create a new instance.

```smalltalk
fromJson: aStream
    | jsonObject |
    jsonObject := Json readFrom: aStream.
    ^VDog
        id: (jsonObject at: #id)
        name: (jsonObject at: #name)
        breed: (jsonObject at: #breed)
```

Here's how we can parse a JSON string to get a `Dog` object.

```smalltalk
newDog := VDog fromJson: json readStream
```

## Web Development

Enter `Feature require: 'WebClient'` and "Do it".
This adds many classes in the
<a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Packages/Features/WebClient.pck.st"
target="_blank">WebClient</a> - Core category including
`WebClient`, `WebRequest`, `WebResponse`, `WebServer`, and `WebSocket`.

Also see the
<a href="https://github.com/SeasideSt/Seaside" target="_blank">Seaside</a> and
<a href="https://github.com/zeroflag/Teapot" target="_blank">Teapot</a> frameworks.

See the <a href="https://book.seaside.st/book" target="_blank">Seaside Book</a>.

### Sending HTTP Requests

The `WebClient` class defines class methods that send HTTP requests.
For example:

```smalltalk
res := WebClient httpGet: 'https://pokeapi.co/api/v2/pokemon/pikachu'.
```

The value of the variable `res` above is a `WebResponse` object.
It has many instance variables including:

- `code` - status code such as 200
- `content` - response body
- `headers` - an `OrderedCollection` of `Association` objects
  that describe the HTTP response headers such as `content-type` with a
  value like `text/html; charset=utf-8` or `application/json; charset=utf-8`
- `protocol` - such as the `String` `HTTP/1.1`
- `status` - such as the `String` `HTTP/1.1 200 OK`
- `url` - the URL to which the request was sent

### Implementing an HTTP Server

Creating a `WebServer` instance and sending it the `listenOn:` message
starts a Smalltalk process called "WebServers's listener process".
To kill it, open a "Process Browser", select the process,
and press cmd-t (Terminate).

See the class `MyWebServer` in the `Volkmann` package.

## Foreign Function Interface (FFI)

See <a href="https://itchyeyes.net/articles/cuis-smalltalk.html"
target="_blank">Using the FFI</a>.

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

## Annoyances

- inconsistent case in method names

  For example, the `CharacterSequence` class defines
  the methods `subStrings` and `substringsSeparatedBy:`.

- inconsistent case menu items

  For examples, see the Preferences and Windows submenus of the World menu.

- inconsistent case in button labels

  For example, in Debug windows the first row of buttons starting with "browse"
  have labels that are all lowercase.
  But the second row of buttons starting with "Proceed"
  have labels that are capitalized.

- new windows open in random locations, not near where they were requested

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
- What package can you install to get a color picker?
- Are there any linting tools for Smalltalk?
- Learn how to write and distribute command-line utilities/apps in Smalltalk.
- Learn how to write and distribute GUI apps in Smalltalk.
- Learn how to write and distribute web apps in Smalltalk.
- Study the code in the "Morphic-Games-Solitaire" package
  to see what you can learn from it.
- Is there a String method that does interpolation?
  I'm imagining something like this:
  s := 'Player %s is number %d.' interpolate: #('Gretzky' 99).
- See this! <a href="https://squeak.js.org" target="_blank">SqueakJS</a>
- Learn how to add items to existing menus in the Cuis UI that do something
  when you select them such as repositioning the currrently active window.
- Consider submitting an update to Cuis that standardizes menu item names.
  Currently there are three styles:
  - all words lower
  - All Words Upper
  - Mixture of styles
- Describe the use of "class instance variables" (not shared with subclasses)
  which are different from "class variables" (shared with subclasses)?
  To see them in a System Browser, select a class and click the class button.
  They will look like this:

  ```smalltalk
  Dog class
      instanceVariableNames: ''
  ```

- Does Smalltalk expose its own AST? If so, maybe you can use that to
  generate code and another programming language from my Smalltalk program.
- How can you examine the bytecode for a method?
- Learn how to draw on a canvas in Cuis using "Morphic 3".
- Is Morphic 3 only supported in Cuis?
- Does Cuis run on the "OpenSmalltalk Virtual Machine"?
  Is this the same VM that is used by Squeak and Pharo?
- In a talk by Juan Vuletich, one of his slides says Cuis is
  "A practical system, used for teaching, Satellite image processing,
  research in sign, image, and audio processing,
  research in programming languages, and many other areas of application."
  This seems to imply that it is not for "normal" application development.
- Learn about the Smalltalk "primitive" syntax.
  For example, the `DisplayScreen` instance method `fullScreenMode:`
  contains `<primitive: 233>`.
- Can you configure your image so it never starts in full screen mode
  even if it was saved that way?
- What do you click in a morph halo to cause it to
  display its coordinate system axes?
- Some packages are built-in meaning that they are included in the
  distribution of the Smalltalk implementation.
  Other packages must be downloaded, sometimes by cloning a GitHub repository.
- Formalize your code to generate HTML from an Association object.
- Does “morph removeAll” remove a given morph and all of its sub morphs?
- Another way to browse a class besides pressing cmd-f in the left pane of a
  system browser and searching for it by name is to enter the name
  and it workspace and press cmd-b to browse it.
- Is it more common to include a space after the return caret or not?
- The protocol of a class is the set of all its instance methods,
  including those that are inherited from superclasses.
- Learn how to use all the buttons in a system browser window.
- The term "extent" in Morphic means the combination of width and height
  as a Point object.
- Is "location" a Point object containing X and Y values?
- Is Cuis Smalltalk the only implementation that supports
  Unicode characters, TrueType fonts, vector graphics?
- Try the Connectors package.
  Does it work in Cuis Smalltalk or is there a port of it?
  https://youtu.be/QBRm_hnl7sE?si=w4FDZLfoyMAehZv6
- Submit a PR for the SVG repository that modifies `README.md`
  to explain how to clone the `Numerics` repository
  which provides the `LinearAlgebra` package, and install that.
- Submit a PR to this for the typos you found in
  https://github.com/Cuis-Smalltalk/TheCuisBook.
- Try https://github.com/Cuis-Smalltalk/DatabaseSupport.
- Is there a library of collection types for Smalltalk
  that support structural sharing for immutability?
- Is there a way to revert changes to more than one method at a time?
- Where is the `new` method defined? Does that
  explicitly call `initialize` on the newly created object?
- Perhaps you should refer to most windows as browers.
