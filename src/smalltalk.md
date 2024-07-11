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
Everything is represented by an object that is an instance of some class.
This includes classes themselves and
all the GUI elements in the development environment.
Everything happens by sending messages to objects.
The objects decide whether and how to act on the messages.

<a href="https://en.wikipedia.org/wiki/Simula" target="_blank">Simula</a>
is considered to be the first object-oriented programming (OOP) language.
In Simula, the data that models a simulation are represented by objects,
but primitive data types like numbers are not.
While Simula preceded Smalltalk, Smalltalk was
the first programming language to make OOP popular.

Running Smalltalk programs requires two parts,
a virtual machine (VM) and an image.
The VM is specific to the operating system being used.
It reads and executes Smalltalk code found in an image file.

Smalltalk is not an interpreted language. Smalltalk code is compiled
to optimized bytecode that is executed by a virtual machine.

Everything in Smalltalk is represented by an object, including
classes and all GUI elements in the development environment.
An image file can be thought of as a snapshot
of the current state of the development environment.
It describes a collection of all the active objects.
During development, changes can be saved to the current image
or to a new image.

Smalltalk is perhaps most known for its incredible development tools.
These support:

- finding code in many ways
- live environment where code changes are immediately reflected
  (no need to recompile or restart the environment)
- debugging with ability to modify code and data, then continue or restart
- ability to modify the classes and methods that implement the
  development environment as easily as modifying your own classes and methods
- TODO: add more?

Smalltalk is a dynamically typed language.
Types of variables, method parameters,
and method return types are never specified.
Instead, duck typing is used. Any object can be used as long as it
is able to respond to all the messages that are sent to it.
This is determined at run-time.

Alan Kay, Dan Ingalls, and Adele Goldberg worked at
Xerox PARC (Palo Alto Research Center) in the 1970s.
All of them collaborated to create Smalltalk.
Alan Kay was the primary designer of Smalltalk and gave it its name.
Dan Ingalls was the primary implementor.
Adele Goldberg primarily focused on documentation
and promoting Smalltalk outside of PARC.
The original goal was to use Smalltalk to teach programming.

Many other technologies were invented at PARC including
graphical user interfaces, the mouse, and virtual machines.

Alan Kay said "OOP to me means only messaging,
local retention and protection and hiding of state-process,
and extreme late-binding of all things."
He also said "I'm sorry that I, long ago,
coined the term 'Objects' for this topic, because it gets
many people to focus on the lesser idea. The big idea is messaging!".

Late binding means that messages sent to objects
are looked up for compatible methods at runtime.
However, Smalltalk editors do check for "unknown selectors" when
code is entered that sends a message to a literal object (not to a variable).

Smalltalk did not gain much traction outside Xerox Parc until
BYTE magizine published an issue focused on Smalltalk in August 1981.
The cover, shown at the beginning of this article,
featured a colorful hot air balloon.

In the 1990's the popularity of Smalltalk had risen enough
that it was seen as a possible alternative to C++.
For a time, IBM promoted replacing COBOL with Smalltalk.
In 1995, Smalltalk was the second most popular OO language after C++.
Smalltalk had a 15% market share compared to 71% for C++.

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

## Why Learn Smalltalk

Some reasons to learn Smalltalk include:

- Gaining an understanding of its pros and cons compared to other languages.
- Getting ideas for features to be added to other languages.
- Getting ideas for features to be added in the
  development environments of other languages.
- Actually using it as an alternative to other languages.

## Resources

- <a href="https://en.wikipedia.org/wiki/Smalltalk"
  target="_blank">Smalltalk in Wikipedia</a>

- <a href="https://archive.org/details/byte-magazine-1981-08"
  target="_blank">Byte Magazine issue on Smalltalk</a>

- <a href="https://cuis.st" target="_blank">Cuis Smalltalk</a>

- <a href="https://github.com/Cuis-Smalltalk" target="_blank">Cuis GitHub repositories</a>

- <a href="https://pharo.org" target="_blank">Pharo Smalltalk</a>

- <a href="https://squeak.org" target="_blank">Squeak Smalltalk</a>

- <a href="https://squeak.js.org" target="_blank">SqueakJS</a> -
  "A Squeak VM in JavaScript" by Vanessa Freudenberg

- <a href="https://github.com/Cuis-Smalltalk/Learning-Cuis/blob/master/Quick-UI-Tour.md"
  target="_blank">Quick-UI-Tour</a> for Cuis Smalltalk

- <a href="https://www.fast.org.ar"
  target="_blank">Fundación Argentina de Smalltalk</a> (FAST)

- <a href="https://www.gnu.org/software/dr-geo/" target="_blank">Dr. Geo</a>

  "A program to design and manipulate interactive geometric sketches.
  It helps kids to explore geometry."

- <a href="https://www.goodreads.com/shelf/show/smalltalk"
  target="_blank">Smalltalk Books</a> list on goodreads.

- <a href="https://www.youtube.com/playlist?list=PL6601A198DF14788D"
  target="_blank">Squeak from the very start</a>
  YouTube videos by Lawson English

There is a <a href="https://lists.cuis.st/mailman/listinfo/cuis-dev"
target="_blank">Cuis Smalltalk mailing list</a>,
but no Discord or Slack channel.

There is a <a href="https://discord.gg/43VEWSw2"
target="_blank">Discord channel channel for Squeak Smalltalk</a>.

The following recent podcast episodes discuss Smalltalk:

- <a href="https://www.youtube.com/watch?v=sokb6zZC-ZE&t=3105s"
  target="_blank">Cuis Smalltalk and the History of Computing’s Future</a>
  with Juan Vuletich
- <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955?i=1000656742775"
  target="_blank">A Haskller Tries Smalltalk</a> with Ian Jeffries
- <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955"
  target="_blank">Smalltalk's Past, Present, and Future</a> with Juan Vuletich

## Pros and Cons

Smalltalk has the following pros:

- It has a small, consistently applied syntax.
- It has a great development environment consisting of tools such as
  System Browser, Workspace, Transcript, Method Finder, Debugger,
  Hierarchy Browser, Protocol Browser, and more.
- Everything is an object.
- All methods are invoked through message passing
  which supports extreme polymorphism.
  Any kind of object, referred to as a "receiver",
  can be passed as a message argument as long as it
  responds to the messages that will be sent to it.
- It provides automatic version control.
- It has a great web app. framework (Seaside) and a great CMS framework (Pier).

Smalltalk has the following cons:

- It isn't as popular as many other programming languages.

  - Schools generally don't teach it.
  - Few jobs using it are available.
  - IT press rarely talks about it.
  - It's difficult to convince others to use it.

- Help is limited.

  There are fewer developers using Smalltalk
  than many other programming languages.
  This means there are fewer people available
  to answer questions for new developers.

- Library documentation is lacking.

  Many Smalltalk libraries have little to no documentation and example code.
  There seems to be a feeling that since the source code is easily accessible,
  developers can just read it to determine how to use a library.
  This makes it difficult to get started using new libraries.

- Classes are global and not in namespaces, so all class names must be unique.

  Using class name prefixes is sometimes recommended.
  These use 1, 2, or 3 capital letters.
  Prefixes are important for packages intended to be reused by others.
  Squeak has a <a href="http://wiki.squeak.org/squeak/3318"
  target="_blank">prefix registry</a> in its wiki.
  Unfortunately it was last updated in 2010.

- Immutability is not favored.

  While it is possible to define Smalltalk classes whose objects are immutable,
  this is not common.
  The lack of focus on immutability will feel wrong
  to developers that prefer functional programming.

- Application deployment is tedious.

  Tools to strip a Smalltalk image of developer-only features
  in order to create an image that is suitable for deployment are lacking.
  This is a highly manual process.

- Smalltalks use of late binding for resolving message sends
  means that there are more errors that can only be detected at run-time
  than in statically typed languages such as C++, C#, and Java.
  However, Smalltalk does do incremental compiling when methods are saved,
  so it finds syntax errors before runtime, unlike most scripting languages.

- All the code for a project is stored in one big image file.
  The base image for Cuis Smalltalk is 19 MB, but installing
  optional packages can easily increase the size to around 200 MB.
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

- Smalltalk-76

  This version added most of GUI tools present in current versions of Smalltalk.

- Smalltalk-80

  This version added support for metaclasses of everything,
  including classes, so everything could be treated as an object.
  This was the first version of Smalltalk that was shared outside of PARC.

- ANSI Smalltalk

  This became the standard language reference for Smalltalk in 1998.

## Implementations

The following implementations of Smalltalk
were created after those listed above in the "History" section.

- Squeak

  This was the first popular implementation that was free and open source.

- VisualWorks

- ObjectWorks

- ParcPlace Systems for Unix and Sun systems

- Digitalk for Windows and OS/2 systems

- Enfin

- Cincom

- GemTalk

- Etoys

  This was created for the One Laptop per Child (OLPC) project.

- GNU Smalltalk

  This uses text files for code rather than
  providing a GUI environment that uses an image file.

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

- Pharo - forked from Squeak with goal to be more comprehensive

- Cuis - forked from Squeak with goal to remain small and easy to learn

The most popular Smalltalk implementations include the following:

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

Squeak, Pharo, and Cuis all use the MIT license.
Both Pharo and Cuis began as forks of Squeak
after maintenance of Squeak was turned over to the community
and there was a lack of concensus on its future goals.

The number of predefined classes in each implementation above
were obtained by printing the result of `Smalltalk allClasses size`
with latest versions as of June 10, 2024.

## Cuis Smalltalk

This article primarily focuses on Cuis Smalltalk and running in macOS.

Cuis Smalltalk was created by Juan Vuletich who
has been active in the Smalltalk community since 1997.
Juan began work on Cuis Smalltalk began in 2005
and version 1.0 was released in March 2009.

The objectives of Cuis Smalltalk are to:

- strive for the simplicity of Smalltalk-80
- include a minimal number of classes required for the kernel,
  a GUI framework, and development tools
- create a system that is good for learning and experimenting

More classes can be added by installing packages.

Some advantages that Cuis has over Squeak and Pharo are that
it supports TrueType fonts and high quality vector graphics.

The Cuis mascot is southern mountain cavy which is a "tailless rodent with
short, speckled, greyish-yellow fur, fading to pale grey on the underparts."
They look similar to a mouse, but grow to around eight inches in length.
They are found in Argentina.
Juan Vuletich began development of Cuis Smalltalk in Buenos Aires, Argentina.
The word "cuis" means "squeak" in Rioplatense Spanish.

The GitHub repository for Cuis Smalltalk is at
<a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev"
  target="_blank">Cuis-Smalltalk-Dev</a>.
As of May 2024, 96.8% of the code in the repository is written in Smalltalk.

### Installing

To install Cuis Smalltalk:

1. Browse <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev"
   target="_blank">Cuis Smalltalk Git repository</a>.
1. Click the "Zip file" link near the bottom of the page.
1. Unzip the downloaded file to obtain the directory "Cuis-Smalltalk-Dev-master".
1. Move this directory to your desired location.

Alternatively, clone the Git repository.
This makes it possible to obtain updates by simply running "git pull".

To start Cuis Smalltalk, run the appropriate start script found
in the installed directory based on your operating system.

- for Windows, open a Command Prompt and run `RunCuisOnWindows.bat`
- for Linux, open a Terminal and run `RunCuisOnLinux.sh`
- for macOS ...

  1. Double-click the file `CuisVM.app`
     which will fail because the app is not verified.
  1. Open the System Settings app.
  1. Select "Privacy & Security".
  1. Scroll down to the "Security" section.
  1. Look for the message '"CuisVM.app" was blocked from use
     because it is not from an identified developer.'
  1. Click the "Open Anyway" button.
  1. Click the "Open" button in the next dialog that appears.
  1. Select the file `Cuis*.image` found in the subdirectory as `CuisImage`.
  1. Click the "Open" button.
  1. You may see several dialogs that say '"CuisVM.app"
     would like to access files in your Documents folder'.
     Click the "Allow" button each time.

The file `CuisVM.app` (36 MB) includes the Squeak virtual machine (VM).
This does not differ in any way from the VM used by Squeak.
All the differences between Cuis and Squeak are implemented in
its base image file (19 MB).

The Squeak VM is implemented in Slang and C.
Slang is a subset of Smalltalk that can be easily translated to C.
C is used for performance critical parts.
There is a different version of the Squeak VM executable
for each operating system.

By contrast, Pharo Smalltalk provides `PharoLauncher.app` is 198MB.

The reported name of the `CuisVM.app` app in macOS is "Squeak 5.0".
If you wish to change this:

- Open a terminal window.
- `cd` to the directory where you placed the file `CuisVM.app`.
- Enter `cd CuisVM.app/Contents`.
- Edit the file `Info.plist`.
- Find the key "CFBundleName".
- Change its value from "Squeak" to "Cuis".

### Updating

If Cuis Smalltak is started using a platform-specific shell script,
it will automatically update to the latest version.
These scripts have a name that begins with `RunCuisOn`.

If the Cuis Smalltalk GitHub repository was cloned,
the steps to update are:

- Do a `git pull` on the repository.
- Start Cuis Smalltalk with an image.
- Open the World menu and select "Changes...Install New Updates".

## Squeak Smalltalk

To install Squeak Smalltalk:

- Browse <a href="https://squeak.org" target="_blank">Squeak home</a> page.
- Click the "Downloads" link.
- Click the "Link" button for your operating system.
- Double-click the downloaded file.

On macOS:

- Drag the application icon to the Finder "Applications" directory.
- Double-click the `Squeak*.app` file.

## Pharo Smalltalk

TODO: Add this detail.

## Getting Started

There are at least three ways to start working in Smalltalk.

1. Double-click a Smalltalk image file such as
   the base image found in `CuisImage/Cuis*.image`.
1. Double-click the Cuis app (named `CuisVM.app` in macOS).
   That will open a dialog that prompts for an image file.
1. Drag an image file onto the Cuis app icon.

The initial Cuis Smalltalk window will appear similar to the following:

<img alt="Cuis Smalltalk startup" class="logo" style="width: 400px"
  src="/blog/assets/cuis-smalltalk-startup.png?v={{pkg.version}}">

### The World

The main window renders an instance of the class `WorldMorph`.
This is referred to as the "World".
Clicking anywhere on the "World" opens the World menu.
The items on this menu are described later.
It is not necessary to hold the mouse button down while selecting a menu item.
Just move the mouse to a menu item and click it.

By default, focus moves to a window by moving the mouse cursor over it.
To change this so focus only moves when a window is clicked,
open the World menu and select "Preferences...Focus when click".
To return to the default behavior,
open the World menu and select "Preferences...Focus follows Mouse".

### Font Size

It is likely that the default font size used in all the Smalltalk windows
will not be ideal for you. To change the font size:

- Open the World menu.
- Click "Preferences.".
- Select "Size of GUI elements".
- Select a point size.

The menu of font sizes will remain open, so it's easy to try various sizes.
Once you have selected a suitable size, close the menu
by clicking anywhere on the World or clicking the red circle.

### Full Screen

When developing in Smalltalk it is typical to
open many windows inside the World.
You will likely want to work in full screen mode
so those windows appear to be top-level instead of
floating inside the window of the Smalltalk app.

To enable full-screen mode, open the World menu
and select Preferences ... Full screen on.
To disable this, select Preferences ... Full screen on.

TODO: Move the rest of the content in this section to later.

Jumping ahead, you can add buttons to the World
that simplify toggling full screen mode.
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

### Themes

Colors and other features of the Cuis Smalltalk UI
are determined by selecting a theme.

By default, only two themes are provided, "BrightColorTheme" and "DarkTheme".
To add more themes, open the World menu, select Preferences...Themes...,
and select "\* Load Additional Themes \*".
This adds the themes "ClassicTheme", "DarkBlueTheme", "DarkColorTheme",
"HighContractBlackTheme", "HighContrastWhiteTheme", "LightBluetheme",
"LightGrayTheme", "LightTheme", and "PersonalizedTheme".
Select one of these to switch to that theme.

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

### Morphs

All the graphical elements visible on the World are referred to as "morphs".

Morphs can be dragged to different locations and resized.

To open an context-sensitive menu for a morph, right-click it.
After a menu item is selected from a menu, it will close.

To cause a menu to remain open so multiple selections can be made,
click its push pin in the upper-right corner.
If a menu is closed and re-opened, the push pin state will be reset.

### Workspace Windows

Workspace windows enable experimenting with code.
They are somewhat like REPLs in other programming languages.

To open a Workspace, open a World menu and select "Open...Browser".

Enter any number of expressions separated by periods.

TODO: Replace this with a better screenshot.
<img alt="Cuis Workspace window" style="width: 80%"
  src="/blog/assets/cuis-workspace-window.png?v={{pkg.version}}">

To prepare to execute expressions, select them or
place the cursor at the end of a single-line expression.

To execute the code for its side effects, press cmd-d (Do it).
For example, enter the following and press cmd-d to set a variable:

```smalltalk
message := 'Hello, Smalltalk!'
```

To execute the code and print the result of the last expression
at the cursor position, press cmd-p (Print it).
This sends the message `#printString` to the result object
and outputs the return value.
The output will be selected,
so it can be removed by pressing the delete key.
For example, enter the following and press cmd-p to get the output `6`:

```smalltalk
2 * 3
```

You will use "Do it" and "Print it" often, so memorize their keyboard shortcuts.

To browse a class, enter its name and press cmd-b (Browse it).
This will open a System Browser focused on the class.

To remove the last expression added in a Workspace, press cmd-z.

To save all the code in a Workspace to a text file,
click the blue circle in the upper-left and select "Save as...".
A popup will prompt for a file name. Include the `.st` file extension.
The title bar will change from "Workspace"
to the file path where the code was saved.
It may be necessary to make the window wider to see all of the path.
Once the contents have been saved, subsequent changes in the Workspace
can be saved to the file by pressing cmd-s.
A warning will be displayed if an attempt is made
to close a Workspace with unsaved changes.

To open a new Workspace that contains the contents of a `.st` file,
open a File List, select the file, right-click it,
and select "workspace with contents".

If running code goes into an infinite loop, press cmd-period to break out of it.

### Transcript Windows

Transcript windows provide a read-only window display of output written to it.

To open a Transcript, open the World menu and select "Open...Transcript".

One way to write to the Transcript is to execute `Transcript show: <object>`,
perhaps in a Workspace window.
This can output any kind of object
because every object has a string representation.

Another way to write to the Transcript is to
send the `#print` message to an object.
For example, `'Hello World!' print`.

It is common in Smalltalk documentation that describes specific messages
to precede their name with the `#`,
which indicates that message names are symbols.

The `print` method works with any kind of object because it is
implemented in the `Object` class which is a superclass of all classes.
This executes `Transcript show: self; newLine`.
For objects of some classes such as `Boolean`, `Number`, `String`, and `Array`,
their value will be printed.
For other objects, only their class name will be printed.

For debugging purposes, a better approach is to
add the following instnace method to the `Object class:

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

TODO: Change this image to one that shows ouptut from the examples given above.
<img alt="Cuis Transcript window" style="width: 80%"
  src="/blog/assets/cuis-transcript-window.png?v={{pkg.version}}">

## Implementing in Debugger

Smalltalk makes it possible to implement missing methods in the debugger.
Here's an example of a session doing this.

1. Open a Workspace.
1. Enter `numbers := #(6 2 9 4 3).` and "Do it".
1. Enter `numbers median.` and "Print it".
1. In the "Unknown selector" popup that appears,
   click "median" to confirm that is the message you meant to send.
1. A Debug window will apppear with the title "MessageNotUnderstood".
1. Click the "Create" button on the right.
1. In the "Define" popup that appears, click "SequenceableCollection"
   as the class where the method should be added.
1. In the "Select category" popup that appears, select "math functions".
1. In the code pane, replace `self shouldBeImplemented.` with the following:

   ```smalltalk
   | mid size sorted value |
   sorted := self sorted.
   size := sorted size.
   mid := size // 2 + 1.
   value := sorted at: mid.
   size odd
       ifTrue: [ ^ value ]
       ifFalse: [ ^ (value + (sorted at: mid - 1)) / 2 ]
   ```

1. Press cmd-s to save.
1. Click the "Proceed" button.
1. Verify that the result is `4`.
1. Add `5` to the end of the `numbers` `Array`.
1. Test the `median` method again and
   verify that the result is the `Fraction` `9/2`.

An alternate approach to testing from a Workspace
is to write and run a unit test. See the "Unit Test" section.
This will also open a Debug window where the missing code can be created.

## Syntax

Before diving into the functionality provided
by the Smalltalk development environment,
it's important to understand the syntax of the Smalltalk programming language.

### Summary

The following table summarizes all the syntax.

| Item                                              | Example                                                    |
| ------------------------------------------------- | ---------------------------------------------------------- |
| comment                                           | `"some text"`                                              |
| temporary (local) variable with private scope     | `myTemp` (camelCase)                                       |
| global variable with shared scope                 | `MyGlobal` (CamelCase)                                     |
| pseudo variable (cannot assign)                   | `self`, `super`, `nil`, `true`, `false`, and `thisContext` |
| integer                                           | `123`                                                      |
| float                                             | `3.14`                                                     |
| exponential notation                              | `1.23e4`                                                   |
| character                                         | `$a`                                                       |
| string                                            | `'text'` (use double ' to include)                         |
| string concatenation (comma message)              | `'foo', 'bar', 'baz'`                                      |
| symbol (globally unique string)                   | `#name`                                                    |
| static array (elements are literal values)        | `#(1 4 8)`                                                 |
| dynamic array (elements are computed at run time) | `{1. 2 * 2. 2 raisedTo: 3}`                                |
| array concatenation (comma message)               | `#(1 2), #(3 4)`                                           |
| assignment                                        | `<variable> := <expression>`                               |
| method and block variable declarations            | `\| foo bar baz \|`                                        |
| block with no arguments                           | `[ <expressions> ]`                                        |
| block with arguments                              | `[:a :b \| a + b]`                                         |
| unary message send                                | `<object> <message>` such as `5 factorial`                 |
| binary message send (look like operators)         | `<object> <message> <argument>` such as `4 * 5`            |
| keyword message send                              | `2 raisedTo: 4 modulo: 3`                                  |
| message cascade - sends to initial receiver       | `Transcript show: 'foo'; newLine; show: 'bar'`             |
| message chaining - sends to previous result       | `2 * 3 :: squared :: negated` (-36)                        |
| method return value                               | `^<expression>` such as ^42                                |
| expression separator (period)                     | `'foo print'. 'bar' print`                                 |
| reference to current object in a method           | `self`                                                     |

The caret (^) in a return expression can be followed by a space.
The pretty printer includes a space, but some developers prefer to omit it.

In static arrays the elements are separated by spaces.

In dynamic arrays the expressions are separated by periods.

TODO: What is a "compound literal"?

### Naming Conventions

Names of classes, methods, and variables use camelCase.
They consist of letters and digits.
They start with a letter that is uppercase for classes and class variables,
and lowercase for all other names.
The only special character allowed is the underscore character,
but that is rarely used because camelCase is preferred.

### Assignment Operator

Assignment operators (`:=`) can be rendered as a left pointing arrow
for the purpose looking cool!

To enable this, open the World menu and
select Preferences...Show ST-80 Assignments.
The next time code is modified, all the `:=` messages
will be rendered by a left pointing arrow.

TODO: Does this also enable returning the `^` return character as an up arrow?

To disable this, open the World menu and
select Preferences...Show ANSI Assignments.

Typing an underscore is a shorthand way to type `:=`.
Typing either will be rendered as a left pointing arrow.
This does not change the characters that appear
when code is saved in a "file out" or package.

## Creating Objects

New objects can be created by
sending the message `#new` or `#basicNew` to a class.
By default, both initialize all attributes of the new object to `nil`.
The difference between them is that `new` can be overridden
to do something different, whereas `basicNew` cannot be overridden.

## Variables

Smalltalk supports five kinds of variables:

- Pseudo-variables are provided by the system and cannot be modified.
- Instance variables are associated with a specific instance of a class.
- Class variables are associated with a class
  and all subclasses share the same value.
- Class instance variables are similar to class variables, but
  they allow subclasses to have different values for the same variable name.
- Temporary (or local) variables are accessible only within
  the method or block where they are declared.

### Pseudo-variables

Pseudo-variables are variables whose value
is provided automatically and cannot be modified.
There are six of these with the names
`true`, `false`, `nil`, `self`, `super`, and `thisContext`.
These names are reserved words, meaning
they cannot be used for other kinds of variables.

`true` and `false` represent Boolean values.
They refer to singleton instances of the `True` and `False` classes.

`nil` represents the lack of a real value.

`self` can be used in instance methods to refer to the current object.
It can also be used in class methods to refer to the current class.

`super` can be used in instance methods
to refer to the superclass of the current object.
For example, the superclass of the `SmallInteger` class is `Integer`
and the superclass of the `Integer` class is `Number`.

`self` and `super` are often used as the receiver of messages.
For example, is typical for the instance method `initialize`
to begin with `super initialize`.
We will have more to say about the `initialize` method later.

From the
<a href="https://cuis-smalltalk.github.io/TheCuisBook/Pseudo_002dvariables.html"
target="_blank">Cuis book</a>, "`thisContext` ...
represents the top frame of the run-time stack. ...
It is essential for implementing development tools like the Debugger and
it is also used to implement exception handling and continuations."

### Blocks

A block is closure (anonymous function) that can have parameters
and contain many expressions.
They are represented by the class `BlockClosure`.
The value of the block is the value of its last expression.

Blocks take zero or more positional arguments,
which is something methods cannot do.
Argument names appear at the beginning of a block
and each name is preceded by a colon.
The argument list is separated from the expressions by a vertical bar.

Blocks can be saved in variables,
passed as arguments to methods and other blocks,
and can be evaluated multiple times. For example:

```smalltalk
noArgBlock := [2 * 3].
singleArgBlock := [:a | a * 3].
multipleArgBlock := [:a :b | a * b].
```

Blocks support several messages that evaluate the block
whose names begin with `value`.
These message enable providing zero to four arguments.
For blocks with more than four parameters, send the message
`#valueWithArguments:` and an array holding all the arguments.
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
a Debug window will open.

Blocks can declare and use temporary (local) variables just like a method.

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
b value: 2 "result is 21"
```

To use a block as an iteration condition,
use the methods `whileTrue`, `whileFalse`, `whileNotNil`, and `whileNil`
that are defined in the `BlockClosure` class.
Note that these are not methods on the `Boolean` class.

For example, the following code prints the integer values 1 through 10
in the Transcript:

```smalltalk
| counter |
counter := 1.
[counter <= 10] whileTrue: [
    counter print.
    counter := counter + 1.
].
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

If a block uses the caret symbol (`^`) to return a value,
the containing method will exit and return that value.

### Instance Variables

Instance variables are associated with a specific instance of a class.
They are declared in a space-separated string that is
the value of the `instanceVariableNames:` keyword in a class definition.

Instance variables are always private, which means they can only be accessed by
instance methods in the class that defines them, and in subclasses.
To expose an instance variable value to methods in other classes,
define an instance method that returns it.

When a new instance of a class is created,
its instance method `initialize` is called.
This is, as the name suggests, a perfect place to
assign an initial value to each instance variable.

### Class Variables

Class variables are associated with a class
and the same value is shared with all subclasses.
They are declared in a space-separated string that is
the value of the `classVariableNames:` keyword in a class definition.

Class variables are described in the same way as instance variables,
but their names must begin uppercase.
It is common for a class to not have any class variables.

Like instance variables, class variables are always private.
To expose a class variable value to methods in other classes,
define a class method that returns it.

To assign initial values to the class variables of a class,
define the class method `initialize`
and explicitly send that message to the class.

### Class Instance Variables

Class instance variables are defined as an
instance variables in the metaclass of a given class.
Unlike class variables, subclasses can have a different value
for the variable than that used by the class where it is defined.
Class instance variables are not commonly used.

TODO: Describe how to define a class instance variable
and set different values in the class and subclasses.
Perhaps an example could be an Animal class with a legs variable
that is set to 0 in the Animal class and 4 in the Dog subclass.

### Temporary Variables

Temporary variables are declared in a space-separated string
between vertical bars inside a method or block definition.
Method parameters are also considered to be temporary variables.
These can only be accessed within the method or block that declares them.

In the following example, `numbers` and `sum` are temporary variables.

```smalltalk
average: numbers
    | sum |
    sum := numbers inject: 0 into: [ :acc :each | acc + each ].
    ^ sum / numbers size
```

### Global Variables

While Smalltalk does not support global variables,
the `Smalltalk` `SystemDictionary` object can be used for this purpose.
The following code adds the key "color" with the value "yellow"
to that `Dictionary`, and then retrieves the value for that key:

```smalltalk
Smalltalk at: 'color' put: 'yellow'.
color := Smalltalk at: 'color' ifAbsent: 'none'.
```

## Messages

The only mechanism for communicating with an object is to send it a message.
A message is a combination of a selector (or message name) and arguments.
Messages are always sent to a explicit receiver.
When inside an instance method, to send a message to the current object,
use the pseudo-variable `self` as the receiver.
To send a message to the superclass of the current object,
use the pseudo-variable `super` as the receiver.

If Smalltalk were to add the ability to
specify the parameter and return types of methods,
it would just be specifying the set of messages
to which compatible objects can respond.
This is not done, so compatibility is determined at run-time.

Arguments in messages are always passed by reference, not by value.

In Smalltalk documentation, selectors are preceded by `#`
to indicate that they are symbols.
But the `#` is not included when actually sending a message.

A method is code found in a class that responds to a message.

Smalltalk supports three types of messages:

- unary

  These message do not take any arguments.
  Their names are alphanumeric and begin lowercase.
  For example, in `5 factorial`, `#factorial` is a unary message.

- binary

  These message take a single argument and
  have names that use a restricted set of characters that
  make them look like operators in other programming languages.
  Binary message names can only contain one or more of the following characters:
  `+ - * / \ ~ < > = @ % | & ? ,`

  For example, in `a * b`, `#*` is a binary message.
  This sends the message `#*` to the object `a`, passing it the argument `b`.

  The binary message `=` tests whether two objects are equal,
  meaning one can be used in place of the other.
  Each class can define this method to decide
  how their objects should be compared.
  If they do not define `=`, an implementation
  will be found in the inheritance hierarchy.
  The `Object` class defines `=` to be the same as `==`.

  The binary message `==` tests whether
  two objects are identical (same objects in memory).

- keyword

  These messages take one or more arguments
  that are each preceded by a keyword.
  Each keyword is alphanumeric, begins lowercase, and ends in a colon.
  For example, `#at:put` is a keyword message in the
  `OrderedCollection` class which is the superclass of the `Array` class.

  The following code creates an array of colors and then
  changes the second element from `'green'` to `'yellow'`:

  ```smalltalk
  colors := #('red' 'green' 'blue').
  colors at: 2 put: 'yellow'.
  ```

  The parts of a keyword message must be specified in the order
  in which they appear in the corresponding method definition.
  It's possible define additional methods that support other orders,
  but that is not typically done.

When multiple messages of these types are combined in a single expression,
the order of evaluation is:

- all unary messages from left to right
- all binary messages from left to right
- all keyword messages from left to right

For example, in `2 raisedTo: 1 + 3 factorial`,
the order is `#factorial`, `#+`, and `#raisedTo`.

The evaluation order can be changed by adding parentheses.
For example:

```smalltalk
a := 2.
b := 3.
c := 4.
x := a + b * c. "20"
y := a + (b * c). "24"
```

Parentheses are never needed around unary messages since
those are always evaluated before binary and keyword messages.

If a message is sent to an object and no compatible method is found,
the following popup will appear:

<img alt="Unknown Selector popup" style="width: 50%"
  src="/blog/assets/smalltalk-unknown-selector.png?v={{pkg.version}}">

If the selector was incorrectly typed,
any implemented selector can be selected from this popup.

If the selector is confirmed or if such a message is sent from runnning code,
the following Debugger window will appear:

<img alt="Debugger MessageNotUnderstood"
  src="/blog/assets/smalltalk-message-not-understood.png?v={{pkg.version}}">

One option is to implement the missing method.
To do this:

- Click the "Create" button.
- In the popup that appears, select the class within the inheritance hierarchy
  where the method will be added.
- In the next popup that appears, select a method category for the new method.
- Initially the method implementation will only contain `self shouldBeImplemented`.
- Modify the implementation as desired.
- Press cmd-s to save it.
- Optionally click the "Proceed" button to
  resume execution with calling the new method.

### Tab Completions

When entering code to send a message, completion hints are provided
if at least the first letter in the message name is typed
and the tab key is pressed.
For example, entering `7 s` and pressing the tab key
shows possible completions of `shallowCopy`, `sqrt`, and more.
Use the up and down arrow keys to select a completion
and press the return key to accept it.

To enable completions without typing any characters in the name,
enter `Preferences at: #spaceTabOpensAutoCompletion put: true`
in a Workspace and "Do it".
For example, with this preference set, you can enter `'test'`
followed by a space and press the tab key to get completion hints.

Matching messages found anywhere in the inheritance hierarchy appear in black.
If there are no matching messages,
it will show all known selectors that match in any class in blue.
The reason is that you can send any message to any object.
Even if the object has no matching method anywhere in its inheritance hierarchy,
it could still respond by handling it in a `doesNotUnderstand` method.
Personally I do not find this helpful and wish it did not show those messages.

### Dynamic Messages

The `#perform:` message and its variations can be sent to any class or object
to send a message specified by the symbol that follows `perform:`.
This is useful in situations where the message to send
must be determined at run-time.

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

To provide more than three keyword arguments with `perform`,
send the `#perform:withArguments` message.

## Classes

A class defines a set of associated class variables, instance variables,
class methods, and instance methods.

A class is defined by sending the message
`#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:`
to a superclass which can be `Object` or any other class.
This message is handled by an instance method of the class `Class`.

The `subclass` keyword takes a symbol.
The remaining keywords all take strings.
All the keywords must be supplied, even if their value is an empty string.
The following is an example class definition.

```smalltalk
Object subclass: #Dog
    instanceVariableNames: 'breed name'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Programming languages use many terms to describe data
that is encapsulated by objects created from a class.
Examples include "attribute", "property", and "field".
Smalltalk calls these "instance variables".

Instance variables are described by a single `String`
where the names are separated by spaces.
The names must begin lowercase.

Each class is a associated with a category.
Classes provided by the image are in categories such as
"Collections", "Kernel", "Morphic", "System", "Tools", "UI", and many more.
If a new class definition is saved with an empty string category,
it will be changed to 'as yet unclassified'.

Pool dictionaries enable sharing data between related classes.
They reside in the `Smalltalk` dictionary.
To create a pool dictionary named "MyPool",
enter the following in a Workspace and "Do it":
`Smalltalk at: #MyPool put: (Dictionary new)`.
Then refer to it from any number of classes with `poolDictionaries: 'MyPool'`.
It is common for a class not have any pool dictionaries.

All classes are global and there is no namespacing.
Class names are added to the global variable `Smalltalk`
which is a `SystemDictionary`.
This requires all class names to be unique.
Typically a common prefix, perhaps 2 or 3 uppercase letters,
is added to a set of related class names in order to make the unique.
Lack of namespacing is seen by some as a weakness of Smalltalk.

All classes inherit from one other class,
except `Object` which is the highest superclass of all classes.

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

To find a class without needing to know its class category:

- Press shift-return OR
  hover over the top, first column in a System Browser and press cmd-f.
- Enter part of the class name.
- Select a class name from the popup list that appears.
- If shift-return was pressed, a new System Browser
  will open to display the selected class.
  If cmd-f was pressed in an existing System Browser,
  the selected class will be displayed there.

### Accessor Methods

"Getter methods" allow instance or class variable values to be
accessed from outside the class that defines them.

"Setter methods" allow instance or class variables to be
modified from outside the class the defines them.

Suppose a class `Dog` has the instance variable `breed`.
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

## Objects

Code and data are both represented by objects.
Code can be described by a method or block, both of which are kinds of objects.

Objects are created by sending a message to a class.
In addition, some kinds of objects, such as numbers, strings, and arrays,
can be created from a literal syntax.

Every class supports the class method `new`,
which creates and returns a new instance of the class.
If the class defines the instance method `initialize`,
the `new` method will call it.
The `initialize` method typically initializes
each of the instance variables of the object.

Classes are also represented by objects.
Every class object inherits from `Class`,
which inherits from `ClassDescription`,
which inherits from `Behavior`.
The `Behavior` class defines the instance method `new`
which contains the following:

```smalltalk
    ^ self basicNew initialize.
```

Every class also supports the method `basicNew` which is similar to
the `new` method, but does not call the instance method `initialize`.

Let's look at an example `Rect` class
with instance variables `height` and `width`.

We can define the class method `height:width:`
that provides an alternate way to create objects as follows:

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth
```

We can then define the following instance methods:

```smalltalk
initialize
    super initialize.
    height := 1.
    width := 1

setHeight: aHeight width: aWidth
    height := aHeight.
    width := aWidth

area
    ^height * width
```

The `setHeight:width:` method should be in the "private" category
to indicate that it is not meant to invoked from outside this class.

We can use the `Rectangle` class as follows:

```smalltalk
r1 := Rect new.
Transcript show: r1 area. "1"

r2 := Rect height: 2 width: 3.
Transcript show: r2 area. "6"
```

To determine the class of an object, send it the `#class` unary message.
For example, `19 class` returns `SmallInteger`.

Variables defined in a Workspace hold references to their object values.
It may be necessary to close a Workspace in order to
trigger garbage collection of those objects.

Some classes such as `Morph` implement a `delete` method.
To delete all instances of such classes,
enter the following in Workspace and "Do it":

```smalltalk
SomeClass allInstancesDo: [ :obj | obj delete ]
```

### Immutability

A class can enforce the immutability of its object by
simply not implementing any methods that change its instance variable.

Another option is to use the "Immutability" package
so attempts to change any instance variable of a given object
wll result in a "ModificationForbidden" window opening that
contains a stack trace which indicates where the attempt was made.

To install the "Immutability" package,
enter `Feature require: 'Immutability'` and "Do it".
Among other things, this package adds
the instance method `beImmutable` to the `Object` class.

To enforce a specific object to be immutable,
send it the `#beImmutable` message.
For example the `Rectangle` class described above
could have the following class method for creating new instances:

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth; beImmutable; yourself
```

Note the use of `yourself` to return the current object
rather than the return value of the `beImmutable` method.

### Methods

Methods are associated with a specific class.
Class methods handle messages sent to the class.
Instance methods handle messages sent to objects instantiated from the class.

In binary and keyword methods, parameter variable names typically
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

To find a method when its class is not known:

- Open the World menu and select Open ... Message Names.
  This opens a "Message Names" window.

- Enter any part of a message name and press the return key.
  For example, entering "nj" will find several methods including
  the "inject:into:" message that is implemented by the `Collection` class.

  A list of matching message names will be displayed in the top left pane
  with the first one selected.
  A list of classes that implement the selected method will be displayed
  in the top right pane with the first one selected.
  The implementation of the selected method
  will be displayed in the bottom pane.

- Click another message name to see the classes that implement it.

- Click another class name to see its implementation of the method.

- Click the "Browse" button to open a System Browser focused on that method.

The buttons in the "Message Names" window are
the same as the buttons "System Browser" windows.

Squeak Smalltalk supports finding methods by part of their name
OR by providing example input and output.
To find a method in Squeak Smalltalk:

- Click the "Tools" menu and select "Method Finder".
  This opens a "Selector Browser".
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
- Select the class category of the class to which the method will be added
  in the top, first pane.
- Select the class in the top, second pane.
- Select the method category in which the method will be added.
  If no suitable category appears in the list, press cmd-n (new category...)
  to create a new one.
  Alternatively, select "-- all --" and assign the method to a category later.
  In that case the method will be assigned to the "as yet unclassified" category.
- A starting template for a new method definition
  will appear in the bottom pane.
- Change "messageSelectorAndArgumentNames" to the name of the new method,
  including any parameter names it uses.
- Modify the comment describing the method.
- Update the list of temporary (local) variable names or delete that line.
- Replace "statements" with the method implementation.
- To associate the method with a different method category,
  drag its name from the top, fourth pane to
  the desired method category in the top, third pane.

For example, try adding the following methods to the `Integer` class
which is in the class category "Kernel-Numbers".

```smalltalk
predecessor
    "Answer the predecessor of this integer."
    ^self - 1

successor
    "Answer the successor of this integer."
    ^self + 1
```

Superclasses can define methods that subclasses must implement.
For example, a class named `VShape` can define the following method:

```smalltalk
area
    "Answer the area of the shape."
    self subclassResponsibility
```

This does not prevent instances of the class from being created,
but calling such methods will result in an Error window
with the title "My subclass should have overridden {method-name}" will appear.

The classes `VCircle` and `VRectangle` can be defined as subclasses of `VShape`.
If they do not define the `area` method
and that message is sent to an instance, an Error window
with the title "My subclass should have overridden #area" will appear.

To add the missing method from the Error window:

- Click the "Create" button.
- Select a message category for the method.
- Enter its implemenation.
- Press cmd-s to save.
- Press the "Proceed" button to continue running the code
  at the point of the failed message send.

The example classes above adds the prefix "V" (first letter of my last name)
to their names because the class name `Rectangle is already defined.

The `VCircle` class can add the following class method for creating instances:

```smalltalk
radius: aNumber
    ^self new setRadius: aNumber
```

The `VCircle` class can add the following instance methods:

```smalltalk
setRadius: aNumber
    radius := aNumber

area
    ^Float pi * radius * radius
```

A common way to provide constant values is
to define class methods that return them.
For example, `pi` is a class method in the `Float` class.

The `VRectangle` class can add the following class method for creating instances:

```smalltalk
height: aHeight width: aWidth
    ^self new setHeight: aHeight width: aWidth
```

The `VRectangle` class can add the following instance methods:

```smalltalk
setHeight: aHeight width: aWidth
    height := aHeight.
    width := aWidth

area
    ^height * width
```

To delete a method, select its name in the top, fourth pane
and press cmd-x (remove method).

To delete a method category and all the methods in it,
select its name in the top, third pane and press cmd-x (remove).

Both class and instance methods can call themselves recursively.

Here is an example of a class method from a class I created named `Math`
that calls itself recursively:

```smalltalk
factorial: n
    "Answer the factorial of a given integer."
    ^(n = 1
        ifTrue: 1
        ifFalse: [n * (Math factorial: n - 1)])
```

Here is an example of an instance method I added to the `Integer` class
that calls itself recursively.
The method `factorial` already exists in that class
and is more efficient than the version below.

```smalltalk
factorial2
    "Answer the factorial of this integer."
    ^(self = 1
        ifTrue: 1
        ifFalse: [self * (self - 1) factorial2])
```

If you edit the name of a method in code editing pane of a System Browser,
it will create a copy of the method with the new name.
The method with the previous name will still exist and can be deleted.
An alternative is to right-click the method in the 4th pane
and select "refactorings...rename...".

While it is not commonly done, a method can check the types of its arguments
and alter its functionality based on those.
For example, this class method returns a number
that is double what is passed to it.
If it is given a `String` instead of a `Number`,
it converts it to a `Number` and doubles it.
If it is given any other kind of object, it just returns `0`.

```smalltalk
double: obj
    "Answer double the value of the argument."
    (obj isKindOf: Number) ifTrue: [ ^ obj * 2 ].
    (obj isKindOf: String) ifTrue: [
        [ ^ obj asNumber * 2 ]
            on: Error "error converting string to number"
            do: [ ^ 0 ]].
    ^ 0.
```

Instance and class methods can dynamically added to a class
by sending the message `#compile:` to a class or its metaclass.
The argument is a string of Smalltalk code.
For example, the following code adds the class method `legs`
and the instance method `speak` to the `Dog` class.

```smalltalk
Dog class compile: 'legs ^4'.
Dog compile: 'speak ''Woof!'' print'
```

The expression `Dog class` returns the metaclass of the `Dog` class
and adding a method there makes it a class method.

The single quotes inside the `speak` method string are doubled to escape them.

The new methods will appear in System Browsers.

### Primitive Methods

Primitive methods are implemented in the VM,
often in a way that is more efficient than
what could be achieved in Smalltalk code.

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

In Cuis Smalltalk, the comment at the beginning of
the class method `whatIsAPrimitive` in the `Object` class
contains the following:

> When the Smalltalk interpreter begins to execute a method which specifies a
> primitive response, it tries to perform the primitive action and to return a
> result. If the routine in the interpreter for this primitive is successful,
> it will return a value and the expressions in the method will not be evaluated.
> If the primitive routine is not successful, the primitive 'fails', and the
> Smalltalk expressions in the method are executed instead. These
> expressions are evaluated as though the primitive routine had not been
> called.

From Vanessa Freudenberg, "The VM is mostly written in
a subset of Smalltalk called Slang, transpiled to C, then compiled and linked
with platform-specific code to create the VM executable."

The functionality of specific numbered primitives
can differ between VM implementations.
To get a sense of typical mappings, see the ones used by SqueakJS in the file
<a href="https://github.com/codefrau/SqueakJS/blob/2b9ce0cd94b9ab3cb0aae28052c809b0bd3c14ea/vm.primitives.js#L80"
target="_blank">vm.primitives.js</a>.
For the OpenSmalltalk version, see the file
<a href="https://github.com/OpenSmalltalk/opensmalltalk-vm/blob/Cog/src/spur32.cog/cointerp.c"
target="_blank">cointerp.c</a>.

TODO: Is there a limit of 256 primitive numbers?

## Control Flow

Control flow is provided through message passing.

### Conditional Logic

The `Boolean` class in the `Kernel-Objects` category contains the methods
`#ifTrue`, `#ifFalse`, `#ifTrue:ifFalse`, and `#ifFalse:ifTrue`.
For example:

```smalltalk
result := a < b ifTrue: ['less'] ifFalse: ['more'].
```

The values for `ifTrue` and `ifFalse` can be
literal values, variables, or blocks with no parameters.
The messages listed above just send the `value` message to the argument value.
Typically the `value` message is used to evaluate a no-arg block.
However, the `Object` class defines the `value` method to just return `self`,
which is what enables any kind of object to be used.

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

### Iteration

The `timesRepeat` method in the `Integer` class can be used
to evaluate a block a given number of times.
For example:

```smalltalk
3 timesRepeat: ['Ho' print]
```

See the "Collections" section for messages that iterate over a collection.

See the "Blocks" section for messages that
use a block as an iteration condition.

## Images

A Smalltalk image contains a collection of
all objects in the development environment.
Everything in Smalltalk is represented by an object,
including class definitions (yours and those provided),
GUI elements in the development environment,
and objects created from Workspaces.

An image can be used to manage collections of data,
perhaps held in `Dictionary` objects, as an alternative to using a database.

One way to start an image is to double-click its file.

To save any changes, open the World menu and select "Save Image",
"Save Image as", or "Save Image and Quit".
The changes include open windows, their position and size,
selections made (ex. System Browser top pane selections),
and their content (ex. Workspaces).

To quit without saving changes,
open the World menu and select "Quit without saving".
If there are any unsaved changes,
a confirmation popup will appear that says
"Some ChangeSets for Cuis core have unsaved changes.
If you continue, they would be lost.
Do you really want to exit Cuis without saving the image?"
Click "Yes" or "No".

While the classes and methods provided by a base image can be modified,
it is not a good idea to do so because
there won't be an easy way to apply those changes to a fresh image.

It is better to create new subclasses of provided classes that
override methods and save the subclasses in a new package or "file out".
Doing this enables installing the new package or file out in a fresh image.

There are three ways to discover the file path of the current image:

1. Hover over the task bar at the bottom of the World window.
   This opens a Text Editor window that displays
   the current version of Cuis Smalltalk, the latest update number,
   and the file path of the current image.
1. Open the World menu and select Help...About this System...
   This opens a Text Editor window that displays a description of Cuis Smalltalk.
   It also displays a popup containing the same information as described above.
1. Open a Workspace, enter `DirectoryEntry smalltalkImageDirectory`,
   and "Print it".
   This outputs the directory path of the current image without its file name.

## Help

To get basic help on a token or an expression in any window,
select it, right-click, and select explain.
This displays a popup containing help text.
If the selection is a literal value, its type is displayed.
If the selection is a class name, its category is displayed.
If the select is a message, a list of classes that implement it are displayed.

The World menu contains a Help submenu which contains the following:

- About this System ...

  This was described at the end of the previous section.

- Terse Guide to Cuis

  An excellent source of categoried code snippets for
  performing a large number of common tasks.

  If the prompt "The Terse Guide is not loaded.
  Would you like me to load it for you now?" appears, select "Yes".
  The following window will open.

  <img alt="Cuis Terse Guide"
    src="/blog/assets/cuis-terse-guide.jpg?v={{pkg.version}}">

  Click a topic to see example code.
  Select code and "Do it" or "Print it" to experiment.
  Modify the code as desired.
  Changes will not be saved, so it is safe to experiment.

- Class Comment Browser

  The first time this is selected, a message will be displayed
  stating that it requires cloning the
  <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Tools"
  target="_blank">Cuis-Smalltalk-Tools</a> repository.
  After this is done, selecting this menu item again
  will ask for permission to install the tool.
  After this is done, selecting this menu item yet again
  will open a Class Comment Browser.

  The left pane displays an alphabetical list of class names
  where each name is preceded by a disclosure triangle.
  Clicking a class name displays its first comment in the right pane.
  Clicking a disclosure triangle expands the class to show a
  numbered list of comments found in the source code for the class.
  Clicking any of those displays the comment text in the right pane.

  It's probably more useful to open a System Browser,
  find a class of interest, an view the comments there.

- Code management in Cuis

  Selecting this opens a window titled "Managing your code in Cuis".
  That offers the following advice:

  > Code that is not part of the Cuis Core image itself,
  > like applications, frameworks and libraries, should be stored in Packages.
  > New code that are meant as patches, fixes or additions;
  > that could eventually become part of Cuis itself, is not part of any Package,
  > and is therefore automatically stored in ChangeSets.

- Using GitHub to host Cuis packages

  This opens a window containing text that explains
  the recommended development process for
  managing external packages in Cuis Smalltalk using GitHub.

  For more detail see
  <a href="https://github.com/Cuis-Smalltalk/Learning-Cuis/blob/master/SamplePackage1.md"
  target="_blank">Making a Simple Package for Cuis</a>.

- Editor keyboard shortcuts

  This opens a window containing text that describes all the keyboard shortcuts
  supported by Cuis Smalltalk for editing text, including code.

  For example, to rename a parameter or local variable in a method,
  select it, press cmd-shift-r, and enter the new name.

- Useful Expressions

  This opens a window that contains useful expressions.
  To execute one, selecte it and "Do it" or "Print it".

- VM Statistics

  This opens a window that displays VM Statistics including
  uptime, memory usage, and garbage collection (GC) statistics.

- Space Left

  This opens a popup that displays remaining memory.
  It may only be useful for debugging issues with running out of memory.

## Other Preferences

Many supported preferences are not directly on
the Preferences submenu of the World menu.
To access those, click "All preferences..."
which opens a Preferences window.
Click a preference symbol in the left pane
to display its current value in the right pane.

To change the value of a preference:

- Select it in the left pane.
- Press cmd-i (inspect) to open an Inspect window.
- In the bottom pane, enter `value := {new-value}` and "Do it".
- Close the Inspect window.
- Close the Preferences window.

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

## Windows

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
- Change Sorter: TODO: Describe this.
- Process Browser: displays the state of all Smalltalk processes
  and enables terminating them
- Emergency Evaluator: TODO: Describe this.
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

### Editing Code

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
To selet an entire line, triple-click it.

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

### Inspect Windows

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

### Explore Windows

Explore windows display an object tree starting at a specific object.
Select an object reference or place the cursor immediately after it
and press cmd-shift-i (Explore it).
Click the disclosure triangles to drill down into instance variable values.

Use the bottom pane to enter and execute Smalltalk expressions
where `self` refers to the selected object in the top pane.

<img alt="Cuis Explore window" style="width: 40%"
  src="/blog/assets/cuis-explore-window.png?v={{pkg.version}}">

### System Browsers

To open a System Browser, open the World menu and select Open...Browser.
Alternatively, type a class name (ex. String) in a Workspace window and
press cmd-b (Browse it) to open a System Browser with that class already selected.

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-the-system-Browser.html"
target="_blank">A brief introduction to the System Browser</a>.

#### System Browser UI

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

  The items in these panes are not sorted alphabetically by default.
  To sort them, hover over a pane and press cmd-shift-a (alphabetize).
  TODO: The menu shows the shortcut as just "a". Is that a bug?

  To scroll any list to the first item that begins with a given letter,
  over over the lsit and type the letter.

- The second row displays a message describing the item selected in the top row.

  | Item Type       | Description                                                         |
  | --------------- | ------------------------------------------------------------------- |
  | class category  | # of classes in the category, total # of instance and class methods |
  | class           | # of instance and class methods defined in the class                |
  | method category | # of methods (TODO: Why labelled as "messages"?)                    |
  | method          | # of sends, implementors, and senders                               |

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

- The fourth row displays information about the selected item
  based on the checkbox that is selected for the "show..." button.
  By default it displays Smalltalk code for the selected item
  and can be used to edit the code.
  When there are unsaved code changes in this pane,
  a thin, red border appears around it.
  Press cmd-s (Accept) to save the changes
  and the thin, red border will disappear.

#### Code Formatting

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

#### Working with Classes

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

#### Working with Methods

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

#### Refactorings

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

### Search Browser

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

### Hierarchy Browsers

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
They are not intended to be used to edit Smalltalk source code.

The text can be saved in external text files,
but all the formatting is discarded and only the raw text is saved.

### Message Names Windows

These windows enable searching for method implementations
whose name contains a given substring.
For example, enter "select:" to find all the classes
that have a method whose names end with that.
The results include `Bag`, `Collection`, `Dictionary`, `Heap`,
`OrderedCollection`, `SequenceableCollection`, and `SortedCollection`.
Click one the class names to see the method implementation.

<img alt="Cuis Smalltalk Protocol window" style="width: 100%"
  src="/blog/assets/cuis-protocol-window.png?v={{pkg.version}}">

### MessageNotUnderstood Errors

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

### Debug Windows

To debug code, select one or more lines in a Workspace window
and press cmd-shift-d (Debug it).
A Debug window will appear.

TODO: Describe other ways to open a Debug window.
TODO: Is adding `self halt` to a method and then running it another way?

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

### Change Sorter Windows

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

### File List

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

### Installed Packages Window

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

### Process Browsers

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

### Emergency Evaluator

The Emergency Evaluator can be useful to save work
when the current session becomes mostly unusable.

<img alt="Cuis Emergency Evaluator" style="width: 30%"
  src="/blog/assets/cuis-emergency-evaluator.png?v={{pkg.version}}">

TODO: Explain when this is useful.

## Saving Code

One way to save code that you develop is to save the image.
While this works, it is not recommended for two main reasons.

1. Corrupt images

   On rare occassions, changes make to an image can render it unusable.
   If this happens and the code you developed has not be saved elsewhere,
   it could lost.

1. Code Sharing

   The best ways to share Smalltalk code you have written
   are to create a "fileOut" or a package.
   Other developers can "fileIn" the fileOuts you create
   or they can install packages you create.
   Both are options described below.

### fileOut and fileIn

"fileOut" can be used to save any of the following in a `.st` text file:

- a single method
- all the methods in a single method category
- a single class and all its methods
- a single class category

To create a fileOut:

- Open a System Browser.
- Select a class category, class, method category, or method
  in the top row of panes.
- Right-click and select "fileOut".

The file will be saved in
`{distribution-name}-UserFiles/FileOuts/{name}.st`.

These files use the bang-separated "chunked format".
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

The following is an example fileOut for a `Dog` class:

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

To read a fileOut into the current image:

- Open the World menu.
- Select "Open...File List".
- Locate and select a `.st` file created by a fileOut.
- Right-click and select "fileIn".
- Enter your initials and then your name
  for tracking who performed the fileIn.

All the class categories, classes, method categories, and methods
defined in the fileOut will now be available in the current image.

### Packages

Cuis Smalltalk supports the ability to save code outside an image file
as a "package" and load the package into running images.
This is an alternative to <a href="https://wiki.squeak.org/squeak/1287"
target="_blank">Monticello</a> which is used in Squeak and Pharo.

Packages are collections of Smalltalk code
stored in files with a `.pck.st` file.

Package names are used as prefixes on class and method categories names.
Package name abbreviations are often used as prefixes on class names.

The GitHub account "Cuis-Smalltalk" provides many package repositories,
32 as of June 2024.
Sadly the documentation included in the `README.md` files of these packages
is quite sparse.
These repositories must be cloned in order to install them.

For additional packages, search GitHub for
repositories whose names begin with "Cuis-Smalltalk-".

There are three ways to install a package.

1. Drag a package file onto the World and select "install package".
1. Open a File List, locate a package file,
   select it, and click the "install package" button.
1. Open a Workspace,
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

See the earlier section "Installed Packages Window"
for more details on working with packages.

By default packages are saved in a directory that is relative to
the directory that holds the current image file.
To determine this:

- Open a Workspace.
- Enter `Smalltalk imagePath.`
- Press cmd-p to "Print it".

Determine the name and location of the directory
that holds your Cuis Smalltalk installation.
For me this is `~/Documents/dev/lang/smalltalk/Cuis-Smalltalk-Dev`.
My base image file is in the subdirectory `CuisImage` in the file `Cuis7.1-6367.image`.

By default, packages I create go in a similar path which is
`~/Documents/dev/lang/smalltalk/Cuis-Smalltalk-Dev-UserFiles/NewPackages/{package-name}.pck.st`.

To define new classes and save them in your package:

- Add a class category whose name is the same as the new package name.
- Add classes in the new class category.
- Add methods to the new classes in any method category.
- Open an "Installed Packages" window and select the new package.
- Click the "Save" button.

To add or override methods in existing classes
and save the changes in your package:

- Add a message category to an existing class whose name is
  an asterisk followed by the new package name.
  For example, I used "\*Volkmann".
  This can optionally be followed by "-" and a meaningful method category name.
  For example, "\*Volkmann-geometry".
- Add new methods to the existing class in the new message category.
- Open an "Installed Packages" window and select the package.
  An asterisk before the name indicates that it has unsaved changes.
- Click the "Save" button.

To verify that all this worked:

- Open the World menu and select "Quit without saving"
  so the changes are not saved in the current image.
- Restart Cuis Smalltalk with the same image.
- Verify that the classes and methods that were added are not present.
- Install the package.
- Verify that the classes and methods that were saved in the package
  are now present.

There is no provided way to uninstall a package.
The only way to remove it from the image is to start with a fresh image
and only install the desired packages.

TODO: What does the "delete/merge" button in the "Installed Packages" window do?
TODO: It does not uninstall the selected package or delete the file that defines it.

It is recommended to save packages in GitHub.
For details on doing this, open the World menu and
select Help...Using GitHub to host Cuis packages.

## Reflection

Smalltalk provides many methods for
getting information about classes and objects.
The following table lists some of them.

| Method                                                  | Answers                                                                                                      |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `Smalltalk allClasses`                                  | an `Array` of all classes defined in the current image                                                       |
| `Smalltalk allClassesImplementing: #selector`           | an `Array` of all classes that implement a given selector                                                    |
| `SystemOrganization categoryOfElement: #SomeClass`      | name of the class category to which a given class belongs                                                    |
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

## Data Types

TODO: Move this section up.

The following subsections provide a review of
commonly used Smalltalk classes that represent data types.

### UndefinedObject

The pseudo-variable `nil` represents not having a value.
It refers to the singleton instance of the `UndefinedObject` class.
Creation of additional instances is prevented by
overriding the class method `new` in the `UndefinedObject` class.

The following table describes some of the instance methods
defined in the `UndefinedObject` class.
These can be invoked on the pseudo-variable `nil`.

| Method            | Description                                              |
| ----------------- | -------------------------------------------------------- |
| `ifNil:`          | always evaluates its argument                            |
| `ifNil:ifNotNil:` | always evaluates its first argument and never its second |
| `ifNotNil:`       | never evaluates its argument                             |
| `ifNotNil:ifNil:` | always evaluates its second argument and never its first |
| `isEmptyOrNil:`   | always answers `true`                                    |
| `isLiteral`       | always answers `true`                                    |
| `isNil`           | always answers `true`                                    |
| `notNil`          | always answers `false`                                   |

### Booleans

The pseudo-variables `true` and `false` refer to
singleton instances of the classes `True` and `False`
which are subclasses of the class `Boolean`.

`True` and `False` are singleton classes.
Creating of additional instances is prevented by
overriding the class method `new` in the `Boolean` class.

Representing the values `true` and `false` by distinct classes
simplifies the implementation of many of their methods.
For example, here are the implementations of the
`&` and `ifTrue:` instance methods in the `True` class.

```smalltalk
"This is simplified because it can assume
 the receiver (left-hand side) is true."
& alternativeObject
    ^alternativeObject

"This is simplified by not needing to test whether the receiver is true."
ifTrue: alternativeBlock
    ^alternativeBlock value
```

The following table describes most of the instance methods
defined in the `Boolean` class.
Since `True` and `False` are subclasses, they also have these methods.

| Method             | Description                                                   |
| ------------------ | ------------------------------------------------------------- |
| `&`                | "and" without short-circuiting                                |
| `\|`               | "or" without short-circuiting                                 |
| `and:`             | "and" with short-circuiting                                   |
| `and:and:`         | like `and:` but for three values                              |
| `and:and:and:`     | like `and:` but for four values                               |
| `and:and:and:and:` | like `and:` but for five values                               |
| `eqv:`             | answers whether two `Boolean` values are equivalent           |
| `ifFalse:`         | evaluates argument (typically a block) if receiver is `false` |
| `ifFalse:ifTrue:`  | conditionally evaluates arguments (typically blocks)          |
| `ifTrue:`          | evaluates argument (typically a block) if receiver is `true`  |
| `ifTrue:ifFalse:`  | conditionally evaluates arguments (typically blocks)          |
| `isLiteral`        | always answers `true`                                         |
| `not`              | answers opposite Boolean value                                |
| `or:`              | "or" with short-circuiting                                    |
| `or:or:`           | like `or:` but for three values                               |
| `or:or:or:`        | like `or:` but for four values                                |
| `or:or:or:or:`     | like `or:` but for five values                                |
| `xor:`             | exclusive "or" of two `Boolean` values                        |

The `True` and `False` classes implement some of the methods described above,
but they do not add any methods.

### Numbers

The following list depicts the class hierarchy for various kinds of numbers:

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

The assignment operator `:=` can be used to
assign a literal number to a variable.
For example:

```smalltalk
n := 1.
n := n + 1
```

There are no shorthand assignment operators like `+=` for numbers.

Numbers are automatically converted to objects of the appropriate type.
For example:

```smalltalk
| a b c |
a := 1000000000000000000.
b := a * 10.
c := b / 10.
a class print. "SmallInteger"
b class print. "LargePositiveInteger"
c class print. "SmallInteger"
```

The following table describes most of the instance methods
defined in the `Number` class.
These can be invoked on instances of all `Number` subclasses.

| Method                | Description                                                                     |
| --------------------- | ------------------------------------------------------------------------------- |
| `*`                   | answers product of two numbers                                                  |
| `+`                   | answers sum of two numbers                                                      |
| `-`                   | answers difference of two numbers                                               |
| `/`                   | answers quotient of two numbers                                                 |
| `//`                  | answers integer quotient of two numbers truncating toward negative infinity     |
| `=`                   | answers if two numbers are equivalent                                           |
| `@`                   | answers a `Point` object where receiver is x and argument is y                  |
| `\\`                  | answers same as `mod:`                                                          |
| `^`                   | answers same as `raisedTo:`                                                     |
| `abs`                 | answers absolute value of receiver                                              |
| `arcCos`              | answers arccosine of receiver                                                   |
| `arcSin`              | answers arcsine of receiver                                                     |
| `arcTan`              | answers arctangent of receiver                                                  |
| `asFloat`             | answers equivalent `Float` value                                                |
| `asInteger`           | answers equivalent `Integer` value                                              |
| `ceiling`             | answers nearest integer rounding toward infinity                                |
| `cos`                 | answers cosine of receiver in radians                                           |
| `degreeCos`           | answers cosine of receiver in degrees                                           |
| `degreeSin`           | answers sine of receiver in degrees                                             |
| `degreeTan`           | answers tangent of receiver in degrees                                          |
| `cubed`               | answers receiver raised to 3rd power                                            |
| `degreesToRadians`    | answers result of converting receiver in degees to radians                      |
| `div:`                | answers integer division rounding toward negative infinity                      |
| `even`                | answers `Boolean` value indicating if receiver is equivalent to an even integer |
| `floor`               | answers nearest integer rounding toward negative infinity                       |
| `fractionPart`        | answers fractional part (ex. `3.25 fractionPart` gives 0.25)                    |
| `ifNotZero:`          | evaluates argument (typically a block) if receiver is not zero                  |
| `integerPart`         | answers integer part (ex. `3.25 integerPart` gives 3.0)                         |
| `isDivisibleBy:`      | answers `Boolean` value indicating if receiver is divisible by argument         |
| `isNaN`               | always answers `false`                                                          |
| `isNumber`            | always answers `true`                                                           |
| `isZero`              | answers `Boolean` value indicating if receiver is zero                          |
| `lg`                  | answers same as `log2`                                                          |
| `ln`                  | answers natural log of receiver                                                 |
| `log`                 | answers base 10 log of receiver                                                 |
| `log2`                | answers base 2 log of receiver                                                  |
| `log:`                | answers log of receiver where argument is the base                              |
| `magnitude`           | same as `abs`                                                                   |
| `mod:`                | answers receiver modulo argument                                                |
| `moduloTwoPiAsFloat:` | answers receiver modulo 2 \* pi as a `Float`                                    |
| `negated`             | answers receiver with opposite sign                                             |
| `negative`            | answers `Boolean` value indicating if receiver is negative                      |
| `nthRoot:`            | answers argument root of receiver                                               |
| `odd`                 | answers `Boolean` value indicating if receiver is equivalent to an odd integer  |
| `positive`            | answers `Boolean` value indicating if receiver is positive or zero              |
| `radiansToDegrees`    | answers result of converting receiver in radians to degrees                     |
| `raisedTo:`           | answers receiver raised to argument exponent                                    |
| `reciprocal`          | answers reciprocoal of receiver (`1 / self`)                                    |
| `rem:`                | answers remainder of integer division of receiver by argument                   |
| `roundTo:`            | answers nearest value or receiver rounded to a multiple of argument             |
| `rounded`             | answers nearest integer to receiver                                             |
| `sign`                | answers `1`, `0`, or `-1` based on sign of receiver                             |
| `sin`                 | answers sine of receiver in radians                                             |
| `sqrt`                | answers square root of receiver                                                 |
| `squared`             | answers square of receiver                                                      |
| `strictlyPositive`    | answers `Boolean` value indicating if receiver is positive and not zero         |
| `tan`                 | answers tangent of receiver in radians                                          |
| `to:`                 | answers an `Interval` from receiver to argument                                 |
| `to:by:`              | answers an `Interval` from receiver to `to:` in steps of `by:`                  |
| `to:by:do:`           | evaluates "do" block with every value from receiver to `to:` in steps of `by:`  |
| `to:do:`              | evaluates "do" block with every value from receiver to `to:`                    |
| `toSelfPlus:`         | answers an `Interval` from receiver to receiver plus argument                   |
| `truncated`           | answers closes integer rounding toward zero                                     |

For example, `3.14159 roundTo: 0.0001` gives `3.1416`.

All subclasses of `Number` except `Fraction`
implement the `isLiteral` method to always return `true`.
The `Fraction` class implements the `isLiteral` method
to return `true` if the denominator is a multiple of 2 or 5,
and `false` otherwise. TODO: Why?

The subclasses `Float`, `BoxedFloat64`, and `SmallFloat64`
do not implement any particularly interesting methods
that were not already described for the `Number` class.

The following table describes some of the instance methods
defined in the `BoxedFloat64` and `SmallFloat64` classes
(both subclasses of `Integer`) that are not defined in the `Number` class.

| Method | Description                                                                         |
| ------ | ----------------------------------------------------------------------------------- |
| `<`    | answers `Boolean` value indicating if receiver is less than argument                |
| `<=`   | answers `Boolean` value indicating if receiver is less than or equal to argument    |
| `=`    | answers `Boolean` value indicating if receiver is equal to argument                 |
| `>`    | answers `Boolean` value indicating if receiver is greater than argument             |
| `>=`   | answers `Boolean` value indicating if receiver is greater than or equal to argument |
| `~=`   | answers `Boolean` value indicating if receiver is not equal to argument             |

The `Integer` class overrides the `/` method to
return a `Fraction` object when the argument is an `Integer`.
For example, the following sets `result` to the `Fraction` `4/3`
rather than the `Float` `1.333333...`.

```smalltalk
result := (1/3) * 4
```

Fraction objects have the instance variables `numerator` and `denominator`.

Operations of fractions always return a new `Fraction` object
rather than a `Float` object in order to maintain accuracy.

The following table describes some of the instance methods defined
in the `Fraction` class that are not defined in the `Number` class.

| Method        | Description                                                         |
| ------------- | ------------------------------------------------------------------- |
| `denominator` | answers the denominator of the fraction                             |
| `numerator`   | answers the numerator of the fraction                               |
| `reduced`     | answers a new fraction that is a reduced equivalent of the receiver |

The following table describes some of the instance methods defined
in the `Integer` class that are not defined in the `Number` class.

| Method         | Description                                                                         |
| -------------- | ----------------------------------------------------------------------------------- |
| `<`            | answers `Boolean` value indicating if receiver is less than argument                |
| `<=`           | answers `Boolean` value indicating if receiver is less than or equal to argument    |
| `=`            | answers `Boolean` value indicating if receiver is equal to argument                 |
| `>`            | answers `Boolean` value indicating if receiver is greater than argument             |
| `>=`           | answers `Boolean` value indicating if receiver is greater than or equal to argument |
| `~=`           | answers `Boolean` value indicating if receiver is not equal to the argument         |
| `<<`           | answers new `Integer` obtained by shifting argument bits left                       |
| `>>`           | answers new `Integer` obtained by shifting argument bits right                      |
| `atRandom`     | answers a random integer from 1 to receiver                                         |
| `atRandom:`    | answers a random integer from 1 to receiver using argument as a generator           |
| `bitAnd:`      | answers new `Integer` obtained by anding the bits in receiver and argument          |
| `bitAt:`       | answers the bit (0 or 1) in receiver at argument position                           |
| `bitAt:put:`   | answers new `Integer` obtained by changing the bit at `bitAt:` to `put:`            |
| `bitOr:`       | answers new `Integer` obtained by oring the bits in receiver and argument           |
| `bitXor:`      | answers new `Integer` obtained by exclusive oring the bits in receiver and argument |
| `gcd`          | answers greatest common divisor of receiver and argument                            |
| `hex`          | answers equivalent hexadecimal string                                               |
| `isPrime`      | answers `Boolean` value indicating if receiver is a prime number                    |
| `lcm`          | answers least common multiple of receiver and argument                              |
| `timesRepeat:` | evaluate argument block receiver times                                              |

### Characters

Characters are represented by the `Character` class.
Printable literal characters are preceded by a dollar sign.
For example, `$a`.
Non-printable characters can be obtained from unary class methods
in the `Character` class such as `cr`, `space`, and `tab`.

The following table describes some of the class methods defined
in the `Character` class.

| Method             | Description                                                           |
| ------------------ | --------------------------------------------------------------------- |
| `codePoint:`       | answers `Character` instance that corresponds to argument code        |
| `cr`               | answers carriage return instance                                      |
| `digitValue:`      | answers `Character` instance that corresponds to argument digit (0-9) |
| `escape`           | answers `Character` instance that corresponds to escape character     |
| `lf`               | answers `Character` instance that corresponds to line feed character  |
| `newLineCharacter` | answers same as `lf`                                                  |
| `separators`       | answers array of whitespace characters                                |
| `space`            | answers `Character` instance that corresponds to space character      |
| `tab`              | answers `Character` instance that corresponds to tab character        |

The following table describes some of the instance methods defined
in the `Character` class.

| Method                     | Description                                                                                                               |
| -------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `<`                        | answers `Boolean` value indicating if receiver is less than argument                                                      |
| `<=`                       | answers `Boolean` value indicating if receiver is less than or equal to argument                                          |
| `=`                        | answers `Boolean` value indicating if receiver is equal to argument                                                       |
| `>`                        | answers `Boolean` value indicating if receiver is greater than argument                                                   |
| `>=`                       | answers `Boolean` value indicating if receiver is greater than or equal to argument                                       |
| `asLowercase`              | answers lowercase version of receiver                                                                                     |
| `asString`                 | answers `String` or `UnicodeString` representation of receiver                                                            |
| `asSymbol`                 | answers `Symbol` of receiver                                                                                              |
| `asUppercase`              | answers uppercase version of receiver                                                                                     |
| `asciiValue`               | answers decimal ASCII value of receiver                                                                                   |
| `codePoint`                | answers decimal Unicode value of receiver                                                                                 |
| `digitValue`               | answers `Integer` value of digit `Character`; opposite of class method `digitValue:`                                      |
| `hex`                      | answers hexadecimal ASCII value of receiver                                                                               |
| `isAlphaNumeric`           | answers `Boolean` value indicating if receiver is a letter or digit                                                       |
| `isDigit`                  | answers `Boolean` value indicating if receiver is a digit                                                                 |
| `isLetter`                 | answers `Boolean` value indicating if receiver is a letter                                                                |
| `isLiteral`                | always answers `true`                                                                                                     |
| `isLowercase`              | answers `Boolean` value indicating if receiver is lowercase                                                               |
| `isSeparator`              | answers `Boolean` value indicating if receiver is whitespace                                                              |
| `isUppercase`              | answers `Boolean` value indicating if receiver is uppercase                                                               |
| `isValidInBinarySelectors` | answers `Boolean` value indicating if receiver can appear in a binary selector name                                       |
| `isValidInFilenames`       | answers `Boolean` value indicating if receiver can appear in a file name                                                  |
| `isValidInIdentifier`      | answers `Boolean` value indicating if receiver can appear in a variable name or unary/keyword selector                    |
| `isValidStartOfIdentifier` | answers `Boolean` value indicating if receiver can appear as first character in a variable name or unary/keyword selector |
| `isVowel`                  | answers `Boolean` value indicating if receiver is a vowel                                                                 |
| `to:`                      | answers `Array` of `Character` instances from receiver to argument                                                        |
| `tokenish`                 | answers `Boolean` value indicating if receiver can appear in a token (letter, digit, or colon)                            |

### Strings

The following list depicts the class hierarchy for character data:

- `Collection`
  - `SequenceableCollection`
    - `CharacterSequence`
      - `String`
        - `Symbol`
      - `UnicodeString`
        - `UnicodeSymbol`

Instances of `String` and `UnicodeString` are mutable collections of characters.
But instances of `CharacterSequence`, `Symbol`, and `UnicodeSymbol` are immutable.

Literal strings are delimited by single quotes,
not double quotes which are used to delimit comments.

The `CharacterSequence` class method `readFrom:` answers an instance
created by reading text from a stream.

The following table describes some of the instance methods
defined in the `CharacterSequence` class.

| Method                                 | Description                                                                                                                   |
| -------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `append:`                              | answers new `CharacterSequence` containing argument characters appended to receiver characters; same as `,` in `String` class |
| `asCamelCase`                          | answers copy created by camelCasing white-space separated words (first letter lower)                                          |
| `asDate`                               | answers `Date` parsed from receiver                                                                                           |
| `asLowercase`                          | answers copy that is all lowercase                                                                                            |
| `asNumber`                             | answers number parsed from receiver                                                                                           |
| `asPlural`                             | answers plural of an English word                                                                                             |
| `asUnicodeString`                      | answers receiver converted to a `UnicodeString`                                                                               |
| `asUppercase`                          | answers copy that is all uppercase                                                                                            |
| `beginsWith:`                          | answers `Boolean` indicating if receiver begins with given substring                                                          |
| `capitalized`                          | answers copy where first letter is changed to uppercase                                                                       |
| `collect:`                             | answers result of applying block argument to each character                                                                   |
| `endsWith:`                            | answers `Boolean` indicating if receiver ends with given substring                                                            |
| `findString:`                          | answers index where argument substring begins                                                                                 |
| `findString:startingAt:caseSensitive:` | answers index after `startingAt:` where a substring begins, optionally case sensitive                                         |
| `findTokens:`                          | answers `Array` of instances created by splitting receiver on delimiters in argument                                          |
| `format:`                              | answers instance created using interpolation                                                                                  |
| `includesSubString:`                   | answers `Boolean` indicating if receiver contains substring                                                                   |
| `includesSubstring:caseSensitive:`     | answers `Boolean` indicating if receiver contains substring                                                                   |
| `indexOf:`                             | answers index of a character                                                                                                  |
| `isEmpty`                              | answers `Boolean` indicating if receiver size is zero                                                                         |
| `isLiteral`                            | always answers `true`                                                                                                         |
| `join:`                                | answers instance formed by joining `Array` elements of any type with receiver delimiter                                       |
| `match:`                               | answers `Boolean` indicating whether receiver matches a pattern                                                               |
| `padded:to:width:`                     | answers copy formed by padding receiver on left or right with a given `Character`                                             |
| `prefixAndSuffix:`                     | answers `Array` of instances formed by splitting receiver on last occurrence of a `Character`                                 |
| `size`                                 | answers largest legal index                                                                                                   |
| `squeezedTo:`                          | answers instance that optimizes readability of receiver in given number of characters                                         |
| `subStrings:`                          | answers `Array` of instances formed by splitting receiver on delimiters                                                       |
| `substrings`                           | answers `Array` of instances created by splitting receiver on whitespace                                                      |
| `substringsSeparatedBy:`               | answers `Array` of instances formed by splitting receiver on a single delimiter `Character`                                   |
| `truncateWithElipsisTo:`               | answers instance formed by truncating receiver to given length with elipsis in last 3 of length                               |
| `uncapitalized`                        | answers copy where first letter is changed to lowercase                                                                       |
| `withBlanksCondensed`                  | answers instance created by removing leading and trailing spaces and replacing consecutive spaces with one                    |
| `withBlanksTrimmed`                    | answers instance created by removing leading and trailing spaces                                                              |
| `withoutEnclosing:`                    | answers instance created by removing first and last characters if they match a given `Character`                              |
| `withoutLeadingBlanks`                 | answers instance created by removing leading blanks                                                                           |
| `withoutPrefix`                        | answers instance created by removing given substring prefix                                                                   |
| `withoutSuffix`                        | answers instance created by removing given substring suffix                                                                   |
| `withoutTrailingBlanks`                | answers instance created by removing trailing blanks                                                                          |

The following table describes some of the class methods
defined in the `String` class.

| Method                                  | Description                                                                                   |
| --------------------------------------- | --------------------------------------------------------------------------------------------- |
| `compare:with:`                         | answer 1 if `compare:` is less than `with:`, 2 if equal, and 3 if greater than                |
| `compareIgnoringCase:with:`             | same as `compare:with:`, but case is ignored                                                  |
| `crString`                              | answers instance containing the carriage return character                                     |
| `crlfString`                            | answers instance containing the carriage return and line feed characters                      |
| `findString:in:startingAt:`             | answers index of `findString:` in `in:` starting at index `startingAt:`                       |
| `findStringIgnoringCase:in:startingAt:` | same as `findString:in:startingAt:`, but case is ignored                                      |
| `is:equalTo:`                           | answers `Boolean` indicating if `is:` is equal to `equalTo:`                                  |
| `isAscii:`                              | answers `Boolean` indicating if all the characters are ASCII                                  |
| `isEmpy:`                               | answers `Boolean` indicating if size is zero                                                  |
| `lfString`                              | answers instance containing only a line feed character                                        |
| `new:withAll:`                          | answers instance with length `new:` where all characters are `withAll:`                       |
| `newLineString`                         | answers instance containing only a newline character                                          |
| `percentEscapingNonAscii`               | answers URL encoded instance where non-ASCII characters are percent encoded                   |
| `string:lineIndicesDo:`                 | evalautes block `lineIndicesDo:` for each substring of `string:` delimited by CR, LF, or CRLF |
| `substringsIn:`                         | answers an `Array` of substrings delimited by whitespace characters                           |
| `tab`                                   | answers instance containing the tab character                                                 |

To get a substring of a `String`, use the `copyFrom:to:` method
defined in `SequenceableCollection`. For example:

```smalltalk
'foobarbaz' copyFrom: 4 to: 6 "bar"
```

The following code demonstrates processing lines in a `String`:

```smalltalk
cr := String crString.
s := 'foo', cr, 'bar', cr, 'baz'.
"Alternate way to embed newline characters in a String"
s := 'foo
bar
baz'.

"Print each line."
String string: s lineIndicesDo: [:start :end :endWith |
    (s copyFrom: start to: end) print
].
"Alternate way to iterate over the lines in a String"
s substrings do: [:sub | sub print].
```

The following table describes some of the instance methods
defined in the `String` class
that are not also defined in its superclass `CharacterSequence`.

| Method                                  | Description                                                                                   |
| --------------------------------------- | --------------------------------------------------------------------------------------------- |
| `,`                                     | answers new string that results from appending argument                                       |
| `<`                                     | answers `Boolean` value indicating if receiver is less than argument                          |
| `<=`                                    | answers `Boolean` value indicating if receiver is less than or equal to argument              |
| `=`                                     | answers `Boolean` value indicating if receiver is equal to argument                           |
| `>`                                     | answers `Boolean` value indicating if receiver is greater than argument                       |
| `>=`                                    | answers `Boolean` value indicating if receiver is greater than or equal to argument           |
| `at:`                                   | answers `Character` at given index                                                            |
| `at:put:`                               | replaces `Character` at given index                                                           |
| `byteSize`                              | answers size in bytes                                                                         |
| `findString:startingAt:`                | answers index after `startingAt:` where a substring begins, case sensitive                    |
| `findStringCaseInsenstive:startingAt::` | answers index after `startingAt:` where a substring begins, case insensitive                  |
| `lineIndicesDo:`                        | evalautes block `lineIndicesDo:` for each substring of `string:` delimited by CR, LF, or CRLF |
| `percentEscapeUrl`                      | answers URL encoded instance where non-ASCII characters are percent encoded                   |
| `percentEscapeUrlField`                 | answers URL encoded instance where non-ASCII characters in fields are percent encoded         |
| `size`                                  | answers largest index                                                                         |
| `substrings`                            | answers `Array` of substrings delimited by whitespace characters                              |
| `unescapePercents`                      | answers reverse of `percentEscapeUrl`                                                         |

The `format:` method returns a `String` created from a template
using interpolation where input comes from an `Array`.
For example, both of the following produce the string
`'Player Gretzky is number 99.'`:

```smalltalk
s := 'Player {1} is number {2}.' format: #('Gretzky' 99).

name := 'Gretzky'.
number := 99.
s := 'Player {1} is number {2}.' format: {name. number}.
```

The `String` `format:` method is useful for print-style debugging.
For example, the following is the equivalent
of a `console.log` call in JavaScript.

```smalltalk
('myVariable = {1}' format: {myVariable}) print
```

An even better approach is to defined the `logAs:` method in the `Object` class.
This is described in the earlier "Transcript Windows" section.

The `padded:to:with:` method answers a copy formed by
padding receiver on the left or right with a given `Character`.
For example, the following code answers a `String`
containing three spaces followed by `'19'`:

```smalltalk
19 asString padded: #left to: 5 with: Character space
```

The `prefixAndSuffix:` method answers an `Array` of instances
formed by splitting receiver on last occurrence of a `Character`.
For exsample, the following code answers
an `Array` containing `'/foo/bar'` and `'baz.txt')`.

```smalltalk
'/foo/bar/baz.txt' prefixAndSuffix: $/
```

#### Symbols

There are no particularly interesting class methods in the `Symbol` class.

The following table describes some of the instance methods
defined in the `Symbol` class that are not also defined in superclasses.

| Method             | Description                                                                  |
| ------------------ | ---------------------------------------------------------------------------- |
| `asString`         | answers a `String` containing the same characters as receiver                |
| `isLiteral`        | always answers `true`                                                        |
| `numArgs`          | answers number of arguments in a keyword message or 0 if not                 |
| `precedence`       | answers 0 if not a valid selector, 1 if unary, 2 if binary, and 3 if keyword |
| `separateKeywords` | answers space-separated `String` containing keywords                         |
| `value:`           | answers result of sending receiver as a unary message to argument            |

Many of the `Symbol` instance methods are useful for
run-time evaluation of instances as keyword messages.

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

#### Association

An `Association` represents a key/value pair.
These are used by several other classes including `Dictionary` and `Bag`.

An `Association` instance can be created in the following ways:

```smalltalk
a := Association key: someKey value: someValue.
a := someKey -> someValue.
```

The message `->` is defined in the `Object` class
which make it easy to create an `Association` with any object as the key.

The following table describes some of the instance methods
defined in the `Association` class and its superclass `LookupKey`.

| Method       | Description                         |
| ------------ | ----------------------------------- |
| `key`        | answers the receiver key            |
| `key:`       | modifies the receiver key           |
| `key:value:` | modifies the receiver key and value |
| `value`      | answers the receiver value          |
| `value:`     | modifies the receiver alue          |

#### Collection

The following table describes some of the class methods
defined in the `Collection` class.
These must be called on a concrete subclass, not on `Collection`.

| Method                      | Description                                                         |
| --------------------------- | ------------------------------------------------------------------- |
| `with:`                     | answers an instance containing one value                            |
| `with:with:`                | answers an instance containing two values                           |
| `with:with:with:`           | answers an instance containing three values                         |
| `with:with:with:with:`      | answers an instance containing four values                          |
| `with:with:with:with:with:` | answers an instance containing five values                          |
| `withAll:`                  | answers an instance containing all the values in another collection |

The following table describes some of the instance methods
defined in the `Collection` class.

| Method                | Description                                                                                                                                |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `*`                   | answers instance whose elements are receiver elements times argument                                                                       |
| `+`                   | answers instance whose elements are receiver elements plus argument                                                                        |
| `,`                   | answers instance whose elements are the concatenation of receiver and argument collections                                                 |
| `-`                   | answers instance whose elements are receiver elements minus argument                                                                       |
| `/`                   | answers instance whose elements are receiver elements divided by argument                                                                  |
| `//`                  | answers instance whose elements are receiver elements integer divided by argument                                                          |
| `=`                   | answers `Boolean` indicating if receiver and argument are equivalent                                                                       |
| `\\`                  | answers instance whose elements are receiver elements modulo argument                                                                      |
| `abs`                 | answers instance whose elements are absolute value of receiver elements                                                                    |
| `allSatisfy:`         | answers `Boolean` indicating if ALL elements satisfy a block; like `every` in JavaScript                                                   |
| `anySatisfy:`         | answers `Boolean` indicating if ANY elements satisfy a block; like `some` in JavaScript                                                    |
| `asArray`             | answers `Array` instance whose elements are those in receiver                                                                              |
| `asBag`               | answers `Bag` instance whose elements are those in receiver                                                                                |
| `asCommaStringAnd`    | answers comma-separated `String` where last elements are separated by "and"                                                                |
| `asIdentitySet`       | answers `IdentifySet` instance whose elements are those in receiver with no duplicates                                                     |
| `asOrderedCollection` | answers `OrderedCollection` instance whose elements are those in receiver                                                                  |
| `asSortedCollection`  | answers `SortedCollection` instance whose elements are those in receiver                                                                   |
| `asSortedCollection:` | same as `asSortedCollection`, but take block that defines sort order                                                                       |
| `asSet`               | answers `Set` instance whose elements are those in receiver with no duplicates                                                             |
| `atRandom`            | answers a random element                                                                                                                   |
| `average`             | same as `mean`                                                                                                                             |
| `ceiling`             | answers instances whose elements are the ceiling of receiver elements                                                                      |
| `collect:`            | answers instance whose elements are results of passing receiver elements to a block; like `map` in JavaScript                              |
| `collect:andFold`     | answers instance whose elements are results of passing receiver elements to a block; like `map` in JavaScript                              |
| `count:`              | answers number of receiver elements that satisfy argument block                                                                            |
| `detect:`             | answers first element in receiver that satisfies block argument; like `find` in JavaScript                                                 |
| `do:`                 | evaluates block argument for each element; like `forEach` in JavaScript                                                                    |
| `floor`               | answers instances whose elements are the floor of receiver elements                                                                        |
| `fold:`               | answers value that results from folding receiver elements with a block; like `reduce` in JavaScript                                        |
| `fold:ifEmpty:`       | like `fold:`, but specifies value to use if collection is empty                                                                            |
| `groupBy:`            | answers `Dictionary` where keys are values returned by passing each element to block argument and values are `OrderedCollection` instances |
| `ifEmpty:`            | evalutes block argument if collection is empty                                                                                             |
| `ifEmpty:ifNotEmpty:` | combines `ifEmpty:` and `ifNotEmpty:`                                                                                                      |
| `ifNotEmpty:`         | evalutes block argument if collection is not empty                                                                                         |
| `ifNotEmpty:ifEmpty:` | combines `ifNotEmpty:` and `ifEmpty:`                                                                                                      |
| `includes:`           | answers `Boolean` indicating if argument is an element of receiver                                                                         |
| `includesAllOf:`      | answers `Boolean` indicating if all elements in argument collection are elements of receiver                                               |
| `includesAnyOf:`      | answers `Boolean` indicating if any elements in argument collection are elements of receiver                                               |
| `inject:into:`        | similar to `fold:`, but can specify initial accumulator value; like `reduce` in JavaScript                                                 |
| `intersection:`       | answers instance that only includes elements present in receiver and argument collection                                                   |
| `isEmpty`             | answers `Boolean` indicating if collection does not contain any elements                                                                   |
| `max`                 | answers largest number element                                                                                                             |
| `mean`                | answers mean of number elements                                                                                                            |
| `min`                 | answers smallest number element                                                                                                            |
| `noneSatisfy:`        | answers `Boolean` indicating if NONE of the elements satisfy a block                                                                       |
| `notEmpty`            | answers `Boolean` indicating if collection is not empty                                                                                    |
| `occurrencesOf:`      | answers number of elements that are equal to argument                                                                                      |
| `product`             | answers product of number elements                                                                                                         |
| `range`               | answers difference between max and min values                                                                                              |
| `reduce:`             | same as fold:                                                                                                                              |
| `reject:`             | answers instance containing receiver elements that do not satisfy a block                                                                  |
| `select:`             | answers instance containing receiver elements that satisfy a block; like `filter` in JavaScript                                            |
| `select:thenCollect`  | combines `select:` and `collect:`                                                                                                          |
| `select:thenDo:`      | combines `select:` and `do:`                                                                                                               |
| `sizes`               | answers number of elements in receiver                                                                                                     |
| `sorted`              | answers instance containing all receiver elements in sorted order                                                                          |
| `sqrt`                | answers instance whose elements are square root of receiver elements                                                                       |
| `squared`             | answers instance whose elements are squared values of receiver elements                                                                    |
| `sum`                 | answers sum of receiver number elements                                                                                                    |
| `union:`              | answers `Set` instance that includes elements present in receiver or argument collection                                                   |

Collections support binary messages that operate on all the elements
and return a new array containing the results.
For example, `#(1 2 3) * 2` returns `#(2 4 6)`.

The `fold:` method uses the first element as the initial value
and folds in the remaining elements.
The `inject:into:` method takes an initial value
and folds in all the elements.

The following code demonstrates some of the methods described above:

```smalltalk
#('red' 'green' 'blue') asCommaStringAnd "gives 'red, green and blue'

#(1 2 3) inject: 0 into: [:acc :n | acc + n] "gives 6""

#(1 2 3 4) mean` "gives Fraction 5/2"
```

I implemented the method `asOxfordCommaAnd` so
the example above gives `'red, green, and blue;`.

#### SequenceableCollection

There are no particularly interesting class methods
in the `SequenceableCollection` class.

The following table describes some of the instance methods
defined in the `SequenceableCollection` class
that are not defined in the `Collection` superclass.

| Method                    | Description                                                                                                                   |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `+=`                      | modifies receiver number elements by adding argument to each                                                                  |
| `-=`                      | modifies receiver number elements by subtracting argument from each                                                           |
| `allButFirst`             | answers copy including all but first element                                                                                  |
| `allButFirst:`            | answers copy including all but first argument elements                                                                        |
| `allButFirstDo:`          | evaluates block argument with all but first element                                                                           |
| `allButLast`              | answers copy including all but last element                                                                                   |
| `allButLast:`             | answers copy including all but last argument elements                                                                         |
| `allButLastDo:`           | evaluates block argument with all but last element                                                                            |
| `at:ifAbsent:`            | answers element at `at:` index or `ifAbsent:` if index out of bounds                                                          |
| `atLast:`                 | answers element an index that is argument from end (1 for last)                                                               |
| `atLast:ifAbsent:`        | like `atLast:` but specifies value to return if not enough elements                                                           |
| `combinations:atATimeDo:` | evaluates `atATimeDo` once for every unique combination of `combinations:` elements                                           |
| `copyFrom:to:`            | answers instance containing only receiver elements from index `copyFrom:` to index `to:`                                      |
| `do:displayingProgress:`  | evaluates a block for each receiver element and displays a progress bar (see example below)                                   |
| `findFirst:`              | like `detect:`, but answers element index of value instead of element value                                                   |
| `findFirst:startingAt`    | like `findFirst:`, but starts search at a given index                                                                         |
| `findLast:`               | like `findFirst:`, but answers element index of last element that satisifies block argument                                   |
| `first`                   | answers first element in receiver                                                                                             |
| `first:`                  | answers copy of first argument elements in receiver                                                                           |
| `head:`                   | same as `first:`                                                                                                              |
| `includes:`               | answers `Boolean` indicating if receiver contains argument value                                                              |
| `indexOf:`                | answers first index of argument value or 0                                                                                    |
| `indexOf:startingAt:`     | answers index of argument value starting at a given index or 0                                                                |
| `keysAndValuesDo:`        | evaluates block argument with all key/value pairs                                                                             |
| `last`                    | answers last element in receiver                                                                                              |
| `last:`                   | answers copy of last argument elements in receiver                                                                            |
| `lastIndexOf:`            | answers last index of argument value or 0                                                                                     |
| `middle`                  | answers the middle element in receiver                                                                                        |
| `permutationsDo:`         | evaluates block argument with an `OrderedCollection` once for each permutation of all receiver elements                       |
| `polynomialEval:`         | answers result of using receiver number elements as polynomial coefficients with argument for `x`                             |
| `printStringWithNewline`  | answers `String` with newline between each element value                                                                      |
| `replace:`                | replace each element in receiver with result of passing it to argument block                                                  |
| `reverse`                 | same as `reversed`                                                                                                            |
| `reversed`                | answers copy with elements in reverse order                                                                                   |
| `shuffled`                | answers copy with elements in random order                                                                                    |
| `tail:`                   | same as `last:`                                                                                                               |
| `with:collect:`           | answers `Array` of results from evaluating `collect:` block with corresponding elements from receiver and `with:` collections |
| `with:do:`                | evaluates `do:` block with corresponding elements from receiver and `with:` collections                                       |
| `with:with:collect:`      | like `with:collect:`, but operates on three collections                                                                       |
| `with:with:do:`           | like `with:do:`, but operates on three collections                                                                            |
| `withIndexCollect:`       | like `collect:`, but passes element and index values to argument block                                                        |
| `withIndexDo:`            | like `do:`, but passes element and index values to argument block                                                             |
| `withNextDo:`             | evaluate argument block with each receiver element and the next element, using nil for next of last element                   |

In `polynomialEval:`, the first element is the constant,
the second is the `x` coefficient, the third is the `x^2` coefficient,
and so on.

The following code demonstrates using the `do:displayingProgress:` method.

<img alt="Cuis SequenceableCollection do:displayingProgress:" style="width: 20%"
  src="/blog/assets/cuis-SequenceableCollection-do-displayingProgress.png?v={{pkg.version}}">

```smalltalk
c := #(10 20 30 40 50).
delay := Delay forSeconds: 1.
c do: [:n |
    ('processing {1}' format: {n}) print.
    delay wait.
] displayingProgress: 'doing stuff'
```

#### ArrayedCollection

There are no particularly interesting methods in this class.

#### Array

`Array` instances are fixed-length, ordered collections.
Most of the interesting `Array` methods are defined in
the superclasses `SequenceableCollection` and `Collection`.

Compile-time literal arrays begin with `#(`, end with `)`,
and contain space-separated values.
For example, `#(true 7 'Tami' (Color red))`.

Run-time literal arrays begin with `{`, end with `}`,
and contain dot-separated values.
For example, `{name. breed}`.

The following table describes some of the instance methods
defined in the `Array` class
that are not defined in its superclasses.

| Method        | Description                                                                                                      |
| ------------- | ---------------------------------------------------------------------------------------------------------------- |
| `evalStrings` | answers `Array` whose elements are the results of evaluating receiver `String` elements as Smalltalk expressions |

To create an array of numbers from a range,
send the `#asArray` message to a `Range`.
For example, `(1 to: 5) asArray` returns `#(1 2 3 4 5)`.

In addition to the `Array` class that can hold values of any type,
there are predefined, type-specific array classes including
`ByteArray`, `ColorArray`,
`DoubleByteArray`, `DoubleWordArray`, `Int16PointArray`,
`FloatArray`, `Float32Array`, `Float32PointArray`, `Float64Array`,
`IntegerArray`, `PointArray`, `RunArray`, `RunNotArray`, and `WordArray`.

#### Interval

Instances of the `Interval` class represent a finite arithmetic progression
which is a sequence of numbers where
the difference between consecutive terms is constant.
An example is the numbers 2, 4, 6, and 8.

The `Interval` class is a subclass of `SequenceableCollection`
which is a subclaass of `Collection`.

The following table describes some of the class methods
defined in the `Interval` class.

| Method                   | Description                                                                                                             |
| ------------------------ | ----------------------------------------------------------------------------------------------------------------------- |
| `from:to:`               | answers an instance where the increment between values is 1                                                             |
| `from:to:by:`            | answers an instance where the increment between values is `by:`                                                         |
| `from:to:count:`         | answers an instance where the number of values is `count:` and the increment can be a `Fraction`                        |
| `integersFrom:to:count:` | answers an instance where the number of values is `count:` and the increment is the closest `Integer`, not a `Fraction` |

The following table describes some of the instance methods
defined in the `Interval` class that are not also defined in superclasses.

| Method       | Description                                                   |
| ------------ | ------------------------------------------------------------- |
| `at:`        | answers the value at a given index                            |
| `do:`        | evaluates a block for each value from first to last           |
| `extent`     | answers the difference between the last and first values      |
| `first`      | answers the first value                                       |
| `includes:`  | answers `Boolean` indicating if argument is one of the values |
| `increment`  | answers the increment between values                          |
| `isEmpty`    | answers `Boolean` indicating if the size is 0                 |
| `last`       | answers the last value                                        |
| `size`       | answers the number of values                                  |
| `reverseDo:` | evaluates a block for each value from last to first           |

#### LinkedList

`LinkedList` instances represent a singly-linked list of `Link` subclasses.
Custom `Link` subclasses must be defined.
For example, the `StringLink` class defined below
has a single instance variable named "value".

```smalltalk
Link subclass: #StringLink
    instanceVariableNames: 'value'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'!
```

The `StringLink` class has the following class method for creating instances.

```smalltalk
value: aString
    | link |
    link := self new.
    link setValue: aString.
    ^ link
```

The `StringLink` class has the following instance methods.

```smalltalk
setValue: aString
    value := aString

value
    ^value

value: aString
    value := aString
```

We can now create an instance of `LinkList`
whose links are `StringLink` instances.

```smalltalk
list := LinkedList new.
list addLast: (StringLink value: 'banana').
list addLast: (StringLink value: 'cherry').
list addFirst: (StringLink value: 'apple').

"Output all the link values which are apple, banana, and cherry."
list do: [:link | link value print].

list size print. "3"

('first is {1}' format: {list first value}) print. "apple"
('last is {1}' format: {list last value}) print. "cherry"

list removeLast. "removes cherry"

"Output all the link values which are apple and banana."
list do: [:link | link value print].
```

`Link` instances have the methods `nextLink` and `nextLink:`
to get and set the link to which they refer.
In addition to the `LinkedList` methods described below,
these can be used to insert new `Link` instances into a `LinkedList`.

The following table describes some of the instance methods defined in the
`LinkedList` class that are not also defined in superclasses.

| Method             | Description                                                                  |
| ------------------ | ---------------------------------------------------------------------------- |
| `add:`             | same as `addLast:`                                                           |
| `add:before:`      | adds `add:` `Link` before `before:` `Link`                                   |
| `addFirst:`        | adds argument `Link` to beginning                                            |
| `addLast:`         | adds argument `Link` to end                                                  |
| `at:`              | answers `Link` at argument index; error if absent                            |
| `at:ifAbsent:`     | answers `Link` at `at:` index or value of `ifAbsent:` if absent              |
| `do:`              | evaluates argument block for each `Link`                                     |
| `first`            | answers first `Link`                                                         |
| `isEmpty`          | answers `Boolean` indicating if `size` is 0                                  |
| `last`             | answers last `Link`                                                          |
| `remove:ifAbsent:` | removes `remove:` `Link` and answers it; answers `ifAbsent:` value if absent |
| `removeFirst`      | removes and answers first `Link`; treats like a queue                        |
| `removeLast`       | removes and answers last `Link`; treats like a stack                         |

#### OrderedCollection

`OrderedCollection` instances are variable-length, ordered collections
that can contain duplicates.

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

The following table describes some of the instance methods defined in the
`OrderedCollection` class that are not also defined in superclasses.

| Method                | Description                                                                       |
| --------------------- | --------------------------------------------------------------------------------- |
| `add:`                | adds argument to end                                                              |
| `add:after:`          | adds `add:` object after `after:` object                                          |
| `add:afterIndex:`     | adds `add:` object after `afterIndex:` index                                      |
| `add:before:`         | adds `add:` object before `before:` object                                        |
| `add:beforeIndex:`    | adds `add:` object before `beforeIndex:` index                                    |
| `addAllFirst:`        | adds all objects in argument `OrderedColection` at beginning                      |
| `addAllLast:`         | adds all objects in argument `OrderedColection` at end                            |
| `at:`                 | answers element at argument index                                                 |
| `at:ifAbsentPut:`     | answers element at `at:` index; if not present, add value of `ifAbsentPut:` block |
| `at:put:`             | replaces existing element at `at:` index with `put:` object                       |
| `collect:thenSelect:` | combines `collect:` and `select:`                                                 |
| `find:`               | answers index of first occurrence of argument value                               |
| `removeAll`           | removes all elements                                                              |
| `removeAllSuchThat:`  | removes all elements that satisfy argument block                                  |
| `removeAt:`           | removes element at argument index                                                 |
| `removeFirst`         | removes first element and answers it                                              |
| `removeFirst:`        | removes first argument elements and answers `Array` of them                       |
| `removeLast`          | removes first element and answers it                                              |
| `removeLast:`         | removes last argument elements and answers `Array` of them                        |
| `sort`                | sorts the elements in place                                                       |
| `sort:`               | sorts the elements in place using argument block to compare them                  |

The following code creates an instance of `OrderedCollection`
containing `Symbol` objects that are names of fruits.
It then sorts them on their length.

```smalltalk
oc := OrderedCollection newFrom: #(#banana #watermelon #cherry #apple #plum).
oc sort: [:a :b | a size < b size].
```

#### SortedCollection

The `SortedCollection` class is a subclass of `OrderedCollection`.
Instances keep their elements in sorted order.
The element objects must implement the `<=` instance method.

The following code demonstrates defining a `Dog` class
whose instances can used as elements in a `SortedCollection`.

```smalltalk
Object subclass: #Dog
    instanceVariableNames: 'breed name'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'

"class methods follow"

name: nameString breed: breedString
    ^self new setName: nameString breed: breedString

"instance methods follow"

breed
    ^ breed

name
    ^ name

<= aDog
    ^ self name <= aDog name

setName: nameString breed: breedString
    name := nameString.
    breed := breedString
```

The following code demonstrates creating a `SortedCollection` of `Dog` objects
and printing their names.

```smalltalk
comet := VDog name: 'Comet' breed: 'Whippet'.
oscar := VDog name: 'Oscar' breed: 'German Shorthaired Pointer'.
dogs := SortedCollection newFrom: {oscar. comet}.
dogs do: [:dog | dog name print]
```

The output in the Transcript will be "Comet" followed by "Oscar".

#### Bag

`Bag` instances are variable-length, unordered collections
that can contain duplicates.
Elements are stored in a dictionary where the keys are the objects
and the values are their number of occurrences.

The following table describes some of the instance methods
defined in the `OrderedCollection` class that are
not also defined in the superclass `Collection`.

| Method               | Description                                                                                         |
| -------------------- | --------------------------------------------------------------------------------------------------- |
| `sortedCounts`       | answers a `SortedCollection` of `Association` objects where keys are counts and values are elements |
| `sortedElements`     | answers a `SortedCollection` of `Association` objects where keys are elements and values are counts |
| `withOccurrencesDo:` | evaluates argument block for each unique element, passing it an element and its count               |

The following code creates a `Bag` instance containing `String` fruit names,
including many duplicates. It then sends several messages to the `Bag`.

```smalltalk
b := Bag newFrom: #('apple' 'banana' 'cherry' 'apple' 'cherry' 'apple').
b sortedCounts print.
b sortedElements print.
b withOccurrencesDo: [:obj :count |
   ('{1} occurs {2} times.' format: {obj. count}) print
].
```

The code above produces the following output in the Transcript:

```text
a SortedCollection(3 -> 'apple' 2 -> 'cherry' 1 -> 'banana')
a SortedCollection('apple' -> 3 'banana' -> 1 'cherry' -> 2)
cherry occurs 2 times.
apple occurs 3 times.
banana occurs 1 times.
```

#### Set

`Set` instances are unordered collections of unique values.
The elements must supports the `=` and `hash` messages and cannot be `nil`.

To create an empty `Set`:

```smalltalk
set := Set new
```

To create a `Set` that is populated from an `Array`:

```smalltalk
set := Set newFrom: #('apple' 'banana' 'cherry' 'apple')
```

The following code demonstrates adding an element, removing an element,
and testing for the existence of an element:

```smalltalk
set add: `orange`.
set remove: 'banana'.
set includes 'apple`. "true"
set includes 'banana`. "false"
```

#### Dictionary

`Dictionary` instances are collections of key/value pairs.
The keys are often symbols, but they can be
any kind of object that supports the `=` and `hash` messages.
The values can be any kind of object.

To create a `Dictionary`:

```smalltalk
dict := Dictionary new.

"Can populate from a run-time Array of Association objects."
dict := Dictionary newFrom: { k1 -> v1. k2 -> v2. ... }.

dict := { k1 -> v1. k2 -> v2. ... } asDictionary.
```

It is not possible to create a `Dictionary` instance
from a compile-time `Array` of `Association` objects
because `Association` objects cannot be created at compile-time.
This means the following cannot be used:

```smalltalk
#( k1->v1 k2->v2 ... )
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

The following table describes some of the instance methods
defined in the `Dictionary` class that are
not also defined in the superclasses `Set` or `Collection`.

| Method                      | Description                                                                                                        |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `addAll:`                   | adds key/value pairs described by a `Collection` of `Association` objects                                          |
| `associationAt:`            | answers `Association` for argument key; error if not found                                                         |
| `associationAt:ifAbsent:`   | answers `Association` for `associationAt:` key or `ifAbsent:` value if not found                                   |
| `associations`              | answers `Array` of `Association` objects for all key/value pairs                                                   |
| `associationsDo:`           | evaluates argument block for each `Association`                                                                    |
| `at:`                       | answers value for argument key; error if not found                                                                 |
| `at:ifAbsent:`              | answers value for `at:` key or `ifAbsent:` value if not found                                                      |
| `at:ifAbsentPut:`           | answers value for `at:` key; if not found, adds and answers `ifAbsentPut:` value                                   |
| `at:ifPresent:`             | if `at:` key is present, answers value of passing value to `ifAbsent` block; otherwise answers `nil`               |
| `at:ifPresent:ifAbsent:`    | combines `at:ifPresent` and `at:ifAbsent`                                                                          |
| `at:put:`                   | adds key `at:` with value `put:`                                                                                   |
| `bindingOf:`                | answers `Assocication` for argument key                                                                            |
| `bindingsDo:`               | same as `associationsDo`                                                                                           |
| `hasBindingThatBeginsWith:` | answers `Boolean` indicating if any `String` key begins with argument; error if non-`String` keys                  |
| `includesKey:`              | answers `Boolean` indicating if argument key is present                                                            |
| `keys`                      | answers `Array` of all keys                                                                                        |
| `keysAndValuesDo:`          | evaluates argument block for each key/value pair passing key and value arguments to block                          |
| `keysAndValuesRemove:`      | removes all key/value pairs that satisfy argument block, passing key and value arguments to block                  |
| `keysDo:`                   | evaluates argument block for each key                                                                              |
| `removeKey:`                | removes key/value pair for argument key                                                                            |
| `removeKey:ifAbsent:`       | removes key/value pair for `removeKey:` key; answers removed value if present; otherwise answers `ifAbsent:` value |
| `select:`                   | answers new `Dictionary` containing all key/value pairs whose value satisfies argument block                       |
| `values`                    | answers `Array` of all values                                                                                      |
| `valuesDo:`                 | evaluates argument block for each value                                                                            |

#### OrderedDictionary

The `OrderedDictionary` class is a subclass of `Dictionary`.
Instances remember the order in which entries were added.
Iteration methods like `do:`, `keysDo:` and `associationsDo:` and `valuesDo:`.
process the entries in that order.

The following code demonstrates creating an `OrderedDictionary`
whose values are objects from the `Dog` class
defined the `SortedCollection` section above,
and printing their names.

```smalltalk
comet := VDog name: 'Comet' breed: 'Whippet'.
oscar := VDog name: 'Oscar' breed: 'German Shorthaired Pointer'.
dict := OrderedDictionary new.
dict at: comet name put: comet.
dict at: oscar name put: oscar.
dogs do: [:dog | dog name print]
```

The output in the Transcript will be "Comet" followed by "Oscar".

#### Identity Collections

The collection classes `IdentityBag`, `IdentityDictionary`, and `IdentitySet`
are similar to their non-identity counterparts,
but differ in that elements are compared using `==` instead of `=`.
This means it checks to see if they are the same object in memory
rather than checking for equal values.

#### Weak Collections

The collection classes `WeakArray`, `WeakKeyDictionary`,
`WeakIdentityKeyDictionary`, `WeakValueDictionary`,
`WeakSet`, and `WeakIdentitySet`
are similar to their non-week counterparts,
but they differ in that objects to which they refer
are not prevented from being garbage collected
if they are only referred to by weak collections.

If an attempt is made to use an element in one of these collections
that has been gargage collected, its value will be `nil`.

For example, create a class named `WeakSetDemo` with
the instance variable `set` and the following instance methods:

```smalltalk
initialize
    | comet oscar |
    comet := Dog name: 'Comet' breed: 'Whippet'.
    oscar := Dog name: 'Oscar' breed: 'GSP'.
    set := WeakSet newFrom: {comet. oscar}.
    set do: [:dog | dog name print].

first
    ^ set asArray first
```

Enter the following lines in a Workspace, select them, and "Print it":

```smalltalk
demo := WeakSetDemo new. "invokes initialize"
demo first
```

The result will be `nil` because the `comet` and `oscar` objects
will be garbage collected after the `initialize` method executes.

## Exception Handling

Methods can throw exceptions.
Exceptions that are thrown by code in a block can be caught and handled.
Unhandled exceptions result in a Debug window being opened
that contains a stack trace.

Smalltalk seems to use the words "exception" and "error" interchangably.

To throw a generic `Exception`:

```smalltalk
Error signal: 'some message'
```

Smalltalk provides many subclasses of the `Exception` class.
Examples include `ArithmeticError`, `AssertionFailure`, `Error`, `Halt`,
`MessageNotUnderstood`, `NotYetImplemented`, and `ZeroDivide`.

To define a custom exception, create a class that is a subclass
of the `Exception` class or one of its subclasses such as `Error`.
The custom class can include instance variables and methods
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
lower: lowerNumber upper: upperNumber
    ^self new setLower: lowerNumber upper: upperNumber
```

The following instance method is used by the class method above:

```smalltalk
setLower: lowerNumber upper: upperNumber
    super initialize.
    lowerBound := lowerNumber.
    upperBound := upperNumber
```

If an exception subclass defines class methods that create a new instance,
make sure to call `super initialize` as shown above.

To throw a custom exception send the class, or an instance of it,
the `#signal:` message.
For example, the following instance method can be defined
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
and send the `#on:do:` message to the block.
Sending the message `#messageText` to an exception object
returns the message text of the exception.

For example:

```smalltalk
[s := Game new score: 5] on: OutOfBoundsException do: [ :ex |
    ex messageText print.
]
```

## Unit Tests

Unit tests verify that the code is working as expected now.
They can also be run again in the future
to verify that the code has not regressed.

One way to learn how to write Smalltalk unit tests
is to install some tests provided in the Smalltalk distribution.
and study them. To do this:

- Open a "File List" from the World menu.
- Navigate to and expand `Cuis-Smalltalk-Dev`
  or the name of your version of Cuis.
- Navigate to and expand "Packages" and then "Features".
- Enter "test" in the filter input in the upper-left.
- Select one of more of the packages whose names begin with "Tests-".
- Click the "install package" button.
- View the code for those packages in a System Browser.

Let's walk through the steps to create and run unit tests for a class.

The class we will test is `Pets` which is defined by the following:

```smalltalk
Object subclass: #Pets
    instanceVariableNames: 'dogs cats'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

The `Pets` class has the following instance methods:

```smalltalk
initialize
    dogs := 0.
    cats := 0

addCat
    cats := cats + 1

addDog
    dogs := dogs + 1

cats
    ^ cats

dogs
    ^ dogs
```

The supported assertion methods defined in the `TestCase` class include:

- `assert:`

  This asserts that the value of the argument is `true`
  or is a block whose value is `true`.

- `assert:changes:`

  This asserts that value of the `changes:` block
  changes after evaluating the `assert:` block.

- `assert:changes:by`

  This asserts that value of the `changes:` block
  changes by `by:` after evaluating the `assert:` block.

- `assert:changes:from:to`

  This asserts that value of the `changes:` block
  changes from `from:` to `to:` after evaluating the `assert: block`.

- `assert:description`

  This asserts that the value of `assert:` is `true`.
  If not, the test files with the message `description:`.

- `assert:description:resumable`

  TODO: What is a `resumableFailure`?

- `assert:doesNotChange`

  This is the opposite of `assert:changes:`.
  It asserts that value of the `doesNotChange:` block
  does not change after evaluating the `assert:` block.

- `assert:equals`

  This asserts that the value of `assert:` (not a block) is equal to `equals:`.

- `assert:includes`

  This asserts that the collection `assert:`
  includes an element equal to `includes:`.

For comparing floating point numbers, consider adding
the following instance methods to the `TestCase` class.

```smalltalk
assert: aNumber isCloseTo: anotherNumber
    "This asserts that the value of `assert:` is
    within the default precision (0.0001) of `isCloseTo:`."

    self assert: aNumber isCloseTo: anotherNumber
        withPrecision: self defaultPrecision
assert: aNumber isCloseTo: anotherNumber withPrecision: aPrecision
    "This asserts that the value of `assert:`
    is within `withinPrecision:` of `isCloseTo:`."

    self assert:
        (self is: aNumber closeTo: anotherNumber withPrecision: aPrecision)

defaultPrecision
    ^ 0.0001

is: aNumber closeTo: anotherNumber withPrecision: aPrecision
    aNumber = 0 ifTrue: [ ^ anotherNumber abs < aPrecision ].
    ^ (aNumber - anotherNumber) abs <
      (aPrecision * (aNumber abs max: anotherNumber abs))
```

To create unit tests for the `Pets` class:

- Create a new class in the same class category as the class to be tested
  that is a subclass of `TestCase`. For example:

  ```smalltalk
  TestCase subclass: #PetsTests
      instanceVariableNames: ''
      classVariableNames: ''
      poolDictionaries: ''
      category: 'Volkmann'
  ```

- Add the message category "testing".

- Add instance methods in the "testing" category whose names begin with "test".
  Each method can contain any number of assertions.
  For example:

  ```smalltalk
  testDogs
      | demo |
      demo := Pets new.

      "dogs is now 0."
      self assert: [ demo addDog ] changes: [ demo dogs].

      "dogs is now 1."
      self assert: [ demo addDog ] changes: [ demo dogs] by: 1.

      "dogs is now 2."
      self assert: [ demo addDog ] changes: [ demo dogs] from: 2 to: 3.

      "dogs is now 3."
      self assert: demo dogs equals: 3.

      self assert: [ demo addCat ] doesNotChange: [ demo dogs].

  testCats
      | demo |
      demo := Pets new.
      self assert: [ demo cats = 0 ] description: 'no cats'.

  testCollections
      "This test isn't related to the Pets class."
      | coll |
      coll := #(2 5 9).
      self assert: coll includes: 5

  testNumbers
      "This test isn't related to the Pets class."
      self assert: Float pi isCloseTo: 3.14159
  ```

To run tests, select a test class, test method category, or test method,
and press cmd-t (run tests).
Alternatively, open a "SUnit Test Runner" from the World menu,
select one or more test classes, and click the "Run" button.
After adding new test classes, click the "Refresh" button
to make the "SUnit Test Runner" window aware of them.

<img alt="Cuis SUnit Test Runner" style="width: 90%"
  src="/blog/assets/cuis-sunit-test-runner.png?v={{pkg.version}}">

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

Morphic is a GUI framework that is included into popular Smalltalk images.
It defines user interfaces with "morphs" which are
what other graphical systems refer to as widgets or components.
Morphs are graphical items that can be added to
the `WorldMorph` (desktop) or a `LayoutMorph`.
TODO: Technically speaking, can any morph be embedded in any other morph?

For a great introduction to Morphic, see
<a href="https://www.youtube.com/watch?v=62baNn3c56Y"
target="_blank">Holistic computing with Smalltalk and Morphic. Part 1</a>.

Some of the methods in Morphic classes are inconsistently named.
For example, the class `TextModelMorph` defines the methods
`alwaysHideVerticalScrollbar` and `hideScrollBarsIndefinitely`.
Note how the "b" is sometimes lowercase and sometimes uppercase.

### Creating and Modifying Morphs

To create a morph:

- Open the World menu.
- Select "New morph...".
- In the dialog that appears, select a category of morphs
  and then a specific kind (ex. `BoxedMorph`).
- The morph will appear attached to the cursor
  (uses the `Morph` method `openInHand`).
- Move the cursor to the location where the morph should be placed
  and click to drop it.

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
To install this, open a Workspace, enter `Feature require: 'UI-Tools'`,
and press cmd-d (Do it).
This installs many other packages including:

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
`Sonogram`, and `TileResizeMorph`.
TODO: Why does selecting `Sonogram` lock up the image, requiring a Force Quit?

TODO: Supposedly Cuis 7 will remove support for the UI-Tools package
and the desired subpackages will need to be installed individually.
Still true?

See my Morphic demos in package `Volkmann`
in the classes `MorphicDemos` and `MorphicGreet`.
In a Workspace, enter `MorphicDemos incDecButtons.` and "Do it".
Then enter `MorphicGreet new.` and "Do it".

### Halo

A morph halo is a set of circle buttons that surround a morph
that each change the morph in some way or reveal information about it.

To open the halo for a morph,
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

(2) It seems most of the help tooltips default to "Help not yet supplied".
To edit the help text, click the orange circle on the right,
select "edit balloon help", and modify the help text.

(3) To restore a collapsed item, click it's thumbnail in the bottom bar.

(4) "Explore" windows enable viewing data associated with an item
such as its location, extent (size), and color.

For more detail, see
<a href="https://cuis-smalltalk.github.io/TheCuisBook/A-brief-introduction-to-Inspectors.html"
target="_blank">A brief introduction to Inspectors</a>.

### Desktop Color

One way to change the desktop color is to:

- Right-click the desktop to open its halo.
- Click the blue menu button.
- Select debug...inspect morph to open an Inspect window.
- Click in the bottom pane of the Inspect window.
- Enter `self color: Color red` or use some other color name.
- "Do it".

This works with all colors except `transparent` and alpha values are ignored.

### Embedding

To embed a morph into another (such as a LayoutMorph) so
they are treated as a single unit and can be positioned together:

- Drag a morph on top of its intended parent morph.
- Open the halo of the morph.
- Click the blue circle on the top.
- Select "embed into" ... {parent morph name}.
  The parent morph name is typically "LayoutMorph".

### LayoutMorph

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

### Editing Characteristics

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

### Button Labels

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

### Button Demo in Cuis

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

### Canvas Drawing

To draw on a canvas, create a subclass of `Morph` as follows:

```smalltalk
Morph subclass: #CanvasDemo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Volkmann'
```

Then define the instance method `drawOn:`.
For example, the following draws a red rectangle
with a line from its upper-left to lower-right.

```smalltalk
drawOn: aCanvas
	| height width x1 x2 y1 y2 |
	height := 200.
	width := 400.
	x1 := 50.
	y1 := 50.
	x2 := x1 + width.
	y2 := y1 + height.
	aCanvas strokeWidth: 20 color: Color red do: [
	aCanvas
 		moveTo: x1 @ y1;
	 	lineTo: x2 @ y2;
		lineTo: x2 @ y1;
		lineTo: x1 @ y1;
		lineTo: x1 @ y2;
		lineTo: x2 @ y2
    ]
```

TODO: Change this to use the extend of the morph instead of hard-coded height and width values.

Open a Workspace, enter `CanvasDemo new openInWorld` and "Do it".

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

### Running Headless

To run Smalltalk programs that are command-line utilities, apps, and servers,
use the Smalltalk VM that is bundled inside the macOS app
`CuisVM.app` that is included in the Cuis-Smalltlk-Dev GitHub repository.
This is actually a Squeak VM.

Squeak VMs can also be obtained from
<a href="https://github.com/OpenSmalltalk/opensmalltalk-vm"
target="_blank">OpenSmalltalk</a>.
Under "Releases" on the right side, click "Latest".
Click the proper file for your operating system and processor.
For example, `squeak.cog.spur_macos64ARMv8.dmg`.
Double-click the downloaded file to install it.

Alternatively, to build a Squeak VM for macOS:

1. Open a Terminal and cd to the directory where the VM will be created.
1. Enter `git clone https://github.com/OpenSmalltalk/opensmalltalk-vm.git`
1. Enter `cd opensmalltalk-vm`
1. Enter `./scripts/updateSCCSVersions`
1. Enter `cd building`
1. `cd` to the appropriate subdirectory for the target OS and processor.
   For example, `macos64ARMv8`.
1. See the instructions in the file "HowToBuild".
1. Enter `cd squeak.coq.spur`
1. Enter `./mvm -A`. This will run for around 10 minutes and
   generate an extreme amount of output.
1. Enter `chmod a+x Squeak.app`
1. Create a symbolic link to the VM that is inside this app
   by entering `ln -s "./Squeak.app/Contents/MacOS/Squeak" squeak-vm`.

To simplify running files containing Smalltalk code from the command line,
create a script named "cuis" in a directory in your PATH
containing the following:

```bash
#!/usr/bin/env bash
# Runs a file containing Smalltalk code in headless mode using Cuis Smalltalk.

if [ $# -ne 1 ]; then
  echo usage: cuis {file-name}
  exit 1
fi

CUIS_DIR=$LANG_DIR/smalltalk/Cuis-Smalltalk-Dev
VM=$CUIS_DIR/CuisVM.app/Contents/MacOS/Squeak
IMAGE=$CUIS_DIR/CuisImage/Cuis7.1-6452.image
$VM -headless $IMAGE -s $1
```

Create `.st` files containing Smalltalk code.
For example, the file `demo.st` could contain the following:

```smalltalk
| stdout |
stdout := StdIOWriteStream.
stdout nextPutAll: 'Hello, World!'; newLine.
Smalltalk quit
```

To run this, enter `cuis demo.st`.

To get help on options, cd to your `Cuis-Smalltalk-Dev` directory
and enter the following:

```bash
./CuisVM.app/Contents/MacOS/Squeak -help
```

For more detail see the `SystemDictionary` class
`displayCommandLineUsageOn:` class method.

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

## Editor Customization

I would like to implement a subset of the Vim keybindings
for editing code in System Browsers.
Here is what I have tried so far.

- Create a subclass of the `SmalltalkEditor` class called `VimEditor`.

- Add the instance variable `mode`
  that will hold the string "normal" or "insert".

- Initialize `mode` to `'insert'` by adding the following instance method:

  ```smalltalk
  initialize
      mode := 'insert'.
  ```

- Implement the class method `initializeShortcuts` as follows:

  ```smalltalk
  initializeShortcuts
      super initializeShortcuts.
      shortcuts
          at: 27 + 1
          put: #switchToCommandMode:.
      "escape key - when in insert mode, switch to command mode"
      shortcuts
          at: 105 + 1
          put: #xKey:.
      "i key - when in command mode, switch to insert mode"
      shortcuts
          at: 120 + 1
          put: #xKey:.
      "x key - when in command mode, delete character under cursor"
  ```

- Implement the following instance methods:

  ```smalltalk
  escapeKey: aKeyboardEvent
      | inCommandMode |
      inCommandMode := mode = 'command'.
      inCommandMode ifFalse: [ mode := 'command' ].
      "Hopefully returning false means that a superclass can process the event."
      ^ inCommandMode not.

  iKey: aKeyboardEvent
      | inCommandMode |
      inCommandMode := mode = 'command'.
      inCommandMode ifTrue: [ mode := 'insert' ].
      "Hopefully returning false means that a superclass can process the event."
      ^ inCommandMode.

  xKey: aKeyboardEvent
      | inCommandMode |
      inCommandMode := mode = 'command'.
      inCommandMode ifTrue: [ 'delete character under cursor' print ].
      "Hopefully returning false means that a superclass can process the event."
      ^ inCommandMode.
  ```

- TODO: Determine how to modify the System Browser to use `VimEditor`
  instead of `SmalltalkEditor` in its bottom pane.

- TODO: Determine how to modify the `xKey:` method to really delete a character.

Changes will likely have no effect until the image is saved and restarted.

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
