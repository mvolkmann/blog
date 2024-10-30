---
eleventyNavigation:
  key: Quick Introduction to Smalltalk
  order: 1
  parent: Smalltalk
layout: topic-layout.njk
---

<style>
   code {
     color: purple;
     font-weight: bold;
   }
</style>

"The further backward you can look, the further forward you are likely to see."
— Winston Churchill

"The single most important thing I have learned about software development
over my career is that if you do not aggressively fight complexity,
it will eat you alive." - Vicki Boykis

## Overview

The Smalltalk programming language was created at
Xerox PARC (Palo Alto Research Center) in the 1970s.
It is a purely object-oriented programming (OOP) language.
Everything is represented by an object that is an instance of some class.
This includes classes themselves and
all the GUI elements in the development environment.
Everything happens by sending messages to objects.
The objects decide whether and how to act on the messages.

Smalltalk was the first popular OOP.
In the 1990's its popularity had risen to the point
that it was seen as an alternative to C++.
In 1995 Smalltalk had a 15% market share compared to 71% for C++.

The characteristic of Smalltalk that first attracted me
is its simple, minimal syntax.
The entire syntax of Smalltalk can be demonstrated on a post card.

<img alt="Smalltalk on a post card"
  src="/blog/assets/smalltalk-on-postcard.jpg?v={{pkg.version}}">

Smalltalk is a dynamically typed language.
Types of variables, method parameters,
and method return types are never specified.
Instead, duck typing is used. Any object can be used as long as it
is able to respond to all the messages that are sent to it.
This is determined at run-time.

Running Smalltalk programs requires two parts,
a virtual machine (VM) and an image file.
The VM reads and executes Smalltalk code found in an image file.
It is specific to the operating system and CPU architecture being used.

Image files can be moved between
operating systems and different CPU architectures.
It will render the same windows pixel for pixel,
across environments, only differing based on screen size.
There is no need to recompile code for different environments.
This makes Smalltalk code highly portable.

An image file can be thought of as a snapshot
of the current state of the development environment.
It describes the collection of all active objects.
During development, changes can be saved to the current image
or to a new image.

Today Smalltalk is still used by the financial industry,
manufacturing, utilities, transportation, and academia.

## Bit of History

Squeak Smalltalk, released in 1996,
was the first free, open-source implementation.
Until then all Smalltalk implementations were commericial and expensive.
The Java programming language was released in 1995 and was free.
That removed most of the wind from the sails of Smalltalk.

Today many developers assume that Smalltalk has
receded into the history of computer programming.
However, Smalltalk is alive and well outside the United States.
Two free, open-source forks of Squeak have been created, Pharo and Cuis.

Pharo Smalltalk was created in France and
aims to provide a more comprehensive feature set than Squeak.

Cuis Smalltalk was created in Argentina and
aims to remain small and easy to learn.
Missing features can be implemented by combining the features that are present
and by installing more packages.
The Cuis mascot is a southern mountain cavy found in Argentina,
which is a tailless rodent that looks similar to a very large mouse.
The word "cuis" in Rioplatense Spanish means "squeak".

Squeak, Pharo, and Cuis all use the MIT license.

There are also many commercial Smalltalk implementations including
Cincom Smalltalk, GemStone/S, and VA Smalltalk.

## Development Environment

Smalltalk is perhaps most known for its incredible development tools.
These support:

- a live environment where code changes are immediately reflected
  (no need to recompile or restart the environment)
- finding code in many ways
- modifying the classes and methods that implement the development environment
  as easily as modifying your own classes and methods
- inspecting objects to see their class and instance variables
- debugging with the ability to modify code and data, then continue or restart
- rapid prototyping

## Why Learn Smalltalk Now?

Learning Smalltalk will enable you to:

- Experience a beautifully minimal syntax.
- Gain an understanding of its pros and cons compared to other languages.
- Get ideas for features that can be added to other languages
  and their development enviroments.
- Actually use it as an alternative to other languages.
- Have fun!

## Control Structures

Part of simplicity of Smalltalk is that it
doesn't have special syntax for control structures.
Other programming languages support statements like
`if`, `switch`, `for`, and `while`.
In Smalltalk, the same functionality is provided by methods
in classes like `Boolean` and `Collection`.

These Smalltalk methods take arguments that are "blocks".
A block is like an anonymous function in other programming languages.
Blocks are delimited by square brackets, take any number of arguments,
contain any number of expressions, and
automatically returns the value of their last expression.

The following code prints an evaluation of the temperature.
The `Boolean` method `ifTrue:ifFalse:` takes two blocks as arguments.
This code can be written on a single line or split over multiple lines.

```smalltalk
temperature > 80
    ifTrue: [ 'hot' print ]
    ifFalse: [ 'not' print ]
```

The following code iterates over all the elements in an `Array` and prints them.
The block passed to the `do:` method takes a single argument
represented by the variable `score` inside the block.
The vertical bar separates the block parameter list from its expressions.

```smalltalk
scores do: [:score | score print]
```

## Taste of Smalltalk

To really get a feel for what it's like to use Smalltalk,
you have to try it. Follow the instructions below
to install, start, and use Cuis Smalltalk.

To install Cuis Smalltalk:

1. Browse the <a href="https://cuis.st" target="_blank">Cuis Smalltalk home page</a>.
1. Click the "Download" link at the top of the page.
1. Click "Cuis Stable Release" or "Cuis Rolling Release".
   Both lead to a GitHub repository. The later is recommended
   so you can obtain updates by simply running `git pull`.
1. Clone the repository.
   If the rolling release was selected,
   this will create the directory `Cuis-Smalltalk-Dev`.

To start Cuis Smalltalk, run the appropriate start script found
in the installed directory based on your operating system.

- For Windows, open a Command Prompt and enter `RunCuisOnWindows.bat`.
- For Linux, open a Terminal and enter `./RunCuisOnLinux.sh`.
- For macOS, open a Terminal and enter `./RunCuisOnMacTerminal.sh`.
  Alternatively, double-click the file `Cuis7.1-6770.image`
  in the `CuisImage` subdirectory.

The window that appears has a blue background by default.
This is referred to as the "World".
Clicking the World opens the World menu.
Click "Open" in that menu to see a submenu of
windows that can be opened inside the World.
We will look at four of these:
Workspace, Browser, Message Names, and Transcript.
Of these, only a Transcript window will be open by default in the initial image.

If the font size is not to your liking, open the World menu,
select "Preferences ... Size of GUI elements...", and select a different size.

A Workspace window supports entering and evaluating Smalltalk expressions.
It is similar to a read-eval-print-loop (REPL) in other programming languages.
Open a Workspace and enter the expressions shown in the following screenshot.
The text in red boxes are the results of "printing"
the value of the preceding expression.
Do not enter that text.
We will discuss how to print those results below.

<img alt="Cuis Smalltalk Workspace session"
  src="/blog/assets/cuis-workspace-session.png?v={{pkg.version}}"
  style="width: 50%">

The period character is used to separate, not terminate, expressions.

The third expressions assigns a value to the variable `cityToTeam`.
To assign the value of an expression to a variable,
enter a variable name, the assignment operator `:=`, and the expression.

To cause assignment operators to be rendered as a left pointing arrow
(because its stylish and cool), open the World menu
and select "Preferences ... Show ST-80 assignments".
The opposite setting is "Preferences ... Show ANSI assignments".
Regardless of this setting, if an underscore is typed in place of
the assignment operator, it will be rendered as a left pointing arrow.

There are shortcut keys for many actions in the development environment.
In macOS, these use the command key (cmd).
In other platforms these use the control key.
All the examples that follow use the command key,
so substitute the control key if you are not using a Mac.

With the cursor positioned at the end of an expression in the Workspace,
right-click and select "Do it" (cmd-d) or "Print it" (cmd-p).
Selecting "Do it" evaluates the expression, but does not output its value.
Selecting "Print it" evaluates the expression and outputs its value
immediately after the expression in the Workspace.
The result will be selected, so pressing the delete key will remove it.

Smalltalk supports three kinds of messages: unary (no arguments),
binary (one argument), and keyword (one or more arguments).

The first expression sends the unary message `asUppercase`
to the literal string `'demo'`.
Its result is the string `'DEMO'`.

The second expression sends the binary message `+`
to the number `2` with the argument `3`.
Its result is the number `5`.

The next three expressions demonstrate working with a `Dictionary`,
which you may know as a Map or HashMap in other programming languages.

1. The expression `cityToTeam := Dictionary new` sends the message `new`
   to the `Dictionary` class. That creates and returns a new instance.
1. The expression `cityToTeam at: 'Kansas City' put: 'Chiefs'`
   sends the keyword message `at:put:` to `cityToTeam`
   which adds a key/value pair to the `Dictionary`.
1. The expression `cityTeam at: 'Kansas City'`
   sends the keyword message `at:` to the `Dictionary`
   which returns the value at the specified key.
   In this case that is the string `'Chiefs'`.

The last expression, `#(1 2 3 4) average`, returns the `Fraction` `5/2`
rather than a `Float` in order to preserve accuracy.
It does this because the `average` method
returns the result of an integer division.
The syntax `#(1 2 3 4)` creates a compile-time `Array`.
`average` is an instance method defined in the `Collection` class
which is a superclass of the `Array` class.

We can examine the implementation of the `average` method.
To do so, use the World menu to open a Browser.
Brower windows contains four panes across the top row.

- The first pane displays a list of class categories.
- The second pane displays a list of classes in the selected class category.
  There are three buttons at the bottom of this pane.
  - The "instance" button causes the remaining panes to show instance methods.
  - The "?" button causes the bottom pane to display
    the comment for the selected class.
  - The "class" button causes the remaining panes to show class methods.
- The third pane displays a list of method categories in the selected class.
- The fourth pane displays a list of methods in the selected method category.

Move the mouse cursor over the first pane and press cmd-f for "find class...".
Enter "Array" and press return.
The `Array` class will be selected in the second pane.

<img alt="Cuis Smalltalk Browser"
  src="/blog/assets/cuis-browser.png?v={{pkg.version}}"
  style="width: 100%">

There is no method named `average` in the `Array` class,
so it must be defined in a superclass, but which one?
Right-click "Array" in the second pane and select "browse hierarchy"
to open a Hierarchy Browser (shown in the screenshot below).
This shows that `Array` is a subclass of `ArrayedCollection`
which is a subclass of `SequenceableCollection`
which is a subclass of `Collection`.
We could examine each of those classes to
find the one that defines the `average` method,
but there's an easier way to find it.

Open a "Message Names" window (shown in the screenshot below),
and enter "average" in the "Search" input.
This lists three methods whose names contain "average"
which are `average`, `average:`, and `average:ifEmpty`.
Select the first one. This shows that the only class
that implements the selected method is `Collection`.
Click "Collection average" in the "Message Names" window
to see its implementation.

When the caret (`^`) character appears at
the beginning of an expressions within a method,
it returns the value of that expression from the method.
If the preference "Show ST-80 Assignments" is selected,
the caret character is rendered as an up pointing arrow.

The `average` method returns the result of sending the message `mean`
to `self`, which is a `Collection` instance.
Double-click "mean" to select it and press cmd-b to "Browse it".
This opens a new window that shows the method implementation,
which returns the result of divding `self sum` by `self size`.

<img alt="Cuis Smalltalk Array average"
  src="/blog/assets/cuis-array-average.png?v={{pkg.version}}"
  style="width: 100%">

What we have learned from this is that all the code can be easily browsed.
This includes code that you write and code in provided classes.

For our final exercise let's create a new class and use it.

1. Open a Browser window.
1. Right-click in the first pane and select "add item...".
1. Enter your last name to create a class category with that name.
1. In the bottom pane, replace "NameOfSubclass" with "Dog",
   retaining the leading `#`.
1. Add the instance variables `breed` and `name` to the `Dog` class
   by changing the value after `instanceVariableNames` to `'breed name'`,
   which is a string containing
   a space-separated list of instance variable names.
1. Press cmd-s to save the changes.
1. You will prompted to enter your initials and full name
   so Cuis Smalltalk can track who made each change.
1. Instance variables in Smalltalk are always private to their class.
   They are only exposed outside the class through "accessor methods".

   Right-click the class name `Dog` in the second pane
   and select "more...create inst var accessors".
   This creates the instance methods `breed` (gets value),
   `breed:` (sets value), and `name:` (sets value),
   all in the method category "accessing".
   It does not create the instance method `name` because
   that already exists in the superclass `Object`.
   But we want to override that to
   return the value of the `name` instance variable.

1. Click "accessing" in the third pane.
1. Enter the following in the bottom pane and save it.

   ```smalltalk
   name
       ^name
   ```

   Note how simple it is write accessor methods.

1. Right-click in the third pane and select "new category...".
1. Select "printing".
1. Enter the following in the bottom pane and save it.

   ```smalltalk
   printString
       ^'{1} is a {2}.' format: { name. breed }
   ```

   This method returns a string describing a `Dog` instance.
   It is called automatically when a `Dog` object is printed.

1. Click the "class" button at the bottom of the second pane.
1. Right-click in the third pane and select "new category...".
1. Select "instance creation".
1. Enter the following in the bottom lpane and save it.

   ```smalltalk
   newName: nameString breed: breedString
       | dog |
       dog := Dog new.
       dog name: nameString.
       dog breed: breedString.
       ^dog.
   ```

   This method name follows a convention where class methods
   that create and return a new instance begin with "new".
   It takes two arguments, one of the dog name and one for the dog breed.
   The parameter names follow a convention that they end with
   the name of their expected type, `String` in this case.

1. If there is no Transcript window open then open one.
   This is only used for displaying output.
1. To clear the contents of the Transcript window,
   right-click inside it and select "Clear Transcript".
1. Open a Workspace.
1. Enter the following expressions:

   ```smalltalk
   myDog := Dog newName: 'Comet' breed: 'Whippet'.
   myDog print.
   ```

1. Select both lines in the Workspace and press cmd-d to "Do it".
   This will print "Comet is a Whippet." in the Transcript window.

1. Open the World menu and select "Save Image as...".
   This is preferred over selecting "Save Image"
   which would save your changes in the initial image.
   That way you have to option to start from a clean slate.

1. Enter a name such as your last name and press return.
1. Open the World menu and select "Save Image and Quit" or
   "Quit without saving" (since you have already saved the changes).
1. Locate the image file and double-click it to reopen the image.

   In Windows, the first time you do this a dialog will appear asking
   "How do you want to open this file?".
   Click "Look for another app on this PC" and
   navigate to the file `Squeak.exe`.
   It will be in the `CuisVM.app\Contents\Windows-x86_64` subdirectory
   of where Cuis Smalltalk is installed.
   Select the that file and check the
   "Always use this app to open .image files" checkbox.

1. Verify that all the windows reappear in their saved locations
   and that all the code you added is present.

Eager to learn more about Smalltalk?
See my extensive set of <a href="https://mvolkmann.github.io/blog/"
target="_blank">blog pages on Smalltalk</a>.
Click "Smalltalk" in the hamburger menu to open a long list of subtopics.

## Resources

- <a href="https://en.wikipedia.org/wiki/Smalltalk"
  target="_blank">Smalltalk in Wikipedia</a>

- <a href="https://cuis.st" target="_blank">Cuis Smalltalk</a>

- <a href="https://pharo.org" target="_blank">Pharo Smalltalk</a>

- <a href="https://squeak.org" target="_blank">Squeak Smalltalk</a>

- <a href="https://www.youtube.com/playlist?list=PL6601A198DF14788D"
  target="_blank">Squeak from the very start</a>
  YouTube videos by Lawson English

The following recent podcast episodes discuss Smalltalk:

- Developer Voices:
  <a href="https://www.youtube.com/watch?v=sokb6zZC-ZE&t=3105s"
  target="_blank">Cuis Smalltalk and the History of Computing’s Future</a>
  with Juan Vuletich
- Software Unscripted:
  <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955?i=1000656742775"
  target="_blank">A Haskeller Tries Smalltalk</a> with Ian Jeffries
- Software Unscripted:
  <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955"
  target="_blank">Smalltalk's Past, Present, and Future</a> with Juan Vuletich

Feel free to send any questions you have about Smalltalk
to <mark@objectcomputing.com>.
