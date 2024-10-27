---
eleventyNavigation:
  key: ZZZ article
layout: topic-layout.njk
---

# Lessons from Smalltalk

"The further backward you can look, the further forward you are likely to see."
— Winston Churchill

"The single most important thing I have learned about software development
over my career is that if you do not aggressively fight complexity,
it will eat you alive." - Vicki Boykis

## Smalltalk Overview

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
The Cuis mascot is southern mountain cavy found in Argentina
which is a tailless rodent that looks similar to a very large mouse.
The word "cuis" in Rioplatense Spanish means "squeak".

Squeak, Pharo, and Cuis all use the MIT license.

There are also many commercial Smalltalk implementations including
Cincom Smalltalk, GemStone/S, and VA Smalltalk.

## Smalltalk Development Environment

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

- Gain an understanding of its pros and cons compared to other languages.
- Get ideas for features that can be added to other languages.
- Get ideas for features that can be added in the
  development environments of other languages.
- Actually use it as an alternative to other languages.
- Have fun!

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

- For Windows, open a Command Prompt and enter `RunCuisOnWindows.bat`
- For Linux, open a Terminal and enter `./RunCuisOnLinux.sh`
- For macOS, open a Terminal and enter `./RunCuisOnMacTerminal.sh`
  Alternatively, double-click the file `Cuis7.1-6770.image`
  in the `CuisImage` subdirectory.

The window that appears has a blue background by default.
This is referred to as the "World".
Clicking the World opens the World menu.
Click "Open" to see a submenu of windows that can be opened inside the World.
We will look at three of these:
Workspace, Browser, Message Names, and Transcript.
Of these, only a Transcript window will be open by default in the initial image.

A Workspace is a window for experimenting with Smalltalk code.
It is similar to a read-eval-print-loop (REPL) in other programming languages.
Open a Workspace and enter the statments shown in the following screenshot.

<img alt="Cuis Smalltalk Workspace session"
  src="/blog/assets/cuis-workspace-session.png?v={{pkg.version}}"
  style="width: 50%">

The period character is used to separate, not terminate, statements.

The third statement assigns a value to the variable `cityToTeam`.
To assign the value of an expression to a variable,
enter a variable name, the assignment operator `:=`, and the expression.
To cause assignment operators be rendered as a left pointing arrow,
open the World menu and select "Preferences ... Show ST-80 assignments".
The opposite setting is "Preferences ... Show ANSI assignments".
Regardless of this setting, if an underscore is typed in place of
the assignment operator, it will be rendered as a left pointing arrow.

There are shortcut keys for many actions.
In macOS, these use the command key (cmd).
In other platforms these use the control key.
All the examples that follow use the command key,
so substitute the control key if you are not using a Mac.

With the cursor positioned at the end of an expression,
right-click and select "Do it" (cmd-d) or "Print it" (cmd-p).
Selecting "Do it" evaluates the expression, but does not output its value.
Selecting "Print it" evaluates the expression and outputs its value
immediately after the expression in the Workspace.
The result will be selected, so pressing the delete key will remove it.

The first expression sends the unary message `asUppercase`
to the literal string `'demo'`.

The second expression sends the binary message `+`
to the number `2` with the argument `3`.

The next three expressions demonstrate working with a `Dictionary`,
which is called a `Map` or `HashMap` in other programming languages.
The expression `city := Dictionary new` sends the message `new`
to the `Dictionary` class which creates and returns a new instance.
The syntax `:=` is automatically change to a left pointing arrow.
The expression `city at: 'Kansas City' put: 'Chiefs'`
sends the keyword message `at:put:` to `cityToTeam`
which adds a key/value pair to the `Dictionary`.
The expression `city at: 'Kansas City'`
sends the keyword message `at:` to the `Dictionary`
which returns the value at the specified key.

The last expression, `#(1 2 3 4) average`, returns the `Fraction` `5/2`
rather than a `Float` in order to preserve accuracy.
It does this because the `average` method
returns the result of an integer division.
The syntax `#(1 2 3 4)` creates a compile-time `Array`.
`average` is an instance method defined in the `Collection` class
which is a superclass of the `Array` class.

We can examine the implementation of the `average` method.
To do so, open a Browser.
Brower windows contains four panes across the top row.

- The first pane displays a list of class categories.
- The second pane displays a list of classes in the selected class category.
  There are three buttons at the bottom of this pane.
  - The "instance" button causes the remaining panes to show instance information.
  - The "?" button causes the bottom pane to display
    the comment for the selected class.
  - The "class" button causes the remaining panes to show class information.
- The third pane displays a list of method categories in the selected class.
- The fourth pane displays a list of methods in the selected method category.

Move the mouse cursor over the first pane and press cmd-f.
Enter "Array" and press return.
The `Array` class will be selected in the second pane.

<img alt="Cuis Smalltalk Browser"
  src="/blog/assets/cuis-browser.png?v={{pkg.version}}"
  style="width: 100%">

There is no method named `average` in the `Array` class,
so it must be defined in a superclass, but which one?
Right-click "Array" in the second pane and select "browse hierarchy"
to open a Hierarchy Browser.
This shows that `Array` is a subclass of `ArrayedCollection`
which is a subclass of `SequenceableCollection`
which is a subclass of `Collection`.
We could check each of those classes to
find the one that defines the `average` method.
But there's another way.

Open a "Message Names" window, and enter "average" in the "Search" input.
This lists three methods whose names contain "average"
which are `average`, `average:`, and `average:ifEmpty`.
Select the first one.
This shows that the only class that defines the selected method is `Collection`.
Click "Collection average" in that window to see its implementation.

When the caret (`^`) character appears at
the beginning of a statement within a method,
it returns the value of the expression that follows from the method.
When the preference "Show ST-80 Assigments" is selected,
the caret character is rendered as an up pointing arrow.

The `average` method returns the result of sending the message `mean`
to `self` which is the `Collection` instance.
Double-click "mean" to select it and press cmd-b to "Browse it".
This opens a new window that shows the method implementation
which returns the result of divding `self sum` by `self size`.

<img alt="Cuis Smalltalk Array average"
  src="/blog/assets/cuis-array-average.png?v={{pkg.version}}"
  style="width: 100%">

What we have learned from this is that all the code can be easily browsed.
This includes code that you write and code in provided classes.

For our final exercise we will create a new class and use it.

- Open a Browser.
- Right-click in the first pane and select "add item...".
- Enter your last name to create a class category with that name.
- In the bottom pane, replace "NameOfSubclass" with "Dog",
  keeping the leading `#`.
- Add the instance variables `breed` and `name` to the `Dog` class
  by changing the value after `instanceVariableNames` to `'breed name'`.
- Press cmd-s to save the changes.
- You will prompted to enter your initials and full name
  so Cuis Smalltalk can track who made each change.
- Right-click the class name `Dog` in the second pane
  and select "more...create inst var accessors".
  This creates the instance methods `breed` (gets value),
  `breed:` (sets value), and `name:` (sets value),
  all in the method category "accessing".
  It does not create the instance method `name` because
  that already exists in the superclass `Object`.
  But we want to override that.
- Click "accessing" in the third pane.
- Enter the following in the bottom pane and save it.

  ```smalltalk
  name
      ^name
  ```

  The caret character is following by an expression that specifies a value to be returned from the method.
  It is automatically replaced by an up pointing arrow.

- Right-click in the third pane and select "new category...".
- Select "printing".
- Enter the following in the bottom pane and save it.

  ```smalltalk
  printString
      ^'{1} is a {2}.' format: { name. breed }
  ```

- Click the "class" button at the bottom of the second pane.
- Right-click in the third pane and select "new category...".
- Select "instance creation".
- Enter the following in the bottom lpane and save it.

  ```smalltalk
  newName: nameString breed: breedString
      | dog |
      dog := Dog new.
      dog name: nameString.
      dog breed: breedString.
      ^dog.
  ```

- Open a Workspace.
- Enter the following statements:

  ```smalltalk
  myDog := Dog newName: 'Comet' breed: 'Whippet'.
  myDog print.
  ```

- If there is no Transcript window open then open one.
- To clear the contents of the Transcript window,
  right-click inside it and select "Clear Transcript".
- Select both lines in the Workspace and "Do it".
  This will print "Comet is a Whippet." in the Transcript window.

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
