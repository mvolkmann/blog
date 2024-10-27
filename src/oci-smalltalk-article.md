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

## Enter Smalltalk

The Smalltalk programming language was created at
Xerox PARC (Palo Alto Research Center) in the 1970s.
It is a purely object-oriented programming (OOP) language.
Everything is represented by an object that is an instance of some class.
This includes classes themselves and
all the GUI elements in the development environment.
Everything happens by sending messages to objects.
The objects decide whether and how to act on the messages.

Smalltalk was the first programming language to make OOP popular.
In the 1990's the popularity of Smalltalk had risen enough
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
operating systems on different CPU architectures.
It will render the same windows, pixel for pixel,
across Windows, Linux, and MacOS, only differing based on screen size.
There is no need to recompile code for different environments.
This makes Smalltalk code highly portable.

An image file can be thought of as a snapshot
of the current state of the development environment.
It describes the collection of all active objects.
During development, changes can be saved to the current image
or to a new image.

Today Smalltalk is still used by the financial industry,
manufacturing, utilities, transportation, and academia.

## A Bit of history

Squeak Smalltalk, released in 1996,
was the first free, open-source implementation.
Until then all Smalltalk implementations were commericial and expensive.
The Java programming language was released in 1995 and was free.
That removed most of the wind from Smalltalk sails
and for most part it receded into the history of computer programming.

However, Smalltalk is alive and well outside the United States.
Two free, open-source forks of Squeak have been created, Pharo and Cuis.

Pharo Smalltalk was created in France and
aims to provide a more comprehensive set of features than Squeak.

Cuis Smalltalk was created in Argentina and
aims to remain small and easy to learn.
Missing features can be defined by combining the features that are present
and by installing more packages.
The Cuis mascot is southern mountain cavy found in Argentina
which is a tailless rodent that looks similar to a very large mouse.
The word "cuis" in Rioplatense Spanish means "squeak".

Squeak, Pharo, and Cuis all use the MIT license.

There are also many commercial Smalltalk implementations including
Cincom Smalltalk, GemStone/S, and VA Smalltalk.

## Smalltalk development environment

Smalltalk is perhaps most known for its incredible development tools.
These support:

- finding code in many ways
- a live environment where code changes are immediately reflected
  (no need to recompile or restart the environment)
- debugging with the ability to modify code and data, then continue or restart
- ability to modify the classes and methods that implement the
  development environment as easily as modifying your own classes and methods
- inspecting objects to see their class and instance variables
- rapid prototyping

## Why learn Smalltalk now?

Learning Smalltalk will enable you to:

- Gain an understanding of its pros and cons compared to other languages.
- Get ideas for features that can be added to other languages.
- Get ideas for features that can be added in the
  development environments of other languages.
- Actually use it as an alternative to other languages.
- Have fun!

## Taste of Using Cuis Smalltalk

Let's walk through using Cuis Smalltalk.

To install Cuis Smalltalk:

1. Browse the <a href="https://cuis.st" target="_blank">Cuis Smalltalk home page</a>.
1. Click the "Download" link at the top of the page.
1. Click "Cuis Stable Release" or "Cuis Rolling Release". The later is
   recommended so you can obtain updates by simply running `git pull`.
   Both lead to a GitHub repository.
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

On macOS, if starting Cuis Smalltalk is blocked, follow these steps:

1. Open the System Settings app.
1. Select "Privacy & Security".
1. Scroll down to the "Security" section.
1. Look for the message '"CuisVM.app" was blocked from use
   because it is not from an identified developer.'
1. Click the "Open Anyway" button.
1. Click the "Open" button in the next dialog that appears.
1. Select the file `Cuis*.image` found in the subdirectory as `CuisImage`.
1. Click the "Open" button.

The window that appears has a blue background by default.
This is referred to as the "World".
Clicking the World opens the World menu.
Click "Open" to see a submenu of windows that can be opened inside the World.
We will look at three of these: Workspace, Transcript, and Browser.

TODO: Continue here!

- use a Workspace and Transcript
- use a Browser to view and edit code
- create a custom class
- experiment with the class in a Browser

There's much more to learn.
See my extensive set of <a href="https://mvolkmann.github.io/blog/"
target="_blank">blog pages on Smalltalk</a>.
Click "Smalltalk" in the hamburger menu to see a long list of subtopics.

## Resources

- <a href="https://en.wikipedia.org/wiki/Smalltalk"
  target="_blank">Smalltalk in Wikipedia</a>

- <a href="https://archive.org/details/byte-magazine-1981-08"
  target="_blank">Byte Magazine issue on Smalltalk</a>

- <a href="https://cuis.st" target="_blank">Cuis Smalltalk</a>

- <a href="https://pharo.org" target="_blank">Pharo Smalltalk</a>

- <a href="https://squeak.org" target="_blank">Squeak Smalltalk</a>

- <a href="https://www.youtube.com/playlist?list=PL6601A198DF14788D"
  target="_blank">Squeak from the very start</a>
  YouTube videos by Lawson English

The following recent podcast episodes discuss Smalltalk:

- <a href="https://www.youtube.com/watch?v=sokb6zZC-ZE&t=3105s"
  target="_blank">Cuis Smalltalk and the History of Computing’s Future</a>
  with Juan Vuletich
- <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955?i=1000656742775"
  target="_blank">A Haskller Tries Smalltalk</a> with Ian Jeffries
- <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955"
  target="_blank">Smalltalk's Past, Present, and Future</a> with Juan Vuletich
