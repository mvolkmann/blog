---
eleventyNavigation:
  key: Questions
  order: 1.9
  parent: Smalltalk
layout: topic-layout.njk
---

- How can you work with SQLite and Postgres databases from Smalltalk?
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
