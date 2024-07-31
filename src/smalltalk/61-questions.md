---
eleventyNavigation:
  key: Questions
  order: 61
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
- What package can you install to get a color picker?
- Are there any linting tools for Smalltalk?
- Learn how to write and distribute command-line utilities/apps in Smalltalk.
- Learn how to write and distribute GUI apps in Smalltalk.
- Learn how to write and distribute web apps in Smalltalk.
- Study the code in the "Morphic-Games-Solitaire" package
  to see what you can learn from it.
- See <a href="https://squeak.js.org" target="_blank">SqueakJS</a>.
- Learn how to add items to existing menus in the Cuis UI that do something
  when you select them such as repositioning the currrently active window.
- Consider submitting an update to Cuis that standardizes menu item names.
  Currently there are three styles:
  - all words lower
  - All Words Upper
  - Mixture of styles
- Does Smalltalk expose its own AST? If so, maybe you can use that to
  generate code and another programming language from my Smalltalk program.
- Is Morphic 3 only supported in Cuis?
- Does Cuis run on the "OpenSmalltalk Virtual Machine"?
  Is this the same VM that is used by Squeak and Pharo?
- In a talk by Juan Vuletich, one of his slides says Cuis is
  "A practical system, used for teaching, Satellite image processing,
  research in sign, image, and audio processing,
  research in programming languages, and many other areas of application."
  Does this imply that it is not for "normal" application development.
- Can you configure your image so it never starts in full screen mode
  even if it was saved that way?
- What do you click in a `Morph` halo to cause it to
  display its coordinate system axes?
- Some packages are built-in meaning that they are included
  in the base image of the Smalltalk implementation.
  Other packages must be downloaded, sometimes by cloning a GitHub repository.
- Formalize your code to generate HTML from an `Association` object.
- Does "morph removeAll" remove a given `Morph` and all of its sub morphs?
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
