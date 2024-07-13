---
eleventyNavigation:
  key: Help
  order: 1.3
  parent: Smalltalk
layout: topic-layout.njk
---

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
