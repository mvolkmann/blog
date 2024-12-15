---
eleventyNavigation:
  key: Images
  order: 5
  parent: Smalltalk
layout: topic-layout.njk
---

A Smalltalk image contains a collection of
all objects in the development environment.
Everything in Smalltalk is represented by an object,
including class definitions (yours and those provided),
GUI elements in the development environment,
and objects created from Workspaces.

An image can be used to manage collections of data,
perhaps held in `Dictionary` objects, as an alternative to using a database.

One way to start an image is to double-click its file.

To save any changes, open the World menu.
Select "Save Image" to save in the current image file.
Select "Save Image as" to save in a new image file.
Select "Save Image and Quit" to save in the current image file and exit the VM.

The changes include all placed morphs (includes development enviroment windows),
their position, size, and colors,
selections made (ex. System Browser top pane selections),
and window content (ex. Workspaces).

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
1. Open a Workspace, enter `DirectoryEntry smalltalkImageDirectory`
   or `SystemDictionary new imageName`, and "Print it".
   This outputs the directory path of the current image without its file name.
