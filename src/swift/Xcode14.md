---
eleventyNavigation:
  key: Xcode 14
  parent: Swift
layout: topic-layout.njk
---

## Overview

This is a summary of the new features added to Xcode in version 14.
It is derived from the excellent video by Paul Hudson at {% aTargetBlank
"https://www.youtube.com/watch?v=V2TDGeevDWo", "What's New in Xcode 14?" %}

## Reduced Download Size

Xcode no longer downloads SDKs for every OS.
By default only the iOS and macOS SDKs are downloaded, not watchOS or tvOS.
This makes it faster to install Xcode.

## Member-wise Initializer Code Completion

Xcode can generate code for the memberwise initializer
of a `struct` or `class`.
Just type `init` and accept the completion.
Then customize the provided code as needed.

## Default Protocol Method Implementations

Xcode can generate implementations of protocol implementations.
For example, in a `struct` that conforms to the `Codable` protocol,
type `encode` and accept the completion.
Then customize the provided code as needed.

## Function Call Completions

Xcode code completion of function/method calls.
Optional arguments appear dimmed.
When a completion with optional arguments is selected,
those will not be included by default.
To include them, hold down the option key before selecting the completion.

## Plus More Completions

Xcode will hide some code completions and indicate this
with the text "+n more" at the end of some completions.
To reveal them, press the right arrow key.

## Code Generation for Arrays

Some code completions generate code that processes arrays.
For example, type "lplay" and choose the `List(players) { player...` completion.
This generates the following code, perhaps choosing to
use some `String` property such as `name`:

```swift
List(players) { player in
    Text(player.name)
}
```

## Auto Indent

Surrounding existing code with a new block
automatically indents the surrounded code
when the closing brace is typed.

## Sticky Headers

When scrolling down through code, the first line of each
type and function definition that encloses the code being viewed
sticks at the top of the editor window to provide context.

## SF Symbols

Access to SF Symbols is now built into Xcode.
To access it, press cmd-shift-l or
click the "+" in the upper-left and click the circled star.
Type part of a symbol name in the input at the top
to filter the list of symbols.
Click a symbol name to see the symbol
and a list of OSes where it is supported.
Double-click a symbol name to insert it.

## App Icons

Xcode can generate all the required sizes of an icon
from one 1024x1024 image.
To do this, open `Assets.xcassets`, select `AppIcon`,
open the Inspector on the right,
change the value for a specific OS (such as `iOS`) to "Single Size",
and drag in a 1024x1024 image.

## Documentation Generation

To generate DocC documentation for the current project
select Product ... Build Documentation.
This can take a couple of minutes to complete.
When it does, a dialog will open that contains lists of all the
classes, structs, functions, and enums defined by the project.
Click a name to see details about it.
Use the filter input at the top to filter the documentation
to values containing specific text.

Supposedly the generated documentation can be exported to GitHub
for viewing outside of Xcode, but it's not clear how to do that.

## Recent Devices

The device dropdown at the top now shows a list of device types
that have been used to run the app in the Simulator.
This makes it easier to select frequently used device types.

## Previews

Previews are now always live.
There is no need to click a button to make them live.
However, previews still sometimes need to be resumed
by either clicking the "Resume" button or pressing cmd-option-p.

Previews now start and update faster than in Xcode 13.

A new "variants" button appears as the third button
at the bottom of the preview area.
Clicking this offers the options to show one of
"Color Scheme Variants" (Light and Dark),
"Orientation Variants" (Portrait, Landscape left, and Landscape right),
and "Dynamic Text Variants" (X Small through XXX Large and AX 1 through AX 5)

If a preview defines multiple views,
they are presented in separate tabs in the preview area.
TODO: How are the tab titles specified?

## Performance

Xcode 14 is faster at building, linking, and testing projects.
