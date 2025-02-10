---
eleventyNavigation:
  key: Preferences
  order: 31
  parent: Smalltalk
layout: topic-layout.njk
---

## Overview

The Cuis Smalltalk development environment is customized via many preferences.
These are stored in the global variable `Preferences`
which is an instance of the class `PreferenceSet`.

## Setting from World Menu

Some preferences can be set from the "Preferences" submenu of the World menu.
These include the following where the ones separated by a slash
included opposite `Boolean` values for the same preference.

- Focus follows Mouse (true; default) / Focus when Click (false)

  These both set the `Boolean` preference `#focusFollowsMouse`.

- Show ST-80 assignments (true; default) / Show ANSI assignments (false)

  These both set the `Boolean` preference `#showAssignmentAsLeftArrow`.

- Size of GUI elements...

  This sets the `SmallInteger` preference `#defaultFontSize`
  which defaults to 21.

- Set System Font...

  This opens a menu that lists all the installed fonts
  where their names provide a samples of the fonts.
  Selecting one sets the `Font` preferences
  `#standardButtonFont`, `#standardCodeFont`, `#standardListFont`,
  `#standardMenuFont`, and `#WindowTitleFont`.
  The default font is "DejaVu Sans".

- Load all TrueType Fonts

  This loads all TrueType font files found in the directory
  Cuis-Smalltalk-Dev/TrueTypeFonts.
  Initially this directory contains the following font directories:
  AlexBrush, Amaranth, ComputerModern, Cream, DejaVu, JetBrainsMono,
  KiwiMaru, KurintoSans, LearningCurve, NotoEgyptianHieroglyphs, and SourceSans.
  Additional font directories can be placed here before selecting this menu item.

- Icons...

  This opens a submenu containing "Use icons for menu entries" (true; default)
  and "Don't use icons for menu entries" (false).
  Both set the `Boolean` preference `#wantsMenuIcons`.

- Themes...

  This opens a submenu containing all the loaded themes.
  Selecting one changes the theme used by newly opened windows,
  but not the theme used by already open windows.
  Initially the only themes available are "BrightColorTheme" and "DarkTheme".
  Select "\* Load Additional Themes \*" to load more.
  These include:

  - ClassicTheme
  - DarkBlueTheme
  - DarkColorTheme
  - HighContrastBlackTheme
  - HighContrastWhiteTheme
  - LightBluetheme (The "t" in "theme" shoud be uppercase for consistency!)
  - LightGrayTheme
  - LightTheme
  - PersonalizedTheme

- Show taskbar / Hide taskbar

  The taskbar is shown by default.
  Selecting these menu items does not change a preference value.
  They just modify the instance of `WorldMorph` to show or hide the taskbar
  which is persisted when the image is saved.

- Full screen on / Full screen off

  Full screen mode is off by default.
  Selecting these sends the message `#fullScreenMode:`
  to the `DisplayScreen` class which evaluates the primitive 233.
  This triggers the platform to change whether the Cuis window
  is rendered in full screen mode.

- Save Prefs in UserPrefs.txt / Save Prefs in the Image
- Set Code Author...
- All prefernces...

  This evaluates `Preferences allPreferences inspect`
  which opens an Inspect window that displays all the current preferences.

## Built-in Preferences

The base Cuis image populates this with the following preferences.
This list was obtained by evaluating
`Preferences allPreferences keys asSortedCollection print`.

- `#aaFontsColormapDepth`
- `#allowBlockArgumentAssignment`
- `#allowNonBooleanReceiversOfBooleanMessages`
- `#allowNonLocalReturnsInExceptionHandlers`
- `#alternativeBrowseIt`
- `#askConfirmationOnQuit`
- `#askToInstallComplexPackage`
- `#atMinusDigitMeaning`
- `#autoNumberUserChanges`
- `#autoReplaceNamedCharacters`
- `#automaticPlatformSettings`
- `#backgroundEffect`
- `#balloonHelpEnabled`
- `#biggerCursors`
- `#browseWithPrettyPrint`
- `#browserWindowClass`
- `#cacheDisplayContentWhenMovingMorphs`
- `#cacheTrueTypeGlyphs`
- `#caseSensitiveFinds`
- `#changeSetVersionNumbers`
- `#cheapWindowReframe`
- `#checkForSlips`
- `#checkLostChangesOnStartUp`
- `#classAnnotations`
- `#classFinder`
- `#clearPackagePathsOnImageMove`
- `#clickGrabsMorphs`
- `#cmdDotEnabled`
- `#cmdDotInterruptTakesStatistics`
- `#ctrlArrowsScrollHorizontally`
- `#debugHaloHandle`
- `#debugLogTimestamp`
- `#debugShowDamage`
- `#decorateBrowserButtons`
- `#defaultAuthorName`
- `#defaultFontSize`
- `#diffsInChangeList`
- `#diffsWithPrettyPrint`
- `#dismissAllOnOptionClose`
- `#drawKeyboardFocusIndicator`
- `#extraDebuggerButtons`
- `#fileOutANSIassignment`
- `#focusFollowsMouse`
- `#focusIndicatorWidth`
- `#fullPrintItInWorkspaces`
- `#fullScreenLeavesDeskMargins`
- `#haloEnclosesFullBounds`
- `#haloHandleSize`
- `#haloSpecifications`
- `#halosMorphBoundsFillColor`
- `#halosMorphBoundsFrameColor`
- `#halosShowCoordinateSystem`
- `#highlightBlockNesting`
- `#initialFileListDirectories`
- `#italicsInShout`
- `#listClassesHierarchically`
- `#logDebuggerStackToFile`
- `#machine`
- `#menuKeyboardControl`
- `#messageCategoryAnnotations`
- `#methodAnnotations`
- `#optionalButtons`
- `#pointer`
- `#prettyPrintRectangularBlocks`
- `#properDisplayAlphaForFonts`
- `#roundedButtonRadius`
- `#roundedWindowRadius`
- `#saveReleaseOptionsInWorldMenu`
- `#scrollbarThickness`
- `#selectionsMayShrink`
- `#selectiveHalos`
- `#serverMode`
- `#shiftClickShowsImplementors`
- `#shoutInWorkspaces`
- `#showAnnotations`
- `#showAssignmentAsLeftArrow`
- `#showLinesInHierarchyViews`
- `#soundQuickStart`
- `#soundStopWhenDone`
- `#soundsEnabled`
- `#standardButtonFont`
- `#standardCodeFont`
- `#standardListFont`
- `#standardMenuFont`
- `#stylingWithEmphasisInWorkspaces`
- `#subPixelRenderColorFonts`
- `#subPixelRenderFonts`
- `#syntaxHighlightingAsYouType`
- `#systemCategoryAnnotations`
- `#systemWindowEmbedOK`
- `#tapAndHoldEmulatesButton2`
- `#thoroughSenders`
- `#tileResizerInWindowMenu`
- `#transcriptLogVerbose`
- `#usePreDebugWindow`
- `#useUserPrefsFile`
- `#userChangesFileNameExtension`
- `#wantsMenuIcons`
- `#warnAboutNonLocalReturnsInExceptionHandlers`
- `#warnAndOfferLastUserChanges`
- `#warnIfNoChangesFile`
- `#warnIfNoSourcesFile`
- `#windowTitleFont`
- `#worldMenu`

## Setting in an Inspector

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

The following code sets a preference to enable playing sounds
and plays a basic sound:

```smalltalk
Preferences at: #soundsEnabled put: true.
Smalltalk primitiveBeep.
```

## PreferenceSet

Your packages can store user preferences in a `PreferenceSet`.

The first step is to ensure that a default value for the preference is set.
To do this, add the class method `initialize` to one of your classes.
For example:

```smalltalk
initialize

    Preferences
        name: #backgroundEffect
        description: 'determines how backgroundImage is applied'
        category: #gui
        type: Symbol
        value: #stretch.
```

Preference names must be instances of `Symbol` rather than `String`.

The provided category symbols can be retrieved with `Preferences categories`.
By default that returns `#(#font #gui #programming #system)`.

To change a preference value:

```smalltalk
Preferences name: #backgroundEffect category: #gui value: #tile.
"or this to omit specifying the category"
Preferences at: #backgroundEffect put: #tile.
```

To save a preference to the file
`Cuis-Smalltalk-Dev-UserFiles/UserPrefs.txt`
from which preferences will be restored
each time an image is started:

```smalltalk
Preferences saveToDisk: #backgroundEffect.
```

To retrieve a preference value:

```smalltalk
effect := Preferences at: #backgroundEffect.
```
