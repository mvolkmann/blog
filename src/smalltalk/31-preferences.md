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
included opposite Boolean values for the same preference.

- Focus follows Mouse / Focus when Click
- Show ST-80 assignments / Show ANSI assignments
- Size of GUI elements...
- Set System Font...
- Load all TrueType Fonts
- Icons...
- Themes...
- Show taskbar / Hide taskbar
- Full screen on / Full screen off
- Save Prefs in UserPrefs.txt / Save Prefs in the Image
- Set Code Author...
- All prefernces...

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
    | prefs |
    prefs := PreferenceSet sysPreferences.
    prefs
        name: #backgroundEffect
        description: 'determines how backgroundImage is applied'
        category: #gui
        type: Symbol
        value: #stretch.
```

The provided category symbols can be retrieved with
`PreferenceSet sysPreferences categories`.
By default that returns `#(#font #gui #programming #system)`.

To change the preference value:

```smalltalk
PreferenceSet sysPreferences at: #backgroundEffect put: #tile.
```

To retrieve the preference value:

```smalltalk
effect := PreferenceSet sysPreferences at: #backgroundEffect.
```
