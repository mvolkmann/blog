---
eleventyNavigation:
  key: Localization
  parent: Swift
layout: topic-layout.njk
---

## Overview

To specify the languages to be supported:

1. Select the top entry in the File Navigator.
1. In the left nav of the project editor, select the project.
1. Select the "Info" tab.
1. In the "Localization" section, for each language to be added,
   click the "+" Button and select a language.

To create a "Strings" file:

1. Add a file to the project by pressing cmd-n.
1. In the Resource section, select the "Strings File" template.
1. Click the "Next" button.
1. Keep the default name of "Localizable.strings".
1. Click the "Create" button.

To populate the "Strings" file:

1. Select the "Localizable.strings" file in the File Navigator.
1. Open the Inspector panel on the right.
1. Under "Localization", check all the language checkboxes.
1. This will add one entry under "Localizable.strings" in the File Navigator
   for each selected language.
1. For each language
   1. Select its entry in the File Navigator.
   1. For each string to be translated
      1. Enter an assignment statement of the form
         `"english-or-key" = "translation";`
         (note the semicolon at the end).

To select a locale to use in the Preview,
apply the `environment` view modifier to the top-most view.
For example:

```swift
ContentView()
    .environment(\.locale, .init(identifier: "la"))
```

where `la` is replaced by a language abbreviation such as `fr` for French.

To select a locale in the Simulator:

- Open the Settings app.
- Select General.
- If the desired language is not displayed under "PREFERRED LANGUAGES",
  tap "Add Language...", select the language,
  and tap one of the "Use {language}" buttons
  to specify which language should be active now.
- Drag the language to be used now to the top of the list of languages
  using the drag handles to right of each language name.
- Tap the "Continue" button.
- Return to the app being tested.

## OLD STUFF YOU CAN PROBABLY DELETE FOLLOWS

To generate a directory of localization files:

1. Select Product ... Export Localizations...
1. Select the directory where a new directory will be created.
   Typically the project directory is used.
1. Enter a name of the new directory such as "localizations".
1. Click the "Export" button.
1. Select Files ... Add Files to...
1. Select the newly created directory.

This automatically captures all the strings passed to UI views.
TODO: Only those strings or ALL strings?

The generated directory will contain one "Xcode Localization Catalog"
for each supported language.
Each of these will have a name of the form `{language-abbreviation}.xcloc`.
For example, for English it will be `en.xcloc`
and for French it will be `fr.xcloc`.
Each of these will contain a subdirectory named "Localized Contents"
that contains the file "{language-abbreviation}.xliff".
These are XML files that hold mappings from source text to translated text.

Rather than manually editing these XML files,
they can be edited using a special editor within Xcode.
However, the directory structure appears differently inside Xcode.
Selecting a `.xcloc` file will expose
what appears to be a file named `Localizable`.
Selecting that file will open an editor that makes entering translations
similar to entering values in a spreadsheet.

1. Select Product ... Export Localizations...
1. Select the directory where a new directory will be created.
   Typically the project directory is used.
1. Enter a name of the new directory such as "localizations".
1. Click the "Export" button.
1. Select Files ... Add Files to...
1. Select the newly created directory.

This automatically captures all the strings passed to UI views.
TODO: Only those strings or ALL strings?

The generated directory will contain one "Xcode Localization Catalog"
for each supported language.
Each of these will have a name of the form `{language-abbreviation}.xcloc`.
For example, for English it will be `en.xcloc`
and for French it will be `fr.xcloc`.
Each of these will contain a subdirectory named "Localized Contents"
that contains the file "{language-abbreviation}.xliff".
These are XML files that hold mappings from source text to translated text.

Rather than manually editing these XML files,
they can be edited using a special editor within Xcode.
However, the directory structure appears differently inside Xcode.
Selecting a `.xcloc` file will expose
what appears to be a file named `Localizable`.
Selecting that file will open an editor that makes entering translations
similar to entering values in a spreadsheet.
