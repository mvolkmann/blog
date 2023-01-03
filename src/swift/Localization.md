---
eleventyNavigation:
  key: Localization
  parent: Swift
layout: topic-layout.njk
---

## Overview

TODO: Which SwiftUI views automatically look for String translations?
TODO: I know Text does. How about Label, Button, TextField, Link, navigation title, ...?
TODO: When should LocalizedStringKey be used?

## Specifying Supported Languages

1. Select the top entry in the File Navigator.
1. In the left nav of the project editor, select the project.
1. Select the "Info" tab.
1. In the "Localization" section there will already be an entry for "English".
   For each additional language to be supported,
   click the "+" Button and select a language.

## Creating a "Strings" File

Translations are described in a "Strings" file.
To create one:

1. Add a file to the project by pressing cmd-n.
1. In the Resource section, select the "Strings File" template.
1. Click the "Next" button.
1. Keep the default directory and the default file name "Localizable.strings".
1. Click the "Create" button.
1. Select the "Localizable.strings" file in the File Navigator.
1. Open the Inspector panel on the right.
1. Under "Localization", click the "Localize..." button.
1. In the dialog that appears, click the "Localize" button.
1. Once again, select the "Localizable.strings" file in the File Navigator.
1. Once again, open the Inspector panel on the right.
1. Under "Localization", check all the language checkboxes.
1. This will add one entry under "Localizable.strings" in the File Navigator
   for each selected language.

## Populating the "Strings" File

1. In the File Navigator, expand the "Localizable.strings" entry
   to expose an entry for each supported language.
1. For each supported language
   1. Select its entry in the File Navigator.
   1. For each string to be translated
      1. Enter an assignment statement of the form `"key" = "translation";`
         (note the semicolon at the end).

For example, the key could be "greeting",
the English translation could be "Hello",
and the French translation could be "Bonjour".

When no translation is found for a given key in the current language,
the key itself is used.

If a key is the same as its English translation then it is
not necessary to add an entry for it in the English file.
For example, if the key is "Hello" then
there is no need to add `"Hello" = "Hello";"` in the English file
and the French file could contain `"Hello" = "Bonjour";`.
This is the recommend approach unless the English translation is long.

Translation strings can contain Markdown syntax.
For example, this can be used to make some of the text
bold, italic, or underlined.

## Preview Locale

To select a locale to use in the Preview,
apply the `environment` view modifier to the top-most view.
For example:

```swift
ContentView()
    .environment(\.locale, .init(identifier: "la"))
```

where `la` is replaced by a language abbreviation such as `fr` for French.

## Simulator Locale

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

## User-selected Locale

The `environment` view modifier can be used to
allow the user to select a language within the app.
For example:

```swift
        VStack {
            Picker("Language", selection: $language) {
                Text("English").tag("en")
                Text("French").tag("fr")
                Text("Spanish").tag("es")
            }
            .pickerStyle(.segmented)

            Text("Hello!")
        }
        .padding()
        .environment(\.locale, .init(identifier: language))
```

## Localized String Arguments

Localized strings can specific arguments accepted and
where they should be inserted in translations.
Arguments are represented by `%@` for string values and `%d` for numeric values.
Arguments are passed to the `LocalizedStringKey` initializer
using string interpolation.
For example:

```swift
let thing = "sun"
let time = "tomorrow"
Text(LocalizedStringKey("The \(thing) will come out \(time)."))
```
