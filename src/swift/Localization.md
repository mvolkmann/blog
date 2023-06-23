---
eleventyNavigation:
  key: Localization
  parent: Swift
layout: topic-layout.njk
---

## Overview

There many facets to localization including
text translation, enabling users to select a locale within an app,
plural forms, numbers, currency, dates, and images.
Each of these are discussed here.

Many SwiftUI views automatically perform translations.
These include `Text`, `Label`, and `Button`.

## Specifying Supported Languages

1. Select the top entry in the Project Navigator.
1. In the left nav of the project editor, select the project.
1. Select the "Info" tab.
1. In the "Localization" section there will already be an entry for "English".
   For each additional language to be supported,
   click the "+" Button and select a language.

## String Catalogs

Support for String Catalogs was added in iOS 17.
These supercede "Strings" and "StringsDict" files.

A String Catalog is a JSON file
that stores the translations for all supported languages.
Most projects will have a single String Catalog file.

Xcode provides a specialized editor for String Catalog files
that displays a separate table for each supported language.

### Creating

To create a String Catalog file:

- Select File ... New or press cmd-n.
- Select "String Catalog" in the "Resource" section.
- Click the "Next" button.
- Specify the name of the file.
  Typically the default name "Localizable.xcstrings" is retained.
- Click the "Create" button.

A project can use multiple String Catalog,
but for most projects there is no reason to have more than one.

### Languages

There are two ways to add a language to a project.

The older, long way is to:

- Select the topmost entry in the Project Navigator.
- Select the project.
- Select the "Info" tab.
- Expand the "Localizations" section.
- Click the "+" at the bottom of the section.
- Select a language from the dropdown.

The newer, short way is to:

- Select the String Catalog file which is typically named `Localized.xcstrings`.
- Click the "+" in the lower-left corner of the language list.
- Select a language from the dropdown.

Either way all current translation keys will now appear
in the String Catalog editor for the new language,
ready for their translations to be entered.

To delete a language:

- Select the topmost entry in the Project Navigator.
- Select the project.
- Select the "Info" tab.
- Expand the "Localizations" section.
- Select the language to be deleted.
- Click the "-" at the bottom of the section.

Languages cannot be deleted from the String Catalog editor.

### Automatic Keys

A String Catalog can be automatically populated from string literals
in source files that define SwiftUI views.
To enable this:

- select the top-most entry in the Project Navigator
- select the project
- select the "Build Settings" tab
- scroll down to the "Localization" section
- change "Use Compiler to Extract Swift Strings" from "No" to "Yes"

<img alt="Xcode Build Settings for Localization" style="width: 80%"
  src="/blog/assets/xcode-build-settings-localization.png?v={{pkg.version}}"
  title="Xcode Build Settings for Localization">

Once this is enabled, the String Catalog file will be updated during each build.

The build process looks for string literals in view code.
It uses all strings passed to functions where
the parameter type is `LocalizedStringKey`.

It also looks for the following kinds of values in non-view code.
Use these instead of plain String values
to indicate that they require language translation.

```swift
LocalizedStringResource(
    "some text",
    table: "SomeTable", // optional
    local: someLocale, // optional
    comment: "some comment" // optiona
)

String(localized: "some text")

AtributedString(localized: "some text")
```

`LocalizedStringResource` is a new type that can hold
the following four components of localizable strings:

- key

  This is a unique identifier (within a table) that is used at runtime
  to lookup the text to render. It can match the English text.

- defaultValue

  This is an optional value that defaults to the value
  corresponding to the key in the default locale.
  Specifying this value is useful when the
  strings in the source files are not in English.
  The default locale can be selected in the project editor.

- comment

  This describes where and how a particular translation is being used in the UI.

- table

  One or more files where translations are stored.
  By default the "localizable" table is used.

For long sentences or paragraphs, it is useful to choose a key
rather than typing the full text into a source file.
This requires entering the translation for the default locale
(often English).

For example, the key "app-overview" could have the English translation
"This app provides weather forecasts in a delightful way."

### Translations

To add or edit translations for a given language:

- Select the Project Navigator.
- Select the String Catalog file, typically named `Localized.xcstrings`.
- Select a language on the left side of the String Catalog editor.
- Select a key.
- Enter or modify the translation text for the key.

For languages that are missing translations, a percentage of
supplied translations is displayed after the language name.
Once all the translations for a language are supplied,
the percentage is replaced by a green check mark.
When all languages have a green check mark,
language translation is complete for the current set of keys.

The last column in the translations tables indicates the
state of the translations which can be one of the following:

- "NEW" - found in code, but no translation has been supplied yet
- "STALE" - no longer found in code, but a translation has been supplied
- NEEDS REVIEW - someone marked the translation as needing review
  by right-clicking the row and selecting "Mark for Review"
- green check mark - successfully translateds

Keys for stale translations can be deleted
if it is suspected that it will never be used in the future.

After a translation that needs review is reviewed,
right-click it and select "Mark as Reviewed".

To delete a translation, select it and press the delete key.
This removes the key from all languages,
not just from the currently selected language.
Do not press cmd-delete because that offers to
delete the entire String Catalog file!

### Manual Keys

Translations can be manually added to String Catalog.
This enables working on translations before
their keys are actually used in the code.

To add a new key:

- Select any language on the left.
- Click the "+" at the top of the list of keys.
- Enter a key which adds it to the table for all current languages.
- For each language:
  - Select the language.
  - Enter a translation for the key.

Manually added keys are not managed by Xcode.
This means they will not be automatically updated or removed by Xcode.
To cause a manually added key to be managed by Xcode,
select its row, open the inspector on the right, and
change the value for "Managed" from "Manually" to "Automatically".

<img alt="Xcode Strings Catalog Inspector" style="width: 50%"
  src="/blog/assets/xcode-strings-catalog-inspector.png?v={{pkg.version}}"
  title="Xcode Strings Catalog Inspector">

### Plurals

To support plurals in a translation:

- Add count placeholders for each count in the key.
  For example, "The %lld dog barked at the %lld squirrel.".
  Note that `%lld` is a C-style format specifier.
- For each count placeholder, right-click the row and select "Vary by Plural".
  This adds sub-rows that are appropriate for the language.
- Enter translations for each plural option.

Repeat the steps above for each language that requires
platform-specific translations.

When there is more than one number placeholder in a key,
each is given a name like `@arg1`.
These placeholders can be renamed to make their meaning more clear.
To rename a placeholder, click the name in its sub-row and
enter a new name that still begins with `@`.

In the example above, `@arg1` might be renamed to `@dogs`
and `@arg2` might be renamed to `@squirrels`.

### Device-specific Translations

The translated strings can vary by device.
For example, the word "tap" which is appropriate for iOS and iPadOS
can be changed to "click" for macOS.

To add device-specific translations for a key:

- Select a language.
- Right-click a translation row.
- Select "Vary by Device" and a device type such as "Mac" or "Apple Watch".
- Enter the translation text for each device type.
  For example, it may be desirable to have shorter translation strings
  on "Apple Watch" to fit on the smaller screen.

This will add sub-rows to the row where each row
begins with a platform name or "Other".

<img alt="Xcode Strings Catalog Vary by Device" style="width: 100%"
  src="/blog/assets/xcode-strings-catalog-vary-by-device.png?v={{pkg.version}}"
  title="Xcode Strings Catalog Vary by Device">

Repeat the steps above for each language that requires
platform-specific translations.

### More Strings Catalog Editor Features

The "Filter" input can be used to filter the list of translations
to those that contain given text in their key or translation.

Any table column heading can be clicked to sort the rows on that column
in either ascending or descending order.

## Strings Files

These were used before iOS 17.
For iOS 17 and beyond, using String Catalogs is preferred.

### Creating a Strings File

Translations are described in a "Strings" file.
To create one:

1. Add a file to the project by pressing cmd-n.
1. In the Resource section, select the "Strings File" template.
1. Click the "Next" button.
1. Keep the default directory and the default file name "Localizable.strings".
1. Click the "Create" button.
1. Select the "Localizable.strings" file in the Project Navigator.
1. Open the Inspector panel on the right.
1. Under "Localization", click the "Localize..." button.
1. In the dialog that appears, click the "Localize" button.
1. Once again, select the "Localizable.strings" file in the Project Navigator.
1. In the Inspector panel on the right under "Localization",
   check all the language checkboxes.
   This will add one entry under "Localizable.strings"
   in the Project Navigator for each selected language.

### Populating a Strings File

1. In the Project Navigator, expand the "Localizable.strings" entry
   to expose an entry for each supported language.
1. For each supported language

   1. Select its entry in the Project Navigator.
   1. For each string to be translated

      1. Enter an assignment statement of the form `"key" = "translation";`

         **Note the semicolon at the end.**

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

Comments using the syntax `//` or `/* ... */` can be used.

To validate a `Localizable.strings` file,
cd to the project directory that contains the `.lproj` files and
enter `plutil -lint {language-code}.lproj/Localizable.strings`.

### Preview Locale

To select a locale to use in the Preview,
apply the `environment` view modifier to the top-most view.
For example:

```swift
ContentView()
    .environment(\.locale, .init(identifier: "la"))
```

where `la` is replaced by a language abbreviation such as `en` for English
or a combination of a language and region abbreviation such as `en-US`.

A view can have multiple previews.
A button for each preview is displays at the top of the canvas.
Click a button to see the corresponding preview.

<img alt="Xcode Preview buttons" style="width: 50%"
  src="/blog/assets/xcode-preview-buttons.png?v={{pkg.version}}"
  title="Xcode Preview buttons">

```swift
#Preview("English") {
    ContentView()
        .environment(\.locale, .init(identifier: "en"))
}

#Preview("Spanish") {
    ContentView()
        .environment(\.locale, .init(identifier: "es"))
}

#Preview("French") {
    ContentView()
        .environment(\.locale, .init(identifier: "fr"))
}
```

## Simulator and Device Locale

To select a locale in the Simulator or on a device:

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
allow the user to select a locale within the app.

This {% aTargetBlank
"https://developer.apple.com/forums/thread/13155?answerId=36704022#36704022",
"Apple Forums post" %} says the following:

> If the system puts up a UI component on your behalf it will do so
> in the localization that it thinks that your app is running in.
> If your app is 'pretending' to run in some other language,
> you'll end up with a mixed localization, where some parts of your app
> (the parts you control) are in one language and other parts of your app
> (the parts controlled by the OS) are in another.
> That's a very poor user experience.

Despite this warning, the `environment` view modifier
can be used to change the current locale.
For example:

```swift
        VStack {
            Picker("Locale", selection: $language) {
                Text("English in U.S.").tag("en-US")
                Text("French in France").tag("fr-FR")
                Text("Spanish in Spain").tag("es-ES")
            }
            .pickerStyle(.segmented)

            Text("Hello!")
        }
        .padding()
        .environment(\.locale, .init(identifier: language))
```

Changing the locale in this way rather than in the Settings app
works for many kinds of translations, but not all.

Either of the following approaches can be used to get a translation in code:

```swift
let translation = Bundle.main.localizedString(
    forKey: key,
    value: key, // translation defaults to the key
    table: nil // defaults to "Localizable"
               // which uses the file Localizable.strings
)

// This is a shortcut for the above where table is nil
// which causes it to use `Localizable` for the table.
let translation = NSLocalizedString(key, comment: "")
```

These approaches get a translation based on
the locale selected in the Settings app.
However, neither of these honor the locale specified with
the environment view modifier.

The same issue exists with retrieving localized images from an "Image Set".
TODO: Is this a bug in SwiftUI?

## Localizing Strings

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/nslocalizedstring",
"macro" %} returns a localized string.
To simplify use of this, define the following `String` extension:

```swift
import Foundation

extension String {
    var localized: String {
        return NSLocalizedString(self, comment: "")
    }
}
```

To use this, add references to this new computed property to any string.
For example:

```swift
print("Hello".localized)

let greeting = "Hello"
print(greeting.localized)
```

## Localized String Arguments

Localized strings can specify arguments accepted and
where they should be inserted in translations.
Arguments are represented by `%@` for `String` values,
`%lld` for `Int` values, and `%f` for `Double` values.
Arguments are passed to the `LocalizedStringKey` initializer
using string interpolation.

For example, the following translations can be defined:

```text
// English
"Annie %@ %@" = "The %@ will come out %@.";

// French
"Annie %@ %@" = "Le %@ sortira %@.";
"sun" = "soleil";
"tomorrow" = "demain";

// Spanish
"Annie %@ %@" = "El %@ saldra %@.";
"sun" = "sol";
"tomorrow" = "mañana";
```

The following code uses these translations:

```swift
let thing = "sun"
let time = "tomorrow"
Text("Annie \(sun) \(tomorrow)")
```

TODO: How can we translate the strings "sun" and "tomorrow"?

## Plurals (old way)

This approach for supporting plurals was used before iOS 17.
For iOS 17 and beyond, using String Catalogs is preferred.

SwiftUI can automate displaying phrases that describe a number of things
where the word that describes the thing varies based on the count.
For example, "dog" vs. "dogs" or "cactus" vs. "cacti".

There are two kinds of plural values to consider.
Cardinal numbers specify a count or quantity (ex. two).
Ordinal numbers specify a position within an ordered list (ex. second).

Pluralization of cardinal numbers is supported by a `.stringsdict` file
which is described in this section.
Pluralization of ordinal numbers is supported by `NumberFormatter`
as described in the [Numbers and Currency](#numbers-and-currency) section.

To define how specific words should be pluralized based on a count:

1. Add a file to the project by pressing cmd-n.
1. In the Resource section, select the "Stringsdict File" template.
1. Click the "Next" button.
1. Keep the default directory and the default file name
   "Localizable.stringsdict".
1. Click the "Create" button.
1. To support multiple languages:
   1. Select the "Localizable.stringsdict" file in the Project Navigator.
   1. Open the Inspector panel on the right.
   1. Under "Localization", click the "Localize..." button.
   1. In the dialog that appears, click the "Localize" button.
   1. Once again, select the "Localizable.stringsdict" file
      in the Project Navigator.
   1. In the Inspector panel on the right under "Localization",
      check all the language checkboxes.
      This will add one entry under "Localizable.stringsdict"
      in the Project Navigator for each selected language.
1. For each supported language:
   1. Select its entry in the Project Navigator.
   1. Add rows with keys, types, and values similar to what is
      shown in the screenshot below for each phrase to be pluralized.
      A row and all its descendant rows can be copied
      from one language entry to another to save typing.

<figure style="width: 90%">
  <img alt="SwiftUI English plural localization"
    src="/blog/assets/swiftui-plural-localization-en.png?v={{pkg.version}}"
    title="Swift English plural localization">
  <figcaption>English plural rules</figcaption>
</figure>
<figure style="width: 90%">
  <img alt="SwiftUI French plural localization"
    src="/blog/assets/swiftui-plural-localization-fr.png?v={{pkg.version}}"
    title="Swift French plural localization">
  <figcaption>French plural rules</figcaption>
</figure>
<figure style="width: 90%">
  <img alt="SwiftUI Spanish plural localization"
    src="/blog/assets/swiftui-plural-localization-es.png?v={{pkg.version}}"
    title="Swift Spanish plural localization">
  <figcaption>Spanish plural rules</figcaption>
</figure>

The supported format specifiers include:

- `%d` - signed integer
- `%u` - unsigned integer
- `%lld` - long long decimal (`Int64`)
- `%f` - double

`%lld` works for `Int` values, but it seems the `%d` and `%u` do not!

The following keys are used to define how counts should be pluralized.
Their meaning differs based on the locale,
especially the meaning of "few" and "many".

| Key     | English Meaning         |
| ------- | ----------------------- |
| `zero`  | 0                       |
| `one`   | 1                       |
| `two`   | 2                       |
| `few`   | 3                       |
| `many`  | > 3                     |
| `other` | any value not specified |

Many languages such as English, French, German, Italian, and Spanish
only use `zero`, `one`, and `other`.
Chinese and Japanese only use one form.
Arabic uses all six forms.

For more details see the Unicode Common Locale Data Repository (CLDR)
{% aTargetBlank "https://cldr.unicode.org/index/cldr-spec/plural-rules",
"Plural Rules" %} and {% aTargetBlank
"https://unicode-org.github.io/cldr-staging/charts/latest/supplemental/language_plural_rules.html",
"Language Plural Rules" %}.

The following code demonstrates displaying a number of apples.
The string to be formatted must contain one string interpolation.
The count can be hard-coded in the interpolation
or provided by a variable.

```swift
Text("apple \(2)") // a pair of apples
let appleCount = 7
Text("apple \(appleCount)") // 7 apples
```

## Numbers and Currency

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/numberformatter",
"NumberFormatter" %} class will localize date formats
when its `locale` property is set.

Currency formatting requires the locale identifier
to contain both a language code and a country code.

The following code demonstrates all the supported number formatting styles:

```swift
let number = NSNumber(value: 1234.5678)
let formatter = NumberFormatter()

func demo(_ style: NumberFormatter.Style) {
    formatter.numberStyle = style
    formatter.locale = Locale(identifier: "en-US") // English U.S.
    print(formatter.string(from: number) ?? "invalid")
    formatter.locale = Locale(identifier: "fr-FR") // French France
    print(formatter.string(from: number) ?? "invalid")
}

demo(.none)
// 1235
// 1235

demo(.decimal)
// 1,234.568
// 1 234,568

demo(.percent)
// 123,457%
// 123 457 %

demo(.scientific)
// 1.2345678E3
// 1,2345678E3

demo(.spellOut)
// one thousand two hundred thirty-four point five six seven eight
// mille deux cent trente-quatre virgule cinq six sept huit

demo(.ordinal)
// 1,235th
// 1 235e

demo(.currency)
// $1,234.57
// 1 234,57 €

// From the Apple documentation,
// "This style behaves like the .currency style,
// except that negative numbers representations are
// surrounded by parentheses rather than preceded by a negative symbol.
demo(.currencyAccounting)
// $1,234.57
// 1 234,57 €

demo(.currencyPlural)
// 1,234.57 US dollars
// 1 234,57 euros
```

## Dates

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/dateformatter",
"DateFormatter" %} class will localize date formats
when its `locale` property is set.
This works with predefined date/time styles
and also with custom date/time templates.
For example:

```swift
let date = Date()
let formatter = DateFormatter()

formatter.dateStyle = .short
formatter.timeStyle = .short
formatter.locale = Locale(identifier: "en") // English
print(formatter.string(from: date)) // 1/3/23, 1:36 PM

formatter.locale = Locale(identifier: "fr") // French
print(formatter.string(from: date)) // 3/01/2023 13:36

Text("apple \(0)")
Text("apple \(1)")
Text("apple \(2)")
Text("apple \(3)")
Text("apple \(4)")
let appleCount = 7
Text("apple \(appleCount)")

formatter.setLocalizedDateFormatFromTemplate("dd MMMM")
formatter.locale = Locale(identifier: "en") // English
print(formatter.string(from: date)) // 03 January

formatter.locale = Locale(identifier: "fr") // French
print(formatter.string(from: date)) // 03 janvier
```

For a summary of patterns for formatting dates and times see {% aTargetBlank
"https://www.datetimeformatter.com/how-to-format-date-time-in-swift/",
"How To Format Date Time In Swift" %}.

## Images

To render a localized image where
a different image is displayed for each supported language:

1. Add an "Image Set" to the `Assets.xcassets` file.
1. Open the Inspector pane on the right side.
1. Click the last tab which shows the "Attributes Inspector".
1. In the "Localizations" section, check all the languages to be supported.
   This opens a separate area in the image editor panel
   for each language where 1x, 2x, and 3x images can be added.
1. Add at least one image size for each language.
1. Render the image by passing the image set name to the `Image` view
   in the same way as for any other image.

The following code assumes that an Image Set named "landmark"
was created in the `Assets.xcassets` file and it
contains different images for English, French, and Spanish.

```swift
Image("landmark")
    .resizable()
    .aspectRatio(contentMode: .fit)
    .frame(height: 150)
    .id(locale) // This does not help!
```

The image changes when the user changes the region
specified in the Settings app. For the steps to do this, see
[## Simulator and Device Locale](#simulator-and-device-locale).

If the locale is changed within the app using the
`environment` view modifier, localized images do not update.
Perhaps this is a SwiftUI bug.

Another approach that does work is to create a separate Image Set
for each supported locale.
The following code assumes that Image Sets named
"landmark-en-US", "landmark-fr-FR", and "landmark-es-ES"
were created in the `Assets.xcassets` file.
It does update when the value of the `locale` variable changes.

```swift
Image("landmark-" + locale)
    .resizable()
    .aspectRatio(contentMode: .fit)
    .frame(height: 150)
```

## Device Testing

To change the default language on an iOS device:

1. Open the Settings app.
1. Tap "General".
1. Tap "Language & Region".
1. To add a language to the list of "PREFERRED LANGUAGES":
   1. Tap "Add Language..."
   1. Tap a language in the list.
   1. Tap a button to either use that language now or continue using the current language.
1. To change the current language, drag it to the top of the list
   using the drag handles on the trailing edge of the list rows.
1. To delete a language, swipe its row to the left,
   tap the "Delete" button, and tap the "Continue" button.

It can be important to also select a specific region.
For example, an iOS device in the United States will typically have
the language set to "English" and the region set to "United States".
If the language is changed to "Français",
but the region remains set to "United States"
then dates will not be formatted in the expected way for French.
Changing the region to "France" corrects this.

To also change the default region on an iOS device:

1. Tap "Region"
1. Tap the name of a region.
1. Tap the "Change to {region-name}" button.

## Exporting and Importing Localizations

Localization strings found in String Catalog files, Strings files,
and StringsDict files can be exported to a file that can be
sent to localization experts that will supply translations.
Once the file is returned, it can be imported into the project
to update the files that define them.

When using String Catalogs:

- Select the top-most entry in the Project Navigator.
- Select the project.
- Select the "Build Settings" tab.
- Scroll down to the "Localizations" section.
- Verify that "Localization Prefers String Catalogs" is set to "Yes"

<img alt="Xcode Build Settings for Localization" style="width: 80%"
  src="/blog/assets/xcode-build-settings-localization.png?v={{pkg.version}}"
  title="Xcode Build Settings for Localization">

To export the localizations:

- Select Product ... Export Localizations...
- Select the directory where a new directory will be created
  (defaults to the project directory).
- Enter a name for the new directory
  (defaults to "{project-name} Localizations").
- Uncheck any languages whose translations should not be exported.
- Click the "Export" button.

The new directory will contain one localization catalog (`.xcloc` directory)
for each supported language.
A localization catalog contains several subdirectories and files.
The translations for a localization catalog can be found in the `.xliff` file
inside the "Localized Contents" subdirectory.

{% aTargetBlank
"http://docs.oasis-open.org/xliff/xliff-core/v2.1/os/xliff-core-v2.1-os.html",
"XLIFF" %} is an industry standard XML format.
XLIFF files contains one `trans-unit` element for each translation.
These elements contain the child elements `source`, `target`, and `comment`.
People with language translation experience can edit these files,
modifying the `target` and `comment` element contents.

To import modified localizations:

- Select Product ... Import Localizations...
- Select a `.xcloc` file for a specific language.
- Click the "Import" button.
- Differences between the current translations and the
  translations to be imported will be displayed side-by-side.
  No editing can be performed here.
- If the differences are acceptable, click the "Import" button.
  The changes will be merged into the file from which they originated,
  a Strings Catalog file or a Strings File.
  However, it seems that modified comments are not merged.
