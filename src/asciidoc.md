---
eleventyNavigation:
  key: AsciiDoc
layout: topic-layout.njk
---

## Overview

[AsciiDoc](http://asciidoc.org/) is a markup language
that is an alternative to Markdown.
Markdown was created in 2004 by John Gruber and Aaron Swartz.
Markdown is simpler, a bit more readable, and more popular.
AsciiDoc was created in 2002 by Stuart Rackham.
AsciiDoc supports more features that Markdown, can easily be
converted to several output formats, and is less popular.

[Asciidoctor](https://asciidoctor.org) is software that reads AsciiDoc files
and converts them into output formats such as HTML, PDF, EPUB, and DocBook.
It is implemented in Ruby and was created by
Dan Allen, Sarah White, and Ryan Waldron.

AsciiDoc files are text files that can be edited in any text editor.
The recommended file extension for AsciiDoc files is `.adoc`.

Some AsciiDoc features are only supported by specific backends.
For example, a feature that works when converting to HTML
may not work when converting to PDF or EPUB.

## VS Code

If VS Code is used to edit `.adoc` files,
install the AsciiDoc extension from asciidoctor.org.
It provides syntax highlighting, snippets, live preview, and
the several command palette commands including:

- AsciiDoc: Export Document as PDF
- AsciiDoc: Save HTML Document
- AsciiDoc: Save to DocBook
- AsciiDoc: Open Preview
- AsciiDoc: Open Preview to the Side

A preview can also be opened to the side by clicking the icon in
the upper-right that looks like a book with a magnifier glass on top.

By default, previews use the same styling as other VS Code tabs
which is typically white text on a black background.
To instead display black text on a white background,
modify the AsciiDoc extension settings so that
"Asciidoc > Preview: Style" has no value and
"Asciidoc > Preview: Use Editor Style" is unchecked.

There are many AsciiDoc features that are not supported by VS Code previews.
So if something doesn't seem to be working, generate HTML or PDF
and verify that is works correctly there.

Also consider installing the "Code Spell Checker" extension
from streetsidesoftware.com.

## Syntax Details

AsciiDoc documents have two parts, a header section and a body section,
that are separated by a blank line.

The term "directive" encompasses attributes, macros,
block delimiters, and replacement/flow syntax.

The header section contains directives that control document settings.
Examples include `= Document Title`,
`:sectnum:` (to enable section numbering),
`:toc:` (to enable generating a table of contents),
and user-defined attributes.

User-defined attributes represent Boolean values (set or not set)
or replacement values that can be inserted
anywhere in the document after their definition.

Macros are processed and replaced by some output.
They have two syntaxes, one for inline use and one for block use.

Inline macro syntax is `name:target[attributes]`.
The square brackets are required even when no attributes are specified.
For example, `link:https://example.com[Example Site]`
and `footnote:[This is a note]`.

Block macro syntax is `name::target[attributes]`.
Note the use of two colons instead of one.
For example, `image::logo.png[Logo]`
and `include::common-attributes.adoc[]`.
Some macros, such as `endif::[]`, do not require specifying a target.

Replacement/flow syntax includes:

- page breaks with `<<<`
- horizontal rules with `'''`
- paragraph continuation with `+` preceded by a space
- conditional rendering with `ifdef` and `ifndef`

## Basic Formatting

To format text, add specific characters before and after the text.

| Formatting  | Character        |
| ----------- | ---------------- |
| bold        | `*`              |
| highlight   | `#`              |
| italic      | `_` (underscore) |
| monospace   | \` (backtick)    |
| superscript | `^`              |
| subscript   | `~`              |

These special characters can be nested to apply multiple effects,
but they must be nested in a specific order.
For example, the following applies all four formatting effects:

```text
This is `#*_monospace, highlighted, bold, and italic text_*#`.
```

Other styling is specified by surrounding the text to be styled
with either single of double `#` characters
and preceding it with square brackets that contain
references to CSS classes (only for HTML output),
referred to as "roles". For example:

| Styling          | Syntax                             |
| ---------------- | ---------------------------------- |
| background color | `[.{color}-background]#some text#` |
| text color       | `[.{color}]#some text#`            |
| slightly bigger  | `[.big]#some text#`                |
| slightly smaller | `[.small]#some text#`              |
| strikethrough    | `[.line-through]#some text#`       |
| overline         | `[.overline]#some text#`           |
| underline        | `[.underline]#some text#`          |

Surrounding the text to be styled with double `#` characters
is needed when that text contains single `#` characters.
For example:

```adoc
Before I [.underline]##eat # cake## I will get milk.
```

The color must be one of these 16 colors:
aqua, black, blue, fuchsia, gray, green, lime, maroon,
navy, olive, purple, red, silver, teal, white, and
yellow.
To use other colors in HTML output, specify a custom CSS class.

To apply multiple roles, list them inside
the square brackets with no spaces between them.
For example:

```adoc
The following is [.red.big.underline]#special text#.
```

When converting to HTML, custom styles can be
defined in a CSS file and used as roles.
For example, suppose the file `styles.css` is created in
the same directory as the `.adoc` files that wish to use it.
It can contain the following:

```css
.custom {
  background-color: yellow;
  color: blue;
  font-size: 24px;
  font-weight: bold;
  text-decoration: underline;
}
```

To use this in a `.adoc` file, add the following attribute in the header:

```adoc
:stylesheet: styles.css
```

Use the CSS classes it defines as follows:

```adoc
See my [.custom]#custom styling# defined in a CSS file.
```

This does not work in VS Code previews or when converting to PDF and EPUB.
To use custom roles in conversion to PDF, see
{% aTargetBlank "https://docs.asciidoctor.org/pdf-converter/latest/roles/",
"Roles" %}.
This explains that the custom roles must be defined
in a YAML theme file rather than in a CSS file.

The following theme file is the equivalent of the CSS above.

```yaml
extends: default

role:
  custom:
    background-color: #FFFF00
    font-color: #0000FF
    font-size: 24
    font-style: bold
    text-decoration: underline
```

To use this, add the following attribute in the document header:

```adoc
:pdf-theme: custom-theme.yml
```

This will work when running the `asciidoctor-pdf` from a terminal,
but it will not work when running the
VS Code command "AsciiDoc: Export Document as PDF".

## Admonitions (Callouts)

There are four supported kinds of admonitions:
CAUTION, IMPORTANT, NOTE, TIP, and WARNING.
By default, each is preceded by its type.
To render icon instead, add the ":icons: font" document attribute
near the beginning of the document.

The following are examples of single paragraph admonitions:

```text
CAUTION: This is a caution admonition.

IMPORTANT: This is an important admonition.

NOTE: This is a note admonition.

TIP: This is a tip admonition.

WARNING: This is a warning admonition.
```

For multiple paragraph admonitions, use the following syntax:

```text
[{kind}]
====
paragraph 1

paragraph 2

paragraph 3
====
```

Add a title to any admonition by preceding it with a line that
begins with a period and is followed by the title
with no space between the period and the first letter in the title.
For example:

```text
.Don't do this!
WARNING: Rock climbing without ropes is dangerous.
```

## Aligning Text

By default, text is left-aligned.
To change this, add one of the following before the text:

```adoc
[.text-left]
[.text-center]
[.text-right]
```

## Asciidoctor

To install the `asciidoctor` command in macOS:

1. Install {% aTargetBlank "https://brew.sh", "Homebrew" %}.
1. Install the latest version of Ruby.

   Enter `brew install ruby` to install by the `ruby` and `gem` commands.

1. Configure the shell to use this new version of Ruby
   instead of the version that ships with macOS.

   Edit `.zshrc` and add the following:

   ```bash
   export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
   export PATH="$(ruby -r rubygems -e 'puts Gem.bindir'):$PATH"
   ```

1. Enter `gem install asciidoctor`.

## Asciidoctor.js Live Preview

The browser extension "Asciidoctor.js Live Preview"
is supported in the Chrome and Firefox browsers.
It provides a live preview of AsciiDoc files.
Changes saved in any text editor are reflected in the browser
after a couple of seconds.

To install this in Chrome:

- Click the vertical ellipsis button in the upper-right.
- Select Extensions ... Visit Chrome Web Store.
- Search for "asciidoc".
- Click "Asciidoctor.js Live Preview".
- Click the "Add to Chrome" button.
- Click the "Add extension" button.
- Click the extensions button (looks like a puzzle piece) in the upper-right.
- Click the vertical ellipsis after "Asciidoctor.js Live Preview".
- Select "Manage Extension".
- Enable "Allow access to file URLs".

To use this, select File ... Open File... and open any `.adoc` file.

## Attributes

AsciiDoc supports system-defined (built-in) and user-defined attributes.

### System-defined Attributes

There are two categories of system-defined attributes,
document and character replacement.

{% aTargetBlank
"https://docs.asciidoctor.org/asciidoc/latest/attributes/document-attributes-ref/",
"Document attributes" %} globally affect how the document is rendered.
They must follow the level 1 header with no blank lines preceding them.

Examples of document attributes include:

- `:doctype: {type}` sets the document type to `article`, `book`,
  `inline` (for embedding in another document), or `manpage`.
  This affects the top-level structure.

- `:icons:` specifies how visual markers like those in admonitions are rendered.
  Supported values are:

  - `font`: uses font icons like those in Font Awesome
  - `image`: uses an image file for icons
  - `static`: uses plain text (default)

- `:imagesdir:` specifies the directory to search for images
  referenced using the `image` macro when not specified.

- `:sectnums:` enables automatic section numbering for all headings.

- `:sectnumlevels: {levels}` specifies the maximum number of levels to number.
  The default is 3.

- `:source-highlighter:` specifies the syntax highlighter
  used for source blocks. By default, code is not highlighted.

- `:toc:` enables generation of a table of contents based on headings.

- `:toclevels:` specifies the number of levels to include in the table of contents.
  The default is 3.

- `:toc-title: Some Title` sets the title of the generated table of contents
  which defaults to "Table of Contents"

TODO: Finish summarizing the system-defined attributes.

- `:experimental:` enables features still under development
  (e.g. DITA or other experimental syntax).
- `:tabsize:` sets the number of spaces represented by a tab character
  in source code blocks.
- `:idprefix:` specifies the prefix to prepend to section IDs.
- `:idseparator:` specifies the character used to replace spaces
  in section titles when generating IDs.

References to {% aTargetBlank
"https://docs.asciidoctor.org/asciidoc/latest/attributes/character-replacement-ref/",
"character replacement attributes" %} are replaced by a specific character.
For example, `{deg}` is replaced by the degree symbol.

Commonly used character replacement attributes include:

| Attribute | Replacement              |
| --------- | ------------------------ |
| `deg`     | degree symbol            |
| `lsquo`   | smart left single quote  |
| `rsquo`   | smart right single quote |
| `ldquo`   | smart left double quote  |
| `rdquo`   | smart right double quote |
| `nbsp`    | non-breaking space       |

### User-defined Attributes

User-defined attributes are used for text replacement and conditional rendering.
They are defined with the syntax `:name: [value]`.
The value can be any text, including AsciiDoc markup such as `image` macros.

The following attribute is Boolean in nature,
meaning it is either set or not set.
Directives described under "Conditional Rendering" can test whether it is set.

```text
:black-friday:
```

The following attribute has an assigned value.
Directives described under "Conditional Rendering" can test its value.

```text
:year: 2025
```

It can be substituted into text that follows with the syntax `{name}`.
For example:

```text
Year: {year}
```

User-defined attributes can use any name except
those reserved for system-defined attributes.
They can be anywhere in an AsciiDoc file,
but they are typically defined in the header.
References to them can appear anywhere after they are defined.

User-defined attributes can also be defined in a separate file
that is included in files that use them.
This allows changing their values without modifying the files that use them.
It enables generating multiple versions of the same base document.
For example, the file `attributes.adoc` can contain the following:

```adoc
:black-friday:
:year: 2025
```

Files that use these attributes can include them with the following:

```adoc
include::attributes.adoc[]
```

## Audio

The `audio` macro renders an HTML-based audio player
for playing a given audio file.
Supported formats include MP3 (`.mp3`), Wave (`.wav`), and Ogg Vorbis (`.ogg`).
The audio file is assumed to be in the same directory as the `.adoc` file
or in the directory specified by the `:imagesdir:` attribute.
For example:

```adoc
.Dog Barking
audio::dog-bark.mp3[]
```

A URL can be specified to play an audio file on the internet.

To play only a segment, specify start and end values in seconds.
For example:

```adoc
.Dog Barking
audio::dog-bark.mp3[start=2, end=4]
```

## Author

Specify the author name and email address
as the first line after the level 1 header.
For example:

```text
R. Mark Volkmann <r.mark.volkmann@gmail.com>
```

To insert the author name in the document, use `{author}`.
To insert the author email in the document, use `{email}`.
For example:

```text
This document was written by {author}.
Contact the author at {email}.
```

To hide the author line at the top of the document,
add the document attribute `:!author:`.

## Block Quotes

To create a block of quoted text with a single paragraph:

```adoc
[quote, Microsoft Haiku, late 1990's]
Out of memory.
We wish to hold the whole sky,
But we never will.
```

The `quote` keyword can be followed by one or two pieces of text
that appear below the quote and right-aligned.

To create a block of quoted text with multiple paragraphs:

```adoc
[quote, multiple paragraphs]
____
This is the first paragraph.

This is the second paragraph.
____
```

## Blocks

A block is a range of lines that have a specific purpose.
There are two ways to assign a block type to a range of lines.
The first is to preceded a contiguous set of lines (no blank lines)
with a block type in square brackets.
The second is to add a line containing only a block delimiter
before and after a range of lines.
A block delimiter consists of four of the same character.

| Block Type  | Delimiter Line | Purpose                                             |
| ----------- | -------------- | --------------------------------------------------- |
| comment     | `////`         | comment out a range of lines                        |
| example     | `====`         | show results of an operation                        |
| listing     | `----`         | show source code without syntax highlighting        |
| literal     | `....`         | display text exactly as written in a monospace font |
| passthrough | `++++`         | avoid processing Asciidoc substitutions             |
| sidebar     | `****`         | separate auxiliary text from main text              |
| source      | `----`         | show source code with syntax highlighting           |

To get syntax highlighting in a source block,
add the `:source-highlighter:` attribute
followed by a syntax highlighter name in the header.
The options are:

- `coderay`: This is an older option implemented in Ruby
  that is replaced by `rouge`.
  It is supposed to work with HTML, PDF, and EPUB3 output,
  but it doesn't work with HTML.
- `highlight.js`: This requires client-side JavaScript in HTML.
  It is the only option that works for HTML output and in VS Code previews.
- `pygments`: This is implemented in Python.
  It is supposed to work with HTML, PDF, and EPUB3 output,
  but it doesn't work with HTML.
- `rouge`: This is a newer option implemented in Ruby that
  does not require client-side JavaScript in HTML (recommended).
  It is supposed to work with HTML, PDF, and EPUB3 output,
  but it doesn't work with HTML.

For example:

```adoc
:source-highlighter: highlight.js
```

The following are examples of listing and source blocks:

```adoc
// This block ends at the first blank line.
// It does not render with syntax highlighting
// because the block type is listing rather than source.
[listing, javascript]
function add(a, b) {
  return a + b;
}

// This block is referred to as a "delimited block".
// It ends after the matching block delimiter.
// It does render with syntax highlighting.
[source, javascript]
----
function add(a, b) {
  return a + b;
}

console.log(add(2, 3)); // outputs 5
----
```

For more detail on source blocks, see the "Source Blocks" section.

Any kind of block can be given a title by preceding it with
a line that begins with a period, followed by a caption.

## Comments

Single-line comments begin with `//`.

Multi-line comments begin with the line `////`
and end with the same.

## Conditional Content

The `ifdef` (short for "if defined") and `ifndef` (short for "if not defined")
directives test whether an attribute is set or unset
and conditionally include content up to the next `endif` directive.
This enables generating multiple versions of output
from the same input document.
For example:

```adoc
:happy:

ifdef::happy[]
Have a great day!
endif::[]

ifndef::happy[]
Cheer up.  Things will get better soon.
endif::[]
```

This will render "Have a great day!" because the `happy` attribute is set.
To instead render "Cheer up.",
delete the line that sets the `happy` attribute or comment it out.

To have an attribute that is set for part of the document and unset later,
unset it by adding an exclamation mark after the name.
For example: `:happy!`.

Content can be conditionally included based on the target output format.
For example:

```adoc
ifdef::backend-html5[]
I am HTML 5.
endif::[]

ifdef::backend-pdf[]
I am PDF.
endif::[]

ifdef::backend-epub3[]
I am EPUB 3.
endif::[]
```

To test for multiple attributes, separate them by comma or plus characters.
For example:

```adoc
ifdef::attr1,attr2[] // tests whether either is set
ifdef::attr1+attr2[] // tests whether both are set
```

To test a condition other that an attribute being set,
use the `ifeval` directive.
It can use the relational operators `==`, `!=`, `<`, `<=`, `>=`, and `>`
to test both numeric and string values.
It cannot use arithmetic operators like `+`, `-`, `*`, and `/`.
For example:

```adoc
// When assign a string value to an attribute,
// do not surround the value in quotes.
:city: St. Louis
:season: Winter
:my-score: 7
:opponent-score: 3

// Quotes are optional when the value is a single word,
// but including them is recommended.
ifeval::[{season} == Winter]
Keep warm!
endif::[]

// Quotes are needed when the value is a multiple words.
ifeval::["{city}" == "St. Louis"]
Welcome to St. Louis!
endif::[]

ifeval::[{my-score} > {opponent-score}]
You are winning!
endif::[]
```

The `ifeval` macro cannot test multiple conditions
and it cannot test the values of counter variables
such as `counter:section`.

## Diagrams

GraphViz diagrams can be described in AsciiDoc files and rendered.
For example, the following can be added in a `.adoc` file:

```adoc
[graphviz, flow-diagram, svg]
----
digraph G {
    // Set some styling attributes
    rankdir="LR"; // left to right layout
    node [shape=box, style=rounded];

    // Define the nodes
    A [label="Start Process"];
    B [label="Data Input"];
    C [label="Data Validation"];
    D [label="Finish"];

    // Define the edges (connections)
    A -> B;
    B -> C [label="Success"];
    B -> D [label="Failure"]; // Flow bypasses validation on failure
    C -> D;
}
```

The installs required to support this are:

- `brew install graphviz`
- `gem install bundler`
- `gem install asciidoctor-diagram`.

The steps convert the `.adoc` file to HTML are:

1. `cd` to the directory containing the `.adoc` file to be converted.
1. Create the file `Gemfile` containing the following:

   ```bash
   source 'https://rubygems.org'
   gem 'asciidoctor'
   gem 'asciidoctor-diagram'
   ```

1. Enter `bundle install`.
1. Enter `bundle exec asciidoctor -r asciidoctor-diagram {name}.adoc`

This creates the file `flow-diagram.svg` in the same directory as `{name}.adoc`.

The generated HTML will contain the following `img` element:

```html
<img src="flow-diagram.svg" alt="flow diagram" width="631" height="113" />
```

The rendered diagram will be the following:

<img alt="generated diagram" src="/blog/assets/graphviz-in-asciidoc.svg?v={{pkg.version}}">

## DocBook Output

DocBook is a standardized XML format for technical documentation.
These files can be easily converted to EPUB, HTML, man pages, and PDF.
AsciiDoc files can be directly converted to these formats
without generating an intermediate DocBook file.

To generate a DocBook file from an AsciiDoc file:

```bash
asciidoctor -b docbook5 {name}.adoc
```

This creates the file `{name}.xml`.

## Em Dash

To render an em dash, use two dashes (`--`) surrounded by spaces.

## EPUB Output

{% aTargetBlank "https://www.w3.org/TR/epub-33/", "EPUB" %}
is a W3C standard data format typically used for e-books.
It is supported by many e-readers, smartphones, and tablets.

To generate an EPUB file from an AsciiDoc file:

- `gem install asciidoctor-epub3` (one time)
- `asciidoctor-epub {name}.adoc`

This automatically adds page numbers.

## Escaping Text

These characters must be escaped for literal versions: `+` and `{`.

To escape text so special characters like curly braces are not
interpreted specially, surround the text with `+` characters.
To also render the text in a monospace font,
additionally surround the text with backticks.

## Footnotes

The `footnote` macro inserts an automatically numbered footnote.
The syntax is `footnote:{id}[{text}]`.
The id is optional and is only needed
to add the same footnote in multiple locations.
For example:

```adoc
My whippet footnote:fn1[The Whippet is a British breed of dog of sighthound type.] is very fast!
...
Comet footnote:fn1[] is ready to play.
```

No space is required before the `footnote` macro,
but omitting it will cause spell checkers to complain.

Adding footnotes in headings breaks footnote numbering.

If HTML output, all the footnotes appear at the bottom of the last page.
If PDF output, footnotes appear at the end of each chapter in which they appear.

Footnote numbers are links.
Clicking a footnote number in the text jumps to the footnote.
Clicking a footnote number before a footnote jumps to where it is referenced.

## Horizontal Rules

To add a horizontal rule, add a line containing three single quotes.

## HTML Output

To generate HTML from an AsciiDoc file, enter `asciidoctor {name}.adoc`.
This generates the file `{name}.html` in the same directory as the `.adoc` file.

## Icons

To enable access to the large set Font Awesome icons (over 2000),
include the following in the header:

```adoc
:icons: font
```

To render an icon, use `icon:{name}[]`.
For example: `icon:leaf[]`.

To change the size of an icon, include the `size` attribute
with a value of `lg`, `2x`, `3x`, `4x`, or `5x`.
The `lg` size is larger than the default size and smaller than `2x`.
For example: `icon:leaf[size=3x]`.

To change the color of an icon, add the `role` attribute
with one of the following 16 color names:
aqua, black, blue, fuchsia, gray, green, lime, maroon,
navy, olive, purple, red, silver, teal, white, and yellow.
For example:

```adoc
icon:leaf[role=green]
```

To rotate an icon, include the `rotate` attribute
with a value of `90`, `180`, or `270`.

To flip an icon, include the `flip` attribute
with a value of `horizontal` or `vertical`.

These attributes can be combined, but if both `flip` and `rotate`
area specified, only `flip` is honored. For example:

```adoc
icon:leaf[size=2x, role=green, rotate=90]
```

Icons can be used as bullets in an unordered list.
If multiple icons are used, adding the fixed width (fw) attribute
causes the icons to all occupy the same width.
The `hardbreaks` option causes each consecutive line below
to return its line break.

```adoc
[%hardbreaks]
icon:paw[fw] Comet
icon:paw[fw] Greta
icon:paw[fw] Oscar
icon:paw[fw] Ramsay
icon:thermometer[fw] Temperature
```

This renders the following:

<img alt="icon bullets"
  src="/blog/assets/asciidoc-icon-bullets.png?v={{pkg.version}}"
  style="width: 25%">

The following additional icon libraries can be used for PDF output.

| Library Name         | Library Abbreviation |
| -------------------- | -------------------- |
| Font Awesome Solid   | fas                  |
| Font Awesome Brands  | fab                  |
| Font Awesome Regular | far                  |
| Foundation Icons     | fi                   |

To specify the default library,
add `:icon-set: {library-abbreviation}` in the header.
For example:

```adoc
:icon-set: fab

icon:apple[]
icon:amazon[]
icon:android[]
icon:css3[]
icon:github[]
icon:google[]
icon:html5[]
icon:youtube[]
```

<img alt="fab icons"
  src="/blog/assets/asciidoc-fab-icons.png?v={{pkg.version}}"
  style="width: 30%">

Icons from a library not specified with the `:icon-set` attribute,
can used by including the library abbreviation and a dash before icon names.
For example: `icon:fas-lightbulb[]`.
However, it seems all the icon libraries are available
without specifying their prefix. For example, `icon:lightbulb[]`.

## Images

To render an image, use the `image` macro.
This takes a relative file path to an image file.
Its attribute list can specify alt text,
a width in pixels, and a height in pixels.
If both a width and height are specified,
the aspect ratio can differ from that of the image.
So it's best to only specify one of those values.

When followed by two colons, it renders an image on its own line.
In the following examples, "htmx" is the alt text.

```text
// Use default size.
image::assets/htmx-logo.png[htmx]
// Only specify a width.
image::assets/htmx-logo.png[htmx, width=200]
// Only specify a height.
image::assets/htmx-logo.png[htmx, height=100]
```

The width and height can also be specified as the
2nd and 3rd positional arguments.

An inline image uses the same syntax, except
only a single colon follows the `image` macro name.
The image `image:assets/htmx-logo.png[htmx, width=80]` is inline.

To specify the directory to search for images
when not specified in an `image` macro,
add `:imagesdir: {some-directory}` in the header.

To automatically add figure numbers and a caption to an image,
add a line before the `image` macro that begins with a period
and is followed by the caption. For example:

```text
.htmx was created by Carson Gross.
image::assets/htmx-logo.png[htmx, 200]
```

To make an image be a hyperlink that opens in a new browser tab,
add the `link` and `window` attributes.
All of this must be on a single line.
For example:

```adoc
image::dog.png[dog, width=75, link="https://somesite.com", window="_blank"]
```

To add a light gray border around an image,
precede it with the line `[.thumb]`.
This works for HTML output, but not for PDF output or in VS Code previews.

To turn an image into an link, add `link={url}`. For example:

```adoc
image::comet.jpg[my whippet, width=100, link=https://en.wikipedia.org/wiki/Whippet]
```

There will be no indication that this is a link.
To add a blue outline around images that are links,
add the CSS rule `a > img { border: 1px solid blue; }`.

To cause the link to open in a new browser tab,
add the attribute `window=_blank` inside the square brackets.

In HTML output, to float an image left or right so text wraps around it,
add the attribute `role=left` or `role=right`.
For example:

```adoc
.Comet, the whippet
image::comet.jpg[my whippet, width=125, role=right]
The Whippet is a British breed of dog of sighthound type. It closely resembles
the Greyhound and the smaller Italian Greyhound, and is intermediate between
them in size. In the nineteenth century it was sometimes called "the poor man's
racehorse". It is commonly kept as a companion dog, for competitive showing or
for amateur racing, and may participate in various dog sports, including lure
coursing, agility, and flyball. It has the fastest running speed within its
weight and size range, and is believed to have the fastest idle-to-running
acceleration of any dog.
```

This renders the following:

![image float](/blog/assets/asciidoc-image-float.png)

Note how the image is left-aligned with its caption.
To center the image over the caption, add `align=center`.
To right-align the image over the caption, add `align=right`.

To add a light gray border around the image, add "thumb" to the role.
For example, `role=thumb right`.

In PDF output, to float an image left or right so text wraps around it,
specify the `float` attribute instead of the `role` attribute.
For example:

```adoc
.Comet, the whippet
image::comet.jpg[my whippet, width=125, float=right]
```

PDF output does not support the `align` attribute or the `thumb` value.

## Including Files

The `include` directive includes the contents of a text file
into the current file, even other AsciiDoc files.
It can be used to partition a large file into smaller ones.
When the files represent chapters in a book,
it may be necessary to include a blank line between each `include`.

The `include` directive is commonly used to
include files `[listing]` and `[source]` blocks.
Listing blocks display verbatim text with no syntax highlighting.
Source blocks display programming language source code
with optional syntax highlighting.

The syntax is `include::{file-path}[{attributes}]`.
The file path can be absolute, relative to the current file, or a URL.
All the attributes are optional, but the square brackets are required.

To temporarily disable an include, precede it with a backslash.

The supported attributes are:

- `encoding`: supported values are `UTF-8` (default),
  `US-ASCII`, and `ISO-8850-1`
- `indent`: controls indentation in `[source]` and `[listing]` blocks
- `leveloffset`: adjusts the section/heading level of the
  content being included, relative to the structure of this document
- `lines`: specifies the range of lines to be included
- `opts`: provides instructions that modify the inclusion process
- `tags`: specifies named regions of lines to include

Specifying `[opts=optional]` causes AsciiDoc to silently ignore
the `include` directive if the file it references is not found.

## Line Breaks

To force a line break, add a space and `+` at the end of a line.
For example:

```adoc
first line +
second line
```

To include an empty line, add a line containing only a space followed by `+`.
Alternatively, include the line `{empty} +`.

## Links

To add a link to a URL, enter the URL
followed by the the link text inside square brackets.
For example: `https://chatgpt.com[ChatGPT]`.
For HTML output, to cause clicking the link to open in a
new browser tab/window, add a caret at the end of the link text.
For example: `https://chatgpt.com[ChatGPT^]`.

If the link text contains commas or equal signs,
it must be enclosed in double quotes.

For links that may need to appear multiple times,
or just to manage all the links in one place,
create a file with a name like `links.adoc`.
Add lines that define attributes whose values are links.
For example:

```adoc
:chatgpt-link: https://chatgpt.com[ChatGPT^]
```

To insert the links in other AsciiDoc files,
include the file that defines the links
and add attribute references where needed.
For example:

```adoc
include::links.adoc[]
...
{chatgpt-link}
```

To add a link to a location in the current document,
mark the location with `[[{id}]]` and link to it with `<<{id}, {link-text}>>`.
For example:

```adoc
<<details, Important Details>>
...
[[details]]Details you need to know:
```

To create a mailto link, add `mailto:{email-address}[{link-text}]`.
For example, `Send email to mailto:r.mark.volkmann@gmail.com[Mark].`.

## Listing Blocks

To display text lines in a monospace font, add lines containing
four consecutive dashes before and after the lines.

```text
----
lines go here
----
```

Optional preceded the first line of four dashes
with a line containing `[listing]`.

## Lists

For a bulleted lists, precede each item with \* or -.
Alternate for each nested level.

```text
* Fruit
  - Apple
  - Orange
* Meat
  - Beef
  - Chicken
  - Pork
* Vegetable
  - Corn
  - Tomato
```

Another option is to use multiple \* characters to indicate the level.
Indenting the lines to indicate their nesting level is optional.

```adoc
* Level 1
** Level 2
*** Level 3
```

To include paragraphs of text below a bullet,
precede each blank line with a `+` character.
For example:

```text
* Football
+
American Football is a high-intensity team sport played between two teams
of 11 players on a rectangular field with goalposts at each end.
The objective is to score points by advancing the ball into the opposing
team's end zone, primarily through running plays or passing plays.
+
It’s known for its strategic complexity, physicality, and passionate fan base,
especially in the United States where it's a cultural staple from high school
to the NFL.

* Hockey
+
Ice hockey is a fast-paced, physical team sport played on an ice rink
between two teams of six players each — five skaters and one goaltender.
The objective is to score goals by shooting a puck into the opposing team's net
using a hockey stick.
```

To create an ordered list (numbered),
use multiple `.` characters to indicate the level.
Indenting the lines to indicate their nesting level is optional.

```text
. Item 1
.. Item 1.1
... Item 1.1.1
. Item 2
.. Item 2.1
. Item 3
.. Item 3.1
```

To explicitly specify each item "number",
enter them as they should be rendered.
The VS Code AsciiDoc extension provides warnings
if any are skipped or they are out of order.

```text
A. Item 1
   1. Item 1.1
      a. Item 1.1.1
B. Item 2
   1. Item 2.1
C. Item 3
```

For definition lists, separate terms and definitions with ::.
By default, definitions will be indented on their own line
even if they aren't written that way.

```text
Term 1:: Definition 1
Term 2::
  Definition 2
```

For non-indented definition lists,
precede them with the "horizontal" attribute?

```text
[horizontal]
Term 1:: Definition 1
Term 2::
  Definition 2
```

To create a checklist,
precede each unchecked item with `* [ ]`
and each checked item with `* [*]`.
For example:

```adoc
* [ ] buy bread
* [*] buy peanut butter
* [ ] eat lunch
```

## Markdown to AsciiDoc

[Pandoc](http://pandoc.org/) converts between various markup formats.
To install Pandoc in macOS, enter `brew install pandoc`.

To convert a Markdown file to AsciiDoc:

```bash
pandoc -f markdown -t asciidoc {name}.md -o {name}.adoc
```

## Math Equations

To render math equations, enable it with the document attribute
`:stem: latexmath`. Then use the `stem` directive.

The examples below render the quadratic equation.

For inline formulas, use the following syntax:

```text
stem:[x = {-b \pm \sqrt{b^2-4ac} \over 2a}]
```

For block formulas, use the following syntax:

```text
[stem]
++++
x = {-b \pm \sqrt{b^2-4ac} \over 2a}
++++
```

## Page Breaks

To add a page break in PDF output,
insert a blank line followed by a line containing only `<<<`.
It's unclear when the blank line is needed.

## PDF Output

To generate a PDF from an AsciiDoc file:

1. `gem install asciidoctor-pdf` (one time)

   This assumes that `asciidoctor` has already been installed.

1. `asciidoctor-pdf {name}.adoc`

This generates the file `{name}.pdf` in the same directory as the `.adoc` file.
It automatically adds page numbers which are in the page footers by default.

## Sections

The beginning of each section in a document is marked by a line that
starts with a number of `=` characters, followed by a title.
For example, `== Introduction`.

The document begins with a level 0 section which uses a single `=`.
Level 1 sections begin with `==`, level 2 with `===` and so on.

TODO: "The interpretation of the level 0 title depends entirely on
the document's doctype setting." Where is the doctype set?

To include automatically generated section numbers,
add `:sectnums:` in the header.

By default, only section header levels 1, 2, and 3 are numbered.
To change this, add `:sectnumlevels: {n}` in the header.

## Slide Output

To generate slidy slides, enter `asciidoctor -b slidy {input-file}`.
This generates `{name}.html`, but it's in slidy format.

Level 1 & 2 headers start new slides.

Slidy supports the following keyboard shortcuts:

- left arrow goes to previous slide
- right arrow or spacebar goes to next slide
  (can also mouse click anywhere on slide)
- s or < makes font smaller
- b or > makes font bigger
- c shows table of contents
- f toggles display of status line
- home goes to first slide
- end goes to last slide
- a toggles between showing all slides on one page
  and one slide at a time

## Source Blocks

To mark a set of lines as source code, use the following syntax.

```text
[source, {language}]
----
source code goes here
----
```

By default, the code is not highlighted.
To add syntax highlighting, specify a syntax highlighter
using the `:source-highlighter:` document attribute.
Its value is one of the following supported syntax highlighter libraries:

- [coderay](http://coderay.rubychan.de) is an older option
  implemented in Ruby that is replaced by `rouge`.
  It doesn't seem to work!
- [highlight.js](https://highlightjs.org) requires client-side JavaScript
  in HTML. It is the only option that works in VS Code previews.
- [pygments](https://pygments.org) is implemented in Python.
  It doesn't seem to work!
- [rouge](https://rouge.jneen.net) is a newer option implemented in Ruby
  that does not require client-side JavaScript in HTML.
  It is the only option that works with `asciidoctor-pdf`.
  To install this, run `gem install rouge`.

For example, `:source-highlighter: highlight.js`.

When using VS Code, if the command palette command
"AsciiDoc: Save HTML Document" is used to generate HTML,
highlight.js is the only syntax highlighter that works.
If instead HTML is generated by running the `asciidoctor` command
from a terminal, all the syntax highlighters work.

Then specify the programming language for each code block.
For example:

```text
[source, javascript]
----
function greet(name) {
  const message = `Hello, ${name}!`;
  return message;
}
----
```

The supported programming languages and the words used to specify them
vary based on the selected syntax highlighting library.
For highlight.js, see
[Supported Languages](https://highlightjs.readthedocs.io/en/latest/supported-languages.html).

To cause the source code lines to be numbered, add the `linenums` attribute.
This is not supported by highlight.js.
For example:

```adoc
[source, javascript, linenums, highlight=1..3]
----
code goes here
----
```

To highlight selected lines with a light yellow background,
add the `highlight` attribute with a line range value.
For example:

```adoc
[source, javascript, highlight=1..3]
----
code goes here
----
```

## Table of Contents

To enable automatically generating a table of contents,
add `:toc:` in the header.

By default, only section header levels 1, 2, and 3 are included.
To change this, add `:toclevels: {n}` in the header.

## Tables

The markup for a table begins and ends with a line
that contains a vertical bar followed by three equal signs.
The first line inside those specifies the column headings
where each is preceded by a vertical bar.
These can all be on one line or be placed on separate lines.
A blank line separates the heading row from the data rows.
The data rows follow the same pattern as the heading rows.

The basic syntax for tables is:

```adoc
|===
| Heading 1 | Heading 2 | Heading 3

| row 1 col 1 | row 1 col 2 | row 1 col 3
| row 2 col 1 | row 2 col 2 | row 2 col 3
|===
```

<img src="/blog/assets/asciidoc-table-basic.png" alt="AsciiDoc basic table" />

This table spans the width of the page,
the heading row is given a light gray background,
and each column is allocated the same width.

To change the table width, add a line before the table
that specifies the `width` attribute inside square brackets.
For example, `[width=50%]`.

To specify the alignment of each column,
add the `cols` attribute inside the square brackets.
Its value specifies the alignment of each column,
and thereby the number of columns.
For example, the following specifies that
the first column is left-aligned (`<`),
the second column is centered (`^`), and
the third column is right-aligned (`>`).

```adoc
[cols="<,^,>"]
```

When the columns are specified in this way,
the value of each column within a data row (not the heading row)
can be on its own line.
For example, the previous table markup can be changed as follows:

```adoc
[cols="<,^,>"]
|===
| Heading 1 | Heading 2 | Heading 3

| row 1 col 1
| row 1 col 2
| row 1 col 3
| row 2 col 1
| row 2 col 2
| row 2 col 3
|===
```

<img src="/blog/assets/asciidoc-table-alignment.png" alt="AsciiDoc table alignment" />

To specify the relative width of each column
so they do not all have the same width, add the `cols` attribute
with a value that is a comma-separated list of relative widths.
For example, `[cols="2,1,3"]` specifies three columns
where the first is twice as wide as the second
and the third is three times as wide as the second.

The alignment of each column can be specified
before each of the relative widths.
For example, `[cols="<2,^1,>3"]`.

The alignment of a heading can differ from
the alignment of the data in its column.
To override the alignment for a heading,
add an alignment character before its vertical bar.
The following example demonstrates this,
and also the ability for a data cell to
span multiple columns and/or multiple rows.
All the columns are left-aligned,
but all the headings are centered.

```adoc
[cols="<,<,<"]
|===
// Heading row
^|Column 1 ^|Column 2 ^|Column 3

// First data row - "foo" spans two columns
|Hello 2+|I span two columns.

// Second data row - "apple pie" spans two rows
.2+|I span two rows. |apple |banana

// Third data row - nothing in the first column
|cherry |grape

// Fourth data row
2.2+|I span two columns and two rows. |lemon

// Fifth data row - nothing in first two columns
| orange

// Sixth data row
|raspberry |strawberry |watermelon
|===
```

<img src="/blog/assets/asciidoc-table-spanning.png" alt="AsciiDoc table with spanning" />

## Themes

Themes define many characteristics of PDF output including
page size, margins, headers, and footers.

There is a default theme defined in the file `default-theme.yml`.
The location of this file differs based on the operating system.
Suppose the main `.adoc` file has the name `book.adoc`.
One way to discover the location of yours is to run the following command:

```bash
asciidoctor-pdf --theme missing book.adoc
```

In macOS, this outputs the following error message
which gives the location of the default theme file:

```text
asciidoctor: ERROR: could not locate or load the built-in pdf theme `missing'
because of Errno::ENOENT No such file or directory @ rb_sysopen -
/opt/homebrew/lib/ruby/gems/3.4.0/gems/asciidoctor-pdf-2.3.24/data/themes/missing-theme.yml;
reverting to default theme
```

Study the contents of the `default-theme.yml` file in that directory
to understand which settings you may wish to override in a custom theme.

To define a custom theme, create a file in the same directory as the main
`.adoc` file whose name ends with `-theme.yml` such as `custom-theme.yml`.
This file typically begins with the following line:

```yaml
extends: default
```

Let's create a custom theme that overrides
the header and footer sections of each page.
This is documented at {% aTargetBlank
"https://docs.asciidoctor.org/pdf-converter/2.0/theme/add-running-content/",
"Add Running Content" %}.

```yaml
extends: default

page:
  # Order of values is [top, right, bottom, left].
  margin: [$base_line_height_length * 4, 0, $base_line_height_length * 2, 0]

  # This causes Roman numeral page numbers to be used before page 5
  # (which includes the preface and table of contents)
  # and Arabic numeral page numbers to be used starting on page 5
  # (which is where the first chapter begins).
  numbering:
    start-at: 5

header:
  height: $base_line_height_length * 4
  padding: [$base_line_height_length, 0, 0, 0]
  # This prevents {section-title} from including the section number.
  title-style: basic
  vertical_align: top

  verso:
    left:
      content: '*{page-number}*'
    center:
      content: 'Chapter {chapter-numeral} {nbsp} {chapter-title}'
    right:
      content: ''
  recto:
    left:
      content: ''
    center:
      content: '{section-title}'
    right:
      content: '*{page-number}*'

# The default footer is suppressed by setting :nofooter:
# in main .adoc file, so the following isn't strictly necessary.
footer:
  border_width: ~
  height: 0
  recto:
    right:
      content: ~
  verso:
    left:
      content: ~
```

TODO: The verso center content is not rendering anything.
The docs say "If you reference an attribute which is not defined,
all the text on that same line in the running content will be dropped."
TODO: See https://asciidoctor.zulipchat.com/#narrow/channel/288690-users.2Fasciidoctor-pdf/topic/custom.20theme.20header.2Ffooter.20substitutions/with/561002388

Attribute substitutions that are commonly used in headers and footers include
`chapter-numeral`, `chapter-title`, `document-title`, `document-subtitle`,
`page-number`, `page-count`, `part-numeral`, `part-title`,
`section-number`, and `section-title`.

There are two ways to use a custom theme.
The first way is to add the following attributes in the header of `book.adoc`:
This defaults to searching for the specified theme file
in the same directory as `default-theme.yml`.
Note that only the part of the custom theme file name
that precedes `-theme.yml` is specified.

```adoc
:pdf-themesdir: .
:pdf-theme: custom
```

Interestingly, the same result is obtained by only
adding the following attribute, in which case it defaults to
searching for the theme file in the current directory:

```adoc
:pdf-theme: custom-theme.yml
```

This is not honored by the VS Code AsciiDoc extension.
See {% aTargetBlank
"https://github.com/asciidoctor/asciidoctor-vscode/issues/979",
"issue 979" %}.

The second way to use a custom theme is to
add the following option to the `asciidoctor-pdf` command.
This defaults to searching for the specified theme file
in the current directory.
Note that the entire custom theme file name is specified.

```bash
asciidoctor-pdf --theme custom-theme.yml book.adoc
```

## Video

The `video` macro renders an HTML-based video player
for playing a given video file.
The video file is assumed to be in the same directory as the `.adoc` file
or in the directory specified by the `:imagesdir:` attribute.

```adoc
.Rabbit Waking
video::rabbit-waking.mp4[]
```

A URL can be specified to play a video file on the internet.

To play only a segment, specify start and end values in seconds.
For example:

```adoc
.Rabbit Waking
video::rabbit-waking.mp4[start=2, end=4]
```

To specify the size of the video viewport,
add the `width` and `height` attributes. For example:

```adoc
.Rabbit Waking
video::rabbit-waking.mp4[width=240, height=135]
```

To play a YouTube video, specify its id and the `youtube` attribute.
The id can be obtained by opening the video in a web browser and
noting the value of the `v` query parameter in the address bar.
For example, let's enable playing the video at {% aTargetBlank
"https://www.youtube.com/watch?v=gBhOg0Vriik",
"Lambda Calculus iJS New York 2025" %}.

```adoc
.Lambda Calculus
video::gBhOg0Vriik[youtube, width=600, height=338]
```

Supposedly there are options to cause the video to autoplay and loop,
but I could not get them to work.

## VS Code Extension

Install the AsciiDoc extension.

To preview an `.adoc` file in VS Code:

- Open an `.adoc` file.
- Click the "Open Preview" button in the upper-left.
  The icon looks like a two-column document
  with a magnifier glass in the lower-left.
  Alternatively, press cmd-k v.

To generate HTML output, open the command palette
and select "AsciiDoc: Save HTML Document".
To view the rendered HTML, locate the `.html` file in the Explorer pane,
right-click it, and select "Open with Live Server".

To generate PDF output, open the command palette
and select "AsciiDoc: Export Document as PDF".
A file dialog will open where you can enter the file name to use
and select the destination directory.
If a previous version of the PDF is open in VS Code,
it will update automatically.
If a previous version of the PDF is open in the macOS Preview app,
it will update automatically when that app becomes active.

To paste an image in the system clipboard into an AsciiDoc document
using the `image:` directive, open the command palette
and select "AsciiDoc: Paste Image".
An image file whose name is the current timestamp is
created in the same directory as the document,
and the `image:` directive will refer to that file.

To generate DocBook output, open the command palette
and select "AsciiDoc: Save to DocBook".

TODO: See other "AsciiDoc: ?" commands in the command palette.

## Resources

- [AsciiDoc: The Complete Guid in 2025](https://www.adoc-studio.app/blog/asciidoc-guide?utm-source=ChrisChinchilla)
- [POWERMAN AsciiDoc cheatsheet](https://powerman.name/doc/asciidoc)
