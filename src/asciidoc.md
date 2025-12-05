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

Block delimiters are lines containing four of the same character
that define the boundaries of a specific kind of block.

| Block Type  | Delimiter Line |
| ----------- | -------------- |
| comment     | `////`         |
| example     | `====`         |
| listing     | `----`         |
| literal     | `....`         |
| passthrough | `++++`         |
| sidebar     | `****`         |

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

AsciiDoc supports built-in and user-defined attributes.

### Built-in Attributes

Built-in attributes configure how the document is rendered.
They fall into several categories such as document and table.

Document attributes configure global behavior.
They must follow the level 1 header
with no blank lines preceding them.

Examples of document attributes include:

- `:doctype: {type}` sets the document type to `article`, `book`,
  `inline` (for embedding in another document), or `manpage`.
  This affects the top-level structure.

- `:icons:` specifies how visual markers like those in admonitions are rendered.
  Supported values are:

  - `font`: uses font icons like those in Font Awesome
  - `image`: uses an image file for icons
  - `static`: uses plain text (default)

- `:sectnums:` enables automatic section numbering for all headings.

- `:sectnumlevels: {levels}` specifies the maximum number of levels to number.
  The default is 3.

- `:source-highlighter:` specifies the syntax highlighter used for code blocks.
  By default, code is not highlighted.
  The options are:

  - `coderay`: This is an older option implemented in Ruby that is replaced by `rouge`.
    It doesn't seem to work!
  - `highlight.js`: requires client-side JavaScript in HTML
    This is the only option that works in VS Code previews.
  - `pygments`: This is implemented in Python.
    It doesn't seem to work!
  - `rouge`: newer option implemented in Ruby that
    does not require client-side JavaScript in HTML (recommended)

- `:toc:` enables generation of a table of contents based on headings.

- `:toclevels:` specifies the number of levels to include in the table of contents.
  The default is 3.

- `:toc-title: Some Title` sets the title of the generated table of contents
  which defaults to "Table of Contents"

TODO: Finish summarizing the built-in attributes.

- `:experimental:` enables features still under development
  (e.g. DITA or other experimental syntax).
- `:tabsize:` sets the number of spaces represented by a tab character
  in source code blocks.
- `:idprefix:` specifies the prefix to prepend to section IDs.
- `:idseparator:` specifies the character used to replace spaces
  in section titles when generating IDs.

### User-defined Attributes

User-defined attributes are used for text replacement and conditional rendering.
They are defined with the syntax `:name:[ value]`.

The following attribute is Boolean in nature.
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

User-defined variables can be defined in a separate file
that is included in files that use them.
This allows changing their values without modifying the files that use them.
It enables generating multiple versions of the same base document.
For example, the file `variables.adoc` can contain the following:

```adoc
:black-friday:
:year: 2025
```

Files that use these variables can include them with the following:

```adoc
include::variables.adoc[]
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

## Comments

Single-line comments begin with `//`.

Multi-line comments begin with the line `////`
and end with the same.

## Conditional Content

The `ifdef` and `ifndef` directives test whether an attribute is set or unset
and conditionally include content up to the next `endif` directive.
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

To test a condition other that an attribute being set,
use the `ifeval` directive.
It can use the relational operators `==`, `!=`, `<`, `<=`, `>=`, and `>`.
It cannot use arithmetic operators like `+`, `-`, `*`, and `/`.
For example:

```adoc
:my-score: 7
:opponent-score: 3

ifeval::[{my-score} > {opponent-score}]
You are winning!
endif::[]
```

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

## Horizontal Rules

To add a horizontal rule, add a line containing three single quotes.

## HTML Output

To generate HTML from an AsciiDoc file, enter `asciidoctor {name}.adoc`.
This generates the file `{name}.html` in the same directory as the `.adoc` file.

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

To automatically add figure numbers and a caption to an image,
add a line before the `image` macro that begins with a period
and is followed by the caption. For example:

```text
.htmx was created by Carson Gross.
image::assets/htmx-logo.png[htmx, 200]
```

To make an image be a hyperlink that opens in a new browser tab,
add the `link` and `window` attributes.
For example:

```adoc
image::dog.png[dog, width=75, link="https://somesite.com", window="_blank"]
```

To add a light gray border around an image,
precede it with the line `[.thumb]`.
This works for HTML output, but not for PDF output or in VS Code previews.

To float an image left or right so text wraps around it,
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

To cause the source code lines to be numbered,
add `, numbered` after the language.
TODO: This works with rouge, but I haven't been able to get it to work
with highlight.js! Maybe the AsciiDoc book you ordered will show how.

## Table of Contents

To enable automatically generating a table of contents,
add `:toc:` in the header.

By default, only section header levels 1, 2, and 3 are included.
To change this, add `:toclevels: {n}` in the header.

## Tables

TODO: Describe how to render tables.

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
