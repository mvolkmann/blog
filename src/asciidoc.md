---
eleventyNavigation:
  key: AsciiDoc
layout: topic-layout.njk
---

## Overview

[AsciiDoc](http://asciidoc.org/) is an alternative to Markdown.

AsciiDoc was created in 2002 by Stuart Rackham.
It supports more features, can easily be converted
to DocBook, HTML, and PDF formats, and is less popular.

Markdown was created in 2004 by John Gruber and Aaron Swartz.
It is simpler, a bit more readable, and more popular.

The recommended file extension for AsciiDoc files is `.adoc`.

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

## Asciidoctor

Asciidoctor is an open-source tool that converts AsciiDoc files
into other formats like HTML, PDF, EPUB, and DocBook.

To install the `asciidoctor` command:

1. `brew install ruby` (also installs the `gem` command)
1. `brew install ruby`
1. Edit `.zshrc` and add the following:

   ```bash
   export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
   ```

1. `gem install asciidoctor`

## Attributes

AsciiDoc supports built-in and user-defined attributes.

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

:sectnums:
source-highlighter,"Specifies the syntax highlighter used for code blocks (e.g., Rouge, highlight.js).",:source-highlighter: rouge
experimental,"Enables features still under development (e.g., DITA or other experimental syntax).",:experimental:
tabsize,Sets the number of spaces represented by a tab character in source code blocks.,:tabsize: 4
idprefix,Prefix to prepend to section IDs.,:idprefix: \_
idseparator,Character used to replace spaces in section titles when generating IDs.,:idseparator: -

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

```text
[quote, Microsoft Haiku, late 1990's]
Out of memory.
We wish to hold the whole sky,
But we never will.
```

To create a block of quoted text with multiple paragraphs:

```text
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

## Escaping Text

These characters must be escaped for literal versions: `+` and `{`.

To escape text so special characters like curly braces are not
interpreted specially, surround the text with `+` characters.
To also render the text in a monospace font,
additionally surround the text with backticks.

## HTML Output

To generate HTML from an AsciiDoc file, enter `asciidoctor {name}.adoc`.
This generates `{name}.html`.

## Images

To render an image, use the `image` macro.
This takes a relative file path to an image file.
Its attribute list can specify alt text,
a width in pixels, and a height in pixels.
If both a width and height are specified,
the aspect ratio can differ from that of the image.
So it's best to only specify one of those values.

When followed by two colons, it renders an image on its own line.

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

To add figure numbers and a caption to an image,
add a line before the `image` macro that begins with a period
and is followed by the caption. For example:

```text
.htmx was created by Carson Gross.
image::assets/htmx-logo.png[htmx, 200]
```

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

## Links

To add a link to a URL, enter the URL
followed by the the link text inside square brackets.
For example: `https://chatgpt.com[ChatGPT]`.
For HTML output, to cause clicking the link to open in a
new browser tab/window, add a caret at the end of the link text.
For example: `https://chatgpt.com[ChatGPT^]`.

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

Another option is to use multiple \* characters to indicate the level.

```text
* Level 1
** Level 2
*** Level 3
```

For ordered lists:

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
The editor extension will provide warnings
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

## Markdown to AsciiDoc

[Pandoc](http://pandoc.org/) converts between various markup formats.
To install Pandoc in macOS, enter `brew install pandoc`.

To convert a Markdown file to AsciiDoc:

```bash
pandoc -f markdown -t asciidoc {name}.md -o {name}.adoc
```

## Math Formulas

To render math formulas, enable it with the document attribute
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

- `gem install asciidoctor-pdf` (one time)
- `asciidoctor-pdf {name}.adoc`

This automatically adds page numbers.

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

## Tables

TODO: Describe how to render tables.

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
