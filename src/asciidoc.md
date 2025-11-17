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

## Attributes

AsciiDoc supports built-in and user=defined attributes.
Built-in attributes configure how the document is rendered.
This fall into several categories such as document and table.
User-defined attributes are used for text replacement.

Document attributes configure global behavior.
This must follow the level 1 header
with no blank lines preceding them.

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

  :sectnums:
  source-highlighter,"Specifies the syntax highlighter used for code blocks (e.g., Rouge, highlight.js).",:source-highlighter: rouge
  experimental,"Enables features still under development (e.g., DITA or other experimental syntax).",:experimental:
  tabsize,Sets the number of spaces represented by a tab character in source code blocks.,:tabsize: 4
  idprefix,Prefix to prepend to section IDs.,:idprefix: \_
  idseparator,Character used to replace spaces in section titles when generating IDs.,:idseparator: -

Table attributes ...

Custom attributes ...

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

## Code Blocks

To mark a set of lines as source code, use the following syntax.

```text
[source]
----
code goes here
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

## Comments

Single-line comments begin with `//`.

Multi-line comments begin with the line `////`
and end with the same.

## Converting to HTML

To generate HTML from an AsciiDoc file, enter `asciidoctor {name}.adoc`.
This generates `{name}.html`.

## Pandoc

[Pandoc](http://pandoc.org/) converts between various markup formats.
To install Pandoc in macOS, enter `brew install pandoc`.

## Converting from Markdown

To convert a Markdown file to AsciiDoc in macOS:

```bash
pandoc -f markdown -t asciidoc {name}.md -o {name}.adoc
```

## Converting to PDF

To generate a PDF from an AsciiDoc file:

- `gem install asciidoctor-pdf` (one time)
- `asciidoctor-pdf {name}.adoc`

This automatically adds page numbers.

## Character Escaping

These characters must be escaped for literal versions: `+` and `{`.

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
image::assets/htmx-logo.png[htmx, 200]
// Only specify a height.
image::assets/htmx-logo.png[htmx, , 100]
```

An inline image uses the same syntax, except
only a single colon follows the `image` macro name.
The image image:assets/htmx-logo.png[htmx, 80] is inline.

To add figure numbers and a caption to an image,
add a line before the `image` macro that begins with a period
and is followed by the caption. For example:

```text
.htmx was created by Carson Gross.
image::assets/htmx-logo.png[htmx, 200]
```

## VS Code Extension

Install the AsciiDoc extension.

To preview an `.adoc` file in VS Code:

- Open an `.adoc` file.
- Click the "Open Preview" button in the upper-left.
  The icon looks like a two-column document
  with a magnifier glass in the lower-left.
  Alternatively, press cmd-k v.

The AsciiDoc VS Code extension can also:

- export to DocBook, HTML, or PDF
- paste Markup for an image in the clipboard

  An image file with the current timestamp is created
  in the same directory as the document.

## Generating Slides

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

## Resources

- [AsciiDoc: The Complete Guid in 2025](https://www.adoc-studio.app/blog/asciidoc-guide?utm-source=ChrisChinchilla)
