---
eleventyNavigation:
  key: AsciiDoc
layout: topic-layout.njk
---

## Overview

[AsciiDoc](http://asciidoc.org/) is an alternative to Markdown
that supports more features.

The recommended file extension for AsciiDoc files is `.adoc`.

## Attributes

AsciiDoc supports built-in and user=defined attributes.
Built-in attributes configure how the document is rendered.
This fall into several categories such as document and table.
User-defined attributes are used for text replacement.

Document attributes configure global behavior.

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

  - `coderay`: older option implemented in Ruby that is replaced by `rouge`
  - `highlight.js`: requires client-side JavaScript in HTML
  - `pygments`: implemented in Python
  - `rouge`: newer option implmented in Ruby that
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
