---
eleventyNavigation:
  key: AsciiDoc
layout: topic-layout.njk
---

## Overview

[AsciiDoc](http://asciidoc.org/) is an alternative to Markdown
that supports more features.

The recommended file extension for AsciiDoc files is `.adoc`.

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

## Character Escaping

These characters must be escaped for literal versions: `+` and `{`.

## Previewing

To preview `.adoc` files in VS Code:

- Install the AsciiDoc extension.
- Open an `.adoc` file.
- Click the "Open Preview" button in the upper-left.
  The icon looks like a two-column document
  with a magnifier glass in the lower-left.

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
