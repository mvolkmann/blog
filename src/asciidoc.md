---
eleventyNavigation:
  key: AsciiDoc
layout: topic-layout.njk
---

## Overview

- alternative to Markdown
- homepage is http://www.methods.co.nz/asciidoc/
- recommended file extension is `.txt`

## Installing

- option #1
  - brew install ruby
  - gem install asciidoctor

## AsciiDoctor

- Continuation of the AsciiDoc language and tools is happening
  in the Asciidoctor project.
- Asciidoctor is a modern implementation of AsciiDoc in Ruby
  that adheres as closely as possible to the syntax in AsciiDoc Python
  while offering numerous modern enhancements.
- Asciidoctor also offers a much broader ecosystem of
  extensions, tools, and integrations.

## Character Escaping

These characters must be escaped for literal versions: `+` and `{`.

## Previewing

To preview `.adoc` files in Chrome

- install "Asciidoctor.js Live Preview" at
  <https://chrome.google.com/webstore/search/asciidoctor>
- click the vertical ellipsis in the upper-right
- select More Tools ... Extensions
- find "Asciidoctor.js Live Preview" and click the "Details" button
- toggle "Allow access to file URLs" to on
- open a `.adoc` file in Chrome
- it should not be necessary to refresh
  to see results of changes in `.adoc` files

## dblatex

To install dblatex

- download source from http://sourceforge.net/projects/dblatex/?source=dlp
- unzip and cd to directory
- sudo python setup.py install

## Generating HTML

To generate HTML, enter `asciidoctor {name}.txt`.
This generates `{name}.html`.

## Generating Slides

To generate slidy slides

- `asciidoctor -b slidy {input-file}`

  This generates {name}.html, but it's in slidy format.

- new slides

  - level 1 & 2 headers start new slides

- slidy keys
  - left arrow goes to previous slide
  - right arrow or spacebar goes to next slide
    - can also mouse click anywhere on slide
  - s or < makes font smaller
  - b or > makes font bigger
  - c shows table of contents
  - f toggles display of status line
  - home goes to first slide
  - end goes to last slide
  - a toggles between showing all slides on one page
    and one slide at a time

## Generating PDFs

To generate a PDF:

- `brew install fop`
- `a2x --fop -f pdf --verbose {input-file}`

## Pandoc

[Pandoc](http://johnmacfarlane.net/pandoc/) converts various markup formats.
There is a Mac `.dmg` installer file.
Pandoc requires pdflatex to convert to PDF.
You may be able to get this from http://www.tug.org/mactex/2012/.
