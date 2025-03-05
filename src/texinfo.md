---
eleventyNavigation:
  key: Texinfo
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

{% aTargetBlank "https://www.gnu.org/software/texinfo/", "Texinfo" %}
is ...

Text files containing Texinfo markup have a file extension of
`.texi`, `.texinfo`, `.txi`, or `.tex`, but using `.tex` is discouraged
because files for TeX and LaTeX use that file extension.
All the examples in this document will use `.texinfo`.

## Minimum Markup

All Texinfo files must begin and end with the following lines:

```text
\input textinfo
...
@bye
```

## Tooling

To install commands for working with Texinfo files in macOS using Homebrew,
enter `brew install texinfo`.
This installs the following commands:

- `info`: This renders `.info` files.
- `makeinfo`: This translates Texinfo source documentation to other formats
  including DocBook, DVI, EPUB 3, HTML, info, LaTeX, plain text, and XML.
  By default it produces `.info` files that can be
  rendered in Emacs or by the GNU info command.
- `texi2any` - same as `makeinfo`
- `texi2dvi` - This converts Texinfo files to DVI or PDF formats.
- `texi2pdf` - same as `texi2dvi`

If using VS Code to edit Texinfo files, consider installing
the extension "Texinfo Language Support" from CismonX.
This adds syntax highlighting, code completion, HTML preview,
block folding, breadcrumb navigation, and diagnostics.

To convert a Texinfo file to PDF from the command line,
enter one of the following commands:

```bash
texi2pdf name.texinfo
texi2dvi --pdf name.texinfo
```

## Resources

- {% aTargetBlank "https://www.gnu.org/software/texinfo/manual/texinfo/texinfo.pdf",
  "Official texinfo Manual" %}

```

```
