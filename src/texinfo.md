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
To view an HTML preview, open a Texinfo file and click the preview button
in the upper-right (document with small magnifier glass).

To convert a Texinfo file to PDF from the command line,
enter one of the following commands:

```bash
texi2pdf name.texinfo
texi2dvi --pdf name.texinfo
```

## info Command

The `info` command enables viewing and navigating
the contents of a `.info` file.

The `info` command provides information on itself.
To view it, enter `info info`.

The following keyboard shortcuts are supported:

| Key         | Action                                |
| ----------- | ------------------------------------- |
| h           | open help on GNU info                 |
| H           | toggles display of help window        |
| q           | quit info, returning to command line  |
| space       | scroll down one page                  |
| backspace   | scroll up one page (delete key)       |
| arrows keys | move cursor in associated direction   |
| b           | go to beginning of curent node        |
| d           | go to directory node                  |
| e           | go to end of curent node              |
| g           | go to a node by name                  |
| i           | search for text in index              |
| l           | return to last visited node           |
| n           | go to next node at this level         |
| p           | go to previous node at this level     |
| t           | go to top node                        |
| u           | go up to parent node                  |
| [           | go to previous node in document order |
| ]           | go to next node in document order     |
| return      | go to menu item under cursor          |
| 1-9         | select item n in current menu         |
| /           | search forward for given text         |
| {           | search for previous text occurrence   |
| }           | search for next text occurrence       |
| ctrl-g      | cancel current operation              |

## Resources

- {% aTargetBlank "https://www.gnu.org/software/texinfo/manual/texinfo/texinfo.pdf",
  "Official texinfo Manual" %}
- {% aTargetBlank "https://learnxinyminutes.com/texinfo/",
  "Learn Texinfo in Y minutes" %}
