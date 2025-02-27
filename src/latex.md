---
eleventyNavigation:
  key: LaTeX
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

{% aTargetBlank "https://www.latex-project.org", "LaTeX" %} is

Consider using the VSCode extension "LaTeX Workshop" from James Yu.

## Installing in macOS

Install Homebrew and then enter `brew install mactex`.
This installs the following commands:

| Command     | Description                                                       |
| ----------- | ----------------------------------------------------------------- |
| `pdflatex`  | compiles .tex files into PDF format                               |
| `xelatex`   | like pdflatex, but supports system fonts and Unicode              |
| `lualatex`  | uses Lua for advanced typesetting and scripting                   |
| `latex`     | compiles .tex files into DVI (older format)                       |
| `dvips`     | converts DVI files to PostScript                                  |
| `dvipdfmx`  | converts DVI files to PDF                                         |
| `bibtex`    | processes .bib files for bibliography references                  |
| `biber`     | more powerful bibliography processor (alternative to bibtex)      |
| `makeindex` | generates an index for documents                                  |
| `tlmgr`     | TeX Live package manager (for installing/updating LaTeX packages) |
| `kpsewhich` | searches for installed TeX files                                  |
| `texhash`   | updates TeX's file database after installing packages             |
| `latexmk`   | automates LaTeX compilation (runs multiple passes as needed)      |

The most used commands are probably `pdflatex` and `xelatex`.

If using VS Code to edit `.tex` files,
install the extension "LaTeX" from Mathematic Inc.
This automatically generates a PDF
every time changes to a `.tex` file are saved
and the PDF can be viewed inside VS Code.

## Generating PDFs from Command Line

- `pdflatex {name}.tex`
- `open {name}.pdf`
