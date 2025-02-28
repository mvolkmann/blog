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

{% aTargetBlank "https://www.latex-project.org", "LaTeX" %} is a
document preparation system and markup language used for
creating high-quality documents, especially those containing
mathematical formulas, scientific notations, and structured content.

LaTeX is pronounced "Lay-tech" or "Lah-tech".
The "X" at the end is actually the Greek letter chi.
That is why it is not pronounced the same as "latex", the substance
that comes from trees and plants which is used to produce rubber.

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

## VS Code

VS Code can be used to edit `.tex` files.

Install the extension "LaTeX Workshop" from James Yu.
This automatically generates a PDF
every time changes to a `.tex` file are saved
and the PDF can be viewed inside VS Code.
It requires that a compatible LaTeX distribute be found in the `PATH`.
<a href="https://www.tug.org/texlive/" target="_blank">TeX Live</a>
is recommended. `mactex` is the macOS version of TeX Live.

## Generating PDFs from Command Line

- `pdflatex {name}.tex`
- `open {name}.pdf`

## Syntax

The most basic LaTeX document contains the following:

```latex
\documentclass{article}
\begin{document}
Hello
\end{document}
```

Other options for `\documentclass` include:

- `article` supports sections and subsections, but not chapters
- `beamer` for presentations
- `book` supports a title page, abstract, table of contents, chapters, and bibliography
- `exam` for lists of questions
- `leaflet`
- `letter`
- `memoir` based on the `book` class
- `minimal` only sets page size and a base font (mostly for debugging)
- `paper`
- `proc` for proceedings; based on the `article` class
- `report` for documents with chapters
- `slides` for presentations, but `beamer` is preferred

See the video tutorial at https://www.youtube.com/watch?v=ydOTMQC7np0!

## Comments

Single-line comments begin with `%` and extend to the end of the line.
For example:

```latex
% This is a single-line comment.
```

The "comment" package adds support for multi-line comments.
For example:

```latex
\usepackage{comment}
...
\begin{comment}
This is a
multi-line comment.
\end{comment}

## Greek Letters

Greek letters are produced using the following commands:

- `$\alpha$`
- `$\beta$`
- `$\gamma$`
- `$\delta$`
- `$\epsilon$`
- `$\zeta$`
- `$\eta$`
- `$\theta$`
- `$\iota$`
- `$\kappa$`
- `$\lambda$`
- `$\mu$`
- `$\nu$`
- `$\xi$`
- `$\pi$`
- `$\rho$`
- `$\sigma$`
- `$\tau$`
- `$\upsilon$`
- `$\phi$`
- `$\chi$`
- `$\psi$`
- `$\omega$`

This includes all the Greek letters except omicron,
which is not included because it is identical to a lowercase "o".

## Resources

- {% aTargetBlank "https://ctan.org/pkg/catalogue?lang=en", "CTAN" %}
  Comprehensive TeX Archive Network
```
