---
eleventyNavigation:
  key: LaTeX
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.latex-project.org", "LaTeX" %} is a
document preparation system and markup language used for
creating high-quality documents, especially those containing
mathematical formulas, scientific notations, and structured content.

LaTeX is pronounced "Lah-tech" or "Lay-tech".
The "X" at the end is actually the Greek letter chi.
That is why it is not pronounced the same as "latex", the substance
that comes from trees and plants which is used to produce rubber.

## Installing

Generating PDF documents from LaTeX documents
requires a distribution and an editor.

The distribution provides executables that must be found
in a directory listed in your `PATH` environment variable.
The <a href="https://www.tug.org/texlive/" target="_blank">TeX Live</a>
distribution is recommended.

### Installing in macOS

The macOS version of TeX Live is named mactex.
One way to install mactex is to install Homebrew
and then enter `brew install mactex`.
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

To generate a `.pdf` file from a `.tex` file in a terminal window,
run the following command:

```bash
pdflatex {name}.tex
open {name}.tex
```

TeX editors will do this for you.

## Editors

Recommended LaTeX editors include TeXmaker, VS Code, and Overleaf.

### TeXmaker

<a href="https://www.xm1math.net/texmaker/" target="_blank">TeXmaker</a>
is a free, cross-platform LaTeX editor.
Download the installer, run it, and drag the app icon to the Applications directory.
Double-click the app to run it.
It will likely show an error dialog that says
"texmaker.app is damaged and can't be opened", but it is not damaged.
To fix this, open a terminal window and enter the following command:

```bash
sudo xattr -dr com.apple.quarantine /Applications/texmaker.app
```

### VS Code

VS Code can be used to edit `.tex` files.

Install the extension "LaTeX Workshop" from James Yu.
This automatically generates a PDF
every time changes to a `.tex` file are saved
and the PDF can be viewed inside VS Code.

### Overleaf

<a href="https://www.overleaf.com" target="_blank">Overleaf</a>
is a web-based editor that doesn't require installing any software.
It requires creating an account.
There are free and paid accounts.
Free accounts have the following limitations:

- collaborating on documents with multiple editors is not supported
- compiling can timeout for large documents
- no document history is maintained
- advanced search is not supported
- the symbol palette for inserting math symbols with a click is not supported
- no integrations with version control systems is supported
- no technical support is provided

Paid accounts are $199 (10 collaborators per project)
or $399 (unlimited collaborators per project) per year.

## Syntax

The most basic LaTeX document contains the following:

```latex
\documentclass{article}
\begin{document}
Hello
\end{document}
```

LaTeX documents consist of a sequence of commands and content.
The commands before `\begin{document}` are referred to as the preamble.
They do the following:

- describe the class of document being created
- import packages which provide support for addition commands
- configure document-wide formatting

Commands have the syntax `\name[options]{content}`,
but some commands do not support options and/or content.

The first command must be `\documentclass{some-class}`.

The content must be surrounded by `\begin{document}` and `\end{document}`.

## Document Classes

Document classes change the default formatting and add support for new commands.

Options for `\documentclass{some-class}` include:

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
```

## Basic Formatting

```latex
\textbf{Bold}

\textit{Italic}

\underline{Underline}

\textbf{\textit{\underline{Bold, Italic, and Underline}}}
```

## Font Family

To change the font family for a section of the content,
surround it with:

TODO: TEST THESE!

- `textrm{ ... }` for a serif font (Roman)
- `textsf{ ... }` for a sans serif font
- `texttt{ ... }` for a typewriter (monospace) font

## Font Style

To change the font size for a section of content,
surround it with:

TODO: TEST THESE!

- `textmd{ ... }` for medium weight
- `textbf{ ... }` for bold face
- `textup{ ... }` for upright (not italic or slanted)
- `textit{ ... }` for italic
- `textsl{ ... }` for slanted
- `textsc{ ... }` for small caps

## Font Size

To change the font size of all the content,
TODO: ADD THIS!

To change the font size of a section of the content,
surround it with `{\size ... }` where size is one of
`tiny`, `scriptsize`, `footnotesize`, `small`, `normalsize`,
`large`, `Large`, `LARGE`, `huge`, or `Huge`.
TODO: Do these use a percentage of the current default font size?

For example:

```latex
{\huge
some content
}
```

A specific font size is specified with `\fontsize{size}{spacing}\selectfont`
where `size` the font point size and `spacing` the point size between lines.
For example:

TODO: THIS IS NOT WORKING!

```latex
\fontsize{18}{10}\selectfont
some content
\end
```

## Paragraphs

Paragraphs are separated by blank lines.
By default, the first line in each paragraph will be indented
and there will be no extra space separating the pargraphs.

To opt for not indenting the first line of each paragraph and
instead add vertical space between them, add `\usepackage(parskip)`.

To customize the first line indentation:

```latex
\setlength{\parindent}{1em}
```

To customize the vertical space between paragraphs:

```latex
\setlength{\parskip}{1em}
```

## Space

To add a single newline after a line, add `\\` at its end.

To add a page break, insert the command `\newpage` or `\pagebreak`.

To add a given amount of vertical space,
insert the command `\vspace{amount}` where `amount` is a value like `1cm`.

To push the remaining content to the bottom of the current page,
insert the command `\vfill`.
If this command appears multiple times on page,
the available space will be divided evenly between them.
For example:

```latex
\newpage
top
\vfill
middle
\vfill
bottom
```

To push a single line to the right side of the page,
insert the command `\hfill` at the begining of the line.

To split the content of a single line so the beginning is at
the left side of the page and the end is at the right side,
insert the command `\hfill` in the middle of the line.

To insert a given amount of horizontal space in a line,
insert the command `\hspace{amount}` where `amount` is a value like `1cm`.

## Justifying and Aligning

In many document classes such as `article`, `report`, and `book`,
paragraphs are justified by default so
each full line of text has the same length.
This is accomplished by adding variable spacing between the words.

To change the default alignment so variable spacing between words is not used,
add `\raggedright` or `\raggedleft`.

When this is not the default, to justify multiple lines of text, use:

```latex
{\justify
This is a justified paragraph\\
that spans multiple lines.\\
Each line is justified.
}
```

To center a single line of text, use:

```latex
\centerline{some-text}
```

To center multiple lines of text instead of justifying them, use:

```latex
{\centering
This is a centered paragraph\\
that spans multiple lines.\\
Each line is centered.
\par}
```

OR

```latex
\begin{center}
This is a centered paragraph\\
that spans multiple lines.\\
Each line is centered.
\end{center}
```

To right-align multiple lines of text instead of justifying them, use:

```latex
\begin{flushright}
This is a right-aligned paragraph\\
that spans multiple lines.\\
Each line is centered.
\end{flushright}
```

## Lists

Bulleted lists are created with `\item` commands
inside the "itemize" environment. For example:

<img alt="LaTeX itemize list" style="width: 10%"
  src="/blog/assets/LaTeX-itemize-list.png?v={{pkg.version}}">

```latex
\begin{itemize}
  \item red
  \item green
  \item blue
\end{itemize}
```

Numbered lists are created with `\item` commands
inside the "enumerate" environment. For example:

<img alt="LaTeX enumerate list" style="width: 10%"
  src="/blog/assets/LaTeX-enumerate-list.png?v={{pkg.version}}">

```latex
\begin{enumerate}
  \item red
  \item green
  \item blue
\end{enumerate}
```

Description lists are created with `\item` commands
inside the "description" environment. For example:

<img alt="LaTeX description list" style="width: 25%"
  src="/blog/assets/LaTeX-description-list.png?v={{pkg.version}}">

```latex
\begin{description}
  \item[apple] a red fruit
  \item[kiwi] a green fruit
  \item [blueberry] a blue fruit
\end{description}
```

Lists can be nested up to four levels deep.
For example:

<img alt="LaTeX nested lists" style="width: 25%"
  src="/blog/assets/LaTeX-nested-lists.png?v={{pkg.version}}">

```latex
\begin{itemize}
  \item red
    \begin{enumerate}
      \item apple
      \item cherry
      \item strawberry
    \end{enumerate}
  \item green
    \begin{enumerate}
      \item kiwi
      \item lime
    \end{enumerate}
  \item blue
    \begin{enumerate}
      \item blueberry
    \end{enumerate}
\end{itemize}
```

## Sections

Documents can have up to seven levels of sections,
but not all of them are supported for every document class.
The following example shows how to specify all seven levels.

```latex
\part{First Part}
This is a paragraph in a part.

\chapter{First Chapter}
This is a paragraph in a chapter.

\section{First Section}
This is a paragraph in a section.

\subsection{First subsection}
This is a paragraph in a subsection.

\subsubsection{First Subsubsection}
This is a paragraph in a subsubsection.

\paragraph{First Paragraph}
This is a paragraph in a paragraph.

\subparagraph{First Subparagraph}
This is a paragraph in a subparagraph.
```

Unless the document is a book, the most common kinds of sections to use
include `\section`, `\subsection`, and `\subsubsection`.

Parts, chapters, sections, and subsections are
automatically assigned increasing numbers starting from 1.
Subsubsecdtions, paragraphs, and subparagraphs are not assigned numbers.

## Horizontal Rules

To draw a horizontal line across the page, use `\hrule`.

## Tables

```latex
\begin{tabular}{|l|l|c|}
  \hline
  Name & Breed & Age \\
  \hline\hline
  Comet & Whippet & 4 \\
  \hline
  Greta & German Shorthaired Poiner & 1 \\
  \hline
  Oscar & Lab mix & 7 \\
  \hline
  Ramsay & Native American Indian Dog & 8 \\
  \hline
\end{tabular}
```

## Greek Letters

Greek letters are produced using the following commands.
There are no commands for Greek letters that are
identical to the corresponding Latin letter,
Latin letters are used for those.

<img alt="LaTeX Greek letters" style="width: 30%"
  src="/blog/assets/LaTeX-greek-letters.png?v={{pkg.version}}">

| Name    | Lowercase    | Uppercase   |
| ------- | ------------ | ----------- |
| alpha   | `$\alpha$`   | `A`         |
| beta    | `$\beta$`    | `B`         |
| gamma   | `$\gamma$`   | `$\Gamma$`  |
| delta   | `$\delta$`   | `$\Delta$`  |
| epsilon | `$\epsilon$` | `E`         |
| zeta    | `$\zeta$`    | `Z`         |
| eta     | `$\eta$`     | `H`         |
| theta   | `$\theta$`   | `$\Theta$`  |
| iota    | `$\iota$`    | `I`         |
| kappa   | `$\kappa$`   | `K`         |
| lambda  | `$\lambda$`  | `$\Lambda$` |
| mu      | `$\mu$`      | `M`         |
| nu      | `$\nu$`      | `N`         |
| xi      | `$\xi$`      | `$\Xi$`     |
| omicron | `o`          | `O`         |
| pi      | `$\pi$`      | `$\Pi$`     |
| rho     | `$\rho$`     | `P`         |
| sigma   | `$\sigma$`   | `$\Sigma$`  |
| tau     | `$\tau$`     | `T`         |
| upsilon | `$\upsilon$` | `$\Upsilon` |
| phi     | `$\phi$`     | `$\Phi$`    |
| chi     | `$\chi$`     | `X`         |
| psi     | `$\psi$`     | `$\Psi$`    |
| omega   | `$\omega$`   | `$\Omega$`  |

## Formatting the Word LaTeX

The word LaTeX is specially formatted by surrounding it with backslashes.
For example, `\LaTeX\` is rendered as follows:

<img alt="LaTeX rendered" style="width: 10%"
  src="/blog/assets/LaTeX-rendered.png?v={{pkg.version}}">

## Resources

- {% aTargetBlank "https://ctan.org/pkg/catalogue?lang=en", "CTAN" %}
  Comprehensive TeX Archive Network
