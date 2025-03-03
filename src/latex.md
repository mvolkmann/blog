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

TeX is a low-level typesetting system created by Donald Knuth
in 1978 at Stanford University.
The "X" at the end is actually the Greek letter chi.
That is why TeX is pronounced "tech" and not "tex".

{% aTargetBlank "https://www.latex-project.org", "LaTeX" %} is a
high-level typesetting system built on TeX.
It was created by Leslie Lamport in 1984.
LaTeX is widely used in academia for publication of
techical articles, papers, and books.

LaTeX is pronounced "Lah-tech" or "Lay-tech".
It is not pronounced the same as "latex", the substance
that comes from trees and plants which is used to produce rubber.

Both TeX and LaTeX are markup languages.
Other commonly used markup languages include HTML, Markdown, SVG, and XML.

## Pros and Cons

Some pros of using LaTeX include:

- provides high-quality typesetting for professional looking PDF documents,
  especially those that contain mathematical equations
- allows users to focus on content while maintaining consistent formatting,
  such as automatic numbering of pages, chapters, sections, figures, and tables
- supports extensive customization through thousands of packages
  and templates for various document types
- supports generating a table of contents, bibligraphy (with references),
  index, and footnotes
- scales to support large projects like books and theses
  by allowing documents to be split into multiple files
- supports defining parameterized macros that reduce duplicated code
- works in Windows, macOS, and Linux
- free and open source
- being text-based enables easy viewing of diffs in version control systems

Some cons of using LaTeX include:

- steep learning curve for its markup syntax
- customizing layouts and formatting can be time-consuming
- markup syntax must be compiled in order to view
  what will be rendered in the output PDF
- error messages can by cryptic
- creating tables and managing placement of images is cumbersome
  compared to many word processing applications
- collaboration requires all contributes to have knowledge of LaTeX syntax

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

| Command     | Description                                                            |
| ----------- | ---------------------------------------------------------------------- |
| `pdflatex`  | compiles .tex files into PDF format                                    |
| `xelatex`   | like pdflatex, but supports TrueType font, OpenType fonts, and Unicode |
| `lualatex`  | like xelatex, but supports Lua scripting                               |
| `latex`     | compiles .tex files into DVI (older format)                            |
| `dvips`     | converts DVI files to PostScript                                       |
| `dvipdfmx`  | converts DVI files to PDF                                              |
| `bibtex`    | processes .bib files for bibliography references                       |
| `biber`     | more powerful bibliography processor (alternative to bibtex)           |
| `makeindex` | generates an index for documents                                       |
| `tlmgr`     | TeX Live package manager (for installing/updating LaTeX packages)      |
| `kpsewhich` | searches for installed TeX files                                       |
| `texhash`   | updates TeX's file database after installing packages                  |
| `latexmk`   | automates LaTeX compilation (runs multiple passes as needed)           |

The most used commands are probably `pdflatex` and `xelatex`.

To generate a `.pdf` file from a `.tex` file in a terminal window,
run one of the following commands:

```bash
pdflatex {name}.tex
xelatex {name}.tex
lualatex {name}.tex
```

Open the generated PDF document with the following command:

```bash
open {name}.pdf
```

TeX editors will handle both of these steps for you.

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

The `\begin` command starts a new "environment"
and must be paired with a corresponding `end` command.
The name of the environment is specified as
an argument in a pair of curly braces.
The content of all documents must be contained in a document environment.
Some environments take additional arguments
that are specified in additional pairs of curly braces.

LaTeX documents consist of a sequence of commands and content.
The commands before `\begin{document}` are referred to as the preamble
and must begin with `\documentclass{some-class}`.
These commands:

- describe the class of document being created
- import packages which provide support for additional commands
- configure document-wide formatting

All commands begin with a backslash and a name.
Command names are case-sensitive and consist of letters,
not numbers or other special characters.

Some commands are followed by required arguments
where each is contained in its own pair of curly braces.

Some commands take optional arguments
where each is contained in its own pair of square brackets.
But sometimes all the optional arguments appear in a comma-separated list
inside a single pair of square brackets.
Usually the optional arguments appear before the required arguments,
but sometimes they appear after them.

The entire content of a document must be surrounded by
`\begin{document}` and `\end{document}`.

A command group is defined by a pair of curly braces
and limits the scope of a command
so it only affects the content inside the group.
For example, the following causes all the text rendered inside the group
to in a huge font.

```latex
{\huge
  ...
}
```

Many commands support a "star variant" that affects its behavior.
For example, `\chapter{some name}` starts a new, numbered chapter
and `\chapter*{some name}` starts a new, unnumbered chapter.

A `.tex` file can include other `.tex` files.
This enables breaking a large document into smaller documents
that can be edited independently, such as each chapter of a book.
The documents being included should not contain a preamble section
or the `\begin{document}` and `\end{document}` commands.
For example:

```latex
\include{other-file-name}
```

This will include the contents of the file named `other-file-name.tex`
found in the same directory as the file that contains the `\include` command.

## Document Classes

Document classes change the default formatting and add support for new commands.

Options for `\documentclass{some-class}` include:

- `article` supports sections and subsections, but not chapters
- `beamer` for slide presentations
- `book` supports a title page, abstract, table of contents, chapters, and bibliography
- `exam` for lists of questions
- `leaflet`
- `letter`
- `memoir` based on the `book` class
- `minimal` only sets page size and a base font (mostly for debugging)
- `paper`
- `proc` for proceedings; based on the `article` class
- `report` for documents with chapters
- `slides` for slide presentations, but `beamer` is preferred

See the video tutorial at https://www.youtube.com/watch?v=ydOTMQC7np0!

## Packages

Packages can add support for additional commands.
They can also modify default settings that affect how documents are rendered.

To import a package, use the `\usepackage` command.
This can take a set of optional arguments in square brackets.
It also takes a comma-separated list of package names in curly braces.
For example:

```latex
\usepackage{amsfonts, amsmath, amsthm, amssymb}
\usepackage{float, graphicx}
\usepackage{hyperref}
```

### Popular Packages

Documentation on all LaTeX packages can be found at
<a href="https://ctan.org/pkg" target="_blank">
Comprehensive TEX Archive Network</a> (CTAN).

In packages whose names begin with "ams", that stands for
<a href="https://www.ams.org/home/page" target="_blank">
American Mathematical Society</a>.

| Package  | Description                                                                      |
| -------- | -------------------------------------------------------------------------------- |
| amsfonts | adds fonts for use in mathematics                                                |
| amsmath  | adds commands for rendering mathematical formulas                                |
| amssymb  | adds commands for additional mathematical symbols                                |
| comment  | adds support for multi-line comments                                             |
| fancyhdr | adds commands to configure page headers and footers                              |
| float    | improves the ability to control the placement of objects like figures and tables |
| geometry | adjusts page margins, page size, and layout                                      |
| graphicx | builds on the graphic package to enhance support for graphics                    |
| hyperref | adds commands to create clickable hyperlinks                                     |
| inputenc | adds support for various input encodings like utf8                               |
| lipsum   | generates Lorem Ipsum text for testing                                           |
| listings | adds commands to typeset programming language source code                        |
| xcolor   | adds commands to change the color of text                                        |

## Portrait vs. Landscape

By default all pages will be in portrait mode.
To cause all pages to use landscape mode, use the geometry package.
For example:

```latex
\usepackage[letterpaper,landscape]{geometry}
```

To cause a specific page to use landscape mode, use the lscape package.
For example:

```latex
\usepackage{lscape}
...
\begin{document}
...
\begin{landscape}
...
\end{landscape}
...
\end{document}
```

## Page Numbers

Many document classes such as `article`, `book`, and `report`
include page numbers by default.
To suppress the page numbers,
add one of the following commands in the preamble:

```latex
\pagestyle{empty}
\pagenumbering{gobble}
```

The `\pagenumbering` command can also specify the following values
to use a particular kind of page numbering:

- `arabic`: 1, 2, 3, ...
- `alph`: a, b, c, ...
- `Alph`: A, B, C, ...
- `roman`: i, ii, iii, ...
- `Roman`: I, II, III, ...

Each chapter or section can change the page numbering style
and restart page numbering from one.
In the following example,
the preface page numbers use lowercase roman numerals and
the chapters that follow use Arabic numbers.

The preface chapter is defined with the `\chapter*` below
to avoid numbering it and make the cause first chapter
defined with the `\chapter` command be considered the first chapter.

```latex
\newpage
\chapter*{Preface}
\pagenumbering{roman}

preface content goes here

\newpage
\chapter{Getting Started}
\pagenumbering{arabic}

getting started content goes here
```

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

## Fonts

### Font Family

To change the font family for a section of the content,
surround it with:

- `textrm{ ... }` for a serif font (Roman)
- `textsf{ ... }` for a sans serif font
- `texttt{ ... }` for a typewriter (monospace) font

### Font Style

To change the font style for a section of content,
surround with commands shown in the following table.
The style "upright" seems to be same as "medium".

<img alt="LaTeX font styles" style="width: 30%"
  src="/blog/assets/latex-font-styles.png?v={{pkg.version}}">

### Font Size

To change the default font size for the entire document,
modify the `\documentclass` command as follows:

```latex
\documentclass[12pt]{book}
```

For some reason the only font sizes that can be specified here
are `10pt`, `11pt`, and `12pt`.

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

A specific font size is specified with `\fontsize{size}{baselineskip}`
where `size` the font point size and `baselineskip` is
the distance between the baselines of each line of text.
The `baselineskip` value will be ignored unless each paragraph,
including the last, is followed by a blank line.

For example:

```latex
{\fontsize{18pt}{24pt}\selectfont
Come and listen to a story about a man named Jed
A poor mountaineer, barely kept his family fed,
Then one day he was shootin at some food
And up through the ground came a bubblin crude.

Oil that is, black gold, Texas tea.

Well the first thing you know ol Jed's a millionaire,
Kinfolk said "Jed move away from there"
Said "Californy is the place you ought to be"
So they loaded up the truck and moved to Beverly.

}
```

## TrueType and OpenType Fonts

The shell command `pdflatex` only supports using Type1 and bitmap fonts,
not TrueType or OpenType fonts.
The shell cvommands `xelatex` and `lualatex`
both support TrueType and OpenType fonts.

To use those kinds of fonts:

1. User the `fontspec` package.

   ```latex
   \usepackage{fontspec}
   ```

1. Specify the font to use with a relative file path
   or the name of an installed font.

   ```latex
   \setmainfont{Pangolin-Regular.ttf} % relative file path.
   \setmainfont{Apple Chancery} % installed font name
   ```

1. Use `xelatex` or `lualatex` instead of `pdflatex` to generate a PDF document.

   ```bash
   xelatex some-name.tex
   ```

## Colors

The `color` package provides basic support the changing text color.

```latex
\usepackage{color}
...
% Change text color within a group
{\color{red} This text will be red.}
% Or with a command
\textcolor{blue}{This text will be blue.}
```

The `xcolor` package provides more features and color models
than the `color` package and is recommended.

The following color models are supported:
cmy, cmyk, gray, Gray, HSB, hsb, HTML, natural, rgb, and RGB.

The following commands provide several examples
of rendering text in a specific color:

{% raw %}

```latex
\usepackage[dvipsnames]{xcolor} % dvipsnames gives 68 more predefined colors.
...

% Using a color name from a small set.
\textcolor{red}{This is red.}

% rgb color
\textcolor[rgb]{0.5, 0.1, 0.9}{Custom purple text}

% HTML color
\textcolor[HTML]{00FF00}{Hex green}

% cmyk color
\textcolor[cmyk]{1,0,0,0}{CMYK cyan}

% Defining a name for a custom color and using it.
\definecolor{darkblue}{RGB}{0,0,102}
\textcolor{darkblue}{This is dark blue text}

% Defining a command for a custom color and using it.
\newcommand{\important}[1]{\textcolor{red}{#1}} % in preamble section
This is a \important{serious issue!}.

% Setting the default color for all the text remaining in a block.
\begin{center}
  \color{purple}
  one
  two
  three
\end{center}

% Rendering text in a box with a colored background.
\colorbox{yellow}{Text with yellow background}

% Rendering text in a box with a colored border and a colored background.
\fcolorbox{red}{lightgray}{Red-bordered box with gray background and black text}
```

{% endraw %}

For more colors, see
<a href="https://latexcolor.com" target="_blank">LaTeX Color</a>.

## Paragraphs

Paragraphs are separated by blank lines which introduce "hard returns".

By default, the first line in each paragraph will be indented
(except for the first paragraph in each chapter or section).
And there will be no extra space separating the pargraphs,
despite having a blank line between them in the `.tex` file.

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

The LaTeX compiler typically removes extra spaces as it sees fit.
To force a space to be retained, use `\,`.

To add a single newline after a line, add `\\` at its end,
which introduces a "soft return".

To add a page break, insert the command `\newpage` or `\pagebreak`.

To add a given amount of vertical space,
insert the command `\vspace{amount}` where `amount` is a value like `1cm`.
Alternatively, insert the commands
`\smallskip` (3pt +/- 1pt),
`\medskip` (6pt +/- 2pt), or
`\bigskip` (12pt +/- 4pt).

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
  src="/blog/assets/latex-itemize-list.png?v={{pkg.version}}">

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
  src="/blog/assets/latex-enumerate-list.png?v={{pkg.version}}">

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
  src="/blog/assets/latex-description-list.png?v={{pkg.version}}">

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
  src="/blog/assets/latex-nested-lists.png?v={{pkg.version}}">

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

## Code Listings

The `listings` package renders programming code source code.
It can render code in many programming languages including
Bash, C, C++, Go, Haskell, Java, JavaScript, Lisp, Lua, Matlab,
Perl, PHP, Python, R, Ruby, Rust, Scheme, SQL, and Swift.
It can also render many markup languages including
CSS, HTML, JSON, LaTeXTeX, XML, and YAML.

See the `\lstdefinelanguage` command that adds support for more languages.

For example:

<img alt="LaTeX listings" style="width: 80%"
  src="/blog/assets/latex-listings.png?v={{pkg.version}}">

```latex
\usepackage{listings}
\usepackage[dvipsnames]{xcolor} % dvipsnames gives 68 more predefined colors.
...
\lstset{backgroundcolor=\color{Apricot}, numbers=left}
\begin{lstlisting}[caption={Hello World code}, frame=single, frameround=tttt, language=Python]
  def hello():
      print("Hello, World!")

  hello()
\end{lstlisting}
```

## Sections

Documents can have up to seven levels of sections,
but not all of them are supported for every document class.
For example, the `\chapter` command can be used in the document class `book`,
but not in the document class `article`.

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
Subsubsections, paragraphs, and subparagraphs are not assigned numbers.

To suppress numbering of a chapter, section, or subsection,
include an asterisk at the end of its command name.
This is commonly do for sections like a preface.
When a table of contents is being generated,
unnumbered chapters and section not appear in the table of contents.

## Horizontal Rules

To draw a horizontal line across the page, use `\hrule`.

## URLs and Links

To render a URL in a monospace font, use the `\texttt` command.
For example, `\texttt{https://mvolkmann.github.io/blog/}`.
The URL will not be clickable.

To render a clickable URL, include the hyperref package
and use the `\url` or `\href` command.
The `\url` command renders the URL.
The `\href` command renders alternate text instead of the URL.
For example:

```latex
\usepackage{hyperref}
...
\url{https://mvolkmann.github.io/blog/}

\href{https://mvolkmann.github.io/blog/}{My Blog}
```

## Verbatim Text

Text can be rendered verbatim to avoid
interpreting anything in it as LaTeX commands.

For short text, use the `\verb` command
with the text delimited by vertical bars (pipes).
For example:

```latex
\verb|This text will be rendered verbatim|
```

For long text, use a `verbatim` environment.
For example:

```latex
\begin{verbatim}
This text will be rendered verbatim.
\end{verbatim}
```

## Images

To include images, use the `graphicx` package.
Use the `\includegraphics` command, specifying a file name.
It is not necessary to include the file extension.
The image file must reside in the same directory as the `.tex` file.
The supported image formats include JPEG (.jpg or .jpeg),
PNG (.png), and PDF (.pdf).

The `float` package is required in order to use the "H" option
which keeps the graphic "here" meaning where it occurs in the document flow.
Without that the compiler can choose another location for the image
that it deems better.
Using the "t" option moves the image to the top of the page and
using the "b" option moves it to the bottom of the page.

Surrounding the image with a "figure" enables adding a caption
which will be automatically numbered along with other figures
and can be placed above or below the image.

For example:

```latex
\usepackage{float, graphicx}
...
% non-figure image - caption specified with an argument
\includegraphics[width=3in]{Smalltalk-balloon}{Smalltalk Programming}
...
% figure image - caption specified in its own command
\begin{figure}[H]
  \centering % horizontally centers image (caption always is centered)
  \includegraphics[width=3in]{Smalltalk-balloon}
  \caption{Smalltalk Programming}
\end{figure}
```

In the example above we specified the image width.
Any supported unit of measure can be used.
These include `cm` (centimeters), `em` (width of M), `ex` (height of x),
`in` (inches), and `pt` (points).

To scale an image so its width matches that of the current text area,
use `\textwidth` for the `width` value.
This can be preceded by a percentage.
For example, `[width=0.7\textwidth]`.

To scale an image so its width matches that of the current line,
use `\linewidth` for the `width` value.

Instead of specifying a `width`, a `height` (a measure like `width`)
or `scale` (a number treated as a percentage) can be specified.

## Math

Mathematical equations are specially formatted when in math mode.
This includes making variables names italicized and
properly formatting fractions, subscripts, supercripts, and more.

There are two kinds of math mode, inline and display.
Inline math mode renders mathematical text inline with other content.
Display math mode renders mathematical text on its own line,
separated from surrounding content and horizontally centered by default.

To use inline math mode, surround content by single dollar signs.
For example:

<img alt="LaTeX inline math mode" style="width: 70%"
  src="/blog/assets/latex-inline-math-mode.png?v={{pkg.version}}">

{% raw %}

```latex
The Pythagorean theorem states that
for a triangle with sides of length $a$ and $b$
and hypotenuse of length $c$, $a^2 + b^2 = c^2$.
```

Fractions are rendered using the `\frac` (small) and `\dfrac` (large) commands.
When in display mode (inside double dollar signs), both are rendered large,
so `\dfrac` is only needed in inline math mode.
For both commands, the numerator and denominator
are specified in their own pair of curly braces.

For example:

<img alt="LaTeX inline fractions" style="width: 60%"
  src="/blog/assets/latex-inline-fractions.png?v={{pkg.version}}">

```latex
Inline fractions can be small like $\frac{x}{y}$ or large like $\dfrac{x}{y}$.
```

To use display math mode, surround content by double dollar signs.

<img alt="LaTeX display math mode" style="width: 70%"
  src="/blog/assets/latex-display-math-mode.png?v={{pkg.version}}">

```latex
The Pythagorean theorem states that
for a triangle with sides of length $a$ and $b$
and hypotenuse of length $c$, $a^2 + b^2 = c^2$.

The following is the formula calculates the velocity $v$
that an object will be travelling after falling over time $t$
given acceleration due to gravity of $g$ (9.8 $m/s^2$ on Earth).

$$ v = \frac{1}{2} g t^2 $$
```

A vertically centered dot represents multiplication and can add clarity.
In the expression $ 2xy^2 $ it is clear that
$2$, $x$, and $y^2$ are to be multiplied.
In this case adding dots between the terms doesn't add clarity.
On the other hand, $23$ represents a single number, not multiplying $2$ and $3$.
In this case it is appropriate to write \verb|$2 \cdot 3|
which is rendered as follows:

<img alt="LaTeX cdot command" style="width: 6%"
  src="/blog/assets/latex-cdot-command.png?v={{pkg.version}}">

All roots, square and otherwise, are rendered with the `\sqrt` command.
For example:

<img alt="LaTeX math roots" style="width: 25%"
  src="/blog/assets/latex-math-roots.png?v={{pkg.version}}">

```latex
$$ \sqrt{25} = 5 $$
$$ \sqrt[3]{8} = 2 $$
$$ x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} $$
```

The following are additional examples of using math mode:

<img alt="LaTeX more math mode" style="width: 18%"
  src="/blog/assets/latex-more-math-mode.png?v={{pkg.version}}">

```latex
$$ \frac{a}{b + \frac{c}{d}} $$ % nested fractions
$$ 3x^{2} $$ % single-character exponent
$$ 3x^{12} $$ % exponent must be in curly braces if more than one character
$$ 3x^{2x - 4} $$ % more complex exponent
$$ 3x^{y^z} $$ % multiple levels of exponents
$$ 3x^{2x^5 - 4} $$ % more complex multiple levels of exponents
$$ y_1 $$ % single-character subscript
$$ y_{12} $$ % multiple-character subscript
$$ y_{1_2} $$ % multiple levels of subscripts
$$ x_0, x_1, \ldots, x_n $$ % sequence of subscripted variables with ellipsis
```

The `\left` and `\right` commands are used to make grouping characters
like parentheses, square brackets, curly braces, and vertical bars
have a height that matches their content.
The following example demonstrates what is rendered
without and with those commands.

<img alt="LaTeX \left and \right commands" style="width: 15%"
  src="/blog/assets/latex-left-right-commands.png?v={{pkg.version}}">

```latex
$$ a (\frac{b + 1}{c + 2}) $$
$$ a \left(\frac{b + 1}{c + 2}\right) $$
```

To align equal signs when showing the solution to an equation,
use the `amsmath` package, wrap the steps in an `align` environment,
preceded each `=` with `&`, and end each line with `\\`.
The steps will be numbered by default.
To prevent numbering, use `align*` instead of `align`.

For example:

<img alt="LaTeX equation equals align" style="width: 50%"
  src="/blog/assets/latex-equation-equals-align.png?v={{pkg.version}}">

```latex
\begin{align}
2x^2 &= 13x - 6 \\
2x^2 - 13x + 6 &= 0 \\
(2x - 1)(x - 6) &= 0 \\
x &= 1/2 \,or\, 6
\end{align}
```

The following examples demonstrate rendering limits, sums, and integrals:

<img alt="LaTeX limits, sums, and integrals" style="width: 25%"
  src="/blog/assets/latex-lim-sum-int.png?v={{pkg.version}}">

```latex
$$ \lim_{x \to \infty} \frac{1}{x^r} = 0, r \in \mathbb{Z^+} $$
$$ \sum_{n=1}^\infty \frac{1}{n^2} = \frac{\pi^2}{6} $$
$$ \int_0^\infty e^{-x^2} dx = \sqrt{\pi} $$
```

{% endraw %}

## Dots

Dots are the ellipsis character rendered in different orientations.
The dots commands must be used in math mode.
Some of the dots commands require the amsmath package.

To display an ellipsis, insert the `\ldots` command.

The following examples demonstrate the dot commands:

<img alt="LaTeX dots" style="width: 15%"
  src="/blog/assets/latex-dots.png?v={{pkg.version}}">

```latex
$ 1, 2, \dots, 10 $ \\ % horizontal dots
$ 2, 3, \ldots, 10 $ \\ % lower horizontal dots
$ 3 \cdots 10 $ \\ % vertically centered horizontal dots
$ 4~ \vdots ~10 $ \\ % vertical dots; ~ ("tie") and is like &nbsp; in HTML
$ 5 \ddots 10 $ \\ % diagonal dots
```

## Tables

To create a table, use the `\begin{tabular}{columns}` command.
"columns" is replaced by text that specifies:

- the number of columns
- whether they should be left-aligned (`l`), centered (`c`), or right-aligned (`r`)
- whether there should be vertical borders
  before the columns, between the columns, and after the columns

For example, `\begin{tabular}{|c|lr|}` creates a table with three columns.
The first column is centered, the second is left-aligned, and the last is right-aligned.
There will be vertical lines before the first and second columns,
and after the last column.

The table rows are specified with content that follows
up to the `\end{tabular}` command.

The cells of each row are separated by the `&` character.
The end of each row is marked by `\\`.

To add lines before and/or after a row, add the `\hline` command.
To add double lines, such as below the heading row,
add two `\hline` commands.

The following example creates a table describing dogs.

<img alt="LaTeX table" style="width: 50%"
  src="/blog/assets/latex-table.png?v={{pkg.version}}">

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

The LaTeX compiler can place a table somewhere else in the content flow
if it decides that it fits better in another location.
To prevent this, use the `float` package,
wrap the `tabular` section in a `table` section, and add the option
`h` (place here if possible) or `H` (absolutely place here).

The command `\def\arraystretch` adds padding to table cells.

The `\caption` command adds a caption above or below a table
based on its placement in the flow.
The caption will be automatically numbered by default.
To prevent numbering, use the `caption` package and the `\caption*` command.

For example:

<img alt="LaTeX tabular in table" style="width: 50%"
  src="/blog/assets/latex-tabular-in-table.png?v={{pkg.version}}">

```latex
\usepackage{float}
...
\begin{table}[H] % absolutely positions table here
  \centering % centers table on page
  \def\arraystretch{2} % adds padding inside cells
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
  \caption{Dogs in my family}
\end{table}
```

Text in a table cell can contain paragraphs of text that wrap to multiple lines.
For example:

<img alt="LaTeX table with paragraphs" style="width: 70%"
  src="/blog/assets/latex-table-paragraphs.png?v={{pkg.version}}">

```latex
\begin{table}[H]
  \def\arraystretch{1.5} % adds padding inside cells
  \begin{tabular}{|l|p{3in}|} % note use of p with a specified width
    \hline
    Sport & Definition \\
    \hline\hline
    ice hockey & a game played on ice between two teams of six skaters each, the object being to score goals by shooting a puck into the opponents' cage using a stick with a wooden blade set at an obtuse angle to the shaft. \\
    \hline
    basketball & a game played by two teams of usually five players each on a rectangular court having a raised basket or goal at each end, points being scored by tossing the ball through the opponent's basket. \\
    \hline
  \end{tabular}
\end{table}
```

The table below demonstrates the following:

- cells that span multiple columns
- cells that span multiple rows
- cells that span both multiple columns and multiple rows

The `\multicolumn` command does not require an additional package.

The `\multirow` command is defined in the `multirow` package.

The `\cellcolor` command is defined in the `colortbl` package
and requires the `xcolor` package.
When the `\cellcolor` command is applied to a cell that spans more than one row,
only the first row is affected. Is this a bug?

<img alt="LaTeX table with spans" style="width: 30%"
  src="/blog/assets/latex-table-with-spans.png?v={{pkg.version}}">

```latex
\usepackage{colortbl, multirow, xcolor}
...
\def\arraystretch{1.5} % adds padding inside cells
\begin{tabular}{|c|c|c|c|}
  \hline
  & A & B & C \\
  \hline\hline
  1 & \cellcolor{yellow} A1 & B1 & C1 \\
  \hline

  2 & A2 & \multicolumn{2}{c|}{B2 \& C2} \\
  \hline

  3 & \multirow{2}{*}{\cellcolor{yellow} A3 \& A4} & B3 & C3 \\
  \cline{1-1} \cline{3-4} % Draw cell lines below cells 1, 3, and 4.

  4 & & B4 & C4 \\
  \hline

  5 & A5 & \multicolumn{2}{c|}{
    \multirow{2}{1.5cm}{
      \cellcolor{yellow} B5, C5, B6, \& C6
    }
  } \\
  \cline{1-2} % Draw cell lines below cells 1 and 2.

  6 & A6 & \multicolumn{2}{c|}{} \\
  \hline
\end{tabular}
```

## Greek Letters

Greek letters are produced using the following commands.
There are no commands for Greek letters that are
identical to the corresponding Latin letter,
Latin letters are used for those.

<img alt="LaTeX Greek letters" style="width: 30%"
  src="/blog/assets/latex-greek-letters.png?v={{pkg.version}}">

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

## Symbols

Some of the supported symbols are shown in the table below:

<img alt="LaTeX symbols" style="width: 65%"
  src="/blog/assets/latex-symbols.png?v={{pkg.version}}">

For more, see
<a href="https://artofproblemsolving.com/wiki/index.php/LaTeX:Symbols"
target="_blank">LaTeX:Symbols</a>.

TODO: Are more symbols available if you use the package `amssymb`?

## Math Mode

Math mode is used to display mathematical equations.

Inline math mode content is delimited by single `$` characters.
Display math mode content which appears on its on line, centered by default,
is delimited by double `$$` characters.

## Macros

Macros make it unnecessary to repeat
commonly used content and sequences of commands.

The `\def` command is a TeX primitive.

The `\newcommand` command is a LaTeX command that uses `\def`.
It adds checking whether a command being defined already exists.
It also adds support for optional arguments.

Here's an example of defining a macro using `\def`
which just inserts some static text.

```latex
\def\email{someone@gmail.com}
```

To use this, add `\email` everywhere that content should be inserted.

Here's an example of defining a macro using `\newcommand`.
The command being defined is `\image` and it takes three arguments.
The arguments are inserted where `#1`, `#2`, and `#3` appear.

{% raw %}

```latex
\newcommand{\image}[3]{
  \begin{figure}[H]
    \centering
    \includegraphics[width=#2]{#1}
    \caption{#3}
  \end{figure}
}
```

{% endraw %}

To use this, add `\image` followed by the three arguments,
each in their own pair of curly braces.
For example:

```latex
\image{smalltalk-balloon}{3in}{Smalltalk Programming}
```

This greatly simplies adding images in a document
as long as all should be centered and have a caption.

The `ifthen` package adds commands that support conditional logic
for choosing the text to render.
This can be used anywhere in a document, including in macros.

## Document Title, Author, and Date

The preamble can used the `\title`, `\author`, and `\date` commands to
specify information that will be used in a nicely formatted document title.
For example:

```latex
\documentclass{article}
\title{Server-Driven Web Apps with htmx}
\author{R. Mark Volkmann}
\date{\today}
```

Use the `\maketitle` command inside the document to render the title information.
For example:

```latex
\begin{document}
\maketitle
...
\end{document}
```

This renders the following title information:

<img alt="LaTeX maketitle output" style="width: 50%"
  src="/blog/assets/latex-maketitle.png?v={{pkg.version}}">

## Table of Contents

A table of contents can be generated for documents that use the
`\chapter`, `\section`, `\subsection`, and `\subsubsection` commands.
If the package `hyperref` is included,
the entries in the table of contents will be clickable links.

For example:

```latex
\documentclass{article}
\usepackage{hyperref}

\begin{document}
\tableofcontents

\section{Jumping In}
...
\subsection{Choosing a Tech Stack}
...
\subsection{Using htmx Attributes}
...
\subsection{Creating Your First Project}
...
\section{Exploring Server Options}
...
\subsection{Making the Grade}
...
\subsection{Popular Choices}
...
\subsection{Our Choice}
...
\section{Developing Endpoints}
...
\subsection{HTTP Requests}
...
\subsection{HTTP Responses}
...
\subsection{Endpoint Targets}
...
\end{document}
```

This generates the following table of contents:

<img alt="LaTeX table of contents" style="width: 80%"
  src="/blog/assets/latex-table-of-contents.png?v={{pkg.version}}">

## Formatting the Word LaTeX

The word LaTeX is specially formatted by surrounding it with backslashes.
For example, `\LaTeX\` is rendered as follows:

<img alt="LaTeX rendered" style="width: 10%"
  src="/blog/assets/latex-rendered.png?v={{pkg.version}}">

## Footnotes

To add numbered footnotes, use the `\footnote` command.
For example:

```latex
Oil that is, black gold, Texas tea.
\footnote{Texas tea is another name for oil.}
```

This adds a section at the bottom for page like the following:

<img alt="LaTeX footnote" style="width: 40%"
src="/blog/assets/latex-footnote.png?v={{pkg.version}}">

It also adds a number in square brackets like the following
that acts a clickable link to the corresponding footnote.

<img alt="LaTeX footnote number" style="width: 35%"
src="/blog/assets/latex-footnote-number.png?v={{pkg.version}}">

## Bibliography

To add a bibliography pages to a document:

1. Create a `.bib` file, perhaps with the same name as
   the `.tex` file that will use it.

1. Add directives to the `.bib` file. For example:

   ```text
   @book{htmx-volkmann,
     author = "Volkmann, R. Mark",
     isbn = "9798888650769",
     journal = "Pragmatic Bookshelf",
     pages = "184",
     publisher = "Pragmatic Bookshelf",
     title = "Server-Driven Web Apps with Htmx",
     url = "https://pragprog.com/titles/mvhtmx/server-driven-web-apps-with-htmx/",
     year = "2024"
   }
   ```

   Also see the directives @article, @inproceedings, and @misc.
   Each of these have a different set of required attributes.

1. Add the following commands near the bottom of the `.tex` file:

   ```latex
   \bibliographystyle{plain}
   \bibliography{some-name} % refers to the file some-name.bib
   ```

   This causes bibliography pages similar to the following to be generated.

   <img alt="LaTeX bibliography" style="width: 60%"
     src="/blog/assets/latex-bibliography.png?v={{pkg.version}}">

1. Add citations within the document that link to bibliography entries.
   For example:

   ```latex
   Learn about using htmx to build web applications.\cite{htmx-volkmann}
   ```

   This adds a number in square brackets that acts a
   clickable link to the corresponding bibliography entry.

## Slide Presentations

LaTeX can create PDF-based slide presentations
using the "beamer" document class.
See the following YouTube videos:

- <a href="https://www.youtube.com/watch?v=rx7wwtmFlD8"
  target="_blank">How I Make Presentations Using LaTeX & Beamer</a>
- <a href="https://www.youtube.com/watch?v=0fsWGg81RwU"
  target="_blank">LaTeX Tutorial 11: Beamer Slide Presentation</a>

## Resources

- {% aTargetBlank "https://latexref.xyz",
  "LaTeX2e: An unoffical reference manual" %}
- {% aTargetBlank "https://ctan.org/", "CTAN" %}
  Comprehensive TeX Archive Network
- <a href="https://latexcolor.com" target="_blank">LaTeX Color</a>
